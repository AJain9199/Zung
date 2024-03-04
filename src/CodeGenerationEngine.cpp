#include <CodeGenerationEngine.h>
#include <iostream>

#define STACK_RET(x) stack_return(x); return
#define STACK_GET(x) (x)(stack_get())

#define i1(x) builder_->CreateTrunc(x, Type::getInt1Ty(*llvm_context_))

using namespace llvm;

void CodeGenerationEngine::visit(const AST::NumericConstantExpression &expression) {
    STACK_RET(ConstantInt::get(*llvm_context_, APInt(32, expression.val, true)));
}

void CodeGenerationEngine::visit(const AST::VariableExpression &expression) {
    auto var = symbol_table_[expression.variable];
    STACK_RET(builder_->CreateLoad(var->getAllocatedType(), var, expression.variable->name));
}

void CodeGenerationEngine::visit(const AST::BinaryExpression &expression) {
    if (expression.op == EQ) {
        if (dynamic_cast<AST::AssignableExpression *>(expression.LHS.get()) == nullptr) {
            throw std::runtime_error("LHS of assignment must be an assignable expression");
        }

        Value *LHS = nullptr;
        auto *exp = dynamic_cast<AST::VariableExpression *>(expression.LHS.get());
        if (exp != nullptr) {
            LHS = symbol_table_[exp->variable];
        }

        expression.RHS->accept(*this);
        auto *RHS = STACK_GET(Value *);

        builder_->CreateStore(RHS, LHS);
        STACK_RET(RHS);
    }

    expression.LHS->accept(*this);
    auto LHS = STACK_GET(Value *);

    expression.RHS->accept(*this);
    auto RHS = STACK_GET(Value *);

    switch (expression.op) {
        case ADD:
        STACK_RET(builder_->CreateAdd(LHS, RHS, "addtmp"));
        case SUB:
        STACK_RET(builder_->CreateSub(LHS, RHS, "subtmp"));
        case MUL:
        STACK_RET(builder_->CreateMul(LHS, RHS, "multmp"));
        case DIV:
        STACK_RET(builder_->CreateSDiv(LHS, RHS, "divtmp"));
        case MOD:
        STACK_RET(builder_->CreateSRem(LHS, RHS, "modtmp"));
        case RSH:
        STACK_RET(builder_->CreateLShr(LHS, RHS, "rshtmp"));
        case LSH:
        STACK_RET(builder_->CreateShl(LHS, RHS, "lshtmp"));
        case AND:
        STACK_RET(builder_->CreateAnd(LHS, RHS, "andtmp"));
        case OR:
        STACK_RET(builder_->CreateOr(LHS, RHS, "ortmp"));
        case XOR:
        STACK_RET(builder_->CreateXor(LHS, RHS, "xortmp"));
        case LOGICAL_AND:
        STACK_RET(builder_->CreateAnd(i1(LHS), i1(RHS), "andtmp"));
        case LOGICAL_OR:
        STACK_RET(builder_->CreateOr(i1(LHS), i1(RHS), "ortmp"));
        case GE:
        STACK_RET(builder_->CreateICmpSGT(LHS, RHS, "gttmp"));
        case LE:
        STACK_RET(builder_->CreateICmpSLT(LHS, RHS, "gttmp"));
        case GEQ:
        STACK_RET(builder_->CreateICmpSGE(LHS, RHS, "gttmp"));
        case LEQ:
        STACK_RET(builder_->CreateICmpSLE(LHS, RHS, "gttmp"));
        case EQ:
        STACK_RET(builder_->CreateICmpEQ(LHS, RHS, "gttmp"));
        case NEQ:
        STACK_RET(builder_->CreateICmpNE(LHS, RHS, "gttmp"));
        default:
            throw std::runtime_error("Not implemented yet");
    }
}

void CodeGenerationEngine::visit(const AST::Function &function) {
    AST::FunctionPrototype &P = *(function.prototype);
    function_table_.insert<func_table_t::value_type>(
            func_table_t::value_type(P.name, std::make_unique<AST::FunctionPrototype>(P)));

    get_llvm_function(P.name);
    auto *F = STACK_GET(Function *);


    if (function.body == nullptr) {
        STACK_RET(F);
    }

    if (!F->empty()) {
        throw std::runtime_error("Function already defined");
    }

    BasicBlock *block = BasicBlock::Create(*llvm_context_, "entry", F);
    builder_->SetInsertPoint(block);

    symbol_table_.clear();
    for (auto &arg: F->args()) {
        AllocaInst *alloca = builder_->CreateAlloca(arg.getType(), nullptr, arg.getName());
        builder_->CreateStore(&arg, alloca);
        symbol_table_[function.symbol_table->find(std::string(arg.getName()))] = alloca;
    }

    function.body->accept(*this);
    verifyFunction(*F);

    /* TODO: optimizer pass */

    STACK_RET(F);
}


void CodeGenerationEngine::visit(const AST::FunctionPrototype &prototype) {
    symbol_table_.clear();

    std::vector<llvm::Type *> args(prototype.args.size());
    uint i = 0;
    for (const auto &arg: prototype.args) {
        args[i++] = arg->type;
    }

    FunctionType *FT = FunctionType::get(prototype.return_type, args, false);

    Function *F = Function::Create(FT, Function::ExternalLinkage, prototype.name, module_.get());

    i = 0;
    for (auto &arg: F->args()) {
        arg.setName(prototype.args[i++]->name);
    }

    STACK_RET(F);
}

void CodeGenerationEngine::visit(const AST::DeclarationStatement &statement) {
    Function *F = builder_->GetInsertBlock()->getParent();

    for (auto &it: statement.init_list) {
        it.second->accept(*this);
        auto *val = STACK_GET(Value *);

        AllocaInst *alloca = create_entry_block_alloca(F, it.first->name, val->getType());
        builder_->CreateStore(val, alloca);
        symbol_table_[it.first] = alloca;
    }
}

void CodeGenerationEngine::visit(const AST::ExpressionStatement &statement) {
    statement.expr->accept(*this);
}

void CodeGenerationEngine::visit(const AST::FunctionCallExpression &expression) {
    get_llvm_function(expression.callee);
    auto F = STACK_GET(Function *);

    if (F->arg_size() != expression.args.size()) {
        throw std::runtime_error("Incorrect number of arguments");
    }

    std::vector<Value *> args(expression.args.size());
    int i = 0;
    for (auto &arg: expression.args) {
        arg->accept(*this);
        args[i++] = (STACK_GET(Value *));
    }
    if (F->getReturnType()->isVoidTy()) {
        STACK_RET(builder_->CreateCall(F, args));
    } else {
        STACK_RET(builder_->CreateCall(F, args, "calltmp"));
    }
}


void CodeGenerationEngine::visit(const AST::CompoundStatement &statement) {
    for (auto &stmt: statement.statements) {
        stmt->accept(*this);
    }
}

void CodeGenerationEngine::visit(const AST::TranslationUnit &unit) {
    for (auto &i: unit.prototypes) {
        i->accept(*this);
        auto *F = STACK_GET(Function *);
        F->print(errs());
    }

    for (auto &i: unit.functions) {
        i->accept(*this);
        auto *F = STACK_GET(Function *);
        F->print(errs());
    }
}

void CodeGenerationEngine::get_llvm_function(const std::string &name) {
    if (auto *F = module_->getFunction(name)) {
        STACK_RET(F);
    }

    auto FI = function_table_.find(name);
    if (FI != function_table_.end()) {
        FI->second->accept(*this);
        return;
    }

    STACK_RET(nullptr);
}

llvm::AllocaInst *
CodeGenerationEngine::create_entry_block_alloca(llvm::Function *func, const std::string &var_name, llvm::Type *type) {
    return IRBuilder<>(&func->getEntryBlock(), func->getEntryBlock().begin()).CreateAlloca(type, nullptr, var_name);
}

void CodeGenerationEngine::visit(const AST::UnaryExpression &expression) {
    expression.Operand->accept(*this);
    auto *val = STACK_GET(Value *);
    switch (expression.op) {
        case SUB:
        STACK_RET(builder_->CreateNeg(val));
        default:
            throw std::runtime_error("Not implemented yet");
    }
}

void CodeGenerationEngine::visit(const AST::ExternFunction &function) {
    std::vector<llvm::Type *> args(function.args.size());
    uint i = 0;
    for (const auto &arg: function.args) {
        args[i++] = arg;
    }

    FunctionType *FT = FunctionType::get(function.return_type, args, false);
    Function *F = Function::Create(FT, Function::ExternalLinkage, function.name, module_.get());

    STACK_RET(F);
}