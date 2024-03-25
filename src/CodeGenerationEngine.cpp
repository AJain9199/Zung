#include <CodeGenerationEngine.h>
#include <iostream>
#include <llvm/IRPrinter/IRPrintingPasses.h>

#define STACK_RET(x) stack_return(x); return
#define STACK_GET(x) (stack_get<x>())

using namespace llvm;

void CodeGenerationEngine::visit(const AST::IntegralLiteralExpression &expression) {
    STACK_RET(ConstantInt::get(*llvm_context_, APInt(32, expression.val, true)));
}

void CodeGenerationEngine::visit(const AST::VariableExpression &expression) {
    if (expression.variable->scope == Symbols::GLOBAL) {
        auto var = global_symbol_table_[expression.variable];
        STACK_RET(builder_->CreateLoad(var->getValueType(), var, expression.variable->name()));
    } else {
        auto var = symbol_table_[expression.variable];
        STACK_RET(builder_->CreateLoad(var->getAllocatedType(), var, expression.variable->name()));
    }
}

void CodeGenerationEngine::visit(const AST::BinaryExpression &expression) {
    if (expression.op == EQ) {
        if (dynamic_cast<AST::AssignableExpression *>(expression.LHS.get()) == nullptr) {
            throw std::runtime_error("LHS of assignment must be an assignable expression");
        }

        Value *RHS;
        if (dynamic_cast<AST::AggregateLiteralExpression *>(expression.RHS.get()) != nullptr) {
            auto *agg = dynamic_cast<AST::AggregateLiteralExpression *>(expression.RHS.get());
            this->lhs = expression.LHS.get();

            agg->cast_type = expression.LHS->type(llvm_context_.get());
            agg->accept(*this);
            if (RHS = STACK_GET(Value *); RHS == nullptr) {
                return;
            }
        } else {
            expression.RHS->accept(*this);
            RHS = STACK_GET(Value *);
        }

        expression.LHS->accept(*rvalue_engine_);
        auto *LHS = STACK_GET(Value *);

        builder_->CreateStore(RHS, LHS);
        STACK_RET(RHS);
    }

    expression.LHS->accept(*this);
    auto LHS = STACK_GET(Value *);

    expression.RHS->accept(*this);
    auto RHS = STACK_GET(Value *);

    Instruction::BinaryOps binop;
    ICmpInst::Predicate icmp;

    switch (expression.op) {
        case ADD:
            binop = Instruction::Add;
            goto binary_ops;
        case SUB:
            binop = Instruction::Sub;
            goto binary_ops;
        case MUL:
            binop = Instruction::Mul;
            goto binary_ops;
        case DIV:
            binop = Instruction::SDiv;
            goto binary_ops;
        case MOD:
            binop = Instruction::SRem;
            goto binary_ops;
        case RSH:
            binop = Instruction::LShr;
            goto binary_ops;
        case LSH:
            binop = Instruction::Shl;
            goto binary_ops;
        case AND:
            binop = Instruction::And;
            goto binary_ops;
        case OR:
            binop = Instruction::Or;
            goto binary_ops;
//        case XOR:
//            binop = Instruction::Xor;
//            goto binary_ops;
        case LOGICAL_AND:
            binop = Instruction::And;
            goto rel_ops;
        case LOGICAL_OR:
            binop = Instruction::Or;
            goto rel_ops;
        case EQ_EQ:
            icmp = ICmpInst::ICMP_EQ;
            goto cmp_ops;
        case NEQ:
            icmp = ICmpInst::ICMP_NE;
            goto cmp_ops;
        case GE:
            icmp = ICmpInst::ICMP_SGT;
            goto cmp_ops;
        case GEQ:
            icmp = llvm::CmpInst::ICMP_SGE;
            goto cmp_ops;
        case LE:
            icmp = ICmpInst::ICMP_SLT;
            goto cmp_ops;
        case LEQ:
            icmp = ICmpInst::ICMP_SLE;
            goto cmp_ops;
        case EXP:
        STACK_RET(builder_->CreateCall(module_->getFunction("pow"), {LHS, RHS}));
        case FLR:
        STACK_RET(builder_->CreateCall(module_->getFunction("flr"), {LHS, RHS}));
        default:
            throw std::runtime_error("Not implemented yet");
    }
    binary_ops:
STACK_RET(builder_->CreateBinOp(binop, LHS, RHS, "tmp"));
    rel_ops:
STACK_RET(builder_->CreateLogicalOp(binop, LHS, RHS, "tmp"));
    cmp_ops:
STACK_RET(builder_->CreateCmp(icmp, LHS, RHS, "tmp"));
}

void CodeGenerationEngine::visit(const AST::Function &function) {
    AST::FunctionPrototype &P = *(function.prototype);
    function_table_[P.name] = function.prototype.get();

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

    delete function.symbol_table;

    STACK_RET(F);
}


void CodeGenerationEngine::visit(const AST::FunctionPrototype &prototype) {
    symbol_table_.clear();

    std::vector<llvm::Type *> args(prototype.args.size());
    uint i = 0;
    for (const auto &arg: prototype.args) {
        args[i++] = arg->type->type;
    }

    FunctionType *FT = FunctionType::get(prototype.return_type->type, args, prototype.var_args);

    Function *F = Function::Create(FT, Function::ExternalLinkage, prototype.name, module_.get());

    i = 0;
    for (auto &arg: F->args()) {
        arg.setName(prototype.args[i++]->name());
    }

    STACK_RET(F);
}

void CodeGenerationEngine::visit(const AST::ReturnStatement &statement) {
    if (statement.expr != nullptr) {
        statement.expr->accept(*this);
        builder_->CreateRet(STACK_GET(Value *));
    } else {
        builder_->CreateRetVoid();
    }
}

void CodeGenerationEngine::visit(const AST::DeclarationStatement &statement) {
    for (auto &it: statement.init_list) {
        it.second->accept(*this);
        auto *val = STACK_GET(Value *);

        if (it.first->scope == Symbols::GLOBAL) {
            auto *global = new GlobalVariable(*module_, it.first->type->type, false, GlobalValue::ExternalLinkage,
                                              nullptr, it.first->name());
            if (!isa<Constant>(val)) {
                throw std::runtime_error("Global variable must be initialized with a constant");
            }

            global->setInitializer(dyn_cast<Constant>(val));
            global_symbol_table_[it.first] = global;
        } else {
            Function *F = builder_->GetInsertBlock()->getParent();
            AllocaInst *alloca = create_entry_block_alloca(F, it.first->name(), it.first->type->type);
            symbol_table_[it.first] = alloca;
            builder_->CreateStore(val, alloca);
        }
    }
}

void CodeGenerationEngine::visit(const AST::ExpressionStatement &statement) {
    statement.expr->accept(*this);
}

void CodeGenerationEngine::visit(const AST::FunctionCallExpression &expression) {
    get_llvm_function(std::get<AST::FunctionPrototype *>(expression.callee->func)->name);
    auto F = STACK_GET(Function *);

    if (F->arg_size() != expression.args.size() && !F->isVarArg()) {
        throw std::runtime_error("Incorrect number of arguments");
    }

    std::vector<Value *> args(expression.args.size());
    int i = 0;
    for (auto &arg: expression.args) {
        if (expression.is_method && i == 0) {
            arg->accept(*rvalue_engine_);
            args[i++] = STACK_GET(Value *);
            continue;
        }

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
    for (auto &i : unit.global_declarations) {
        i->accept(*this);
    }

    for (auto &i: unit.prototypes) {
        i->accept(*this);
        STACK_GET(Function *); // don't pollute stack, returns are always popped off the stack
    }

    for (auto &i: unit.functions) {
        i->accept(*this);
        STACK_GET(Function *);
    }


    std::error_code errorCode;
    llvm::raw_fd_ostream file("../output.ll", errorCode);
    module_->print(file, nullptr);
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
    if (expression.op == MUL) {
        expression.operand->accept(*this);
        auto *val = STACK_GET(Value *);

        STACK_RET(builder_->CreateLoad(expression.operand->type(llvm_context_.get())->pointee_type->type, val));
    }

    expression.operand->accept(*this);
    auto *val = STACK_GET(Value *);

    val->getType()->print(llvm::errs(), true);
    switch (expression.op) {
        case SUB:
        STACK_RET(builder_->CreateNeg(val));
        case AND: {
            if (dynamic_cast<AST::VariableExpression *>(expression.operand.get()) != nullptr) {
                STACK_RET(symbol_table_[dynamic_cast<AST::VariableExpression *>(expression.operand.get())->variable]);
            }

            auto var = create_entry_block_alloca(builder_->GetInsertBlock()->getParent(), "",
                                                 val->getType()->getPointerTo());
            builder_->CreateStore(val, var);
            STACK_RET(var);
        }
        default:
            throw std::runtime_error("Not implemented yet");
    }
}

void CodeGenerationEngine::visit(const AST::ExternFunction &function) {
    std::vector<llvm::Type *> args(function.args.size());
    uint i = 0;
    for (const auto &arg: function.args) {
        args[i++] = arg->type->type;
    }

    FunctionType *FT = FunctionType::get(function.return_type->type, args, function.var_args);
    Function *F = Function::Create(FT, Function::ExternalLinkage, function.name, module_.get());

    STACK_RET(F);
}

void CodeGenerationEngine::visit(const AST::StringLiteralExpression &expression) {
    STACK_RET(builder_->CreateGlobalStringPtr(expression.val));
}

void CodeGenerationEngine::visit(const AST::FloatLiteralExpression &expression) {
    STACK_RET(ConstantFP::get(*llvm_context_, APFloat(expression.val)));
}

void CodeGenerationEngine::visit(const AST::ForStatement &statement) {
    statement.init->accept(*this);

    auto *loop = BasicBlock::Create(*llvm_context_, "loop", builder_->GetInsertBlock()->getParent());
    auto *cond = BasicBlock::Create(*llvm_context_, "cond", builder_->GetInsertBlock()->getParent());
    auto *update = BasicBlock::Create(*llvm_context_, "update", builder_->GetInsertBlock()->getParent());
    auto *exit = BasicBlock::Create(*llvm_context_, "exit", builder_->GetInsertBlock()->getParent());

    builder_->CreateBr(cond);

    builder_->SetInsertPoint(cond);

    statement.condition->accept(*this);
    auto *cond_val = STACK_GET(Value *);
    builder_->CreateCondBr(cond_val, loop, exit);

    builder_->SetInsertPoint(loop);
    statement.body->accept(*this);

    builder_->CreateBr(update);
    builder_->SetInsertPoint(update);
    statement.update->accept(*this);
    STACK_GET(Value *);
    builder_->CreateBr(cond);

    builder_->SetInsertPoint(exit);
}

void CodeGenerationEngine::visit(const AST::IfStatement &statement) {
    auto if_block = BasicBlock::Create(*llvm_context_, "if", builder_->GetInsertBlock()->getParent());
    auto exit = BasicBlock::Create(*llvm_context_, "exit", builder_->GetInsertBlock()->getParent());
    statement.condition->accept(*this);
    auto cond = STACK_GET(Value *);

    if (statement.else_body != nullptr) {
        auto else_block = BasicBlock::Create(*llvm_context_, "else", builder_->GetInsertBlock()->getParent());
        builder_->CreateCondBr(cond, if_block, else_block);

        builder_->SetInsertPoint(else_block);
        statement.else_body->accept(*this);
        builder_->CreateBr(exit);
    } else {
        builder_->CreateCondBr(cond, if_block, exit);
    }

    builder_->SetInsertPoint(if_block);
    statement.body->accept(*this);
    builder_->CreateBr(exit);

    builder_->SetInsertPoint(exit);
}

void CodeGenerationEngine::visit(const AST::FieldAccessExpression &expression) {
    llvm::Value *base_struct;
    if (expression.struct_->assignable()) {
        expression.struct_->accept(*rvalue_engine_);
        base_struct = STACK_GET(llvm::Value *);
        base_struct->getType()->print(llvm::errs(), true);
        auto get_field = builder_->CreateStructGEP(expression.struct_->type(llvm_context_.get())->type, base_struct,
                                                   expression.field.idx);
        STACK_RET(builder_->CreateLoad(
                expression.struct_->type(llvm_context_.get())->type->getStructElementType(expression.field.idx),
                get_field));
    } else {
        expression.struct_->accept(*rvalue_engine_);
        auto *struct_ptr = STACK_GET(llvm::Value *);
        auto type = expression.struct_->type(llvm_context_.get())->type;

        auto get_field = builder_->CreateStructGEP(type, struct_ptr, expression.field.idx);
        STACK_RET(builder_->CreateLoad(type->getStructElementType(expression.field.idx), get_field));
    }
}

void CodeGenerationEngine::visit(const AST::ArrayIndexingExpression &expression) {
    expression.array->accept(*rvalue_engine_);
    auto *array = STACK_GET(llvm::Value *);

    std::vector<llvm::Value *> indices;
    indices.reserve(expression.index.size());
    for (auto &i: expression.index) {
        i->accept(*this);
        indices.push_back(STACK_GET(llvm::Value *));
    }

    auto *type = expression.array->type(llvm_context_.get())->type;
    for (int i = 0; i < expression.index.size(); i++) {
        type = type->getArrayElementType();
    }

    auto *ptr = builder_->CreateGEP(expression.array->type(llvm_context_.get())->type, array, indices);
    STACK_RET(builder_->CreateLoad(type, ptr));
}

RETURNS(llvm::Value *) CodeGenerationEngine::visit(const AST::AggregateLiteralExpression &expression) {
    bool is_const = true;
    std::vector<Value *> elements;
    elements.reserve(expression.elements.size());
    for (auto &i : expression.elements) {
        i->accept(*this);
        auto val = STACK_GET(llvm::Value *);
        elements.push_back(val);
        if (!isa<Constant>(val)) {
            is_const = false;
        }
    }

    if (is_const) {
        if (expression.cast_type->type->isStructTy()) {
            std::vector<Constant *> const_elements;
            const_elements.reserve(elements.size());
            for (int i = 0; i < elements.size(); i++) {
                auto el = dyn_cast<Constant>(elements[i]);
                expression.cast_type->type->getStructElementType(i)->print(llvm::outs());
                el->mutateType(expression.cast_type->type->getStructElementType(i));
                const_elements.push_back(el);
            }
            STACK_RET(ConstantStruct::get(dyn_cast<StructType>(expression.cast_type->type), const_elements));
        } else {
            std::vector<Constant *> const_elements;
            const_elements.reserve(elements.size());
            for (auto el : elements) {
                el->mutateType(expression.cast_type->type->getArrayElementType());
                const_elements.push_back(dyn_cast<Constant>(el));
            }
            STACK_RET(ConstantArray::get(dyn_cast<ArrayType>(expression.cast_type->type), const_elements));
        }
    } else {
        if (expression.cast_type->type->isStructTy()) {
            lhs->accept(*rvalue_engine_);
            auto *lhs_val = STACK_GET(Value *);
            for (int i = 0; i < elements.size(); i++) {
                auto field = builder_->CreateStructGEP(expression.cast_type->type, lhs_val, i);
                builder_->CreateStore(elements[i], field);
            }
            STACK_RET(nullptr);
        }
    }
}

void CodeGenerationEngine::visit(const AST::WhileStatement &statement) {
    auto *loop = BasicBlock::Create(*llvm_context_, "loop", builder_->GetInsertBlock()->getParent());
    auto *cond = BasicBlock::Create(*llvm_context_, "cond", builder_->GetInsertBlock()->getParent());
    auto *exit = BasicBlock::Create(*llvm_context_, "exit", builder_->GetInsertBlock()->getParent());
    builder_->CreateBr(loop);
    builder_->SetInsertPoint(cond);

    statement.condition->accept(*this);
    auto *cond_val = STACK_GET(Value *);
    builder_->CreateCondBr(cond_val, loop, exit);

    builder_->SetInsertPoint(loop);
    statement.body->accept(*this);
    builder_->CreateBr(cond);

    builder_->SetInsertPoint(exit);
}

void CodeGenerationEngine::visit(const AST::BooleanLiteralExpression &expression) {
    STACK_RET(ConstantInt::get(Type::getInt1Ty(*llvm_context_), expression.val));
}

void CodeGenerationEngine::visit(const AST::NullLiteralExpression &) {
    STACK_RET(ConstantPointerNull::getNullValue(PointerType::get(*llvm_context_, 0)));
}
