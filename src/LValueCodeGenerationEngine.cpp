#include <CodeGenerationEngine.h>
#include <AST.h>

void CodeGenerationEngine::LValueCodeGenerationEngine::visit(const AST::VariableExpression &expr) {
    engine_->stack_return(engine_->symbol_table_[expr.variable]);
}

void CodeGenerationEngine::LValueCodeGenerationEngine::visit(const AST::FieldAccessExpression &expr) {
    if (expr.struct_->assignable()) {
        expr.struct_->accept(*this);
        auto base_struct = engine_->stack_get<llvm::AllocaInst *>();
        auto get_field = engine_->builder_->CreateStructGEP(base_struct->getAllocatedType(), base_struct, expr.field);
        engine_->stack_return(get_field);
    } else {
        expr.struct_->accept(*engine_);
        auto *struct_ptr = engine_->stack_get<llvm::Value *>();
        auto type = struct_ptr->getType();
        auto tmp = CodeGenerationEngine::create_entry_block_alloca(engine_->builder_->GetInsertBlock()->getParent(), "", type);
        engine_->builder_->CreateStore(struct_ptr, tmp);

        auto get_field = engine_->builder_->CreateStructGEP(tmp->getAllocatedType(), tmp, expr.field);
        engine_->stack_return(get_field);
    }
}

