#include <CodeGenerationEngine.h>
#include <AST.h>

void CodeGenerationEngine::LValueCodeGenerationEngine::visit(const AST::VariableExpression &expr) {
    expr.variable->type->type->print(llvm::errs(), true);
    engine_->symbol_table_[expr.variable]->getType()->print(llvm::errs(), true);
    engine_->stack_return(engine_->symbol_table_[expr.variable]);
}

void CodeGenerationEngine::LValueCodeGenerationEngine::visit(const AST::FieldAccessExpression &expr) {
    if (expr.struct_->assignable()) {
        expr.struct_->accept(*this);
        auto base_struct = engine_->stack_get<llvm::AllocaInst *>();
        auto get_field = engine_->builder_->CreateStructGEP(expr.struct_->type(engine_->llvm_context_.get())->type, base_struct, expr.field.idx);
        engine_->stack_return(get_field);
    } else {
        expr.struct_->accept(*engine_);
        auto *struct_ptr = engine_->stack_get<llvm::Value *>();
        auto get_field = engine_->builder_->CreateStructGEP(expr.struct_->type(engine_->llvm_context_.get())->type, struct_ptr, expr.field.idx);
        engine_->stack_return(get_field);
    }
}

