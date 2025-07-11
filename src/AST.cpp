#include <AST.h>
#include <utility>

using namespace AST;
using namespace Symbols;

FunctionPrototype::FunctionPrototype(std::string n, const std::vector<SymbolTableEntry *> &a, TypeWrapper *ret,
                                     bool va) : name(
                                                    std::move(n)), args(a),
                                                return_type(ret), var_args(va) {
}

Function::Function(std::unique_ptr<FunctionPrototype> proto, std::unique_ptr<CompoundStatement> stmt) : prototype(
    std::move(proto)), body(std::move(stmt)) {
}

UnaryExpression::UnaryExpression(Operator o, std::unique_ptr<Expression> op) : op(o), operand(std::move(op)) {
}

BinaryExpression::BinaryExpression(std::unique_ptr<Expression> lhs, Operator o, std::unique_ptr<Expression> rhs,
                                   llvm::LLVMContext *ctxt) : LHS(
                                                                  std::move(lhs)), op(o), RHS(std::move(rhs)) {
    if (dynamic_cast<AST::AggregateLiteralExpression *>(rhs.get()) != nullptr) {
        dynamic_cast<AST::AggregateLiteralExpression *>(rhs.get())->cast_type = LHS->type(ctxt);
    }
}

ArrayIndexingExpression::ArrayIndexingExpression(std::unique_ptr<Expression> arr,
                                                 std::vector<std::unique_ptr<Expression> > idx) : array(std::move(arr)),
    index(std::move(
        idx)) {
}

VariableExpression::VariableExpression(SymbolTableEntry *var) : variable(var) {
}

IntegralLiteralExpression::IntegralLiteralExpression(int v) : val(v) {
}

ExpressionStatement::ExpressionStatement(std::unique_ptr<Expression> e) : expr(std::move(e)) {
}

DeclarationStatement::DeclarationStatement(std::map<SymbolTableEntry *, std::unique_ptr<Expression> > i_list)
    : init_list(std::move(i_list)) {
}

CompoundStatement::CompoundStatement(std::vector<std::unique_ptr<Statement> > stmts) : statements(std::move(stmts)) {
}
