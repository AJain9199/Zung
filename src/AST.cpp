#include <AST.h>
#include <utility>

using namespace AST;
using namespace Symbols;

FunctionPrototype::FunctionPrototype(std::string n, const std::vector<SymbolTableEntry *> &a, Type ret) : name(
        std::move(n)), args(a),
                                                                                                          return_type(
                                                                                                                  std::move(
                                                                                                                          ret)) {}

Function::Function(std::unique_ptr<FunctionPrototype> proto, std::unique_ptr<CompoundStatement> stmt) : prototype(
        std::move(proto)), body(std::move(stmt)) {}

UnaryExpression::UnaryExpression(Operator o, std::unique_ptr<Expression> op) : op(o), Operand(std::move(op)) {}

BinaryExpression::BinaryExpression(std::unique_ptr<Expression> lhs, Operator o, std::unique_ptr<Expression> rhs) : LHS(
        std::move(lhs)), op(o), RHS(std::move(rhs)) {}

ArrayIndexingExpression::ArrayIndexingExpression(std::unique_ptr<VariableExpression> arr,
                                                 std::vector<std::unique_ptr<Expression>> idx) : array(std::move(arr)),
                                                                                                 index(std::move(
                                                                                                         idx)) {}

FunctionCallExpression::FunctionCallExpression(std::string name, std::vector<std::unique_ptr<Expression>> a) : callee(
        std::move(name)), args(std::move(a)) {}

VariableExpression::VariableExpression(SymbolTableEntry *var) : variable(var) {}

NumericConstantExpression::NumericConstantExpression(int v) : val(v) {}

IfStatement::IfStatement(std::unique_ptr<Expression> cond, std::unique_ptr<CompoundStatement> t) : condition(
        std::move(cond)), then(std::move(t)) {}

ExpressionStatement::ExpressionStatement(std::unique_ptr<Expression> e) : expr(std::move(e)) {}

DeclarationStatement::DeclarationStatement(std::map<SymbolTableEntry *, std::unique_ptr<Expression>> i_list)
        : init_list(std::move(i_list)) {}

CompoundStatement::CompoundStatement(std::vector<std::unique_ptr<Statement>> stmts) : statements(std::move(stmts)) {}
