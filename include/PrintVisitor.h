#ifndef ZUNG_PRINTVISITOR_H
#define ZUNG_PRINTVISITOR_H

#include <AST.h>

class PrintVisitor : public AST::ASTVisitor {
private:
    int tab_level = 0;

public:
    void visit(const AST::AbstractNode &) override;
    void visit(const AST::TranslationUnit &) override;
    void visit(const AST::Function &) override;
    void visit(const AST::FunctionPrototype &) override;
    void visit(const AST::CompoundStatement &) override;
    void visit(const AST::DeclarationStatement &) override;
    void visit(const AST::ExpressionStatement &) override;
    void visit(const AST::IfStatement &) override;
    void visit(const AST::ArrayIndexingExpression &) override;
    void visit(const AST::FunctionCallExpression &) override;
    void visit(const AST::VariableExpression &) override;
    void visit(const AST::BinaryExpression &) override;
    void visit(const AST::UnaryExpression &) override;
    void visit(const AST::NumericConstantExpression &) override;
    void visit(const AST::ExternFunction &) override;
    void visit(const AST::StringLiteralExpression &) override;
};

#endif //ZUNG_PRINTVISITOR_H
