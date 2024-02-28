#ifndef ZUNG_CODE_GENERATION_ENGINE_H
#define ZUNG_CODE_GENERATION_ENGINE_H

#include <AST.h>

class CodeGenerationEngine : public AST::ASTVisitor {
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
};

#endif //ZUNG_CODE_GENERATION_ENGINE_H