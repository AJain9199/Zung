#ifndef ZUNG_AST_H
#define ZUNG_AST_H

#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <SymbolTable.h>
#include <lexer.h>
#include <map>

#define INJECT_ACCEPT() void accept(ASTVisitor &v) override { v.visit(*this); }

namespace AST {
    class ASTVisitor;

    class AbstractNode;
    class TranslationUnit;
    class Function;
    class FunctionPrototype;

    class Statement; /* Base statement */
    class CompoundStatement;
    class DeclarationStatement;
    class ExpressionStatement;
    class IfStatement;

    class Expression; /* Base expression */
    class AssignableExpression;
    class ArrayIndexingExpression;
    class FunctionCallExpression;
    class VariableExpression;
    class BinaryExpression;
    class UnaryExpression;
    class NumericConstantExpression;

    class ASTVisitor {
    public:
        virtual void visit(const AST::AbstractNode &) = 0;
        virtual void visit(const AST::Function &) = 0;
        virtual void visit(const AST::FunctionPrototype &) = 0;
        virtual void visit(const AST::CompoundStatement &) = 0;
        virtual void visit(const AST::DeclarationStatement &) = 0;
        virtual void visit(const AST::ExpressionStatement &) = 0;
        virtual void visit(const AST::IfStatement &) = 0;
        virtual void visit(const AST::ArrayIndexingExpression &) = 0;
        virtual void visit(const AST::BinaryExpression &) = 0;
        virtual void visit(const AST::UnaryExpression &) = 0;
        virtual void visit(const AST::VariableExpression &) = 0;
        virtual void visit(const AST::FunctionCallExpression &) = 0;
        virtual void visit(const AST::NumericConstantExpression &) = 0;
        virtual void visit(const AST::TranslationUnit &) = 0;
    };

    class AbstractNode {
    public:
        virtual ~AbstractNode() = default;

        virtual void accept(ASTVisitor &v)  = 0;
    };

    class TranslationUnit : public AbstractNode {
    public:
        std::vector<std::unique_ptr<Function>> functions;

        INJECT_ACCEPT();
    };

    class Function : public AbstractNode {
    public:
        Function(std::unique_ptr<FunctionPrototype> proto, std::unique_ptr<CompoundStatement> stmt);

        std::unique_ptr<FunctionPrototype> prototype;
        std::unique_ptr<CompoundStatement> body;

        INJECT_ACCEPT();
    };

    class FunctionPrototype : public AbstractNode {
    public:
        FunctionPrototype(std::string n, const std::vector<SymbolTableEntry *> &a, Type ret);

        std::string name;
        std::vector<SymbolTableEntry *> args;
        Type return_type;

        INJECT_ACCEPT();
    };

    class Statement : public AbstractNode {
    public:
        ~Statement() override = default;

        INJECT_ACCEPT();
    };

    class CompoundStatement : public Statement {
    public:
        explicit CompoundStatement(std::vector<std::unique_ptr<Statement>> stmts);

        std::vector<std::unique_ptr<Statement>> statements;

        INJECT_ACCEPT();
    };

    class DeclarationStatement : public Statement {

    public:
        explicit DeclarationStatement(std::map<SymbolTableEntry *, std::unique_ptr<Expression>> i_list);

        std::map<SymbolTableEntry *, std::unique_ptr<Expression>> init_list;

        INJECT_ACCEPT();
    };

    class ExpressionStatement : public Statement {
    public:
        explicit ExpressionStatement(std::unique_ptr<Expression> e);

        std::unique_ptr<Expression> expr;

        INJECT_ACCEPT();
    };

    class IfStatement : public Statement {
    public:
        std::unique_ptr<Expression> condition;
        std::unique_ptr<CompoundStatement> then;
        IfStatement(std::unique_ptr<Expression> cond, std::unique_ptr<CompoundStatement> t);
    };

    class Expression : public AbstractNode {};
    class AssignableExpression : public Expression {};

    class NumericConstantExpression : public Expression {
    public:
        explicit NumericConstantExpression(int v);

        int val;

        INJECT_ACCEPT();
    };

    class VariableExpression : public AssignableExpression {
    public:
        explicit VariableExpression(SymbolTableEntry *var);

        SymbolTableEntry *variable{};

        INJECT_ACCEPT();
    };

    class FunctionCallExpression : public Expression {
    public:
        FunctionCallExpression(std::string name, std::vector<std::unique_ptr<Expression>> a);

        std::string callee;
        std::vector<std::unique_ptr<Expression>> args;

        INJECT_ACCEPT();
    };

    class ArrayIndexingExpression : public AssignableExpression {
    public:
        ArrayIndexingExpression(std::unique_ptr<VariableExpression> arr, std::vector<std::unique_ptr<Expression>> idx);

        std::unique_ptr<VariableExpression> array;
        std::vector<std::unique_ptr<Expression>> index;

        INJECT_ACCEPT();
    };

    class BinaryExpression : public Expression {
    public:
        BinaryExpression(std::unique_ptr<Expression> lhs, Operator o, std::unique_ptr<Expression> rhs);

        std::unique_ptr<Expression> LHS;
        Operator op;
        std::unique_ptr<Expression> RHS;

        INJECT_ACCEPT();
    };

    class UnaryExpression : public Expression {
    public:
        UnaryExpression(Operator o, std::unique_ptr<Expression> op);

        Operator op;
        std::unique_ptr<Expression> Operand;

        INJECT_ACCEPT();
    };
}

#endif //ZUNG_AST_H
