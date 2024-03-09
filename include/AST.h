#ifndef ZUNG_AST_H
#define ZUNG_AST_H

#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <SymbolTable.h>
#include <Lexer.h>
#include <map>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/DerivedTypes.h>

#define INJECT_ACCEPT() void accept(ASTVisitor &v) override { v.visit(*this); }

struct TypeInfo;

template<typename T, typename ...Tokens>
static bool in(T token, T first, Tokens... tokens) {
    return ((token == first) || in(token, tokens...));
}

namespace AST {
    class ASTVisitor;

    class AbstractNode;
    class ExternFunction;
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
    class StringLiteralExpression;
    class FloatLiteralExpression;

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
        virtual void visit(const AST::StringLiteralExpression &) = 0;
        virtual void visit(const AST::TranslationUnit &) = 0;
        virtual void visit(const AST::ExternFunction &) = 0;
        virtual void visit(const AST::FloatLiteralExpression &) = 0;
    };

    class AbstractNode {
    public:
        virtual ~AbstractNode() = default;

        virtual void accept(ASTVisitor &v)  = 0;
    };

    class TranslationUnit : public AbstractNode {
    public:
        std::vector<std::unique_ptr<Function>> functions;
        std::vector<std::unique_ptr<ExternFunction>> prototypes;

        std::map<std::string, struct TypeInfo> type_table;

        INJECT_ACCEPT();
    };

    class ExternFunction : public AbstractNode {
    public:
        std::string name;
        std::vector<llvm::Type *> args;
        llvm::Type *return_type;

        bool is_var_args = false;

        ExternFunction(std::string name, std::vector<llvm::Type *> args, llvm::Type *return_type, bool va): name(std::move(name)), args(std::move(args)), return_type(return_type), is_var_args(va) {}

        INJECT_ACCEPT();
    };

    class Function : public AbstractNode {
    public:
        Function(std::unique_ptr<FunctionPrototype> proto, std::unique_ptr<CompoundStatement> stmt);

        Symbols::SymbolTable *symbol_table;

        std::unique_ptr<FunctionPrototype> prototype;
        std::unique_ptr<CompoundStatement> body;

        INJECT_ACCEPT();
    };

    class FunctionPrototype : public AbstractNode {
    public:
        FunctionPrototype(std::string n, const std::vector<Symbols::SymbolTableEntry *> &a, llvm::Type *ret, bool va);

        std::string name;
        std::vector<Symbols::SymbolTableEntry *> args;
        bool var_args = false;
        llvm::Type *return_type;

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
        explicit DeclarationStatement(std::map<Symbols::SymbolTableEntry *, std::unique_ptr<Expression>> i_list);

        std::map<Symbols::SymbolTableEntry *, std::unique_ptr<Expression>> init_list;

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

    class Expression : public AbstractNode {
    public:
        virtual llvm::Type *type(llvm::LLVMContext *) =  0;
    };
    class AssignableExpression : public Expression {
    public:
        llvm::Type *type(llvm::LLVMContext *) override = 0;
    };

    class FloatLiteralExpression : public Expression {
    public:
        explicit FloatLiteralExpression(double v) : val(v) {};

        double val;

        INJECT_ACCEPT();

        llvm::Type *type(llvm::LLVMContext *context) override {
            return llvm::Type::getDoubleTy(*context);
        };
    };

    class NumericConstantExpression : public Expression {
    public:
        explicit NumericConstantExpression(int v);

        int val;

        INJECT_ACCEPT();

        llvm::Type *type(llvm::LLVMContext *context) override {
            return llvm::Type::getInt32Ty(*context);
        };
    };

    class VariableExpression : public AssignableExpression {
    public:
        explicit VariableExpression(Symbols::SymbolTableEntry *var);

        Symbols::SymbolTableEntry *variable{};

        INJECT_ACCEPT();

        llvm::Type *type(llvm::LLVMContext *context) override {
            return variable->type;
        };
    };

    class FunctionCallExpression : public Expression {
    public:
        FunctionCallExpression(std::string name, std::vector<std::unique_ptr<Expression>> a);

        std::string callee;
        std::vector<std::unique_ptr<Expression>> args;

        INJECT_ACCEPT();

        llvm::Type *type(llvm::LLVMContext *context) override {
            return llvm::Type::getVoidTy(*context);
        };
    };

    class ArrayIndexingExpression : public AssignableExpression {
    public:
        ArrayIndexingExpression(std::unique_ptr<VariableExpression> arr, std::vector<std::unique_ptr<Expression>> idx);

        std::unique_ptr<VariableExpression> array;
        std::vector<std::unique_ptr<Expression>> index;

        INJECT_ACCEPT();

        llvm::Type *type(llvm::LLVMContext *context) override {
            return array->variable->type;
        };
    };

    class BinaryExpression : public Expression {
    public:
        BinaryExpression(std::unique_ptr<Expression> lhs, Operator o, std::unique_ptr<Expression> rhs);

        std::unique_ptr<Expression> LHS;
        Operator op;
        std::unique_ptr<Expression> RHS;

        INJECT_ACCEPT();

        llvm::Type * type(llvm::LLVMContext *context) override {
            auto ltype = LHS->type(context);
            auto rtype = RHS->type(context);

            if (ltype == nullptr && rtype == nullptr) {
                return nullptr;
            }

            if (ltype == nullptr) {
                return rtype;
            }

            if (rtype == nullptr) {
                return ltype;
            }

            if (ltype->isIntegerTy()) {
                if (op == Operator::ADD || op == Operator::SUB || op == Operator::MUL || op == Operator::FLR || op == Operator::MOD) {
                    if (rtype->isIntegerTy()) {
                        return rtype->getIntegerBitWidth() > ltype ->getIntegerBitWidth() ? rtype : ltype;
                    } else if (rtype->isFloatTy()) {
                        return rtype;
                    }
                } else if (op == Operator::DIV) {
                    return llvm::Type::getFloatTy(*context);
                }
            }

            if (ltype->isFloatTy() || rtype->isFloatTy()) {
                return ltype;
            }

            if (op == Operator::EQ || op == Operator::NEQ || op == Operator::LE || op == Operator::GE || op == Operator::LEQ || op == Operator::GEQ) {
                return llvm::Type::getInt1Ty(*context);
            }

            if (ltype->isPointerTy() || rtype->isPointerTy()) {
                return ltype;
            }

            return nullptr;
        }
    };

    class UnaryExpression : public Expression {
    public:
        UnaryExpression(Operator o, std::unique_ptr<Expression> op);

        Operator op;
        std::unique_ptr<Expression> operand;

        INJECT_ACCEPT();

        llvm::Type *type(llvm::LLVMContext *context) override {
            auto optype = operand->type(context);

            if (op == Operator::AND) {
                return optype->getPointerTo();
            }

            if (op == Operator::MUL) {
                return nullptr;
            }

            if (optype->isIntegerTy()) {
                if (op == Operator::SUB) {
                    return optype;
                }
            } else if (optype->isFloatTy()) {
                return optype;
            }
            return nullptr;
        };
    };

    class StringLiteralExpression : public Expression {
    public:
        explicit StringLiteralExpression(std::string s) : val(s) {};

        std::string val;

        INJECT_ACCEPT();

        llvm::Type *type(llvm::LLVMContext *context) override {
            return llvm::Type::getInt8Ty(*context)->getPointerTo();
        };
    };
}

#endif //ZUNG_AST_H
