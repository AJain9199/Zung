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
#include "Types.h"

#define INJECT_ACCEPT() void accept(ASTVisitor &v) override { v.visit(*this); }

struct TypeInfo;
struct FieldInfo;

/*
 * Contains all the AST nodes and the abstract visitor class.
 */
namespace AST {
    class ASTVisitor;

    class AbstractNode;

    class ExternFunction;

    class TranslationUnit;

    class Function;

    class FunctionPrototype;

    class Statement; /* Base statement */
    class CompoundStatement;

    class ReturnStatement;

    class ForStatement;

    class DeclarationStatement;

    class ExpressionStatement;

    class IfStatement;

    class Expression; /* Base expression */
    class AssignableExpression;

    class ArrayIndexingExpression;

    class FunctionCallExpression;

    class VariableExpression;

    class FieldAccessExpression;

    class BinaryExpression;

    class UnaryExpression;

    class IntegralLiteralExpression;

    class StringLiteralExpression;

    class FloatLiteralExpression;

    /*
     * The visitor class for the AST.
     * Each method in the visitor class corresponds to a node in the AST.
     *
     * Implementation details: each AST node must have an accept method. The accept method of the AST nodes may not call
     * the accept methods of its children. This is the responsibility of the visitor.
     */
    class ASTVisitor {
    public:
        virtual void visit(const AST::AbstractNode &) = 0;

        virtual void visit(const AST::Function &) = 0;

        virtual void visit(const AST::FunctionPrototype &) = 0;

        virtual void visit(const AST::CompoundStatement &) = 0;

        virtual void visit(const AST::DeclarationStatement &) = 0;

        virtual void visit(const AST::ReturnStatement &) = 0;

        virtual void visit(const AST::ForStatement &) = 0;

        virtual void visit(const AST::ExpressionStatement &) = 0;

        virtual void visit(const AST::IfStatement &) = 0;

        virtual void visit(const AST::ArrayIndexingExpression &) = 0;

        virtual void visit(const AST::BinaryExpression &) = 0;

        virtual void visit(const AST::UnaryExpression &) = 0;

        virtual void visit(const AST::VariableExpression &) = 0;

        virtual void visit(const AST::FieldAccessExpression &) = 0;

        virtual void visit(const AST::FunctionCallExpression &) = 0;

        virtual void visit(const AST::IntegralLiteralExpression &) = 0;

        virtual void visit(const AST::StringLiteralExpression &) = 0;

        virtual void visit(const AST::TranslationUnit &) = 0;

        virtual void visit(const AST::ExternFunction &) = 0;

        virtual void visit(const AST::FloatLiteralExpression &) = 0;
    };

    /* Base class for all AST nodes */
    class AbstractNode {
    public:
        virtual ~AbstractNode() = default;

        virtual void accept(ASTVisitor &v) = 0;
    };

    /*
     * Stores the translation unit, which is the source file as of writing.
     * The source file may contain multiple functions and prototypes (both extern and local) and structures.
     */
    class TranslationUnit : public AbstractNode {
    public:
        std::vector<std::unique_ptr<Function>> functions;
        std::vector<std::unique_ptr<ExternFunction>> prototypes;

        std::map<std::string, struct TypeInfo *> type_table;

        INJECT_ACCEPT();
    };


    /*
     * Stores the function, which is the main unit of execution in the source file.  It may or may not include the function body
     * (in the case of a prototype).
     */
    class Function : public AbstractNode {
    public:
        Function(std::unique_ptr<FunctionPrototype> proto, std::unique_ptr<CompoundStatement> stmt);

        Symbols::SymbolTable *symbol_table;

        std::unique_ptr<FunctionPrototype> prototype;
        std::unique_ptr<CompoundStatement> body;

        INJECT_ACCEPT();
    };

    /*
     * Represents the function signature
     * internal_name(args) : return_type
     */
    class FunctionPrototype : public AbstractNode {
    public:
        FunctionPrototype(std::string n, const std::vector<Symbols::SymbolTableEntry *> &a, TypeWrapper *ret, bool va);

        std::string name;
        std::vector<Symbols::SymbolTableEntry *> args;
        bool var_args = false;
        TypeWrapper *return_type;

        INJECT_ACCEPT();

        ~FunctionPrototype() override {
            delete return_type;
        }
    };

    /*
     * Stores the function prototype, for an extern function. Should be linked with the translation unit.
     */
    class ExternFunction : public FunctionPrototype {
    public:
        std::vector<TypeWrapper *> typed_args;

        ExternFunction(std::string name, std::vector<TypeWrapper *> args, TypeWrapper *return_type, bool va)
                : FunctionPrototype(std::move(name), {}, return_type, va), typed_args(std::move(args)) {}

        ~ExternFunction() override {
            for (auto &arg: typed_args) {
                delete arg;
            }
        }

        INJECT_ACCEPT();
    };

    /*
     * The base statement class.
     * Children: CompoundStatement, DeclarationStatement, ExpressionStatement, IfStatement
     */
    class Statement : public AbstractNode {
    public:
        ~Statement() override = default;

        INJECT_ACCEPT();
    };

    /*
     * A collection of statements.
     */
    class CompoundStatement : public Statement {
    public:
        explicit CompoundStatement(std::vector<std::unique_ptr<Statement>> stmts);

        std::vector<std::unique_ptr<Statement>> statements;

        INJECT_ACCEPT();
    };

    /*
     * A return statement with an optional return value.
     */
    class ReturnStatement : public Statement {
    public:
        explicit ReturnStatement(std::unique_ptr<Expression> e) : expr(std::move(e)) {};

        std::unique_ptr<Expression> expr;

        INJECT_ACCEPT();
    };

    /*
     * A declaration statement. Multiple variables may be declared, with different types and initial values.
     */
    class DeclarationStatement : public Statement {
    public:
        explicit DeclarationStatement(std::map<Symbols::SymbolTableEntry *, std::unique_ptr<Expression>> i_list);

        std::map<Symbols::SymbolTableEntry *, std::unique_ptr<Expression>> init_list;

        INJECT_ACCEPT();
    };

    /*
     * An expression statement. It may be any kind of expression, including function calls, assignments, etc.
     */
    class ExpressionStatement : public Statement {
    public:
        explicit ExpressionStatement(std::unique_ptr<Expression> e);

        std::unique_ptr<Expression> expr;

        INJECT_ACCEPT();
    };

    /*
     * Represents a for loop.
     */
    class ForStatement : public Statement {
    public:
        std::unique_ptr<Statement> init;
        std::unique_ptr<Expression> condition;
        std::unique_ptr<Expression> update;
        std::unique_ptr<CompoundStatement> body;

        ForStatement(std::unique_ptr<Statement> i, std::unique_ptr<Expression> cond, std::unique_ptr<Expression> up,
                     std::unique_ptr<CompoundStatement> b) : init(std::move(i)), condition(std::move(cond)),
                                                             update(std::move(up)), body(std::move(b)) {};

        INJECT_ACCEPT();
    };

    /*
     * An if statement. It may or may not have an else clause.
     */
    class IfStatement : public Statement {
    public:
        std::unique_ptr<Expression> condition;
        std::unique_ptr<Statement> body;
        std::unique_ptr<Statement> else_body;

        IfStatement(std::unique_ptr<Expression> cond, std::unique_ptr<Statement> b, std::unique_ptr<Statement> e)
                : condition(std::move(cond)), body(std::move(b)), else_body(std::move(e)) {};

        INJECT_ACCEPT();
    };

    /*
     * Base class for all expressions.
     * Children: FloatLiteralExpression, IntegralLiteralExpression, VariableExpression, FunctionCallExpression, ArrayIndexingExpression, BinaryExpression, UnaryExpression, StringLiteralExpression
     */
    class Expression : public AbstractNode {
    public:
        virtual TypeWrapper *type(llvm::LLVMContext *) = 0;

        virtual bool assignable() {
            return false;
        }
    };

    /*
     * Base class for all assignable expressions.
     * Children: VariableExpression, ArrayIndexingExpression
     */
    class AssignableExpression : public Expression {
    public:
        TypeWrapper *type(llvm::LLVMContext *) override = 0;

        bool assignable() override {
            return true;
        }
    };

    class FunctionNameExpression : public Expression {
    public:
        explicit FunctionNameExpression(std::variant<AST::FunctionPrototype *, std::vector<AST::FunctionPrototype *>> f)
                : func(std::move(f)) {};

        std::variant<AST::FunctionPrototype *, std::vector<AST::FunctionPrototype *>> func;

        INJECT_ACCEPT();

        TypeWrapper *type(llvm::LLVMContext *context) override {
            return nullptr;
        };
    };

    class MethodNameExpression : public Expression {
    public:
        MethodNameExpression(std::unique_ptr<Expression> base,
                             std::variant<FunctionPrototype *, std::vector<FunctionPrototype *>> name) : struct_(
                std::move(base)), internal_name(std::move(name)) {};

        std::unique_ptr<Expression> struct_;
        std::variant<AST::FunctionPrototype *, std::vector<AST::FunctionPrototype *>> internal_name;

        INJECT_ACCEPT();

        TypeWrapper *type(llvm::LLVMContext *context) override {
            return nullptr;
        };
    };

    /*
     * A floating point literal expression.
     */
    class FloatLiteralExpression : public Expression {
    public:
        explicit FloatLiteralExpression(double v) : val(v) {};

        double val;

        INJECT_ACCEPT();

        TypeWrapper *type(llvm::LLVMContext *context) override {
            return new TypeWrapper(llvm::Type::getDoubleTy(*context));
        };
    };

    /*
     * A numeric constant expression.
     */
    class IntegralLiteralExpression : public Expression {
    public:
        explicit IntegralLiteralExpression(int v);

        int val;

        INJECT_ACCEPT();

        TypeWrapper *type(llvm::LLVMContext *context) override {
            return new TypeWrapper(llvm::Type::getInt32Ty(*context));
        };
    };

    /*
     * A variable expression. It may be a local variable, a global variable, or a function argument.
     */
    class VariableExpression : public AssignableExpression {
    public:
        explicit VariableExpression(Symbols::SymbolTableEntry *var);

        Symbols::SymbolTableEntry *variable{};

        INJECT_ACCEPT();

        TypeWrapper *type(llvm::LLVMContext *context) override {
            return variable->type;
        };
    };

    /*
     * Field Access
     */
    class FieldAccessExpression : public AssignableExpression {
    public:
        FieldAccessExpression(std::unique_ptr<AST::Expression> s, FieldInfo f) : struct_(std::move(s)), field(f) {};

        std::unique_ptr<AST::Expression> struct_;
        FieldInfo field;

        INJECT_ACCEPT();

        TypeWrapper *type(llvm::LLVMContext *ctx) override {
            return field.type;
        }
    };

    /*
     * A unary expression with an operator and an operand.
     */
    class UnaryExpression : public Expression {
    public:
        UnaryExpression(Operator o, std::unique_ptr<Expression> op);

        Operator op;
        std::unique_ptr<Expression> operand;

        INJECT_ACCEPT();

        TypeWrapper *type(llvm::LLVMContext *context) override {
            auto optype = operand->type(context);

            if (op == Operator::AND) {
                return TypeWrapper::getPointerTo(optype);
            }

            if (op == Operator::MUL) {
                return optype->pointee_type;
            }

            if (optype->type->isIntegerTy()) {
                if (op == Operator::SUB) {
                    return optype;
                }
            } else if (optype->type->isFloatTy()) {
                return optype;
            }
            return nullptr;
        };
    };

    /*
     * A function call expression. It may or may not have arguments.
     */

    class FunctionCallExpression : public Expression {
    public:
        FunctionCallExpression(std::unique_ptr<AST::Expression> f, std::vector<std::unique_ptr<Expression>> a,
                               Symbols::FunctionTable *funcTab, llvm::LLVMContext *ctxt) : args(std::move(a)) {
            if (dynamic_cast<AST::Expression *>(f.get()) == nullptr) {
                throw std::runtime_error("FunctionCallExpression: callee is not an expression");
            } else if (dynamic_cast<AST::MethodNameExpression *>(f.get()) != nullptr) {
                auto *m = dynamic_cast<AST::MethodNameExpression *>(f.get());
                if (std::holds_alternative<std::vector<AST::FunctionPrototype *>>(m->internal_name)) {
                    std::vector<TypeWrapper *> types;
                    types.reserve(args.size() + 1);
                    types.push_back(TypeWrapper::getPointerTo(m->struct_->type(ctxt)));
                    for (auto &i: args) {
                        types.push_back(i->type(ctxt));
                    }

                    auto target_args = std::get<std::vector<AST::FunctionPrototype *>>(m->internal_name);
                    for (auto &potential_func: target_args) {
                        if (potential_func->args.size() == types.size()) {
                            bool match = true;
                            for (int i = 0; i < types.size(); i++) {
                                if (potential_func->args[i]->type->type != types[i]->type) {
                                    match = false;
                                    break;
                                }
                            }
                            if (match) {
                                callee = std::make_unique<AST::FunctionNameExpression>(potential_func);
                                break;
                            }
                        }
                    }
                } else {
                    callee = std::make_unique<AST::FunctionNameExpression>(m->internal_name);
                }
                std::unique_ptr<Expression> base = std::make_unique<UnaryExpression>(Operator::AND,
                                                                                     std::move(m->struct_));
                args.insert(args.begin(), std::move(base));
            } else {
                auto *func = dynamic_cast<AST::FunctionNameExpression *>(f.get());
                if (std::holds_alternative<std::vector<AST::FunctionPrototype *>>(func->func)) { // overloaded function
                    auto target_args = std::get<std::vector<AST::FunctionPrototype *>>(func->func);
                    for (auto &i: target_args) {
                        if (i->args.size() == args.size()) {
                            bool match = true;
                            for (int j = 0; j < i->args.size(); j++) {
                                if (i->args[j]->type->type != args[j]->type(ctxt)->type) {
                                    match = false;
                                    break;
                                }
                            }
                            if (match) {
                                callee = std::make_unique<AST::FunctionNameExpression>(i);
                                break;
                            }
                        }
                    }
                } else {
                    callee = std::move(std::unique_ptr<AST::FunctionNameExpression>(
                            dynamic_cast<AST::FunctionNameExpression *>(f.release())));
                }
            }
            return_type = std::get<AST::FunctionPrototype *>(callee->func)->return_type;
        };

        std::unique_ptr<AST::FunctionNameExpression> callee;
        std::vector<std::unique_ptr<Expression>> args;

        TypeWrapper *return_type;

        INJECT_ACCEPT();

        TypeWrapper *type(llvm::LLVMContext *context) override {
            return return_type;
        };
    };

    /*
     * An array indexing expression. It can have multiple indices.
     */
    class ArrayIndexingExpression : public AssignableExpression {
    public:
        ArrayIndexingExpression(std::unique_ptr<Expression> arr, std::vector<std::unique_ptr<Expression>> idx);

        std::unique_ptr<Expression> array;
        std::vector<std::unique_ptr<Expression>> index;

        INJECT_ACCEPT();

        TypeWrapper *type(llvm::LLVMContext *context) override {
            return array->type(context)->pointee_type;
        };
    };

    /*
     * An expression with a left-hand side, an operator, and a right-hand side.
     */
    class BinaryExpression : public Expression {
    public:
        BinaryExpression(std::unique_ptr<Expression> lhs, Operator o, std::unique_ptr<Expression> rhs);

        std::unique_ptr<Expression> LHS;
        Operator op;
        std::unique_ptr<Expression> RHS;

        INJECT_ACCEPT();

        TypeWrapper *type(llvm::LLVMContext *context) override {
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

            if (ltype->type->isIntegerTy()) {
                if (op == Operator::ADD || op == Operator::SUB || op == Operator::MUL || op == Operator::FLR ||
                    op == Operator::MOD) {
                    if (rtype->type->isIntegerTy()) {
                        return rtype->type->getIntegerBitWidth() > ltype->type->getIntegerBitWidth() ? rtype : ltype;
                    } else if (rtype->type->isFloatTy()) {
                        return rtype;
                    }
                } else if (op == Operator::DIV) {
                    return new TypeWrapper(llvm::Type::getFloatTy(*context));
                }
            }

            if (ltype->type->isFloatTy() || rtype->type->isFloatTy()) {
                return ltype;
            }

            if (op == Operator::EQ || op == Operator::NEQ || op == Operator::LE || op == Operator::GE ||
                op == Operator::LEQ || op == Operator::GEQ) {
                return new TypeWrapper(llvm::Type::getInt1Ty(*context));
            }

            if (ltype->type->isPointerTy() || rtype->type->isPointerTy()) {
                return ltype;
            }

            return nullptr;
        }
    };


    /*
     * A string literal expression. Represents a char*.
     */
    class StringLiteralExpression : public Expression {
    public:
        explicit StringLiteralExpression(std::string s) : val(std::move(s)) {};

        std::string val;

        INJECT_ACCEPT();

        TypeWrapper *type(llvm::LLVMContext *context) override {
            return new TypeWrapper(llvm::PointerType::get(*context, 0),
                                   new TypeWrapper(llvm::Type::getInt8Ty(*context)));
        };
    };
}

#endif //ZUNG_AST_H
