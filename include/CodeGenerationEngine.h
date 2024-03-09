#ifndef ZUNG_CODEGENERATIONENGINE_H
#define ZUNG_CODEGENERATIONENGINE_H

#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include <AST.h>
#include <stack>
#include <any>

/*
 * macro to define the return type of the visitor subroutines.
 * It only defines the return type, which would be put onto the internal stack. The "actual" return type for the vitisor
 * methods are all void.
 * */
#define RETURNS(x) void

typedef std::map<std::string, std::unique_ptr<AST::FunctionPrototype>> func_table_t;

/*
 * The code generation engine is responsible for generating LLVM IR from the AST. It uses the visitor pattern to traverse
 * the Abstract Syntax Tree, and generates the corresponding LLVM IR for each node.
 *
 * The engine uses an internal stack to store the results of the code generation process. This is necessary because the
 * visitor does not have return types for the visitor subroutines.
 */
class CodeGenerationEngine : public AST::ASTVisitor {
private:
    std::stack<void *> stack_; /* TODO: Use a variant instead later in development */
    std::unique_ptr<llvm::LLVMContext> llvm_context_;
    std::unique_ptr<llvm::IRBuilder<>> builder_;
    std::unique_ptr<llvm::Module> module_;

    std::map<Symbols::SymbolTableEntry *, llvm::AllocaInst *> symbol_table_;
    func_table_t function_table_;

    void get_llvm_function(const std::string &name);
    static llvm::AllocaInst *create_entry_block_alloca(llvm::Function *func, const std::string &var_name, llvm::Type *type);

public:
    explicit CodeGenerationEngine(std::unique_ptr<llvm::LLVMContext> context) : llvm_context_(std::move(context)),
                             builder_(std::make_unique<llvm::IRBuilder<>>(*llvm_context_)),
                             module_(std::make_unique<llvm::Module>("main", *llvm_context_)) {}

    void visit(const AST::AbstractNode &) override {};

    void visit(const AST::TranslationUnit &) override;

    RETURNS(llvm::Function *) visit(const AST::Function &) override;

    RETURNS(llvm::Function *) visit(const AST::FunctionPrototype &) override;

    void visit(const AST::ExternFunction &) override;

    void visit(const AST::CompoundStatement &) override;

    void visit(const AST::DeclarationStatement &) override;

    void visit(const AST::ExpressionStatement &) override;

    void visit(const AST::IfStatement &) override {};

    void visit(const AST::ArrayIndexingExpression &) override {};

    RETURNS(Value *) visit(const AST::FunctionCallExpression &) override;

    RETURNS(llvm::AllocaInst*) visit(const AST::VariableExpression &) override;

    RETURNS(Value *) visit(const AST::BinaryExpression &) override;

    RETURNS(Value *) visit(const AST::UnaryExpression &) override;

    RETURNS(llvm::ConstantInt *) visit(const AST::NumericConstantExpression &) override;

    RETURNS(llvm::Constant *) visit(const AST::StringLiteralExpression &) override;

    RETURNS(llvm::ConstantFP *) visit(const AST::FloatLiteralExpression &) override;

    /* The following methods are used to manage the internal stack of the code generation engine. */
    /*
     * "returns" a value to the stack, by pushing onto it.
     * @param val: the value to be pushed onto the stack
     */
    inline void stack_return(void * val) {
        stack_.push(val);
    }

    /*
     * "pops" a value from the stack, by returning the top value and removing it from the stack.
     * Should only be used with the STACK_GET macro
     *
     * @return: the top value from the stack
     */
    inline void *stack_get() {
        auto val = stack_.top();
        stack_.pop();
        return val;
    }
};

#endif //ZUNG_CODEGENERATIONENGINE_H
