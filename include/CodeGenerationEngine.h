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
#include <llvm/IR/PassManager.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Passes/StandardInstrumentations.h>
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/Transforms/Scalar/Reassociate.h"
#include "llvm/Transforms/Scalar/SimplifyCFG.h"
#include <AST.h>
#include <stack>
#include <any>
#include <utility>
#include <filesystem>

/*
 * macro to define the return type of the visitor subroutines.
 * It only defines the return type, which would be put onto the internal stack. The "actual" return type for the vitisor
 * methods are all void.
 * */
#define RETURNS(x) void

typedef std::map<std::string, AST::FunctionPrototype *> func_table_t;

class CodeGenerationEngine;

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

    /* Optimization passes */
    std::unique_ptr<llvm::FunctionPassManager> fpm_;
    std::unique_ptr<llvm::LoopAnalysisManager> lam_;
    std::unique_ptr<llvm::FunctionAnalysisManager> fam_;
    std::unique_ptr<llvm::CGSCCAnalysisManager> cgam_;
    std::unique_ptr<llvm::ModuleAnalysisManager> mam_;
    std::unique_ptr<llvm::PassInstrumentationCallbacks> pic_;
    std::unique_ptr<llvm::StandardInstrumentations> si_;

    llvm::PassBuilder pb_;

    llvm::ModulePassManager mpm_;

    std::map<Symbols::SymbolTableEntry *, llvm::GlobalVariable *> global_symbol_table_;
    std::map<Symbols::SymbolTableEntry *, llvm::AllocaInst *> symbol_table_;
    func_table_t function_table_;

    void get_llvm_function(const std::string &name);

    AST::Expression *lhs;

    static llvm::AllocaInst *
    create_entry_block_alloca(llvm::Function *func, const std::string &var_name, llvm::Type *type);

    class LValueCodeGenerationEngine : public AST::ASTVisitor {
    private:
        CodeGenerationEngine *engine_;
    public:

        explicit LValueCodeGenerationEngine(CodeGenerationEngine *engine) : engine_(engine) {}

        void visit(const AST::VariableExpression &) override;

        void visit(const AST::FieldAccessExpression &) override;

        void visit(const AST::AbstractNode &) override {};

        void visit(const AST::TranslationUnit &) override {};

        void visit(const AST::Function &) override {};

        void visit(const AST::FunctionPrototype &) override {};

        void visit(const AST::ExternFunction &) override {};

        void visit(const AST::CompoundStatement &) override {};

        void visit(const AST::ReturnStatement &) override {};

        void visit(const AST::ForStatement &) override {};

        void visit(const AST::DeclarationStatement &) override {};

        void visit(const AST::ExpressionStatement &) override {};

        void visit(const AST::IfStatement &) override {};

        void visit(const AST::ArrayIndexingExpression &) override;

        void visit(const AST::FunctionCallExpression &) override {};

        void visit(const AST::BinaryExpression &) override {};

        void visit(const AST::UnaryExpression &) override;

        void visit(const AST::IntegralLiteralExpression &) override {};

        void visit(const AST::StringLiteralExpression &) override {};

        void visit(const AST::FloatLiteralExpression &) override {};

        void visit(const AST::AggregateLiteralExpression &) override {};

        void visit(const AST::WhileStatement &) override {};

        void visit(const AST::BooleanLiteralExpression &) override {};

        void visit(const AST::NullLiteralExpression &) override {};
    };

    LValueCodeGenerationEngine *rvalue_engine_;

    bool no_red_zone = false;


public:
    explicit CodeGenerationEngine(std::unique_ptr<llvm::LLVMContext> context, char *cg_options) : llvm_context_(std::move(context)),
                                                                                builder_(
                                                                                        std::make_unique<llvm::IRBuilder<>>(
                                                                                                *llvm_context_)),
                                                                                module_(std::make_unique<llvm::Module>(
                                                                                        "main", *llvm_context_)),
                                                                                rvalue_engine_(
                                                                                        new LValueCodeGenerationEngine(
                                                                                                this)) {
        if (cg_options != nullptr) {
            std::string cg_opts(cg_options);
            if (cg_opts.find("no-red-zone") != std::string::npos) {
                no_red_zone = true;
            }
        }

        fpm_ = std::make_unique<llvm::FunctionPassManager>();
        lam_ = std::make_unique<llvm::LoopAnalysisManager>();
        fam_ = std::make_unique<llvm::FunctionAnalysisManager>();
        cgam_ = std::make_unique<llvm::CGSCCAnalysisManager>();
        mam_ = std::make_unique<llvm::ModuleAnalysisManager>();
        pic_ = std::make_unique<llvm::PassInstrumentationCallbacks>();
        si_ = std::make_unique<llvm::StandardInstrumentations>(*llvm_context_, false);

        fpm_->addPass(llvm::InstCombinePass());
        fpm_->addPass(llvm::ReassociatePass());
        fpm_->addPass(llvm::GVNPass());
        fpm_->addPass(llvm::SimplifyCFGPass());

        pb_.registerModuleAnalyses(*mam_);
        pb_.registerFunctionAnalyses(*fam_);
        pb_.crossRegisterProxies(*lam_, *fam_, *cgam_, *mam_);

        mpm_ = pb_.buildPerModuleDefaultPipeline(llvm::OptimizationLevel::O3);
    }

    void visit(const AST::AbstractNode &) override {};

    void visit(const AST::TranslationUnit &) override;

    RETURNS(llvm::Function *) visit(const AST::Function &) override;

    RETURNS(llvm::Function *) visit(const AST::FunctionPrototype &) override;

    void visit(const AST::ExternFunction &) override;

    void visit(const AST::CompoundStatement &) override;

    void visit(const AST::ReturnStatement &) override;

    void visit(const AST::ForStatement &) override;

    void visit(const AST::WhileStatement &) override;

    void visit(const AST::DeclarationStatement &) override;

    void visit(const AST::ExpressionStatement &) override;

    void visit(const AST::IfStatement &) override;

    void visit(const AST::ArrayIndexingExpression &) override;

    RETURNS(Value *) visit(const AST::FunctionCallExpression &) override;

    RETURNS(Value *) visit(const AST::VariableExpression &) override;

    RETURNS(Value *) visit(const AST::BinaryExpression &) override;

    RETURNS(Value *) visit(const AST::UnaryExpression &) override;

    RETURNS(llvm::ConstantInt *) visit(const AST::IntegralLiteralExpression &) override;

    RETURNS(llvm::Constant *) visit(const AST::StringLiteralExpression &) override;

    RETURNS(llvm::ConstantFP *) visit(const AST::FloatLiteralExpression &) override;

    RETURNS(llvm::Value *) visit(const AST::AggregateLiteralExpression &) override;

    void visit(const AST::FieldAccessExpression &) override;

    void visit(const AST::BooleanLiteralExpression &) override;

    void visit(const AST::NullLiteralExpression &) override;

    void writeCode(std::filesystem::path &filename, const std::string &target_triple, int filetype);

    /* The following methods are used to manage the internal stack of the code generation engine. */
    /*
     * "returns" a value to the stack, by pushing onto it.
     * @param val: the value to be pushed onto the stack
     */
    inline void stack_return(void *val) {
        stack_.push(val);
    }

    /*
     * "pops" a value from the stack, by returning the top value and removing it from the stack.
     * Should only be used with the STACK_GET macro
     *
     * @return: the top value from the stack
     */
    template<typename T>
    inline T stack_get() {
        auto val = (T) (stack_.top());
        stack_.pop();
        return val;
    }

    ~CodeGenerationEngine() {
        delete rvalue_engine_;
    }
};

#endif //ZUNG_CODEGENERATIONENGINE_H
