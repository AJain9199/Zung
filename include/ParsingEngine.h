#ifndef ZUNG_PARSER_H
#define ZUNG_PARSER_H

#include <Lexer.h>
#include <AST.h>
#include <llvm/IR/Type.h>
#include "llvm/IR/LLVMContext.h"
#include <Types.h>

/*
 * The parsing engine is responsible for parsing the input file and constructing the Abstract Syntax Tree (AST) from the
 * input. It uses the lexer to get tokens from the input file.
 *
 * All AST nodes are constructed by the parseXXX methods.
 */
class ParsingEngine {
public:
    explicit ParsingEngine(const std::string& filename, std::unique_ptr<llvm::LLVMContext> context) : lexer(filename), llvm_context_(std::move(context)), type_table(nullptr) {
        type_table = new std::map<std::string, struct TypeInfo *>();
        funcTab_ = new Symbols::FunctionTable();
        lexer.advance();
    }

    /* parsing methods for constructing the AST */
    AST::TranslationUnit *parseTranslationUnit();

    void get_context(std::unique_ptr<llvm::LLVMContext> &context) {
        context = std::move(llvm_context_);
    }

    ~ParsingEngine() {
        for (auto &i: *type_table) {
            delete i.second;
        }

        delete type_table;
        delete funcTab_;
    }
private:
    Lexer lexer;
    llvm::StructType *currentStruct{nullptr};

    std::map<std::basic_string<char>, TypeInfo *> *type_table;

    std::unique_ptr<AST::Function> parseFunction();
    std::unique_ptr<AST::CompoundStatement> parseCompoundStatement();
    std::vector<Symbols::SymbolTableEntry *> parseArgList(bool *is_var_args=nullptr);
    std::unique_ptr<AST::ExternFunction> parseExtern();
    std::unique_ptr<AST::Function> parseMethod();
    std::vector<std::unique_ptr<AST::Function>> parseStruct();


    TypeWrapper * parseType();

    /* Parse expressions */
    std::unique_ptr<AST::Expression> parseNumericLiteralExpression();
    std::unique_ptr<AST::Expression> parsePostfix(std::unique_ptr<AST::Expression> LHS);
    std::unique_ptr<AST::Expression> parseIdentifierExpression();
    std::unique_ptr<AST::Expression> parseParenthesizedExpression();
    std::unique_ptr<AST::Expression> parseUnaryExpression();
    std::unique_ptr<AST::Expression> parsePrimaryExpression();
    std::unique_ptr<AST::Expression> parseBinaryExpression(unsigned int min_precedence, std::unique_ptr<AST::Expression> LHS);
    std::unique_ptr<AST::Expression> parseStringLiteralExpression();
    std::unique_ptr<AST::Expression> parseExpression();
    std::unique_ptr<AST::Expression> parseFloatLiteralExpression();

    /* Parsing statements */
    std::unique_ptr<AST::Statement> parseStatement();
    std::unique_ptr<AST::Statement> parseReturnStatement();
    std::unique_ptr<AST::Statement> parseForStatement();
    std::unique_ptr<AST::Statement> parseExpressionStatement();
    std::unique_ptr<AST::Statement> parseIfStatement();
    std::unique_ptr<AST::Statement> parseDeclarationStatement();


    void eat(TokenType t);
    void eat(enum Keyword K);
    void eat(char C);
    void eat(enum Operator op);

    template<typename ...Tokens>
    auto is(Tokens... tokens);

    std::string eat_identifier();

    bool is(char C);
    bool is(enum Keyword K);
    bool is(TokenType T);
    bool is(enum Operator op);

    Symbols::SymbolTable *symTab_{};
    Symbols::FunctionTable *funcTab_{};

    static std::map<enum Operator, int> precedence;
    static inline int getTokenPrecedence(enum Operator op) {
        int prec = precedence[op];
        if (prec <= 0) {
            return -1;
        }
        return prec;
    }

    std::unique_ptr<llvm::LLVMContext> llvm_context_;
};

#endif //ZUNG_PARSER_H
