#ifndef ZUNG_PARSER_H
#define ZUNG_PARSER_H

#include <lexer.h>
#include <AST.h>

class ParsingEngine {
public:
    explicit ParsingEngine(const std::string& filename) : lexer(filename) {
        lexer.advance();
    }

    /* parsing methods for constructing the AST */
    AST::TranslationUnit *parseTranslationUnit();
private:
    Lexer lexer;

    std::unique_ptr<AST::Function> parseFunction();
    std::unique_ptr<AST::CompoundStatement> parseCompoundStatement();
    std::vector<SymbolTableEntry *> parseArgList();


    Type parseType();

    /* Parse expressions */
    std::unique_ptr<AST::Expression> parseNumericLiteralExpression();
    std::unique_ptr<AST::Expression> parseIdentifierExpression();
    std::unique_ptr<AST::Expression> parseParenthesizedExpression();
    std::unique_ptr<AST::Expression> parseUnaryExpression();
    std::unique_ptr<AST::Expression> parsePrimaryExpression();
    std::unique_ptr<AST::Expression> parseBinaryExpression(unsigned int min_precedence, std::unique_ptr<AST::Expression> LHS);
    std::unique_ptr<AST::Expression> parseExpression();

    /* Parsing statements */
    std::unique_ptr<AST::Statement> parse_statement();
    std::unique_ptr<AST::Statement> parse_expression_statement();
    std::unique_ptr<AST::Statement> parse_if_statement();
    std::unique_ptr<AST::Statement> parse_declaration_statement();


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

    symbol_table symTab_;

    static std::map<enum Operator, int> precedence;
    static inline int getTokenPrecedence(enum Operator op) {
        int prec = precedence[op];
        if (prec <= 0) {
            return -1;
        }
        return prec;
    }
};

#endif //ZUNG_PARSER_H
