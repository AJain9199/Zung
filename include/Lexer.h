#ifndef ZUNG_LEXER_H
#define ZUNG_LEXER_H

#include <fstream>
#include <cstdarg>

enum TokenType {
    KEYWORD,
    DEFAULT_TYPE,
    DECLARATION_SPECIFIER,

    IDENTIFIER, // internal_name not beginning with a digit

    /* literals */
    NUMERIC_LITERAL, // [0-9]+
    STR_LITERAL, // ".+"
    BOOLEAN_LITERAL, // true, false
    FLOAT_LITERAL,
    NULL_LITERAL,

    /* operators */
    OP,

    ELLIPSIS,

    PUNCTUATION,

    END
};

enum Operator {
    ADD = '+', // 43
    SUB = '-', // 45
    MUL = '*', // 42
    DIV = '/', // 47
    EXP = '*' + '*', // 84
    FLR = '/' + '/', // 94
    MOD = '%', // 37
    NOT = '!', // 33
    AND = '&', // 38
    OR = '|', // 124
    EQ = '=', // 61
    EQ_EQ = '=' * 2, // 122
    XOR = '^', // 94
    LOGICAL_AND = '&' + '&', // 76
    LOGICAL_OR = '|' + '|', // 248
    RSH = '>' + '|', // 186
    LSH = '|' + '<', // 184

    GE = '>',
    GEQ = '>' + '=', // 123
    LE = '<',
    LEQ = '<' + '=', // 121
    NEQ = '!' + '=' + 1, // 33 + 61
    PTR = '-' + '>'
};
// TODO: Token detection overhaul

enum Keyword {
    FN,
    STRUCT,
    BITSTRUCT,
    FOR,
    RETURN,
    IF,
    ELIF,
    ELSE,
    VAR,
    EXTERN,
    CLASS,
    PACKED,
    WHILE,
    IMPORT
};

enum DeclarationSpecifier {
    STATIC,
    CONST
};

enum DefaultType {
    INT,
    CHAR,
    VOID,
    DOUBLE,
    BOOL
};

class Lexer {
public:
    explicit Lexer(const std::string &filename);

    void advance();

    bool hasMoreTokens();

    TokenType get();

    /* Getters for accessing token values */
    [[nodiscard]] Keyword keyword() const;

    [[nodiscard]] std::string identifier() const;

    [[nodiscard]] Operator operator_token() const;

    [[nodiscard]] char character() const;

    [[nodiscard]] int numeric_literal() const;

    [[nodiscard]] DeclarationSpecifier declaration_specifier() const;

    [[nodiscard]] DefaultType default_type() const;

    [[nodiscard]] bool boolean_literal() const;

    [[nodiscard]] double float_literal() const;

private:
    TokenType getToken();

    void next();

    void rewind(int pos);

    void rewind();

    std::ifstream filestream_;

    char current_char_ = 0;
    char char_value_ = 0;
    bool boolean_val_ = false;
    std::string identifier_{};
    Operator operator_{};
    int int_val_ = 0;
    double float_val_ = 0;
    Keyword keyword_{};
    DeclarationSpecifier declaration_specifier_{};
    DefaultType default_type_{};
    TokenType current_token_{};
};

#endif //ZUNG_LEXER_H
