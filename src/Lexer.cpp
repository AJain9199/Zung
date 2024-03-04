#include <iostream>
#include "Lexer.h"

Lexer::Lexer(const std::string &filename) : filestream_(filename) {}

TokenType Lexer::getToken() {
    next();
    while (isspace(current_char_)) {
        next();
    }
    operator_ = (Operator) (0);
    char prev;

    switch (current_char_) {
        case '{':
        case '}':
        case '[':
        case ']':
        case '(':
        case ')':
        case ',':
        case '.':
        case ';':
        case ':':
            char_value_ = current_char_;
            return PUNCTUATION;

        case '=':
            next();
            switch (current_char_) {
                case '=':
                    operator_ = EQ_EQ;
                    return OP;
                default:
                    rewind();
                    operator_ = EQ;
                    return OP;
            }

        case '>':
            next();
            switch (current_char_) {
                case '|':
                    operator_ = (Operator) '>';
                case '=':
                    operator_ = GEQ;
                    return OP;
                default:
                    rewind();
                    operator_ = GE;
                    return OP;
            }
        case '<':
            next();
            switch (current_char_) {
                case '=':
                    operator_ = LEQ;
                    return OP;
                default:
                    rewind();
                    operator_ = LE;
                    return OP;
            }
        case '|':
            next();
            switch (current_char_) {
                case '<':
                    operator_ = (Operator) '|';
                case '|':
                    operator_ = LOGICAL_OR;
                    return OP;
                default:
                    rewind();
            }
        case '*':
        case '/':
            prev = current_char_;
            next();
            if (current_char_ == prev) {
                operator_ = (Operator) current_char_;
            } else {
                rewind();
            }
        case '%':
        case '^':
        case '!':
        case '+':
        case '-':
            operator_ = (Operator) (operator_ + current_char_);
            return OP;

        case '\n':
        case '\r':
        case '\t':
        case ' ':
            next();
            break;
        case EOF:
            return END;
    }

    if (isalpha(current_char_) || current_char_ == '_') {
        identifier_ = current_char_;
        next();
        while (isalnum(current_char_) || current_char_ == '_') {
            identifier_ += current_char_;
            next();
        }
        rewind();

        if (identifier_ == "fn") {
            keyword_ = FN;
        } else if (identifier_ == "struct") {
            keyword_ = STRUCT;
        } else if (identifier_ == "bitstruct") {
            keyword_ = BITSTRUCT;
        } else if (identifier_ == "int") {
            default_type_ = INT;
            return DEFAULT_TYPE;
        } else if (identifier_ == "for") {
            keyword_ = FOR;
        } else if (identifier_ == "const") {
            declaration_specifier_ = CONST;
            return DECLARATION_SPECIFIER;
        } else if (identifier_ == "return") {
            keyword_ = RETURN;
        } else if (identifier_ == "char") {
            default_type_ = CHAR;
            return DEFAULT_TYPE;
        } else if (identifier_ == "var") {
            keyword_ = VAR;
            return KEYWORD;
        } else if (identifier_ == "extern") {
            keyword_ = EXTERN;
            return KEYWORD;
        } else if (identifier_ == "if") {
            keyword_ = IF;
        } else if(identifier_ == "class") {
            keyword_ = CLASS;
        } else if(identifier_ == "packed") {
            keyword_ = PACKED;
        }else {
            return IDENTIFIER;
        }

        return KEYWORD;
    } else if (isdigit(current_char_)) {
        identifier_ = current_char_;
        next();
        while (isdigit(current_char_)) {
            identifier_ += current_char_;
            next();
        }
        rewind();

        int_val_ = (int)strtol(identifier_.c_str(), nullptr, 10);
        return NUMERIC_LITERAL;
    }

    return static_cast<TokenType>(0);
}

void Lexer::next() {
    if (!filestream_.get(current_char_)) {
        current_char_ = EOF;
    }
}

void Lexer::rewind(int pos) {
    filestream_.seekg(-pos, std::ios::cur);
}

void Lexer::rewind() {
    rewind(2);
    next();
}

enum Keyword Lexer::keyword() const {
    return keyword_;
}

std::string Lexer::identifier() const {
    return identifier_;
}

enum TokenType Lexer::get() {
    return current_token_;
}

void Lexer::advance() {
    current_token_ = getToken();
}

bool Lexer::hasMoreTokens() {
    return current_token_ != END;
}

char Lexer::character() const {
    return char_value_;
}

enum DefaultType Lexer::default_type() const {
    return default_type_;
}

enum Operator Lexer::operator_token() const {
    return operator_;
}

int Lexer::numeric_literal() const {
    return int_val_;
}

enum DeclarationSpecifier Lexer::declaration_specifier() const {
    return declaration_specifier_;
}
