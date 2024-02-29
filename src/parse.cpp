#include <parse.h>
#include <iostream>
#include "AST.h"

std::map<enum Operator, int> ParsingEngine::precedence = {
        {EQ, 1},
        {LOGICAL_OR, 2},
        {LOGICAL_AND, 3},
        {OR, 4},
        {XOR, 5},
        {AND, 6},
        {EQ_EQ, 7}, {NEQ, 7},
        {GE, 8}, {GEQ, 8}, {LE, 8}, {LEQ, 8},
        {LSH, 9}, {RSH, 9},
        {ADD, 10}, {SUB, 10},
        {MUL, 11}, {DIV, 11}, {FLR, 11}, {MOD, 11},
        {EXP, 12}
};

AST::TranslationUnit *ParsingEngine::parseTranslationUnit() {
    auto *translation_unit = new AST::TranslationUnit();
    while (lexer.hasMoreTokens()) {
        if (is(FN)) {
            auto f = parseFunction();
            translation_unit->functions.push_back(std::move(f));
        }
    }
    return translation_unit;
}

/* Parses a function definition of the syntax:
 * fn (identifier_) (arg_list) (: type)? { compound_statement }
 * */
std::unique_ptr<AST::Function> ParsingEngine::parseFunction() {
    eat(FN);

    std::string id = eat_identifier();

    auto *sym = new Symbols::SymbolTable();
    symTab_ = sym;


    auto args = parseArgList();

    Symbols::Type return_type;
    if (is(':')) {
        lexer.advance();
        return_type = parseType();
    } else {
        return_type = Symbols::Type(VOID, {});
    }

    auto body = parseCompoundStatement();

    auto func = std::make_unique<AST::Function>(std::make_unique<AST::FunctionPrototype>(id, args, return_type),
                                           std::move(body));
    func->symbol_table = sym;
    return func;
}

/* Parses an argument list for a function definition of the syntax:
 * (type identifier_ (,type identifier_)*)
 * */
std::vector<Symbols::SymbolTableEntry *> ParsingEngine::parseArgList() {
    std::vector<Symbols::SymbolTableEntry *> args = {};

    eat('(');
    while (!is(')')) {
        Symbols::Type type = parseType();
        std::string name = eat_identifier();
        args.push_back(symTab_->define(type, name, Symbols::LOCAL));

        if (!is(',')) {
            break;
        }
        eat(',');
    }
    eat(')');
    return args;
}

/* Parses a compound statement of the syntax:
 * { (statement)* }
 * */
std::unique_ptr<AST::CompoundStatement> ParsingEngine::parseCompoundStatement() {
    std::vector<std::unique_ptr<AST::Statement>> statements;
    eat('{');
    while (!is('}')) {
        statements.push_back(parse_statement());
    }
    eat('}');
    return std::make_unique<AST::CompoundStatement>(std::move(statements));
}

void ParsingEngine::eat(TokenType t) {
    if (lexer.get() == t) {
        if (lexer.hasMoreTokens()) {
            lexer.advance();
        }
    } else {
        std::cerr << "Unexpected token" << std::endl;
    }
}

void ParsingEngine::eat(char C) {
    if (lexer.character() == C) {
        if (lexer.hasMoreTokens()) {
            lexer.advance();
        }
    } else {
        std::cerr << "Unexpected token" << std::endl;
    }
}

bool ParsingEngine::is(char C) {
    return lexer.character() == C;
}

bool ParsingEngine::is(enum Keyword K) {
    return lexer.get() == KEYWORD && lexer.keyword() == K;
}

template<typename... Tokens>
auto ParsingEngine::is(Tokens... tokens) {
    for (auto t: {tokens...}) {
        if (is(t)) {
            return t;
        }
    }

    std::cerr << "Unexpected token" << std::endl;
    return (TokenType) NULL;
}

Symbols::Type ParsingEngine::parseType() {
    if (is(IDENTIFIER, DEFAULT_TYPE) == DEFAULT_TYPE) {
        lexer.advance();
        return {lexer.default_type(), {}};
    } else {
        lexer.advance();
        return {lexer.identifier(), {}};
    }
}

void ParsingEngine::eat(enum Keyword K) {
    if (lexer.get() == KEYWORD && lexer.keyword() == K) {
        if (lexer.hasMoreTokens()) {
            lexer.advance();
        }
    } else {
        std::cerr << "Unexpected token" << std::endl;
    }
}

/* Parses identifier-based expressions of the syntax:
 * (Variable reference) identifier_
 * (Function Call) identifier_ ((args_list)?)
 * (Array Indexing) identifier_ ([expression (,expression)*])
 * */
std::unique_ptr<AST::Expression> ParsingEngine::parseIdentifierExpression() {
    std::string name = eat_identifier();
    if (lexer.get() == PUNCTUATION && lexer.character() == '(') {
        // parse function call or array access
        eat('(');
        std::vector<std::unique_ptr<AST::Expression>> args;
        while (!is(')')) {
            args.push_back(parseExpression());
            if (is(',')) {
                eat(',');
            }
        }

        return std::make_unique<AST::FunctionCallExpression>(name, std::move(args));
    } else if (lexer.character() == '[') {
        eat('[');

        std::vector<std::unique_ptr<AST::Expression>> indices;

        while (!is(']')) {
            indices.push_back(parseExpression());
            if (is(',')) {
                eat(',');
            }
        }

        return std::make_unique<AST::ArrayIndexingExpression>(
                std::make_unique<AST::VariableExpression>(symTab_->find(name)), std::move(indices));
    } else {
        return std::make_unique<AST::VariableExpression>(symTab_->find(name));
    }
}

std::string ParsingEngine::eat_identifier() {
    if (!is(IDENTIFIER)) {
        std::cerr << "Unexpected token" << std::endl;
    }

    std::string id = lexer.identifier();
    lexer.advance();
    return id;
}

bool ParsingEngine::is(TokenType T) {
    return lexer.get() == T;
}

/* Parses a numeric literal (as an expression) of the syntax:
 * [0-9]+
 * */
std::unique_ptr<AST::Expression> ParsingEngine::parseNumericLiteralExpression() {
    if (is(NUMERIC_LITERAL)) {
        int val = lexer.numeric_literal();
        lexer.advance();
        return std::make_unique<AST::NumericConstantExpression>(val);
    } else {
        std::cerr << "Unexpected token" << std::endl;
        return nullptr;
    }
}

/* Parses a parenthesized expression of the syntax:
 * (expression)
 * */
std::unique_ptr<AST::Expression> ParsingEngine::parseParenthesizedExpression() {
    eat('(');
    auto expr = parseExpression();
    eat(')');
    return expr;
}

/* Parses a unary expression of the syntax:
 * (unary_op) expression
 * primary_expression
 *
 * unary_op: -, !, ~
 * */
std::unique_ptr<AST::Expression> ParsingEngine::parseUnaryExpression() {
    if (lexer.get() == OP) {
        enum Operator op = lexer.operator_token();
        lexer.advance();

        auto operand = parseUnaryExpression();
        if (operand) {
            return std::make_unique<AST::UnaryExpression>(op, std::move(operand));
        }
        return nullptr;
    } else {
        return parsePrimaryExpression();
    }
}


/* Parses an expression of the syntax:
 * expression (binary_op expression)*
 * */
std::unique_ptr<AST::Expression> ParsingEngine::parseExpression() {
    auto LHS = parseUnaryExpression();
    if (!LHS) {
        return nullptr;
    }

    return parseBinaryExpression(0, std::move(LHS));
}


std::unique_ptr<AST::Expression>
ParsingEngine::parseBinaryExpression(unsigned int min_precedence, std::unique_ptr<AST::Expression> LHS) {
    while (true) {
        if (!is(OP)) {
            return LHS;
        }

        enum Operator op = lexer.operator_token();
        unsigned int op_precedence = getTokenPrecedence(op);

        if (op_precedence < min_precedence) {
            return LHS;
        }

        lexer.advance();

        auto RHS = parseUnaryExpression();
        if (!RHS) {
            return nullptr;
        }

        if (is(OP)) {
            enum Operator next_op = lexer.operator_token();
            unsigned int next_op_precedence = getTokenPrecedence(next_op);

            if (next_op_precedence > op_precedence) {
                RHS = parseBinaryExpression(op_precedence + 1, std::move(RHS));
                if (!RHS) {
                    return nullptr;
                }
            }
        }

        LHS = std::make_unique<AST::BinaryExpression>(std::move(LHS), op, std::move(RHS));
    }
}

/* Parses an expression statement of the syntax:
 * expression;
 * */
std::unique_ptr<AST::Statement> ParsingEngine::parse_expression_statement() {
    auto expr = parseExpression();
    eat(';');

    return std::make_unique<AST::ExpressionStatement>(std::move(expr));
}

/* Parses a primary expression of the syntax:
 * numeric_literal
 * identifier_expression
 * parenthesized_expression
 * */
std::unique_ptr<AST::Expression> ParsingEngine::parsePrimaryExpression() {
    switch (is(NUMERIC_LITERAL, IDENTIFIER, PUNCTUATION)) {
        case NUMERIC_LITERAL:
            return parseNumericLiteralExpression();
        case IDENTIFIER:
            return parseIdentifierExpression();
        case PUNCTUATION:
            return parseParenthesizedExpression();
        default:
            std::cerr << "Unexpected token";
            return nullptr;
    }
}

/* Parses a basic if statement of the syntax:
 * if (expression) compound_statement
 * */
std::unique_ptr<AST::Statement> ParsingEngine::parse_if_statement() {
    eat(IF);
    eat('(');
    auto condition = parseExpression();
    eat(')');
    auto then = parseCompoundStatement();

    return std::make_unique<AST::IfStatement>(std::move(condition), std::move(then));
}

/* Parses a declaration statement of the syntax:
 * var type identifier_ (= expression)? (, identifier_ (= expression)?)*;
 * */
std::unique_ptr<AST::Statement> ParsingEngine::parse_declaration_statement() {
    eat(VAR);

    std::map<Symbols::SymbolTableEntry *, std::unique_ptr<AST::Expression>> init_list;
    Symbols::Type type = parseType();

    while (true) {
        std::string name = eat_identifier();
        Symbols::SymbolTableEntry *entry = symTab_->define(type, name, Symbols::LOCAL);

        if (is(EQ)) {
            lexer.advance();
            auto expr = parseExpression();
            init_list[entry] = std::move(expr);
        }

        if (!is(',')) {
            break;
        }
        eat(',');
    }

    eat(';');
    return std::make_unique<AST::DeclarationStatement>(std::move(init_list));
}

std::unique_ptr<AST::Statement> ParsingEngine::parse_statement() {
    if (lexer.get() == KEYWORD) {
        if (lexer.keyword() == VAR) {
            return parse_declaration_statement();
        }
        if (lexer.keyword() == IF) {
            return parse_if_statement();
        } else {
            std::cerr << "Unexpected token" << std::endl;
            return nullptr;
        }
    }

    return parse_expression_statement();
}

void ParsingEngine::eat(enum Operator op) {
    if (lexer.get() == OP && lexer.operator_token() == op) {
        if (lexer.hasMoreTokens()) {
            lexer.advance();
        }
    } else {
        std::cerr << "Unexpected token" << std::endl;
    }
}

bool ParsingEngine::is(enum Operator op) {
    return lexer.get() == OP && lexer.operator_token() == op;
}
