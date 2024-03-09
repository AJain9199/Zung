#include <ParsingEngine.h>
#include <AST.h>
#include "llvm/IR/Type.h"
#include "llvm/IR/DerivedTypes.h"
#include <iostream>
#include <regex>

std::map<enum Operator, int> ParsingEngine::precedence = {
        {EQ,          1},
        {LOGICAL_OR,  2},
        {LOGICAL_AND, 3},
        {OR,          4},
        {XOR,         5},
        {AND,         6},
        {EQ_EQ,       7},
        {NEQ,         7},
        {GE,          8},
        {GEQ,         8},
        {LE,          8},
        {LEQ,         8},
        {LSH,         9},
        {RSH,         9},
        {ADD,         10},
        {SUB,         10},
        {MUL,         11},
        {DIV,         11},
        {MOD,         11},
        {EXP,         12},
        {FLR,         12}
};

AST::TranslationUnit *ParsingEngine::parseTranslationUnit() {
    auto *translation_unit = new AST::TranslationUnit();
    while (lexer.hasMoreTokens()) {
        if (is(FN)) {
            auto f = parseFunction();
            translation_unit->functions.push_back(std::move(f));
        } else if (is(EXTERN)) {
            auto f = parseExtern();
            translation_unit->prototypes.push_back(std::move(f));
        } else if (is(STRUCT)) {
            parseStruct();
        } else {
            std::cerr << "Unexpected token" << std::endl;
        }
    }
    translation_unit->type_table = *type_table;
    return translation_unit;
}

std::unique_ptr<AST::ExternFunction> ParsingEngine::parseExtern() {
    eat(EXTERN);
    std::string id = eat_identifier();
    std::vector<llvm::Type *> args = {};
    bool va = false;

    eat('(');
    while (!is(')')) {
        if (is(ELLIPSIS)) {
            eat(ELLIPSIS);
            va = true;
            break;
        }

        llvm::Type *type = parseType();
        std::string name = eat_identifier();

        args.push_back(type);

        if (!is(',')) {
            break;
        }
        eat(',');
    }
    eat(')');
    llvm::Type *return_type;
    if (is(':')) {
        lexer.advance();
        return_type = parseType();
    } else {
        return_type = llvm::Type::getVoidTy(*llvm_context_);
    }
    eat(';');
    return std::make_unique<AST::ExternFunction>(id, args, return_type, va);
}

/* Parses a function definition of the syntax:
 * fn (identifier_) (arg_list) (: type)? { compound_statement }
 * */
std::unique_ptr<AST::Function> ParsingEngine::parseFunction() {
    eat(FN);

    std::string id = eat_identifier();

    auto *sym = new Symbols::SymbolTable();
    symTab_ = sym;

    bool var_args = false;

    auto args = parseArgList(&var_args);

    llvm::Type *return_type;
    if (is(':')) {
        lexer.advance();
        return_type = parseType();
    } else {
        return_type = llvm::Type::getVoidTy(*llvm_context_);
    }

    if (is(';')) {
        eat(';');
        return std::make_unique<AST::Function>(
                std::make_unique<AST::FunctionPrototype>(id, args, return_type, var_args),
                nullptr);
    }

    auto body = parseCompoundStatement();

    auto func = std::make_unique<AST::Function>(
            std::make_unique<AST::FunctionPrototype>(id, args, return_type, var_args),
            std::move(body));
    func->symbol_table = sym;
    return func;
}

/* Parses an argument list for a function definition of the syntax:
 * (type identifier_ (,type identifier_)*)
 * */
std::vector<Symbols::SymbolTableEntry *> ParsingEngine::parseArgList(bool *is_var_args) {
    std::vector<Symbols::SymbolTableEntry *> args = {};

    eat('(');
    while (!is(')')) {
        if (is(ELLIPSIS)) {
            eat(ELLIPSIS);
            if (is_var_args != nullptr) {
                *is_var_args = true;
            }
            break;
        }

        llvm::Type *type = parseType();
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
    while (!(is('}') && lexer.get() == PUNCTUATION)) {
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

llvm::Type *ParsingEngine::parseType() {
    static std::regex type_regex("i(\\d+)");

    llvm::Type *basic_type;

    if (is(IDENTIFIER, DEFAULT_TYPE) == DEFAULT_TYPE) {
        lexer.advance();
        switch (lexer.default_type()) {
            case INT:
                basic_type = llvm::Type::getInt32Ty(*llvm_context_);
                break;
            case CHAR:
                basic_type = llvm::Type::getInt8Ty(*llvm_context_);
                break;
            case VOID:
                basic_type = llvm::Type::getVoidTy(*llvm_context_);
                break;
            case DOUBLE:
                basic_type = llvm::Type::getDoubleTy(*llvm_context_);
                break;
        }
    } else { // token is an identifier
        std::string id = eat_identifier();
        if (std::regex_match(id, type_regex)) {
            int width = std::stoi(id.substr(1));
            basic_type = llvm::Type::getIntNTy(*llvm_context_, width);
        } else {
            basic_type = (*type_table)[id].type;
        }
    }

    if (is(MUL)) {
        while (is(MUL)) {
            lexer.advance();
            basic_type = llvm::PointerType::get(basic_type, 0);
        }
    }

    return basic_type;
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

        eat(')');

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
        return std::make_unique<AST::IntegralLiteralExpression>(val);
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
    switch (is(NUMERIC_LITERAL, IDENTIFIER, PUNCTUATION, STR_LITERAL, FLOAT_LITERAL)) {
        case NUMERIC_LITERAL:
            return parseNumericLiteralExpression();
        case STR_LITERAL:
            return parseStringLiteralExpression();
        case IDENTIFIER:
            return parseIdentifierExpression();
        case PUNCTUATION:
            return parseParenthesizedExpression();
        case FLOAT_LITERAL:
            return parseFloatLiteralExpression();
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
    llvm::Type *type = parseType();

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
        } else if (lexer.keyword() == IF) {
            return parse_if_statement();
        } else if (lexer.keyword() == RETURN) {
            return parseReturnStatement();
        } else if (lexer.keyword() == FOR) {
            return parseForStatement();
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

void ParsingEngine::parseStruct() {
    eat(STRUCT);
    std::string id = eat_identifier();
    auto *t = llvm::StructType::create(*llvm_context_, id);
    (*type_table)[id] = (struct TypeInfo) {t, {}};
    bool packed = false;

    if (is(PACKED)) {
        eat(PACKED);
        packed = true;
    }

    eat('{');
    std::vector<llvm::Type *> members;
    int i = 0;
    while (!is('}')) {
        llvm::Type *type = parseType();
        std::string name = eat_identifier();
        (*type_table)[id].fields[name] = i++;
        members.push_back(type);

        if (!is(';')) {
            break;
        }
        eat(';');
    }
    eat('}');
    t->setBody(members, packed);
}

std::unique_ptr<AST::Expression> ParsingEngine::parseStringLiteralExpression() {
    if (is(STR_LITERAL)) {
        std::string str = lexer.identifier();
        lexer.advance();
        return std::make_unique<AST::StringLiteralExpression>(str);
    } else {
        std::cerr << "Unexpected token" << std::endl;
        return nullptr;
    }
}

std::unique_ptr<AST::Expression> ParsingEngine::parseFloatLiteralExpression() {
    if (is(FLOAT_LITERAL)) {
        double val = lexer.float_literal();
        lexer.advance();
        return std::make_unique<AST::FloatLiteralExpression>(val);
    } else {
        std::cerr << "Unexpected token" << std::endl;
        return nullptr;
    }
}

std::unique_ptr<AST::Statement> ParsingEngine::parseReturnStatement() {
    eat(RETURN);
    std::unique_ptr<AST::Expression> expr = nullptr;
    if (!(lexer.get() == PUNCTUATION && is(';'))) {
        expr = parseExpression();
        eat(';');
        return std::make_unique<AST::ReturnStatement>(std::move(expr));
    } else {
        eat(';');
        return std::make_unique<AST::ReturnStatement>(nullptr);
    }
}

std::unique_ptr<AST::Statement> ParsingEngine::parseForStatement() {
    eat(FOR);

    eat('(');
    auto init = parse_statement();
    auto cond = parseExpression();
    eat(';');
    auto update = parseExpression();

    eat(')');
    auto body = parseCompoundStatement();

    return std::make_unique<AST::ForStatement>(std::move(init), std::move(cond), std::move(update), std::move(body));
}
