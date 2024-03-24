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
    auto *global_sym = new Symbols::SymbolTable();
    globalSymTab_ = global_sym;
    auto *translation_unit = new AST::TranslationUnit();
    translation_unit->global_symbol_table = global_sym;

    while (lexer.hasMoreTokens()) {
        if (is(FN)) {
            auto f = parseFunction();
            translation_unit->functions.push_back(std::move(f));
        } else if (is(EXTERN)) {
            auto f = parseExtern();
            funcTab_->define(f->name, f.get());
            translation_unit->prototypes.push_back(std::move(f));
        } else if (is(STRUCT)) {
            auto methods = std::move(parseStruct());
            translation_unit->functions.reserve(
                    translation_unit->functions.size() + std::distance(methods.begin(), methods.end()));
            translation_unit->functions.insert(translation_unit->functions.end(),
                                               std::make_move_iterator(methods.begin()),
                                               std::make_move_iterator(methods.end()));
        } else if (is(VAR)) {
            auto s = parseDeclarationStatement(true);
            translation_unit->global_declarations.push_back(std::move(s));
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
    std::vector<TypeWrapper *> args = {};
    bool va = false;

    eat('(');
    while (!is(')')) {
        if (is(ELLIPSIS)) {
            eat(ELLIPSIS);
            va = true;
            break;
        }

        TypeWrapper *type = parseType();
        std::string name = eat_identifier();

        args.push_back(type);

        if (!is(',')) {
            break;
        }
        eat(',');
    }
    eat(')');
    TypeWrapper *return_type;
    if (is(':')) {
        lexer.advance();
        return_type = parseType();
    } else {
        return_type = new TypeWrapper(llvm::Type::getVoidTy(*llvm_context_));
    }
    eat(';');
    return std::make_unique<AST::ExternFunction>(id, args, return_type, va);
}

/* Parses a function definition of the syntax:
 * fn (identifier_) (arg_list) (: type)? { compound_statement }
 * */
std::unique_ptr<AST::Function>
ParsingEngine::parseFunction(const std::vector<Symbols::SymbolTableEntry *> &begin_args, std::string *original_name) {
    eat(FN);

    std::string id = eat_identifier();

    auto *sym = new Symbols::SymbolTable();
    symTab_ = sym;

    bool var_args = false;

    auto args = parseArgList(&var_args);

    if (!begin_args.empty()) {
        for (auto &i: begin_args) {
            args.insert(args.begin(), symTab_->define(i, Symbols::LOCAL));
        }
    }

    TypeWrapper *return_type;
    if (is(':')) {
        lexer.advance();
        return_type = parseType();
    } else {
        return_type = new TypeWrapper(llvm::Type::getVoidTy(*llvm_context_));
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
    if (original_name != nullptr) {
        *original_name = func->prototype->name;
    }

    if (funcTab_->exists(id)) {
        mangleFunctionName(func->prototype.get());
        funcTab_->define(id, func->prototype.get());
    } else {
        funcTab_->define(mangleFunctionName(func->prototype.get()), func->prototype.get());
    }

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

        TypeWrapper *type = parseType();
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
        statements.push_back(parseStatement());
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

TypeWrapper *ParsingEngine::parseType() {
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
            basic_type = (*type_table)[id]->type;
        }
    }

    auto wrapper = new TypeWrapper(basic_type);
    if (is(MUL)) {
        while (is(MUL)) {
            lexer.advance();
            wrapper = TypeWrapper::getPointerTo(wrapper);
        }
    }

    if (is('[')) {
        eat('[');
        while (!is(']')) {
            if (is(NUMERIC_LITERAL)) {
                wrapper = TypeWrapper::getArrayTo(wrapper, lexer.numeric_literal());
                lexer.advance();
            }

            if (!is(',')) {
                break;
            }
            eat(',');
        }
        eat(']');
    }

    return wrapper;
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
    auto s = symTab_->find(name);
    if (s == nullptr) {
        if (globalSymTab_->find(name) != nullptr) {
            return std::make_unique<AST::VariableExpression>(globalSymTab_->find(name));
        }

        if (currentStruct != nullptr) {
            auto typeinfo = (*type_table)[currentStruct->getStructName().str()]->fields;
            if (typeinfo.find(name) != typeinfo.end()) {
                return std::make_unique<AST::FieldAccessExpression>(std::make_unique<AST::VariableExpression>(
                                                                            symTab_->find(
                                                                                    "this")),
                                                                    typeinfo[name]);
            }
        }
        return std::make_unique<AST::FunctionNameExpression>(funcTab_->find(name));
    }

    return std::make_unique<AST::VariableExpression>(s);
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

std::unique_ptr<AST::Expression> ParsingEngine::parsePostfix(std::unique_ptr<AST::Expression> LHS) {
    while (true) {
        if (lexer.get() == PUNCTUATION) {
            switch (lexer.character()) {
                case '(': {
                    eat('(');
                    std::vector<std::unique_ptr<AST::Expression>> args;
                    while (!(is(')') && lexer.get() == PUNCTUATION)) {
                        args.push_back(parseExpression());
                        if (is(',')) {
                            eat(',');
                        }
                    }
                    eat(')');
                    LHS = std::make_unique<AST::FunctionCallExpression>(std::move(LHS), std::move(args), funcTab_,
                                                                        llvm_context_.get());
                }
                    break;
                case '[': {
                    std::vector<std::unique_ptr<AST::Expression>> indices;
                    eat('[');

                    while (!is(']')) {
                        indices.push_back(parseExpression());
                        if (is(',')) {
                            eat(',');
                        }
                    }
                    eat(']');

                    LHS = std::make_unique<AST::ArrayIndexingExpression>(std::move(LHS), std::move(indices));
                    break;
                }
                case '.': {
                    eat('.');
                    std::string field = eat_identifier();
                    auto ltype = LHS->type(llvm_context_.get())->type->getStructName();
                    auto type_info = (*type_table)[ltype.str()];
                    if (type_info->fields.find(field) != type_info->fields.end()) {
                        auto f = (*type_table)[ltype.str()]->fields[field];
                        LHS = std::make_unique<AST::FieldAccessExpression>(std::move(LHS), f);
                    } else {
                        LHS = std::make_unique<AST::MethodNameExpression>(std::move(LHS), type_info->methods[field]);
                    }

                    break;
                }
                default:
                    return LHS;
            }
        } else if (lexer.get() == OP && lexer.operator_token() == PTR) {
            lexer.advance();
            std::string field = eat_identifier();

            LHS = std::make_unique<AST::UnaryExpression>(MUL, std::move(LHS));

            auto ltype = LHS->type(llvm_context_.get())->type->getStructName();
            auto type_info = (*type_table)[ltype.str()];
            if (type_info->fields.find(field) != type_info->fields.end()) {
                auto f = (*type_table)[ltype.str()]->fields[field];
                LHS = std::make_unique<AST::FieldAccessExpression>(std::move(LHS), f);
            } else {
                LHS = std::make_unique<AST::MethodNameExpression>(std::move(LHS), type_info->methods[field]);
            }
        } else {
            return LHS;
        }
    }
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

        if (LHS->type(llvm_context_.get())->type->isStructTy() && op != EQ) {
            auto ltype = (*type_table)[LHS->type(llvm_context_.get())->type->getStructName().str()];
            std::string method_name;
            switch (op) {
                case ADD:
                    method_name = "__add__";
                    break;
                case SUB:
                    method_name = "__sub__";
                    break;
                case MUL:
                    method_name = "__mul__";
                    break;
                case DIV:
                    method_name = "__div__";
                    break;
                default:
                    std::cerr << "Unsupported operator" << std::endl;
                    return nullptr;
            }
            std::vector<std::unique_ptr<AST::Expression>> args(1);
            args[0] = std::move(RHS);
            auto m = std::make_unique<AST::MethodNameExpression>(std::move(LHS), ltype->methods[method_name]);
            LHS = std::make_unique<AST::FunctionCallExpression>(std::move(m), std::move(args), funcTab_,
                                                                llvm_context_.get());
        } else {
            LHS = std::make_unique<AST::BinaryExpression>(std::move(LHS), op, std::move(RHS));
        }
    }
}

/* Parses an expression statement of the syntax:
 * expression;
 * */
std::unique_ptr<AST::Statement> ParsingEngine::parseExpressionStatement() {
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
    std::unique_ptr<AST::Expression> expr = nullptr;
    switch (is(NUMERIC_LITERAL, IDENTIFIER, PUNCTUATION, STR_LITERAL, FLOAT_LITERAL)) {
        case NUMERIC_LITERAL:
            expr = parseNumericLiteralExpression();
            break;
        case STR_LITERAL:
            expr = parseStringLiteralExpression();
            break;
        case IDENTIFIER:
            expr = parseIdentifierExpression();
            break;
        case PUNCTUATION:
            expr = parseParenthesizedExpression();
            break;
        case FLOAT_LITERAL:
            expr = parseFloatLiteralExpression();
            break;
        default:
            std::cerr << "Unexpected token";
            return nullptr;
    }

    return parsePostfix(std::move(expr));
}

/* Parses a basic if statement of the syntax:
 * if (expression) compound_statement
 * */
std::unique_ptr<AST::Statement> ParsingEngine::parseIfStatement() {
    eat(IF);
    eat('(');
    auto condition = parseExpression();
    eat(')');
    auto then = parseStatement();

    if (is(ELSE)) {
        eat(ELSE);
        auto else_stmt = parseStatement();
        return std::make_unique<AST::IfStatement>(std::move(condition), std::move(then), std::move(else_stmt));
    } else {
        return std::make_unique<AST::IfStatement>(std::move(condition), std::move(then), nullptr);
    }
}

/* Parses a declaration statement of the syntax:
 * var type identifier_ (= expression)? (, identifier_ (= expression)?)*;
 * */
std::unique_ptr<AST::Statement> ParsingEngine::parseDeclarationStatement(bool global) {
    eat(VAR);

    std::map<Symbols::SymbolTableEntry *, std::unique_ptr<AST::Expression>> init_list;
    TypeWrapper *type = parseType();

    while (true) {
        std::string name = eat_identifier();
        Symbols::SymbolTableEntry *entry;
        if (global) {
            entry = globalSymTab_->define(type, name, Symbols::GLOBAL);
        } else {
            entry = symTab_->define(type, name, Symbols::LOCAL);
        }

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

std::unique_ptr<AST::Statement> ParsingEngine::parseStatement() {
    if (lexer.get() == KEYWORD) {
        if (lexer.keyword() == VAR) {
            return parseDeclarationStatement();
        } else if (lexer.keyword() == IF) {
            return parseIfStatement();
        } else if (lexer.keyword() == RETURN) {
            return parseReturnStatement();
        } else if (lexer.keyword() == FOR) {
            return parseForStatement();
        } else {
            std::cerr << "Unexpected token" << std::endl;
            return nullptr;
        }
    } else if (lexer.get() == PUNCTUATION && lexer.character() == '{') {
        return parseCompoundStatement();
    }

    return parseExpressionStatement();
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

std::vector<std::unique_ptr<AST::Function>> ParsingEngine::parseStruct() {
    eat(STRUCT);
    std::string id = eat_identifier();
    auto *t = llvm::StructType::create(*llvm_context_, id);

    currentStruct = t;

    (*type_table)[id] = new (struct TypeInfo) {t, {}};
    bool packed = false;

    if (is(PACKED)) {
        eat(PACKED);
        packed = true;
    }

    eat('{');
    std::vector<llvm::Type *> members;
    std::vector<std::unique_ptr<AST::Function>> methods;
    int i = 0;
    while (!is('}')) {
        if (is(FN)) {
            auto this_arg = new Symbols::SymbolTableEntry;
            this_arg->type = TypeWrapper::getPointerTo(new TypeWrapper(t));
            this_arg->n = "this";

            std::string original_name;

            auto f = parseFunction(
                    {this_arg}, &original_name);

            std::string func_name = f->prototype->name;
            // unmangled required here
            if ((*type_table)[id]->methods.find(original_name) != (*type_table)[id]->methods.end()) {
                (*type_table)[id]->methods[original_name] = std::vector<AST::FunctionPrototype *>(
                        {std::get<AST::FunctionPrototype *>((*type_table)[id]->methods[original_name]),
                         f->prototype.get()});
            } else {
                (*type_table)[id]->methods[original_name] = f->prototype.get();
            }
            methods.push_back(std::move(f));
            continue;
        }

        TypeWrapper *type = parseType();
        std::string name = eat_identifier();
        (*type_table)[id]->fields[name] = {i++, type};
        members.push_back(type->type);

        if (!is(';')) {
            break;
        }
        eat(';');
    }
    eat('}');
    t->setBody(members, packed);
    currentStruct = nullptr;
    return methods;
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
    if (!is(';')) {
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
    auto init = parseStatement();
    auto cond = parseExpression();
    eat(';');
    auto update = parseExpression();

    eat(')');
    auto body = parseCompoundStatement();

    return std::make_unique<AST::ForStatement>(std::move(init), std::move(cond), std::move(update), std::move(body));
}

std::string ParsingEngine::mangleFunctionName(AST::FunctionPrototype *proto) {
    std::string mangled = proto->name;
    if (currentStruct != nullptr) {
        proto->name = currentStruct->getStructName().str() + "_" + proto->name;
        mangled = proto->name;
    }

    if (funcTab_->exists(mangled)) {
        std::string overloaded_id;
        llvm::raw_string_ostream s(overloaded_id);
        for (auto &i: proto->args) {
            i->type->type->print(s);
        }

        proto->name += overloaded_id;
    }

    return mangled;
}
