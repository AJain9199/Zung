#ifdef DEBUG

#include <PrintVisitor.h>
#include <AST.h>
#include <iostream>

#define INDENT() for (int i = 0; i < tab_level; i++) std::cout << "  "

void PrintVisitor::visit(const AST::TranslationUnit &unit) {
    std::cout << "Top-level Translation Unit" << std::endl;

    for (auto &f : unit.functions) {
        f->accept(*this);
    }
}

void PrintVisitor::visit(const AST::Function &function) {
    tab_level++;
    INDENT();

    std::cout << "Function: " << std::endl;

    INDENT();
    std::cout << "Function Prototype: " << std::endl;
    function.prototype->accept(*this);

    INDENT();
    std::cout << "Function Body: " << std::endl;
    if (function.body == nullptr) {
        INDENT();
        std::cout << "Function has no body" << std::endl;
        tab_level--;
        return;
    }
    function.body->accept(*this);
    tab_level--;
}

void PrintVisitor::visit(const AST::FunctionPrototype &prototype) {
    tab_level++;
    INDENT();
    std::cout << "Function " << prototype.name << " : " << prototype.args.size() << " Parameters" << std::endl;

    for (auto &j : prototype.args) {
        INDENT();
        std::cout << j->name() << " : ";
        j->type->type->print(llvm::errs());
        std::cout << std::endl;
    }
    tab_level--;
}

void PrintVisitor::visit(const AST::CompoundStatement &statement) {
    tab_level++;
    INDENT();
    std::cout << "Compound Statement: " << statement.statements.size() << " Statements" << std::endl;
    for (auto &stmt: statement.statements) {
        stmt->accept(*this);
    }
    tab_level--;
}

void PrintVisitor::visit(const AST::DeclarationStatement &stmt) {
    tab_level++;
    INDENT();
    std::cout << "Declaration Statement: " << std::endl;

    for (const auto & it : stmt.init_list) {
        INDENT();
        std::cout << it.first->name() << " = ";
        it.first->type->type->print(llvm::errs());
        std::cout << std::endl;
        it.second->accept(*this);
    }
    tab_level--;
}

void PrintVisitor::visit(const AST::ExpressionStatement &statement) {
    tab_level++;
    INDENT();
    std::cout << "Expression Statement: " << std::endl;

    statement.expr->accept(*this);
    tab_level--;
}

void PrintVisitor::visit(const AST::IfStatement &) {
    tab_level++;
    INDENT();
    std::cout << "If Statement" << std::endl;
    tab_level--;
}

void PrintVisitor::visit(const AST::VariableExpression &expression) {
    tab_level++;
    INDENT();
    std::cout << "Variable Expression: " << expression.variable->name() << std::endl;
    tab_level--;
}

void PrintVisitor::visit(const AST::BinaryExpression &expression) {
    tab_level++;
    INDENT();
    std::cout << "Binary Expression: " << std::endl;
    expression.LHS->accept(*this);
    INDENT();
    std::cout << "Operator: " << (char)expression.op << std::endl;
    expression.RHS->accept(*this);
    tab_level--;
}

void PrintVisitor::visit(const AST::UnaryExpression &expression) {
    tab_level++;
    INDENT();
    std::cout << "Unary Expression" << std::endl;
    INDENT();
    std::cout << "Operator: " << (char)expression.op << std::endl;
    expression.operand->accept(*this);
    tab_level--;
}

void PrintVisitor::visit(const AST::IntegralLiteralExpression &expression) {
    tab_level++;
    INDENT();
    std::cout << "Numeric Constant Expression: " << expression.val << std::endl;
    tab_level--;
}

void PrintVisitor::visit(const AST::ArrayIndexingExpression &) {
    tab_level++;
    INDENT();
    std::cout << "Array Indexing Expression" << std::endl;
    tab_level--;
}

void PrintVisitor::visit(const AST::FunctionCallExpression &call) {
    tab_level++;
    INDENT();
    std::cout << "Function Call Expression" << call.callee->func->name <<  std::endl;
    tab_level--;
}

void PrintVisitor::visit(const AST::AbstractNode &) {
    std::cout << "Unspecified node type";
}

void PrintVisitor::visit(const AST::ExternFunction &) {
    tab_level++;
    INDENT();
    std::cout << "Extern Function" << std::endl;
    tab_level--;
}

void PrintVisitor::visit(const AST::StringLiteralExpression &) {
    tab_level++;
    INDENT();
    std::cout << "String Literal Expression" << std::endl;
    tab_level--;
}

void PrintVisitor::visit(const AST::FloatLiteralExpression &) {

}

void PrintVisitor::visit(const AST::ReturnStatement &) {
    std::cout << "return";
}

#endif // DEBUG
