#include <parse.h>
#include <print_visitor.h>

int main() {
    ParsingEngine parse("../test.zng");
    auto p = parse.parseTranslationUnit();
    PrintVisitor pv;
    p->accept(pv);
}
