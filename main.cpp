#include <parse.h>
#include <PrintVisitor.h>

int main() {
    ParsingEngine parse("../test.zng");
    auto p = parse.parseTranslationUnit();
    PrintVisitor pv;
    p->accept(pv);
}
