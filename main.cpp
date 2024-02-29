#include <parse.h>
#include <print_visitor.h>
#include <code_generation_engine.h>

int main() {
    ParsingEngine parse("../test.zng");
    auto p = parse.parseTranslationUnit();
    PrintVisitor pv;
    p->accept(pv);

    CodeGenerationEngine cge;
    p->accept(cge);
}
