#include <ParsingEngine.h>
#include <PrintVisitor.h>
#include <CodeGenerationEngine.h>

int main() {
    ParsingEngine parse("../test.zng");
    auto p = parse.parseTranslationUnit();
    PrintVisitor pv;
    p->accept(pv);

    CodeGenerationEngine cge;
    p->accept(cge);
}
