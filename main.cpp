#include <ParsingEngine.h>
#include <PrintVisitor.h>
#include <CodeGenerationEngine.h>
#include <llvm/IR/LLVMContext.h>

int main() {
    std::unique_ptr<llvm::LLVMContext> context = std::make_unique<llvm::LLVMContext>();
    ParsingEngine parse("../test.zng", std::move(context));
    auto p = parse.parseTranslationUnit();
    PrintVisitor pv;
    p->accept(pv);

    parse.get_context(context);

    CodeGenerationEngine cge(std::move(context));
    p->accept(cge);
}
