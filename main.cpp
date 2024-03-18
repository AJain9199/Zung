#include <ParsingEngine.h>
#include <PrintVisitor.h>
#include <CodeGenerationEngine.h>
#include <llvm/IR/LLVMContext.h>
#include <iostream>
#include <chrono>

int main() {
    auto parse_pre = std::chrono::high_resolution_clock::now();

    std::unique_ptr<llvm::LLVMContext> context = std::make_unique<llvm::LLVMContext>();
    ParsingEngine parse("../test.zng", std::move(context));
    auto p = parse.parseTranslationUnit();
    auto parse_post = std::chrono::high_resolution_clock::now();

    std::cout << "Parsing took " << std::chrono::duration_cast<std::chrono::milliseconds>(parse_post - parse_pre).count() << "ms" << std::endl;

    auto print_pre = std::chrono::high_resolution_clock::now();
    PrintVisitor pv;
    p->accept(pv);
    auto print_post = std::chrono::high_resolution_clock::now();
    std::cout << "Printing took " << std::chrono::duration_cast<std::chrono::milliseconds>(print_post - print_pre).count() << "ms" << std::endl;

    parse.get_context(context);

    auto codegen_pre = std::chrono::high_resolution_clock::now();
    CodeGenerationEngine cge(std::move(context));
    p->accept(cge);
    delete p;
    auto codegen_post = std::chrono::high_resolution_clock::now();
    std::cout << "Code generation took " << std::chrono::duration_cast<std::chrono::milliseconds>(codegen_post - codegen_pre).count() << "ms" << std::endl;
    std::cout << "Total time: " << std::chrono::duration_cast<std::chrono::milliseconds>(codegen_post - parse_pre).count() << "ms" << std::endl;
}
