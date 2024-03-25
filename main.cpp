#include <ParsingEngine.h>
#include <PrintVisitor.h>
#include <CodeGenerationEngine.h>
#include <llvm/IR/LLVMContext.h>
#include <iostream>
#include <chrono>
#include <filesystem>

int main(int argc, char **argv) {
    auto parse_pre = std::chrono::high_resolution_clock::now();

    std::unique_ptr<llvm::LLVMContext> context = std::make_unique<llvm::LLVMContext>();

    if (argc != 2) {
        std::cerr << "Usage: " << argv[0] << " <filename>" << std::endl;
        return 1;
    }

    ParsingEngine parse(argv[1], std::move(context), argv[0]);
    auto p = parse.parseTranslationUnit();
    auto parse_post = std::chrono::high_resolution_clock::now();

    std::cout << "Parsing took " << std::chrono::duration_cast<std::chrono::milliseconds>(parse_post - parse_pre).count() << "ms" << std::endl;
#ifdef DEBUG
    auto print_pre = std::chrono::high_resolution_clock::now();
    PrintVisitor pv;
    p->accept(pv);
    auto print_post = std::chrono::high_resolution_clock::now();
    std::cout << "Printing took " << std::chrono::duration_cast<std::chrono::milliseconds>(print_post - print_pre).count() << "ms" << std::endl;
#endif // DEBUG

    parse.get_context(context);

    auto codegen_pre = std::chrono::high_resolution_clock::now();
    std::filesystem::path path(argv[1]);
    path.replace_extension(".ll");
    CodeGenerationEngine cge(std::move(context), path.string());
    p->accept(cge);
    delete p;
    auto codegen_post = std::chrono::high_resolution_clock::now();
    std::cout << "Code generation took " << std::chrono::duration_cast<std::chrono::milliseconds>(codegen_post - codegen_pre).count() << "ms" << std::endl;
    std::cout << "Total time for compiling " << argv[1] << ": " << std::chrono::duration_cast<std::chrono::milliseconds>(codegen_post - parse_pre).count() << "ms" << std::endl;
}
