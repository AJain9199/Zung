#include <ParsingEngine.h>
#include <CodeGenerationEngine.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/TargetParser/Host.h>
#include <iostream>
#include <chrono>
#include <filesystem>
#include <cmd.h>


int main(int argc, char **argv) {
    auto parse_pre = std::chrono::high_resolution_clock::now();

    std::unique_ptr<llvm::LLVMContext> context = std::make_unique<llvm::LLVMContext>();

    if (argc < 2) {
        std::cerr << "Usage: " << argv[0] << " <filename>" << std::endl;
        return 1;
    }

    std::string build_dir = GET_CMD_DEFAULT("-o", "./");
    std::string target = GET_CMD_DEFAULT("-target", llvm::sys::getDefaultTargetTriple());

    char **args = (char **) malloc(sizeof(char *) * argc + 1);
    for (int i = 2; i < argc; i++) {
        args[i] = argv[i];
    }
    args[argc] = nullptr;

    ParsingEngine parse(argv[1], args, std::move(context), argv[0]);
    auto p = parse.parseTranslationUnit();
    auto parse_post = std::chrono::high_resolution_clock::now();

    std::cout << "Parsing took "
              << std::chrono::duration_cast<std::chrono::milliseconds>(parse_post - parse_pre).count() << "ms"
              << std::endl;
#ifdef DEBUG
    auto print_pre = std::chrono::high_resolution_clock::now();
    PrintVisitor pv;
    p->accept(pv);
    auto print_post = std::chrono::high_resolution_clock::now();
    std::cout << "Printing took " << std::chrono::duration_cast<std::chrono::milliseconds>(print_post - print_pre).count() << "ms" << std::endl;
#endif // DEBUG

    parse.get_context(context);

    auto codegen_pre = std::chrono::high_resolution_clock::now();

    int filetype = 1;
    if (cmdOptionExists(argv, argv + argc, "-S")) {
        filetype = 0;
    } else if (cmdOptionExists(argv, argv + argc, "-emit-llvm")) {
        filetype = 2;
    }

    std::filesystem::path path(argv[1]);


    CodeGenerationEngine cge(std::move(context), GET_CMD("-cg"));
    p->accept(cge);

    std::filesystem::path out_dir(build_dir);
    if (out_dir.is_absolute()) {
        out_dir /= path.filename();
    } else {
        out_dir = path.parent_path() / build_dir;
        out_dir /= path.filename();
    }
    cge.writeCode(out_dir, target, filetype);
    delete p;
    auto codegen_post = std::chrono::high_resolution_clock::now();
    std::cout << "Code generation took "
              << std::chrono::duration_cast<std::chrono::milliseconds>(codegen_post - codegen_pre).count() << "ms"
              << std::endl;
    std::cout << "Total time for compiling " << argv[1] << ": "
              << std::chrono::duration_cast<std::chrono::milliseconds>(codegen_post - parse_pre).count() << "ms"
              << std::endl;
}
