#ifndef ZUNG_SYMBOLTABLE_H
#define ZUNG_SYMBOLTABLE_H

#include <string>
#include <Lexer.h>
#include <utility>
#include <variant>
#include <vector>
#include <llvm/IR/Type.h>

namespace Symbols {
typedef struct {
    llvm::Type *type;
    std::string name;
} SymbolTableEntry;

enum VarType {
    GLOBAL,
    LOCAL
};

class SymbolTable {
public:
    SymbolTableEntry *define(llvm::Type *type, std::string name, enum VarType varType);
    SymbolTableEntry *find(const std::string& name);
private:
    std::vector<SymbolTableEntry *> subroutine_symbols_;
    std::vector<SymbolTableEntry *> global_symbols_;
};
}

#endif //ZUNG_SYMBOLTABLE_H
