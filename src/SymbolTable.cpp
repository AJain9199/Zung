#include <SymbolTable.h>

using namespace Symbols;

SymbolTableEntry *SymbolTable::define(TypeWrapper *type, std::string name, enum VarType varType) {
    auto *entry = new SymbolTableEntry();

    entry->n = name;
    entry->type = type;
    if (varType == GLOBAL) {
        global_symbols_[name] = entry;
    } else {
        subroutine_symbols_[name] = entry;
    }
    return entry;
}

SymbolTableEntry *SymbolTable::find(const std::string &name) {
    auto it = subroutine_symbols_.find(name);
    return it == subroutine_symbols_.end() ? global_symbols_[name] : it->second;
}
