#include <SymbolTable.h>

using namespace Symbols;

SymbolTableEntry *SymbolTable::define(Type type, std::string name, enum VarType varType) {
    auto *entry = new SymbolTableEntry();
    entry->name = std::move(name);
    entry->type = std::move(type);
    if (varType == GLOBAL) {
        global_symbols_.push_back(entry);
    } else {
        subroutine_symbols_.push_back(entry);
    }
    return entry;
}

SymbolTableEntry *SymbolTable::find(const std::string &name) {
    for (auto &entry: subroutine_symbols_) {
        if (entry->name == name) {
            return entry;
        }
    }

    for (auto &entry: global_symbols_) {
        if (entry->name == name) {
            return entry;
        }
    }

    return nullptr;
}

type_t::type_t(BasicType t, std::vector<int> a) : type(std::move(t)), array_dim(std::move(a)) {}

type_t::type_t() : type(), array_dim({}) {}

