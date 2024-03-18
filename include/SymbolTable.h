#ifndef ZUNG_SYMBOLTABLE_H
#define ZUNG_SYMBOLTABLE_H

#include <string>
#include <Lexer.h>
#include <utility>
#include <variant>
#include <vector>
#include <llvm/IR/Type.h>
#include "Types.h"

namespace Symbols {
typedef struct sym_t {
    TypeWrapper *type;
    std::string n;

    [[nodiscard]] std::string name() const {
        return n;
    }
} SymbolTableEntry;

typedef struct {
    std::string name;
    TypeWrapper *return_type;
} FunctionTableEntry;

enum VarType {
    GLOBAL,
    LOCAL
};

class SymbolTable {
public:
    SymbolTableEntry *define(TypeWrapper *type, std::string name, enum VarType varType);
    SymbolTableEntry *find(const std::string& name);
private:
    std::unordered_map<std::string, SymbolTableEntry *> subroutine_symbols_;
    std::unordered_map<std::string, SymbolTableEntry *> global_symbols_;
};

class FunctionTable {
public:
    Symbols::FunctionTableEntry *define(std::string name, TypeWrapper *return_type) {
        auto *entry = new Symbols::FunctionTableEntry();
        entry->name = std::move(name);
        entry->return_type = return_type;
        function_table_[entry->name] = entry;
        return entry;
    }

    Symbols::FunctionTableEntry *find(const std::string& name) {
        return function_table_[name];
    }

private:
    std::unordered_map<std::string, FunctionTableEntry *> function_table_;
};
}

/* Stores information about the fields of a struct */
typedef struct FieldInfo {
    int idx;
    TypeWrapper *type;
} FieldInfo;

/*
 * Stores information about the structural aggregate types.
 */
struct TypeInfo {
    llvm::Type *type;
    std::map<std::string, FieldInfo> fields;
    std::map<std::string, Symbols::FunctionTableEntry *> methods;
};

#endif //ZUNG_SYMBOLTABLE_H
