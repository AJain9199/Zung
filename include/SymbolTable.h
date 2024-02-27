#ifndef ZUNG_SYMBOLTABLE_H
#define ZUNG_SYMBOLTABLE_H

#include <string>
#include <lexer.h>
#include <AST.h>
#include <utility>
#include <variant>

typedef std::variant<enum DefaultType, std::string> BasicType;

typedef struct type_t {
    BasicType type;
    std::vector<int> array_dim;

    type_t(BasicType t, std::vector<int> a) : type(std::move(t)), array_dim(std::move(a)) {}
    type_t(): type(), array_dim({}) {}
} Type;


typedef struct {
    Type type;
    std::string name;
} SymbolTableEntry;

enum VarType {
    GLOBAL,
    LOCAL
};

class SymbolTable {
public:
    SymbolTableEntry *define(Type type, std::string name, enum VarType varType) {
        auto *entry = new SymbolTableEntry();
        entry->name = std::move(name);
        entry->type = std::move(type);
        if (varType == GLOBAL) {
            global_symbols_.push_back(entry);
        } else {
            subroutine_symbols_.push_back(entry);
        }
        return entry;
    };

    SymbolTableEntry *find(const std::string& name) {
        for (auto &entry : subroutine_symbols_) {
            if (entry->name == name) {
                return entry;
            }
        }

        for (auto &entry : global_symbols_) {
            if (entry->name == name) {
                return entry;
            }
        }

        return nullptr;
    }
private:
    std::vector<SymbolTableEntry *> subroutine_symbols_;
    std::vector<SymbolTableEntry *> global_symbols_;
};

#endif //ZUNG_SYMBOLTABLE_H
