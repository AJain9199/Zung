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

    typedef struct func_t {
        std::string name;
        TypeWrapper *return_type;

        ~func_t() {}
    } FunctionTableEntry;

    enum VarType {
        GLOBAL,
        LOCAL
    };

    class SymbolTable {
    public:
        SymbolTableEntry *define(TypeWrapper *type, std::string name, enum VarType varType);

        SymbolTableEntry *find(const std::string &name);

        ~SymbolTable() {
            for (auto &i: subroutine_symbols_) {
                delete i.second->type;
                delete i.second;
            }
            for (auto &i: global_symbols_) {
                if (i.second) {
                    delete i.second->type;
                    delete i.second;
                }
            }
        }

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

        Symbols::FunctionTableEntry *find(const std::string &name) {
            return function_table_[name];
        }

        ~FunctionTable() {
            for (auto &i: function_table_) {
                delete i.second;
            }
            function_table_.clear();
        };

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

    ~TypeInfo() {
        for (auto &i: fields) {
            delete i.second.type;
        }
    }
};

#endif //ZUNG_SYMBOLTABLE_H
