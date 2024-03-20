#ifndef ZUNG_SYMBOLTABLE_H
#define ZUNG_SYMBOLTABLE_H

#include <string>
#include <Lexer.h>
#include <utility>
#include <variant>
#include <vector>
#include <llvm/IR/Type.h>
#include "Types.h"

namespace AST {
    class FunctionPrototype;
}

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
        SymbolTableEntry *define(SymbolTableEntry *entry, enum VarType varType) {
            if (varType == GLOBAL) {
                global_symbols_[entry->n] = entry;
            } else {
                subroutine_symbols_[entry->n] = entry;
            }
            return entry;
        }

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
        AST::FunctionPrototype *define(const std::string& name, AST::FunctionPrototype *function) {
            return function_table_[name] = function;
        }

        AST::FunctionPrototype *find(const std::string &name) {
            return function_table_[name];
        }

    private:
        std::unordered_map<std::string, AST::FunctionPrototype *> function_table_;
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
    std::map<std::string, AST::FunctionPrototype *> methods;

    ~TypeInfo() {
        for (auto &i: fields) {
            delete i.second.type;
        }
    }
};

#endif //ZUNG_SYMBOLTABLE_H
