#include <SymbolTable.h>
#include <AST.h>

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

AST::FunctionPrototype *FunctionTable::find(const std::string &name, std::vector<TypeWrapper *> arg_types)  {
    if (is_overloaded(name)) {
        return std::get<AST::FunctionPrototype *>(function_table_[name]);
    } else {
        for (auto &f: std::get<std::vector<AST::FunctionPrototype *>>(function_table_[name])) {
            if (f->args.size() == arg_types.size()) {
                bool match = true;
                for (int i = 0; i < f->args.size(); i++) {
                    if (f->args[i]->type->type != arg_types[i]->type) {
                        match = false;
                        break;
                    }
                }
                if (match) {
                    return f;
                }
            }
        }
        return nullptr;
    }
}