#ifndef ZUNG_SYMBOLTABLE_H
#define ZUNG_SYMBOLTABLE_H

#include <string>
#include <Lexer.h>
#include <utility>
#include <variant>
#include <vector>

namespace Symbols {
typedef std::variant<enum DefaultType, std::string> BasicType;

typedef struct type_t {
    BasicType type;
    std::vector<int> array_dim;

    type_t(BasicType t, std::vector<int> a);
    type_t();
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
    SymbolTableEntry *define(Type type, std::string name, enum VarType varType);
    SymbolTableEntry *find(const std::string& name);
private:
    std::vector<SymbolTableEntry *> subroutine_symbols_;
    std::vector<SymbolTableEntry *> global_symbols_;
};
}

#endif //ZUNG_SYMBOLTABLE_H
