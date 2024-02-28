#ifndef ZUNG_SYMBOL_TABLE_H
#define ZUNG_SYMBOL_TABLE_H

#include <string>
#include <lexer.h>
#include <AST.h>
#include <utility>
#include <variant>

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

class symbol_table {
public:
    SymbolTableEntry *define(Type type, std::string name, enum VarType varType);
    SymbolTableEntry *find(const std::string& name);
private:
    std::vector<SymbolTableEntry *> subroutine_symbols_;
    std::vector<SymbolTableEntry *> global_symbols_;
};

#endif //ZUNG_SYMBOL_TABLE_H
