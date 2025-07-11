#ifndef ZUNG_TYPES_H
#define ZUNG_TYPES_H

#include <map>
#include <SymbolTable.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/DerivedTypes.h>

struct TypeWrapper {
    llvm::Type *type;
    TypeWrapper *pointee_type{nullptr};

    TypeWrapper(llvm::Type *type, TypeWrapper *pointee_type) : type(type), pointee_type(pointee_type) {}
    explicit TypeWrapper(llvm::Type *t) : type(t) {}

    static TypeWrapper *getPointerTo(TypeWrapper *type) {
        return new TypeWrapper(llvm::PointerType::get(type->type, 0), type);
    }

    static TypeWrapper *getArrayTo(TypeWrapper *type, int size) {
        return new TypeWrapper(llvm::ArrayType::get(type->type, size), type);
    }

    ~TypeWrapper() {
        delete pointee_type;
    }
};

#endif //ZUNG_TYPES_H
