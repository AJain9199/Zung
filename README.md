# Zung
Zung is a C-like compiled low-level language. Zung is inspired by C, Rust, and a variety of other languages, and aims to be simple and concise. 

Zung was developed as a learning effort to understand compiler design and implementation.


## Example Code

```
extern printf(i8 *str, ...);
extern pow(double base, double exp): double;

struct Point {
    i32 x;
    i32 y;
    i32 z;
    
    # all functions/methods return void by default
    fn init(i32 x, i32 y) {
        this->x = x;
        this->y = y;
        this->z = 0;
        return;
    }

    fn init(i32 x, i32 y, i32 z) {
        this->x = x;
        this->y = y;
        this->z = z;
        return;
    }

    
    # operator overloading
    fn __add__(Point other) : Point {
        var Point result;
        result.x = this->x + other.x;
        result.y = this->y + other.y;
        result.z = this->z + other.z;
        return result;
    }

    fn __mul__(Point other) : i32 {
        return (this->x * other.x) + (this->y * other.y) + (this->z * other.z);
    }

    fn __mul__(i32 scalar) : Point {
        var Point result;
        result.x = this->x * scalar;
        result.y = this->y * scalar;
        result.z = this->z * scalar;
        return result;
    }
}

fn mod(Point p) : double {
    var double sq_sum =  (p.x ** 2) + (p.y ** 2) + (p.z ** 2);
    return sq_sum ** 0.5;
}

fn main() {
    var Point p1, p2;
    
    # function overloading
    p1.init(3, 4);
    p2.init(1, 2, 3);
   
    # calls __add__ and __mul__
    var Point p3 = p1 + p2;
    p3 = p3 * 2;
    
    # method overloading
    var i32 dot = p1 * p2;
    
    printf("(%d, %d, %d)\n", p3.x, p3.y, p3.z);
    printf("dot = %d\n", dot);
    printf("|p1| = %f\n", mod(p1));
    return;
}
```

It prints:
```
(8, 12, 6)
dot = 11
|p1| = 5.000000
```

## Features
- Functions, variables, and basic control flow
- Structs, struct packing and methods
- Pointers and pointer arithmetic
- Operator overloading: `__add__`, `__mul__`, etc.
- Function/method overloading
- Interoperability with C libraries by using `extern`
- Expandable type system: `iN` gives an `n` bit-wide integer, useful for system programming (e.g. `i8`, `i16`)
- Diverse set of operators: `+`, `-`, `*`, `/`, `//` (floor division), `%`, `**` (exponentiation), `&`, `|`, `&&`, `||`, `|<`, `>|`, etc.

## Using
### Building
To build Zung, you must install (or [build](https://llvm.org/docs/CMake.html)) LLVM, making sure you build/install the `libLLVM.so` shared library. Then, run `cmake` with the `-DLLVM_DIR` option pointing to the directory containing `LLVMConfig.cmake`. 

### Compiling Code
To use zung to come code, run `Zung <file>.zng -o output.o`. Then, link the object code with the C standard libraries to access the full featureset: 
`gcc output.o -o a.out -lm -lc`

### Options
Full set of options:
- `-o <file>`: Output file name
- `-target <target>`: Target architecture (defaults to host architecture)
- `-emit-llvm`: Emit LLVM IR instead of object code
- `-S`: Emit assembly code instead of object code
- `-cg no-red-zone`: Disable red zone in the generated code (mainly for OSdev)

## Resources
Zung has been made possible by the following resources:
- [LLVM](https://llvm.org/)
- Principles of Compiler Design by Alfred V. Aho and Jeffery D. Ullman
- [Crafting Interpreters](https://craftinginterpreters.com/)
