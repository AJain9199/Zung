# Zung
Zung is an in-progress C-like compiled low-level language. This is the compiler for Zung. (Not intended for actual use)

Current status:
- Lexer: Done (Deterministic Finite Automata (DFA)-based lexer)
- Parser: Done (Recursive Descent Parser, with operator-precedence parsing for expressions)

As of now, the program successfully parses the input and generates an abstract syntax tree (AST), and prints the structure of the tree using a Visitor-based printer.

To try it out, create a file named `test.zung` and write some code like:

```
fn main(int d) {
    var a = 5;
    var b = 10;
    a = b * a;
    b = a + d ** 2;
}
```

The compiler supports all the basic arithmetic operations, variable declarations, and function declarations.

Operators (in order of precedence):
- `**`
- `//, %, *, /`
- `+, -`
- `|<, >|` (Left and right shift)
- `>, <, <=, >=`
- `==, !=`
- `&`
- `^`
- `|`
- `&&`
- `||`
- `=`

## Future:
- Code generation, with LLVM and visitor-based code generation engine
- `struct`s and `bitstruct`s (built-in bitfields)
- Pointers
- Generic typing system