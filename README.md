# Zung
Zung is a C-like compiled low-level language. This is the compiler for Zung.

Current status:
- Lexer: Done (Deterministic Finite Automata (DFA)-based lexer)
- Parser: Done (Recursive Descent Parser, with operator-precedence parsing for expressions)
- Code Generator: Done (LLVM)

To try it out, create a file named `test.zung` and write some code like:

```
fn main(int d) {
    var a = 5;
    var b = 10;
    a = b * a;
    b = a + d ** 2;
}
```

Cool Features:
- Structs
- Pointer Arithmetic
- 

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
- Generic typing system
