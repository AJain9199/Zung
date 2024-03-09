#include <stdio.h>
#include <stdlib.h>

#define PRECISION   3

double pow(double n, double exp)
{
    // math note: x^y = 2^(y*(log2(x))

#define ROUND_NEAR  0x027F;
#define ROUND_DOWN  0x067F;
#define ROUND_UP    0x0A7F;
#define ROUND_TRUNC 0x0E7F;

    int newRoundingMode = ROUND_UP;
    int originalRoundingMode = 0x0000;

    asm(
            "fnstcw %[originalRoundingMode]"    "\n"    // save original rounding mode
            "fldcw  %[newRoundingMode]"         "\n"    // load new rounding mode

            "fldl   %[expIn]"                   "\n"    // load exponent
            "fldl   %[nIn]"                     "\n"    // load n value
            "fyl2x"                             "\n"    // y*(log2(x))                  (ex: 3.962)
            "fld    %%st(0)"                    "\n"    // duplicate log calculation
            "frndint"                           "\n"    // round to int                 (ex: 4)
            "fldz"                              "\n"    // load +0.0 for testing
            "fcomi"                             "\n"    // check if rounded number is negative
            "ja NEGATIVE"                       "\n"    // jump if negative

            "POSITIVE:"                         "\n"
            "faddp"                             "\n"    // get rid of test +0.0
            "mov    $0, %%eax"                  "\n"    // set positive flag in %eax for later
            "jmp    FRACTIONSTEPS"              "\n"

            "NEGATIVE:"                         "\n"
            "faddp"                             "\n"    // get rid of test +0.0
            "mov    $1, %%eax"                  "\n"    // set negative flag in %eax for later
            "jmp    FRACTIONSTEPS"              "\n"

            "FRACTIONSTEPS:"                    "\n"
            "fsub   %%st(1),%%st(0)"            "\n"    // rounded up int - log calculation
            "fldz"                              "\n"    // load +0.0 for testing
            "fcomi"                             "\n"    // check if fraction exists in result
            "je     NOREMAINDER"                "\n"
            "jmp    REMAINDER"                  "\n"

            "REMAINDER:"                        "\n"
            "faddp"                             "\n"    // get rid of test +0.0
            "fld1"                              "\n"    // load +1.0
            "fsubp"                             "\n"    // +1.0 - fraction part
            "f2xm1"                             "\n"    // 2**x-1 (raise 2 to the power of ST(0),
            // subtract one, store result back into ST(0)
            "fld1"                              "\n"    // load +1.0
            "faddp"                             "\n"    // add one back (because f2xm1 subtracts 1)
            "fscale"                            "\n"    // scale result by power of 2
            "test   %%eax, %%eax"               "\n"    // check if this number is negative
            "jz     FINAL"                      "\n"    // if 0, number is positive (nothing more to do)
            "jmp    DIVIDEBYTWO"                "\n"    // if 1, number is negative (deal with negative exponents)

            "NOREMAINDER:"                      "\n"
            "faddp"                             "\n"    // get rid of test +0.0
            "fxch"                              "\n"    // switch registers
            "fld1"                              "\n"    // load +1.0
            "fscale"                            "\n"    // scale result by power of 2
            "jmp    FINAL"                      "\n"

            "DIVIDEBYTWO:"                      "\n"
            "fld1"                              "\n"    // load +1.0
            "fld1"                              "\n"    // load +1.0
            "faddp"                             "\n"    // add in order to get +2.0 for division (for negative exponents)
            "fxch"                              "\n"    // switch registers
            "fdivp"                             "\n"    // divide: fraction result / +2.0
            "jmp FINAL"                         "\n"

            "FINAL:"                            "\n"
            "fstpl  %[nOut]"                    "\n"    // return result to "n" var
            "fldcw  %[originalRoundingMode]"    "\n"    // reload original rounding mode

            : [nOut]                    "=m"    (n)     // outputs
    : [nIn]                     "m"     (n),    // inputs
    [expIn]                   "m"     (exp),
    [originalRoundingMode]    "m"     (originalRoundingMode),
    [newRoundingMode]         "m"     (newRoundingMode)
    : "eax", "ebx", "ecx"                       // clobbers
    );

    return n;
}