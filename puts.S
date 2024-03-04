#include <asm/unistd.h>    // This is a standard Linux / glibc header file
      // includes unistd_64.h or unistd_32.h depending on current mode
      // Contains only #define constants (no C prototypes) so we can include it from asm without syntax errors.

.p2align 4
.globl puts            #void print_uint64(uint64_t value)
puts:
    lea -4(%rsp), %rsi
    mov %edi, (%rsi)
    mov $1, %eax
    mov $1, %edi
    mov $1, %edx
    syscall
    ret