text:
.global _start
_start:
    li a0, 24
    li a1, 60
    call gcd
    call print_int
    li a0, 0
    li a7, 93
    ecall 

gcd:
    rem a2, a0, a1
    mv a0, a1
    mv a1, a2
    beq a1, x0, gcd_done
    j gcd

gcd_done:
    ret
