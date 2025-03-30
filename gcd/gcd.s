text:
.global _start
_start:
    li a0, 24
    li a1, 60
    call gdc
    call print_int
    li a0, 0
    li a7, 93
    ecall 

gdc:
    rem a2, a0, a1
    mv a0, a1
    mv a1, a2
    beq a1, x0, gdc_done
    j gdc

gdc_done:
    ret
