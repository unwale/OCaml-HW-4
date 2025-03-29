  $ echo "let f x y z = x + y + z" > expr1.txt
  $ echo "let main = print_int (f 2 3 4)" >> expr1.txt
  $ ../bin/main.exe expr1.txt
  $ riscv64-linux-gnu-as -march=rv64gc output.s -o expr1.o
  $ riscv64-linux-gnu-ld expr1.o -o expr1.exe
  $ qemu-riscv64 expr1.exe
  [9]

  $ echo "let f a b = a + b" > expr2.txt
  $ echo "let main = print_int (f 5 10)" >> expr2.txt
  $ ../bin/main.exe expr2.txt
  $ riscv64-linux-gnu-as -march=rv64gc output.s -o expr2.o
  $ riscv64-linux-gnu-ld expr2.o -o expr2.exe
  $ qemu-riscv64 expr2.exe
  [15]

  $ echo "let f x y z = x * y - z" > expr3.txt
  $ echo "let main = print_int (f 7 8 9)" >> expr3.txt
  $ ../bin/main.exe expr3.txt
  $ riscv64-linux-gnu-as -march=rv64gc output.s -o expr3.o
  $ riscv64-linux-gnu-ld expr3.o -o expr3.exe
  $ qemu-riscv64 expr3.exe
  [47]

  $ echo "let f x y z = x * (y - z) + 100" > expr3.txt
  $ echo "let main = print_int (f 7 8 9)" >> expr3.txt
  $ ../bin/main.exe expr3.txt
  $ riscv64-linux-gnu-as -march=rv64gc output.s -o expr3.o
  $ riscv64-linux-gnu-ld expr3.o -o expr3.exe
  $ qemu-riscv64 expr3.exe
  [93]
