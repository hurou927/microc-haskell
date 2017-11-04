# MICROC compiler

input : stdin / output : stdout

`./exe < count.c > count.asm`

count.c
``` c : count.c
n=in;
L1:
    out(n);
    unless(n) goto L2;
    n=n-1;
    goto L1;
L2:
    halt;
    int n;
```

↓↓↓

count.asm
``` assembly:count.asm
        IN
        POP n
L1:
        PUSH n
        OUT
        PUSH n
        JZ L2
        PUSH n
        PUSHI 1
        SUB
        POP n
        JMP L1
L2:
        HALT
n: 0
```

