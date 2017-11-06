# MICROC compiler

#

`microcCompilerLine :: String -> String`

Convert a line of microc code to asm

`microcCompilerStr :: String -> String`

Convert microc code to asm

`microcCompiler::IO()`

Input:Stdin / Output:Stdout

## Example

```haskell:microc
main::IO()
main = microcCompiler
```

`./microc < count.c > count.asm`

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

Hiroshima Univ. / Embedded Software

https://momiji.hiroshima-u.ac.jp/syllabusHtml/2017_58_U4090201.html
