| Tag                    | Enums                       |   Description |
|------------------------|-----------------------------|---------------|
| `(bool)`               | NifasmType                  | boolean type |
| `(i N)`                | NifasmType                  | signed integer type of N bits |
| `(u N)`                | NifasmType                  | unsigned integer type of N bits |
| `(f N)`                | NifasmType                  | float type of N bits |
| `(ptr T)`              | NifasmType                  | pointer to single element |
| `(aptr T)`             | NifasmType                  | pointer to array of elements |
| `(array T N)`          | NifasmType                  | array type |
| `(type D ...)`         | NifasmDecl                  | type declaration |
| `(object ...)`         | NifasmType                  | object type definition |
| `(fld D T)`            | NifasmType                  | field definition |
| `(proc D ...)`         | NifasmDecl                  | proc declaration |
| `(params ...)`         | NifasmDecl                  | parameters block |
| `(param D L T)`        | NifasmDecl                  | parameter declaration |
| `(rets D L T)`         | NifasmDecl                  | return value declaration |
| `(clobber ...)`        | NifasmDecl                  | clobbered registers list |
| `(body ...)`           | NifasmDecl                  | proc body |
| `(var D L T)`          | NifasmDecl                  | variable declaration |
| `(s)`                  | NifasmOther                 | stack slot location tag |
| `(ssize)`              | NifasmExpr                  | stack size expression |
| `(mov D S)`            | NifasmInst                  | move instruction |
| `(lea D S)`            | NifasmInst                  | load effective address |
| `(movapd D S)`         | NifasmInst                  | move aligned packed double |
| `(movsd D S)`          | NifasmInst                  | move scalar double |
| `(add D S)`            | NifasmInst                  | add instruction |
| `(sub D S)`            | NifasmInst                  | subtract instruction |
| `(mul S)`              | NifasmInst                  | unsigned multiply |
| `(imul D S)`           | NifasmInst                  | signed multiply |
| `(div D S R)`          | NifasmInst                  | unsigned divide |
| `(idiv D S R)`         | NifasmInst                  | signed divide |
| `(addsd D S)`          | NifasmInst                  | add scalar double |
| `(subsd D S)`          | NifasmInst                  | subtract scalar double |
| `(mulsd D S)`          | NifasmInst                  | multiply scalar double |
| `(divsd D S)`          | NifasmInst                  | divide scalar double |
| `(and D S)`            | NifasmInst                  | bitwise and |
| `(or D S)`             | NifasmInst                  | bitwise or |
| `(xor D S)`            | NifasmInst                  | bitwise xor |
| `(shl D S)`            | NifasmInst                  | shift left |
| `(shr D S)`            | NifasmInst                  | shift right |
| `(sal D S)`            | NifasmInst                  | shift arithmetic left |
| `(sar D S)`            | NifasmInst                  | shift arithmetic right |
| `(inc O)`              | NifasmInst                  | increment |
| `(dec O)`              | NifasmInst                  | decrement |
| `(neg O)`              | NifasmInst                  | negate |
| `(not O)`              | NifasmInst                  | bitwise not |
| `(cmp D S)`            | NifasmInst                  | compare |
| `(test D S)`           | NifasmInst                  | test |
| `(sete D)`             | NifasmInst                  | set byte if equal |
| `(setz D)`             | NifasmInst                  | set byte if zero |
| `(setne D)`            | NifasmInst                  | set byte if not equal |
| `(setnz D)`            | NifasmInst                  | set byte if not zero |
| `(seta D)`             | NifasmInst                  | set byte if above |
| `(setnbe D)`           | NifasmInst                  | set byte if not below or equal |
| `(setae D)`            | NifasmInst                  | set byte if above or equal |
| `(setnb D)`            | NifasmInst                  | set byte if not below |
| `(setnc D)`            | NifasmInst                  | set byte if not carry |
| `(setb D)`             | NifasmInst                  | set byte if below |
| `(setnae D)`           | NifasmInst                  | set byte if not above or equal |
| `(setc D)`             | NifasmInst                  | set byte if carry |
| `(setbe D)`            | NifasmInst                  | set byte if below or equal |
| `(setna D)`            | NifasmInst                  | set byte if not above |
| `(setg D)`             | NifasmInst                  | set byte if greater |
| `(setnle D)`           | NifasmInst                  | set byte if not less or equal |
| `(setge D)`            | NifasmInst                  | set byte if greater or equal |
| `(setnl D)`            | NifasmInst                  | set byte if not less |
| `(setl D)`             | NifasmInst                  | set byte if less |
| `(setnge D)`           | NifasmInst                  | set byte if not greater or equal |
| `(setle D)`            | NifasmInst                  | set byte if less or equal |
| `(setng D)`            | NifasmInst                  | set byte if not greater |
| `(seto D)`             | NifasmInst                  | set byte if overflow |
| `(sets D)`             | NifasmInst                  | set byte if sign |
| `(setp D)`             | NifasmInst                  | set byte if parity |
| `(jmp L)`              | NifasmInst                  | unconditional jump |
| `(je L)`               | NifasmInst                  | jump if equal |
| `(jz L)`               | NifasmInst                  | jump if zero |
| `(jne L)`              | NifasmInst                  | jump if not equal |
| `(jnz L)`              | NifasmInst                  | jump if not zero |
| `(jg L)`               | NifasmInst                  | jump if greater |
| `(jng L)`              | NifasmInst                  | jump if not greater |
| `(jge L)`              | NifasmInst                  | jump if greater or equal |
| `(jnge L)`             | NifasmInst                  | jump if not greater or equal |
| `(ja L)`               | NifasmInst                  | jump if above |
| `(jna L)`              | NifasmInst                  | jump if not above |
| `(jae L)`              | NifasmInst                  | jump if above or equal |
| `(jnae L)`             | NifasmInst                  | jump if not above or equal |
| `(call T ...)`         | NifasmInst                  | function call |
| `(ret)`                | NifasmInst                  | return instruction |
| `(push O)`             | NifasmInst                  | push to stack |
| `(pop O)`              | NifasmInst                  | pop from stack |
| `(nop)`                | NifasmInst                  | no operation |
| `(syscall)`            | NifasmInst                  | system call |
| `(lab L)`              | NifasmInst                  | label definition |
| `(ite ...)`            | NifasmInst                  | if-then-else structure |
| `(loop ...)`           | NifasmInst                  | loop structure |
| `(stmts ...)`          | NifasmInst                  | statement block |
| `(dot B F)`            | NifasmExpr                  | field access |
| `(at B I)`             | NifasmExpr                  | array index |
| `(mem ...)`            | NifasmExpr                  | memory reference |
| `(rodata L S)`         | NifasmDecl                  | read-only data (string/bytes) |

