Dump of assembler code for function phase_6:
   0x000000000040114d <+0>:	push   %r13
   0x000000000040114f <+2>:	push   %r12
   0x0000000000401151 <+4>:	push   %rbp
   0x0000000000401152 <+5>:	push   %rbx
   0x0000000000401153 <+6>:	sub    $0x58,%rsp
   0x0000000000401157 <+10>:	lea    0x30(%rsp),%rsi
   0x000000000040115c <+15>:	call   0x401906 <read_six_numbers>
   0x0000000000401161 <+20>:	lea    0x30(%rsp),%r12
   0x0000000000401166 <+25>:	mov    $0x1,%r13d
   0x000000000040116c <+31>:	jmp    0x401196 <phase_6+73>
   0x000000000040116e <+33>:	call   0x401681 <explode_bomb>
   0x0000000000401173 <+38>:	jmp    0x4011a5 <phase_6+88>
   0x0000000000401175 <+40>:	add    $0x1,%rbx
   0x0000000000401179 <+44>:	cmp    $0x5,%ebx
   0x000000000040117c <+47>:	jg     0x40118e <phase_6+65>
   0x000000000040117e <+49>:	mov    0x30(%rsp,%rbx,4),%eax
   0x0000000000401182 <+53>:	cmp    %eax,0x0(%rbp)
   0x0000000000401185 <+56>:	jne    0x401175 <phase_6+40>
   0x0000000000401187 <+58>:	call   0x401681 <explode_bomb>
   0x000000000040118c <+63>:	jmp    0x401175 <phase_6+40>
   0x000000000040118e <+65>:	add    $0x1,%r13
   0x0000000000401192 <+69>:	add    $0x4,%r12
   0x0000000000401196 <+73>:	mov    %r12,%rbp
   0x0000000000401199 <+76>:	mov    (%r12),%eax
   0x000000000040119d <+80>:	sub    $0x1,%eax
   0x00000000004011a0 <+83>:	cmp    $0x5,%eax
   0x00000000004011a3 <+86>:	ja     0x40116e <phase_6+33>
   0x00000000004011a5 <+88>:	cmp    $0x5,%r13d
   0x00000000004011a9 <+92>:	jg     0x4011b0 <phase_6+99>
   0x00000000004011ab <+94>:	mov    %r13,%rbx
   0x00000000004011ae <+97>:	jmp    0x40117e <phase_6+49>
   0x00000000004011b0 <+99>:	mov    $0x0,%esi
   0x00000000004011b5 <+104>:	mov    0x30(%rsp,%rsi,4),%ecx
   0x00000000004011b9 <+108>:	mov    $0x1,%eax
   0x00000000004011be <+113>:	mov    $0x6042f0,%edx
   0x00000000004011c3 <+118>:	cmp    $0x1,%ecx
   0x00000000004011c6 <+121>:	jle    0x4011d3 <phase_6+134>
   0x00000000004011c8 <+123>:	mov    0x8(%rdx),%rdx
   0x00000000004011cc <+127>:	add    $0x1,%eax
   0x00000000004011cf <+130>:	cmp    %ecx,%eax
   0x00000000004011d1 <+132>:	jne    0x4011c8 <phase_6+123>
   0x00000000004011d3 <+134>:	mov    %rdx,(%rsp,%rsi,8)
   0x00000000004011d7 <+138>:	add    $0x1,%rsi
   0x00000000004011db <+142>:	cmp    $0x6,%rsi
   0x00000000004011df <+146>:	jne    0x4011b5 <phase_6+104>
   0x00000000004011e1 <+148>:	mov    (%rsp),%rbx
   0x00000000004011e5 <+152>:	mov    0x8(%rsp),%rax
   0x00000000004011ea <+157>:	mov    %rax,0x8(%rbx)
   0x00000000004011ee <+161>:	mov    0x10(%rsp),%rdx
   0x00000000004011f3 <+166>:	mov    %rdx,0x8(%rax)
   0x00000000004011f7 <+170>:	mov    0x18(%rsp),%rax
   0x00000000004011fc <+175>:	mov    %rax,0x8(%rdx)
   0x0000000000401200 <+179>:	mov    0x20(%rsp),%rdx
   0x0000000000401205 <+184>:	mov    %rdx,0x8(%rax)
   0x0000000000401209 <+188>:	mov    0x28(%rsp),%rax
   0x000000000040120e <+193>:	mov    %rax,0x8(%rdx)
   0x0000000000401212 <+197>:	movq   $0x0,0x8(%rax)
   0x000000000040121a <+205>:	mov    $0x5,%ebp
   0x000000000040121f <+210>:	jmp    0x40122a <phase_6+221>
   0x0000000000401221 <+212>:	mov    0x8(%rbx),%rbx
   0x0000000000401225 <+216>:	sub    $0x1,%ebp
   0x0000000000401228 <+219>:	je     0x40123b <phase_6+238>
   0x000000000040122a <+221>:	mov    0x8(%rbx),%rax
   0x000000000040122e <+225>:	mov    (%rax),%eax
   0x0000000000401230 <+227>:	cmp    %eax,(%rbx)
   0x0000000000401232 <+229>:	jle    0x401221 <phase_6+212>
   0x0000000000401234 <+231>:	call   0x401681 <explode_bomb>
   0x0000000000401239 <+236>:	jmp    0x401221 <phase_6+212>
   0x000000000040123b <+238>:	cmpl   $0x21,0x2035ce(%rip)        # 0x604810 <trap>
   0x0000000000401242 <+245>:	je     0x40124f <phase_6+258>
   0x0000000000401244 <+247>:	add    $0x58,%rsp
   0x0000000000401248 <+251>:	pop    %rbx
   0x0000000000401249 <+252>:	pop    %rbp
   0x000000000040124a <+253>:	pop    %r12
   0x000000000040124c <+255>:	pop    %r13
   0x000000000040124e <+257>:	ret    
   0x000000000040124f <+258>:	call   0x401681 <explode_bomb>
   0x0000000000401254 <+263>:	jmp    0x401244 <phase_6+247>
End of assembler dump.
