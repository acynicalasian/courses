Dump of assembler code for function s3cret_phase:
   0x0000000000401293 <+0>:	push   %rbx
   0x0000000000401294 <+1>:	call   0x401945 <read_line>
   0x0000000000401299 <+6>:	mov    %rax,%rdi
   0x000000000040129c <+9>:	mov    $0xa,%edx
   0x00000000004012a1 <+14>:	mov    $0x0,%esi
   0x00000000004012a6 <+19>:	call   0x400c80 <strtol@plt>
   0x00000000004012ab <+24>:	mov    %rax,%rbx
   0x00000000004012ae <+27>:	lea    -0x1(%rax),%eax
   0x00000000004012b1 <+30>:	cmp    $0x3e8,%eax
   0x00000000004012b6 <+35>:	ja     0x4012e4 <s3cret_phase+81>
   0x00000000004012b8 <+37>:	mov    %ebx,%esi
   0x00000000004012ba <+39>:	mov    $0x604110,%edi
   0x00000000004012bf <+44>:	call   0x401256 <fun7>
   0x00000000004012c4 <+49>:	cmp    $0x2,%eax
   0x00000000004012c7 <+52>:	jne    0x4012eb <s3cret_phase+88>
   0x00000000004012c9 <+54>:	mov    $0x402780,%edi
   0x00000000004012ce <+59>:	call   0x400bb0 <puts@plt>
   0x00000000004012d3 <+64>:	mov    $0x4027a8,%edi
   0x00000000004012d8 <+69>:	call   0x400bb0 <puts@plt>
   0x00000000004012dd <+74>:	call   0x401646 <phase_defused>
   0x00000000004012e2 <+79>:	pop    %rbx
   0x00000000004012e3 <+80>:	ret    
   0x00000000004012e4 <+81>:	call   0x401681 <explode_bomb>
   0x00000000004012e9 <+86>:	jmp    0x4012b8 <s3cret_phase+37>
   0x00000000004012eb <+88>:	call   0x401681 <explode_bomb>
   0x00000000004012f0 <+93>:	jmp    0x4012c9 <s3cret_phase+54>
End of assembler dump.
