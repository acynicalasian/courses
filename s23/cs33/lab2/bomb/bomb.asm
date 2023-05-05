
./bomb:     file format elf64-x86-64


Disassembly of section .init:

0000000000400b38 <_init>:
  400b38:	48 83 ec 08          	sub    $0x8,%rsp
  400b3c:	48 8b 05 b5 34 20 00 	mov    0x2034b5(%rip),%rax        # 603ff8 <__gmon_start__>
  400b43:	48 85 c0             	test   %rax,%rax
  400b46:	74 05                	je     400b4d <_init+0x15>
  400b48:	e8 23 01 00 00       	callq  400c70 <__gmon_start__@plt>
  400b4d:	48 83 c4 08          	add    $0x8,%rsp
  400b51:	c3                   	retq   

Disassembly of section .plt:

0000000000400b60 <.plt>:
  400b60:	ff 35 a2 34 20 00    	pushq  0x2034a2(%rip)        # 604008 <_GLOBAL_OFFSET_TABLE_+0x8>
  400b66:	ff 25 a4 34 20 00    	jmpq   *0x2034a4(%rip)        # 604010 <_GLOBAL_OFFSET_TABLE_+0x10>
  400b6c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000400b70 <getenv@plt>:
  400b70:	ff 25 a2 34 20 00    	jmpq   *0x2034a2(%rip)        # 604018 <getenv@GLIBC_2.2.5>
  400b76:	68 00 00 00 00       	pushq  $0x0
  400b7b:	e9 e0 ff ff ff       	jmpq   400b60 <.plt>

0000000000400b80 <strcasecmp@plt>:
  400b80:	ff 25 9a 34 20 00    	jmpq   *0x20349a(%rip)        # 604020 <strcasecmp@GLIBC_2.2.5>
  400b86:	68 01 00 00 00       	pushq  $0x1
  400b8b:	e9 d0 ff ff ff       	jmpq   400b60 <.plt>

0000000000400b90 <__errno_location@plt>:
  400b90:	ff 25 92 34 20 00    	jmpq   *0x203492(%rip)        # 604028 <__errno_location@GLIBC_2.2.5>
  400b96:	68 02 00 00 00       	pushq  $0x2
  400b9b:	e9 c0 ff ff ff       	jmpq   400b60 <.plt>

0000000000400ba0 <strcpy@plt>:
  400ba0:	ff 25 8a 34 20 00    	jmpq   *0x20348a(%rip)        # 604030 <strcpy@GLIBC_2.2.5>
  400ba6:	68 03 00 00 00       	pushq  $0x3
  400bab:	e9 b0 ff ff ff       	jmpq   400b60 <.plt>

0000000000400bb0 <puts@plt>:
  400bb0:	ff 25 82 34 20 00    	jmpq   *0x203482(%rip)        # 604038 <puts@GLIBC_2.2.5>
  400bb6:	68 04 00 00 00       	pushq  $0x4
  400bbb:	e9 a0 ff ff ff       	jmpq   400b60 <.plt>

0000000000400bc0 <write@plt>:
  400bc0:	ff 25 7a 34 20 00    	jmpq   *0x20347a(%rip)        # 604040 <write@GLIBC_2.2.5>
  400bc6:	68 05 00 00 00       	pushq  $0x5
  400bcb:	e9 90 ff ff ff       	jmpq   400b60 <.plt>

0000000000400bd0 <strlen@plt>:
  400bd0:	ff 25 72 34 20 00    	jmpq   *0x203472(%rip)        # 604048 <strlen@GLIBC_2.2.5>
  400bd6:	68 06 00 00 00       	pushq  $0x6
  400bdb:	e9 80 ff ff ff       	jmpq   400b60 <.plt>

0000000000400be0 <printf@plt>:
  400be0:	ff 25 6a 34 20 00    	jmpq   *0x20346a(%rip)        # 604050 <printf@GLIBC_2.2.5>
  400be6:	68 07 00 00 00       	pushq  $0x7
  400beb:	e9 70 ff ff ff       	jmpq   400b60 <.plt>

0000000000400bf0 <alarm@plt>:
  400bf0:	ff 25 62 34 20 00    	jmpq   *0x203462(%rip)        # 604058 <alarm@GLIBC_2.2.5>
  400bf6:	68 08 00 00 00       	pushq  $0x8
  400bfb:	e9 60 ff ff ff       	jmpq   400b60 <.plt>

0000000000400c00 <close@plt>:
  400c00:	ff 25 5a 34 20 00    	jmpq   *0x20345a(%rip)        # 604060 <close@GLIBC_2.2.5>
  400c06:	68 09 00 00 00       	pushq  $0x9
  400c0b:	e9 50 ff ff ff       	jmpq   400b60 <.plt>

0000000000400c10 <read@plt>:
  400c10:	ff 25 52 34 20 00    	jmpq   *0x203452(%rip)        # 604068 <read@GLIBC_2.2.5>
  400c16:	68 0a 00 00 00       	pushq  $0xa
  400c1b:	e9 40 ff ff ff       	jmpq   400b60 <.plt>

0000000000400c20 <__libc_start_main@plt>:
  400c20:	ff 25 4a 34 20 00    	jmpq   *0x20344a(%rip)        # 604070 <__libc_start_main@GLIBC_2.2.5>
  400c26:	68 0b 00 00 00       	pushq  $0xb
  400c2b:	e9 30 ff ff ff       	jmpq   400b60 <.plt>

0000000000400c30 <fgets@plt>:
  400c30:	ff 25 42 34 20 00    	jmpq   *0x203442(%rip)        # 604078 <fgets@GLIBC_2.2.5>
  400c36:	68 0c 00 00 00       	pushq  $0xc
  400c3b:	e9 20 ff ff ff       	jmpq   400b60 <.plt>

0000000000400c40 <signal@plt>:
  400c40:	ff 25 3a 34 20 00    	jmpq   *0x20343a(%rip)        # 604080 <signal@GLIBC_2.2.5>
  400c46:	68 0d 00 00 00       	pushq  $0xd
  400c4b:	e9 10 ff ff ff       	jmpq   400b60 <.plt>

0000000000400c50 <gethostbyname@plt>:
  400c50:	ff 25 32 34 20 00    	jmpq   *0x203432(%rip)        # 604088 <gethostbyname@GLIBC_2.2.5>
  400c56:	68 0e 00 00 00       	pushq  $0xe
  400c5b:	e9 00 ff ff ff       	jmpq   400b60 <.plt>

0000000000400c60 <fprintf@plt>:
  400c60:	ff 25 2a 34 20 00    	jmpq   *0x20342a(%rip)        # 604090 <fprintf@GLIBC_2.2.5>
  400c66:	68 0f 00 00 00       	pushq  $0xf
  400c6b:	e9 f0 fe ff ff       	jmpq   400b60 <.plt>

0000000000400c70 <__gmon_start__@plt>:
  400c70:	ff 25 22 34 20 00    	jmpq   *0x203422(%rip)        # 604098 <__gmon_start__>
  400c76:	68 10 00 00 00       	pushq  $0x10
  400c7b:	e9 e0 fe ff ff       	jmpq   400b60 <.plt>

0000000000400c80 <strtol@plt>:
  400c80:	ff 25 1a 34 20 00    	jmpq   *0x20341a(%rip)        # 6040a0 <strtol@GLIBC_2.2.5>
  400c86:	68 11 00 00 00       	pushq  $0x11
  400c8b:	e9 d0 fe ff ff       	jmpq   400b60 <.plt>

0000000000400c90 <fflush@plt>:
  400c90:	ff 25 12 34 20 00    	jmpq   *0x203412(%rip)        # 6040a8 <fflush@GLIBC_2.2.5>
  400c96:	68 12 00 00 00       	pushq  $0x12
  400c9b:	e9 c0 fe ff ff       	jmpq   400b60 <.plt>

0000000000400ca0 <__isoc99_sscanf@plt>:
  400ca0:	ff 25 0a 34 20 00    	jmpq   *0x20340a(%rip)        # 6040b0 <__isoc99_sscanf@GLIBC_2.7>
  400ca6:	68 13 00 00 00       	pushq  $0x13
  400cab:	e9 b0 fe ff ff       	jmpq   400b60 <.plt>

0000000000400cb0 <memmove@plt>:
  400cb0:	ff 25 02 34 20 00    	jmpq   *0x203402(%rip)        # 6040b8 <memmove@GLIBC_2.2.5>
  400cb6:	68 14 00 00 00       	pushq  $0x14
  400cbb:	e9 a0 fe ff ff       	jmpq   400b60 <.plt>

0000000000400cc0 <fopen@plt>:
  400cc0:	ff 25 fa 33 20 00    	jmpq   *0x2033fa(%rip)        # 6040c0 <fopen@GLIBC_2.2.5>
  400cc6:	68 15 00 00 00       	pushq  $0x15
  400ccb:	e9 90 fe ff ff       	jmpq   400b60 <.plt>

0000000000400cd0 <gethostname@plt>:
  400cd0:	ff 25 f2 33 20 00    	jmpq   *0x2033f2(%rip)        # 6040c8 <gethostname@GLIBC_2.2.5>
  400cd6:	68 16 00 00 00       	pushq  $0x16
  400cdb:	e9 80 fe ff ff       	jmpq   400b60 <.plt>

0000000000400ce0 <sprintf@plt>:
  400ce0:	ff 25 ea 33 20 00    	jmpq   *0x2033ea(%rip)        # 6040d0 <sprintf@GLIBC_2.2.5>
  400ce6:	68 17 00 00 00       	pushq  $0x17
  400ceb:	e9 70 fe ff ff       	jmpq   400b60 <.plt>

0000000000400cf0 <exit@plt>:
  400cf0:	ff 25 e2 33 20 00    	jmpq   *0x2033e2(%rip)        # 6040d8 <exit@GLIBC_2.2.5>
  400cf6:	68 18 00 00 00       	pushq  $0x18
  400cfb:	e9 60 fe ff ff       	jmpq   400b60 <.plt>

0000000000400d00 <connect@plt>:
  400d00:	ff 25 da 33 20 00    	jmpq   *0x2033da(%rip)        # 6040e0 <connect@GLIBC_2.2.5>
  400d06:	68 19 00 00 00       	pushq  $0x19
  400d0b:	e9 50 fe ff ff       	jmpq   400b60 <.plt>

0000000000400d10 <sleep@plt>:
  400d10:	ff 25 d2 33 20 00    	jmpq   *0x2033d2(%rip)        # 6040e8 <sleep@GLIBC_2.2.5>
  400d16:	68 1a 00 00 00       	pushq  $0x1a
  400d1b:	e9 40 fe ff ff       	jmpq   400b60 <.plt>

0000000000400d20 <__ctype_b_loc@plt>:
  400d20:	ff 25 ca 33 20 00    	jmpq   *0x2033ca(%rip)        # 6040f0 <__ctype_b_loc@GLIBC_2.3>
  400d26:	68 1b 00 00 00       	pushq  $0x1b
  400d2b:	e9 30 fe ff ff       	jmpq   400b60 <.plt>

0000000000400d30 <socket@plt>:
  400d30:	ff 25 c2 33 20 00    	jmpq   *0x2033c2(%rip)        # 6040f8 <socket@GLIBC_2.2.5>
  400d36:	68 1c 00 00 00       	pushq  $0x1c
  400d3b:	e9 20 fe ff ff       	jmpq   400b60 <.plt>

Disassembly of section .text:

0000000000400d40 <_start>:
  400d40:	31 ed                	xor    %ebp,%ebp
  400d42:	49 89 d1             	mov    %rdx,%r9
  400d45:	5e                   	pop    %rsi
  400d46:	48 89 e2             	mov    %rsp,%rdx
  400d49:	48 83 e4 f0          	and    $0xfffffffffffffff0,%rsp
  400d4d:	50                   	push   %rax
  400d4e:	54                   	push   %rsp
  400d4f:	49 c7 c0 b0 25 40 00 	mov    $0x4025b0,%r8
  400d56:	48 c7 c1 40 25 40 00 	mov    $0x402540,%rcx
  400d5d:	48 c7 c7 12 0e 40 00 	mov    $0x400e12,%rdi
  400d64:	e8 b7 fe ff ff       	callq  400c20 <__libc_start_main@plt>
  400d69:	f4                   	hlt    
  400d6a:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

0000000000400d70 <deregister_tm_clones>:
  400d70:	b8 80 47 60 00       	mov    $0x604780,%eax
  400d75:	48 3d 80 47 60 00    	cmp    $0x604780,%rax
  400d7b:	74 13                	je     400d90 <deregister_tm_clones+0x20>
  400d7d:	b8 00 00 00 00       	mov    $0x0,%eax
  400d82:	48 85 c0             	test   %rax,%rax
  400d85:	74 09                	je     400d90 <deregister_tm_clones+0x20>
  400d87:	bf 80 47 60 00       	mov    $0x604780,%edi
  400d8c:	ff e0                	jmpq   *%rax
  400d8e:	66 90                	xchg   %ax,%ax
  400d90:	c3                   	retq   
  400d91:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)
  400d96:	66 2e 0f 1f 84 00 00 	nopw   %cs:0x0(%rax,%rax,1)
  400d9d:	00 00 00 

0000000000400da0 <register_tm_clones>:
  400da0:	be 80 47 60 00       	mov    $0x604780,%esi
  400da5:	48 81 ee 80 47 60 00 	sub    $0x604780,%rsi
  400dac:	48 89 f0             	mov    %rsi,%rax
  400daf:	48 c1 ee 3f          	shr    $0x3f,%rsi
  400db3:	48 c1 f8 03          	sar    $0x3,%rax
  400db7:	48 01 c6             	add    %rax,%rsi
  400dba:	48 d1 fe             	sar    %rsi
  400dbd:	74 11                	je     400dd0 <register_tm_clones+0x30>
  400dbf:	b8 00 00 00 00       	mov    $0x0,%eax
  400dc4:	48 85 c0             	test   %rax,%rax
  400dc7:	74 07                	je     400dd0 <register_tm_clones+0x30>
  400dc9:	bf 80 47 60 00       	mov    $0x604780,%edi
  400dce:	ff e0                	jmpq   *%rax
  400dd0:	c3                   	retq   
  400dd1:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)
  400dd6:	66 2e 0f 1f 84 00 00 	nopw   %cs:0x0(%rax,%rax,1)
  400ddd:	00 00 00 

0000000000400de0 <__do_global_dtors_aux>:
  400de0:	80 3d b1 39 20 00 00 	cmpb   $0x0,0x2039b1(%rip)        # 604798 <completed.0>
  400de7:	75 17                	jne    400e00 <__do_global_dtors_aux+0x20>
  400de9:	55                   	push   %rbp
  400dea:	48 89 e5             	mov    %rsp,%rbp
  400ded:	e8 7e ff ff ff       	callq  400d70 <deregister_tm_clones>
  400df2:	c6 05 9f 39 20 00 01 	movb   $0x1,0x20399f(%rip)        # 604798 <completed.0>
  400df9:	5d                   	pop    %rbp
  400dfa:	c3                   	retq   
  400dfb:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)
  400e00:	c3                   	retq   
  400e01:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)
  400e06:	66 2e 0f 1f 84 00 00 	nopw   %cs:0x0(%rax,%rax,1)
  400e0d:	00 00 00 

0000000000400e10 <frame_dummy>:
  400e10:	eb 8e                	jmp    400da0 <register_tm_clones>

0000000000400e12 <main>:
  400e12:	55                   	push   %rbp
  400e13:	53                   	push   %rbx
  400e14:	48 83 ec 08          	sub    $0x8,%rsp
  400e18:	89 fb                	mov    %edi,%ebx
  400e1a:	83 ff 01             	cmp    $0x1,%edi
  400e1d:	0f 84 e5 00 00 00    	je     400f08 <main+0xf6>
  400e23:	48 89 f5             	mov    %rsi,%rbp
  400e26:	83 ff 02             	cmp    $0x2,%edi
  400e29:	0f 85 08 01 00 00    	jne    400f37 <main+0x125>
  400e2f:	48 8b 7e 08          	mov    0x8(%rsi),%rdi
  400e33:	be c4 25 40 00       	mov    $0x4025c4,%esi
  400e38:	e8 83 fe ff ff       	callq  400cc0 <fopen@plt>
  400e3d:	48 89 05 5c 39 20 00 	mov    %rax,0x20395c(%rip)        # 6047a0 <infile>
  400e44:	48 85 c0             	test   %rax,%rax
  400e47:	0f 84 ce 00 00 00    	je     400f1b <main+0x109>
  400e4d:	e8 86 05 00 00       	callq  4013d8 <initialize_bomb>
  400e52:	89 df                	mov    %ebx,%edi
  400e54:	b8 00 00 00 00       	mov    $0x0,%eax
  400e59:	e8 2f 06 00 00       	callq  40148d <welcome_message>
  400e5e:	e8 e2 0a 00 00       	callq  401945 <read_line>
  400e63:	48 89 c7             	mov    %rax,%rdi
  400e66:	e8 e8 00 00 00       	callq  400f53 <phase_1>
  400e6b:	e8 d6 07 00 00       	callq  401646 <phase_defused>
  400e70:	bf 00 26 40 00       	mov    $0x402600,%edi
  400e75:	e8 36 fd ff ff       	callq  400bb0 <puts@plt>
  400e7a:	e8 c6 0a 00 00       	callq  401945 <read_line>
  400e7f:	48 89 c7             	mov    %rax,%rdi
  400e82:	e8 eb 00 00 00       	callq  400f72 <phase_2>
  400e87:	e8 ba 07 00 00       	callq  401646 <phase_defused>
  400e8c:	bf 40 26 40 00       	mov    $0x402640,%edi
  400e91:	e8 1a fd ff ff       	callq  400bb0 <puts@plt>
  400e96:	e8 aa 0a 00 00       	callq  401945 <read_line>
  400e9b:	48 89 c7             	mov    %rax,%rdi
  400e9e:	e8 74 01 00 00       	callq  401017 <phase_3>
  400ea3:	e8 9e 07 00 00       	callq  401646 <phase_defused>
  400ea8:	bf 68 26 40 00       	mov    $0x402668,%edi
  400ead:	e8 fe fc ff ff       	callq  400bb0 <puts@plt>
  400eb2:	e8 8e 0a 00 00       	callq  401945 <read_line>
  400eb7:	48 89 c7             	mov    %rax,%rdi
  400eba:	e8 f6 01 00 00       	callq  4010b5 <phase_4>
  400ebf:	e8 82 07 00 00       	callq  401646 <phase_defused>
  400ec4:	bf 90 26 40 00       	mov    $0x402690,%edi
  400ec9:	e8 e2 fc ff ff       	callq  400bb0 <puts@plt>
  400ece:	e8 72 0a 00 00       	callq  401945 <read_line>
  400ed3:	48 89 c7             	mov    %rax,%rdi
  400ed6:	e8 2d 02 00 00       	callq  401108 <phase_5>
  400edb:	e8 66 07 00 00       	callq  401646 <phase_defused>
  400ee0:	bf c8 26 40 00       	mov    $0x4026c8,%edi
  400ee5:	e8 c6 fc ff ff       	callq  400bb0 <puts@plt>
  400eea:	e8 56 0a 00 00       	callq  401945 <read_line>
  400eef:	48 89 c7             	mov    %rax,%rdi
  400ef2:	e8 56 02 00 00       	callq  40114d <phase_6>
  400ef7:	e8 4a 07 00 00       	callq  401646 <phase_defused>
  400efc:	b8 00 00 00 00       	mov    $0x0,%eax
  400f01:	48 83 c4 08          	add    $0x8,%rsp
  400f05:	5b                   	pop    %rbx
  400f06:	5d                   	pop    %rbp
  400f07:	c3                   	retq   
  400f08:	48 8b 05 79 38 20 00 	mov    0x203879(%rip),%rax        # 604788 <stdin@@GLIBC_2.2.5>
  400f0f:	48 89 05 8a 38 20 00 	mov    %rax,0x20388a(%rip)        # 6047a0 <infile>
  400f16:	e9 32 ff ff ff       	jmpq   400e4d <main+0x3b>
  400f1b:	48 8b 55 08          	mov    0x8(%rbp),%rdx
  400f1f:	48 8b 75 00          	mov    0x0(%rbp),%rsi
  400f23:	bf c6 25 40 00       	mov    $0x4025c6,%edi
  400f28:	e8 b3 fc ff ff       	callq  400be0 <printf@plt>
  400f2d:	bf 08 00 00 00       	mov    $0x8,%edi
  400f32:	e8 b9 fd ff ff       	callq  400cf0 <exit@plt>
  400f37:	48 8b 36             	mov    (%rsi),%rsi
  400f3a:	bf e3 25 40 00       	mov    $0x4025e3,%edi
  400f3f:	b8 00 00 00 00       	mov    $0x0,%eax
  400f44:	e8 97 fc ff ff       	callq  400be0 <printf@plt>
  400f49:	bf 08 00 00 00       	mov    $0x8,%edi
  400f4e:	e8 9d fd ff ff       	callq  400cf0 <exit@plt>

0000000000400f53 <phase_1>:
  400f53:	53                   	push   %rbx
  400f54:	48 89 fb             	mov    %rdi,%rbx
  400f57:	80 7f 01 00          	cmpb   $0x0,0x1(%rdi)
  400f5b:	75 07                	jne    400f64 <phase_1+0x11>
  400f5d:	80 3b 6d             	cmpb   $0x6d,(%rbx)
  400f60:	75 09                	jne    400f6b <phase_1+0x18>
  400f62:	5b                   	pop    %rbx
  400f63:	c3                   	retq   
  400f64:	e8 18 07 00 00       	callq  401681 <explode_bomb>
  400f69:	eb f2                	jmp    400f5d <phase_1+0xa>
  400f6b:	e8 11 07 00 00       	callq  401681 <explode_bomb>
  400f70:	eb f0                	jmp    400f62 <phase_1+0xf>

0000000000400f72 <phase_2>:
  400f72:	41 54                	push   %r12
  400f74:	55                   	push   %rbp
  400f75:	53                   	push   %rbx
  400f76:	48 83 ec 20          	sub    $0x20,%rsp
  400f7a:	48 89 e6             	mov    %rsp,%rsi
  400f7d:	e8 84 09 00 00       	callq  401906 <read_six_numbers>
  400f82:	83 3c 24 04          	cmpl   $0x4,(%rsp)
  400f86:	7e 0a                	jle    400f92 <phase_2+0x20>
  400f88:	48 89 e3             	mov    %rsp,%rbx
  400f8b:	bd 01 00 00 00       	mov    $0x1,%ebp
  400f90:	eb 1e                	jmp    400fb0 <phase_2+0x3e>
  400f92:	e8 ea 06 00 00       	callq  401681 <explode_bomb>
  400f97:	eb ef                	jmp    400f88 <phase_2+0x16>
  400f99:	41 8b 44 24 04       	mov    0x4(%r12),%eax
  400f9e:	89 05 6c 38 20 00    	mov    %eax,0x20386c(%rip)        # 604810 <trap>
  400fa4:	83 c5 01             	add    $0x1,%ebp
  400fa7:	48 83 c3 04          	add    $0x4,%rbx
  400fab:	83 fd 06             	cmp    $0x6,%ebp
  400fae:	74 13                	je     400fc3 <phase_2+0x51>
  400fb0:	49 89 dc             	mov    %rbx,%r12
  400fb3:	8b 03                	mov    (%rbx),%eax
  400fb5:	29 e8                	sub    %ebp,%eax
  400fb7:	39 43 04             	cmp    %eax,0x4(%rbx)
  400fba:	74 dd                	je     400f99 <phase_2+0x27>
  400fbc:	e8 c0 06 00 00       	callq  401681 <explode_bomb>
  400fc1:	eb d6                	jmp    400f99 <phase_2+0x27>
  400fc3:	48 83 c4 20          	add    $0x20,%rsp
  400fc7:	5b                   	pop    %rbx
  400fc8:	5d                   	pop    %rbp
  400fc9:	41 5c                	pop    %r12
  400fcb:	c3                   	retq   

0000000000400fcc <func_switch>:
  400fcc:	83 ff 07             	cmp    $0x7,%edi
  400fcf:	77 2d                	ja     400ffe <func_switch+0x32>
  400fd1:	89 ff                	mov    %edi,%edi
  400fd3:	ff 24 fd 00 27 40 00 	jmpq   *0x402700(,%rdi,8)
  400fda:	b8 5d 01 00 00       	mov    $0x15d,%eax
  400fdf:	c3                   	retq   
  400fe0:	b8 f7 00 00 00       	mov    $0xf7,%eax
  400fe5:	c3                   	retq   
  400fe6:	b8 ea 01 00 00       	mov    $0x1ea,%eax
  400feb:	c3                   	retq   
  400fec:	b8 2b 01 00 00       	mov    $0x12b,%eax
  400ff1:	c3                   	retq   
  400ff2:	b8 db 01 00 00       	mov    $0x1db,%eax
  400ff7:	c3                   	retq   
  400ff8:	b8 ad 00 00 00       	mov    $0xad,%eax
  400ffd:	c3                   	retq   
  400ffe:	48 83 ec 08          	sub    $0x8,%rsp
  401002:	e8 7a 06 00 00       	callq  401681 <explode_bomb>
  401007:	b8 00 00 00 00       	mov    $0x0,%eax
  40100c:	48 83 c4 08          	add    $0x8,%rsp
  401010:	c3                   	retq   
  401011:	b8 71 00 00 00       	mov    $0x71,%eax
  401016:	c3                   	retq   

0000000000401017 <phase_3>:
  401017:	53                   	push   %rbx
  401018:	48 83 ec 10          	sub    $0x10,%rsp
  40101c:	c7 44 24 0c 00 00 00 	movl   $0x0,0xc(%rsp)
  401023:	00 
  401024:	c7 44 24 08 00 00 00 	movl   $0x0,0x8(%rsp)
  40102b:	00 
  40102c:	48 8d 4c 24 08       	lea    0x8(%rsp),%rcx
  401031:	48 8d 54 24 0c       	lea    0xc(%rsp),%rdx
  401036:	be 51 2e 40 00       	mov    $0x402e51,%esi
  40103b:	b8 00 00 00 00       	mov    $0x0,%eax
  401040:	e8 5b fc ff ff       	callq  400ca0 <__isoc99_sscanf@plt>
  401045:	83 f8 01             	cmp    $0x1,%eax
  401048:	7e 0a                	jle    401054 <phase_3+0x3d>
  40104a:	8b 44 24 08          	mov    0x8(%rsp),%eax
  40104e:	39 44 24 0c          	cmp    %eax,0xc(%rsp)
  401052:	75 05                	jne    401059 <phase_3+0x42>
  401054:	e8 28 06 00 00       	callq  401681 <explode_bomb>
  401059:	8b 7c 24 0c          	mov    0xc(%rsp),%edi
  40105d:	e8 6a ff ff ff       	callq  400fcc <func_switch>
  401062:	89 c3                	mov    %eax,%ebx
  401064:	8b 7c 24 08          	mov    0x8(%rsp),%edi
  401068:	e8 5f ff ff ff       	callq  400fcc <func_switch>
  40106d:	39 c3                	cmp    %eax,%ebx
  40106f:	75 06                	jne    401077 <phase_3+0x60>
  401071:	48 83 c4 10          	add    $0x10,%rsp
  401075:	5b                   	pop    %rbx
  401076:	c3                   	retq   
  401077:	e8 05 06 00 00       	callq  401681 <explode_bomb>
  40107c:	eb f3                	jmp    401071 <phase_3+0x5a>

000000000040107e <func4>:
  40107e:	b8 00 00 00 00       	mov    $0x0,%eax
  401083:	85 ff                	test   %edi,%edi
  401085:	7e 2d                	jle    4010b4 <func4+0x36>
  401087:	41 54                	push   %r12
  401089:	55                   	push   %rbp
  40108a:	53                   	push   %rbx
  40108b:	89 fb                	mov    %edi,%ebx
  40108d:	89 f5                	mov    %esi,%ebp
  40108f:	89 f0                	mov    %esi,%eax
  401091:	83 ff 01             	cmp    $0x1,%edi
  401094:	74 19                	je     4010af <func4+0x31>
  401096:	8d 7f ff             	lea    -0x1(%rdi),%edi
  401099:	e8 e0 ff ff ff       	callq  40107e <func4>
  40109e:	44 8d 24 28          	lea    (%rax,%rbp,1),%r12d
  4010a2:	8d 7b fe             	lea    -0x2(%rbx),%edi
  4010a5:	89 ee                	mov    %ebp,%esi
  4010a7:	e8 d2 ff ff ff       	callq  40107e <func4>
  4010ac:	44 01 e0             	add    %r12d,%eax
  4010af:	5b                   	pop    %rbx
  4010b0:	5d                   	pop    %rbp
  4010b1:	41 5c                	pop    %r12
  4010b3:	c3                   	retq   
  4010b4:	c3                   	retq   

00000000004010b5 <phase_4>:
  4010b5:	48 83 ec 18          	sub    $0x18,%rsp
  4010b9:	48 8d 4c 24 0c       	lea    0xc(%rsp),%rcx
  4010be:	48 8d 54 24 08       	lea    0x8(%rsp),%rdx
  4010c3:	be 51 2e 40 00       	mov    $0x402e51,%esi
  4010c8:	b8 00 00 00 00       	mov    $0x0,%eax
  4010cd:	e8 ce fb ff ff       	callq  400ca0 <__isoc99_sscanf@plt>
  4010d2:	83 f8 02             	cmp    $0x2,%eax
  4010d5:	75 0c                	jne    4010e3 <phase_4+0x2e>
  4010d7:	8b 44 24 0c          	mov    0xc(%rsp),%eax
  4010db:	83 e8 02             	sub    $0x2,%eax
  4010de:	83 f8 02             	cmp    $0x2,%eax
  4010e1:	76 05                	jbe    4010e8 <phase_4+0x33>
  4010e3:	e8 99 05 00 00       	callq  401681 <explode_bomb>
  4010e8:	8b 74 24 0c          	mov    0xc(%rsp),%esi
  4010ec:	bf 05 00 00 00       	mov    $0x5,%edi
  4010f1:	e8 88 ff ff ff       	callq  40107e <func4>
  4010f6:	39 44 24 08          	cmp    %eax,0x8(%rsp)
  4010fa:	75 05                	jne    401101 <phase_4+0x4c>
  4010fc:	48 83 c4 18          	add    $0x18,%rsp
  401100:	c3                   	retq   
  401101:	e8 7b 05 00 00       	callq  401681 <explode_bomb>
  401106:	eb f4                	jmp    4010fc <phase_4+0x47>

0000000000401108 <phase_5>:
  401108:	53                   	push   %rbx
  401109:	48 89 fb             	mov    %rdi,%rbx
  40110c:	e8 52 02 00 00       	callq  401363 <string_length>
  401111:	83 f8 06             	cmp    $0x6,%eax
  401114:	75 29                	jne    40113f <phase_5+0x37>
  401116:	48 89 d8             	mov    %rbx,%rax
  401119:	48 8d 7b 06          	lea    0x6(%rbx),%rdi
  40111d:	b9 00 00 00 00       	mov    $0x0,%ecx
  401122:	0f b6 10             	movzbl (%rax),%edx
  401125:	83 e2 0f             	and    $0xf,%edx
  401128:	03 0c 95 40 27 40 00 	add    0x402740(,%rdx,4),%ecx
  40112f:	48 83 c0 01          	add    $0x1,%rax
  401133:	48 39 f8             	cmp    %rdi,%rax
  401136:	75 ea                	jne    401122 <phase_5+0x1a>
  401138:	83 f9 32             	cmp    $0x32,%ecx
  40113b:	75 09                	jne    401146 <phase_5+0x3e>
  40113d:	5b                   	pop    %rbx
  40113e:	c3                   	retq   
  40113f:	e8 3d 05 00 00       	callq  401681 <explode_bomb>
  401144:	eb d0                	jmp    401116 <phase_5+0xe>
  401146:	e8 36 05 00 00       	callq  401681 <explode_bomb>
  40114b:	eb f0                	jmp    40113d <phase_5+0x35>

000000000040114d <phase_6>:
  40114d:	41 55                	push   %r13
  40114f:	41 54                	push   %r12
  401151:	55                   	push   %rbp
  401152:	53                   	push   %rbx
  401153:	48 83 ec 58          	sub    $0x58,%rsp
  401157:	48 8d 74 24 30       	lea    0x30(%rsp),%rsi
  40115c:	e8 a5 07 00 00       	callq  401906 <read_six_numbers>
  401161:	4c 8d 64 24 30       	lea    0x30(%rsp),%r12
  401166:	41 bd 01 00 00 00    	mov    $0x1,%r13d
  40116c:	eb 28                	jmp    401196 <phase_6+0x49>
  40116e:	e8 0e 05 00 00       	callq  401681 <explode_bomb>
  401173:	eb 30                	jmp    4011a5 <phase_6+0x58>
  401175:	48 83 c3 01          	add    $0x1,%rbx
  401179:	83 fb 05             	cmp    $0x5,%ebx
  40117c:	7f 10                	jg     40118e <phase_6+0x41>
  40117e:	8b 44 9c 30          	mov    0x30(%rsp,%rbx,4),%eax
  401182:	39 45 00             	cmp    %eax,0x0(%rbp)
  401185:	75 ee                	jne    401175 <phase_6+0x28>
  401187:	e8 f5 04 00 00       	callq  401681 <explode_bomb>
  40118c:	eb e7                	jmp    401175 <phase_6+0x28>
  40118e:	49 83 c5 01          	add    $0x1,%r13
  401192:	49 83 c4 04          	add    $0x4,%r12
  401196:	4c 89 e5             	mov    %r12,%rbp
  401199:	41 8b 04 24          	mov    (%r12),%eax
  40119d:	83 e8 01             	sub    $0x1,%eax
  4011a0:	83 f8 05             	cmp    $0x5,%eax
  4011a3:	77 c9                	ja     40116e <phase_6+0x21>
  4011a5:	41 83 fd 05          	cmp    $0x5,%r13d
  4011a9:	7f 05                	jg     4011b0 <phase_6+0x63>
  4011ab:	4c 89 eb             	mov    %r13,%rbx
  4011ae:	eb ce                	jmp    40117e <phase_6+0x31>
  4011b0:	be 00 00 00 00       	mov    $0x0,%esi
  4011b5:	8b 4c b4 30          	mov    0x30(%rsp,%rsi,4),%ecx
  4011b9:	b8 01 00 00 00       	mov    $0x1,%eax
  4011be:	ba f0 42 60 00       	mov    $0x6042f0,%edx
  4011c3:	83 f9 01             	cmp    $0x1,%ecx
  4011c6:	7e 0b                	jle    4011d3 <phase_6+0x86>
  4011c8:	48 8b 52 08          	mov    0x8(%rdx),%rdx
  4011cc:	83 c0 01             	add    $0x1,%eax
  4011cf:	39 c8                	cmp    %ecx,%eax
  4011d1:	75 f5                	jne    4011c8 <phase_6+0x7b>
  4011d3:	48 89 14 f4          	mov    %rdx,(%rsp,%rsi,8)
  4011d7:	48 83 c6 01          	add    $0x1,%rsi
  4011db:	48 83 fe 06          	cmp    $0x6,%rsi
  4011df:	75 d4                	jne    4011b5 <phase_6+0x68>
  4011e1:	48 8b 1c 24          	mov    (%rsp),%rbx
  4011e5:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  4011ea:	48 89 43 08          	mov    %rax,0x8(%rbx)
  4011ee:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  4011f3:	48 89 50 08          	mov    %rdx,0x8(%rax)
  4011f7:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  4011fc:	48 89 42 08          	mov    %rax,0x8(%rdx)
  401200:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  401205:	48 89 50 08          	mov    %rdx,0x8(%rax)
  401209:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  40120e:	48 89 42 08          	mov    %rax,0x8(%rdx)
  401212:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  401219:	00 
  40121a:	bd 05 00 00 00       	mov    $0x5,%ebp
  40121f:	eb 09                	jmp    40122a <phase_6+0xdd>
  401221:	48 8b 5b 08          	mov    0x8(%rbx),%rbx
  401225:	83 ed 01             	sub    $0x1,%ebp
  401228:	74 11                	je     40123b <phase_6+0xee>
  40122a:	48 8b 43 08          	mov    0x8(%rbx),%rax
  40122e:	8b 00                	mov    (%rax),%eax
  401230:	39 03                	cmp    %eax,(%rbx)
  401232:	7e ed                	jle    401221 <phase_6+0xd4>
  401234:	e8 48 04 00 00       	callq  401681 <explode_bomb>
  401239:	eb e6                	jmp    401221 <phase_6+0xd4>
  40123b:	83 3d ce 35 20 00 21 	cmpl   $0x21,0x2035ce(%rip)        # 604810 <trap>
  401242:	74 0b                	je     40124f <phase_6+0x102>
  401244:	48 83 c4 58          	add    $0x58,%rsp
  401248:	5b                   	pop    %rbx
  401249:	5d                   	pop    %rbp
  40124a:	41 5c                	pop    %r12
  40124c:	41 5d                	pop    %r13
  40124e:	c3                   	retq   
  40124f:	e8 2d 04 00 00       	callq  401681 <explode_bomb>
  401254:	eb ee                	jmp    401244 <phase_6+0xf7>

0000000000401256 <fun7>:
  401256:	48 85 ff             	test   %rdi,%rdi
  401259:	74 32                	je     40128d <fun7+0x37>
  40125b:	48 83 ec 08          	sub    $0x8,%rsp
  40125f:	8b 17                	mov    (%rdi),%edx
  401261:	39 f2                	cmp    %esi,%edx
  401263:	7f 0c                	jg     401271 <fun7+0x1b>
  401265:	b8 00 00 00 00       	mov    $0x0,%eax
  40126a:	75 12                	jne    40127e <fun7+0x28>
  40126c:	48 83 c4 08          	add    $0x8,%rsp
  401270:	c3                   	retq   
  401271:	48 8b 7f 08          	mov    0x8(%rdi),%rdi
  401275:	e8 dc ff ff ff       	callq  401256 <fun7>
  40127a:	01 c0                	add    %eax,%eax
  40127c:	eb ee                	jmp    40126c <fun7+0x16>
  40127e:	48 8b 7f 10          	mov    0x10(%rdi),%rdi
  401282:	e8 cf ff ff ff       	callq  401256 <fun7>
  401287:	8d 44 00 01          	lea    0x1(%rax,%rax,1),%eax
  40128b:	eb df                	jmp    40126c <fun7+0x16>
  40128d:	b8 ff ff ff ff       	mov    $0xffffffff,%eax
  401292:	c3                   	retq   

0000000000401293 <s3cret_phase>:
  401293:	53                   	push   %rbx
  401294:	e8 ac 06 00 00       	callq  401945 <read_line>
  401299:	48 89 c7             	mov    %rax,%rdi
  40129c:	ba 0a 00 00 00       	mov    $0xa,%edx
  4012a1:	be 00 00 00 00       	mov    $0x0,%esi
  4012a6:	e8 d5 f9 ff ff       	callq  400c80 <strtol@plt>
  4012ab:	48 89 c3             	mov    %rax,%rbx
  4012ae:	8d 40 ff             	lea    -0x1(%rax),%eax
  4012b1:	3d e8 03 00 00       	cmp    $0x3e8,%eax
  4012b6:	77 2c                	ja     4012e4 <s3cret_phase+0x51>
  4012b8:	89 de                	mov    %ebx,%esi
  4012ba:	bf 10 41 60 00       	mov    $0x604110,%edi
  4012bf:	e8 92 ff ff ff       	callq  401256 <fun7>
  4012c4:	83 f8 02             	cmp    $0x2,%eax
  4012c7:	75 22                	jne    4012eb <s3cret_phase+0x58>
  4012c9:	bf 80 27 40 00       	mov    $0x402780,%edi
  4012ce:	e8 dd f8 ff ff       	callq  400bb0 <puts@plt>
  4012d3:	bf a8 27 40 00       	mov    $0x4027a8,%edi
  4012d8:	e8 d3 f8 ff ff       	callq  400bb0 <puts@plt>
  4012dd:	e8 64 03 00 00       	callq  401646 <phase_defused>
  4012e2:	5b                   	pop    %rbx
  4012e3:	c3                   	retq   
  4012e4:	e8 98 03 00 00       	callq  401681 <explode_bomb>
  4012e9:	eb cd                	jmp    4012b8 <s3cret_phase+0x25>
  4012eb:	e8 91 03 00 00       	callq  401681 <explode_bomb>
  4012f0:	eb d7                	jmp    4012c9 <s3cret_phase+0x36>

00000000004012f2 <sig_handler>:
  4012f2:	48 83 ec 08          	sub    $0x8,%rsp
  4012f6:	bf d0 27 40 00       	mov    $0x4027d0,%edi
  4012fb:	e8 b0 f8 ff ff       	callq  400bb0 <puts@plt>
  401300:	bf 03 00 00 00       	mov    $0x3,%edi
  401305:	e8 06 fa ff ff       	callq  400d10 <sleep@plt>
  40130a:	bf 73 2d 40 00       	mov    $0x402d73,%edi
  40130f:	b8 00 00 00 00       	mov    $0x0,%eax
  401314:	e8 c7 f8 ff ff       	callq  400be0 <printf@plt>
  401319:	48 8b 3d 60 34 20 00 	mov    0x203460(%rip),%rdi        # 604780 <stdout@@GLIBC_2.2.5>
  401320:	e8 6b f9 ff ff       	callq  400c90 <fflush@plt>
  401325:	bf 01 00 00 00       	mov    $0x1,%edi
  40132a:	e8 e1 f9 ff ff       	callq  400d10 <sleep@plt>
  40132f:	bf 7b 2d 40 00       	mov    $0x402d7b,%edi
  401334:	e8 77 f8 ff ff       	callq  400bb0 <puts@plt>
  401339:	bf 10 00 00 00       	mov    $0x10,%edi
  40133e:	e8 ad f9 ff ff       	callq  400cf0 <exit@plt>

0000000000401343 <invalid_phase>:
  401343:	48 83 ec 08          	sub    $0x8,%rsp
  401347:	48 89 fe             	mov    %rdi,%rsi
  40134a:	bf 83 2d 40 00       	mov    $0x402d83,%edi
  40134f:	b8 00 00 00 00       	mov    $0x0,%eax
  401354:	e8 87 f8 ff ff       	callq  400be0 <printf@plt>
  401359:	bf 08 00 00 00       	mov    $0x8,%edi
  40135e:	e8 8d f9 ff ff       	callq  400cf0 <exit@plt>

0000000000401363 <string_length>:
  401363:	b8 00 00 00 00       	mov    $0x0,%eax
  401368:	80 3f 00             	cmpb   $0x0,(%rdi)
  40136b:	74 0e                	je     40137b <string_length+0x18>
  40136d:	48 83 c7 01          	add    $0x1,%rdi
  401371:	83 c0 01             	add    $0x1,%eax
  401374:	3d 10 27 00 00       	cmp    $0x2710,%eax
  401379:	75 ed                	jne    401368 <string_length+0x5>
  40137b:	c3                   	retq   

000000000040137c <strings_not_equal>:
  40137c:	41 54                	push   %r12
  40137e:	55                   	push   %rbp
  40137f:	53                   	push   %rbx
  401380:	48 89 fb             	mov    %rdi,%rbx
  401383:	48 89 f5             	mov    %rsi,%rbp
  401386:	e8 d8 ff ff ff       	callq  401363 <string_length>
  40138b:	41 89 c4             	mov    %eax,%r12d
  40138e:	48 89 ef             	mov    %rbp,%rdi
  401391:	e8 cd ff ff ff       	callq  401363 <string_length>
  401396:	89 c2                	mov    %eax,%edx
  401398:	b8 01 00 00 00       	mov    $0x1,%eax
  40139d:	41 39 d4             	cmp    %edx,%r12d
  4013a0:	75 31                	jne    4013d3 <strings_not_equal+0x57>
  4013a2:	0f b6 13             	movzbl (%rbx),%edx
  4013a5:	84 d2                	test   %dl,%dl
  4013a7:	74 1e                	je     4013c7 <strings_not_equal+0x4b>
  4013a9:	b8 00 00 00 00       	mov    $0x0,%eax
  4013ae:	38 54 05 00          	cmp    %dl,0x0(%rbp,%rax,1)
  4013b2:	75 1a                	jne    4013ce <strings_not_equal+0x52>
  4013b4:	48 83 c0 01          	add    $0x1,%rax
  4013b8:	0f b6 14 03          	movzbl (%rbx,%rax,1),%edx
  4013bc:	84 d2                	test   %dl,%dl
  4013be:	75 ee                	jne    4013ae <strings_not_equal+0x32>
  4013c0:	b8 00 00 00 00       	mov    $0x0,%eax
  4013c5:	eb 0c                	jmp    4013d3 <strings_not_equal+0x57>
  4013c7:	b8 00 00 00 00       	mov    $0x0,%eax
  4013cc:	eb 05                	jmp    4013d3 <strings_not_equal+0x57>
  4013ce:	b8 01 00 00 00       	mov    $0x1,%eax
  4013d3:	5b                   	pop    %rbx
  4013d4:	5d                   	pop    %rbp
  4013d5:	41 5c                	pop    %r12
  4013d7:	c3                   	retq   

00000000004013d8 <initialize_bomb>:
  4013d8:	53                   	push   %rbx
  4013d9:	48 81 ec 40 20 00 00 	sub    $0x2040,%rsp
  4013e0:	be f2 12 40 00       	mov    $0x4012f2,%esi
  4013e5:	bf 02 00 00 00       	mov    $0x2,%edi
  4013ea:	e8 51 f8 ff ff       	callq  400c40 <signal@plt>
  4013ef:	be 40 00 00 00       	mov    $0x40,%esi
  4013f4:	48 8d bc 24 00 20 00 	lea    0x2000(%rsp),%rdi
  4013fb:	00 
  4013fc:	e8 cf f8 ff ff       	callq  400cd0 <gethostname@plt>
  401401:	85 c0                	test   %eax,%eax
  401403:	75 43                	jne    401448 <initialize_bomb+0x70>
  401405:	48 8b 3d 74 2f 20 00 	mov    0x202f74(%rip),%rdi        # 604380 <host_table>
  40140c:	bb 88 43 60 00       	mov    $0x604388,%ebx
  401411:	48 85 ff             	test   %rdi,%rdi
  401414:	74 1e                	je     401434 <initialize_bomb+0x5c>
  401416:	48 8d b4 24 00 20 00 	lea    0x2000(%rsp),%rsi
  40141d:	00 
  40141e:	e8 5d f7 ff ff       	callq  400b80 <strcasecmp@plt>
  401423:	85 c0                	test   %eax,%eax
  401425:	74 51                	je     401478 <initialize_bomb+0xa0>
  401427:	48 83 c3 08          	add    $0x8,%rbx
  40142b:	48 8b 7b f8          	mov    -0x8(%rbx),%rdi
  40142f:	48 85 ff             	test   %rdi,%rdi
  401432:	75 e2                	jne    401416 <initialize_bomb+0x3e>
  401434:	bf 40 28 40 00       	mov    $0x402840,%edi
  401439:	e8 72 f7 ff ff       	callq  400bb0 <puts@plt>
  40143e:	bf 08 00 00 00       	mov    $0x8,%edi
  401443:	e8 a8 f8 ff ff       	callq  400cf0 <exit@plt>
  401448:	bf 08 28 40 00       	mov    $0x402808,%edi
  40144d:	e8 5e f7 ff ff       	callq  400bb0 <puts@plt>
  401452:	bf 08 00 00 00       	mov    $0x8,%edi
  401457:	e8 94 f8 ff ff       	callq  400cf0 <exit@plt>
  40145c:	48 89 e6             	mov    %rsp,%rsi
  40145f:	bf 94 2d 40 00       	mov    $0x402d94,%edi
  401464:	b8 00 00 00 00       	mov    $0x0,%eax
  401469:	e8 72 f7 ff ff       	callq  400be0 <printf@plt>
  40146e:	bf 08 00 00 00       	mov    $0x8,%edi
  401473:	e8 78 f8 ff ff       	callq  400cf0 <exit@plt>
  401478:	48 89 e7             	mov    %rsp,%rdi
  40147b:	e8 ad 0e 00 00       	callq  40232d <init_driver>
  401480:	85 c0                	test   %eax,%eax
  401482:	78 d8                	js     40145c <initialize_bomb+0x84>
  401484:	48 81 c4 40 20 00 00 	add    $0x2040,%rsp
  40148b:	5b                   	pop    %rbx
  40148c:	c3                   	retq   

000000000040148d <welcome_message>:
  40148d:	83 ff 01             	cmp    $0x1,%edi
  401490:	74 01                	je     401493 <welcome_message+0x6>
  401492:	c3                   	retq   
  401493:	48 83 ec 08          	sub    $0x8,%rsp
  401497:	bf 78 28 40 00       	mov    $0x402878,%edi
  40149c:	e8 0f f7 ff ff       	callq  400bb0 <puts@plt>
  4014a1:	bf c0 28 40 00       	mov    $0x4028c0,%edi
  4014a6:	e8 05 f7 ff ff       	callq  400bb0 <puts@plt>
  4014ab:	bf 08 29 40 00       	mov    $0x402908,%edi
  4014b0:	e8 fb f6 ff ff       	callq  400bb0 <puts@plt>
  4014b5:	bf 50 29 40 00       	mov    $0x402950,%edi
  4014ba:	e8 f1 f6 ff ff       	callq  400bb0 <puts@plt>
  4014bf:	bf 50 29 40 00       	mov    $0x402950,%edi
  4014c4:	e8 e7 f6 ff ff       	callq  400bb0 <puts@plt>
  4014c9:	bf 98 29 40 00       	mov    $0x402998,%edi
  4014ce:	e8 dd f6 ff ff       	callq  400bb0 <puts@plt>
  4014d3:	bf e0 29 40 00       	mov    $0x4029e0,%edi
  4014d8:	e8 d3 f6 ff ff       	callq  400bb0 <puts@plt>
  4014dd:	bf 28 2a 40 00       	mov    $0x402a28,%edi
  4014e2:	e8 c9 f6 ff ff       	callq  400bb0 <puts@plt>
  4014e7:	bf 70 2a 40 00       	mov    $0x402a70,%edi
  4014ec:	e8 bf f6 ff ff       	callq  400bb0 <puts@plt>
  4014f1:	bf b8 2a 40 00       	mov    $0x402ab8,%edi
  4014f6:	e8 b5 f6 ff ff       	callq  400bb0 <puts@plt>
  4014fb:	bf 00 2b 40 00       	mov    $0x402b00,%edi
  401500:	e8 ab f6 ff ff       	callq  400bb0 <puts@plt>
  401505:	bf 48 2b 40 00       	mov    $0x402b48,%edi
  40150a:	e8 a1 f6 ff ff       	callq  400bb0 <puts@plt>
  40150f:	bf 90 2b 40 00       	mov    $0x402b90,%edi
  401514:	e8 97 f6 ff ff       	callq  400bb0 <puts@plt>
  401519:	bf d8 2b 40 00       	mov    $0x402bd8,%edi
  40151e:	e8 8d f6 ff ff       	callq  400bb0 <puts@plt>
  401523:	bf 20 2c 40 00       	mov    $0x402c20,%edi
  401528:	e8 83 f6 ff ff       	callq  400bb0 <puts@plt>
  40152d:	bf 68 2c 40 00       	mov    $0x402c68,%edi
  401532:	e8 79 f6 ff ff       	callq  400bb0 <puts@plt>
  401537:	48 83 c4 08          	add    $0x8,%rsp
  40153b:	c3                   	retq   

000000000040153c <initialize_bomb_solve>:
  40153c:	c3                   	retq   

000000000040153d <blank_line>:
  40153d:	55                   	push   %rbp
  40153e:	53                   	push   %rbx
  40153f:	48 83 ec 08          	sub    $0x8,%rsp
  401543:	48 89 fd             	mov    %rdi,%rbp
  401546:	0f b6 5d 00          	movzbl 0x0(%rbp),%ebx
  40154a:	84 db                	test   %bl,%bl
  40154c:	74 1e                	je     40156c <blank_line+0x2f>
  40154e:	e8 cd f7 ff ff       	callq  400d20 <__ctype_b_loc@plt>
  401553:	48 83 c5 01          	add    $0x1,%rbp
  401557:	48 0f be db          	movsbq %bl,%rbx
  40155b:	48 8b 00             	mov    (%rax),%rax
  40155e:	f6 44 58 01 20       	testb  $0x20,0x1(%rax,%rbx,2)
  401563:	75 e1                	jne    401546 <blank_line+0x9>
  401565:	b8 00 00 00 00       	mov    $0x0,%eax
  40156a:	eb 05                	jmp    401571 <blank_line+0x34>
  40156c:	b8 01 00 00 00       	mov    $0x1,%eax
  401571:	48 83 c4 08          	add    $0x8,%rsp
  401575:	5b                   	pop    %rbx
  401576:	5d                   	pop    %rbp
  401577:	c3                   	retq   

0000000000401578 <skip>:
  401578:	53                   	push   %rbx
  401579:	48 63 05 94 32 20 00 	movslq 0x203294(%rip),%rax        # 604814 <num_input_strings>
  401580:	48 8d 3c 80          	lea    (%rax,%rax,4),%rdi
  401584:	48 c1 e7 04          	shl    $0x4,%rdi
  401588:	48 81 c7 20 48 60 00 	add    $0x604820,%rdi
  40158f:	48 8b 15 0a 32 20 00 	mov    0x20320a(%rip),%rdx        # 6047a0 <infile>
  401596:	be 50 00 00 00       	mov    $0x50,%esi
  40159b:	e8 90 f6 ff ff       	callq  400c30 <fgets@plt>
  4015a0:	48 89 c3             	mov    %rax,%rbx
  4015a3:	48 85 c0             	test   %rax,%rax
  4015a6:	74 0c                	je     4015b4 <skip+0x3c>
  4015a8:	48 89 c7             	mov    %rax,%rdi
  4015ab:	e8 8d ff ff ff       	callq  40153d <blank_line>
  4015b0:	85 c0                	test   %eax,%eax
  4015b2:	75 c5                	jne    401579 <skip+0x1>
  4015b4:	48 89 d8             	mov    %rbx,%rax
  4015b7:	5b                   	pop    %rbx
  4015b8:	c3                   	retq   

00000000004015b9 <send_msg>:
  4015b9:	48 81 ec 08 40 00 00 	sub    $0x4008,%rsp
  4015c0:	44 8b 05 4d 32 20 00 	mov    0x20324d(%rip),%r8d        # 604814 <num_input_strings>
  4015c7:	41 8d 40 ff          	lea    -0x1(%r8),%eax
  4015cb:	48 98                	cltq   
  4015cd:	48 8d 04 80          	lea    (%rax,%rax,4),%rax
  4015d1:	48 c1 e0 04          	shl    $0x4,%rax
  4015d5:	85 ff                	test   %edi,%edi
  4015d7:	b9 ae 2d 40 00       	mov    $0x402dae,%ecx
  4015dc:	ba b6 2d 40 00       	mov    $0x402db6,%edx
  4015e1:	48 0f 44 ca          	cmove  %rdx,%rcx
  4015e5:	4c 8d 88 20 48 60 00 	lea    0x604820(%rax),%r9
  4015ec:	8b 15 82 2d 20 00    	mov    0x202d82(%rip),%edx        # 604374 <bomb_id>
  4015f2:	be bf 2d 40 00       	mov    $0x402dbf,%esi
  4015f7:	48 8d bc 24 00 20 00 	lea    0x2000(%rsp),%rdi
  4015fe:	00 
  4015ff:	b8 00 00 00 00       	mov    $0x0,%eax
  401604:	e8 d7 f6 ff ff       	callq  400ce0 <sprintf@plt>
  401609:	49 89 e0             	mov    %rsp,%r8
  40160c:	b9 00 00 00 00       	mov    $0x0,%ecx
  401611:	48 8d 94 24 00 20 00 	lea    0x2000(%rsp),%rdx
  401618:	00 
  401619:	be 50 43 60 00       	mov    $0x604350,%esi
  40161e:	bf 68 43 60 00       	mov    $0x604368,%edi
  401623:	e8 ab 0e 00 00       	callq  4024d3 <driver_post>
  401628:	85 c0                	test   %eax,%eax
  40162a:	78 08                	js     401634 <send_msg+0x7b>
  40162c:	48 81 c4 08 40 00 00 	add    $0x4008,%rsp
  401633:	c3                   	retq   
  401634:	48 89 e7             	mov    %rsp,%rdi
  401637:	e8 74 f5 ff ff       	callq  400bb0 <puts@plt>
  40163c:	bf 00 00 00 00       	mov    $0x0,%edi
  401641:	e8 aa f6 ff ff       	callq  400cf0 <exit@plt>

0000000000401646 <phase_defused>:
  401646:	48 83 ec 08          	sub    $0x8,%rsp
  40164a:	bf 01 00 00 00       	mov    $0x1,%edi
  40164f:	e8 65 ff ff ff       	callq  4015b9 <send_msg>
  401654:	83 3d b9 31 20 00 05 	cmpl   $0x5,0x2031b9(%rip)        # 604814 <num_input_strings>
  40165b:	7e 09                	jle    401666 <phase_defused+0x20>
  40165d:	83 3d ac 31 20 00 21 	cmpl   $0x21,0x2031ac(%rip)        # 604810 <trap>
  401664:	75 05                	jne    40166b <phase_defused+0x25>
  401666:	48 83 c4 08          	add    $0x8,%rsp
  40166a:	c3                   	retq   
  40166b:	bf b0 2c 40 00       	mov    $0x402cb0,%edi
  401670:	e8 3b f5 ff ff       	callq  400bb0 <puts@plt>
  401675:	bf e0 2c 40 00       	mov    $0x402ce0,%edi
  40167a:	e8 31 f5 ff ff       	callq  400bb0 <puts@plt>
  40167f:	eb e5                	jmp    401666 <phase_defused+0x20>

0000000000401681 <explode_bomb>:
  401681:	48 81 ec d8 00 00 00 	sub    $0xd8,%rsp
  401688:	83 3d 85 31 20 00 06 	cmpl   $0x6,0x203185(%rip)        # 604814 <num_input_strings>
  40168f:	77 17                	ja     4016a8 <explode_bomb+0x27>
  401691:	8b 05 7d 31 20 00    	mov    0x20317d(%rip),%eax        # 604814 <num_input_strings>
  401697:	ff 24 c5 e8 2f 40 00 	jmpq   *0x402fe8(,%rax,8)
  40169e:	bf cb 2d 40 00       	mov    $0x402dcb,%edi
  4016a3:	e8 08 f5 ff ff       	callq  400bb0 <puts@plt>
  4016a8:	48 b8 53 6f 20 79 6f 	movabs $0x6d20756f79206f53,%rax
  4016af:	75 20 6d 
  4016b2:	48 ba 61 64 65 20 69 	movabs $0x7420746920656461,%rdx
  4016b9:	74 20 74 
  4016bc:	48 89 04 24          	mov    %rax,(%rsp)
  4016c0:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
  4016c5:	48 b8 6f 20 74 68 65 	movabs $0x337320656874206f,%rax
  4016cc:	20 73 33 
  4016cf:	48 ba 63 72 65 74 20 	movabs $0x6168702074657263,%rdx
  4016d6:	70 68 61 
  4016d9:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  4016de:	48 89 54 24 18       	mov    %rdx,0x18(%rsp)
  4016e3:	48 b8 73 65 2e 20 20 	movabs $0x63694e20202e6573,%rax
  4016ea:	4e 69 63 
  4016ed:	48 ba 65 2c 20 62 75 	movabs $0x640a747562202c65,%rdx
  4016f4:	74 0a 64 
  4016f7:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  4016fc:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  401701:	48 b8 6f 6e 27 74 20 	movabs $0x6c65742074276e6f,%rax
  401708:	74 65 6c 
  40170b:	48 ba 6c 20 61 6e 79 	movabs $0x656e6f796e61206c,%rdx
  401712:	6f 6e 65 
  401715:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  40171a:	48 89 54 24 38       	mov    %rdx,0x38(%rsp)
  40171f:	48 b8 20 61 62 6f 75 	movabs $0x692074756f626120,%rax
  401726:	74 20 69 
  401729:	48 ba 74 20 28 65 73 	movabs $0x6365707365282074,%rdx
  401730:	70 65 63 
  401733:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  401738:	48 89 54 24 48       	mov    %rdx,0x48(%rsp)
  40173d:	48 b8 69 61 6c 6c 79 	movabs $0x6e6f20796c6c6169,%rax
  401744:	20 6f 6e 
  401747:	48 ba 20 50 69 61 7a 	movabs $0x29617a7a61695020,%rdx
  40174e:	7a 61 29 
  401751:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  401756:	48 89 54 24 58       	mov    %rdx,0x58(%rsp)
  40175b:	48 b8 2e 0a 49 66 20 	movabs $0x756f792066490a2e,%rax
  401762:	79 6f 75 
  401765:	48 ba 20 64 6f 20 79 	movabs $0x20756f79206f6420,%rdx
  40176c:	6f 75 20 
  40176f:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  401774:	48 89 54 24 68       	mov    %rdx,0x68(%rsp)
  401779:	48 b8 77 6f 6e 27 74 	movabs $0x65672074276e6f77,%rax
  401780:	20 67 65 
  401783:	48 ba 74 20 61 6e 79 	movabs $0x786520796e612074,%rdx
  40178a:	20 65 78 
  40178d:	48 89 44 24 70       	mov    %rax,0x70(%rsp)
  401792:	48 89 54 24 78       	mov    %rdx,0x78(%rsp)
  401797:	48 b8 74 72 61 20 63 	movabs $0x6465726320617274,%rax
  40179e:	72 65 64 
  4017a1:	48 ba 69 74 21 20 61 	movabs $0x77796e6120217469,%rdx
  4017a8:	6e 79 77 
  4017ab:	48 89 84 24 80 00 00 	mov    %rax,0x80(%rsp)
  4017b2:	00 
  4017b3:	48 89 94 24 88 00 00 	mov    %rdx,0x88(%rsp)
  4017ba:	00 
  4017bb:	48 b8 61 79 73 2c 0a 	movabs $0x6e69660a2c737961,%rax
  4017c2:	66 69 6e 
  4017c5:	48 ba 64 69 6e 67 20 	movabs $0x20746920676e6964,%rdx
  4017cc:	69 74 20 
  4017cf:	48 89 84 24 90 00 00 	mov    %rax,0x90(%rsp)
  4017d6:	00 
  4017d7:	48 89 94 24 98 00 00 	mov    %rdx,0x98(%rsp)
  4017de:	00 
  4017df:	48 b8 61 6e 64 20 73 	movabs $0x766c6f7320646e61,%rax
  4017e6:	6f 6c 76 
  4017e9:	48 ba 69 6e 67 20 69 	movabs $0x6120746920676e69,%rdx
  4017f0:	74 20 61 
  4017f3:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  4017fa:	00 
  4017fb:	48 89 94 24 a8 00 00 	mov    %rdx,0xa8(%rsp)
  401802:	00 
  401803:	48 b8 72 65 20 71 75 	movabs $0x6574697571206572,%rax
  40180a:	69 74 65 
  40180d:	48 ba 20 64 69 66 66 	movabs $0x6572656666696420,%rdx
  401814:	65 72 65 
  401817:	48 89 84 24 b0 00 00 	mov    %rax,0xb0(%rsp)
  40181e:	00 
  40181f:	48 89 94 24 b8 00 00 	mov    %rdx,0xb8(%rsp)
  401826:	00 
  401827:	48 b8 6e 74 2e 2e 2e 	movabs $0x2e2e2e746e,%rax
  40182e:	00 00 00 
  401831:	48 89 84 24 c0 00 00 	mov    %rax,0xc0(%rsp)
  401838:	00 
  401839:	83 3d d4 2f 20 00 06 	cmpl   $0x6,0x202fd4(%rip)        # 604814 <num_input_strings>
  401840:	75 0d                	jne    40184f <explode_bomb+0x1ce>
  401842:	83 3d c7 2f 20 00 21 	cmpl   $0x21,0x202fc7(%rip)        # 604810 <trap>
  401849:	0f 84 8c 00 00 00    	je     4018db <explode_bomb+0x25a>
  40184f:	bf 25 2e 40 00       	mov    $0x402e25,%edi
  401854:	e8 57 f3 ff ff       	callq  400bb0 <puts@plt>
  401859:	bf 2e 2e 40 00       	mov    $0x402e2e,%edi
  40185e:	e8 4d f3 ff ff       	callq  400bb0 <puts@plt>
  401863:	bf 00 00 00 00       	mov    $0x0,%edi
  401868:	e8 4c fd ff ff       	callq  4015b9 <send_msg>
  40186d:	bf 50 2d 40 00       	mov    $0x402d50,%edi
  401872:	e8 39 f3 ff ff       	callq  400bb0 <puts@plt>
  401877:	bf 08 00 00 00       	mov    $0x8,%edi
  40187c:	e8 6f f4 ff ff       	callq  400cf0 <exit@plt>
  401881:	bf 28 2d 40 00       	mov    $0x402d28,%edi
  401886:	e8 25 f3 ff ff       	callq  400bb0 <puts@plt>
  40188b:	e9 18 fe ff ff       	jmpq   4016a8 <explode_bomb+0x27>
  401890:	bf d0 2d 40 00       	mov    $0x402dd0,%edi
  401895:	e8 16 f3 ff ff       	callq  400bb0 <puts@plt>
  40189a:	e9 09 fe ff ff       	jmpq   4016a8 <explode_bomb+0x27>
  40189f:	bf e5 2d 40 00       	mov    $0x402de5,%edi
  4018a4:	e8 07 f3 ff ff       	callq  400bb0 <puts@plt>
  4018a9:	e9 fa fd ff ff       	jmpq   4016a8 <explode_bomb+0x27>
  4018ae:	bf fb 2d 40 00       	mov    $0x402dfb,%edi
  4018b3:	e8 f8 f2 ff ff       	callq  400bb0 <puts@plt>
  4018b8:	e9 eb fd ff ff       	jmpq   4016a8 <explode_bomb+0x27>
  4018bd:	bf 12 2e 40 00       	mov    $0x402e12,%edi
  4018c2:	e8 e9 f2 ff ff       	callq  400bb0 <puts@plt>
  4018c7:	e9 dc fd ff ff       	jmpq   4016a8 <explode_bomb+0x27>
  4018cc:	bf 1b 2e 40 00       	mov    $0x402e1b,%edi
  4018d1:	e8 da f2 ff ff       	callq  400bb0 <puts@plt>
  4018d6:	e9 cd fd ff ff       	jmpq   4016a8 <explode_bomb+0x27>
  4018db:	48 89 e7             	mov    %rsp,%rdi
  4018de:	e8 cd f2 ff ff       	callq  400bb0 <puts@plt>
  4018e3:	e8 5e fd ff ff       	callq  401646 <phase_defused>
  4018e8:	c7 05 1e 2f 20 00 00 	movl   $0x0,0x202f1e(%rip)        # 604810 <trap>
  4018ef:	00 00 00 
  4018f2:	b8 00 00 00 00       	mov    $0x0,%eax
  4018f7:	e8 97 f9 ff ff       	callq  401293 <s3cret_phase>
  4018fc:	bf 00 00 00 00       	mov    $0x0,%edi
  401901:	e8 ea f3 ff ff       	callq  400cf0 <exit@plt>

0000000000401906 <read_six_numbers>:
  401906:	48 83 ec 08          	sub    $0x8,%rsp
  40190a:	48 89 f2             	mov    %rsi,%rdx
  40190d:	48 8d 4e 04          	lea    0x4(%rsi),%rcx
  401911:	48 8d 46 14          	lea    0x14(%rsi),%rax
  401915:	50                   	push   %rax
  401916:	48 8d 46 10          	lea    0x10(%rsi),%rax
  40191a:	50                   	push   %rax
  40191b:	4c 8d 4e 0c          	lea    0xc(%rsi),%r9
  40191f:	4c 8d 46 08          	lea    0x8(%rsi),%r8
  401923:	be 45 2e 40 00       	mov    $0x402e45,%esi
  401928:	b8 00 00 00 00       	mov    $0x0,%eax
  40192d:	e8 6e f3 ff ff       	callq  400ca0 <__isoc99_sscanf@plt>
  401932:	48 83 c4 10          	add    $0x10,%rsp
  401936:	83 f8 05             	cmp    $0x5,%eax
  401939:	7e 05                	jle    401940 <read_six_numbers+0x3a>
  40193b:	48 83 c4 08          	add    $0x8,%rsp
  40193f:	c3                   	retq   
  401940:	e8 3c fd ff ff       	callq  401681 <explode_bomb>

0000000000401945 <read_line>:
  401945:	55                   	push   %rbp
  401946:	53                   	push   %rbx
  401947:	48 83 ec 08          	sub    $0x8,%rsp
  40194b:	b8 00 00 00 00       	mov    $0x0,%eax
  401950:	e8 23 fc ff ff       	callq  401578 <skip>
  401955:	48 85 c0             	test   %rax,%rax
  401958:	74 54                	je     4019ae <read_line+0x69>
  40195a:	8b 2d b4 2e 20 00    	mov    0x202eb4(%rip),%ebp        # 604814 <num_input_strings>
  401960:	48 63 c5             	movslq %ebp,%rax
  401963:	48 8d 1c 80          	lea    (%rax,%rax,4),%rbx
  401967:	48 c1 e3 04          	shl    $0x4,%rbx
  40196b:	48 81 c3 20 48 60 00 	add    $0x604820,%rbx
  401972:	48 89 df             	mov    %rbx,%rdi
  401975:	e8 56 f2 ff ff       	callq  400bd0 <strlen@plt>
  40197a:	83 f8 4e             	cmp    $0x4e,%eax
  40197d:	0f 8f 9d 00 00 00    	jg     401a20 <read_line+0xdb>
  401983:	83 e8 01             	sub    $0x1,%eax
  401986:	48 98                	cltq   
  401988:	48 63 d5             	movslq %ebp,%rdx
  40198b:	48 8d 14 92          	lea    (%rdx,%rdx,4),%rdx
  40198f:	48 c1 e2 04          	shl    $0x4,%rdx
  401993:	c6 84 10 20 48 60 00 	movb   $0x0,0x604820(%rax,%rdx,1)
  40199a:	00 
  40199b:	83 c5 01             	add    $0x1,%ebp
  40199e:	89 2d 70 2e 20 00    	mov    %ebp,0x202e70(%rip)        # 604814 <num_input_strings>
  4019a4:	48 89 d8             	mov    %rbx,%rax
  4019a7:	48 83 c4 08          	add    $0x8,%rsp
  4019ab:	5b                   	pop    %rbx
  4019ac:	5d                   	pop    %rbp
  4019ad:	c3                   	retq   
  4019ae:	48 8b 05 d3 2d 20 00 	mov    0x202dd3(%rip),%rax        # 604788 <stdin@@GLIBC_2.2.5>
  4019b5:	48 39 05 e4 2d 20 00 	cmp    %rax,0x202de4(%rip)        # 6047a0 <infile>
  4019bc:	74 19                	je     4019d7 <read_line+0x92>
  4019be:	bf 75 2e 40 00       	mov    $0x402e75,%edi
  4019c3:	e8 a8 f1 ff ff       	callq  400b70 <getenv@plt>
  4019c8:	48 85 c0             	test   %rax,%rax
  4019cb:	74 1e                	je     4019eb <read_line+0xa6>
  4019cd:	bf 00 00 00 00       	mov    $0x0,%edi
  4019d2:	e8 19 f3 ff ff       	callq  400cf0 <exit@plt>
  4019d7:	bf 57 2e 40 00       	mov    $0x402e57,%edi
  4019dc:	e8 cf f1 ff ff       	callq  400bb0 <puts@plt>
  4019e1:	bf 08 00 00 00       	mov    $0x8,%edi
  4019e6:	e8 05 f3 ff ff       	callq  400cf0 <exit@plt>
  4019eb:	48 8b 05 96 2d 20 00 	mov    0x202d96(%rip),%rax        # 604788 <stdin@@GLIBC_2.2.5>
  4019f2:	48 89 05 a7 2d 20 00 	mov    %rax,0x202da7(%rip)        # 6047a0 <infile>
  4019f9:	b8 00 00 00 00       	mov    $0x0,%eax
  4019fe:	e8 75 fb ff ff       	callq  401578 <skip>
  401a03:	48 85 c0             	test   %rax,%rax
  401a06:	0f 85 4e ff ff ff    	jne    40195a <read_line+0x15>
  401a0c:	bf 57 2e 40 00       	mov    $0x402e57,%edi
  401a11:	e8 9a f1 ff ff       	callq  400bb0 <puts@plt>
  401a16:	bf 00 00 00 00       	mov    $0x0,%edi
  401a1b:	e8 d0 f2 ff ff       	callq  400cf0 <exit@plt>
  401a20:	bf 80 2e 40 00       	mov    $0x402e80,%edi
  401a25:	e8 86 f1 ff ff       	callq  400bb0 <puts@plt>
  401a2a:	8b 05 e4 2d 20 00    	mov    0x202de4(%rip),%eax        # 604814 <num_input_strings>
  401a30:	8d 50 01             	lea    0x1(%rax),%edx
  401a33:	89 15 db 2d 20 00    	mov    %edx,0x202ddb(%rip)        # 604814 <num_input_strings>
  401a39:	48 98                	cltq   
  401a3b:	48 6b c0 50          	imul   $0x50,%rax,%rax
  401a3f:	48 b9 2a 2a 2a 74 72 	movabs $0x636e7572742a2a2a,%rcx
  401a46:	75 6e 63 
  401a49:	48 bb 61 74 65 64 2a 	movabs $0x2a2a2a64657461,%rbx
  401a50:	2a 2a 00 
  401a53:	48 89 88 20 48 60 00 	mov    %rcx,0x604820(%rax)
  401a5a:	48 89 98 28 48 60 00 	mov    %rbx,0x604828(%rax)
  401a61:	e8 1b fc ff ff       	callq  401681 <explode_bomb>

0000000000401a66 <sigalrm_handler>:
  401a66:	48 83 ec 08          	sub    $0x8,%rsp
  401a6a:	ba 00 00 00 00       	mov    $0x0,%edx
  401a6f:	be 20 30 40 00       	mov    $0x403020,%esi
  401a74:	48 8b 3d 15 2d 20 00 	mov    0x202d15(%rip),%rdi        # 604790 <stderr@@GLIBC_2.2.5>
  401a7b:	b8 00 00 00 00       	mov    $0x0,%eax
  401a80:	e8 db f1 ff ff       	callq  400c60 <fprintf@plt>
  401a85:	bf 01 00 00 00       	mov    $0x1,%edi
  401a8a:	e8 61 f2 ff ff       	callq  400cf0 <exit@plt>

0000000000401a8f <rio_readlineb>:
  401a8f:	41 56                	push   %r14
  401a91:	41 55                	push   %r13
  401a93:	41 54                	push   %r12
  401a95:	55                   	push   %rbp
  401a96:	53                   	push   %rbx
  401a97:	49 89 f4             	mov    %rsi,%r12
  401a9a:	48 83 fa 01          	cmp    $0x1,%rdx
  401a9e:	0f 86 92 00 00 00    	jbe    401b36 <rio_readlineb+0xa7>
  401aa4:	48 89 fb             	mov    %rdi,%rbx
  401aa7:	4c 8d 74 16 ff       	lea    -0x1(%rsi,%rdx,1),%r14
  401aac:	41 bd 01 00 00 00    	mov    $0x1,%r13d
  401ab2:	48 8d 6f 10          	lea    0x10(%rdi),%rbp
  401ab6:	eb 56                	jmp    401b0e <rio_readlineb+0x7f>
  401ab8:	e8 d3 f0 ff ff       	callq  400b90 <__errno_location@plt>
  401abd:	83 38 04             	cmpl   $0x4,(%rax)
  401ac0:	75 55                	jne    401b17 <rio_readlineb+0x88>
  401ac2:	ba 00 20 00 00       	mov    $0x2000,%edx
  401ac7:	48 89 ee             	mov    %rbp,%rsi
  401aca:	8b 3b                	mov    (%rbx),%edi
  401acc:	e8 3f f1 ff ff       	callq  400c10 <read@plt>
  401ad1:	89 c2                	mov    %eax,%edx
  401ad3:	89 43 04             	mov    %eax,0x4(%rbx)
  401ad6:	85 c0                	test   %eax,%eax
  401ad8:	78 de                	js     401ab8 <rio_readlineb+0x29>
  401ada:	85 c0                	test   %eax,%eax
  401adc:	74 42                	je     401b20 <rio_readlineb+0x91>
  401ade:	48 89 6b 08          	mov    %rbp,0x8(%rbx)
  401ae2:	48 8b 43 08          	mov    0x8(%rbx),%rax
  401ae6:	0f b6 08             	movzbl (%rax),%ecx
  401ae9:	48 83 c0 01          	add    $0x1,%rax
  401aed:	48 89 43 08          	mov    %rax,0x8(%rbx)
  401af1:	83 ea 01             	sub    $0x1,%edx
  401af4:	89 53 04             	mov    %edx,0x4(%rbx)
  401af7:	49 83 c4 01          	add    $0x1,%r12
  401afb:	41 88 4c 24 ff       	mov    %cl,-0x1(%r12)
  401b00:	80 f9 0a             	cmp    $0xa,%cl
  401b03:	74 3c                	je     401b41 <rio_readlineb+0xb2>
  401b05:	41 83 c5 01          	add    $0x1,%r13d
  401b09:	4d 39 f4             	cmp    %r14,%r12
  401b0c:	74 30                	je     401b3e <rio_readlineb+0xaf>
  401b0e:	8b 53 04             	mov    0x4(%rbx),%edx
  401b11:	85 d2                	test   %edx,%edx
  401b13:	7e ad                	jle    401ac2 <rio_readlineb+0x33>
  401b15:	eb cb                	jmp    401ae2 <rio_readlineb+0x53>
  401b17:	48 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%rax
  401b1e:	eb 05                	jmp    401b25 <rio_readlineb+0x96>
  401b20:	b8 00 00 00 00       	mov    $0x0,%eax
  401b25:	85 c0                	test   %eax,%eax
  401b27:	75 29                	jne    401b52 <rio_readlineb+0xc3>
  401b29:	b8 00 00 00 00       	mov    $0x0,%eax
  401b2e:	41 83 fd 01          	cmp    $0x1,%r13d
  401b32:	75 0d                	jne    401b41 <rio_readlineb+0xb2>
  401b34:	eb 13                	jmp    401b49 <rio_readlineb+0xba>
  401b36:	41 bd 01 00 00 00    	mov    $0x1,%r13d
  401b3c:	eb 03                	jmp    401b41 <rio_readlineb+0xb2>
  401b3e:	4d 89 f4             	mov    %r14,%r12
  401b41:	41 c6 04 24 00       	movb   $0x0,(%r12)
  401b46:	49 63 c5             	movslq %r13d,%rax
  401b49:	5b                   	pop    %rbx
  401b4a:	5d                   	pop    %rbp
  401b4b:	41 5c                	pop    %r12
  401b4d:	41 5d                	pop    %r13
  401b4f:	41 5e                	pop    %r14
  401b51:	c3                   	retq   
  401b52:	48 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%rax
  401b59:	eb ee                	jmp    401b49 <rio_readlineb+0xba>

0000000000401b5b <submitr>:
  401b5b:	41 57                	push   %r15
  401b5d:	41 56                	push   %r14
  401b5f:	41 55                	push   %r13
  401b61:	41 54                	push   %r12
  401b63:	55                   	push   %rbp
  401b64:	53                   	push   %rbx
  401b65:	48 81 ec 68 a0 00 00 	sub    $0xa068,%rsp
  401b6c:	49 89 fd             	mov    %rdi,%r13
  401b6f:	89 f5                	mov    %esi,%ebp
  401b71:	48 89 14 24          	mov    %rdx,(%rsp)
  401b75:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  401b7a:	4c 89 44 24 18       	mov    %r8,0x18(%rsp)
  401b7f:	4c 89 4c 24 10       	mov    %r9,0x10(%rsp)
  401b84:	48 8b 9c 24 a0 a0 00 	mov    0xa0a0(%rsp),%rbx
  401b8b:	00 
  401b8c:	4c 8b bc 24 a8 a0 00 	mov    0xa0a8(%rsp),%r15
  401b93:	00 
  401b94:	c7 84 24 3c 20 00 00 	movl   $0x0,0x203c(%rsp)
  401b9b:	00 00 00 00 
  401b9f:	ba 00 00 00 00       	mov    $0x0,%edx
  401ba4:	be 01 00 00 00       	mov    $0x1,%esi
  401ba9:	bf 02 00 00 00       	mov    $0x2,%edi
  401bae:	e8 7d f1 ff ff       	callq  400d30 <socket@plt>
  401bb3:	85 c0                	test   %eax,%eax
  401bb5:	0f 88 0e 01 00 00    	js     401cc9 <submitr+0x16e>
  401bbb:	41 89 c4             	mov    %eax,%r12d
  401bbe:	4c 89 ef             	mov    %r13,%rdi
  401bc1:	e8 8a f0 ff ff       	callq  400c50 <gethostbyname@plt>
  401bc6:	48 85 c0             	test   %rax,%rax
  401bc9:	0f 84 4a 01 00 00    	je     401d19 <submitr+0x1be>
  401bcf:	48 c7 84 24 50 a0 00 	movq   $0x0,0xa050(%rsp)
  401bd6:	00 00 00 00 00 
  401bdb:	48 c7 84 24 58 a0 00 	movq   $0x0,0xa058(%rsp)
  401be2:	00 00 00 00 00 
  401be7:	66 c7 84 24 50 a0 00 	movw   $0x2,0xa050(%rsp)
  401bee:	00 02 00 
  401bf1:	48 63 50 14          	movslq 0x14(%rax),%rdx
  401bf5:	48 8b 40 18          	mov    0x18(%rax),%rax
  401bf9:	48 8b 30             	mov    (%rax),%rsi
  401bfc:	48 8d bc 24 54 a0 00 	lea    0xa054(%rsp),%rdi
  401c03:	00 
  401c04:	e8 a7 f0 ff ff       	callq  400cb0 <memmove@plt>
  401c09:	66 c1 cd 08          	ror    $0x8,%bp
  401c0d:	66 89 ac 24 52 a0 00 	mov    %bp,0xa052(%rsp)
  401c14:	00 
  401c15:	ba 10 00 00 00       	mov    $0x10,%edx
  401c1a:	48 8d b4 24 50 a0 00 	lea    0xa050(%rsp),%rsi
  401c21:	00 
  401c22:	44 89 e7             	mov    %r12d,%edi
  401c25:	e8 d6 f0 ff ff       	callq  400d00 <connect@plt>
  401c2a:	85 c0                	test   %eax,%eax
  401c2c:	0f 88 52 01 00 00    	js     401d84 <submitr+0x229>
  401c32:	48 89 df             	mov    %rbx,%rdi
  401c35:	e8 96 ef ff ff       	callq  400bd0 <strlen@plt>
  401c3a:	48 89 c5             	mov    %rax,%rbp
  401c3d:	48 8b 3c 24          	mov    (%rsp),%rdi
  401c41:	e8 8a ef ff ff       	callq  400bd0 <strlen@plt>
  401c46:	49 89 c6             	mov    %rax,%r14
  401c49:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  401c4e:	e8 7d ef ff ff       	callq  400bd0 <strlen@plt>
  401c53:	49 89 c5             	mov    %rax,%r13
  401c56:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  401c5b:	e8 70 ef ff ff       	callq  400bd0 <strlen@plt>
  401c60:	48 89 c2             	mov    %rax,%rdx
  401c63:	4b 8d 84 2e 80 00 00 	lea    0x80(%r14,%r13,1),%rax
  401c6a:	00 
  401c6b:	48 01 d0             	add    %rdx,%rax
  401c6e:	48 8d 54 6d 00       	lea    0x0(%rbp,%rbp,2),%rdx
  401c73:	48 01 d0             	add    %rdx,%rax
  401c76:	48 3d 00 20 00 00    	cmp    $0x2000,%rax
  401c7c:	0f 87 5f 01 00 00    	ja     401de1 <submitr+0x286>
  401c82:	48 8d 94 24 40 40 00 	lea    0x4040(%rsp),%rdx
  401c89:	00 
  401c8a:	b9 00 04 00 00       	mov    $0x400,%ecx
  401c8f:	b8 00 00 00 00       	mov    $0x0,%eax
  401c94:	48 89 d7             	mov    %rdx,%rdi
  401c97:	f3 48 ab             	rep stos %rax,%es:(%rdi)
  401c9a:	48 89 df             	mov    %rbx,%rdi
  401c9d:	e8 2e ef ff ff       	callq  400bd0 <strlen@plt>
  401ca2:	85 c0                	test   %eax,%eax
  401ca4:	0f 84 4a 05 00 00    	je     4021f4 <submitr+0x699>
  401caa:	8d 40 ff             	lea    -0x1(%rax),%eax
  401cad:	4c 8d 6c 03 01       	lea    0x1(%rbx,%rax,1),%r13
  401cb2:	48 8d ac 24 40 40 00 	lea    0x4040(%rsp),%rbp
  401cb9:	00 
  401cba:	49 be d9 ff 00 00 00 	movabs $0x2000000000ffd9,%r14
  401cc1:	00 20 00 
  401cc4:	e9 a5 01 00 00       	jmpq   401e6e <submitr+0x313>
  401cc9:	48 b8 45 72 72 6f 72 	movabs $0x43203a726f727245,%rax
  401cd0:	3a 20 43 
  401cd3:	48 ba 6c 69 65 6e 74 	movabs $0x6e7520746e65696c,%rdx
  401cda:	20 75 6e 
  401cdd:	49 89 07             	mov    %rax,(%r15)
  401ce0:	49 89 57 08          	mov    %rdx,0x8(%r15)
  401ce4:	48 b8 61 62 6c 65 20 	movabs $0x206f7420656c6261,%rax
  401ceb:	74 6f 20 
  401cee:	48 ba 63 72 65 61 74 	movabs $0x7320657461657263,%rdx
  401cf5:	65 20 73 
  401cf8:	49 89 47 10          	mov    %rax,0x10(%r15)
  401cfc:	49 89 57 18          	mov    %rdx,0x18(%r15)
  401d00:	41 c7 47 20 6f 63 6b 	movl   $0x656b636f,0x20(%r15)
  401d07:	65 
  401d08:	66 41 c7 47 24 74 00 	movw   $0x74,0x24(%r15)
  401d0f:	b8 ff ff ff ff       	mov    $0xffffffff,%eax
  401d14:	e9 03 03 00 00       	jmpq   40201c <submitr+0x4c1>
  401d19:	48 b8 45 72 72 6f 72 	movabs $0x44203a726f727245,%rax
  401d20:	3a 20 44 
  401d23:	48 ba 4e 53 20 69 73 	movabs $0x6e7520736920534e,%rdx
  401d2a:	20 75 6e 
  401d2d:	49 89 07             	mov    %rax,(%r15)
  401d30:	49 89 57 08          	mov    %rdx,0x8(%r15)
  401d34:	48 b8 61 62 6c 65 20 	movabs $0x206f7420656c6261,%rax
  401d3b:	74 6f 20 
  401d3e:	48 ba 72 65 73 6f 6c 	movabs $0x2065766c6f736572,%rdx
  401d45:	76 65 20 
  401d48:	49 89 47 10          	mov    %rax,0x10(%r15)
  401d4c:	49 89 57 18          	mov    %rdx,0x18(%r15)
  401d50:	48 b8 73 65 72 76 65 	movabs $0x6120726576726573,%rax
  401d57:	72 20 61 
  401d5a:	49 89 47 20          	mov    %rax,0x20(%r15)
  401d5e:	41 c7 47 28 64 64 72 	movl   $0x65726464,0x28(%r15)
  401d65:	65 
  401d66:	66 41 c7 47 2c 73 73 	movw   $0x7373,0x2c(%r15)
  401d6d:	41 c6 47 2e 00       	movb   $0x0,0x2e(%r15)
  401d72:	44 89 e7             	mov    %r12d,%edi
  401d75:	e8 86 ee ff ff       	callq  400c00 <close@plt>
  401d7a:	b8 ff ff ff ff       	mov    $0xffffffff,%eax
  401d7f:	e9 98 02 00 00       	jmpq   40201c <submitr+0x4c1>
  401d84:	48 b8 45 72 72 6f 72 	movabs $0x55203a726f727245,%rax
  401d8b:	3a 20 55 
  401d8e:	48 ba 6e 61 62 6c 65 	movabs $0x6f7420656c62616e,%rdx
  401d95:	20 74 6f 
  401d98:	49 89 07             	mov    %rax,(%r15)
  401d9b:	49 89 57 08          	mov    %rdx,0x8(%r15)
  401d9f:	48 b8 20 63 6f 6e 6e 	movabs $0x7463656e6e6f6320,%rax
  401da6:	65 63 74 
  401da9:	48 ba 20 74 6f 20 74 	movabs $0x20656874206f7420,%rdx
  401db0:	68 65 20 
  401db3:	49 89 47 10          	mov    %rax,0x10(%r15)
  401db7:	49 89 57 18          	mov    %rdx,0x18(%r15)
  401dbb:	41 c7 47 20 73 65 72 	movl   $0x76726573,0x20(%r15)
  401dc2:	76 
  401dc3:	66 41 c7 47 24 65 72 	movw   $0x7265,0x24(%r15)
  401dca:	41 c6 47 26 00       	movb   $0x0,0x26(%r15)
  401dcf:	44 89 e7             	mov    %r12d,%edi
  401dd2:	e8 29 ee ff ff       	callq  400c00 <close@plt>
  401dd7:	b8 ff ff ff ff       	mov    $0xffffffff,%eax
  401ddc:	e9 3b 02 00 00       	jmpq   40201c <submitr+0x4c1>
  401de1:	48 b8 45 72 72 6f 72 	movabs $0x52203a726f727245,%rax
  401de8:	3a 20 52 
  401deb:	48 ba 65 73 75 6c 74 	movabs $0x747320746c757365,%rdx
  401df2:	20 73 74 
  401df5:	49 89 07             	mov    %rax,(%r15)
  401df8:	49 89 57 08          	mov    %rdx,0x8(%r15)
  401dfc:	48 b8 72 69 6e 67 20 	movabs $0x6f6f7420676e6972,%rax
  401e03:	74 6f 6f 
  401e06:	48 ba 20 6c 61 72 67 	movabs $0x202e656772616c20,%rdx
  401e0d:	65 2e 20 
  401e10:	49 89 47 10          	mov    %rax,0x10(%r15)
  401e14:	49 89 57 18          	mov    %rdx,0x18(%r15)
  401e18:	48 b8 49 6e 63 72 65 	movabs $0x6573616572636e49,%rax
  401e1f:	61 73 65 
  401e22:	48 ba 20 53 55 42 4d 	movabs $0x5254494d42555320,%rdx
  401e29:	49 54 52 
  401e2c:	49 89 47 20          	mov    %rax,0x20(%r15)
  401e30:	49 89 57 28          	mov    %rdx,0x28(%r15)
  401e34:	48 b8 5f 4d 41 58 42 	movabs $0x46554258414d5f,%rax
  401e3b:	55 46 00 
  401e3e:	49 89 47 30          	mov    %rax,0x30(%r15)
  401e42:	44 89 e7             	mov    %r12d,%edi
  401e45:	e8 b6 ed ff ff       	callq  400c00 <close@plt>
  401e4a:	b8 ff ff ff ff       	mov    $0xffffffff,%eax
  401e4f:	e9 c8 01 00 00       	jmpq   40201c <submitr+0x4c1>
  401e54:	49 0f a3 c6          	bt     %rax,%r14
  401e58:	73 1e                	jae    401e78 <submitr+0x31d>
  401e5a:	88 55 00             	mov    %dl,0x0(%rbp)
  401e5d:	48 8d 6d 01          	lea    0x1(%rbp),%rbp
  401e61:	48 83 c3 01          	add    $0x1,%rbx
  401e65:	49 39 dd             	cmp    %rbx,%r13
  401e68:	0f 84 86 03 00 00    	je     4021f4 <submitr+0x699>
  401e6e:	0f b6 13             	movzbl (%rbx),%edx
  401e71:	8d 42 d6             	lea    -0x2a(%rdx),%eax
  401e74:	3c 35                	cmp    $0x35,%al
  401e76:	76 dc                	jbe    401e54 <submitr+0x2f9>
  401e78:	89 d0                	mov    %edx,%eax
  401e7a:	83 e0 df             	and    $0xffffffdf,%eax
  401e7d:	83 e8 41             	sub    $0x41,%eax
  401e80:	3c 19                	cmp    $0x19,%al
  401e82:	76 d6                	jbe    401e5a <submitr+0x2ff>
  401e84:	80 fa 20             	cmp    $0x20,%dl
  401e87:	74 45                	je     401ece <submitr+0x373>
  401e89:	8d 42 e0             	lea    -0x20(%rdx),%eax
  401e8c:	3c 5f                	cmp    $0x5f,%al
  401e8e:	76 09                	jbe    401e99 <submitr+0x33e>
  401e90:	80 fa 09             	cmp    $0x9,%dl
  401e93:	0f 85 ce 02 00 00    	jne    402167 <submitr+0x60c>
  401e99:	0f b6 d2             	movzbl %dl,%edx
  401e9c:	be f8 30 40 00       	mov    $0x4030f8,%esi
  401ea1:	48 8d 7c 24 28       	lea    0x28(%rsp),%rdi
  401ea6:	b8 00 00 00 00       	mov    $0x0,%eax
  401eab:	e8 30 ee ff ff       	callq  400ce0 <sprintf@plt>
  401eb0:	0f b6 44 24 28       	movzbl 0x28(%rsp),%eax
  401eb5:	88 45 00             	mov    %al,0x0(%rbp)
  401eb8:	0f b6 44 24 29       	movzbl 0x29(%rsp),%eax
  401ebd:	88 45 01             	mov    %al,0x1(%rbp)
  401ec0:	0f b6 44 24 2a       	movzbl 0x2a(%rsp),%eax
  401ec5:	88 45 02             	mov    %al,0x2(%rbp)
  401ec8:	48 8d 6d 03          	lea    0x3(%rbp),%rbp
  401ecc:	eb 93                	jmp    401e61 <submitr+0x306>
  401ece:	c6 45 00 2b          	movb   $0x2b,0x0(%rbp)
  401ed2:	48 8d 6d 01          	lea    0x1(%rbp),%rbp
  401ed6:	eb 89                	jmp    401e61 <submitr+0x306>
  401ed8:	48 01 c5             	add    %rax,%rbp
  401edb:	48 29 c3             	sub    %rax,%rbx
  401ede:	0f 84 73 03 00 00    	je     402257 <submitr+0x6fc>
  401ee4:	48 89 da             	mov    %rbx,%rdx
  401ee7:	48 89 ee             	mov    %rbp,%rsi
  401eea:	44 89 e7             	mov    %r12d,%edi
  401eed:	e8 ce ec ff ff       	callq  400bc0 <write@plt>
  401ef2:	48 85 c0             	test   %rax,%rax
  401ef5:	7f e1                	jg     401ed8 <submitr+0x37d>
  401ef7:	e8 94 ec ff ff       	callq  400b90 <__errno_location@plt>
  401efc:	83 38 04             	cmpl   $0x4,(%rax)
  401eff:	0f 85 03 02 00 00    	jne    402108 <submitr+0x5ad>
  401f05:	4c 89 e8             	mov    %r13,%rax
  401f08:	eb ce                	jmp    401ed8 <submitr+0x37d>
  401f0a:	48 b8 45 72 72 6f 72 	movabs $0x43203a726f727245,%rax
  401f11:	3a 20 43 
  401f14:	48 ba 6c 69 65 6e 74 	movabs $0x6e7520746e65696c,%rdx
  401f1b:	20 75 6e 
  401f1e:	49 89 07             	mov    %rax,(%r15)
  401f21:	49 89 57 08          	mov    %rdx,0x8(%r15)
  401f25:	48 b8 61 62 6c 65 20 	movabs $0x206f7420656c6261,%rax
  401f2c:	74 6f 20 
  401f2f:	48 ba 72 65 61 64 20 	movabs $0x7269662064616572,%rdx
  401f36:	66 69 72 
  401f39:	49 89 47 10          	mov    %rax,0x10(%r15)
  401f3d:	49 89 57 18          	mov    %rdx,0x18(%r15)
  401f41:	48 b8 73 74 20 68 65 	movabs $0x6564616568207473,%rax
  401f48:	61 64 65 
  401f4b:	48 ba 72 20 66 72 6f 	movabs $0x73206d6f72662072,%rdx
  401f52:	6d 20 73 
  401f55:	49 89 47 20          	mov    %rax,0x20(%r15)
  401f59:	49 89 57 28          	mov    %rdx,0x28(%r15)
  401f5d:	41 c7 47 30 65 72 76 	movl   $0x65767265,0x30(%r15)
  401f64:	65 
  401f65:	66 41 c7 47 34 72 00 	movw   $0x72,0x34(%r15)
  401f6c:	44 89 e7             	mov    %r12d,%edi
  401f6f:	e8 8c ec ff ff       	callq  400c00 <close@plt>
  401f74:	b8 ff ff ff ff       	mov    $0xffffffff,%eax
  401f79:	e9 9e 00 00 00       	jmpq   40201c <submitr+0x4c1>
  401f7e:	ba 00 20 00 00       	mov    $0x2000,%edx
  401f83:	48 8d b4 24 40 60 00 	lea    0x6040(%rsp),%rsi
  401f8a:	00 
  401f8b:	48 8d bc 24 40 80 00 	lea    0x8040(%rsp),%rdi
  401f92:	00 
  401f93:	e8 f7 fa ff ff       	callq  401a8f <rio_readlineb>
  401f98:	48 85 c0             	test   %rax,%rax
  401f9b:	0f 8e 8d 00 00 00    	jle    40202e <submitr+0x4d3>
  401fa1:	80 bc 24 40 60 00 00 	cmpb   $0xd,0x6040(%rsp)
  401fa8:	0d 
  401fa9:	75 d3                	jne    401f7e <submitr+0x423>
  401fab:	80 bc 24 41 60 00 00 	cmpb   $0xa,0x6041(%rsp)
  401fb2:	0a 
  401fb3:	75 c9                	jne    401f7e <submitr+0x423>
  401fb5:	80 bc 24 42 60 00 00 	cmpb   $0x0,0x6042(%rsp)
  401fbc:	00 
  401fbd:	75 bf                	jne    401f7e <submitr+0x423>
  401fbf:	ba 00 20 00 00       	mov    $0x2000,%edx
  401fc4:	48 8d b4 24 40 60 00 	lea    0x6040(%rsp),%rsi
  401fcb:	00 
  401fcc:	48 8d bc 24 40 80 00 	lea    0x8040(%rsp),%rdi
  401fd3:	00 
  401fd4:	e8 b6 fa ff ff       	callq  401a8f <rio_readlineb>
  401fd9:	48 85 c0             	test   %rax,%rax
  401fdc:	0f 8e b3 00 00 00    	jle    402095 <submitr+0x53a>
  401fe2:	48 8d b4 24 40 60 00 	lea    0x6040(%rsp),%rsi
  401fe9:	00 
  401fea:	4c 89 ff             	mov    %r15,%rdi
  401fed:	e8 ae eb ff ff       	callq  400ba0 <strcpy@plt>
  401ff2:	44 89 e7             	mov    %r12d,%edi
  401ff5:	e8 06 ec ff ff       	callq  400c00 <close@plt>
  401ffa:	41 0f b6 07          	movzbl (%r15),%eax
  401ffe:	83 e8 4f             	sub    $0x4f,%eax
  402001:	75 0f                	jne    402012 <submitr+0x4b7>
  402003:	41 0f b6 47 01       	movzbl 0x1(%r15),%eax
  402008:	83 e8 4b             	sub    $0x4b,%eax
  40200b:	75 05                	jne    402012 <submitr+0x4b7>
  40200d:	41 0f b6 47 02       	movzbl 0x2(%r15),%eax
  402012:	85 c0                	test   %eax,%eax
  402014:	0f 95 c0             	setne  %al
  402017:	0f b6 c0             	movzbl %al,%eax
  40201a:	f7 d8                	neg    %eax
  40201c:	48 81 c4 68 a0 00 00 	add    $0xa068,%rsp
  402023:	5b                   	pop    %rbx
  402024:	5d                   	pop    %rbp
  402025:	41 5c                	pop    %r12
  402027:	41 5d                	pop    %r13
  402029:	41 5e                	pop    %r14
  40202b:	41 5f                	pop    %r15
  40202d:	c3                   	retq   
  40202e:	48 b8 45 72 72 6f 72 	movabs $0x43203a726f727245,%rax
  402035:	3a 20 43 
  402038:	48 ba 6c 69 65 6e 74 	movabs $0x6e7520746e65696c,%rdx
  40203f:	20 75 6e 
  402042:	49 89 07             	mov    %rax,(%r15)
  402045:	49 89 57 08          	mov    %rdx,0x8(%r15)
  402049:	48 b8 61 62 6c 65 20 	movabs $0x206f7420656c6261,%rax
  402050:	74 6f 20 
  402053:	48 ba 72 65 61 64 20 	movabs $0x6165682064616572,%rdx
  40205a:	68 65 61 
  40205d:	49 89 47 10          	mov    %rax,0x10(%r15)
  402061:	49 89 57 18          	mov    %rdx,0x18(%r15)
  402065:	48 b8 64 65 72 73 20 	movabs $0x6f72662073726564,%rax
  40206c:	66 72 6f 
  40206f:	48 ba 6d 20 73 65 72 	movabs $0x726576726573206d,%rdx
  402076:	76 65 72 
  402079:	49 89 47 20          	mov    %rax,0x20(%r15)
  40207d:	49 89 57 28          	mov    %rdx,0x28(%r15)
  402081:	41 c6 47 30 00       	movb   $0x0,0x30(%r15)
  402086:	44 89 e7             	mov    %r12d,%edi
  402089:	e8 72 eb ff ff       	callq  400c00 <close@plt>
  40208e:	b8 ff ff ff ff       	mov    $0xffffffff,%eax
  402093:	eb 87                	jmp    40201c <submitr+0x4c1>
  402095:	48 b8 45 72 72 6f 72 	movabs $0x43203a726f727245,%rax
  40209c:	3a 20 43 
  40209f:	48 ba 6c 69 65 6e 74 	movabs $0x6e7520746e65696c,%rdx
  4020a6:	20 75 6e 
  4020a9:	49 89 07             	mov    %rax,(%r15)
  4020ac:	49 89 57 08          	mov    %rdx,0x8(%r15)
  4020b0:	48 b8 61 62 6c 65 20 	movabs $0x206f7420656c6261,%rax
  4020b7:	74 6f 20 
  4020ba:	48 ba 72 65 61 64 20 	movabs $0x6174732064616572,%rdx
  4020c1:	73 74 61 
  4020c4:	49 89 47 10          	mov    %rax,0x10(%r15)
  4020c8:	49 89 57 18          	mov    %rdx,0x18(%r15)
  4020cc:	48 b8 74 75 73 20 6d 	movabs $0x7373656d20737574,%rax
  4020d3:	65 73 73 
  4020d6:	48 ba 61 67 65 20 66 	movabs $0x6d6f726620656761,%rdx
  4020dd:	72 6f 6d 
  4020e0:	49 89 47 20          	mov    %rax,0x20(%r15)
  4020e4:	49 89 57 28          	mov    %rdx,0x28(%r15)
  4020e8:	48 b8 20 73 65 72 76 	movabs $0x72657672657320,%rax
  4020ef:	65 72 00 
  4020f2:	49 89 47 30          	mov    %rax,0x30(%r15)
  4020f6:	44 89 e7             	mov    %r12d,%edi
  4020f9:	e8 02 eb ff ff       	callq  400c00 <close@plt>
  4020fe:	b8 ff ff ff ff       	mov    $0xffffffff,%eax
  402103:	e9 14 ff ff ff       	jmpq   40201c <submitr+0x4c1>
  402108:	48 b8 45 72 72 6f 72 	movabs $0x43203a726f727245,%rax
  40210f:	3a 20 43 
  402112:	48 ba 6c 69 65 6e 74 	movabs $0x6e7520746e65696c,%rdx
  402119:	20 75 6e 
  40211c:	49 89 07             	mov    %rax,(%r15)
  40211f:	49 89 57 08          	mov    %rdx,0x8(%r15)
  402123:	48 b8 61 62 6c 65 20 	movabs $0x206f7420656c6261,%rax
  40212a:	74 6f 20 
  40212d:	48 ba 77 72 69 74 65 	movabs $0x6f74206574697277,%rdx
  402134:	20 74 6f 
  402137:	49 89 47 10          	mov    %rax,0x10(%r15)
  40213b:	49 89 57 18          	mov    %rdx,0x18(%r15)
  40213f:	48 b8 20 74 68 65 20 	movabs $0x7265732065687420,%rax
  402146:	73 65 72 
  402149:	49 89 47 20          	mov    %rax,0x20(%r15)
  40214d:	41 c7 47 28 76 65 72 	movl   $0x726576,0x28(%r15)
  402154:	00 
  402155:	44 89 e7             	mov    %r12d,%edi
  402158:	e8 a3 ea ff ff       	callq  400c00 <close@plt>
  40215d:	b8 ff ff ff ff       	mov    $0xffffffff,%eax
  402162:	e9 b5 fe ff ff       	jmpq   40201c <submitr+0x4c1>
  402167:	48 b8 45 72 72 6f 72 	movabs $0x52203a726f727245,%rax
  40216e:	3a 20 52 
  402171:	48 ba 65 73 75 6c 74 	movabs $0x747320746c757365,%rdx
  402178:	20 73 74 
  40217b:	49 89 07             	mov    %rax,(%r15)
  40217e:	49 89 57 08          	mov    %rdx,0x8(%r15)
  402182:	48 b8 72 69 6e 67 20 	movabs $0x6e6f6320676e6972,%rax
  402189:	63 6f 6e 
  40218c:	48 ba 74 61 69 6e 73 	movabs $0x6e6120736e696174,%rdx
  402193:	20 61 6e 
  402196:	49 89 47 10          	mov    %rax,0x10(%r15)
  40219a:	49 89 57 18          	mov    %rdx,0x18(%r15)
  40219e:	48 b8 20 69 6c 6c 65 	movabs $0x6c6167656c6c6920,%rax
  4021a5:	67 61 6c 
  4021a8:	48 ba 20 6f 72 20 75 	movabs $0x72706e7520726f20,%rdx
  4021af:	6e 70 72 
  4021b2:	49 89 47 20          	mov    %rax,0x20(%r15)
  4021b6:	49 89 57 28          	mov    %rdx,0x28(%r15)
  4021ba:	48 b8 69 6e 74 61 62 	movabs $0x20656c6261746e69,%rax
  4021c1:	6c 65 20 
  4021c4:	48 ba 63 68 61 72 61 	movabs $0x6574636172616863,%rdx
  4021cb:	63 74 65 
  4021ce:	49 89 47 30          	mov    %rax,0x30(%r15)
  4021d2:	49 89 57 38          	mov    %rdx,0x38(%r15)
  4021d6:	66 41 c7 47 40 72 2e 	movw   $0x2e72,0x40(%r15)
  4021dd:	41 c6 47 42 00       	movb   $0x0,0x42(%r15)
  4021e2:	44 89 e7             	mov    %r12d,%edi
  4021e5:	e8 16 ea ff ff       	callq  400c00 <close@plt>
  4021ea:	b8 ff ff ff ff       	mov    $0xffffffff,%eax
  4021ef:	e9 28 fe ff ff       	jmpq   40201c <submitr+0x4c1>
  4021f4:	48 83 ec 08          	sub    $0x8,%rsp
  4021f8:	48 8d 84 24 48 40 00 	lea    0x4048(%rsp),%rax
  4021ff:	00 
  402200:	50                   	push   %rax
  402201:	4c 8b 4c 24 20       	mov    0x20(%rsp),%r9
  402206:	4c 8b 44 24 28       	mov    0x28(%rsp),%r8
  40220b:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  402210:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  402215:	be 78 30 40 00       	mov    $0x403078,%esi
  40221a:	48 8d bc 24 50 60 00 	lea    0x6050(%rsp),%rdi
  402221:	00 
  402222:	b8 00 00 00 00       	mov    $0x0,%eax
  402227:	e8 b4 ea ff ff       	callq  400ce0 <sprintf@plt>
  40222c:	48 8d bc 24 50 60 00 	lea    0x6050(%rsp),%rdi
  402233:	00 
  402234:	e8 97 e9 ff ff       	callq  400bd0 <strlen@plt>
  402239:	48 89 c3             	mov    %rax,%rbx
  40223c:	48 83 c4 10          	add    $0x10,%rsp
  402240:	48 8d ac 24 40 60 00 	lea    0x6040(%rsp),%rbp
  402247:	00 
  402248:	41 bd 00 00 00 00    	mov    $0x0,%r13d
  40224e:	48 85 c0             	test   %rax,%rax
  402251:	0f 85 8d fc ff ff    	jne    401ee4 <submitr+0x389>
  402257:	44 89 a4 24 40 80 00 	mov    %r12d,0x8040(%rsp)
  40225e:	00 
  40225f:	c7 84 24 44 80 00 00 	movl   $0x0,0x8044(%rsp)
  402266:	00 00 00 00 
  40226a:	48 8d 84 24 50 80 00 	lea    0x8050(%rsp),%rax
  402271:	00 
  402272:	48 89 84 24 48 80 00 	mov    %rax,0x8048(%rsp)
  402279:	00 
  40227a:	ba 00 20 00 00       	mov    $0x2000,%edx
  40227f:	48 8d b4 24 40 60 00 	lea    0x6040(%rsp),%rsi
  402286:	00 
  402287:	48 8d bc 24 40 80 00 	lea    0x8040(%rsp),%rdi
  40228e:	00 
  40228f:	e8 fb f7 ff ff       	callq  401a8f <rio_readlineb>
  402294:	48 85 c0             	test   %rax,%rax
  402297:	0f 8e 6d fc ff ff    	jle    401f0a <submitr+0x3af>
  40229d:	4c 8d 44 24 30       	lea    0x30(%rsp),%r8
  4022a2:	48 8d 8c 24 3c 20 00 	lea    0x203c(%rsp),%rcx
  4022a9:	00 
  4022aa:	48 8d 94 24 40 20 00 	lea    0x2040(%rsp),%rdx
  4022b1:	00 
  4022b2:	be ff 30 40 00       	mov    $0x4030ff,%esi
  4022b7:	48 8d bc 24 40 60 00 	lea    0x6040(%rsp),%rdi
  4022be:	00 
  4022bf:	b8 00 00 00 00       	mov    $0x0,%eax
  4022c4:	e8 d7 e9 ff ff       	callq  400ca0 <__isoc99_sscanf@plt>
  4022c9:	8b 94 24 3c 20 00 00 	mov    0x203c(%rsp),%edx
  4022d0:	81 fa c8 00 00 00    	cmp    $0xc8,%edx
  4022d6:	0f 84 c5 fc ff ff    	je     401fa1 <submitr+0x446>
  4022dc:	48 8d 4c 24 30       	lea    0x30(%rsp),%rcx
  4022e1:	be 48 30 40 00       	mov    $0x403048,%esi
  4022e6:	4c 89 ff             	mov    %r15,%rdi
  4022e9:	b8 00 00 00 00       	mov    $0x0,%eax
  4022ee:	e8 ed e9 ff ff       	callq  400ce0 <sprintf@plt>
  4022f3:	44 89 e7             	mov    %r12d,%edi
  4022f6:	e8 05 e9 ff ff       	callq  400c00 <close@plt>
  4022fb:	b8 ff ff ff ff       	mov    $0xffffffff,%eax
  402300:	e9 17 fd ff ff       	jmpq   40201c <submitr+0x4c1>

0000000000402305 <init_timeout>:
  402305:	85 ff                	test   %edi,%edi
  402307:	75 01                	jne    40230a <init_timeout+0x5>
  402309:	c3                   	retq   
  40230a:	53                   	push   %rbx
  40230b:	89 fb                	mov    %edi,%ebx
  40230d:	be 66 1a 40 00       	mov    $0x401a66,%esi
  402312:	bf 0e 00 00 00       	mov    $0xe,%edi
  402317:	e8 24 e9 ff ff       	callq  400c40 <signal@plt>
  40231c:	85 db                	test   %ebx,%ebx
  40231e:	bf 00 00 00 00       	mov    $0x0,%edi
  402323:	0f 49 fb             	cmovns %ebx,%edi
  402326:	e8 c5 e8 ff ff       	callq  400bf0 <alarm@plt>
  40232b:	5b                   	pop    %rbx
  40232c:	c3                   	retq   

000000000040232d <init_driver>:
  40232d:	55                   	push   %rbp
  40232e:	53                   	push   %rbx
  40232f:	48 83 ec 18          	sub    $0x18,%rsp
  402333:	48 89 fd             	mov    %rdi,%rbp
  402336:	be 01 00 00 00       	mov    $0x1,%esi
  40233b:	bf 0d 00 00 00       	mov    $0xd,%edi
  402340:	e8 fb e8 ff ff       	callq  400c40 <signal@plt>
  402345:	be 01 00 00 00       	mov    $0x1,%esi
  40234a:	bf 1d 00 00 00       	mov    $0x1d,%edi
  40234f:	e8 ec e8 ff ff       	callq  400c40 <signal@plt>
  402354:	be 01 00 00 00       	mov    $0x1,%esi
  402359:	bf 1d 00 00 00       	mov    $0x1d,%edi
  40235e:	e8 dd e8 ff ff       	callq  400c40 <signal@plt>
  402363:	ba 00 00 00 00       	mov    $0x0,%edx
  402368:	be 01 00 00 00       	mov    $0x1,%esi
  40236d:	bf 02 00 00 00       	mov    $0x2,%edi
  402372:	e8 b9 e9 ff ff       	callq  400d30 <socket@plt>
  402377:	85 c0                	test   %eax,%eax
  402379:	78 7c                	js     4023f7 <init_driver+0xca>
  40237b:	89 c3                	mov    %eax,%ebx
  40237d:	bf 2e 2f 40 00       	mov    $0x402f2e,%edi
  402382:	e8 c9 e8 ff ff       	callq  400c50 <gethostbyname@plt>
  402387:	48 85 c0             	test   %rax,%rax
  40238a:	0f 84 b3 00 00 00    	je     402443 <init_driver+0x116>
  402390:	48 c7 04 24 00 00 00 	movq   $0x0,(%rsp)
  402397:	00 
  402398:	48 c7 44 24 08 00 00 	movq   $0x0,0x8(%rsp)
  40239f:	00 00 
  4023a1:	66 c7 04 24 02 00    	movw   $0x2,(%rsp)
  4023a7:	48 63 50 14          	movslq 0x14(%rax),%rdx
  4023ab:	48 8b 40 18          	mov    0x18(%rax),%rax
  4023af:	48 8b 30             	mov    (%rax),%rsi
  4023b2:	48 8d 7c 24 04       	lea    0x4(%rsp),%rdi
  4023b7:	e8 f4 e8 ff ff       	callq  400cb0 <memmove@plt>
  4023bc:	66 c7 44 24 02 3b 6e 	movw   $0x6e3b,0x2(%rsp)
  4023c3:	ba 10 00 00 00       	mov    $0x10,%edx
  4023c8:	48 89 e6             	mov    %rsp,%rsi
  4023cb:	89 df                	mov    %ebx,%edi
  4023cd:	e8 2e e9 ff ff       	callq  400d00 <connect@plt>
  4023d2:	85 c0                	test   %eax,%eax
  4023d4:	0f 88 d1 00 00 00    	js     4024ab <init_driver+0x17e>
  4023da:	89 df                	mov    %ebx,%edi
  4023dc:	e8 1f e8 ff ff       	callq  400c00 <close@plt>
  4023e1:	66 c7 45 00 4f 4b    	movw   $0x4b4f,0x0(%rbp)
  4023e7:	c6 45 02 00          	movb   $0x0,0x2(%rbp)
  4023eb:	b8 00 00 00 00       	mov    $0x0,%eax
  4023f0:	48 83 c4 18          	add    $0x18,%rsp
  4023f4:	5b                   	pop    %rbx
  4023f5:	5d                   	pop    %rbp
  4023f6:	c3                   	retq   
  4023f7:	48 b8 45 72 72 6f 72 	movabs $0x43203a726f727245,%rax
  4023fe:	3a 20 43 
  402401:	48 ba 6c 69 65 6e 74 	movabs $0x6e7520746e65696c,%rdx
  402408:	20 75 6e 
  40240b:	48 89 45 00          	mov    %rax,0x0(%rbp)
  40240f:	48 89 55 08          	mov    %rdx,0x8(%rbp)
  402413:	48 b8 61 62 6c 65 20 	movabs $0x206f7420656c6261,%rax
  40241a:	74 6f 20 
  40241d:	48 ba 63 72 65 61 74 	movabs $0x7320657461657263,%rdx
  402424:	65 20 73 
  402427:	48 89 45 10          	mov    %rax,0x10(%rbp)
  40242b:	48 89 55 18          	mov    %rdx,0x18(%rbp)
  40242f:	c7 45 20 6f 63 6b 65 	movl   $0x656b636f,0x20(%rbp)
  402436:	66 c7 45 24 74 00    	movw   $0x74,0x24(%rbp)
  40243c:	b8 ff ff ff ff       	mov    $0xffffffff,%eax
  402441:	eb ad                	jmp    4023f0 <init_driver+0xc3>
  402443:	48 b8 45 72 72 6f 72 	movabs $0x44203a726f727245,%rax
  40244a:	3a 20 44 
  40244d:	48 ba 4e 53 20 69 73 	movabs $0x6e7520736920534e,%rdx
  402454:	20 75 6e 
  402457:	48 89 45 00          	mov    %rax,0x0(%rbp)
  40245b:	48 89 55 08          	mov    %rdx,0x8(%rbp)
  40245f:	48 b8 61 62 6c 65 20 	movabs $0x206f7420656c6261,%rax
  402466:	74 6f 20 
  402469:	48 ba 72 65 73 6f 6c 	movabs $0x2065766c6f736572,%rdx
  402470:	76 65 20 
  402473:	48 89 45 10          	mov    %rax,0x10(%rbp)
  402477:	48 89 55 18          	mov    %rdx,0x18(%rbp)
  40247b:	48 b8 73 65 72 76 65 	movabs $0x6120726576726573,%rax
  402482:	72 20 61 
  402485:	48 89 45 20          	mov    %rax,0x20(%rbp)
  402489:	c7 45 28 64 64 72 65 	movl   $0x65726464,0x28(%rbp)
  402490:	66 c7 45 2c 73 73    	movw   $0x7373,0x2c(%rbp)
  402496:	c6 45 2e 00          	movb   $0x0,0x2e(%rbp)
  40249a:	89 df                	mov    %ebx,%edi
  40249c:	e8 5f e7 ff ff       	callq  400c00 <close@plt>
  4024a1:	b8 ff ff ff ff       	mov    $0xffffffff,%eax
  4024a6:	e9 45 ff ff ff       	jmpq   4023f0 <init_driver+0xc3>
  4024ab:	ba 2e 2f 40 00       	mov    $0x402f2e,%edx
  4024b0:	be d0 30 40 00       	mov    $0x4030d0,%esi
  4024b5:	48 89 ef             	mov    %rbp,%rdi
  4024b8:	b8 00 00 00 00       	mov    $0x0,%eax
  4024bd:	e8 1e e8 ff ff       	callq  400ce0 <sprintf@plt>
  4024c2:	89 df                	mov    %ebx,%edi
  4024c4:	e8 37 e7 ff ff       	callq  400c00 <close@plt>
  4024c9:	b8 ff ff ff ff       	mov    $0xffffffff,%eax
  4024ce:	e9 1d ff ff ff       	jmpq   4023f0 <init_driver+0xc3>

00000000004024d3 <driver_post>:
  4024d3:	53                   	push   %rbx
  4024d4:	4c 89 c3             	mov    %r8,%rbx
  4024d7:	85 c9                	test   %ecx,%ecx
  4024d9:	75 17                	jne    4024f2 <driver_post+0x1f>
  4024db:	48 85 ff             	test   %rdi,%rdi
  4024de:	74 05                	je     4024e5 <driver_post+0x12>
  4024e0:	80 3f 00             	cmpb   $0x0,(%rdi)
  4024e3:	75 2f                	jne    402514 <driver_post+0x41>
  4024e5:	66 c7 03 4f 4b       	movw   $0x4b4f,(%rbx)
  4024ea:	c6 43 02 00          	movb   $0x0,0x2(%rbx)
  4024ee:	89 c8                	mov    %ecx,%eax
  4024f0:	5b                   	pop    %rbx
  4024f1:	c3                   	retq   
  4024f2:	48 89 d6             	mov    %rdx,%rsi
  4024f5:	bf 10 31 40 00       	mov    $0x403110,%edi
  4024fa:	b8 00 00 00 00       	mov    $0x0,%eax
  4024ff:	e8 dc e6 ff ff       	callq  400be0 <printf@plt>
  402504:	66 c7 03 4f 4b       	movw   $0x4b4f,(%rbx)
  402509:	c6 43 02 00          	movb   $0x0,0x2(%rbx)
  40250d:	b8 00 00 00 00       	mov    $0x0,%eax
  402512:	eb dc                	jmp    4024f0 <driver_post+0x1d>
  402514:	41 50                	push   %r8
  402516:	52                   	push   %rdx
  402517:	41 b9 27 31 40 00    	mov    $0x403127,%r9d
  40251d:	49 89 f0             	mov    %rsi,%r8
  402520:	48 89 f9             	mov    %rdi,%rcx
  402523:	ba 2b 31 40 00       	mov    $0x40312b,%edx
  402528:	be 6e 3b 00 00       	mov    $0x3b6e,%esi
  40252d:	bf 2e 2f 40 00       	mov    $0x402f2e,%edi
  402532:	e8 24 f6 ff ff       	callq  401b5b <submitr>
  402537:	48 83 c4 10          	add    $0x10,%rsp
  40253b:	eb b3                	jmp    4024f0 <driver_post+0x1d>
  40253d:	0f 1f 00             	nopl   (%rax)

0000000000402540 <__libc_csu_init>:
  402540:	41 57                	push   %r15
  402542:	41 89 ff             	mov    %edi,%r15d
  402545:	41 56                	push   %r14
  402547:	49 89 f6             	mov    %rsi,%r14
  40254a:	41 55                	push   %r13
  40254c:	49 89 d5             	mov    %rdx,%r13
  40254f:	41 54                	push   %r12
  402551:	4c 8d 25 c0 18 20 00 	lea    0x2018c0(%rip),%r12        # 603e18 <__frame_dummy_init_array_entry>
  402558:	55                   	push   %rbp
  402559:	48 8d 2d c0 18 20 00 	lea    0x2018c0(%rip),%rbp        # 603e20 <__init_array_end>
  402560:	53                   	push   %rbx
  402561:	4c 29 e5             	sub    %r12,%rbp
  402564:	31 db                	xor    %ebx,%ebx
  402566:	48 c1 fd 03          	sar    $0x3,%rbp
  40256a:	48 83 ec 08          	sub    $0x8,%rsp
  40256e:	e8 c5 e5 ff ff       	callq  400b38 <_init>
  402573:	48 85 ed             	test   %rbp,%rbp
  402576:	74 1e                	je     402596 <__libc_csu_init+0x56>
  402578:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  40257f:	00 
  402580:	4c 89 ea             	mov    %r13,%rdx
  402583:	4c 89 f6             	mov    %r14,%rsi
  402586:	44 89 ff             	mov    %r15d,%edi
  402589:	41 ff 14 dc          	callq  *(%r12,%rbx,8)
  40258d:	48 83 c3 01          	add    $0x1,%rbx
  402591:	48 39 eb             	cmp    %rbp,%rbx
  402594:	75 ea                	jne    402580 <__libc_csu_init+0x40>
  402596:	48 83 c4 08          	add    $0x8,%rsp
  40259a:	5b                   	pop    %rbx
  40259b:	5d                   	pop    %rbp
  40259c:	41 5c                	pop    %r12
  40259e:	41 5d                	pop    %r13
  4025a0:	41 5e                	pop    %r14
  4025a2:	41 5f                	pop    %r15
  4025a4:	c3                   	retq   
  4025a5:	90                   	nop
  4025a6:	66 2e 0f 1f 84 00 00 	nopw   %cs:0x0(%rax,%rax,1)
  4025ad:	00 00 00 

00000000004025b0 <__libc_csu_fini>:
  4025b0:	f3 c3                	repz retq 

Disassembly of section .fini:

00000000004025b4 <_fini>:
  4025b4:	48 83 ec 08          	sub    $0x8,%rsp
  4025b8:	48 83 c4 08          	add    $0x8,%rsp
  4025bc:	c3                   	retq   
