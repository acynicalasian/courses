
rtarget:     file format elf64-x86-64


Disassembly of section .init:

0000000000400bc8 <_init>:
  400bc8:	48 83 ec 08          	sub    $0x8,%rsp
  400bcc:	48 8b 05 25 44 20 00 	mov    0x204425(%rip),%rax        # 604ff8 <__gmon_start__>
  400bd3:	48 85 c0             	test   %rax,%rax
  400bd6:	74 05                	je     400bdd <_init+0x15>
  400bd8:	e8 33 01 00 00       	callq  400d10 <__gmon_start__@plt>
  400bdd:	48 83 c4 08          	add    $0x8,%rsp
  400be1:	c3                   	retq   

Disassembly of section .plt:

0000000000400bf0 <.plt>:
  400bf0:	ff 35 12 44 20 00    	pushq  0x204412(%rip)        # 605008 <_GLOBAL_OFFSET_TABLE_+0x8>
  400bf6:	ff 25 14 44 20 00    	jmpq   *0x204414(%rip)        # 605010 <_GLOBAL_OFFSET_TABLE_+0x10>
  400bfc:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000400c00 <strcasecmp@plt>:
  400c00:	ff 25 12 44 20 00    	jmpq   *0x204412(%rip)        # 605018 <strcasecmp@GLIBC_2.2.5>
  400c06:	68 00 00 00 00       	pushq  $0x0
  400c0b:	e9 e0 ff ff ff       	jmpq   400bf0 <.plt>

0000000000400c10 <__errno_location@plt>:
  400c10:	ff 25 0a 44 20 00    	jmpq   *0x20440a(%rip)        # 605020 <__errno_location@GLIBC_2.2.5>
  400c16:	68 01 00 00 00       	pushq  $0x1
  400c1b:	e9 d0 ff ff ff       	jmpq   400bf0 <.plt>

0000000000400c20 <srandom@plt>:
  400c20:	ff 25 02 44 20 00    	jmpq   *0x204402(%rip)        # 605028 <srandom@GLIBC_2.2.5>
  400c26:	68 02 00 00 00       	pushq  $0x2
  400c2b:	e9 c0 ff ff ff       	jmpq   400bf0 <.plt>

0000000000400c30 <strncmp@plt>:
  400c30:	ff 25 fa 43 20 00    	jmpq   *0x2043fa(%rip)        # 605030 <strncmp@GLIBC_2.2.5>
  400c36:	68 03 00 00 00       	pushq  $0x3
  400c3b:	e9 b0 ff ff ff       	jmpq   400bf0 <.plt>

0000000000400c40 <strcpy@plt>:
  400c40:	ff 25 f2 43 20 00    	jmpq   *0x2043f2(%rip)        # 605038 <strcpy@GLIBC_2.2.5>
  400c46:	68 04 00 00 00       	pushq  $0x4
  400c4b:	e9 a0 ff ff ff       	jmpq   400bf0 <.plt>

0000000000400c50 <puts@plt>:
  400c50:	ff 25 ea 43 20 00    	jmpq   *0x2043ea(%rip)        # 605040 <puts@GLIBC_2.2.5>
  400c56:	68 05 00 00 00       	pushq  $0x5
  400c5b:	e9 90 ff ff ff       	jmpq   400bf0 <.plt>

0000000000400c60 <write@plt>:
  400c60:	ff 25 e2 43 20 00    	jmpq   *0x2043e2(%rip)        # 605048 <write@GLIBC_2.2.5>
  400c66:	68 06 00 00 00       	pushq  $0x6
  400c6b:	e9 80 ff ff ff       	jmpq   400bf0 <.plt>

0000000000400c70 <mmap@plt>:
  400c70:	ff 25 da 43 20 00    	jmpq   *0x2043da(%rip)        # 605050 <mmap@GLIBC_2.2.5>
  400c76:	68 07 00 00 00       	pushq  $0x7
  400c7b:	e9 70 ff ff ff       	jmpq   400bf0 <.plt>

0000000000400c80 <printf@plt>:
  400c80:	ff 25 d2 43 20 00    	jmpq   *0x2043d2(%rip)        # 605058 <printf@GLIBC_2.2.5>
  400c86:	68 08 00 00 00       	pushq  $0x8
  400c8b:	e9 60 ff ff ff       	jmpq   400bf0 <.plt>

0000000000400c90 <memset@plt>:
  400c90:	ff 25 ca 43 20 00    	jmpq   *0x2043ca(%rip)        # 605060 <memset@GLIBC_2.2.5>
  400c96:	68 09 00 00 00       	pushq  $0x9
  400c9b:	e9 50 ff ff ff       	jmpq   400bf0 <.plt>

0000000000400ca0 <alarm@plt>:
  400ca0:	ff 25 c2 43 20 00    	jmpq   *0x2043c2(%rip)        # 605068 <alarm@GLIBC_2.2.5>
  400ca6:	68 0a 00 00 00       	pushq  $0xa
  400cab:	e9 40 ff ff ff       	jmpq   400bf0 <.plt>

0000000000400cb0 <close@plt>:
  400cb0:	ff 25 ba 43 20 00    	jmpq   *0x2043ba(%rip)        # 605070 <close@GLIBC_2.2.5>
  400cb6:	68 0b 00 00 00       	pushq  $0xb
  400cbb:	e9 30 ff ff ff       	jmpq   400bf0 <.plt>

0000000000400cc0 <read@plt>:
  400cc0:	ff 25 b2 43 20 00    	jmpq   *0x2043b2(%rip)        # 605078 <read@GLIBC_2.2.5>
  400cc6:	68 0c 00 00 00       	pushq  $0xc
  400ccb:	e9 20 ff ff ff       	jmpq   400bf0 <.plt>

0000000000400cd0 <__libc_start_main@plt>:
  400cd0:	ff 25 aa 43 20 00    	jmpq   *0x2043aa(%rip)        # 605080 <__libc_start_main@GLIBC_2.2.5>
  400cd6:	68 0d 00 00 00       	pushq  $0xd
  400cdb:	e9 10 ff ff ff       	jmpq   400bf0 <.plt>

0000000000400ce0 <signal@plt>:
  400ce0:	ff 25 a2 43 20 00    	jmpq   *0x2043a2(%rip)        # 605088 <signal@GLIBC_2.2.5>
  400ce6:	68 0e 00 00 00       	pushq  $0xe
  400ceb:	e9 00 ff ff ff       	jmpq   400bf0 <.plt>

0000000000400cf0 <gethostbyname@plt>:
  400cf0:	ff 25 9a 43 20 00    	jmpq   *0x20439a(%rip)        # 605090 <gethostbyname@GLIBC_2.2.5>
  400cf6:	68 0f 00 00 00       	pushq  $0xf
  400cfb:	e9 f0 fe ff ff       	jmpq   400bf0 <.plt>

0000000000400d00 <fprintf@plt>:
  400d00:	ff 25 92 43 20 00    	jmpq   *0x204392(%rip)        # 605098 <fprintf@GLIBC_2.2.5>
  400d06:	68 10 00 00 00       	pushq  $0x10
  400d0b:	e9 e0 fe ff ff       	jmpq   400bf0 <.plt>

0000000000400d10 <__gmon_start__@plt>:
  400d10:	ff 25 8a 43 20 00    	jmpq   *0x20438a(%rip)        # 6050a0 <__gmon_start__>
  400d16:	68 11 00 00 00       	pushq  $0x11
  400d1b:	e9 d0 fe ff ff       	jmpq   400bf0 <.plt>

0000000000400d20 <strtol@plt>:
  400d20:	ff 25 82 43 20 00    	jmpq   *0x204382(%rip)        # 6050a8 <strtol@GLIBC_2.2.5>
  400d26:	68 12 00 00 00       	pushq  $0x12
  400d2b:	e9 c0 fe ff ff       	jmpq   400bf0 <.plt>

0000000000400d30 <memcpy@plt>:
  400d30:	ff 25 7a 43 20 00    	jmpq   *0x20437a(%rip)        # 6050b0 <memcpy@GLIBC_2.14>
  400d36:	68 13 00 00 00       	pushq  $0x13
  400d3b:	e9 b0 fe ff ff       	jmpq   400bf0 <.plt>

0000000000400d40 <time@plt>:
  400d40:	ff 25 72 43 20 00    	jmpq   *0x204372(%rip)        # 6050b8 <time@GLIBC_2.2.5>
  400d46:	68 14 00 00 00       	pushq  $0x14
  400d4b:	e9 a0 fe ff ff       	jmpq   400bf0 <.plt>

0000000000400d50 <random@plt>:
  400d50:	ff 25 6a 43 20 00    	jmpq   *0x20436a(%rip)        # 6050c0 <random@GLIBC_2.2.5>
  400d56:	68 15 00 00 00       	pushq  $0x15
  400d5b:	e9 90 fe ff ff       	jmpq   400bf0 <.plt>

0000000000400d60 <_IO_getc@plt>:
  400d60:	ff 25 62 43 20 00    	jmpq   *0x204362(%rip)        # 6050c8 <_IO_getc@GLIBC_2.2.5>
  400d66:	68 16 00 00 00       	pushq  $0x16
  400d6b:	e9 80 fe ff ff       	jmpq   400bf0 <.plt>

0000000000400d70 <__isoc99_sscanf@plt>:
  400d70:	ff 25 5a 43 20 00    	jmpq   *0x20435a(%rip)        # 6050d0 <__isoc99_sscanf@GLIBC_2.7>
  400d76:	68 17 00 00 00       	pushq  $0x17
  400d7b:	e9 70 fe ff ff       	jmpq   400bf0 <.plt>

0000000000400d80 <munmap@plt>:
  400d80:	ff 25 52 43 20 00    	jmpq   *0x204352(%rip)        # 6050d8 <munmap@GLIBC_2.2.5>
  400d86:	68 18 00 00 00       	pushq  $0x18
  400d8b:	e9 60 fe ff ff       	jmpq   400bf0 <.plt>

0000000000400d90 <bcopy@plt>:
  400d90:	ff 25 4a 43 20 00    	jmpq   *0x20434a(%rip)        # 6050e0 <bcopy@GLIBC_2.2.5>
  400d96:	68 19 00 00 00       	pushq  $0x19
  400d9b:	e9 50 fe ff ff       	jmpq   400bf0 <.plt>

0000000000400da0 <fopen@plt>:
  400da0:	ff 25 42 43 20 00    	jmpq   *0x204342(%rip)        # 6050e8 <fopen@GLIBC_2.2.5>
  400da6:	68 1a 00 00 00       	pushq  $0x1a
  400dab:	e9 40 fe ff ff       	jmpq   400bf0 <.plt>

0000000000400db0 <getopt@plt>:
  400db0:	ff 25 3a 43 20 00    	jmpq   *0x20433a(%rip)        # 6050f0 <getopt@GLIBC_2.2.5>
  400db6:	68 1b 00 00 00       	pushq  $0x1b
  400dbb:	e9 30 fe ff ff       	jmpq   400bf0 <.plt>

0000000000400dc0 <strtoul@plt>:
  400dc0:	ff 25 32 43 20 00    	jmpq   *0x204332(%rip)        # 6050f8 <strtoul@GLIBC_2.2.5>
  400dc6:	68 1c 00 00 00       	pushq  $0x1c
  400dcb:	e9 20 fe ff ff       	jmpq   400bf0 <.plt>

0000000000400dd0 <gethostname@plt>:
  400dd0:	ff 25 2a 43 20 00    	jmpq   *0x20432a(%rip)        # 605100 <gethostname@GLIBC_2.2.5>
  400dd6:	68 1d 00 00 00       	pushq  $0x1d
  400ddb:	e9 10 fe ff ff       	jmpq   400bf0 <.plt>

0000000000400de0 <sprintf@plt>:
  400de0:	ff 25 22 43 20 00    	jmpq   *0x204322(%rip)        # 605108 <sprintf@GLIBC_2.2.5>
  400de6:	68 1e 00 00 00       	pushq  $0x1e
  400deb:	e9 00 fe ff ff       	jmpq   400bf0 <.plt>

0000000000400df0 <exit@plt>:
  400df0:	ff 25 1a 43 20 00    	jmpq   *0x20431a(%rip)        # 605110 <exit@GLIBC_2.2.5>
  400df6:	68 1f 00 00 00       	pushq  $0x1f
  400dfb:	e9 f0 fd ff ff       	jmpq   400bf0 <.plt>

0000000000400e00 <connect@plt>:
  400e00:	ff 25 12 43 20 00    	jmpq   *0x204312(%rip)        # 605118 <connect@GLIBC_2.2.5>
  400e06:	68 20 00 00 00       	pushq  $0x20
  400e0b:	e9 e0 fd ff ff       	jmpq   400bf0 <.plt>

0000000000400e10 <socket@plt>:
  400e10:	ff 25 0a 43 20 00    	jmpq   *0x20430a(%rip)        # 605120 <socket@GLIBC_2.2.5>
  400e16:	68 21 00 00 00       	pushq  $0x21
  400e1b:	e9 d0 fd ff ff       	jmpq   400bf0 <.plt>

Disassembly of section .text:

0000000000400e20 <_start>:
  400e20:	31 ed                	xor    %ebp,%ebp
  400e22:	49 89 d1             	mov    %rdx,%r9
  400e25:	5e                   	pop    %rsi
  400e26:	48 89 e2             	mov    %rsp,%rdx
  400e29:	48 83 e4 f0          	and    $0xfffffffffffffff0,%rsp
  400e2d:	50                   	push   %rax
  400e2e:	54                   	push   %rsp
  400e2f:	49 c7 c0 80 2d 40 00 	mov    $0x402d80,%r8
  400e36:	48 c7 c1 10 2d 40 00 	mov    $0x402d10,%rcx
  400e3d:	48 c7 c7 e0 10 40 00 	mov    $0x4010e0,%rdi
  400e44:	e8 87 fe ff ff       	callq  400cd0 <__libc_start_main@plt>
  400e49:	f4                   	hlt    
  400e4a:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

0000000000400e50 <deregister_tm_clones>:
  400e50:	b8 b7 54 60 00       	mov    $0x6054b7,%eax
  400e55:	55                   	push   %rbp
  400e56:	48 2d b0 54 60 00    	sub    $0x6054b0,%rax
  400e5c:	48 83 f8 0e          	cmp    $0xe,%rax
  400e60:	48 89 e5             	mov    %rsp,%rbp
  400e63:	77 02                	ja     400e67 <deregister_tm_clones+0x17>
  400e65:	5d                   	pop    %rbp
  400e66:	c3                   	retq   
  400e67:	b8 00 00 00 00       	mov    $0x0,%eax
  400e6c:	48 85 c0             	test   %rax,%rax
  400e6f:	74 f4                	je     400e65 <deregister_tm_clones+0x15>
  400e71:	5d                   	pop    %rbp
  400e72:	bf b0 54 60 00       	mov    $0x6054b0,%edi
  400e77:	ff e0                	jmpq   *%rax
  400e79:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)

0000000000400e80 <register_tm_clones>:
  400e80:	b8 b0 54 60 00       	mov    $0x6054b0,%eax
  400e85:	55                   	push   %rbp
  400e86:	48 2d b0 54 60 00    	sub    $0x6054b0,%rax
  400e8c:	48 c1 f8 03          	sar    $0x3,%rax
  400e90:	48 89 e5             	mov    %rsp,%rbp
  400e93:	48 89 c2             	mov    %rax,%rdx
  400e96:	48 c1 ea 3f          	shr    $0x3f,%rdx
  400e9a:	48 01 d0             	add    %rdx,%rax
  400e9d:	48 d1 f8             	sar    %rax
  400ea0:	75 02                	jne    400ea4 <register_tm_clones+0x24>
  400ea2:	5d                   	pop    %rbp
  400ea3:	c3                   	retq   
  400ea4:	ba 00 00 00 00       	mov    $0x0,%edx
  400ea9:	48 85 d2             	test   %rdx,%rdx
  400eac:	74 f4                	je     400ea2 <register_tm_clones+0x22>
  400eae:	5d                   	pop    %rbp
  400eaf:	48 89 c6             	mov    %rax,%rsi
  400eb2:	bf b0 54 60 00       	mov    $0x6054b0,%edi
  400eb7:	ff e2                	jmpq   *%rdx
  400eb9:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)

0000000000400ec0 <__do_global_dtors_aux>:
  400ec0:	80 3d 11 46 20 00 00 	cmpb   $0x0,0x204611(%rip)        # 6054d8 <completed.6355>
  400ec7:	75 11                	jne    400eda <__do_global_dtors_aux+0x1a>
  400ec9:	55                   	push   %rbp
  400eca:	48 89 e5             	mov    %rsp,%rbp
  400ecd:	e8 7e ff ff ff       	callq  400e50 <deregister_tm_clones>
  400ed2:	5d                   	pop    %rbp
  400ed3:	c6 05 fe 45 20 00 01 	movb   $0x1,0x2045fe(%rip)        # 6054d8 <completed.6355>
  400eda:	f3 c3                	repz retq 
  400edc:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000400ee0 <frame_dummy>:
  400ee0:	48 83 3d 38 3f 20 00 	cmpq   $0x0,0x203f38(%rip)        # 604e20 <__JCR_END__>
  400ee7:	00 
  400ee8:	74 1e                	je     400f08 <frame_dummy+0x28>
  400eea:	b8 00 00 00 00       	mov    $0x0,%eax
  400eef:	48 85 c0             	test   %rax,%rax
  400ef2:	74 14                	je     400f08 <frame_dummy+0x28>
  400ef4:	55                   	push   %rbp
  400ef5:	bf 20 4e 60 00       	mov    $0x604e20,%edi
  400efa:	48 89 e5             	mov    %rsp,%rbp
  400efd:	ff d0                	callq  *%rax
  400eff:	5d                   	pop    %rbp
  400f00:	e9 7b ff ff ff       	jmpq   400e80 <register_tm_clones>
  400f05:	0f 1f 00             	nopl   (%rax)
  400f08:	e9 73 ff ff ff       	jmpq   400e80 <register_tm_clones>
  400f0d:	0f 1f 00             	nopl   (%rax)

0000000000400f10 <usage>:
  400f10:	48 83 ec 08          	sub    $0x8,%rsp
  400f14:	48 89 fe             	mov    %rdi,%rsi
  400f17:	83 3d ea 45 20 00 00 	cmpl   $0x0,0x2045ea(%rip)        # 605508 <is_checker>
  400f1e:	74 39                	je     400f59 <usage+0x49>
  400f20:	bf a0 2d 40 00       	mov    $0x402da0,%edi
  400f25:	b8 00 00 00 00       	mov    $0x0,%eax
  400f2a:	e8 51 fd ff ff       	callq  400c80 <printf@plt>
  400f2f:	bf d8 2d 40 00       	mov    $0x402dd8,%edi
  400f34:	e8 17 fd ff ff       	callq  400c50 <puts@plt>
  400f39:	bf 50 2f 40 00       	mov    $0x402f50,%edi
  400f3e:	e8 0d fd ff ff       	callq  400c50 <puts@plt>
  400f43:	bf 00 2e 40 00       	mov    $0x402e00,%edi
  400f48:	e8 03 fd ff ff       	callq  400c50 <puts@plt>
  400f4d:	bf 6a 2f 40 00       	mov    $0x402f6a,%edi
  400f52:	e8 f9 fc ff ff       	callq  400c50 <puts@plt>
  400f57:	eb 2d                	jmp    400f86 <usage+0x76>
  400f59:	bf 86 2f 40 00       	mov    $0x402f86,%edi
  400f5e:	b8 00 00 00 00       	mov    $0x0,%eax
  400f63:	e8 18 fd ff ff       	callq  400c80 <printf@plt>
  400f68:	bf 28 2e 40 00       	mov    $0x402e28,%edi
  400f6d:	e8 de fc ff ff       	callq  400c50 <puts@plt>
  400f72:	bf 50 2e 40 00       	mov    $0x402e50,%edi
  400f77:	e8 d4 fc ff ff       	callq  400c50 <puts@plt>
  400f7c:	bf a4 2f 40 00       	mov    $0x402fa4,%edi
  400f81:	e8 ca fc ff ff       	callq  400c50 <puts@plt>
  400f86:	bf 00 00 00 00       	mov    $0x0,%edi
  400f8b:	e8 60 fe ff ff       	callq  400df0 <exit@plt>

0000000000400f90 <initialize_target>:
  400f90:	55                   	push   %rbp
  400f91:	53                   	push   %rbx
  400f92:	48 81 ec 08 21 00 00 	sub    $0x2108,%rsp
  400f99:	89 f5                	mov    %esi,%ebp
  400f9b:	89 3d 57 45 20 00    	mov    %edi,0x204557(%rip)        # 6054f8 <check_level>
  400fa1:	8b 3d c1 41 20 00    	mov    0x2041c1(%rip),%edi        # 605168 <target_id>
  400fa7:	e8 37 1d 00 00       	callq  402ce3 <gencookie>
  400fac:	89 05 52 45 20 00    	mov    %eax,0x204552(%rip)        # 605504 <cookie>
  400fb2:	89 c7                	mov    %eax,%edi
  400fb4:	e8 2a 1d 00 00       	callq  402ce3 <gencookie>
  400fb9:	89 05 41 45 20 00    	mov    %eax,0x204541(%rip)        # 605500 <authkey>
  400fbf:	8b 05 a3 41 20 00    	mov    0x2041a3(%rip),%eax        # 605168 <target_id>
  400fc5:	8d 78 01             	lea    0x1(%rax),%edi
  400fc8:	e8 53 fc ff ff       	callq  400c20 <srandom@plt>
  400fcd:	e8 7e fd ff ff       	callq  400d50 <random@plt>
  400fd2:	89 c7                	mov    %eax,%edi
  400fd4:	e8 ca 02 00 00       	callq  4012a3 <scramble>
  400fd9:	89 c3                	mov    %eax,%ebx
  400fdb:	85 ed                	test   %ebp,%ebp
  400fdd:	74 18                	je     400ff7 <initialize_target+0x67>
  400fdf:	bf 00 00 00 00       	mov    $0x0,%edi
  400fe4:	e8 57 fd ff ff       	callq  400d40 <time@plt>
  400fe9:	89 c7                	mov    %eax,%edi
  400feb:	e8 30 fc ff ff       	callq  400c20 <srandom@plt>
  400ff0:	e8 5b fd ff ff       	callq  400d50 <random@plt>
  400ff5:	eb 05                	jmp    400ffc <initialize_target+0x6c>
  400ff7:	b8 00 00 00 00       	mov    $0x0,%eax
  400ffc:	01 c3                	add    %eax,%ebx
  400ffe:	0f b7 db             	movzwl %bx,%ebx
  401001:	8d 04 dd 00 01 00 00 	lea    0x100(,%rbx,8),%eax
  401008:	89 c0                	mov    %eax,%eax
  40100a:	48 89 05 8f 44 20 00 	mov    %rax,0x20448f(%rip)        # 6054a0 <buf_offset>
  401011:	c6 05 10 51 20 00 72 	movb   $0x72,0x205110(%rip)        # 606128 <target_prefix>
  401018:	83 3d 89 44 20 00 00 	cmpl   $0x0,0x204489(%rip)        # 6054a8 <notify>
  40101f:	0f 84 b1 00 00 00    	je     4010d6 <initialize_target+0x146>
  401025:	83 3d dc 44 20 00 00 	cmpl   $0x0,0x2044dc(%rip)        # 605508 <is_checker>
  40102c:	0f 85 a4 00 00 00    	jne    4010d6 <initialize_target+0x146>
  401032:	be 00 01 00 00       	mov    $0x100,%esi
  401037:	48 89 e7             	mov    %rsp,%rdi
  40103a:	e8 91 fd ff ff       	callq  400dd0 <gethostname@plt>
  40103f:	85 c0                	test   %eax,%eax
  401041:	74 25                	je     401068 <initialize_target+0xd8>
  401043:	bf 80 2e 40 00       	mov    $0x402e80,%edi
  401048:	e8 03 fc ff ff       	callq  400c50 <puts@plt>
  40104d:	bf 08 00 00 00       	mov    $0x8,%edi
  401052:	e8 99 fd ff ff       	callq  400df0 <exit@plt>
  401057:	48 89 e6             	mov    %rsp,%rsi
  40105a:	e8 a1 fb ff ff       	callq  400c00 <strcasecmp@plt>
  40105f:	85 c0                	test   %eax,%eax
  401061:	74 21                	je     401084 <initialize_target+0xf4>
  401063:	83 c3 01             	add    $0x1,%ebx
  401066:	eb 05                	jmp    40106d <initialize_target+0xdd>
  401068:	bb 00 00 00 00       	mov    $0x0,%ebx
  40106d:	48 63 c3             	movslq %ebx,%rax
  401070:	48 8b 3c c5 80 51 60 	mov    0x605180(,%rax,8),%rdi
  401077:	00 
  401078:	48 85 ff             	test   %rdi,%rdi
  40107b:	75 da                	jne    401057 <initialize_target+0xc7>
  40107d:	b8 00 00 00 00       	mov    $0x0,%eax
  401082:	eb 05                	jmp    401089 <initialize_target+0xf9>
  401084:	b8 01 00 00 00       	mov    $0x1,%eax
  401089:	85 c0                	test   %eax,%eax
  40108b:	75 17                	jne    4010a4 <initialize_target+0x114>
  40108d:	48 89 e6             	mov    %rsp,%rsi
  401090:	bf b8 2e 40 00       	mov    $0x402eb8,%edi
  401095:	e8 e6 fb ff ff       	callq  400c80 <printf@plt>
  40109a:	bf 08 00 00 00       	mov    $0x8,%edi
  40109f:	e8 4c fd ff ff       	callq  400df0 <exit@plt>
  4010a4:	48 8d bc 24 00 01 00 	lea    0x100(%rsp),%rdi
  4010ab:	00 
  4010ac:	e8 c9 19 00 00       	callq  402a7a <init_driver>
  4010b1:	85 c0                	test   %eax,%eax
  4010b3:	79 21                	jns    4010d6 <initialize_target+0x146>
  4010b5:	48 8d b4 24 00 01 00 	lea    0x100(%rsp),%rsi
  4010bc:	00 
  4010bd:	bf f8 2e 40 00       	mov    $0x402ef8,%edi
  4010c2:	b8 00 00 00 00       	mov    $0x0,%eax
  4010c7:	e8 b4 fb ff ff       	callq  400c80 <printf@plt>
  4010cc:	bf 08 00 00 00       	mov    $0x8,%edi
  4010d1:	e8 1a fd ff ff       	callq  400df0 <exit@plt>
  4010d6:	48 81 c4 08 21 00 00 	add    $0x2108,%rsp
  4010dd:	5b                   	pop    %rbx
  4010de:	5d                   	pop    %rbp
  4010df:	c3                   	retq   

00000000004010e0 <main>:
  4010e0:	41 56                	push   %r14
  4010e2:	41 55                	push   %r13
  4010e4:	41 54                	push   %r12
  4010e6:	55                   	push   %rbp
  4010e7:	53                   	push   %rbx
  4010e8:	41 89 fc             	mov    %edi,%r12d
  4010eb:	48 89 f3             	mov    %rsi,%rbx
  4010ee:	be a1 1e 40 00       	mov    $0x401ea1,%esi
  4010f3:	bf 0b 00 00 00       	mov    $0xb,%edi
  4010f8:	e8 e3 fb ff ff       	callq  400ce0 <signal@plt>
  4010fd:	be 53 1e 40 00       	mov    $0x401e53,%esi
  401102:	bf 07 00 00 00       	mov    $0x7,%edi
  401107:	e8 d4 fb ff ff       	callq  400ce0 <signal@plt>
  40110c:	be ef 1e 40 00       	mov    $0x401eef,%esi
  401111:	bf 04 00 00 00       	mov    $0x4,%edi
  401116:	e8 c5 fb ff ff       	callq  400ce0 <signal@plt>
  40111b:	83 3d e6 43 20 00 00 	cmpl   $0x0,0x2043e6(%rip)        # 605508 <is_checker>
  401122:	74 20                	je     401144 <main+0x64>
  401124:	be 3d 1f 40 00       	mov    $0x401f3d,%esi
  401129:	bf 0e 00 00 00       	mov    $0xe,%edi
  40112e:	e8 ad fb ff ff       	callq  400ce0 <signal@plt>
  401133:	bf 05 00 00 00       	mov    $0x5,%edi
  401138:	e8 63 fb ff ff       	callq  400ca0 <alarm@plt>
  40113d:	bd c2 2f 40 00       	mov    $0x402fc2,%ebp
  401142:	eb 05                	jmp    401149 <main+0x69>
  401144:	bd bd 2f 40 00       	mov    $0x402fbd,%ebp
  401149:	48 8b 05 70 43 20 00 	mov    0x204370(%rip),%rax        # 6054c0 <stdin@@GLIBC_2.2.5>
  401150:	48 89 05 99 43 20 00 	mov    %rax,0x204399(%rip)        # 6054f0 <infile>
  401157:	41 bd 00 00 00 00    	mov    $0x0,%r13d
  40115d:	41 be 00 00 00 00    	mov    $0x0,%r14d
  401163:	e9 b9 00 00 00       	jmpq   401221 <main+0x141>
  401168:	83 e8 61             	sub    $0x61,%eax
  40116b:	3c 10                	cmp    $0x10,%al
  40116d:	0f 87 93 00 00 00    	ja     401206 <main+0x126>
  401173:	0f b6 c0             	movzbl %al,%eax
  401176:	ff 24 c5 08 30 40 00 	jmpq   *0x403008(,%rax,8)
  40117d:	48 8b 3b             	mov    (%rbx),%rdi
  401180:	e8 8b fd ff ff       	callq  400f10 <usage>
  401185:	be 5d 33 40 00       	mov    $0x40335d,%esi
  40118a:	48 8b 3d 37 43 20 00 	mov    0x204337(%rip),%rdi        # 6054c8 <optarg@@GLIBC_2.2.5>
  401191:	e8 0a fc ff ff       	callq  400da0 <fopen@plt>
  401196:	48 89 05 53 43 20 00 	mov    %rax,0x204353(%rip)        # 6054f0 <infile>
  40119d:	48 85 c0             	test   %rax,%rax
  4011a0:	75 7f                	jne    401221 <main+0x141>
  4011a2:	48 8b 15 1f 43 20 00 	mov    0x20431f(%rip),%rdx        # 6054c8 <optarg@@GLIBC_2.2.5>
  4011a9:	be ca 2f 40 00       	mov    $0x402fca,%esi
  4011ae:	48 8b 3d 1b 43 20 00 	mov    0x20431b(%rip),%rdi        # 6054d0 <stderr@@GLIBC_2.2.5>
  4011b5:	e8 46 fb ff ff       	callq  400d00 <fprintf@plt>
  4011ba:	b8 01 00 00 00       	mov    $0x1,%eax
  4011bf:	e9 d6 00 00 00       	jmpq   40129a <main+0x1ba>
  4011c4:	ba 10 00 00 00       	mov    $0x10,%edx
  4011c9:	be 00 00 00 00       	mov    $0x0,%esi
  4011ce:	48 8b 3d f3 42 20 00 	mov    0x2042f3(%rip),%rdi        # 6054c8 <optarg@@GLIBC_2.2.5>
  4011d5:	e8 e6 fb ff ff       	callq  400dc0 <strtoul@plt>
  4011da:	41 89 c6             	mov    %eax,%r14d
  4011dd:	eb 42                	jmp    401221 <main+0x141>
  4011df:	ba 0a 00 00 00       	mov    $0xa,%edx
  4011e4:	be 00 00 00 00       	mov    $0x0,%esi
  4011e9:	48 8b 3d d8 42 20 00 	mov    0x2042d8(%rip),%rdi        # 6054c8 <optarg@@GLIBC_2.2.5>
  4011f0:	e8 2b fb ff ff       	callq  400d20 <strtol@plt>
  4011f5:	41 89 c5             	mov    %eax,%r13d
  4011f8:	eb 27                	jmp    401221 <main+0x141>
  4011fa:	c7 05 a4 42 20 00 00 	movl   $0x0,0x2042a4(%rip)        # 6054a8 <notify>
  401201:	00 00 00 
  401204:	eb 1b                	jmp    401221 <main+0x141>
  401206:	40 0f be f6          	movsbl %sil,%esi
  40120a:	bf e7 2f 40 00       	mov    $0x402fe7,%edi
  40120f:	b8 00 00 00 00       	mov    $0x0,%eax
  401214:	e8 67 fa ff ff       	callq  400c80 <printf@plt>
  401219:	48 8b 3b             	mov    (%rbx),%rdi
  40121c:	e8 ef fc ff ff       	callq  400f10 <usage>
  401221:	48 89 ea             	mov    %rbp,%rdx
  401224:	48 89 de             	mov    %rbx,%rsi
  401227:	44 89 e7             	mov    %r12d,%edi
  40122a:	e8 81 fb ff ff       	callq  400db0 <getopt@plt>
  40122f:	89 c6                	mov    %eax,%esi
  401231:	3c ff                	cmp    $0xff,%al
  401233:	0f 85 2f ff ff ff    	jne    401168 <main+0x88>
  401239:	be 01 00 00 00       	mov    $0x1,%esi
  40123e:	44 89 ef             	mov    %r13d,%edi
  401241:	e8 4a fd ff ff       	callq  400f90 <initialize_target>
  401246:	83 3d bb 42 20 00 00 	cmpl   $0x0,0x2042bb(%rip)        # 605508 <is_checker>
  40124d:	74 25                	je     401274 <main+0x194>
  40124f:	44 3b 35 aa 42 20 00 	cmp    0x2042aa(%rip),%r14d        # 605500 <authkey>
  401256:	74 1c                	je     401274 <main+0x194>
  401258:	44 89 f6             	mov    %r14d,%esi
  40125b:	bf 20 2f 40 00       	mov    $0x402f20,%edi
  401260:	b8 00 00 00 00       	mov    $0x0,%eax
  401265:	e8 16 fa ff ff       	callq  400c80 <printf@plt>
  40126a:	b8 00 00 00 00       	mov    $0x0,%eax
  40126f:	e8 e7 08 00 00       	callq  401b5b <check_fail>
  401274:	8b 35 8a 42 20 00    	mov    0x20428a(%rip),%esi        # 605504 <cookie>
  40127a:	bf fa 2f 40 00       	mov    $0x402ffa,%edi
  40127f:	b8 00 00 00 00       	mov    $0x0,%eax
  401284:	e8 f7 f9 ff ff       	callq  400c80 <printf@plt>
  401289:	48 8b 3d 10 42 20 00 	mov    0x204210(%rip),%rdi        # 6054a0 <buf_offset>
  401290:	e8 f6 0c 00 00       	callq  401f8b <launch>
  401295:	b8 00 00 00 00       	mov    $0x0,%eax
  40129a:	5b                   	pop    %rbx
  40129b:	5d                   	pop    %rbp
  40129c:	41 5c                	pop    %r12
  40129e:	41 5d                	pop    %r13
  4012a0:	41 5e                	pop    %r14
  4012a2:	c3                   	retq   

00000000004012a3 <scramble>:
  4012a3:	b8 00 00 00 00       	mov    $0x0,%eax
  4012a8:	eb 11                	jmp    4012bb <scramble+0x18>
  4012aa:	69 c8 93 82 00 00    	imul   $0x8293,%eax,%ecx
  4012b0:	01 f9                	add    %edi,%ecx
  4012b2:	89 c2                	mov    %eax,%edx
  4012b4:	89 4c 94 d0          	mov    %ecx,-0x30(%rsp,%rdx,4)
  4012b8:	83 c0 01             	add    $0x1,%eax
  4012bb:	83 f8 09             	cmp    $0x9,%eax
  4012be:	76 ea                	jbe    4012aa <scramble+0x7>
  4012c0:	8b 44 24 d0          	mov    -0x30(%rsp),%eax
  4012c4:	69 c0 81 d9 00 00    	imul   $0xd981,%eax,%eax
  4012ca:	89 44 24 d0          	mov    %eax,-0x30(%rsp)
  4012ce:	8b 44 24 e0          	mov    -0x20(%rsp),%eax
  4012d2:	69 c0 d5 3c 00 00    	imul   $0x3cd5,%eax,%eax
  4012d8:	89 44 24 e0          	mov    %eax,-0x20(%rsp)
  4012dc:	8b 44 24 dc          	mov    -0x24(%rsp),%eax
  4012e0:	69 c0 49 4e 00 00    	imul   $0x4e49,%eax,%eax
  4012e6:	89 44 24 dc          	mov    %eax,-0x24(%rsp)
  4012ea:	8b 44 24 d4          	mov    -0x2c(%rsp),%eax
  4012ee:	69 c0 5d ca 00 00    	imul   $0xca5d,%eax,%eax
  4012f4:	89 44 24 d4          	mov    %eax,-0x2c(%rsp)
  4012f8:	8b 44 24 d0          	mov    -0x30(%rsp),%eax
  4012fc:	69 c0 11 3c 00 00    	imul   $0x3c11,%eax,%eax
  401302:	89 44 24 d0          	mov    %eax,-0x30(%rsp)
  401306:	8b 44 24 d8          	mov    -0x28(%rsp),%eax
  40130a:	69 c0 f4 18 00 00    	imul   $0x18f4,%eax,%eax
  401310:	89 44 24 d8          	mov    %eax,-0x28(%rsp)
  401314:	8b 44 24 f4          	mov    -0xc(%rsp),%eax
  401318:	69 c0 4b 5f 00 00    	imul   $0x5f4b,%eax,%eax
  40131e:	89 44 24 f4          	mov    %eax,-0xc(%rsp)
  401322:	8b 44 24 e0          	mov    -0x20(%rsp),%eax
  401326:	69 c0 8c 6e 00 00    	imul   $0x6e8c,%eax,%eax
  40132c:	89 44 24 e0          	mov    %eax,-0x20(%rsp)
  401330:	8b 44 24 e4          	mov    -0x1c(%rsp),%eax
  401334:	69 c0 37 be 00 00    	imul   $0xbe37,%eax,%eax
  40133a:	89 44 24 e4          	mov    %eax,-0x1c(%rsp)
  40133e:	8b 44 24 ec          	mov    -0x14(%rsp),%eax
  401342:	69 c0 ca a8 00 00    	imul   $0xa8ca,%eax,%eax
  401348:	89 44 24 ec          	mov    %eax,-0x14(%rsp)
  40134c:	8b 44 24 f0          	mov    -0x10(%rsp),%eax
  401350:	69 c0 49 c9 00 00    	imul   $0xc949,%eax,%eax
  401356:	89 44 24 f0          	mov    %eax,-0x10(%rsp)
  40135a:	8b 44 24 dc          	mov    -0x24(%rsp),%eax
  40135e:	69 c0 6d 79 00 00    	imul   $0x796d,%eax,%eax
  401364:	89 44 24 dc          	mov    %eax,-0x24(%rsp)
  401368:	8b 44 24 dc          	mov    -0x24(%rsp),%eax
  40136c:	69 c0 4d 89 00 00    	imul   $0x894d,%eax,%eax
  401372:	89 44 24 dc          	mov    %eax,-0x24(%rsp)
  401376:	8b 44 24 dc          	mov    -0x24(%rsp),%eax
  40137a:	69 c0 2f 5a 00 00    	imul   $0x5a2f,%eax,%eax
  401380:	89 44 24 dc          	mov    %eax,-0x24(%rsp)
  401384:	8b 44 24 e4          	mov    -0x1c(%rsp),%eax
  401388:	69 c0 56 ad 00 00    	imul   $0xad56,%eax,%eax
  40138e:	89 44 24 e4          	mov    %eax,-0x1c(%rsp)
  401392:	8b 44 24 f0          	mov    -0x10(%rsp),%eax
  401396:	69 c0 de 82 00 00    	imul   $0x82de,%eax,%eax
  40139c:	89 44 24 f0          	mov    %eax,-0x10(%rsp)
  4013a0:	8b 44 24 f4          	mov    -0xc(%rsp),%eax
  4013a4:	69 c0 2f 5f 00 00    	imul   $0x5f2f,%eax,%eax
  4013aa:	89 44 24 f4          	mov    %eax,-0xc(%rsp)
  4013ae:	8b 44 24 f4          	mov    -0xc(%rsp),%eax
  4013b2:	69 c0 fa e2 00 00    	imul   $0xe2fa,%eax,%eax
  4013b8:	89 44 24 f4          	mov    %eax,-0xc(%rsp)
  4013bc:	8b 44 24 e4          	mov    -0x1c(%rsp),%eax
  4013c0:	69 c0 00 c3 00 00    	imul   $0xc300,%eax,%eax
  4013c6:	89 44 24 e4          	mov    %eax,-0x1c(%rsp)
  4013ca:	8b 44 24 e8          	mov    -0x18(%rsp),%eax
  4013ce:	69 c0 6e 85 00 00    	imul   $0x856e,%eax,%eax
  4013d4:	89 44 24 e8          	mov    %eax,-0x18(%rsp)
  4013d8:	8b 44 24 e0          	mov    -0x20(%rsp),%eax
  4013dc:	69 c0 03 82 00 00    	imul   $0x8203,%eax,%eax
  4013e2:	89 44 24 e0          	mov    %eax,-0x20(%rsp)
  4013e6:	8b 44 24 e8          	mov    -0x18(%rsp),%eax
  4013ea:	69 c0 b2 36 00 00    	imul   $0x36b2,%eax,%eax
  4013f0:	89 44 24 e8          	mov    %eax,-0x18(%rsp)
  4013f4:	8b 44 24 d8          	mov    -0x28(%rsp),%eax
  4013f8:	69 c0 02 c3 00 00    	imul   $0xc302,%eax,%eax
  4013fe:	89 44 24 d8          	mov    %eax,-0x28(%rsp)
  401402:	8b 44 24 ec          	mov    -0x14(%rsp),%eax
  401406:	69 c0 05 c7 00 00    	imul   $0xc705,%eax,%eax
  40140c:	89 44 24 ec          	mov    %eax,-0x14(%rsp)
  401410:	8b 44 24 ec          	mov    -0x14(%rsp),%eax
  401414:	69 c0 16 bf 00 00    	imul   $0xbf16,%eax,%eax
  40141a:	89 44 24 ec          	mov    %eax,-0x14(%rsp)
  40141e:	8b 44 24 e4          	mov    -0x1c(%rsp),%eax
  401422:	69 c0 f6 d4 00 00    	imul   $0xd4f6,%eax,%eax
  401428:	89 44 24 e4          	mov    %eax,-0x1c(%rsp)
  40142c:	8b 44 24 f4          	mov    -0xc(%rsp),%eax
  401430:	69 c0 aa e9 00 00    	imul   $0xe9aa,%eax,%eax
  401436:	89 44 24 f4          	mov    %eax,-0xc(%rsp)
  40143a:	8b 44 24 ec          	mov    -0x14(%rsp),%eax
  40143e:	69 c0 62 63 00 00    	imul   $0x6362,%eax,%eax
  401444:	89 44 24 ec          	mov    %eax,-0x14(%rsp)
  401448:	8b 44 24 f0          	mov    -0x10(%rsp),%eax
  40144c:	69 c0 4f 29 00 00    	imul   $0x294f,%eax,%eax
  401452:	89 44 24 f0          	mov    %eax,-0x10(%rsp)
  401456:	8b 44 24 ec          	mov    -0x14(%rsp),%eax
  40145a:	69 c0 42 73 00 00    	imul   $0x7342,%eax,%eax
  401460:	89 44 24 ec          	mov    %eax,-0x14(%rsp)
  401464:	8b 44 24 e8          	mov    -0x18(%rsp),%eax
  401468:	69 c0 35 d4 00 00    	imul   $0xd435,%eax,%eax
  40146e:	89 44 24 e8          	mov    %eax,-0x18(%rsp)
  401472:	8b 44 24 dc          	mov    -0x24(%rsp),%eax
  401476:	69 c0 7d 5d 00 00    	imul   $0x5d7d,%eax,%eax
  40147c:	89 44 24 dc          	mov    %eax,-0x24(%rsp)
  401480:	8b 44 24 d4          	mov    -0x2c(%rsp),%eax
  401484:	69 c0 d4 b5 00 00    	imul   $0xb5d4,%eax,%eax
  40148a:	89 44 24 d4          	mov    %eax,-0x2c(%rsp)
  40148e:	8b 44 24 d8          	mov    -0x28(%rsp),%eax
  401492:	69 c0 de e0 00 00    	imul   $0xe0de,%eax,%eax
  401498:	89 44 24 d8          	mov    %eax,-0x28(%rsp)
  40149c:	8b 44 24 e8          	mov    -0x18(%rsp),%eax
  4014a0:	69 c0 ce ed 00 00    	imul   $0xedce,%eax,%eax
  4014a6:	89 44 24 e8          	mov    %eax,-0x18(%rsp)
  4014aa:	8b 44 24 f0          	mov    -0x10(%rsp),%eax
  4014ae:	69 c0 e6 77 00 00    	imul   $0x77e6,%eax,%eax
  4014b4:	89 44 24 f0          	mov    %eax,-0x10(%rsp)
  4014b8:	8b 44 24 d0          	mov    -0x30(%rsp),%eax
  4014bc:	69 c0 ae 04 00 00    	imul   $0x4ae,%eax,%eax
  4014c2:	89 44 24 d0          	mov    %eax,-0x30(%rsp)
  4014c6:	8b 44 24 e8          	mov    -0x18(%rsp),%eax
  4014ca:	69 c0 20 51 00 00    	imul   $0x5120,%eax,%eax
  4014d0:	89 44 24 e8          	mov    %eax,-0x18(%rsp)
  4014d4:	8b 44 24 ec          	mov    -0x14(%rsp),%eax
  4014d8:	69 c0 d7 53 00 00    	imul   $0x53d7,%eax,%eax
  4014de:	89 44 24 ec          	mov    %eax,-0x14(%rsp)
  4014e2:	8b 44 24 dc          	mov    -0x24(%rsp),%eax
  4014e6:	69 c0 e9 48 00 00    	imul   $0x48e9,%eax,%eax
  4014ec:	89 44 24 dc          	mov    %eax,-0x24(%rsp)
  4014f0:	8b 44 24 d0          	mov    -0x30(%rsp),%eax
  4014f4:	69 c0 f5 87 00 00    	imul   $0x87f5,%eax,%eax
  4014fa:	89 44 24 d0          	mov    %eax,-0x30(%rsp)
  4014fe:	8b 44 24 e4          	mov    -0x1c(%rsp),%eax
  401502:	69 c0 4c 9b 00 00    	imul   $0x9b4c,%eax,%eax
  401508:	89 44 24 e4          	mov    %eax,-0x1c(%rsp)
  40150c:	8b 44 24 f0          	mov    -0x10(%rsp),%eax
  401510:	69 c0 43 6c 00 00    	imul   $0x6c43,%eax,%eax
  401516:	89 44 24 f0          	mov    %eax,-0x10(%rsp)
  40151a:	8b 44 24 e8          	mov    -0x18(%rsp),%eax
  40151e:	69 c0 96 e4 00 00    	imul   $0xe496,%eax,%eax
  401524:	89 44 24 e8          	mov    %eax,-0x18(%rsp)
  401528:	8b 44 24 dc          	mov    -0x24(%rsp),%eax
  40152c:	69 c0 40 bc 00 00    	imul   $0xbc40,%eax,%eax
  401532:	89 44 24 dc          	mov    %eax,-0x24(%rsp)
  401536:	8b 44 24 e0          	mov    -0x20(%rsp),%eax
  40153a:	69 c0 38 6c 00 00    	imul   $0x6c38,%eax,%eax
  401540:	89 44 24 e0          	mov    %eax,-0x20(%rsp)
  401544:	8b 44 24 dc          	mov    -0x24(%rsp),%eax
  401548:	69 c0 f9 4d 00 00    	imul   $0x4df9,%eax,%eax
  40154e:	89 44 24 dc          	mov    %eax,-0x24(%rsp)
  401552:	8b 44 24 d4          	mov    -0x2c(%rsp),%eax
  401556:	69 c0 db c3 00 00    	imul   $0xc3db,%eax,%eax
  40155c:	89 44 24 d4          	mov    %eax,-0x2c(%rsp)
  401560:	8b 44 24 f4          	mov    -0xc(%rsp),%eax
  401564:	69 c0 2d 0a 00 00    	imul   $0xa2d,%eax,%eax
  40156a:	89 44 24 f4          	mov    %eax,-0xc(%rsp)
  40156e:	8b 44 24 e4          	mov    -0x1c(%rsp),%eax
  401572:	69 c0 a2 7c 00 00    	imul   $0x7ca2,%eax,%eax
  401578:	89 44 24 e4          	mov    %eax,-0x1c(%rsp)
  40157c:	8b 44 24 e8          	mov    -0x18(%rsp),%eax
  401580:	69 c0 05 0b 00 00    	imul   $0xb05,%eax,%eax
  401586:	89 44 24 e8          	mov    %eax,-0x18(%rsp)
  40158a:	8b 44 24 ec          	mov    -0x14(%rsp),%eax
  40158e:	69 c0 cd 67 00 00    	imul   $0x67cd,%eax,%eax
  401594:	89 44 24 ec          	mov    %eax,-0x14(%rsp)
  401598:	8b 44 24 f0          	mov    -0x10(%rsp),%eax
  40159c:	69 c0 21 a3 00 00    	imul   $0xa321,%eax,%eax
  4015a2:	89 44 24 f0          	mov    %eax,-0x10(%rsp)
  4015a6:	8b 44 24 e0          	mov    -0x20(%rsp),%eax
  4015aa:	69 c0 64 d7 00 00    	imul   $0xd764,%eax,%eax
  4015b0:	89 44 24 e0          	mov    %eax,-0x20(%rsp)
  4015b4:	8b 44 24 f4          	mov    -0xc(%rsp),%eax
  4015b8:	69 c0 13 d1 00 00    	imul   $0xd113,%eax,%eax
  4015be:	89 44 24 f4          	mov    %eax,-0xc(%rsp)
  4015c2:	8b 44 24 e8          	mov    -0x18(%rsp),%eax
  4015c6:	69 c0 e5 09 00 00    	imul   $0x9e5,%eax,%eax
  4015cc:	89 44 24 e8          	mov    %eax,-0x18(%rsp)
  4015d0:	8b 44 24 f4          	mov    -0xc(%rsp),%eax
  4015d4:	69 c0 2d 3e 00 00    	imul   $0x3e2d,%eax,%eax
  4015da:	89 44 24 f4          	mov    %eax,-0xc(%rsp)
  4015de:	8b 44 24 e4          	mov    -0x1c(%rsp),%eax
  4015e2:	69 c0 a7 ca 00 00    	imul   $0xcaa7,%eax,%eax
  4015e8:	89 44 24 e4          	mov    %eax,-0x1c(%rsp)
  4015ec:	8b 44 24 e8          	mov    -0x18(%rsp),%eax
  4015f0:	69 c0 df a3 00 00    	imul   $0xa3df,%eax,%eax
  4015f6:	89 44 24 e8          	mov    %eax,-0x18(%rsp)
  4015fa:	8b 44 24 e8          	mov    -0x18(%rsp),%eax
  4015fe:	69 c0 03 40 00 00    	imul   $0x4003,%eax,%eax
  401604:	89 44 24 e8          	mov    %eax,-0x18(%rsp)
  401608:	8b 44 24 f4          	mov    -0xc(%rsp),%eax
  40160c:	69 c0 17 07 00 00    	imul   $0x717,%eax,%eax
  401612:	89 44 24 f4          	mov    %eax,-0xc(%rsp)
  401616:	8b 44 24 d8          	mov    -0x28(%rsp),%eax
  40161a:	69 c0 96 f0 00 00    	imul   $0xf096,%eax,%eax
  401620:	89 44 24 d8          	mov    %eax,-0x28(%rsp)
  401624:	8b 44 24 ec          	mov    -0x14(%rsp),%eax
  401628:	69 c0 f1 75 00 00    	imul   $0x75f1,%eax,%eax
  40162e:	89 44 24 ec          	mov    %eax,-0x14(%rsp)
  401632:	8b 44 24 d0          	mov    -0x30(%rsp),%eax
  401636:	69 c0 de 58 00 00    	imul   $0x58de,%eax,%eax
  40163c:	89 44 24 d0          	mov    %eax,-0x30(%rsp)
  401640:	8b 44 24 d8          	mov    -0x28(%rsp),%eax
  401644:	69 c0 2a 59 00 00    	imul   $0x592a,%eax,%eax
  40164a:	89 44 24 d8          	mov    %eax,-0x28(%rsp)
  40164e:	8b 44 24 e4          	mov    -0x1c(%rsp),%eax
  401652:	69 c0 19 78 00 00    	imul   $0x7819,%eax,%eax
  401658:	89 44 24 e4          	mov    %eax,-0x1c(%rsp)
  40165c:	8b 44 24 f4          	mov    -0xc(%rsp),%eax
  401660:	69 c0 08 ca 00 00    	imul   $0xca08,%eax,%eax
  401666:	89 44 24 f4          	mov    %eax,-0xc(%rsp)
  40166a:	8b 44 24 f4          	mov    -0xc(%rsp),%eax
  40166e:	69 c0 e2 73 00 00    	imul   $0x73e2,%eax,%eax
  401674:	89 44 24 f4          	mov    %eax,-0xc(%rsp)
  401678:	8b 44 24 d0          	mov    -0x30(%rsp),%eax
  40167c:	69 c0 d5 98 00 00    	imul   $0x98d5,%eax,%eax
  401682:	89 44 24 d0          	mov    %eax,-0x30(%rsp)
  401686:	8b 44 24 f0          	mov    -0x10(%rsp),%eax
  40168a:	69 c0 21 64 00 00    	imul   $0x6421,%eax,%eax
  401690:	89 44 24 f0          	mov    %eax,-0x10(%rsp)
  401694:	8b 44 24 e0          	mov    -0x20(%rsp),%eax
  401698:	69 c0 94 6b 00 00    	imul   $0x6b94,%eax,%eax
  40169e:	89 44 24 e0          	mov    %eax,-0x20(%rsp)
  4016a2:	8b 44 24 e4          	mov    -0x1c(%rsp),%eax
  4016a6:	69 c0 f1 c2 00 00    	imul   $0xc2f1,%eax,%eax
  4016ac:	89 44 24 e4          	mov    %eax,-0x1c(%rsp)
  4016b0:	8b 44 24 e8          	mov    -0x18(%rsp),%eax
  4016b4:	69 c0 37 13 00 00    	imul   $0x1337,%eax,%eax
  4016ba:	89 44 24 e8          	mov    %eax,-0x18(%rsp)
  4016be:	8b 44 24 d4          	mov    -0x2c(%rsp),%eax
  4016c2:	69 c0 32 54 00 00    	imul   $0x5432,%eax,%eax
  4016c8:	89 44 24 d4          	mov    %eax,-0x2c(%rsp)
  4016cc:	8b 44 24 e0          	mov    -0x20(%rsp),%eax
  4016d0:	69 c0 d6 3c 00 00    	imul   $0x3cd6,%eax,%eax
  4016d6:	89 44 24 e0          	mov    %eax,-0x20(%rsp)
  4016da:	8b 44 24 e4          	mov    -0x1c(%rsp),%eax
  4016de:	69 c0 37 17 00 00    	imul   $0x1737,%eax,%eax
  4016e4:	89 44 24 e4          	mov    %eax,-0x1c(%rsp)
  4016e8:	8b 44 24 f4          	mov    -0xc(%rsp),%eax
  4016ec:	69 c0 71 40 00 00    	imul   $0x4071,%eax,%eax
  4016f2:	89 44 24 f4          	mov    %eax,-0xc(%rsp)
  4016f6:	8b 44 24 f0          	mov    -0x10(%rsp),%eax
  4016fa:	69 c0 fe 98 00 00    	imul   $0x98fe,%eax,%eax
  401700:	89 44 24 f0          	mov    %eax,-0x10(%rsp)
  401704:	8b 44 24 e0          	mov    -0x20(%rsp),%eax
  401708:	69 c0 3e bc 00 00    	imul   $0xbc3e,%eax,%eax
  40170e:	89 44 24 e0          	mov    %eax,-0x20(%rsp)
  401712:	8b 44 24 e8          	mov    -0x18(%rsp),%eax
  401716:	69 c0 80 36 00 00    	imul   $0x3680,%eax,%eax
  40171c:	89 44 24 e8          	mov    %eax,-0x18(%rsp)
  401720:	8b 44 24 e8          	mov    -0x18(%rsp),%eax
  401724:	69 c0 5c c7 00 00    	imul   $0xc75c,%eax,%eax
  40172a:	89 44 24 e8          	mov    %eax,-0x18(%rsp)
  40172e:	8b 44 24 e8          	mov    -0x18(%rsp),%eax
  401732:	69 c0 97 79 00 00    	imul   $0x7997,%eax,%eax
  401738:	89 44 24 e8          	mov    %eax,-0x18(%rsp)
  40173c:	8b 44 24 f4          	mov    -0xc(%rsp),%eax
  401740:	69 c0 81 8e 00 00    	imul   $0x8e81,%eax,%eax
  401746:	89 44 24 f4          	mov    %eax,-0xc(%rsp)
  40174a:	8b 44 24 f0          	mov    -0x10(%rsp),%eax
  40174e:	69 c0 8e fe 00 00    	imul   $0xfe8e,%eax,%eax
  401754:	89 44 24 f0          	mov    %eax,-0x10(%rsp)
  401758:	8b 44 24 dc          	mov    -0x24(%rsp),%eax
  40175c:	69 c0 c0 38 00 00    	imul   $0x38c0,%eax,%eax
  401762:	89 44 24 dc          	mov    %eax,-0x24(%rsp)
  401766:	8b 44 24 e0          	mov    -0x20(%rsp),%eax
  40176a:	69 c0 3e 91 00 00    	imul   $0x913e,%eax,%eax
  401770:	89 44 24 e0          	mov    %eax,-0x20(%rsp)
  401774:	8b 44 24 d4          	mov    -0x2c(%rsp),%eax
  401778:	69 c0 b6 d0 00 00    	imul   $0xd0b6,%eax,%eax
  40177e:	89 44 24 d4          	mov    %eax,-0x2c(%rsp)
  401782:	8b 44 24 dc          	mov    -0x24(%rsp),%eax
  401786:	69 c0 15 4e 00 00    	imul   $0x4e15,%eax,%eax
  40178c:	89 44 24 dc          	mov    %eax,-0x24(%rsp)
  401790:	8b 44 24 e0          	mov    -0x20(%rsp),%eax
  401794:	69 c0 c8 e1 00 00    	imul   $0xe1c8,%eax,%eax
  40179a:	89 44 24 e0          	mov    %eax,-0x20(%rsp)
  40179e:	8b 44 24 e0          	mov    -0x20(%rsp),%eax
  4017a2:	69 c0 04 9b 00 00    	imul   $0x9b04,%eax,%eax
  4017a8:	89 44 24 e0          	mov    %eax,-0x20(%rsp)
  4017ac:	8b 44 24 ec          	mov    -0x14(%rsp),%eax
  4017b0:	69 c0 7e 36 00 00    	imul   $0x367e,%eax,%eax
  4017b6:	89 44 24 ec          	mov    %eax,-0x14(%rsp)
  4017ba:	8b 44 24 e4          	mov    -0x1c(%rsp),%eax
  4017be:	69 c0 8c 62 00 00    	imul   $0x628c,%eax,%eax
  4017c4:	89 44 24 e4          	mov    %eax,-0x1c(%rsp)
  4017c8:	8b 44 24 d0          	mov    -0x30(%rsp),%eax
  4017cc:	69 c0 08 ba 00 00    	imul   $0xba08,%eax,%eax
  4017d2:	89 44 24 d0          	mov    %eax,-0x30(%rsp)
  4017d6:	8b 44 24 e8          	mov    -0x18(%rsp),%eax
  4017da:	69 c0 52 2d 00 00    	imul   $0x2d52,%eax,%eax
  4017e0:	89 44 24 e8          	mov    %eax,-0x18(%rsp)
  4017e4:	8b 44 24 e8          	mov    -0x18(%rsp),%eax
  4017e8:	69 c0 66 ed 00 00    	imul   $0xed66,%eax,%eax
  4017ee:	89 44 24 e8          	mov    %eax,-0x18(%rsp)
  4017f2:	8b 44 24 e0          	mov    -0x20(%rsp),%eax
  4017f6:	69 c0 ae 8b 00 00    	imul   $0x8bae,%eax,%eax
  4017fc:	89 44 24 e0          	mov    %eax,-0x20(%rsp)
  401800:	8b 44 24 ec          	mov    -0x14(%rsp),%eax
  401804:	69 c0 6c 12 00 00    	imul   $0x126c,%eax,%eax
  40180a:	89 44 24 ec          	mov    %eax,-0x14(%rsp)
  40180e:	ba 00 00 00 00       	mov    $0x0,%edx
  401813:	b8 00 00 00 00       	mov    $0x0,%eax
  401818:	eb 0b                	jmp    401825 <scramble+0x582>
  40181a:	89 d1                	mov    %edx,%ecx
  40181c:	8b 4c 8c d0          	mov    -0x30(%rsp,%rcx,4),%ecx
  401820:	01 c8                	add    %ecx,%eax
  401822:	83 c2 01             	add    $0x1,%edx
  401825:	83 fa 09             	cmp    $0x9,%edx
  401828:	76 f0                	jbe    40181a <scramble+0x577>
  40182a:	f3 c3                	repz retq 

000000000040182c <getbuf>:
  40182c:	48 83 ec 28          	sub    $0x28,%rsp
  401830:	48 89 e7             	mov    %rsp,%rdi
  401833:	e8 52 03 00 00       	callq  401b8a <Gets>
  401838:	b8 01 00 00 00       	mov    $0x1,%eax
  40183d:	48 83 c4 28          	add    $0x28,%rsp
  401841:	c3                   	retq   

0000000000401842 <touch1>:
  401842:	48 83 ec 08          	sub    $0x8,%rsp
  401846:	c7 05 ac 3c 20 00 01 	movl   $0x1,0x203cac(%rip)        # 6054fc <vlevel>
  40184d:	00 00 00 
  401850:	bf b3 31 40 00       	mov    $0x4031b3,%edi
  401855:	e8 f6 f3 ff ff       	callq  400c50 <puts@plt>
  40185a:	bf 01 00 00 00       	mov    $0x1,%edi
  40185f:	e8 15 05 00 00       	callq  401d79 <validate>
  401864:	bf 00 00 00 00       	mov    $0x0,%edi
  401869:	e8 82 f5 ff ff       	callq  400df0 <exit@plt>

000000000040186e <touch2>:
  40186e:	48 83 ec 08          	sub    $0x8,%rsp
  401872:	89 fe                	mov    %edi,%esi
  401874:	c7 05 7e 3c 20 00 02 	movl   $0x2,0x203c7e(%rip)        # 6054fc <vlevel>
  40187b:	00 00 00 
  40187e:	3b 3d 80 3c 20 00    	cmp    0x203c80(%rip),%edi        # 605504 <cookie>
  401884:	75 1b                	jne    4018a1 <touch2+0x33>
  401886:	bf d8 31 40 00       	mov    $0x4031d8,%edi
  40188b:	b8 00 00 00 00       	mov    $0x0,%eax
  401890:	e8 eb f3 ff ff       	callq  400c80 <printf@plt>
  401895:	bf 02 00 00 00       	mov    $0x2,%edi
  40189a:	e8 da 04 00 00       	callq  401d79 <validate>
  40189f:	eb 19                	jmp    4018ba <touch2+0x4c>
  4018a1:	bf 00 32 40 00       	mov    $0x403200,%edi
  4018a6:	b8 00 00 00 00       	mov    $0x0,%eax
  4018ab:	e8 d0 f3 ff ff       	callq  400c80 <printf@plt>
  4018b0:	bf 02 00 00 00       	mov    $0x2,%edi
  4018b5:	e8 71 05 00 00       	callq  401e2b <fail>
  4018ba:	bf 00 00 00 00       	mov    $0x0,%edi
  4018bf:	e8 2c f5 ff ff       	callq  400df0 <exit@plt>

00000000004018c4 <hexmatch>:
  4018c4:	41 54                	push   %r12
  4018c6:	55                   	push   %rbp
  4018c7:	53                   	push   %rbx
  4018c8:	48 83 ec 70          	sub    $0x70,%rsp
  4018cc:	41 89 fc             	mov    %edi,%r12d
  4018cf:	48 89 f5             	mov    %rsi,%rbp
  4018d2:	e8 79 f4 ff ff       	callq  400d50 <random@plt>
  4018d7:	48 89 c1             	mov    %rax,%rcx
  4018da:	48 ba 0b d7 a3 70 3d 	movabs $0xa3d70a3d70a3d70b,%rdx
  4018e1:	0a d7 a3 
  4018e4:	48 f7 ea             	imul   %rdx
  4018e7:	48 8d 04 0a          	lea    (%rdx,%rcx,1),%rax
  4018eb:	48 c1 f8 06          	sar    $0x6,%rax
  4018ef:	48 89 ce             	mov    %rcx,%rsi
  4018f2:	48 c1 fe 3f          	sar    $0x3f,%rsi
  4018f6:	48 29 f0             	sub    %rsi,%rax
  4018f9:	48 8d 04 80          	lea    (%rax,%rax,4),%rax
  4018fd:	48 8d 04 80          	lea    (%rax,%rax,4),%rax
  401901:	48 c1 e0 02          	shl    $0x2,%rax
  401905:	48 29 c1             	sub    %rax,%rcx
  401908:	48 8d 1c 0c          	lea    (%rsp,%rcx,1),%rbx
  40190c:	44 89 e2             	mov    %r12d,%edx
  40190f:	be d0 31 40 00       	mov    $0x4031d0,%esi
  401914:	48 89 df             	mov    %rbx,%rdi
  401917:	b8 00 00 00 00       	mov    $0x0,%eax
  40191c:	e8 bf f4 ff ff       	callq  400de0 <sprintf@plt>
  401921:	ba 09 00 00 00       	mov    $0x9,%edx
  401926:	48 89 de             	mov    %rbx,%rsi
  401929:	48 89 ef             	mov    %rbp,%rdi
  40192c:	e8 ff f2 ff ff       	callq  400c30 <strncmp@plt>
  401931:	85 c0                	test   %eax,%eax
  401933:	0f 94 c0             	sete   %al
  401936:	0f b6 c0             	movzbl %al,%eax
  401939:	48 83 c4 70          	add    $0x70,%rsp
  40193d:	5b                   	pop    %rbx
  40193e:	5d                   	pop    %rbp
  40193f:	41 5c                	pop    %r12
  401941:	c3                   	retq   

0000000000401942 <touch3>:
  401942:	53                   	push   %rbx
  401943:	48 89 fb             	mov    %rdi,%rbx
  401946:	c7 05 ac 3b 20 00 03 	movl   $0x3,0x203bac(%rip)        # 6054fc <vlevel>
  40194d:	00 00 00 
  401950:	48 89 fe             	mov    %rdi,%rsi
  401953:	8b 3d ab 3b 20 00    	mov    0x203bab(%rip),%edi        # 605504 <cookie>
  401959:	e8 66 ff ff ff       	callq  4018c4 <hexmatch>
  40195e:	85 c0                	test   %eax,%eax
  401960:	74 1e                	je     401980 <touch3+0x3e>
  401962:	48 89 de             	mov    %rbx,%rsi
  401965:	bf 28 32 40 00       	mov    $0x403228,%edi
  40196a:	b8 00 00 00 00       	mov    $0x0,%eax
  40196f:	e8 0c f3 ff ff       	callq  400c80 <printf@plt>
  401974:	bf 03 00 00 00       	mov    $0x3,%edi
  401979:	e8 fb 03 00 00       	callq  401d79 <validate>
  40197e:	eb 1c                	jmp    40199c <touch3+0x5a>
  401980:	48 89 de             	mov    %rbx,%rsi
  401983:	bf 50 32 40 00       	mov    $0x403250,%edi
  401988:	b8 00 00 00 00       	mov    $0x0,%eax
  40198d:	e8 ee f2 ff ff       	callq  400c80 <printf@plt>
  401992:	bf 03 00 00 00       	mov    $0x3,%edi
  401997:	e8 8f 04 00 00       	callq  401e2b <fail>
  40199c:	bf 00 00 00 00       	mov    $0x0,%edi
  4019a1:	e8 4a f4 ff ff       	callq  400df0 <exit@plt>

00000000004019a6 <test>:
  4019a6:	48 83 ec 08          	sub    $0x8,%rsp
  4019aa:	b8 00 00 00 00       	mov    $0x0,%eax
  4019af:	e8 78 fe ff ff       	callq  40182c <getbuf>
  4019b4:	89 c6                	mov    %eax,%esi
  4019b6:	bf 78 32 40 00       	mov    $0x403278,%edi
  4019bb:	b8 00 00 00 00       	mov    $0x0,%eax
  4019c0:	e8 bb f2 ff ff       	callq  400c80 <printf@plt>
  4019c5:	48 83 c4 08          	add    $0x8,%rsp
  4019c9:	c3                   	retq   

00000000004019ca <start_farm>:
  4019ca:	b8 01 00 00 00       	mov    $0x1,%eax
  4019cf:	c3                   	retq   

00000000004019d0 <getval_130>:
  4019d0:	b8 48 89 c7 c1       	mov    $0xc1c78948,%eax
  4019d5:	c3                   	retq   

00000000004019d6 <setval_286>:
  4019d6:	c7 07 34 48 89 c7    	movl   $0xc7894834,(%rdi)
  4019dc:	c3                   	retq   

00000000004019dd <getval_481>:
  4019dd:	b8 48 89 c7 c3       	mov    $0xc3c78948,%eax
  4019e2:	c3                   	retq   

00000000004019e3 <addval_460>:
  4019e3:	8d 87 c5 58 c3 50    	lea    0x50c358c5(%rdi),%eax
  4019e9:	c3                   	retq   

00000000004019ea <setval_293>:
  4019ea:	c7 07 78 90 90 90    	movl   $0x90909078,(%rdi)
  4019f0:	c3                   	retq   

00000000004019f1 <setval_232>:
  4019f1:	c7 07 58 90 94 c3    	movl   $0xc3949058,(%rdi)
  4019f7:	c3                   	retq   

00000000004019f8 <getval_487>:
  4019f8:	b8 48 89 c7 c7       	mov    $0xc7c78948,%eax
  4019fd:	c3                   	retq   

00000000004019fe <setval_229>:
  4019fe:	c7 07 8c 22 f1 58    	movl   $0x58f1228c,(%rdi)
  401a04:	c3                   	retq   

0000000000401a05 <mid_farm>:
  401a05:	b8 01 00 00 00       	mov    $0x1,%eax
  401a0a:	c3                   	retq   

0000000000401a0b <add_xy>:
  401a0b:	48 8d 04 37          	lea    (%rdi,%rsi,1),%rax
  401a0f:	c3                   	retq   

0000000000401a10 <getval_367>:
  401a10:	b8 8b d1 90 c3       	mov    $0xc390d18b,%eax
  401a15:	c3                   	retq   

0000000000401a16 <addval_497>:
  401a16:	8d 87 89 ce 90 c1    	lea    -0x3e6f3177(%rdi),%eax
  401a1c:	c3                   	retq   

0000000000401a1d <addval_390>:
  401a1d:	8d 87 8d d1 c3 55    	lea    0x55c3d18d(%rdi),%eax
  401a23:	c3                   	retq   

0000000000401a24 <getval_323>:
  401a24:	b8 70 49 89 e0       	mov    $0xe0894970,%eax
  401a29:	c3                   	retq   

0000000000401a2a <getval_320>:
  401a2a:	b8 89 ce 48 db       	mov    $0xdb48ce89,%eax
  401a2f:	c3                   	retq   

0000000000401a30 <setval_470>:
  401a30:	c7 07 89 c2 c3 7e    	movl   $0x7ec3c289,(%rdi)
  401a36:	c3                   	retq   

0000000000401a37 <addval_247>:
  401a37:	8d 87 89 ce 08 c9    	lea    -0x36f73177(%rdi),%eax
  401a3d:	c3                   	retq   

0000000000401a3e <addval_387>:
  401a3e:	8d 87 89 c2 94 90    	lea    -0x6f6b3d77(%rdi),%eax
  401a44:	c3                   	retq   

0000000000401a45 <addval_159>:
  401a45:	8d 87 89 c2 a4 c0    	lea    -0x3f5b3d77(%rdi),%eax
  401a4b:	c3                   	retq   

0000000000401a4c <getval_101>:
  401a4c:	b8 c8 89 e0 c3       	mov    $0xc3e089c8,%eax
  401a51:	c3                   	retq   

0000000000401a52 <setval_306>:
  401a52:	c7 07 27 88 d1 c3    	movl   $0xc3d18827,(%rdi)
  401a58:	c3                   	retq   

0000000000401a59 <getval_127>:
  401a59:	b8 8d c2 08 d2       	mov    $0xd208c28d,%eax
  401a5e:	c3                   	retq   

0000000000401a5f <addval_278>:
  401a5f:	8d 87 88 ce 20 d2    	lea    -0x2ddf3178(%rdi),%eax
  401a65:	c3                   	retq   

0000000000401a66 <addval_144>:
  401a66:	8d 87 89 ce 30 d2    	lea    -0x2dcf3177(%rdi),%eax
  401a6c:	c3                   	retq   

0000000000401a6d <setval_275>:
  401a6d:	c7 07 fe 89 d1 92    	movl   $0x92d189fe,(%rdi)
  401a73:	c3                   	retq   

0000000000401a74 <addval_176>:
  401a74:	8d 87 89 d1 90 90    	lea    -0x6f6f2e77(%rdi),%eax
  401a7a:	c3                   	retq   

0000000000401a7b <addval_482>:
  401a7b:	8d 87 08 89 e0 90    	lea    -0x6f1f76f8(%rdi),%eax
  401a81:	c3                   	retq   

0000000000401a82 <getval_222>:
  401a82:	b8 c9 c2 38 d2       	mov    $0xd238c2c9,%eax
  401a87:	c3                   	retq   

0000000000401a88 <addval_282>:
  401a88:	8d 87 89 d1 60 db    	lea    -0x249f2e77(%rdi),%eax
  401a8e:	c3                   	retq   

0000000000401a8f <getval_299>:
  401a8f:	b8 d2 89 c2 90       	mov    $0x90c289d2,%eax
  401a94:	c3                   	retq   

0000000000401a95 <addval_451>:
  401a95:	8d 87 89 ce c3 58    	lea    0x58c3ce89(%rdi),%eax
  401a9b:	c3                   	retq   

0000000000401a9c <setval_429>:
  401a9c:	c7 07 89 d1 90 c3    	movl   $0xc390d189,(%rdi)
  401aa2:	c3                   	retq   

0000000000401aa3 <addval_136>:
  401aa3:	8d 87 c9 ce 38 db    	lea    -0x24c73137(%rdi),%eax
  401aa9:	c3                   	retq   

0000000000401aaa <getval_350>:
  401aaa:	b8 48 89 e0 c7       	mov    $0xc7e08948,%eax
  401aaf:	c3                   	retq   

0000000000401ab0 <setval_364>:
  401ab0:	c7 07 48 89 e0 91    	movl   $0x91e08948,(%rdi)
  401ab6:	c3                   	retq   

0000000000401ab7 <getval_283>:
  401ab7:	b8 55 99 d1 90       	mov    $0x90d19955,%eax
  401abc:	c3                   	retq   

0000000000401abd <setval_147>:
  401abd:	c7 07 99 c2 84 c9    	movl   $0xc984c299,(%rdi)
  401ac3:	c3                   	retq   

0000000000401ac4 <setval_430>:
  401ac4:	c7 07 48 89 e0 92    	movl   $0x92e08948,(%rdi)
  401aca:	c3                   	retq   

0000000000401acb <setval_331>:
  401acb:	c7 07 48 89 e0 c3    	movl   $0xc3e08948,(%rdi)
  401ad1:	c3                   	retq   

0000000000401ad2 <getval_449>:
  401ad2:	b8 1f 89 ce 94       	mov    $0x94ce891f,%eax
  401ad7:	c3                   	retq   

0000000000401ad8 <addval_241>:
  401ad8:	8d 87 48 89 e0 c3    	lea    -0x3c1f76b8(%rdi),%eax
  401ade:	c3                   	retq   

0000000000401adf <setval_279>:
  401adf:	c7 07 89 c2 28 c0    	movl   $0xc028c289,(%rdi)
  401ae5:	c3                   	retq   

0000000000401ae6 <end_farm>:
  401ae6:	b8 01 00 00 00       	mov    $0x1,%eax
  401aeb:	c3                   	retq   
  401aec:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000401af0 <save_char>:
  401af0:	8b 05 2e 46 20 00    	mov    0x20462e(%rip),%eax        # 606124 <gets_cnt>
  401af6:	3d ff 03 00 00       	cmp    $0x3ff,%eax
  401afb:	7f 49                	jg     401b46 <save_char+0x56>
  401afd:	8d 14 40             	lea    (%rax,%rax,2),%edx
  401b00:	89 f9                	mov    %edi,%ecx
  401b02:	c0 e9 04             	shr    $0x4,%cl
  401b05:	83 e1 0f             	and    $0xf,%ecx
  401b08:	0f b6 b1 a0 35 40 00 	movzbl 0x4035a0(%rcx),%esi
  401b0f:	48 63 ca             	movslq %edx,%rcx
  401b12:	40 88 b1 20 55 60 00 	mov    %sil,0x605520(%rcx)
  401b19:	8d 4a 01             	lea    0x1(%rdx),%ecx
  401b1c:	83 e7 0f             	and    $0xf,%edi
  401b1f:	0f b6 b7 a0 35 40 00 	movzbl 0x4035a0(%rdi),%esi
  401b26:	48 63 c9             	movslq %ecx,%rcx
  401b29:	40 88 b1 20 55 60 00 	mov    %sil,0x605520(%rcx)
  401b30:	83 c2 02             	add    $0x2,%edx
  401b33:	48 63 d2             	movslq %edx,%rdx
  401b36:	c6 82 20 55 60 00 20 	movb   $0x20,0x605520(%rdx)
  401b3d:	83 c0 01             	add    $0x1,%eax
  401b40:	89 05 de 45 20 00    	mov    %eax,0x2045de(%rip)        # 606124 <gets_cnt>
  401b46:	f3 c3                	repz retq 

0000000000401b48 <save_term>:
  401b48:	8b 05 d6 45 20 00    	mov    0x2045d6(%rip),%eax        # 606124 <gets_cnt>
  401b4e:	8d 04 40             	lea    (%rax,%rax,2),%eax
  401b51:	48 98                	cltq   
  401b53:	c6 80 20 55 60 00 00 	movb   $0x0,0x605520(%rax)
  401b5a:	c3                   	retq   

0000000000401b5b <check_fail>:
  401b5b:	48 83 ec 08          	sub    $0x8,%rsp
  401b5f:	0f be 35 c2 45 20 00 	movsbl 0x2045c2(%rip),%esi        # 606128 <target_prefix>
  401b66:	b9 20 55 60 00       	mov    $0x605520,%ecx
  401b6b:	8b 15 87 39 20 00    	mov    0x203987(%rip),%edx        # 6054f8 <check_level>
  401b71:	bf 9b 32 40 00       	mov    $0x40329b,%edi
  401b76:	b8 00 00 00 00       	mov    $0x0,%eax
  401b7b:	e8 00 f1 ff ff       	callq  400c80 <printf@plt>
  401b80:	bf 01 00 00 00       	mov    $0x1,%edi
  401b85:	e8 66 f2 ff ff       	callq  400df0 <exit@plt>

0000000000401b8a <Gets>:
  401b8a:	41 54                	push   %r12
  401b8c:	55                   	push   %rbp
  401b8d:	53                   	push   %rbx
  401b8e:	49 89 fc             	mov    %rdi,%r12
  401b91:	c7 05 89 45 20 00 00 	movl   $0x0,0x204589(%rip)        # 606124 <gets_cnt>
  401b98:	00 00 00 
  401b9b:	48 89 fb             	mov    %rdi,%rbx
  401b9e:	eb 11                	jmp    401bb1 <Gets+0x27>
  401ba0:	48 8d 6b 01          	lea    0x1(%rbx),%rbp
  401ba4:	88 03                	mov    %al,(%rbx)
  401ba6:	0f b6 f8             	movzbl %al,%edi
  401ba9:	e8 42 ff ff ff       	callq  401af0 <save_char>
  401bae:	48 89 eb             	mov    %rbp,%rbx
  401bb1:	48 8b 3d 38 39 20 00 	mov    0x203938(%rip),%rdi        # 6054f0 <infile>
  401bb8:	e8 a3 f1 ff ff       	callq  400d60 <_IO_getc@plt>
  401bbd:	83 f8 ff             	cmp    $0xffffffff,%eax
  401bc0:	74 05                	je     401bc7 <Gets+0x3d>
  401bc2:	83 f8 0a             	cmp    $0xa,%eax
  401bc5:	75 d9                	jne    401ba0 <Gets+0x16>
  401bc7:	c6 03 00             	movb   $0x0,(%rbx)
  401bca:	b8 00 00 00 00       	mov    $0x0,%eax
  401bcf:	e8 74 ff ff ff       	callq  401b48 <save_term>
  401bd4:	4c 89 e0             	mov    %r12,%rax
  401bd7:	5b                   	pop    %rbx
  401bd8:	5d                   	pop    %rbp
  401bd9:	41 5c                	pop    %r12
  401bdb:	c3                   	retq   

0000000000401bdc <notify_server>:
  401bdc:	83 3d 25 39 20 00 00 	cmpl   $0x0,0x203925(%rip)        # 605508 <is_checker>
  401be3:	0f 85 8e 01 00 00    	jne    401d77 <notify_server+0x19b>
  401be9:	53                   	push   %rbx
  401bea:	48 81 ec 10 40 00 00 	sub    $0x4010,%rsp
  401bf1:	89 fb                	mov    %edi,%ebx
  401bf3:	8b 05 2b 45 20 00    	mov    0x20452b(%rip),%eax        # 606124 <gets_cnt>
  401bf9:	83 c0 64             	add    $0x64,%eax
  401bfc:	3d 00 20 00 00       	cmp    $0x2000,%eax
  401c01:	7e 19                	jle    401c1c <notify_server+0x40>
  401c03:	bf d0 33 40 00       	mov    $0x4033d0,%edi
  401c08:	b8 00 00 00 00       	mov    $0x0,%eax
  401c0d:	e8 6e f0 ff ff       	callq  400c80 <printf@plt>
  401c12:	bf 01 00 00 00       	mov    $0x1,%edi
  401c17:	e8 d4 f1 ff ff       	callq  400df0 <exit@plt>
  401c1c:	44 0f be 0d 04 45 20 	movsbl 0x204504(%rip),%r9d        # 606128 <target_prefix>
  401c23:	00 
  401c24:	83 3d 7d 38 20 00 00 	cmpl   $0x0,0x20387d(%rip)        # 6054a8 <notify>
  401c2b:	74 09                	je     401c36 <notify_server+0x5a>
  401c2d:	44 8b 05 cc 38 20 00 	mov    0x2038cc(%rip),%r8d        # 605500 <authkey>
  401c34:	eb 06                	jmp    401c3c <notify_server+0x60>
  401c36:	41 b8 ff ff ff ff    	mov    $0xffffffff,%r8d
  401c3c:	85 db                	test   %ebx,%ebx
  401c3e:	74 07                	je     401c47 <notify_server+0x6b>
  401c40:	b9 b1 32 40 00       	mov    $0x4032b1,%ecx
  401c45:	eb 05                	jmp    401c4c <notify_server+0x70>
  401c47:	b9 b6 32 40 00       	mov    $0x4032b6,%ecx
  401c4c:	48 c7 44 24 08 20 55 	movq   $0x605520,0x8(%rsp)
  401c53:	60 00 
  401c55:	89 34 24             	mov    %esi,(%rsp)
  401c58:	8b 15 0a 35 20 00    	mov    0x20350a(%rip),%edx        # 605168 <target_id>
  401c5e:	be bb 32 40 00       	mov    $0x4032bb,%esi
  401c63:	48 8d bc 24 10 20 00 	lea    0x2010(%rsp),%rdi
  401c6a:	00 
  401c6b:	b8 00 00 00 00       	mov    $0x0,%eax
  401c70:	e8 6b f1 ff ff       	callq  400de0 <sprintf@plt>
  401c75:	83 3d 2c 38 20 00 00 	cmpl   $0x0,0x20382c(%rip)        # 6054a8 <notify>
  401c7c:	74 78                	je     401cf6 <notify_server+0x11a>
  401c7e:	85 db                	test   %ebx,%ebx
  401c80:	74 68                	je     401cea <notify_server+0x10e>
  401c82:	4c 8d 4c 24 10       	lea    0x10(%rsp),%r9
  401c87:	41 b8 00 00 00 00    	mov    $0x0,%r8d
  401c8d:	48 8d 8c 24 10 20 00 	lea    0x2010(%rsp),%rcx
  401c94:	00 
  401c95:	48 8b 15 d4 34 20 00 	mov    0x2034d4(%rip),%rdx        # 605170 <lab>
  401c9c:	48 8b 35 d5 34 20 00 	mov    0x2034d5(%rip),%rsi        # 605178 <course>
  401ca3:	48 8b 3d b6 34 20 00 	mov    0x2034b6(%rip),%rdi        # 605160 <user_id>
  401caa:	e8 94 0f 00 00       	callq  402c43 <driver_post>
  401caf:	85 c0                	test   %eax,%eax
  401cb1:	79 1e                	jns    401cd1 <notify_server+0xf5>
  401cb3:	48 8d 74 24 10       	lea    0x10(%rsp),%rsi
  401cb8:	bf d7 32 40 00       	mov    $0x4032d7,%edi
  401cbd:	b8 00 00 00 00       	mov    $0x0,%eax
  401cc2:	e8 b9 ef ff ff       	callq  400c80 <printf@plt>
  401cc7:	bf 01 00 00 00       	mov    $0x1,%edi
  401ccc:	e8 1f f1 ff ff       	callq  400df0 <exit@plt>
  401cd1:	bf 00 34 40 00       	mov    $0x403400,%edi
  401cd6:	e8 75 ef ff ff       	callq  400c50 <puts@plt>
  401cdb:	bf e3 32 40 00       	mov    $0x4032e3,%edi
  401ce0:	e8 6b ef ff ff       	callq  400c50 <puts@plt>
  401ce5:	e9 85 00 00 00       	jmpq   401d6f <notify_server+0x193>
  401cea:	bf ed 32 40 00       	mov    $0x4032ed,%edi
  401cef:	e8 5c ef ff ff       	callq  400c50 <puts@plt>
  401cf4:	eb 79                	jmp    401d6f <notify_server+0x193>
  401cf6:	85 db                	test   %ebx,%ebx
  401cf8:	74 08                	je     401d02 <notify_server+0x126>
  401cfa:	be b1 32 40 00       	mov    $0x4032b1,%esi
  401cff:	90                   	nop
  401d00:	eb 05                	jmp    401d07 <notify_server+0x12b>
  401d02:	be b6 32 40 00       	mov    $0x4032b6,%esi
  401d07:	bf 38 34 40 00       	mov    $0x403438,%edi
  401d0c:	b8 00 00 00 00       	mov    $0x0,%eax
  401d11:	e8 6a ef ff ff       	callq  400c80 <printf@plt>
  401d16:	48 8b 35 43 34 20 00 	mov    0x203443(%rip),%rsi        # 605160 <user_id>
  401d1d:	bf f4 32 40 00       	mov    $0x4032f4,%edi
  401d22:	b8 00 00 00 00       	mov    $0x0,%eax
  401d27:	e8 54 ef ff ff       	callq  400c80 <printf@plt>
  401d2c:	48 8b 35 45 34 20 00 	mov    0x203445(%rip),%rsi        # 605178 <course>
  401d33:	bf 01 33 40 00       	mov    $0x403301,%edi
  401d38:	b8 00 00 00 00       	mov    $0x0,%eax
  401d3d:	e8 3e ef ff ff       	callq  400c80 <printf@plt>
  401d42:	48 8b 35 27 34 20 00 	mov    0x203427(%rip),%rsi        # 605170 <lab>
  401d49:	bf 0d 33 40 00       	mov    $0x40330d,%edi
  401d4e:	b8 00 00 00 00       	mov    $0x0,%eax
  401d53:	e8 28 ef ff ff       	callq  400c80 <printf@plt>
  401d58:	48 8d b4 24 10 20 00 	lea    0x2010(%rsp),%rsi
  401d5f:	00 
  401d60:	bf 16 33 40 00       	mov    $0x403316,%edi
  401d65:	b8 00 00 00 00       	mov    $0x0,%eax
  401d6a:	e8 11 ef ff ff       	callq  400c80 <printf@plt>
  401d6f:	48 81 c4 10 40 00 00 	add    $0x4010,%rsp
  401d76:	5b                   	pop    %rbx
  401d77:	f3 c3                	repz retq 

0000000000401d79 <validate>:
  401d79:	53                   	push   %rbx
  401d7a:	89 fb                	mov    %edi,%ebx
  401d7c:	83 3d 85 37 20 00 00 	cmpl   $0x0,0x203785(%rip)        # 605508 <is_checker>
  401d83:	74 60                	je     401de5 <validate+0x6c>
  401d85:	39 3d 71 37 20 00    	cmp    %edi,0x203771(%rip)        # 6054fc <vlevel>
  401d8b:	74 14                	je     401da1 <validate+0x28>
  401d8d:	bf 22 33 40 00       	mov    $0x403322,%edi
  401d92:	e8 b9 ee ff ff       	callq  400c50 <puts@plt>
  401d97:	b8 00 00 00 00       	mov    $0x0,%eax
  401d9c:	e8 ba fd ff ff       	callq  401b5b <check_fail>
  401da1:	8b 35 51 37 20 00    	mov    0x203751(%rip),%esi        # 6054f8 <check_level>
  401da7:	39 fe                	cmp    %edi,%esi
  401da9:	74 1b                	je     401dc6 <validate+0x4d>
  401dab:	89 fa                	mov    %edi,%edx
  401dad:	bf 60 34 40 00       	mov    $0x403460,%edi
  401db2:	b8 00 00 00 00       	mov    $0x0,%eax
  401db7:	e8 c4 ee ff ff       	callq  400c80 <printf@plt>
  401dbc:	b8 00 00 00 00       	mov    $0x0,%eax
  401dc1:	e8 95 fd ff ff       	callq  401b5b <check_fail>
  401dc6:	0f be 35 5b 43 20 00 	movsbl 0x20435b(%rip),%esi        # 606128 <target_prefix>
  401dcd:	b9 20 55 60 00       	mov    $0x605520,%ecx
  401dd2:	89 fa                	mov    %edi,%edx
  401dd4:	bf 40 33 40 00       	mov    $0x403340,%edi
  401dd9:	b8 00 00 00 00       	mov    $0x0,%eax
  401dde:	e8 9d ee ff ff       	callq  400c80 <printf@plt>
  401de3:	eb 44                	jmp    401e29 <validate+0xb0>
  401de5:	39 3d 11 37 20 00    	cmp    %edi,0x203711(%rip)        # 6054fc <vlevel>
  401deb:	74 18                	je     401e05 <validate+0x8c>
  401ded:	bf 22 33 40 00       	mov    $0x403322,%edi
  401df2:	e8 59 ee ff ff       	callq  400c50 <puts@plt>
  401df7:	89 de                	mov    %ebx,%esi
  401df9:	bf 00 00 00 00       	mov    $0x0,%edi
  401dfe:	e8 d9 fd ff ff       	callq  401bdc <notify_server>
  401e03:	eb 24                	jmp    401e29 <validate+0xb0>
  401e05:	0f be 15 1c 43 20 00 	movsbl 0x20431c(%rip),%edx        # 606128 <target_prefix>
  401e0c:	89 fe                	mov    %edi,%esi
  401e0e:	bf 88 34 40 00       	mov    $0x403488,%edi
  401e13:	b8 00 00 00 00       	mov    $0x0,%eax
  401e18:	e8 63 ee ff ff       	callq  400c80 <printf@plt>
  401e1d:	89 de                	mov    %ebx,%esi
  401e1f:	bf 01 00 00 00       	mov    $0x1,%edi
  401e24:	e8 b3 fd ff ff       	callq  401bdc <notify_server>
  401e29:	5b                   	pop    %rbx
  401e2a:	c3                   	retq   

0000000000401e2b <fail>:
  401e2b:	48 83 ec 08          	sub    $0x8,%rsp
  401e2f:	83 3d d2 36 20 00 00 	cmpl   $0x0,0x2036d2(%rip)        # 605508 <is_checker>
  401e36:	74 0a                	je     401e42 <fail+0x17>
  401e38:	b8 00 00 00 00       	mov    $0x0,%eax
  401e3d:	e8 19 fd ff ff       	callq  401b5b <check_fail>
  401e42:	89 fe                	mov    %edi,%esi
  401e44:	bf 00 00 00 00       	mov    $0x0,%edi
  401e49:	e8 8e fd ff ff       	callq  401bdc <notify_server>
  401e4e:	48 83 c4 08          	add    $0x8,%rsp
  401e52:	c3                   	retq   

0000000000401e53 <bushandler>:
  401e53:	48 83 ec 08          	sub    $0x8,%rsp
  401e57:	83 3d aa 36 20 00 00 	cmpl   $0x0,0x2036aa(%rip)        # 605508 <is_checker>
  401e5e:	74 14                	je     401e74 <bushandler+0x21>
  401e60:	bf 55 33 40 00       	mov    $0x403355,%edi
  401e65:	e8 e6 ed ff ff       	callq  400c50 <puts@plt>
  401e6a:	b8 00 00 00 00       	mov    $0x0,%eax
  401e6f:	e8 e7 fc ff ff       	callq  401b5b <check_fail>
  401e74:	bf c0 34 40 00       	mov    $0x4034c0,%edi
  401e79:	e8 d2 ed ff ff       	callq  400c50 <puts@plt>
  401e7e:	bf 5f 33 40 00       	mov    $0x40335f,%edi
  401e83:	e8 c8 ed ff ff       	callq  400c50 <puts@plt>
  401e88:	be 00 00 00 00       	mov    $0x0,%esi
  401e8d:	bf 00 00 00 00       	mov    $0x0,%edi
  401e92:	e8 45 fd ff ff       	callq  401bdc <notify_server>
  401e97:	bf 01 00 00 00       	mov    $0x1,%edi
  401e9c:	e8 4f ef ff ff       	callq  400df0 <exit@plt>

0000000000401ea1 <seghandler>:
  401ea1:	48 83 ec 08          	sub    $0x8,%rsp
  401ea5:	83 3d 5c 36 20 00 00 	cmpl   $0x0,0x20365c(%rip)        # 605508 <is_checker>
  401eac:	74 14                	je     401ec2 <seghandler+0x21>
  401eae:	bf 75 33 40 00       	mov    $0x403375,%edi
  401eb3:	e8 98 ed ff ff       	callq  400c50 <puts@plt>
  401eb8:	b8 00 00 00 00       	mov    $0x0,%eax
  401ebd:	e8 99 fc ff ff       	callq  401b5b <check_fail>
  401ec2:	bf e0 34 40 00       	mov    $0x4034e0,%edi
  401ec7:	e8 84 ed ff ff       	callq  400c50 <puts@plt>
  401ecc:	bf 5f 33 40 00       	mov    $0x40335f,%edi
  401ed1:	e8 7a ed ff ff       	callq  400c50 <puts@plt>
  401ed6:	be 00 00 00 00       	mov    $0x0,%esi
  401edb:	bf 00 00 00 00       	mov    $0x0,%edi
  401ee0:	e8 f7 fc ff ff       	callq  401bdc <notify_server>
  401ee5:	bf 01 00 00 00       	mov    $0x1,%edi
  401eea:	e8 01 ef ff ff       	callq  400df0 <exit@plt>

0000000000401eef <illegalhandler>:
  401eef:	48 83 ec 08          	sub    $0x8,%rsp
  401ef3:	83 3d 0e 36 20 00 00 	cmpl   $0x0,0x20360e(%rip)        # 605508 <is_checker>
  401efa:	74 14                	je     401f10 <illegalhandler+0x21>
  401efc:	bf 88 33 40 00       	mov    $0x403388,%edi
  401f01:	e8 4a ed ff ff       	callq  400c50 <puts@plt>
  401f06:	b8 00 00 00 00       	mov    $0x0,%eax
  401f0b:	e8 4b fc ff ff       	callq  401b5b <check_fail>
  401f10:	bf 08 35 40 00       	mov    $0x403508,%edi
  401f15:	e8 36 ed ff ff       	callq  400c50 <puts@plt>
  401f1a:	bf 5f 33 40 00       	mov    $0x40335f,%edi
  401f1f:	e8 2c ed ff ff       	callq  400c50 <puts@plt>
  401f24:	be 00 00 00 00       	mov    $0x0,%esi
  401f29:	bf 00 00 00 00       	mov    $0x0,%edi
  401f2e:	e8 a9 fc ff ff       	callq  401bdc <notify_server>
  401f33:	bf 01 00 00 00       	mov    $0x1,%edi
  401f38:	e8 b3 ee ff ff       	callq  400df0 <exit@plt>

0000000000401f3d <sigalrmhandler>:
  401f3d:	48 83 ec 08          	sub    $0x8,%rsp
  401f41:	83 3d c0 35 20 00 00 	cmpl   $0x0,0x2035c0(%rip)        # 605508 <is_checker>
  401f48:	74 14                	je     401f5e <sigalrmhandler+0x21>
  401f4a:	bf 9c 33 40 00       	mov    $0x40339c,%edi
  401f4f:	e8 fc ec ff ff       	callq  400c50 <puts@plt>
  401f54:	b8 00 00 00 00       	mov    $0x0,%eax
  401f59:	e8 fd fb ff ff       	callq  401b5b <check_fail>
  401f5e:	be 05 00 00 00       	mov    $0x5,%esi
  401f63:	bf 38 35 40 00       	mov    $0x403538,%edi
  401f68:	b8 00 00 00 00       	mov    $0x0,%eax
  401f6d:	e8 0e ed ff ff       	callq  400c80 <printf@plt>
  401f72:	be 00 00 00 00       	mov    $0x0,%esi
  401f77:	bf 00 00 00 00       	mov    $0x0,%edi
  401f7c:	e8 5b fc ff ff       	callq  401bdc <notify_server>
  401f81:	bf 01 00 00 00       	mov    $0x1,%edi
  401f86:	e8 65 ee ff ff       	callq  400df0 <exit@plt>

0000000000401f8b <launch>:
  401f8b:	55                   	push   %rbp
  401f8c:	48 89 e5             	mov    %rsp,%rbp
  401f8f:	48 89 fa             	mov    %rdi,%rdx
  401f92:	48 8d 47 1e          	lea    0x1e(%rdi),%rax
  401f96:	48 83 e0 f0          	and    $0xfffffffffffffff0,%rax
  401f9a:	48 29 c4             	sub    %rax,%rsp
  401f9d:	48 8d 7c 24 0f       	lea    0xf(%rsp),%rdi
  401fa2:	48 83 e7 f0          	and    $0xfffffffffffffff0,%rdi
  401fa6:	be f4 00 00 00       	mov    $0xf4,%esi
  401fab:	e8 e0 ec ff ff       	callq  400c90 <memset@plt>
  401fb0:	48 8b 05 09 35 20 00 	mov    0x203509(%rip),%rax        # 6054c0 <stdin@@GLIBC_2.2.5>
  401fb7:	48 39 05 32 35 20 00 	cmp    %rax,0x203532(%rip)        # 6054f0 <infile>
  401fbe:	75 0f                	jne    401fcf <launch+0x44>
  401fc0:	bf a4 33 40 00       	mov    $0x4033a4,%edi
  401fc5:	b8 00 00 00 00       	mov    $0x0,%eax
  401fca:	e8 b1 ec ff ff       	callq  400c80 <printf@plt>
  401fcf:	c7 05 23 35 20 00 00 	movl   $0x0,0x203523(%rip)        # 6054fc <vlevel>
  401fd6:	00 00 00 
  401fd9:	b8 00 00 00 00       	mov    $0x0,%eax
  401fde:	e8 c3 f9 ff ff       	callq  4019a6 <test>
  401fe3:	83 3d 1e 35 20 00 00 	cmpl   $0x0,0x20351e(%rip)        # 605508 <is_checker>
  401fea:	74 14                	je     402000 <launch+0x75>
  401fec:	bf b1 33 40 00       	mov    $0x4033b1,%edi
  401ff1:	e8 5a ec ff ff       	callq  400c50 <puts@plt>
  401ff6:	b8 00 00 00 00       	mov    $0x0,%eax
  401ffb:	e8 5b fb ff ff       	callq  401b5b <check_fail>
  402000:	bf bc 33 40 00       	mov    $0x4033bc,%edi
  402005:	e8 46 ec ff ff       	callq  400c50 <puts@plt>
  40200a:	c9                   	leaveq 
  40200b:	c3                   	retq   

000000000040200c <stable_launch>:
  40200c:	53                   	push   %rbx
  40200d:	48 89 3d d4 34 20 00 	mov    %rdi,0x2034d4(%rip)        # 6054e8 <global_offset>
  402014:	41 b9 00 00 00 00    	mov    $0x0,%r9d
  40201a:	41 b8 00 00 00 00    	mov    $0x0,%r8d
  402020:	b9 32 01 00 00       	mov    $0x132,%ecx
  402025:	ba 07 00 00 00       	mov    $0x7,%edx
  40202a:	be 00 00 10 00       	mov    $0x100000,%esi
  40202f:	bf 00 60 58 55       	mov    $0x55586000,%edi
  402034:	e8 37 ec ff ff       	callq  400c70 <mmap@plt>
  402039:	48 89 c3             	mov    %rax,%rbx
  40203c:	48 3d 00 60 58 55    	cmp    $0x55586000,%rax
  402042:	74 32                	je     402076 <stable_launch+0x6a>
  402044:	be 00 00 10 00       	mov    $0x100000,%esi
  402049:	48 89 c7             	mov    %rax,%rdi
  40204c:	e8 2f ed ff ff       	callq  400d80 <munmap@plt>
  402051:	ba 00 60 58 55       	mov    $0x55586000,%edx
  402056:	be 70 35 40 00       	mov    $0x403570,%esi
  40205b:	48 8b 3d 6e 34 20 00 	mov    0x20346e(%rip),%rdi        # 6054d0 <stderr@@GLIBC_2.2.5>
  402062:	b8 00 00 00 00       	mov    $0x0,%eax
  402067:	e8 94 ec ff ff       	callq  400d00 <fprintf@plt>
  40206c:	bf 01 00 00 00       	mov    $0x1,%edi
  402071:	e8 7a ed ff ff       	callq  400df0 <exit@plt>
  402076:	48 8d 90 f8 ff 0f 00 	lea    0xffff8(%rax),%rdx
  40207d:	48 89 15 ac 40 20 00 	mov    %rdx,0x2040ac(%rip)        # 606130 <stack_top>
  402084:	48 89 e0             	mov    %rsp,%rax
  402087:	48 89 d4             	mov    %rdx,%rsp
  40208a:	48 89 c2             	mov    %rax,%rdx
  40208d:	48 89 15 4c 34 20 00 	mov    %rdx,0x20344c(%rip)        # 6054e0 <global_save_stack>
  402094:	48 8b 3d 4d 34 20 00 	mov    0x20344d(%rip),%rdi        # 6054e8 <global_offset>
  40209b:	e8 eb fe ff ff       	callq  401f8b <launch>
  4020a0:	48 8b 05 39 34 20 00 	mov    0x203439(%rip),%rax        # 6054e0 <global_save_stack>
  4020a7:	48 89 c4             	mov    %rax,%rsp
  4020aa:	be 00 00 10 00       	mov    $0x100000,%esi
  4020af:	48 89 df             	mov    %rbx,%rdi
  4020b2:	e8 c9 ec ff ff       	callq  400d80 <munmap@plt>
  4020b7:	5b                   	pop    %rbx
  4020b8:	c3                   	retq   
  4020b9:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)

00000000004020c0 <rio_readinitb>:
  4020c0:	89 37                	mov    %esi,(%rdi)
  4020c2:	c7 47 04 00 00 00 00 	movl   $0x0,0x4(%rdi)
  4020c9:	48 8d 47 10          	lea    0x10(%rdi),%rax
  4020cd:	48 89 47 08          	mov    %rax,0x8(%rdi)
  4020d1:	c3                   	retq   

00000000004020d2 <sigalrm_handler>:
  4020d2:	48 83 ec 08          	sub    $0x8,%rsp
  4020d6:	ba 00 00 00 00       	mov    $0x0,%edx
  4020db:	be b0 35 40 00       	mov    $0x4035b0,%esi
  4020e0:	48 8b 3d e9 33 20 00 	mov    0x2033e9(%rip),%rdi        # 6054d0 <stderr@@GLIBC_2.2.5>
  4020e7:	b8 00 00 00 00       	mov    $0x0,%eax
  4020ec:	e8 0f ec ff ff       	callq  400d00 <fprintf@plt>
  4020f1:	bf 01 00 00 00       	mov    $0x1,%edi
  4020f6:	e8 f5 ec ff ff       	callq  400df0 <exit@plt>

00000000004020fb <urlencode>:
  4020fb:	41 54                	push   %r12
  4020fd:	55                   	push   %rbp
  4020fe:	53                   	push   %rbx
  4020ff:	48 83 ec 10          	sub    $0x10,%rsp
  402103:	48 89 fb             	mov    %rdi,%rbx
  402106:	48 89 f5             	mov    %rsi,%rbp
  402109:	b8 00 00 00 00       	mov    $0x0,%eax
  40210e:	48 c7 c1 ff ff ff ff 	mov    $0xffffffffffffffff,%rcx
  402115:	f2 ae                	repnz scas %es:(%rdi),%al
  402117:	48 f7 d1             	not    %rcx
  40211a:	8d 41 ff             	lea    -0x1(%rcx),%eax
  40211d:	e9 93 00 00 00       	jmpq   4021b5 <urlencode+0xba>
  402122:	0f b6 13             	movzbl (%rbx),%edx
  402125:	80 fa 2a             	cmp    $0x2a,%dl
  402128:	0f 94 c1             	sete   %cl
  40212b:	80 fa 2d             	cmp    $0x2d,%dl
  40212e:	0f 94 c0             	sete   %al
  402131:	08 c1                	or     %al,%cl
  402133:	75 1f                	jne    402154 <urlencode+0x59>
  402135:	80 fa 2e             	cmp    $0x2e,%dl
  402138:	74 1a                	je     402154 <urlencode+0x59>
  40213a:	80 fa 5f             	cmp    $0x5f,%dl
  40213d:	74 15                	je     402154 <urlencode+0x59>
  40213f:	8d 42 d0             	lea    -0x30(%rdx),%eax
  402142:	3c 09                	cmp    $0x9,%al
  402144:	76 0e                	jbe    402154 <urlencode+0x59>
  402146:	8d 42 bf             	lea    -0x41(%rdx),%eax
  402149:	3c 19                	cmp    $0x19,%al
  40214b:	76 07                	jbe    402154 <urlencode+0x59>
  40214d:	8d 42 9f             	lea    -0x61(%rdx),%eax
  402150:	3c 19                	cmp    $0x19,%al
  402152:	77 09                	ja     40215d <urlencode+0x62>
  402154:	88 55 00             	mov    %dl,0x0(%rbp)
  402157:	48 8d 6d 01          	lea    0x1(%rbp),%rbp
  40215b:	eb 51                	jmp    4021ae <urlencode+0xb3>
  40215d:	80 fa 20             	cmp    $0x20,%dl
  402160:	75 0a                	jne    40216c <urlencode+0x71>
  402162:	c6 45 00 2b          	movb   $0x2b,0x0(%rbp)
  402166:	48 8d 6d 01          	lea    0x1(%rbp),%rbp
  40216a:	eb 42                	jmp    4021ae <urlencode+0xb3>
  40216c:	8d 42 e0             	lea    -0x20(%rdx),%eax
  40216f:	3c 5f                	cmp    $0x5f,%al
  402171:	0f 96 c1             	setbe  %cl
  402174:	80 fa 09             	cmp    $0x9,%dl
  402177:	0f 94 c0             	sete   %al
  40217a:	08 c1                	or     %al,%cl
  40217c:	74 45                	je     4021c3 <urlencode+0xc8>
  40217e:	0f b6 d2             	movzbl %dl,%edx
  402181:	be 48 36 40 00       	mov    $0x403648,%esi
  402186:	48 89 e7             	mov    %rsp,%rdi
  402189:	b8 00 00 00 00       	mov    $0x0,%eax
  40218e:	e8 4d ec ff ff       	callq  400de0 <sprintf@plt>
  402193:	0f b6 04 24          	movzbl (%rsp),%eax
  402197:	88 45 00             	mov    %al,0x0(%rbp)
  40219a:	0f b6 44 24 01       	movzbl 0x1(%rsp),%eax
  40219f:	88 45 01             	mov    %al,0x1(%rbp)
  4021a2:	0f b6 44 24 02       	movzbl 0x2(%rsp),%eax
  4021a7:	88 45 02             	mov    %al,0x2(%rbp)
  4021aa:	48 8d 6d 03          	lea    0x3(%rbp),%rbp
  4021ae:	48 83 c3 01          	add    $0x1,%rbx
  4021b2:	44 89 e0             	mov    %r12d,%eax
  4021b5:	44 8d 60 ff          	lea    -0x1(%rax),%r12d
  4021b9:	85 c0                	test   %eax,%eax
  4021bb:	0f 85 61 ff ff ff    	jne    402122 <urlencode+0x27>
  4021c1:	eb 05                	jmp    4021c8 <urlencode+0xcd>
  4021c3:	b8 ff ff ff ff       	mov    $0xffffffff,%eax
  4021c8:	48 83 c4 10          	add    $0x10,%rsp
  4021cc:	5b                   	pop    %rbx
  4021cd:	5d                   	pop    %rbp
  4021ce:	41 5c                	pop    %r12
  4021d0:	c3                   	retq   

00000000004021d1 <rio_writen>:
  4021d1:	41 55                	push   %r13
  4021d3:	41 54                	push   %r12
  4021d5:	55                   	push   %rbp
  4021d6:	53                   	push   %rbx
  4021d7:	48 83 ec 08          	sub    $0x8,%rsp
  4021db:	41 89 fc             	mov    %edi,%r12d
  4021de:	48 89 f5             	mov    %rsi,%rbp
  4021e1:	49 89 d5             	mov    %rdx,%r13
  4021e4:	48 89 d3             	mov    %rdx,%rbx
  4021e7:	eb 28                	jmp    402211 <rio_writen+0x40>
  4021e9:	48 89 da             	mov    %rbx,%rdx
  4021ec:	48 89 ee             	mov    %rbp,%rsi
  4021ef:	44 89 e7             	mov    %r12d,%edi
  4021f2:	e8 69 ea ff ff       	callq  400c60 <write@plt>
  4021f7:	48 85 c0             	test   %rax,%rax
  4021fa:	7f 0f                	jg     40220b <rio_writen+0x3a>
  4021fc:	e8 0f ea ff ff       	callq  400c10 <__errno_location@plt>
  402201:	83 38 04             	cmpl   $0x4,(%rax)
  402204:	75 15                	jne    40221b <rio_writen+0x4a>
  402206:	b8 00 00 00 00       	mov    $0x0,%eax
  40220b:	48 29 c3             	sub    %rax,%rbx
  40220e:	48 01 c5             	add    %rax,%rbp
  402211:	48 85 db             	test   %rbx,%rbx
  402214:	75 d3                	jne    4021e9 <rio_writen+0x18>
  402216:	4c 89 e8             	mov    %r13,%rax
  402219:	eb 07                	jmp    402222 <rio_writen+0x51>
  40221b:	48 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%rax
  402222:	48 83 c4 08          	add    $0x8,%rsp
  402226:	5b                   	pop    %rbx
  402227:	5d                   	pop    %rbp
  402228:	41 5c                	pop    %r12
  40222a:	41 5d                	pop    %r13
  40222c:	c3                   	retq   

000000000040222d <rio_read>:
  40222d:	41 56                	push   %r14
  40222f:	41 55                	push   %r13
  402231:	41 54                	push   %r12
  402233:	55                   	push   %rbp
  402234:	53                   	push   %rbx
  402235:	48 89 fb             	mov    %rdi,%rbx
  402238:	49 89 f6             	mov    %rsi,%r14
  40223b:	49 89 d5             	mov    %rdx,%r13
  40223e:	4c 8d 67 10          	lea    0x10(%rdi),%r12
  402242:	eb 2a                	jmp    40226e <rio_read+0x41>
  402244:	ba 00 20 00 00       	mov    $0x2000,%edx
  402249:	4c 89 e6             	mov    %r12,%rsi
  40224c:	8b 3b                	mov    (%rbx),%edi
  40224e:	e8 6d ea ff ff       	callq  400cc0 <read@plt>
  402253:	89 43 04             	mov    %eax,0x4(%rbx)
  402256:	85 c0                	test   %eax,%eax
  402258:	79 0c                	jns    402266 <rio_read+0x39>
  40225a:	e8 b1 e9 ff ff       	callq  400c10 <__errno_location@plt>
  40225f:	83 38 04             	cmpl   $0x4,(%rax)
  402262:	74 0a                	je     40226e <rio_read+0x41>
  402264:	eb 37                	jmp    40229d <rio_read+0x70>
  402266:	85 c0                	test   %eax,%eax
  402268:	74 3c                	je     4022a6 <rio_read+0x79>
  40226a:	4c 89 63 08          	mov    %r12,0x8(%rbx)
  40226e:	8b 6b 04             	mov    0x4(%rbx),%ebp
  402271:	85 ed                	test   %ebp,%ebp
  402273:	7e cf                	jle    402244 <rio_read+0x17>
  402275:	89 e8                	mov    %ebp,%eax
  402277:	4c 39 e8             	cmp    %r13,%rax
  40227a:	72 03                	jb     40227f <rio_read+0x52>
  40227c:	44 89 ed             	mov    %r13d,%ebp
  40227f:	4c 63 e5             	movslq %ebp,%r12
  402282:	48 8b 73 08          	mov    0x8(%rbx),%rsi
  402286:	4c 89 e2             	mov    %r12,%rdx
  402289:	4c 89 f7             	mov    %r14,%rdi
  40228c:	e8 9f ea ff ff       	callq  400d30 <memcpy@plt>
  402291:	4c 01 63 08          	add    %r12,0x8(%rbx)
  402295:	29 6b 04             	sub    %ebp,0x4(%rbx)
  402298:	4c 89 e0             	mov    %r12,%rax
  40229b:	eb 0e                	jmp    4022ab <rio_read+0x7e>
  40229d:	48 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%rax
  4022a4:	eb 05                	jmp    4022ab <rio_read+0x7e>
  4022a6:	b8 00 00 00 00       	mov    $0x0,%eax
  4022ab:	5b                   	pop    %rbx
  4022ac:	5d                   	pop    %rbp
  4022ad:	41 5c                	pop    %r12
  4022af:	41 5d                	pop    %r13
  4022b1:	41 5e                	pop    %r14
  4022b3:	c3                   	retq   

00000000004022b4 <rio_readlineb>:
  4022b4:	41 55                	push   %r13
  4022b6:	41 54                	push   %r12
  4022b8:	55                   	push   %rbp
  4022b9:	53                   	push   %rbx
  4022ba:	48 83 ec 18          	sub    $0x18,%rsp
  4022be:	49 89 fd             	mov    %rdi,%r13
  4022c1:	48 89 f5             	mov    %rsi,%rbp
  4022c4:	49 89 d4             	mov    %rdx,%r12
  4022c7:	bb 01 00 00 00       	mov    $0x1,%ebx
  4022cc:	eb 3d                	jmp    40230b <rio_readlineb+0x57>
  4022ce:	ba 01 00 00 00       	mov    $0x1,%edx
  4022d3:	48 8d 74 24 0f       	lea    0xf(%rsp),%rsi
  4022d8:	4c 89 ef             	mov    %r13,%rdi
  4022db:	e8 4d ff ff ff       	callq  40222d <rio_read>
  4022e0:	83 f8 01             	cmp    $0x1,%eax
  4022e3:	75 12                	jne    4022f7 <rio_readlineb+0x43>
  4022e5:	48 8d 55 01          	lea    0x1(%rbp),%rdx
  4022e9:	0f b6 44 24 0f       	movzbl 0xf(%rsp),%eax
  4022ee:	88 45 00             	mov    %al,0x0(%rbp)
  4022f1:	3c 0a                	cmp    $0xa,%al
  4022f3:	75 0f                	jne    402304 <rio_readlineb+0x50>
  4022f5:	eb 1b                	jmp    402312 <rio_readlineb+0x5e>
  4022f7:	85 c0                	test   %eax,%eax
  4022f9:	75 23                	jne    40231e <rio_readlineb+0x6a>
  4022fb:	48 83 fb 01          	cmp    $0x1,%rbx
  4022ff:	90                   	nop
  402300:	75 13                	jne    402315 <rio_readlineb+0x61>
  402302:	eb 23                	jmp    402327 <rio_readlineb+0x73>
  402304:	48 83 c3 01          	add    $0x1,%rbx
  402308:	48 89 d5             	mov    %rdx,%rbp
  40230b:	4c 39 e3             	cmp    %r12,%rbx
  40230e:	72 be                	jb     4022ce <rio_readlineb+0x1a>
  402310:	eb 03                	jmp    402315 <rio_readlineb+0x61>
  402312:	48 89 d5             	mov    %rdx,%rbp
  402315:	c6 45 00 00          	movb   $0x0,0x0(%rbp)
  402319:	48 89 d8             	mov    %rbx,%rax
  40231c:	eb 0e                	jmp    40232c <rio_readlineb+0x78>
  40231e:	48 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%rax
  402325:	eb 05                	jmp    40232c <rio_readlineb+0x78>
  402327:	b8 00 00 00 00       	mov    $0x0,%eax
  40232c:	48 83 c4 18          	add    $0x18,%rsp
  402330:	5b                   	pop    %rbx
  402331:	5d                   	pop    %rbp
  402332:	41 5c                	pop    %r12
  402334:	41 5d                	pop    %r13
  402336:	c3                   	retq   

0000000000402337 <submitr>:
  402337:	41 57                	push   %r15
  402339:	41 56                	push   %r14
  40233b:	41 55                	push   %r13
  40233d:	41 54                	push   %r12
  40233f:	55                   	push   %rbp
  402340:	53                   	push   %rbx
  402341:	48 81 ec 48 a0 00 00 	sub    $0xa048,%rsp
  402348:	49 89 fc             	mov    %rdi,%r12
  40234b:	89 74 24 04          	mov    %esi,0x4(%rsp)
  40234f:	49 89 d7             	mov    %rdx,%r15
  402352:	49 89 ce             	mov    %rcx,%r14
  402355:	4c 89 44 24 08       	mov    %r8,0x8(%rsp)
  40235a:	4d 89 cd             	mov    %r9,%r13
  40235d:	48 8b 9c 24 80 a0 00 	mov    0xa080(%rsp),%rbx
  402364:	00 
  402365:	c7 84 24 1c 20 00 00 	movl   $0x0,0x201c(%rsp)
  40236c:	00 00 00 00 
  402370:	ba 00 00 00 00       	mov    $0x0,%edx
  402375:	be 01 00 00 00       	mov    $0x1,%esi
  40237a:	bf 02 00 00 00       	mov    $0x2,%edi
  40237f:	e8 8c ea ff ff       	callq  400e10 <socket@plt>
  402384:	89 c5                	mov    %eax,%ebp
  402386:	85 c0                	test   %eax,%eax
  402388:	79 4e                	jns    4023d8 <submitr+0xa1>
  40238a:	48 b8 45 72 72 6f 72 	movabs $0x43203a726f727245,%rax
  402391:	3a 20 43 
  402394:	48 89 03             	mov    %rax,(%rbx)
  402397:	48 b8 6c 69 65 6e 74 	movabs $0x6e7520746e65696c,%rax
  40239e:	20 75 6e 
  4023a1:	48 89 43 08          	mov    %rax,0x8(%rbx)
  4023a5:	48 b8 61 62 6c 65 20 	movabs $0x206f7420656c6261,%rax
  4023ac:	74 6f 20 
  4023af:	48 89 43 10          	mov    %rax,0x10(%rbx)
  4023b3:	48 b8 63 72 65 61 74 	movabs $0x7320657461657263,%rax
  4023ba:	65 20 73 
  4023bd:	48 89 43 18          	mov    %rax,0x18(%rbx)
  4023c1:	c7 43 20 6f 63 6b 65 	movl   $0x656b636f,0x20(%rbx)
  4023c8:	66 c7 43 24 74 00    	movw   $0x74,0x24(%rbx)
  4023ce:	b8 ff ff ff ff       	mov    $0xffffffff,%eax
  4023d3:	e9 68 06 00 00       	jmpq   402a40 <submitr+0x709>
  4023d8:	4c 89 e7             	mov    %r12,%rdi
  4023db:	e8 10 e9 ff ff       	callq  400cf0 <gethostbyname@plt>
  4023e0:	48 85 c0             	test   %rax,%rax
  4023e3:	75 67                	jne    40244c <submitr+0x115>
  4023e5:	48 b8 45 72 72 6f 72 	movabs $0x44203a726f727245,%rax
  4023ec:	3a 20 44 
  4023ef:	48 89 03             	mov    %rax,(%rbx)
  4023f2:	48 b8 4e 53 20 69 73 	movabs $0x6e7520736920534e,%rax
  4023f9:	20 75 6e 
  4023fc:	48 89 43 08          	mov    %rax,0x8(%rbx)
  402400:	48 b8 61 62 6c 65 20 	movabs $0x206f7420656c6261,%rax
  402407:	74 6f 20 
  40240a:	48 89 43 10          	mov    %rax,0x10(%rbx)
  40240e:	48 b8 72 65 73 6f 6c 	movabs $0x2065766c6f736572,%rax
  402415:	76 65 20 
  402418:	48 89 43 18          	mov    %rax,0x18(%rbx)
  40241c:	48 b8 73 65 72 76 65 	movabs $0x6120726576726573,%rax
  402423:	72 20 61 
  402426:	48 89 43 20          	mov    %rax,0x20(%rbx)
  40242a:	c7 43 28 64 64 72 65 	movl   $0x65726464,0x28(%rbx)
  402431:	66 c7 43 2c 73 73    	movw   $0x7373,0x2c(%rbx)
  402437:	c6 43 2e 00          	movb   $0x0,0x2e(%rbx)
  40243b:	89 ef                	mov    %ebp,%edi
  40243d:	e8 6e e8 ff ff       	callq  400cb0 <close@plt>
  402442:	b8 ff ff ff ff       	mov    $0xffffffff,%eax
  402447:	e9 f4 05 00 00       	jmpq   402a40 <submitr+0x709>
  40244c:	48 c7 84 24 30 a0 00 	movq   $0x0,0xa030(%rsp)
  402453:	00 00 00 00 00 
  402458:	48 c7 84 24 38 a0 00 	movq   $0x0,0xa038(%rsp)
  40245f:	00 00 00 00 00 
  402464:	66 c7 84 24 30 a0 00 	movw   $0x2,0xa030(%rsp)
  40246b:	00 02 00 
  40246e:	48 8b 48 18          	mov    0x18(%rax),%rcx
  402472:	48 63 50 14          	movslq 0x14(%rax),%rdx
  402476:	48 8d b4 24 34 a0 00 	lea    0xa034(%rsp),%rsi
  40247d:	00 
  40247e:	48 8b 39             	mov    (%rcx),%rdi
  402481:	e8 0a e9 ff ff       	callq  400d90 <bcopy@plt>
  402486:	0f b7 44 24 04       	movzwl 0x4(%rsp),%eax
  40248b:	66 c1 c8 08          	ror    $0x8,%ax
  40248f:	66 89 84 24 32 a0 00 	mov    %ax,0xa032(%rsp)
  402496:	00 
  402497:	ba 10 00 00 00       	mov    $0x10,%edx
  40249c:	48 8d b4 24 30 a0 00 	lea    0xa030(%rsp),%rsi
  4024a3:	00 
  4024a4:	89 ef                	mov    %ebp,%edi
  4024a6:	e8 55 e9 ff ff       	callq  400e00 <connect@plt>
  4024ab:	85 c0                	test   %eax,%eax
  4024ad:	79 59                	jns    402508 <submitr+0x1d1>
  4024af:	48 b8 45 72 72 6f 72 	movabs $0x55203a726f727245,%rax
  4024b6:	3a 20 55 
  4024b9:	48 89 03             	mov    %rax,(%rbx)
  4024bc:	48 b8 6e 61 62 6c 65 	movabs $0x6f7420656c62616e,%rax
  4024c3:	20 74 6f 
  4024c6:	48 89 43 08          	mov    %rax,0x8(%rbx)
  4024ca:	48 b8 20 63 6f 6e 6e 	movabs $0x7463656e6e6f6320,%rax
  4024d1:	65 63 74 
  4024d4:	48 89 43 10          	mov    %rax,0x10(%rbx)
  4024d8:	48 b8 20 74 6f 20 74 	movabs $0x20656874206f7420,%rax
  4024df:	68 65 20 
  4024e2:	48 89 43 18          	mov    %rax,0x18(%rbx)
  4024e6:	c7 43 20 73 65 72 76 	movl   $0x76726573,0x20(%rbx)
  4024ed:	66 c7 43 24 65 72    	movw   $0x7265,0x24(%rbx)
  4024f3:	c6 43 26 00          	movb   $0x0,0x26(%rbx)
  4024f7:	89 ef                	mov    %ebp,%edi
  4024f9:	e8 b2 e7 ff ff       	callq  400cb0 <close@plt>
  4024fe:	b8 ff ff ff ff       	mov    $0xffffffff,%eax
  402503:	e9 38 05 00 00       	jmpq   402a40 <submitr+0x709>
  402508:	48 c7 c2 ff ff ff ff 	mov    $0xffffffffffffffff,%rdx
  40250f:	4c 89 ef             	mov    %r13,%rdi
  402512:	b8 00 00 00 00       	mov    $0x0,%eax
  402517:	48 89 d1             	mov    %rdx,%rcx
  40251a:	f2 ae                	repnz scas %es:(%rdi),%al
  40251c:	48 f7 d1             	not    %rcx
  40251f:	48 89 ce             	mov    %rcx,%rsi
  402522:	4c 89 ff             	mov    %r15,%rdi
  402525:	48 89 d1             	mov    %rdx,%rcx
  402528:	f2 ae                	repnz scas %es:(%rdi),%al
  40252a:	48 f7 d1             	not    %rcx
  40252d:	49 89 c8             	mov    %rcx,%r8
  402530:	4c 89 f7             	mov    %r14,%rdi
  402533:	48 89 d1             	mov    %rdx,%rcx
  402536:	f2 ae                	repnz scas %es:(%rdi),%al
  402538:	49 29 c8             	sub    %rcx,%r8
  40253b:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  402540:	48 89 d1             	mov    %rdx,%rcx
  402543:	f2 ae                	repnz scas %es:(%rdi),%al
  402545:	49 29 c8             	sub    %rcx,%r8
  402548:	48 8d 44 76 fd       	lea    -0x3(%rsi,%rsi,2),%rax
  40254d:	49 8d 44 00 7b       	lea    0x7b(%r8,%rax,1),%rax
  402552:	48 3d 00 20 00 00    	cmp    $0x2000,%rax
  402558:	76 72                	jbe    4025cc <submitr+0x295>
  40255a:	48 b8 45 72 72 6f 72 	movabs $0x52203a726f727245,%rax
  402561:	3a 20 52 
  402564:	48 89 03             	mov    %rax,(%rbx)
  402567:	48 b8 65 73 75 6c 74 	movabs $0x747320746c757365,%rax
  40256e:	20 73 74 
  402571:	48 89 43 08          	mov    %rax,0x8(%rbx)
  402575:	48 b8 72 69 6e 67 20 	movabs $0x6f6f7420676e6972,%rax
  40257c:	74 6f 6f 
  40257f:	48 89 43 10          	mov    %rax,0x10(%rbx)
  402583:	48 b8 20 6c 61 72 67 	movabs $0x202e656772616c20,%rax
  40258a:	65 2e 20 
  40258d:	48 89 43 18          	mov    %rax,0x18(%rbx)
  402591:	48 b8 49 6e 63 72 65 	movabs $0x6573616572636e49,%rax
  402598:	61 73 65 
  40259b:	48 89 43 20          	mov    %rax,0x20(%rbx)
  40259f:	48 b8 20 53 55 42 4d 	movabs $0x5254494d42555320,%rax
  4025a6:	49 54 52 
  4025a9:	48 89 43 28          	mov    %rax,0x28(%rbx)
  4025ad:	48 b8 5f 4d 41 58 42 	movabs $0x46554258414d5f,%rax
  4025b4:	55 46 00 
  4025b7:	48 89 43 30          	mov    %rax,0x30(%rbx)
  4025bb:	89 ef                	mov    %ebp,%edi
  4025bd:	e8 ee e6 ff ff       	callq  400cb0 <close@plt>
  4025c2:	b8 ff ff ff ff       	mov    $0xffffffff,%eax
  4025c7:	e9 74 04 00 00       	jmpq   402a40 <submitr+0x709>
  4025cc:	48 8d b4 24 20 40 00 	lea    0x4020(%rsp),%rsi
  4025d3:	00 
  4025d4:	b9 00 04 00 00       	mov    $0x400,%ecx
  4025d9:	b8 00 00 00 00       	mov    $0x0,%eax
  4025de:	48 89 f7             	mov    %rsi,%rdi
  4025e1:	f3 48 ab             	rep stos %rax,%es:(%rdi)
  4025e4:	4c 89 ef             	mov    %r13,%rdi
  4025e7:	e8 0f fb ff ff       	callq  4020fb <urlencode>
  4025ec:	85 c0                	test   %eax,%eax
  4025ee:	0f 89 8a 00 00 00    	jns    40267e <submitr+0x347>
  4025f4:	48 b8 45 72 72 6f 72 	movabs $0x52203a726f727245,%rax
  4025fb:	3a 20 52 
  4025fe:	48 89 03             	mov    %rax,(%rbx)
  402601:	48 b8 65 73 75 6c 74 	movabs $0x747320746c757365,%rax
  402608:	20 73 74 
  40260b:	48 89 43 08          	mov    %rax,0x8(%rbx)
  40260f:	48 b8 72 69 6e 67 20 	movabs $0x6e6f6320676e6972,%rax
  402616:	63 6f 6e 
  402619:	48 89 43 10          	mov    %rax,0x10(%rbx)
  40261d:	48 b8 74 61 69 6e 73 	movabs $0x6e6120736e696174,%rax
  402624:	20 61 6e 
  402627:	48 89 43 18          	mov    %rax,0x18(%rbx)
  40262b:	48 b8 20 69 6c 6c 65 	movabs $0x6c6167656c6c6920,%rax
  402632:	67 61 6c 
  402635:	48 89 43 20          	mov    %rax,0x20(%rbx)
  402639:	48 b8 20 6f 72 20 75 	movabs $0x72706e7520726f20,%rax
  402640:	6e 70 72 
  402643:	48 89 43 28          	mov    %rax,0x28(%rbx)
  402647:	48 b8 69 6e 74 61 62 	movabs $0x20656c6261746e69,%rax
  40264e:	6c 65 20 
  402651:	48 89 43 30          	mov    %rax,0x30(%rbx)
  402655:	48 b8 63 68 61 72 61 	movabs $0x6574636172616863,%rax
  40265c:	63 74 65 
  40265f:	48 89 43 38          	mov    %rax,0x38(%rbx)
  402663:	66 c7 43 40 72 2e    	movw   $0x2e72,0x40(%rbx)
  402669:	c6 43 42 00          	movb   $0x0,0x42(%rbx)
  40266d:	89 ef                	mov    %ebp,%edi
  40266f:	e8 3c e6 ff ff       	callq  400cb0 <close@plt>
  402674:	b8 ff ff ff ff       	mov    $0xffffffff,%eax
  402679:	e9 c2 03 00 00       	jmpq   402a40 <submitr+0x709>
  40267e:	4d 89 e1             	mov    %r12,%r9
  402681:	4c 8d 84 24 20 40 00 	lea    0x4020(%rsp),%r8
  402688:	00 
  402689:	4c 89 f9             	mov    %r15,%rcx
  40268c:	4c 89 f2             	mov    %r14,%rdx
  40268f:	be d8 35 40 00       	mov    $0x4035d8,%esi
  402694:	48 8d bc 24 20 60 00 	lea    0x6020(%rsp),%rdi
  40269b:	00 
  40269c:	b8 00 00 00 00       	mov    $0x0,%eax
  4026a1:	e8 3a e7 ff ff       	callq  400de0 <sprintf@plt>
  4026a6:	48 8d bc 24 20 60 00 	lea    0x6020(%rsp),%rdi
  4026ad:	00 
  4026ae:	b8 00 00 00 00       	mov    $0x0,%eax
  4026b3:	48 c7 c1 ff ff ff ff 	mov    $0xffffffffffffffff,%rcx
  4026ba:	f2 ae                	repnz scas %es:(%rdi),%al
  4026bc:	48 f7 d1             	not    %rcx
  4026bf:	48 8d 51 ff          	lea    -0x1(%rcx),%rdx
  4026c3:	48 8d b4 24 20 60 00 	lea    0x6020(%rsp),%rsi
  4026ca:	00 
  4026cb:	89 ef                	mov    %ebp,%edi
  4026cd:	e8 ff fa ff ff       	callq  4021d1 <rio_writen>
  4026d2:	48 85 c0             	test   %rax,%rax
  4026d5:	79 6e                	jns    402745 <submitr+0x40e>
  4026d7:	48 b8 45 72 72 6f 72 	movabs $0x43203a726f727245,%rax
  4026de:	3a 20 43 
  4026e1:	48 89 03             	mov    %rax,(%rbx)
  4026e4:	48 b8 6c 69 65 6e 74 	movabs $0x6e7520746e65696c,%rax
  4026eb:	20 75 6e 
  4026ee:	48 89 43 08          	mov    %rax,0x8(%rbx)
  4026f2:	48 b8 61 62 6c 65 20 	movabs $0x206f7420656c6261,%rax
  4026f9:	74 6f 20 
  4026fc:	48 89 43 10          	mov    %rax,0x10(%rbx)
  402700:	48 b8 77 72 69 74 65 	movabs $0x6f74206574697277,%rax
  402707:	20 74 6f 
  40270a:	48 89 43 18          	mov    %rax,0x18(%rbx)
  40270e:	48 b8 20 74 68 65 20 	movabs $0x7365722065687420,%rax
  402715:	72 65 73 
  402718:	48 89 43 20          	mov    %rax,0x20(%rbx)
  40271c:	48 b8 75 6c 74 20 73 	movabs $0x7672657320746c75,%rax
  402723:	65 72 76 
  402726:	48 89 43 28          	mov    %rax,0x28(%rbx)
  40272a:	66 c7 43 30 65 72    	movw   $0x7265,0x30(%rbx)
  402730:	c6 43 32 00          	movb   $0x0,0x32(%rbx)
  402734:	89 ef                	mov    %ebp,%edi
  402736:	e8 75 e5 ff ff       	callq  400cb0 <close@plt>
  40273b:	b8 ff ff ff ff       	mov    $0xffffffff,%eax
  402740:	e9 fb 02 00 00       	jmpq   402a40 <submitr+0x709>
  402745:	89 ee                	mov    %ebp,%esi
  402747:	48 8d bc 24 20 80 00 	lea    0x8020(%rsp),%rdi
  40274e:	00 
  40274f:	e8 6c f9 ff ff       	callq  4020c0 <rio_readinitb>
  402754:	ba 00 20 00 00       	mov    $0x2000,%edx
  402759:	48 8d b4 24 20 60 00 	lea    0x6020(%rsp),%rsi
  402760:	00 
  402761:	48 8d bc 24 20 80 00 	lea    0x8020(%rsp),%rdi
  402768:	00 
  402769:	e8 46 fb ff ff       	callq  4022b4 <rio_readlineb>
  40276e:	48 85 c0             	test   %rax,%rax
  402771:	7f 7d                	jg     4027f0 <submitr+0x4b9>
  402773:	48 b8 45 72 72 6f 72 	movabs $0x43203a726f727245,%rax
  40277a:	3a 20 43 
  40277d:	48 89 03             	mov    %rax,(%rbx)
  402780:	48 b8 6c 69 65 6e 74 	movabs $0x6e7520746e65696c,%rax
  402787:	20 75 6e 
  40278a:	48 89 43 08          	mov    %rax,0x8(%rbx)
  40278e:	48 b8 61 62 6c 65 20 	movabs $0x206f7420656c6261,%rax
  402795:	74 6f 20 
  402798:	48 89 43 10          	mov    %rax,0x10(%rbx)
  40279c:	48 b8 72 65 61 64 20 	movabs $0x7269662064616572,%rax
  4027a3:	66 69 72 
  4027a6:	48 89 43 18          	mov    %rax,0x18(%rbx)
  4027aa:	48 b8 73 74 20 68 65 	movabs $0x6564616568207473,%rax
  4027b1:	61 64 65 
  4027b4:	48 89 43 20          	mov    %rax,0x20(%rbx)
  4027b8:	48 b8 72 20 66 72 6f 	movabs $0x72206d6f72662072,%rax
  4027bf:	6d 20 72 
  4027c2:	48 89 43 28          	mov    %rax,0x28(%rbx)
  4027c6:	48 b8 65 73 75 6c 74 	movabs $0x657320746c757365,%rax
  4027cd:	20 73 65 
  4027d0:	48 89 43 30          	mov    %rax,0x30(%rbx)
  4027d4:	c7 43 38 72 76 65 72 	movl   $0x72657672,0x38(%rbx)
  4027db:	c6 43 3c 00          	movb   $0x0,0x3c(%rbx)
  4027df:	89 ef                	mov    %ebp,%edi
  4027e1:	e8 ca e4 ff ff       	callq  400cb0 <close@plt>
  4027e6:	b8 ff ff ff ff       	mov    $0xffffffff,%eax
  4027eb:	e9 50 02 00 00       	jmpq   402a40 <submitr+0x709>
  4027f0:	4c 8d 44 24 10       	lea    0x10(%rsp),%r8
  4027f5:	48 8d 8c 24 1c 20 00 	lea    0x201c(%rsp),%rcx
  4027fc:	00 
  4027fd:	48 8d 94 24 20 20 00 	lea    0x2020(%rsp),%rdx
  402804:	00 
  402805:	be 4f 36 40 00       	mov    $0x40364f,%esi
  40280a:	48 8d bc 24 20 60 00 	lea    0x6020(%rsp),%rdi
  402811:	00 
  402812:	b8 00 00 00 00       	mov    $0x0,%eax
  402817:	e8 54 e5 ff ff       	callq  400d70 <__isoc99_sscanf@plt>
  40281c:	e9 98 00 00 00       	jmpq   4028b9 <submitr+0x582>
  402821:	ba 00 20 00 00       	mov    $0x2000,%edx
  402826:	48 8d b4 24 20 60 00 	lea    0x6020(%rsp),%rsi
  40282d:	00 
  40282e:	48 8d bc 24 20 80 00 	lea    0x8020(%rsp),%rdi
  402835:	00 
  402836:	e8 79 fa ff ff       	callq  4022b4 <rio_readlineb>
  40283b:	48 85 c0             	test   %rax,%rax
  40283e:	7f 79                	jg     4028b9 <submitr+0x582>
  402840:	48 b8 45 72 72 6f 72 	movabs $0x43203a726f727245,%rax
  402847:	3a 20 43 
  40284a:	48 89 03             	mov    %rax,(%rbx)
  40284d:	48 b8 6c 69 65 6e 74 	movabs $0x6e7520746e65696c,%rax
  402854:	20 75 6e 
  402857:	48 89 43 08          	mov    %rax,0x8(%rbx)
  40285b:	48 b8 61 62 6c 65 20 	movabs $0x206f7420656c6261,%rax
  402862:	74 6f 20 
  402865:	48 89 43 10          	mov    %rax,0x10(%rbx)
  402869:	48 b8 72 65 61 64 20 	movabs $0x6165682064616572,%rax
  402870:	68 65 61 
  402873:	48 89 43 18          	mov    %rax,0x18(%rbx)
  402877:	48 b8 64 65 72 73 20 	movabs $0x6f72662073726564,%rax
  40287e:	66 72 6f 
  402881:	48 89 43 20          	mov    %rax,0x20(%rbx)
  402885:	48 b8 6d 20 74 68 65 	movabs $0x657220656874206d,%rax
  40288c:	20 72 65 
  40288f:	48 89 43 28          	mov    %rax,0x28(%rbx)
  402893:	48 b8 73 75 6c 74 20 	movabs $0x72657320746c7573,%rax
  40289a:	73 65 72 
  40289d:	48 89 43 30          	mov    %rax,0x30(%rbx)
  4028a1:	c7 43 38 76 65 72 00 	movl   $0x726576,0x38(%rbx)
  4028a8:	89 ef                	mov    %ebp,%edi
  4028aa:	e8 01 e4 ff ff       	callq  400cb0 <close@plt>
  4028af:	b8 ff ff ff ff       	mov    $0xffffffff,%eax
  4028b4:	e9 87 01 00 00       	jmpq   402a40 <submitr+0x709>
  4028b9:	0f b6 84 24 20 60 00 	movzbl 0x6020(%rsp),%eax
  4028c0:	00 
  4028c1:	83 e8 0d             	sub    $0xd,%eax
  4028c4:	75 15                	jne    4028db <submitr+0x5a4>
  4028c6:	0f b6 84 24 21 60 00 	movzbl 0x6021(%rsp),%eax
  4028cd:	00 
  4028ce:	83 e8 0a             	sub    $0xa,%eax
  4028d1:	75 08                	jne    4028db <submitr+0x5a4>
  4028d3:	0f b6 84 24 22 60 00 	movzbl 0x6022(%rsp),%eax
  4028da:	00 
  4028db:	85 c0                	test   %eax,%eax
  4028dd:	0f 85 3e ff ff ff    	jne    402821 <submitr+0x4ea>
  4028e3:	ba 00 20 00 00       	mov    $0x2000,%edx
  4028e8:	48 8d b4 24 20 60 00 	lea    0x6020(%rsp),%rsi
  4028ef:	00 
  4028f0:	48 8d bc 24 20 80 00 	lea    0x8020(%rsp),%rdi
  4028f7:	00 
  4028f8:	e8 b7 f9 ff ff       	callq  4022b4 <rio_readlineb>
  4028fd:	48 85 c0             	test   %rax,%rax
  402900:	0f 8f 83 00 00 00    	jg     402989 <submitr+0x652>
  402906:	48 b8 45 72 72 6f 72 	movabs $0x43203a726f727245,%rax
  40290d:	3a 20 43 
  402910:	48 89 03             	mov    %rax,(%rbx)
  402913:	48 b8 6c 69 65 6e 74 	movabs $0x6e7520746e65696c,%rax
  40291a:	20 75 6e 
  40291d:	48 89 43 08          	mov    %rax,0x8(%rbx)
  402921:	48 b8 61 62 6c 65 20 	movabs $0x206f7420656c6261,%rax
  402928:	74 6f 20 
  40292b:	48 89 43 10          	mov    %rax,0x10(%rbx)
  40292f:	48 b8 72 65 61 64 20 	movabs $0x6174732064616572,%rax
  402936:	73 74 61 
  402939:	48 89 43 18          	mov    %rax,0x18(%rbx)
  40293d:	48 b8 74 75 73 20 6d 	movabs $0x7373656d20737574,%rax
  402944:	65 73 73 
  402947:	48 89 43 20          	mov    %rax,0x20(%rbx)
  40294b:	48 b8 61 67 65 20 66 	movabs $0x6d6f726620656761,%rax
  402952:	72 6f 6d 
  402955:	48 89 43 28          	mov    %rax,0x28(%rbx)
  402959:	48 b8 20 72 65 73 75 	movabs $0x20746c7573657220,%rax
  402960:	6c 74 20 
  402963:	48 89 43 30          	mov    %rax,0x30(%rbx)
  402967:	c7 43 38 73 65 72 76 	movl   $0x76726573,0x38(%rbx)
  40296e:	66 c7 43 3c 65 72    	movw   $0x7265,0x3c(%rbx)
  402974:	c6 43 3e 00          	movb   $0x0,0x3e(%rbx)
  402978:	89 ef                	mov    %ebp,%edi
  40297a:	e8 31 e3 ff ff       	callq  400cb0 <close@plt>
  40297f:	b8 ff ff ff ff       	mov    $0xffffffff,%eax
  402984:	e9 b7 00 00 00       	jmpq   402a40 <submitr+0x709>
  402989:	8b 94 24 1c 20 00 00 	mov    0x201c(%rsp),%edx
  402990:	81 fa c8 00 00 00    	cmp    $0xc8,%edx
  402996:	74 28                	je     4029c0 <submitr+0x689>
  402998:	48 8d 4c 24 10       	lea    0x10(%rsp),%rcx
  40299d:	be 18 36 40 00       	mov    $0x403618,%esi
  4029a2:	48 89 df             	mov    %rbx,%rdi
  4029a5:	b8 00 00 00 00       	mov    $0x0,%eax
  4029aa:	e8 31 e4 ff ff       	callq  400de0 <sprintf@plt>
  4029af:	89 ef                	mov    %ebp,%edi
  4029b1:	e8 fa e2 ff ff       	callq  400cb0 <close@plt>
  4029b6:	b8 ff ff ff ff       	mov    $0xffffffff,%eax
  4029bb:	e9 80 00 00 00       	jmpq   402a40 <submitr+0x709>
  4029c0:	48 8d b4 24 20 60 00 	lea    0x6020(%rsp),%rsi
  4029c7:	00 
  4029c8:	48 89 df             	mov    %rbx,%rdi
  4029cb:	e8 70 e2 ff ff       	callq  400c40 <strcpy@plt>
  4029d0:	89 ef                	mov    %ebp,%edi
  4029d2:	e8 d9 e2 ff ff       	callq  400cb0 <close@plt>
  4029d7:	0f b6 03             	movzbl (%rbx),%eax
  4029da:	83 e8 4f             	sub    $0x4f,%eax
  4029dd:	75 18                	jne    4029f7 <submitr+0x6c0>
  4029df:	0f b6 53 01          	movzbl 0x1(%rbx),%edx
  4029e3:	83 ea 4b             	sub    $0x4b,%edx
  4029e6:	75 11                	jne    4029f9 <submitr+0x6c2>
  4029e8:	0f b6 53 02          	movzbl 0x2(%rbx),%edx
  4029ec:	83 ea 0a             	sub    $0xa,%edx
  4029ef:	75 08                	jne    4029f9 <submitr+0x6c2>
  4029f1:	0f b6 53 03          	movzbl 0x3(%rbx),%edx
  4029f5:	eb 02                	jmp    4029f9 <submitr+0x6c2>
  4029f7:	89 c2                	mov    %eax,%edx
  4029f9:	85 d2                	test   %edx,%edx
  4029fb:	74 30                	je     402a2d <submitr+0x6f6>
  4029fd:	bf 60 36 40 00       	mov    $0x403660,%edi
  402a02:	b9 05 00 00 00       	mov    $0x5,%ecx
  402a07:	48 89 de             	mov    %rbx,%rsi
  402a0a:	f3 a6                	repz cmpsb %es:(%rdi),%ds:(%rsi)
  402a0c:	0f 97 c1             	seta   %cl
  402a0f:	0f 92 c2             	setb   %dl
  402a12:	38 d1                	cmp    %dl,%cl
  402a14:	74 1e                	je     402a34 <submitr+0x6fd>
  402a16:	85 c0                	test   %eax,%eax
  402a18:	75 0d                	jne    402a27 <submitr+0x6f0>
  402a1a:	0f b6 43 01          	movzbl 0x1(%rbx),%eax
  402a1e:	83 e8 4b             	sub    $0x4b,%eax
  402a21:	75 04                	jne    402a27 <submitr+0x6f0>
  402a23:	0f b6 43 02          	movzbl 0x2(%rbx),%eax
  402a27:	85 c0                	test   %eax,%eax
  402a29:	75 10                	jne    402a3b <submitr+0x704>
  402a2b:	eb 13                	jmp    402a40 <submitr+0x709>
  402a2d:	b8 00 00 00 00       	mov    $0x0,%eax
  402a32:	eb 0c                	jmp    402a40 <submitr+0x709>
  402a34:	b8 00 00 00 00       	mov    $0x0,%eax
  402a39:	eb 05                	jmp    402a40 <submitr+0x709>
  402a3b:	b8 ff ff ff ff       	mov    $0xffffffff,%eax
  402a40:	48 81 c4 48 a0 00 00 	add    $0xa048,%rsp
  402a47:	5b                   	pop    %rbx
  402a48:	5d                   	pop    %rbp
  402a49:	41 5c                	pop    %r12
  402a4b:	41 5d                	pop    %r13
  402a4d:	41 5e                	pop    %r14
  402a4f:	41 5f                	pop    %r15
  402a51:	c3                   	retq   

0000000000402a52 <init_timeout>:
  402a52:	53                   	push   %rbx
  402a53:	89 fb                	mov    %edi,%ebx
  402a55:	85 ff                	test   %edi,%edi
  402a57:	74 1f                	je     402a78 <init_timeout+0x26>
  402a59:	85 ff                	test   %edi,%edi
  402a5b:	79 05                	jns    402a62 <init_timeout+0x10>
  402a5d:	bb 00 00 00 00       	mov    $0x0,%ebx
  402a62:	be d2 20 40 00       	mov    $0x4020d2,%esi
  402a67:	bf 0e 00 00 00       	mov    $0xe,%edi
  402a6c:	e8 6f e2 ff ff       	callq  400ce0 <signal@plt>
  402a71:	89 df                	mov    %ebx,%edi
  402a73:	e8 28 e2 ff ff       	callq  400ca0 <alarm@plt>
  402a78:	5b                   	pop    %rbx
  402a79:	c3                   	retq   

0000000000402a7a <init_driver>:
  402a7a:	55                   	push   %rbp
  402a7b:	53                   	push   %rbx
  402a7c:	48 83 ec 18          	sub    $0x18,%rsp
  402a80:	48 89 fd             	mov    %rdi,%rbp
  402a83:	be 01 00 00 00       	mov    $0x1,%esi
  402a88:	bf 0d 00 00 00       	mov    $0xd,%edi
  402a8d:	e8 4e e2 ff ff       	callq  400ce0 <signal@plt>
  402a92:	be 01 00 00 00       	mov    $0x1,%esi
  402a97:	bf 1d 00 00 00       	mov    $0x1d,%edi
  402a9c:	e8 3f e2 ff ff       	callq  400ce0 <signal@plt>
  402aa1:	be 01 00 00 00       	mov    $0x1,%esi
  402aa6:	bf 1d 00 00 00       	mov    $0x1d,%edi
  402aab:	e8 30 e2 ff ff       	callq  400ce0 <signal@plt>
  402ab0:	ba 00 00 00 00       	mov    $0x0,%edx
  402ab5:	be 01 00 00 00       	mov    $0x1,%esi
  402aba:	bf 02 00 00 00       	mov    $0x2,%edi
  402abf:	e8 4c e3 ff ff       	callq  400e10 <socket@plt>
  402ac4:	89 c3                	mov    %eax,%ebx
  402ac6:	85 c0                	test   %eax,%eax
  402ac8:	79 4f                	jns    402b19 <init_driver+0x9f>
  402aca:	48 b8 45 72 72 6f 72 	movabs $0x43203a726f727245,%rax
  402ad1:	3a 20 43 
  402ad4:	48 89 45 00          	mov    %rax,0x0(%rbp)
  402ad8:	48 b8 6c 69 65 6e 74 	movabs $0x6e7520746e65696c,%rax
  402adf:	20 75 6e 
  402ae2:	48 89 45 08          	mov    %rax,0x8(%rbp)
  402ae6:	48 b8 61 62 6c 65 20 	movabs $0x206f7420656c6261,%rax
  402aed:	74 6f 20 
  402af0:	48 89 45 10          	mov    %rax,0x10(%rbp)
  402af4:	48 b8 63 72 65 61 74 	movabs $0x7320657461657263,%rax
  402afb:	65 20 73 
  402afe:	48 89 45 18          	mov    %rax,0x18(%rbp)
  402b02:	c7 45 20 6f 63 6b 65 	movl   $0x656b636f,0x20(%rbp)
  402b09:	66 c7 45 24 74 00    	movw   $0x74,0x24(%rbp)
  402b0f:	b8 ff ff ff ff       	mov    $0xffffffff,%eax
  402b14:	e9 23 01 00 00       	jmpq   402c3c <init_driver+0x1c2>
  402b19:	bf 40 31 40 00       	mov    $0x403140,%edi
  402b1e:	e8 cd e1 ff ff       	callq  400cf0 <gethostbyname@plt>
  402b23:	48 85 c0             	test   %rax,%rax
  402b26:	75 68                	jne    402b90 <init_driver+0x116>
  402b28:	48 b8 45 72 72 6f 72 	movabs $0x44203a726f727245,%rax
  402b2f:	3a 20 44 
  402b32:	48 89 45 00          	mov    %rax,0x0(%rbp)
  402b36:	48 b8 4e 53 20 69 73 	movabs $0x6e7520736920534e,%rax
  402b3d:	20 75 6e 
  402b40:	48 89 45 08          	mov    %rax,0x8(%rbp)
  402b44:	48 b8 61 62 6c 65 20 	movabs $0x206f7420656c6261,%rax
  402b4b:	74 6f 20 
  402b4e:	48 89 45 10          	mov    %rax,0x10(%rbp)
  402b52:	48 b8 72 65 73 6f 6c 	movabs $0x2065766c6f736572,%rax
  402b59:	76 65 20 
  402b5c:	48 89 45 18          	mov    %rax,0x18(%rbp)
  402b60:	48 b8 73 65 72 76 65 	movabs $0x6120726576726573,%rax
  402b67:	72 20 61 
  402b6a:	48 89 45 20          	mov    %rax,0x20(%rbp)
  402b6e:	c7 45 28 64 64 72 65 	movl   $0x65726464,0x28(%rbp)
  402b75:	66 c7 45 2c 73 73    	movw   $0x7373,0x2c(%rbp)
  402b7b:	c6 45 2e 00          	movb   $0x0,0x2e(%rbp)
  402b7f:	89 df                	mov    %ebx,%edi
  402b81:	e8 2a e1 ff ff       	callq  400cb0 <close@plt>
  402b86:	b8 ff ff ff ff       	mov    $0xffffffff,%eax
  402b8b:	e9 ac 00 00 00       	jmpq   402c3c <init_driver+0x1c2>
  402b90:	48 c7 04 24 00 00 00 	movq   $0x0,(%rsp)
  402b97:	00 
  402b98:	48 c7 44 24 08 00 00 	movq   $0x0,0x8(%rsp)
  402b9f:	00 00 
  402ba1:	66 c7 04 24 02 00    	movw   $0x2,(%rsp)
  402ba7:	48 8b 48 18          	mov    0x18(%rax),%rcx
  402bab:	48 63 50 14          	movslq 0x14(%rax),%rdx
  402baf:	48 8d 74 24 04       	lea    0x4(%rsp),%rsi
  402bb4:	48 8b 39             	mov    (%rcx),%rdi
  402bb7:	e8 d4 e1 ff ff       	callq  400d90 <bcopy@plt>
  402bbc:	66 c7 44 24 02 47 26 	movw   $0x2647,0x2(%rsp)
  402bc3:	ba 10 00 00 00       	mov    $0x10,%edx
  402bc8:	48 89 e6             	mov    %rsp,%rsi
  402bcb:	89 df                	mov    %ebx,%edi
  402bcd:	e8 2e e2 ff ff       	callq  400e00 <connect@plt>
  402bd2:	85 c0                	test   %eax,%eax
  402bd4:	79 50                	jns    402c26 <init_driver+0x1ac>
  402bd6:	48 b8 45 72 72 6f 72 	movabs $0x55203a726f727245,%rax
  402bdd:	3a 20 55 
  402be0:	48 89 45 00          	mov    %rax,0x0(%rbp)
  402be4:	48 b8 6e 61 62 6c 65 	movabs $0x6f7420656c62616e,%rax
  402beb:	20 74 6f 
  402bee:	48 89 45 08          	mov    %rax,0x8(%rbp)
  402bf2:	48 b8 20 63 6f 6e 6e 	movabs $0x7463656e6e6f6320,%rax
  402bf9:	65 63 74 
  402bfc:	48 89 45 10          	mov    %rax,0x10(%rbp)
  402c00:	48 b8 20 74 6f 20 73 	movabs $0x76726573206f7420,%rax
  402c07:	65 72 76 
  402c0a:	48 89 45 18          	mov    %rax,0x18(%rbp)
  402c0e:	66 c7 45 20 65 72    	movw   $0x7265,0x20(%rbp)
  402c14:	c6 45 22 00          	movb   $0x0,0x22(%rbp)
  402c18:	89 df                	mov    %ebx,%edi
  402c1a:	e8 91 e0 ff ff       	callq  400cb0 <close@plt>
  402c1f:	b8 ff ff ff ff       	mov    $0xffffffff,%eax
  402c24:	eb 16                	jmp    402c3c <init_driver+0x1c2>
  402c26:	89 df                	mov    %ebx,%edi
  402c28:	e8 83 e0 ff ff       	callq  400cb0 <close@plt>
  402c2d:	66 c7 45 00 4f 4b    	movw   $0x4b4f,0x0(%rbp)
  402c33:	c6 45 02 00          	movb   $0x0,0x2(%rbp)
  402c37:	b8 00 00 00 00       	mov    $0x0,%eax
  402c3c:	48 83 c4 18          	add    $0x18,%rsp
  402c40:	5b                   	pop    %rbx
  402c41:	5d                   	pop    %rbp
  402c42:	c3                   	retq   

0000000000402c43 <driver_post>:
  402c43:	53                   	push   %rbx
  402c44:	48 83 ec 10          	sub    $0x10,%rsp
  402c48:	4c 89 cb             	mov    %r9,%rbx
  402c4b:	45 85 c0             	test   %r8d,%r8d
  402c4e:	74 22                	je     402c72 <driver_post+0x2f>
  402c50:	48 89 ce             	mov    %rcx,%rsi
  402c53:	bf 65 36 40 00       	mov    $0x403665,%edi
  402c58:	b8 00 00 00 00       	mov    $0x0,%eax
  402c5d:	e8 1e e0 ff ff       	callq  400c80 <printf@plt>
  402c62:	66 c7 03 4f 4b       	movw   $0x4b4f,(%rbx)
  402c67:	c6 43 02 00          	movb   $0x0,0x2(%rbx)
  402c6b:	b8 00 00 00 00       	mov    $0x0,%eax
  402c70:	eb 39                	jmp    402cab <driver_post+0x68>
  402c72:	48 85 ff             	test   %rdi,%rdi
  402c75:	74 26                	je     402c9d <driver_post+0x5a>
  402c77:	80 3f 00             	cmpb   $0x0,(%rdi)
  402c7a:	74 21                	je     402c9d <driver_post+0x5a>
  402c7c:	4c 89 0c 24          	mov    %r9,(%rsp)
  402c80:	49 89 c9             	mov    %rcx,%r9
  402c83:	49 89 d0             	mov    %rdx,%r8
  402c86:	48 89 f9             	mov    %rdi,%rcx
  402c89:	48 89 f2             	mov    %rsi,%rdx
  402c8c:	be 26 47 00 00       	mov    $0x4726,%esi
  402c91:	bf 40 31 40 00       	mov    $0x403140,%edi
  402c96:	e8 9c f6 ff ff       	callq  402337 <submitr>
  402c9b:	eb 0e                	jmp    402cab <driver_post+0x68>
  402c9d:	66 c7 03 4f 4b       	movw   $0x4b4f,(%rbx)
  402ca2:	c6 43 02 00          	movb   $0x0,0x2(%rbx)
  402ca6:	b8 00 00 00 00       	mov    $0x0,%eax
  402cab:	48 83 c4 10          	add    $0x10,%rsp
  402caf:	5b                   	pop    %rbx
  402cb0:	c3                   	retq   

0000000000402cb1 <check>:
  402cb1:	89 f8                	mov    %edi,%eax
  402cb3:	c1 e8 1c             	shr    $0x1c,%eax
  402cb6:	85 c0                	test   %eax,%eax
  402cb8:	74 1d                	je     402cd7 <check+0x26>
  402cba:	b9 00 00 00 00       	mov    $0x0,%ecx
  402cbf:	eb 0b                	jmp    402ccc <check+0x1b>
  402cc1:	89 f8                	mov    %edi,%eax
  402cc3:	d3 e8                	shr    %cl,%eax
  402cc5:	3c 0a                	cmp    $0xa,%al
  402cc7:	74 14                	je     402cdd <check+0x2c>
  402cc9:	83 c1 08             	add    $0x8,%ecx
  402ccc:	83 f9 1f             	cmp    $0x1f,%ecx
  402ccf:	7e f0                	jle    402cc1 <check+0x10>
  402cd1:	b8 01 00 00 00       	mov    $0x1,%eax
  402cd6:	c3                   	retq   
  402cd7:	b8 00 00 00 00       	mov    $0x0,%eax
  402cdc:	c3                   	retq   
  402cdd:	b8 00 00 00 00       	mov    $0x0,%eax
  402ce2:	c3                   	retq   

0000000000402ce3 <gencookie>:
  402ce3:	53                   	push   %rbx
  402ce4:	83 c7 01             	add    $0x1,%edi
  402ce7:	e8 34 df ff ff       	callq  400c20 <srandom@plt>
  402cec:	e8 5f e0 ff ff       	callq  400d50 <random@plt>
  402cf1:	89 c3                	mov    %eax,%ebx
  402cf3:	89 c7                	mov    %eax,%edi
  402cf5:	e8 b7 ff ff ff       	callq  402cb1 <check>
  402cfa:	85 c0                	test   %eax,%eax
  402cfc:	74 ee                	je     402cec <gencookie+0x9>
  402cfe:	89 d8                	mov    %ebx,%eax
  402d00:	5b                   	pop    %rbx
  402d01:	c3                   	retq   
  402d02:	66 2e 0f 1f 84 00 00 	nopw   %cs:0x0(%rax,%rax,1)
  402d09:	00 00 00 
  402d0c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000402d10 <__libc_csu_init>:
  402d10:	41 57                	push   %r15
  402d12:	41 89 ff             	mov    %edi,%r15d
  402d15:	41 56                	push   %r14
  402d17:	49 89 f6             	mov    %rsi,%r14
  402d1a:	41 55                	push   %r13
  402d1c:	49 89 d5             	mov    %rdx,%r13
  402d1f:	41 54                	push   %r12
  402d21:	4c 8d 25 e8 20 20 00 	lea    0x2020e8(%rip),%r12        # 604e10 <__frame_dummy_init_array_entry>
  402d28:	55                   	push   %rbp
  402d29:	48 8d 2d e8 20 20 00 	lea    0x2020e8(%rip),%rbp        # 604e18 <__init_array_end>
  402d30:	53                   	push   %rbx
  402d31:	4c 29 e5             	sub    %r12,%rbp
  402d34:	31 db                	xor    %ebx,%ebx
  402d36:	48 c1 fd 03          	sar    $0x3,%rbp
  402d3a:	48 83 ec 08          	sub    $0x8,%rsp
  402d3e:	e8 85 de ff ff       	callq  400bc8 <_init>
  402d43:	48 85 ed             	test   %rbp,%rbp
  402d46:	74 1e                	je     402d66 <__libc_csu_init+0x56>
  402d48:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  402d4f:	00 
  402d50:	4c 89 ea             	mov    %r13,%rdx
  402d53:	4c 89 f6             	mov    %r14,%rsi
  402d56:	44 89 ff             	mov    %r15d,%edi
  402d59:	41 ff 14 dc          	callq  *(%r12,%rbx,8)
  402d5d:	48 83 c3 01          	add    $0x1,%rbx
  402d61:	48 39 eb             	cmp    %rbp,%rbx
  402d64:	75 ea                	jne    402d50 <__libc_csu_init+0x40>
  402d66:	48 83 c4 08          	add    $0x8,%rsp
  402d6a:	5b                   	pop    %rbx
  402d6b:	5d                   	pop    %rbp
  402d6c:	41 5c                	pop    %r12
  402d6e:	41 5d                	pop    %r13
  402d70:	41 5e                	pop    %r14
  402d72:	41 5f                	pop    %r15
  402d74:	c3                   	retq   
  402d75:	90                   	nop
  402d76:	66 2e 0f 1f 84 00 00 	nopw   %cs:0x0(%rax,%rax,1)
  402d7d:	00 00 00 

0000000000402d80 <__libc_csu_fini>:
  402d80:	f3 c3                	repz retq 

Disassembly of section .fini:

0000000000402d84 <_fini>:
  402d84:	48 83 ec 08          	sub    $0x8,%rsp
  402d88:	48 83 c4 08          	add    $0x8,%rsp
  402d8c:	c3                   	retq   
