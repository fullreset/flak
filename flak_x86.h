/* 
  flak 

  Copyright © 2016 Q2 GAMES
  See flak.h for instructions and license.  

  References

    http://www.cs.virginia.edu/~evans/cs216/guides/x86.html#calling
    http://www.swansontec.com/sregisters.html
    http://lists.gnu.org/archive/html/lightning/2011-06/msg00000.html
   
*/

#if !defined(__FLAK_X86_H__)
#define __FLAK_X86_H__

typedef enum X86Reg32E { EAX = 0, ECX, EDX, EBX, ESP, EBP, ESI, EDI } X86Reg32;
typedef enum X86Reg8E  { AL  = 0, CL,  DL,  BL,  AH,  CH,  DH,  BH  } X86Reg8;

#define REG0             EAX
#define REG1             ECX
#define REG2             __REG2_NOT_AVAILABLE__

#define e8(v)          (ep=(*ep=((v)&0xFF),(char*)ep+1))
#define e16(v)         e8(v), e8((v)>>8)
#define e32(v)         e16((v)), e16((v)>>16)

#define x86inclr(r)    e8(0x40+(r))
#define x86declr(r)    e8(0x48+(r))
#define x86addln8(n,r) e16(0xC083 + ((r)<<8)), e8(n)
#define x86subln8(n,r) e16(0xE883 + ((r)<<8)), e8(n)
#define x86addlr(s,d)  e16(0xC001 + ((d)<<8) + ((s)<<11))
#define x86sublr(s,d)  e16(0xC029 + ((d)<<8) + ((s)<<11))
#define x86cmplr(s,d)  e16(0xC039 + ((d)<<8) + ((s)<<11))
#define x86cmpl(n,d)   e8(0x3B), e8(0x05+(d<<3)), e32(n)

#define x86setl(r)     e8(0x0F), e8(0x9C), e8(0xC0+(r))
#define x86setle(r)    e8(0x0F), e8(0x9E), e8(0xC0+(r))
#define x86setg(r)     e8(0x0F), e8(0x9f), e8(0xC0+(r))
#define x86setge(r)    e8(0x0F), e8(0x9d), e8(0xC0+(r))
#define x86sete(r)     e8(0x0F), e8(0x94), e8(0xC0+(r))
#define x86setne(r)    e8(0x0F), e8(0x95), e8(0xC0+(r))

#define x86call(a)     x86callrel((char*)a-(char*)ep-4)
#define x86callrel(a)  e8(0xE8), e32(a)
#define x86movln(n,d)  e8(0xB8 + (d)), e32((n))
#define x86movlr(s,d)  e16(0xC089 + ((d)<<8) + ((s)<<11))
#define x86nop()       e8(0x90)
#define x86push32(a)   e8(0x68), e32(a)
#define x86pushr(r)    e8(0x50 + (r))
#define x86popr(r)     e8(0x58 + (r))
#define x86ret()       e8(0xC3)

#define x86addln(n,r)  ( (r==EAX) ? e8(0x05) : e16(0xC081+((r)<<8)) ), e32(n)

#define x86mullr(s,d)  e8(0x0F),e8(0xAF),e8(0xC0+(s)+((d)<<3))


/*

  Notes
    
  - OSX requires 16bytes stack alignment at call boundaries.  
  - { Function(name); [ ... ]; End(); } -- returns the value in REG0
  - REG1 is user saved
  - { BeginCall(nargs); [ Push(); ... ]  Call(f); EndCall(); }
  - Order of operands is regularized for sanity (eg sub a b = a-b )
  
*/

static int __x86stack = 4; 

#define __x86stack16(n)  ( ((n*4+__x86stack)%16)!=0 ? (x86addln( -(16-((n*4+__x86stack)%16)),ESP), n*4+(16-((n*4+__x86stack)%16)) ) : (n*4) )
#define __x86stack0(n)   x86addln(n,ESP)

#define __x86align16()   ( ((int)ep)&15? ep+=(16-((int)ep)&15) : 0 )

#define __x86prologue()  x86pushr(EBP), x86movlr(ESP,EBP), x86pushr(EBX), x86pushr(ECX), x86pushr(EDX)
#define __x86epilogue()  x86popr(EDX), x86popr(ECX), x86popr(EBX), x86movlr(EBP,ESP), x86popr(EBP)

/*
  This is the small generic portion all platforms must support -- for now.

*/

#define Function(f)      __x86align16(); func f = ((func)ep); { int __x86stack0 = __x86stack; __x86stack=4; __x86prologue();
#define Reserve(n)       (__x86stack+=(n*4))
#define End()            __x86epilogue(); x86ret(); __x86stack = __x86stack0; }

#define BeginCall(n)     { int __x86stack_base=__x86stack; int __x86stack_adj=__x86stack16(n);
#define Call(f)          x86call(f)
#define EndCall()        __x86stack=__x86stack_base; __x86stack0(__x86stack_adj); }

#define Call0(f)         BeginCall(0); Call(f); EndCall(); 
#define Call1(f,a)       BeginCall(1); Push(a); Call(f); EndCall(); 
#define Call2(f,a,b)     BeginCall(2); Push(b); Push(a); Call(f); EndCall(); 
#define Call3(f,a,b,c)   BeginCall(3); Push(c); Push(b); Push(a); Call(f); EndCall(); 
#define Call4(f,a,b,c,d) BeginCall(4); Push(d); Push(c); Push(b); Push(a); Call(f); EndCall(); 

#define Load(n,r)        x86movln(n,r)
#define Move(s,d)        x86movlr(s,d)
#define Push(r)          (__x86stack+=4,x86pushr(r))
#define Pop(r)           (__x86stack-=4,x86popr(r))

#define IAdd(d,s)        x86addlr(s,d)
#define ISub(d,s)        x86sublr(s,d)
#define IMul(d,s)        x86mullr(s,d)

#define IEq(d,s)         x86cmplr(s,d); x86sete(REG0);
#define ILt(d,s)         x86cmplr(s,d); x86setl(REG0);
#define IGt(d,s)         x86cmplr(s,d); x86setg(REG0);
#define ILEq(d,s)        x86cmplr(s,d); x86setle(REG0);
#define IGEq(d,s)        x86cmplr(s,d); x86setge(REG0);
#define INEq(d,s)        x86cmplr(s,d); x86setne(REG0);

#endif // __FLAK_X86_H__
