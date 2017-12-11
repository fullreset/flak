/* 
  flak 
 
  Copyright © 2016 Q2 GAMES

  Introduction
  
    A tiny, eager, unfinished core functional language with the following design goals:

    X native JIT code generation (x86) -- amd64 or other CPUs are relatively easy to add (see flak_x86.h)
    X a high level of portability, and a tiny footprint
    / type inference, pattern matching, and abstract data types
    / suitability as a compiler target
    / built-in type inference with [some] support for type classes
    - generate native executables (future)
    - portable object file via AST dump (future)

    [ X == done, / == partial, - == pending ]

  Use Cases

    - Embeddable scripting language, or JIT compiler for other languages via translation.
    - Easy use of native system libraries from the command line (eg: ?? .)

    But you should probably finish it first. ^^

  Requirements
  
    GC providing (at minimum) a suitable malloc replacement -- see GC Configuration below. 
    [Using malloc/valloc is only acceptable for small short-lived programs.]

    A functioning C++ compiler.

    Note: Implementation language is largely C99, but uses some basic C++ features. (This note is probably out of date.)
 
  Building
  
    g++ -I../../mesongc/ -m32 -o flak flak.cc

    Note: -fopenmp is (possibly) required by mesongc.
    
  Running
  
    flak -h                             # basic help
    flak -t                             # sanity test
    flak -x somefile                    # compile/run from somefile
    echo '_=((9*9*9)+271)*10' | flak -  # compile/rum from stdin

  License
  
    BSD License 

    Copyright © 2016 Q2 GAMES

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated 
    documentation files (the “Software”), to deal in the Software without restriction, including without limitation 
    the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, 
    and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions 
    of the Software.

    THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED 
    TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL 
    THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF 
    CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER 
    DEALINGS IN THE SOFTWARE.  

*/

#if !defined(__FLAK_H__)
#define __FLAK_H__

/* GC Configuration   
   alloc_page is used internally to allocate page aligned blocks (for code generation, mainly.)
*/

#if 0  
#  include "mesongc.h"
#  define alloc_init()    GC_init()
#  define alloc(n)        GC_malloc(n)
#  define alloc_atomic(n) GC_malloc_atomic(n)
#  define alloc_page(n)   GC_malloc_atomic(n)
#else
#  define alloc_init()    fputs("warning: gc disabled\n",stderr)
#  define alloc(n)        malloc(n)
#  define alloc_atomic(n) malloc(n)
#  define alloc_page(n)   valloc(n)
#endif

/* getopt -- plan9-ish 
*/

#define GETARG    do { int _AC=argc,_AI=1; for(;_AI<argc;_AI++,_AC--) { if (argv[_AI][0]=='-' && argv[_AI][1]) switch (argv[_AI][1]) {
#define AFLAG     (_AI<argc?argv[_AI]:"")
#define APARAM    (++_AI<argc?argv[_AI]:"")
#define ENDARG    } } } while(0);


/* Compiler Types and Defines
*/

typedef float          real;
typedef const char*    cstr;
typedef unsigned int   uint;
typedef unsigned char* code;
typedef uint32_t (*func) ();
// const int Nil = 0; // results in too many warnings. :)
#define Nil 0

#define public_api extern "C"
#define api static inline

/* Optimizer Routines
*/

// public_api Ast optimize (Ast a);

#endif // __FLAK_H__