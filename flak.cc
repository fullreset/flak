/*
  flak   
  Copyright © 2016 Q2 GAMES
  See flak.h for instructions and licensing.  
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h> 
#include <assert.h>
#include <ctype.h>
#include <dlfcn.h>
#include <fcntl.h>
#include <math.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <unistd.h>
#include "flak.h"

// #define BE_TTY_ENABLED
// #include "ttyhack.h"

/* Compiler Target
*/

#if  (defined(__arm__) || defined(__thumb__))
#  include "flak_arm.h" // Incomplete, and never tested. ^^
#else
#  include "flak_x86.h"
#endif

/* Global Structures
*/

static cstr keywords[] = { "and", "case", "class", "data", "else", "extern", "fn", "import", "instance", "in", "let", "module", "newtype", "nil", "of", "or", "type", Nil };

typedef enum ErrE   { EError=-256, ESyntax, EStringEOL, EType, ENotImpl, ENotSup, EInternal, EIO, EProt, EArgs, ELink, EDup, EMax } ErrTag;
typedef enum AstE   { ANil=512, AAny, ABool, AInt, AReal, AStr, AExt, ATuple, AList, AVar, ADecl, ACon, AAbs, AApp, ABinop, AUnop, ACase, AData, AType, ALet, ATree, AExprList, AMax } AstTag;
typedef enum TokE   { TEof=256, TUnit, TInt, TReal, TStr, TSym, TCon, TExt, TLEQ, TGEQ, TNEQ, TSHL, TSHR, TARR, TMax } TokTag;
typedef enum TypeE  { TCNil=0, TCAny, TCUnit, TCBool, TCInt, TCReal, TCStr, TCApp, TCMax } TypeTag;

typedef struct TypeS *Type;  // too simplistic; and why (or why not) kludge more into Ast? :)
struct TypeS { TypeTag tag; };

api cstr type_show (Type t);

static Type nilt    = Nil; // See type_init.
static Type anyt    = Nil; 
static Type unitt   = Nil;
static Type boolt   = Nil;
static Type intt    = Nil;
static Type realt   = Nil;
static Type stringt = Nil;

static int opt_verbose = 0;
static const real zerof = 0.;

typedef struct AstS *Ast;  
struct AstS {
  struct { AstTag tag; Type t; };
  struct { union { Ast exp; uint op; cstr name; }; union { Ast left; Ast args; }; Ast right; };
  struct { cstr srcfile; uint srcline; uint srcchar; cstr src; };
};

api void print_real(FILE *fs,real f) { if (floor(f)==f) fprintf(fs,"%g.",f); else fprintf(fs,"%g",f); }


/* Error Handling

   Very basic for now.
*/

static uint error_count = 0;

static struct ErrS { ErrTag id; cstr msg; } emsgs[] = { 
  { EError,     "unknown error" },  { ESyntax,   "syntax error" },    { EType, "type error" },       { ENotImpl, "not implemented" },  
  { ENotSup,    "not supported" },  { EInternal, "internal error" },  { EIO,   "io error" },         { EProt,    "protection fault" },   
  { EStringEOL, "unterminated string or character constant" },        { EArgs, "invalid arguments"}, { ELink,    "link error"},
  { EDup,       "duplicate"}
};

static cstr tokfile = "stdin";
static uint tokline = 1;
static uint tokchar = 1;
static cstr b       = 0;

api cstr  _errstr (int id)      { for (int i=0;emsgs[i].id;i++) if (emsgs[i].id==id) return emsgs[i].msg; return _errstr(EError); }
api void  _error (int e, int l, Ast a=Nil) { 
  error_count ++;
  if (a) fprintf(stderr,"error: %s:%i,%i: %s (E%i)\n",a->srcfile,a->srcline,(uint)a->src-a->srcchar,_errstr(e),l);
  else   fprintf(stderr,"error: %s:%i,%i: %s (E%i)\n",tokfile,tokline,(uint)b-tokchar,_errstr(e),l); 
}
#define   error(e)              (_error(e,__LINE__),exit(-e))

/* AST Helpers
*/

api Ast  _cons  ()                    { Ast a = (Ast)alloc(sizeof(AstS)); a->srcfile = tokfile; a->srcline = tokline; a->srcchar = tokchar; a->src = b; return a; }
api Ast  cons   (AstTag t, Ast a=Nil, Ast b=Nil, Ast c=Nil) { Ast r = _cons(); r->tag=t; r->t=nilt; r->exp=a; r->left=b; r->right=c; return r; }
api Ast  tree   (Ast a, Ast l, Ast r) { return cons(ATree,a,l,r); }
api Ast  tuple  (Ast a, Ast b=Nil)    { return cons(ATuple,a,b); }

typedef  int    (*Ord)    (Ast,Ast);
typedef  void   (*IterF)  (Ast);
typedef  Ast    (*MapF)   (Ast);

api Ast  list   (Ast a, Ast b=Nil)    { return cons(AList,a,b); }
api Ast  head   (Ast a)               { return a?a->exp:Nil; }
api Ast  tail   (Ast a)               { return a->args; }
api Ast  aux    (Ast a)               { return a->right; }
api uint len    (Ast a)               { uint n = 0; while (a) { n++; a=tail(a); } return n; }
api Ast  nth    (Ast a, uint n)       { return n==0 ? a->exp : nth(tail(a),n-1); }
api Ast  insert (Ord p, Ast a, Ast l) { return l ? (p(a,l->exp) ? list(a,l) : list(l->exp,insert(p,a,tail(l)))) : list(a); }
api Ast  sort   (Ord p, Ast a)        { return a ? insert(p,a->exp,sort(p,tail(a))) : Nil; }
api Ast  take   (int n, Ast a)        { return n>0 ? list(a->exp,take(n-1,tail(a))) : Nil; }
api Ast  drop   (int n, Ast a)        { return n>0 ? drop(n-1,tail(a)) : a; }
api Ast  iter   (IterF f, Ast a)      { while (a) { f(a->exp); a=tail(a); }; return a; }
api Ast  map    (MapF f, Ast a)       { return a ? list(f(a->exp),map(f,tail(a))) : Nil; }
api Ast  rev    (Ast a)               { Ast b=Nil; while(a) { b=list(a,b); a=tail(a); }; return a; }
api cstr _strdup (cstr s)             { char* s2 = (char*) alloc_atomic(strlen(s)+1); memcpy(s2,s,strlen(s)+1); return s2; }

/* AST Printing
*/

#define   ast_error(a)          (fprintf(stderr,"\nerror: unexpected ast type %i 0x%x\n",a->tag,a->tag))

api Ast  ast_print (Ast a);
api void ast_print_type (Ast a) { }

api cstr ast_show_op (int op) {
  static char temp[4];
  switch (op) {
    case TGEQ: { return ">="; }
    case TLEQ: { return "<="; }
    case TNEQ: { return "<>"; }
    case TSHL: { return "<<"; }
    case TSHR: { return ">>"; }
    case TARR: { return "->"; }
    default:   { snprintf(temp,4,"%c",op); return temp; }
  }
}

api void ast_print_unop (Ast a) {
  switch (a->op) {
    case '(':     { fprintf(stderr,"%c",a->op); ast_print(a->left); fprintf(stderr,")"); break; }
    case '!':
    case '-':     
    case '~':     { fprintf(stderr,"%c",a->op); ast_print(a->left); break; }
    default:      { error(ENotImpl); break; }
  }
}
api void ast_print_seq   (Ast a) { while (a) { ast_print(a->exp); if (tail(a)) fprintf(stderr,", "); a=tail(a); } }
api void ast_print_binop (Ast a) { ast_print(a->left); fprintf(stderr," %s ",ast_show_op(a->op)); ast_print(a->right); }
api void ast_print_data  (Ast a) { }

api Ast  ast_print       (Ast a) {
  switch (a->tag) {
    case ADecl:   { ast_print(a->exp); fprintf(stderr," = "); ast_print(a->args); break; }    
    case ABool:   { fprintf(stderr,"%s",*(int*)&a->exp?"True":"False"); break; }
    case ACon:    
    case AVar:    { fprintf(stderr,"%s",a->name); break; }
    case AInt:    { fprintf(stderr,"%i",*(int*)&a->exp); break; }
    case AReal:   { print_real(stderr,*(real*)&a->exp); break; }
    case AStr:    { fprintf(stderr,"\"%s\"",a->name); break; }
    case AData:   { ast_print_data(a); break; }
    case AList:   { fprintf(stderr,"⟦"); ast_print_seq(a); fprintf(stderr,"⟧"); break; }
    case ATuple:  { fprintf(stderr,"⟪"); ast_print_seq(a); fprintf(stderr,"⟫"); break; }
    case ABinop:  { fprintf(stderr,opt_verbose>1?"⦗":""); ast_print_binop(a); ast_print_type(a); fprintf(stderr,opt_verbose>1?"⦘":""); break; }
    case AUnop:   { ast_print_unop(a); ast_print_type(a); break; }    
    case AApp:    { fprintf(stderr,"⦗"); ast_print(a->exp); fprintf(stderr," "); ast_print(a->args); fprintf(stderr,"⦘"); break; }
    case AExprList: { iter((IterF)ast_print,a); break; }
    default:      { ast_error(a); error(ENotImpl); break; }
  }
  return a;
}

api Ast ast_save (Ast a) { error(ENotImpl); return a; }
api Ast ast_load (Ast a) { error(ENotImpl); return a; }


/* Lexer
*/

// typedef struct LexBufS { cstr fname; struct LexBufS* next; } *LexBuf;⦗⦘⦅⦆⟨⟩⟪⟦⟧⦗⦘⟪⟫
// static LexBuf lexBuf  = Nil;

static TokTag tokval  = (TokTag)Nil;
static cstr   tokstr  = Nil;
static int    tokint  = Nil;
static real   tokreal = 0.0;

api char*  lex_dup (cstr a, cstr b) { char *r=(char*)alloc_atomic(b-a+1); memcpy((void*)r,(void*)a,b-a); r[b-a+1]=0; return r; }
api void   lex_skipw ()   { while (*b==' ' || *b=='\n' || *b=='\r' || *b=='\t') { if (*b=='\n') { tokchar=(uint)b+1; tokline++; }; b++; } }
api void   lex_skipl ()   { while (*b && *b!='\n') b++; if (*b=='\n') { tokchar=(uint)b+1; tokline++; }; b++; }
api TokTag lex_ident ()   { cstr p=b; while (*p && (isalnum(*p) || *p=='!' || *p=='?' || *p=='\'')) p++; tokstr=lex_dup(b,p); return (b=p,TSym); }
api TokTag lex_string ()  { b++; cstr p=b; while (*p && *p!='"') { if (*p=='\n') error(EStringEOL); p++; } tokstr=lex_dup(b,p); return (b=p+1,TStr); }
api TokTag lex_char ()    { b++; int i=0; while (*b && *b!='\'') { if (*b=='\n') error(EStringEOL); i=(i<<8)|*b++; } tokint=i; return (b++,TInt); }
api TokTag lex_real ()    { b++; tokreal=(real)tokint; real d=1.0; while (isalnum(*b)) { tokreal+=(*b++-'0')/(d*=10.); }; return TReal; }
api TokTag lex_int ()     { int n=0; while (*b && isdigit(*b)) { n=n*10+(*b++-'0'); }; return (tokint=n,TInt); }

api TokTag _lex () {
  lex_skipw();
  if (*b==0 || *b==4)     { return TEof; }
  if (*b>='0' && *b<='9') { lex_int(); if (*b=='.' || *b=='e' || *b=='E') return lex_real(); else return TInt; }
  if (*b>='a' && *b<='z') { return (lex_ident(),TSym); }
  if (*b=='_')            { return (tokstr=lex_dup(b,b+1),b++,TSym); } // special case
  if (*b>='A' && *b<='Z') { return (lex_ident(),TCon); }
  if (*b=='"')            { return lex_string(); }
  if (*b=='\'')           { return lex_char(); } 
  if (*b=='#')            { lex_skipl(); return _lex(); }
  if (*b=='-')            { b++; if (*b=='>') return (b++,TARR); if (*b=='-') return (lex_skipl(),_lex()); return (TokTag)*(b-1); }
  if (*b=='#')            { b++; return (lex_skipl(),_lex()); }
  if (*b=='>')            { b++; if (*b=='=') return (b++,TGEQ); if (*b=='>') return (b++,TSHR); return (TokTag)*(b-1); }
  if (*b=='<')            { b++; if (*b=='=') return (b++,TLEQ); if (*b=='>') return (b++,TNEQ); if (*b=='<') return (b++,TSHL); return (TokTag)*(b-1); }
  if (*b=='(')            { b++; if (*b==')') return (b++,TUnit); return (TokTag)*(b-1); }
  if (*b=='\n')           { return (b++,(TokTag)';'); }
  if (*b=='\0')           { return (b++,(TokTag)';'); }
  // if (*b=='\\')           { b++; if (*b=='n') return (b++,(TokTag)'\n'); if (*b=='r') return (b++,(TokTag)'\r'); if (*b=='t') return (b++,(TokTag)'\t'); error(ESyntax); }
  // if (*b=='\\')           { b++; if (*b=='r') return (TokTag)'\r'; if (*b=='n') return (TokTag)'\n'; if (*b=='t') return (TokTag)'\t'; _lex(); }
  return (TokTag)*b++;
}
public_api TokTag lex ()  { return (tokval=_lex()); }

api TokTag _match (int t) { if (t==tokval) lex(); else error(ESyntax); return (TokTag)t; }
#define    match(t)       ((t==tokval)?((TokTag)_match(t)):(error(ESyntax),(TokTag)0)) 
#define    expect(t)      ((t!=tokval)?(error(ESyntax),t):t);


/* Precedence

   Simple precedence sorting via rotation; credit to Wouter's example via http://strlen.com/bla-language

   Associativity not [yet] fully handled and/or implicit -- fixme. :)
*/


static int prec[TMax];

api void prec_init () { // FIXME
  memset(prec,0,sizeof(prec)); 
  prec['=']=prec['>']=prec['<']=prec[TLEQ]=prec[TGEQ]=prec[TNEQ]=1; 
  prec['+']=prec['-']=3; 
  prec['*']=prec['/']=prec['%']=5; 
  prec[TSHL]=prec[TSHR]=prec['&']=prec['|']=7;
  prec['`']=9;
  prec['~']=prec['^']=11; 
  // prec['$']=9;
}

api Ast prec_sort (Ast a) {     
  switch (a->tag) {
    case ABinop: {
      prec_sort(a->left);
      prec_sort(a->right);
      if (a->left->tag==ABinop) {
        if (prec[a->left->op]<prec[a->op]) {
          uint op = a->op; a->op = a->left->op; a->left->op = op;          
          Ast ll = a->left->left; a->left->left = a->left->right; a->left->right = a->right; a->right = a->left; a->left = ll;
          prec_sort(a);
        }
      }      
      break;
    }
    case AUnop: { prec_sort(a->left); break; }
    default:    { break; }
  }
  return a; 
}


/* Env

   Temporary, slow, and unused.
*/

Ast env = 0;
api Ast env_find0 (cstr n, Ast e) { e=head(e); while (e) { if (strcmp(head(head(e))->name,n)==0) return e; e=tail(e); }; return 0; }
api Ast env_find (cstr n)         { return env_find0(n,env); }
api Ast env_locate (cstr n)       { int l=0; Ast e=env,e2=0; while (e) { if ((e2=env_find0(n,e))) break; l++; e=tail(e); } return e2?tuple((Ast)l,e2):e2; }


/* Parse

   Generate basic AST from source code.
*/

api Ast parse_term ();
api Ast parse_exp ();
api Ast parse_var () { Ast e=cons(AVar,(Ast)tokstr); lex(); return e; }

api Ast parse_binary (TokTag op, Ast e) { Ast a = cons(ABinop,(Ast)op,e); a->right = parse_term(); return a; }

api Ast parse_pattern () {
  Ast e = 0;
  switch ((uint32_t)tokval) {
    case TSym:  { e=parse_var(); break; }
    case TCon:  { e=cons(ACon,(Ast)tokstr); lex(); break; }
    case '(':   { error(ENotImpl); break; } // Con, Tup or List
    default:    { error(ENotImpl); break; }
  }
  return e;
}

api Ast parse_abs () { error(ENotImpl); }

api Ast parse_term () { 
  Ast e = 0;
  switch ((uint32_t)tokval) {
    // case TUnit: { e=cons(AUnit); lex(); break; }
    case TInt:  { e=cons(AInt,(Ast)tokint); lex(); break; }
    case TReal: { e=cons(AReal,*(Ast*)&tokreal); lex(); break; }
    case TSym:  { e=parse_var(); break; }
    case TStr:  { e=cons(AStr,(Ast)_strdup(tokstr)); lex(); break; }
    case '\\':  { match('\\'); e=parse_abs(); break; }
    case '!':
    case '-':
    case '~':   { int op=tokval; match(tokval); e=cons(AUnop,(Ast)op); e->left=parse_term(); break; }
    case '(':   { match('('); e=cons(AUnop,(Ast)'('); e->left=parse_exp(); match(')'); break; }
    // case '$':     { e=parse_exp(); break; }
    default:    { error(ESyntax); break; }
  }
  return e;
}

api Ast parse_term_list () {
  if (tokval==TUnit) { match(TUnit); return 0; }
  if (tokval!=TEof && tokval!=';' && tokval!=')') {
    Ast t = parse_term();
    return list(t,parse_term_list());
  }
  return 0;  
}

api Ast parse_exp () {
  Ast e = parse_term();
  while (prec[tokval] && tokval!=TEof) {
    switch ((uint32_t)tokval) {
      case '+':  case '-':  case '*':  case '/':  
      case '%':  case '^':  case '=':  case '>': 
      case '<':  case '&':  case '|':  case TLEQ: 
      case TGEQ: case TNEQ: case TSHL: case TSHR: { e = parse_binary(match(tokval),e); break; }
      case '`': {
          Ast e0 = e; e = cons(AApp);   
          match('`'); expect(TSym); e->exp = cons(AVar,(Ast)_strdup(tokstr)); match(TSym);  
          e->args = list(e0,list(parse_term())); 
          break; 
        }
      default:  { printf("error: charstream: '%c' %i\n",tokval,tokval); error(ESyntax); break; }
    }
  }
  e = prec_sort(e);
  // applications ()
  switch (e->tag) {
    case AVar: { 
      e=cons(AApp,e,parse_term_list());
      break;
    }
    case ACon: {
      e=cons(AApp,e,parse_term_list());
      break;
    }
    default: break; // ignore all other things
  }
  return e;
}

api Ast parse_decl () {  
  Ast e = 0;
  switch (tokval) {
    case TSym: {
        Ast v = parse_var ();
        if (tokval=='=') { match('='); e = cons(ADecl,v,parse_exp()); } else error(ESyntax);
        if (!env_find(v->name)) { env->exp = list(tuple(v,e),env->exp); } else error(EDup);
        if (!env_find(v->name)) error(EInternal);
        break;
      }
    case TInt: 
    case TReal: 
    case TStr: e = parse_exp(); break; 
    case TEof: break;
    default: printf("tok: %i\n",(int)tokval); error(ESyntax);
  }
  // e = parse_exp();
  if (tokval!=TEof) { match(';'); }
  return e;
}

api Ast parse_toplevel () { 
  return parse_decl();       // what it should be: tuple(env,e);
}

public_api Ast parse (cstr c) { 
  const char* __b=b=c; tokchar=(uint)b; tokline=1; lex(); 
  Ast e = Nil, p = Nil;
  do {
	env = list(0,0);           // reset env (if any)
	e = parse_toplevel(); 
	if (e) p = cons(AExprList,e,p);
  } while (e);
  b=__b; 
  return head(p); 
}

/* Type Inference/Checking

   Type inference performed on original AST for slightly better error reporting.

   We just bubble up types. Simplistic, but ok.
   However, we know nothing about fucntion types, and just trust the user. Oops. :)
*/

#define  _tcerr(a,err) (_error(err,__LINE__,a),exit(-err))
#define  type_error(e,a,b)  { fprintf(stderr,"\nerror: "); ast_print(e); \
                              fprintf(stderr," :: %s <=> %s\n",type_show(a),type_show(b)); _tcerr(e,EType); }

api Type type (TypeTag t) { Type s=(Type)alloc(sizeof(TypeS)); s->tag=t; return s; }

api void type_init () { 
  nilt    = type(TCNil);  anyt = type(TCAny); unitt = type(TCUnit); 
  boolt   = type(TCBool); intt = type(TCInt); realt = type(TCReal); 
  stringt = type(TCStr); 
}

api Type tc (Ast a);
api Type unwind (Type t) { return t; }

api cstr type_show (Type t) {
  cstr s = "Undefined";
  switch (t->tag) {
    case TCNil:    { s = "Undefined"; break; }
    case TCAny:    { s = "Any"; break; }
    case TCUnit:   { s = "()"; break; }
    case TCBool:   { s = "Bool"; break; }
    case TCInt:    { s = "Int"; break; }
    case TCReal:   { s = "Real"; break; }
    case TCStr:    { s = "String"; break; }
    default:       { error(EInternal); break; }
  }
  return s;  
}

api Type tc_unify  (Ast e, Type a, Type b) {
  // a=unwind(a); b=unwind(b);
  if (a==b)    return a;
  if (a==anyt) return b;
  if (b==anyt) return a;
  type_error(e,a,b);  
}

api Type tc_unary  (Ast a) { return (a->t = tc(a->left)); }

api Type tc_binary (Ast a) { 
  switch ( a->op ) {
    case '=':  case '>':  case '<':
    case TLEQ: case TGEQ: case TNEQ: { return (tc_unify(a,tc(a->left),tc(a->right)), a->t = boolt); break; }
    default:                         { return (a->t = tc_unify(a,tc(a->left),tc(a->right))); break; }
  }
}

api Type tc (Ast a) {
  switch (a->tag) {
    case ABool:   { a->t = boolt; break; }
    case AInt:    { a->t = intt; break; }
    case AReal:   { a->t = realt; break; }
    case AStr:    { a->t = stringt; break; }
    case AVar:    { a->t = anyt; break; }                          // fixme
    case ACon:    { a->t = anyt; break; }                          // fixme
    case ADecl:   { a->t = tc(a->args); break; }
    case AUnop:   { a->t = tc_unary(a);  break; }
    case ABinop:  { a->t = tc_binary(a); break; }
    case AApp:    { a->t = intt; iter((IterF)tc,a->args); break; } // fixme ~ defaults to intt 
    case AExprList:  { a->t = anyt; break; } // fixme ~ temporarily using AList as AProg :) 
    default:      { ast_error(a); error(ENotImpl); break; }
  }  
  return a->t;
}

public_api Ast typecheck (Ast a) { if (a) tc(a); return a; }

/* Runtime Support

   Language support, builtins, and some helpers for simplicity and porting (eg real support, etc., that can vary 
   a lot betw platforms.)
*/

public_api int _clos (int size)       { return (int)alloc(size*sizeof(void*)); }
public_api int _alloc(int size)       { return (int)alloc(size); }

public_api int _iadd (int a, int b)   { return a+b; }
public_api int _isub (int a, int b)   { return a-b; }
public_api int _imul (int a, int b)   { return a*b; }
public_api int _idiv (int a, int b)   { return a/b; }

public_api int _ipow (int a, int b)   { return (int)powf(a,b); }
public_api int _imod (int a, int b)   { return a%b; }

public_api int _ishl (int a, int b)   { return a<<b; }
public_api int _ishr (int a, int b)   { return a>>b; }
public_api int _iand (int a, int b)   { return a&b; }
public_api int _ior  (int a, int b)   { return a|b; }
public_api int _inot (int a)          { return !a; }

public_api int _ieq  (int a, int b)   { return a==b; }
public_api int _ilt  (int a, int b)   { return a<b; }
public_api int _igt  (int a, int b)   { return a>b; } 
public_api int _ile  (int a, int b)   { return a<=b; }
public_api int _ige  (int a, int b)   { return a>=b; }
public_api int _ine  (int a, int b)   { return a!=b; }

public_api int _fadd (real a, real b) { a+=b; return *(int*)&a; }
public_api int _fsub (real a, real b) { a-=b; return *(int*)&a; }
public_api int _fmul (real a, real b) { a*=b; return *(int*)&a; }
public_api int _fdiv (real a, real b) { a/=b; return *(int*)&a; }

public_api int _fpow (real a, real b) { a = powf(a,b);  return *(int*)&a; }
public_api int _fmod (real a, real b) { a = fmodf(a,b); return *(int*)&a; }

public_api int _feq  (real a, real b) { return a==b; }
public_api int _flt  (real a, real b) { return a<b; }
public_api int _fgt  (real a, real b) { return a>b; } 
public_api int _fle  (real a, real b) { return a<=b; }
public_api int _fge  (real a, real b) { return a>=b; }
public_api int _fne  (real a, real b) { return a!=b; }

public_api int _nop  ()               { return 0; }

/* Optimize

   Basic built-in optimizations -- eg constant folding; very basic.

   Limitation example: if (mul (mul (int * func)) int) is encountered there is no optimization.  
   Asociativity is not a known concept here. :)    
*/

#define opt_error(a,e)  { ast_error(a); fprintf(stderr,"\nerror: "); ast_print(a); printf(" :: %s\n",type_show(a->t)); \
                          _error(e,__LINE__,a); exit(-e); }

public_api Ast optimize (Ast a);
api Ast opt_exp (Ast a);

api Ast opt_unary (Ast a) {
  a->left = opt_exp(a->left);
  switch (a->op) {
    case '(': {    
      switch (a->left->tag) {
        case AInt: case AReal: case AStr: { a=a->left; break; }        
        default: break;
      }
      break;
    }
    default: break;
  }
  return a;
}

api Ast opt_binary (Ast a) {
  a->left = opt_exp(a->left); a->right = opt_exp(a->right);
  if (a->left->tag==AInt && a->right->tag==AInt)   { 
    int f = *(int*)&a->left->exp; int g = *(int*)&a->right->exp;
    switch (a->op) {
      case '+':  { a = typecheck(cons(AInt,(Ast)(f+g))); break; }
      case '-':  { a = typecheck(cons(AInt,(Ast)(f-g))); break; }
      case '*':  { a = typecheck(cons(AInt,(Ast)(f*g))); break; }
      case '/':  { a = typecheck(cons(AInt,(Ast)(f/g))); break; }
      case '%':  { a = typecheck(cons(AInt,(Ast)_imod(f,g))); break; }
      case '^':  { a = typecheck(cons(AInt,(Ast)_ipow(f,g))); break; }
      case '>':  { a = typecheck(cons(ABool,(Ast)(f>g))); break; }
      case '<':  { a = typecheck(cons(ABool,(Ast)(f<g))); break; }
      case TGEQ: { a = typecheck(cons(ABool,(Ast)(f>=g))); break; }
      case TLEQ: { a = typecheck(cons(ABool,(Ast)(f<=g))); break; }
      case '=':  { a = typecheck(cons(ABool,(Ast)(f==g))); break; }
      case TSHL: { a = typecheck(cons(AInt,(Ast)_ishl(f,g))); break; } 
      case TSHR: { a = typecheck(cons(AInt,(Ast)_ishr(f,g))); break; }
      default:   { opt_error(a,ESyntax); }
    }
  } else if (a->left->tag==AReal && a->right->tag==AReal) { 
    float f = *(float*)&a->left->exp; float g = *(float*)&a->right->exp;
    switch (a->op) {
      case '+':  { f=f+g; a = typecheck(cons(AReal,(Ast)(*(int*)&f))); break; }
      case '-':  { f=f-g; a = typecheck(cons(AReal,(Ast)(*(int*)&f))); break; }
      case '*':  { f=f*g; a = typecheck(cons(AReal,(Ast)(*(int*)&f))); break; }
      case '/':  { f=f/g; a = typecheck(cons(AReal,(Ast)(*(int*)&f))); break; }
      case '%':  { int i=_fmod(f,g); a = typecheck(cons(AReal,(Ast)i)); break; }
      case '^':  { int i=_fpow(f,g); a = typecheck(cons(AReal,(Ast)i)); break; }
      case '>':  { a = typecheck(cons(ABool,(Ast)(f>g))); break; }
      case '<':  { a = typecheck(cons(ABool,(Ast)(f<g))); break; }
      case TGEQ: { a = typecheck(cons(ABool,(Ast)(f>=g))); break; }
      case TLEQ: { a = typecheck(cons(ABool,(Ast)(f<=g))); break; }
      case '=':  { a = typecheck(cons(ABool,(Ast)(f==g))); break; }
      default:   { opt_error(a,ESyntax); }
    }
  }
  return a;
}

api Ast opt_exp (Ast a) {
  switch (a->tag) {
    case ABool:   { break; }
    case AInt:    { break; }
    case AReal:   { break; }
    case AStr:    { break; }
    case ABinop:  { a->left = optimize(a->left); a->right = optimize(a->right); a = opt_binary(a); break; }
    case AUnop:   { a->left = optimize(a->left); a=opt_unary(a); break; }
    case AApp:    { a->args = map(optimize,a->args); break; }
    case AVar:    { break; }
    case ADecl:   { a->args = optimize(a->args); break; }
    case AExprList: { break; }
    default:      { opt_error(a,ENotImpl); }
  }  
  return a;
}

public_api Ast optimize (Ast a) { return a?opt_exp(a):a; }


/* Compile

   Forces minimum 16byte alignment on all platforms (as required by OSX), and assumes ALL values 32bit.
   Some glaring inefficiencies retained in the interest of simplicity and portability (see also Runtime Support.)

   We also use C calls into the "local library" when we don't need to -- for added simpliciy.
*/

#define compile_error(a,e)  { fprintf(stderr,"\nerror: "); ast_print(a); printf(" :: %s\n",type_show(a->t)); _error(e,__LINE__,a); exit(-e); }

public_api int _resolve(cstr extfn, int retries=0, int n=Nil) {         
  if (retries<3 && !(n=(int)dlsym(RTLD_DEFAULT,extfn)))  {
    char extfn2[128];
    extfn2[0]=0;
    strcat(extfn2,"_");
    strcat(extfn2+1,extfn); 
    return _resolve(extfn2,retries+1,Nil);
  }
  return n;
}

char* ep = 0;

api void compile_app (Ast a);
api void compile_binary (Ast a);
api void compile_term (Ast a);
api void compile_decl (Ast a);
api void compile_unary (Ast a);
api func compile_func (Ast a);

api void compile_decl (Ast a) {
  switch (a->tag) {
    case ADecl:   { compile_term(a->args); break; }
    default:      { compile_error(a,ENotImpl); }
  }
}

api void compile_term (Ast a) {
  switch (a->tag) {
    case ABool: case AInt: case AReal: case AStr:
                  { Load(*(int*)&a->exp,REG0); break; }
    case ABinop:  { compile_binary(a); break; }
    case AUnop:   { compile_unary(a); break; }
    case AApp:    { compile_app(a); break; }
    case ADecl:   { compile_decl(a); break; }
    case AVar:    { compile_error(a,ENotImpl); }    
    case AExprList: { iter(compile_term,a); break; }
    default:      { compile_error(a,ENotImpl); }
  }  
}

api void compile_unary (Ast a) {  
  switch (a->op) {
    case '(': { compile_term(a->left); break; }
    case '!': { 
      switch (unwind(a->t)->tag) {
        case TCBool:
        case TCInt:  { compile_term(a->left); Call1(_inot,REG0); break; }
        default:     { compile_error(a,ENotSup); } 
      }      
      break; 
    }
    case '-': case '~':
      switch (unwind(a->t)->tag) {
        case TCInt:  { Push(REG1); compile_term(a->left); Move(REG0,REG1); Load(0,REG0); ISub(REG0,REG1); Pop(REG1); break; }
        case TCReal: { Push(REG1); compile_term(a->left); Move(REG0,REG1); Load(*(int*)&zerof,REG0); Call2(_fsub,REG0,REG1); Pop(REG1); break; }
        default:     { compile_error(a,ENotSup); } 
      }
      break;
    default:  { compile_error(a,ENotImpl); }
  } 
} 

api void compile_binary_op_bool (Ast a) {
  switch (a->op) {            
    case '=':  { IEq(REG0,REG1); break; }
    case '<':  { ILt(REG0,REG1); break; }
    case '>':  { IGt(REG0,REG1); break; }
    case TLEQ: { ILEq(REG0,REG1); break; }
    case TGEQ: { IGEq(REG0,REG1); break; }
    case TNEQ: { INEq(REG0,REG1); break; }
    default:   { compile_error(a,ENotImpl); break; }
  }
}

api void compile_binary_op_int (Ast a) {
  switch (a->op) {
    case '+':  { IAdd(REG0,REG1); break; }
    case '-':  { ISub(REG0,REG1); break; }
    case '*':  { IMul(REG0,REG1); break; }
    case '/':  { Call2(_idiv,REG0,REG1); break; } 
    case '^':  { Call2(_ipow,REG0,REG1); break; }
    case '=':  { IEq(REG0,REG1); break; }
    case '<':  { ILt(REG0,REG1); break; }
    case '>':  { IGt(REG0,REG1); break; }
    case TLEQ: { ILEq(REG0,REG1); break; }
    case TGEQ: { IGEq(REG0,REG1); break; }
    case TNEQ: { INEq(REG0,REG1); break; }
    case TSHL: { Call2(_ishl,REG0,REG1); break; } 
    case TSHR: { Call2(_ishr,REG0,REG1); break; }
    case '%':  { Call2(_imod,REG0,REG1); break; }
    case '&':  { Call2(_iand,REG0,REG1); break; }
    case '|':  { Call2(_ior,REG0,REG1); break; }
    default:   { compile_error(a,ENotImpl); }
  }
}

api void compile_binary_op_real (Ast a) {
  switch (a->op) {
    case '+':  { Call2(_fadd,REG0,REG1); break; }
    case '-':  { Call2(_fsub,REG0,REG1); break; } 
    case '*':  { Call2(_fmul,REG0,REG1); break; } 
    case '/':  { Call2(_fdiv,REG0,REG1); break; } 
    case '^':  { Call2(_fpow,REG0,REG1); break; }
    case '=':  { Call2(_feq,REG0,REG1); break; }
    case '<':  { Call2(_flt,REG0,REG1); break; }
    case '>':  { Call2(_fgt,REG0,REG1); break; }
    case TLEQ: { Call2(_fle,REG0,REG1); break; }
    case TGEQ: { Call2(_fge,REG0,REG1); break; }
    case TNEQ: { Call2(_fne,REG0,REG1); break; }
    case '%':  { Call2(_fmod,REG0,REG1); break; }
    default:   { compile_error(a,ENotImpl); break; }
  }
}

api void compile_binary (Ast a) {
  Push(REG1);
  compile_term(a->right); Move(REG0,REG1); 
  compile_term(a->left);
  switch (unwind(a->left->t)->tag) {
    case TCBool: { compile_binary_op_bool(a); break; }
    case TCInt:  { compile_binary_op_int(a);  break; }
    case TCReal: { compile_binary_op_real(a); break; }
    default: { printf("err: type: %s\n",type_show(unwind(a->left->t))); compile_error(a,ENotSup); }
  }
  Pop(REG1);
}

api void compile_app (Ast a) { 
  int n = _resolve(a->exp->name); // search environment too ... fixme.
  if (!n) compile_error(a,ELink);
  switch (len(a->args)) {
    case 0: { Push(REG1); Call0(n); Pop(REG1); break; }
    case 1: { Push(REG1); compile_term(nth(a->args,0)); Call1(n,REG0); Pop(REG1); break; }
    case 2: {
      Push(REG1);
      compile_term(nth(a->args,1)); Move(REG0,REG1);
      compile_term(nth(a->args,0));
      Call2(n,REG0,REG1);
      Pop(REG1);
      break;
    }
    default: { compile_error(a,ENotImpl); break; }
  }
}

api func compile_func (Ast a) { 
  Function(f);
    compile_term(a); 
  End();
  return f;
}

api func compile_case (Ast a) { error(ENotImpl); }

static char *__ep;
public_api func compile (Ast a) { 
  if (!a) return Nil;
  ep = (char*)alloc_page(1024*16);
  __ep = ep;
  if (mprotect(ep,1024*16, PROT_READ|PROT_WRITE|PROT_EXEC )) error(EProt);
  func f = compile_func(a);
  // ep = __ep;
  return f; 
}

/* Tests

   Small sanity tests
*/

static struct TestS { cstr src; TypeTag t; int ival; real rval; } tsts[] = {   
  { "_ = \"hello, world\"",                           TCStr, (int)"hello, world", 0. }, 
  { "_ = 13*22-6/4+3*21-32*12-2+12^2+33-12*2^3+1<<4", TCInt, 59,  0. }, 
  { "_ = 5./2.+5.125*2.+2.^2.+10.%4.",                TCReal, 0, 18.75 }, 
  { "_ = 42*2=21*(8-4)",                              TCBool, 1,  0. },
  { "_ = 9*9>=9^3",                                   TCBool, 0,  0. },
  { "_ = 21./3.<=8.",                                 TCBool, 1,  0. },
  { "_ = (12+2)/2",                                   TCInt,  7,  0. },
  { "_ = 12 `iadd 2 `idiv 2",                         TCInt,  7,  0. },
  { "_ = idiv (iadd 12 2) 2",                         TCInt,  7,  0. },
  { "_ = printf \"hello there\"",                     TCInt, 11,  0. },
  { "_ = printf \"%i == 33\" (5*2*4/2+13)",           TCInt, 8,  0. },
  {"1 `iadd 41",                                      TCInt, 42, 0. },
  // { "data Tree a = Node a (Tree a) (Tree a)",     TCNil,  0,  0. },
  // { "(\\f=f 3 + f 4) (\\x=x*x)",                  TCInt,  25, 0. },
  { Nil, TCInt, Nil, Nil } 
};

api void result(Ast a,int rval) {  
  if (!a) return;
  switch (unwind(a->t)->tag) {      
    case TCBool:   { fprintf(stderr,"# [result]    %s :: Bool\n",rval?"True":"False"); break; }
    case TCInt:    { fprintf(stderr,"# [result]    %i :: Int\n",rval); break; }
    case TCReal:   { fprintf(stderr,"# [result]    "); print_real(stderr,*(real*)&rval); fprintf(stderr," :: Real\n"); break; } 
    case TCStr:    { fprintf(stderr,"# [result]    \"%s\" :: String\n",(char*)rval); break; }
    default:       { error(ENotImpl); break; }
  }
}

public_api int test () {
  Ast a = Nil; func f = Nil; int rval = 0;
  for (int i=0; tsts[i].src!=0; i++) {
    fprintf(stderr,"%s\n",tsts[i].src);
    if ( !(f = compile(a=optimize(typecheck(parse(tsts[i].src))))) ) error(EInternal);
    fprintf(stderr,"# "); ast_print(a); fprintf(stderr,"\n");
    rval = f();
    if (unwind(a->t)->tag!=tsts[i].t) error(EType);
    switch (unwind(a->t)->tag) {
      case TCBool:
        rval=rval?1:0;
        if (rval!=tsts[i].ival) fprintf(stderr,"!! [ERR] %s = %s <=> %s\n",tsts[i].src,rval?"True":"False",tsts[i].ival?"True":"False"); 
        else result(a,rval);
        break;
      case TCInt:    
        if (rval!=tsts[i].ival) fprintf(stderr,"!! [ERR] %s = %i <=> %i\n",tsts[i].src,rval,tsts[i].ival); 
        else result(a,rval);
        break;
      case TCReal:   
        if (*(real*)&rval!=tsts[i].rval) fprintf(stderr,"!! [ERR] %s = %g <=> %g\n",tsts[i].src,*(real*)&rval,tsts[i].rval); 
        else result(a,rval);
        break;
      case TCStr: 
        if (strcmp((char*)rval,(char*)tsts[i].ival)) fprintf(stderr,"!! [ERR] \"%s\" = %s <=> %s\n",tsts[i].src,(char*)rval,(char*)tsts[i].ival); 
        else result(a,rval);
        break;
      default:       
        error(ENotImpl);
    }
    
    puts("");        
  }
  return Nil;
}

/* Entry Point
*/

api void flak_init () { alloc_init(); prec_init(); type_init(); srandomdev(); setvbuf (stdout, NULL, _IOLBF, 0); }

int main (int argc, char **argv) {

  flak_init();
  dlopen(NULL,RTLD_NOW);
  const char *source = 0, *script = 0;
  int silent = 0;

  GETARG{
    case 'c': error(ENotImpl); 
    case 'q': silent = 1; break;
    case 't': return test();
    case 'x': source = APARAM; break;    
    case 'v': opt_verbose = atoi(APARAM); opt_verbose = opt_verbose>1?opt_verbose:1; break;  
    case 'h': 
    default:  return(fprintf(stderr, "usage: %s [-c] [-h] [-t] [-x source.fk] [-v n] [-]\n",argv[0]));
  }ENDARG ;

  int fd=0, n=0, rval=0;
  if (source && (tokfile=source) && (fd=open(source,O_RDONLY))==-1) error(EIO);
  while (1) {
    char *buf = (char*)alloc_atomic(1024*64);
    buf[n=read(fd,buf,1024*64-3)]='\n'; 
    if (n<=0) break;
    buf[n+1]=0; buf[n+2]=0;
    Ast po=parse(buf), oo=Nil;
    if (opt_verbose) { fprintf(stderr,"# [parsed]    "); ast_print(po); fprintf(stderr,"\n"); }
    func fn = compile(oo=optimize(typecheck(po)));
    if (fn) {
      rval = fn();
      if (opt_verbose) { 
        fprintf(stderr,"# [optimized] "); ast_print(oo); fprintf(stderr,"\n"); 
        fprintf(stderr,"# [generated] %i bytes\n",ep-__ep);  
      }
      if (!silent) result(oo,rval);
    }
    if (source && fd) { close(fd); return rval; }
  }
  return 0;
}

