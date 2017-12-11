
#if !defined(__TTYHACK_H__)
#define __TTYHACK_H__
 
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h> 
#include <sys/stat.h>

#if (defined(__arm__) || defined(__thumb__))
#define BE_TTY_ENABLED 0
#else
#define BE_TTY_ENABLED 1
#endif

/*
static inline bool term_interactivep() {
  struct stat stats;
  fstat(0, &stats);
  if (S_ISREG(stats.st_mode) && 
     !S_ISFIFO(stats.st_mode)) { puts("fifo"); return false;}
  return true;
}
*/
static int ttylevel = 0;

#if BE_TTY_ENABLED
static inline bool xterm_color(FILE* where) {
  static bool xc_ran = false;
  static bool xc_result = false;
  if (xc_ran) return xc_result;

  char *term = NULL;
  int tty = fileno(where); //
  // int tty = ttyslot()?ttyslot():1;
  // int tty = 1;
  xc_ran = true;

  if (!isatty (tty)) { return false; } 

  if ((term=getenv("TERM")) &&
      (strcasecmp(term,"xterm-color")==0 ||
       strcasecmp(term,"vt-100")==0 ||
       strcasecmp(term,"xterm")==0 ||
       strcasecmp(term,"ansi")==0)) {
    return xc_result = true;
  }
  if ( (term=ttyname(tty)) &&
      (strcasecmp(term,"xterm-color")==0 ||
       strcasecmp(term,"vt-100")==0 ||
       strcasecmp(term,"xterm")==0 ||
       strcasecmp(term,"ansi")==0)) {
    return xc_result = true;
  }

  return xc_result = false;
}
#else
static inline bool xterm_color(FILE* where) { return false ; }
#endif
#define TTYCLEAR(where)   { if (xterm_color(where)) fprintf(where,"%c[2J",0x1B); fprintf(where,"%c[21;1f",0x1B); }
#define TTYBOLD(where)    { ttylevel++; if (xterm_color(where)) fprintf(where,"%c[1m",0x1B); }
#define TTYULINE(where)   { ttylevel++; if (xterm_color(where)) fprintf(where,"%c[4m",0x1B); }
#define TTYRED(where)     { ttylevel++; if (xterm_color(where)) fprintf(where,"%c[31m",0x1B); }
#define TTYGREEN(where)   { ttylevel++; if (xterm_color(where)) fprintf(where,"%c[32m",0x1B); }
#define TTYYELLOW(where)  { ttylevel++; if (xterm_color(where)) fprintf(where,"%c[33m",0x1B); }
#define TTYBLUE(where)    { ttylevel++; if (xterm_color(where)) fprintf(where,"%c[34m",0x1B); }
#define TTYMAGENTA(where) { ttylevel++; if (xterm_color(where)) fprintf(where,"%c[35m",0x1B); }
#define TTYCYAN(where)    { ttylevel++; if (xterm_color(where)) fprintf(where,"%c[36m",0x1B); }
#define TTYWHITE(where)   { ttylevel++; if (xterm_color(where)) fprintf(where,"%c[37m",0x1B); }
#define TTYRESET(where)   { ttylevel--; if (ttylevel<=0 && xterm_color(where)) { fprintf(where,"%c[0m",0x1B); ttylevel=0; }; }

#endif