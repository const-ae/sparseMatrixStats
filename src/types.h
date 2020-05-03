#ifndef types_h
#define types_h


#include <Rinternals.h> /* R_xlen_t, ... */





/* As in <R>/src/include/Defn.h */
#ifdef HAVE_LONG_DOUBLE
#define LDOUBLE long double
#else
#define LDOUBLE double
#endif


/* Macro to check for user interrupts every 2^20 iteration */
#define R_CHECK_USER_INTERRUPT(i) if (i % 1048576 == 0) R_CheckUserInterrupt()



#endif /* types_h*/
