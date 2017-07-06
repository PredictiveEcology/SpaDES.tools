#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _SpaDES_tools_duplicatedInt(SEXP);
extern SEXP _SpaDES_tools_pointDistance2(SEXP, SEXP);
extern SEXP _SpaDES_tools_pointDistance3(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _SpaDES_tools_runifC(SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"_SpaDES_tools_duplicatedInt",  (DL_FUNC) &_SpaDES_tools_duplicatedInt,  1},
  {"_SpaDES_tools_pointDistance2", (DL_FUNC) &_SpaDES_tools_pointDistance2, 2},
  {"_SpaDES_tools_pointDistance3", (DL_FUNC) &_SpaDES_tools_pointDistance3, 5},
  {"_SpaDES_tools_runifC",         (DL_FUNC) &_SpaDES_tools_runifC,         1},
  {NULL, NULL, 0}
};

void R_init_SpaDES_tools(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
