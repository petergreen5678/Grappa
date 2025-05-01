#include <R_ext/RS.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Fortran calls */
extern void F77_NAME(mcwh)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *,
                              void *, void *, void *, void *);
extern void F77_NAME(mcs)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *,
                              void *, void *, void *, void *);
extern void F77_NAME(trav)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *,
                              void *);
extern void F77_NAME(dopass)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *,
                              void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(pass)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *,
                              void *, void *, void *, void *, void *, void *, void *, void *, void *, void *,
                              void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(setup)(void *, void *, void *, void *, void *, void *);
extern void F77_NAME(init)(void *, void *, void *, void *, void *, void *);
extern void F77_NAME(setq)(void *, void *, void *, void *, void *);

static const R_FortranMethodDef FortranEntries[] = {
    {"mcwh", (DL_FUNC) &F77_NAME(mcwh), 14},
    {"mcs", (DL_FUNC) &F77_NAME(mcs), 14},
    {"trav", (DL_FUNC) &F77_NAME(trav), 11},
    {"dopass", (DL_FUNC) &F77_NAME(dopass), 20},
    {"pass", (DL_FUNC) &F77_NAME(pass), 29},
    {"setup", (DL_FUNC) &F77_NAME(setup), 6},
    {"init", (DL_FUNC) &F77_NAME(init), 6},
    {"setq", (DL_FUNC) &F77_NAME(setq), 5},
    {NULL, NULL, 0}
};

void R_init_Grappa(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, NULL, FortranEntries, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
