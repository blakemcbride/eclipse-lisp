/* Generated by Eclipse Common Lisp 1.1-HOSTED on cad0.
   Monday the fourth of May, 1998, 10:48:11 am CDT. */
#include <eclipse.h>

clObject clCanonicalizeClass(clProto), clCar(clProto), clCdr(clProto),
  clEndp(clProto), clList(clProto), clMapcar(clProto),
  clMissingArgs(clProto), clPrint(clProto), clRplacd(clProto);

extern clObject clCANONICALIZE_CLASS, clCM, clCMC,
  clCOMPUTE_APPLICABLE_METHODS,
  clCOMPUTE_APPLICABLE_METHODS_USING_CLASSES, clDELETE_METHOD,
  clFIND_METHOD, clREMOVE_METHOD;

static clObject I_1, I_2;

clObject clCm clVdecl(_ap)
{ clObject name, args;
  { clBeginParse(_ap);
    clSetq(name,
           (_clVp(_ap) ? clVpop(_ap) : clMissingArgs(I_1, clEOA)));
    clSetq(args, clVargs(_ap));
    clEndParse(_ap); }
  { clObject L_0;
    clSetq(L_0, clFdefinition(name, clEOA));
    return(clFuncallFunction(clSymbolFunctionValue(clCOMPUTE_APPLICABLE_METHODS),
                             L_0,
                             args,
                             clEOA)); } }

clObject clCmc clVdecl(_ap)
{ clObject name, classes;
  { clBeginParse(_ap);
    clSetq(name,
           (_clVp(_ap) ? clVpop(_ap) : clMissingArgs(I_1, clEOA)));
    clSetq(classes, clVargs(_ap));
    clEndParse(_ap); }
  { clObject L_1, L_0;
    clSetq(L_0, clFdefinition(name, clEOA));
    { clObject CL_class, L_g0;
      clSetq(CL_class, clNIL);
      clSetq(L_g0, classes);
      { clObject L_g1, L_g2;
        clSetq(L_g1, clList(clNIL, clEOA));
        clSetq(L_g2, L_g1);
        clLabel(NEXT_LOOP);
        if (clTrue(clEndp(L_g0, clEOA))) goto END_LOOP;
        clSetq(CL_class, clCar(L_g0, clEOA));
        { clObject L_0__R1;
          clSetq(L_0__R1, L_g0);
          clSetq(L_g0, clCdr(L_0__R1, clEOA)); }
        { clObject L_1__R1, L_0__R1;
          clSetq(L_0__R1, L_g2);
          { clObject L_0__R2;
            { clObject L_0__R3;
              clSetq(L_0__R3, CL_class);
              clSetq(L_0__R2,
                     clCanonicalizeClass(L_0__R3, clT, clEOA)); }
            clSetq(L_1__R1, clSetq(L_g2, clList(L_0__R2, clEOA))); }
          clRplacd(L_0__R1, L_1__R1, clEOA); }
        goto NEXT_LOOP;
        clLabel(END_LOOP);
        clSetq(L_1, clCdr(L_g1, clEOA));
        clLocalReturn(NIL);
        clSetq(L_1, clNIL);
        clBlockEnd(NIL); } }
    return(clFuncallFunction(clSymbolFunctionValue(clCOMPUTE_APPLICABLE_METHODS_USING_CLASSES),
                             L_0,
                             L_1,
                             clEOA)); } }

clObject clDeleteMethod clVdecl(_ap)
{ clObject name, qualifiers, classes;
  { clBeginParse(_ap);
    clSetq(name,
           (_clVp(_ap) ? clVpop(_ap) : clMissingArgs(I_1, clEOA)));
    clSetq(qualifiers,
           (_clVp(_ap) ? clVpop(_ap) : clMissingArgs(I_2, clEOA)));
    clSetq(classes, clVargs(_ap));
    clEndParse(_ap); }
  { clObject gf, method;
    clSetq(gf, clFdefinition(name, clEOA));
    { clObject L_2;
      { clObject L_0__R1;
        clSetq(L_0__R1, clSymbolFunctionValue(clCANONICALIZE_CLASS));
        clSetq(L_2, clMapcar(L_0__R1, classes, clEOA)); }
      clSetq(method,
             clFuncallFunction(clSymbolFunctionValue(clFIND_METHOD),
                               gf,
                               qualifiers,
                               L_2,
                               clEOA)); }
    clPrint(method, clEOA);
    return(clFuncallFunction(clSymbolFunctionValue(clREMOVE_METHOD),
                             gf,
                             method,
                             clEOA)); } }

void clInitTemp __P((void)) 
{ clDbind(clstarPACKAGEstar);
  clDbind(clstarREADTABLEstar);
  clDbind(clstarLOAD_TRUENAMEstar);
  clDbind(clstarLOAD_PATHNAMEstar);
  clSetq(I_1, clIntFixnum(1));
  clSetq(I_2, clIntFixnum(2));

  clSetSymbolFunctionValue(clCM, clMakeClosure(0, clCm, clNULL_HOOK));
  (void) clCM;
  clSetSymbolFunctionValue(clCMC,
                           clMakeClosure(0, clCmc, clNULL_HOOK));
  (void) clCMC;
  clSetSymbolFunctionValue(clDELETE_METHOD,
                           clMakeClosure(0,
                                         clDeleteMethod,
                                         clNULL_HOOK));
  (void) clDELETE_METHOD;
  clUnwind(4); }