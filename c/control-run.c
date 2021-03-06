/* Generated by Eclipse Common Lisp 1.1-HOSTED on cad0.
   Tuesday the twenty-sixth of May, 1998, 3:33:24 pm CDT. */
#include <eclipse.h>

clObject clApply(clProto), clCar(clProto), clCddr(clProto),
  clCdr(clProto), clCharpSimpleBaseString __P((clCharp)),
  clEndp(clProto), clEq(clProto), clEql(clProto), clError(clProto),
  clExtraArgs(clProto), clFindBadKey(clProto), clGetf(clProto),
  clMakeKeyword(clProto), clMissingArgs(clProto), clNot(clProto),
  clSignalProgramError(clProto), clSymbolp(clProto);

extern clObject clCHECK_KEYS, clFIND_BAD_KEY, clKEY_NOT_ALLOWED,
  clMULTIPLE_APPEARANCE_ERROR, clPROGRAM_ERROR, clSIGNAL_PROGRAM_ERROR;

static clObject I_1, I_2, keyALL_OTHER_KEYS, keyALLOW_OTHER_KEYS,
  keyFORMAT_ARGUMENTS, keyFORMAT_CONTROL, STR_ALL_OTHER_KEYS__0,
  STR_ALLOW_OTHER_KEYS__1, STR_FORMAT_ARGUMENTS__4,
  STR_FORMAT_CONTROL__3, STR___2, STRn_5;

clObject clFindBadKey clVdecl(_ap)
{ clObject args, keys;
  { clBeginParse(_ap);
    clSetq(args,
           (_clVp(_ap) ? clVpop(_ap) : clMissingArgs(I_1, clEOA)));
    clSetq(keys,
           (_clVp(_ap) ? clVpop(_ap) : clMissingArgs(I_2, clEOA)));
    if (_clVp(_ap)) clExtraArgs(clVargs(_ap), clEOA);
    clEndParse(_ap); }
  { clObject key, L_g0;
    clSetq(key, clNIL);
    clSetq(L_g0, args);
    clLabel(NEXT_LOOP);
    if (clTrue(clEndp(L_g0, clEOA))) goto END_LOOP;
    clSetq(key, clCar(L_g0, clEOA));
    { clObject L_0;
      clSetq(L_0, L_g0);
      clSetq(L_g0, clCddr(L_0, clEOA)); }
    { clObject L_test;
      { clObject L_0;
        { clObject L_test__R1;
          { clObject L_0__R1;
            clSetq(L_0__R1, key);
            clSetq(L_test__R1, clSymbolp(L_0__R1, clEOA)); }
          if (clTrue(L_test__R1))
            { clObject L_g395;
              { clObject L_0__R1;
                clSetq(L_0__R1, key);
                clSetq(L_g395,
                       clEq(L_0__R1, keyALL_OTHER_KEYS, clEOA)); }
              if (clTrue(L_g395))
                clSetq(L_0, L_g395);
              else
                { { clObject L_item397, L_sub396;
                    clSetq(L_item397, key);
                    clSetq(L_sub396, keys);
                    clLabel(l_ITERATE399);
                    if (clTrue(clEndp(L_sub396, clEOA)))
                      { clSetq(L_0, clNIL); clLocalReturn(NIL); }
                    if (clTrue(clEql(L_item397,
                                     clCar(L_sub396, clEOA),
                                     clEOA)))
                      { clSetq(L_0, L_sub396);
                        clLocalReturn(l_MEMBER398); }
                    { clObject L_value400;
                      clSetq(L_value400, clCdr(L_sub396, clEOA));
                      clSetq(L_sub396, L_value400); }
                    goto l_ITERATE399; }
                  clBlockEnd(NIL);
                  clBlockEnd(l_MEMBER398); } }
          else clSetq(L_0, clNIL); }
        clSetq(L_test, clNot(L_0, clEOA)); }
      if (clTrue(L_test)) return(clValues1(key)); }
    goto NEXT_LOOP;
    clLabel(END_LOOP);
    return(clValues1(clNIL)); } }

clObject clCheckKeys clVdecl(_ap)
{ clObject args, keys, error_fmt, error_args;
  { clBeginParse(_ap);
    clSetq(args,
           (_clVp(_ap) ? clVpop(_ap) : clMissingArgs(I_1, clEOA)));
    clSetq(keys,
           (_clVp(_ap) ? clVpop(_ap) : clMissingArgs(I_2, clEOA)));
    clSetq(error_fmt, (_clVp(_ap) ? clVpop(_ap) : clNIL));
    clSetq(error_args, clVargs(_ap));
    clEndParse(_ap); }
  { clObject L_test;
    { clObject L_0;
      { clObject L_0__R1;
        clSetq(L_0__R1, args);
        clSetq(L_0, clGetf(L_0__R1, keyALLOW_OTHER_KEYS, clEOA)); }
      clSetq(L_test, clNot(L_0, clEOA)); }
    if (clTrue(L_test))
      { clObject bad;
        { clObject L_1, L_0;
          clSetq(L_0, args);
          clSetq(L_1, keys);
          clSetq(bad, clFindBadKey(L_0, L_1, clEOA)); }
        if (clTrue(bad))
          { clObject L_2, L_0;
            clSetq(L_0, clSymbolFunctionValue(clKEY_NOT_ALLOWED));
            clSetq(L_2, error_fmt);
            return(clApply(L_0, bad, L_2, error_args, clEOA)); }
        else return(clValues1(clNIL)); }
    else return(clValues1(clNIL)); } }

clObject clSignalProgramError clVdecl(_ap)
{ clObject fmt, args;
  { clBeginParse(_ap);
    clSetq(fmt, (_clVp(_ap) ? clVpop(_ap) : STR___2));
    clSetq(args, clVargs(_ap));
    clEndParse(_ap); }
  { clObject L_2;
    clSetq(L_2, fmt);
    return(clError(clPROGRAM_ERROR, keyFORMAT_CONTROL, L_2,
                   keyFORMAT_ARGUMENTS, args, clEOA)); } }

clObject clMultipleAppearanceError clVdecl(_ap)
{ clObject name, context;
  { clBeginParse(_ap);
    clSetq(name,
           (_clVp(_ap) ? clVpop(_ap) : clMissingArgs(I_1, clEOA)));
    clSetq(context, (_clVp(_ap) ? clVpop(_ap) : clNIL));
    if (_clVp(_ap)) clExtraArgs(clVargs(_ap), clEOA);
    clEndParse(_ap); }
  { clObject L_2, L_1;
    clSetq(L_1, name);
    clSetq(L_2, context);
    return(clSignalProgramError(STRn_5, L_1, L_2, clEOA)); } }

void clInitControlRun __P((void)) 
{ clDbind(clstarPACKAGEstar);
  clDbind(clstarREADTABLEstar);
  clDbind(clstarLOAD_TRUENAMEstar);
  clDbind(clstarLOAD_PATHNAMEstar);
  clSetq(I_1, clIntFixnum(1));
  clSetq(I_2, clIntFixnum(2));
  clSetq(STR_ALL_OTHER_KEYS__0,
         clCharpSimpleBaseString("ALL-OTHER-KEYS"));
  clSetq(keyALL_OTHER_KEYS,
         clMakeKeyword(STR_ALL_OTHER_KEYS__0, clEOA));
  clSetq(STR_ALLOW_OTHER_KEYS__1,
         clCharpSimpleBaseString("ALLOW-OTHER-KEYS"));
  clSetq(keyALLOW_OTHER_KEYS,
         clMakeKeyword(STR_ALLOW_OTHER_KEYS__1, clEOA));
  clSetq(STR___2, clCharpSimpleBaseString(""));
  clSetq(STR_FORMAT_CONTROL__3,
         clCharpSimpleBaseString("FORMAT-CONTROL"));
  clSetq(keyFORMAT_CONTROL,
         clMakeKeyword(STR_FORMAT_CONTROL__3, clEOA));
  clSetq(STR_FORMAT_ARGUMENTS__4,
         clCharpSimpleBaseString("FORMAT-ARGUMENTS"));
  clSetq(keyFORMAT_ARGUMENTS,
         clMakeKeyword(STR_FORMAT_ARGUMENTS__4, clEOA));
  clSetq(STRn_5,
         clCharpSimpleBaseString("~s appears more than once~@[ in ~a~]."));


  clSetSymbolFunctionValue(clFIND_BAD_KEY,
                           clMakeClosure(0,
                                         clFindBadKey,
                                         clNULL_HOOK));
  (void) clFIND_BAD_KEY;
  clSetSymbolFunctionValue(clCHECK_KEYS,
                           clMakeClosure(0, clCheckKeys, clNULL_HOOK));
  (void) clCHECK_KEYS;
  clSetSymbolFunctionValue(clSIGNAL_PROGRAM_ERROR,
                           clMakeClosure(0,
                                         clSignalProgramError,
                                         clNULL_HOOK));
  (void) clSIGNAL_PROGRAM_ERROR;
  clSetSymbolFunctionValue(clMULTIPLE_APPEARANCE_ERROR,
                           clMakeClosure(0,
                                         clMultipleAppearanceError,
                                         clNULL_HOOK));
  (void) clMULTIPLE_APPEARANCE_ERROR;
  clUnwind(4); }
