/*********************************************************************
  This data and information is proprietary to, and a valuable trade
  secret of, Elwood Corporation.  It is given in confidence by Elwood,
  and may only be used as permitted under the license agreement under
  which it has been distributed and in no other way.  No disclosure of
  this information shall be made to any person or organization without
  the prior consent of Elwood Corporation. 

  (c) Copyright 1998 by Elwood Corp.  All rights reserved.

  Written by HRS.
 *********************************************************************/
#include <eclipse.h>

extern void clInitCommonComp __P((void)), 
  clInitDevMeth __P((void)), clInitProgComp __P((void)), 
  clInitContComp __P((void)), clInitContComp2 __P((void)), 
  clInitMacro __P((void)), clInitMacroComp __P((void)),
  clInitOpts __P((void)), clInitNumberComp __P((void)),
  clInitListComp __P((void)), clInitClosComp __P((void)),
  clInitPrettyComp __P((void)), clInitStructComp __P((void)),
  clInitCondComp __P((void)), clInitMoreCompile __P((void)),
  clInitEnvComp __P((void)), clInitLoop __P((void)),
  clInitDirectives __P((void)), clInitDescribe __P((void)),
  clInitDebug __P((void)), clInitDefaultPp __P((void)),
  clInitWalkUtil __P((void)), clInitWalkTop __P((void)),
  clInitWalkSpecial __P((void)), clInitLiteral __P((void)),
  clInitEvaluation __P((void)), clInitC __P((void)), 
  clInitFileWalk __P((void)), clInitCWalk __P((void)), 
  clInitPrimDecs __P((void)), clInitCPkg __P((void));

extern void clProgress __P((const char *)); 

void clInitD __P((void));

void clInitD()
{
  /*clProgress("c-pkg"); clInitCPkg(); */
  clProgress("dev-meth"); clInitDevMeth();
  clProgress("env-comp"); clInitEnvComp();
  clProgress("common-comp"); clInitCommonComp();
  clProgress("prog-comp"); clInitProgComp(); 
  clProgress("cont-comp"); clInitContComp(); 
  clProgress("cont-comp2"); clInitContComp2(); 
  clProgress("macro"); clInitMacro(); 
  clProgress("macro-comp"); clInitMacroComp(); 
  clProgress("opts"); clInitOpts();
  clProgress("number-comp"); clInitNumberComp(); 
  clProgress("list-comp"); clInitListComp(); 
  clProgress("clos-comp"); clInitClosComp(); 
  clProgress("pretty-comp"); clInitPrettyComp(); 
  clProgress("struct-comp"); clInitStructComp(); 
  clProgress("cond-comp"); clInitCondComp(); 
  clProgress("more-compile"); clInitMoreCompile(); 
  clProgress("env-comp"); clInitEnvComp();
  clProgress("loop"); clInitLoop();
  clProgress("directives"); clInitDirectives();
  clProgress("describe"); clInitDescribe(); 
  clProgress("debug"); clInitDebug();
  clProgress("default-pp"); clInitDefaultPp(); 
  clProgress("walk-util"); clInitWalkUtil();
  clProgress("walk-top"); clInitWalkTop();
  clProgress("walk-special"); clInitWalkSpecial(); 
  clProgress("literal"); clInitLiteral(); 
  clProgress("evaluation"); clInitEvaluation(); 
  clProgress("c"); clInitC(); 
  clProgress("file-walk"); clInitFileWalk(); 
  clProgress("c-walk"); clInitCWalk();
  clProgress("prim-decs"); clInitPrimDecs();
  clProgress("\n\n");
}
