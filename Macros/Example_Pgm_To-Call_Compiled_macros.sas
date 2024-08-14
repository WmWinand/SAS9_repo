libname health  "/greenmonthly-export/ssemonthly/homes/T.Winand@sas.com/Git_Repo/Data";

libname loct clear;
libname loct '/greenmonthly-export/ssemonthly/homes/T.Winand@sas.com/Git_Repo/Macros/Macro_Compiled/';

PROC CATALOG catalog=loct.sasmacr;
  Contents;
Run;

	options mstored sasmstore=loct;

	/* %claiminfo_iterative1 */
%claiminfo_conditional2(LISTING, 2005) 
%claiminfo_conditional2(STATS, 2006)