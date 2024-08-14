libname health "/greenmonthly-export/ssemonthly/homes/T.Winand@sas.com/Git_Repo/Data";

filename AUTODIR '/greenmonthly-export/ssemonthly/homes/T.Winand@sas.com/Git_Repo/Macros/Macro_AUTOCALL';
options sasautos=autodir;

options mcompilenote=ALL mprint;

/* %claiminfo_iterative1 */
%claiminfo_conditional1(LISTING,2005)
%claiminfo_conditional1(STATS,2006)