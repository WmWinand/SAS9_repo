/* SET LIBRARY TO MACRO CATALOG */
libname loct "/greenmonthly-export/ssemonthly/homes/T.Winand@sas.com/Git_Repo/Macros/Macro_Compiled/";
options mstored sasmstore = loct;

/* %macro storemacroval / store source des="Description of Macro"; */

/* conditionally process the PROC PRINT or the PROC MEANS code */
options mcompilenote=ALL;

%macro claiminfo_conditional2(report, year) / store source 
                                              des="Conditionally process the PROC PRINT or the PROC MEANS code";
%if %upcase(&report)= LISTING %then %do;
	proc print data=health.claims_sample(obs=25);
	   var providerID memberID chgamt lndiscamt eeresp paidamt;
	   title "Report for &year - first 25 rows";
	   where svc_year="&year";
	run;
	%end;
%else %do;
	proc means data=health.claims_sample;
	   title "Summary for &year";
	   var chgamt lndiscamt eeresp paidamt;
	   where svc_year = "&year";
	run;
 %end;
%mend; 


