/* conditionally process the PROC PRINT or the PROC MEANS code */
%macro claiminfo_conditional1(report, year);
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