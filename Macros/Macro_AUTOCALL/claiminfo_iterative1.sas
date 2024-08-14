%macro claiminfo_iterative1;
%do year=2005 %to 2007;
	proc print data=health.claims_sample(obs=25);
	   var providerID memberID chgamt lndiscamt eeresp paidamt;
	   title "Report for &year - first 25 rows";
	   where svc_year="&year";
	run;

	proc means data=health.claims_sample;
	   title "Summary for &year";
	   var chgamt lndiscamt eeresp paidamt;
	   where svc_year = "&year";
	run;
%end;
%mend;