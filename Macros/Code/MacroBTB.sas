libname health  "/greenmonthly-export/ssemonthly/homes/T.Winand@sas.com/Git_Repo/Data";

options mcompilenote=all mprint;
/* macro with parameters */
/* create the macro with a parameter (corresponds to the macro variable in the code */

%macro claiminfo(year);
proc print data=health.claims_sample (obs=20);
   var providerID memberID chgamt lndiscamt eeresp paidamt;
   title "Report for &year";
   where svc_year="&year";
run;

proc means data=health.claims_sample;
   title "Summary for &year";
   var chgamt lndiscamt eeresp paidamt;
   where svc_year = "&year";
run;
%mend;

/* call the macro with a value for the parameter (Year) */

%claiminfo(2005)
%claiminfo(2006)
%claiminfo(2007)

/* Use iterative processing to dynamically generate year values */

%macro claiminfo;
%do year=2005 %to 2007;
	proc print data=health.claims_sample (obs=20);
	   var providerID memberID chgamt lndiscamt eeresp paidamt;
	   title "Report for &year";
	   where svc_year="&year";
	run;

	proc means data=health.claims_sample;
	   title "Summary for &year";
	   var chgamt lndiscamt eeresp paidamt;
	   where svc_year = "&year";
	run;
%end;
%mend;

%claiminfo



%macro claiminfo(start, end);
%do year=&start %to &end;
	proc print data=health.claims_sample (obs=20);
	   var providerID memberID chgamt lndiscamt eeresp paidamt;
	   title "Report for &year";
	   where svc_year="&year";
	run;

	proc means data=health.claims_sample;
	   title "Summary for &year";
	   var chgamt lndiscamt eeresp paidamt;
	   where svc_year = "&year";
	run;
%end;
%mend;

%claiminfo(2005,2008)

/* conditionally process the PROC PRINT or the PROC MEANS code */
%macro claiminfo(report, year);
%if %upcase(&report)= LISTING %then %do;
	proc print data=health.claims_sample (obs=20);
	   var providerID memberID chgamt lndiscamt eeresp paidamt;
	   title "Report for &year";
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

%claiminfo(listing ,2005)
%claiminfo(hsfg ,2006)


/* CALL SYMPUTX */
/* use PROC MEANS to create an overall average for each 
portion of the charges and then use those variables to 
create macro variables */

proc means data=health.claims_sample noprint;
	   title "Summary for 2005";
	   var chgamt lndiscamt eeresp paidamt;
	   where svc_year = "2005";
	   output out=claims_stats mean=;
run;
proc print data=claims_stats;
   title 'Claims with Mean Statistic';
run;

data _null_;
   set claims_stats;
   call symputx('chgmean',chgamt);
   call symputx('discountmean',lndiscamt);
   call symputx('eerespmean', eeresp);
   call symputx('paidmean', paidamt);
run;
%put &chgmean &discountmean &eerespmean &paidmean;

proc sql noprint;
  select count (*)
    into :total trimmed
	from sashelp.cars;
quit;

%put Total number of observations = &total;


/* Create a data set that contains all default statistics for PROC MEANS 
and use this data to create a series of macro variables for CHGAMT
*/
proc means data=health.claims_sample noprint;
	   title "Summary for 2005";
	   var chgamt lndiscamt eeresp paidamt;
	   where svc_year = "2005";
	   output out=claims_stats ;
run;
proc print data=claims_stats;
run;

data _null_;
   set claims_stats;
   call symputx('chgamt'||_stat_,round(chgamt));
 run;
%put &chgamtN &chgamtmin &chgamtmax &chgamtmean &chgamtstd;

/*create a series of macro variables whose names are suitable
for iterative processing */
data _null_;
   set claims_stats end=nomore;
   call symputx('chgamt'||left(_n_),round(chgamt));
   if nomore then call symputx('totalrows',_n_);
run;
%put &chgamt1 &chgamt2 &chgamt3 &chgamt4 &chgamt5;
%put total rows: &totalrows;

/* use a macro loop to display the variables */
%macro loop;
   %do i=1 %to &totalrows;
        %put value chgamt&i is: &&chgamt&i;
    %end;
%mend;

%loop