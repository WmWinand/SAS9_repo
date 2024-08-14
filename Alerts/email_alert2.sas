%let mpg=15;
%let mpg_alert=0;


data work.cars_usa (keep=make model mpg_city mpg_highway mpg_avg);
  set sashelp.cars;
  where origin="USA";
  mpg_avg=(mpg_city+mpg_highway)/2;
  if mpg_avg < &mpg 
	then call symput('mpg_alert',1);
run;

proc sort data=work.cars_usa out=work.cars_usa_sorted;
  by mpg_avg;
run;

ods excel file='/home/sasdemo/WTW_Demo_Repo/data/cars_alert2.xlsx';
		Title "Cars Minimum MPG Violation Report ";
		Title2 "Cars with Average MPG Less Than &mpg Miles Per Gallon";
		Footnote "Generated &sysdate";

proc report data=work.cars_usa;
    define mpg_avg / style(column)= {tagattr='format:[red][<20]; ###'};
run;

ods excel close;

proc options group=email; run;

%macro alert_email;
  %if &mpg_alert=0 %then
    %do;
      %put No alerts generated. No emails sent.;
    %end;
  %else
    %do;
		options emailsys=smtp emailhost=mailhost.na.sas.com;
		filename mymail email 
			To=(
				"T Winand <T.Winand@sas.com>" 
/*				"Rex Pruitt <rex.pruitt@sas.com>" */
				)
			From="From T <t.winand@sas.com>"
			subject="Cars MPG Alert Report - &sysdate9"
			ct="text/html"
			attach="/home/sasdemo/WTW_Demo_Repo/data/cars_alert2.xlsx"
			;

		ods tagsets.msoffice2k(id=email)
			file=mymail(title="Test_Email_&sysdate9")
			style=printer
			;
		Title "Cars Minimum MPG Violation Report ";
		Title2 "Cars with Average MPG Less Than &mpg Miles Per Gallon";
		Footnote "Generated &sysdate";

		proc report data=work.cars_usa;
		    define mpg_avg / style(column)= {tagattr='format:[red][<20]; ###'};
		run;
		ods tagsets.msoffice2k(id=email) close;

    %end;

%mend alert_email;

%alert_email

