%let mpg=15;

/* Init check flag */
%let dsempty=0;

data work.cars_usa (keep=make model mpg_city mpg_highway mpg_avg);
  set sashelp.cars;
  where origin="USA";
  mpg_avg=(mpg_city+mpg_highway)/2;
run;

proc sort data=work.cars_usa;
  by mpg_avg;
run;

data work.cars_flagged;
  set work.cars_usa;
  where mpg_avg<&mpg;
run;

/* Check for empty data */
data _null_;
  if eof then
    do;
     call symput('dsempty',1);
     put 'NOTE: EOF - no records in data!';
    end;
  stop;
  set work.cars_flagged end=eof;
run;

%macro alert;
  %if &dsempty. %then
    %do;
      %put No alerts generated.;
    %end;
  %else
    %do;
      /*if not empty then  create listing report*/
      ods pdf file='/home/sasdemo/WTW_Demo_Repo/data/cars_alert.pdf' pdftoc=2;
      title "Cars with Average Mileage Less Than &mpg MPG";
      proc print data=work.cars_flagged;
      run;
      ods pdf close;
    %end;
%mend alert;

%alert


proc options group=email; run;

%macro alert_email;
  %if &dsempty. %then
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
			attach="/home/sasdemo/WTW_Demo_Repo/data/cars_alert.pdf"
			;

		ods tagsets.msoffice2k(id=email)
			file=mymail(title="Test_Email_&sysdate9")
			style=printer
			;
		Title "Cars Minimum MPG Violation Report ";
		Title2 "Cars with Average MPG Less Than &mpg Miles Per Gallon";
		Footnote "Generated &sysdate";

		Proc Print 
			Data=work.cars_flagged (obs=10) 
			n  
			label 
			;

				Run;
		ods tagsets.msoffice2k(id=email) close;

    %end;

%mend alert_email;

%alert_email


