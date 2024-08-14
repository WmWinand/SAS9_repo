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

%put &dsempty;

%macro alert;
  %if &dsempty. %then
    %do;
      %put No alerts generated.;
    %end;
  %else
    %do;
      /*if not empty then  create listing report*/
      ods pdf file='/greenmonthly-export/ssemonthly/homes/T.Winand@sas.com/Data/cars_alert.pdf' pdftoc=2;
      title "Cars with Average Mileage Less Than &mpg MPG";
      proc print data=work.cars_flagged;
      run;
      ods pdf close;
    %end;
%mend alert;

%alert

%macro alert_email;
  %if &dsempty. %then
    %do;
      %put No alerts generated. No emails sent.;
    %end;
  %else
    %do;
      /*if not empty then email listing report*/


    %end;

%mend alert_email;

%alert_email

