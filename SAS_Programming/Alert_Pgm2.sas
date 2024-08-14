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

ods excel file='/greenmonthly-export/ssemonthly/homes/T.Winand@sas.com/Data/cars_alert2.xlsx';

proc report data=work.cars_usa_sorted;
    define mpg_avg / style(column)= {tagattr='format:[red][<15]; ###'};
run;

ods excel close;



%macro alert_email;
  %if &mpg_alert=0 %then
    %do;
      %put No alerts generated. No emails sent.;
    %end;
  %else
    %do;
      /*if not empty then  create listing report*/


    %end;

%mend alert_email;

%alert_email

