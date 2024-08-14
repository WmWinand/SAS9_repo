/**************************************
*Jane Eslinger
*SAS
*
*PROC REPORT and PROC TABULATE
*
*All examples use SASHELP data sets
**************************************/

title 'PROC REPORT - very basic example';
proc report data=sashelp.prdsal2;
	column country state product predict actual;
run;


title 'PROC REPORT - with groupings/aggregations';
proc report data=sashelp.prdsal2;
	column country state product predict actual;
	define country / group noprint;
	define state / group;
	define product / group;
run;


ods powerpoint file = '';
ods word file = '';

title 'PROC REPORT - with groupings/aggregations';
proc report data=sashelp.prdsal2;
	column country state product predict actual;
	define country / group noprint;
	define state / group;
	define product / group;
run;



title 'PROC REPORT general example - country section header';
proc report data=sashelp.prdsal2;
	column country state product predict actual;
	define country / group noprint;
	define state / group;
	define product / group;

	compute before country;
		line 'Section for ' country $20.;
	endcomp;
run;


title 'PROC REPORT general example - country summary totals';
proc report data=sashelp.prdsal2;
	column country state product predict actual;
	define country / group noprint;
	define state / group;
	define product / group;

	compute before country;
		line 'Section for ' country $20.;
	endcomp;

    break after country / summarize;

    compute after country;
		state = 'total';
	endcomp;

run;

/*******************************************************************************
PROC REPORT - General usage example
*******************************************************************************/
/* Create a new column and add customized text */
title 'PROC REPORT general example - Dif Column';
proc report data=sashelp.prdsal2;
	column country state product predict actual diff;
	define country / group noprint;
	define state / group;
	define product / group;
	define diff / computed 'Dif=Pred-Act';

	compute before country;
		line 'Section for ' country $20.;
	endcomp;

	break after country / summarize;

	compute after country;
		state = 'Total';
	endcomp;

	compute diff;
		diff = predict.sum - actual.sum;
	endcomp;
run;



/*******************************************************************************
PROC REPORT - General usage example
*******************************************************************************/
/* Create a new column and add customized text */
title 'PROC REPORT general example - Page breaks & formats';
proc report data=sashelp.prdsal2;
	column country state product predict actual diff;
	define country / group noprint;
	define state / group;
	define product / group;
	define predict / sum format=dollar14.2;
	define actual / sum format=dollar14.2;
	define diff / computed 'Dif=Pred-Act' format=dollar14.2;

	compute before country;
		line 'Section for ' country $20.;
	endcomp;

	break after country / summarize;
	break after country / page;

	compute after country;
		state = 'Total';
	endcomp;

	compute diff;
		diff = predict.sum - actual.sum;
	endcomp;
run;


/*******************************************************************************
PROC REPORT vs PROC TABULATE - examples
*******************************************************************************/
/* Calculate percentages */
proc format;                           
   picture mypct low-high='009.9%';   
run; 

title 'Percentages from PROC REPORT';
proc report data=sashelp.cars(obs=90);
   column make type invoice invper msrp msrpper;
   define make / group;
   define type / group;
   define invper / computed format=mypct. 'Invoice/Percent';
   define msrpper / computed format=mypct. 'MSRP/Percent';

   /* Put the total sum of analysis variables into a temporary variables */
   compute before make;
      invden = invoice.sum;
      msrpden = msrp.sum;
   endcomp;

   /* Calculate the percentages */
   compute invper;
      if invden > 0 then invper = invoice.sum / invden * 100;
   endcomp;
   compute msrpper;
      if msrpden > 0 then msrpper = msrp.sum / msrpden * 100;
   endcomp;
run;

title 'Percentages from PROC TABULATE';
proc tabulate data=sashelp.cars(obs=90);
   class make type;
   var invoice msrp;
   tables make*type, invoice*(sum*f=dollar12. pctsum<type>*f=mypct.) 
                        msrp*(sum*f=dollar12. pctsum<type>*f=mypct.);
run;

/* Calculate subtotals */
data cars1;
   set sashelp.cars;
   dummy1 = 1;
   dummy2 = 1;
   dummy3 = 1;
   dummy4 = 1;
   dummy5 = 1;
   dummy6 = 1;
run;

title 'Subtotals from PROC REPORT';
proc report data=cars1;
   column dummy1 dummy2 dummy3 dummy4 dummy5 dummy6
          origin type msrp invoice;
   define dummy1 /group noprint;
   define dummy2 /group noprint;
   define dummy3 /group noprint;
   define dummy4 /group noprint;
   define dummy5 /group noprint;
   define dummy6 /group noprint;
   define origin /group;
   define type /group;
   define msrp /format=dollar12.;
   define invoice /format=dollar12.;

   break after dummy1 / summarize;
   break after dummy2 / summarize;
   break after dummy3 / summarize;
   break after dummy4 / summarize;
   break after dummy5 / summarize;
   break after dummy6 / summarize;

	compute invoice;
		array types{6} $ ("HYBRID" "SUV" "SEDAN" "SPORTS" "TRUCK"
                              "WAGON");
		array msrps{6} msrptyp6-msrptyp1;
		array invoices{6}invotyp6-invotyp1;

		/* Calculate subtotals */
		do i=1 to dim(types);
			if upcase(type) = types{i} then do;
				msrps{i} + msrp.sum;
				invoices{i} + invoice.sum;
			end;
		end;

		/*reassign to subtotal values*/
		array breaks{6} $ ("DUMMY6" "DUMMY5" "DUMMY4" "DUMMY3" "DUMMY2"
                               "DUMMY1");
		do i=1 to dim(breaks);
			if upcase(_break_) = breaks{i} then do;
				if upcase(breaks{i}) = "DUMMY6" then 
                            origin = "TOTALS";
				type = propcase(types{i});
				msrp.sum = msrps{i};
				invoice.sum = invoices{i};
			end;
		end;
	endcomp;
run;

title 'Subtotals from PROC TABULATE';
proc tabulate data=sashelp.cars;
   class origin type;
   var msrp invoice;
   tables origin*type all='TOTALS'*type='', (msrp invoice)*f=dollar12.;
   keylabel sum='';
run;

/*******************************************************************************
PROC REPORT - Color 
*******************************************************************************/
/* Use a format to apply color */
proc format;
	value sysp 
		131-high = 'red';
run;

proc tabulate data=sashelp.heart;
	class bp_status weight_status;
	var diastolic systolic;
	table bp_status*weight_status all='TOTALS'*weight_status='', 
       (diastolic systolic*[s={foreground=sysp.}])*mean*f=8.;
	keylabel mean=' ';
run;

/* Use a format to apply color */
proc format;
	value bmi 
		low- 18.4= 'yellow'
		18.5-24.9='green'
		25-29.9='orange'
		30-high='red'
	;
run;

title 'PROC REPORT Color from a Format';
proc report data=sashelp.heart;
	column bp_status sex,(height weight bmi);
	define bp_status / group;
	define sex / across ' ';
	define height / mean 'Height (Avg)' f=8.2;
	define weight / mean 'Weight (Avg)' f=8.1;
	define bmi / computed 'BMI (Avg)' f=8. 
       style(column)=[background=bmi.];

	compute bmi;
		_c4_ = 703 * (_c3_ / (_c2_*_c2_));
		_c7_ = 703 * (_c6_ / (_c5_*_c5_));
	endcomp;
	compute after;
		abmi = sum(_c4_,_c7_)/2;
		line "The overall average BMI is: " abmi 8.1;
	endcomp;
run;

/* Create a new column and format based on another column */
title 'PROC REPORT Color from CALL DEFINE';
proc report data=sashelp.prdsal2;
	column country product predict actual diff;
	define country / group;
	define product / group;
	define diff / computed 'Dif=Pred-Act' format=dollar10.2;
	break after country / summarize;
	compute diff;
		diff = predict.sum - actual.sum;
		if _break_ ^= '' then do;
			if country = 'U.S.A.' 
              then call define('diff','style','style=[background=lightblue]');
			else if country = 'Mexico' 
              then call define('diff','style','style=[background=lightgreen]');
			else if country = 'Canada' 
             then call define('diff','style','style=[background=lightred]');
		end;
	endcomp;
run;

/* highlight an entire row based on the value of one column */
title 'PROC REPORT Hightlight a Row';
proc report data=sashelp.class;
	define age / display;

	compute sex;
		if sex = "F" then call define(_row_,'style','style=[background=pink]');
	endcomp;
	compute age;
		if age = 12 then call define(_row_,'style/merge','style=[font_weight=bold font_size=12pt]');
	endcomp;
run;
