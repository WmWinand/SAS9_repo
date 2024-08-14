/*****************************************************/
/* This program introduces basic SAS programming     */
/* concepts to programmers new to SAS - CHANGE       */
/*****************************************************/

/* SET SAS LIBREF */
/* libname mydata "/greenmonthly-export/ssemonthly/homes/T.Winand@sas.com/Git_Repo/data"; */
libname mydata "/innovationlab-export/innovationlab/homes/T.Winand@sas.com/Data/Datasets";


/* libname mydblib hadoop  */
/*                   user=myusr1  */
/*                   password=mypwd1  */
/*                   server=hadoopsvr  */
/*                   schema=hrdept;  */



/* THIS IS AN EXAMPLE OF A VERY SIMPLE DATA STEP */
data work.class;
  set sashelp.class;
run;


/* THIS IS AN EXAMPLE OF A SIMPLE PROCEDURE */
proc print data=work.class;
run;


/* THIS IS AN EXAMPLE OF A SIMPLE PROCEDURE WITH CHANGES */
proc print data=class (obs=10);
  var name sex age weight;
run;


/* DATA STEP CREATING A CALCULATED VARIABLE */
data class;
  set sashelp.class;
  bmi = (weight / (height*height)) * 703;
run;


/* DATA STEP CREATING MULTIPLE OUTPUT DATASETS & FORMATTING BMI */
data class_m_bmi class_f_bmi;
	set sashelp.class;
	format bmi comma6.3;
	bmi=(weight / (height*height)) * 703;

	if sex='M' then
		output class_m_bmi;
	else if sex='F' then
		output class_f_bmi;
run;

/* PROC PRINT TO EXCEL WITH A FILTER */
/* %let datadir = C:/GitDemos/WTW_Demo_LocalRepo/data; */
/* ods excel file="&datadir/class_m_highBmi.xlsx"; */
/* title "Listing of Class Males with High BMI"; */
/* proc print data=class_m_bmi; */
/*   where bmi > 18; */
/* run; */
/* ods excel close; */
/* title""; */


/* USE DATASTEP MERGE STATEMENT TO MERGE TWO TABLES (INNER JOIN) */
proc sort data=sashelp.class out=class_sorted;
  by name;
run;

proc sort data=sashelp.classfit out=classfit_sorted;
  by name;
run; 

data class_join;
  merge class_sorted (in=c) classfit_sorted (in=cf);
    by name;
    if c and cf;
run;

proc print data=class_join;
  var name age sex height weight predict lower upper;
run;


/* USE PROC SQL TO CREATE A QUERY TO JOIN TWO TABLES */
proc sql;
  create table sql_join as  
    select c.*, cf.predict, cf.lower, cf.upper
    from sashelp.class c, sashelp.classfit cf
    where c.name=cf.name;
quit;

proc print data=sql_join;
run;


/* PROC FREQ - THIS CODE WAS GENERATED FROM TABLE ANALYSIS TASK */
/* RUN ONE-WAY FREQUENCY TASK ON CARS,ORIGIN */
/* RUN TABLE ANALYSIS TASK TO GENERATE THIS CODE */

ods noproctitle;

proc freq data=SASHELP.CARS;
	tables  (Origin) *(Type) / chisq expected nopercent norow nocol cellchi2 
		plots(only)=(freqplot mosaicplot);
run;


/* CODE GENERATED FROM SUMMARY STATISTICS TASK */
ods noproctitle;
ods graphics / imagemap=on;

proc means data=SASHELP.CARS chartype mean std min max n vardef=df;
	var Invoice MSRP MPG_Highway Weight;
	class Origin;
	output out=work.cars_stats mean=std=min=max=n= / autoname;
run;

proc univariate data=SASHELP.CARS vardef=df noprint;
	var Invoice MSRP MPG_Highway Weight;
	class Origin;
	histogram Invoice MSRP MPG_Highway Weight / normal(noprint);
run;

proc sort data=SASHELP.CARS out=WORK.TempSorted2236;
	by Origin;
run;

proc boxplot data=WORK.TempSorted2236;
	plot (Invoice MSRP MPG_Highway Weight)*Origin / boxstyle=schematic;
run;

proc datasets library=WORK noprint;
	delete TempSorted2236;
	run;


/* EXPORT SUMMARY STATISTICS */
proc export data=work.cars_stats
            dbms=xlsx
            OUTfile="/greenmonthly-export/ssemonthly/homes/T.Winand@sas.com/Data/cars_stats.xlsx"
            replace;
run;


/* SNIPPET - HISTOGRAM PLOT */
title 'Distribution of Mileage';
proc sgplot data=sashelp.cars(where=(type ne 'Hybrid'));
  histogram mpg_city;
  density mpg_city / lineattrs=(pattern=solid);
  density mpg_city / type=kernel lineattrs=(pattern=solid);
  keylegend / location=inside position=topright across=1;
  yaxis offsetmin=0 grid;
run;

