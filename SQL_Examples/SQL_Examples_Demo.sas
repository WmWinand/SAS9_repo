/********************************/
/* CREATE DATASETS FOR EXAMPLES */
/********************************/

data fruits;
   input day: $11. fruit $;
cards;
1-Monday Apples
3-Wednesday Bananas
4-Thursday Peaches
6-Saturday Pears
;
run;

title "one to one left - fruits";
proc print;
run;

data veggies;
  input day : $11. veggie $;
cards;
1-Monday Broccoli
2-Tuesday Spinach
4-Thursday Beans
5-Friday Carrots
;
run;

title "one to one right - vegetables";
proc print;
run;

data work.staff2;
input IdNum $4. @7 Lname $12. @20 Fname $8. @30 City $10. 
      @42 State $2. @50 Hphone $12.;
   datalines;
1106  MARSHBURN    JASPER    STAMFORD    CT      203/781-1457
1430  DABROWSKI    SANDRA    BRIDGEPORT  CT      203/675-1647
1118  DENNIS       ROGER     NEW YORK    NY      718/383-1122
1126  KIMANI       ANNE      NEW YORK    NY      212/586-1229
1402  BLALOCK      RALPH     NEW YORK    NY      718/384-2849
1882  TUCKER       ALAN      NEW YORK    NY      718/384-0216
1479  BALLETTI     MARIE     NEW YORK    NY      718/384-8816
1420  ROUSE        JEREMY    PATERSON    NJ      201/732-9834
1403  BOWDEN       EARL      BRIDGEPORT  CT      203/675-3434
1616  FUENTAS      CARLA     NEW YORK    NY      718/384-3329
;
run;

title "staff2";
proc print;
run;

data work.schedule2;
   input flight $3. +5 date date7. +2 dest $3. +3 idnum $4.;
   format date date7.;
   informat date date7.;
   datalines;
132     01MAR94  BOS   1118
132     01MAR94  BOS   1402
219     02MAR94  PAR   1616
219     02MAR94  PAR   1478
622     03MAR94  LON   1430
622     03MAR94  LON   1882
271     04MAR94  NYC   1430
271     04MAR94  NYC   1118
579     05MAR94  RDU   1126
579     05MAR94  RDU   1106
;
run;

title "schedule2";
proc print;
run;

data work.superv2;
   input supid $4. +8 state $2. +5  jobcat  $2.;
   label supid='Supervisor Id' jobcat='Job Category';
   datalines;
1417        NJ     NA
1352        NY     NA
1106        CT     PT
1442        NJ     PT
1118        NY     PT
1405        NJ     SC
1564        NY     SC
1639        CT     TA
1126        NY     TA
1882        NY     ME
;
run;

title "superv2";
proc print;
run;


/*******************************/
/* LISTING REPORT SASHELP.CARS */
/*******************************/

title "sashelp.cars";
PROC PRINT DATA=SASHELP.CARS (obs=10);
RUN;

title "";


/********************************************************************/
/* SIMPLE PROC SQL:                                                 */
/* An asterisk on the SELECT statement will select all columns from */
/* the data set.                                                    */
/********************************************************************/

PROC SQL;
	SELECT *
		FROM SASHELP.CARS;
QUIT;


/***********************************************************************/
/* LIMITING INFORMATION ON THE SELECT                                  */
/* To specify that only certain variables should appear on the report, */
/* the variables are listed and separated on the SELECT statement.     */
/* The NUMBER option will print a column on the report                 */
/* labeled 'ROW' which contains the observation number                 */
/***********************************************************************/

PROC SQL NUMBER;
/* CREATE TABLE work.query1 AS */
SELECT MAKE, MODEL, ORIGIN, MSRP
		FROM SASHELP.CARS;
QUIT;


/*****************************************************************/
/* SUBSETTING USING THE WHERE:                                   */
/* The WHERE statement will process a subset of data rows before */
/* they are processed. */
/*****************************************************************/

PROC SQL;
	SELECT *
		FROM SASHELP.CARS
		WHERE ORIGIN = 'USA'
			AND MSRP > 30000;
QUIT;


/***********************************************************************/
/* SORTING THE DATA IN PROC SQL:                                       */
/* The ORDER BY clause will return the data in sorted order: Much      */
/* like PROC SORT, if the data is already in sorted order, PROC        */
/* SQL will print a message in the LOG stating the sorting utility was */
/* not used. When sorting on an existing column, PROC SQL and          */
/* PROC SORT are nearly comparable in terms of efficiency. SQL         */
/* may be more efficient when you need to sort on a dynamically        */
/* created variable                                                    */
/***********************************************************************/

PROC SQL;
	SELECT MAKE, MODEL, ORIGIN, MSRP
		FROM SASHELP.CARS
		ORDER BY ORIGIN, MSRP DESC;
QUIT;


/*****************************************************/
/* CREATING NEW VARIABLES, ADDING LABELS AND FORMATS */
/*****************************************************/

PROC SQL;
	SELECT SUBSTR(TYPE,1,3) AS TYPCD LABEL='TYPE CODE',
		   INVOICE,
           (INVOICE * .05) AS EST_TAX FORMAT=DOLLAR8.,
		   MEAN(MPG_CITY, MPG_HIGHWAY) AS MPG_AVG FORMAT=5.2
	FROM SASHELP.CARS;
QUIT;


/******************************************************************/
/* USE GROUP BY AND HAVING:                                       */
/* In order to subset data when grouping is in effect, the HAVING */
/* clause must be used.                                           */
/******************************************************************/

PROC SQL;
	SELECT ORIGIN, TYPE, MAKE, MODEL, MPG_HIGHWAY
	FROM SASHELP.CARS
		GROUP BY ORIGIN, TYPE
			HAVING AVG(MPG_HIGHWAY) > 25
		ORDER BY MPG_HIGHWAY DESC;
QUIT;



title "one to one right";
proc print;
run;


/* JOINING TABLES */

/***************************************/
/* DEFAULT JOIN WITH NO MATCH CRITERIA */
/***************************************/

proc sql;
  create table sql_default_join1 as
    select f.*, v.*
	from fruits f, veggies v
	;
quit;

proc print data=sql_default_join1;
run;


/**********************************************************************/
/* A Conventional or Inner Join combines datasets only if an          */
/* observation is in both datasets. This type of join is similar to a */
/* DATA step merge using the IN Data Set Option and IF logic          */
/* requiring that the observation is on both data sets (IF ONA AND    */
/* ONB).                                                              */
/**********************************************************************/

/**************************/
/* DEFAULT SQL INNER JOIN */
/**************************/

proc sql;
  create table sql_key_join2 as
    select f.*, v.veggie
	from fruits f, veggies v
	where f.day=v.day
	;
quit;

title 'SQL Inner Join';
proc print; 
run;




/******************************/
/* INNER JOIN ON THREE TABLES */
/******************************/

proc sql;
   title 'All Flights for Each Supervisor';
   select s.IdNum, Lname, City 'Hometown', Jobcat,
          Flight, Date
from WORK.schedule2 s, WORK.staff2 t, WORK.superv2 v
where s.idnum=t.idnum and t.idnum=v.supid;




/*************************/
/* DEFAULT SQL FULL JOIN */
/*************************/

proc sql;
  create table sql_full_join5 as
    select f.*, v.veggie
	from fruits f
	full join
	veggies v
	on f.day=v.day
	;
quit;

title 'SQL Full Join - One to One';
proc print; 
run;


/**************************************/
/* SQL FULL JOIN WITH COALESCE OPTION */
/**************************************/

proc sql;
  create table sql_full_join5a as
    select coalesce (f.day, v.day) as Key, f.fruit, v.veggie
	from fruits f
	full join
	veggies v
	on f.day=v.day
	;
quit;

title 'SQL Full Join - One to One - w Coalesce Option';
proc print; 
run;


/*******************************************/
/* CREATING EXECUTION TIME MACRO VARIABLES */
/*******************************************/

title 'WORK.Houses Table';
proc sql;
   select * from WORK.houses;

proc sql noprint;
   select style, sqfeet
      into :m_style, :m_sqfeet
      from WORK.houses
	  WHERE STYLE='SPLIT' AND SqFeet<1750;

%put &m_style &m_sqfeet;


/************************************/
/* IIMPLICIT SQL QUERY TO SNOWFLAKE */
/************************************/

/* Options to help with determining when execution occurs in-database.  */
OPTION 
	SASTRACE=',,,ds' 
	SASTRACELOC=SASLOG 
	NOSTSUFFIX 
	SQL_IP_TRACE=(note, source) 
	msglevel=i 
	DBIDIRECTEXEC
	sqlgeneration=none
	;


/* BEGIN - Set up IMPLICIT LIbname statements to leverage In-Database PROCS. */
/* Set up connection to Snowflake... */
libname snow SASIOSNF 
	server="saspartner.snowflakecomputing.com"
	user=REPRUI 
	pw="{SAS002}B87C6F3C3D2FB7D53AC0FC1C"
	schema=REPRUI 
	preserve_tab_names=yes
	bulkload=yes 
	bulkunload=yes 
	bl_internal_stage="user/test1"
	;


/****************************/
/* RUN IMPLICIT SQL QUERIES */
/****************************/

PROC SQL;
	SELECT *
		FROM SNOW.CARS
		WHERE ORIGIN = 'USA';
QUIT;


proc sql;
   select count(unique(make)) as u_make 'Number of the car makers',
     count(unique(origin)) as u_origin 'Number of the car origins',
     count(unique(type)) as u_type 'Number of the car types'
       from SNOW.cars
   ;
quit;



