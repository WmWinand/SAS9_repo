/* PIE CHARTS */
goptions reset=all border;

data totals;
	length dept $ 7 site $ 8;
	input dept site quarter sales;
	datalines;
Parts Sydney 1 7043.97
Parts Atlanta 1 8225.26
Parts Paris 1 5543.97
Tools Sydney 4 1775.74
Tools Atlanta 4 3424.19
Tools Paris 4 6914.25
;

title "Total Sales";

proc gchart data=totals;
	format sales dollar8.;
	pie site / sumvar=sales;
run;

pie3d site / sumvar=sales
	explode="Paris";
run;

quit;

/* SUBGROUPING WITH A PIE CHART */
goptions reset=all border;

data totals;
	length dept $ 7 site $ 8;
	input dept site quarter sales;
	datalines;
Parts Sydney 1 7043.97
Parts Atlanta 1 8225.26
Parts Paris 1 5543.97
Tools Sydney 4 1775.74
Tools Atlanta 4 3424.19
Tools Paris 4 6914.25
;

title "Sales by Site and Department";
legend1 label=none
	position=(middle left)
	offset=(5,)
	across=1;

proc gchart data=totals;
	format sales dollar8.;
	donut site / sumvar=sales
		subgroup=dept
		donutpct=30
		label=("All" justify=center "Quarters")
		noheading
		legend=legend1;
run;

quit;

/* GROUPING AND ARRANGING PIE CHARTS */
goptions reset=all border;
title "Types of Trucks Produced Worldwide";

proc gchart data=sashelp.cars(where=(type="SUV" or type="Truck"));
	pie make / group=type
		across=2
		other=5 otherlabel="Other Makes"
		clockwise value=none
		slice=outside percent=outside;
run;

quit;

/* CHARTING A DISCRETE NUMERIC VARIABLE WITH A STAR CHART */
goptions reset=all border;

data rejects;
	informat date date9.;
	input site $ date badparts;
	datalines;
Sydney 01JAN1997 8
Sydney 01FEB1997 11
Sydney 28JUN1997 13
Sydney 31OCT1997 6
Paris 11APR1997 12
Paris 04MAY1997 12
Paris 30AUG1997 14
Paris 01DEC1997 7
Atlanta 15MAR1997 7
Atlanta 18JUL1997 12
Atlanta 03SEP1997 10
Atlanta 12NOV1997 9
;

title "Rejected Parts";

proc gchart data=rejects;
	format date worddate3.;
	star date / discrete
		sumvar=badparts
		noheading
		fill=s;
run;

star date / discrete
	sumvar=badparts
	noheading
	noconnect;
run;

quit;

/* BAR CHART - SUMMARY STATISTICS */
goptions reset=all border;

data totals;
	length dept $ 7 site $ 8;
	input dept site quarter sales;
	datalines;
Parts Sydney 1 7043.97
Parts Atlanta 1 8225.26
Parts Paris 1 5543.97
Tools Sydney 4 1775.74
Tools Atlanta 4 3424.19
Tools Paris 4 6914.25
;

title1 "Total Sales";

proc gchart data=totals;
	format sales dollar8.;
	hbar site / sumvar=sales;
run;

vbar3d site / sumvar=sales;
run;

quit;

/* BAR */
goptions reset=all cback=white;

data totals;
	length dept $ 7 site $ 8;
	input dept site quarter sales;
	datalines;
Parts Sydney 1 7043.97
Parts Atlanta 1 8225.26
Tools Paris 4 1775.74
Tools Atlanta 4 3424.19
Repairs Sydney 2 5543.97
Repairs Paris 3 6914.25
;

title1 "Total Sales by Site";
axis1 label=none offset=(10,8);
axis2 label=none order=(0 to 20000 by 5000) minor=none offset=(,0);
legend1 label=none shape=bar(.15in,.15in) cborder=black;

proc gchart data=totals;
	format sales dollar8.;
	vbar3d site / sumvar=sales subgroup=dept inside=subpct
		outside=sum
		width=9
		space=7
		maxis=axis1
		raxis=axis2
		cframe=white
		autoref cref=gray
		legend=legend1;
run;

quit;


/* PLOTTING TWO VARIABLES */
goptions reset=all border;
title "Study of Height vs Weight";
footnote1 j=l "Source: T. Lewis & L. R. Taylor";
footnote2 j=l "Introduction to Experimental Ecology";

proc gplot data=sashelp.class;
	plot height*weight;
run;

footnote1; /* this clears footnote1 and footnote2 */
symbol1 interpol=rcclm95
	value=circle
	cv=darkred
	ci=black
	co=blue
	width=2;
plot height*weight / haxis=45 to 155 by 10
	vaxis=48 to 78 by 6
	hminor=1
	regeqn;
run;

quit;

/* SIMPLE BUBBLE PLOT */
goptions reset=all border;

data jobs;
	length eng $5;
	input eng dollars num;
	datalines;
Civil 27308 73273
Aero  29844 70192
Elec  22920 89382
Mech  32816 19601
Chem  28116 25541
Petro 18444 34833
;

title1 "Member Profile";
title2 "Salaries and Number of Member Engineers";
axis1 offset=(5,5);

proc gplot data=jobs;
	format dollars dollar9.;
	bubble dollars*eng=num / haxis=axis1;
run;

quit;

/* LABELING AND SIZING BUBBLE PLOTS */
goptions reset=all border;

data jobs;
	length eng $5;
	input eng dollars num;
	datalines;
Civil 27308 73273
Aero  29844 70192
Elec  22920 89382
Mech  32816 19601
Chem  28116 25541
Petro 18444 34833
;

title1 "Member Profile";
title2 "Salaries and Number of Member Engineers";
axis1 label=none
	offset=(5,5);
axis2 order=(0 to 40000 by 10000)
	label=none;

proc gplot data=jobs;
	format dollars dollar9. num comma7.0;
	bubble dollars*eng=num / haxis=axis1
		vaxis=axis2
		vminor=1
		bcolor=darkred
		blabel
		bsize=3;
run;

quit;

/* ADDING A RIGHT VERTICAL AXIS */
goptions reset=all border;

data jobs;
	length eng $5;
	input eng dollars num;
	datalines;
Civil 27308 73273
Aero  29844 70192
Elec  22920 89382
Mech  32816 19601
Chem  28116 25541
Petro 18444 34833
;

data jobs2;
	set jobs;
	yen=dollars*125;
run;

title1 "Member Profile";
title2 "Salaries and Number of Member Engineers";
axis1 label=none 
	offset=(5,5);

proc gplot data=jobs2;
	format dollars dollar7. num yen comma9.0;
	bubble dollars*eng=num / haxis=axis1
		vaxis=10000 to 40000 by 10000
		hminor=0
		vminor=1
		blabel;
	bubble2 yen*eng=num / vaxis=1250000 to 5000000 by 1250000
		vminor=1;
run;

quit;

/* GENERATING AN OVERLAY PLOT */
goptions reset=all border;

data stocks;
	input year high low @@;
	datalines;
1956  521.05  462.35 1957  520.77  419.79
1958  583.65  436.89 1959  679.36  574.46
1960  685.47  568.05 1961  734.91  610.25
1962  726.01  535.76 1963  767.21  646.79
1964  891.71  768.08 1965  969.26  840.59
1966  995.15  744.32 1967  943.08  786.41
1968  985.21  825.13 1969  968.85  769.93
1970  842.00  631.16 1971  950.82  797.97
1972 1036.27 889.15 1973 1051.70  788.31
1974  891.66  577.60 1975  881.81  632.04
1976 1014.79  858.71 1977  999.75  800.85
1978  907.74  742.12 1979   897.61  796.67
1980 1000.17  759.13 1981 1024.05  824.01
1982 1070.55  776.92 1983 1287.20 1027.04
1984 1286.64 1086.57 1985 1553.10 1184.96
1986 1955.57 1502.29 1987 2722.42 1738.74
1988 2183.50 1879.14 1989 2791.41 2144.64
1990 2999.75 2365.10 1991 3168.83 2470.30
1992 3413.21 3136.58 1993 3794.33 3241.95
1994 3978.36 3593.35 1995 5216.47 3832.08
;

title1 "Dow Jones Yearly Highs and Lows";
footnote1 j=l " Source: 1997 World Almanac"
;
symbol1 interpol=join
	value=dot
	color=_style_;
symbol2 interpol=join
	value=C
	font=marker
	color=_style_;
axis1 order=(1955 to 1995 by 5) offset=(2,2)
	label=none
	major=(height=2)
	minor=(height=1)
;
axis2 order=(0 to 6000 by 1000) offset=(0,0)
	label=none
	major=(height=2)
	minor=(height=1)
;
legend1 label=none
	position=(top center inside)
	mode=share;

proc gplot data=stocks;
	plot high*year low*year / overlay legend=legend1
		vref=1000 to 5000 by 1000
		lvref=2
		haxis=axis1 hminor=4
		vaxis=axis2 vminor=1;
run;

quit;

/* FILLING AREAS IN AN OVERLAY PLOT */
goptions reset=all border;

data stocks;
	input year high low @@;
	datalines;
1956  521.05  462.35 1957  520.77  419.79
1958  583.65  436.89 1959  679.36  574.46
1960  685.47  568.05 1961  734.91  610.25
1962  726.01  535.76 1963  767.21  646.79
1964  891.71  768.08 1965  969.26  840.59
1966  995.15  744.32 1967  943.08  786.41
1968  985.21  825.13 1969  968.85  769.93
1970  842.00  631.16 1971  950.82  797.97
1972 1036.27  889.15 1973 1051.70  788.31
1974  891.66  577.60 1975  881.81  632.04
1976 1014.79  858.71 1977  999.75  800.85
1978  907.74  742.12 1979  897.61  796.67
1980 1000.17  759.13 1981 1024.05  824.01
1982 1070.55  776.92 1983 1287.20 1027.04
1984 1286.64 1086.57 1985 1553.10 1184.96
1986 1955.57 1502.29 1987 2722.42 1738.74
1988 2183.50 1879.14 1989 2791.41 2144.64
1990 2999.75 2365.10 1991 3168.83 2470.30
1992 3413.21 3136.58 1993 3794.33 3241.95
1994 3978.36 3593.35 1995 5216.47 3832.08
;
run;

title1 "Dow Jones Yearly Highs and Lows";
footnote1 j=l " Source: 1997 World Almanac";
symbol1 interpol=join;
axis1 order=(1955 to 1995 by 5) offset=(2,2)
	label=none
	major=(height=2)
	minor=(height=1);
axis2 order=(0 to 6000 by 1000) offset=(0,0)
	label=none
	major=(height=2)
	minor=(height=1);

proc gplot data=stocks;
	plot low*year high*year / overlay
		haxis=axis1
		hminor=4
		vaxis=axis2
		vminor=1
		caxis=black
		areas=2;
run;

quit;