/* SGPLOT EXAMPLES */
title "Scatter Plot of Student Height versus Weight";
proc sgplot data=sashelp.class;
  scatter x=height y=weight;
  ellipse x=height y=weight;
run;
title;

title "Histogram of Patient Cholesterol";
proc sgplot data=sashelp.heart;
  histogram cholesterol;
  density cholesterol;
  density cholesterol / type=kernel;
run;
title;

title "Power Generation (GWh)";
proc sgplot data=sashelp.electric(where=
  (year >= 2001 and customer="Residential"));
  xaxis type=discrete;
  series x=year y=coal / datalabel;
  series x=year y=naturalgas / 
      datalabel y2axis;
run;
title;


/* SGPANEL EXAMPLES */
title "Yearly Sales by Product";
proc sgpanel data=sashelp.prdsale;
  panelby year / novarname columns=1;
  hbar product / response=actual;
run;
title;

title "Histogram Panel of Patient Cholesterol by Gender";
proc sgpanel data=sashelp.heart;
    panelby sex;
    histogram cholesterol;
    density cholesterol;
    density cholesterol / type=kernel;
run;
title;

title "Sales - Actual & Prediction - Panel by Quarter";
proc sgpanel data=sashelp.prdsale;
	by year;
	panelby quarter;
	rowaxis label="Sales";
	vbar product / response=predict transparency=0.3;
	vbar product / response=actual barwidth=0.5 transparency=0.3;
run;
title;

title "Cholesterol by Smoking Status - Panel by Gender";
proc sgpanel data=sashelp.heart;
    format ageatstart age.;
    panelby SEX / columns=1
    layout=rowlattice novarname;
    hbox cholesterol / category=SMOKING_STATUS;
run;
title;

/**/
/*proc sgpanel data=sashelp.heart;*/
/*    format ageatstart age.;*/
/*    panelby sex / columns=1*/
/*    layout=rowlattice novarname;*/
/*    hbox cholesterol / category=ageatstart;*/
/*run;*/


/* SGSCATTER EXAMPLES */
title "Scatter Plot Matrix - Systolic Diastolic Cholesterol ";
proc sgscatter data=sashelp.heart(obs=250); 
     matrix systolic diastolic cholesterol;
run;
title;

title "Scatter Plot Matrix - Systolic Diastolic Cholesterol - #2 ";
proc sgscatter data=sashelp.heart(obs=250); 
     matrix systolic diastolic cholesterol /
     ellipse=(alpha=0.05type=predicted)
     diagonal=(histogram normal);
run;
title;

title "Comparison of Height and Weight by Gender";
proc sgscatter data=sashelp.class;
 compare x=age y=(weight height) / loess group=sex;
run;
title;








