ods graphics off;
proc reg data=sashelp.class;
   model Weight = Height;
run; quit;

ods graphics on;
proc reg data=sashelp.class;
   model Weight = Height;
run; quit;
ods graphics off;

ods graphics on;
ods 
proc reg data=sashelp.class;
   model Weight = Height;
run; quit;
ods graphics off;

ods graphics on;
ods html path="c:\MyDemos\"
         gpath="c:\MyDemos\images" (url="http://www.sas.com/images/")
         body="barGraph.htm"
         style=Seaside;
proc reg data=sashelp.class;
   model Weight = Height;
run; quit;
ods html close;
ods graphics off;


proc lifetest data=sashelp.BMT plots=survival(cb=hw test);
   time T * Status(0);
   strata Group / test=logrank;
run;

ods graphics on;
proc lifetest data=sashelp.BMT plots=survival(cb=hw test);
   time T * Status(0);
   strata Group / test=logrank;
run;
ods graphics off;

ods graphics on;
ods excel file="c:\MyDemos\EG_Files\survival.xlsx";
proc lifetest data=sashelp.BMT plots=survival(cb=hw test);
   time T * Status(0);
   strata Group / test=logrank;
run;
ods excel close;
ods graphics off;

ods graphics on;
ods excel file="c:\MyDemos\EG_Files\survival.xlsx" style=Harvest;
proc lifetest data=sashelp.BMT plots=survival(cb=hw test);
   time T * Status(0);
   strata Group / test=logrank;
run;
ods excel close;
ods graphics off;

ods graphics on;
ods excel file="c:\MyDemos\EG_Files\survival.xlsx" 
              options(embedded_titles='yes'
                      embedded_footnotes='yes'
                      tab_color='purple')
              style=Harvest;
title "Survival Analysis Title";
footnote "An example of ODS to Excel output";
proc lifetest data=sashelp.BMT plots=survival(cb=hw test);
   time T * Status(0);
   strata Group / test=logrank;
run;
ods excel close;
ods graphics off;


