
data fruits;
   input day: $11. fruit $;
cards;
1-Monday Apples
3-Wednesday Bananas
4-Thursday Peaches
6-Saturday Pears
;
run;

title "one to one left";
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

title "one to one right";
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

proc sql;
   title 'work.Staff2';
   select * from work.staff2;
   title;


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

proc sql;
   title 'work.Schedule2';
   select * from work.schedule2;
   title;

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

proc sql;
   title 'work.Superv2';
   select * from work.superv2
   title;


libname proclib 'SAS-library';

data WORK.houses;
input Style $ 1-8 SqFeet 15-18;
datalines;
CONDO          900
CONDO         1000
RANCH         1200
RANCH         1400
SPLIT         1600
SPLIT         1800
TWOSTORY      2100
TWOSTORY      3000
TWOSTORY      1940
TWOSTORY      1860
;