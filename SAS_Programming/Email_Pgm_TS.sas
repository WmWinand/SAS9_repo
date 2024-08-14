proc options group=email; run;
proc options option=config; run;

options emailsys=smtp;
options emailhost=localhost;
options emailackwait=60;
options emailport=25;
options emailauthprotocol=NONE;
options emailid="sasdemo";
 
filename myemail EMAIL DEBUG
from="t.winand@sas.com"
sender="t.winand@sas.com"
to="t.winand@sas.com"
subject="Test"
attach="/greenmonthly-export/ssemonthly/homes/T.Winand@sas.com/Data/cars_alert.pdf";
 
data _null_;
   file myemail;
   put "Hello";
run;
