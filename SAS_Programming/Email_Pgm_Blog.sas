/* Email Notification - Viya Jobs */
/* https://medium.com/@sridharamirneni/email-notification-sas-viya-jobs-b527261d8517 */

FILENAME mail EMAIL TO=(t.winand@sas.com) CC=() SUBJECT=”Cars Alert” CONTENT_TYPE=”text/html”;
ods msoffice2k file=mail style=sasweb;

data _null_;
	y=datetime();
	format y datetime20.;
	file print;
	put “Hi all, ”;
	put;
	put “<TABLE NAME> from <DATABASE NAME> has been refreshed now. The last update 
		time was: “ y;
	put;
	put”Thanks.”;
run;

RUN;
ods msoffice2k close;