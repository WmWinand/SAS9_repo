%macro Check_SLA_Count;
	%if &SLA_Count>0 %then
		%do;

			/* Create EMAIL Output file from a CAS table. */
			options emailsys=smtp emailhost=mailhost.na.sas.com;
			filename mymail email To=("first.last@sas.com" 
				"Name2 <first2.last2@Sas.com>") 
				From="Name ITViya <xxxxxxxx@itviyap102.itviya.sashq-d.openstack.sas.com>" 
				subject="SN_SLA_Alerts_&sysdate9" ct="text/html";
			ods tagsets.msoffice2k(id=email) 
				file=mymail(title="SN_SLA_Alerts_&sysdate9") style=printer;
			Title "ServiceNow Tickets Exceeding SLA (24 Hours) for review.";
			Title2 "Updated SN Ticket Data as of &File_Create_DT";

			Proc Print Data=Work.SN_SLA_Alerts n label;
				Id 'number'n;
				Var Ticket_URL opened_at closed_at SLA_Hours dv_state dv_assigned_to 
					dv_u_customer short_description;
				label 'number'n='Ticket Number' Ticket_URL='Ticket URL' 
					opened_at='Opened Date/Time' closed_at='Closed Date/Time' 
					SLA_Hours='SLA Hours' dv_state='State' dv_assigned_to='SE Assigned' 
					dv_u_customer='Customer (CSM)' short_description='Short Desc';
			Run;

			ods tagsets.msoffice2k(id=email) close;
		%end;
%mend Check_SLA_Count;

/* END - Set up Macro to Check for existing table, then process.  */
/*********************************************************************************************/
%Check_SLA_Count;