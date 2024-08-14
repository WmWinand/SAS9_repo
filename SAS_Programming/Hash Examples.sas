/* CREATE WORK.COLORS */
data WORK.COLORS ;
  input @1 Type $8.
       @10 Exterior_Color $10.
       @21 Interior_Color $10. ;
  datalines ;
Hybrid   White      Black
SUV      White      Blue
Sedan    White      Black
Sports   Red        Gold
Truck    Black      Black
Wagon    Blue       Black
;
run ;

/* PRINT WORK.COLORS */
proc print data=WORK.COLORS ;
  var Type Exterior_Color Interior_Color ;
run ;

/* EXAMPLE 1: HASH SEARCH */
data hash_search(keep=Origin Type Make Model MSRP) ;
  if 0 then set SASHELP.CARS ;
  Origin = 'USA' ;
  if _n_ = 1  then do ;
     declare Hash HCars (dataset:'SASHELP.CARS') ;
     HCars.DefineKey ('ORIGIN') ;
     HCars.DefineData ('Type','Make','Model','MSRP') ;
     HCars.DefineDone () ;
  end ;
  if HCars.find() = 0 then output ;
  stop ;
run ;

proc print data=hash_search ;
  var Origin Type Make Model MSRP ;
run ;

/* EXAMPLE 2: MATCH-MERGE */
data hash_match_merge ;
  if 0 then set WORK.COLORS ;
  if _n_ = 1  then do ;
     declare Hash HColors (dataset:'WORK.COLORS') ;
     HColors.DefineKey ('TYPE') ;
     HColors.DefineData ('Exterior_Color',
                         'Interior_Color') ;
     HColors.DefineDone () ;
  end ;
  set SASHELP.CARS (Keep=Origin Type Make Model MSRP) ;
  if HColors.find(key:TYPE) = 0 then output ;
run ;

proc print data=hash_match_merge ;
  var Origin Type Make Model Exterior_Color 
      Interior_Color MSRP ;
run ;

/* EXAMPLE 3: HASH SORT - ASCENDING */
data _null_ ;
  if 0 then set SASHELP.CARS (Keep=Origin Type Make Model MSRP) ;
  if _n_ = 1 then do ;
     declare Hash HSort (ordered:'a') ; /* declare sort order */
     HSort.DefineKey ('Make','Model','MSRP') ; /* define key */
     HSort.DefineData ('Origin',
                       'Type',
                       'Make',
                       'Model',
                       'MSRP') ; /* define columns of data */
     HSort.DefineDone () ; /* complete hash table definition */
  end ;
  set SASHELP.CARS end=eof ;
  HSort.add () ; /* add data with key to hash object */
  if eof then
     HSort.output(dataset:'Hash_Sorted') ; /* sorted dataset */
run ;

proc print data=Hash_Sorted ;
  var Origin Type Make Model MSRP ;
run ;

/* EXAMPLE 4: HASH SORT - DESCENDING */
data _null_ ;
  if 0 then set SASHELP.CARS (Keep=Origin Type Make Model MSRP) ;
  if _n_ = 1 then do ;
     declare Hash HSort (ordered:'d') ; /* declare sort order */
     HSort.DefineKey ('Make','Model','MSRP') ; /* define key */
     HSort.DefineData ('Origin',
                       'Type',
                       'Make',
                       'Model',
                       'MSRP') ; /* define columns of data */
     HSort.DefineDone () ; /* complete hash table definition */
  end ;
  set SASHELP.CARS end=eof ;
  HSort.add () ; /* add data with key to hash object */
  if eof then
     HSort.output(dataset:'Hash_Sorted') ; /* sorted dataset */
run ;

proc print data=Hash_Sorted ;
  var Origin Type Make Model MSRP ;
run ;