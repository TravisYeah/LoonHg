
/***** NDMMF SAS Directives for TCU Study -- March 25, 2014   *****/ 
/******************************************************************/ 
/*****       Using the new interval-censoring approach        *****/
/******************************************************************/


/***** STEP 1 :  Input Data and Create Computed Variables      *****/ 
/*******************************************************************/ 
 
data a1a ; infile 'C:\Applications\TCU_Study\March2014a\SAS_Run_C\RunC.csv' dlm=','; 
input Rec $ DL Hg length SPC Event Wt SamInEve;

   upper = log(hg*1000+1); 
   if DL = 1 then lower = 0; else lower = upper; 
   if length > 0 then llength = log(length+1); else delete; 
   if SamInEve = 1 then delete;
   if Wt < 2 then Wt1 = 1; else Wt1 = Wt;

run;


/***** STEP 2 :  Execute the "lifereg" procedure               *****/ 
/*******************************************************************/ 


proc lifereg noprint data=a1a outest=b6a ;
   Class SPC Event ;
   Weight Wt1 ;
   Model (lower, upper) = SPC*llength Event / d=normal noint ;
   output out=M6Pred p=pred6 ;
run ;


/***** STEP 3 : Put the Parameter Data into a Convenient Format*****/ 
/*******************************************************************/ 


proc transpose data=b6a;
   var _LNLIKE_ Intercept SPC1llength--SPC291llength Event1--Event5626 _SCALE_;

run ;