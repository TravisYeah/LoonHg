PROC IMPORT OUT= WORK.BLOODHG 
            DATAFILE= "N:\KKenow_work\Richard Erickson Analyses\AA- MN l
oon patterns of contaminant exposure\Data\FinalDataAnalysis\LoonHGblood.
csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;
