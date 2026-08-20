/* Adapted from RCB-Latin_Sq-Split_Plot-RMD.sas, "Question 5" block:
   soil-CO2 trial data summarized by PROC MEANS (BY Density / BY Moisture,
   CLASS Day) into per-day treatment means, then plotted with PROC SGPLOT
   (grouped SCATTER + SERIES over Day). Data and logic unchanged from
   source. */
data data5;
	input Density Moisture Unit Day CO2;
	cards;
	1.1 0.10 1 1 2.70
	1.1 0.10 1 2 0.34
	1.1 0.10 1 3 0.11
	1.1 0.10 2 1 2.90
	1.1 0.10 2 2 1.57
	1.1 0.10 2 3 1.25
	1.1 0.20 3 1 5.20
	1.1 0.20 3 2 5.04
	1.1 0.20 3 3 3.70
	1.1 0.20 4 1 3.60
	1.1 0.20 4 2 3.92
	1.1 0.20 4 3 2.69
	1.1 0.24 5 1 4.00
	1.1 0.24 5 2 3.47
	1.1 0.24 5 3 3.47
	1.1 0.24 6 1 4.10
	1.1 0.24 6 2 3.47
	1.1 0.24 6 3 2.46
	1.4 0.10 1 1 2.60
	1.4 0.10 1 2 1.12
	1.4 0.10 1 3 0.90
	1.4 0.10 2 1 2.20
	1.4 0.10 2 2 0.78
	1.4 0.10 2 3 0.34
	1.4 0.20 3 1 4.30
	1.4 0.20 3 2 3.36
	1.4 0.20 3 3 3.02
	1.4 0.20 4 1 3.90
	1.4 0.20 4 2 2.91
	1.4 0.20 4 3 2.35
	1.4 0.24 5 1 1.90
	1.4 0.24 5 2 3.02
	1.4 0.24 5 3 2.58
	1.4 0.24 6 1 3.00
	1.4 0.24 6 2 3.81
	1.4 0.24 6 3 2.69
	1.6 0.10 1 1 2.00
	1.6 0.10 1 2 0.67
	1.6 0.10 1 3 0.22
	1.6 0.10 2 1 3.00
	1.6 0.10 2 2 0.78
	1.6 0.10 2 3 0.22
	1.6 0.20 3 1 3.80
	1.6 0.20 3 2 2.80
	1.6 0.20 3 3 2.02
	1.6 0.20 4 1 2.60
	1.6 0.20 4 2 3.14
	1.6 0.20 4 3 2.46
	1.6 0.24 5 1 1.30
	1.6 0.24 5 2 2.69
	1.6 0.24 5 3 2.46
	1.6 0.24 6 1 0.50
	1.6 0.24 6 2 0.34
	1.6 0.24 6 3 0.00
	;
run;
proc sort data=data5 out=sort5;
	by Density Moisture Day;
run;
*Density;
proc means data=sort5 mean order=data;
	by Density;
    class Day;
    var CO2;
    output out=means5a mean=MeanCO2;
run;
*Plot;
proc sgplot data=means5a;
	scatter x=Day y=MeanCO2 / group=Density markerattrs=(symbol=diamondfilled);
    series x=Day y=MeanCO2 / group=Density;
    xaxis label='Day';
    yaxis label='Mean CO2';
    keylegend / title='Density Level';
run;

*Moisture;
proc sort data=data5 out=sort5b;
	by Moisture Density Day;
run;
proc means data=sort5b mean order=data;
	by Moisture;
	class Day;
    var CO2;
    output out=means5b mean=MeanCO2;
run;
*Plot;
proc sgplot data=means5b;
	scatter x=Day y=MeanCO2 / group=Moisture markerattrs=(symbol=diamondfilled);
    series x=Day y=MeanCO2 / group=Moisture;
    xaxis label='Day';
    yaxis label='Mean CO2';
    keylegend / title='Moisture Level';
run;
