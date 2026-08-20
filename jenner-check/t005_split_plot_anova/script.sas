/* Adapted from RCB-Latin_Sq-Split_Plot-RMD.sas, "Question 5" block:
   split-split-plot soil-CO2 trial (Density = whole plot, Moisture = split
   plot nested in Density via Unit, Day = repeated sub-sub-plot measure).
   Unit is collapsed from 6 levels to 2 true replicates per the source's
   own recoding, then PROC ANOVA fits the split-split-plot model with TEST
   statements against the correct nested error terms (Unit*Density and
   Unit*Moisture(Density)). Data and logic unchanged from source; one
   mechanical fix applied -- the source's "proc sort data5a;" (line 408 of
   the original file) is missing "=" before the dataset name, a typo the
   same script writes correctly two lines later ("proc sort data=data5a;"
   at its own line 481); corrected here to "proc sort data=data5a;". */
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

*/ ANOVA (split-plot);
*/With replicates;
*/Convert Unit into replicates;
data data5a;
	set data5;
	if Unit=3 then Unit=1;
	if Unit=5 then Unit=1;
	if Unit=4 then Unit=2;
	if Unit=6 then Unit=2;
run;
proc sort data=data5a;
	by Unit Density Moisture Day;
	run;

*/Run ANOVA for split-split plot analysis and run tests;
proc anova data=data5a;
	class Unit Density Moisture Day;
	model CO2 = Unit Density Unit*Density
				Moisture Density*Moisture Unit*Moisture(Density)
                Day Density*Day Moisture*Day Density*Moisture*Day;
	test h=Unit e=Unit*Density;
	test h=Density e=Unit*Density; 
	test h=Moisture e=Unit*Moisture(Density);
	test h=Density*Moisture e=Unit*Moisture(Density);
run;
