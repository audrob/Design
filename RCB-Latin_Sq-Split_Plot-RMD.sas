ods graphics on;
run;
*/ Question 1;
DATA data1;
    INPUT Vendor $ Heat_Level Bar_Size $ Rep $ Strength;
    CARDS;
    1 1 1   1 1.230
    1 1 1   2 1.259
    1 2 1   1 1.346
	1 2 1   2 1.400
    1 3 1   1 1.235
	1 3 1   2 1.206
    1 1 1.5 1 1.316
    1 1 1.5 2 1.300
	1 2 1.5 1 1.329
	1 2 1.5 2 1.362
	1 3 1.5 1 1.250
	1 3 1.5 2 1.239
	1 1 2   1 1.287
	1 1 2   2 1.292
	1 2 2   1 1.346
	1 2 2   2 1.382
	1 3 2   1 1.273
	1 3 2   2 1.215
	2 1 1   1 1.301
	2 1 1   2 1.263
	2 2 1   1 1.346
	2 2 1   2 1.392
	2 3 1   1 1.315
	2 3 1   2 1.320
	2 1 1.5 1 1.274
	2 1 1.5 2 1.268
	2 2 1.5 1 1.384
	2 2 1.5 2 1.375
	2 3 1.5 1 1.346
	2 3 1.5 2 1.357
	2 1 2   1 1.247
	2 1 2   2 1.215
	2 2 2   1 1.362
	2 2 2   2 1.328
	2 3 2   1 1.336
	2 3 2   2 1.342
	3 1 1   1 1.247
	3 1 1   2 1.296
	3 2 1   1 1.275
	3 2 1   2 1.268
	3 3 1   1 1.324
	3 3 1   2 1.315
	3 1 1.5 1 1.273
	3 1 1.5 2 1.264
	3 2 1.5 1 1.260
	3 2 1.5 2 1.265
	3 3 1.5 1 1.392
	3 3 1.5 2 1.364
	3 1 2   1 1.301
	3 1 2   2 1.262
	3 2 2   1 1.280
	3 2 2   2 1.271
	3 3 2   1 1.319
	3 3 2   2 1.323
	;
proc sort data=data1;
	by Bar_Size Vendor Heat_Level Rep;
run;

*/ANOVA table and test hypotheses;
proc glm data=data1;
    class Bar_Size Vendor Heat_Level Rep;
    model Strength = Bar_Size Vendor Bar_Size*Vendor Heat_Level(Vendor) 
			Bar_Size*Heat_Level(Vendor);
		test h=Bar_size e=Bar_Size*Heat_Level(Vendor);
		test h=Vendor e=Heat_Level(Vendor);
		test h=Heat_Level(Vendor) e=Bar_Size*Heat_Level(Vendor);
		test h=Bar_Size*Vendor e=Bar_Size*Heat_Level(Vendor);
run;


*/ Question 2;
data data2;
	input Block $ Diet $ Fat;
	cards;
	1 1 0.73
	1 2 0.67
	1 3 0.15
	2 1 0.86
	2 2 0.75
	2 3 0.21
	3 1 0.94
	3 2 0.81
	3 3 0.26
	4 1 1.40
	4 2 1.32
	4 3 0.75
	5 1 1.62
	5 2 1.41
	5 3 0.78
	;
	run;
*/ ANOVA and bonferroni t simultaneous intervals;
proc glm data=data2 plots=intplot;
	class Diet Block;
	model Fat = Diet Block;
	means Diet / bon cldiff;
run;


*/ Question 3;
data data3;
	input Intersection Period Signal $ Resp;
	cards;
	1 1 A 15.2
	1 2 B 33.8
	1 3 C 13.5
	1 4 D 27.4
	1 5 E 29.1
	2 1 B 16.5
	2 2 C 26.5
	2 3 D 19.2
	2 4 E 25.8
	2 5 A 22.7
	3 1 C 12.1
	3 2 D 31.4
	3 3 E 17.0
	3 4 A 31.5
	3 5 B 30.2
	4 1 D 10.7
	4 2 E 34.2
	4 3 A 19.5
	4 4 B 27.2
	4 5 C 21.6
	5 1 E 14.6
	5 2 A 31.7
	5 3 B 16.7
	5 4 C 26.3
	5 5 D 23.8
	;
run;
proc sort data=data3 out=data3;
	by Signal Intersection Period ;
run;
*/ANOVA for latin square;
proc glm data=data3;
	class Intersection Period Signal;
	model Resp = Signal Intersection Period;
*/part b) SE and difference of signal means;
		estimate 'A VS B' Signal 1 -1 0 0 0;
	means Signal / bon cldiff;
	lsmeans Signal / bylevel stderr; 
run;

*/MC with Best Procedure;
*/Calculate treatment means;
proc means data=data3 noprint;
    by Signal;
    var Resp;
    output out=means3 mean=MeanResp;
run;
proc print data=means3;
run;
*/Create multiple comparison table;
data MCB3;
	set means3 (keep=Signal MeanResp);
	if MeanResp=20.0 then MinMean=21.14;
		else MinMean=20.0;
	Di=MeanResp-MinMean;
	M=4.050693645;
	Lower=Di-M;
	Upper=Di+M;
run;
proc print data=MCB3;
run;


*/RE of blcoking by time periods;
proc glm data=data3;
    class Intersection Period Signal;
    model Resp = Signal Intersection Period Signal*Intersection*Period;
    means Resp / n var;
run;

proc glm data=data3;
    class Intersection Signal;
    model Resp = Signal Intersection;
    means Resp / n var;
run;

*/Question 4;
data data4;
	input Block Nitrogen Rep Response;
	cards;
	1 0   1 100
	1 0   2 104
	1 50  1 124
	1 50  2 120
	1 100 1 136
	1 100 2 132
	1 150 1 137
	1 150 2 150
	1 200 1 123
	1 200 2 136
	2 0   1  99
	2 0   2 114
	2 50  1 144
	2 50  2 154
	2 100 1 142
	2 100 2 146
	2 150 1 150
	2 150 2 153
	2 200 1 146
	2 200 2 151
	;
run;
proc sort data=data4;
	by Block Nitrogen Rep;
run;

*/ ANOVA;
proc glm data=data4;
	class Block Nitrogen;
	model Response = Block Nitrogen Block*Nitrogen;
	contrast 'Nitrogen Linear'    Nitrogen -2 -1  0  1  2;
	contrast 'Nitrogen Quadratic' Nitrogen  2 -1 -2 -1  2;
	contrast 'Nitrogen Cubic'     Nitrogen -1  2  0 -2  1;
	contrast 'Nitrogen Quartic'   Nitrogen  1 -4  6 -4  1;
run;

*/ ANOVA with alternative method for error;
proc glm data=data4;
	class Block Nitrogen Rep;
	model Response = Block Nitrogen Block*Nitrogen Block*Nitrogen*Rep;
	contrast 'Nitrogen Linear'    Nitrogen -2 -1  0  1  2;
	contrast 'Nitrogen Quadratic' Nitrogen  2 -1 -2 -1  2;
	contrast 'Nitrogen Cubic'     Nitrogen -1  2  0 -2  1;
	contrast 'Nitrogen Quartic'   Nitrogen  1 -4  6 -4  1;
	test h=Block e=Block*Nitrogen*Rep;
	test h=Nitrogen e=Block*Nitrogen*Rep;
	test h=Block*Nitrogen e=Block*Nitrogen*Rep;
run;


*/Question 5;
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


*/mean CO2 for each denisity and moisture at each time;
proc means data=sort5 mean order=data;
    class Density Moisture Day;
    var CO2;
	types Density*Day Moisture*Day;
    output out=means5 mean=MeanCO2;
run;
proc means data=sort5 mean order=data;
    class Density Moisture Day;
    var CO2;
	types Density*Moisture*Day;
    output out=means5 mean=MeanCO2;
run;
proc print data=means5;
run;
proc sgplot data=means5;
	scatter x=Day y=MeanCO2 / group=Density markerattrs=(symbol=diamondfilled);
    series x=Day y=MeanCO2 / group=Density;
	scatter x=Day y=MeanCO2 / group=Moisture markerattrs=(symbol=diamondfilled);
    series x=Day y=MeanCO2 / group=Moisture;
    xaxis label='Day';
    yaxis label='Mean CO2';
    keylegend / title='Density and Moisture Level Combination';
run;

*Factor Combinations;
data data5s;
	set data5;
	if Density=1.1 and Moisture=0.10 then Trt='1.1*0.10';
	if Density=1.1 and Moisture=0.20 then Trt='1.1*0.20';
	if Density=1.1 and Moisture=0.24 then Trt='1.1*0.24';
	if Density=1.4 and Moisture=0.10 then Trt='1.4*0.10';
	if Density=1.4 and Moisture=0.20 then Trt='1.4*0.20';
	if Density=1.4 and Moisture=0.24 then Trt='1.4*0.24';
	if Density=1.6 and Moisture=0.10 then Trt='1.6*0.10';
	if Density=1.6 and Moisture=0.20 then Trt='1.6*0.20';
	if Density=1.6 and Moisture=0.24 then Trt='1.6*0.24';
run;
proc sort data=data5s out=sort5s;
	by Trt Day;
run;
*/mean CO2 for each denisity*moisture combo at each time;
proc means data=sort5s mean order=data;
	by Trt;
	class Day;
    var CO2;
    output out=means5s mean=MeanCO2;
run;

*Plot;
proc sgplot data=means5s;
	scatter x=Day y=MeanCO2 / group=Trt markerattrs=(symbol=diamondfilled);
    series x=Day y=MeanCO2 / group=Trt;
    xaxis label='Day';
    yaxis label='Mean CO2';
    keylegend / title='Density and Moisture Level Combination';
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
proc sort data5a;
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

*/Diagnostic and resid plots;
*/Create glm output;
proc glm data=data5a plots(unpack)=diagnostics;
	class Unit Density Moisture Day;
	model CO2 = Unit Density Unit*Density
				Moisture Density*Moisture Unit*Moisture(Density)
                Day Density*Day Moisture*Day Density*Moisture*Day;
	output out=glm_out predicted=yhat residual=resid student=student order=;
run;
data obs;
	set glm_out;
	order=_N_;
run;
proc print obs;run;

*/Density Means;
proc sort data=glm_out;
	by Density;
run;
proc means data=data5a;
	by Density;
	var CO2;
	output out=means5A mean=MeanCO2;
run;
proc print data=means5A;
run;
proc sort data=means5A;
	by Density;
run;
data resid_density;
    merge glm_out (keep = Density resid) means5A (keep=Density MeanCO2);
    by Density;
run;
*/ Plot residuals against Density treatment means;
proc sgplot data=resid_density;
    scatter x=MeanCO2 y=resid / group=Density markerattrs=(symbol=diamond);
	refline 0 / lineattrs=(color=black);
    xaxis label="Density Treatment Means";
    yaxis label="Residuals";
run;
data student_density;
    merge glm_out (keep = Density student) means5A (keep=Density MeanCO2);
    by Density;
run;
*/ Plot studentized residuals against Density treatment means;
proc sgplot data=student_density;
    scatter x=MeanCO2 y=student / group=Density markerattrs=(symbol=diamond);
	refline 0 / lineattrs=(color=black);
    xaxis label="Density Treatment Means";
    yaxis label="Studentized Residuals";
run;


/* Moisture treatment means*/
proc sort data=glm_out;
	by Moisture;
run;
proc sort data=data5a;
	by Moisture;
run;
proc means data=data5a;
	by Moisture;
	var CO2;
	output out=means5B mean=MeanCO2;
run;
proc sort data=means5B;
	by Moisture;
run;
data resid_moisture;
    merge glm_out (keep = Moisture resid) means5B (keep=Moisture MeanCO2);
    by Moisture;
run;
/* Plot residuals against Moisture treatment means */
proc sgplot data=resid_moisture;
    scatter x=MeanCO2 y=resid / group=Moisture markerattrs=(symbol=diamond);
	refline 0 / lineattrs=(color=black);
    xaxis label="Moisture Treatment Means";
    yaxis label="Residuals";
run;
data student_moisture;
    merge glm_out (keep = Moisture student) means5B (keep=Moisture MeanCO2);
    by Moisture;
run;
/* Plot studentized residuals against Moisture treatment means */
proc sgplot data=student_moisture;
    scatter x=MeanCO2 y=student / group=Moisture markerattrs=(symbol=diamond);
	refline 0 / lineattrs=(color=black);
    xaxis label="Moisture Treatment Means";
    yaxis label="Studentized Residuals";
run;


*/ Day means;
proc sort data=glm_out;
	by Day;
run;
proc sort data=data5a;
	by Day;
run;
proc means data=data5a;
	by Day;
	var CO2;
	output out=means5C mean=MeanCO2;
run;
proc sort data=means5C;
	by Day;
run;
data resid_day;
    merge glm_out (keep = Day resid) means5C (keep=Day MeanCO2);
    by Day;
run;
/* Plot residuals against Day of measurement means */
proc sgplot data=resid_day;
    scatter x=MeanCO2 y=resid / group=Day markerattrs=(symbol=diamond);
	refline 0 / lineattrs=(color=black);
    xaxis label="Day of Measurement Means";
    yaxis label="Residuals";
run;
data student_day;
    merge glm_out (keep = Day student) means5C (keep=Day MeanCO2);
    by Day;
run;
/* Plot studentized residuals against Day of measurement means */
proc sgplot data=student_day;
    scatter x=MeanCO2 y=student / group=Day markerattrs=(symbol=diamond);
	refline 0 / lineattrs=(color=black);
    xaxis label="Day of Measurement Means";
    yaxis label="Studentized Residuals";
run;


/* Plot residuals against treatment means */
proc sgplot data=residuals_means;
    scatter x=MeanCO2 y=resid / group=Density markerattrs=(symbol=circlefilled);
	refline 0 / lineattrs=(color=black);
	scatter x=MeanCO2 y=resid / group=Moisture markerattrs=(symbol=circlefilled);
    refline 0 / lineattrs=(color=black);
    xaxis label="Treatment Means";
    yaxis label="Residuals";
    title "Residual Plot against Treatment Means";
run;

*/ Residual plot for Treatment combination means;
proc sort data=glm_out out=glm_out;
	by Density Moisture Day;
run;
proc sort data=data5s out=sort5s2;
	by Density Moisture Day;
run;
*/mean CO2 for each denisity*moisture combo at each time;
proc means data=sort5s2 mean order=data;
	by Density Moisture;
	class Density Moisture Day Trt;
    var CO2;
    output out=means5s2 mean=MeanCO2;
run;
proc sort data=means5s2;
	by Density Moisture Day;
	run;
data residuals_means2;
    merge glm_out (keep=Unit Density Moisture Day resid) means5s2 (keep=Density Moisture Day MeanCO2 Trt);
    by Density Moisture Day;
run;
proc sgplot data=residual_means2;
	scatter x=MeansCO2 y=resid / group=Trt markerattrs=(symbol=diamondfilled);
    series x=MeanCO2 y=resid / group=Trt;
    xaxis label='Day';
    yaxis label='Mean CO2';
    keylegend / title='Density and Moisture Level Combination';
run;


*/SS Partitions. Orthogonal Polynomial Contrasts;
proc glm data=data5a;
	class Unit Density Moisture Day;
	model CO2 = Unit Density Unit*Density
				Moisture Density*Moisture Unit*Moisture(Density)
                Day Density*Day Moisture*Day Density*Moisture*Day;
		contrast 'C Linear '      Day -1  0 1;
		contrast 'C Quadratic'    Day  1 -2 1;
		contrast 'AC Linear '     Density*Day 1 0 -1 0 0 0 -1 0 1;
		contrast 'AC Quadratic'   Density*Day  1 -2 1 -2 4 -2 1 -2 1;
		contrast 'BC Linear '     Moisture*Day 1 0 -1 0 0 0 -1 0 1;
		contrast 'BC Quadratic'   Moisture*Day  1 -2 1 -2 4 -2 1 -2 1;
		contrast 'ABC Linear '    Density*Moisture*Day -1 0 1 0 0 0 1 0 -1 
										0 0 0 0 0 0 0 0 0 1 0 -1 0 0 0 -1 0 1;
		contrast 'ABC Quadratic'  Density*Moisture*Day  1 -2 1 -2 4 -2 1 -2 1 
										-2 4 -2 4 -8 4 -2 4 -2 1 -2 1 -2 4 -2 
										1 -2 1;
run;

*/split plot with between tests + polynomial contrasts;
proc glm data=data5a;
	class Unit Density Moisture Day;
	model CO2 = Unit Density Moisture Moisture*Density Unit*Moisture*Density Day 
		 Density*Day Moisture*Day Density*Moisture*Day;
	test h=Unit Density Moisture Moisture*Density e=Unit*Moisture*Density;
		contrast 'C Linear '      Day -1  0 1;
		contrast 'C Quadratic'    Day  1 -2 1;
		contrast 'AC Linear '     Density*Day 1 0 -1 0 0 0 -1 0 1;
		contrast 'AC Quadratic'   Density*Day  1 -2 1 -2 4 -2 1 -2 1;
		contrast 'BC Linear '     Moisture*Day 1 0 -1 0 0 0 -1 0 1;
		contrast 'BC Quadratic'   Moisture*Day  1 -2 1 -2 4 -2 1 -2 1;
		contrast 'ABC Linear '    Density*Moisture*Day -1 0 1 0 0 0 1 0 -1 0 0 0 0 0 0 0 0 0 1 0 -1 0 0 0 -1 0 1;
		contrast 'ABC Quadratic'  Density*Moisture*Day  1 -2 1 -2 4 -2 1 -2 1 -2 4 -2 4 -8 4 -2 4 -2 1 -2 1 -2 4 -2 1 -2 1;
run;

*Density Resid Plots;
proc sgplot data=glm_out;
    scatter x=Density y=student / group=Density;
    xaxis label="Density";
    yaxis label="Studentized Residuals";
    title "Studentized Residuals vs. Density";
run;
proc means data=glm_out noprint;
    var CO2;
    class Density Moisture Day student;
    output out=treatment_means mean=mean_CO2;
run;
* Create scatter plot;
proc sgplot data=treatment_means;
    scatter y=student x=mean_CO2 / group=Density;
    xaxis label="Estimated Density Means";
    yaxis label="Studentized Residuals";
    title "Studentized Residuals vs. Estimated Treatment Means";
run;

*Moisture Resid Plots;
proc sgplot data=glm_out;
    scatter x=Moisture y=student / group=Moisture;
    xaxis label="Moisture";
    yaxis label="Studentized Residuals";
    title "Studentized Residuals vs. Moisture";
run;
* Create scatter plot;
proc sgplot data=treatment_means;
    scatter y=student x=mean_CO2 / group=Moisture;
    xaxis label="Estimated Moisture Means";
    yaxis label="Studentized Residuals";
    title "Studentized Residuals vs. Estimated Treatment Means";
run;


*Day Resid Plots;
proc sgplot data=glm_out;
    scatter x=Day y=student / group=Day;
    xaxis label="Day";
    yaxis label="Studentized Residuals";
    title "Studentized Residuals vs. Day";
run;
* Create scatter plot;
proc sgplot data=treatment_means;
    scatter y=student x=mean_CO2 / group=Day;
    xaxis label="Estimated Day Means";
    yaxis label="Studentized Residuals";
    title "Studentized Residuals vs. Estimated Treatment Means";
run;

*Residual versus observation order;
proc sgplot data=obs;
    scatter y=student x=order;
    xaxis label="Observation Order";
    yaxis label="Studentized Residuals";
    title "Studentized Residuals vs. Observation Order";
run;


*close out program;
ods graphics off;
run;
quit;
