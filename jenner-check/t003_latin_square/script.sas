/* Adapted from RCB-Latin_Sq-Split_Plot-RMD.sas, "Question 3" block:
   5x5 Latin square (Signal is the treatment, Intersection/Period are the
   two blocking factors). GLM ANOVA with an ESTIMATE contrast between two
   signal levels, Bonferroni means, and LSMEANS with BYLEVEL STDERR;
   followed by a "multiple comparisons with the best" (MCB) table built
   from PROC MEANS treatment means. Data and logic unchanged from source. */
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
proc glm data=data3;
	class Intersection Period Signal;
	model Resp = Signal Intersection Period;
		estimate 'A VS B' Signal 1 -1 0 0 0;
	means Signal / bon cldiff;
	lsmeans Signal / bylevel stderr; 
run;

proc means data=data3 noprint;
    by Signal;
    var Resp;
    output out=means3 mean=MeanResp;
run;
proc print data=means3;
run;
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
