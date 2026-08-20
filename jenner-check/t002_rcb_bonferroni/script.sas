/* Adapted from RCB-Latin_Sq-Split_Plot-RMD.sas, "Question 2" block:
   randomized complete block design (5 blocks x 3 diets) with GLM ANOVA
   and Bonferroni simultaneous confidence intervals on diet means.
   Data and PROC options unchanged from source. */
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
proc glm data=data2 plots=intplot;
	class Diet Block;
	model Fat = Diet Block;
	means Diet / bon cldiff;
run;
