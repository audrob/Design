/* Adapted from RCB-Latin_Sq-Split_Plot-RMD.sas, "Question 4" block:
   RCB nitrogen-response trial (2 blocks x 5 nitrogen levels x 2 reps).
   GLM fits the block/nitrogen model with orthogonal polynomial CONTRAST
   statements (linear/quadratic/cubic/quartic trend in nitrogen). Data and
   logic unchanged from source. (The source file's second GLM in this block
   adds Rep as its own CLASS term on top of Block*Nitrogen*Rep, which
   consumes all 19 residual df on this 20-obs design -- a saturated/aliased
   model by construction, not something a compatibility bundle should pin.) */
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

proc glm data=data4;
	class Block Nitrogen;
	model Response = Block Nitrogen Block*Nitrogen;
	contrast 'Nitrogen Linear'    Nitrogen -2 -1  0  1  2;
	contrast 'Nitrogen Quadratic' Nitrogen  2 -1 -2 -1  2;
	contrast 'Nitrogen Cubic'     Nitrogen -1  2  0 -2  1;
	contrast 'Nitrogen Quartic'   Nitrogen  1 -4  6 -4  1;
run;
