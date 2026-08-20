/* Adapted from RCB-Latin_Sq-Split_Plot-RMD.sas, "Question 1" block:
   nested RCB design (Bar_Size within Vendor, Heat_Level nested in Vendor)
   with GLM TEST statements for the nested error terms. Data unchanged
   from source (Vendor/Bar_Size/Heat_Level/Rep/Strength bar-strength trial). */
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

proc glm data=data1;
    class Bar_Size Vendor Heat_Level Rep;
    model Strength = Bar_Size Vendor Bar_Size*Vendor Heat_Level(Vendor) 
			Bar_Size*Heat_Level(Vendor);
		test h=Bar_size e=Bar_Size*Heat_Level(Vendor);
		test h=Vendor e=Heat_Level(Vendor);
		test h=Heat_Level(Vendor) e=Bar_Size*Heat_Level(Vendor);
		test h=Bar_Size*Vendor e=Bar_Size*Heat_Level(Vendor);
run;
