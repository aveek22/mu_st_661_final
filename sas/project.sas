proc import out = bank
	datafile = "/home/u57836410/ST662/bank.csv"
	dbms = csv replace;
	getnames = yes;
run;

/* Checking the contents of the dataset to understand the variable types */

proc contents data= bank;
run;

proc freq data=bank;
tables contact default education housing job loan marital month poutcome/ nocum nopercent;
run;

/* Visualising Data by variables relating to y */

title "Proportion of Yes vs. No";
proc sgplot data=bank;
vbar y;
run;


title "Jobs subscription rate";
proc sgplot data=bank pctlevel=group;
vbar job / group=y stat=percent missing;
run;

title "Martial status subscription rate";
proc sgplot data=bank pctlevel=group;
vbar marital / group=y stat=percent missing;
run;

title "Personal loan subscription rate";
proc sgplot data=bank pctlevel=group;
vbar loan / group=y stat=percent missing;
run;

title "Communication type subscription rate";
proc sgplot data=bank pctlevel=group;
vbar contact / group=y stat=percent missing;
run;

title "Last day of contact subscription rate";
proc sgplot data=bank pctlevel=group;
vbar day / group=y stat=percent missing;
run;

title "Last month of contact subscription rate";
proc sgplot data=bank pctlevel=group;
vbar month / group=y stat=percent missing;
run;

title "Campaign subscription rate";
proc sgplot data=bank pctlevel=group;
vbar campaign / group=y stat=percent missing;
run;

title "Outcome of previous marketing campaign subscription rate";
proc sgplot data=bank pctlevel=group;
vbar poutcome / group=y stat=percent missing;
run;

/* Checking for outliers */ 

proc sgplot data = bank;
	vbox duration / category= y;
run;

proc sgplot data = bank;
	vbox campaign / category= y;
run;

proc means data = bank n nmiss mean max min;
run;

/* Splitting dataset into test and training datasets */

proc surveyselect data = bank
         method= srs
         samprate= 0.3
         out=samples_train_valid outall;
run;

data training;
set samples_train_valid;
if Selected=1;
run;

data test;
set samples_train_valid;
if Selected=0;
run;

proc print data=test;
run;

/* Logistic regression, putting categorical variables into class statement */

proc logistic data = bank descending plots=EFFECT plots=ROC plots=oddsratio; 
       class y job marital education default housing loan contact month poutcome / param=glm; 
       model y (event='yes') = job marital education default balance housing loan contact day month duration campaign pdays previous poutcome/ link=logit
       lackfit selection=backward slstay=0.05 hierarchy=single technique=fisher;
       output out = temp predicted=estprob l=lower95 u=upper95;
run;


proc logistic data = test descending plots=EFFECT plots=ROC plots=oddsratio; 
       class y job marital education default housing loan contact month poutcome / param=glm; 
       model y (event='yes') = job marital education default balance housing loan contact day month duration campaign pdays previous poutcome/ link=logit
       lackfit selection=backward slstay=0.05 hierarchy=single technique=fisher;
       output out = temp1 predicted=estprob l=lower95 u=upper95;
run;

proc logistic data = training descending plots=EFFECT plots=ROC plots=oddsratio; 
       class y job marital education default housing loan contact month poutcome / param=glm; 
       model y (event='yes') = job marital education default balance housing loan contact day month duration campaign pdays previous poutcome/ link=logit
       lackfit selection=backward slstay=0.05 hierarchy=single technique=fisher;
       output out = temp2 predicted=estprob l=lower95 u=upper95;
run;



