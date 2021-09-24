#Overview------
#Ok so i ran the gc in two kinda-sorta batches:

#the 9-13 group, and the 9-17 group
#9-13:
#relatively straightforward because only had to restart twice nstead of 4 times. ugh
#Had to restart once because the ecd stopped giving signal but was technically on
#Had to restart the second time because i thought that I re-ran a few of the wrong vials but i think it turned out thatI had rerun the right vials
#
#CAN PROBABLY USE N2O FROM THE RERUNS IN THIS GORUP ABOVE TO LOOK AT THE EQUATION FOR RE-SAMPLED VIALS
#Also i apparently emailed them the original rerun data, then thought it was wronmg, then emailed them the 2nd time rerun data to royally confuse things

#anyways lets start by comparing the n2o for the two reruns. Start simple, with a manual observation of n2 for the same group in the two runs. or all three, i dont care. 

#9-17:
#ran once: 9-17
#reran the first time because again ecd stopped working, 9-17_2
#Didnt realize it was the very first sample causing the problem so reran again for some:  9-17_3
#Realized it was probaby the first one so reran the ones after taht again, with the first at the end: 9-17_4
#Reran just the first one but diluted as 3mL of the original vial in a flushed 11mL vial (9-17_5)


#so i guess for 9-17 the first thing is probably to read in all of the txts for them with R, then rbind them? yes ok
#maybe start with the 
#checking co2--------------
ecdon<-
read.csv(file="G:/My Drive/Studies/N_efficiency/Data/GC/Incubation_2021-09-13/bhomik_2021_09_13_NC_R1_CC_SF_PL_INC_Day_2_Blank_water_3_2.csv")%>%
	select(c(Name,Conc.,SN,X))%>%
subset(., SN%in%c(281:295))#%>%
	head()
	
	ecdoff<-
read.csv(file="G:/My Drive/Studies/N_efficiency/Data/GC/Incubation_2021-09-13/bhomik_2021_09_13_NC_R1_CC_SF_PL_INC_Day_2_Blank_water_3_2_orig_wrong.csv")%>%
	select(c(Name,Conc.,SN,X))%>%
		subset(., SN%in%c(281:295))#%>%
	head()
	
	
	my.formula <- y ~ x
merge(ecdon,ecdoff,by=c("Name","SN","X"),suffixes=c("ecd_on","ecd_off"))%>%
	subset(Name!="N2O")%>%
	print()
	dim()
	ggplot(.,aes(x=Conc.ecd_on,y=Conc.ecd_off))+facet_wrap("Name", scales = "free")+
	   stat_poly_eq(formula = my.formula, 
                aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                parse = TRUE) +       
	geom_smooth(method = "lm", se=FALSE, color=alpha("blue",alpha = 0.4), formula = my.formula) +
	geom_point()


#so they are of by the intercept, or about 7%
1580/20000
#also the slope says 1.06 so maybe 

#checking  n2o --------------
#289-295 were rerun correctly (tray 1, 1-7) and can be found in _2
#281-288 were rerun incorecrlt (tray 3, 25-32) - 
#they were actually the following 8 presumably from tray 1, 9-16 (so probably 281-288 in _2, as well as so probably 297-304 in 9-13 original )

#pick up fixing these read ins to read in what i mention above
run2<-
read.csv(file="G:/My Drive/Studies/N_efficiency/Data/GC/Incubation_2021-09-13/bhomik_2021_09_13_NC_R1_CC_SF_PL_INC_Day_2_Blank_water_3_2.csv")%>%
	select(c(Name,Conc.,SN,X))%>%
subset(., SN%in%c(281:295))#%>%
	head()
	
	run1<-
read.csv(file="G:/My Drive/Studies/N_efficiency/Data/GC/Incubation_2021-09-13_2/bhomik_2021_09_13_NC_R1_CC_SF_PL_INC_Day_2_Blank_water_3_2_orig_wrong.csv")%>%
	select(c(Name,Conc.,SN,X))%>%
		subset(., SN%in%c(281:295))#%>%
	head()
	

	my.formula <- y ~ x
merge(ecdon,ecdoff,by=c("Name","SN","X"),suffixes=c("ecd_on","ecd_off"))%>%
	subset(Name!="N2O")%>%
	ggplot(.,aes(x=Conc.ecd_on,y=Conc.ecd_off))+facet_wrap("Name", scales = "free")+
	   stat_poly_eq(formula = my.formula, 
                aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                parse = TRUE) +       
	geom_smooth(method = "lm", se=FALSE, color=alpha("blue",alpha = 0.4), formula = my.formula) +
	geom_point()

 
#read in 9-13_2 and 9-13_3 ot be able to compare n2o from two runs of the same samples----
#read in 9-13_2
Lines<-readLines(
"G:/My Drive/Studies/N_efficiency/Data/GC/Incubation_2021-09-13_2/2021-09-13_NC_R1_CC_SF_PL_INC-NC_R3_Control_Day_30.txt")
#This takes care of when fucking gcsolution decides NOT TO PRINT THAT THERE IS NO N2O, just that there IS NO DATA, ie it just skips reporting anything when N2O was 0 on the gcd in these ASCII files. As if ther were no results. I fucking hate this software
#This will have to be fixed if co2 or ch4 have  no data (but im pretty sure its ok b/c co2 and ch4 are negative instead of 0 when they are low)
ind<-which((Lines==""&lag(Lines)=="# of IDs,0")==TRUE)-1
for(i in 0:(length(ind)-1)) 
	{
		Lines<-
			append(Lines, #This will have to be fixed if co2 or ch4 have  no data (but im pretty sure its ok b/c co2 and ch4 are negative instead of 0 when they are low)
		"ID#,Name,R.Time,Area,Height,Conc.,Curve,3rd,2nd,1st,Constant",
			after=ind[i+1]+i)
}
#there should be a way to put the above and this together for the two lines but IDK how to do that higher math
ind<-which((Lines==""&lag(Lines,2)=="# of IDs,0")==TRUE)-1
for(i in 0:(length(ind)-1)) 
	{
		Lines<-
			append(Lines, #This will have to be fixed if co2 or ch4 have  no data (but im pretty sure its ok b/c co2 and ch4 are negative instead of 0 when they are low)
		"1,N2O,0,0,0,0,Default,0,0,0,0,",
				after=ind[i+1]+i) 
}
#The above picks up cases when there was 0ppm for n2o  and the fucking gcsolution software decides not to print that, and instead just leaves it blank as in 
		#[Compound Results (Ch2)]
		## of IDs,0
		#(tumbleweeds)
#instead of... (probably ought to create dummy lines within "conclines" based on coming after "# of IDs,0")
		#[Compound Results (Ch2)]
		## of IDs,1
		#ID#,Name,R.Time,Area,Height,Conc.,Curve,3rd,2nd,1st,Constant
		#1,N2O,7.314,9844,1340,0.55427,Default,0.0000000,0.0000000,5.630598e-005,0.0000000,
firstchars<-substr(Lines,1,1) #need list of first characters to distinquish kinds of lines in the ascii files
conclines<- Lines[firstchars %in% c("I","1","2")] #concentrattion lines start iwth I, 1, or 2
grpz<- cumsum(substr(conclines,1,3)=="ID#") # #only gives the lines of Lines that have useful cnocentration data
conclines<-lapply(split(conclines, grpz), #read the lines of Lines in grpz as a list of csvs
                  function(x) read.csv(text=x,header = T))
conclines<-rbindlist(conclines) #take the list of csvs and bind them
colnames(conclines) <- c(colnames(conclines)[-1],"x") # names are just shifted one to the right from the columns; move back

redo_9.13_2<- #This section creates a df with the file name split up into columns and the ch4, n2o, and co2 data togeter
 Lines[firstchars %in% c("D")]%>% #ok now we need the other important info in the Lines that start with D (the .gcd file name lines in the ASCII file for each sample)
  gsub(".*\\GCsolution(.*)\\gcd.*", "\\1", .)%>% #say "fuck off" to the .gcd part outside of these delimiters
 	 rep(.,each=3)%>%
  as.data.frame()%>%
  rename(f=".")%>%
  mutate(.,f=as.character(f))%>%
  cSplit(indt=.,splitCols =  "f",sep="\\")%>% #want all these as identifyer columns
  cbind(.,conclines)%>%
 	rename(Sample_ID_SN="f_7")%>%
	select(-c(f_1,	f_2,	f_3,	f_4	,f_5,	f_6,X3rd,X2nd,x))%>%
 	mutate(.,Sample_ID_SN=gsub("bhomik_","",Sample_ID_SN))%>%
 	mutate(.,Sample_ID_SN=gsub("_1.$","",Sample_ID_SN))%>%
 	mutate(.,Sample_ID_SN=paste0(substr(Sample_ID_SN,1,nchar(Sample_ID_SN)-4),sub("_",".",substr(Sample_ID_SN,nchar(Sample_ID_SN)-3,nchar(Sample_ID_SN)))))%>%
 	cSplit(indt=.,splitCols =  "Sample_ID_SN",sep=".")%>% #want all these as identifyer columns
 	rename(Sample_ID="Sample_ID_SN_1",SN="Sample_ID_SN_2")

#write.csv(bhomik_2021_09_13_NC_R1_CC_SF_PL_INC_Day_2_Blank_water_3,file="G:/My Drive/Studies/N_efficiency/Data/GC/Incubation_2021-09-13/bhomik_2021_09_13_NC_R1_CC_SF_PL_INC_Day_2_Blank_water_3_2XXXXXXXXX.csv")

	#*************************************************************************************************************************************************************************************************
#read in 9-13_3
Lines<-readLines(
"G:/My Drive/Studies/N_efficiency/Data/GC/Incubation_2021-09-13_3/2021-09-13_NC_R1_CC_SF_PL_INC-NC_R1_CC_SF_PL_SF.txt")
#This takes care of when fucking gcsolution decides NOT TO PRINT THAT THERE IS NO N2O, just that there IS NO DATA, ie it just skips reporting anything when N2O was 0 on the gcd in these ASCII files. As if ther were no results. I fucking hate this software
#This will have to be fixed if co2 or ch4 have  no data (but im pretty sure its ok b/c co2 and ch4 are negative instead of 0 when they are low)
ind<-which((Lines==""&lag(Lines)=="# of IDs,0")==TRUE)-1
for(i in 0:(length(ind)-1)) 
	{
		Lines<-
			append(Lines, #This will have to be fixed if co2 or ch4 have  no data (but im pretty sure its ok b/c co2 and ch4 are negative instead of 0 when they are low)
		"ID#,Name,R.Time,Area,Height,Conc.,Curve,3rd,2nd,1st,Constant",
			after=ind[i+1]+i)
}
#there should be a way to put the above and this together for the two lines but IDK how to do that higher math
ind<-which((Lines==""&lag(Lines,2)=="# of IDs,0")==TRUE)-1
for(i in 0:(length(ind)-1)) 
	{
		Lines<-
			append(Lines, #This will have to be fixed if co2 or ch4 have  no data (but im pretty sure its ok b/c co2 and ch4 are negative instead of 0 when they are low)
		"1,N2O,0,0,0,0,Default,0,0,0,0,",
				after=ind[i+1]+i) 
}
#The above picks up cases when there was 0ppm for n2o  and the fucking gcsolution software decides not to print that, and instead just leaves it blank as in 
		#[Compound Results (Ch2)]
		## of IDs,0
		#(tumbleweeds)
#instead of... (probably ought to create dummy lines within "conclines" based on coming after "# of IDs,0")
		#[Compound Results (Ch2)]
		## of IDs,1
		#ID#,Name,R.Time,Area,Height,Conc.,Curve,3rd,2nd,1st,Constant
		#1,N2O,7.314,9844,1340,0.55427,Default,0.0000000,0.0000000,5.630598e-005,0.0000000,
firstchars<-substr(Lines,1,1) #need list of first characters to distinquish kinds of lines in the ascii files
conclines<- Lines[firstchars %in% c("I","1","2")] #concentrattion lines start iwth I, 1, or 2
grpz<- cumsum(substr(conclines,1,3)=="ID#") # #only gives the lines of Lines that have useful cnocentration data
conclines<-lapply(split(conclines, grpz), #read the lines of Lines in grpz as a list of csvs
                  function(x) read.csv(text=x,header = T))
conclines<-rbindlist(conclines) #take the list of csvs and bind them
colnames(conclines) <- c(colnames(conclines)[-1],"x") # names are just shifted one to the right from the columns; move back

redo_9.13_3<- #This section creates a df with the file name split up into columns and the ch4, n2o, and co2 data togeter
 Lines[firstchars %in% c("D")]%>% #ok now we need the other important info in the Lines that start with D (the .gcd file name lines in the ASCII file for each sample)
  gsub(".*\\GCsolution(.*)\\gcd.*", "\\1", .)%>% #say "fuck off" to the .gcd part outside of these delimiters
 	 rep(.,each=3)%>%
  as.data.frame()%>%
  rename(f=".")%>%
  mutate(.,f=as.character(f))%>%
  cSplit(indt=.,splitCols =  "f",sep="\\")%>% #want all these as identifyer columns
  cbind(.,conclines)%>%
 	rename(Sample_ID_SN="f_7")%>%
	select(-c(f_1,	f_2,	f_3,	f_4	,f_5,	f_6,X3rd,X2nd,x))%>%
 	mutate(.,Sample_ID_SN=gsub("bhomik_","",Sample_ID_SN))%>%
 	mutate(.,Sample_ID_SN=gsub("_1.$","",Sample_ID_SN))%>%
 	mutate(.,Sample_ID_SN=paste0(substr(Sample_ID_SN,1,nchar(Sample_ID_SN)-4),sub("_",".",substr(Sample_ID_SN,nchar(Sample_ID_SN)-3,nchar(Sample_ID_SN)))))%>%
 	cSplit(indt=.,splitCols =  "Sample_ID_SN",sep=".")%>% #want all these as identifyer columns
 	rename(Sample_ID="Sample_ID_SN_1",SN="Sample_ID_SN_2")



#write.csv(bhomik_2021_09_13_NC_R1_CC_SF_PL_INC_Day_2_Blank_water_3,file="G:/My Drive/Studies/N_efficiency/Data/GC/Incubation_2021-09-13/bhomik_2021_09_13_NC_R1_CC_SF_PL_INC_Day_2_Blank_water_3_2XXXXXXXXX.csv")

#****************************************
#Merge them and plot
redo_9.13_2<-redo_9.13_2%>%
	select(c(Name,Conc.,Sample_ID,SN))
redo_9.13_3<-redo_9.13_3%>%
	select(c(Name,Conc.,Sample_ID,SN))

	my.formula <- y ~ x
merge(redo_9.13_2,redo_9.13_3,by=c("Name","Sample_ID","SN"),suffixes=c("_2","_3"))%>%
	subset(Name!="CH4")%>%
	ggplot(.,aes(x=Conc._3,y=Conc._2))+facet_wrap("Name", scales = "free")+
	   stat_poly_eq(formula = my.formula, 
                aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                parse = TRUE) +       
	geom_smooth(method = "lm", se=FALSE, color=alpha("blue",alpha = 0.4), formula = my.formula) +
	geom_point()

#bottom line: probably need to multiply n2o by 1.25 every time a sample is drawn another time from the same glass vial

#read in  and binding all the 9-17ts what a fucking mess: redo_9.17_1----------
Lines<-readLines(
"G:/My Drive/Studies/N_efficiency/Data/GC/Incubation_2021-09-17/2021-09-17_NC_R1_CC_SF_PL_INC_Day 35-H2O 3 Day 35.txt")
#This takes care of when fucking gcsolution decides NOT TO PRINT THAT THERE IS NO N2O, just that there IS NO DATA, ie it just skips reporting anything when N2O was 0 on the gcd in these ASCII files. As if ther were no results. I fucking hate this software
#This will have to be fixed if co2 or ch4 have  no data (but im pretty sure its ok b/c co2 and ch4 are negative instead of 0 when they are low)
ind<-which((Lines==""&lag(Lines)=="# of IDs,0")==TRUE)-1
for(i in 0:(length(ind)-1)) 
	{
		Lines<-
			append(Lines, #This will have to be fixed if co2 or ch4 have  no data (but im pretty sure its ok b/c co2 and ch4 are negative instead of 0 when they are low)
		"ID#,Name,R.Time,Area,Height,Conc.,Curve,3rd,2nd,1st,Constant",
			after=ind[i+1]+i)
}
#there should be a way to put the above and this together for the two lines but IDK how to do that higher math
ind<-which((Lines==""&lag(Lines,2)=="# of IDs,0")==TRUE)-1
for(i in 0:(length(ind)-1)) 
	{
		Lines<-
			append(Lines, #This will have to be fixed if co2 or ch4 have  no data (but im pretty sure its ok b/c co2 and ch4 are negative instead of 0 when they are low)
		"1,N2O,0,0,0,0,Default,0,0,0,0,",
				after=ind[i+1]+i) 
}
#The above picks up cases when there was 0ppm for n2o  and the fucking gcsolution software decides not to print that, and instead just leaves it blank as in 
		#[Compound Results (Ch2)]
		## of IDs,0
		#(tumbleweeds)
#instead of... (probably ought to create dummy lines within "conclines" based on coming after "# of IDs,0")
		#[Compound Results (Ch2)]
		## of IDs,1
		#ID#,Name,R.Time,Area,Height,Conc.,Curve,3rd,2nd,1st,Constant
		#1,N2O,7.314,9844,1340,0.55427,Default,0.0000000,0.0000000,5.630598e-005,0.0000000,
firstchars<-substr(Lines,1,1) #need list of first characters to distinquish kinds of lines in the ascii files
conclines<- Lines[firstchars %in% c("I","1","2")] #concentrattion lines start iwth I, 1, or 2
grpz<- cumsum(substr(conclines,1,3)=="ID#") # #only gives the lines of Lines that have useful cnocentration data
conclines<-lapply(split(conclines, grpz), #read the lines of Lines in grpz as a list of csvs
                  function(x) read.csv(text=x,header = T))
conclines<-rbindlist(conclines) #take the list of csvs and bind them
colnames(conclines) <- c(colnames(conclines)[-1],"x") # names are just shifted one to the right from the columns; move back

redo_9.17_1<- #This section creates a df with the file name split up into columns and the ch4, n2o, and co2 data togeter
 Lines[firstchars %in% c("D")]%>% #ok now we need the other important info in the Lines that start with D (the .gcd file name lines in the ASCII file for each sample)
  gsub(".*\\GCsolution(.*)\\gcd.*", "\\1", .)%>% #say "fuck off" to the .gcd part outside of these delimiters
 	 rep(.,each=3)%>%
  as.data.frame()%>%
  rename(f=".")%>%
  mutate(.,f=as.character(f))%>%
  cSplit(indt=.,splitCols =  "f",sep="\\")%>% #want all these as identifyer columns
  cbind(.,conclines)%>%
 	rename(Sample_ID_SN="f_7")%>%
	select(-c(f_1,	f_2,	f_3,	f_4	,f_5,	f_6,X3rd,X2nd,x))%>%
 	mutate(.,Sample_ID_SN=gsub("bhomik_","",Sample_ID_SN))%>%
 	mutate(.,Sample_ID_SN=gsub("_1.$","",Sample_ID_SN))%>%
 	mutate(.,Sample_ID_SN=paste0(substr(Sample_ID_SN,1,nchar(Sample_ID_SN)-4),sub("_",".",substr(Sample_ID_SN,nchar(Sample_ID_SN)-3,nchar(Sample_ID_SN)))))%>%
 	cSplit(indt=.,splitCols =  "Sample_ID_SN",sep=".")%>% #want all these as identifyer columns
 	rename(Sample_ID="Sample_ID_SN_1",SN="Sample_ID_SN_2")

#read in  and binding all the 9-17ts what a fucking mess: redo_9.17_2----------
Lines<-readLines(
"G:/My Drive/Studies/N_efficiency/Data/GC/Incubation_2021-09-17_2/2021-09-17_NC_R1_CC_SF_PL_INC_Day 35-GA_R3_CC_INC_PL_SF_Day35.txt")
#This takes care of when fucking gcsolution decides NOT TO PRINT THAT THERE IS NO N2O, just that there IS NO DATA, ie it just skips reporting anything when N2O was 0 on the gcd in these ASCII files. As if ther were no results. I fucking hate this software
#This will have to be fixed if co2 or ch4 have  no data (but im pretty sure its ok b/c co2 and ch4 are negative instead of 0 when they are low)
ind<-which((Lines==""&lag(Lines)=="# of IDs,0")==TRUE)-1
for(i in 0:(length(ind)-1)) 
	{
		Lines<-
			append(Lines, #This will have to be fixed if co2 or ch4 have  no data (but im pretty sure its ok b/c co2 and ch4 are negative instead of 0 when they are low)
		"ID#,Name,R.Time,Area,Height,Conc.,Curve,3rd,2nd,1st,Constant",
			after=ind[i+1]+i)
}
#there should be a way to put the above and this together for the two lines but IDK how to do that higher math
ind<-which((Lines==""&lag(Lines,2)=="# of IDs,0")==TRUE)-1
for(i in 0:(length(ind)-1)) 
	{
		Lines<-
			append(Lines, #This will have to be fixed if co2 or ch4 have  no data (but im pretty sure its ok b/c co2 and ch4 are negative instead of 0 when they are low)
		"1,N2O,0,0,0,0,Default,0,0,0,0,",
				after=ind[i+1]+i) 
}
#The above picks up cases when there was 0ppm for n2o  and the fucking gcsolution software decides not to print that, and instead just leaves it blank as in 
		#[Compound Results (Ch2)]
		## of IDs,0
		#(tumbleweeds)
#instead of... (probably ought to create dummy lines within "conclines" based on coming after "# of IDs,0")
		#[Compound Results (Ch2)]
		## of IDs,1
		#ID#,Name,R.Time,Area,Height,Conc.,Curve,3rd,2nd,1st,Constant
		#1,N2O,7.314,9844,1340,0.55427,Default,0.0000000,0.0000000,5.630598e-005,0.0000000,
firstchars<-substr(Lines,1,1) #need list of first characters to distinquish kinds of lines in the ascii files
conclines<- Lines[firstchars %in% c("I","1","2")] #concentrattion lines start iwth I, 1, or 2
grpz<- cumsum(substr(conclines,1,3)=="ID#") # #only gives the lines of Lines that have useful cnocentration data
conclines<-lapply(split(conclines, grpz), #read the lines of Lines in grpz as a list of csvs
                  function(x) read.csv(text=x,header = T))
conclines<-rbindlist(conclines) #take the list of csvs and bind them
colnames(conclines) <- c(colnames(conclines)[-1],"x") # names are just shifted one to the right from the columns; move back

redo_9.17_2<- #This section creates a df with the file name split up into columns and the ch4, n2o, and co2 data togeter
 Lines[firstchars %in% c("D")]%>% #ok now we need the other important info in the Lines that start with D (the .gcd file name lines in the ASCII file for each sample)
  gsub(".*\\GCsolution(.*)\\gcd.*", "\\1", .)%>% #say "fuck off" to the .gcd part outside of these delimiters
 	 rep(.,each=3)%>%
  as.data.frame()%>%
  rename(f=".")%>%
  mutate(.,f=as.character(f))%>%
  cSplit(indt=.,splitCols =  "f",sep="\\")%>% #want all these as identifyer columns
  cbind(.,conclines)%>%
 	rename(Sample_ID_SN="f_7")%>%
	select(-c(f_1,	f_2,	f_3,	f_4	,f_5,	f_6,X3rd,X2nd,x))%>%
 	mutate(.,Sample_ID_SN=gsub("bhomik_","",Sample_ID_SN))%>%
 	mutate(.,Sample_ID_SN=gsub("_1.$","",Sample_ID_SN))%>%
 	mutate(.,Sample_ID_SN=paste0(substr(Sample_ID_SN,1,nchar(Sample_ID_SN)-4),sub("_",".",substr(Sample_ID_SN,nchar(Sample_ID_SN)-3,nchar(Sample_ID_SN)))))%>%
 	cSplit(indt=.,splitCols =  "Sample_ID_SN",sep=".")%>% #want all these as identifyer columns
 	rename(Sample_ID="Sample_ID_SN_1",SN="Sample_ID_SN_2")

#read in  and binding all the 9-17ts what a fucking mess: redo_9.17_3----------
Lines<-readLines(
"G:/My Drive/Studies/N_efficiency/Data/GC/Incubation_2021-09-17_3/2021-09-17_NC_R1_CC_SF_PL_INC_Day 35-NC_R2_CC_SF_PL_SF_Day35.txt")
#This takes care of when fucking gcsolution decides NOT TO PRINT THAT THERE IS NO N2O, just that there IS NO DATA, ie it just skips reporting anything when N2O was 0 on the gcd in these ASCII files. As if ther were no results. I fucking hate this software
#This will have to be fixed if co2 or ch4 have  no data (but im pretty sure its ok b/c co2 and ch4 are negative instead of 0 when they are low)
ind<-which((Lines==""&lag(Lines)=="# of IDs,0")==TRUE)-1
for(i in 0:(length(ind)-1)) 
	{
		Lines<-
			append(Lines, #This will have to be fixed if co2 or ch4 have  no data (but im pretty sure its ok b/c co2 and ch4 are negative instead of 0 when they are low)
		"ID#,Name,R.Time,Area,Height,Conc.,Curve,3rd,2nd,1st,Constant",
			after=ind[i+1]+i)
}
#there should be a way to put the above and this together for the two lines but IDK how to do that higher math
ind<-which((Lines==""&lag(Lines,2)=="# of IDs,0")==TRUE)-1
for(i in 0:(length(ind)-1)) 
	{
		Lines<-
			append(Lines, #This will have to be fixed if co2 or ch4 have  no data (but im pretty sure its ok b/c co2 and ch4 are negative instead of 0 when they are low)
		"1,N2O,0,0,0,0,Default,0,0,0,0,",
				after=ind[i+1]+i) 
}
#The above picks up cases when there was 0ppm for n2o  and the fucking gcsolution software decides not to print that, and instead just leaves it blank as in 
		#[Compound Results (Ch2)]
		## of IDs,0
		#(tumbleweeds)
#instead of... (probably ought to create dummy lines within "conclines" based on coming after "# of IDs,0")
		#[Compound Results (Ch2)]
		## of IDs,1
		#ID#,Name,R.Time,Area,Height,Conc.,Curve,3rd,2nd,1st,Constant
		#1,N2O,7.314,9844,1340,0.55427,Default,0.0000000,0.0000000,5.630598e-005,0.0000000,
firstchars<-substr(Lines,1,1) #need list of first characters to distinquish kinds of lines in the ascii files
conclines<- Lines[firstchars %in% c("I","1","2")] #concentrattion lines start iwth I, 1, or 2
grpz<- cumsum(substr(conclines,1,3)=="ID#") # #only gives the lines of Lines that have useful cnocentration data
conclines<-lapply(split(conclines, grpz), #read the lines of Lines in grpz as a list of csvs
                  function(x) read.csv(text=x,header = T))
conclines<-rbindlist(conclines) #take the list of csvs and bind them
colnames(conclines) <- c(colnames(conclines)[-1],"x") # names are just shifted one to the right from the columns; move back

redo_9.17_3<- #This section creates a df with the file name split up into columns and the ch4, n2o, and co2 data togeter
 Lines[firstchars %in% c("D")]%>% #ok now we need the other important info in the Lines that start with D (the .gcd file name lines in the ASCII file for each sample)
  gsub(".*\\GCsolution(.*)\\gcd.*", "\\1", .)%>% #say "fuck off" to the .gcd part outside of these delimiters
 	 rep(.,each=3)%>%
  as.data.frame()%>%
  rename(f=".")%>%
  mutate(.,f=as.character(f))%>%
  cSplit(indt=.,splitCols =  "f",sep="\\")%>% #want all these as identifyer columns
  cbind(.,conclines)%>%
 	rename(Sample_ID_SN="f_7")%>%
	select(-c(f_1,	f_2,	f_3,	f_4	,f_5,	f_6,X3rd,X2nd,x))%>%
 	mutate(.,Sample_ID_SN=gsub("bhomik_","",Sample_ID_SN))%>%
 	mutate(.,Sample_ID_SN=gsub("_1.$","",Sample_ID_SN))%>%
 	mutate(.,Sample_ID_SN=paste0(substr(Sample_ID_SN,1,nchar(Sample_ID_SN)-4),sub("_",".",substr(Sample_ID_SN,nchar(Sample_ID_SN)-3,nchar(Sample_ID_SN)))))%>%
 	cSplit(indt=.,splitCols =  "Sample_ID_SN",sep=".")%>% #want all these as identifyer columns
 	rename(Sample_ID="Sample_ID_SN_1",SN="Sample_ID_SN_2")

#read in  and binding all the 9-17ts what a fucking mess: redo_9.17_4----------
Lines<-readLines(
"G:/My Drive/Studies/N_efficiency/Data/GC/Incubation_2021-09-17_4/2021-09-17_NC_R1_CC_SF_PL_INC_Day 35-.txt")
#This takes care of when fucking gcsolution decides NOT TO PRINT THAT THERE IS NO N2O, just that there IS NO DATA, ie it just skips reporting anything when N2O was 0 on the gcd in these ASCII files. As if ther were no results. I fucking hate this software
#This will have to be fixed if co2 or ch4 have  no data (but im pretty sure its ok b/c co2 and ch4 are negative instead of 0 when they are low)
ind<-which((Lines==""&lag(Lines)=="# of IDs,0")==TRUE)-1
for(i in 0:(length(ind)-1)) 
	{
		Lines<-
			append(Lines, #This will have to be fixed if co2 or ch4 have  no data (but im pretty sure its ok b/c co2 and ch4 are negative instead of 0 when they are low)
		"ID#,Name,R.Time,Area,Height,Conc.,Curve,3rd,2nd,1st,Constant",
			after=ind[i+1]+i)
}
#there should be a way to put the above and this together for the two lines but IDK how to do that higher math
ind<-which((Lines==""&lag(Lines,2)=="# of IDs,0")==TRUE)-1
for(i in 0:(length(ind)-1)) 
	{
		Lines<-
			append(Lines, #This will have to be fixed if co2 or ch4 have  no data (but im pretty sure its ok b/c co2 and ch4 are negative instead of 0 when they are low)
		"1,N2O,0,0,0,0,Default,0,0,0,0,",
				after=ind[i+1]+i) 
}
#The above picks up cases when there was 0ppm for n2o  and the fucking gcsolution software decides not to print that, and instead just leaves it blank as in 
		#[Compound Results (Ch2)]
		## of IDs,0
		#(tumbleweeds)
#instead of... (probably ought to create dummy lines within "conclines" based on coming after "# of IDs,0")
		#[Compound Results (Ch2)]
		## of IDs,1
		#ID#,Name,R.Time,Area,Height,Conc.,Curve,3rd,2nd,1st,Constant
		#1,N2O,7.314,9844,1340,0.55427,Default,0.0000000,0.0000000,5.630598e-005,0.0000000,
firstchars<-substr(Lines,1,1) #need list of first characters to distinquish kinds of lines in the ascii files
conclines<- Lines[firstchars %in% c("I","1","2")] #concentrattion lines start iwth I, 1, or 2
grpz<- cumsum(substr(conclines,1,3)=="ID#") # #only gives the lines of Lines that have useful cnocentration data
conclines<-lapply(split(conclines, grpz), #read the lines of Lines in grpz as a list of csvs
                  function(x) read.csv(text=x,header = T))
conclines<-rbindlist(conclines) #take the list of csvs and bind them
colnames(conclines) <- c(colnames(conclines)[-1],"x") # names are just shifted one to the right from the columns; move back

redo_9.17_4<- #This section creates a df with the file name split up into columns and the ch4, n2o, and co2 data togeter
 Lines[firstchars %in% c("D")]%>% #ok now we need the other important info in the Lines that start with D (the .gcd file name lines in the ASCII file for each sample)
  gsub(".*\\GCsolution(.*)\\gcd.*", "\\1", .)%>% #say "fuck off" to the .gcd part outside of these delimiters
 	 rep(.,each=3)%>%
  as.data.frame()%>%
  rename(f=".")%>%
  mutate(.,f=as.character(f))%>%
  cSplit(indt=.,splitCols =  "f",sep="\\")%>% #want all these as identifyer columns
  cbind(.,conclines)%>%
 	rename(Sample_ID_SN="f_7")%>%
	select(-c(f_1,	f_2,	f_3,	f_4	,f_5,	f_6,X3rd,X2nd,x))%>%
 	mutate(.,Sample_ID_SN=gsub("bhomik_","",Sample_ID_SN))%>%
 	mutate(.,Sample_ID_SN=gsub("_1.$","",Sample_ID_SN))%>%
 	mutate(.,Sample_ID_SN=paste0(substr(Sample_ID_SN,1,nchar(Sample_ID_SN)-4),sub("_",".",substr(Sample_ID_SN,nchar(Sample_ID_SN)-3,nchar(Sample_ID_SN)))))%>%
 	cSplit(indt=.,splitCols =  "Sample_ID_SN",sep=".")%>% #want all these as identifyer columns
 	rename(Sample_ID="Sample_ID_SN_1",SN="Sample_ID_SN_2")

#read in  and binding all the 9-17ts what a fucking mess: redo_9.17_5 (the main bind sent to dipti and janeva)----------
Lines<-readLines(
"G:/My Drive/Studies/N_efficiency/Data/GC/Incubation_2021-09-17_5/2021-09-17_NC_R1_CC_SF_PL_INC_Day 35-H2O 3 Day 35_compiled.txt")
#This takes care of when fucking gcsolution decides NOT TO PRINT THAT THERE IS NO N2O, just that there IS NO DATA, ie it just skips reporting anything when N2O was 0 on the gcd in these ASCII files. As if ther were no results. I fucking hate this software
#This will have to be fixed if co2 or ch4 have  no data (but im pretty sure its ok b/c co2 and ch4 are negative instead of 0 when they are low)
ind<-which((Lines==""&lag(Lines)=="# of IDs,0")==TRUE)-1
for(i in 0:(length(ind)-1)) 
	{
		Lines<-
			append(Lines, #This will have to be fixed if co2 or ch4 have  no data (but im pretty sure its ok b/c co2 and ch4 are negative instead of 0 when they are low)
		"ID#,Name,R.Time,Area,Height,Conc.,Curve,3rd,2nd,1st,Constant",
			after=ind[i+1]+i)
}
#there should be a way to put the above and this together for the two lines but IDK how to do that higher math
ind<-which((Lines==""&lag(Lines,2)=="# of IDs,0")==TRUE)-1
for(i in 0:(length(ind)-1)) 
	{
		Lines<-
			append(Lines, #This will have to be fixed if co2 or ch4 have  no data (but im pretty sure its ok b/c co2 and ch4 are negative instead of 0 when they are low)
		"1,N2O,0,0,0,0,Default,0,0,0,0,",
				after=ind[i+1]+i) 
}
#The above picks up cases when there was 0ppm for n2o  and the fucking gcsolution software decides not to print that, and instead just leaves it blank as in 
		#[Compound Results (Ch2)]
		## of IDs,0
		#(tumbleweeds)
#instead of... (probably ought to create dummy lines within "conclines" based on coming after "# of IDs,0")
		#[Compound Results (Ch2)]
		## of IDs,1
		#ID#,Name,R.Time,Area,Height,Conc.,Curve,3rd,2nd,1st,Constant
		#1,N2O,7.314,9844,1340,0.55427,Default,0.0000000,0.0000000,5.630598e-005,0.0000000,
firstchars<-substr(Lines,1,1) #need list of first characters to distinquish kinds of lines in the ascii files
conclines<- Lines[firstchars %in% c("I","1","2")] #concentrattion lines start iwth I, 1, or 2
grpz<- cumsum(substr(conclines,1,3)=="ID#") # #only gives the lines of Lines that have useful cnocentration data
conclines<-lapply(split(conclines, grpz), #read the lines of Lines in grpz as a list of csvs
                  function(x) read.csv(text=x,header = T))
conclines<-rbindlist(conclines) #take the list of csvs and bind them
colnames(conclines) <- c(colnames(conclines)[-1],"x") # names are just shifted one to the right from the columns; move back

redo_9.17_5<- #This section creates a df with the file name split up into columns and the ch4, n2o, and co2 data togeter
 Lines[firstchars %in% c("D")]%>% #ok now we need the other important info in the Lines that start with D (the .gcd file name lines in the ASCII file for each sample)
  gsub(".*\\GCsolution(.*)\\gcd.*", "\\1", .)%>% #say "fuck off" to the .gcd part outside of these delimiters
 	 rep(.,each=3)%>%
  as.data.frame()%>%
  rename(f=".")%>%
  mutate(.,f=as.character(f))%>%
  cSplit(indt=.,splitCols =  "f",sep="\\")%>% #want all these as identifyer columns
  cbind(.,conclines)%>%
 	rename(Sample_ID_SN="f_7")%>%
	select(-c(f_1,	f_2,	f_3,	f_4	,f_5,	f_6,X3rd,X2nd,x))%>%
 	mutate(.,Sample_ID_SN=gsub("bhomik_","",Sample_ID_SN))%>%
 	mutate(.,Sample_ID_SN=gsub("_1.$","",Sample_ID_SN))%>%
 	mutate(.,Sample_ID_SN=paste0(substr(Sample_ID_SN,1,nchar(Sample_ID_SN)-4),sub("_",".",substr(Sample_ID_SN,nchar(Sample_ID_SN)-3,nchar(Sample_ID_SN)))))%>%
 	cSplit(indt=.,splitCols =  "Sample_ID_SN",sep=".")%>% #want all these as identifyer columns
 	rename(Sample_ID="Sample_ID_SN_1",SN="Sample_ID_SN_2")


#PUt 9-17 together:
#So for n2o the reconstructed order should be:

#19-35 this folder (C:\GCsolution\Data\Sean\2020\Batchfiles_and_Data\Incubation_2021-09-17)
#9-18 in C:\GCsolution\Data\Sean\2020\Batchfiles_and_Data\Incubation_2021-09-17_2 (gc sampled the second time this run)
#2-8 in C:\GCsolution\Data\Sean\2020\Batchfiles_and_Data\Incubation_2021-09-17_3 (gc sampled the 3rd time this run)
#1 in C:\GCsolution\Data\Sean\2020\Batchfiles_and_Data\Incubation_2021-09-17_X (gc sampled the 4th time this run)

rbind(
(redo_9.17_1%>%
subset(.,SN%in%19:35)),
	(redo_9.17_2%>%
subset(.,SN%in%9:18)),
(redo_9.17_3%>%
subset(.,SN%in%2:8)),
	(redo_9.17_4%>%
subset(.,SN==1)))%>%
		arrange(SN,Name)%>%
	all.equal(redo_9.17_5%>%arrange(SN,Name))
#ok so checks out each one is the same as the compiled file

#since theyre the same, use the number of the run to modify the N2O concentrations
mod<-rbindlist(list(redo_9.17_1=
(redo_9.17_1%>%
subset(.,SN%in%19:35)),
	redo_9.17_2=(redo_9.17_2%>%
subset(.,SN%in%9:18)),
redo_9.17_3=(redo_9.17_3%>%
subset(.,SN%in%2:8)),
	redo_9.17_4=(redo_9.17_4%>%
subset(.,SN==1))),idcol = "extr")%>%
	mutate(.,extr=as.integer(substr(extr,11,11)))%>%
	#mutate(.,Concm=ifelse(Name=="N2O"&extr>=2,Conc.*(1.25^(extr-1)),Conc.))%>% #modified to get back to orignal concentration based on multiple extractions #this 1.25 comes from the ggplot of "merge(redo_9.13_2,redo_9.13_3,by=c....."
	mutate(.,Concm=ifelse(Name=="N2O"&extr>=2,Conc.*10000,Conc.))%>% #modified to get back to orignal concentration based on multiple extractions #this 1.25 comes from the ggplot of "merge(redo_9.13_2,redo_9.13_3,by=c....."
	mutate(.,Concm=ifelse(SN==1,Conc.*(11/3),Conc.)) #vial is 11 mL, but I did a 3mL dilution for this vial 1 since it was beyond ecd detaction limits ##modified to get back due to dilution, see all the readmes in the folders where the data comes from

#now get the data from reruns together with the co2 and ch4 from the original, un-reextracted vials
mod2<-
redo_9.17_1%>%
	merge(.,(mod%>%select(.,c(Name,SN,Concm))),by=c("Name","SN"))

#write to csv.; this is the file I gave to dipti and janeva:
write.csv(mod2,file="G:/My Drive/Studies/N_efficiency/Data/GC/Incubation_2021-09-17_5/2021-09-17_NC_R1_CC_SF_PL_INC_Day 35-H2O 3 Day 35_compiled.csv")

#Notes and takeomes: 
#So, you'll see a Concm and Conc. : use Concm for N2O, Conc. for CO2 and CH4. 


#yet another figure showing CO2 from one sampling of the same vial to the next:
mod2%>%
	ggplot(.,aes(x=Concm,y=Conc.))+facet_wrap("Name", scales = "free")+
		   stat_poly_eq(formula = my.formula, 
                aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                parse = TRUE) +       
	geom_smooth(method = "lm", se=FALSE, color=alpha("blue",alpha = 0.4), formula = my.formula) +
	geom_point()


