#Read in first  ----------------


Lines<-readLines(
"G:/My Drive/Studies/N_efficiency/Data/GC/Incubation_2021-09-13/2021-09-13_NC R1 CC_SF, PL_INC Day 2-Blank water 3.txt")
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

bhomik_2021_09_13_NC_R1_CC_SF_PL_INC_Day_2_Blank_water_3<- #This section creates a df with the file name split up into columns and the ch4, n2o, and co2 data togeter
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

write.csv(bhomik_2021_09_13_NC_R1_CC_SF_PL_INC_Day_2_Blank_water_3,file="G:/My Drive/Studies/N_efficiency/Data/GC/Incubation_2021-09-13/bhomik_2021_09_13_NC_R1_CC_SF_PL_INC_Day_2_Blank_water_3_2.csv")
