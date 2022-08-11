library(dplyr)







#need to add hypertension_bin to 2018 
#need to add alcohol_days_week to 2008 
#add anxiety to all years 
#add depression new bin to all years (NEW)
#need to add CVD to all years 
#check smoking 
#add PTSD for all years  
#add Alzheimer's for all years   
#add kidney disease for all years    
#limiting longstanding condition (recode to bin)




directory = "/Users/aliya/my_docs"

#"/Users/aliyaamirova/"

DATA_ROOT = "/KCL_postDoc/Data_analysis/"
#SOURCE_ROOT = "/proj/Cumulative_effects_HRS/Version_2_analysis/"


HRS_2008_data = read.csv(paste(directory, DATA_ROOT, "HRS_2008_data/HRS2008_dataset_latest_renamed_vars.csv", sep=""))
HRS_2010_data = read.csv(paste(directory, DATA_ROOT, "HRS_2010_data/HRS2010_dataset_latest_renamed_vars.csv", sep=""))
HRS_2012_data =  read.csv(paste(directory, DATA_ROOT, "HRS_2012_data/HRS2012_dataset_latest_renamed_vars.csv", sep=""))
HRS_2014_data =  read.csv(paste(directory, DATA_ROOT, "HRS_2014_data/HRS2014_dataset_latest_renamed_vars.csv", sep=""))
HRS_2016_data =  read.csv(paste(directory, DATA_ROOT, "HRS_2016_data/HRS2016_dataset_latest_renamed_vars.csv", sep=""))
HRS_2018_data =  read.csv(paste(directory, DATA_ROOT, "HRS_2018_data/HRS2018_dataset_latest_renamed_vars.csv", sep="")) 





HRS_2008_data$HHIDPN = HRS_2008_data$HRS2008_data.HHIDPN
HRS_2010_data$HHIDPN = HRS_2010_data$HRS_2010_data.HHIDPN
HRS_2012_data$HHIDPN = HRS_2012_data$HRS_2012_data.HHIDPN
HRS_2014_data$HHIDPN = HRS_2014_data$HRS_2014_data.HHIDPN
HRS_2016_data$HHIDPN = HRS_2016_data$HRS_2016_data.HHIDPN
HRS_2018_data$HHIDPN = HRS_2018_data$HRS_2018_data.HHIDPN

print("renamed")

# only include the variables of interest, because later we need ot drop the NAs. 
HRS_2008_data = data.frame(HRS_2008_data$HHIDPN,  
                           HRS_2008_data$HRS2008_checklist_depression_bin, 
                           HRS_2008_data$HRS2008_discrim_bin,
                           HRS_2008_data$HRS2008_wealth_noIRA)

print("done 1")


colnames(HRS_2008_data) = c("HHIDPN", 
                            "checklist_depression_bin_2008", 
                            "discrim_bin_2008",
                            "wealth_noIRA_2008")


HRS_2010_data = data.frame(HRS_2010_data$HHIDPN,
                           HRS_2010_data$HRS2010_checklist_depression_bin, 
                           HRS_2010_data$HRS2010_discrim_bin,
                           HRS_2010_data$HRS2010_wealth_noIRA) 

colnames(HRS_2010_data) = c("HHIDPN", 
                            "checklist_depression_bin_2010", 
                            "discrim_bin_2010",
                            "wealth_noIRA_2010")


print("done 2")

HRS_2012_data = data.frame(HRS_2012_data$HHIDPN,
                           HRS_2012_data$HRS2012_checklist_depression_bin, 
                           HRS_2012_data$HRS2012_discrim_bin,
                           HRS_2012_data$HRS2012_wealth_noIRA) 



colnames(HRS_2012_data) = c("HHIDPN", 
                            "checklist_depression_bin_2012", 
                            "discrim_bin_2012",
                            "wealth_noIRA_2012")

print("done 3")
HRS_2014_data = data.frame(HRS_2014_data$HHIDPN,
                           HRS_2014_data$HRS2014_checklist_depression_bin, 
                           HRS_2014_data$HRS2014_discrim_bin,
                           HRS_2014_data$HRS2014_wealth_noIRA) 

colnames(HRS_2014_data) = c("HHIDPN", 
                            "checklist_depression_bin_2014", 
                            "discrim_bin_2014",
                            "wealth_noIRA_2014")

print("done 4")
HRS_2016_data = data.frame(HRS_2016_data$HHIDPN,
                           HRS_2016_data$HRS2016_checklist_depression_bin, 
                           HRS_2016_data$HRS2016_discrim_bin,
                           HRS_2016_data$HRS2016_wealth_noIRA) 


colnames(HRS_2016_data) = c("HHIDPN", 
                            "checklist_depression_bin_2016", 
                            "discrim_bin_2016",
                            "wealth_noIRA_2016")


print("done 5")

HRS_2018_data = data.frame(HRS_2018_data$HHIDPN,
                           HRS_2018_data$HRS2018_checklist_depression_bin, 
                           HRS_2018_data$HRS2018_discrim_bin,
                           HRS_2018_data$HRS2018_wealth_noIRA)


colnames(HRS_2018_data) = c("HHIDPN", 
                            "checklist_depression_bin_2018", 
                            "discrim_bin_2018",
                            "wealth_noIRA_2018")



print("done 6")


HRS_2008_data$HHIDPN
HRS_2010_data$HHIDPN
HRS_2012_data$HHIDPN
HRS_2014_data$HHIDPN
HRS_2016_data$HHIDPN
HRS_2018_data$HHIDPN



ls(HRS_2008_data)
ls(HRS_2010_data)
ls(HRS_2012_data)
ls(HRS_2014_data)
ls(HRS_2016_data)
ls(HRS_2018_data)

all_HRS =  full_join(HRS_2008_data, 
                         HRS_2010_data, 
                         HRS_2012_data, 
                         HRS_2014_data,
                         HRS_2016_data, 
                         HRS_2018_data) 


          
          