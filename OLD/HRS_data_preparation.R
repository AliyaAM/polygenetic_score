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


#add depressive symptoms from HRS RAND file : /Users/aliya/my_docs/KCL_postDoc/Data_analysis/randhrs1992_2018v1.csv
RAND =  read.csv(paste(directory, DATA_ROOT, "randhrs1992_2018v1.csv", sep="")) 


polygenic_scores_data = read.csv(paste(directory, DATA_ROOT, "/HRS_polygenetic_scores_biomarkers/pgenscore4e_r.csv", sep = ""))

harmonised_data_all_waves = read.csv(paste(directory, DATA_ROOT, "H_HRS_c.csv", sep=""))

# match ID names 

HRS_2008_data$HHIDPN = HRS_2008_data$HRS2008_data.HHIDPN
HRS_2010_data$HHIDPN = HRS_2010_data$HRS_2010_data.HHIDPN
HRS_2012_data$HHIDPN = HRS_2012_data$HRS_2012_data.HHIDPN
HRS_2014_data$HHIDPN = HRS_2014_data$HRS_2014_data.HHIDPN
HRS_2016_data$HHIDPN = HRS_2016_data$HRS_2016_data.HHIDPN
HRS_2018_data$HHIDPN = HRS_2018_data$HRS_2018_data.HHIDPN

#use this to subset each HRS by id: ELSA_data_polygenic_scores = polygenic_scores_ELSA_data[ELSA_data$idauniq %in% ID_ELSA,]



one =  inner_join(HRS_2008_data, 
                         HRS_2010_data, 
                  by = c("HHIDPN")) 

two =  inner_join(HRS_2012_data, 
                 HRS_2014_data,
                 
                 by = c("HHIDPN"))

three = inner_join(HRS_2016_data, 
         HRS_2018_data, 
         
         by = c("HHIDPN")) 

one_two = inner_join(one, two, 
                     
                     by = c("HHIDPN"))

all_HRS = inner_join(one_two, three, 
                     
                     by = c("HHIDPN"))


#included cases with unique IDs across the years

nrow(all_HRS)
length(unique(all_HRS$HHIDPN))
ID = unique(all_HRS$HHIDPN)

all_HRS_unique = subset(all_HRS, all_HRS$HHIDPN == ID)

#check the variable list and the numbre of cases 
ls(all_HRS_unique)
nrow(all_HRS_unique)

# write the csv file where the HRS is arranged by year (var1 = HRS2008_age, var2 = HRS2010_age etc)

#write.csv(all_HRS_unique, file = paste(directory, DATA_ROOT, "HRS_2008_data/all_HRS_by_years.csv", sep = "")) 

polygenic_scores_data$HHIDPN = paste(polygenic_scores_data$HHID, 0, polygenic_scores_data$PN, sep = "")
polygenic_scores_data$HHIDPN = as.numeric(polygenic_scores_data$HHIDPN)

all_HRS_by_years_PGS = inner_join(all_HRS, polygenic_scores_data, 
                                  by = c("HHIDPN"))



############### the rand file (subset to the cases included in the rest of our sample): 


#write.csv(all_HRS_by_years_PGS, file = paste(directory, DATA_ROOT, "HRS_2008_data/all_HRS_by_years_PGS.csv", sep = "")) 



# join dataframes 

harmonised_data_all_waves$HHIDPN = harmonised_data_all_waves$hhidpn

ID_hhidpn = unique(all_HRS_by_years_PGS$HHIDPN)

harmonised_data_all_waves = subset(harmonised_data_all_waves, harmonised_data_all_waves$HHIDPN %in% c(ID_hhidpn))

#harmonised_data_all_waves = case_when(harmonised_data_all_waves, harmonised_data_all_waves$hhidpn == )

all_HRS_by_years_PGS$HRS2010_mi_bin = case_when(harmonised_data_all_waves$r10hrtatt == 0 ~ 0, 
                                                harmonised_data_all_waves$r10hrtatt == 1 ~ 1)


all_HRS_by_years_PGS$HRS2012_mi_bin = case_when(harmonised_data_all_waves$r11hrtatt == 0 ~ 0, 
                                                harmonised_data_all_waves$r11hrtatt == 1 ~ 1)


all_HRS_by_years_PGS$HRS2014_mi_bin = case_when(harmonised_data_all_waves$r12hrtatt == 0 ~ 0, 
                                                harmonised_data_all_waves$r12hrtatt == 1 ~ 1)


all_HRS_by_years_PGS$HRS2016_mi_bin = case_when(harmonised_data_all_waves$r13hrtatt == 0 ~ 0, 
                                                harmonised_data_all_waves$r13hrtatt == 1 ~ 1)


all_HRS_by_years_PGS$HRS2018_mi_bin = case_when(harmonised_data_all_waves$r14hrtatt == 0 ~ 0, 
                                                harmonised_data_all_waves$r14hrtatt == 1 ~ 1)


#######################


RAND = subset(RAND, RAND$HHIDPN %in% c(ID_hhidpn))


#adding depressive symptoms var
#RAND: 1992 - 1; 1194 - 2; 1996 - 3; 1998 - 4; 2000 - 5; 2002 - 6; 2004 - 7; 2006 - 8; 2008 - 9; 2010 - 10; 2012 - 11; 2014 - 12; 2016 - 13; 2018 - 14
 
#2008: 
all_HRS_by_years_PGS$HRS2008_cesd = RAND$R9CESD

  
# 2010 
all_HRS_by_years_PGS$HRS2010_cesd = RAND$R10CESD

# 2012 
all_HRS_by_years_PGS$HRS2012_cesd = RAND$R11CESD


# 2014
all_HRS_by_years_PGS$HRS2014_cesd = RAND$R12CESD

# 2016
all_HRS_by_years_PGS$HRS2016_cesd = RAND$R13CESD

# 2018
all_HRS_by_years_PGS$HRS2018_cesd = RAND$R14CESD

############### add stressful event var

#lonliness summary score (three-item: has 3)
all_HRS_by_years_PGS$HRS2008_loneliness3 = harmonised_data_all_waves$r9lnlys3
all_HRS_by_years_PGS$HRS2008_loneliness = harmonised_data_all_waves$r9lnlys

all_HRS_by_years_PGS$HRS2010_loneliness = harmonised_data_all_waves$r10lnlys
all_HRS_by_years_PGS$HRS2010_loneliness3 = harmonised_data_all_waves$r10lnlys3

all_HRS_by_years_PGS$HRS2012_loneliness = harmonised_data_all_waves$r11lnlys
all_HRS_by_years_PGS$hRS2012_loneliness3 = harmonised_data_all_waves$r11lnlys3

all_HRS_by_years_PGS$HRS2014_loneliness = harmonised_data_all_waves$r12lnlys
all_HRS_by_years_PGS$HRS2014_loneliness3 = harmonised_data_all_waves$r12lnlys3

all_HRS_by_years_PGS$HRS2016_loneliness = harmonised_data_all_waves$r13lnlys
all_HRS_by_years_PGS$HRS2016_loneliness3 = harmonised_data_all_waves$r13lnlys3

all_HRS_by_years_PGS$HRS2018_loneliness = harmonised_data_all_waves$r14lnlys
all_HRS_by_years_PGS$HRS2018_loneliness3 = harmonised_data_all_waves$r14lnlys3

#summary of childhood stressful envents 
all_HRS_by_years_PGS$chldhd_stress_event = harmonised_data_all_waves$racsevent

#summary count of lifetime stressful events 
all_HRS_by_years_PGS$life_stress_event = harmonised_data_all_waves$ralsevent


# job stress summary score 
all_HRS_by_years_PGS$HRS2008_jobstress = harmonised_data_all_waves$r9jobsum
all_HRS_by_years_PGS$HRS2010_jobstress = harmonised_data_all_waves$r10jobsum
all_HRS_by_years_PGS$HRS2012_jobstress = harmonised_data_all_waves$r11jobsum


# 2018

all_HRS_by_years_PGS$HRS2008_jobstress
write.csv(all_HRS_by_years_PGS, file = paste(directory, DATA_ROOT, "HRS_2008_data/all_HRS_by_years_PGS.csv", sep = "")) 




