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

write.csv(all_HRS_unique, file = paste(directory, DATA_ROOT, "HRS_2008_data/all_HRS_by_years.csv", sep = "")) 

polygenic_scores_data$HHIDPN = paste(polygenic_scores_data$HHID, 0, polygenic_scores_data$PN, sep = "")
polygenic_scores_data$HHIDPN = as.numeric(polygenic_scores_data$HHIDPN)

all_HRS_by_years_PGS = inner_join(all_HRS, polygenic_scores_data, 
                                  by = c("HHIDPN"))

write.csv(all_HRS_by_years_PGS, file = paste(directory, DATA_ROOT, "HRS_2008_data/all_HRS_by_years_PGS.csv", sep = "")) 



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
