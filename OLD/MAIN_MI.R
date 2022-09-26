
directory = "/Users/aliya/my_docs/"

#"/Users/aliyaamirova/"

###### Set the source location on the user's local machine  for sourcing functions 
SOURCE_ROOT = paste(directory, "proj/polygenetic_score/", sep = "")



###### sourcing code for the adjusted analysis 
source(paste(SOURCE_ROOT, "PGS_glm_function_ELSA.R", sep=""))

source(paste(SOURCE_ROOT, "subsetting_function.R", sep=""))



DATA_ROOT = "KCL_postDoc/Data_analysis/"


# harmonised_data_all_waves = read.csv(paste(directory, DATA_ROOT, "H_HRS_c.csv", sep=""))


all_HRS_by_years_PGS = read.csv(paste(directory, DATA_ROOT, "HRS_2008_data/all_HRS_by_years_PGS.csv", sep = "")) 
ELSA_data_with_PGS = read.csv(paste(directory, DATA_ROOT, "DATA_ELSA/ELSA_data_with_PGS.csv", sep = "")) 

#unique(all_HRS_by_years_PGS$HRS2012_race_nonwhite)
#all_HRS_by_years_PGS = subset(all_HRS_by_years_PGS, all_HRS_by_years_PGS$HRS2012_race 

######  Set the root location on the user's local machine to save output files.
OUTPUT_ROOT = paste(directory, "KCL_postDoc/Data_analysis/polygenetic_score/RESULTS/MI/", sep = "")



