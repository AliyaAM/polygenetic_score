directory = "/Users/aliya/my_docs/"

OUTPUT = "KCL_postDoc/Data_analysis/polygenetic_score/RESULTS/depression/"

folder_unadjusted = paste("depression_unadjusted_discrim_bin/", sep = "")
folder_adjusted = paste("depression_adjusted_discrim_bin/", sep = "")

names_adjusted_pca = c("Intercept", 
                       "gene", 
                       "discrimination", 
                       "age", 
                       "sex", 
                       "wealth", 
                       "pca", 
                       "gene x discrimination") 


names_adjusted = c("Intercept", 
                   "gene", 
                   "discrimination", 
                   "age", 
                   "sex", 
                   "wealth", 
                   "gene x discrimination") 


names_unadjusted_pca = c("Intercept", 
                         "gene", 
                         "discrimination", 
                         "pca", 
                         "gene x discrimination") 


names_unadjusted = c("Intercept", 
                     "gene", 
                     "discrimination", 
                     "gene x discrimination") 

############ ############ ############ ############ ############  ############ ############ ############ ############ ############  
####  ####  #### unadjusted 
############
#adjusted for pca 
ELSA_pca_unadjusted_depression = cbind(names_unadjusted_pca, read.csv(paste(directory, OUTPUT, folder_unadjusted, "depression_results_ELSA_pca_unadjusted.csv", sep = "")))
HRS_pca_unadjusted_depression = cbind(names_unadjusted_pca, read.csv(paste(directory, OUTPUT, folder_unadjusted, "depression_results_HRS_pca_unadjusted.csv", sep = "")))
#not adjusted for pca 
ELSA_unadjusted_depression = cbind(names_unadjusted, read.csv(paste(directory, OUTPUT, folder_unadjusted, "depression_results_ELSA_unadjusted.csv", sep = "")))
HRS_unadjusted_depression = cbind(names_unadjusted, read.csv(paste(directory, OUTPUT,  folder_unadjusted, "depression_results_HRS_unadjusted.csv", sep = "")))


############ ############ ############ ############ ############  ############ ############ ############ ############ ############  
####  ####  #### adjusted 
############
ELSA_pca_adjusted_depression = cbind(names_adjusted_pca, read.csv(paste(directory, OUTPUT, folder_adjusted, "depression_results_ELSA_pca_adjusted.csv", sep = "")))
HRS_pca_adjusted_depression = cbind(names_adjusted_pca, read.csv(paste(directory, OUTPUT, folder_adjusted, "depression_results_HRS_pca_adjusted.csv", sep = "")))
#not adjusted for pca 
#ELSA_adjusted_depression = cbind(names_adjusted, read.csv(paste(directory, OUTPUT, folder_adjusted, "depression_results_ELSA_adjusted.csv", sep = "")))
#HRS_adjusted_depression = cbind(names_adjusted, read.csv(paste(directory, OUTPUT, folder_adjusted, "depression_results_HRS_adjusted.csv", sep = "")))

na_filler = rep("NA", 7)


depression_unadjusted = rbind(ELSA_pca_unadjusted_depression,
                            na_filler,
                            na_filler,
                            na_filler,
                            na_filler,
                            na_filler,
                            na_filler,
                            na_filler,
                            na_filler,
                            na_filler, 
                            
                            HRS_pca_unadjusted_depression, 
                            na_filler,
                            na_filler,
                            na_filler,
                            na_filler,
                            na_filler,
                            na_filler,
                            na_filler,
                            na_filler,
                            na_filler)

depression_adjusted = rbind(ELSA_pca_adjusted_depression, HRS_pca_adjusted_depression)

Table_depression = cbind(depression_unadjusted, depression_adjusted)
write.csv(Table_depression, file = paste(directory, OUTPUT, "Table_depression_per_wave.csv", sep = ""))

############ ############ ############ ############ ############  ############ ############ ############ ############ ############  
############ ############ ############ ############ ############  ############ ############ ############ ############ ############  
############ ############ ############ ############ ############  ############ ############ ############ ############ ############  
############ ############ ############ ############ ############  ############ ############ ############ ############ ############  
############ ############ ############ ############ ############  ############ ############ ############ ############ ############  

############ ############ ############ ############ ############  ############ ############ ############ ############ ############  
############
############ #composite over waves  
############
############ #composite over waves  
############
############ #composite over waves  
############

#composite over waves #### unadjusted 

#composite over waves HRS 
#adjutsted for pca
HRS_composite_pca_unadjusted_depression = cbind(names_unadjusted_pca,  read.csv(paste(directory, OUTPUT, folder_unadjusted, "depression_HRS_composite_pca_unadjusted.csv", sep = "")))
#not adjusted for pca 
#HRS_composite_unadjusted_depression = cbind(names_unadjusted,  read.csv(paste(directory, OUTPUT, folder_unadjusted, "depression_HRS_composite_unadjusted.csv", sep = "")))


#adjutsted for pca ELSA
ELSA_composite_pca_unadjusted_depression = cbind(names_unadjusted_pca,  read.csv(paste(directory, OUTPUT, folder_unadjusted, "depression_ELSA_composite_pca_unadjusted.csv", sep = "")))
#not adjusted for pca 
#ELSA_composite_unadjusted_depression = cbind(names_unadjusted,  read.csv(paste(directory, OUTPUT, folder_unadjusted, "depression_ELSA_composite_unadjusted.csv", sep = "")))



#composite over waves #### adjusted 

#adjutsted for pca HRS
HRS_composite_pca_adjusted_depression = cbind(names_adjusted_pca, read.csv(paste(directory, OUTPUT, folder_adjusted, "depression_HRS_composite_pca_adjusted.csv", sep = "")))
#not adjusted for pca 
#HRS_composite_adjusted_depression = cbind(names_adjusted, read.csv(paste(directory, OUTPUT, folder_adjusted, "depression_HRS_composite_adjusted.csv", sep = "")))


#adjutsted for pca ELSA
ELSA_composite_pca_adjusted_depression = cbind(names_adjusted_pca, read.csv(paste(directory, OUTPUT, folder_adjusted, "depression_ELSA_composite_pca_adjusted.csv", sep = "")))
#not adjusted for pca 
#ELSA_composite_adjusted_depression = cbind(names_adjusted, read.csv(paste(directory, OUTPUT, folder_adjusted, "depression_ELSA_composite_adjusted.csv", sep = "")))

######## Table 2 composite & adjusted for PCA 
composite_depression_unadjusted = rbind(ELSA_composite_pca_unadjusted_depression,
                                      na_filler,
                                      na_filler,
                                      na_filler,
                                      
                                      
                                      HRS_composite_pca_unadjusted_depression,
                                      na_filler,
                                      na_filler,
                                      na_filler)

composite_depression_adjusted = rbind(ELSA_composite_pca_adjusted_depression, HRS_composite_pca_adjusted_depression)

Table_composite_depression = cbind(composite_depression_unadjusted, composite_depression_adjusted)
write.csv(Table_composite_depression, file = paste(directory, OUTPUT, "Table_composite_depression.csv", sep = ""))






