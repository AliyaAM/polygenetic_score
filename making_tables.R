directory = "/Users/aliya/my_docs/"

OUTPUT = "KCL_postDoc/Data_analysis/polygenetic_score/RESULTS/MI/"

folder_unadjusted = paste("MI_unadjusted_discrim_bin/", sep = "")
folder_adjusted = paste("MI_adjusted_discrim_bin/", sep = "")

############ ############ ############ ############ ############  ############ ############ ############ ############ ############  
####  ####  #### unadjusted 
############
#adjusted for pca 
ELSA_pca_unadjusted_mi = read.csv(paste(directory, OUTPUT, folder_unadjusted, "mi_results_ELSA_pca_unadjusted.csv", sep = ""))
HRS_pca_unadjusted_mi = read.csv(paste(directory, OUTPUT, folder_unadjusted, "mi_results_HRS_pca_unadjusted.csv", sep = ""))
#not adjusted for pca 
ELSA_unadjusted_mi = read.csv(paste(directory, OUTPUT, folder_unadjusted, "mi_results_ELSA_unadjusted.csv", sep = "")) 
HRS_unadjusted_mi = read.csv(paste(directory, OUTPUT,  folder_unadjusted, "mi_results_HRS_unadjusted.csv", sep = ""))


############ ############ ############ ############ ############  ############ ############ ############ ############ ############  
####  ####  #### adjusted 
############
ELSA_pca_adjusted_mi = read.csv(paste(directory, OUTPUT, folder_adjusted, "mi_results_ELSA_pca_adjusted.csv", sep = ""))
HRS_pca_adjusted_mi = read.csv(paste(directory, OUTPUT, folder_adjusted, "mi_results_HRS_pca_adjusted.csv", sep = ""))
#not adjusted for pca 
ELSA_adjusted_mi = read.csv(paste(directory, OUTPUT, folder_adjusted, "mi_results_ELSA_adjusted.csv", sep = "")) 
HRS_adjusted_mi = read.csv(paste(directory, OUTPUT, folder_adjusted, "mi_results_HRS_adjusted.csv", sep = ""))

MI_unadjusted = cbind(ELSA_pca_unadjusted_mi, HRS_pca_unadjusted_mi)
MI_adjusted = cbind(ELSA_pca_adjusted_mi, HRS_pca_adjusted_mi)
Table_MI = rbind(MI_unadjusted, MI_adjusted)
write.csv(Table_MI, file = paste(directory, OUTPUT, "Table_MI_per_wave.csv", sep = ""))

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
HRS_composite_pca_unadjusted_mi = read.csv(paste(directory, OUTPUT, folder_unadjusted, "MI_HRS_composite_pca_unadjusted.csv", sep = ""))
#not adjusted for pca 
HRS_composite_unadjusted_mi = read.csv(paste(directory, OUTPUT, folder_unadjusted, "MI_HRS_composite_unadjusted.csv", sep = ""))


#adjutsted for pca ELSA
ELSA_composite_pca_unadjusted_mi = read.csv(paste(directory, OUTPUT, folder_unadjusted, "MI_ELSA_composite_pca_unadjusted.csv", sep = ""))
#not adjusted for pca 
ELSA_composite_unadjusted_mi = read.csv(paste(directory, OUTPUT, folder_unadjusted, "MI_ELSA_composite_unadjusted.csv", sep = ""))



#composite over waves #### adjusted 
 
#adjutsted for pca HRS
HRS_composite_pca_adjusted_mi = read.csv(paste(directory, OUTPUT, folder_adjusted, "MI_HRS_composite_pca_adjusted.csv", sep = ""))
#not adjusted for pca 
HRS_composite_adjusted_mi = read.csv(paste(directory, OUTPUT, folder_adjusted, "MI_HRS_composite_adjusted.csv", sep = ""))


#adjutsted for pca ELSA
ELSA_composite_pca_adjusted_mi = read.csv(paste(directory, OUTPUT, folder_adjusted, "MI_ELSA_composite_pca_adjusted.csv", sep = ""))
#not adjusted for pca 
ELSA_composite_adjusted_mi = read.csv(paste(directory, OUTPUT, folder_adjusted, "MI_ELSA_composite_adjusted.csv", sep = ""))

######## Table 2 composite & adjusted for PCA 
composite_MI_unadjusted = cbind(ELSA_composite_pca_unadjusted_mi, HRS_composite_pca_unadjusted_mi)
composite_MI_adjusted = cbind(ELSA_composite_pca_adjusted_mi, HRS_composite_pca_adjusted_mi)
Table_composite_MI = rbind(composite_MI_unadjusted, composite_MI_adjusted)
write.csv(Table_composite_MI, file = paste(directory, OUTPUT, "Table_composite_MI.csv", sep = ""))


######## Table 3 composite & not adjusted for PCA 
composite_MI_unadjusted = cbind(ELSA_composite_pca_unadjusted_mi, HRS_composite_pca_unadjusted_mi)
composite_MI_adjusted = cbind(ELSA_composite_pca_adjusted_mi, HRS_composite_pca_adjusted_mi)
composite_MI_unadjusted_no_pca = cbind(ELSA_composite_unadjusted_mi, HRS_composite_unadjusted_mi)
composite_MI_adjusted_no_pca = cbind(ELSA_composite_adjusted_mi, HRS_composite_adjusted_mi)
Table_composite_MI_no_pca = rbind(composite_MI_unadjusted_no_pca, composite_MI_adjusted_no_pca)
write.csv(Table_composite_MI_no_pca, file = paste(directory, OUTPUT, "Table_composite_MI_no_pca.csv", sep = ""))

