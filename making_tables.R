directory = "/Users/aliya/my_docs/"

folder_unadjusted = paste("KCL_postDoc/Data_analysis/polygenetic_score/RESULTS/MI/MI_unadjusted_discrim_bin/", sep = "")
folder_adjusted = paste("KCL_postDoc/Data_analysis/polygenetic_score/RESULTS/MI/MI_adjusted_discrim_bin/", sep = "")

#adjusted for pca 
ELSA_pca_unadjusted_mi = read.csv(paste(directory, folder_unadjusted, "mi_results_ELSA_pca_unadjusted.csv", sep = ""))
HRS_pca_unadjusted_mi = read.csv(paste(directory, folder_unadjusted, "mi_results_HRS_pca_unadjusted.csv", sep = ""))


#not adjusted for pca 
ELSA_unadjusted_mi = read.csv(paste(directory, folder_unadjusted, "mi_results_ELSA_unadjusted.csv", sep = "")) 
results_HRS_unadjusted_mi = read.csv(paste(directory, folder_unadjusted, "mi_results_HRS_unadjusted.csv", sep = ""))


#coposite over waves 
#adjutsted for pca
HRS_composite_pca_unadjusted_mi = read.csv(paste(directory, folder_unadjusted, "MI_HRS_composite_pca_unadjusted.csv", sep = ""))
#not adjusted for pca 
HRS_composite_unadjusted_mi = read.csv(paste(directory, folder_unadjusted, "MI_HRS_composite_unadjusted.csv", sep = ""))

############
############

ELSA_pca_adjusted_mi = read.csv(paste(directory, folder_adjusted, "mi_results_ELSA_pca_adjusted.csv", sep = ""))
HRS_pca_adjusted_mi = read.csv(paste(directory, folder_adjusted, "mi_results_HRS_pca_adjusted.csv", sep = ""))


#not adjusted for pca 
ELSA_adjusted_mi = read.csv(paste(directory, folder_adjusted, "mi_results_ELSA_adjusted.csv", sep = "")) 
results_HRS_adjusted_mi = read.csv(paste(directory, folder_adjusted, "mi_results_HRS_adjusted.csv", sep = ""))


#coposite over waves 
#adjutsted for pca
HRS_composite_pca_adjusted_mi = read.csv(paste(directory, folder_adjusted, "MI_HRS_composite_pca_adjusted.csv", sep = ""))
#not adjusted for pca 
HRS_composite_adjusted_mi = read.csv(paste(directory, folder_adjusted, "MI_HRS_composite_adjusted.csv", sep = ""))

