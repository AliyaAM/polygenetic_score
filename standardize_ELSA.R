
library(dplyr)


directory = "/Users/aliyaamirova/Documents/KCL_postDoc/"

SOURCE_ROOT = "Data_analysis/"
OUTPUT_ROOT = "Data_analysis/"

#ELSA polygenic scores data: 

polygenic_scores_ELSA_data = read.csv(paste(directory, SOURCE_ROOT, "DATA_ELSA/PGS_ELSA_2021.csv", sep = ""))

#polygenic_scores_ELSA_data = read.csv( paste(directory, SOURCE_ROOT   #"/Users/aliya/my_docs/KCL_postDoc/polygenic_score/ELSA_PGS_data/

## To aid interpretability of the results, all PGSs were standardised (mean = 0, SD = 1). In this context, a 1 standard deviation increase in the explanatory variable is equivalent to a unit increase in the standardized version of the variable.: https://bmcpsychiatry.biomedcentral.com/articles/10.1186/s12888-022-03717-5#MOESM1
polygenic_scores_ELSA_data$Diabetes = scale(polygenic_scores_ELSA_data$Diabetes)
polygenic_scores_ELSA_data$loneliness_2018 = scale(polygenic_scores_ELSA_data$loneliness_2018) 
polygenic_scores_ELSA_data$CT_2020 = scale(polygenic_scores_ELSA_data$CT_2020) 
polygenic_scores_ELSA_data$migraine_2016 = scale(polygenic_scores_ELSA_data$migraine_2016)
polygenic_scores_ELSA_data$chronic_pain_2018 = scale(polygenic_scores_ELSA_data$chronic_pain_2018)
polygenic_scores_ELSA_data$IQ_2018 = scale(polygenic_scores_ELSA_data$IQ_2018)
polygenic_scores_ELSA_data$ASD_2016 = scale(polygenic_scores_ELSA_data$ASD_2016)
polygenic_scores_ELSA_data$ADHD_2017 = scale(polygenic_scores_ELSA_data$ADHD_2017)
polygenic_scores_ELSA_data$GC_2018 = scale(polygenic_scores_ELSA_data$GC_2018)
polygenic_scores_ELSA_data$Aggression_2015 = scale(polygenic_scores_ELSA_data$Aggression_2015)
polygenic_scores_ELSA_data$CAD_2018 = scale(polygenic_scores_ELSA_data$CAD_2018)
polygenic_scores_ELSA_data$Grip_2017 = scale(polygenic_scores_ELSA_data$Grip_2017)
polygenic_scores_ELSA_data$SWB = scale(polygenic_scores_ELSA_data$SWB)
polygenic_scores_ELSA_data$CRP = scale(polygenic_scores_ELSA_data$CRP)
polygenic_scores_ELSA_data$MDD19 = scale(polygenic_scores_ELSA_data$MDD19) 
polygenic_scores_ELSA_data$SMK_AgeInitiated19 = scale(polygenic_scores_ELSA_data$SMK_AgeInitiated19)
polygenic_scores_ELSA_data$LONELINESS = scale(polygenic_scores_ELSA_data$LONELINESS)
polygenic_scores_ELSA_data$BMI2018 = scale(polygenic_scores_ELSA_data$BMI2018)
polygenic_scores_ELSA_data$DrinksPerWeek19 = scale(polygenic_scores_ELSA_data$DrinksPerWeek19)
polygenic_scores_ELSA_data$SmokingCessation19 = scale(polygenic_scores_ELSA_data$SmokingCessation19 )
polygenic_scores_ELSA_data$NUM_CIG_DAY_2019 = scale(polygenic_scores_ELSA_data$NUM_CIG_DAY_2019)
polygenic_scores_ELSA_data$SmokingInitiation_2019 = scale(polygenic_scores_ELSA_data$SmokingInitiation_2019)
polygenic_scores_ELSA_data$Child_Aggression = scale(polygenic_scores_ELSA_data$Child_Aggression)
polygenic_scores_ELSA_data$EA_2 = scale(polygenic_scores_ELSA_data$EA_2 )
polygenic_scores_ELSA_data$EA_3 = scale(polygenic_scores_ELSA_data$EA_3)
polygenic_scores_ELSA_data$AGREE = scale(polygenic_scores_ELSA_data$AGREE)
polygenic_scores_ELSA_data$CON = scale(polygenic_scores_ELSA_data$CON)
polygenic_scores_ELSA_data$EXTRA = scale(polygenic_scores_ELSA_data$EXTRA)
polygenic_scores_ELSA_data$NEURO = scale(polygenic_scores_ELSA_data$NEURO) 
polygenic_scores_ELSA_data$OPEN = scale(polygenic_scores_ELSA_data$OPEN)
polygenic_scores_ELSA_data$DS = scale(polygenic_scores_ELSA_data$DS)
polygenic_scores_ELSA_data$GC = scale(polygenic_scores_ELSA_data$GC)
polygenic_scores_ELSA_data$ANXIETY_FC = scale(polygenic_scores_ELSA_data$ANXIETY_FC)
polygenic_scores_ELSA_data$SMK_EVER = scale(polygenic_scores_ELSA_data$SMK_EVER)
polygenic_scores_ELSA_data$SMK_NUMBER = scale(polygenic_scores_ELSA_data$SMK_NUMBER)
polygenic_scores_ELSA_data$CAD = scale(polygenic_scores_ELSA_data$CAD )
polygenic_scores_ELSA_data$RA = scale(polygenic_scores_ELSA_data$RA)
polygenic_scores_ELSA_data$MI = scale(polygenic_scores_ELSA_data$MI)
polygenic_scores_ELSA_data$LONGEVITY = scale(polygenic_scores_ELSA_data$LONGEVITY)
polygenic_scores_ELSA_data$Height = scale(polygenic_scores_ELSA_data$Height)
polygenic_scores_ELSA_data$WHR = scale(polygenic_scores_ELSA_data$WHR)
polygenic_scores_ELSA_data$Waist = scale(polygenic_scores_ELSA_data$Waist)
polygenic_scores_ELSA_data$BMI = scale(polygenic_scores_ELSA_data$BMI )
polygenic_scores_ELSA_data$M_Plasma = scale(polygenic_scores_ELSA_data$M_Plasma)
polygenic_scores_ELSA_data$AGE_MENARCHE = scale(polygenic_scores_ELSA_data$AGE_MENARCHE)
polygenic_scores_ELSA_data$AGE_MENOPAUSE = scale(polygenic_scores_ELSA_data$AGE_MENOPAUSE)
polygenic_scores_ELSA_data$Age_Birth_F = scale(polygenic_scores_ELSA_data$Age_Birth_F)
polygenic_scores_ELSA_data$Age_Birth_M = scale(polygenic_scores_ELSA_data$Age_Birth_M)
polygenic_scores_ELSA_data$NEB_F = scale(polygenic_scores_ELSA_data$NEB_F)
polygenic_scores_ELSA_data$NEB_M = scale(polygenic_scores_ELSA_data$NEB_M )
polygenic_scores_ELSA_data$SLP_DUR = scale(polygenic_scores_ELSA_data$SLP_DUR)
polygenic_scores_ELSA_data$DAI = scale(polygenic_scores_ELSA_data$DAI)
polygenic_scores_ELSA_data$INS_COM = scale(polygenic_scores_ELSA_data$INS_COM)
polygenic_scores_ELSA_data$SEC_DEP = scale(polygenic_scores_ELSA_data$SEC_DEP)
polygenic_scores_ELSA_data$ANXIETY_CC = scale(polygenic_scores_ELSA_data$ANXIETY_CC)
polygenic_scores_ELSA_data$Gait_speed_2017 = scale(polygenic_scores_ELSA_data$Gait_speed_2017)
polygenic_scores_ELSA_data$INT_2014 = scale(polygenic_scores_ELSA_data$INT_2014)
polygenic_scores_ELSA_data$ALZ_2013 = scale(polygenic_scores_ELSA_data$ALZ_2013 )
polygenic_scores_ELSA_data$ALZ_2019 = scale(polygenic_scores_ELSA_data$ALZ_2019)
polygenic_scores_ELSA_data$SZ_2014 = scale(polygenic_scores_ELSA_data$SZ_2014)
polygenic_scores_ELSA_data$SZ_2020 = scale(polygenic_scores_ELSA_data$SZ_2020)
polygenic_scores_ELSA_data$T2D_2018 = scale(polygenic_scores_ELSA_data$T2D_2018)
polygenic_scores_ELSA_data$ADHD_2017 = scale(polygenic_scores_ELSA_data$ADHD_2017)

polygenic_scores_ELSA_data

write.csv(polygenic_scores_ELSA_data, file = paste(directory, SOURCE_ROOT, "HRS_2012_data/polygenic_scores_ELSA_data_standardized.csv", sep = "")) 
