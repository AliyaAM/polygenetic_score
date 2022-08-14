

subsetting_function = function (data_ELSA, 
                                  
                                  
                                  
                                  subsetting_VAR1_ELSA, 
                                  subsetting_VAR2_ELSA,
                                  
                                  ELSA_var1_value,
                                  
                                  ELSA_var2_value){
  
  #data_HRS <- data_HRS[ , subsetting_VAR_HRS]
#data_ELSA <- data_ELSA[ , subsetting_VAR_ELSA]


# subsetting data to the right variable for the analysis (eg, sex, physical lim.)
# if there is only one subsetting var: subsetting_VAR1_ELSA and subsetting_VAR1_HRS
if (subsetting_VAR1_ELSA == "NA" & subsetting_VAR2_ELSA =="NA"){
  
  data_ELSA_subset = data_ELSA 
} 

if (subsetting_VAR1_ELSA != "NA" & subsetting_VAR2_ELSA =="NA"){
  
  data_ELSA_subset = subset(data_ELSA, data_ELSA[ , subsetting_VAR1_ELSA] == ELSA_var1_value)
  
  print("done 1")
} 

if (subsetting_VAR1_ELSA != "NA" & subsetting_VAR2_ELSA !="NA"){
  
  data_ELSA_subset = subset(data_ELSA, data_ELSA[ , subsetting_VAR1_ELSA] == ELSA_var1_value & data_ELSA[subsetting_VAR2_ELSA] == ELSA_var2_value)
  print("done 2")
  
}



return(data_ELSA_subset)

}
