# datasets to split into:

# sociodemographics
data_main <- subset(
  data,
  select=c(
    "record_number", 
    "hosp_number",                             
    "health_centre",                            
    "hospital_visited",                        
    "num_hospital_visits",                                  
    "health_status",  
    "age",                                     
    "health_insurance_status",                  
    "education",                               
    "marital_status",                           
    "occupation",                              
    "parity",                                   
    "household_size",                          
    "works",                                    
    "occupation_main",                         
    "occupation_main_specific",                 
    "monthly_income",                          
    "religion",                                 
    "belief_cannot_control_health",            
    "belief_things_do_affect_pregnancy",        
    "behaviours_exercise",                      
    "behaviours_fruit_veg",                    
    "behaviours_less_bad_food",                 
    "behaviours_less_salt",                    
    "behaviours_eat_protein",                   
    "behaviours_eat_carbs",                    
    "behaviours_lower_blood_pressure",          
    "behaviours_control_diabetes",             
    "behaviours_smoke_less",                    
    "seen_nurse_last_year",                    
    "breastfeeding",                            
    "breastfeeding_how_often",                 
    "breastfeeding_meals_per_day",              
    "ante_natal_vitamins",                     
    "pregnancy_wanted",                         
    "pregnancy_wanted_why",                    
    "family_planning_important",                
    "family_planning_important_yes_why",       
    "family_planning_important_no_why",        
    "used_contraceptives",                     
    "contraceptives_helped",   
    "mother_hb",                               
    "height",                                   
    "bp_ratio",                                
    "bmi",  
    "delivery_complications",                   
    "delivery_facts",                                      
    "family_planning",                         
    "family_planning_where",                    
    "sex_education",                           
    "sex_education_when",                       
    "family_planning_who_responsible",         
    "family_planning_who_responsible_specific", 
    "family_planning_charges",                 
    "family_planning_charges_expensive"    
    )                  
  )


# visits
data_visits <- subset(
  data,
  select=c(
    "record_number", 
    "visit_1_date",                            
    "visit_1_weight",                           
    "visit_1_bp_num",                          
    "visit_1_bp_denom",                         
    "visit_1_urine_protein",                   
    "visit_1_urine_sugar",                      
    "visit_1_gestation",                       
    "visit_2_date",                             
    "visit_2_weight",                          
    "visit_2_bp_num",                           
    "visit_2_bp_denom",                        
    "visit_2_urine_protein",                    
    "visit_2_urine_sugar",                     
    "visit_2_gestation",                        
    "visit_3_date",                            
    "visit_3_weight",                           
    "visit_3_bp_num",                          
    "visit_3_bp_denom",                         
    "visit_3_urine_protein",                   
    "visit_3_urine_sugar",                      
    "visit_3_gestation",                       
    "visit_4_date",                             
    "visit_4_weight",                          
    "visit_4_bp_num",                           
    "visit_4_bp_denom",                        
    "visit_4_urine_protein",                    
    "visit_4_urine_sugar",                     
    "visit_4_gestation",                        
    "visit_5_date",                            
    "visit_5_weight",                           
    "visit_5_bp_num",                          
    "visit_5_bp_denom",                         
    "visit_5_urine_protein",                   
    "visit_5_urine_sugar",                      
    "visit_5_gestation",                       
    "visit_6_date",                             
    "visit_6_weight",                          
    "visit_6_bp_num",                           
    "visit_6_bp_denom",                        
    "visit_6_urine_protein",                    
    "visit_6_urine_sugar",                     
    "visit_6_gestation",                        
    "visit_7_date",                            
    "visit_7_weight",                           
    "visit_7_bp_num",                          
    "visit_7_bp_denom",                         
    "visit_7_urine_protein",                   
    "visit_7_urine_sugar",                      
    "visit_7_gestation",                       
    "visit_8_date",                             
    "visit_8_weight",                          
    "visit_8_bp_num",                           
    "visit_8_bp_denom",                        
    "visit_8_urine_protein",                    
    "visit_8_urine_sugar",                     
    "visit_8_gestation"      
    
    )
  )

reform <- function(dta){
  visits <- 1:8
  output <- data.frame(
    record_number=NULL,
    visit_number=NULL,
    date= NULL,
    weight=NULL,
    bp_num=NULL,
    bp_denom=NULL,
    urine_protein=NULL,
    urine_sugar=NULL,
    gestation=NULL
    )
  all_names <- names(dta)
  for (i in visits){
    names_of_interest <- c(
      "record_number",
      all_names[
        str_detect(
          string=all_names, 
          pattern=paste0("^visit_", i, "_")
        )
      ]
    )
    dta_ss <- subset(dta, select=names_of_interest)
    dta_ss <- data.frame(record_number=dta_ss[,1], visit_number=i, dta_ss[,-1])
    names(dta_ss) <- gsub(paste0("^visit_", i, "_"), "", names(dta_ss))
    output <- rbind(output, dta_ss)
  }
  
  return(output)
}

data_visits <- reform(data_visits)

# family planning methods
data_family_planning_methods <- subset(
  data,
  select=c(
    "record_number" ,
    "family_planning_method_1",                
    "family_planning_method_2",                 
    "family_planning_method_3",                
    "family_planning_method_4",                 
    "family_planning_method_5",                
    "family_planning_method_6",                 
    "family_planning_method_7",                
    "family_planning_method_8",                 
    "family_planning_method_9",                
    "family_planning_method_10",                
    "family_planning_method_11",               
    "family_planning_method_12",                
    "family_planning_method_13",               
    "family_planning_method_14"       
    )
  )


# contraceptives_methods
data_contraceptive_methods <- subset(
  data,
  select=c(
    "record_number" ,
    "used_contraceptives_method_1",             
    "used_contraceptives_method_2" ,           
    "used_contraceptives_method_3",             
    "used_contraceptives_method_4",            
    "used_contraceptives_method_5",             
    "used_contraceptives_method_6",            
    "used_contraceptives_method_7",             
    "used_contraceptives_method_8",            
    "used_contraceptives_method_9",             
    "used_contraceptives_method_10",           
    "used_contraceptives_method_11",            
    "used_contraceptives_method_12",           
    "used_contraceptives_method_13",            
    "used_contraceptives_method_14"      
    )
  )




# diseased

data_diseased <- subset(
  data,
  select=c(
    "record_number" ,
    "whether_diseased"       ,                 
    "whether_diseased_what"   ,
    "whether_diseased_this_1" ,                 
    "whether_diseased_this_2" ,                
    "whether_diseased_this_3" ,                 
    "whether_diseased_this_4" ,
    "whether_diseased_other"   
    )
)

# birth details

data_birth_details <- subset(
  data,
  select=c(
    "record_number",
    "geometric_gest_age",                     
    "baby_birth_weight",                        
    "baby_sex",                                
    "baby_birth_gestation",                     
    "baby_birth_gestation_2",                  
    "baby_birth_gestation_status",              
    "baby_birth_bps",                          
    "baby_birth_bpd",                           
    "baby_birth_wta",                          
    "baby_hb"     
    )
  )

  


# information_interest
data_information_interest <- subset(
  data,
  select=c(
    "record_number" ,
    "information_interest_1" ,                 
    "information_interest_2"  ,                 
    "information_interest_3" ,                 
    "information_interest_4"    
    )
  )

# true_about_deliveries 
data_true_deliveries <- subset(
  data,
  select=c(
    "record_number", 
    "true_about_deliveries_1",                 
    "true_about_deliveries_2" ,                 
    "true_about_deliveries_3"     
    )
  )

 

write.csv(data_birth_details, "data/csv/data_birth_details.csv")
write.csv(data_contraceptive_methods, "data/csv/contraceptive_methods.csv")
write.csv(data_diseased, "data/csv/data_diseased.csv")
write.csv(data_family_planning_methods, "data/csv/data_family_planning_methods.csv")
write.csv(data_information_interest, "data/csv/data_information_interest.csv")
write.csv(data_visits, "data/csv/data_visits.csv")
write.csv(data_true_deliveries, "data/csv/data_true_deliveries.csv")
write.csv(data_main, "data/csv/data_main.csv")

                


               
    
         
                               
                                   
