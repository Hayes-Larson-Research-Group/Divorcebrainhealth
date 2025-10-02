# Author: Suhani Amin
# Purpose: generate Table 1 data (with and without MI) for each sample (cognitive, MRI, PET)
# v2 - Added PET data

library(gtsummary)
library(tidyverse)
library(magrittr)
library(labelled)
library(openxlsx)


load(file = ".../divorce_petmri_v3.Rdata")
output_path <- "/.../"


all_dat_final_df2 <- all_dat_final %>% mutate(W1_PREV_DIVORCE_YN = case_when(all_dat_final$W1_PREV_DIVORCE== 0 ~ 'Never experienced a divorce',
                                                                                 all_dat_final$W1_PREV_DIVORCE == 1 ~ 'Experienced a divorce',
                                                                                 TRUE ~ NA)) %>%
  mutate(W1_INCMRANGE_COLLAPSE = case_when(all_dat_final$W1_INCMRANGE_HMNZD %in% c(1,2,3,4,5) ~ "$0 - $34,999",
         all_dat_final$W1_INCMRANGE_HMNZD == 6 ~ "$35,000 - $74,999",
         all_dat_final$W1_INCMRANGE_HMNZD == 7 ~ "$75,000 - $99,999",
         all_dat_final$W1_INCMRANGE_HMNZD == 8 ~ "$100,000 - $124,999",
         all_dat_final$W1_INCMRANGE_HMNZD == 9 ~ "$125,000 and over")) %>% 
  
  mutate(W1_INCMRANGE_COLLAPSE = factor(W1_INCMRANGE_COLLAPSE, 
                                        levels = c("$0 - $34,999", "$35,000 - $74,999", 
                                                   "$75,000 - $99,999", "$100,000 - $124,999", 
                                                   "$125,000 and over"))) %>%
  
  mutate(W1_HEALTH_SUMMARY = case_when(all_dat_final$W1_HEALTH == 1 ~ "Excellent",
                                           all_dat_final$W1_HEALTH == 2 ~ "Very good",
                                           all_dat_final$W1_HEALTH == 3 ~ "Good",
                                           all_dat_final$W1_HEALTH == 4 ~ "Fair",
                                           all_dat_final$W1_HEALTH == 5 ~ "Poor")) %>% 
  
  mutate(W1_HEALTH_SUMMARY = factor(W1_HEALTH_SUMMARY, 
                                        levels = c("Poor", "Fair", 
                                                   "Good", "Very good", 
                                                   "Excellent"))) %>%
  set_variable_labels(
    STUDY = 'Cohort',
    W1_INTERVIEW_AGE = 'Age at baseline (years)',
    W1_SENAS_EXEC_POOLZ = 'Wave 1 executive function (z-score)',
    W1_SENAS_VRMEM_POOLZ = 'Wave 1 verbal memory (z-score)',
    W1_FEMALE = 'Female',
    W1_D_RACE_SUMMARY = 'Race/ethnicity',
    W1_MOMORDAD_EDU_GT12 = 'Parent with over 12 years of education',
    W1_USBORN = 'US born',
    W1_SOUTHERN_BIRTH = "Southern birth",
    W1_CHD_OKFINANCIALLY = "Financially average or well-off in childhood",
    W1_CHD_EVERHUNGRY = "Ever insufficient money for food in childhood",
    W1_CHD_COMMSTANDING = "Family community standing in childhood (1=lowest, 10=highest)",
    W1_PAR_SEPDIV = "Parents have ever separated or divorced",
    W1_EDU_YRS_CERT = 'Years of education',
    W1_MARRIED = "Married/partnered",
    W1_INCMRANGE_COLLAPSE = "Annual household income",
    W1_HEALTH_SUMMARY = "Quality of health",
    age_at_mri = "Age at MRI (years)",
    Cerebrum_tcv = "Total cranial volume",
    Cerebrum_tcb = "Total brain volume",
    Total_hippo = "Total hippocampus",
    Total_gray = "Total gray matter",
    Total_white = "Total white matter",
    Total_wmh = "Total white matter hyperintensity",
    Total_brain = "Total brain",
    Frontal_Cortical = "Frontal lobe",
    Occipital_Cortical = "Occipital lobe",
    Parietal_Cortical = "Parietal lobe",
    Temporal_Cortical = "Temporal lobe",
    age_at_pet = "Age at PET scan",
    Abeta_pos = "Amyloid-positive"
    
  ) %>% 
  mutate(MRI_sample_YN = case_when(MRI_sample == 1 ~ "MRI Sample",
                                         MRI_sample == 0 ~ "Not in MRI Sample")) %>%
  mutate(PET_sample_YN = case_when(PET_sample == 1 ~ "PET Sample",
                                   PET_sample == 0 ~ "Not in PET Sample")) %>%
  
  purrr::modify_if(labelled::is.labelled, labelled::to_factor)


var_order <- c(
  "STUDY", "W1_INTERVIEW_AGE", "W1_FEMALE", "W1_MARRIED", "W1_D_RACE_SUMMARY", "W1_HEALTH_SUMMARY","W1_USBORN",
  "W1_SOUTHERN_BIRTH", "W1_INCMRANGE_COLLAPSE", "W1_MOMORDAD_EDU_GT12", "W1_CHD_OKFINANCIALLY",
  "W1_CHD_EVERHUNGRY", "W1_CHD_COMMSTANDING", "W1_EDU_YRS_CERT",
  "W1_PAR_SEPDIV",
  "age_at_mri", "age_at_pet", "Abeta_pos"
  # "Cerebrum_tcv", "Cerebrum_tcb",
  # "Total_hippo", "Total_gray", "Total_white", "Total_wmh", "Total_brain",
  # "Frontal_Cortical", "Occipital_Cortical", "Parietal_Cortical", "Temporal_Cortical"
)




#do this in all data
## create table 1

tab1_cogsamp <- all_dat_final_df2 %>%
  tbl_summary(missing = "ifany",
              missing_text="Missing",
              by='W1_PREV_DIVORCE_YN',
              type = list(all_continuous() ~ "continuous2"),
              statistic = list(all_continuous() ~ c("{mean} ({sd})"),
                               all_categorical() ~ "{n} ({p}%)"),
              digits = list(all_categorical() ~ c(0, 1),
                            all_continuous() ~ c(1,1)),
              include = c(all_of(var_order))) %>%
  add_stat_label(location = "row") %>%
  modify_column_indent(columns=label, undo = T) %>%
  bold_labels() %>%
  add_overall(last = TRUE)

tab1_cogsamp

tab1_df<-tab1_cogsamp %>% as_tibble()

write.xlsx(tab1_df,
           paste0(output_path, "table1_allsamp_petmri_v3.xlsx")
)



all_dat_final_df_complete <- all_dat_final_df2[complete.cases(all_dat_final_df2[ , c("STUDY", 
                                                                                     "W1_INTERVIEW_AGE",
                                                                                     "W1_FEMALE",
                                                                                     "W1_D_RACE_SUMMARY",
                                                                                     "W1_USBORN",
                                                                                     "W1_SOUTHERN_BIRTH",
                                                                                     # "W1_MOMORDAD_EDU_GT12",
                                                                                     # "W1_CHD_OKFINANCIALLY",
                                                                                     # "W1_CHD_EVERHUNGRY",
                                                                                     # "W1_CHD_COMMSTANDING",
                                                                                     "W1_EDU_YRS_CERT",
                                                                                     "W1_PAR_SEPDIV",
                                                                                     "age_at_mri", 
                                                                                    "Cerebrum_tcb",
                                                                                     "Total_hippo", "Total_gray", "Total_white", "Total_wmh", "Total_brain",
                                                                                     "Frontal_Cortical", "Occipital_Cortical", "Parietal_Cortical", "Temporal_Cortical")]), ]


tab1_cogsamp_complete <- all_dat_final_df_complete %>%  
  tbl_summary(missing = "ifany", 
              missing_text="Missing",
              by='W1_PREV_DIVORCE_YN',
              type = list(all_continuous() ~ "continuous2"),
              statistic = list(all_continuous() ~ c("{mean} ({sd})"),
                               all_categorical() ~ "{n} ({p}%)"),
              digits = list(all_categorical() ~ c(0, 1),
                            all_continuous() ~ c(1,1)),
              include = c(all_of(var_order))) %>%
  add_stat_label(location = "row") %>%
  modify_column_indent(columns=label, undo = T) %>%
  bold_labels() %>%
  add_overall(last = TRUE)

tab1_cogsamp_complete

tab1_df_complete<-tab1_cogsamp_complete %>% as_tibble()

write.xlsx(tab1_df_complete,
  paste0(output_path, "table1_allsamp_completecases_petmri_v3.xlsx")
)

save(all_dat_final_df_complete, file=".../divorce_completecases_petmri_v3.Rdata")

