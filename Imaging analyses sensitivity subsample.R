# Added PET data analyses and adjusted Model 2/3 to include years of education
library(tidyverse)
library(mice)
library(sandwich)
library(broom)
library(lmtest)

load(file = ".../divorce_completecases_petmri.Rdata")

data<-all_dat_final_df_complete
data_over65 <- data %>% filter(age_at_mri>=65.00)
data_pet <- data %>% filter(PET_sample==1)
# data <- data[!apply(data[, c("Total_wmh_log")], 1, function(x) any(is.infinite(x))), ]



  
options(scipen=999)

vols<-c("Cerebrum_tcb", "Total_hippo", 
        "Total_gray", "Total_white", "Total_wmh", "Total_brain", 
        "Frontal_Cortical", "Occipital_Cortical", "Parietal_Cortical", "Temporal_Cortical"
)

all_vol_outcomes<-c(paste0(vols, "_resid"), 
                    paste0(vols, "_residblom"), 
                    paste0(vols, "_residzscore"), 
                    paste0(vols, "_residlog"), 
                    "Total_wmh_log")

all_expvars<-c("W1_PREV_DIVORCE") 

#Suhani to figure out how to pull beta coefficients, std error, and CIs from model output

models1<-expand_grid(all_expvars,all_vol_outcomes)
models2<-expand_grid(all_expvars,vols) %>% rename(all_vol_outcomes=vols)

models <-rbind(models1, models2)

MRI_analysis<-function(dat=NULL){
  
  for (i in 1:nrow(models)){
    # i<-1
    exposure<-rlang::parse_expr(models$all_expvars[i])
    outcome<-rlang::parse_expr(models$all_vol_outcomes[i])
 
    
    temp.model1 <- lm(eval(outcome) ~ eval(exposure), data=dat)
    temp_model1 <- summary(temp.model1)
    temp_model1
    
    summary_model1_df <- as.data.frame(temp_model1$coefficients)
    conf_int_model1 <- confint(temp.model1)
    summary_df_bind_model1 <- cbind(summary_model1_df, conf_int_model1)
    colnames(summary_df_bind_model1) <- c("Estimate", "Std Error", "T Value", "P value", "2.5 %", "97.5 %")
    summary_df_bind_model1$Model <- "Model 1"
    summary_df_bind_model1$Outcome <- models$all_vol_outcomes[i]
    summary_df_bind_model1$Term <- rownames(summary_df_bind_model1)
    summary_df_bind_model1$Term <- ifelse(summary_df_bind_model1$Term == "eval(exposure)", "Divorced", 
                                          summary_df_bind_model1$Term)
                                   
    temp.summ.model1 <- summary_df_bind_model1 %>% 
      select("Term", "Model", "Outcome", "Estimate", "Std Error", "P value", "2.5 %", "97.5 %") 
    row.names(temp.summ.model1) <- NULL
    
    temp.model2 <- lm(eval(outcome) ~ eval(exposure)  + age_at_mri +
                          W1_FEMALE +
                          W1_SOUTHERN_BIRTH + W1_D_RACE_SUMMARY + W1_PAR_SEPDIV + W1_EDU_YRS_CERT, data=dat)
    
    temp_model2 <- summary(temp.model2)
    temp_model2
    
    summary_model2_df <- as.data.frame(temp_model2$coefficients)
    conf_int_model2 <- confint(temp.model2)
    summary_df_bind_model2 <- cbind(summary_model2_df, conf_int_model2)
    colnames(summary_df_bind_model2) <- c("Estimate", "Std Error", "T Value", "P value", "2.5 %", "97.5 %")
    summary_df_bind_model2$Model <- "Model 2"
    summary_df_bind_model2$Outcome <- models$all_vol_outcomes[i]
    summary_df_bind_model2$Term <- rownames(summary_df_bind_model2)
    summary_df_bind_model2$Term <- ifelse(summary_df_bind_model2$Term == "eval(exposure)", "Divorced", 
                                          summary_df_bind_model2$Term)
    temp.summ.model2 <- summary_df_bind_model2 %>% 
      select("Term", "Model", "Outcome", "Estimate", "Std Error", "P value", "2.5 %", "97.5 %") 
    row.names(temp.summ.model2) <- NULL
    
    
    temp.model3 <- lm(eval(outcome) ~ eval(exposure)  + age_at_mri +
                        W1_FEMALE +
                        W1_SOUTHERN_BIRTH + W1_D_RACE_SUMMARY + W1_PAR_SEPDIV + W1_EDU_YRS_CERT +
                          W1_MOMORDAD_EDU_GT12 + W1_CHD_OKFINANCIALLY +
                        W1_CHD_EVERHUNGRY + W1_CHD_COMMSTANDING, data=dat)
    temp_model3 <- summary(temp.model3)
    temp_model3
    
    summary_model3_df <- as.data.frame(temp_model3$coefficients)
    conf_int_model3 <- confint(temp.model3)
    summary_df_bind_model3 <- cbind(summary_model3_df, conf_int_model3)
    colnames(summary_df_bind_model3) <- c("Estimate", "Std Error", "T Value", "P value", "2.5 %", "97.5 %")
    summary_df_bind_model3$Model <- "Model 3"
    summary_df_bind_model3$Outcome <- models$all_vol_outcomes[i]
    summary_df_bind_model3$Term <- rownames(summary_df_bind_model3)
    summary_df_bind_model3$Term <- ifelse(summary_df_bind_model3$Term == "eval(exposure)", "Divorced", 
                                          summary_df_bind_model3$Term)
    temp.summ.model3 <- summary_df_bind_model3 %>% 
      select("Term", "Model", "Outcome", "Estimate", "Std Error", "P value", "2.5 %", "97.5 %") 
    row.names(temp.summ.model3) <- NULL
    
    
    if (i==1){all_res<-rbind(temp.summ.model1, temp.summ.model2, temp.summ.model3)} else {
      all_res<-rbind(all_res, temp.summ.model1, temp.summ.model2, temp.summ.model3)
    }
    
    if (models$all_vol_outcomes[i] %in% vols){

      temp.adjvol.model4 <-with(data = data,
                             lm(eval(outcome) ~ eval(exposure) + Cerebrum_tcv))

      temp_adjvol_model4 <- summary(temp.adjvol.model4)
      temp_adjvol_model4

      summary_adjvol_model4_df <- as.data.frame(temp_adjvol_model4$coefficients)
      conf_int_adjvol_model4 <- confint(temp.adjvol.model4)
      summary_df_bind_adjvol_model4 <- cbind(summary_adjvol_model4_df, conf_int_adjvol_model4)
      colnames(summary_df_bind_adjvol_model4) <- c("Estimate", "Std Error", "T Value", "P value", "2.5 %", "97.5 %")
      summary_df_bind_adjvol_model4$Model <- "Model 4 Adjusted Vol"
      summary_df_bind_adjvol_model4$Outcome <- models$all_vol_outcomes[i]
      summary_df_bind_adjvol_model4$Term <- rownames(summary_df_bind_adjvol_model4)
      summary_df_bind_adjvol_model4$Term <- ifelse(summary_df_bind_adjvol_model4$Term == "eval(exposure)", "Divorced",
                                                   summary_df_bind_adjvol_model4$Term)

      temp.summ.adjvol.model4 <- summary_df_bind_adjvol_model4 %>%
        select("Term", "Model", "Outcome", "Estimate", "Std Error", "P value", "2.5 %", "97.5 %")
      row.names(temp.summ.adjvol.model4) <- NULL

      temp.adjvol.model5 <-with(data = data,
                           lm(eval(outcome) ~ eval(exposure)  + age_at_mri +
                                W1_FEMALE + W1_SOUTHERN_BIRTH + W1_D_RACE_SUMMARY + W1_PAR_SEPDIV + W1_EDU_YRS_CERT +
                                Cerebrum_tcv))
      temp_adjvol_model5 <- summary(temp.adjvol.model5)
      temp_adjvol_model5

      summary_adjvol_model5_df <- as.data.frame(temp_adjvol_model5$coefficients)
      conf_int_adjvol_model5 <- confint(temp.adjvol.model5)
      summary_df_bind_adjvol_model5 <- cbind(summary_adjvol_model5_df, conf_int_adjvol_model5)
      colnames(summary_df_bind_adjvol_model5) <- c("Estimate", "Std Error", "T Value", "P value", "2.5 %", "97.5 %")
      summary_df_bind_adjvol_model5$Model <- "Model 5 Adjusted Vol"
      summary_df_bind_adjvol_model5$Outcome <- models$all_vol_outcomes[i]
      summary_df_bind_adjvol_model5$Term <- rownames(summary_df_bind_adjvol_model5)
      summary_df_bind_adjvol_model5$Term <- ifelse(summary_df_bind_adjvol_model5$Term == "eval(exposure)", "Divorced",
                                                   summary_df_bind_adjvol_model5$Term)

      temp.summ.adjvol.model5 <- summary_df_bind_adjvol_model5 %>%
        select("Term", "Model", "Outcome", "Estimate", "Std Error", "P value", "2.5 %", "97.5 %")
      row.names(temp.summ.adjvol.model5) <- NULL



      temp.adjvol.model6 <-with(data = data,
                           lm(eval(outcome) ~ eval(exposure)  + age_at_mri +
                                W1_FEMALE +
                                W1_SOUTHERN_BIRTH + W1_D_RACE_SUMMARY + W1_PAR_SEPDIV + W1_EDU_YRS_CERT +
                                W1_MOMORDAD_EDU_GT12 + W1_CHD_OKFINANCIALLY + W1_CHD_EVERHUNGRY + W1_CHD_COMMSTANDING + 
                                Cerebrum_tcv))
      temp_adjvol_model6 <- summary(temp.adjvol.model6)
      temp_adjvol_model6

      summary_adjvol_model6_df <- as.data.frame(temp_adjvol_model6$coefficients)
      conf_int_adjvol_model6 <- confint(temp.adjvol.model6)
      summary_df_bind_adjvol_model6 <- cbind(summary_adjvol_model6_df, conf_int_adjvol_model6)
      colnames(summary_df_bind_adjvol_model6) <- c("Estimate", "Std Error", "T Value", "P value", "2.5 %", "97.5 %")
      summary_df_bind_adjvol_model6$Model <- "Model 6 Adjusted Vol"
      summary_df_bind_adjvol_model6$Outcome <- models$all_vol_outcomes[i]
      summary_df_bind_adjvol_model6$Term <- rownames(summary_df_bind_adjvol_model6)
      summary_df_bind_adjvol_model6$Term <- ifelse(summary_df_bind_adjvol_model6$Term == "eval(exposure)", "Divorced",
                                                   summary_df_bind_adjvol_model6$Term)

      temp.summ.adjvol.model6 <- summary_df_bind_adjvol_model6 %>%
        select("Term", "Model", "Outcome", "Estimate", "Std Error", "P value", "2.5 %", "97.5 %")
      row.names(temp.summ.adjvol.model6) <- NULL


      all_res<-rbind(all_res, temp.summ.adjvol.model4, temp.summ.adjvol.model5, temp.summ.adjvol.model6)

    }
  }
  return(all_res)
  
}

all_res_mri<-MRI_analysis(dat=data)
all_res_mri_over65<-MRI_analysis(dat=data_over65)
all_res_mri_petsamp <- MRI_analysis(dat=data_pet)

#PET Data
for (i in 1:length(all_expvars)){
  # i<-1
  exposure<-rlang::parse_expr(all_expvars[i])
  
  #Using log poisson with robust SE. This is needed for adjusted model, 
  # so using here as well for consistency.
  temp.mod.crude <-with(data_pet,
                        coeftest(glm(Abeta_pos ~ eval(exposure), 
                                     family=poisson(link="log")),
                                 vcov. = vcovHC(glm(Abeta_pos ~ eval(exposure), 
                                                    family=poisson(link="log")), type = "HC3")))
  # temp_adjvol_model6 <- summary(temp.adjvol.model6)
  # temp_adjvol_model6

  temp.mod.crude.summ <- temp.mod.crude
  temp.mod.crude.summ
  
  temp.mod.crude.summ.matrix <-  as.matrix(temp.mod.crude)
  class(temp.mod.crude.summ.matrix) <- NULL
  temp.mod.crude.summ.df <- as.data.frame(temp.mod.crude.summ.matrix)
  colnames(temp.mod.crude.summ.df) <- c("estimate", "std.error", "z.value", "p.value")
  
  temp.mod.crude.summ.df <- temp.mod.crude.summ.df %>%
    rownames_to_column("term") %>%
    mutate(
      `2.5 %` = estimate - 1.96 * std.error,
      `97.5 %` = estimate + 1.96 * std.error,
      RR = exp(estimate),
      LCI_RR = exp(`2.5 %`),
      UCI_RR = exp(`97.5 %`),
      exposure = all_expvars[i],
      outcome = "Amyloid positive", 
      model = "Crude",
      term = case_when(term == "eval(exposure)" ~ "Divorced",
                       TRUE ~ term)
    ) %>%
    select(term, estimate, std.error, `2.5 %`, `97.5 %`, RR, LCI_RR, UCI_RR, exposure, outcome, model)
  
  #Need robust SE's here because using poisson
  temp.mod.adj1 <-with(data_pet,
                       coeftest(glm(Abeta_pos ~ eval(exposure)  + age_at_pet +
                                      W1_FEMALE + W1_SOUTHERN_BIRTH + W1_D_RACE_SUMMARY + 
                                      W1_PAR_SEPDIV + W1_EDU_YRS_CERT,
                                    family=poisson(link="log")),
                                vcov. = vcovHC(glm(Abeta_pos ~ eval(exposure)  + age_at_pet +
                                                     W1_FEMALE + W1_SOUTHERN_BIRTH + W1_D_RACE_SUMMARY +
                                                     W1_PAR_SEPDIV + W1_EDU_YRS_CERT,
                                                   family=poisson(link="log")), type = "HC3")))
  
  temp.mod.adj1.summ <- temp.mod.adj1
  temp.mod.adj1.summ
  
  temp.mod.adj1.summ.matrix <-  as.matrix(temp.mod.adj1)
  class(temp.mod.adj1.summ.matrix) <- NULL
  temp.mod.adj1.summ.df <- as.data.frame(temp.mod.adj1.summ.matrix)
  colnames(temp.mod.adj1.summ.df) <- c("estimate", "std.error", "z.value", "p.value")
  
  temp.mod.adj1.summ.df <- temp.mod.adj1.summ.df %>%
    rownames_to_column("term") %>%
    mutate(
      `2.5 %` = estimate - 1.96 * std.error,
      `97.5 %` = estimate + 1.96 * std.error,
      RR = exp(estimate),
      LCI_RR = exp(`2.5 %`),
      UCI_RR = exp(`97.5 %`),
      exposure = all_expvars[i],
      outcome = "Amyloid positive", 
      model = "Adjusted 1",
      term = case_when(term == "eval(exposure)" ~ "Divorced",
                       TRUE ~ term)
    ) %>%
    select(term, estimate, std.error, `2.5 %`, `97.5 %`, RR, LCI_RR, UCI_RR, exposure, outcome, model)
  
  
  #Need robust SE's here because using poisson
  temp.mod.adj2 <-with(data_pet,
                      coeftest(glm(Abeta_pos ~ eval(exposure)  + age_at_pet +
                                     W1_FEMALE + W1_SOUTHERN_BIRTH + W1_D_RACE_SUMMARY + W1_PAR_SEPDIV + W1_EDU_YRS_CERT +
                                   W1_MOMORDAD_EDU_GT12 + W1_CHD_OKFINANCIALLY + 
                                     W1_CHD_EVERHUNGRY + W1_CHD_COMMSTANDING,
                                   family=poisson(link="log")),
                               vcov. = vcovHC(glm(Abeta_pos ~ eval(exposure)  + age_at_pet +
                                                    W1_FEMALE +
                                                    W1_SOUTHERN_BIRTH + W1_D_RACE_SUMMARY + W1_PAR_SEPDIV + W1_EDU_YRS_CERT +
                                                    W1_MOMORDAD_EDU_GT12 + W1_CHD_OKFINANCIALLY +
                                                    W1_CHD_EVERHUNGRY + W1_CHD_COMMSTANDING,
                                                  family=poisson(link="log")), type = "HC3")))
  
  temp.mod.adj2.summ <- temp.mod.adj2
  temp.mod.adj2.summ
  
  temp.mod.adj2.summ.matrix <-  as.matrix(temp.mod.adj2)
  class(temp.mod.adj2.summ.matrix) <- NULL
  temp.mod.adj2.summ.df <- as.data.frame(temp.mod.adj2.summ.matrix)
  colnames(temp.mod.adj2.summ.df) <- c("estimate", "std.error", "z.value", "p.value")
  
  temp.mod.adj2.summ.df <- temp.mod.adj2.summ.df %>%
    rownames_to_column("term") %>%
    mutate(
      `2.5 %` = estimate - 1.96 * std.error,
      `97.5 %` = estimate + 1.96 * std.error,
      RR = exp(estimate),
      LCI_RR = exp(`2.5 %`),
      UCI_RR = exp(`97.5 %`),
      exposure = all_expvars[i],
      outcome = "Amyloid positive", 
      model = "Adjusted 2",
      term = case_when(term == "eval(exposure)" ~ "Divorced",
                       TRUE ~ term)
    ) %>%
    select(term, estimate, std.error, `2.5 %`, `97.5 %`, RR, LCI_RR, UCI_RR, exposure, outcome, model)
  
  if (i==1){all_res_pet <-rbind(temp.mod.crude.summ.df, temp.mod.adj1.summ.df, temp.mod.adj2.summ.df)} else {
    all_res_pet <-rbind(all_res_pet, temp.mod.crude.summ.df, temp.mod.adj1.summ.df, temp.mod.adj2.summ.df)
  }
  
}

for (i in 1:length(all_expvars)){
  #i<-1
  exposure<-rlang::parse_expr(all_expvars[i])
  
  temp.mod.SUVR.crude <-with(data_pet,
                             lm(Landau_All_FSR ~ eval(exposure)))
  
  temp.mod.SUVR.crude.summ <- summary(temp.mod.SUVR.crude)
  temp.mod.SUVR.crude.summ
  
  temp.mod.SUVR.crude.summ <- as.data.frame(temp.mod.SUVR.crude.summ$coefficients)
  conf_int_SUVR_crude <- confint(temp.mod.SUVR.crude)
  temp.mod.SUVR.crude.bind <- cbind(temp.mod.SUVR.crude.summ, conf_int_SUVR_crude)
  colnames(temp.mod.SUVR.crude.bind) <- c("Estimate", "Std Error", "T Value", "P value", "2.5 %", "97.5 %")
  temp.mod.SUVR.crude.bind$Model <- "Crude"
  temp.mod.SUVR.crude.bind$Outcome <- "SUVR"
  temp.mod.SUVR.crude.bind$Term <- rownames(temp.mod.SUVR.crude.bind)
  temp.mod.SUVR.crude.bind$Term <- ifelse(temp.mod.SUVR.crude.bind$Term == "eval(exposure)", "Divorced", 
                                          temp.mod.SUVR.crude.bind$Term)  
  
  temp.mod.SUVR.crude <- temp.mod.SUVR.crude.bind %>% 
    select("Term", "Model", "Outcome", "Estimate", "Std Error", "P value", "2.5 %", "97.5 %") 
  row.names(temp.mod.SUVR.crude) <- NULL
  
  temp.mod.SUVR.adj1 <-with(data_pet,
                            lm(Landau_All_FSR ~ eval(exposure)  + age_at_pet +
                                 W1_FEMALE + W1_SOUTHERN_BIRTH + W1_D_RACE_SUMMARY +
                                 W1_PAR_SEPDIV + W1_EDU_YRS_CERT))
  
  temp.mod.SUVR.adj1.summ <- summary(temp.mod.SUVR.adj1)
  temp.mod.SUVR.adj1.summ
  
  temp.mod.SUVR.adj1.summ <- as.data.frame(temp.mod.SUVR.adj1.summ$coefficients)
  conf_int_SUVR_adj1 <- confint(temp.mod.SUVR.adj1)
  temp.mod.SUVR.adj1.bind <- cbind(temp.mod.SUVR.adj1.summ, conf_int_SUVR_adj1)
  colnames(temp.mod.SUVR.adj1.bind) <- c("Estimate", "Std Error", "T Value", "P value", "2.5 %", "97.5 %")
  temp.mod.SUVR.adj1.bind$Model <- "Adjusted 1"
  temp.mod.SUVR.adj1.bind$Outcome <- "SUVR"
  temp.mod.SUVR.adj1.bind$Term <- rownames(temp.mod.SUVR.adj1.bind)
  temp.mod.SUVR.adj1.bind$Term <- ifelse(temp.mod.SUVR.adj1.bind$Term == "eval(exposure)", "Divorced", 
                                         temp.mod.SUVR.adj1.bind$Term)  
  temp.mod.SUVR.adj1 <- temp.mod.SUVR.adj1.bind %>% 
    select("Term", "Model", "Outcome", "Estimate", "Std Error", "P value", "2.5 %", "97.5 %") 
  row.names(temp.mod.SUVR.adj1) <- NULL
  
  
  temp.mod.SUVR.adj2 <-with(data_pet,
                            lm(Landau_All_FSR ~ eval(exposure)  + age_at_pet +
                                 W1_FEMALE + W1_SOUTHERN_BIRTH + W1_D_RACE_SUMMARY + W1_PAR_SEPDIV + W1_EDU_YRS_CERT +
                                 W1_MOMORDAD_EDU_GT12 + W1_CHD_OKFINANCIALLY + 
                                 W1_CHD_EVERHUNGRY + W1_CHD_COMMSTANDING))
  
  temp.mod.SUVR.adj2.summ <- summary(temp.mod.SUVR.adj2)
  temp.mod.SUVR.adj2.summ
  
  temp.mod.SUVR.adj2.summ <- as.data.frame(temp.mod.SUVR.adj2.summ$coefficients)
  conf_int_SUVR_adj2 <- confint(temp.mod.SUVR.adj2)
  temp.mod.SUVR.adj2.bind <- cbind(temp.mod.SUVR.adj2.summ, conf_int_SUVR_adj2)
  colnames(temp.mod.SUVR.adj2.bind) <- c("Estimate", "Std Error", "T Value", "P value", "2.5 %", "97.5 %")
  temp.mod.SUVR.adj2.bind$Model <- "Adjusted 2"
  temp.mod.SUVR.adj2.bind$Outcome <- "SUVR"
  temp.mod.SUVR.adj2.bind$Term <- rownames(temp.mod.SUVR.adj2.bind)
  temp.mod.SUVR.adj2.bind$Term <- ifelse(temp.mod.SUVR.adj2.bind$Term == "eval(exposure)", "Divorced", 
                                         temp.mod.SUVR.adj2.bind$Term) 
  
  temp.mod.SUVR.adj2 <- temp.mod.SUVR.adj2.bind %>% 
    select("Term", "Model", "Outcome", "Estimate", "Std Error", "P value", "2.5 %", "97.5 %") 
  row.names(temp.mod.SUVR.adj2) <- NULL
  
  if (i==1){all_res_suvr <-rbind(temp.mod.SUVR.crude, temp.mod.SUVR.adj1, temp.mod.SUVR.adj2)} else {
    all_res_suvr <-rbind(all_res_suvr, temp.mod.SUVR.crude, temp.mod.SUVR.adj1, temp.mod.SUVR.adj2)
  }
  
}

save(all_res_mri, file=".../all_res_mripet.Rdata")
save(all_res_mri_over65, file=".../all_res_mripet_over65.Rdata")
save(all_res_mri_petsamp, file=".../all_res_mri_petsamp.Rdata")
save(all_res_pet, file=".../all_res_pet.Rdata")
save(all_res_suvr, file=".../all_res_suvr.Rdata")

