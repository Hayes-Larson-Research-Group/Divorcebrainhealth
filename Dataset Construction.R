#Recoded based on marital status and if divorce was NA and never married then recoded to 0 to increase sample size, added PET data
library(haven)
library(readxl)
library(tidyverse)
library(rcompanion)


K_raw<-read_sas('.../raw_data.sas7bdat')
K_mri <- read_excel('.../raw_data_mri.xlsx')
K_pet <- read_excel('.../raw_data_pet.xlsx')

S_raw <- read_sas('.../raw_data.sas7bdat')
S_mri <- read_excel('.../raw_data_mri.xlsx')

#Merge khandle data, sorted by studyid and date and keeping first scan of each person
K_mri$STUDYID <- K_mri$Subject
K_pet$STUDYID<-K_pet$Subject
K_raw$STUDYID <- paste0("K", K_raw$STUDYID)

K_mri2 <- K_mri %>% select(-Subject, -study) %>%
  rename(age_at_mri=Age_at_Date, MRI_precovid=Date_Precovid) %>%
  group_by(STUDYID) %>% arrange(age_at_mri) %>% filter(row_number()==1) %>% ungroup()

K_pet2<-K_pet %>% select(-Subject, -study) %>% 
  rename(age_at_pet=Age_at_Date, PET_precovid=Date_Precovid) %>% 
  group_by(STUDYID) %>% arrange(age_at_pet) %>% filter(row_number()==1) %>% ungroup()

K <- right_join(K_raw, K_mri2, by="STUDYID") %>% 
  left_join(.,K_pet2,by="STUDYID") %>% mutate(Study="KHANDLE") 

#Merge all star data
S_mri$STUDYID <- S_mri$Subject
S_raw$STUDYID <- paste0("KS", S_raw$STUDYID)


S_mri2 <- S_mri %>% select(-Subject, -Study, -Status) %>% 
  rename(age_at_mri=Age_at_Date, MRI_precovid=Date_Precovid)

S<-right_join(S_raw, S_mri2, by="STUDYID") %>% mutate(Study="STAR",
                                                     W1_INCMRANGE_HMNZD = case_when(W1_INCOME_RANGE %in% c(1:5) ~ W1_INCOME_RANGE,
                                                                                    W1_INCOME_RANGE %in% c(6:9) ~ 6,
                                                                                    W1_INCOME_RANGE == 10 ~ 7,
                                                                                    W1_INCOME_RANGE == 11 ~ 8,
                                                                                    W1_INCOME_RANGE %in% c(12:13) ~ 9
                                                     )) %>% 
  rename(W1_SENAS_EXEC=W1_SENAS_exec, W1_SENAS_VRMEM=W1_SENAS_vrmem) %>%
  mutate(COHORT = 1) %>%
  mutate(W1_SENAS_TELEPHONE="N") #Creating a W1 telephone variable to harmonize with KHANDLE

#Raw variables to keep

admin<-c("Study", "STUDYID", "COHORT")

div_items <- c("W1_DIVORCE", #/*Previous divorce*/
               "W1_DIVORCE_TEXT" #/*Divorce number*/
               )
covariates<-c("W1_INTERVIEW_AGE",
              
              "W1_D_GENDER", "W1_D_RACE_SUMMARY", "W1_MARITAL_STATUS",
              
              "W1_SENAS_TELEPHONE",
              
              "W1_MATERNAL_EDUCATION", "W1_PATERNAL_EDUCATION", 
              #             "W1_MATERNAL_EDUCATION_TEXT", "W1_PATERNAL_EDUCATION_TEXT", #add back once not PHI
              
              "W1_US_STATE", "W1_COUNTRY_BORN",
              # "W1_PA_HVY_WRK", "W1_PA_VIG_EX", "W1_PA_VIG_HSE",
              
              "W1_GROWINGUP_FINANCE",
              "W1_GROWINGUP_GOHUNGRY",
              "W1_LADDER1", 
              
              "W1_INCMRANGE_HMNZD",
              
              "W1_EDU_EDUCATION", "W1_EDU_EDUCATION_TEXT", 
              "W1_EDU_LONGCERT", "W1_EDU_TRNCERT", 
              "W1_HEALTH",
              
              "W1_CHILDHX_EVENTS_AGE_1", "W1_CHILDHX_EVENTS_AGE_2",
              "W1_CHILDHX_EVENTS_YN_1", "W1_CHILDHX_EVENTS_YN_2"
              )
cogoutcomes <- c("W1_SENAS_EXEC", "W1_SENAS_VRMEM")   

khandle_only <- c("Landau_All_FSR", "age_at_pet", "PET_precovid", #pet data
                  "W1_RES_1_4_STATE_RGN"
                  )

imaging <- c("Cerebrum_tcv", "Cerebrum_tcb", "Total_hippo", "Total_gray", "Total_white", 
           "Total_wmh", "Total_brain",   "Frontal_Cortical", "Occipital_Cortical", 
           "Parietal_Cortical", "Temporal_Cortical",
           "age_at_mri", "MRI_precovid")

#filter to raw variables
K2<-K %>% 
  select (all_of(c(admin, covariates, div_items, khandle_only, cogoutcomes, imaging)))

S2<-S %>% select (all_of(c(admin, covariates, div_items, cogoutcomes, imaging)))

all_dat<-bind_rows(K2, S2) %>% filter(W1_D_RACE_SUMMARY %in% c("Asian", "Black", "LatinX", "White"))

#Define baseline means and SD for z-scoring cognitive variables
W1_SENAS_exec_mean <- mean(all_dat$W1_SENAS_EXEC)
W1_SENAS_exec_sd <- sd(all_dat$W1_SENAS_EXEC)

W1_SENAS_vrmem_mean<-mean(all_dat$W1_SENAS_VRMEM)
W1_SENAS_vrmem_sd<-sd(all_dat$W1_SENAS_VRMEM)

div <-c("W1_PREV_DIVORCE" #/*Previous divorce*/
)

all_dat2 <- all_dat %>% mutate(W1_PREV_DIVORCE = case_when(W1_DIVORCE==1 ~ 1,
                                                          W1_DIVORCE==2 & W1_MARITAL_STATUS==4 ~ 1,
                                                          W1_DIVORCE==2 ~ 0,
                                                          W1_DIVORCE %in% c(77, 88, 99) ~ NA,
                                                          TRUE ~ NA)) %>%
                                                          #/*PREVIOUS DIVORCE*/

  mutate(W1_FEMALE = case_when(W1_D_GENDER==2 ~ 1,
                               W1_D_GENDER==1 ~ 0,
                               TRUE ~ NA),
         
         W1_MATERNAL_EDUCATION = case_when(W1_MATERNAL_EDUCATION %in% c(66,77,88,99) ~ 0,
                                           TRUE ~ W1_MATERNAL_EDUCATION),
         W1_PATERNAL_EDUCATION = case_when(W1_PATERNAL_EDUCATION %in% c(66,77,88,99) ~ 0,
                                           TRUE ~ W1_PATERNAL_EDUCATION),
  ) %>% #recode missings
  mutate_at(covariates,  ~ ifelse(. %in% c(77, 88, 99), NA, .)) %>%
  
  mutate(W1_MARRIED = case_when(W1_MARITAL_STATUS %in% c(1,2) ~ 1,
                                W1_MARITAL_STATUS %in% c(3,4,5,6) ~ 0,
                                TRUE ~ NA),
         
         W1_MOM_EDU_GT12 = case_when(W1_MATERNAL_EDUCATION %in% c(0,88,99) ~ 0,
                                     W1_MATERNAL_EDUCATION %in% c(1,2,3,4,5) ~ 1,
                                     TRUE ~ NA),
         W1_DAD_EDU_GT12 = case_when(W1_PATERNAL_EDUCATION %in% c(0,88,99) ~ 0,
                                     W1_PATERNAL_EDUCATION %in% c(1,2,3,4,5) ~ 1,
                                     TRUE ~ NA),
         W1_MOMORDAD_EDU_GT12 = case_when((W1_MOM_EDU_GT12 == 1 | W1_DAD_EDU_GT12 ==1)  ~ 1,
                                          (W1_MOM_EDU_GT12  == 0 & W1_DAD_EDU_GT12 ==0) ~ 0,
                                          TRUE ~ NA),
         W1_PAR_SEPDIV = case_when((W1_CHILDHX_EVENTS_YN_1==1) ~ 1,
                                                   (W1_CHILDHX_EVENTS_YN_1==2) ~ 0,
                                                   TRUE ~ NA),
         W1_PAR_REMARRIED = case_when((W1_CHILDHX_EVENTS_YN_2==1) ~ 1,
                                                  (W1_CHILDHX_EVENTS_YN_2==2) ~ 0,
                                                  TRUE ~ NA),
         W1_USBORN = case_when((W1_COUNTRY_BORN %in% c(1,77,88,99)) ~ 1,
                               W1_COUNTRY_BORN %in% c(2,3,4,5,6,7,8,9,10,11,12,
                                                      13,14,15,16,17,18,19,20,
                                                      21,22,23,24,25,26,27) ~ 0,
                               TRUE ~ NA),
         W1_SOUTHERN_BIRTH = case_when((W1_COUNTRY_BORN %in% c(1,77,88,99) &
                                          W1_US_STATE %in% c("US-DE", "US-DC", "US-FL",
                                                             "US-GA", "US-MD", "US-NC",
                                                             "US-SC", "US-VA", "US-WV",
                                                             "US-AL", "US-KY", "US-MS",
                                                             "US-TN", "US-AR", "US-LA",
                                                             "US-OK", "US-TX")) ~ 1,
                                       (W1_COUNTRY_BORN %in% c(1,77,88,99) & W1_US_STATE != "" &
                                          W1_US_STATE != 88 & W1_US_STATE != 99) ~ 0,
                                       W1_COUNTRY_BORN %in% c(2,3,4,5,6,7,8,9,10,11,12,
                                                              13,14,15,16,17,18,19,20,
                                                              21,22,23,24,25,26,27) ~ 0,
                                       TRUE ~ NA,
         ),
         W1_CHD_OKFINANCIALLY = case_when(W1_GROWINGUP_FINANCE %in% c(1,2) ~ 1,
                                          W1_GROWINGUP_FINANCE %in% c(3,4) ~ 0,
                                          TRUE ~ NA),
         W1_CHD_EVERHUNGRY = case_when(W1_GROWINGUP_GOHUNGRY == 1 ~ 0,
                                       W1_GROWINGUP_GOHUNGRY %in% c(2,3,4,5) ~ 1,
                                       TRUE ~ NA),
         W1_CHD_COMMSTANDING = case_when(W1_LADDER1 %in% c(1:10) ~ W1_LADDER1,
                                         TRUE ~ NA),
         #----****total years of education----
         #code is from CWE
         edu_yrs = case_when(
           !is.na(W1_EDU_EDUCATION_TEXT) ~ W1_EDU_EDUCATION_TEXT,
           W1_EDU_EDUCATION == 1 ~ 13,
           W1_EDU_EDUCATION == 2 ~ 14,
           W1_EDU_EDUCATION == 3 ~ 16,
           W1_EDU_EDUCATION == 4 ~ 18,
           W1_EDU_EDUCATION == 5 ~ 20,
           TRUE ~ NA
         ),

         cert_flag = case_when(W1_EDU_TRNCERT == 2 &
                                 W1_EDU_LONGCERT == 4 ~ 1,
                               TRUE ~ 0),

         W1_EDU_YRS_CERT = case_when(
           edu_yrs <= 12 & !is.na(cert_flag) ~ edu_yrs + cert_flag,
           TRUE ~ edu_yrs
         ),
         #Z-score cognitive outcomes to pooled sample        
         W1_SENAS_EXEC_POOLZ = scale(W1_SENAS_EXEC, 
                                     center=W1_SENAS_exec_mean, 
                                     scale=W1_SENAS_exec_sd) %>% as.numeric(),
         W1_SENAS_VRMEM_POOLZ = scale(W1_SENAS_VRMEM, 
                                      center=W1_SENAS_vrmem_mean, 
                                      scale=W1_SENAS_vrmem_sd) %>% as.numeric()
         ) 
#Check variable cleaning
table(all_dat2$W1_DIVORCE, all_dat2$W1_PREV_DIVORCE, exclude=NULL)
all_dat2 %>% count(W1_DIVORCE_TEXT, W1_PREV_DIVORCE, W1_PAR_SEPDIV)
table(all_dat2$W1_DIVORCE, all_dat2$W1_MARITAL_STATUS, exclude=NULL)
table(all_dat2$W1_PREV_DIVORCE, all_dat2$W1_MARITAL_STATUS, exclude=NULL)

#Check covariates
table(all_dat2$W1_CHILDHX_EVENTS_YN_1, all_dat2$W1_PAR_SEPDIV, exclude=NULL)
table(all_dat2$W1_CHILDHX_EVENTS_YN_2, all_dat2$W1_PAR_REMARRIED, exclude=NULL)
table(all_dat2$W1_D_GENDER, all_dat2$W1_FEMALE, exclude=NULL)
table(all_dat2$W1_D_RACE_SUMMARY, exclude=NULL)
table(all_dat2$W1_MARITAL_STATUS, exclude=NULL)
table(all_dat2$W1_MATERNAL_EDUCATION, exclude=NULL)
table(all_dat2$W1_PATERNAL_EDUCATION, exclude=NULL) 
table(all_dat2$W1_US_STATE, exclude=NULL)
table(all_dat2$W1_COUNTRY_BORN, exclude=NULL)
table(all_dat2$W1_GROWINGUP_FINANCE, exclude=NULL)
table(all_dat2$W1_GROWINGUP_GOHUNGRY, exclude=NULL)
table(all_dat2$W1_LADDER1, exclude=NULL)
table(all_dat2$W1_INCMRANGE_HMNZD, exclude=NULL)

table(all_dat2$W1_MARITAL_STATUS, all_dat2$W1_MARRIED, exclude=NULL) #Check data dictionary
table(all_dat2$W1_MATERNAL_EDUCATION, all_dat2$W1_MOM_EDU_GT12, exclude=NULL)
table(all_dat2$W1_PATERNAL_EDUCATION, all_dat2$W1_DAD_EDU_GT12, exclude=NULL)
table(all_dat2$W1_MOM_EDU_GT12, all_dat2$W1_DAD_EDU_GT12, all_dat2$W1_MOMORDAD_EDU_GT12, exclude=NULL)
table(all_dat2$W1_COUNTRY_BORN, all_dat2$W1_USBORN, exclude=NULL)
table(all_dat2$W1_US_STATE, all_dat2$W1_COUNTRY_BORN, all_dat2$W1_USBORN, exclude=NULL)
table(all_dat2$W1_US_STATE, all_dat2$W1_USBORN, all_dat2$W1_SOUTHERN_BIRTH, exclude=NULL)

table(all_dat2$W1_GROWINGUP_FINANCE, all_dat2$W1_CHD_OKFINANCIALLY, exclude=NULL)
table(all_dat2$W1_GROWINGUP_GOHUNGRY, all_dat2$W1_CHD_EVERHUNGRY, exclude=NULL)
table(all_dat2$W1_LADDER1, all_dat2$W1_CHD_COMMSTANDING, exclude=NULL)

summary(all_dat2$W1_SENAS_EXEC_POOLZ)
sd(all_dat2$W1_SENAS_EXEC_POOLZ)
summary(all_dat2$W1_SENAS_VRMEM_POOLZ)
sd(all_dat2$W1_SENAS_VRMEM_POOLZ)

vols<-c("Cerebrum_tcb", "Total_hippo", 
        "Total_gray", "Total_white", "Total_wmh", "Total_brain", 
        "Frontal_Cortical", "Occipital_Cortical", "Parietal_Cortical", "Temporal_Cortical"
)
MRIsample<-all_dat2 %>% filter(!is.na(Cerebrum_tcv) & !is.na(Total_wmh))
PETsample<-all_dat2 %>% filter(!is.na(Landau_All_FSR)) %>% 
  select(Study, STUDYID, Landau_All_FSR) %>% 
  mutate(PET_sample=1,
         Abeta_pos = case_when(Landau_All_FSR >= 1.1 ~ 1,
                               Landau_All_FSR < 1.1 ~ 0,
                               TRUE ~ NA))

for (i in vols){
  # i<-"Cerebrum_tcb"
  temp<-lm(get(i) ~ Cerebrum_tcv, data=MRIsample)
  summary(temp)
  
  #Get residuals
  MRIsample[,paste0(i,"_resid")]<-MRIsample[,i]-predict(temp, newdata=MRIsample)
  
  #Blom transform residuals 
  MRIsample[,paste0(i,"_residblom")]<-blom(MRIsample[,paste0(i,"_resid")], method="blom")
  
  #Z-score transform residuals 
  tempmean<-mean(MRIsample[,paste0(i,"_resid"), drop=T])
  tempSD<-sd(MRIsample[,paste0(i,"_resid"), drop=T])
  
  MRIsample[,paste0(i,"_residzscore")]<-(MRIsample[,paste0(i,"_resid")]-tempmean)/tempSD
  
  #Log transform residuals
  tempmin<-min(MRIsample[,paste0(i,"_resid")])
  MRIsample[,paste0(i,"_residlog")]<-log((MRIsample[,paste0(i,"_resid")]-tempmin+0.01))
  
}

MRIsample$Total_wmh_log<-log(MRIsample$Total_wmh+0.01)


MRImerge<-MRIsample %>% select(STUDYID, Study, Cerebrum_tcv, all_of(c(vols, 
                                                                      paste0(vols, "_resid"), 
                                                                      paste0(vols, "_residblom"), 
                                                                      paste0(vols, "_residzscore"), 
                                                                      paste0(vols, "_residlog"))), 
                               Total_wmh_log) %>% mutate(MRI_sample=1)

all_dat3<-left_join(all_dat2 %>% select(-all_of(vols), -Cerebrum_tcv, -Landau_All_FSR), 
                    MRImerge, by=c("Study", "STUDYID")) %>% 
  left_join(.,PETsample, by=c("Study", "STUDYID")) %>% 
  mutate(MRI_sample = case_when(is.na(MRI_sample) ~ 0,
                                T ~ MRI_sample),
         PET_sample =case_when(is.na(PET_sample) ~ 0,
                               T ~ PET_sample)) %>%
  mutate(W1_PREV_DIVORCE = ifelse(is.na(W1_PREV_DIVORCE) & 
                                    W1_MARITAL_STATUS == 6, 0, W1_PREV_DIVORCE)) %>% 
  filter(!is.na(W1_PREV_DIVORCE))

all_dat_final<-all_dat3 %>% select(all_of(c(admin, 
                                             #exposures
                                             div)), 
                                    
                                    #age and cognitive outcomes
                                    W1_INTERVIEW_AGE,
                                    # W1_SENAS_TELEPHONE,
                                    all_of(cogoutcomes),
                                    W1_SENAS_EXEC_POOLZ,
                                    W1_SENAS_VRMEM_POOLZ,
                                    
                                    #covariates/descriptives
                                    W1_D_RACE_SUMMARY, 
                                    W1_FEMALE, 
                                   W1_MARITAL_STATUS,
                                   W1_INCMRANGE_HMNZD, 
                                    W1_EDU_EDUCATION, W1_EDU_YRS_CERT,
                                    # W1_MATERNAL_EDUCATION,  W1_PATERNAL_EDUCATION, 
                                    W1_US_STATE, W1_COUNTRY_BORN,
                                    W1_HEALTH,
                                    W1_GROWINGUP_FINANCE,  W1_GROWINGUP_GOHUNGRY, W1_LADDER1,
                                    W1_MARRIED, 
                                   # W1_MOM_EDU_GT12, W1_DAD_EDU_GT12, 
                                    W1_PAR_SEPDIV, 
                                   # W1_PAR_REMARRIED,
                                    W1_MOMORDAD_EDU_GT12, W1_USBORN, W1_SOUTHERN_BIRTH, W1_CHD_OKFINANCIALLY,
                                    W1_CHD_EVERHUNGRY, W1_CHD_COMMSTANDING, 
                                    
                                    #imaging
                                    Cerebrum_tcv, all_of(c(vols, 
                                                           paste0(vols, "_resid"), 
                                                           paste0(vols, "_residblom"), 
                                                           paste0(vols, "_residzscore"), 
                                                           paste0(vols, "_residlog"))), Total_wmh_log,
                                    age_at_mri, MRI_precovid,
                                   Landau_All_FSR, Abeta_pos, age_at_pet, PET_precovid, 
                                   MRI_sample, PET_sample) %>% rename(STUDY=Study) 


save(all_dat_final, file=".../divorce_petmri_v3.Rdata")
