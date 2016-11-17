setwd('I:\\A-Lydia-Work-Plan\\Projects\\BioBank\\PredictiveModel\\final_data')
library(data.table) #for fread fast reading files
library(dplyr)      #for data manipulate

#Foundation data manage work
#-------------------------------------------------------------------------------------------
#reading data in
#fread, na.strings convert everything to string NA, function f_dowle3 convert all NA to 0
#convert to NA string or numeric 0 depends on usage
baseTable <- fread('MasterData.txt', header=T, stringsAsFactors=F, na.strings = 'NULL')
Demo <- fread('Demographics.txt', header=T, stringsAsFactors=F, na.strings = 'NULL')
Demo <- select(Demo, -OLD_PAT_ID) 
Demo <- select(Demo, -RACE_NAME)
Demo <- select(Demo, -ETHNIC_GROUP)
#write.table(Demo, "demo_new.txt", sep="\t", col.names = NA)#use this
#write.csv(Demo, 'demographics.csv')
#demographic <- fread('demo_new.txt', header=T, stringsAsFactors=F, na.strings ='NULL')
demographics <- fread('demographics.csv', header=T, stringsAsFactors=F, na.strings ='NULL')
#MedInfo <- fread('MedInfo.txt', header=T, stringsAsFactors=F, na.strings = 'NULL')
#Meds<- fread('Medication.txt', header=T, stringsAsFactors=F, na.strings = 'NULL')
Enc <- fread('Encounters.txt', header=T, stringsAsFactors=F, na.strings = 'NULL')
Enc <- select(Enc, -OLD_PAT_ID)


#make sure the format is correctly match what's already in the data, it will be y-m-d
baseTable$MIN_VISIT_DATE <- as.Date(baseTable$MIN_VISIT_DATE, '%Y-%m-%d')
baseTable$MAX_VISIT_DATE <- as.Date(baseTable$MAX_VISIT_DATE, '%Y-%m-%d')
Enc$CONTACT_DATE <- as.Date(Enc$CONTACT_DATE, '%Y-%m-%d')
Meds$ORDERING_DATE <- as.Date(Meds$ORDERING_DATE, '%Y-%m-%d')
Demo$DOB <- as.Date(Demo$DOB, '%m/%d/%Y') 

##----how to get date time difference in years---------------------------------
#http://stackoverflow.com/questions/15569333/r-get-date-difference-in-years-floating-point
encounter_data <- merge(x = baseTable, y = Enc, by='PAT_ID', all.x=T) 

outcome1 <- filter(encounter_data, OUTCOME==1)%>%filter(
    3<=as.numeric(difftime(MAX_VISIT_DATE, CONTACT_DATE, unit="weeks"))/52.25 
    & as.numeric(difftime(MAX_VISIT_DATE, CONTACT_DATE, unit="weeks"))/52.25<=4)


outcome0 <- filter(encounter_data, OUTCOME==0)%>%filter(
    (as.numeric(difftime(CONTACT_DATE, MIN_VISIT_DATE,  unit="weeks"))/52.25<=1))

#http://stackoverflow.com/questions/1299871/how-to-join-merge-data-frames-inner-outer-left-right

final_encounter <- rbind(outcome1, outcome0)  ##this is the master table for encounters

enc_demo <- merge(x = final_encounter, y = Demo, by = 'PAT_ID', all.x=T)
enc_demo <- mutate(enc_demo, AGE =  (as.numeric(difftime(MIN_VISIT_DATE, 
                                                         DOB,  unit="weeks"))/52.2))
#pt age at their first pcp visit
risk_factor <- read.csv('enc_disease.csv', header=T, stringsAsFactors = F )

enc_demo_risk <- merge(x = enc_demo, y = risk_factor, by = 'PAT_ENC_CSN_ID', all.x=T)

#fwrite(DF, "output.csv")



############################################################################
med_data <- merge(x = baseTable, y = Meds, by='PAT_ID', all.x=T) 

outcomeM1 <- filter(med_data , OUTCOME==1)%>%filter(
    3<=as.numeric(difftime(MAX_VISIT_DATE, ORDERING_DATE, unit="weeks"))/52.25 & as.numeric(difftime(MAX_VISIT_DATE, ORDERING_DATE, unit="weeks"))/52.25<=4)


outcomeM0 <- filter(med_data, OUTCOME==0)%>%filter(
    (as.numeric(difftime(ORDERING_DATE, MIN_VISIT_DATE, unit="weeks"))/52.25<=1))

final_med <- rbind(outcomeM1, outcomeM0)

#########################################################################

#http://stackoverflow.com/questions/22850026/filtering-row-which-contains-a-certain-string-using-dplyr

#http://stackoverflow.com/questions/9809166/is-there-an-aggregate-fun-option-to-count-occurrences
#http://stackoverflow.com/questions/35670213/replace-values-in-some-rows-based-on-other-dataframe-mapping-with-r
library(data.table)
disease <- fread('enc_disease.csv', header=T, stringsAsFactors=F, na.strings = 'NULL')
diseaseTry <- disease
diseaseTry <- select(diseaseTry, -PAT_ENC_CSN_ID)
disScore <- group_by(diseaseTry, PAT_ID)%>%summarize(
    CHF_ENC = sum(CHF_ENC, na.rm=T), 
    COPD_ENC = sum(COPD_ENC, na.rm=T), 
    CKD_ENC = sum(CKD_ENC, na.rm=T), 
    PEP_ULCER_ENC = sum(PEP_ULCER_ENC, na.rm=T), 
    CANCER_ENC = sum(CANCER_ENC, na.rm=T), 
    LIVER_ENC = sum(LIVER_ENC, na.rm=T), 
    META_TUMOR_ENC = sum(META_TUMOR_ENC, na.rm=T), 
    RENAL_ENC = sum(RENAL_ENC, na.rm=T), 
    RA_ENC = sum(RA_ENC, na.rm=T), 
    CAD_ENC = sum(CAD_ENC, na.rm=T), 
    MI_ENC = sum(MI_ENC, na.rm=T), 
    AIDS_ENC = sum(AIDS_ENC, na.rm=T), 
    PAD_ENC = sum(PAD_ENC, na.rm=T), 
    DM_ENC = sum(DM_ENC, na.rm=T), 
    DEMENTIA_ENC = sum(DEMENTIA_ENC, na.rm=T), 
    HEMI_ENC = sum(HEMI_ENC, na.rm=T), 
    ASTHMA_ENC = sum(ASTHMA_ENC, na.rm=T), 
    FIBRO_ENC = sum(FIBRO_ENC, na.rm=T), 
    IBS_ENC = sum(IBS_ENC, na.rm=T), 
    DEP_ENC = sum(DEP_ENC, na.rm=T), 
    ANXIETY_ENC = sum(ANXIETY_ENC, na.rm=T), 
    ALLER_RHIN_ENC = sum(ALLER_RHIN_ENC, na.rm=T), 
    GERD_ENC = sum(GERD_ENC, na.rm=T), 
    EPILEPSY_ENC = sum(EPILEPSY_ENC, na.rm=T), 
    MS_ENC = sum(MS_ENC, na.rm=T),
    MIG_ENC = sum(MIG_ENC, na.rm=T),
    BACK_PAIN_ENC = sum(BACK_PAIN_ENC, na.rm=T)
)   










#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------
#11/15/2016 Final Clean up

raw_enc <- fread('raw_encounter.csv', header=T, stringsAsFactors=F, na.strings = 'NULL')


raw_enc0 <- raw_enc #for file manipulations keep the original data

#add a variable age
raw_enc0$CONTACT_DATE <- as.Date(raw_enc0$CONTACT_DATE, '%Y-%m-%d')
raw_enc0$MIN_VISIT_DATE <- as.Date(raw_enc0$MIN_VISIT_DATE, '%Y-%m-%d')
raw_enc0$BIRTH_DATE <- as.Date(raw_enc0$BIRTH_DATE, '%Y-%m-%d')
raw_enc0 <- mutate(raw_enc0, AGE =(as.numeric(difftime(MIN_VISIT_DATE, 
                                                         BIRTH_DATE,  unit="weeks"))/52.2))

#eliminate unneccesary variables
raw_enc0 <- select(raw_enc0, -MIN_CONTACT_DATE)
raw_enc0 <- select(raw_enc0, -MAX_CONTACT_DATE)
raw_enc0 <- select(raw_enc0, -FIN_CLASS_NAME)
raw_enc0 <- select(raw_enc0, -DEPARTMENT_NAME) 
raw_enc0 <- select(raw_enc0, -DX_NAME)
raw_enc0 <- select(raw_enc0, -REF_BILL_CODE)
raw_enc0 <- select(raw_enc0, -PRIMARY_DX_YN)
raw_enc0 <- select(raw_enc0, -OB_GYN_YN)
raw_enc0 <- select(raw_enc0, -GENERAL_PC_YN)



#------------------------------------------------------------------------------
#clean up insurance part

insurance <- raw_enc0%>%select(PAT_ID, FINANCIAL_CLASS_NAME) 
insurance <- insurance%>%filter(FINANCIAL_CLASS_NAME != '') 


insurance=insurance%>%group_by(PAT_ID, FINANCIAL_CLASS_NAME)%>%
    mutate(insurance_count=n())

insurance=insurance%>%group_by(PAT_ID)%>%select(PAT_ID, FINANCIAL_CLASS_NAME, insurance_count)%>%
    filter(insurance_count==max(insurance_count)) 

insurance <- distinct(insurance, .keep_all=T)

insurance <- select(insurance, -insurance_count)




#------------------------------------------------------------------------------------------
#construct new variables

raw_enc0 <- raw_enc0 %>%
    mutate(visit_inclusion = 
               ifelse(grepl('Office Visit', ENC_TYPE_NAME) | 
                          grepl('MHO', ENC_TYPE_NAME) | 
                          grepl('Telephone', ENC_TYPE_NAME) | 
                          grepl('Refill', ENC_TYPE_NAME)|
                          grepl('Released Order', ENC_TYPE_NAME)|
                          grepl('Orders Only', ENC_TYPE_NAME)|
                          grepl('Office Visit OB Est', ENC_TYPE_NAME)|
                          grepl('Office Visit OB New', ENC_TYPE_NAME)|
                          grepl('Urgent Care Office Visit', ENC_TYPE_NAME)|
                          grepl('Telephone OB', ENC_TYPE_NAME)|
                          grepl( 'Hospital', ENC_TYPE_NAME)|
                          grepl( '*Ambulatory Surgery Center Visit', ENC_TYPE_NAME)| 
                          grepl('*Hospital Inpatient Visit', ENC_TYPE_NAME)| 
                          grepl( 'Mntl Hlth Office Visit', ENC_TYPE_NAME)|
                          grepl( '*Hospital Outpatient Visit', ENC_TYPE_NAME)|
                          grepl( 'Mntl Hlth Office Visit', ENC_TYPE_NAME)|
                          grepl('*Hospital Outpaatient Visit', ENC_TYPE_NAME)|
                          grepl('Outpt Surg', ENC_TYPE_NAME)|
                          grepl('Radiology Visit', ENC_TYPE_NAME)|
                          grepl('Express Care', ENC_TYPE_NAME)|
                          grepl('*Emergency Room Visit', ENC_TYPE_NAME)|
                          grepl('Mntl Hlth Inpatient', ENC_TYPE_NAME)|
                          grepl('*Psychiatry Fac Visit', ENC_TYPE_NAME), 1,
                      ifelse( grepl('Education', ENC_TYPE_NAME)|
                                  grepl( 'Immunization', ENC_TYPE_NAME)|
                                  grepl( 'Appointment', ENC_TYPE_NAME)|
                                  grepl( 'Referral', ENC_TYPE_NAME), 2, 0))
    ) 

raw_enc0 <- raw_enc0%>%mutate(
    telephone = ifelse(grepl('Telephone', ENC_TYPE_NAME)|
                           grepl('Telephone OB', ENC_TYPE_NAME),1,0
    ))


raw_enc0 <- raw_enc0%>%mutate(
    urgent_care= ifelse(grepl('Urgent Care Office Visit', ENC_TYPE_NAME),1,0
    ))


raw_enc0 <- raw_enc0%>%mutate(
    MHO= ifelse(grepl('MHO', ENC_TYPE_NAME),1,0
    ))


raw_enc0 <- raw_enc0%>%mutate(
    med = ifelse(grepl('Refill', ENC_TYPE_NAME)|
                     grepl('Released Order', ENC_TYPE_NAME)|
                     grepl('Orders Only', ENC_TYPE_NAME),1,0
    ))


raw_enc0 <- raw_enc0%>%mutate(
    OB_ENC = ifelse(grepl('Office Visit OB Est', ENC_TYPE_NAME)|
                        grepl('Office Visit OB New', ENC_TYPE_NAME)|
                        (grepl('Gynecology', DEPARTMENT_SPECIALTY)&
                             grepl('Office Visit', ENC_TYPE_NAME))|
                        (grepl('Obsterics-Gynecology', DEPARTMENT_SPECIALTY)&
                             grepl('Office Visit', ENC_TYPE_NAME)),1,0
    ))



raw_enc0 <- raw_enc0%>%mutate(
    PCP_ENC = ifelse(grepl('Office Visit OB Est', ENC_TYPE_NAME)|
                         grepl('Office Visit OB New', ENC_TYPE_NAME)|
                         (grepl('Gynecology', DEPARTMENT_SPECIALTY)&
                              grepl('Office Visit', ENC_TYPE_NAME))|
                         (grepl('Obsterics-Gynecology', DEPARTMENT_SPECIALTY)&
                              grepl('Office Visit', ENC_TYPE_NAME))|
                         (grepl('Internal Medicine', DEPARTMENT_SPECIALTY)&
                              grepl('Office Visit', ENC_TYPE_NAME))|
                         (grepl('Family Medicine', DEPARTMENT_SPECIALTY)&
                              grepl('Office Visit', ENC_TYPE_NAME)),1,0
    ))


raw_enc0 <- raw_enc0%>%mutate(
    Spec_enc= ifelse(PCP_ENC==0&grepl('Office Visit', ENC_TYPE_NAME), 1, 0
    ))


raw_enc0 <- raw_enc0%>%mutate(
    enc_type = ifelse(telephone==1, 'Telephone',
                      ifelse(urgent_care==1, 'Urgent',
                             ifelse(MHO==1, 'MHO', 
                                    ifelse(med==1, 'MED',
                                           ifelse(OB_ENC==1|PCP_ENC==1,'PCP',
                                                  ifelse(Spec_enc==1, 'Specialty',
                                                         ifelse(visit_inclusion==2,'Ancillary',
                                                                ifelse(visit_inclusion==0, 'Not-related', 'Other') )))))) ))




raw_enc0 <- raw_enc0 %>%filter(visit_inclusion!=0) #filter out unwanted data



raw_enc0 <-raw_enc0 %>%group_by(PAT_ID) %>%mutate(total_num_enc = n_distinct(PAT_ENC_CSN_ID))




raw_enc0 <- raw_enc0%>%mutate(
    canceled_appt= ifelse(grepl('Canceled', APPT_STATUS_NAME)&
                               grepl('Appointment', ENC_TYPE_NAME), 1, 0
    ))




raw_enc0 <- raw_enc0%>%mutate(
    lab_enc= ifelse(med==1&grepl('Laboratory Medicine', DEPARTMENT_SPECIALTY), 1, 0
    ))




raw_enc0 <- distinct(raw_enc0, .keep_all=T) #keep_all is very important here




#----------------------------------------------------------------------------------------
#summarizing counting of encounters
raw_enc0 <- group_by(raw_enc0, PAT_ID)%>%mutate(  #change summarize to mutate
    telephone = sum(telephone, na.rm=T), 
    urgent_care = sum(urgent_care, na.rm=T), 
    MHO = sum(MHO, na.rm=T), 
    med = sum(med, na.rm=T), 
    OB_ENC = sum(OB_ENC, na.rm=T), 
    PCP_ENC = sum(PCP_ENC, na.rm=T), 
    Spec_enc = sum(Spec_enc, na.rm=T),
    canceled_appt = sum(canceled_appt, na.rm=T),
    lab_enc = sum(lab_enc, na.rm=T)
    ) 
raw_enc0 <- raw_enc0 %>%mutate(med=(med-lab_enc))


raw_enc0 <- raw_enc0 %>%mutate(other_num_enc = total_num_enc - (telephone + urgent_care + med+ PCP_ENC + Spec_enc + canceled_appt + lab_enc + MHO))#OB_ENC overlap with PCP_ENC


raw_enc0 <- raw_enc0 %>%mutate(final_other_num_enc=ifelse(other_num_enc>=0, other_num_enc, 0))

    
raw_enc0 <- select(raw_enc0, -other_num_enc)    
raw_enc0 <- select(raw_enc0, -MIN_VISIT_DATE)  
raw_enc0 <- select(raw_enc0, -MAX_VISIT_DATE) 
raw_enc0 <- select(raw_enc0, -APPT_STATUS_NAME)  
raw_enc0 <- select(raw_enc0, -ENC_TYPE_NAME)  
raw_enc0 <- select(raw_enc0, -FINANCIAL_CLASS_NAME)



#join insurance------------------------------------------------------------------------

raw_enc1 <- merge(x=raw_enc0, y=insurance, by='PAT_ID', all.x=T)


#demographic---------------------------------------------------------------------------
demographic <- fread('demographics.csv', header=T, stringsAsFactors=F, na.strings = 'NULL')

raw_encDemo <- merge(x=raw_enc1, y=demographic, by='PAT_ID', all.x=T)

#disease score
disease <- fread('disease_score.csv', header=T, stringsAsFactors=F, na.strings = 'NULL')


#master table combining all the necessary information---------------------------------

baseData <- merge(x=raw_encDemo, y=disease, by='PAT_ID', all.x=T)
baseData <- select(baseData, -PAT_ENC_CSN_ID)
baseData <- select(baseData, -CONTACT_DATE)
baseData <- distinct(baseData, .keep_all=T)


#write.csv(baseData, 'baseData.csv', row.names=FALSE)

data <- baseData
data$GENDER <- as.factor(data$GENDER)
data$OUTCOME <- as.factor(data$OUTCOME)









#-------------------------------------------------------------------------------------------
#m<- enc_total%>%filter(final_other_num_enc<0)

# m <- raw_enc0 %>%select(PAT_ID, total_num_enc)
# n <- select(enc_total, PAT_ID, other_num_enc)
# t <- merge(x=n, y=m, by='PAT_ID', all.x=T)
# t <- mutate(t, )


# temp <- dat %>% 
#     gather(key, value, 2:4) %>% 
#     filter(value != "") %>%
#     select(-key)
# 
# dat %>% 
#     gather(key, value, -team) %>% 
#     select(-key) %>%
#     mutate(value = ifelse(value == "", NA, value)) %>%
#     na.omit %>%
#     arrange(team)
# 
# outcomeless0 <- filter(encounter_dataless, OUTCOME==0)
# m=raw_enc1%>%group_by(PAT_ID, FINANCIAL_CLASS_NAME)%>%
#     mutate(insurance_count=n())
# 
# n= m%>%group_by(PAT_ID)%>%select(PAT_ID, FINANCIAL_CLASS_NAME, insurance_count)%>%
#     filter(insurance_count==max(insurance_count)) 
# 
# raw2 <- distinct(rawEnc)
#-----------------------------------------------------------------------------
#http://stackoverflow.com/questions/26720349/get-dplyr-count-of-distinct-in-a-readable-way
#http://stackoverflow.com/questions/17421776/how-to-add-count-of-unique-values-by-group-to-r-data-frame
#df %>%group_by(color) %>%mutate(unique_types = n_distinct(type))