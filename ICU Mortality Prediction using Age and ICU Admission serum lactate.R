library(tidyverse)
library(bigrquery)
library(rsample)
library(plotROC)

con <- DBI::dbConnect(drv = bigquery(),
                      project = "learnclinicaldatascience")

admissions <- tbl(con, 'mimic3_demo.ADMISSIONS') %>% 
  collect()
patients  <- tbl(con, 'mimic3_demo.PATIENTS') %>% 
  collect()
icustays <- tbl(con, "mimic3_demo.ICUSTAYS") %>% 
  collect()
labevents <- tbl(con, "mimic3_demo.LABEVENTS") %>% 
  collect()




dob <- patients %>%
  select(SUBJECT_ID, DOB) %>%
  filter(is.na(DOB)==FALSE)
dob

death <- admissions %>%
  select(HADM_ID, HOSPITAL_EXPIRE_FLAG) 
death

lactate <- labevents %>%
  filter(ITEMID==50813) %>%
  select(HADM_ID, VALUENUM,CHARTTIME)

lactate


analytic_dataset <- icustays %>%
  select(SUBJECT_ID,ICUSTAY_ID, HADM_ID, INTIME, OUTTIME)  %>%
  inner_join(dob) %>%
  mutate(age = round(as.numeric((INTIME - DOB)/365.25))) %>%
  left_join(death) %>%
  group_by(HADM_ID) %>%
  mutate(HOSPITAL_EXPIRE_FLAG= case_when(OUTTIME != max(OUTTIME) ~ 0, TRUE ~ as.numeric(HOSPITAL_EXPIRE_FLAG))) %>%
  ungroup() %>%
  left_join(lactate) %>%
  rename(LACTATE = VALUENUM) %>%
  group_by(ICUSTAY_ID) %>%
  mutate(time_diff = CHARTTIME - INTIME) %>%
  filter(time_diff>=0) %>%
  filter(time_diff == min(time_diff)) %>%
  ungroup()%>%
  filter(time_diff<=86400) %>%
  select(age,LACTATE,HOSPITAL_EXPIRE_FLAG)
  
analytic_dataset
  
data_split <- initial_split(analytic_dataset, prop = 7/10)

training_data <- training(data_split)
testing_data <- testing(data_split)

training_data
testing_data

model <- training_data %>% 
  glm(formula = HOSPITAL_EXPIRE_FLAG ~ LACTATE + age, 
      family = "binomial")

summary(model)

training_data$predicted_outcome <- predict(model, training_data, type = "response")

training_roc <- training_data %>% 
  ggplot(aes(m = predicted_outcome, d = HOSPITAL_EXPIRE_FLAG)) +
  geom_roc(n.cuts = 10, labels=F, labelround = 4) +
  style_roc(theme = theme_grey) 

training_roc

calc_auc(training_roc)$AUC*100


testing_data$predicted_outcome <- predict(model, testing_data, type = "response")

testing_roc <- testing_data %>% 
  ggplot(aes(m = predicted_outcome, d = HOSPITAL_EXPIRE_FLAG)) +
  geom_roc(n.cuts = 10, labels=F, labelround = 4) +
  style_roc(theme = theme_grey) 

testing_roc

calc_auc(testing_roc)$AUC*100

