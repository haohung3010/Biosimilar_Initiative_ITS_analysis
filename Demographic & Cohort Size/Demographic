library("dplyr")
library("tidyr")
library("lubridate")
library("ggplot2")
library("purrr")


msp = read.csv("R:/working/hdang_AnalysisFiles/Input/msp_related.csv", stringsAsFactors=FALSE)
msp$ServDate = as.Date(msp$ServDate)
msp = msp %>% filter(ServDate >= "2015-01-01")


drug_bydate = read.csv("R:/working/hdang_AnalysisFiles/Input/tnfa_drug_bydate.csv", stringsAsFactors=FALSE)
drug_bydate$rx_date = as.Date(drug_bydate$rx_date)



drug_bydate_cohort = drug_bydate %>%
  mutate(year = year(rx_date),
         month = month(rx_date)) %>%
  mutate(cohort2 = case_when(dx == "RA" | dx == "AS" ~ "IJD",
                             dx == "PSA" | dx == "PSO" ~ "ISD",
                             dx == "CD" | dx == "UC" ~ "IBD",
                             cohort == "IAD" & dx == "MIXED" ~ "IJD",
                             cohort == "IBD" & dx == "MIXED" ~ "IBD"))

drug_bydate_size_by_year = drug_bydate_cohort %>% #Table 4.1
  filter(!is.na(cohort2)) %>%
  mutate(drug2 = case_when(drug == "ADA" | drug == "ADAB" ~ "ADA",
                           drug == "ETA" | drug == "ETAB" ~ "ETA",
                           drug == "INF" | drug == "INFB" ~ "INF",
                           drug == "CER" | drug == "GOL" ~ "OTHER")) %>%
  group_by(STUDYID, drug2, year) %>%
  slice(1) %>% 
  ungroup() %>%
  filter(drug2 != "OTHER") %>%
  group_by(year) %>%
  summarize(cohort_size = n())

drug_bydate_size_by_diseases = drug_bydate_cohort %>% #Table 4.1
  filter(!is.na(cohort2)) %>%
  mutate(drug2 = case_when(drug == "ADA" | drug == "ADAB" ~ "ADA",
                           drug == "ETA" | drug == "ETAB" ~ "ETA",
                           drug == "INF" | drug == "INFB" ~ "INF",
                           drug == "CER" | drug == "GOL" ~ "OTHER")) %>%
  group_by(STUDYID, drug2) %>%
  slice(1) %>% 
  ungroup() %>%
  filter(drug2 != "OTHER") %>%
  group_by(cohort2) %>%
  summarize(cohort_size = n())

drug_bydate_drug_overall = drug_bydate_cohort %>% #Table 4.1
  filter(!is.na(cohort2)) %>%
  mutate(drug2 = case_when(drug == "ADA" | drug == "ADAB" ~ "ADA",
                           drug == "ETA" | drug == "ETAB" ~ "ETA",
                           drug == "INF" | drug == "INFB" ~ "INF",
                           drug == "CER" | drug == "GOL" ~ "OTHER")) %>%
  group_by(STUDYID, drug2) %>%
  slice(1) %>% 
  ungroup() %>%
  filter(drug2 != "OTHER") %>%
  group_by(drug2) %>%
  summarize(cohort_size = n())

drug_bydate_drug_disease_overall = drug_bydate_cohort %>% #Table 3.2
  filter(!is.na(cohort2)) %>%
  mutate(drug2 = case_when(drug == "ADA" | drug == "ADAB" ~ "ADA",
                           drug == "ETA" | drug == "ETAB" ~ "ETA",
                           drug == "INF" | drug == "INFB" ~ "INF",
                           drug == "CER" | drug == "GOL" ~ "OTHER")) %>%
  group_by(STUDYID, drug2) %>%
  slice(1) %>% 
  ungroup() %>%
  filter(drug2 != "OTHER") %>%
  group_by(cohort2, drug2) %>%
  summarize(cohort_size = n())

drug_bydate_2drugs = drug_bydate_cohort %>% #Table 3.2
  filter(!is.na(cohort2)) %>%
  mutate(drug2 = case_when(drug == "ADA" | drug == "ADAB" ~ "ADA",
                           drug == "ETA" | drug == "ETAB" ~ "ETA",
                           drug == "INF" | drug == "INFB" ~ "INF",
                           drug == "CER" | drug == "GOL" ~ "OTHER")) %>%
  group_by(STUDYID, drug2) %>%
  slice(1) %>% 
  ungroup() %>%
  filter(drug2 != "OTHER") %>%
  group_by(cohort2, STUDYID) %>%
  summarize(drug_count = n()) %>%
  ungroup() %>%
  filter(drug_count == 2) %>%
  group_by(cohort2) %>%
  summarize(cohort_size = n())

drug_bydate_3drugs = drug_bydate_cohort %>% #Table 3.2
  filter(!is.na(cohort2)) %>%
  mutate(drug2 = case_when(drug == "ADA" | drug == "ADAB" ~ "ADA",
                           drug == "ETA" | drug == "ETAB" ~ "ETA",
                           drug == "INF" | drug == "INFB" ~ "INF",
                           drug == "CER" | drug == "GOL" ~ "OTHER")) %>%
  group_by(STUDYID, drug2) %>%
  slice(1) %>% 
  ungroup() %>%
  filter(drug2 != "OTHER") %>%
  group_by(cohort2, STUDYID) %>%
  summarize(drug_count = n()) %>%
  ungroup() %>%
  filter(drug_count == 3) %>%
  group_by(cohort2) %>%
  summarize(cohort_size = n())

drug_bydate_drug_diagnosis_overall = drug_bydate_cohort %>% #Table 4.1
  filter(!is.na(cohort2)) %>%
  mutate(drug2 = case_when(drug == "ADA" | drug == "ADAB" ~ "ADA",
                           drug == "ETA" | drug == "ETAB" ~ "ETA",
                           drug == "INF" | drug == "INFB" ~ "INF",
                           drug == "CER" | drug == "GOL" ~ "OTHER")) %>%
  group_by(STUDYID, drug2, dx) %>%
  slice(1) %>% 
  ungroup() %>%
  filter(drug2 != "OTHER") %>%
  group_by(dx, drug2) %>%
  summarize(cohort_size = n())

drug_bydate_diseases = drug_bydate_cohort %>% #Table 4.1
  filter(!is.na(cohort2)) %>%
  mutate(drug2 = case_when(drug == "ADA" | drug == "ADAB" ~ "ADA",
                           drug == "ETA" | drug == "ETAB" ~ "ETA",
                           drug == "INF" | drug == "INFB" ~ "INF",
                           drug == "CER" | drug == "GOL" ~ "OTHER")) %>%
  group_by(STUDYID, cohort2) %>%
  slice(1) %>% 
  ungroup() %>%
  filter(drug2 != "OTHER") %>%
  group_by(cohort2) %>%
  summarize(cohort_size = n())

drug_bydate_diseases_drug = drug_bydate_cohort %>% #Table 4.1
  filter(!is.na(cohort2)) %>%
  mutate(drug2 = case_when(drug == "ADA" | drug == "ADAB" ~ "ADA",
                           drug == "ETA" | drug == "ETAB" ~ "ETA",
                           drug == "INF" | drug == "INFB" ~ "INF",
                           drug == "CER" | drug == "GOL" ~ "OTHER")) %>%
  group_by(STUDYID, cohort2,drug2) %>%
  slice(1) %>% 
  ungroup() %>%
  filter(drug2 != "OTHER") %>%
  group_by(cohort2,drug2) %>%
  summarize(cohort_size = n())

write.csv(drug_bydate_diseases, file = "R:/working/hdang_AnalysisFiles/R_Output/diseases.csv", row.names = FALSE)
write.csv(drug_bydate_diseases_drug, file = "R:/working/hdang_AnalysisFiles/R_Output/diseases_drug.csv", row.names = FALSE)



drug_bydate_diseases_drug_by_year = drug_bydate_cohort %>% #Table 3.3
  filter(!is.na(cohort2)) %>%
  mutate(drug2 = case_when(drug == "ADA" | drug == "ADAB" ~ "ADA",
                           drug == "ETA" | drug == "ETAB" ~ "ETA",
                           drug == "INF" | drug == "INFB" ~ "INF",
                           drug == "CER" | drug == "GOL" ~ "OTHER")) %>%
  group_by(STUDYID, drug2, year) %>%
  slice(1) %>% 
  ungroup() %>%
  filter(drug2 != "OTHER") %>%
  group_by(year, cohort2, drug2) %>%
  summarize(cohort_size = n())

write.csv(drug_bydate_diseases_by_year, file = "R:/working/hdang_AnalysisFiles/R_Output/diseases_byyear.csv", row.names = FALSE)
write.csv(drug_bydate_diseases_drug_by_year, file = "R:/working/hdang_AnalysisFiles/R_Output/diseases_byyeardrug.csv", row.names = FALSE)



drug_bydate_diseases_by_year = drug_bydate_cohort %>% #Table 4.1
  filter(!is.na(cohort2)) %>%
  mutate(drug2 = case_when(drug == "ADA" | drug == "ADAB" ~ "ADA",
                           drug == "ETA" | drug == "ETAB" ~ "ETA",
                           drug == "INF" | drug == "INFB" ~ "INF",
                           drug == "CER" | drug == "GOL" ~ "OTHER")) %>%
  group_by(STUDYID, drug2, year) %>%
  slice(1) %>% 
  ungroup() %>%
  filter(drug2 != "OTHER") %>%
  group_by(year, cohort2) %>%
  summarize(cohort_size = n())


drug_bydate_diseases_drug_by_year = drug_bydate_cohort %>% #Table 3.3
  filter(!is.na(cohort2)) %>%
  mutate(drug2 = case_when(drug == "ADA" | drug == "ADAB" ~ "ADA",
                           drug == "ETA" | drug == "ETAB" ~ "ETA",
                           drug == "INF" | drug == "INFB" ~ "INF",
                           drug == "CER" | drug == "GOL" ~ "OTHER")) %>%
  group_by(STUDYID, drug2, year) %>%
  slice(1) %>% 
  ungroup() %>%
  filter(drug2 != "OTHER") %>%
  group_by(year, cohort2, drug2) %>%
  summarize(cohort_size = n())

write.csv(drug_bydate_diseases_by_year, file = "R:/working/hdang_AnalysisFiles/R_Output/diseases_byyear.csv", row.names = FALSE)
write.csv(drug_bydate_diseases_drug_by_year, file = "R:/working/hdang_AnalysisFiles/R_Output/diseases_byyearand drug.csv", row.names = FALSE)


##############################################################################
drug_bydate_cohort_size5 = drug_bydate_cohort %>% 
  filter(!is.na(cohort2)) %>%
  mutate(drug2 = case_when(drug == "ADA" | drug == "ADAB" ~ "ADA",
                           drug == "ETA" | drug == "ETAB" ~ "ETA",
                           drug == "INF" | drug == "INFB" ~ "INF",
                           drug == "CER" | drug == "GOL" ~ "OTHER")) %>%
  filter(drug2 != "OTHER") %>%
  group_by(STUDYID, drug2, year) %>%
  slice(1) %>% 
  ungroup()

drug_bydate_cohort_size5 = drug_bydate_cohort %>% 
  filter(!is.na(cohort2)) %>%
  mutate(drug2 = case_when(drug == "ADA" | drug == "ADAB" ~ "ADA",
                           drug == "ETA" | drug == "ETAB" ~ "ETA",
                           drug == "INF" | drug == "INFB" ~ "INF",
                           drug == "CER" | drug == "GOL" ~ "OTHER")) %>%
  filter(drug2 != "OTHER") %>%
  group_by(STUDYID, cohort2, year) %>%
  slice(1) %>% 
  ungroup()

drug_bydate_cohort_drug_size5 = drug_bydate_cohort %>% 
  filter(!is.na(cohort2)) %>%
  mutate(drug2 = case_when(drug == "ADA" | drug == "ADAB" ~ "ADA",
                           drug == "ETA" | drug == "ETAB" ~ "ETA",
                           drug == "INF" | drug == "INFB" ~ "INF",
                           drug == "CER" | drug == "GOL" ~ "OTHER")) %>%
  filter(drug2 != "OTHER") %>%
  group_by(STUDYID, cohort2, drug2, year) %>%
  slice(1) %>% 
  ungroup()

demog = read.csv("R:/working/hdang_AnalysisFiles/Input/demog_tnfa.csv", stringsAsFactors=FALSE)

demog_age_sex = demog %>% #Table 3.3
  filter(YEAR >= 2015) %>%
  mutate(age2 = case_when(age < 50 ~ "below 50", 
                          age >= 50 & age <= 69 ~ "50-69",
                          age > 69 ~ "above 69")) %>%
  group_by(STUDYID, YEAR) %>%
  slice(1) %>%
  ungroup() %>%
  inner_join(drug_bydate_cohort_size5, by = c("STUDYID" = "STUDYID", "YEAR" = "year")) %>%
  group_by(YEAR, age2, SEX) %>%
  summarize(cohort_size = n())

demog_test = demog %>%
  filter(YEAR >= 2015) %>%
  inner_join(drug_bydate_cohort, by = c("STUDYID" = "STUDYID", "YEAR" = "year")) %>%
  filter(!is.na(cohort2)) %>%
  group_by(STUDYID, YEAR) %>%
  slice(1) %>% 
  ungroup()

mismatch = anti_join(drug_bydate_cohort_size5, demog_test, by = c("STUDYID" = "STUDYID", "year" = "YEAR"))
matched_demog = demog_test %>% filter(STUDYID %in% mismatch$STUDYID) 

drug_bydate_cohort_size7 = drug_bydate_cohort %>% 
  filter(!is.na(cohort2)) %>%
  mutate(drug2 = case_when(drug == "ADA" | drug == "ADAB" ~ "ADA",
                           drug == "ETA" | drug == "ETAB" ~ "ETA",
                           drug == "INF" | drug == "INFB" ~ "INF",
                           drug == "CER" | drug == "GOL" ~ "OTHER")) %>%
  group_by(STUDYID, drug2) %>%
  slice(1) %>% 
  ungroup() %>%
  filter(drug2 != "OTHER") %>%
  group_by(STUDYID) %>%
  slice(1) %>%
  ungroup()

demog_sex_distinct_by_disease = demog %>% #Table 3.1
  filter(YEAR >= 2015) %>%
  mutate(age2 = case_when(age < 50 ~ "below 50", 
                          age >= 50 & age <= 69 ~ "50-69",
                          age > 69 ~ "above 69")) %>%
  group_by(STUDYID) %>%
  slice(1) %>%
  ungroup() %>%
  inner_join(drug_bydate_cohort_size7, by = c("STUDYID" = "STUDYID")) %>%
  group_by(cohort2, SEX) %>%
  summarize(cohort_size = n())

demog_sex_distinct_by_disease_drug = demog %>% #Table 3.1
  filter(YEAR >= 2015) %>%
  group_by(STUDYID) %>%
  slice(1) %>%
  ungroup() %>%
  inner_join(drug_bydate_cohort_size7, by = c("STUDYID" = "STUDYID")) %>%
  group_by(cohort2, SEX, drug2) %>%
  summarize(cohort_size = n())

write.csv(demog_sex_distinct_by_disease, file = "R:/working/hdang_AnalysisFiles/R_Output/sex_bydisease.csv", row.names = FALSE)
write.csv(demog_sex_distinct_by_disease_drug, file = "R:/working/hdang_AnalysisFiles/R_Output/sex_bydiseasedrug.csv", row.names = FALSE)


demog_year_distinct_by_cohort = demog %>% #Table 3.1
  filter(YEAR >= 2015) %>%
  mutate(age2 = case_when(age < 50 ~ "below 50", 
                          age >= 50 & age <= 69 ~ "50-69",
                          age > 69 ~ "above 69")) %>%
  group_by(STUDYID) %>%
  slice(1) %>%
  ungroup() %>%
  inner_join(drug_bydate_cohort_size7, by = c("STUDYID" = "STUDYID")) %>%
  group_by(cohort2, YEAR) %>%
  summarize(cohort_size = n())

demog_age_distinct_by_disease = demog %>% #Table 3.1
  filter(YEAR >= 2015) %>%
  mutate(age2 = case_when(age < 50 ~ "below 50", 
                          age >= 50 & age <= 69 ~ "50-69",
                          age > 69 ~ "above 69")) %>%
  group_by(STUDYID) %>%
  slice(1) %>%
  ungroup() %>%
  inner_join(drug_bydate_cohort_size7, by = c("STUDYID" = "STUDYID")) %>%
  group_by(cohort2, age2) %>%
  summarize(cohort_size = n())

demog_mean_age_at_index_by_disease2= demog %>% #Table 3.1
  filter(YEAR >= 2015) %>%
  group_by(STUDYID) %>%
  slice(1) %>%
  ungroup() %>%
  inner_join(drug_bydate_cohort_size7, by = c("STUDYID" = "STUDYID")) %>%
  group_by(cohort2) %>%
  summarize(mean_age = mean(age),
            sd_age = sd(age))
demog_mean_age_at_index_by_disease2_drug= demog %>% #Table 3.1
  filter(YEAR >= 2015) %>%
  group_by(STUDYID) %>%
  slice(1) %>%
  ungroup() %>%
  inner_join(drug_bydate_cohort_size7, by = c("STUDYID" = "STUDYID")) %>%
  group_by(cohort2,drug2) %>%
  summarize(mean_age = mean(age),
            sd_age = sd(age))

write.csv(demog_mean_age_at_index_by_disease2, file = "R:/working/hdang_AnalysisFiles/R_Output/age_bydisease.csv", row.names = FALSE)
write.csv(demog_mean_age_at_index_by_disease2_drug, file = "R:/working/hdang_AnalysisFiles/R_Output/age_bydiseasedrug.csv", row.names = FALSE)


#######################################################


#IJD Drug breakdown at NMS
cond<-
cbind(IJD_dad_combined$year[IJD_dad_combined$year==2019 & IJD_dad_combined$month==5],
      IJD_dad_combined$month[IJD_dad_combined$year==2019 & IJD_dad_combined$month==5],
      IJD_dad_combined$cohort_size[IJD_dad_combined$year==2019 & IJD_dad_combined$month==5],
      IJD_dad_combined$drug[IJD_dad_combined$year==2019 & IJD_dad_combined$month==5])

cbind(IJD_dad_combined$year[IJD_dad_combined$year==2019 & IJD_dad_combined$month==12],
      IJD_dad_combined$month[IJD_dad_combined$year==2019 & IJD_dad_combined$month==12],
      IJD_dad_combined$cohort_size[IJD_dad_combined$year==2019 & IJD_dad_combined$month==12],
      IJD_dad_combined$drug[IJD_dad_combined$year==2019 & IJD_dad_combined$month==12])


cbind(IJD_dad_combined$year[IJD_dad_combined$year==2021 & IJD_dad_combined$month==3],
      IJD_dad_combined$month[IJD_dad_combined$year==2021 & IJD_dad_combined$month==3],
      IJD_dad_combined$cohort_size[IJD_dad_combined$year==2021 & IJD_dad_combined$month==3],
      IJD_dad_combined$drug[IJD_dad_combined$year==2021 & IJD_dad_combined$month==3])

cbind(IJD_dad_combined$year[IJD_dad_combined$year==2021 & IJD_dad_combined$month==10],
      IJD_dad_combined$month[IJD_dad_combined$year==2021 & IJD_dad_combined$month==10],
      IJD_dad_combined$cohort_size[IJD_dad_combined$year==2021 & IJD_dad_combined$month==10],
      IJD_dad_combined$drug[IJD_dad_combined$year==2021 & IJD_dad_combined$month==10])
