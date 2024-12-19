pn_clm = read.csv("R:/working/hdang_AnalysisFiles/Input/PN_cost/pn_clm.csv")

pn_clm$Servdate = as.Date(pn_clm$Servdate)
pn_clm = pn_clm %>% filter(Servdate >= "2014-01-01")

#loop through year function:
generate_day = function (start_date, num_days) {
  seq(start_date, by = "days", length.out = num_days)
}

loop_yr_IAD = function(st, end) {
  pn_count_temp = data.frame() 
  for (i in st:end) {
    pn_related = pn_clm %>% 
      filter( #filter for concomitant 
        year(Servdate) == i,
        iad_category == "CSDMARD" | iad_category == "NSAID" |iad_category == "OCS")
    temp1 = pn_related %>% 
      mutate( # populate the rows with dates of supply
        date = pmap(list(Servdate, Dispensed_Days_Supply), generate_day)
      ) %>% 
      unnest(date) %>% 
      mutate(date = as.Date(date, "%Y-%m-%d"))
    pn_count_temp = bind_rows(pn_count_temp, temp1)
  }
  return (pn_count_temp)
}

loop_yr_IBD = function(st, end) {
  pn_count_temp = data.frame() 
  for (i in st:end) {
    pn_related = pn_clm %>% 
      filter( #filter for concomitant 
        year(Servdate) == i,
        IBD_Category == "AMINOSALICYLATES" | IBD_Category == "CORTICOSTEROIDS" | IBD_Category == "IMMUNE SUPPRESSANTS" | IBD_Category == "ORAL GLUCOCORTICOIDS")
    temp1 = pn_related %>% 
      mutate( # populate the rows with dates of supply
        date = pmap(list(Servdate, Dispensed_Days_Supply), generate_day)
      ) %>% 
      unnest(date) %>% 
      mutate(date = as.Date(date, "%Y-%m-%d"))
    pn_count_temp = bind_rows(pn_count_temp, temp1)
  }
  return (pn_count_temp)
}


pn_count_temp_IAD_2014_2022 = loop_yr_IAD(2014, 2022)
pn_count_temp_IBD_2014_2022 = loop_yr_IBD(2014, 2022)

pn_count_IAD = pn_count_temp_IAD_2014_2022 %>%
  inner_join(drug_bydate_cohort, by = c("STUDYID" = "STUDYID", "date" = "rx_date")) %>%
  mutate(drug2 = case_when(drug == "ADA" | drug == "ADAB" ~ "ADA",
                           drug == "ETA" | drug == "ETAB" ~ "ETA",
                           drug == "INF" | drug == "INFB" ~ "INF",
                           drug == "GOL" | drug == "CER" ~ "OTHER")) %>%
  filter(!is.na(drug)) %>%
  group_by(STUDYID, year, month, cohort2, drug2) %>%
  summarise(count_conc = n()) %>%
  filter(year >= 2015 & year <= 2022) %>%
  group_by(year, month, cohort2, drug2) %>%
  summarize(cohort_count_conc = sum(count_conc))
  

pn_count_IJD = pn_count_IAD %>% 
  filter(cohort2 == "IJD") %>%
  inner_join(drug_bydate_cohort_size_IJD, by = c("year" = "year", "drug2" = "drug2"))  %>%
  mutate(count_per_patient = cohort_count_conc/cohort_size) 

pn_count_IJD_ADA = pn_count_IJD %>%
  filter(drug2 == "ADA")

pn_count_IJD_ETAINF = pn_count_IJD %>%
  filter(drug2 == "ETA" | drug2 == "INF") %>%
  summarize(cohort_count_conc1 = sum(cohort_count_conc),cohort_size1=sum(cohort_size)) %>%
  mutate(count_per_patient = cohort_count_conc1/cohort_size1)




pn_count_IBD = pn_count_temp_IBD_2014_2022 %>%
  inner_join(drug_bydate_cohort, by = c("STUDYID" = "STUDYID", "date" = "rx_date")) %>%
  
  mutate(drug2 = case_when(drug == "ADA" | drug == "ADAB" ~ "ADA",
                           drug == "ETA" | drug == "ETAB" ~ "ETA",
                           drug == "INF" | drug == "INFB" ~ "INF",
                           drug == "GOL" | drug == "CER" ~ "Others")) %>%
  filter(!is.na(drug)) %>%
  filter(!is.na(drug)) %>%
  group_by(STUDYID, year, month, cohort2, drug2) %>%
  summarise(count_conc = n()) %>%
  filter(year >= 2015 & year <= 2022) %>%
  group_by(year, month, cohort2, drug2) %>%
  summarize(cohort_count_conc = sum(count_conc))

pn_count_IBD_ADA = pn_count_IJD %>%
  filter(drug2 == "ADA")
pn_count_IBD_INF = pn_count_IJD %>%
  filter(drug2 == "INF")



ggplot(pn_count_IJD) +
  geom_line(aes(x=as.Date(paste(year,month, "01", sep = "-")), y = count_per_patient, color = drug2)) +
  geom_point(aes(x=as.Date(paste(year,month, "01", sep = "-")), y = count_per_patient, color = drug2)) +
  scale_color_manual(values = c("ADA" = "red",
                                "ETA" = "darkblue",
                                "INF" = "cornsilk4",
                                "Others" = "bisque")) +
  labs(x = "Date", y = "Monthly Average Concomitants Count") +
  ggtitle("Monthly Concomitants Count per patient in yearly IJD cohort") +
  geom_vline(xintercept = as.Date(c("2019-05-27", "2019-11-25", "2021-04-07", "2021-10-06"))) +
  ylim(0,20) +
  theme_minimal()

pn_count_ISD = pn_count_IAD %>% 
  filter(cohort2 == "ISD") %>%
  mutate(count_per_patient = count_per_patient_IAD)

ggplot(pn_count_ISD) +
  geom_line(aes(x=as.Date(paste(year,month, "01", sep = "-")), y = count_per_patient, color = drug2)) +
  geom_point(aes(x=as.Date(paste(year,month, "01", sep = "-")), y = count_per_patient, color = drug2)) +
  scale_color_manual(values = c("ADA" = "red",
                                "ETA" = "darkblue",
                                "INF" = "cornsilk4",
                                "Others" = "bisque")) +
  labs(x = "Date", y = "Monthly Average Concomitants Count") +
  ggtitle("Monthly Concomitants Count per patient in yearly ISD cohort") +
  geom_vline(xintercept = as.Date(c("2019-05-27", "2019-11-25", "2021-04-07", "2021-10-06"))) +
  ylim(0,20) +
  theme_minimal()

pn_count_IBD1 = pn_count_IBD %>% 
  filter(cohort2 == "IBD") %>%
  mutate(count_per_patient = count_per_patient_IBD)

ggplot(pn_count_IBD1) +
  geom_line(aes(x=as.Date(paste(year,month, "01", sep = "-")), y = count_per_patient, color = drug2)) +
  geom_point(aes(x=as.Date(paste(year,month, "01", sep = "-")), y = count_per_patient, color = drug2)) +
  scale_color_manual(values = c("ADA" = "red",
                                "ETA" = "darkblue",
                                "INF" = "cornsilk4",
                                "Others" = "bisque")) +
  labs(x = "Date", y = "Monthly Average Concomitants Count") +
  ggtitle("Monthly Concomitants Count per patient in yearly IBD cohort") +
  geom_vline(xintercept = as.Date(c("2019-09-05", "2020-03-05", "2021-04-07", "2021-10-06"))) +
  ylim(0,20) +
  theme_minimal()
