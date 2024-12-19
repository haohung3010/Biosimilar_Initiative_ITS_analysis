
pn_clm = read.csv("R:/working/hdang_AnalysisFiles/Input/PN_cost/pn_clm.csv")
price_index = read.csv("R:/working/hdang_AnalysisFiles/Input/PN_cost/StatCan_price_index.csv", stringsAsFactors = FALSE)

pn_clm$Servdate = as.Date(pn_clm$Servdate)
pn_clm = pn_clm %>% filter(Servdate >= "2014-01-01")


pn_drug_bydate = pn_clm %>%
  left_join(drug_bydate, by = c("STUDYID" = "STUDYID", "Servdate" = "rx_date"))

pn_drug_bydate$iad_category = as.factor(pn_drug_bydate$iad_category)
pn_drug_bydate$IBD_Category = as.factor(pn_drug_bydate$IBD_Category)

index_2022 = price_index$Index[price_index$Fiscal_Year == "2022"]

#loop through year function:
generate_day = function (start_date, num_days) {
  seq(start_date, by = "days", length.out = num_days)
}

loop_yr_IAD = function(st, end) {
  pn_cost_temp = data.frame() 
  for (i in st:end) {
    pn_related = pn_clm %>% 
      filter( #filter for concomitant 
        year(Servdate) == i,
        iad_category == "CSDMARD" | iad_category == "NSAID" |iad_category == "OCS")
    temp0 = pn_related %>%
      mutate( #calculate cost of PharmaCare and cost total
        cost_daily = Total_PharmaCare_Paid / pmax(1, Dispensed_Days_Supply),
        cost2_daily = (Total_PharmaCare_Paid + Total_Patient_Paid) / pmax(1, Dispensed_Days_Supply))
    temp1 = temp0 %>% 
      mutate( # populate the rows with dates of supply
        date = pmap(list(Servdate, Dispensed_Days_Supply), generate_day)
      ) %>% 
      unnest(date) %>% 
      mutate(date = as.Date(date, "%Y-%m-%d"))
    pn_cost_temp = bind_rows(pn_cost_temp, temp1)
  }
  return (pn_cost_temp)
}

loop_yr_IBD = function(st, end) {
  pn_cost_temp = data.frame() 
  for (i in st:end) {
    pn_related = pn_clm %>% 
      filter( #filter for concomitant 
        year(Servdate) == i,
        IBD_Category == "AMINOSALICYLATES" | IBD_Category == "CORTICOSTEROIDS" | IBD_Category == "IMMUNE SUPPRESSANTS" | IBD_Category == "ORAL GLUCOCORTICOIDS")
    temp0 = pn_related %>%
      mutate( #calculate cost of PharmaCare and cost total
        cost_daily = Total_PharmaCare_Paid / pmax(1, Dispensed_Days_Supply),
        cost2_daily = (Total_PharmaCare_Paid + Total_Patient_Paid) / pmax(1, Dispensed_Days_Supply))
    temp1 = temp0 %>% 
      mutate( # populate the rows with dates of supply
        date = pmap(list(Servdate, Dispensed_Days_Supply), generate_day)) %>% 
      unnest(date) %>% 
      mutate(date = as.Date(date, "%Y-%m-%d"))
    pn_cost_temp = bind_rows(pn_cost_temp, temp1)
  }
  return (pn_cost_temp)
}

pn_cost_temp_IAD_2014_2022 = loop_yr_IAD(2014, 2022)
pn_cost_temp_IBD_2014_2022 = loop_yr_IBD(2014, 2022)

pn_cost_IAD = pn_cost_temp_IAD_2014_2022 %>%
  inner_join(drug_bydate_cohort, by = c("STUDYID" = "STUDYID", "date" = "rx_date")) %>%
  
  mutate(fy = ifelse(month(date) <= 3, year - 1, year),
         drug2 = case_when(drug == "ADA" | drug == "ADAB" ~ "ADA",
                           drug == "ETA" | drug == "ETAB" ~ "ETA",
                           drug == "INF" | drug == "INFB" ~ "INF",
                           drug == "GOL" | drug == "CER" ~ "Others")) %>%
  filter(!is.na(drug)) %>%
  
  group_by(STUDYID, year, month, cohort2, drug2, fy) %>%
  summarise(cost_pn = sum(cost_daily), cost2_pn = sum(cost2_daily)) %>%
  
  left_join(price_index, by = c("fy" = "Fiscal_Year")) %>%
  mutate(
    cost_pn_2022 = cost_pn / (Index / index_2022),
    cost2_pn_2022 = cost2_pn / (Index / index_2022)) %>%
  
  filter(year >= 2015 & year <= 2022) %>%
  group_by(year, month, cohort2, drug2) %>%
  summarize(PharmaCare_Cost = sum(cost_pn_2022), PharmaCare_and_Client = sum(cost2_pn_2022)) %>%
  select(year, month, cohort2, drug2, PharmaCare_Cost, PharmaCare_and_Client) %>%
  inner_join(drug_bydate_cohort_size4, by = c ("year" = "year", "cohort2" = "cohort2", "drug2" = "drug2")) %>%
  mutate(cost_per_patient1_IAD = PharmaCare_Cost/cohort_size,
         cost_per_patient2_IAD = PharmaCare_and_Client/cohort_size) 

pn_cost_IBD = pn_cost_temp_IBD_2014_2022 %>%
  inner_join(drug_bydate_cohort, by = c("STUDYID" = "STUDYID", "date" = "rx_date")) %>%
  
  mutate(fy = ifelse(month(date) <= 3, year - 1, year),
  drug2 = case_when(drug == "ADA" | drug == "ADAB" ~ "ADA",
                  drug == "ETA" | drug == "ETAB" ~ "ETA",
                  drug == "INF" | drug == "INFB" ~ "INF",
                  drug == "GOL" | drug == "CER" ~ "Others")) %>%
  filter(!is.na(drug)) %>%
  
  group_by(STUDYID, year, month, cohort2, drug2, fy) %>%
  summarise(cost_pn = sum(cost_daily), cost2_pn = sum(cost2_daily)) %>%
  
  left_join(price_index, by = c("fy" = "Fiscal_Year")) %>%
  mutate(
    cost_pn_2022 = cost_pn / (Index / index_2022),
    cost2_pn_2022 = cost2_pn / (Index / index_2022)) %>%
  
  filter(year >= 2015 & year <= 2022) %>%
  group_by(year, month, cohort2, drug2) %>%
  summarize(PharmaCare_Cost = sum(cost_pn_2022), PharmaCare_and_Client = sum(cost2_pn_2022)) %>%
  select(year, month, cohort2, drug2, PharmaCare_Cost, PharmaCare_and_Client) %>%
  inner_join(drug_bydate_cohort_size4, by = c ("year" = "year", "cohort2" = "cohort2", "drug2" = "drug2")) %>%
  mutate(cost_per_patient1_IBD = PharmaCare_Cost/cohort_size,
         cost_per_patient2_IBD = PharmaCare_and_Client/cohort_size) 

#################################################
pn_cost_IJD = pn_cost_IAD %>% 
  filter(cohort2 == "IJD") %>%
  mutate(cost_per_patient1 = cost_per_patient1_IAD,
         cost_per_patient2 = cost_per_patient2_IAD)

ggplot(pn_cost_IJD) +
  geom_line(aes(x=as.Date(paste(year,month, "01", sep = "-")), y = cost_per_patient2, color = drug2)) +
  geom_point(aes(x=as.Date(paste(year,month, "01", sep = "-")), y = cost_per_patient2, color = drug2)) +
  scale_color_manual(values = c("ADA" = "red",
                                "ETA" = "darkblue",
                                "INF" = "cornsilk4",
                                "Others" = "bisque")) +
  labs(x = "Date", y = "Monthly Average Concomitants cost (2022 CAD)") +
  ggtitle("Monthly Concomitants Cost per patient in yearly IJD cohort") +
  geom_vline(xintercept = as.Date(c("2019-05-27", "2019-11-25", "2021-04-07", "2021-10-06"))) +
  ylim(0,29) +
  theme_minimal()
#################################################
pn_cost_ISD = pn_cost_IAD %>% 
  filter(cohort2 == "ISD") %>%
  mutate(cost_per_patient1 = cost_per_patient1_IAD,
         cost_per_patient2 = cost_per_patient2_IAD)

ggplot(pn_cost_ISD) +
  geom_line(aes(x=as.Date(paste(year,month, "01", sep = "-")), y = cost_per_patient2, color = drug2)) +
  geom_point(aes(x=as.Date(paste(year,month, "01", sep = "-")), y = cost_per_patient2, color = drug2)) +
  scale_color_manual(values = c("ADA" = "red",
                                "ETA" = "darkblue",
                                "INF" = "cornsilk4",
                                "Others" = "bisque")) +
  labs(x = "Date", y = "Monthly Average Concomitants cost (2022 CAD)") +
  ggtitle("Monthly Concomitants Cost per patient in yearly ISD cohort") +
  geom_vline(xintercept = as.Date(c("2019-05-27", "2019-11-25", "2021-04-07", "2021-10-06"))) +
  ylim(0,19) +
  theme_minimal()
###################################################
pn_cost_IBD = pn_cost_IBD %>% 
  filter(cohort2 == "IBD") %>%
  mutate(cost_per_patient1 = cost_per_patient1_IBD,
         cost_per_patient2 = cost_per_patient2_IBD)

ggplot(pn_cost_IBD) +
  geom_line(aes(x=as.Date(paste(year,month, "01", sep = "-")), y = cost_per_patient2, color = drug2)) +
  geom_point(aes(x=as.Date(paste(year,month, "01", sep = "-")), y = cost_per_patient2, color = drug2)) +
  scale_color_manual(values = c("ADA" = "red",
                                "ETA" = "darkblue",
                                "INF" = "cornsilk4",
                                "Others" = "bisque")) +
  labs(x = "Date", y = "Monthly Average Concomitants cost (2022 CAD)") +
  ggtitle("Monthly Concomitants Cost per patient in yearly IBD cohort") +
  geom_vline(xintercept = as.Date(c("2019-09-05", "2020-03-05", "2021-04-07", "2021-10-06"))) +
  ylim(0,75) +
  theme_minimal()

#cohort count check
count_check_pn_cost = pn_cost_temp_IAD_2014_2022 %>%
  rbind(pn_cost_temp_IBD_2014_2022) %>%
  inner_join(drug_bydate_cohort, by = c("STUDYID" = "STUDYID", "date" = "rx_date")) %>%
  mutate(drug2 = case_when(drug == "ADA" | drug == "ADAB" ~ "ADA",
                           drug == "ETA" | drug == "ETAB" ~ "ETA",
                           drug == "INF" | drug == "INFB" ~ "INF",
                           drug == "GOL"  | drug == "CER" ~ "Others")) %>%
  filter(!is.na(drug)) %>%
  group_by(STUDYID, year) %>%
  slice(1) %>% 
  ungroup() %>%
  group_by(year, cohort2, drug2) %>%
  summarize(cohort_size = n())
# -> smaller than drug_bydate_cohort_size4
