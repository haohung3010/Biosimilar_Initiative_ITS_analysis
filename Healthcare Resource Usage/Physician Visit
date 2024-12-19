msp_visit_drug =  msp %>%
  left_join(drug_bydate_cohort, by = c("STUDYID" = "STUDYID", "ServDate" = "rx_date", "year" = "year")) %>%
  mutate(md_spec = case_when(spec == 1 ~ "DERM",
                             spec == 44 ~ "RHEUM",
                             spec == 56 ~ "GASTRO",
                             spec == 15 ~ "INTMED",
                             spec == 8 ~ "SURG",
                             spec == 0 ~ "GP"))

drug_bydate_cohort_size4 = drug_bydate_cohort %>% 
  filter(!is.na(cohort2)) %>%
  mutate(drug2 = case_when(drug == "ADA" | drug == "ADAB" ~ "ADA",
                           drug == "ETA" | drug == "ETAB" ~ "ETA",
                           drug == "INF" | drug == "INFB" ~ "INF",
                           drug == "CER" | drug == "GOL" ~ "OTHER")) %>%
  group_by(STUDYID, drug2, year) %>%
  slice(1) %>% 
  ungroup() %>%
  group_by(year, cohort2, drug2) %>%
  summarize(cohort_size = n())

drug_bydate_cohort_size_IJD = drug_bydate_cohort_size4 %>% filter(cohort2 == "IJD")
drug_bydate_cohort_size_ISD = drug_bydate_cohort_size4 %>% filter(cohort2 == "ISD")
drug_bydate_cohort_size_IBD = drug_bydate_cohort_size4 %>% filter(cohort2 == "IBD")

##########################################
################ IJD #####################
##########################################

msp_rate_ADA_IJD_SP = msp_visit_drug %>%
  filter(cohort2 == "IJD") %>%
  filter(drug == "ADA" | drug == "ADAB") %>%
  filter(md_spec == "RHEUM") %>%
  mutate(year = year(ServDate),
         month = month(ServDate)) %>%
  group_by(year, month, ServDate, rfr_pracnum) %>%
  summarize(msp_visit_per_day = n_distinct(rfr_pracnum)) %>%
  ungroup() %>%
  group_by(year, month) %>%
  summarize(msp_visit = sum(msp_visit_per_day)) %>%
  mutate(drug = "ADA") %>%
  left_join(drug_bydate_cohort_size_IJD, by = c("year" = "year", "drug" = "drug2")) %>%
  mutate(msp_rate = msp_visit/cohort_size,
         line_type = "ADA-Specialist",
         md = "Specialist")

msp_rate_ADA_IJD_Oth = msp_visit_drug %>%
  filter(cohort2 == "IJD") %>%
  filter(drug == "ADA" | drug == "ADAB") %>%
  filter(is.na(md_spec) | md_spec != "RHEUM") %>%
  mutate(year = year(ServDate),
         month = month(ServDate)) %>%
  group_by(year, month, ServDate, rfr_pracnum) %>%
  summarize(msp_visit_per_day = n_distinct(rfr_pracnum)) %>%
  ungroup() %>%
  group_by(year, month) %>%
  summarize(msp_visit = sum(msp_visit_per_day)) %>%
  mutate(drug = "ADA") %>%
  left_join(drug_bydate_cohort_size_IJD, by = c("year" = "year", "drug" = "drug2")) %>%
  mutate(msp_rate = msp_visit/cohort_size,
         line_type = "ADA-Others",
         md = "Others")

msp_rate_ETA_IJD_SP = msp_visit_drug %>%
  filter(cohort2 == "IJD") %>%
  filter(drug == "ETA" | drug == "ETAB") %>%
  filter(md_spec == "RHEUM") %>%
  mutate(year = year(ServDate),
         month = month(ServDate)) %>%
  group_by(year, month, ServDate, rfr_pracnum) %>%
  summarize(msp_visit_per_day = n_distinct(rfr_pracnum)) %>%
  ungroup() %>%
  group_by(year, month) %>%
  summarize(msp_visit = sum(msp_visit_per_day)) %>%
  mutate(drug = "ETA") %>%
  left_join(drug_bydate_cohort_size_IJD, by = c("year" = "year", "drug" = "drug2")) %>%
  mutate(msp_rate = msp_visit/cohort_size,
         line_type = "ETA-Specialist",
         md = "Specialist")
msp_rate_ETA_IJD_Oth = msp_visit_drug %>%
  filter(cohort2 == "IJD") %>%
  filter(drug == "ETA" | drug == "ETAB") %>%
  filter(is.na(md_spec) | md_spec != "RHEUM") %>%
  mutate(year = year(ServDate),
         month = month(ServDate)) %>%
  group_by(year, month, ServDate, rfr_pracnum) %>%
  summarize(msp_visit_per_day = n_distinct(rfr_pracnum)) %>%
  ungroup() %>%
  group_by(year, month) %>%
  summarize(msp_visit = sum(msp_visit_per_day)) %>%
  mutate(drug = "ETA") %>%
  left_join(drug_bydate_cohort_size_IJD, by = c("year" = "year", "drug" = "drug2")) %>%
  mutate(msp_rate = msp_visit/cohort_size,
         line_type = "ETA-Others",
         md = "Others")

msp_rate_INF_IJD_SP = msp_visit_drug %>%
  filter(cohort2 == "IJD") %>%
  filter(drug == "INF" | drug == "INFB") %>%
  filter(md_spec == "RHEUM") %>%
  mutate(year = year(ServDate),
         month = month(ServDate)) %>%
  group_by(year, month, ServDate, rfr_pracnum) %>%
  summarize(msp_visit_per_day = n_distinct(rfr_pracnum)) %>%
  ungroup() %>%
  group_by(year, month) %>%
  summarize(msp_visit = sum(msp_visit_per_day)) %>%
  mutate(drug = "INF") %>%
  left_join(drug_bydate_cohort_size_IJD, by = c("year" = "year", "drug" = "drug2")) %>%
  mutate(msp_rate = msp_visit/cohort_size,
         line_type = "INF-Specialist",
         md = "Specialist")

msp_rate_INF_IJD_Oth = msp_visit_drug %>%
  filter(cohort2 == "IJD") %>%
  filter(drug == "INF" | drug == "INFB") %>%
  filter(is.na(md_spec) | md_spec != "RHEUM") %>%
  mutate(year = year(ServDate),
         month = month(ServDate)) %>%
  group_by(year, month, ServDate, rfr_pracnum) %>%
  summarize(msp_visit_per_day = n_distinct(rfr_pracnum)) %>%
  ungroup() %>%
  group_by(year, month) %>%
  summarize(msp_visit = sum(msp_visit_per_day)) %>%
  mutate(drug = "INF") %>%
  left_join(drug_bydate_cohort_size_IJD, by = c("year" = "year", "drug" = "drug2")) %>%
  mutate(msp_rate = msp_visit/cohort_size,
         line_type = "INF-Others",
         md = "Others")


msp_rate_ETAINF_IJD_SP = rbind(msp_rate_ETA_IJD_SP,msp_rate_INF_IJD_SP)
msp_rate_ETAINF_IJD_SP1 = msp_rate_ETAINF_IJD_SP %>%
  group_by(year, month) %>%
  summarize(msp_visit1 = sum(msp_visit),cohort_size1=sum(cohort_size)) %>%
  mutate(msp_rate = msp_visit1/cohort_size1,
         line_type = "ETAINF-Spec",
         md = "Spec")

msp_rate_ETAINF_IJD_Oth = rbind(msp_rate_ETA_IJD_Oth,msp_rate_INF_IJD_Oth)
msp_rate_ETAINF_IJD_Oth1 = msp_rate_ETAINF_IJD_Oth %>%
  group_by(year, month) %>%
  summarize(msp_visit1 = sum(msp_visit),cohort_size1=sum(cohort_size)) %>%
  mutate(msp_rate = msp_visit1/cohort_size1,
         line_type = "ETAINF-Others",
         md = "Others")

IJD_msp_combined = rbind(msp_rate_ADA_IJD_SP, msp_rate_ADA_IJD_Oth, msp_rate_ETA_IJD_SP, msp_rate_ETA_IJD_Oth, msp_rate_INF_IJD_SP, msp_rate_INF_IJD_Oth)
ggplot(IJD_msp_combined) +
  geom_line(aes(x=as.Date(paste(year,month, "01", sep = "-")), y = msp_rate, color = line_type, linetype = md)) +
  geom_point(aes(x=as.Date(paste(year,month, "01", sep = "-")), y = msp_rate, color = line_type, shape = md)) +
  scale_color_manual(values = c("ADA-Specialist" = "red", "ADA-Others" = "pink",
                                "ETA-Specialist" = "darkblue", "ETA-Others" = "lightblue",
                                "INF-Specialist" = "cornsilk4", "INF-Others" = "bisque")) +
  scale_linetype_manual(values = c("Specialist" = "solid", "Others" = "dashed")) +
  scale_shape_manual(values = c("Specialist" = "circle", "Others" = "triangle")) +
  labs(x = "Date", y = "Monthly Physician Visit") +
  ggtitle("Monthly Physician Visit Rate per patient in the yearly IJD cohort") +
  geom_vline(xintercept = as.Date(c("2019-05-27", "2019-11-25", "2021-04-07", "2021-10-06"))) +
  ylim(0,1.6) +
  theme_minimal()

##########################################
################ ISD #####################
##########################################

msp_rate_ADA_ISD_SP = msp_visit_drug %>%
  filter(cohort2 == "ISD") %>%
  filter(drug == "ADA" | drug == "ADAB") %>%
  filter(md_spec == "RHEUM" | md_spec == "DERM") %>%
  mutate(year = year(ServDate),
         month = month(ServDate)) %>%
  group_by(year, month, ServDate, rfr_pracnum) %>%
  summarize(msp_visit_per_day = n_distinct(rfr_pracnum)) %>%
  ungroup() %>%
  group_by(year, month) %>%
  summarize(msp_visit = sum(msp_visit_per_day)) %>%
  mutate(drug = "ADA") %>%
  left_join(drug_bydate_cohort_size_ISD, by = c("year" = "year", "drug" = "drug2")) %>%
  mutate(msp_rate = msp_visit/cohort_size,
         line_type = "ADA-Specialist",
         md = "Specialist")
msp_rate_ADA_ISD_Oth = msp_visit_drug %>%
  filter(cohort2 == "ISD") %>%
  filter(drug == "ADA" | drug == "ADAB") %>%
  filter(is.na(md_spec) | (md_spec != "RHEUM" & md_spec != "DERM")) %>%
  mutate(year = year(ServDate),
         month = month(ServDate)) %>%
  group_by(year, month, ServDate, rfr_pracnum) %>%
  summarize(msp_visit_per_day = n_distinct(rfr_pracnum)) %>%
  ungroup() %>%
  group_by(year, month) %>%
  summarize(msp_visit = sum(msp_visit_per_day)) %>%
  mutate(drug = "ADA") %>%
  left_join(drug_bydate_cohort_size_ISD, by = c("year" = "year", "drug" = "drug2")) %>%
  mutate(msp_rate = msp_visit/cohort_size,
         line_type = "ADA-Others",
         md = "Others")

msp_rate_ETA_ISD_SP = msp_visit_drug %>%
  filter(cohort2 == "ISD") %>%
  filter(drug == "ETA" | drug == "ETAB") %>%
  filter(md_spec == "RHEUM" | md_spec == "DERM") %>%
  mutate(year = year(ServDate),
         month = month(ServDate)) %>%
  group_by(year, month, ServDate, rfr_pracnum) %>%
  summarize(msp_visit_per_day = n_distinct(rfr_pracnum)) %>%
  ungroup() %>%
  group_by(year, month) %>%
  summarize(msp_visit = sum(msp_visit_per_day)) %>%
  mutate(drug = "ETA") %>%
  left_join(drug_bydate_cohort_size_ISD, by = c("year" = "year", "drug" = "drug2")) %>%
  mutate(msp_rate = msp_visit/cohort_size,
         line_type = "ETA-Specialist",
         md = "Specialist")
msp_rate_ETA_ISD_Oth = msp_visit_drug %>%
  filter(cohort2 == "ISD") %>%
  filter(drug == "ETA" | drug == "ETAB") %>%
  filter(is.na(md_spec) | (md_spec != "RHEUM" & md_spec != "DERM")) %>%
  mutate(year = year(ServDate),
         month = month(ServDate)) %>%
  group_by(year, month, ServDate, rfr_pracnum) %>%
  summarize(msp_visit_per_day = n_distinct(rfr_pracnum)) %>%
  ungroup() %>%
  group_by(year, month) %>%
  summarize(msp_visit = sum(msp_visit_per_day)) %>%
  mutate(drug = "ETA") %>%
  left_join(drug_bydate_cohort_size_ISD, by = c("year" = "year", "drug" = "drug2")) %>%
  mutate(msp_rate = msp_visit/cohort_size,
         line_type = "ETA-Others",
         md = "Others")

msp_rate_INF_ISD_SP = msp_visit_drug %>%
  filter(cohort2 == "ISD") %>%
  filter(drug == "INF" | drug == "INFB") %>%
  filter(md_spec == "RHEUM" | md_spec == "DERM") %>%
  mutate(year = year(ServDate),
         month = month(ServDate)) %>%
  group_by(year, month, ServDate, rfr_pracnum) %>%
  summarize(msp_visit_per_day = n_distinct(rfr_pracnum)) %>%
  ungroup() %>%
  group_by(year, month) %>%
  summarize(msp_visit = sum(msp_visit_per_day)) %>%
  mutate(drug = "INF") %>%
  left_join(drug_bydate_cohort_size_ISD, by = c("year" = "year", "drug" = "drug2")) %>%  
  mutate(msp_rate = msp_visit/cohort_size,
         line_type = "INF-Specialist",
         md = "Specialist")
msp_rate_INF_ISD_Oth = msp_visit_drug %>%
  filter(cohort2 == "ISD") %>%
  filter(drug == "INF" | drug == "INFB") %>%
  filter(is.na(md_spec) | (md_spec != "RHEUM" & md_spec != "DERM")) %>%
  mutate(year = year(ServDate),
         month = month(ServDate)) %>%
  group_by(year, month, ServDate, rfr_pracnum) %>%
  summarize(msp_visit_per_day = n_distinct(rfr_pracnum)) %>%
  ungroup() %>%
  group_by(year, month) %>%
  summarize(msp_visit = sum(msp_visit_per_day)) %>%
  mutate(drug = "INF") %>%
  left_join(drug_bydate_cohort_size_ISD, by = c("year" = "year", "drug" = "drug2")) %>%  
  mutate(msp_rate = msp_visit/cohort_size,
         line_type = "INF-Others",
         md = "Others")

ISD_msp_combined = rbind(msp_rate_ADA_ISD_SP, msp_rate_ADA_ISD_Oth, msp_rate_ETA_ISD_SP, msp_rate_ETA_ISD_Oth, msp_rate_INF_ISD_SP, msp_rate_INF_ISD_Oth)
ggplot(ISD_msp_combined) +
  geom_line(aes(x=as.Date(paste(year,month, "01", sep = "-")), y = msp_rate, color = line_type, linetype = md)) +
  geom_point(aes(x=as.Date(paste(year,month, "01", sep = "-")), y = msp_rate, color = line_type, shape = md)) +
  scale_color_manual(values = c("ADA-Specialist" = "red", "ADA-Others" = "pink",
                                "ETA-Specialist" = "darkblue", "ETA-Others" = "lightblue",
                                "INF-Specialist" = "cornsilk4", "INF-Others" = "bisque")) +
  scale_linetype_manual(values = c("Specialist" = "solid", "Others" = "dashed")) +
  scale_shape_manual(values = c("Specialist" = "circle", "Others" = "triangle")) +labs(x = "Date", y = "Monthly Physician Visit Rate") +
  ggtitle("Monthly Physician Visit Rate per patient in the yearly ISD cohort") +
  geom_vline(xintercept = as.Date(c("2019-05-27", "2019-11-25", "2021-04-07", "2021-10-06"))) +
  ylim(0,1.6) +
  theme_minimal()

##########################################
################ IBD #####################
##########################################
msp_rate_ADA_IBD_SP = msp_visit_drug %>%
  filter(cohort2 == "IBD") %>%
  filter(drug == "ADA" | drug == "ADAB") %>%
  filter(md_spec == "GASTRO") %>%
  mutate(year = year(ServDate),
         month = month(ServDate)) %>%
  group_by(year, month, ServDate, rfr_pracnum) %>%
  summarize(msp_visit_per_day = n_distinct(rfr_pracnum)) %>%
  ungroup() %>%
  group_by(year, month) %>%
  summarize(msp_visit = sum(msp_visit_per_day)) %>%
  mutate(drug = "ADA") %>%
  left_join(drug_bydate_cohort_size_IBD, by = c("year" = "year", "drug" = "drug2")) %>%  
  mutate(msp_rate = msp_visit/cohort_size,
         line_type = "ADA-Specialist",
         md = "Specialist")
msp_rate_ADA_IBD_Oth = msp_visit_drug %>%
  filter(cohort2 == "IBD") %>%
  filter(drug == "ADA" | drug == "ADAB") %>%
  filter(is.na(md_spec) | md_spec != "GASTRO") %>%
  mutate(year = year(ServDate),
         month = month(ServDate)) %>%
  group_by(year, month, ServDate, rfr_pracnum) %>%
  summarize(msp_visit_per_day = n_distinct(rfr_pracnum)) %>%
  ungroup() %>%
  group_by(year, month) %>%
  summarize(msp_visit = sum(msp_visit_per_day)) %>%
  mutate(drug = "ADA") %>%
  left_join(drug_bydate_cohort_size_IBD, by = c("year" = "year", "drug" = "drug2")) %>%
  mutate(msp_rate = msp_visit/cohort_size,
         line_type = "ADA-Others",
         md = "Others")

msp_rate_INF_IBD_SP = msp_visit_drug %>%
  filter(cohort2 == "IBD") %>%
  filter(drug == "INF" | drug == "INFB") %>%
  filter(md_spec == "GASTRO") %>%
  mutate(year = year(ServDate),
         month = month(ServDate)) %>%
  group_by(year, month, ServDate, rfr_pracnum) %>%
  summarize(msp_visit_per_day = n_distinct(rfr_pracnum)) %>%
  ungroup() %>%
  group_by(year, month) %>%
  summarize(msp_visit = sum(msp_visit_per_day)) %>%
  mutate(drug = "INF") %>%
  left_join(drug_bydate_cohort_size_IBD, by = c("year" = "year", "drug" = "drug2")) %>%  
  mutate(msp_rate = msp_visit/cohort_size,
         line_type = "INF-Specialist",
         md = "Specialist")
msp_rate_INF_IBD_Oth = msp_visit_drug %>%
  filter(cohort2 == "IBD") %>%
  filter(drug == "INF" | drug == "INFB") %>%
  filter(is.na(md_spec) | md_spec != "GASTRO") %>%
  mutate(year = year(ServDate),
         month = month(ServDate)) %>%
  group_by(year, month, ServDate, rfr_pracnum) %>%
  summarize(msp_visit_per_day = n_distinct(rfr_pracnum)) %>%
  ungroup() %>%
  group_by(year, month) %>%
  summarize(msp_visit = sum(msp_visit_per_day)) %>%
  mutate(drug = "INF") %>%
  left_join(drug_bydate_cohort_size_IBD, by = c("year" = "year", "drug" = "drug2")) %>%
  mutate(msp_rate = msp_visit/cohort_size,
         line_type = "INF-Others",
         md = "Others")

IBD_msp_combined = rbind(msp_rate_ADA_IBD_SP, msp_rate_ADA_IBD_Oth, msp_rate_INF_IBD_SP, msp_rate_INF_IBD_Oth)
ggplot(IBD_msp_combined) +
  geom_line(aes(x=as.Date(paste(year,month, "01", sep = "-")), y = msp_rate, color = line_type, linetype = md)) +
  geom_point(aes(x=as.Date(paste(year,month, "01", sep = "-")), y = msp_rate, color = line_type, shape = md)) +
  scale_color_manual(values = c("ADA-Specialist" = "red", "ADA-Others" = "pink",
                                "ETA-Specialist" = "darkblue", "ETA-Others" = "lightblue",
                                "INF-Specialist" = "cornsilk4", "INF-Others" = "bisque")) +
  scale_linetype_manual(values = c("Specialist" = "solid", "Others" = "dashed")) +
  scale_shape_manual(values = c("Specialist" = "circle", "Others" = "triangle")) +
  labs(x = "Date", y = "Monthly Physician Visit") +
  ggtitle("Monthly Physician Visit Rate per patient in the yearly IBD cohort") +
  geom_vline(xintercept = as.Date(c("2019-09-05", "2020-03-05", "2021-04-07", "2021-10-06"))) +
  ylim(0,1.6) +
  theme_minimal()

# cohort count check
count_check_msp_rate = msp_visit_drug %>%
  mutate(drug2 = case_when(drug == "ADA" | drug == "ADAB" ~ "ADA",
                           drug == "ETA" | drug == "ETAB" ~ "ETA",
                           drug == "INF" | drug == "INFB" ~ "INF",
                           drug == "GOL" | drug == "CER" ~ "Others")) %>%
  mutate(year = year(ServDate),
         month = month(ServDate)) %>%
  group_by(year, month, ServDate, rfr_pracnum) %>%
  slice(1) %>%
  ungroup() %>%
  group_by(year, month, cohort2, drug2, md_spec) %>%
  summarize(cohort_size = n()) 
# -> IBD cohort in January 2015 is 2625, smaller than drug_bydate_cohort_size4
