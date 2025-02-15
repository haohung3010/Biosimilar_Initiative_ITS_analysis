
nacrs = read.csv("R:/working/hdang_AnalysisFiles/Input/nacrs_related.csv")
nacrs$servdate = as.Date(nacrs$REGDATE) 
nacrs = nacrs %>% filter(servdate >= "2015-01-01")

nacrs_drug_bydate = nacrs %>%
  left_join(drug_bydate_cohort, by = c("STUDYID" = "STUDYID", "servdate" = "rx_date")) %>%
  select("STUDYID", "servdate", "AS", "RA", "CD", "PA", "UC", "drug", "cohort", "cohort2", "dx", "biosimilar") %>%
  mutate(year = year(servdate),
         month = month(servdate),
         related = case_when(RA == 1 | AS == 1 ~ "IJD",
                             PA == 1 ~ "ISD",
                             (CD == 1 | UC == 1 ~ "IBD")))

date_range = expand.grid(year = 2015:2022,
                         month = 1:12)
#####################################
########### IJD Cohort ##############
#####################################

nacrs_visit_ADA_IJD_related = nacrs_drug_bydate %>%
  filter(cohort2 == "IJD") %>%
  filter(drug == "ADA" | drug == "ADAB") %>%
  filter(related == "IJD") %>%
  group_by(year, month) %>%
  summarize(nacrs_visit = n()) %>%
  right_join(date_range, by = c("year", "month")) %>%
  mutate(nacrs_visit = ifelse(is.na(nacrs_visit), 0, nacrs_visit),
         drug = "ADA") %>%
  left_join(drug_bydate_cohort_size_IJD, by = c("year" = "year", "drug" = "drug2")) %>%
  mutate(nacrs_visit_per_100 = nacrs_visit/cohort_size * 100,
         line_type = "ADA-Related ED Visits",
         relation = "related",
         nacrs_visit_per_100 = ifelse(is.na(nacrs_visit_per_100), 0, nacrs_visit_per_100))

nacrs_visit_ADA_IJD_unrelated = nacrs_drug_bydate %>%
  filter(cohort2 == "IJD") %>%
  filter(drug == "ADA" | drug == "ADAB") %>%
  filter(is.na(related) | related != "IJD") %>%
  group_by(year, month) %>%
  summarize(nacrs_visit = n()) %>%
  right_join(date_range, by = c("year", "month")) %>%
  mutate(nacrs_visit = ifelse(is.na(nacrs_visit), 0, nacrs_visit),
         drug = "ADA") %>%
  left_join(drug_bydate_cohort_size_IJD, by = c("year" = "year", "drug" = "drug2")) %>%
  mutate(nacrs_visit_per_100 = nacrs_visit/cohort_size * 100,
         line_type = "ADA-Unrelated ED Visits",
         relation = "unrelated",
         nacrs_visit_per_100 = ifelse(is.na(nacrs_visit_per_100), 0, nacrs_visit_per_100))

nacrs_visit_ETA_IJD_related = nacrs_drug_bydate %>%
  filter(cohort2 == "IJD") %>%
  filter(drug == "ETA" | drug == "ETAB") %>%
  filter(related == "IJD") %>%
  group_by(year, month) %>%
  summarize(nacrs_visit = n()) %>%
  right_join(date_range, by = c("year", "month")) %>%
  mutate(nacrs_visit = ifelse(is.na(nacrs_visit), 0, nacrs_visit),
         drug = "ETA") %>%
  left_join(drug_bydate_cohort_size_IJD, by = c("year" = "year", "drug" = "drug2")) %>%
  mutate(nacrs_visit_per_100 = nacrs_visit/cohort_size * 100,
         line_type = "ETA-Related ED Visits",
         relation = "related",
         nacrs_visit_per_100 = ifelse(is.na(nacrs_visit_per_100), 0, nacrs_visit_per_100))
nacrs_visit_ETA_IJD_unrelated = nacrs_drug_bydate %>%
  filter(cohort2 == "IJD") %>%
  filter(drug == "ETA" | drug == "ETAB") %>%
  filter(is.na(related) | related != "IJD") %>%
  group_by(year, month) %>%
  summarize(nacrs_visit = n()) %>%
  right_join(date_range, by = c("year", "month")) %>%
  mutate(nacrs_visit = ifelse(is.na(nacrs_visit), 0, nacrs_visit),
         drug = "ETA") %>%
  left_join(drug_bydate_cohort_size_IJD, by = c("year" = "year", "drug" = "drug2")) %>%
  mutate(nacrs_visit_per_100 = nacrs_visit/cohort_size * 100,
         line_type = "ETA-Unrelated ED Visits",
         relation = "unrelated",
         nacrs_visit_per_100 = ifelse(is.na(nacrs_visit_per_100), 0, nacrs_visit_per_100))

nacrs_visit_INF_IJD_related = nacrs_drug_bydate %>%
  filter(cohort2 == "IJD") %>%
  filter(drug == "INF" | drug == "INFB") %>%
  filter(related == "IJD") %>%
  group_by(year, month) %>%
  summarize(nacrs_visit = n()) %>%
  right_join(date_range, by = c("year", "month")) %>%
  mutate(nacrs_visit = ifelse(is.na(nacrs_visit), 0, nacrs_visit),
         drug = "INF") %>%
  left_join(drug_bydate_cohort_size_IJD, by = c("year" = "year", "drug" = "drug2")) %>%
  mutate(nacrs_visit_per_100 = nacrs_visit/cohort_size * 100,
         line_type = "INF-Related ED Visits",
         relation = "related",
         nacrs_visit_per_100 = ifelse(is.na(nacrs_visit_per_100), 0, nacrs_visit_per_100))
nacrs_visit_INF_IJD_unrelated = nacrs_drug_bydate %>%
  filter(cohort2 == "IJD") %>%
  filter(drug == "INF" | drug == "INFB") %>%
  filter(is.na(related) | related != "IJD") %>%
  group_by(year, month) %>%
  summarize(nacrs_visit = n()) %>%
  right_join(date_range, by = c("year", "month")) %>%
  mutate(nacrs_visit = ifelse(is.na(nacrs_visit), 0, nacrs_visit),
         drug = "INF") %>%
  left_join(drug_bydate_cohort_size_IJD, by = c("year" = "year", "drug" = "drug2")) %>%
  mutate(nacrs_visit_per_100 = nacrs_visit/cohort_size * 100,
         line_type = "INF-Unrelated ED Visits",
         relation = "unrelated",
         nacrs_visit_per_100 = ifelse(is.na(nacrs_visit_per_100), 0, nacrs_visit_per_100))

nacrs_visit_ETAINF_IJD_related = rbind(nacrs_visit_ETA_IJD_related,nacrs_visit_INF_IJD_related)
nacrs_visit_ETAINF_IJD_related1 = nacrs_visit_ETAINF_IJD_related %>%
  group_by(year, month) %>%
  summarize(nacrs_visit1 = sum(nacrs_visit),cohort_size1=sum(cohort_size)) %>%
  mutate(nacrs_visit = nacrs_visit1/cohort_size1, nacrs_visit_per_100 = nacrs_visit1/cohort_size1*100,
         line_type = "ETAINF-related",
         relation = "related")

nacrs_visit_ETAINF_IJD_unrelated = rbind(nacrs_visit_ETA_IJD_unrelated,nacrs_visit_INF_IJD_unrelated)
nacrs_visit_ETAINF_IJD_unrelated1 = nacrs_visit_ETAINF_IJD_unrelated %>%
  group_by(year, month) %>%
  summarize(nacrs_visit1 = sum(nacrs_visit),cohort_size1=sum(cohort_size)) %>%
  mutate(nacrs_visit = nacrs_visit1/cohort_size1, nacrs_visit_per_100 = nacrs_visit1/cohort_size1*100,
         line_type = "ETAINF-Unrelated",
         relation = "unrelated")


IJD_nacrs_combined = rbind(nacrs_visit_ADA_IJD_related, nacrs_visit_ADA_IJD_unrelated, nacrs_visit_ETA_IJD_related, nacrs_visit_ETA_IJD_unrelated, nacrs_visit_INF_IJD_related, nacrs_visit_INF_IJD_unrelated)
ggplot(IJD_nacrs_combined) +
  geom_line(aes(x=as.Date(paste(year,month, "01", sep = "-")), y = nacrs_visit_per_100, color = line_type, linetype = relation)) +
  geom_point(aes(x=as.Date(paste(year,month, "01", sep = "-")), y = nacrs_visit_per_100, color = line_type, shape = relation)) +
  scale_color_manual(values = c("ADA-Related ED Visits" = "red", "ADA-Unrelated ED Visits" = "pink",
                                "ETA-Related ED Visits" = "darkblue", "ETA-Unrelated ED Visits" = "lightblue",
                                "INF-Related ED Visits" = "cornsilk4", "INF-Unrelated ED Visits" = "bisque")) +
  scale_linetype_manual(values = c("related" = "solid", "unrelated" = "dashed")) +
  scale_shape_manual(values = c("related" = "circle", "unrelated" = "triangle")) +
  labs(x = "Date", y = "Monthly Emergency visits per 100") +
  ggtitle("Monthly Emergency Visits per 100 patients in the yearly IJD cohort") +
  geom_vline(xintercept = as.Date(c("2019-05-27", "2019-11-25", "2021-04-07", "2021-10-06"))) +
  ylim(0,16) +
  theme_minimal()

#####################################
########### ISD Cohort ##############
#####################################

nacrs_visit_ADA_ISD_related = nacrs_drug_bydate %>%
  filter(cohort2 == "ISD") %>%
  filter(drug == "ADA" | drug == "ADAB") %>%
  filter(related == "ISD") %>%
  group_by(year, month) %>%
  summarize(nacrs_visit = n()) %>%
  right_join(date_range, by = c("year", "month")) %>%
  mutate(nacrs_visit = ifelse(is.na(nacrs_visit), 0, nacrs_visit),
         drug = "ADA") %>%
  left_join(drug_bydate_cohort_size_ISD, by = c("year" = "year", "drug" = "drug2")) %>%
  mutate(nacrs_visit_per_100 = nacrs_visit/cohort_size * 100,
         line_type = "ADA-Related ED Visits",
         relation = "related",
         nacrs_visit_per_100 = ifelse(is.na(nacrs_visit_per_100), 0, nacrs_visit_per_100))
nacrs_visit_ADA_ISD_unrelated = nacrs_drug_bydate %>%
  filter(cohort2 == "ISD") %>%
  filter(drug == "ADA" | drug == "ADAB") %>%
  filter(is.na(related) | related != "ISD") %>%
  group_by(year, month) %>%
  summarize(nacrs_visit = n()) %>%
  right_join(date_range, by = c("year", "month")) %>%
  mutate(nacrs_visit = ifelse(is.na(nacrs_visit), 0, nacrs_visit),
         drug = "ADA") %>%
  left_join(drug_bydate_cohort_size_ISD, by = c("year" = "year", "drug" = "drug2")) %>%
  mutate(nacrs_visit_per_100 = nacrs_visit/cohort_size * 100,
         line_type = "ADA-Unrelated ED Visits",
         relation = "unrelated",
         nacrs_visit_per_100 = ifelse(is.na(nacrs_visit_per_100), 0, nacrs_visit_per_100))

nacrs_visit_ETA_ISD_related = nacrs_drug_bydate %>%
  filter(cohort2 == "ISD") %>%
  filter(drug == "ETA" | drug == "ETAB") %>%
  filter(related == "ISD") %>%
  group_by(year, month) %>%
  summarize(nacrs_visit = n()) %>%
  right_join(date_range, by = c("year", "month")) %>%
  mutate(nacrs_visit = ifelse(is.na(nacrs_visit), 0, nacrs_visit),
         drug = "ETA") %>%
  left_join(drug_bydate_cohort_size_ISD, by = c("year" = "year", "drug" = "drug2")) %>%
  mutate(nacrs_visit_per_100 = nacrs_visit/cohort_size * 100,
         line_type = "ETA-Related ED Visits",
         relation = "related",
         nacrs_visit_per_100 = ifelse(is.na(nacrs_visit_per_100), 0, nacrs_visit_per_100))
nacrs_visit_ETA_ISD_unrelated = nacrs_drug_bydate %>%
  filter(cohort2 == "ISD") %>%
  filter(drug == "ETA" | drug == "ETAB") %>%
  filter(is.na(related) | related != "ISD") %>%
  group_by(year, month) %>%
  summarize(nacrs_visit = n()) %>%
  right_join(date_range, by = c("year", "month")) %>%
  mutate(nacrs_visit = ifelse(is.na(nacrs_visit), 0, nacrs_visit),
         drug = "ETA") %>%
  left_join(drug_bydate_cohort_size_ISD, by = c("year" = "year", "drug" = "drug2")) %>%
  mutate(nacrs_visit_per_100 = nacrs_visit/cohort_size * 100,
         line_type = "ETA-Unrelated ED Visits",
         relation = "unrelated",
         nacrs_visit_per_100 = ifelse(is.na(nacrs_visit_per_100), 0, nacrs_visit_per_100))

nacrs_visit_INF_ISD_related = nacrs_drug_bydate %>%
  filter(cohort2 == "ISD") %>%
  filter(drug == "INF" | drug == "INFB") %>%
  filter(related == "ISD") %>%
  group_by(year, month) %>%
  summarize(nacrs_visit = n()) %>%
  right_join(date_range, by = c("year", "month")) %>%
  mutate(nacrs_visit = ifelse(is.na(nacrs_visit), 0, nacrs_visit),
         drug = "INF") %>%
  left_join(drug_bydate_cohort_size_ISD, by = c("year" = "year", "drug" = "drug2")) %>%
  mutate(nacrs_visit_per_100 = nacrs_visit/cohort_size * 100,
         line_type = "INF-Related ED Visits",
         relation = "related",
         nacrs_visit_per_100 = ifelse(is.na(nacrs_visit_per_100), 0, nacrs_visit_per_100))
nacrs_visit_INF_ISD_unrelated = nacrs_drug_bydate %>%
  filter(cohort2 == "ISD") %>%
  filter(drug == "INF" | drug == "INFB") %>%
  filter(is.na(related) | related != "ISD") %>%
  group_by(year, month) %>%
  summarize(nacrs_visit = n()) %>%
  right_join(date_range, by = c("year", "month")) %>%
  mutate(nacrs_visit = ifelse(is.na(nacrs_visit), 0, nacrs_visit),
         drug = "INF") %>%
  left_join(drug_bydate_cohort_size_ISD, by = c("year" = "year", "drug" = "drug2")) %>%
  mutate(nacrs_visit_per_100 = nacrs_visit/cohort_size * 100,
         line_type = "INF-Unrelated ED Visits",
         relation = "unrelated",
         nacrs_visit_per_100 = ifelse(is.na(nacrs_visit_per_100), 0, nacrs_visit_per_100))

ISD_nacrs_combined = rbind(nacrs_visit_ADA_ISD_related, nacrs_visit_ADA_ISD_unrelated, nacrs_visit_ETA_ISD_related, nacrs_visit_ETA_ISD_unrelated, nacrs_visit_INF_ISD_related, nacrs_visit_INF_ISD_unrelated)
ggplot(ISD_nacrs_combined) +
  geom_line(aes(x=as.Date(paste(year,month, "01", sep = "-")), y = nacrs_visit_per_100, color = line_type, linetype = relation)) +
  geom_point(aes(x=as.Date(paste(year,month, "01", sep = "-")), y = nacrs_visit_per_100, color = line_type, shape = relation)) +
  scale_color_manual(values = c("ADA-Related ED Visits" = "red", "ADA-Unrelated ED Visits" = "pink",
                                "ETA-Related ED Visits" = "darkblue", "ETA-Unrelated ED Visits" = "lightblue",
                                "INF-Related ED Visits" = "cornsilk4", "INF-Unrelated ED Visits" = "bisque")) +
  scale_linetype_manual(values = c("related" = "solid", "unrelated" = "dashed")) +
  scale_shape_manual(values = c("related" = "circle", "unrelated" = "triangle")) +
  labs(x = "Date", y = "Monthly Emergency visits per 100") +
  ggtitle("Monthly Emergency Visits per 100 patients in the yearly ISD cohort") +
  geom_vline(xintercept = as.Date(c("2019-05-27", "2019-11-25", "2021-04-07", "2021-10-06"))) +
  ylim(0,15) +
  theme_minimal()

#####################################
########### IBD Cohort ##############
#####################################

nacrs_visit_ADA_IBD_related = nacrs_drug_bydate %>%
  filter(cohort2 == "IBD") %>%
  filter(drug == "ADA" | drug == "ADAB") %>%
  filter(related == "IBD") %>%
  group_by(year, month) %>%
  summarize(nacrs_visit = n()) %>%
  right_join(date_range, by = c("year", "month")) %>%
  mutate(nacrs_visit = ifelse(is.na(nacrs_visit), 0, nacrs_visit),
         drug = "ADA") %>%
  left_join(drug_bydate_cohort_size_IBD, by = c("year" = "year", "drug" = "drug2")) %>%
  mutate(nacrs_visit_per_100 = nacrs_visit/cohort_size * 100,
         line_type = "ADA-Related ED Visits",
         relation = "related",
         nacrs_visit_per_100 = ifelse(is.na(nacrs_visit_per_100), 0, nacrs_visit_per_100))
nacrs_visit_ADA_IBD_unrelated = nacrs_drug_bydate %>%
  filter(cohort2 == "IBD") %>%
  filter(drug == "ADA" | drug == "ADAB") %>%
  filter(is.na(related) | related != "IBD") %>%
  group_by(year, month) %>%
  summarize(nacrs_visit = n()) %>%
  right_join(date_range, by = c("year", "month")) %>%
  mutate(nacrs_visit = ifelse(is.na(nacrs_visit), 0, nacrs_visit),
         drug = "ADA") %>%
  left_join(drug_bydate_cohort_size_IBD, by = c("year" = "year", "drug" = "drug2")) %>%
  mutate(nacrs_visit_per_100 = nacrs_visit/cohort_size * 100,
         line_type = "ADA-Unrelated ED Visits",
         relation = "unrelated",
         nacrs_visit_per_100 = ifelse(is.na(nacrs_visit_per_100), 0, nacrs_visit_per_100))

nacrs_visit_INF_IBD_related = nacrs_drug_bydate %>%
  filter(cohort2 == "IBD") %>%
  filter(drug == "INF" | drug == "INFB") %>%
  filter(related == "IBD") %>%
  group_by(year, month) %>%
  summarize(nacrs_visit = n()) %>%
  right_join(date_range, by = c("year", "month")) %>%
  mutate(nacrs_visit = ifelse(is.na(nacrs_visit), 0, nacrs_visit),
         drug = "INF") %>%
  left_join(drug_bydate_cohort_size_IBD, by = c("year" = "year", "drug" = "drug2")) %>%
  mutate(nacrs_visit_per_100 = nacrs_visit/cohort_size * 100,
         line_type = "INF-Related ED Visits",
         relation = "related",
         nacrs_visit_per_100 = ifelse(is.na(nacrs_visit_per_100), 0, nacrs_visit_per_100))
nacrs_visit_INF_IBD_unrelated = nacrs_drug_bydate %>%
  filter(cohort2 == "IBD") %>%
  filter(drug == "INF" | drug == "INFB") %>%
  filter(is.na(related) | related != "IBD") %>%
  group_by(year, month) %>%
  summarize(nacrs_visit = n()) %>%
  right_join(date_range, by = c("year", "month")) %>%
  mutate(nacrs_visit = ifelse(is.na(nacrs_visit), 0, nacrs_visit),
         drug = "INF") %>%
  left_join(drug_bydate_cohort_size_IBD, by = c("year" = "year", "drug" = "drug2")) %>%
  mutate(nacrs_visit_per_100 = nacrs_visit/cohort_size * 100,
         line_type = "INF-Unrelated ED Visits",
         relation = "unrelated",
         nacrs_visit_per_100 = ifelse(is.na(nacrs_visit_per_100), 0, nacrs_visit_per_100))

IBD_nacrs_combined = rbind(nacrs_visit_ADA_IBD_related, nacrs_visit_ADA_IBD_unrelated,nacrs_visit_INF_IBD_related, nacrs_visit_INF_IBD_unrelated)
ggplot(IBD_nacrs_combined) +
  geom_line(aes(x=as.Date(paste(year,month, "01", sep = "-")), y = nacrs_visit_per_100, color = line_type, linetype = relation)) +
  geom_point(aes(x=as.Date(paste(year,month, "01", sep = "-")), y = nacrs_visit_per_100, color = line_type, shape = relation)) +
  scale_color_manual(values = c("ADA-Related ED Visits" = "red", "ADA-Unrelated ED Visits" = "pink",
                                "ETA-Related ED Visits" = "darkblue", "ETA-Unrelated ED Visits" = "lightblue",
                                "INF-Related ED Visits" = "cornsilk4", "INF-Unrelated ED Visits" = "bisque")) +
  scale_linetype_manual(values = c("related" = "solid", "unrelated" = "dashed")) +
  scale_shape_manual(values = c("related" = "circle", "unrelated" = "triangle")) +
  labs(x = "Date", y = "Monthly Emergency visits per 100") +
  ggtitle("Monthly Emergency Visits per 100 patients in the yearly IBD cohort") +
  geom_vline(xintercept = as.Date(c("2019-09-05", "2020-03-05", "2021-04-07", "2021-10-06"))) +
  ylim(0,15) +
  theme_minimal()

# count check 
count_check_ed_visit = nacrs_drug_bydate %>%
  mutate(drug2 = case_when(drug == "ADA" | drug == "ADAB" ~ "ADA",
                           drug == "ETA" | drug == "ETAB" ~ "ETA",
                           drug == "INF" | drug == "INFB" ~ "INF",
                           drug == "GOL" | drug == "CER" ~ "Others")) %>%
  group_by(year, month, cohort2, drug2, related) %>%
  summarize(cohort_size = n_distinct(STUDYID)) 
#compare with drug_bydate_cohort_size4
