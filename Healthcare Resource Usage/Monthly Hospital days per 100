
dad = read.csv("R:/working/hdang_AnalysisFiles/Input/dad_related.csv", stringsAsFactors=FALSE)
dad$ADDATE = as.Date(dad$ADDATE) 
dad$fy_ad = ifelse(month(dad$ADDATE) <= 3, year(dad$ADDATE) - 1, year(dad$ADDATE))
dad = dad %>% filter(ADDATE >= "2015-01-01")

########################
## 1. Creating cohort ##
########################

## 1a. Hospital days ##

genedays_day = function (start_date, num_days) {
  seq(start_date, by = "days", length.out = num_days)
}

dad2 = dad %>%
  group_by(STUDYID,ADDATE, TDAYS) %>%
  mutate( 
    TDAYS_2 = ifelse(TDAYS == 0, 1, TDAYS), #because the function skip observation if TDAYS = 0
    date = pmap(list(ADDATE, TDAYS_2), genedays_day) #create a list of days
  ) %>% 
  unnest(date) %>% #populate date list into rows
  mutate(
    date = as.Date(date, "%Y-%m-%d"),
    fy = ifelse(month(date) <= 3, year(date) - 1, year(date))
  )

dad_drug_bydate = dad2 %>%
  left_join(drug_bydate_cohort, by = c("STUDYID" = "STUDYID", "date" = "rx_date")) %>%
  select("STUDYID", "ADDATE", "SEPDATE", "date",  "TDAYS", "TDAYS_2",
       "cohort", "cohort2", "dx", "drug", "RA", "PA", "AS", "CD", "UC", "biosimilar",
         "ADMIT", "EDREGDATE", "ENTRY", "LEFTERDT", "SEPDISP", "fy", ) %>%
  mutate(year = year(date),
         month = month(date),
         related = case_when(RA == 1 | AS == 1 ~ "IJD",
                             PA == 1 ~ "ISD",
                             (CD == 1 | UC == 1 ~ "IBD")))

date_range = expand.grid(year = 2015:2022,
                         month = 1:12)
##########################################
################ IAD #####################
##########################################

dad_days_ADA_IJD_related = dad_drug_bydate %>%
  filter(cohort2 == "IJD") %>%
  filter(drug == "ADA" | drug == "ADAB") %>%
  filter(related == "IJD") %>%
  group_by(year, month) %>%
  summarize(dad_days = n()) %>%
  right_join(date_range, by = c("year", "month")) %>%
  mutate(dad_days = ifelse(is.na(dad_days), 0, dad_days),
         drug = "ADA") %>%
  left_join(drug_bydate_cohort_size_IJD, by = c("year" = "year", "drug" = "drug2")) %>%
  mutate(dad_days_per_100 = dad_days/cohort_size * 100,
         line_type = "ADA-Related Hospitalization",
         relation = "related",
         dad_days_per_100 = ifelse(is.na(dad_days_per_100), 0, dad_days_per_100))
dad_days_ADA_IJD_unrelated = dad_drug_bydate %>%
  filter(cohort2 == "IJD") %>%
  filter(drug == "ADA" | drug == "ADAB") %>%
  filter(is.na(related) | related != "IJD") %>%
  group_by(year, month) %>%
  summarize(dad_days = n()) %>%
  right_join(date_range, by = c("year", "month")) %>%
  mutate(dad_days = ifelse(is.na(dad_days), 0, dad_days),
         drug = "ADA") %>%
  left_join(drug_bydate_cohort_size_IJD, by = c("year" = "year", "drug" = "drug2")) %>%
  mutate(dad_days_per_100 = dad_days/cohort_size * 100,
         line_type = "ADA-Unrelated Hospitalization",
         relation = "unrelated",
         dad_days_per_100 = ifelse(is.na(dad_days_per_100), 0, dad_days_per_100))

dad_days_ADA_IJD_related<-dad_days_ADA_IJD_related[with(dad_days_ADA_IJD_related,order(year,month)),]
dad_days_ADA_IJD_unrelated<-dad_days_ADA_IJD_unrelated[with(dad_days_ADA_IJD_unrelated,order(year,month)),]


dad_days_ETA_IJD_related = dad_drug_bydate %>%
  filter(cohort2 == "IJD") %>%
  filter(drug == "ETA" | drug == "ETAB") %>%
  filter(related == "IJD") %>%
  group_by(year, month) %>%
  summarize(dad_days = n()) %>%
  right_join(date_range, by = c("year", "month")) %>%
  mutate(dad_days = ifelse(is.na(dad_days), 0, dad_days),
         drug = "ETA") %>%
  left_join(drug_bydate_cohort_size_IJD, by = c("year" = "year", "drug" = "drug2")) %>%
  mutate(dad_days_per_100 = dad_days/cohort_size * 100,
         line_type = "ETA-Related Hospitalization",
         relation = "related",
         dad_days_per_100 = ifelse(is.na(dad_days_per_100), 0, dad_days_per_100))
dad_days_ETA_IJD_unrelated = dad_drug_bydate %>%
  filter(cohort2 == "IJD") %>%
  filter(drug == "ETA" | drug == "ETAB") %>%
  filter(is.na(related) | related != "IJD") %>%
  group_by(year, month) %>%
  summarize(dad_days = n()) %>%
  right_join(date_range, by = c("year", "month")) %>%
  mutate(dad_days = ifelse(is.na(dad_days), 0, dad_days),
         drug = "ETA") %>%
  left_join(drug_bydate_cohort_size_IJD, by = c("year" = "year", "drug" = "drug2")) %>%
  mutate(dad_days_per_100 = dad_days/cohort_size * 100,
         line_type = "ETA-Unrelated Hospitalization",
         relation = "unrelated",
         dad_days_per_100 = ifelse(is.na(dad_days_per_100), 0, dad_days_per_100))

dad_days_INF_IJD_related = dad_drug_bydate %>%
  filter(cohort2 == "IJD") %>%
  filter(drug == "INF" | drug == "INFB") %>%
  filter(related == "IJD") %>%
  group_by(year, month) %>%
  summarize(dad_days = n()) %>%
  right_join(date_range, by = c("year", "month")) %>%
  mutate(dad_days = ifelse(is.na(dad_days), 0, dad_days),
         drug = "INF") %>%
  left_join(drug_bydate_cohort_size_IJD, by = c("year" = "year", "drug" = "drug2")) %>%
  mutate(dad_days_per_100 = dad_days/cohort_size * 100,
         line_type = "INF-Related Hospitalization",
         relation = "related",
         dad_days_per_100 = ifelse(is.na(dad_days_per_100), 0, dad_days_per_100))
dad_days_INF_IJD_unrelated = dad_drug_bydate %>%
  filter(cohort2 == "IJD") %>%
  filter(drug == "INF" | drug == "INFB") %>%
  filter(is.na(related) | related != "IJD") %>%
  group_by(year, month) %>%
  summarize(dad_days = n()) %>%
  right_join(date_range, by = c("year", "month")) %>%
  mutate(dad_days = ifelse(is.na(dad_days), 0, dad_days),
         drug = "INF") %>%
  left_join(drug_bydate_cohort_size_IJD, by = c("year" = "year", "drug" = "drug2")) %>%
  mutate(dad_days_per_100 = dad_days/cohort_size * 100,
         line_type = "INF-Unrelated Hospitalization",
         relation = "unrelated",
         dad_days_per_100 = ifelse(is.na(dad_days_per_100), 0, dad_days_per_100))


dad_days_ETAINF_IJD_related = rbind(dad_days_ETA_IJD_related,dad_days_INF_IJD_related)
dad_days_ETAINF_IJD_related1 = dad_days_ETAINF_IJD_related %>%
  group_by(year, month) %>%
  sort_by(year, month) %>%
  summarize(dad_days1 = sum(dad_days),cohort_size1=sum(cohort_size)) %>%
  mutate(dad_days = dad_days1/cohort_size1,dad_days_per_100 = dad_days1/cohort_size1*100,
         line_type = "ETAINF-related",
         relation = "related")

dad_days_ETAINF_IJD_unrelated = rbind(dad_days_ETA_IJD_unrelated,dad_days_INF_IJD_unrelated)
dad_days_ETAINF_IJD_unrelated1 = dad_days_ETAINF_IJD_unrelated %>%
  group_by(year, month) %>%
  summarize(dad_days1 = sum(dad_days),cohort_size1=sum(cohort_size)) %>%
  mutate(dad_days = dad_days1/cohort_size1, dad_days_per_100 = dad_days1/cohort_size1*100,
         line_type = "ETAINF-Unrelated",
         relation = "unrelated")

dad_days_ETAINF_IJD_related<-dad_days_ETAINF_IJD_related[with(dad_days_ETAINF_IJD_related,order(year,month)),]
dad_days_ETAINF_IJD_unrelated<-dad_days_ETAINF_IJD_unrelated[with(dad_days_ETAINF_IJD_unrelated,order(year,month)),]



dad_days_ETAINF_IJD_related = dad_drug_bydate %>%
  filter(cohort2 == "IJD") %>%
  filter(drug == "ETA" | drug == "ETAB" | drug == "INF" | drug == "INFB") %>%
  filter(related == "IJD") %>%
  group_by(year, month) %>%
  summarize(dad_days = n()) %>%
  right_join(date_range, by = c("year", "month")) %>%
  mutate(dad_days = ifelse(is.na(dad_days), 0, dad_days),
         drug = "ETA") %>%
  left_join(drug_bydate_cohort_size_IJD, by = c("year" = "year", "drug" = "drug2")) %>%
  mutate(dad_days_per_100 = dad_days/cohort_size * 100,
         line_type = "ETA-Related Hospitalization",
         relation = "related",
         dad_days_per_100 = ifelse(is.na(dad_days_per_100), 0, dad_days_per_100))
dad_days_ETAINF_IJD_unrelated = dad_drug_bydate %>%
  filter(cohort2 == "IJD") %>%
  filter(drug == "ETA" | drug == "ETAB" | drug == "INF" | drug == "INFB") %>%
  filter(is.na(related) | related != "IJD") %>%
  group_by(year, month) %>%
  summarize(dad_days = n()) %>%
  right_join(date_range, by = c("year", "month")) %>%
  mutate(dad_days = ifelse(is.na(dad_days), 0, dad_days),
         drug = "ETA") %>%
  left_join(drug_bydate_cohort_size_IJD, by = c("year" = "year", "drug" = "drug2")) %>%
  mutate(dad_days_per_100 = dad_days/cohort_size * 100,
         line_type = "ETA-Unrelated Hospitalization",
         relation = "unrelated",
         dad_days_per_100 = ifelse(is.na(dad_days_per_100), 0, dad_days_per_100))


IJD_dad_combined = rbind(dad_days_ADA_IJD_related, dad_days_ADA_IJD_unrelated, dad_days_ETA_IJD_related, dad_days_ETA_IJD_unrelated, dad_days_INF_IJD_related, dad_days_INF_IJD_unrelated)
ggplot(IJD_dad_combined) +
  geom_line(aes(x=as.Date(paste(year,month, "01", sep = "-")), y = dad_days_per_100, color = line_type, linetype = relation)) +
  geom_point(aes(x=as.Date(paste(year,month, "01", sep = "-")), y = dad_days_per_100, color = line_type, shape = relation)) +
  scale_color_manual(values = c("ADA-Related Hospitalization" = "red", "ADA-Unrelated Hospitalization" = "pink",
                                "ETA-Related Hospitalization" = "darkblue", "ETA-Unrelated Hospitalization" = "lightblue",
                                "INF-Related Hospitalization" = "cornsilk4", "INF-Unrelated Hospitalization" = "bisque")) +
  scale_linetype_manual(values = c("related" = "solid", "unrelated" = "dashed")) +
  scale_shape_manual(values = c("related" = "circle", "unrelated" = "triangle")) +
  labs(x = "Date", y = "Monthly Hospital Days per 100") +
  ggtitle("Monthly Hospital Days per 100 patients in the yearly IJD cohort") +
  geom_vline(xintercept = as.Date(c("2019-05-27", "2019-11-25", "2021-04-07", "2021-10-06"))) +
  ylim(0,16) +
  theme_minimal()

##########################################
################ ISD #####################
##########################################

dad_days_ADA_ISD_related = dad_drug_bydate %>%
  filter(cohort2 == "ISD") %>%
  filter(drug == "ADA" | drug == "ADAB") %>%
  filter(related == "ISD") %>%
  group_by(year, month) %>%
  summarize(dad_days = n()) %>%
  right_join(date_range, by = c("year", "month")) %>%
  mutate(dad_days = ifelse(is.na(dad_days), 0, dad_days),
         drug = "ADA") %>%
  left_join(drug_bydate_cohort_size_ISD, by = c("year" = "year", "drug" = "drug2")) %>%
  mutate(dad_days_per_100 = dad_days/cohort_size * 100,
         line_type = "ADA-Related Hospitalization",
         relation = "related",
         dad_days_per_100 = ifelse(is.na(dad_days_per_100), 0, dad_days_per_100))

dad_days_ADA_ISD_unrelated = dad_drug_bydate %>%
  filter(cohort2 == "ISD") %>%
  filter(drug == "ADA" | drug == "ADAB") %>%
  filter(is.na(related) | related != "ISD") %>%
  group_by(year, month) %>%
  summarize(dad_days = n()) %>%
  right_join(date_range, by = c("year", "month")) %>%
  mutate(dad_days = ifelse(is.na(dad_days), 0, dad_days),
         drug = "ADA") %>%
  left_join(drug_bydate_cohort_size_ISD, by = c("year" = "year", "drug" = "drug2")) %>%  mutate(dad_days_per_100 = dad_days/cohort_size * 100,
         line_type = "ADA-Unrelated Hospitalization",
         relation = "unrelated",
         dad_days_per_100 = ifelse(is.na(dad_days_per_100), 0, dad_days_per_100))

dad_days_ETA_ISD_related = dad_drug_bydate %>%
  filter(cohort2 == "ISD") %>%
  filter(drug == "ETA" | drug == "ETAB") %>%
  filter(related == "ISD") %>%
  group_by(year, month) %>%
  summarize(dad_days = n()) %>%
  right_join(date_range, by = c("year", "month")) %>%
  mutate(dad_days = ifelse(is.na(dad_days), 0, dad_days),
         drug = "ETA") %>%
  left_join(drug_bydate_cohort_size_ISD, by = c("year" = "year", "drug" = "drug2")) %>%  mutate(dad_days_per_100 = dad_days/cohort_size * 100,
         line_type = "ETA-Related Hospitalization",
         relation = "related",
         dad_days_per_100 = ifelse(is.na(dad_days_per_100), 0, dad_days_per_100))
dad_days_ETA_ISD_unrelated = dad_drug_bydate %>%
  filter(cohort2 == "ISD") %>%
  filter(drug == "ETA" | drug == "ETAB") %>%
  filter(is.na(related) | related != "ISD") %>%
  group_by(year, month) %>%
  summarize(dad_days = n()) %>%
  right_join(date_range, by = c("year", "month")) %>%
  mutate(dad_days = ifelse(is.na(dad_days), 0, dad_days),
         drug = "ETA") %>%
  left_join(drug_bydate_cohort_size_ISD, by = c("year" = "year", "drug" = "drug2")) %>%  mutate(dad_days_per_100 = dad_days/cohort_size * 100,
         line_type = "ETA-Unrelated Hospitalization",
         relation = "unrelated",
         dad_days_per_100 = ifelse(is.na(dad_days_per_100), 0, dad_days_per_100))

dad_days_INF_ISD_related = dad_drug_bydate %>%
  filter(cohort2 == "ISD") %>%
  filter(drug == "INF" | drug == "INFB") %>%
  filter(related == "ISD") %>%
  group_by(year, month) %>%
  summarize(dad_days = n()) %>%
  right_join(date_range, by = c("year", "month")) %>%
  mutate(dad_days = ifelse(is.na(dad_days), 0, dad_days),
         drug = "INF") %>%
  left_join(drug_bydate_cohort_size_ISD, by = c("year" = "year", "drug" = "drug2")) %>%  mutate(dad_days_per_100 = dad_days/cohort_size * 100,
         line_type = "INF-Related Hospitalization",
         relation = "related",
         dad_days_per_100 = ifelse(is.na(dad_days_per_100), 0, dad_days_per_100))
dad_days_INF_ISD_unrelated = dad_drug_bydate %>%
  filter(cohort2 == "ISD") %>%
  filter(drug == "INF" | drug == "INFB") %>%
  filter(is.na(related) | related != "ISD") %>%
  group_by(year, month) %>%
  summarize(dad_days = n()) %>%
  right_join(date_range, by = c("year", "month")) %>%
  mutate(dad_days = ifelse(is.na(dad_days), 0, dad_days),
         drug = "INF") %>%
  left_join(drug_bydate_cohort_size_ISD, by = c("year" = "year", "drug" = "drug2")) %>%  mutate(dad_days_per_100 = dad_days/cohort_size * 100,
         line_type = "INF-Unrelated Hospitalization",
         relation = "unrelated",
         dad_days_per_100 = ifelse(is.na(dad_days_per_100), 0, dad_days_per_100))

ISD_dad_combined = rbind(dad_days_ADA_ISD_related, dad_days_ADA_ISD_unrelated, dad_days_ETA_ISD_related, dad_days_ETA_ISD_unrelated, dad_days_INF_ISD_related, dad_days_INF_ISD_unrelated)
ggplot(ISD_dad_combined) +
  geom_line(aes(x=as.Date(paste(year,month, "01", sep = "-")), y = dad_days_per_100, color = line_type, linetype = relation)) +
  geom_point(aes(x=as.Date(paste(year,month, "01", sep = "-")), y = dad_days_per_100, color = line_type, shape = relation)) +
  scale_color_manual(values = c("ADA-Related Hospitalization" = "red", "ADA-Unrelated Hospitalization" = "pink",
                                "ETA-Related Hospitalization" = "darkblue", "ETA-Unrelated Hospitalization" = "lightblue",
                                "INF-Related Hospitalization" = "cornsilk4", "INF-Unrelated Hospitalization" = "bisque")) +
  scale_linetype_manual(values = c("related" = "solid", "unrelated" = "dashed")) +
  scale_shape_manual(values = c("related" = "circle", "unrelated" = "triangle")) +
  labs(x = "Date", y = "Monthly Hospital Days per 100") +
  ggtitle("Monthly Hospital Days per 100 patients in the yearly ISD cohort") +
  geom_vline(xintercept = as.Date(c("2019-05-27", "2019-11-25", "2021-04-07", "2021-10-06"))) +
  ylim(0,60) +
  theme_minimal()

##########################################
################ IBD #####################
##########################################

dad_days_ADA_IBD_related = dad_drug_bydate %>%
  filter(cohort2 == "IBD") %>%
  filter(drug == "ADA" | drug == "ADAB") %>%
  filter(related == "IBD") %>%
  group_by(year, month) %>%
  summarize(dad_days = n()) %>%
  right_join(date_range, by = c("year", "month")) %>%
  mutate(dad_days = ifelse(is.na(dad_days), 0, dad_days),
         drug = "ADA") %>%
  left_join(drug_bydate_cohort_size_IBD, by = c("year" = "year", "drug" = "drug2")) %>%  
  mutate(dad_days_per_100 = dad_days/cohort_size * 100,
         line_type = "ADA-Related Hospitalization",
         relation = "related",
         dad_days_per_100 = ifelse(is.na(dad_days_per_100), 0, dad_days_per_100))
dad_days_ADA_IBD_unrelated = dad_drug_bydate %>%
  filter(cohort2 == "IBD") %>%
  filter(drug == "ADA" | drug == "ADAB") %>%
  filter(is.na(related) | related != "IBD") %>%
  group_by(year, month) %>%
  summarize(dad_days = n()) %>%
  right_join(date_range, by = c("year", "month")) %>%
  mutate(dad_days = ifelse(is.na(dad_days), 0, dad_days),
         drug = "ADA") %>%
  left_join(drug_bydate_cohort_size_IBD, by = c("year" = "year", "drug" = "drug2")) %>% 
  mutate(dad_days_per_100 = dad_days/cohort_size * 100,
         line_type = "ADA-Unrelated Hospitalization",
         relation = "unrelated",
         dad_days_per_100 = ifelse(is.na(dad_days_per_100), 0, dad_days_per_100))

dad_days_INF_IBD_related = dad_drug_bydate %>%
  filter(cohort2 == "IBD") %>%
  filter(drug == "INF" | drug == "INFB") %>%
  filter(related == "IBD") %>%
  group_by(year, month) %>%
  summarize(dad_days = n()) %>%
  right_join(date_range, by = c("year", "month")) %>%
  mutate(dad_days = ifelse(is.na(dad_days), 0, dad_days),
         drug = "INF") %>%
  left_join(drug_bydate_cohort_size_IBD, by = c("year" = "year", "drug" = "drug2")) %>%   mutate(dad_days_per_100 = dad_days/cohort_size * 100,
         line_type = "INF-Related Hospitalization",
         relation = "related",
         dad_days_per_100 = ifelse(is.na(dad_days_per_100), 0, dad_days_per_100))
dad_days_INF_IBD_unrelated = dad_drug_bydate %>%
  filter(cohort2 == "IBD") %>%
  filter(drug == "INF" | drug == "INFB") %>%
  filter(is.na(related) | related != "IBD") %>%
  group_by(year, month) %>%
  summarize(dad_days = n()) %>%
  right_join(date_range, by = c("year", "month")) %>%
  mutate(dad_days = ifelse(is.na(dad_days), 0, dad_days),
         drug = "INF") %>%
  left_join(drug_bydate_cohort_size_IBD, by = c("year" = "year", "drug" = "drug2")) %>%   mutate(dad_days_per_100 = dad_days/cohort_size * 100,
         line_type = "INF-Unrelated Hospitalization",
         relation = "unrelated",
         dad_days_per_100 = ifelse(is.na(dad_days_per_100), 0, dad_days_per_100))

IBD_dad_combined = rbind(dad_days_ADA_IBD_related, dad_days_ADA_IBD_unrelated, dad_days_INF_IBD_related, dad_days_INF_IBD_unrelated)

tiff("R:/working/hdang_AnalysisFiles/R_Code/Official Codes/Output/dad_visitsbydrug.tiff",width=6.5,height=4,units="in",res=300)

ggplot(IBD_dad_combined) +
  geom_line(aes(x=as.Date(paste(year,month, "01", sep = "-")), y = dad_days_per_100, color = line_type, linetype = relation)) +
  geom_point(aes(x=as.Date(paste(year,month, "01", sep = "-")), y = dad_days_per_100, color = line_type, shape = relation)) +
  scale_color_manual(values = c("ADA-Related Hospitalization" = "red", "ADA-Unrelated Hospitalization" = "pink",
                                "ETA-Related Hospitalization" = "darkblue", "ETA-Unrelated Hospitalization" = "lightblue",
                                "INF-Related Hospitalization" = "cornsilk4", "INF-Unrelated Hospitalization" = "bisque")) +
  scale_linetype_manual(values = c("related" = "solid", "unrelated" = "dashed")) +
  scale_shape_manual(values = c("related" = "circle", "unrelated" = "triangle")) +
  labs(x = "Date", y = "Monthly Hospital Days per 100") +
  ggtitle("Monthly Hospital Days per 100 patients in the yearly IBD cohort") +
  geom_vline(xintercept = as.Date(c("2019-09-05", "2020-03-05", "2021-04-07", "2021-10-06"))) +
  ylim(0,16) +
  theme_minimal()

dev.off()
#############################################
drug_bydate_cohort_size_IBD2 = drug_bydate_cohort_size_IBD %>%
  filter(drug2 != "OTHER") %>%
  group_by(year) %>%
  summarize(cohort_size_yearly = sum(cohort_size))
dad_days_IBD = dad_drug_bydate %>%
  filter(cohort2 == "IBD") %>%
  filter(is.na(related) | related != "IBD") %>%
  group_by(year, month) %>%
  summarize(dad_days = n()) %>%
  right_join(date_range, by = c("year", "month")) %>%
  mutate(dad_days = ifelse(is.na(dad_days), 0, dad_days)) %>%
  left_join(drug_bydate_cohort_size_IBD2, by = c("year" = "year")) %>%
    mutate(dad_days_per_100 = dad_days/cohort_size_yearly * 100,
           dad_days_per_100 = ifelse(is.na(dad_days_per_100), 0, dad_days_per_100))



dad_days_IBD = dad_drug_bydate %>%
  filter(cohort2 == "IBD") %>%
  group_by(year, month) %>%
  summarize(dad_days = n()) %>%
  right_join(date_range, by = c("year", "month")) %>%
  mutate(dad_days = ifelse(is.na(dad_days), 0, dad_days)) %>%
  left_join(drug_bydate_cohort_size_IBD2, by = c("year" = "year")) %>%
  mutate(dad_days_per_100 = dad_days/cohort_size_yearly * 100,
         dad_days_per_100 = ifelse(is.na(dad_days_per_100), 0, dad_days_per_100))


tiff("R:/working/hdang_AnalysisFiles/R_Code/Official Codes/Output/dad_visits.tiff",width=6.5,height=4,units="in",res=300)

ggplot(dad_days_IBD) +
  geom_line(aes(x=as.Date(paste(year,month, "01", sep = "-")), y = dad_days_per_100)) +
  geom_point(aes(x=as.Date(paste(year,month, "01", sep = "-")), y = dad_days_per_100)) +
  labs(x = "Date", y = "Monthly Hospital Days per 100") +
  ggtitle("Monthly Hospital Days per 100 patients in the yearly IBD cohort") +
  geom_vline(xintercept = as.Date(c("2019-09-05", "2020-03-05", "2021-04-07", "2021-10-06"))) +
  ylim(0,16) +
  theme_minimal()

dev.off()


# count check 
count_check_dad_days = dad_drug_bydate %>%
  mutate(drug2 = case_when(drug == "ADA" | drug == "ADAB" ~ "ADA",
                           drug == "ETA" | drug == "ETAB" ~ "ETA",
                           drug == "INF" | drug == "INFB" ~ "INF",
                           drug == "GOL" | drug == "CER" ~ "Others")) %>%
  group_by(year, month, cohort2, drug2, related) %>%
  summarize(cohort_size = n_distinct(STUDYID)) 
#compare with drug_bydate_cohort_size4
