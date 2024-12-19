library("dplyr")
library("tidyr")
library("lubridate")
library("ggplot2")

drug_bydate_cohort_size4 = drug_bydate_cohort %>% 
  filter(!is.na(cohort2)) %>%
  group_by(STUDYID, year, month) %>%
  slice(1) %>% 
  
drug_bydate_cohort_size4_biosim = drug_bydate_cohort_size4 %>%
  group_by(year, month, cohort2) %>%
  summarize(cohort_size = n(),count_biosim=sum(biosimilar))

drug_bydate_cohort_size4_biosim$percent_bio<-100*(drug_bydate_cohort_size4_biosim$count_biosim/drug_bydate_cohort_size4_biosim$cohort_size)

IBD_biosim_its<-drug_bydate_cohort_size4_biosim[drug_bydate_cohort_size4_biosim$cohort2=="IBD",]



drug_bydate_cohort_size4_plot = drug_bydate_cohort_size4 %>%
  group_by(year, month, cohort2, drug) %>%
  summarize(cohort_size = n())

ggplot(drug_bydate_cohort_size4_plot, aes(x = as.Date(paste(year,month, "01", sep = "-")), y = cohort_size)) +
  geom_line(aes(linetype = cohort2, color = drug)) +
  geom_point(aes(shape = cohort2, color = drug)) +
  geom_text(aes(label = cohort_size, vjust = -0.5, hjust = 1)) +
  ggtitle("Drug cohort size over year") +
  labs(x = "Year", y = "Yearly cohort size") +
  ylim(0,2500) +
  theme_minimal()

drug_bydate_cohort_size5 = drug_bydate_cohort %>% 
  filter(!is.na(cohort2)) %>%
  mutate(drug2 = case_when(drug == "ADA" | drug == "ADAB" ~ "ADA",
                           drug == "ETA" | drug == "ETAB" ~ "ETA",
                           drug == "INF" | drug == "INFB" ~ "INF",
                           drug == "CER" | drug == "GOL" ~ "OTHER")) %>%
  filter(drug2 != "OTHER") %>%
  group_by(STUDYID, year) %>%
  slice(1) %>% 
  ungroup()

drug_bydate_uptake_both = drug_bydate_cohort %>%
  group_by(year, month, STUDYID) %>%
  filter(n_distinct(drug) > 1) %>%
  mutate(both = 1) %>%
  ungroup()

drug_bydate_uptake = drug_bydate_cohort %>%
  group_by(year, month, cohort2, STUDYID) %>%
  slice(1) %>%
  mutate(drug_count = n_distinct(drug)) %>%
  ungroup() %>%
  mutate(drug_weight = ifelse(drug_count > 1, 0.5, 1)) %>%
  group_by(year, month, cohort2, drug) %>%
  mutate(cohort_size = sum(drug_weight))

drug_bydate_uptake_table = drug_bydate_uptake %>% # for crude cost saving estimation table
  group_by(year, cohort2, drug) %>%
  slice(1) %>%
  filter(year >= 2018, drug != "GOL", drug != "CER")

write.csv(drug_bydate_uptake_table, file = "R:/working/hdang_AnalysisFiles/R_Code/Official Codes/Output/biosimilar_uptake_output.csv")

drug_bydate_uptake_IBD = drug_bydate_uptake %>%
  filter(cohort2 == "IBD") %>%
  filter(drug != "CER" & drug != "GOL")

drug_bydate_uptake_IBD_sum = drug_bydate_uptake_IBD %>%
  group_by(year, month, cohort2, drug) %>%
  summarize(cohort_size = n())
drug_bydate_uptake_IBD_sum


drug_bydate_uptake_IJD = drug_bydate_uptake %>%
  filter(cohort2 == "IJD") %>%
  filter(drug != "CER" & drug != "GOL")


drug_bydate_uptake_IJD_sum = drug_bydate_uptake_IJD %>%
  group_by(year, month, cohort2, drug) %>%
  summarize(cohort_size = n())
drug_bydate_uptake_IJD_sum


drug_bydate_uptake_IJD_year_drug= drug_bydate_uptake_IJD %>%
  group_by(STUDYID) %>%
  group_by(year, cohort2, drug) %>%
  summarize(cohort_size = n())
drug_bydate_uptake_IJD_year_drug


cbind(drug_bydate_uptake_IJD_sum$year[drug_bydate_uptake_IJD_sum$year==2019 & drug_bydate_uptake_IJD_sum$month==5],
      drug_bydate_uptake_IJD_sum$month[drug_bydate_uptake_IJD_sum$year==2019 & drug_bydate_uptake_IJD_sum$month==5],
      drug_bydate_uptake_IJD_sum$cohort_size[drug_bydate_uptake_IJD_sum$year==2019 & drug_bydate_uptake_IJD_sum$month==5],
      drug_bydate_uptake_IJD_sum$drug[drug_bydate_uptake_IJD_sum$year==2019 & drug_bydate_uptake_IJD_sum$month==5])

cbind(drug_bydate_uptake_IJD_sum$year[drug_bydate_uptake_IJD_sum$year==2020 & drug_bydate_uptake_IJD_sum$month==1],
      drug_bydate_uptake_IJD_sum$month[drug_bydate_uptake_IJD_sum$year==2020 & drug_bydate_uptake_IJD_sum$month==1],
      drug_bydate_uptake_IJD_sum$cohort_size[drug_bydate_uptake_IJD_sum$year==2020 & drug_bydate_uptake_IJD_sum$month==1],
      drug_bydate_uptake_IJD_sum$drug[drug_bydate_uptake_IJD_sum$year==2020 & drug_bydate_uptake_IJD_sum$month==1])

cbind(drug_bydate_uptake_IJD_sum$year[drug_bydate_uptake_IJD_sum$year==2021 & drug_bydate_uptake_IJD_sum$month==3],
      drug_bydate_uptake_IJD_sum$month[drug_bydate_uptake_IJD_sum$year==2021 & drug_bydate_uptake_IJD_sum$month==3],
      drug_bydate_uptake_IJD_sum$cohort_size[drug_bydate_uptake_IJD_sum$year==2021 & drug_bydate_uptake_IJD_sum$month==3],
      drug_bydate_uptake_IJD_sum$drug[drug_bydate_uptake_IJD_sum$year==2021 & drug_bydate_uptake_IJD_sum$month==3])

cbind(drug_bydate_uptake_IJD_sum$year[drug_bydate_uptake_IJD_sum$year==2021 & drug_bydate_uptake_IJD_sum$month==11],
      drug_bydate_uptake_IJD_sum$month[drug_bydate_uptake_IJD_sum$year==2021 & drug_bydate_uptake_IJD_sum$month==11],
      drug_bydate_uptake_IJD_sum$cohort_size[drug_bydate_uptake_IJD_sum$year==2021 & drug_bydate_uptake_IJD_sum$month==11],
      drug_bydate_uptake_IJD_sum$drug[drug_bydate_uptake_IJD_sum$year==2021 & drug_bydate_uptake_IJD_sum$month==11])


drug_bydate_uptake_ISD = drug_bydate_uptake %>%
  filter(cohort2 == "ISD") %>%
  filter(drug != "CER" & drug != "GOL")



ggplot(drug_bydate_uptake_IBD_sum, aes(x = as.Date(paste(year, month, "01", sep = "-")), y = cohort_size, fill = drug)) +
  geom_area() + 
  scale_fill_manual(values = c("ADA" = "red", "ADAB" = "pink",
                               "ETA" = "darkblue", "ETAB" = "lightblue",
                               "INF" = "cornsilk4", "INFB" = "bisque")) +
  labs(x = "Year", y = "Yearly cohort size") +
  ggtitle("Biosimilar uptake of IBD cohort") +
  geom_vline(xintercept = as.Date(c("2019-09-05", "2020-03-05", "2021-04-07", "2021-10-06"))) +
  ylim(0,4500) +
  theme_minimal()

ggplot(drug_bydate_uptake_IJD_sum, aes(x = as.Date(paste(year, month, "01", sep = "-")), y = cohort_size, fill = drug)) +
  geom_area() + 
  scale_fill_manual(values = c("ADA" = "red", "ADAB" = "pink",
                               "ETA" = "darkblue", "ETAB" = "lightblue",
                               "INF" = "cornsilk4", "INFB" = "bisque")) +
  labs(x = "Year", y = "Yearly cohort size") +
  ggtitle("Biosimilar uptake of IJD cohort") +
  geom_vline(xintercept = as.Date(c("2019-09-05", "2020-03-05", "2021-04-07", "2021-10-06"))) +
  ylim(0,4500) +
  theme_minimal()





ggplot(drug_bydate_uptake_IJD, aes(x = as.Date(paste(year, month, "01", sep = "-")), y = cohort_size, fill = drug)) +
  geom_area() + 
  scale_fill_manual(values = c("ADA" = "red", "ADAB" = "pink",
                                "ETA" = "darkblue", "ETAB" = "lightblue",
                                "INF" = "cornsilk4", "INFB" = "bisque")) +
  labs(x = "Year", y = "Yearly cohort size") +
  ggtitle("Biosimilar uptake of IJD cohort") +
  geom_vline(xintercept = as.Date(c("2019-05-27", "2019-11-25", "2021-04-07", "2021-10-06"))) +
  ylim(0,4500) +
  theme_minimal()

ggplot(drug_bydate_uptake_ISD, aes(x = as.Date(paste(year, month, "01", sep = "-")), y = cohort_size, fill = drug)) +
  geom_area() + 
  scale_fill_manual(values = c("ADA" = "red", "ADAB" = "pink",
                               "ETA" = "darkblue", "ETAB" = "lightblue",
                               "INF" = "cornsilk4", "INFB" = "bisque")) +
  labs(x = "Year", y = "Yearly cohort size") +
  ggtitle("Biosimilar uptake of ISD cohort") +
  geom_vline(xintercept = as.Date(c("2019-05-27", "2019-11-25", "2021-04-07", "2021-10-06"))) +
  ylim(0,4500) +
  theme_minimal()
  
write.csv(drug_bydate_cohort_size4_plot, file = "R:/working/hdang_AnalysisFiles/R_Output/cohort_size.csv", row.names = FALSE)
