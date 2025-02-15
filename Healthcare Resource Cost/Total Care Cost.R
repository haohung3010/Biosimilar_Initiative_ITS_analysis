pn_ADA_IJD_total_cost = pn_cost_IJD %>%
  filter(cohort2 == "IJD" & drug2 == "ADA")
pn_ETA_IJD_total_cost = pn_cost_IJD %>%
  filter(cohort2 == "IJD" & drug2 == "ETA")
pn_INF_IJD_total_cost = pn_cost_IJD %>%
  filter(cohort2 == "IJD" & drug2 == "INF")
pn_ADA_ISD_total_cost = pn_cost_ISD %>%
  filter(cohort2 == "ISD" & drug2 == "ADA")
pn_ETA_ISD_total_cost = pn_cost_ISD %>%
  filter(cohort2 == "ISD" & drug2 == "ETA")
pn_INF_ISD_total_cost = pn_cost_ISD %>%
  filter(cohort2 == "ISD" & drug2 == "INF")
pn_ADA_IBD_total_cost = pn_cost_IBD %>%
  filter(cohort2 == "IBD" & drug2 == "ADA")
pn_INF_IBD_total_cost = pn_cost_IBD %>%
  filter(cohort2 == "IBD" & drug2 == "INF")

############# ADA - IJD ###################
msp_ADA_IJD_SP_total_cost = allphy_cost2 %>%
  filter(cohort2 == "IJD") %>%
  filter(drug == "ADA" | drug == "ADAB") %>%
  filter(md_spec == "RHEUM") %>%
  group_by(year, month, cohort2) %>%
  summarise(cost_spec_phy_2022 = sum(phy_cost_2022, na.rm = TRUE)) %>%
  select(year, month, cohort2, cohort_size, cost_spec_phy_2022)

msp_ADA_IJD_Oth_total_cost = allphy_cost2 %>%
  filter(cohort2 == "IJD") %>%
  filter(drug == "ADA" | drug == "ADAB") %>%
  filter(is.na(md_spec) | md_spec != "RHEUM") %>%
  group_by(year, month, cohort2) %>%
  summarise(cost_oth_phy_2022 = sum(phy_cost_2022, na.rm = TRUE)) %>%
  select(year, month, cohort2, cost_oth_phy_2022)

dad_ADA_IJD_total_cost = cohort_cmg3 %>%
  filter(cohort2 == "IJD") %>%
  filter(drug == "ADA" | drug == "ADAB") %>%
  group_by(year, month, cohort2) %>%
  summarize(cost_dad_2022 = sum(cost_daily_2022, na.rm = TRUE)) %>%
  select(year, month, cohort2, cost_dad_2022)

ed_ADA_IJD_facility_cost = ed_temp %>%
  filter(cohort2 == "IJD") %>%
  filter(drug == "ADA" | drug == "ADAB") %>%
  group_by(year, month, cohort2) %>%
  summarise(cost_ED_facility_2022 = sum(ED_facility_2022)) %>%
  select(year, month, cohort2, cost_ED_facility_2022)

ADA_IJD_total_cost = left_join(msp_ADA_IJD_SP_total_cost, msp_ADA_IJD_Oth_total_cost, by = c("year" = "year", "month" = "month"))
ADA_IJD_total_cost = left_join(ADA_IJD_total_cost, dad_ADA_IJD_total_cost, by = c("year" = "year", "month" = "month"))
ADA_IJD_total_cost = left_join(ADA_IJD_total_cost, ed_ADA_IJD_facility_cost, by = c("year" = "year", "month" = "month"))
ADA_IJD_total_cost = left_join(ADA_IJD_total_cost, pn_ADA_IJD_total_cost, by = c("year" = "year", "month" = "month"))

ADA_IJD_total_cost = ADA_IJD_total_cost %>% 
  rowwise() %>%
  mutate(total_cost = sum(cost_spec_phy_2022, cost_oth_phy_2022, cost_dad_2022, cost_ED_facility_2022, PharmaCare_and_Client, PharmaCare_and_Client, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(line_type = "ADA_IJD")

ggplot(ADA_IJD_total_cost, aes(x=as.Date(paste(year,month, "01", sep = "-")), y = total_cost, color = line_type)) +
  geom_line() +
  geom_point() +
  labs(x = "Date", y = "Monthly Total Cost") +
  ggtitle("Monthly Total Cost") +
  geom_vline(xintercept = as.Date(c("2019-05-27", "2019-11-25", "2021-04-07", "2021-10-06"))) +
  ylim(0, 1100000) +
  theme_minimal()

############# ETA - IJD ###################
msp_ETA_IJD_SP_total_cost = allphy_cost2 %>%
  filter(cohort2 == "IJD") %>%
  filter(drug == "ETA" | drug == "ETAB") %>%
  filter(md_spec == "RHEUM") %>%
  group_by(year, month, cohort2) %>%
  summarise(cost_spec_phy_2022 = sum(phy_cost_2022, na.rm = TRUE)) %>%
  select(year, month, cohort2, cost_spec_phy_2022)

msp_ETA_IJD_Oth_total_cost = allphy_cost2 %>%
  filter(cohort2 == "IJD") %>%
  filter(drug == "ETA" | drug == "ETAB") %>%
  filter(is.na(md_spec) | md_spec != "RHEUM") %>%
  group_by(year, month, cohort2) %>%
  summarise(cost_oth_phy_2022 = sum(phy_cost_2022, na.rm = TRUE)) %>%
  select(year, month, cohort2, cost_oth_phy_2022)

dad_ETA_IJD_total_cost = cohort_cmg3 %>%
  filter(cohort2 == "IJD") %>%
  filter(drug == "ETA" | drug == "ETAB") %>%
  group_by(year, month, cohort2) %>%
  summarize(cost_dad_2022 = sum(cost_daily_2022, na.rm = TRUE)) %>%
  select(year, month, cohort2, cost_dad_2022)

ed_ETA_IJD_facility_cost = ed_temp %>%
  filter(cohort2 == "IJD") %>%
  filter(drug == "ETA" | drug == "ETAB") %>%
  group_by(year, month, cohort2) %>%
  summarise(cost_ED_facility_2022 = sum(ED_facility_2022)) %>%
  select(year, month, cohort2, cost_ED_facility_2022)

ETA_IJD_total_cost = left_join(msp_ETA_IJD_SP_total_cost, msp_ETA_IJD_Oth_total_cost, by = c("year" = "year", "month" = "month"))
ETA_IJD_total_cost = left_join(ETA_IJD_total_cost, dad_ETA_IJD_total_cost, by = c("year" = "year", "month" = "month"))
ETA_IJD_total_cost = left_join(ETA_IJD_total_cost, ed_ETA_IJD_facility_cost, by = c("year" = "year", "month" = "month"))
ETA_IJD_total_cost = left_join(ETA_IJD_total_cost, pn_ETA_IJD_total_cost, by = c("year" = "year", "month" = "month"))

ETA_IJD_total_cost = ETA_IJD_total_cost %>% 
  rowwise() %>%
  mutate(total_cost = sum(cost_spec_phy_2022, cost_oth_phy_2022, cost_dad_2022, cost_ED_facility_2022, PharmaCare_and_Client, PharmaCare_and_Client, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(line_type = "ETA_IJD")

############# INF - IJD ###################
msp_INF_IJD_SP_total_cost = allphy_cost2 %>%
  filter(cohort2 == "IJD") %>%
  filter(drug == "INF" | drug == "INFB") %>%
  filter(md_spec == "RHEUM") %>%
  group_by(year, month, cohort2) %>%
  summarise(cost_spec_phy_2022 = sum(phy_cost_2022, na.rm = TRUE)) %>%
  select(year, month, cohort2, cost_spec_phy_2022)

msp_INF_IJD_Oth_total_cost = allphy_cost2 %>%
  filter(cohort2 == "IJD") %>%
  filter(drug == "INF" | drug == "INFB") %>%
  filter(is.na(md_spec) | md_spec != "RHEUM") %>%
  group_by(year, month, cohort2) %>%
  summarise(cost_oth_phy_2022 = sum(phy_cost_2022, na.rm = TRUE)) %>%
  select(year, month, cohort2, cost_oth_phy_2022)

dad_INF_IJD_total_cost = cohort_cmg3 %>%
  filter(cohort2 == "IJD") %>%
  filter(drug == "INF" | drug == "INFB") %>%
  group_by(year, month, cohort2) %>%
  summarize(cost_dad_2022 = sum(cost_daily_2022, na.rm = TRUE)) %>%
  select(year, month, cohort2, cost_dad_2022)

ed_INF_IJD_facility_cost = ed_temp %>%
  filter(cohort2 == "IJD") %>%
  filter(drug == "INF" | drug == "INFB") %>%
  group_by(year, month, cohort2) %>%
  summarise(cost_ED_facility_2022 = sum(ED_facility_2022)) %>%
  select(year, month, cohort2, cost_ED_facility_2022)

INF_IJD_total_cost = left_join(msp_INF_IJD_SP_total_cost, msp_INF_IJD_Oth_total_cost, by = c("year" = "year", "month" = "month"))
INF_IJD_total_cost = left_join(INF_IJD_total_cost, dad_INF_IJD_total_cost, by = c("year" = "year", "month" = "month"))
INF_IJD_total_cost = left_join(INF_IJD_total_cost, ed_INF_IJD_facility_cost, by = c("year" = "year", "month" = "month"))
INF_IJD_total_cost = left_join(INF_IJD_total_cost, pn_INF_IJD_total_cost, by = c("year" = "year", "month" = "month"))

INF_IJD_total_cost = INF_IJD_total_cost %>% 
  rowwise() %>%
  mutate(total_cost = sum(cost_spec_phy_2022, cost_oth_phy_2022, cost_dad_2022, cost_ED_facility_2022, PharmaCare_and_Client, PharmaCare_and_Client, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(line_type = "INF_IJD")

############# ADA - ISD ###################
msp_ADA_ISD_SP_total_cost = allphy_cost2 %>%
  filter(cohort2 == "ISD") %>%
  filter(drug == "ADA" | drug == "ADAB") %>%
  filter(md_spec == "RHEUM" | md_spec == "DERM") %>%
  group_by(year, month, cohort2) %>%
  summarise(cost_spec_phy_2022 = sum(phy_cost_2022, na.rm = TRUE)) %>%
  select(year, month, cohort2, cost_spec_phy_2022)

msp_ADA_ISD_Oth_total_cost = allphy_cost2 %>%
  filter(cohort2 == "ISD") %>%
  filter(drug == "ADA" | drug == "ADAB") %>%
  filter(is.na(md_spec) | (md_spec != "RHEUM" & md_spec != "DERM")) %>%
  group_by(year, month, cohort2) %>%
  summarise(cost_oth_phy_2022 = sum(phy_cost_2022, na.rm = TRUE)) %>%
  select(year, month, cohort2, cost_oth_phy_2022)

dad_ADA_ISD_total_cost = cohort_cmg3 %>%
  filter(cohort2 == "ISD") %>%
  filter(drug == "ADA" | drug == "ADAB") %>%
  group_by(year, month, cohort2) %>%
  summarize(cost_dad_2022 = sum(cost_daily_2022, na.rm = TRUE)) %>%
  select(year, month, cohort2, cost_dad_2022)

ed_ADA_ISD_facility_cost = ed_temp %>%
  filter(cohort2 == "ISD") %>%
  filter(drug == "ADA" | drug == "ADAB") %>%
  group_by(year, month, cohort2) %>%
  summarise(cost_ED_facility_2022 = sum(ED_facility_2022)) %>%
  select(year, month, cohort2, cost_ED_facility_2022)

ADA_ISD_total_cost = left_join(msp_ADA_ISD_SP_total_cost, msp_ADA_ISD_Oth_total_cost, by = c("year" = "year", "month" = "month"))
ADA_ISD_total_cost = left_join(ADA_ISD_total_cost, dad_ADA_ISD_total_cost, by = c("year" = "year", "month" = "month"))
ADA_ISD_total_cost = left_join(ADA_ISD_total_cost, ed_ADA_ISD_facility_cost, by = c("year" = "year", "month" = "month"))
ADA_ISD_total_cost = left_join(ADA_ISD_total_cost, pn_ADA_ISD_total_cost, by = c("year" = "year", "month" = "month"))

ADA_ISD_total_cost = ADA_ISD_total_cost %>% 
  rowwise() %>%
  mutate(total_cost = sum(cost_spec_phy_2022, cost_oth_phy_2022, cost_dad_2022, cost_ED_facility_2022, PharmaCare_and_Client, PharmaCare_and_Client, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(line_type = "ADA_ISD")

############# ETA - ISD ###################
msp_ETA_ISD_SP_total_cost = allphy_cost2 %>%
  filter(cohort2 == "ISD") %>%
  filter(drug == "ETA" | drug == "ETAB") %>%
  filter(md_spec == "RHEUM" | md_spec == "DERM") %>%
  group_by(year, month, cohort2) %>%
  summarise(cost_spec_phy_2022 = sum(phy_cost_2022, na.rm = TRUE)) %>%
  select(year, month, cohort2, cost_spec_phy_2022)

msp_ETA_ISD_Oth_total_cost = allphy_cost2 %>%
  filter(cohort2 == "ISD") %>%
  filter(drug == "ETA" | drug == "ETAB") %>%
  filter(is.na(md_spec) | md_spec != "RHEUM" | md_spec != "DERM") %>%
  group_by(year, month, cohort2) %>%
  summarise(cost_oth_phy_2022 = sum(phy_cost_2022, na.rm = TRUE)) %>%
  select(year, month, cohort2, cost_oth_phy_2022)

dad_ETA_ISD_total_cost = cohort_cmg3 %>%
  filter(cohort2 == "ISD") %>%
  filter(drug == "ETA" | drug == "ETAB") %>%
  group_by(year, month, cohort2) %>%
  summarize(cost_dad_2022 = sum(cost_daily_2022, na.rm = TRUE)) %>%
  select(year, month, cohort2, cost_dad_2022)

ed_ETA_ISD_facility_cost = ed_temp %>%
  filter(cohort2 == "ISD") %>%
  filter(drug == "ETA" | drug == "ETAB") %>%
  group_by(year, month, cohort2) %>%
  summarise(cost_ED_facility_2022 = sum(ED_facility_2022)) %>%
  select(year, month, cohort2, cost_ED_facility_2022)

ETA_ISD_total_cost = left_join(msp_ETA_ISD_SP_total_cost, msp_ETA_ISD_Oth_total_cost, by = c("year" = "year", "month" = "month"))
ETA_ISD_total_cost = left_join(ETA_ISD_total_cost, dad_ETA_ISD_total_cost, by = c("year" = "year", "month" = "month"))
ETA_ISD_total_cost = left_join(ETA_ISD_total_cost, ed_ETA_ISD_facility_cost, by = c("year" = "year", "month" = "month"))
ETA_ISD_total_cost = left_join(ETA_ISD_total_cost, pn_ETA_ISD_total_cost, by = c("year" = "year", "month" = "month"))

ETA_ISD_total_cost = ETA_ISD_total_cost %>% 
  rowwise() %>%
  mutate(total_cost = sum(cost_spec_phy_2022, cost_oth_phy_2022, cost_dad_2022, cost_ED_facility_2022, PharmaCare_and_Client, PharmaCare_and_Client, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(line_type = "ETA_ISD")

############# INF - ISD ###################
msp_INF_ISD_SP_total_cost = allphy_cost2 %>%
  filter(cohort2 == "ISD") %>%
  filter(drug == "INF" | drug == "INFB") %>%
  filter(md_spec == "RHEUM" | md_spec == "DERM") %>%
  group_by(year, month, cohort2) %>%
  summarise(cost_spec_phy_2022 = sum(phy_cost_2022, na.rm = TRUE)) %>%
  select(year, month, cohort2, cost_spec_phy_2022)

msp_INF_ISD_Oth_total_cost = allphy_cost2 %>%
  filter(cohort2 == "ISD") %>%
  filter(drug == "INF" | drug == "INFB") %>%
  filter(is.na(md_spec) | (md_spec != "RHEUM" & md_spec != "DERM")) %>%
  group_by(year, month, cohort2) %>%
  summarise(cost_oth_phy_2022 = sum(phy_cost_2022, na.rm = TRUE)) %>%
  select(year, month, cohort2, cost_oth_phy_2022)

dad_INF_ISD_total_cost = cohort_cmg3 %>%
  filter(cohort2 == "ISD") %>%
  filter(drug == "INF" | drug == "INFB") %>%
  group_by(year, month, cohort2) %>%
  summarize(cost_dad_2022 = sum(cost_daily_2022, na.rm = TRUE)) %>%
  select(year, month, cohort2, cost_dad_2022)

ed_INF_ISD_facility_cost = ed_temp %>%
  filter(cohort2 == "ISD") %>%
  filter(drug == "INF" | drug == "INFB") %>%
  group_by(year, month, cohort2) %>%
  summarise(cost_ED_facility_2022 = sum(ED_facility_2022)) %>%
  select(year, month, cohort2, cost_ED_facility_2022)

INF_ISD_total_cost = left_join(msp_INF_ISD_SP_total_cost, msp_INF_ISD_Oth_total_cost, by = c("year" = "year", "month" = "month"))
INF_ISD_total_cost = left_join(INF_ISD_total_cost, dad_INF_ISD_total_cost, by = c("year" = "year", "month" = "month"))
INF_ISD_total_cost = left_join(INF_ISD_total_cost, ed_INF_ISD_facility_cost, by = c("year" = "year", "month" = "month"))
INF_ISD_total_cost = left_join(INF_ISD_total_cost, pn_INF_ISD_total_cost, by = c("year" = "year", "month" = "month"))

INF_ISD_total_cost = INF_ISD_total_cost %>% 
  rowwise() %>%
  mutate(total_cost = sum(cost_spec_phy_2022, cost_oth_phy_2022, cost_dad_2022, cost_ED_facility_2022, PharmaCare_and_Client, PharmaCare_and_Client, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(line_type = "INF_ISD")

############# ADA - IBD ###################
msp_ADA_IBD_SP_total_cost = allphy_cost2 %>%
  filter(cohort2 == "IBD") %>%
  filter(drug == "ADA" | drug == "ADAB") %>%
  filter(md_spec == "GASTRO") %>%
  group_by(year, month, cohort2) %>%
  summarise(cost_spec_phy_2022 = sum(phy_cost_2022, na.rm = TRUE)) %>%
  select(year, month, cohort2, cost_spec_phy_2022)

msp_ADA_IBD_Oth_total_cost = allphy_cost2 %>%
  filter(cohort2 == "IBD") %>%
  filter(drug == "ADA" | drug == "ADAB") %>%
  filter(is.na(md_spec) | md_spec != "GASTRO") %>%
  group_by(year, month, cohort2) %>%
  summarise(cost_oth_phy_2022 = sum(phy_cost_2022, na.rm = TRUE)) %>%
  select(year, month, cohort2, cost_oth_phy_2022)

dad_ADA_IBD_total_cost = cohort_cmg3 %>%
  filter(cohort2 == "IBD") %>%
  filter(drug == "ADA" | drug == "ADAB") %>%
  group_by(year, month, cohort2) %>%
  summarize(cost_dad_2022 = sum(cost_daily_2022, na.rm = TRUE)) %>%
  select(year, month, cohort2, cost_dad_2022)

ed_ADA_IBD_facility_cost = ed_temp %>%
  filter(cohort2 == "IBD") %>%
  filter(drug == "ADA" | drug == "ADAB") %>%
  group_by(year, month, cohort2) %>%
  summarise(cost_ED_facility_2022 = sum(ED_facility_2022)) %>%
  select(year, month, cohort2, cost_ED_facility_2022)

ADA_IBD_total_cost = left_join(msp_ADA_IBD_SP_total_cost, msp_ADA_IBD_Oth_total_cost, by = c("year" = "year", "month" = "month"))
ADA_IBD_total_cost = left_join(ADA_IBD_total_cost, dad_ADA_IBD_total_cost, by = c("year" = "year", "month" = "month"))
ADA_IBD_total_cost = left_join(ADA_IBD_total_cost, ed_ADA_IBD_facility_cost, by = c("year" = "year", "month" = "month"))
ADA_IBD_total_cost = left_join(ADA_IBD_total_cost, pn_ADA_IBD_total_cost, by = c("year" = "year", "month" = "month"))

names(msp_ADA_IBD_SP_total_cost)
names(msp_ADA_IBD_Oth_total_cost)
names(dad_ADA_IBD_total_cost)
names(ed_ADA_IBD_facility_cost)
names(pn_ADA_IBD_total_cost)

ADA_IBD_total_cost = ADA_IBD_total_cost %>% 
  rowwise() %>%
  mutate(total_cost = sum(cost_spec_phy_2022, cost_oth_phy_2022, cost_dad_2022, cost_ED_facility_2022, PharmaCare_and_Client, PharmaCare_and_Client, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(line_type = "ADA_IBD")



############# INF - IBD ###################
msp_INF_IBD_SP_total_cost = allphy_cost2 %>%
  filter(cohort2 == "IBD") %>%
  filter(drug == "INF" | drug == "INFB") %>%
  filter(md_spec == "GASTRO") %>%
  group_by(year, month, cohort2) %>%
  summarise(cost_spec_phy_2022 = sum(phy_cost_2022, na.rm = TRUE)) %>%
  select(year, month, cohort2, cost_spec_phy_2022)

msp_INF_IBD_Oth_total_cost = allphy_cost2 %>%
  filter(cohort2 == "IBD") %>%
  filter(drug == "INF" | drug == "INFB") %>%
  filter(is.na(md_spec) | md_spec != "GASTRO") %>%
  group_by(year, month, cohort2) %>%
  summarise(cost_oth_phy_2022 = sum(phy_cost_2022, na.rm = TRUE)) %>%
  select(year, month, cohort2, cost_oth_phy_2022)

dad_INF_IBD_total_cost = cohort_cmg3 %>%
  filter(cohort2 == "IBD") %>%
  filter(drug == "INF" | drug == "INFB") %>%
  group_by(year, month, cohort2) %>%
  summarize(cost_dad_2022 = sum(cost_daily_2022, na.rm = TRUE)) %>%
  select(year, month, cohort2, cost_dad_2022)

ed_INF_IBD_facility_cost = ed_temp %>%
  filter(cohort2 == "IBD") %>%
  filter(drug == "INF" | drug == "INFB") %>%
  group_by(year, month, cohort2) %>%
  summarise(cost_ED_facility_2022 = sum(ED_facility_2022)) %>%
  select(year, month, cohort2, cost_ED_facility_2022)

INF_IBD_total_cost = left_join(msp_INF_IBD_SP_total_cost, msp_INF_IBD_Oth_total_cost, by = c("year" = "year", "month" = "month"))
INF_IBD_total_cost = left_join(INF_IBD_total_cost, dad_INF_IBD_total_cost, by = c("year" = "year", "month" = "month"))
INF_IBD_total_cost = left_join(INF_IBD_total_cost, ed_INF_IBD_facility_cost, by = c("year" = "year", "month" = "month"))
INF_IBD_total_cost = left_join(INF_IBD_total_cost, pn_INF_IBD_total_cost, by = c("year" = "year", "month" = "month"))

INF_IBD_total_cost = INF_IBD_total_cost %>% 
  rowwise() %>%
  mutate(total_cost = sum(cost_spec_phy_2022, cost_oth_phy_2022, cost_dad_2022, cost_ED_facility_2022, PharmaCare_and_Client, PharmaCare_and_Client, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(line_type = "INF_IBD")

############# Graph #################
IAD_cohort_total_cost = rbind(ADA_IJD_total_cost, ETA_IJD_total_cost, INF_IJD_total_cost, 
                              ADA_ISD_total_cost, ETA_ISD_total_cost, INF_ISD_total_cost)
IBD_cohort_total_cost = rbind(ADA_IBD_total_cost, INF_IBD_total_cost)

IJD_total_cost = IAD_cohort_total_cost %>% filter(line_type == "ADA_IJD" | line_type == "ETA_IJD" | line_type == "INF_IJD")

ggplot(IJD_total_cost) +
  geom_line(aes(x=as.Date(paste(year,month, "01", sep = "-")), y = total_cost, color = line_type)) +
  geom_point(aes(x=as.Date(paste(year,month, "01", sep = "-")), y = total_cost, color = line_type)) +
  scale_color_manual(values = c("ADA_IJD" = "red", "ETA_IJD" = "darkblue", "INF_IJD" = "cornsilk4")) +
  labs(x = "Date", y = "Monthly Total Secondary Care cost (2021 CAD)") +
  ggtitle("Monthly Total Secondary Care Cost for IJD cohort") +
  geom_vline(xintercept = as.Date(c("2019-05-27", "2019-11-25", "2021-04-07", "2021-10-06"))) +
  ylim(0,2700000) +
  theme_minimal()

IBD_totalcost_2015=sum(IBD_total_cost$total_cost[IBD_total_cost$year==2015])/1000000
IBD_totalcost_2022=sum(IBD_total_cost$total_cost[IBD_total_cost$year==2022])/1000000




ISD_total_cost = IAD_cohort_total_cost %>% filter(line_type == "ADA_ISD" | line_type == "ETA_ISD" | line_type == "INF_ISD")

ggplot(ISD_total_cost) +
  geom_line(aes(x=as.Date(paste(year,month, "01", sep = "-")), y = total_cost, color = line_type)) +
  geom_point(aes(x=as.Date(paste(year,month, "01", sep = "-")), y = total_cost, color = line_type)) +
  scale_color_manual(values = c("ADA_ISD" = "red", "ETA_ISD" = "darkblue", "INF_ISD" = "cornsilk4")) +
  labs(x = "Date", y = "Monthly Total Secondary Care cost (2021 CAD)") +
  ggtitle("Monthly Total Secondary Care Cost for ISD cohort") +
  geom_vline(xintercept = as.Date(c("2019-05-27", "2019-11-25", "2021-04-07", "2021-10-06"))) +
  ylim(0,2700000) +
  theme_minimal()

IBD_total_cost = IBD_cohort_total_cost %>% filter(line_type == "ADA_IBD" | line_type == "ETA_IBD" | line_type == "INF_IBD")

ggplot(IBD_total_cost) +
  geom_line(aes(x=as.Date(paste(year,month, "01", sep = "-")), y = total_cost, color = line_type)) +
  geom_point(aes(x=as.Date(paste(year,month, "01", sep = "-")), y = total_cost, color = line_type)) +
  scale_color_manual(values = c("ADA_IBD" = "red", "ETA_IBD" = "darkblue", "INF_IBD" = "cornsilk4")) +
  labs(x = "Date", y = "Monthly Total Secondary Care cost (2022 CAD)") +
  ggtitle("Monthly Total Secondary Care Cost for IBD cohort") +
  geom_vline(xintercept = as.Date(c("2019-09-05", "2020-03-05", "2021-04-07", "2021-10-06"))) +
  ylim(0,2700000) +
  theme_minimal()





write.csv(IJD_total_cost, file = "R:/working/hdang_AnalysisFiles/R_Output/IJD_total_cost.csv", row.names = FALSE)
write.csv(IBD_total_cost, file = "R:/working/hdang_AnalysisFiles/R_Output/IBD_total_cost.csv", row.names = FALSE)
write.csv(ISD_total_cost, file = "R:/working/hdang_AnalysisFiles/R_Output/ISD_total_cost.csv", row.names = FALSE)

