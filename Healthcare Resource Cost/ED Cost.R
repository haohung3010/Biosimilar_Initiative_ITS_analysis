library(dplyr)
library(lubridate)
library(ggplot2)

# Step 1: Identify ED visits captured in MSP, NACRS, or DAD (using SAS identifying algorithm)
# nacrs = read.csv("R:/working/hdang_AnalysisFiles/Input/ED_cost/nacrs.csv")

# Step 2: Calculate ED Facility cost
# price_index = read.csv("R:/working/hdang_AnalysisFiles/Input/ED_cost/StatCan_price_index.csv", stringsAsFactors = FALSE)

index_2017 = price_index$Index[price_index$Fiscal_Year == "2017"]
index_2021 = price_index$Index[price_index$Fiscal_Year == "2021"]
index_2022 = price_index$Index[price_index$Fiscal_Year == "2022"]

ed_temp = nacrs %>%
  mutate(
    ED_facility_2022 = 346 * (index_2022 / index_2021),
    date = as.Date(ServDate)
  ) %>%
  select(STUDYID, date, ED_facility_2022)

ed_temp = ed_temp %>%
  left_join(drug_bydate_cohort, by = c("STUDYID" = "STUDYID", "date" = "rx_date"))

ed_facility_cost = ed_temp %>%
  group_by(year, month, cohort2) %>%
  summarise(cost_ED_facility_2022 = sum(ED_facility_2022)) %>%
  select(year, month, cohort2, cost_ED_facility_2022)
ggplot(ed_facility_cost, aes(x=as.Date(paste(year,month, "01", sep = "-")), y = cost_ED_facility_2022, color = cohort2)) +
  geom_line() +
  geom_point() +
  labs(x = "Date", y = "Monthly ED cost") +
  ggtitle("ED cost All disease") +
  geom_vline(xintercept = as.Date(c("2019-05-27", "2019-11-25", "2021-04-07", "2021-10-06"))) +
  ylim(0, 350000) +
  theme_minimal()
