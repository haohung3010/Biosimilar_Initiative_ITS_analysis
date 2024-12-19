library(dplyr)
library(tidyr)
library(lubridate)
library(purrr)
library(ggplot2)

###########################################################
# DAD costing :
# costing level: A (acute care), S (day care surgery)
###########################################################
#2024 DAD: There is no separate CMG datatset that needs to be merged.

#Importing datasets
#dad = read.csv("R:/working/hdang_AnalysisFiles/Input/Hospital_cost/dad.csv")
#cmglist = read.csv("R:/working/hdang_AnalysisFiles/Input/Hospital_cost/cmglist.csv")
price_index = read.csv("R:/working/hdang_AnalysisFiles/Input/Hospital_cost/StatCan_price_index.csv", stringsAsFactors = FALSE)
std_hosp_cost =  read.csv("R:/working/hdang_AnalysisFiles/Input/Hospital_cost/CIHI_cost_per_stay.csv")
#drug_bydate = read.csv("R:/working/hdang_AnalysisFiles/Input/tnfa_drug_bydate.csv", stringsAsFactors=FALSE)

#Reformatting and adding fy_ad variable
#dad$ADDATE = as.Date(dad$ADDATE) 
#dad$fy_ad = ifelse(month(dad$ADDATE) <= 3, year(dad$ADDATE) - 1, year(dad$ADDATE))
std_hosp_cost$Cost_per_stay = as.numeric(std_hosp_cost$Cost_per_stay)

#create cohort_cmg w std cost for hosp stay
cohort_cmg = dad %>% 
  left_join(std_hosp_cost, by = c("fy_ad" = "Fiscal_year")) %>%
  filter(!is.na(Cost_per_stay))

#Filter cohort_cmg based on level, riw value is based on level (A=CMGP_RIW, S=CACS_RIW)
#CMGP_RIW: Inpatient Resource Intensity Weight (RIW)
#CACS_RIW: CACS (Comprehensive Ambulatory Classification System) Resource Intensity Weight (RIW)
cohort_cmg = cohort_cmg %>%
  #calculate cost and cost_daily
  mutate(
    cost = GRPR_RIW * Cost_per_stay,
    cost_daily = case_when(
      CARE_LEVEL == "S" ~ cost,
      CARE_LEVEL == "A" ~ cost/pmax(1,TDAYS)
    )
  ) %>%
  select (STUDYID, ADDATE, SEPDATE, CARE_LEVEL, TDAYS, GRPR_RIW, Cost_per_stay, RA, PA, AS, CD, UC, cost, cost_daily)

#Distribute total cost over the entire hospital stay
generate_day = function (start_date, num_days) {
  seq(start_date, by = "days", length.out = num_days)
}

cohort_cmg2 = cohort_cmg %>%
  group_by(STUDYID,ADDATE, TDAYS) %>%
  mutate( 
    TDAYS_2 = ifelse(TDAYS == 0, 1, TDAYS), #because the function skip observation if TDAYS = 0
    date = pmap(list(ADDATE, TDAYS_2), generate_day) #create a list of days
  ) %>% 
  unnest(date) %>% #populate date list into rows
  
  mutate(
    date = as.Date(date, "%Y-%m-%d"),
    fy = ifelse(month(date) <= 3, year(date) - 1, year(date))
  )

#Adjust for inflation
index_2022 = price_index$Index[price_index$Fiscal_Year == 2022]

cohort_cmg3 = cohort_cmg2 %>%
  inner_join(drug_bydate_cohort, by = c("STUDYID" = "STUDYID", "date" = "rx_date"))


cohort_cmg3_temp = cohort_cmg3 %>%
  left_join(price_index, by = c("fy" = "Fiscal_Year")) %>%
  mutate(
    cost_daily_2022 = cost_daily / (Index / index_2022),
    cost_2022 = cost / (Index / index_2022),
    year = year(date),
    month = month(date)) %>%
  filter(year >= 2015 & year <= 2022)

dad_total_cost_daily = cohort_cmg3_temp  %>%
  group_by(year, month, cohort2) %>%
  summarize(cost_dad_2022 = sum(cost_daily_2022, na.rm = TRUE)) %>%
  select(year, month, cohort2, cost_dad_2022)
dad_total_cost = cohort_cmg3_temp  %>%
  group_by(year, month, cohort2) %>%
  summarize(cost_dad_2022 = sum(cost_2022, na.rm = TRUE)) %>%
  select(year, month, cohort2, cost_dad_2022)


cohort_cmg3_daily = cohort_cmg2 %>%
  left_join(price_index, by = c("fy" = "Fiscal_Year")) %>%
  mutate(
    cost_daily_2022 = cost_daily / (Index / index_2022),
    year = year(date),
    month = month(date)) %>%
  filter(year >= 2015 & year <= 2022)

#Create dad_total_cost
dad_total_cost_daily = cohort_cmg3_daily %>%
  group_by(year, month, cohort2) %>%
  summarize(cost_dad_2022 = sum(cost_daily_2022, na.rm = TRUE)) %>%
  select(year, month, cohort2, cost_dad_2022)


ggplot(dad_total_cost_daily, aes(x=as.Date(paste(year,month, "01", sep = "-")), y = cost_dad_2022, color = cohort2)) +
  geom_line() +
  geom_point() +
  labs(x = "Date", y = "Monthly Hospital Cost") +
  ggtitle("Hospital Cost all cohort") +
  geom_vline(xintercept = as.Date(c("2019-05-27", "2019-11-25", "2021-04-07", "2021-10-06"))) +
  ylim(0, 10000000) +
  theme_minimal()


cohort_cmg3 = cohort_cmg2 %>%
  left_join(price_index, by = c("fy" = "Fiscal_Year")) %>%
  mutate(
    cost_2022 = cost / (Index / index_2022),
    year = year(date),
    month = month(date)) %>%
  filter(year >= 2015 & year <= 2022)
#Create dad_total_cost
dad_total_cost = cohort_cmg3 %>%
  group_by(year, month, cohort2) %>%
  summarize(cost_dad_2022 = sum(cost_2022, na.rm = TRUE)) %>%
  select(year, month, cohort2, cost_dad_2022)
ggplot(dad_total_cost, aes(x=as.Date(paste(year,month, "01", sep = "-")), y = cost_dad_2022, color = cohort2)) +
  geom_line() +
  geom_point() +
  labs(x = "Date", y = "Monthly Hospital Cost") +
  ggtitle("Hospital Cost all cohort") +
  geom_vline(xintercept = as.Date(c("2019-05-27", "2019-11-25", "2021-04-07", "2021-10-06"))) +
  ylim(0, 50000000) +
  theme_minimal()

