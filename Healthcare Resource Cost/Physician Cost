library(openxlsx)
library(dplyr)
library(lubridate)
library(ggplot2)

#msp = read.csv("R:/working/hdang_AnalysisFiles/Input/msp.csv")
#msp$servdate = as.Date(msp$servdate)
#drug_bydate = read.csv("R:/working/hdang_AnalysisFiles/Input/tnfa_drug_bydate.csv", stringsAsFactors=FALSE)
#drug_bydate$rx_date = as.Date(drug_bydate$rx_date)
#nacrs = read.csv("R:/working/hdang_AnalysisFiles/Input/ED_cost/nacrs.csv") #dataset from identifying ED case algorithm (SAS)

################################
# Preparing datasets ###########
###############################

# Define function and read unit cost data from Excel
read_ER_unitcost = function(path, name) {
  read.xlsx(
    paste0(path, "/ER_APB_unitcosts_2019.xlsx"),
    sheet = name,
    startRow = 1,
    colNames = TRUE
  )
}

path_unitcost = "R:/working/hdang_AnalysisFiles/Input/Phys_cost"

ER_grandmean_byFY_unitcost = read_ER_unitcost(path_unitcost, "ER_grandmean_byFY")
ER_byTriageLevel_unitcost = read_ER_unitcost(path_unitcost, "ER_byTriageLevel")
ER_msp_1800_unitcost = read_ER_unitcost(path_unitcost, "ER_msp_1800")
ER_msp1800_level_unitcost = read_ER_unitcost(path_unitcost, "ER_msp1800_level")
msp_apb_unitcost = read_ER_unitcost(path_unitcost, "msp_apb")

#ER_byTriageLevel_unitcost$N_TRIAGELEVEL = as.integer(ER_byTriageLevel_unitcost$TRIAGELEVEL_1)

# Import holiday dates data, creat holiday variable
holiday = read.xlsx("R:/working/hdang_AnalysisFiles/Input/Phys_cost/Holiday_Dates_upto2022.xlsx",
                    sheet = "Sheet1",
                    startRow = 1,
                    colNames = TRUE
)

holiday$date = as.Date(paste(holiday$year, holiday$month, holiday$day, sep = "-"))

holiday = holiday %>% select(date) %>% mutate(holiday = 1)

# Import price index data, Get index value for 2021
#SB Change to 2022
price_index = read.csv("R:/working/hdang_AnalysisFiles/Input/Phys_cost/StatCan_price_index.csv", stringsAsFactors = FALSE)

#index_2021 = price_index$Index[price_index$Fiscal_Year == "2021"]
index_2022 = price_index$Index[price_index$Fiscal_Year == "2022"]

###########################################################
# Step 1: Phy cost of ED visits in NACRS but not in MSP
###########################################################
SetA = nacrs %>%
  filter(EDSource %in% c("NACRS & DADE", "NACRS only")) %>%
  mutate(ServDate = as.Date(ServDate)) %>%
  left_join(holiday, by = c("ServDate" = "date")) %>%
  mutate(spec = "-1",
         spec = as.numeric(spec)) 

SetA = SetA %>%
  mutate(
    keyDateTime = ifelse(!is.na(AssessDateTime), AssessDateTime, RegDatetime),
    keyDateTime = as.POSIXct(keyDateTime, origin = "1960-01-01", tz = "UTC"), 
    keydate = as.Date(keyDateTime),
    keytime = format(keyDateTime,"%H:%M:%S"),
    keyhour = as.integer(format(keyDateTime, "%H")),
    fy = ifelse(month(keydate) <= 3, year(keydate) - 1, year(keydate)),
    shift = case_when(weekdays(keydate) %in% c("Saturday", "Sunday") | holiday == 1 ~ "holiday",
                      keyhour >= 8 & keyhour < 18 ~ "day",
                      keyhour >= 18 & keyhour < 24 ~ "evening",
                      keyhour >= 0 & keyhour < 8 ~ "night")
  )

ER_byTriageLevel_unitcost = ER_byTriageLevel_unitcost %>%
  rename(shift_1 = msp_shift, unitcost_fy = fy, TRIAGELEVEL_1 = N_TRIAGELEVEL)
ER_byTriageLevel_unitcost$TRIAGELEVEL_1 = as.integer(ER_byTriageLevel_unitcost$TRIAGELEVEL_1)

SetA_2 = SetA %>%
  left_join(ER_byTriageLevel_unitcost, by = c("N_TRIAGELEVEL" = "TRIAGELEVEL_1", "shift" = "shift_1"))

SetA_Cost = SetA_2 %>%
  left_join(price_index, by = c("unitcost_fy" = "Fiscal_Year"))
SetA_Cost = SetA_Cost %>% 
  rename(index_unitcost = Index) 
#%>%
#  mutate(set = "A",
#         ServDate = as.character(ServDate)) #to use bind row later
  

######################################################
# Step 2: Physician cost of APB and ER-related feeitems
######################################################

msp1 = msp %>%
  mutate(
    fy = ifelse(month(ServDate) <= 3, 
                year(ServDate) - 1, 
                year(ServDate)
    )
  )

SetB = msp1 %>%
  filter(feeitem %in% c(96801,96811,96821,96802,96812,96822,96803,96813,96823,96804,96814,96824,96805,96815,96825)) %>%
  mutate(
    apb_msp_level = case_when(
      feeitem %in% c(96801,96811,96821) ~ 1,
      feeitem %in% c(96802,96812,96822) ~ 2,
      feeitem %in% c(96803,96813,96823) ~ 3,
      feeitem %in% c(96804,96814,96824) ~ 4,
      feeitem %in% c(96805,96815,96825) ~ 5
    ),
    convert_level = case_when(
      apb_msp_level %in% c(1,2) ~ 3,
      apb_msp_level %in% c(3,4) ~ 2,
      apb_msp_level %in% c(5) ~ 1
    ),
    shift = case_when(
      feeitem %in% c(96801,96802,96803,96804,96805) ~ "day",
      feeitem %in% c(96811,96812,96813,96814,96815) ~ "evening",
      feeitem %in% c(96821,96822,96823,96824,96825) ~ "night"
    )
  )

ER_msp_1800_unitcost = ER_msp_1800_unitcost %>%
  rename(feeitem_1 = feeitem, unitcost_fy = fy)

SetB_2 = SetB %>%  
  left_join(ER_msp_1800_unitcost, by = c("convert_level" = "msp_level", "shift" = "msp_shift")) %>%
  select(STUDYID, ServDate, convert_level, shift, fy, unitcost, unitcost_fy, spec, servunits)

SetB_Cost = SetB_2 %>%
  left_join(price_index, by = c("unitcost_fy" = "Fiscal_Year")) %>%
  mutate(set = "B") %>%
  rename(index_unitcost = Index)

##################################################
# Step 3: Physician cost of other APB feeitems
##################################################

msp_apb_unitcost = msp_apb_unitcost %>%
  rename(
    unitcost_fy = source_yr
  )

SetC = msp1 %>%
  filter(feeitem >= 96700 & feeitem <= 97358 & 
           !(feeitem %in% c(96801,96811,96821,96802,96812,96822,96803,96813,96823,96804,96814,96824,96805,96815,96825))) %>%
  left_join(msp_apb_unitcost, by = c("feeitem" = "code", "fy" = "unitcost_fy")) %>% #we missed a lot of feeitem here because the matching dataset does not have feeitem cost on all available year
  select (STUDYID, ServDate, feeitem, fy, unitcost, code_2, spec, servunits)

SetC_Cost = SetC %>%
  left_join(price_index, by = c("fy" = "Fiscal_Year")) %>%
  mutate(set = "C") %>%
  rename(index_unitcost = Index)

####################################################
# Step 4: Phy cost of FFS feeitems
####################################################

ffs = msp1 %>%
  filter(
    feeitem < 96700 | feeitem > 97358
  ) %>%
  select (STUDYID, ServDate, feeitem, paidamt, fy, spec, servunits)

ffs_Cost = ffs %>%
  left_join(price_index, by = c("fy" = "Fiscal_Year")) %>%
  mutate(set = "D") %>%
  rename(index_ServDate = Index)

####################################################
# Combine all the cost datasets
####################################################

allphy_cost = bind_rows(SetA_Cost, SetB_Cost, SetC_Cost, ffs_Cost)

# Calculate cost for each fiscal year
allphy_cost = allphy_cost %>%
  mutate(
    phy_cost_2022 = case_when(
      set %in% c("A","B", "C") ~ unitcost / index_unitcost * index_2022,
      set == "D" ~ paidamt / index_ServDate * index_2022
    ),
    date = as.Date(ServDate))


allphy_cost = allphy_cost %>%
  left_join(drug_bydate_cohort, by = c("STUDYID" = "STUDYID", "date" = "rx_date"))

allphy_cost2 = allphy_cost %>%
  mutate(md_spec = case_when(spec == 1 ~ "DERM",
                             spec == 44 ~ "RHEUM",
                             spec == 56 ~ "GASTRO",
                             spec == 15 ~ "INTMED",
                             spec == 8 ~ "SURG",
                             spec == 0 ~ "GP")) %>%
  mutate(cohort2 = case_when(dx == "RA" | dx == "AS" ~ "IJD",
                             dx == "PSA" | dx == "PSO" ~ "ISD",
                             dx == "CD" | dx == "UC" ~ "IBD",
                             cohort == "IAD" & dx == "MIXED" ~ "IJD",
                             cohort == "IBD" & dx == "MIXED" ~ "IBD"))



