install.packages("ipumsr")
install.packages("scales")
install.packages("ggplot2")

library(ggplot2)
library(scales)
library('ipumsr')
library(dplyr)

getwd()
ddi <- read_ipums_ddi("usa_00008.xml")
data <- read_ipums_micro(ddi)

summary(data)
nrow(data)
ncol(data)

names(data)

#Trimmed Dataset
data_trimmed <- data[c("YEAR","HHTYPE","PUMA","OWNERSHP","OWNERSHPD",
                       "MORTAMT1","MORTAMT2","RENTGRS","HHINCOME","BEDROOMS",
                       "RELATED", "SEX", "AGE", "MARST","RACE","HISPAN",
                       "EDUC","EMPSTAT","UHRSWORK","INCTOT")]
summary(data_trimmed)                     

#Change column values
#OWNERSHP column mutation
data_trimmed <- data_trimmed %>%
    mutate(
        OWNERSHP = case_when(
            OWNERSHP == 0 ~ "N/A",
            OWNERSHP == 1 ~ "Owned or being bought (loan)",
            OWNERSHP == 2 ~ "Rented",
            TRUE ~ as.character(OWNERSHP)
        )
    )
#OWNERSHPD column mutation
data_trimmed <- data_trimmed %>%
    mutate(
        OWNERSHPD = case_when(
            OWNERSHPD == 0  ~ "N/A",
            OWNERSHPD == 10 ~ "Owned or being bought",
            OWNERSHPD == 11 ~ "Check mark (owns?)",
            OWNERSHPD == 12 ~ "Owned free and clear",
            OWNERSHPD == 13 ~ "Owned with mortgage or loan",
            OWNERSHPD == 20 ~ "Rented",
            OWNERSHPD == 21 ~ "No cash rent",
            OWNERSHPD == 22 ~ "With cash rent",
            TRUE ~ as.character(OWNERSHPD)
        )
    )
#RELATED
data_trimmed <- data_trimmed %>%
    mutate(
        RELATED = case_when(
            RELATED == 101 ~ "Head/Householder",
            RELATED == 201 ~ "Spouse",
            RELATED == 202 ~ "2nd/3rd Wife (Polygamous)",
            RELATED == 301 ~ "Child",
            RELATED == 302 ~ "Adopted Child",
            RELATED == 303 ~ "Stepchild",
            RELATED == 304 ~ "Adopted, n.s.",
            RELATED == 401 ~ "Child-in-law",
            RELATED == 402 ~ "Step Child-in-law",
            RELATED == 501 ~ "Parent",
            RELATED == 502 ~ "Stepparent",
            RELATED == 601 ~ "Parent-in-Law",
            RELATED == 602 ~ "Stepparent-in-law",
            RELATED == 701 ~ "Sibling",
            RELATED == 702 ~ "Step/Half/Adopted Sibling",
            RELATED == 801 ~ "Sibling-in-Law",
            RELATED == 802 ~ "Step/Half Sibling-in-law",
            RELATED == 901 ~ "Grandchild",
            RELATED == 902 ~ "Adopted Grandchild",
            RELATED == 903 ~ "Step Grandchild",
            RELATED == 904 ~ "Grandchild-in-law",
            RELATED == 1001 ~ "Other Relatives",
            RELATED == 1011 ~ "Grandparent",
            RELATED == 1012 ~ "Step Grandparent",
            RELATED == 1013 ~ "Grandparent-in-law",
            RELATED == 1021 ~ "Aunt or Uncle",
            RELATED == 1022 ~ "Aunt/Uncle-in-law",
            RELATED == 1031 ~ "Nephew/Niece",
            RELATED == 1032 ~ "Nephew/Niece-in-law",
            RELATED == 1033 ~ "Step/Adopted Nephew/Niece",
            RELATED == 1034 ~ "Grand Niece/Nephew",
            RELATED == 1041 ~ "Cousin",
            RELATED == 1042 ~ "Cousin-in-law",
            RELATED == 1051 ~ "Great Grandchild",
            RELATED == 1061 ~ "Other relatives nec",
            RELATED == 1100 ~ "Partner/Friend/Visitor",
            RELATED == 1110 ~ "Partner/Friend",
            RELATED == 1111 ~ "Friend",
            RELATED == 1112 ~ "Partner",
            RELATED == 1113 ~ "Partner/Roommate",
            RELATED == 1114 ~ "Unmarried Partner",
            RELATED == 1115 ~ "Housemate/Roommate",
            RELATED == 1120 ~ "Relative of Partner",
            RELATED == 1130 ~ "Concubine/Mistress",
            RELATED == 1131 ~ "Visitor",
            RELATED == 1132 ~ "Companion and family of companion",
            RELATED == 1139 ~ "Allocated partner/friend/visitor",
            RELATED == 1200 ~ "Other non-relatives",
            RELATED == 1201 ~ "Roomers/boarders/lodgers",
            RELATED == 1202 ~ "Boarders",
            RELATED == 1203 ~ "Lodgers",
            RELATED == 1204 ~ "Roomer",
            RELATED == 1205 ~ "Tenant",
            RELATED == 1206 ~ "Foster child",
            RELATED == 1210 ~ "Employees",
            RELATED == 1211 ~ "Servant",
            RELATED == 1212 ~ "Housekeeper",
            RELATED == 1213 ~ "Maid",
            RELATED == 1214 ~ "Cook",
            RELATED == 1215 ~ "Nurse",
            RELATED == 1216 ~ "Other probable domestic employee",
            RELATED == 1217 ~ "Other employee",
            RELATED == 1219 ~ "Relative of employee",
            RELATED == 1221 ~ "Military",
            RELATED == 1222 ~ "Students",
            RELATED == 1223 ~ "Members of religious orders",
            RELATED == 1230 ~ "Other non-relatives",
            RELATED == 1239 ~ "Allocated other non-relative",
            RELATED == 1240 ~ "Roomers/boarders/lodgers and foster children",
            RELATED == 1241 ~ "Roomers/boarders/lodgers",
            RELATED == 1242 ~ "Foster children",
            RELATED == 1250 ~ "Employees",
            RELATED == 1251 ~ "Domestic employees",
            RELATED == 1252 ~ "Non-domestic employees",
            RELATED == 1253 ~ "Relative of employee",
            RELATED == 1260 ~ "Other non-relatives (1990 includes employees)",
            RELATED == 1270 ~ "Non-inmate 1990",
            RELATED == 1281 ~ "Head of group quarters",
            RELATED == 1282 ~ "Employees of group quarters",
            RELATED == 1283 ~ "Relative of head/staff/employee in group quarters",
            RELATED == 1284 ~ "Other non-inmate 1940–1959",
            RELATED == 1291 ~ "Military",
            RELATED == 1292 ~ "College dormitories",
            RELATED == 1293 ~ "Residents of rooming houses",
            RELATED == 1294 ~ "Other non-inmate 1980 (includes employees/non-inmates)",
            RELATED == 1295 ~ "Other non-inmates 1960–1970 (includes employees)",
            RELATED == 1296 ~ "Non-inmates in institutions",
            RELATED == 1301 ~ "Institutional inmates",
            RELATED == 9996 ~ "Unclassifiable",
            RELATED == 9997 ~ "Unknown",
            RELATED == 9998 ~ "Illegible",
            RELATED == 9999 ~ "Missing",
            TRUE ~ as.character(RELATED)
        )
    )
#MARST mutation
data_trimmed <- data_trimmed %>%
    mutate(
        MARST = case_when(
            MARST == 1 ~ "Married, spouse present",
            MARST == 2 ~ "Married, spouse absent",
            MARST == 3 ~ "Separated",
            MARST == 4 ~ "Divorced",
            MARST == 5 ~ "Widowed",
            MARST == 6 ~ "Never married / single",
            MARST == 9 ~ "Blank / missing",
            TRUE ~ as.character(MARST)
        )
    )
#RACE mutation
data_trimmed <- data_trimmed %>%
    mutate(
        RACE = case_when(
            RACE == 1 ~ "White",
            RACE == 2 ~ "Black / African American",
            RACE == 3 ~ "American Indian or Alaska Native",
            RACE == 4 ~ "Chinese",
            RACE == 5 ~ "Japanese",
            RACE == 6 ~ "Other Asian or Pacific Islander",
            RACE == 7 ~ "Other race, nec",
            RACE == 8 ~ "Two major races",
            RACE == 9 ~ "Three or more major races",
            TRUE ~ as.character(RACE)
        )
    )
#HISP mutation
data_trimmed <- data_trimmed %>%
    mutate(
        HISPAN = case_when(
            HISPAN == 0 ~ "Not Hispanic",
            HISPAN == 1 ~ "Mexican",
            HISPAN == 2 ~ "Puerto Rican",
            HISPAN == 3 ~ "Cuban",
            HISPAN == 4 ~ "Other Hispanic",
            HISPAN == 9 ~ "Not Reported",
            TRUE ~ as.character(HISPAN)
        )
    )
#EDUC mutation
data_trimmed <- data_trimmed %>%
    mutate(
        EDUC = case_when(
            EDUC == 0 ~ "N/A or no schooling",
            EDUC == 1 ~ "Nursery school to grade 4",
            EDUC == 2 ~ "Grade 5,6,7, or 8",
            EDUC == 3 ~ "Grade 9",
            EDUC == 4 ~ "Grade 10",
            EDUC == 5 ~ "Grade 11",
            EDUC == 6 ~ "Grade 12",
            EDUC == 7 ~ "1 year of college",
            EDUC == 8 ~ "2 years of college",
            EDUC == 9 ~ "3 years of college",
            EDUC == 10 ~ "4 years of college",
            EDUC == 11 ~ "5+ years of college",
            EDUC == 99 ~ "Missing",
            TRUE ~ as.character(EDUC)
        )
    )
#EMPSTAT mutation
data_trimmed <- data_trimmed %>%
    mutate(
        EMPSTAT = case_when(
            EMPSTAT == 0 ~ "N/A",
            EMPSTAT == 1 ~ "Employed",
            EMPSTAT == 2 ~ "Unemployed",
            EMPSTAT == 3 ~ "Not in labor force",
            EMPSTAT == 9 ~ "Unknown/Illegible",
            TRUE ~ as.character(EMPSTAT)
        )
    )
#SEX mutation
data_trimmed <- data_trimmed %>%
    mutate(
        SEX = case_when(
            SEX == 1 ~ "Male",
            SEX == 2 ~ "Female",
            SEX == 9 ~ "Missing/blank",
            TRUE ~ as.character(SEX)
        )
    )

#find second year and filter ----------------------------------------------------------------------
second_year = 2022

data_second_year <- data_trimmed %>%
  filter(YEAR == second_year)

#compare owners vs rent in 2022---------------------------------------------------------------
rentown_table  <- table(data_second_year$OWNERSHP, useNA = "ifany")
rentown_propTable <- prop.table(rentown_table)
round(rentown_propTable, 3)

#Observations-----------------------------------------------------------
second_year
nrow(data_second_year)
table(data_second_year$YEAR)

#Distribution table for 2022----------------------------------------------------------------------
own_dist <- as.data.frame(rentown_table) %>%
  rename(OWNERSHP = Var1, n = Freq) %>%
  mutate(prop = n / sum(n))
own_dist

#Summaries of age, income, and rent in 2022---------------------------------------
summary(data_second_year$AGE)
summary(data_second_year$HHINCOME)
summary(data_second_year$RENTGRS)

#Clean income and rent variables---------------------------------------
data_second_year_clean <- data_second_year %>%
  mutate(
    #Treat negative and top-coded incomes as missing
    HHINCOME_clean = ifelse(HHINCOME <= 0 | HHINCOME >= 9999999,
                            NA, HHINCOME),
    
    #Treat zero rent as missing for "rent amount" distribution
    #(we'll handle no-cash-rent separately using OWNERSHPD)
    RENTGRS_clean = ifelse(RENTGRS <= 0, NA, RENTGRS)
  )


#Rent burden calculations------------------------------------------------------------
data_second_year_burden <- data_second_year_clean %>%
  mutate(
    #1. Simple tenure classification
    tenure_simple = case_when(
      OWNERSHP == "Rented" ~ "Renter",
      OWNERSHP == "Owned or being bought (loan)" ~ "Owner",
      OWNERSHPD == "Owned free and clear"~ "Owner",
      OWNERSHPD == "Owned with mortgage or loan" ~ "Owner",
      TRUE ~ "Other / N.A."
    ),
    
    #2. Rent used for burden calculation
        #With cash rent, use RENTGRS_clean
        #No cash rent, set housing cost to 0 
        #Everyone else  set to NA
    rent_for_burden = case_when(
      tenure_simple == "Renter" & OWNERSHPD == "With cash rent" & !is.na(RENTGRS_clean) ~
        as.numeric(RENTGRS_clean),
      
      tenure_simple == "Renter" & OWNERSHPD == "No cash rent" ~
        0,
      
      #If OWNERSHPD is just Rented, use RENTGRS_clean 
      tenure_simple == "Renter" & OWNERSHPD == "Rented" & !is.na(RENTGRS_clean) ~
        as.numeric(RENTGRS_clean),
      
      TRUE ~ NA_real_
    ),
    
    #3. Rent burden = annual rent / annual household income
    rent_burden = ifelse(
      tenure_simple == "Renter" &
        !is.na(rent_for_burden) &
        !is.na(HHINCOME_clean) &
        HHINCOME_clean > 0,
      (rent_for_burden * 12) / HHINCOME_clean,
      NA_real_
    ),
    
    #4. Owner cost used for burden calculation
    owner_for_burden = case_when(
      tenure_simple == 'Owner' &
        (!is.na(MORTAMT1) | !is.na(MORTAMT2)) ~
        ifelse(is.na(MORTAMT1), 0, as.numeric(MORTAMT1)) + 
        ifelse(is.na(MORTAMT2), 0, as.numeric(MORTAMT2)),
      TRUE ~ NA_real_
    ),

    #5. Owner Burden = annual mortgage payments/ annual household income
    owner_burden = ifelse(
      tenure_simple == 'Owner' &
        !is.na(owner_for_burden) &
        !is.na(HHINCOME_clean) &
        HHINCOME_clean > 0,
      (owner_for_burden * 12 / HHINCOME_clean),
      NA_real_
    )
    
  )

summary(data_second_year_burden$rent_burden)


#Categories for renters------------------------------------------------------------------
renters_burden_cat <- data_second_year_burden %>%
  filter(
    tenure_simple == "Renter",
    !is.na(rent_burden),
    rent_burden >= 0,
    rent_burden < 3      # trim extremely high outliers (300%+)
  ) %>%
  mutate(
    rent_burden_cat = case_when(
      rent_burden < 0.30 ~ "< 30%",
      rent_burden < 0.50 ~ "30–49%",
      TRUE               ~ "50%+"
    ),
    rent_burden_cat = factor(
      rent_burden_cat,
      levels = c("< 30%", "30–49%", "50%+")
    )
  )

renters_burden_dist <- renters_burden_cat %>%
  count(rent_burden_cat) %>%
  mutate(prop = n / sum(n))


#Categories for owners------------------------------------------------------------------
owners_burden_cat <- data_second_year_burden %>%
  filter(
    tenure_simple == "Owner",
    !is.na(owner_burden),
    owner_burden >= 0,
    owner_burden < 3      # trim extremely high outliers (300%+)
  ) %>%
  mutate(
    owner_burden_cat = case_when(
      owner_burden < 0.30 ~ "< 30%",
      owner_burden < 0.50 ~ "30–49%",
      TRUE               ~ "50%+"
    ),
    owner_burden_cat = factor(
      owner_burden_cat,
      levels = c("< 30%", "30–49%", "50%+")
    )
  )
owners_burden_dist <- owners_burden_cat %>%
  count(owner_burden_cat) %>%
  mutate(prop = n / sum(n))

owners_burden_dist


#histogram for renters--------------------------------------------------------------------
ggplot(
  renters_burden_cat,              # renters with valid rent_burden
  aes(x = rent_burden)
) +
  geom_histogram(bins = 30, na.rm = TRUE) +
  
  # vertical dashed lines at 30% and 50%
  geom_vline(xintercept = 0.30, linetype = "dashed") +
  geom_vline(xintercept = 0.50, linetype = "dashed") +

  # explicit ticks + labels so we never get NA
  scale_x_continuous(
    breaks  = c(0, 0.30, 0.50, 1.00),
    labels  = c("0%", "30%", "50%", "100%"),
    limits  = c(0, 1.0)    
  ) +
  labs(
    title = paste("Rent Burden Distribution (Young Adult Renters -", second_year, ")"),
    x = "Rent Burden (Rent / Income, %)",
    y = "Number of Renters"
  )
#histogram for owners--------------------------------------------------------------------
ggplot(
  owners_burden_cat,              # renters with valid rent_burden
  aes(x = owner_burden)
) +
  geom_histogram(bins = 30, na.rm = TRUE) +
  
  # vertical dashed lines at 30% and 50%
  geom_vline(xintercept = 0.30, linetype = "dashed") +
  geom_vline(xintercept = 0.50, linetype = "dashed") +
  
  # explicit ticks + labels so we never get NA
  scale_x_continuous(
    breaks  = c(0, 0.30, 0.50, 1.00),
    labels  = c("0%", "30%", "50%", "100%"),
    limits  = c(0, 1.0)     
  ) +
  labs(
    title = paste("Owner Mortgage Burden Distribution (Young Adult Owners With/Without Mortgage -", second_year, ")"),
    x = "Owner Housing Burden ((Mortgage 1 + Mortgage 2 )/ Income, %)",
    y = "Number of Owners"
  )

 