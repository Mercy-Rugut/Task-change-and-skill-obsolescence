###############################################################################
# PhD in Economics Chapter One
# Project - Human capital depreciation across cohorts:A task-based approach 
# Year 2025/2026
###############################################################################

###############################################################################
###--------------------------- Milestones ----------------------------------### 
###############################################################################

# 1.Average task measure for different age groups over the period of analysis
# 2.Average task growth rate relative to year 1997
# 3.Disagregate by education level across age groups
#
###############################################################################
###------------------------ variables description --------------------------### 
###############################################################################
#year - year of survey
#comp_use - whether job involves use of computer or automated equipment
#employment - Employee or self-employed (filter only employees)
#employment_type -  full/part time
#hours_worked - Hours worked per week
#employees_workplace - workplace size bands(Number of employees at workplace:bworkno,bmanywrk)
#complexity_comp - Complexity level of computer use
#importance_comp - importance of computer use
#highest_qualification - highest qualification
#experience - years of experience since leaving full time education(confirm with exp)
#sector - private/public/charity
#skill_change - skill use change
#ethnicity - ethnic origin
#hourly_pay - gross hourly pay
#weight - weights (60 yrs)
#employees_comp - what proportion of employees work with computerised or automated equipment(Digital workplace)
#pid - personal identification number
###############################################################################
###--------------------- Load/install required packages --------------------### 
###############################################################################
# Install and load haven
if ("haven" %in% installed.packages()){
  library(haven)
} else{
  install.packages("haven")
  library(haven)
}
# Install and load dplyr
if ("dplyr" %in% installed.packages()){
  library(dplyr)
} else{
  install.packages("dplyr")
  library(dplyr)
}
# Install and load tidyr
if ("tidyr" %in% installed.packages()){
  library(tidyr)
} else{
  install.packages("tidyr")
  library(tidyr)
}
# Install and load ggplot2
if ("ggplot2" %in% installed.packages()){
  library(ggplot2)
} else{
  install.packages("ggplot2")
  library(ggplot2)
}
###############################################################################
###--------------- Load SES data--------------------------------------------###
###############################################################################
# Get working directory 
getwd()

setwd("C:/Users//mcr28/Documents/SES")

# Read csv file
data <- read.csv("ses_data1.csv")
names(data) # display all variables
View(data)
###############################################################################
###-------- Part 1: Average task measure for different age groups-----------###
###############################################################################
#=========================================
# Exploratory data analysis
#=========================================
# Excluding highest qualification and weight NAs 
data <- data %>%
  filter(
    !is.na(highest_qualification),
    !is.na(weight))
# checking variables with NAs
data %>%
  summarise(across(everything(), ~ sum(is.na(.))))

#=========================================
# Age group classification
data$age_group <- with(data, ifelse(
  age >= 20 & age <= 29, "20–29",
  ifelse(age >= 30 & age <= 39, "30–39",
         ifelse(age >= 40 & age <= 49, "40–49",
                ifelse(age >= 50 & age <= 59, "50–59", NA)
         )
  )
))
data$age_group <- factor(
  data$age_group,
  levels = c("20–29", "30–39", "40–49", "50–59")
)

data$age_group_code <- as.integer(data$age_group)   #numeric age group codes

#=========================================
# weighted average task measure for the different age groups for each year

task_long <- data %>%
  pivot_longer(
    cols = c(knowledge_share, experience_share),
    names_to = "task_category",
    values_to = "task_share"
  )

task_weighted_means <- task_long %>%
  group_by(year, age_group, task_category) %>%
  summarise(
    weighted_task_share = weighted.mean(task_share, weight, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

# All ages
task_weighted_all_ages <- task_long %>%
  group_by(year, task_category) %>%
  summarise(
    weighted_task_share = weighted.mean(task_share, weight, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

#=========================================
# Computing task growth rates

# Weighted mean task share by year x age group x task category
task_age_year <- task_long %>%
  group_by(year, age_group, task_category) %>%
  summarise(
    T_bar = weighted.mean(task_share, weight, na.rm = TRUE),
    .groups = "drop"
  )

# Extracting the 1997 baseline (T_bar,1997)
base_1997 <- task_age_year %>%
  filter(year == 1997) %>%
  select(age_group, task_category, T_bar_1997 = T_bar)

# Computing TGR relative to 1997 
task_TGR <- task_age_year %>%
  left_join(base_1997, by = c("age_group", "task_category")) %>%
  mutate(
    TGR_pct = ((T_bar - T_bar_1997) / T_bar_1997) * 100
  ) %>%
  arrange(task_category, age_group, year)

task_TGR
