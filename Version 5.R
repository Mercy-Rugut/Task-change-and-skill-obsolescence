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
# weighted average task intensity for the different age groups for each year

age_desc <- data %>%
  group_by(year, age_group) %>%
  summarise(
    cognitive   = weighted.mean(cognitive_PCA, weight, na.rm = TRUE),
    interactive = weighted.mean(interactive_PCA, weight, na.rm = TRUE),
    physical    = weighted.mean(physical_PCA, weight, na.rm = TRUE),
    n_unw = n(),
    n_w   = sum(weight, na.rm = TRUE),
    .groups = "drop"
  )
# Plotting
ggplot(age_desc, aes(x = year, y = cognitive, color = age_group)) +
  geom_line(size = 1.2) +
  geom_point() +
  labs(
    title = "Cognitive task intensity by age group",
    y = "Cognitive task index (0–1)",
    x = "Survey wave"
  ) +
  theme_minimal()

ggplot(age_desc, aes(x = year, y = interactive, color = age_group)) +
  geom_line(size = 1.2) +
  geom_point() +
  labs(
    title = "interactive task intensity by age group",
    y = "interactive task index (0–1)",
    x = "Survey wave"
  ) +
  theme_minimal()

ggplot(age_desc, aes(x = year, y = physical, color = age_group)) +
  geom_line(size = 1.2) +
  geom_point() +
  labs(
    title = "physical task intensity by age group",
    y = "physical task index (0–1)",
    x = "Survey wave"
  ) +
  theme_minimal()

# All ages
overall_desc <- data %>%
  group_by(year) %>%
  summarise(
    cognitive_all   = weighted.mean(cognitive_PCA, weight, na.rm = TRUE),
    interactive_all = weighted.mean(interactive_PCA, weight, na.rm = TRUE),
    physical_all   = weighted.mean(physical_PCA, weight, na.rm = TRUE),
    n_unw = n(),
    n_w   = sum(weight, na.rm = TRUE),
    .groups = "drop"
  )

# Plotting
overall_long <- overall_desc %>%
  pivot_longer(
    cols = c(cognitive_all, interactive_all, physical_all),
    names_to = "task",
    values_to = "value"
  )

ggplot(overall_long, aes(x = year, y = value, color = task)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  coord_cartesian(ylim = c(0.42, 0.72)) +
  labs(
    title = "Overall task intensity trends (all ages)",
    x = "Survey year",
    y = "Task intensity",
    color = "Task type"
  ) +
  theme_minimal()

#=========================================

