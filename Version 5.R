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

setwd("C:/Users/mcr28/OneDrive - University of St Andrews/Documents/SES")

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
# By Education
#=========================================
# Categorizing into two groups

data <- data %>%
  mutate(edu2 = case_when(
    highest_qualification %in% c(1, 2) ~ 1,
    highest_qualification %in% c(3, 4) ~ 2,
    TRUE ~ NA_real_
  ))

# check
table(data$highest_qualification, data$edu2, useNA = "ifany")

# descriptives by education
edu_desc <- data %>%
  group_by(year, edu2) %>%
  summarise(
    cognitive   = weighted.mean(cognitive_PCA, weight, na.rm = TRUE),
    interactive = weighted.mean(interactive_PCA, weight, na.rm = TRUE),
    physical    = weighted.mean(physical_PCA, weight, na.rm = TRUE),
    n_unw = n(),
    n_w   = sum(weight, na.rm = TRUE),
    .groups = "drop"
  )


# reshaping to longer fomart

edu_long <- edu_desc %>%
  pivot_longer(
    cols = c(cognitive, interactive, physical),
    names_to = "task",
    values_to = "value"
  )

# Making edu2 a factor
edu_long$edu2 <- factor(
  edu_long$edu2,
  levels = c(1, 2),
  labels = c("Low education", "High education")
)

#  plotting

ggplot(edu_long, aes(x = year, y = value, color = edu2, linetype = edu2)) +
  geom_line(size = 1.1) +
  geom_point(size = 2) +
  facet_wrap(~ task, ncol = 1) +
  coord_cartesian(ylim = c(0.35, 0.80)) +
  scale_x_continuous(breaks = c(1997, 2001, 2006, 2012, 2017)) +
  theme_minimal() +
  theme(
    panel.spacing = unit(1.1, "lines"),
    legend.position = "right"
  )


#=========================================
# By sex
#=========================================

# factor with labels
data$sex <- factor(
  data$sex,
  levels = c(1, 2),
  labels = c("Men", "Women")
)

# descriptive
sex_desc <- data %>%
  group_by(year, sex) %>%
  summarise(
    cognitive   = weighted.mean(cognitive_PCA, weight, na.rm = TRUE),
    interactive = weighted.mean(interactive_PCA, weight, na.rm = TRUE),
    physical    = weighted.mean(physical_PCA, weight, na.rm = TRUE),
    n_unw = n(),
    n_w   = sum(weight, na.rm = TRUE),
    .groups = "drop"
  )

# reshape to long fomart
library(tidyr)

sex_long <- sex_desc %>%
  pivot_longer(
    cols = c(cognitive, interactive, physical),
    names_to = "task",
    values_to = "value"
  )
# labeling - optional
sex_long$task <- factor(
  sex_long$task,
  levels = c("cognitive", "interactive", "physical"),
  labels = c("Cognitive", "Interactive", "Physical")
)
# plotting

ggplot(sex_long,
       aes(x = year, y = value, color = sex, linetype = sex)) +
  geom_line(size = 1.1) +
  geom_point(size = 2) +
  facet_wrap(~ task, ncol = 1) +
  coord_cartesian(ylim = c(0.35, 0.80)) +
  scale_x_continuous(breaks = c(1997, 2001, 2006, 2012, 2017)) +
  labs(
    title = "Task intensity by sex",
    x = "Survey year",
    y = "Task intensity",
    color = "Sex",
    linetype = "Sex"
  ) +
  theme_minimal()



# Single plots

ggplot(filter(sex_long, task == "Interactive"),
       aes(x = year, y = value, color = sex)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Interactive task intensity by sex",
    x = "Survey year",
    y = "Interactive task intensity",
    color = "Sex"
  ) +
  theme_minimal()


# cognitive
ggplot(filter(sex_long, task == "Cognitive"),
       aes(x = year, y = value, color = sex)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "cognitive task intensity by sex",
    x = "Survey year",
    y = "cognitive task intensity",
    color = "Sex"
  ) +
  theme_minimal()


# Physical
ggplot(filter(sex_long, task == "Physical"),
       aes(x = year, y = value, color = sex)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Physical task intensity by sex",
    x = "Survey year",
    y = "Physical task intensity",
    color = "Sex"
  ) +
  theme_minimal()

###############################################################################
###---------- Part 2: Comparing task intensities across cohorts-------------###
###############################################################################

# Create birth year
data$birth_year <- data$year - data$age

# Quick check
summary(data$birth_year)
head(data)

# create birth cohorts (10 year interval)
data$cohort <- cut(
  data$birth_year,
  breaks = c(1937, 1947, 1957, 1967, 1977, 1987, 1997),
  labels = c(
    "1938–1947",
    "1948–1957",
    "1958–1967",
    "1968–1977",
    "1978–1987",
    "1988–1997"
  ),
  right = TRUE,
  include.lowest = TRUE
)

# Check results
table(data$cohort, useNA = "ifany")
head(data)

# Computing mean task intensities across cohorts

cohort_desc <- data %>%
  group_by(year, cohort) %>%
  summarise(
    cognitive   = weighted.mean(cognitive_PCA, weight, na.rm = TRUE),
    interactive = weighted.mean(interactive_PCA, weight, na.rm = TRUE),
    physical    = weighted.mean(physical_PCA, weight, na.rm = TRUE),
    n_unw = n(),
    n_w   = sum(weight, na.rm = TRUE),
    .groups = "drop"
  )
# Plotting
ggplot(cohort_desc, aes(x = year, y = cognitive, color = cohort)) +
  geom_line(size = 1.2) +
  geom_point() +
  labs(
    title = "Cognitive task intensity by cohort",
    y = "Cognitive task index",
    x = "Survey wave"
  ) +
  theme_minimal()

ggplot(cohort_desc, aes(x = year, y = interactive, color = cohort)) +
  geom_line(size = 1.2) +
  geom_point() +
  labs(
    title = "interactive task intensity by cohort",
    y = "interactive task index",
    x = "Survey wave"
  ) +
  theme_minimal()

ggplot(cohort_desc, aes(x = year, y = physical, color = cohort)) +
  geom_line(size = 1.2) +
  geom_point() +
  labs(
    title = "physical task intensity by cohort",
    y = "physical task index",
    x = "Survey wave"
  ) +
  theme_minimal()

###############################################################################
########--------------- Part 3: Regression descriptive------------------#######
###############################################################################
#Step 1: Cohort differences in task composition
#Do younger cohorts perform more cognitive/interactive tasks and fewer physical tasks?

data$sector <- factor(data$sector)  # Converting categorical variables to factors
data$edu2   <- factor(data$edu2)

lm(cognitive_PCA ~ factor(cohort) + sex + edu2 + sector, data=data , weights = weight)   # Regressions
lm(interactive_PCA ~ factor(cohort) + sex + edu2 + sector, data=data , weights = weight)
lm(physical_PCA ~ factor(cohort) + sex + edu2 + sector, data=data , weights = weight)

# Add more controls if need be

# Step 2: Lifecycle Wage Profile - Classic mincer equation

lm(log(hourly_pay) ~ age + I(age^2) + edu2 + sector + sex, data=data , weights = weight)

# Step 3: skill/task wage premia

# Do tasks affect wage differences? 
lm(log(hourly_pay) ~ cognitive_PCA +
     interactive_PCA +
     physical_PCA +
     age + I(age^2) +
     edu2 + sector + sex,
   data=data , weights = weight)

# Step 4: Do tasks change lifecycle slopes? 
lm(log(hourly_pay) ~ age + I(age^2) +
     cognitive_PCA +
     age:cognitive_PCA +
     edu2 + sector,
   data=data , weights = weight)

lm(log(hourly_pay) ~ age + I(age^2) +
     interactive_PCA +
     age:interactive_PCA +
     edu2 + sector,
   data=data , weights = weight)

lm(log(hourly_pay) ~ age + I(age^2) +
     physical_PCA +
     age:physical_PCA +
     edu2 + sector,
   data=data , weights = weight)

# Step 5: Are cohort wage differences explained by tasks?

# Model A - Without task
modelA <- lm(log(hourly_pay) ~ age + I(age^2) + factor(cohort) + sex + sector + 
               edu2, data=data , weights = weight)
# Model B - With task 
modelB <- lm(log(hourly_pay) ~ age + I(age^2) + factor(cohort) +
               cognitive_PCA + interactive_PCA + physical_PCA +
               sex + sector + edu2, data=data, weights = weight)

summary(modelA)
summary(modelB)


