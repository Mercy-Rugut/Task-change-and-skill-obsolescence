###############################################################################
# PhD in Economics Chapter One
# Project - Human capital depreciation across cohorts:A task-based approach 
# Year 2025/2026
###############################################################################

###############################################################################
###--------------------------- Milestones ----------------------------------### 
###############################################################################

# 1.Identify the variables needed
# 2.Categorize and classify accordingly(Age,education,task & birth cohorts)
# 3.Plot descriptive stats and correlations
# 4.Generate earning curves for depreciation

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

###############################################################################
###--------------- Load Skills and Employment Survey data ------------------###
###############################################################################
# Get working directory 
getwd()

setwd("C:/Users//mcr28/Documents/SES")

# Read Stata file
ses_data <- read_dta("sescombined.dta")

names(ses_data) # display all variables
View(ses_data)
###############################################################################
###------------------ Part 1: Data preparation------------------------------###
###############################################################################
#=========================================
## Variables we need
keep_vars <- ses_data[, c("dataset", "asex", "aage","bauto", "bempsta", "bemptype", 
                          "bfultimep","bhours", "bworkcat", "cusepc", "dusepc",
                          "dquals1","dquals2","dquals3", "dpaidwk", "esector", "jchange","kethnic",
                          "region", "gpayp","gwtall","eproprt","pid",
                          "cdetail","cpeople","cteach","cspeech","cpersuad","cselling","ccaring",
                          "cteamwk","clisten","cstrengt","cstamina","chands","ctools","cproduct",
                          "cspecial","corgwork","cfaults","ccause","csolutn",        
                          "canalyse","cplanme","cplanoth","cmytime",        
                          "cahead","cread","cshort","clong","cwrite","cwritesh",       
                          "cwritelg","ccalca","cpercent","cstats")]

names(keep_vars)
#dataset - year of survey
#bauto - whether job involves use of computer or automated equipment
#bempsta/bemptype - Employee or self-employed (filter only employees)
#bperm - permanent or temporary employee
#bfultimep -  full/part time
#bhours - Hours worked per week
#bworkcat - workplace size bands(Number of employees at workplace:bworkno,bmanywrk)
#dusepc - Complexity level of computer use
#cusepc - importance of computer use
#dquals1 - highest qualification
#dpaidwk - years of experience since leaving full time education(confirm with exp)
#esector - private/public/charity
#jchange - skill use change
#kethnic - ethnic origin
#gpayp - gross hourly pay
#gwtall - weights (60 yrs)
#eproprt - what proportion of employees work with computerised or automated equipment(Digital workplace)
#pid - personal identification number
#cdetail to cforlan - task variables

#=========================================
# Filtering for Years & Regions
selected_data1 <- keep_vars %>%
  filter(!region %in% c(12, 13),        # highlands and islands & Northern Ireland from 2006
         !dataset %in% c(1986, 1992),   # Filter out years without task data
         aage >= 20 & aage <= 59)       # Keep ages 20-59

#=========================================
# Sort employment type for 1997 & 2001
selected_data1$bempsta[is.na(selected_data1$bempsta)] <- selected_data1$bemptype[is.na(selected_data1$bempsta)]
selected_data1 <- selected_data1 %>%
  filter(bempsta %in% 1)              #include only employees

#=========================================
# Adding 1997 qualification and grouping into 4 categories

selected_data1$qualification <- NA_integer_

selected_data1$qualification[selected_data1$dquals1 %in% c(1, 2, 3, 5, 6)]          <- 1
selected_data1$qualification[selected_data1$dquals1 %in% c(4, 7, 8, 9, 10, 11, 22)] <- 2
selected_data1$qualification[selected_data1$dquals1 %in% c(12, 13, 14, 15, 16)]     <- 3
selected_data1$qualification[selected_data1$dquals1 %in% c(17, 18, 19, 20, 21)]     <- 4


# table(selected_data1$qualification)


attr(selected_data1$qualification, "labels") <- c(
  "1 = lower secondary",
  "2 = upper Secondary",
  "3 = higher education sub-degree",
  "4 = first/higher degree"
)

# attr(selected_data1$qualification, "labels")

# categorizing using second  highest qualification
selected_data1 %>%
  filter(
    dquals1 %in% c(23, 24, 28, 31, 32),
    dquals2 %in% 1:22 )

# lower secondary
selected_data1$qualification[
  selected_data1$dquals1 %in% c(23, 24, 28, 31, 32) &
    selected_data1$dquals2 %in% c(1, 2, 3, 5, 6) 
] <- 1

# upper secondary
selected_data1$qualification[
  selected_data1$dquals1 %in% c(23, 24, 28, 31, 32) &
    selected_data1$dquals2 %in% c(4, 7, 8, 9, 10, 11, 22)
] <- 2

# higher education sub-degree
selected_data1$qualification[
  selected_data1$dquals1 %in% c(23, 24, 28, 31, 32) &
    selected_data1$dquals2 %in% c(12, 13, 14, 15, 16) 
] <- 3

# first/higher degree
selected_data1$qualification[
  selected_data1$dquals1 %in% c(23, 24, 28, 31, 32) &
    selected_data1$dquals2 %in% c(17, 18, 19, 20, 21) 
] <- 4

# table(selected_data1$qualification)

#=========================================
# Adding 1997
ses_1997 <- read_dta("ses1997.dta")


ses_1997$qualification <- NA_integer_

ses_1997$qualification[ses_1997$p12a00 %in% c(1, 2, 3, 5)]         <- 1
ses_1997$qualification[ses_1997$p12a00 %in% c(4, 6, 7, 8, 9, 19)]  <- 2
ses_1997$qualification[ses_1997$p12a00 %in% c(10, 11, 12, 13, 14)] <- 3
ses_1997$qualification[ses_1997$p12a00 %in% c(15, 16, 17, 18)]     <- 4

# table(ses_1997$qualification)

# categorizing codes 20 & 21
ses_1997 %>%
  filter(
    p12a00 %in% c(20, 21),
    p12a01 %in% 1:19)

# lower secondary
ses_1997$qualification[
  ses_1997$p12a00 %in% c(20, 21) &
    ses_1997$p12a01 %in% c(1, 2, 3, 5)
] <- 1

# upper secondary
ses_1997$qualification[
  ses_1997$p12a00 %in% c(20, 21) &
    ses_1997$p12a01 %in% c(4, 6, 7, 8, 9, 19)
] <- 2

# higher education sub-degree
ses_1997$qualification[
  ses_1997$p12a00 %in% c(20, 21) &
  ses_1997$p12a01 %in% c(10, 11, 12, 13, 14)
] <- 3

# first/higher degree
ses_1997$qualification[
  ses_1997$p12a00 %in% c(20, 21) &
    ses_1997$p12a01 %in% c(15, 16, 17, 18) 
] <- 4

# table(ses_1997$qualification)


# categorizing codes 20 & 21 using third highest qualification
ses_1997 %>%
  filter(
    p12a00 %in% c(20, 21),
    !(p12a01 %in% 1:19),
    p12a02 %in% c(1:19))

# lower secondary
ses_1997$qualification[
  ses_1997$p12a00 %in% c(20, 21) &
    !(ses_1997$p12a01 %in% c(1:19)) &
    ses_1997$p12a02 %in% c(1, 2, 3, 5)
] <- 1

# upper secondary
ses_1997$qualification[
  ses_1997$p12a00 %in% c(20, 21) &
    !(ses_1997$p12a01 %in% c(1:19)) &
    ses_1997$p12a02 %in% c(4, 6, 7, 8, 9, 19)
] <- 2

# higher education sub-degree
ses_1997$qualification[
  ses_1997$p12a00 %in% c(20, 21) &
    !(ses_1997$p12a01 %in% c(1:19)) &
    ses_1997$p12a02 %in% c(10, 11, 12, 13, 14)
] <- 3

# first/higher degree
ses_1997$qualification[
  ses_1997$p12a00 %in% c(20, 21) &
    !(ses_1997$p12a01 %in% c(1:19)) &
    ses_1997$p12a02 %in% c(15, 16, 17, 18) 
] <- 4

# table(ses_1997$qualification)

# combining 1997 qualification to selected_data1

qual_1997 <- ses_1997[, c("serial", "qualification")]

selected_data1 <- merge(
  selected_data1,
  qual_1997,
  by.x = "pid",
  by.y = "serial",
  all.x = TRUE
)

selected_data1$qualification.x <- ifelse(
  selected_data1$dataset == 1997 & !is.na(selected_data1$qualification.y),
  selected_data1$qualification.y,
  selected_data1$qualification.x
)

# Sanity check
selected_data1[selected_data1$pid %in% ses_1997$serial & 
                 selected_data1$dataset == 1997,
               c("pid", "dataset", "qualification.x")][1:10, ]


table(selected_data1$dataset, selected_data1$qualification.x)

#=========================================

#Task classification

#=========================================
# Identifying task variables
task_vars <- c("cdetail","cpeople","cteach","cspeech","cpersuad","cselling","ccaring",
               "cteamwk","clisten","cstrengt","cstamina","chands","ctools","cproduct",
               "cspecial","corgwork","cfaults","ccause","csolutn",        
               "canalyse","cplanme","cplanoth","cmytime",        
               "cahead","cread","cshort","clong","cwrite","cwritesh",       
               "cwritelg","ccalca","cpercent","cstats") 

# Switching codes into weights
recode_task_weights <- function(x) {
  recode_map <- c(
    `1` = 4,  # Essential
    `2` = 3,  # Very important
    `3` = 2,  # Fairly important
    `4` = 1,  # Not very important
    `5` = 0   # Not at all important
  )
  recode_map[as.character(x)]
}

#Applying the weights to all task variables
selected_data1[paste0(task_vars, "_w")] <-
  lapply(selected_data1[task_vars], recode_task_weights)

#checks
table(selected_data1$cforlan)
table(selected_data1$cforlan_w)

#=========================================
# Task group classification

cognitive <- c("ccause","csolutn","canalyse","clong","cwritelg","cstats",
                "cspecial","corgwork","cproduct","cdetail","cfaults",
               "cplanme","cplanoth","cmytime","cahead","cread","cshort",
               "cwrite","cwritesh","ccalca","cpercent")

interactive <- c(,"cpeople","cteach","cspeech","cpersuad","cselling",
                      "ccaring","cteamwk","clisten")

physical <- c("ctools","cstrengt","cstamina","chands")

# Weighted task group
cognitive_w <- paste0(cognitive, "_w")
interactive_w <- paste0(interactive, "_w")
physical_w <- paste0(physical, "_w")
task_vars_w<- paste0(task_vars, "_w")

# Share of weighted task measure
# Cognitive
selected_data1$cognitive_share <- with(selected_data1,
                                       rowSums(selected_data1[cognitive_w], na.rm = TRUE) /
                                         rowSums(selected_data1[task_vars_w], na.rm = TRUE) * 100)

# Interactive
selected_data1$interactive_share <- with(selected_data1,
                                        rowSums(selected_data1[interactive_w], na.rm = TRUE) /
                                          rowSums(selected_data1[task_vars_w], na.rm = TRUE) * 100)


# Interactive
selected_data1$physical_share <- with(selected_data1,
                                         rowSums(selected_data1[physical_w], na.rm = TRUE) /
                                           rowSums(selected_data1[task_vars_w], na.rm = TRUE) * 100)







#=========================================
# Selecting variables and renaming for easy identification

#=========================================
names(selected_data1)

# Reselecting the variables
new_vars <- selected_data1[, c("dataset", "asex", "aage","bauto", "bempsta", "bfultimep",
                               "bhours", "bworkcat", "cusepc", "dusepc", "dpaidwk","esector",
                               "jchange","kethnic","region", "gpayp","gwtall","eproprt",
                               "pid","qualification.x","knowledge_share","experience_share" )]

#renaming for easy identification
new_vars <- new_vars %>%
  rename(
    year               = dataset,
    sex                = asex,
    age                = aage,
    comp_use           = bauto,
    employment         = bempsta,
    employment_type    = bfultimep,
    hours_worked       = bhours,
    employees_workplace= bworkcat,
    complexity_comp    = dusepc,
    importance_comp    = cusepc,
    highest_qualification = qualification.x,
    experience         = dpaidwk,
    sector             = esector,
    skill_change       = jchange,
    ethnicity          = kethnic,
    hourly_pay         = gpayp,
    weight             = gwtall,
    employees_comp     = eproprt
  )

#=========================================
# saving as csv for future use
write.csv(new_vars, "ses_data1.csv", row.names = FALSE)

new_vars


# categorize age
# Descriptive stats
# Long term trend in digitalisation

