# Install and load haven
install.packages("haven")
library(haven)

setwd("C:/Users/Dell 7320/OneDrive - University of Strathclyde/Documents/SES")

# Read Stata file
ses_data <- read_dta("sescombined.dta")

names(ses_data) # display all variables
View(ses_data["region"]) # view a variable within the dataset
table(ses_data$region)
attr(ses_data$region, "labels")

table(ses_data$dataset, ses_data$cpercent) # subset per year
table(ses_data$dataset, ses_data$bempsta)
attr(ses_data$bemptype, "labels")

summary(ses_data$aage)

library(dplyr)

ses_data %>%
  group_by(dataset) %>%
  summarise(
    n = n(),
    mean_age = mean(aage, na.rm = TRUE),
    sd_age   = sd(aage, na.rm = TRUE),
    min_age  = min(aage, na.rm = TRUE),
    max_age  = max(aage, na.rm = TRUE)
  )



ses_1997 %>%
  filter(b4 == 1)

ses_1997_option1 <- ses_1997 %>%
  dplyr::filter(b4 == 1)


table(ses_1997$b4)

table(ses_1997$qualification)




ses_1997 <- read_dta("ses1997.dta")





table(ses_1997$p12a00)
attr(ses_1997$p12a00, "labels")


table(ses_data$dataset, ses_data$region)



ses_data <- ses_data %>%
  mutate(
    sknumber_factor = as_factor(sknumber)   # preserves value labels!
  )






View("gwtall")

View(ses_data["gwt65"])

View(ses_data["bquals"])
View("bquals")

table(ses_data$bjobs)
attr(ses_data$bfultimep, "labels")


table(ses_data$bempsta, ses_data$bemptype, useNA = "ifany")


table(selected_data1$dataset)
attr(ses_data$bwrkcatp, "labels")


table(ses_data$eproprt)
attr(ses_data$eproprt, "labels")


table(ses_data$dquals1=24)
attr(ses_data$dquals1, "labels")






ses_1997 <- read_dta("ses1997.dta")


table(ses_1997$p12a00)
attr(ses_1997$p12a00, "labels")

table(selected_data1$dataset, selected_data1$dquals1)





View(
  selected_data1 %>% 
    filter(dquals1 == 23)
)








library(dplyr)

selected_data1 %>%
  filter(dquals1 == 41)

table(ses_data$skcimp)
attr(ses_data$skcimp, "labels")
table(ses_data$cusepc)
attr(ses_data$cusepc, "labels")

table(ses_data$dataset, ses_data$cusepc)
table(ses_data$dataset, ses_data$skcimp)


table(selected_data1$dquals1)


# Checking unique identifiers in the combined and individual datasets
# subsetting the combined data

data_1997_from_data <- subset(ses_data, dataset == 1997)

# checking for overlapping identifiers
intersect(data_1997_from_data$pid, ses_1997$serial)
sum(data_1997_from_data$pid %in% ses_1997$serial)


#Check age and sex similarities
merged_pid <- merge(
  data_1997_from_data,
  ses_1997,
  by.x = "pid",
  by.y = "serial"
)

nrow(merged_pid)

# Age comparison
merged_pid$age_diff <- abs(merged_pid$aage - merged_pid$a3)

summary(merged_pid$age_diff)

# Sex comparison
table(
  data_sex = merged_pid$asex,
  ses_sex  = merged_pid$a2
)




data_1997_from_data <- subset(selected_data1, dataset == 1997)

intersect(data_1997_from_data$pid, ses_1997$serial)

merged_pid <- merge(
  data_1997_from_data,
  ses_1997,
  by.x = "pid",
  by.y = "serial"
)


table(
  data_quals = merged_pid$qualification.x,
  ses_quals  = merged_pid$qualification
)







ses_1997 <- read_dta("ses1997.dta")
ses_2001 <- read_dta("ses2001.dta")
ses_2006 <- read_dta("ses2006.dta")
ses_2012 <- read_dta("ses2012.dta")
ses_2017 <- read_dta("ses2017.dta")

names(ses_1997)

table(ses_data$skcomp)
attr(ses_data$eproprt, "labels")

table(ses_1997$m1)
attr(ses_1997$m1, "labels")



table(ses_data$dataset, ses_data$bauto)




## -----------------------------
## 1) Define task variables
## -----------------------------
task_vars <- c("cdetail","cpeople","cteach","cspeech","cpersuad","cselling","ccaring",
              "cteamwk","clisten","cstrengt","cstamina","chands","ctools","cproduct",
              "cspecial","corgwork","cusepc","cfaults","ccause","csolutn",        
              "canalyse","cnoerror","cmistake","cplanme","cplanoth","cmytime",        
              "cahead","cread","cshort","clong","cwrite","cwritesh",       
              "cwritelg","ccalca","cpercent","cstats" ,"cnetuse","ccoop",          
              "cmotivat","cthings","ccoach","ccareers","cfuture","cmefeel",        
              "cothfee","clookpr","csoundp","cforlan") 

## -----------------------------
## 3) Missing variables per year (100% NA in that year)
## -----------------------------
missing_vars_by_year <- lapply(split(ses_data, ses_data$dataset), function(df_year) {
  task_vars[sapply(task_vars, function(v) all(is.na(df_year[[v]])))]
})

# Print list
print(missing_vars_by_year)





names(ses_data)











attr(ses_data$cnetuse, "labels")

table(ses_data$dataset, ses_data$cnetuse)








rm(list = ls())
gc()





























