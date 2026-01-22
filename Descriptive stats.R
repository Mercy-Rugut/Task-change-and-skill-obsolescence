###############################################################################
# PhD in Economics Chapter One
# Project - Human capital depreciation across cohorts:A task-based approach 
# Year 2025/2026
###############################################################################

###############################################################################
###--------------------------- Milestones ----------------------------------### 
###############################################################################

# 1.Identify the variables needed
# 2.Categorize and classify accordingly(Age,education,occupation,task&birth cohorts)
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
# Install and load ggplot2
if ("ggplot2" %in% installed.packages()){
  library(ggplot2)
} else{
  install.packages("ggplot2")
  library(ggplot2)
}
###############################################################################
###--------------- Load Skills and Employment Survey data ------------------###
###############################################################################
# Get working directory 
getwd()

setwd("C:/Users/Dell 7320/OneDrive - University of Strathclyde/Documents/SES")

# Read Stata file
ses_data <- read_dta("sescombined.dta")

names(ses_data) # display all variables
View(ses_data)
###############################################################################
###------------------ Part 1: Data preparation------------------------------###
###############################################################################
#=========================================
## Variables we need
keep_vars <- ses_data[, c("dataset","cusepc", "dusepc","eproprt","gwtall","region","aage")]

names(keep_vars)

#=========================================
# Filtering for Years & Regions
data <- keep_vars %>%
  filter(!region %in% c(12, 13),        # highlands and islands & Northern Ireland from 2006
         !dataset %in% c(1986),   # Filter out years without task data
         aage >= 20 & aage <= 60)       # Keep ages 20-60

#=========================================
# Digital workspace (more than 3/4 of employees use computer)
share_3quarters <- data %>%
  group_by(dataset) %>%
  summarise(
    weighted_share = sum(gwtall[eproprt == 1], na.rm = TRUE) /
      sum(gwtall, na.rm = TRUE)
  )
share_3quarters <- share_3quarters %>%
  mutate(Digital_workspace = weighted_share * 100)

share_3quarters

#=========================================
#computerised job (essential/very important)

share_comp <- data %>%
  group_by(dataset) %>%
  summarise(
    comp_share = sum(gwtall[cusepc %in% c(1, 2)], na.rm = TRUE) /
      sum(gwtall, na.rm = TRUE)
  )
share_comp <- share_comp %>%
  mutate(computerised_job = comp_share * 100)

share_comp
#=========================================
#complexity in computer use (Advanced)

share_advance <- data %>%
  group_by(dataset) %>%
  summarise(
    advance_share = sum(gwtall[dusepc == 4], na.rm = TRUE) /
      sum(gwtall, na.rm = TRUE)
  )
share_advance <- share_advance %>%
  mutate(advanced_computer = advance_share * 100)

share_advance

#=========================================
# plotting - Long term trend in digitalisation
# Combining the three series into one dataset (by year)
plot_df <- share_3quarters %>%
  select(dataset, Digital_workspace) %>%
  left_join(share_comp %>% select(dataset, computerised_job), by = "dataset") %>%
  left_join(share_advance %>% select(dataset, advanced_computer), by = "dataset") %>%
  arrange(dataset)

# Reshaping to long format for ggplot
plot_long <- plot_df %>%
  pivot_longer(
    cols = c(Digital_workspace, computerised_job, advanced_computer),
    names_to = "series",
    values_to = "value"
  ) %>%
  mutate(
    value = na_if(value, 0),
    series = factor(series,
                    levels = c("computerised_job", "Digital_workspace", "advanced_computer"),
                    labels = c("Computerised job", "Digital workplace", "Advanced computer use")
    )
  ) %>%
  filter(!is.na(value))   # <-- remove missing points

# Plotting the three curves
ggplot(plot_long, aes(
  x = dataset,
  y = value,
  color = series,
  linetype = series,
  group = series
)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = sort(unique(plot_df$dataset))) +
  scale_y_continuous(
    limits = c(0, NA),
    breaks = seq(0, max(plot_long$value, na.rm = TRUE), by = 10)
  ) +
  scale_color_manual(
    values = c(
      "Computerised job"      = "red",
      "Digital workplace"     = "blue",
      "Advanced computer use" = "darkgreen"
    )
  ) +
  labs(
    x = "Year",
    y = "Percentage of workers (%)",
    color = NULL,
    linetype = NULL
  ) +
  theme_minimal()








