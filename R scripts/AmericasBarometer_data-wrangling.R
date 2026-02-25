# AmericasBarometer_data-wrangling.R
#------------------------------------------------------------------------------#
#           Workshop LATAM 397 - AmericasBarometer data wrangling              #
#                                                                              #
# Authors:                                                                     #
#   Valeria Gracia Olvera, M.Sc., <vgracia@stanford.edu>                       #
#   Clara Bicalho Maia Correia, PhD, <clara.bicalho@stanford.edu>              #
#                                                                              #
# References:                                                                  #
#   - AmericasBarometer data: http://datasets.americasbarometer.org/database/  #
#   - Boulding, C., Foxworth, R., & Verner, M. (2026). Strength in small       #
#     numbers: Indigenous political participation in Latin America. Comparative#
#     Political Studies, 59(2), 340-369.                                       #
#   - Verner, Marija, 2025, "Replication Data for: Strength in Small Numbers:  #
#     Indigenous Political Participation in Latin America",                    #
#     https://doi.org/10.7910/DVN/LVSYCG, Harvard Dataverse, V1;               #
#     syntax_vars_creation.do                                                  #
#                                                                              #
# Last update: 02/24/26                                                        #
#------------------------------------------------------------------------------#

rm(list = ls())  # clean environment

# Load libraries ----------------------------------------------------------
library(dplyr)       # for data manipulation
library(haven)       # for reading .dta files
library(labelled)    # for handling variable labels

# Load data ---------------------------------------------------------------
# We downloaded the `Grand Merge File` after logging as a free user. 
# The file is in .dta format, so we use the `haven` package to read it.
df_data_raw <- read_dta("data/Grand_Merge_2004-2023_LAPOP_AmericasBarometer_v1.0_FREE.dta")

# View all labels
all_var_labels <- lapply(df_data_raw, var_label)
View(all_var_labels) # View the labels

# Data wrangling ----------------------------------------------------------
# Select variables
df_data <- df_data_raw %>% 
  # (pais) country, (wave) survey wave, (year) year, (q2) edad, 
  # (ed) years of education, 
  # (etid) race, (prot3) participation in protest, 
  # (np1) participation in town meeting, (np2) petition local government,
  # (cp13) participation in political party meetings, (l1) political ideology,
  # (cp7) participation in school parents' association, 
  # (cp8) participation in community organization,
  # (cp20) participation in women's organization
  select(pais, wave, year, q2, ed, etid, prot3, np1, np2, cp13, l1, cp7, cp8, cp20)

# Keep only Latin American countries
# Drop Dominican Republic (21) because it does not include ethnicity question
df_data <- df_data %>% 
  filter(!(pais %in% c(24,27,25,22,40,23,41,26,28,29,30,31,32,34,35,21)))

# Limit the analysis for 2004 - 2019
df_data <- df_data %>% 
  filter(wave >= 2004 & wave <= 2019)

# Rename variables
df_data <- df_data %>% 
  rename(country    = pais,
         age        = q2,
         school_yrs = ed,
         pol_ideol  = l1)

## Create variables -------------------------------------------------------
# COUNTRY
df_data <- df_data %>% 
  mutate(country_lbl = case_when(country == 1 ~ "Argentina",
                                 country == 2 ~ "Bolivia",
                                 country == 3 ~ "Brazil",
                                 country == 4 ~ "Chile",
                                 country == 5 ~ "Colombia",
                                 country == 6 ~ "Costa Rica",
                                 country == 7 ~ "Ecuador",
                                 country == 8 ~ "El Salvador",
                                 country == 9 ~ "Guatemala",
                                 country == 10 ~ "Honduras",
                                 country == 11 ~ "Mexico",
                                 country == 12 ~ "Nicaragua",
                                 country == 13 ~ "Panama",
                                 country == 14 ~ "Paraguay",
                                 country == 15 ~ "Peru",
                                 country == 16 ~ "Uruguay",
                                 country == 17 ~ "Venezuela"))

# INDIGENEOUS self-identification
df_data <- df_data %>% 
  mutate(indig = ifelse(etid == 3, 1,
                        ifelse(etid == 4005, 1, 
                               ifelse(!is.na(etid), 0, NA))))

df_data$indig_lbl <- factor(df_data$indig,
                            levels = c(0, 1),
                            labels = c("No","Yes"))

table(df_data$indig_lbl) # tabulating values

## PROTEST
# Protested in the last 12 Months
df_data <- df_data %>% 
  mutate(protest = ifelse(prot3 == 2, 0, prot3)) 

# Creating a label
df_data$protest_lbl <- factor(df_data$protest, 
                              levels = c(0, 1),
                              labels = c("No","Yes"))

table(df_data$protest_lbl) # tabulating values

## TOWN MEETING
# Attended city council or town meeting in past 12 months
df_data <- df_data %>% 
  mutate(townmeeting = case_when(np1 == 1 ~ 1,
                                 np1 == 2 ~ 0)) 

df_data$townmeeting_lbl <- factor(df_data$townmeeting,
                                  levels = c(0, 1),
                                  labels = c("No","Yes"))

table(df_data$townmeeting_lbl) # tabulating values

## PETITION (LOCAL)
# Request assistance or petition municipal govt in past 12 months
df_data <- df_data %>%
  mutate(petitionlocal = case_when(np2 == 1 ~ 1,
                                   np2 == 2 ~ 0))

df_data$petitionlocal_lbl <- factor(df_data$petitionlocal,
                                    levels = c(0, 1),
                                    labels = c("No","Yes"))

table(df_data$petitionlocal_lbl) # tabulating values

## PARTY MEETING
# Attended meetings of a political party or pol. org at least 
# 1 x month (from 1-4 scale)
df_data <- df_data %>% 
  mutate(polparty1 = case_when(cp13 == 1 ~ 1,
                               cp13 == 2 ~ 1,
                               cp13 == 3 ~ 0,
                               cp13 == 4 ~ 0)) 

df_data$polparty1_lbl <- factor(df_data$polparty1,
                                levels = c(0, 1),
                                labels = c("No","Yes"))

table(df_data$polparty1_lbl) # tabulating values

## POLITICAL IDEOLOGY
df_data <- df_data %>% 
  mutate(rightleft = case_when(pol_ideol == 1 ~ 1,
                               pol_ideol == 4 ~ 1,
                               pol_ideol == 5 ~ 2,
                               pol_ideol == 6 ~ 2,
                               pol_ideol == 7 ~ 3,
                               pol_ideol == 10 ~ 3)) 

df_data$rightleft_lbl <- factor(df_data$rightleft,
                                levels = c(1, 2, 3),
                                labels = c("left","middle","right"))

table(df_data$rightleft_lbl) # tabulating values

# PARENT ORGANIZATION 
# Participation in school parents' association
df_data <- df_data %>% 
  mutate(parentorg = case_when(cp7 == 4 ~ 0,
                               cp7 == 3 ~ 1,
                               cp7 == 2 ~ 2,
                               cp7 == 1 ~ 3)) 

df_data$parentorg_lbl <- factor(df_data$parentorg,
                                levels = c(0, 1, 2, 3),
                                labels = c("Never",
                                           "Once or twice a year",
                                           "Once or twice a month",
                                           "Once a week"))

table(df_data$parentorg_lbl) # tabulating values

# Participation in school parents' association
# At least once in the last year
df_data <- df_data %>% 
  mutate(parentorg01 = ifelse(parentorg %in% c(1,2,3), 1, 
                              ifelse(!is.na(parentorg), 0, NA))) 

df_data$parentorg01_lbl <- factor(df_data$parentorg01,
                                  levels = c(0, 1),
                                  labels = c("No","Yes"))

table(df_data$parentorg01_lbl) # tabulating values

# COMMUNITY ORGANIZATION
# Participation in a community organization
df_data <- df_data %>% 
  mutate(commorg = case_when(cp8 == 4 ~ 0,
                             cp8 == 3 ~ 1,
                             cp8 == 2 ~ 2,
                             cp8 == 1 ~ 3)) 

df_data$commorg_lbl <- factor(df_data$commorg,
                              levels = c(0, 1, 2, 3),
                              labels = c("Never",
                                         "Once or twice a year",
                                         "Once or twice a month",
                                         "Once a week"))

table(df_data$commorg_lbl) # tabulating values

# Participation in a community organization
# At least once in the last year
df_data <- df_data %>% 
  mutate(commorg01 = ifelse(commorg %in% c(1,2,3), 1,
                            ifelse(!is.na(commorg), 0, NA))) 

df_data$commorg01_lbl <- factor(df_data$commorg01,
                                levels = c(0, 1),
                                labels = c("No","Yes"))

table(df_data$commorg01_lbl) # tabulating values

# WOMEN'S ORGANIZATION
# Participation in a womens' organization
df_data <- df_data %>% 
  mutate(womenorg = case_when(cp20 == 4 ~ 0,
                              cp20 == 3 ~ 1,
                              cp20 == 2 ~ 2,
                              cp20 == 1 ~ 3)) 

df_data$womenorg_lbl <- factor(df_data$womenorg,
                               levels = c(0, 1, 2, 3),
                               labels = c("Never",
                                          "Once or twice a year",
                                          "Once or twice a month",
                                          "Once a week"))

table(df_data$womenorg_lbl) # tabulating values

# Participation in a womens' organization
# At least once in the last year
df_data <- df_data %>% 
  mutate(womenorg01 = ifelse(womenorg %in% c(1,2,3), 1,
                             ifelse(!is.na(womenorg), 0, NA))) 

df_data$womenorg01_lbl <- factor(df_data$womenorg01,
                                 levels = c(0, 1),
                                 labels = c("No","Yes"))

table(df_data$womenorg01_lbl) # tabulating values

# Save final dataset ------------------------------------------------------
df_clean <- df_data %>% 
  select(country, country_lbl, wave, year, age, school_yrs, indig, indig_lbl, protest, protest_lbl, 
         townmeeting, townmeeting_lbl, petitionlocal, petitionlocal_lbl, polparty1, 
         polparty1_lbl, pol_ideol, rightleft, rightleft_lbl, parentorg, parentorg_lbl, parentorg01,
         parentorg01_lbl, commorg, commorg_lbl, commorg01, commorg01_lbl, womenorg,
         womenorg_lbl, womenorg01,womenorg01_lbl)

write.csv(df_clean, "AmericasBarometer_clean.csv", row.names = FALSE)


