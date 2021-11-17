library(tidyverse)
library(magrittr)

load("data/37692-0001-Data.rda")
prison <- da37692.0001

prison_filt <- prison %>%
  select(current_age = RV0001, race = RV0003, citizen = RV0004,
         military = RV0008, controlling_offense = RV0036, controlling_offense_type = RV0037,
         education = RV0054, homeless_12mo_prior = V0961, firearm_at_offense = RV0095,
         arrest_year = V0055Y, admit_year = V0056Y, held_by = V0071, sentenced = V0073, awaiting = V0075,
         arrested_during_status = V0078, jail_time_served = V0390, jail_time_yr = V0391, jail_time_mo = V0392,
         jail_time_wk = V0393, jail_time_dy = V0394,
         victim_hispanic = V0481, victim_hispanic2 = V0544, victim_white = V0482,
         victim_white2 = V0545, victim_black = V0483, victim_black2 = V0546,
         victim_native_american = V0484, victim_native_american2 = V0547, victim_asian = V0485, victim_asian2 = V0548,
         victim_hawaiian = V0486, victim_hawaiian2 = V0549, victim_race = V0488, victim_race2 = V0551,
         victim_sex = V0489, victim_age = V0490, victim_known = V0491, victim_offense = V0495,
         victim_injured = V0496, victim_died = V0497, state = V0772, sex = V1212,
         alc_at_offense = V1267, drug_at_offense = V1326) %>%
  mutate(
    across(c("jail_time_yr", "jail_time_mo", "jail_time_wk", "jail_time_dy"),
           ~ifelse(.x < 0, as.numeric(NA), .x)),
    jail_time_yr = ifelse(jail_time_yr > 40, as.numeric(NA), jail_time_yr),
    time_estimate = admit_year - arrest_year) %>%
  rowwise() %>%
  mutate(
    jail_time = case_when(
      is.na(jail_time_yr) & is.na(jail_time_mo) & is.na(jail_time_wk) & is.na(jail_time_dy) ~ as.numeric(NA),
      TRUE ~ round(sum(c(jail_time_yr * 365, jail_time_mo * 365 / 12, jail_time_wk * 7, jail_time_dy),
                       na.rm = TRUE), 0))) %>%
  ungroup()

attributes(prison_filt)$variable.labels <- NULL

prison_filt %<>%
  select(current_age, race, sex, citizen, military, education, homeless_12mo_prior, jail_time_served, jail_time,
         arrest_year, admit_year, held_by, state, sentenced, controlling_offense, controlling_offense_type,
         arrested_during_status, firearm_at_offense, alc_at_offense, drug_at_offense,
         victim_hispanic, victim_hispanic2, victim_white, victim_white2, victim_black, victim_black2,
         victim_native_american, victim_native_american2, victim_asian, victim_asian2,
         victim_hawaiian, victim_hawaiian2, victim_race, victim_race2,
         victim_sex, victim_age, victim_known, victim_offense) %>%
  mutate(across(c("race", "citizen", "military", "controlling_offense", "controlling_offense_type",
                  "education", "firearm_at_offense", "held_by", "sentenced", "arrested_during_status",
                  "jail_time_served", "sex", "alc_at_offense", "drug_at_offense", starts_with("victim"),
                  "homeless_12mo_prior"),
                ~ str_remove(as.character(.x), "\\(\\d+\\) \\d+ = ")),
         across(c("arrest_year", "admit_year"), ~ifelse(.x == 999999, as.numeric(NA), .x)),
         across(c("sex", "alc_at_offense", "drug_at_offense", "sentenced", starts_with("victim"), "homeless_12mo_prior"),
                ~ifelse(.x %in% c("(-1) -1 = Don't Know", "(-2) -2 = Refusal"), as.character(NA), .x)),
         across(c("military", "controlling_offense_type"), ~ifelse(.x == "DK/REF", as.character(NA), .x)),
         race = str_remove(race, " \\(NH\\)"),
         race = case_when(
           race == "Uncategorized - Missing" ~ as.character(NA),
           race == "2+ Races" ~ "Other",
           TRUE ~ race),
         sex = ifelse(sex %in% c("Do not identify as male, female or transgender", "Transgender"),
                      "Transgender/Other", sex),
         citizen = ifelse(citizen == "Missing", as.character(NA), citizen),
         education = ifelse(education == "Missing", as.character(NA), education),
         firearm_at_offense =
           ifelse(firearm_at_offense %in% c("DK/REF", "Missing (in-universe)"), as.character(NA), firearm_at_offense),
         held_by = case_when(
           held_by == "State correctional authorities such as the state department of corrections" ~ "State",
           held_by %in% c("Federal Bureau of Prisons", "U.S. Marshals Service") ~ "Federal",
           held_by == "Local correctional authorities such as local jails or detention centers" ~ "Local",
           held_by == "U.S. Immigration and Customs Enforcement" ~ "ICE",
           held_by == "Some Other Authority" ~ "Other",
           TRUE ~ as.character(NA)),
         arrested_during_status = case_when(
           arrested_during_status == "None of These (No Parole, Probation, or Escape)" ~ "None",
           arrested_during_status == "Probation, including Shock Probation and Split Sentences" ~ "Probation",
           arrested_during_status == "Parole or Post-Release Supervision after serving time" ~ "Parole",
           arrested_during_status %in% c("(-2) -2 = Refusal", "(-1) -1 = Don't Know") ~ as.character(NA),
           TRUE ~ arrested_during_status),
         max_time_est = admit_year - arrest_year,
         max_time_est = ifelse(max_time_est < 0, as.numeric(NA), max_time_est),
         jail_time_served = case_when(
           jail_time_served == "Specify Jail Time" ~ "Yes",
           jail_time_served == "No Time in Jail" ~ "No",
           TRUE ~ as.character(NA)),
         jail_time = ifelse(jail_time_served == "No", 0, jail_time),
         victim_race = case_when(
           victim_race == "Contained a don't know response" ~ "Unknown",
           victim_race == "Contained at least one valid response entry" ~
             str_remove_all(str_remove_all(paste0(victim_white, ",", victim_black, ",", victim_native_american,
                                                  ",", victim_asian, ",", victim_hawaiian), "Blank"), ","),
           TRUE ~ as.character(NA)),
         victim_race = case_when(
           victim_race %in% c("White" , "Unknown") ~ victim_race,
           victim_race == "Black or African American" ~ "Black",
           victim_race == "American Indian or Alaska Native" ~ "American Indian/Alaska Native",
           victim_race %in% c("Native Hawaiian or Other Pacific Islander",
                              "Asian") ~ "Asian/Native Hawaiian/Other Pacific Islander",
           is.na(victim_race) ~ victim_race,
           TRUE ~ "Multiple"),
         victim_race2 = case_when(
           victim_race2 == "Contained a don't know response" ~ "Unknown",
           victim_race2 == "Contained at least one valid response entry" ~
             str_remove_all(str_remove_all(paste0(victim_white2, ",", victim_black2, ",", victim_native_american2,
                                                  ",", victim_asian2, ",", victim_hawaiian2), "Blank"), ","),
           TRUE ~ as.character(NA)),
         victim_race2 = case_when(
           victim_race2 %in% c("White" , "Unknown") ~ victim_race2,
           victim_race2 == "Black or African American" ~ "Black",
           victim_race2 == "American Indian or Alaska Native" ~ "American Indian/Alaska Native",
           victim_race2 %in% c("Native Hawaiian or Other Pacific Islander",
                               "Asian") ~ "Asian/Native Hawaiian/Other Pacific Islander",
           is.na(victim_race2) ~ victim_race2,
           TRUE ~ "Multiple"),
         victim_race = ifelse(is.na(victim_race), victim_race2, victim_race),
         victim_hispanic2 = case_when(
           victim_hispanic2 %in% c("All were Hispanic", "Most were Hispanic", "They were evenly divided") ~ "Yes",
           victim_hispanic2 == "Most were Non-Hispanic" ~ "No",
           TRUE ~ victim_hispanic2),
         victim_hispanic = ifelse(is.na(victim_hispanic), victim_hispanic2, victim_hispanic)) %>%
  select(-victim_hispanic2, -victim_white, -victim_white2, -victim_black, -victim_black2,
         -victim_native_american, -victim_native_american2, -victim_asian, -victim_asian2,
         -victim_hawaiian, -victim_hawaiian2, -victim_race2)

prison_filt %<>%
  mutate(censored = case_when(
    sentenced == "No" ~ 0,
    sentenced == "Yes" ~ 1,
    TRUE ~ as.numeric(NA)),
    race_white = case_when(
      race == "White" ~ "Y",
      is.na(race) ~ as.character(NA),
      TRUE ~ "N"),
    race_black = case_when(
      race == "Black" ~ "Y",
      is.na(race) ~ as.character(NA),
      TRUE ~ "N"))

write_csv(prison_filt, "data/prison-clean.csv")
