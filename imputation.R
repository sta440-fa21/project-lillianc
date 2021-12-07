library(tidyverse)
library(mice)
library(naniar)
library(kableExtra)

prison <- read_csv("data/prison-clean.csv")

woo <- md.pattern(prison)

(colSums(is.na((prison)))*100/nrow(prison)) %>%
  kable(format = "markdown",
        digits = 3,
        col.names = c("Percentage of Missing Observation"))

prison %>%
  select(!starts_with("victim")) %>%
  gg_miss_upset()

# MICE imputation
imput_data <- prison %>%
  select(current_age, race, sex, citizen, military, education, homeless_12mo_prior, arrest_year, admit_year,
         held_by, state, controlling_offense, controlling_offense_type, arrested_during_status,
         firearm_at_offense, alc_at_offense, drug_at_offense, age_at_arrest) %>%
  mutate(across(c(current_age, race, sex, citizen, military, education, homeless_12mo_prior, arrest_year,
                  admit_year, held_by, state, controlling_offense, controlling_offense_type,
                  arrested_during_status, firearm_at_offense, alc_at_offense, drug_at_offense, age_at_arrest),
                as.factor))

imput_data.imp = mice(imput_data, m = 5, seed = 123)
df1 = complete(imput_data.imp, 1)
df2 = complete(imput_data.imp, 2)
df3 = complete(imput_data.imp, 3)
df4 = complete(imput_data.imp, 4)
df5 = complete(imput_data.imp, 5)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# average 4 datasets to get 1 dataset
results <- c()

for (i in 1:ncol(df1)){
  if (typeof(df1[,i]) == "double"){
    list_of_val <- c()
    for (j in 1:nrow(df1)){
      values <- c(df1[j,i], df2[j,i], df3[j,i], df4[j,i], df5[j,i])
      list_of_val <- c(list_of_val, mean(values))
    }
    results <- c(results, list_of_val)
    list_of_val <- c()
  } else{
    list_of_val <- c()
    for (j in 1:nrow(df1)){
      values <- c(df1[j,i], df2[j,i], df3[j,i], df4[j,i], df5[j,i])
      list_of_val <- c(list_of_val, getmode(values))
    }
    results <- c(results, list_of_val)
  }
}

age <- results[1:848]
gender <- results[849:1696]
hispanic <- results[1697: 2544]
income <- results[2545:3392]
yrs_in_durham <- results[3393:4240]
own_or_rent <- results[4241:5088]
race_ethnicity <- results[5089:5936]
