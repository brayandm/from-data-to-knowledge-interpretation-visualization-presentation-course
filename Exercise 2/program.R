# Load the necessary packages

install_and_load <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(new_packages)) install.packages(new_packages)
  sapply(packages, require, character.only = TRUE)
}

packages_needed <- c("dplyr", "tidyr", "readxl")

install_and_load(packages_needed)

library(dplyr)
library(tidyr)
library(readxl)

# Load the data

file_path <- "data-denmark.xlsx"
deaths_data <- read_excel(file_path, sheet = "deaths")
pop_data <- read_excel(file_path, sheet = "pop")

# Ex 1. Using deaths data from Denmark answer the following question:

# Q: Which region had the largest number of deaths among men in the year 2003?

deaths_2003_men <- deaths_data %>%
  filter(year == "y2003" & sex == "m")

deaths_2003_men <- deaths_2003_men %>%
  group_by(region) %>%
  summarise(total_deaths = sum(value)) %>%
  arrange(desc(total_deaths))

print("Exercise 1:")
print(paste("The region with the largest number of deaths among men in 2003 was", deaths_2003_men$region[1], "with", deaths_2003_men$total_deaths[1], "deaths."))

# Ex 2. Using pop data from Denmark answer the following question:

# Q: In which region the sex ration (SR) is highest at ages 15, 45, over75 (coded as "open")

pop_2004 <- pop_data %>%
  filter(year == "y2004")

pop_2004_wide <- pop_2004 %>%
  pivot_wider(names_from = sex, values_from = value)

pop_2004_wide <- pop_2004_wide %>%
  select(-b)

pop_2004_wide <- pop_2004_wide %>%
  mutate(sex_ratio = m / f * 100)

ages <- c("a15", "a45", "open")

highest_sex_ratios <- pop_2004_wide %>%
  filter(age %in% ages) %>%
  group_by(age) %>%
  summarise(region = region[which.max(sex_ratio)],
            sex_ratio = max(sex_ratio, na.rm = TRUE))

print("Exercise 2:")
print(paste("The region with the highest sex ratio at age 15 is", highest_sex_ratios$region[1], "with a ratio of", highest_sex_ratios$sex_ratio[1]))
print(paste("The region with the highest sex ratio at age 45 is", highest_sex_ratios$region[2], "with a ratio of", highest_sex_ratios$sex_ratio[2]))
print(paste("The region with the highest sex ratio at age over 75 is", highest_sex_ratios$region[3], "with a ratio of", highest_sex_ratios$sex_ratio[3]))