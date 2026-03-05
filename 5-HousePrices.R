#setting the working directory and loading necessary packages 
install.packages(c("haven", "AER", "survey", "mitools", "dplyr", "foreign", "tidyselect", "labelled", "laeken", "convey", "ggplot2"))

library(haven)
library(survey)
library(mitools)
library(dplyr)
library(foreign)
library(tidyselect)
library(labelled)
library(laeken)
library(expss)
library(convey)
library(ggplot2)
library(AER)
library(broom)
library(scales)
library(tidyr)

rm(list=ls())

#################################
#### read in the data files ####
################################

## First Core personal files (P1-P5)

p1 <- read_dta("P1.dta")
p2 <- read_dta("P2.dta")
p3 <- read_dta("P3.dta")
p4 <- read_dta("P4.dta")
p5 <- read_dta("P5.dta")

p_file <- rbind(p1, p2, p3, p4, p5)
rm(p1, p2, p3, p4, p5)

names(p_file)<- toupper(names(p_file))



## Second, let's read in all the derived variables

d1 <- read_dta("D1.dta")
d2 <- read_dta("D2.dta")
d3 <- read_dta("D3.dta")
d4 <- read_dta("D4.dta")
d5 <- read_dta("D5.dta")

d_file <- rbind(d1, d2, d3, d4, d5)
rm(d1, d2, d3, d4, d5)

names(d_file)<- toupper(names(d_file))


##  Let's read in the core-household variables


h1 <- read_dta("H1.dta")
h2 <- read_dta("H2.dta")
h3 <- read_dta("H3.dta")
h4 <- read_dta("H4.dta")
h5 <- read_dta("H5.dta")

h_file <- rbind(h1, h2, h3, h4, h5)
rm(h1, h2, h3, h4, h5)

names(h_file)<- toupper(names(h_file))


##  We also need the weights!
w_file <- read_dta("W.dta")
# some countries have NAs instead of 0 in the current replicate weights file in the second wave -> change those to 0
w_file[is.na(w_file)] <-  0

#convert to upper case letters (in some files upper and lower cases are mixed, helps to be consistent)
names(w_file)<- toupper(names(w_file))


# SA0010 = HH ID
# SA0100 = country
# HW0010 = HH weight
# IM0010 = Implicate ID

### merge all separate files to one file (except the weights, which are read in extra)
hfcs<-left_join(d_file, h_file, by = c("ID", "SURVEY", "SA0010", "SA0100", "IM0100", "HW0010") ) # possible warning "Column `id` has different attributes on LHS and RHS of join" can be ignored
hfcs <- left_join(hfcs,
                  p_file %>% 
                    group_by(HID) %>% 
                    filter(RA0100==1) %>% 
                    ungroup(), 
                  by = c("ID" = "HID", "SURVEY", "SA0010", "SA0100", "IM0100"))

names(hfcs)[names(hfcs) == "ID.y"]<-"HID"



## remove big files to reduce used workspace
rm(d_file, h_file, p_file)

write_dta(hfcs, "hfcs_summerschool.dta")


#################################################
# Generate additional variables for HFCS dataset
#################################################

# Rename age variable for clarity

names(hfcs)[names(hfcs) == "DHAGEH1B"] <- "age"

# Create age group variable with consistent intervals and labels
hfcs <- hfcs %>% 
  mutate(agedum = case_when(
    age >= 20 & age <= 24 ~ 1,
    age >= 25 & age <= 29 ~ 2,
    age >= 30 & age <= 34 ~ 3,
    age >= 35 & age <= 39 ~ 4,
    age >= 40 & age <= 44 ~ 5,
    age >= 45 & age <= 49 ~ 6,
    age >= 50 & age <= 54 ~ 7,
    age >= 55 & age <= 59 ~ 8,
    age >= 60 & age <= 64 ~ 9,
    age >= 65 & age <= 69 ~ 10,
    age >= 70 & age <= 74 ~ 11,
    age >= 75 & age <= 79 ~ 12,
    TRUE ~ 0  # Default for missing or out-of-range ages
  ))

# Create gender indicator variable
hfcs <- hfcs %>%
  mutate(female = if_else(RA0200 == 1, 1, 0))

# Use education category variable
hfcs <- hfcs %>%
  mutate(edcat = PA0200)

# Create working status indicator
hfcs <- hfcs %>% 
  mutate(working = if_else(PE0100A == 1, 1, 0))

# Create marital status indicator
hfcs <- hfcs %>% 
  mutate(married = if_else(PA0100 %in% c(2, 3), 1, 0, missing = 0))

# Calculate number of children
hfcs <- hfcs %>%
  mutate(children = DH0001 - DH0006)

# Define family structure categories
hfcs <- hfcs %>% 
  mutate(famstruct = case_when(
    married == 0 & children > 0 ~ 1,  # Not married with children
    married == 0 & children == 0 & age <= 55 ~ 2,  # Single, no kids, under 55
    married == 0 & children == 0 & age > 55 ~ 3,  # Single, no kids, over 55
    married == 1 & children > 0 ~ 4,  # Married with kids
    married == 1 & children == 0 ~ 5  # Married, no kids
  ))

# Assign value labels for family structure
val_lab(hfcs$famstruct) <- make_labels("
  1 Not married with kids
  2 Not married, no kids, under 55
  3 Not married, no kids, over 55
  4 Married with kids
  5 Married, no kids
")

# Create college education indicator
hfcs <- hfcs %>%
  mutate(college = if_else(edcat >= 5, 1, 0, missing = 0))

#################################################
# Rename income-related variables for clarity
#################################################

names(hfcs)[names(hfcs) == "DI1100"] <- "employee_income"
names(hfcs)[names(hfcs) == "DI1200"] <- "self_employment_inc"
names(hfcs)[names(hfcs) == "DI1300"] <- "rental_income_realest"
names(hfcs)[names(hfcs) == "DI1400"] <- "income_finassets"
names(hfcs)[names(hfcs) == "DI1410"] <- "income_finassets_gr_int"
names(hfcs)[names(hfcs) == "DI1412"] <- "interest_payments"
names(hfcs)[names(hfcs) == "DI1420"] <- "income_private_businesses"
names(hfcs)[names(hfcs) == "DI1500"] <- "pension_incomes"
names(hfcs)[names(hfcs) == "DI1510"] <- "pension_incomes_public"
names(hfcs)[names(hfcs) == "DI1520"] <- "pension_incomes_priv_occ"
names(hfcs)[names(hfcs) == "DI1600"] <- "social_transfers"
names(hfcs)[names(hfcs) == "DI1610"] <- "unemployment_benefits"
names(hfcs)[names(hfcs) == "DI1620"] <- "other_social_benefits"
names(hfcs)[names(hfcs) == "DI1700"] <- "private_transfers"
names(hfcs)[names(hfcs) == "DI1800"] <- "other_income"
names(hfcs)[names(hfcs) == "DN3001"] <- "networth"
names(hfcs)[names(hfcs) == "DNNLA"] <-  "netliquidassets"

## Define income based on employee income, social transfers, private transfers, and pensions
hfcs$income <- rowSums(
  hfcs[, c("employee_income", "social_transfers", "private_transfers", "pension_incomes")], 
  na.rm = TRUE
)

## Include self-employment income, interest payments, rental income, and financial income
hfcs$income_psid <- rowSums(
  hfcs[, c("income", "self_employment_inc", "income_private_businesses", "rental_income_realest", "income_finassets")], 
  na.rm = TRUE
)

## Total gross annual household income
hfcs$total_gross_income <- rowSums(
  hfcs[, c("employee_income", "social_transfers", "private_transfers", "pension_incomes", 
           "self_employment_inc", "income_private_businesses", "rental_income_realest", 
           "income_finassets")], 
  na.rm = TRUE
)


summary(hfcs$income)
summary(hfcs$income_psid)
summary(hfcs$total_gross_income)
summary(hfcs$DI2000)



## Keep only data for Austria and Germany
hfcs_at_de <- subset(hfcs, hfcs$SA0100 %in% c('AT', 'DE'))
write_dta(hfcs_at_de, "hfcs_AT_DE_data_summerschool.dta")
names(hfcs_at_de)[names(hfcs_at_de) == "DN3001"] <-  "networth"

## Same for weights!
w_file_at_de <- subset(w_file, w_file$SA0100 %in% c('AT', 'DE'))
write_dta(w_file_at_de, "w_file_AT_DE_summerschool.dta")

## We create a list object with all the implicates
hfcs_at_de.list <- split(as.data.frame(hfcs_at_de), factor(hfcs_at_de$IM0100))

# Creating a survey replicate design object with all 1000 replicate weights
hfcs.svy <- svrepdesign(
  weights = ~HW0010,
  repweights = w_file_at_de %>% 
    select(WR0001:WR0100) %>% as.data.frame(),
  data = imputationList(hfcs_at_de.list),
  scale = 1,
  rscale = rep(1/100, 100), # for only 500 replicate weights use rep(1/1000, 500) and select(wr0001:wr0500)
  mse = TRUE,
  type = "other",
  combined.weights = TRUE)

hfcs_sdesign <- svydesign(id = ~ID,  weights = ~HW0010, data = hfcs_at_de)

##############################################################
##########################   TASK 1   ########################
##############################################################


# 1. Calculation of the property value changes
hfcs_at_de <- hfcs_at_de %>%
  mutate(
    change_ratio = HB0900 / HB0800,        # Ratio of current to acquisition value
    change_absolute = HB0900 - HB0800,    # Absolute difference in values
    change_category = cut(                # Categorization of change ratios into groups
      change_ratio,
      breaks = c(0, 0.5, 1, 1.5, 2, Inf),
      labels = c("Decrease > 50%", "Decrease 0-50%", "Increase 0-50%", 
                 "Increase 50-100%", "Increase > 100%"),
      include.lowest = TRUE
    )
  )

# In this step we calculate 2 key metrics:change_ratio, which indicates relative price change 
# (current value divided by acquisition value) and change_absolute, which represents the absolute monetary 
# difference between current and acquisition values. Also, we introduced change_category, 
# which groups households into certain bins (such as "Increase > 100%").


# In the article that we reference (Albacete et al. (2016)), authors also analyze the distribution of 
# residential property price changes, focusing on both absolute and relative changes.


# 2. Ensuring that change_category is correctly factored. This step is needed to ensure that this variable
# has a consistent order for plotting and analysis, regardless of the data.
hfcs_at_de$change_category <- factor(
  hfcs_at_de$change_category,
  levels = c("Decrease > 50%", "Decrease 0-50%", "Increase 0-50%", 
             "Increase 50-100%", "Increase > 100%")
)

# 3. Handling missing values. 
# First we check the number of missing values in HB0800 and HB0900
sum(is.na(hfcs_at_de$HB0800))  # Acquisition value
sum(is.na(hfcs_at_de$HB0900))  # Current value

# Then we check the proportion of missing values
mean(is.na(hfcs_at_de$HB0800))
mean(is.na(hfcs_at_de$HB0900))

# Approximately 46.7% of the rows in the dataset have missing (NA) values for 
# both HB0800 (acquisition value) and HB0900 (current value). It can be due to households without real estate:
# not all households in the dataset may own real estate. These households will not have values for HB0800 or HB0900.

# Checking for missing or sparse data
table(hfcs_at_de$SA0100, hfcs_at_de$change_category, useNA = "ifany") # Validate data distribution

# Removing rows with NA in change_category, in order to ensure valid analysis
hfcs_at_de <- hfcs_at_de %>% filter(!is.na(change_category))


# 4. Calculating descriptive statistics
change_summary <- hfcs_at_de %>%
  group_by(SA0100) %>%
  summarise(
    mean_change_ratio = mean(change_ratio, na.rm = TRUE),
    median_change_ratio = median(change_ratio, na.rm = TRUE),
    sd_change_ratio = sd(change_ratio, na.rm = TRUE),
    mean_change_absolute = mean(change_absolute, na.rm = TRUE),
    median_change_absolute = median(change_absolute, na.rm = TRUE),
    sd_change_absolute = sd(change_absolute, na.rm = TRUE),
    count = n()
  )

print("Descriptive Statistics for Real Estate Value Changes:")
print(change_summary)

# Here we compute key statistics (mean, median, standard deviation) for both change_ratio and 
# change_absolute, grouped by country (SA0100). 

# SA0100     mean_change_ratio   median_change_ratio   sd_change_ratio   mean_change_absolute   median_change_absolute  sd_change_absolute count
# <chr>              <dbl>               <dbl>           <dbl>                <dbl>                  <dbl>              <dbl>              <int>
# 1 AT               7.00                1.82            26.6                179289.                 110000            268560.             4915
# 2 DE               4.55                1.88            27.4                232454.                 160000            343793.             12160

# - The mean change ratio is significantly higher than the median in both countries, particularly in Austria (mean: 7.00 vs. median: 1.82). 
# This suggests the presence of outliers—households whose properties experienced exceptionally large increases in value.
# - The median change ratio indicates that most households in both countries experienced property value increases of ~82% (Austria) and 
# ~88% (Germany) compared to their acquisition values.
# - The mean absolute price increase is higher in Germany (€232,454) than in Austria (€179,289). Similarly, the median increase 
# in Germany (€160,000) exceeds Austria (€110,000), reflecting higher baseline property values in Germany.
# - More households were analyzed in Germany (12,160) compared to Austria (4,915). 


# 6. Calculating survey-weighted category distribution

# In this step we create the 'change_category' variable within each imputed dataset 
# before constructing the survey design object. These categories allow us to easily compare price changes across HHs.

# Firstly, we add 'change_category' to each imputed dataset 
hfcs_at_de.list <- lapply(hfcs_at_de.list, function(df) {
  df <- df %>%
    mutate(
      change_ratio = HB0900 / HB0800,        # Ratio of current to acquisition value
      change_absolute = HB0900 - HB0800,    # Absolute difference in values
      change_category = cut(
        change_ratio,
        breaks = c(0, 0.5, 1, 1.5, 2, Inf),
        labels = c("Decrease > 50%", "Decrease 0-50%", "Increase 0-50%", 
                   "Increase 50-100%", "Increase > 100%"),
        include.lowest = TRUE
      )
    )
  return(df)
})

# Now each imputed dataset in is updated with the new variables.
# Thus, secondly, we recreate the survey replicate design object with the updated imputed datasets:

hfcs.svy <- svrepdesign(
  weights = ~HW0010,
  repweights = w_file_at_de %>% 
    select(WR0001:WR0100) %>% as.data.frame(),
  data = imputationList(hfcs_at_de.list),
  scale = 1,
  rscale = rep(1/100, 100), # Adjust scaling as needed
  mse = TRUE,
  type = "other",
  combined.weights = TRUE
)

# Thirdly and finally, we initialize an empty list to store results from each imputation
# and calculate survey-weighted category distribution:

imputation_results <- lapply(hfcs.svy$designs, function(single_design) {
  svytable(~change_category + SA0100, design = single_design)
})

# Then we combine results from all imputations by averaging. Here dividing by the number of imputations 
# gives the average weighted distribution across imputations.
combined_results <- Reduce("+", imputation_results) / length(imputation_results)

# After that we convert combined results to a data frame
category_distribution_df <- as.data.frame(as.table(combined_results))
colnames(category_distribution_df) <- c("Change_Category", "Country", "Weighted_Count")

# Finally, we add proportions for each country
category_distribution_df <- category_distribution_df %>%
  group_by(Country) %>%
  mutate(Proportion = Weighted_Count / sum(Weighted_Count))

# Final distribution:
print("Survey-Weighted Distribution of Real Estate Value Changes:")
print(category_distribution_df)


# Here we compute survey-weighted counts for each 'change_category' by country.
# The weighted category distribution also reflects the emphasis of our reference article 
# on analyzing population-level distributions of property price changes.

#    Change_Category  Country.   Weighted_Count   Proportion
#    <fct>            <fct>            <dbl>        <dbl>
#  1 Decrease > 50%     AT              60929.     0.0141
#  2 Decrease 0-50%     AT             164731.     0.0381
#  3 Increase 0-50%     AT            1462735.     0.338 
#  4 Increase 50-100%   AT             761410.     0.176 
#  5 Increase > 100%    AT            1871870.     0.433 
#  6 Decrease > 50%     DE             411085.     0.0101
#  7 Decrease 0-50%     DE            3122481.     0.0769
#  8 Increase 0-50%     DE           13039347.     0.321 
#  9 Increase 50-100%   DE            8943538.     0.220 
#  10 Increase > 100%   DE           15092500.     0.372 

# - Only a small proportion of households in both countries experienced a decline in property value.
# - Most common category is 0-50% increase: in both Austria and Germany, a large proportion of households 
# experienced modest increases in property values of 0–50%, in Austria it is 33.8% of households and in Germany 32.1% of households.
# - At the same time he largest category in both countries is Increase > 100%, accounting for 43.3% of Austrian households and 
# 37.2% of German households. Thus, a substantial proportion of households saw their property values more than double. 
# - As a general trend, the relative proportions of households across categories are broadly similar, 
# which indicates comparable real estate trends in both countries.

# 7. Visualizing category distribution as proportion

# Plotting 
ggplot(category_distribution_df, aes(x = Change_Category, y = Proportion, fill = Country)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Distribution of Real Estate Value Changes",
    x = "Change Category",
    y = "Proportion of Households",
    fill = "Country"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()

# In this step we visualize the relative distribution of households in each change category as proportions.

# What's more, we also want to analyze how property value changes vary by year of property acquisition. 
# To do so, we add a year of acquisition category, then we group the data by these categories, 
# calculate the distribution of property value changes and visualize the results.

# 8. Adding year of acquisition categories using the variable HB0700 ("year of property acquisition")
hfcs_at_de <- hfcs_at_de %>%
  mutate(
    year_acquisition_category = case_when(
      HB0700 < 1980 ~ "Before 1980",
      HB0700 >= 1980 & HB0700 < 1990 ~ "1980-1989",
      HB0700 >= 1990 & HB0700 < 2000 ~ "1990-1999",
      HB0700 >= 2000 & HB0700 < 2010 ~ "2000-2009",
      HB0700 >= 2010 ~ "2010+",
      TRUE ~ NA_character_  
    )
  )

# Ensuring that "year_acquisition_category" is a factor with ordered levels
hfcs_at_de$year_acquisition_category <- factor(
  hfcs_at_de$year_acquisition_category,
  levels = c("Before 1980", "1980-1989", "1990-1999", "2000-2009", "2010+")
)

# Checking the distribution of the year acquisition categories
table(hfcs_at_de$year_acquisition_category, useNA = "ifany")

# Plotting it 
ggplot(hfcs_at_de, aes(x = year_acquisition_category)) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Distribution of Year of Acquisition Categories",
    x = "Year of Acquisition",
    y = "Number of Households"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# 9. In this step we are updating imputed datasets and recreating the survey replicate 
# design object to include the updated imputed datasets:


hfcs_at_de.list <- lapply(hfcs_at_de.list, function(df) {
  df <- df %>%
    mutate(
      year_acquisition_category = case_when(
        HB0700 < 1980 ~ "Before 1980",
        HB0700 >= 1980 & HB0700 < 1990 ~ "1980-1989",
        HB0700 >= 1990 & HB0700 < 2000 ~ "1990-1999",
        HB0700 >= 2000 & HB0700 < 2010 ~ "2000-2009",
        HB0700 >= 2010 ~ "2010+",
        TRUE ~ NA_character_
      )
    )
  df$year_acquisition_category <- factor(
    df$year_acquisition_category,
    levels = c("Before 1980", "1980-1989", "1990-1999", "2000-2009", "2010+")
  )
  return(df)
})

hfcs.svy <- svrepdesign(
  weights = ~HW0010,
  repweights = w_file_at_de %>% 
    select(WR0001:WR0100) %>% as.data.frame(),
  data = imputationList(hfcs_at_de.list),
  scale = 1,
  rscale = rep(1/100, 100), 
  mse = TRUE,
  type = "other",
  combined.weights = TRUE
)

# 10. Then we are computing the survey-weighted distribution of "change_category"
# by "year_acquisition_category":


# First we are calculating survey-weighted category distribution by year of acquisition
imputation_results_by_year <- lapply(hfcs.svy$designs, function(single_design) {
  svytable(~change_category + year_acquisition_category + SA0100, design = single_design)
})

# Then we are combining results from all imputations by averaging
combined_results_by_year <- Reduce("+", imputation_results_by_year) / length(imputation_results_by_year)

# Converting combined results to a data frame
category_distribution_by_year_df <- as.data.frame(as.table(combined_results_by_year))
colnames(category_distribution_by_year_df) <- c("Change_Category", "Year_Acquisition_Category", "SA0100", "Weighted_Count")

# After that we are adding proportions within each acquisition year category for each country
category_distribution_by_year_df <- category_distribution_by_year_df %>%
  group_by(SA0100, Year_Acquisition_Category) %>%
  mutate(Proportion = Weighted_Count / sum(Weighted_Count))

# And finally we are viewing the final data frame
print("Survey-Weighted Distribution of Real Estate Value Changes by Acquisition Year:")
print(category_distribution_by_year_df)


# 11. The last step - visualization:

# Austria: 

category_distribution_by_year_AT <- category_distribution_by_year_df %>%
  filter(SA0100 == "AT")

ggplot(category_distribution_by_year_AT, aes(x = Year_Acquisition_Category, y = Proportion, fill = Change_Category)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(
    title = "Distribution of Real Estate Value Changes by Year of Acquisition (Austria)",
    x = "Year of Acquisition",
    y = "Proportion of Households",
    fill = "Change Category"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  


# Germany:

category_distribution_by_year_DE <- category_distribution_by_year_df %>%
  filter(SA0100 == "DE")

ggplot(category_distribution_by_year_DE, aes(x = Year_Acquisition_Category, y = Proportion, fill = Change_Category)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(
    title = "Distribution of Real Estate Value Changes by Year of Acquisition (Germany)",
    x = "Year of Acquisition",
    y = "Proportion of Households",
    fill = "Change Category"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

# Combined plot:

ggplot(category_distribution_by_year_df, aes(x = Year_Acquisition_Category, y = Proportion, fill = Change_Category)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(~SA0100, ncol = 1, labeller = as_labeller(c("AT" = "Austria", "DE" = "Germany"))) +
  labs(
    title = "Distribution of Real Estate Value Changes by Year of Acquisition (Austria and Germany)",
    x = "Year of Acquisition",
    y = "Proportion of Households",
    fill = "Change Category"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# The "Increase > 100%" category is the largest across all acquisition years in both Austria and Germany, particularly for older properties. 
# However, for more recent acquisitions, after 2010, this proportion decreases, and smaller increases become more common. Also, Austria shows 
# more variability over time, while Germany's distribution is more consistent, with the "Increase > 100%" category remaining dominant across all acquisition years.
# What's more, decreases in property value are minimal in both countries. 

# 12. Saving the datasets
write.csv(hfcs_at_de, "hfcs_real_estate_changes_by_year_AT_DE.csv")
write.csv(category_distribution_by_year_df, "category_distribution_by_year_AT_DE.csv")

##############################################################
##########################   TASK 2   ########################
##############################################################

range(hfcs_at_de$HB0700, na.rm = TRUE)
hasName(hfcs_at_de, "netliquidassets")
hfcs_at_de$SA0100 <- factor(hfcs_at_de$SA0100, levels = c("AT", "DE"))

#######################################################################################
# Scatter plots for Germany and Austria showing absolute changes
# in real estate properties values against the net liquid assets of the household.
######################################################################################

# In a very preliminary analysis and as a visual aid, we create these scatter plots in order to get a 
# sense for the relationship between these two variables, even though we obviously 
# cannot establish any sort of causality with these depictions alone.

#################
# AUSTRIA and GERMANY
#################

ggplot(hfcs_at_de, aes(x = change_absolute, y = netliquidassets, color = SA0100)) +
  geom_point() +
  labs(
    title = "Net liquid assets vs Absolute change (AUSTRIA and GERMANY)",
    x = "Absolute change in main residence real estate properties",
    y = "Net liquid assets",
    fill = "Country"
  ) +
  scale_color_manual(values = c("AT" = "blue", "DE" = "red")) +
  theme_minimal()

# We observe the change in real estate property value tends to increase with the 
# level of net liquid assets of the household. The same behavior can be
# observed for both Austria and Germany alone.

##########
# AUSTRIA
##########

ggplot(hfcs_at_de %>% filter(SA0100 == "AT"), aes(x = change_absolute, y = netliquidassets, color = SA0100)) +
  geom_point() +
  labs(
    title = "Net liquid assets vs Absolute change (AUSTRIA)",
    x = "Absolute change in main residence real estate properties",
    y = "Net liquid assets",
    fill = "Country"
  ) +
  scale_color_manual(values = c("AT" = "blue")) +
  theme_minimal()

##########
# GERMANY
#########

ggplot(hfcs_at_de %>% filter(SA0100 == "DE"), aes(x = change_absolute, y = netliquidassets, color = SA0100)) +
  geom_point() +
  labs(
    title = "Net liquid assets vs Absolute change (GERMANY)",
    x = "Absolute change in main residence real estate properties",
    y = "Net liquid assets",
    fill = "Country"
  ) +
  scale_color_manual(values = c("DE" = "red")) +
  theme_minimal()

#######################################################################################
# SCATTER PLOTS for GERMANY AND AUSTRIA showing absolute changes and change ratios
# in real estate properties values against the NET WORTH of the household.
######################################################################################

#################
# AUSTRIA and GERMANY
#################

ggplot(hfcs_at_de, aes(x = change_absolute, y = networth, color = SA0100)) +
  geom_point() +
  labs(
    title = "Net worth vs Absolute change (AUSTRIA and GERMANY)",
    x = "Absolute change in main residence real estate properties",
    y = "Net worth",
    fill = "Country"
  ) +
  scale_color_manual(values = c("AT" = "blue", "DE" = "red")) +
  theme_minimal()

# Again, we observe that the change in value of real estate properties tends to increase
# with the net worth. The same happens in both Austria and Germany.
# A univariate regression is carried out further down for both net worth and net 
# liquid assets.

##########
# AUSTRIA
##########

ggplot(hfcs_at_de %>% filter(SA0100 == "AT"), aes(x = change_absolute, y = networth, color = SA0100)) +
  geom_point() +
  labs(
    title = "Net worth vs Absolute change (AUSTRIA)",
    x = "Absolute change in main residence real estate properties",
    y = "Net worth",
    fill = "Country"
  ) +
  scale_color_manual(values = c("AT" = "blue")) +
  theme_minimal()

##########
# GERMANY
##########

ggplot(hfcs_at_de %>% filter(SA0100 == "DE"), aes(x = change_absolute, y = networth, color = SA0100)) +
  geom_point() +
  labs(
    title = "Net worth vs Absolute change (GERMANY)",
    x = "Absolute change in main residence real estate properties",
    y = "Net worth",
    fill = "Country"
  ) +
  scale_color_manual(values = c("DE" = "red")) +
  theme_minimal()

##################################################################
# Regresssion of change_absolute on NETLIQUIDASSETS for AT and DE
##################################################################

# This regression helps us see what we initially hinted at in the scatter plots:
# the more net liquid assets a household owns, the more the value of their 
# main residence has changed. The effect is small, but statistically significant and 
# positive. This regression has very limited explanatory power though (R-squared around 0.02).
# The phenomenon occurs in both countries and a more in detail account of the economics
# behind this observation will be given in the seminar paper.

regressions_at_de <- hfcs_at_de %>%
  select(netliquidassets, networth, change_absolute, SA0100)

# We get rid of the top 1% observations for each variable in the above data frame

percentiles <- regressions_at_de %>%
  summarise(
    netliquidassets_99 = quantile(netliquidassets, 0.99, na.rm = TRUE),
    networth_99 = quantile(networth, 0.99, na.rm = TRUE),
    change_absolute_99 = quantile(change_absolute, 0.99, na.rm = TRUE)
  )

regressions_at_de_filtered <- regressions_at_de %>%
  filter(
    netliquidassets <= percentiles$netliquidassets_99,
    networth <= percentiles$networth_99,
    change_absolute <= percentiles$change_absolute_99
  )

netliquidassets_model_at_de <- lm(
  change_absolute ~ netliquidassets,
  data = regressions_at_de_filtered,
)
summary(netliquidassets_model_at_de)

ggplot(regressions_at_de_filtered, aes(x = netliquidassets, y = change_absolute)) +
  geom_point(alpha = 0.6) +  
  geom_smooth(method = "lm", color = "blue", se = TRUE) +  
  labs(
    title = "Change in Absolute Real Estate Value vs Net Liquid assets",
    x = "Net Liquid assets",
    y = "Absolute Change in Real Estate Value"
  ) +
  theme_minimal()

# Regression for Austria

netliquidassets_model_at <- lm(
  change_absolute ~ netliquidassets,
  data = regressions_at_de_filtered,
  subset = SA0100 == "AT"
)
summary(netliquidassets_model_at)

ggplot(regressions_at_de_filtered[regressions_at_de_filtered$SA0100 == "AT", ], aes(x = netliquidassets, y = change_absolute)) +
  geom_point(alpha = 0.6) +  
  geom_smooth(method = "lm", color = "blue", se = TRUE) +  
  labs(
    title = "Absolute Change in Real Estate Values vs Net Liquid Assets",
    subtitle = "Austria",
    x = "Net Liquid Assets",
    y = "Absolute Change in Real Estate Value"
  ) +
  theme_minimal()

# Regression for Germany

netliquidassets_model_de <- lm(
  change_absolute ~ netliquidassets,
  data = regressions_at_de_filtered,
  subset = SA0100 == "DE"
)
summary(netliquidassets_model_de)

ggplot(regressions_at_de_filtered[regressions_at_de_filtered$SA0100 == "DE", ], aes(x = netliquidassets, y = change_absolute)) +
  geom_point(alpha = 0.6) +  
  geom_smooth(method = "lm", color = "blue", se = TRUE) +  
  labs(
    title = "Absolute Change in Real Estate Values vs Net Liquid Assets",
    subtitle = "Germany",
    x = "Net Liquid Assets",
    y = "Absolute Change in Real Estate Value"
  ) +
  theme_minimal()

##################################################################
# Regresssion of change_absolute on NETWORTH for AT and DE
##################################################################

# As it was observed with the net liquid assets, housebolds tend to experience
# a higher change in the value of their main residence over time the higher their
# net worth. The effect is positive, statistically significant and 
# of comparable size to the effect we obtained 
# in the case of the net liquid assets. The regression seems to have better 
# explanatory power (R squared around 0.18).
# 

networth_model_at_de <- lm(
  change_absolute ~ networth,
  data = regressions_at_de_filtered
)
summary(networth_model_at_de)

ggplot(regressions_at_de_filtered, aes(x = networth, y = change_absolute)) +
  geom_point(alpha = 0.6) +  
  geom_smooth(method = "lm", color = "blue", se = TRUE) +  
  labs(
    title = "Absolute Change in Real Estate Value vs Net Worth",
    subtitle = "Austria and Germany",
    x = "Net Worth",
    y = "Absolute Change in Real Estate Value"
  ) +
  theme_minimal()

# Regression for Austria

networth_model_at <- lm(
  change_absolute ~ networth,
  data = regressions_at_de_filtered,
  subset = SA0100 == "AT"
)
summary(networth_model_at)

ggplot(regressions_at_de_filtered[regressions_at_de_filtered$SA0100 == "AT", ], aes(x = networth, y = change_absolute)) +
  geom_point(alpha = 0.6) +  
  geom_smooth(method = "lm", color = "blue", se = TRUE) +  
  labs(
    title = "Absolute Change in Real Estate Values vs Net Worth",
    subtitle = "Austria",
    x = "Net Worth",
    y = "Absolute Change in Real Estate Value"
  ) +
  theme_minimal()

# Regression for Germany

networth_model_de <- lm(
  change_absolute ~ networth,
  data = regressions_at_de_filtered,
  subset = SA0100 == "DE"
)
summary(networth_model_de)

ggplot(regressions_at_de_filtered[regressions_at_de_filtered$SA0100 == "DE", ], aes(x = networth, y = change_absolute)) +
  geom_point(alpha = 0.6) +  
  geom_smooth(method = "lm", color = "blue", se = TRUE) +  
  labs(
    title = "Absolute Change in Real Estate Values vs Net Worth",
    subtitle = "Germany",
    x = "Net Worth",
    y = "Absolute Change in Real Estate Value"
  ) +
  theme_minimal()

# Further down the code we generate box plots relating these variables.

# Now we generate density graphs to observe how the net worth and the net liquid wealth 
# are distributed in Austria and Germany. The distributions are skewed to the left in both 
# countries: very few households have either a high net worth or high net liquid assets
# and the majority of the households are clustered near lower values.

##################################
# Distribution of Net Worth in Austria and Germany
##################################

ggplot(hfcs_at_de[hfcs_at_de$SA0100 == "AT", ], aes(x = networth, fill = SA0100)) +
  geom_density(alpha = 0.6, fill = "blue") + 
  labs(
    title = "Distribution of Net Worth in Austria",
    x = "Net Worth",
    y = "Density"
  ) +
  theme_minimal()

ggplot(hfcs_at_de[hfcs_at_de$SA0100 == "DE", ], aes(x = networth, fill = SA0100)) +
  geom_density(alpha = 0.6, fill = "blue") + 
  labs(
    title = "Distribution of Net Worth in Germany",
    x = "Net Worth",
    y = "Density"
  ) +
  theme_minimal()

ggplot(hfcs_at_de, aes(x = networth)) +
  geom_density(fill = "blue", alpha = 0.6) +
  facet_wrap(~ SA0100, ncol = 1, scales = "free") +
  labs(
    title = "Net Worth Distribution by Country",
    x = "Net Worth",
    y = "Density"
  ) +
  theme_minimal()

##################################
# Distribution of Net Liquid Assets in Austria and Germany
##################################

ggplot(hfcs_at_de[hfcs_at_de$SA0100 == "AT", ], aes(x = netliquidassets, fill = SA0100)) +
  geom_density(alpha = 0.6, fill = "blue") + 
  labs(
    title = "Distribution of Net Liquid Assets in Austria",
    x = "Net Liquid Assets",
    y = "Density"
  ) +
  theme_minimal()

ggplot(hfcs_at_de[hfcs_at_de$SA0100 == "DE", ], aes(x = netliquidassets, fill = SA0100)) +
  geom_density(alpha = 0.6, fill = "blue") + 
  labs(
    title = "Distribution of Net Liquid Assets in Germany",
    x = "Net Liquid Assets",
    y = "Density"
  ) +
  theme_minimal()

ggplot(hfcs_at_de, aes(x = netliquidassets)) +
  geom_density(fill = "blue", alpha = 0.6) +
  facet_wrap(~ SA0100, ncol = 1, scales = "free") +
  labs(
    title = "Net Liquid Assets Distribution by Country",
    x = "Net Liquid Assets",
    y = "Density"
  ) +
  theme_minimal()

# In order to complement our analysis, and the density graphs we generate summary statistics for the main 
# variables of our study for task 2. We find particulary helpful to know the percentiles
# of both the net liquid assets and the net worth.

####################
# Descriptive statistics for the Net Liquid Assets and the Net Worth of the Households.
####################

corrvariables_summary_statistics<- hfcs_at_de %>%
  group_by(SA0100) %>%
  summarise(
    # Summary of the Net Liquid Assets 
    mean_netliquidassets = mean(netliquidassets, na.rm = TRUE),
    median_netliquidassets = median(netliquidassets, na.rm = TRUE),
    sd_netliquidassets = sd(netliquidassets, na.rm = TRUE),
    var_netliquidassets = var(netliquidassets, na.rm = TRUE),
    p10_netliquidassets = quantile(netliquidassets, 0.10, na.rm = TRUE),
    p25_netliquidassets = quantile(netliquidassets, 0.25, na.rm = TRUE),
    p50_netliquidassets = quantile(netliquidassets, 0.50, na.rm = TRUE), # Median
    p75_netliquidassets = quantile(netliquidassets, 0.75, na.rm = TRUE),
    p90_netliquidassets = quantile(netliquidassets, 0.90, na.rm = TRUE),
    
    # Summary of the Net Worth 
    mean_networth = mean(networth, na.rm = TRUE),
    median_networth = median(networth, na.rm = TRUE),
    sd_networth = sd(networth, na.rm = TRUE),
    var_networth = var(networth, na.rm = TRUE),
    p10_networth = quantile(networth, 0.10, na.rm = TRUE),
    p25_networth = quantile(networth, 0.25, na.rm = TRUE),
    p50_networth = quantile(networth, 0.50, na.rm = TRUE), # Median
    p75_networth = quantile(networth, 0.75, na.rm = TRUE),
    p90_networth = quantile(networth, 0.90, na.rm = TRUE),
    
    # Observation Count
    count = n()
  )

# Now we proceed with the main analytical tool for this task which will be the 
# variable correlations.

#######################################################################################
#Correlations between both absolute and ratio changes and NET LIQUID ASSETS
#for AUSTRIA and GERMANY
#######################################################################################

####################
#AUSTRIA and GERMANY
####################

# The amount of net liquid assets that a household owns is positively correlated
# with how much the value of their main property has changed. The correlation
# is statistically significant in both countries, although they differ in size.
# The correlation with the ratio change is either not statistically significant
# or poorly correlated. We believe it has to do with how the ratio is 
# mathematically constructed.

cor(hfcs_at_de$change_absolute, hfcs_at_de$netliquidassets)
# 0.1451296
cor.test(hfcs_at_de$change_absolute, hfcs_at_de$netliquidassets)
#Pearson's product-moment correlation

#t = 19.166, df = 17073, p-value < 2.2e-16
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#  0.1304141 0.1597812

cor(hfcs_at_de$change_ratio, hfcs_at_de$netliquidassets)
#-0.01532924
cor.test(hfcs_at_de$change_ratio, hfcs_at_de$netliquidassets)
#Pearson's product-moment correlation

#t = -2.0032, df = 17073, p-value = 0.04517
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.0303216497 -0.0003299367

# Both correlations are statistically significant

#########
# AUSTRIA
#########

cor(
  subset(hfcs_at_de, SA0100 == "AT")$change_absolute,
  subset(hfcs_at_de, SA0100 == "AT")$netliquidassets,
  use = "complete.obs"
)
#0.105256
cor.test(
  subset(hfcs_at_de, SA0100 == "AT")$change_absolute,
  subset(hfcs_at_de, SA0100 == "AT")$netliquidassets,
  use = "complete.obs"
)

#t = 7.4189, df = 4913, p-value = 1.384e-13
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#  0.07752621 0.13282315

cor(
  subset(hfcs_at_de, SA0100 == "AT")$change_ratio,
  subset(hfcs_at_de, SA0100 == "AT")$netliquidassets,
  use = "complete.obs"
)
# 0.002979336
cor.test(
  subset(hfcs_at_de, SA0100 == "AT")$change_ratio,
  subset(hfcs_at_de, SA0100 == "AT")$netliquidassets,
  use = "complete.obs"
)
# Not statistically significant

#########
# GERMANY
#########

cor(
  subset(hfcs_at_de, SA0100 == "DE")$change_absolute,
  subset(hfcs_at_de, SA0100 == "DE")$netliquidassets,
  use = "complete.obs"
)
# 0.1441967
cor.test(
  subset(hfcs_at_de, SA0100 == "DE")$change_absolute,
  subset(hfcs_at_de, SA0100 == "DE")$netliquidassets,
  use = "complete.obs"
)

#t = 16.068, df = 12158, p-value < 2.2e-16
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#  0.1267474 0.1615568

cor(
  subset(hfcs_at_de, SA0100 == "DE")$change_ratio,
  subset(hfcs_at_de, SA0100 == "DE")$netliquidassets,
  use = "complete.obs"
)
# -0.01293322
cor.test(
  subset(hfcs_at_de, SA0100 == "DE")$change_ratio,
  subset(hfcs_at_de, SA0100 == "DE")$netliquidassets,
  use = "complete.obs"
)
# Not statistically significant.

#######################################################################################
#Correlations between both absolute and ratio changes and NET WORTH
#for AUSTRIA and GERMANY
#######################################################################################

####################
#AUSTRIA and GERMANY
####################

# The absolute change in the value of the main residence and the net worth of the household
# are very strongly correlated. The correlation is very high in both countries
# as well as statistically significant. We encounter the same situation with the ratio
# change all along our analysis.

cor(hfcs_at_de$change_absolute, hfcs_at_de$networth)
# 0.425461
cor.test(hfcs_at_de$change_absolute, hfcs_at_de$networth)
#t = 61.43, df = 17073, p-value < 2.2e-16
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#  0.4130979 0.4376674

cor(hfcs_at_de$change_ratio, hfcs_at_de$networth)
# 0.02070045
cor.test(hfcs_at_de$change_ratio, hfcs_at_de$networth)
#t = 2.7054, df = 17073, p-value = 0.006829
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#  0.005702844 0.035688752

##########
# AUSTRIA
##########

cor(
  subset(hfcs_at_de, SA0100 == "AT")$change_absolute,
  subset(hfcs_at_de, SA0100 == "AT")$networth,
  use = "complete.obs"
)
# 0.4149755
cor.test(
  subset(hfcs_at_de, SA0100 == "AT")$change_absolute,
  subset(hfcs_at_de, SA0100 == "AT")$networth,
  use = "complete.obs"
)
#t = 31.969, df = 4913, p-value < 2.2e-16
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.3915604 0.4378536

cor(
  subset(hfcs_at_de, SA0100 == "AT")$change_ratio,
  subset(hfcs_at_de, SA0100 == "AT")$networth,
  use = "complete.obs"
)
# 0.06753819
cor.test(
  subset(hfcs_at_de, SA0100 == "AT")$change_ratio,
  subset(hfcs_at_de, SA0100 == "AT")$networth,
  use = "complete.obs"
)
#t = 4.7448, df = 4913, p-value = 2.147e-06
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.03965510 0.09531619

##########
# GERMANY
##########

cor(
  subset(hfcs_at_de, SA0100 == "DE")$change_absolute,
  subset(hfcs_at_de, SA0100 == "DE")$networth,
  use = "complete.obs"
)
# 0.4236373
cor.test(
  subset(hfcs_at_de, SA0100 == "DE")$change_absolute,
  subset(hfcs_at_de, SA0100 == "DE")$networth,
  use = "complete.obs"
)
#t = 51.568, df = 12158, p-value < 2.2e-16
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#  0.4089424 0.4381126

cor(
  subset(hfcs_at_de, SA0100 == "DE")$change_ratio,
  subset(hfcs_at_de, SA0100 == "DE")$networth,
  use = "complete.obs"
)
# 0.01818098
cor.test(
  subset(hfcs_at_de, SA0100 == "DE")$change_ratio,
  subset(hfcs_at_de, SA0100 == "DE")$networth,
  use = "complete.obs"
)
#t = 2.005, df = 12158, p-value = 0.04498
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#  0.0004069515 0.0359435294

#######################################################################################
#Correlations between both absolute and ratio changes and the SIZE OF THE MAIN RESIDENCE
#for AUSTRIA and GERMANY
#######################################################################################

cor(hfcs_at_de$change_absolute, hfcs_at_de$HB0100)
# 0.2211591
cor.test(hfcs_at_de$change_absolute, hfcs_at_de$HB0100)
#t = 29.631, df = 17073, p-value < 2.2e-16
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#  0.2068459 0.2353777

##########
# AUSTRIA
##########

cor(
  subset(hfcs_at_de, SA0100 == "AT")$change_absolute,
  subset(hfcs_at_de, SA0100 == "AT")$HB0100,
  use = "complete.obs"
)
#0.1694501

cor.test(
  subset(hfcs_at_de, SA0100 == "AT")$change_absolute,
  subset(hfcs_at_de, SA0100 == "AT")$HB0100,
  use = "complete.obs"
)
# t = 12.052, df = 4913, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.1421656 0.1964772

##########
# GERMANY
##########

cor(
  subset(hfcs_at_de, SA0100 == "DE")$change_absolute,
  subset(hfcs_at_de, SA0100 == "DE")$HB0100,
  use = "complete.obs"
)
# 0.2328015

cor.test(
  subset(hfcs_at_de, SA0100 == "DE")$change_absolute,
  subset(hfcs_at_de, SA0100 == "DE")$HB0100,
  use = "complete.obs"
)
#t = 26.395, df = 12158, p-value < 2.2e-16
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#  0.2159208 0.2495431

#######################################################################################
#Correlations between both absolute and ratio changes and the AGE OF THE MAIN RESIDENCE
#for AUSTRIA and GERMANY
#######################################################################################

####################
#AUSTRIA and GERMANY
####################

# We construct the age of the main residence as the difference between the year the 
# survey was conducted and year of property acquisition. We understand this may not
# be the real age of the property, but it is the time that has passed that ultimately
# allows us to observe a change in value.
# The change in value of the main property is positively correlated with the age
# of the main residence in both countries, which is a finding in line with the 
# consulted literature. We give a more in detail account of the economics behind this result
# in the seminar paper.

hfcs_at_de$age_main_residence <- 2021 - hfcs_at_de$HB0700
sum(is.na(hfcs_at_de$age_main_residence))
hfcs_at_de$age_main_residence[is.na(hfcs_at_de$age_main_residence)] <- 0

cor(hfcs_at_de$change_absolute, hfcs_at_de$age_main_residence)
# 0.1896468
cor.test(hfcs_at_de$change_absolute, hfcs_at_de$age_main_residence)
#t = 25.238, df = 17073, p-value < 2.2e-16
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.1751457 0.2040657

cor(hfcs_at_de$change_ratio, hfcs_at_de$age_main_residence)
# 0.1414796
cor.test(hfcs_at_de$change_ratio, hfcs_at_de$age_main_residence)
#t = 18.674, df = 17073, p-value < 2.2e-16
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#  0.1267492 0.1561477

##########
# AUSTRIA
##########

cor(
  subset(hfcs_at_de, SA0100 == "AT")$change_absolute,
  subset(hfcs_at_de, SA0100 == "AT")$age_main_residence,
  use = "complete.obs"
)
# 0.2134947
cor.test(
  subset(hfcs_at_de, SA0100 == "AT")$change_absolute,
  subset(hfcs_at_de, SA0100 == "AT")$age_main_residence,
  use = "complete.obs"
)
#t = 15.318, df = 4913, p-value < 2.2e-16
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#  0.1866508 0.2400200

cor(
  subset(hfcs_at_de, SA0100 == "AT")$change_ratio,
  subset(hfcs_at_de, SA0100 == "AT")$age_main_residence,
  use = "complete.obs"
)
# 0.2296072
cor.test(
  subset(hfcs_at_de, SA0100 == "AT")$change_ratio,
  subset(hfcs_at_de, SA0100 == "AT")$age_main_residence,
  use = "complete.obs"
)
#t = 16.536, df = 4913, p-value < 2.2e-16
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#0.2029520 0.2559223

##########
# GERMANY
##########

cor(
  subset(hfcs_at_de, SA0100 == "DE")$change_absolute,
  subset(hfcs_at_de, SA0100 == "DE")$age_main_residence,
  use = "complete.obs"
)
# 0.1904536
cor.test(
  subset(hfcs_at_de, SA0100 == "DE")$change_absolute,
  subset(hfcs_at_de, SA0100 == "DE")$age_main_residence,
  use = "complete.obs"
)
#t = 21.392, df = 12158, p-value < 2.2e-16
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#  0.1732660 0.2075253

cor(
  subset(hfcs_at_de, SA0100 == "DE")$change_ratio,
  subset(hfcs_at_de, SA0100 == "DE")$age_main_residence,
  use = "complete.obs"
)
# 0.1027926
cor.test(
  subset(hfcs_at_de, SA0100 == "DE")$change_ratio,
  subset(hfcs_at_de, SA0100 == "DE")$age_main_residence,
  use = "complete.obs"
)
#t = 11.395, df = 12158, p-value < 2.2e-16
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#  0.08517406 0.12034689

###############################################################################
# Correlation between NET LIQUID ASSETS and the CURRENT PRICE OF MAIN RESIDENCE
# for AUSTRIA and GERMANY
###############################################################################

# The correlation between these two variables allows us to refine the profile of 
# household that has experienced the most changes in the value of their main residence.
# Having more liquid assets in both countries is positively correlated with a higher
# value of a main residence.

################################
###### AUSTRIA AND GERMANY
################################

cor(hfcs_at_de$netliquidassets, hfcs_at_de$HB0900)
# 0.2185628
cor.test(hfcs_at_de$netliquidassets, hfcs_at_de$HB0900)
#t = 29.266, df = 17073, p-value < 2.2e-16
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.2042329 0.2327990

##########
# AUSTRIA
##########

cor(
  subset(hfcs_at_de, SA0100 == "AT")$netliquidassets,
  subset(hfcs_at_de, SA0100 == "AT")$HB0900,
  use = "complete.obs"
)
# 0.1657941
cor.test(
  subset(hfcs_at_de, SA0100 == "AT")$netliquidassets,
  subset(hfcs_at_de, SA0100 == "AT")$HB0900,
  use = "complete.obs"
)
#t = 11.784, df = 4913, p-value < 2.2e-16
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.1384780 0.1928582

##########
# GERMANY
##########
cor(
  subset(hfcs_at_de, SA0100 == "DE")$netliquidassets,
  subset(hfcs_at_de, SA0100 == "DE")$HB0900,
  use = "complete.obs"
)
# 0.2117545
cor.test(
  subset(hfcs_at_de, SA0100 == "DE")$netliquidassets,
  subset(hfcs_at_de, SA0100 == "DE")$HB0900,
  use = "complete.obs"
)
#t = 23.891, df = 12158, p-value < 2.2e-16
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#  0.1947132 0.2286680

###############################################################################
# Correlation between AGE OF THE REF. PERSON and the NET LIQUID ASSETS
# for AUSTRIA and GERMANY
###############################################################################

# Though statistically significant, the age of the household's reference person
# doesn't seem to be very strongly correlated with the amount of net liquid assets that 
# the household has.

################################
###### AUSTRIA AND GERMANY
################################

names(hfcs_at_de)[names(hfcs_at_de) == "DHAGEH1"] <-  "agerefperson"

cor(hfcs_at_de$agerefperson, hfcs_at_de$netliquidassets)
#0.08361954
cor.test(hfcs_at_de$agerefperson, hfcs_at_de$netliquidassets)
#t = 10.964, df = 17073, p-value < 2.2e-16
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#  0.06870634 0.09849539

##########
# AUSTRIA
##########

cor(
  subset(hfcs_at_de, SA0100 == "AT")$agerefperson,
  subset(hfcs_at_de, SA0100 == "AT")$netliquidassets,
  use = "complete.obs"
)
#0.02650862
cor.test(
  subset(hfcs_at_de, SA0100 == "AT")$agerefperson,
  subset(hfcs_at_de, SA0100 == "AT")$netliquidassets,
  use = "complete.obs"
)
#t = 1.8587, df = 4913, p-value = 0.06313
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.001450426  0.054426262

##########
# GERMANY
##########
cor(
  subset(hfcs_at_de, SA0100 == "DE")$agerefperson,
  subset(hfcs_at_de, SA0100 == "DE")$netliquidassets,
  use = "complete.obs"
)
#0.08914434
cor.test(
  subset(hfcs_at_de, SA0100 == "DE")$agerefperson,
  subset(hfcs_at_de, SA0100 == "DE")$netliquidassets,
  use = "complete.obs"
)
#t = 9.8686, df = 12158, p-value < 2.2e-16
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.07148344 0.10674936

###############################################################################
# Correlation between NET WORTH and the CURRENT PRICE OF MAIN RESIDENCE
# for AUSTRIA and GERMANY
###############################################################################

# This correlation allows us to further analyse the target of our analysis.
# We obtain an expected result: wealthy people tend to earn more valuable main residences.
# The correlation is very strong in both countries and statistically significant.

################################
###### AUSTRIA AND GERMANY
################################

cor(hfcs_at_de$networth, hfcs_at_de$HB0900)
# 0.5968604
cor.test(hfcs_at_de$networth, hfcs_at_de$HB0900)
#t = 97.2, df = 17073, p-value < 2.2e-16
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.5871172 0.6064307

##########
# AUSTRIA
##########

cor(
  subset(hfcs_at_de, SA0100 == "AT")$networth,
  subset(hfcs_at_de, SA0100 == "AT")$HB0900,
  use = "complete.obs"
)
# 0.4864522
cor.test(
  subset(hfcs_at_de, SA0100 == "AT")$networth,
  subset(hfcs_at_de, SA0100 == "AT")$HB0900,
  use = "complete.obs"
)
# t = 39.025, df = 4913, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.4648158 0.5075079

##########
# GERMANY
##########
cor(
  subset(hfcs_at_de, SA0100 == "DE")$networth,
  subset(hfcs_at_de, SA0100 == "DE")$HB0900,
  use = "complete.obs"
)
# 0.607397
cor.test(
  subset(hfcs_at_de, SA0100 == "DE")$networth,
  subset(hfcs_at_de, SA0100 == "DE")$HB0900,
  use = "complete.obs"
)
#t = 84.307, df = 12158, p-value < 2.2e-16
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#  0.5960579 0.6184939

#######################################################################################
#Correlations between both absolute and ratio changes and NET LIQUID ASSETS
#AS A FRACTION OF ANNUAL GROSS INCOME
#######################################################################################

cor(hfcs_at_de$change_absolute, hfcs_at_de$DNNLARATIO)
# 0.002202499
cor.test(hfcs_at_de$change_absolute, hfcs_at_de$DNNLARATIO)
# Statistically not significant.

cor(hfcs_at_de$change_ratio, hfcs_at_de$DNNLARATIO)
# -0.003573304
cor.test(hfcs_at_de$change_ratio, hfcs_at_de$DNNLARATIO)
# Statistically not significant.

#Both correlations are not statistically signifficant

###############################################################################
# Correlation between the AGE OF THE REFERENCE PERSON  
# and the NET WORTH for AUSTRIA and GERMANY
###############################################################################

# Age is positively correlated with how much net worth a household owns.
# Here we don't obtain a very strong correlation, but we analyse this result in 
# the seminar paper and give reasons for why this may be the case.
# A discussion of the pros and cons of using the reference person as the 
# object of our analysis will follow in the seminar paper. 

################################
###### AUSTRIA AND GERMANY
################################

cor(hfcs_at_de$agerefperson, hfcs_at_de$networth)
#0.04997861
cor.test(hfcs_at_de$agerefperson, hfcs_at_de$networth)
#t = 6.5386, df = 17073, p-value = 6.387e-11
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.03500547 0.06492932

##########
# AUSTRIA
##########

cor(
  subset(hfcs_at_de, SA0100 == "AT")$agerefperson,
  subset(hfcs_at_de, SA0100 == "AT")$networth,
  use = "complete.obs"
)
#0.02220087
cor.test(
  subset(hfcs_at_de, SA0100 == "AT")$agerefperson,
  subset(hfcs_at_de, SA0100 == "AT")$networth,
  use = "complete.obs"
)
# Not statistically significant.

##########
# GERMANY
##########

cor(
  subset(hfcs_at_de, SA0100 == "DE")$agerefperson,
  subset(hfcs_at_de, SA0100 == "DE")$networth,
  use = "complete.obs"
)
#0.04579508
cor.test(
  subset(hfcs_at_de, SA0100 == "DE")$agerefperson,
  subset(hfcs_at_de, SA0100 == "DE")$networth,
  use = "complete.obs"
)
#data:  subset(hfcs_at_de, SA0100 == "DE")$agerefperson and subset(hfcs_at_de, SA0100 == "DE")$networth
#t = 5.0548, df = 12158, p-value = 4.371e-07
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#  0.02804375 0.06351754

###############################################################################
# Correlation between the EDUCATION OF THE REFERENCE PERSON and  
# the NET WORTH, the NET LIQUID ASSETS and the ABSOLUTE CHANGE for AUSTRIA and GERMANY
###############################################################################

# Education is positively correlated with how wealthy households are.
# This is a result in line with what the economic theory predicts.

################################
###### AUSTRIA AND GERMANY
################################

names(hfcs_at_de)[names(hfcs_at_de) == "DHEDUH1"] <-  "edurefperson"

cor(hfcs_at_de$edurefperson, hfcs_at_de$networth)
#0.1405955
cor.test(hfcs_at_de$edurefperson, hfcs_at_de$networth)
# t = 18.555, df = 17073, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# 0.1258616 0.1552675

cor(hfcs_at_de$edurefperson, hfcs_at_de$netliquidassets)
#0.1314238
cor.test(hfcs_at_de$edurefperson, hfcs_at_de$netliquidassets)
# t = 17.323, df = 17073, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# 0.1166544 0.1461351

cor(hfcs_at_de$edurefperson, hfcs_at_de$change_absolute)
# 0.08538458
cor.test(hfcs_at_de$edurefperson, hfcs_at_de$change_absolute)
#Statistically significant

##########
# AUSTRIA
##########

cor(
  subset(hfcs_at_de, SA0100 == "AT")$edurefperson,
  subset(hfcs_at_de, SA0100 == "AT")$networth,
  use = "complete.obs"
)
# 0.03970674

cor.test(
  subset(hfcs_at_de, SA0100 == "AT")$edurefperson,
  subset(hfcs_at_de, SA0100 == "AT")$networth,
  use = "complete.obs"
)
#t = 2.7854, df = 4913, p-value = 0.005367
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.01176182 0.06758968

cor(
  subset(hfcs_at_de, SA0100 == "AT")$edurefperson,
  subset(hfcs_at_de, SA0100 == "AT")$netliquidassets,
  use = "complete.obs"
)
# 0.09331465

cor.test(
  subset(hfcs_at_de, SA0100 == "AT")$edurefperson,
  subset(hfcs_at_de, SA0100 == "AT")$netliquidassets,
  use = "complete.obs"
)
# t = 6.5693, df = 4913, p-value = 5.577e-11
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# 0.06552763 0.12095706

cor(
  subset(hfcs_at_de, SA0100 == "AT")$edurefperson,
  subset(hfcs_at_de, SA0100 == "AT")$change_absolute,
  use = "complete.obs"
)
# 0.0002123282

cor.test(
  subset(hfcs_at_de, SA0100 == "AT")$edurefperson,
  subset(hfcs_at_de, SA0100 == "AT")$change_absolute,
  use = "complete.obs"
)
# Not statistically significant

##########
# GERMANY
##########

cor(
  subset(hfcs_at_de, SA0100 == "DE")$edurefperson,
  subset(hfcs_at_de, SA0100 == "DE")$networth,
  use = "complete.obs"
)
# 0.1282525

cor.test(
  subset(hfcs_at_de, SA0100 == "DE")$edurefperson,
  subset(hfcs_at_de, SA0100 == "DE")$networth,
  use = "complete.obs"
)
#t = 14.259, df = 12158, p-value < 2.2e-16
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#  0.1107307 0.1456945

cor(
  subset(hfcs_at_de, SA0100 == "DE")$edurefperson,
  subset(hfcs_at_de, SA0100 == "DE")$netliquidassets,
  use = "complete.obs"
)
#0.1108675

cor.test(
  subset(hfcs_at_de, SA0100 == "DE")$edurefperson,
  subset(hfcs_at_de, SA0100 == "DE")$netliquidassets,
  use = "complete.obs"
)
# t = 12.3, df = 12158, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.09327711 0.12838863

cor(
  subset(hfcs_at_de, SA0100 == "DE")$edurefperson,
  subset(hfcs_at_de, SA0100 == "DE")$change_absolute,
  use = "complete.obs"
)
# 0.09178095

cor.test(
  subset(hfcs_at_de, SA0100 == "DE")$edurefperson,
  subset(hfcs_at_de, SA0100 == "DE")$change_absolute,
  use = "complete.obs"
)
# t = 10.163, df = 12158, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.07412772 0.10937669

###############################################################################
# Correlation between the AGE OF THE REFERENCE PERSON  
# and the ABSOLUTE CHANGE IN VALUE OF MAIN RESIDENCE for AUSTRIA and GERMANY
# and AGE OF THE REFERENCE PERSON and AGE OF MAIN RESIDENCE
###############################################################################

# The age of the reference person is positively correlated with how much
# the household's main residence changes in value. Also, older people
# own older main residences. Both results go hand in hand and are predicted
# by the economic theory.

################################
###### AUSTRIA AND GERMANY
################################

cor(hfcs_at_de$agerefperson, hfcs_at_de$change_absolute)
#0.1602204
cor.test(hfcs_at_de$agerefperson, hfcs_at_de$change_absolute)
# t = 21.209, df = 17073, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.1455709 0.1747997

cor(hfcs_at_de$agerefperson, hfcs_at_de$age_main_residence)
#0.6347793
cor.test(hfcs_at_de$agerefperson, hfcs_at_de$age_main_residence)
# t = 107.34, df = 17073, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.6257377 0.6436503

##########
# AUSTRIA
##########

cor(
  subset(hfcs_at_de, SA0100 == "AT")$agerefperson,
  subset(hfcs_at_de, SA0100 == "AT")$change_absolute,
  use = "complete.obs"
)
#0.2088866
cor.test(
  subset(hfcs_at_de, SA0100 == "AT")$agerefperson,
  subset(hfcs_at_de, SA0100 == "AT")$change_absolute,
  use = "complete.obs"
)
# t = 14.972, df = 4913, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# 0.1819915 0.2354694

cor(
  subset(hfcs_at_de, SA0100 == "AT")$agerefperson,
  subset(hfcs_at_de, SA0100 == "AT")$age_main_residence,
  use = "complete.obs"
)
#0.6448041
cor.test(
  subset(hfcs_at_de, SA0100 == "AT")$agerefperson,
  subset(hfcs_at_de, SA0100 == "AT")$age_main_residence,
  use = "complete.obs"
)
# t = 59.13, df = 4913, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# 0.6281704 0.6608487

##########
# GERMANY
##########

cor(
  subset(hfcs_at_de, SA0100 == "DE")$agerefperson,
  subset(hfcs_at_de, SA0100 == "DE")$change_absolute,
  use = "complete.obs"
)
#0.1398171
cor.test(
  subset(hfcs_at_de, SA0100 == "DE")$agerefperson,
  subset(hfcs_at_de, SA0100 == "DE")$change_absolute,
  use = "complete.obs"
)
# t = 15.57, df = 12158, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# 0.1223470 0.1572006

cor(
  subset(hfcs_at_de, SA0100 == "DE")$agerefperson,
  subset(hfcs_at_de, SA0100 == "DE")$age_main_residence,
  use = "complete.obs"
)
# 0.6399478
cor.test(
  subset(hfcs_at_de, SA0100 == "DE")$agerefperson,
  subset(hfcs_at_de, SA0100 == "DE")$age_main_residence,
  use = "complete.obs"
)
# t = 91.829, df = 12158, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.6293320 0.6503248

###############################################################################
# Correlation between the GENDER OF THE REFERENCE PERSON  
# and the ABSOLUTE CHANGE IN VALUE OF MAIN RESIDENCE for AUSTRIA and GERMANY
###############################################################################

# The gender of the reference person is not relevant when it comes to 
# studying the changes in value of the household's main residence.

################################
###### AUSTRIA AND GERMANY
################################

cor(hfcs_at_de$DHGENDERH1, hfcs_at_de$change_absolute)
#0.02260614 (Very weak)
cor.test(hfcs_at_de$DHGENDERH1, hfcs_at_de$change_absolute)
#p-value = 0.003135

##########
# AUSTRIA
##########

cor(
  subset(hfcs_at_de, SA0100 == "AT")$DHGENDERH1,
  subset(hfcs_at_de, SA0100 == "AT")$change_absolute,
  use = "complete.obs"
)
# 0.0220021

cor.test(
  subset(hfcs_at_de, SA0100 == "AT")$DHGENDERH1,
  subset(hfcs_at_de, SA0100 == "AT")$change_absolute,
  use = "complete.obs"
)
# t = 1.5426, df = 4913, p-value = 0.123
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.005959537  0.049929367

##########
# GERMANY
##########

cor(
  subset(hfcs_at_de, SA0100 == "DE")$DHGENDERH1,
  subset(hfcs_at_de, SA0100 == "DE")$change_absolute,
  use = "complete.obs"
)
# -0.03526242

cor.test(
  subset(hfcs_at_de, SA0100 == "DE")$DHGENDERH1,
  subset(hfcs_at_de, SA0100 == "DE")$change_absolute,
  use = "complete.obs"
)

# t = -3.8906, df = 12158, p-value = 0.0001005
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.05300336 -0.01749922

####################
# Descriptive statistics for the age and education of the reference person 
# of the household.
####################

# The following summary statistics help us complement our analysis after having
# computed the correlations that include the age and education of the reference
# person.

eduage_summary_statistics <- hfcs_at_de %>%
  group_by(SA0100) %>%
  summarise(
    mean_agerefperson = mean(agerefperson, na.rm = TRUE),
    median_agerefperson = median(agerefperson, na.rm = TRUE),
    sd_agerefperson = sd(agerefperson, na.rm = TRUE),
    mean_edurefperson = mean(edurefperson, na.rm = TRUE),
    median_edurefperson = median(edurefperson, na.rm = TRUE),
    sd_edurefperson = sd(edurefperson, na.rm = TRUE),
    count = n()
  )

print(eduage_summary_statistics)

print("Descriptive Statistics: Education and Age of the Reference person:")
print(corrvariables_summary_statistics)

#####################################
#####################################

# Now that we have been able to profile the households that have suffered 
# the most change in their real estate properties i.e. main residences, 
# we are now in a position that allows us to say something else about 
# where these households live, the age of the financially knowledgeable 
# person (reference person), its educational attainment and how much net 
# liquid assets and net worth they own. 

range(hfcs_at_de$agerefperson, na.rm = TRUE) #18-85 years old

# First it is a good idea to change the coding of the regions for clarity purposes.

hfcs_at_de <- hfcs_at_de %>%
  mutate(DHREGION = case_when(
    DHREGION == "AT11" ~ "Burgenland",
    DHREGION == "AT12" ~ "Lower Austria",
    DHREGION == "AT13" ~ "Vienna",
    DHREGION == "AT21" ~ "Carinthia",
    DHREGION == "AT22" ~ "Styria",
    DHREGION == "AT31" ~ "Upper Austria",
    DHREGION == "AT32" ~ "Salzburg",
    DHREGION == "AT33" ~ "Tyrol",
    DHREGION == "AT34" ~ "Vorarlberg",
    DHREGION == "DENW" ~ "North-Western Germany",
    DHREGION == "DEWW" ~ "Western Germany",
    DHREGION == "DEOS" ~ "Eastern Germany",
    DHREGION == "DESW" ~ "Southern Germany",
    TRUE ~ DHREGION  
  ))

# We split first per region.

#########
# AUSTRIA
#########

profile_region_at <- hfcs_at_de %>%
  filter(SA0100 == "AT") %>%
  group_by(DHREGION) %>%  
  summarize(
    mean_change_absolute = mean(change_absolute, na.rm = TRUE),
    median_change_absolute = median(change_absolute, na.rm = TRUE),
    sd_change_absolute = sd(change_absolute, na.rm = TRUE),
    var_change_absolute = var(change_absolute, na.rm = TRUE),
    mean_age = mean(agerefperson, na.rm = TRUE),
    median_age = median(agerefperson, na.rm = TRUE),
    mean_edu = mean(edurefperson, na.rm = TRUE),
    median_edu = median(edurefperson, na.rm = TRUE),
    mean_net_liquid_assets = mean(netliquidassets, na.rm = TRUE),
    median_net_liquid_assets = median(netliquidassets, na.rm = TRUE),
    mean_net_worth = mean(networth, na.rm = TRUE),
    median_net_worth = median(networth, na.rm = TRUE),
    mean_main_residence_sqm = mean(HB0100, na.rm = TRUE),
    median_main_residence_sqm = mean(HB0100, na.rm = TRUE),
    ratio_change_absolute_main_residence_sqm = 
      mean(change_absolute / HB0100, na.rm = TRUE),
    median(change_absolute / HB0100, na.rm = TRUE)
  )

ggplot(profile_region_at, aes(x = DHREGION, y = mean_change_absolute)) +
  geom_col(aes(color = DHREGION, fill = DHREGION), alpha = 0.3) +
  labs(
    title = "Mean Absolute Change by Region (Austria)",
    x = "Region",
    y = "Mean Absolute Change",
    subtitle = "Grouped by Federal State"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",  
    axis.text.x = element_text(angle = 45, hjust = 1)  
  )

ggplot(profile_region_at, aes(x = DHREGION, y = median_change_absolute)) +
  geom_col(aes(color = DHREGION, fill = DHREGION), alpha = 0.3) +
  labs(
    title = "Median Absolute Change by Region (Austria)",
    x = "Region",
    y = "Median Absolute Change",
    subtitle = "Grouped by Federal State"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",  
    axis.text.x = element_text(angle = 45, hjust = 1)  
  )

ggplot(profile_region_at, aes(x = DHREGION, y = `median(change_absolute/HB0100, na.rm = TRUE)`)) +
  geom_col(aes(color = DHREGION, fill = DHREGION), alpha = 0.3) +
  labs(
    title = "Median Absolute Change per Square Meter (Austria)",
    x = "Federal State",
    y = "Median Absolute Change per Sqm",
    subtitle = "Grouped by Federal State"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",  
    axis.text.x = element_text(angle = 45, hjust = 1)  
  )

#########
# GERMANY
#########

profile_region_de <- hfcs_at_de %>%
  filter(SA0100 == "DE") %>%
  group_by(DHREGION) %>%  
  summarize(
    mean_change_absolute = mean(change_absolute, na.rm = TRUE),
    median_change_absolute = median(change_absolute, na.rm = TRUE),
    sd_change_absolute = sd(change_absolute, na.rm = TRUE),
    var_change_absolute = var(change_absolute, na.rm = TRUE),
    mean_age = mean(agerefperson, na.rm = TRUE),
    median_age = median(agerefperson, na.rm = TRUE),
    mean_edu = mean(edurefperson, na.rm = TRUE),
    median_edu = median(edurefperson, na.rm = TRUE),
    mean_net_liquid_assets = mean(netliquidassets, na.rm = TRUE),
    median_net_liquid_assets = median(netliquidassets, na.rm = TRUE),
    mean_net_worth = mean(networth, na.rm = TRUE),
    median_net_worth = median(networth, na.rm = TRUE),
    mean_main_residence_sqm = mean(HB0100, na.rm = TRUE),
    median_main_residence_sqm = mean(HB0100, na.rm = TRUE),
    ratio_change_absolute_main_residence_sqm = 
      mean(change_absolute / HB0100, na.rm = TRUE),
    median(change_absolute / HB0100, na.rm = TRUE)
  )

ggplot(profile_region_de, aes(x = DHREGION, y = mean_change_absolute)) +
  geom_col(aes(color = DHREGION, fill = DHREGION), alpha = 0.3) +
  labs(
    title = "Mean Absolute Change by Region (Germany)",
    x = "Region",
    y = "Mean Absolute Change",
    subtitle = "Grouped by Region"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",  
    axis.text.x = element_text(angle = 45, hjust = 1)  
  )

ggplot(profile_region_de, aes(x = DHREGION, y = median_change_absolute)) +
  geom_col(aes(color = DHREGION, fill = DHREGION), alpha = 0.3) +
  labs(
    title = "Median Absolute Change by Region (Germany)",
    x = "Region",
    y = "Median Absolute Change",
    subtitle = "Grouped by Region"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",  
    axis.text.x = element_text(angle = 45, hjust = 1)  
  )

ggplot(profile_region_de, aes(x = DHREGION, y = `median(change_absolute/HB0100, na.rm = TRUE)`)) +
  geom_col(aes(color = DHREGION, fill = DHREGION), alpha = 0.3) +
  labs(
    title = "Median Absolute Change per Square Meter (Germany)",
    x = "Region",
    y = "Median Absolute Change per Sqm",
    subtitle = "Grouped by Region"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",  
    axis.text.x = element_text(angle = 45, hjust = 1)  
  )

# We now split the ages of the reference person in brackets of 10 years, except 
# the first age group that lies between 18 to 25 years old. Then, for each age group,
# we compute the summary statistics (mean and median) of the absoulute change in value of real estate
# property, the age of the reference person, the net liquid assets and the net worth.
# We do this separately for Austria and Germany.

#########
# AUSTRIA
#########

profile_ages_at <- hfcs_at_de %>%
  filter(SA0100 == "AT") %>%
  mutate(age_group = cut(agerefperson, 
                         breaks = c(18, 25, 35, 45, 55, 65, 75, 85), 
                         labels = c("18-25", "26-35", "36-45", "46-55", "56-65", "66-75", "76-85"),
                         include.lowest = TRUE)) %>% 
  group_by(DHREGION, age_group) %>%  
  summarize(
    mean_change_absolute = mean(change_absolute, na.rm = TRUE),
    median_change_absolute = median(change_absolute, na.rm = TRUE),
    sd_change_absolute = sd(change_absolute, na.rm = TRUE),
    var_change_absolute = var(change_absolute, na.rm = TRUE),
    mean_age = mean(agerefperson, na.rm = TRUE),
    median_age = median(agerefperson, na.rm = TRUE),
    mean_edu = mean(edurefperson, na.rm = TRUE),
    median_edu = median(edurefperson, na.rm = TRUE),
    mean_net_liquid_assets = mean(netliquidassets, na.rm = TRUE),
    median_net_liquid_assets = median(netliquidassets, na.rm = TRUE),
    mean_net_worth = mean(networth, na.rm = TRUE),
    median_net_worth = median(networth, na.rm = TRUE),
    mean_main_residence_sqm = mean(HB0100, na.rm = TRUE),
    median_main_residence_sqm = mean(HB0100, na.rm = TRUE),
    ratio_change_absolute_main_residence_sqm = 
      mean(change_absolute / HB0100, na.rm = TRUE),
    median(change_absolute / HB0100, na.rm = TRUE)
  )

max_mean_change_absolute_at <- max(profile_ages_at$mean_change_absolute, na.rm = TRUE)
max_median_change_absolute_at <- max(profile_ages_at$median_change_absolute, na.rm = TRUE)

# The maximum mean absolute change in main residence value is of 1148930.05 Euros and
# the maximum median absolute change in main residence value is of 856759.50 Euros. Both
# values correspond to the profile an Austrian household located in the region of 
# Tyrol. The age of the reference person lies in the bracket of 76-85 years old.

min_mean_change_absolute_at <- min(profile_ages_at$mean_change_absolute, na.rm = TRUE)
min_median_change_absolute_at <- min(profile_ages_at$median_change_absolute, na.rm = TRUE)

# On the other end, the minimum mean and median absolute change in residence value is of 10000 Euros.
# They correspond to a household whose reference person is between 26-35 years old and
# is located in the region of Salzburg.

# Box plots for Austria linking the age group, the mean and median absolute change and,
# additionally, the mean and median net worth and the mean and median net liquid assets.

ggplot(profile_ages_at, aes(x = interaction(age_group), y = mean_change_absolute)) +
  geom_boxplot(aes(color = age_group, fill = age_group), alpha = 0.3) +
  labs(
    title = "Mean Absolute Change by Age Group (Austria)",
    x = "Age Group",
    y = "Mean Absolute Change",
    subtitle = "Grouped by age group"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",  
    axis.text.x = element_text(angle = 45, hjust = 1)  
  )

ggplot(profile_ages_at, aes(x = interaction(age_group), y = median_change_absolute)) +
  geom_boxplot(aes(color = age_group, fill = age_group), alpha = 0.3) +
  labs(
    title = "Median Absolute Change by Age Group (Austria)",
    x = "Age Group",
    y = "Median Absolute Change",
    subtitle = "Grouped by age group"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",  
    axis.text.x = element_text(angle = 45, hjust = 1)  
  )

ggplot(profile_ages_at, aes(x = interaction(age_group), y = `median(change_absolute/HB0100, na.rm = TRUE)`)) +
  geom_boxplot(aes(color = age_group, fill = age_group), alpha = 0.3) +
  labs(
    title = "Median Absolute Change per Square Meter (Austria)",
    x = "Age Group",
    y = "Median Absolute Change per Sqm",
    subtitle = "Grouped by age group"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",  
    axis.text.x = element_text(angle = 45, hjust = 1)  
  )

# Net worth.

ggplot(profile_ages_at, aes(x = interaction(age_group), y = mean_net_worth)) +
  geom_boxplot(aes(color = age_group, fill = age_group), alpha = 0.3) +
  labs(
    title = "Mean Net Worth by Age Group (Austria)",
    x = "Age Group",
    y = "Mean Net Worth",
    subtitle = "Grouped by age group"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",  
    axis.text.x = element_text(angle = 45, hjust = 1)  
  )

ggplot(profile_ages_at, aes(x = interaction(age_group), y = median_net_worth)) +
  geom_boxplot(aes(color = age_group, fill = age_group), alpha = 0.3) +
  labs(
    title = "Median Net Worth by Age Group (Austria)",
    x = "Age Group",
    y = "Median Net Worth",
    subtitle = "Grouped by age group"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",  
    axis.text.x = element_text(angle = 45, hjust = 1)  
  )

# Net liquid assets.

ggplot(profile_ages_at, aes(x = interaction(age_group), y = mean_net_liquid_assets)) +
  geom_boxplot(aes(color = age_group, fill = age_group), alpha = 0.3) +
  labs(
    title = "Mean Net Liquid Assets by Age Group (Austria)",
    x = "Age Group",
    y = "Mean Net Liquid Assets",
    subtitle = "Grouped by age group"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",  
    axis.text.x = element_text(angle = 45, hjust = 1)  
  )

ggplot(profile_ages_at, aes(x = interaction(age_group), y = median_net_liquid_assets)) +
  geom_boxplot(aes(color = age_group, fill = age_group), alpha = 0.3) +
  labs(
    title = "Median Net Liquid Assets by Age Group (Austria)",
    x = "Age Group",
    y = "Median Net Liquid Assets",
    subtitle = "Grouped by age group"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",  
    axis.text.x = element_text(angle = 45, hjust = 1)  
  )

#########
# GERMANY
#########

profile_ages_de <- hfcs_at_de %>%
  filter(SA0100 == "DE") %>%
  mutate(age_group = cut(agerefperson, 
                         breaks = c(18, 25, 35, 45, 55, 65, 75, 85), 
                         labels = c("18-25", "26-35", "36-45", "46-55", "56-65", "66-75", "76-85"),
                         include.lowest = TRUE)) %>% 
  group_by(DHREGION, age_group) %>%  
  summarize(
    mean_change_absolute = mean(change_absolute, na.rm = TRUE),
    median_change_absolute = median(change_absolute, na.rm = TRUE),
    sd_change_absolute = sd(change_absolute, na.rm = TRUE),
    var_change_absolute = var(change_absolute, na.rm = TRUE),
    mean_age = mean(agerefperson, na.rm = TRUE),
    median_age = median(agerefperson, na.rm = TRUE),
    mean_edu = mean(edurefperson, na.rm = TRUE),
    median_edu = median(edurefperson, na.rm = TRUE),
    mean_net_liquid_assets = mean(netliquidassets, na.rm = TRUE),
    median_net_liquid_assets = median(netliquidassets, na.rm = TRUE),
    mean_net_worth = mean(networth, na.rm = TRUE),
    median_net_worth = median(networth, na.rm = TRUE),
    mean_main_residence_sqm = mean(HB0100, na.rm = TRUE),
    median_main_residence_sqm = mean(HB0100, na.rm = TRUE),
    ratio_change_absolute_main_residence_sqm = 
      mean(change_absolute / HB0100, na.rm = TRUE),
    median(change_absolute / HB0100, na.rm = TRUE)
  )

max_mean_change_absolute_de <- max(profile_ages_de$mean_change_absolute, na.rm = TRUE)
max_median_change_absolute_de <- max(profile_ages_de$median_change_absolute, na.rm = TRUE)

# The maximum mean and median absolute change don't coincide with the same household profile. 
# The maximum mean value is of 344487.85 Euros
# (standard deviation of 697249.08 Euros) corresponds to a household in Eastern Germany in which 
# the reference person is between 76 and 85 years old. The max. median values are of 250.000 Euros
# and correspond with households in which the reference person is between 66-75 and 76-85 years old
# and that are located in Southern Germany (standard deviations of 261967.53 and 449603.77 Euros respectively).
# Considering that the median is less influenced by extreme values than the mean, 
# it's wise to select the households with the highest median absolute change as the ones which have
# experienced the highest absolute change in value in their main residences, which also
# have lower standard deviations in this case.

min_mean_change_absolute_de <- min(profile_ages_de$mean_change_absolute, na.rm = TRUE)
min_median_change_absolute_de <- min(profile_ages_de$median_change_absolute, na.rm = TRUE)

# The minimum mean (-80175.43 Euros) and median (5000 Euros) absolute changes 
# correspond with a household located in North-Western Germany in which the reference
# person is between 26 and 35 years old (standard deviation of 694009.81 Euros).

# Box plots for Germany linking the age group, the mean and median absolute change and,
# additionally, the mean and median net worth and the mean and median net liquid assets.

ggplot(profile_ages_de, aes(x = interaction(age_group), y = mean_change_absolute)) +
  geom_boxplot(aes(color = age_group, fill = age_group), alpha = 0.3) +
  labs(
    title = "Mean Absolute Change by Age Group (Germany)",
    x = "Age Group",
    y = "Mean Absolute Change",
    subtitle = "Grouped by age group"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",  
    axis.text.x = element_text(angle = 45, hjust = 1)  
  )

ggplot(profile_ages_de, aes(x = interaction(age_group), y = median_change_absolute)) +
  geom_boxplot(aes(color = age_group, fill = age_group), alpha = 0.3) +
  labs(
    title = "Median Absolute Change by Age Group (Germany)",
    x = "Age Group",
    y = "Median Absolute Change",
    subtitle = "Grouped by age group"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",  
    axis.text.x = element_text(angle = 45, hjust = 1)  
  ) 

ggplot(profile_ages_de, aes(x = interaction(age_group), y = `median(change_absolute/HB0100, na.rm = TRUE)`)) +
  geom_boxplot(aes(color = age_group, fill = age_group), alpha = 0.3) +
  labs(
    title = "Median Absolute Change per Square Meter (Germany)",
    x = "Age Group",
    y = "Median Absolute Change per Sqm",
    subtitle = "Grouped by age group"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",  
    axis.text.x = element_text(angle = 45, hjust = 1)  
  )

# Net worth.

ggplot(profile_ages_de, aes(x = interaction(age_group), y = mean_net_worth)) +
  geom_boxplot(aes(color = age_group, fill = age_group), alpha = 0.3) +
  labs(
    title = "Mean Net Worth by Age Group (Germany)",
    x = "Age Group",
    y = "Mean Net Worth",
    subtitle = "Grouped by age group"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",  
    axis.text.x = element_text(angle = 45, hjust = 1)  
  )

ggplot(profile_ages_de, aes(x = interaction(age_group), y = median_net_worth)) +
  geom_boxplot(aes(color = age_group, fill = age_group), alpha = 0.3) +
  labs(
    title = "Median Net Worth by Age Group (Germany)",
    x = "Age Group",
    y = "Median Net Worth",
    subtitle = "Grouped by age group"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",  
    axis.text.x = element_text(angle = 45, hjust = 1)  
  )

# Net liquid assets.

ggplot(profile_ages_de, aes(x = interaction(age_group), y = mean_net_liquid_assets)) +
  geom_boxplot(aes(color = age_group, fill = age_group), alpha = 0.3) +
  labs(
    title = "Mean Net Liquid Assets by Age Group (Germany)",
    x = "Age Group",
    y = "Mean Net Liquid Assets",
    subtitle = "Grouped by age group"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",  
    axis.text.x = element_text(angle = 45, hjust = 1)  
  )

ggplot(profile_ages_de, aes(x = interaction(age_group), y = median_net_liquid_assets)) +
  geom_boxplot(aes(color = age_group, fill = age_group), alpha = 0.3) +
  labs(
    title = "Median Net Liquid Assets by Age Group (Germany)",
    x = "Age Group",
    y = "Median Net Liquid Assets",
    subtitle = "Grouped by age group"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",  
    axis.text.x = element_text(angle = 45, hjust = 1)  
  )

summary(networth_model_de)

##############################################################
##########################   TASK 3   ########################
##############################################################

# Checking for: "The Wealth Effect"; "Changes in spending/saving more"; "The collateral effect, where HH use the house
# for consumer loans

# We gather all the households with significant increase (defined as more than 100% increase) in their main residence value.
# This tells us that an increase in the current house prices (based on historic land prices) leads to increase in
# total consumption (has an active correlation)

# 1: Data prep
# Create subsets for households with significant real estate value increases
hfcs_at_de <- hfcs_at_de %>%
  mutate(significant_increase = if_else(change_category == "Increase > 100%", 1, 0))

hfcs_at_de$log_consumption <- log(hfcs_at_de$DOCOGOOD + 1)  # Avoid log(0) issues
hfcs_at_de$per_capita_consumption <- hfcs_at_de$DOCOGOOD / hfcs_at_de$DH0001  # Normalize by HH size

# Creating a new data set
sig_data <- subset(hfcs_at_de, significant_increase == 1)

# We gather all the households with significant increase (defined as more than 100% increase) in their main residence value

# Create new variable
hfcs_at_de$total_C<-hfcs_at_de$DOFOODC+(hfcs_at_de$HI0210*12)+hfcs_at_de$DOCOGOOD

#correlation testings:
cor(
  subset(hfcs_at_de, SA0100 == "AT")$agerefperson,
  subset(hfcs_at_de, SA0100 == "AT")$networth,
  use = "complete.obs"
)

# 2: Descriptive Statistics
# Summarize consumption and savings behavior by significant increase group
behavior_summary <- hfcs_at_de %>%
  group_by(significant_increase, SA0100) %>%
  summarise(
    mean_consumption = mean(total_C, na.rm = TRUE),
    median_consumption = median(total_C, na.rm = TRUE),
    sd_consumption = sd(total_C, na.rm = TRUE),
    mean_savings = mean(DA2101, na.rm = TRUE),
    median_savings = median(DA2101, na.rm = TRUE),
    sd_savings = sd(DA2101, na.rm = TRUE),
    count = n()
  )

print("Summary Statistics for Consumption and Savings by Significant Increase:")
print(behavior_summary)

# Significance noted

# need to create a data set for both countries (make change on column 0800 [SA0100] to run on both data sets)

# 3: Correlation Analysis
# Correlate changes in real estate wealth with consumption and savings
cor_consumption <- cor(hfcs_at_de$change_ratio, hfcs_at_de$per_capita_consumption, use = "complete.obs")
cor_savings <- cor(hfcs_at_de$change_ratio, hfcs_at_de$DA2101, use = "complete.obs")

cor_test_consumption <- cor.test(hfcs_at_de$change_ratio, hfcs_at_de$total_C)
cor_test_savings <- cor.test(hfcs_at_de$change_ratio, hfcs_at_de$DA2101)

print("Correlation between Change in Real Estate Value and Consumption:")
print(cor_test_consumption)

print("Correlation between Change in Real Estate Value and Savings:")
print(cor_test_savings)

# 4: Regression Analysis
# Regression of consumption and savings on changes in real estate wealth
consumption_model <- lm(DI2000 ~ change_absolute + networth + age + edurefperson + famstruct, data = hfcs_at_de)
savings_model <- lm(netliquidassets ~ change_absolute + networth + age + edurefperson + famstruct, data = hfcs_at_de)

print("Regression Model for Consumption:")
summary(consumption_model)

print("Regression Model for Savings:")
summary(savings_model)

# 5: Visuals
## Boxplots for consumption and savings by significant increase group
# Consumption Boxplot
ggplot(hfcs_at_de, aes(x = factor(significant_increase), y = DI2000, fill = factor(SA0100))) +
  geom_boxplot(outlier.size = 4, outlier.colour = "black", width = 2) +  # Remove dots and adjust box width
  labs(
    title = "Consumption by Significant Real Estate Value Increase",
    x = "Significant Increase in Real Estate Value",
    y = "Consumption",
    fill = "Country"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10),  # Adjust x-axis label size
    axis.text.y = element_text(size = 10),  # Adjust y-axis label size
    plot.title = element_text(size = 14)    # Increase title size for readability
  )

# Savings Boxplot
ggplot(hfcs_at_de, aes(x = factor(significant_increase), y = netliquidassets, fill = factor(SA0100))) +
  geom_boxplot(outlier.size = 4, outlier.colour = "black", width = 0.6) +  # Remove dots and adjust box width
  scale_y_continuous(
    limits = c(0, 1000000),  # Cap y-axis at 1,000,000
    labels = scales::comma  # Use commas for readability
  ) +
  labs(
    title = "Savings by Significant Real Estate Value Increase",
    x = "Significant Increase in Real Estate Value",
    y = "Net Liquid Assets",
    fill = "Country"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10),  # Adjust x-axis label size
    axis.text.y = element_text(size = 10),  # Adjust y-axis label size
    plot.title = element_text(size = 14)    # Increase title size for readability
  )

## Scatter Plots

# Scatter plot for consumption vs. change in real estate value
ggplot(hfcs_at_de, aes(x = change_absolute, y = DI2000, color = factor(SA0100))) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Consumption vs Change in Real Estate Value",
    x = "Change in Real Estate Value",
    y = "Consumption",
    color = "Country"
  ) +
  theme_minimal()

# Scatter plot for savings vs. change in real estate value
ggplot(hfcs_at_de, aes(x = change_absolute, y = netliquidassets, color = factor(SA0100))) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Savings vs Change in Real Estate Value",
    x = "Change in Real Estate Value",
    y = "Net Liquid Assets",
    color = "Country"
  ) +
  theme_minimal()

# IV Models

# 1: Data creation

# Subset increase for AT and DE

# AT >0-50%
subset_at_0_50 <- subset(hfcs_at_de, SA0100 == "AT" & change_category == "Increase 0-50%")

# AT >50-100%
subset_at_50_100 <- subset(hfcs_at_de, SA0100 == "AT" & change_category == "Increase 50-100%")

# AT >100%
subset_at_100 <- subset(hfcs_at_de, SA0100 == "AT" & change_category == "Increase > 100%")

# DE >0-50%
subset_de_0_50 <- subset(hfcs_at_de, SA0100 == "DE" & change_category == "Increase 0-50%")

# DE >50-100%
subset_de_50_100 <- subset(hfcs_at_de, SA0100 == "DE" & change_category == "Increase 50-100%")

# DE >100%
subset_de_100 <- subset(hfcs_at_de, SA0100 == "DE" & change_category == "Increase > 100%")

# 2: IV Regression: Historical Land Prices as an Instrument

iv_model_1 <- ivreg(
  per_capita_consumption ~ HB0900 |
    HB0800 ,
  data = hfcs_at_de
)
summary(iv_model_1)

# 3: IV Regression: Consumption vs Consumption as a share of income as an Instrument
iv_model_2 <- ivreg(
  per_capita_consumption ~ DOCOGOOD |
    DOCOGOODP ,
  data = hfcs_at_de
)
summary(iv_model_2)

# 4: Visualization of IV Regressions1

# Combine Subsets into One Data Frame
subset_at_0_50$group <- "Austria 0-50%"
subset_at_50_100$group <- "Austria 50-100%"
subset_at_100$group <- "Austria >100%"
subset_de_0_50$group <- "Germany 0-50%"
subset_de_50_100$group <- "Germany 50-100%"
subset_de_100$group <- "Germany >100%"

# Combine into a single data frame
comparison_data <- rbind(
  subset_at_0_50, subset_at_50_100, subset_at_100,
  subset_de_0_50, subset_de_50_100, subset_de_100
)

# Prepare Data for Plotting
comparison_summary <- comparison_data %>%
  mutate(
    country = ifelse(grepl("Austria", group), "Austria", "Germany"),
    change_category = case_when(
      grepl("0-50%", group) ~ "0-50%",
      grepl("50-100%", group) ~ "50-100%",
      grepl(">100%", group) ~ ">100%"
    )
  )

# Define IV models as separate columns for visualization
comparison_summary <- comparison_summary %>%
  mutate(
    IV_model_1 = per_capita_consumption,  # Placeholder for IV Model 1 (replace with actual data)
    IV_model_2 = per_capita_consumption   # Placeholder for IV Model 2 (replace with actual data)
  )

# Melt data into long format for faceted plotting
long_data <- comparison_summary %>%
  pivot_longer(
    cols = c(IV_model_1, IV_model_2),
    names_to = "IV_model",
    values_to = "value"
  )

# Create Faceted Boxplots
ggplot(long_data, aes(x = change_category, y = value, fill = change_category)) +
  geom_boxplot() +
  scale_y_continuous(
    breaks = seq(0, 500000, by = 100000),
    labels = label_number(scale = 1e-3, suffix = "K")
  ) +
  coord_cartesian(ylim = c(0, 500000)) +  # Cap y-axis at 500,000
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "Comparison of Total Consumption by Change Category",
    x = "Change Category",
    y = "Total Consumption (in Thousands)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "none"
  ) +
  facet_grid(country ~ IV_model, scales = "free_y")  # Separate panels for country and IV model

# Consumption vs Income ratio

# Marginal Propensity to Consume

# Quintiles for household
hfcs_at_de$Quintile <- cut(
  hfcs_at_de$HB0900, 
  breaks = quantile(hfcs_at_de$HB0900, probs = seq(0, 1, 0.2), na.rm = TRUE), 
  labels = c("lowest", "lower", "middle", "upper middle", "upper"), 
  include.lowest = TRUE
)

# Calculate thresholds
lower_limit <- quantile(hfcs_at_de$DI2000, 0.05, na.rm = TRUE)
upper_limit <- quantile(hfcs_at_de$DI2000, 0.95, na.rm = TRUE)

# Filter data within these limits
filtered_hfcs_at_de <- hfcs_at_de %>%
  filter(DI2000 >= lower_limit & DI2000 <= upper_limit)

# Boxplot 
ggplot(hfcs_at_de, aes(x = Quintile, y = DI2000)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  coord_cartesian(ylim = c(0, 500000)) + 
  labs(title = "Gross Income by Asset Value",
       x = "Asset Value Groups/Quintiles",
       y = "Gross Total Income") +
  theme_classic()

# Estimate MPC for HtM and Non-HtM households
hfcs_at_de <- hfcs_at_de %>%
  group_by(htm_status) %>%
  summarize(
    median_mpc = median(DI2000 / total_gross_income, na.rm = TRUE),
    mean_mpc = mean(DI2000 / total_gross_income, na.rm = TRUE)
  )

print("Median and Mean MPC by HtM Status:")
print(hfcs_at_de)

#consumption-savings model
consumption_saving_model_at <- lm(
  DI2000 ~ netliquidassets + networth + age + edurefperson + famstruct,
  data = hfcs_at_de ,
  subset = SA0100 == "AT"
)

summary(consumption_saving_model_at)

consumption_saving_model_de <- lm(
  DI2000 ~ netliquidassets + networth + age + edurefperson + famstruct,
  data = hfcs_at_de ,
  subset = SA0100 == "DE"
)

summary(consumption_saving_model_de)

# Visualization: Savings vs. Consumption
ggplot(hfcs_at_de, aes(x = netliquidassets, y = DI2000)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(
    title = "Relationship Between Savings and Consumption",
    x = "Net Liquid Assets (Savings)",
    y = "Consumption"
  ) +
  theme_minimal() +
  scale_x_continuous(
    labels = label_number(scale = 1, accuracy = 1),  # Avoid scientific notation
    limits = c(0, 5000000)  # Cap x-axis at 5 million
  )

#correlation testings:
cor(
  subset(hfcs_at_de, SA0100 == "AT")$agerefperson,
  subset(hfcs_at_de, SA0100 == "AT")$networth,
  use = "complete.obs"
)

# 2: Descriptive Statistics

# Summarize consumption and savings behavior by significant increase group
behavior_summary <- hfcs_at_de %>%
  group_by(significant_increase, SA0100) %>%
  summarise(
    mean_consumption = mean(total_C, na.rm = TRUE),
    median_consumption = median(total_C, na.rm = TRUE),
    sd_consumption = sd(total_C, na.rm = TRUE),
    mean_savings = mean(DA2101, na.rm = TRUE),
    median_savings = median(DA2101, na.rm = TRUE),
    sd_savings = sd(DA2101, na.rm = TRUE),
    count = n()
  )

print("Summary Statistics for Consumption and Savings by Significant Increase:")
print(behavior_summary)

# Significance noted

# need to create a data set for both countries (make change on column 0800 [SA0100] to run on both data sets)

# 3: Correlation Analysis

# Correlate changes in real estate wealth with consumption and savings
cor_consumption <- cor(hfcs_at_de$change_ratio, hfcs_at_de$total_C, use = "complete.obs")
cor_savings <- cor(hfcs_at_de$change_ratio, hfcs_at_de$DA2101, use = "complete.obs")

cor_test_consumption <- cor.test(hfcs_at_de$change_ratio, hfcs_at_de$total_C)
cor_test_savings <- cor.test(hfcs_at_de$change_ratio, hfcs_at_de$DA2101)

print("Correlation between Change in Real Estate Value and Consumption:")
print(cor_test_consumption)

print("Correlation between Change in Real Estate Value and Savings:")
print(cor_test_savings)

# Seeing less consumption and less savings (might be driven by German)

# 4: Regression Analysis

# Regression of consumption and savings on changes in real estate wealth
consumption_model <- lm(DI2000 ~ change_absolute + networth + age + edurefperson + famstruct, data = hfcs_at_de)
savings_model <- lm(netliquidassets ~ change_absolute + networth + age + edurefperson + famstruct, data = hfcs_at_de)

print("Regression Model for Consumption:")
summary(consumption_model)

print("Regression Model for Savings:")
summary(savings_model)

# 5: Visuals

#Box plot for comparison of Total Consumption by Group
ggplot(comparison_data, aes(x = group, y = total_C / DH0001, fill = group)) +
  geom_boxplot(outlier.size = 4, outlier.colour = "black") +  # Adjust outlier size and color
  scale_y_continuous(
    limits = c(0, NA),  # Allow dynamic upper limit
    labels = scales::comma  # Add commas for better readability
  ) +
  labs(
    title = "Comparison of Per Capita Total Consumption by Group",
    x = "Group",
    y = "Per Capita Total Consumption",
    fill = "Group",
    caption = "(Consumption divided by number of household members)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for clarity
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

