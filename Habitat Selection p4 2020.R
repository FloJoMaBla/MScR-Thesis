library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)
library(purrr)
library(ggridges)

# let's use a simple dataframe (without GPS coordinates)
df.s_2020 <- st_drop_geometry(HabSel) 
df.s_2020 <- subset(df.s_2020, df.s_2020$year==2020)

# Ensure factors are correctly set
df.s_2020$LC_cat <- as.factor(df.s_2020$LC_cat)
df.s_2020$Season <- as.factor(df.s_2020$Season)
df.s_2020$ID <- as.factor(df.s_2020$ID)

# Subset presence points
df.s_2020 <- subset(df.s_2020, df.s_2020$Pres == 1)

# Calculate proportional use of each habitat category by season for each individual
prop_use_2020 <- df.s_2020 %>%
  group_by(ID, Season, LC_cat) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(ID, Season) %>%
  mutate(total = sum(count), prop = count / total) %>%
  select(ID, Season, LC_cat, prop)

# Spread the data for Schoener's D calculation
prop_use_wide_2020 <- prop_use_2020 %>%
  pivot_wider(names_from = Season, values_from = prop, values_fill = list(prop = 0))

# Function to calculate Schoener's D
schoeners_d_2020 <- function(df.s_2020) {
  1 - 0.5 * sum(abs(df.s_2020$Breeding - df.s_2020$Wintering))
}  

# Apply the function to each individual
schoeners_d_values_2020 <- prop_use_wide_2020 %>%
  group_by(ID) %>% 
  summarise(Schoeners_D = schoeners_d_2020(cur_data()), .groups = 'drop') #this uses the dplyr package

# View Schoener's D values
print(schoeners_d_values_2020)

# Set seed for reproducibility
set.seed(123)

# Function to perform a permutation test
perm_test_2020 <- function(df.s_2020, n_permutations = 1000) {
  observed_d <- schoeners_d(df.s_2020)
  null_d <- numeric(n_permutations)
  
  for (i in 1:n_permutations) {
    permuted <- df.s_2020 %>%
      mutate(Wintering = sample(Wintering))
    null_d[i] <- schoeners_d(permuted)
  }
  
  list(null_d = null_d, p_value = mean(null_d >= observed_d))
}

# Apply permutation test to each individual
perm_test_results_2020 <- prop_use_wide_2020 %>%
  group_by(ID) %>%
  summarise(test_result = list(perm_test(cur_data())), .groups = 'drop')

# Extract p-values and null distributions
p_values_2020 <- perm_test_results_2020 %>%
  transmute(ID, p_value = map_dbl(test_result, "p_value"))

null_distributions_2020 <- perm_test_results_2020 %>%
  transmute(ID, null_d = map(test_result, "null_d"))

# Combine results with Schoener's D values
results_2020 <- schoeners_d_values_2020 %>%
  inner_join(p_values_2020, by = "ID")

print(results_2020)

# Interpretation of Each Individual

# Eric: Schoener's D = 0.555, p-value = 0.019
# Moderate overlap, not significantly different from random expectations. No niche-tracking.

# Summary
# Significant niche-tracking (p-value < 0.05): Eric
# Possible niche-tracking (borderline significant, 0.05 < p-value < 0.1): none
# No significant niche-tracking: none

#### SOME PLOTS 


# Combine observed and null distributions into a single dataframe
null_distribution_df_2020 <- map2_dfr(null_distributions_2020$null_d, null_distributions_2020$ID, function(nd, id) {
  data.frame(Schoeners_D = nd, Type = "Null", ID = id)
}) %>%
  bind_rows(data.frame(Schoeners_D = results_2020$Schoeners_D, Type = "Observed", ID = results_2020$ID))

# Boxplot of Schoener's D Values vs. Null Distribution
ggplot(null_distribution_df_2020, aes(x = ID, y = Schoeners_D)) +
  geom_boxplot() +
  geom_point(data = results, aes(x = ID, y = Schoeners_D), color = "red", size = 4) +
  theme_minimal() +
  labs(title = "Observed vs. Null Distribution of Schoener's D",
       x = "Individual",
       y = "Schoener's D") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylim(0, 1)

# Same but horizontal
ggplot(null_distribution_df_2020, aes(y = ID, x = Schoeners_D)) +
  geom_boxplot() +
  geom_point(data = results, aes(y = ID, x = Schoeners_D), color = "red", size = 4) +
  theme_minimal() +
  labs(title = "Observed vs. Null Distribution of Schoener's D",
       y = "Individual",
       x = "Schoener's D") +
  theme(axis.text.y = element_text(angle = 0, hjust = 1)) +
  xlim(0, 1) 

# Similar but with density plot
ggplot(null_distribution_df_2020, aes(x = Schoeners_D, y = ID)) +
  geom_density_ridges(fill = "steelblue", scale = 0.9, alpha = 0.7) +
  geom_point(data = results, aes(x = Schoeners_D, y = ID), color = "red", size = 4, inherit.aes = FALSE) +
  theme_minimal() +
  labs(title = "Observed vs. Null Distribution of Schoener's D",
       y = "Individual",
       x = "Schoener's D") +
  theme(axis.text.y = element_text(angle = 0, hjust = 1)) +
  xlim(0, 1)


#  same but with 90%confidence intervals instead of the boxplot.
#  this is similar to the paper from Javier Gutiérrez Illá eta l 2002.

# Function to calculate the 95% CI
calculate_ci <- function(null_d) {
  lower <- quantile(null_d, 0.05) #
  upper <- quantile(null_d, 0.95)
  return(c(lower = lower, upper = upper))
}
# Calculate the 95% CI for each individual
ci_df_2020 <- null_distributions_2020 %>%
  mutate(CI = map(null_d, calculate_ci)) %>%
  unnest_wider(CI)
# Rename columns
ci_df_2020 <- ci_df_2020 %>%
  rename(CI_lower = names(ci_df_2020)[3], CI_upper = names(ci_df_2020)[4])

# Combine CI with the observed data
combined_results_2020 <- results_2020 %>%
  inner_join(ci_df_2020, by = "ID")

# Plot horizontal bars with 95% CI and observed Schoener's D values
ggplot(combined_results_2020, aes(y = ID)) +
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), height = 0.2, color = "steelblue") +
  geom_point(aes(x = Schoeners_D), color = "red", size = 2) +
  theme_minimal() +
  labs(title = "Observed vs. Null Distribution of Schoener's D with 95% CI",
       y = "Individual",
       x = "Schoener's D") +
  theme(axis.text.y = element_text(angle = 0, hjust = 1)) +
  xlim(0, 1)
