################################################################################

# Misinformation Meta-analysis - P-Curve Analysis 

################################################################################
# Loading data and libraries
library(tidyverse)
data_es <- read_csv('data/misinformation_clean_data.csv')

# Calculating Power with assumed equal group sizes
power_1 <- power.t.test(n = data_es$n_control, delta = data_es$yi, 
                        sd = sqrt(data_es$vi), sig.level = .05,
                        type = 'two.sample')

power_2 <- power.t.test(n = data_es$n_control, delta = data_es$yi, 
                        sd = sqrt(data_es$vi), sig.level = .05,
                        type = 'paired')
# Calculating p-values
data_es <- data_es %>% 
  mutate(
    p_value = 2 * (1- pnorm(abs(data_es$yi/sqrt(data_es$vi)))),
    post_hoc_power  = case_when(
      within_between=='between' ~ power_1$power,
      within_between=='within' ~  power_2$power)
  )


table(high_power = ifelse(data_es$post_hoc_power > .8, 1, 0),
      sig = ifelse(data_es$p_value < .05, 1, 0))
# We have no high powered insignificant effects
# We have 244 low powered insignificant effects 
# We have 463 high powered insignificant effects
# We have 1059 high powered significant effects

# Plotting the relationship between power and p-values
data_es %>% ggplot(aes(y = p_value, x = post_hoc_power))+
  geom_point(alpha = .1)+
  geom_smooth()+
  geom_hline(yintercept = .05)+
  geom_vline(xintercept = .8)


# Specifing the p-curve data
study_data <- data.frame(
  study_id = data_es$id_record,
  effect_id = data_es$id_effect,
  p_value = data_es$p_value
)

# Function to sample rows from each group
sample_rows_per_group <- function(df, rows) {
  df %>%
    sample_n(size = rows, replace = TRUE) %>%
    pull(p_value)
}

# Function to generate bootstrap samples
generate_bootstrap_samples <- function(data, rows) {
  unique_studies <- unique(data$study_id)
  num_studies <- length(unique_studies)
  bootstrap_samples <- matrix(nrow = rows, ncol = num_studies)
  colnames(bootstrap_samples) <- unique_studies
  
  for (i in 1:num_studies) {
    p_values <- sample_rows_per_group(filter(data, study_id == unique_studies[i]), rows)
    bootstrap_samples[, i] <- sort(p_values)
  }
  
  return(bootstrap_samples)
}

# Generate bootstrap samples with a specified number of rows
bootstrap_samples <- generate_bootstrap_samples(study_data, rows = 10000)  

# Cleaning the bootstrap samples so that only unique rows remain. 
bootstrap_samples <- bootstrap_samples[!duplicated(bootstrap_samples),]

# Convert matrix to data frame
bootstrap_df <- as.data.frame(bootstrap_samples)

# Test of right skewness 
n <- apply(bootstrap_df <= .05, 1, sum)
right_p <- data.frame(p =NA)

for (i in 1:NROW(bootstrap_df)) {
  
  p <- binom.test(x = n[i], n = 294, p = .5, alternative = "greater")$p.value
  right_p[i,] <- p
}

right_p %>% 
  ggplot(aes(p))+
  geom_histogram(alpha = .5, bins = 50, col = 'blue')+
  scale_x_continuous(breaks = c(.01, .05, .1, .25, .50, .75, 1 ), 
                     guide = guide_axis(angle = 90))+
  labs(titel = 'Right-skeweness test p-curve')

sum(ifelse(right_p$p < .05, yes = 1, no = 0))/nrow(right_p)
# 53% of the Bernoulli tests indicate right skewness and the presence of a true effect

## Fishers pp values 
fisher_pp <- data.frame(p = NA)

for (i in 1:NROW(bootstrap_df)) {
    pp <- as.data.frame(t(bootstrap_df[i,]*20))
    chi <- -2*sum(log(pp))
    p <- pchisq(chi, df = 2*294, lower.tail = FALSE)
    fisher_pp[i,] <- p
}
# all values are zero, indicating right-skewdness and a true effect in 100% of curves 
sum(fisher_pp$p)

# I do not think a test for flatness is needed, we certainly do no lack power. 
# all in all, I think these results show evidence for the precense of a true effect
# Making the data tidy for plotting
plt_dat <- as.data.frame(t(bootstrap_df))
plt_dat$row_id <- 1:nrow(plt_dat)

# Making the data long for plotting
bootstrap_df_tidy <- pivot_longer(plt_dat, cols = -row_id, 
                                  names_to = "study_id", 
                                  values_to = "p_value")

bootstrap_df_tidy %>%
  ggplot(aes(p_value, group = row_id))+
  geom_histogram(alpha = .3, bins = 150, col = 'blue')+
  scale_x_continuous(breaks = c(.01, .05, .1, .25, .50, .75, 1 ), 
                     guide = guide_axis(angle = 90))+
  xlim(0, .1)+
  ylim(0,2500)




