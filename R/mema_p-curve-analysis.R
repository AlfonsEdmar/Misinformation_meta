################################################################################

# Misinformation Meta-analysis - P-Curve Analysis 

################################################################################

# Loading data and libraries

library(tidyverse)

data_es <- read_csv('data/mema_data-included.csv')

# Calculating post hoc power with assumed equal group sizes

power_1 <- power.t.test(n = data_es$n_control, delta = data_es$yi, 
                        sd = 1, sig.level = .05,
                        type = 'two.sample')

power_2 <- power.t.test(n = data_es$n_control, delta = data_es$yi, 
                        sd = 1, sig.level = .05,
                        type = 'paired')

# Calculating required sample size for effect detection of the observed effect
# Lower bound of 95% CI (d = 0.25)
lb_power_between <- power.t.test(d = 0.25, power = 0.80, sig.level = 0.05, type = "two.sample", alternative = "two.sided")
lb_power_within  <- power.t.test(d = 0.25, power = 0.80, sig.level = 0.05, type = "paired", alternative = "two.sided")

# Point estimate (d = 0.75)
point_power_between <- power.t.test(d = 0.75, power = 0.80, sig.level = 0.05, type = "two.sample", alternative = "two.sided")
point_power_within  <- power.t.test(d = 0.75, power = 0.80, sig.level = 0.05, type = "paired", alternative = "two.sided")

# Upper bound (d = 1.25)
ub_power_between <- power.t.test(d = 1.25, power = 0.80, sig.level = 0.05, type = "two.sample", alternative = "two.sided")
ub_power_within  <- power.t.test(d = 1.25, power = 0.80, sig.level = 0.05, type = "paired", alternative = "two.sided")
# Calculating p-values

data_es <- data_es %>% 
  mutate(
    p_value = 2 * (1- pnorm(abs(data_es$yi/sqrt(data_es$vi)))),
    post_hoc_power  = case_when(
      within_between=='between' ~ power_1$power,
      within_between=='within' ~  power_2$power),
  )


data_es <- data_es %>%
  mutate(
    p_value = 2 * (1 - pnorm(abs(yi / sqrt(vi)))),
    
    required_n_lb = ifelse(within_between == 'within', lb_power_within$n, lb_power_between$n),
    required_n_point = ifelse(within_between == 'within', point_power_within$n, point_power_between$n),
    required_n_ub = ifelse(within_between == 'within', ub_power_within$n, ub_power_between$n),
    
    high_power_lb = n_control > required_n_lb,
    high_power_point = n_control > required_n_point,
    high_power_ub = n_control > required_n_ub
  )

table(high_power_lb = data_es$high_power_lb, sig = data_es$p_value < 0.05)
# If the true SMD is at the lower bound, all 98% of studies are low powered. 
# There are 694 non-significant low powered studies, and 1038 significant low powered studies.
table(high_power_point = data_es$high_power_point, sig = data_es$p_value < 0.05)
# If the effect is accurate, 73% of studies are high powered. 
# There are 863 significant effect and 428 non-significant effect, roughly twice as many significant.  
table(high_power_ub = data_es$high_power_ub, sig = data_es$p_value < 0.05)
# If the upper bound is accurate, 97% of studies are high powered. 
# There are 1055 significant effect and 669 non-significant effects, 1.56 times more significant studies. 

# If the point estimate is accurate, the prevalence of non-significant high power 
# studies speaks against having much publication bias. The average p-value is over .05,
# Though the median - middle most value = .01.  

table(high_power = ifelse(data_es$post_hoc_power > .8, 1, 0),
      sig = ifelse(data_es$p_value < .05, 1, 0))
# In terms of post-hoc power, we have very few high power insignificant effects - which suggests bias. 
# Though post-hoc power is essentially just a different view of the p-value.
# Roughly 60% of the p-values are significant. 
summary(data_es$p_value)


# Plotting the relationship between power and p-values

data_es %>% ggplot(aes(y = p_value, x = post_hoc_power))+
  geom_point(alpha = .1)+
  geom_smooth(aes(col = within_between))+
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

# I do not think a test for flatness is needed, we certainly do not lack power. 
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



