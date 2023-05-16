################################################################################

# Misinformation Meta-analysis - visuals

################################################################################

# Load packages ----------------------------------------------------------------

library(tidyverse)
library(igraph)
theme_set(theme_classic())
hue   <- 'tomato'
hue_2 <- 'deepskyblue'



# Load data --------------------------------------------------------------------

data <- read_csv2('data/misinformation_data_es.csv')

# Wrangling---------------------------------------------------------------------
  
data$publication_year     <- as.numeric(data$publication_year)

data$language_record      <- as.factor(data$language_record)
data$language_materials   <- as.factor(data$language_materials)
data$country              <- as.factor(data$country)
data$population           <- as.factor(data$population)
data$modality             <- as.factor(data$modality)
data$incentives           <- as.factor(data$incentives)

data$within_between       <- as.factor(data$within_between)
data$accuracy_type        <- as.factor(data$accuracy_type)
data$control_type         <- as.factor(data$control_type)

data$age_mean             <- as.numeric(data$age_mean)

data$preevent_valence     <- as.factor(data$preevent_valence)

data$event_medium         <- as.factor(data$event_medium)

data$exposure_medium      <- as.factor(data$exposure_medium)
data$exposure_method      <- as.factor(data$exposure_method)
data$exposure_valence     <- as.factor(data$exposure_valence)

data$test_medium          <- as.factor(data$test_medium)
data$test_type            <- as.factor(data$test_type)
data$item_centrality      <- as.factor(data$item_centrality)
data <- data[-c(1,2)]

# Visuals-----------------------------------------------------------------------


## Test Type--------------------------------------------------------------------

data %>% 
  filter(test_type == 'recognition'   | test_type == 'cued_recall' |
         test_type == 'modified_test' | test_type == 'free_recall' |
         test_type == 'source_monitoring') %>% 
  ggplot(aes(y = yi, x = test_type))+
  geom_point(alpha = .4, width = .1)+
  geom_boxplot(width = .2, outlier.shape = NA)+
  geom_violin(alpha = .3)+
  ylim(-5,10)+
  scale_y_continuous(breaks = seq(-3,10,1),
                     limits = c(-3,10), name = 'Hedges g')+
  scale_x_discrete(labels = c('Cued Recall','Free Recall','Modified Test', 
                              'Recognition', 'Source Monitoring'), 
                   name = NULL)


##Publication year--------------------------------------------------------------

# Number of studies published 
data %>%   
  filter(duplicated(id_record) == F) %>%
  select(publication_year) %>% 
  ggplot()+
  geom_bar(aes(x = publication_year))+
  scale_x_continuous(breaks = seq(from = 1970, to = 2023, by = 5),
                     name = 'Publication Year')+
  scale_y_continuous(breaks = seq(from = 0, to = 20, by = 5),
                     name = 'Count')

# Misinformation effect across time
data %>% 
  ggplot()+
  geom_boxplot(aes(y = yi, x = as.factor(publication_year)))+
  scale_x_discrete(breaks = seq(1975, 2022, 5))+
  scale_y_continuous(breaks = seq(-3,5, 1), limits = c(-3,5))+
  ylab(label = 'Hedges g')+
  xlab(label = 'Publication Year')
ggsave('visuals/pub_year_boxplot.png',dpi=300)

## Post-event recall------------------------------------------------------------

data %>% filter(!is.na(postevent_recall)) %>% 
  ggplot(aes(y = yi, x = as.factor(postevent_recall)))+
  geom_jitter(alpha = .5, width = .2, size = .5)+
  geom_boxplot(alpha = .5, col = hue, outlier.shape = NA)+
  scale_y_continuous(breaks = seq(-3,10,2), name = 'Hedges g',
                     limits = c(-3,10))+
  xlab(label = 'Number of Post-event recall tests')+
  geom_hline(yintercept = 0, linetype = 1, size = .5,
             col = 'black', alpha = .5)
ggsave('visuals/post_ev_recall.png',dpi=300)

## Age--------------------------------------------------------------------------

data%>% 
  ggplot(aes(x = as.numeric(age_mean), y = yi))+
  geom_point(alpha = .5, size = .5)+
  geom_smooth(col = hue)+
  theme_classic()+
  scale_x_continuous(breaks = seq(from = 0, 
                                  to   = 80,
                                  by   = 10),
                     name = 'Mean Age')+
  scale_y_continuous(breaks=(seq(-3, 10, 2)), limits = c(-3, 10))+
  ylab(label = 'Hedges g')
ggsave('visuals/age_linear.png',dpi=300)

data %>% 
  mutate(quad_age = age_mean^2) %>%
  ggplot(aes(x = quad_age, y = yi))+
  geom_point()+
  geom_smooth(se = F, col = hue)+
  theme_classic()+
  scale_x_continuous(name = 'Mean Age')+
  scale_y_continuous(breaks = seq(from = -5,
                                  to   = 10,
                                  by   = 1))+
  ylim(-5, 10)+
  ylab(label = 'Hedges g')

# Control accuracy and age
data%>% 
  ggplot()+
  geom_point(aes(x = as.numeric(age_mean), 
                 y = total_accuracy_control_mean),
               col = hue)+
  geom_point(aes(x = as.numeric(age_mean), 
                 y = total_accuracy_mi_mean),
             alpha = .5)+
  geom_smooth(aes(x = as.numeric(age_mean), 
                  y = total_accuracy_control_mean),
              col = hue,
              method = 'lm',
              se = F)+
  geom_smooth(aes(x = as.numeric(age_mean), 
                  y = total_accuracy_mi_mean),
              col = 'black',
              method = 'lm',
              se = F)+
  ylab(label = 'Control Mean Accuracy')+
  xlab(label = 'Mean Age of Sample')


## Retention interval-----------------------------------------------------------

data %>%
  ggplot(aes(y = yi, x = postevent_retention_interval/24))+
  geom_point(alpha = .5, size = .5)+
  geom_smooth(method = "lm",
              se = F,
              col = hue) +
  geom_smooth(method = "lm",
              formula = y ~ poly(x, 2), 
              se = F,
              col = hue_2) +
  scale_y_continuous(limits = c(-3,10), name = 'Hedges g', 
                     breaks = seq(-3,10,1))+
  scale_x_continuous(breaks = seq(0,385, 28), name = NULL)+
  geom_hline(yintercept = 0, linetype = 1, size = .5,
             col = 'black', alpha = .5)
ggsave('visuals/ev_retention_interval.png',dpi=300)

data %>%
  ggplot(aes(y = yi, x = postexposure_retention_interval/24))+
  geom_point(alpha = .5, size = .5)+
  geom_smooth(method = "lm",
              se = F,
              col = hue) +
  geom_smooth(method = "lm",
              formula = y ~ poly(x, 2), 
              se = F,
              col = hue_2) +
  scale_y_continuous(limits = c(-3,10), name = 'Hedges g', 
                     breaks = seq(-3,10,1))+
  scale_x_continuous(breaks = seq(0,385, 28), name = NULL)+
  geom_hline(yintercept = 0, linetype = 1, size = .5,
             col = 'black', alpha = .5)
ggsave('visuals/ex_retention_interval.png',dpi=300)

data %>%
  ggplot(aes(y = yi, x = postexposure_retention_interval))+
  geom_point(alpha = .5)+
  geom_smooth(method = "lm",
              se = F,
              col = hue) +
  geom_smooth(method = "lm",
              formula = y ~ poly(x, 2), 
              se = F,
              col = hue_2) +
  scale_y_continuous(limits = c(-3,10), name = 'Hedges g', 
                     breaks = seq(-3,10,1))


data %>%
  ggplot(aes(y = yi, x = postexposure_retention_interval))+
  geom_point()+
  geom_smooth()

data %>% 
  ggplot()+
  geom_point(aes(y   = yi, 
                 x   = postexposure_retention_interval/24),
                 col = 'black')+
  geom_jitter(aes(y  = yi, 
                  x  = postevent_retention_interval/24),
                 col = hue,
               alpha = .2)+
  scale_x_continuous(name = 'Retention intervals in days',
                     breaks = seq(0, 375, 30))+
  ylab( label = 'Hedges g')+
  theme_classic()
ggsave('visuals/retention_interval.png',dpi=300)

# Retenton intervals
data %>% 
  filter(duplicated(id_study) == F) %>% 
  ggplot()+
  geom_point(aes(y   = yi, 
                 x   = postexposure_retention_interval/24)
              , alpha = .2
              , size = 2)+
  geom_point(aes(y  = yi, 
                  x   = postevent_retention_interval/24),
              alpha = .2,
              size = 2)+
  scale_x_continuous(name = 'Retention intervals in days',
                     breaks = seq(0, 375, 30))+
  ylab( label = 'Hedges g')


data %>%
  filter(postexposure_retention_interval > .1 &
           postexposure_retention_interval < 2500 &
           postevent_retention_interval < 2500 &
           postevent_retention_interval > .1) %>% 
  ggplot()+
  geom_histogram(aes(x = postexposure_retention_interval),
                 col   = 'black',
                 fill  = 'black',
                 bins  = 30 )+
  geom_histogram(aes(x = postevent_retention_interval),
                 fill  = hue,
                 col = hue,
                 bins = 30,
                 alpha = .4)+
  theme_classic()
  

data %>%
  filter(postexposure_retention_interval > .1 &
           postexposure_retention_interval < 2500 &
           postevent_retention_interval < 2500 &
           postevent_retention_interval > .1) %>% 
  ggplot()+
  geom_point(aes(x     = postexposure_retention_interval,
                 y     = yi ),
                 col   = 'black',
                 fill  = 'black',
                 bins  = 30 )+
  geom_histogram(aes(x = postevent_retention_interval),
                 fill  = hue,
                 col = hue,
                 bins = 30,
                 alpha = .4)+
  theme_classic()


extreme_values <- data %>% filter(postexposure_retention_interval>2500 |
                                    postevent_retention_interval >2500 )


data %>%
  filter(postevent_retention_interval < 2500 &
           postevent_retention_interval > 1) %>% 
  ggplot(aes(y = yi, x = postevent_retention_interval))+
  geom_point(size = 2, alpha = .5)+
  geom_smooth(method = 'lm', se = F, col = 'black')

data %>%
  filter(postexposure_retention_interval < 5000 &
           postexposure_retention_interval > 1) %>% 
  ggplot(aes(y = yi, x = postexposure_retention_interval))+
  geom_point(size = 2, alpha = .5)+
  geom_smooth(method = 'lm', se = F, col = 'black')

data %>%
  filter(postexposure_retention_interval > 24 &
           postexposure_retention_interval < 170 &
           postevent_retention_interval < 170 &
           postevent_retention_interval > 24) %>% 
  ggplot()+
  geom_point(aes(x = postexposure_retention_interval/24,
                 y = yi ),
             col   = hue,
             fill  = hue)+
  theme_classic()

data %>%
  filter(postexposure_retention_interval >= 0 &
           postexposure_retention_interval < 170 &
           postevent_retention_interval < 170 &
           postevent_retention_interval >= 0) %>% 
  ggplot()+
  geom_point(aes(x = postevent_retention_interval/24,
               y = yi), 
             fill  = hue,
             col = hue, 
             alpha = .4)+
  theme_classic()


# Control accuracy--------------------------------------------------------------

data %>%
  ggplot()+
  geom_point(aes(x = total_accuracy_control_prop, y = yi),
             alpha = .2)+
  geom_point(aes(x = total_accuracy_control_mean, y = yi)
             , alpha = .2)+
  xlab(label = 'Control Accuracy')+
  ylab('Hedges g')

# Control accuracy mean
data %>% 
  ggplot(aes(x = total_accuracy_control_mean, y = yi))+
  geom_point(alpha = .5, shape = 1, size = .5)+
  geom_smooth(method = "lm",
              se = F,
              col = hue) +
  geom_smooth(method = "lm",
              formula = y ~ poly(x, 2), 
              se = F,
              col = hue_2) +
  xlab(label = 'Control Accuracy')+
  scale_y_continuous(breaks=(seq(-3, 5, 1)), limits = c(-3, 5),
                     name = 'Hedges g')+
  geom_hline(yintercept = 0, linetype = 1, size = 1,
             col = 'black', alpha = .5)
ggsave('visuals/control_accuracy.png',dpi=300)


# Control accuracy proportion
data %>% 
  filter(yi < 5.3) %>% 
  ggplot(aes(x = total_accuracy_control_prop, y = yi))+
  geom_point()+
  geom_smooth(method = "lm",
              se = F,
              col = hue) +
  geom_smooth(method = "lm",
              formula = y ~ poly(x, 2), 
              se = F,
              col = hue) +
  xlab(label = 'Control Accuracy')+
  ylab('Hedges´g')


# Control type
data_es %>% 
  ggplot(aes(x = control_type, y = yi))+
  geom_jitter()+
  geom_violin(alpha = .5)+
  geom_boxplot(width = .2, outlier.shape = NA)+
  scale_y_continuous(limits = c(-3,5))+
  xlab(label = 'Control Accuracy')+
  ylab('Hedges´g')


data_es %>% 
  ggplot(aes(y = vi, x = yi))+
  geom_jitter(aes(col = control_type))+
  scale_y_continuous(limits = c(0, 1.5))+
  scale_x_continuous(limits = c(0,5))+
  xlab('Hedges´g')


 # Funnel plots------------------------------------------------------------------
meta_primary_2 <- readRDS("~/R/projects/Misinformation_meta/models/meta_primary_2.rds")

estimate = 0.7925508
se = meta_primary_2$se

sd_vec <- (sqrt(data_es$vi))
se_within <- data %>%
  filter(within_between == 'within') %>% 
  mutate(n = n_control)


se_between <- data %>%
  filter(within_between == 'between') %>% 
  mutate(n = n_mi + n_control)

se_vec <- rbind(se_between, se_within)
se_vec <- se_vec %>% mutate(se = (sqrt(vi))/(sqrt(n)))

se.seq <- seq(0, max(se_vec$se), 0.0004569)

# 95% CI region
ll95 = estimate-(1.96*se.seq)
ul95 = estimate+(1.96*se.seq)

# 99% CI region 
ll99 = estimate-(3.29*se.seq)
ul99 = estimate+(3.29*se.seq)

# 95% CI mean estimate
meanll95 = estimate-(1.96*se)
meanul95 = estimate+(1.96*se)

df_ci = data.frame(ll95, ul95, ll99, ul99, se.seq, estimate, meanll95, meanul95)

#Now we can actually make the funnel plot.
#Using your original data-frame, map standard error to your x-axis (for now) and Zr to your y-axis
fp <- ggplot(aes(x  = se, y = yi), data = se_vec) +
  geom_point(alpha = .2, size  = 2) +
  geom_line(aes(x  = se.seq, y = ll95), linetype = 'dotted', data = df_ci) +
  geom_line(aes(x  = se.seq, y = ul95), linetype = 'dotted', data = df_ci) +
  geom_line(aes(x  = se.seq, y = ll99), linetype = 'dashed', data = df_ci) +
  geom_line(aes(x  = se.seq, y = ul99), linetype = 'dashed', data = df_ci) +
  geom_segment(aes(x    = min(se.seq), y    = meanll95, 
                   xend = max(se.seq), yend = meanll95),
               linetype ='dotted', data = df_ci) +
  geom_segment(aes(x = min(se.seq), y = meanul95, xend = max(se.seq),
                   yend = meanul95), linetype='dotted', data = df_ci) +
  scale_x_reverse()+
  scale_y_continuous(breaks=seq(-3, 20, 1))+
  coord_flip()+
  xlab('Standard Error') + ylab('Hedges g')

fp
ggsave('visuals/funnel_large.png', dpi = 300)


# Adding pet-peese lines
fp + geom_smooth(method = "lm",
                 se = F,
                 col = hue) +
     geom_smooth(method = "lm",
                 formula = y ~ poly(x, 2), 
                 se = F,
                 col = hue) 

# Truncating the graph for easy of view
fp_2 <- ggplot(aes(x  = se, y = yi), data = filter(se_vec, yi < 5.3)) +
  geom_point(alpha = .2, size  = 2) +
  geom_line(aes(x  = se.seq, y = ll95), linetype = 'dotted', data = df_ci) +
  geom_line(aes(x  = se.seq, y = ul95), linetype = 'dotted', data = df_ci) +
  geom_line(aes(x  = se.seq, y = ll99), linetype = 'dashed', data = df_ci) +
  geom_line(aes(x  = se.seq, y = ul99), linetype = 'dashed', data = df_ci) +
  geom_segment(aes(x    = min(se.seq), y    = meanll95, 
                   xend = max(se.seq), yend = meanll95),
               linetype ='dotted', data = df_ci) +
  geom_segment(aes(x = min(se.seq), y = meanul95, xend = max(se.seq),
                   yend = meanul95), linetype='dotted', data = df_ci) +
  scale_x_reverse()+
  scale_y_continuous(breaks=seq(-3, 5, 1))+
  coord_flip()+
  xlab('Standard Error') + ylab('Hedges g')

fp_2
ggsave('visuals/funnel_small.png', dpi = 300)



# Adding pet-peese lines
fp_2 + geom_smooth(method = "lm",
                 se = F,
                 col = hue) +
  geom_smooth(method = "lm",
              formula = y ~ poly(x, 2), 
              se = F,
              col = hue) 

# Network-----------------------------------------------------------------------


# Create a dataframe with sample paper titles and author names
papers <- data %>% 
  filter(duplicated(id_record) == F) %>% 
  select(author, title) %>% 
  na.omit()

# Split the author names into separate columns
authors_split <- separate(papers, col = author, into = paste0("author_", 1:13), sep = "; ")

# Convert the author columns to a long format
authors_long <- authors_split %>%
  pivot_longer(cols = starts_with("author"), values_to = "author") %>%
  filter(!is.na(author))

collab_pairs <- authors_long %>%
  select(-title) %>%
  rename(author1 = author) %>%
  inner_join(authors_long %>%
               select(-title) %>%
               rename(author2 = author))

collab_counts <- collab_pairs %>%
  group_by(author1, author2) %>%
  summarise(count = n()) %>%
  ungroup()

collab_matrix <- collab_counts %>%
  pivot_wider(names_from = author2, values_from = count, values_fill = 0) %>%
  column_to_rownames("author1") %>%
  as.matrix()

row_names <- col_names <- rownames(collab_matrix)
square_matrix <- matrix(NA, nrow = length(row_names), ncol = length(col_names),
                        dimnames = list(row_names, col_names))

for (i in row_names) {
  for (j in col_names) {
    square_matrix[i, j] <- collab_matrix[i, j]
  }
}


# making the matrix smaller 
sorted_data <- collab_counts[order(-collab_counts$count), ]
sorted_data <- sorted_data %>%
  mutate(author1 = pmin(author1, author2),
         author2 = pmax(author1, author2)) %>%
  distinct(author1, author2, .keep_all = TRUE) %>% 
  filter(author1 !=author2)


# Select the top n rows
n <- 20  
top_n_data <- sorted_data[1:n, ]

small_collab_matrix <- top_n_data %>%
  pivot_wider(names_from = author2, values_from = count, values_fill = 0) %>%
  column_to_rownames("author1") %>%
  as.matrix()


row_names <- col_names <- rownames(small_collab_matrix)
square_matrix_2 <- matrix(NA, nrow = length(row_names), ncol = length(col_names),
                        dimnames = list(row_names, col_names))

for (i in row_names) {
  for (j in col_names) {
    square_matrix_2[i, j] <- small_collab_matrix[i, j]
  }
}

# Arc Diagram-------------------------------------------------------------------

# Select the top n rows
n <- 20  
top_n_data <- sorted_data[1:n, ]
arcplot(edgelist = as.matrix(top_n_data[-c(3)]),
        sorted = F,
        col.arcs = top_n_data$count)
#ring---------------------------------------------------------------------------

g <- graph(as.matrix(top_n_data[-c(3)]))
counts <- top_n_data$count

E(g)$count <- counts

layout <- layout.circle(g)

plot(g, layout = layout, edge.width = E(g)$count/10,
     edge.color = E(g)$count,
     vertex.size = 10, edge.arrow.size = 0)

