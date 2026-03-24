################################################################################

# Misinformation Meta-analysis - Back-transformation of Predicted Effects

################################################################################

# Wrangle ----------------------------------------------------------------------

# Calculate mean accuracy and SD as proportions

data_sd <- data_es %>% 
  mutate(
    
    # Mean accuracy
    
    accuracy_control = case_when(
      !is.na(accuracy_control_mean) ~ accuracy_control_mean,
       is.na(accuracy_control_mean) ~ accuracy_control_prop
    ),
    accuracy_control = case_when(
      accuracy_type == "count"      ~ accuracy_control / items_control,
      accuracy_type == "proportion" ~ accuracy_control,
    ),
    accuracy_control = case_when(
      accuracy_control >  1 ~ accuracy_control_mean / items_total,
      accuracy_control <= 1 ~ accuracy_control
    ),
    
    accuracy_misled = case_when(
      !is.na(accuracy_mi_mean) ~ accuracy_mi_mean,
       is.na(accuracy_mi_mean) ~ accuracy_mi_prop
    ),
    accuracy_misled = case_when(
      accuracy_type == "count"      ~ accuracy_misled / items_misled,
      accuracy_type == "proportion" ~ accuracy_misled
    ),
    accuracy_misled = case_when(
      accuracy_misled >  1 ~ accuracy_misled / items_total,
      accuracy_misled <= 1 ~ accuracy_misled
    ),
    
    # SD
    
    accuracy_control_sd = case_when(
      !is.na(accuracy_control_sd) ~ accuracy_control_sd,
       is.na(accuracy_control_sd) ~ sqrt(accuracy_control_prop * (1 - accuracy_control_prop))
    ),
    accuracy_control_sd = case_when(
      accuracy_type == "count"      ~ accuracy_control_sd / items_control,
      accuracy_type == "proportion" ~ accuracy_control_sd
    ),
    
    accuracy_misled_sd = case_when(
      !is.na(accuracy_mi_sd) ~ accuracy_mi_sd,
       is.na(accuracy_mi_sd) ~ sqrt(accuracy_mi_prop * (1 - accuracy_mi_prop))
    ),
    accuracy_misled_sd = case_when(
      accuracy_type == "count"      ~ accuracy_misled_sd / items_misled,
      accuracy_type == "proportion" ~ accuracy_misled_sd
    )
    
  )

# Calculate weighted SD at 10 intervals

summary_sd <- data_sd %>% 
  group_by(cut = cut_interval(accuracy_control, 10)) %>% 
  summarise(
    sd = mean(c(weighted.mean(accuracy_control_sd, n_control),
                weighted.mean(accuracy_misled_sd, n_mi)))
  ) %>% 
  filter(!is.na(sd))

# Calculate predicted proportion changes

predictions <- pred_control_acc %>% 
  as.data.frame() %>% 
  mutate(
    control_acc = round(control_acc, 2)
  ) %>% 
  filter(control_acc %in% c(.05, .15, .25,
                            .35, .45, .55,
                            .65, .75, .85,
                            .95)) %>% 
  select(pred, ci.lb, ci.ub, control_acc)

summary_sd$prediction <- summary_sd$sd*predictions$pred
summary_sd$pred_lb    <- summary_sd$sd*predictions$ci.lb
summary_sd$pred_ub    <- summary_sd$sd*predictions$ci.ub

# Visualization ----------------------------------------------------------------

prediction_prop_plot <- 
ggplot(summary_sd,
       aes(
         x = cut,
         y = prediction
       )) +
  geom_hline(
    yintercept = 0,
    linetype = "dotted"
  ) +
  geom_line(
    group     = 1,
    color     = "darkred",
    linewidth = 1
  ) +
  geom_line(
    aes(
      x = cut,
      y = pred_lb
    ),
    group    = 1,
    color    = "darkred",
    linetype = "dashed"
  ) +
  geom_line(
    aes(
      x = cut,
      y = pred_ub
    ),
    group    = 1,
    color    = "darkred",
    linetype = "dashed"
  ) +
  scale_y_continuous(
    limits = c(-0.20, 0.45),
    breaks = seq(-0.20, 0.45, .05)
  ) +
  scale_x_discrete(
    labels = c(.05, .15, .25,
               .35, .45, .55,
               .65, .75, .85,
               .95)
  ) +
  labs(
    x = "Control Accuracy",
    y = "Predicted Misinformation Effect (Accuracy Change)"
  ) +
  theme_classic()

save_plot("figures/mema_predicted-proportion-plot.png",
          prediction_prop_plot,
          base_height = 4, base_width = 8)
