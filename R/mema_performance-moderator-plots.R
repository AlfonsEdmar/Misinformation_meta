################################################################################

# Misinformation Meta-Analysis - Performance and Moderator Visualizations

################################################################################

# Control accuracy linear visualization ----------------------------------------

# Create prediction line

pred_control_acc <- predict(meta_primary, 
                            newmods = cbind(
                              rep(0, 100),
                              rep(0, 100),
                              rep(0, 100),
                              rep(0, 100),
                              rep(0, 100),
                              seq(min(data_es$control_acc, na.rm = TRUE),
                                  max(data_es$control_acc, na.rm = TRUE), 
                                  length.out = 100),
                              rep(0, 100),
                              rep(0, 100),
                              rep(0, 100),
                              rep(0, 100)
                            ))

pred_control_acc$control_acc <- seq(min(data_es$control_acc, na.rm = TRUE),
                                    max(data_es$control_acc, na.rm = TRUE), 
                                    length.out = 100) - min(data_es$control_acc, na.rm = TRUE)

# Visualization of control accuracy

scatter_control_acc <-
  ggplot(data_es,
         aes(
           x = control_acc - min(control_acc, na.rm = TRUE),
           y = yi
         )) +
  geom_vline(
    xintercept = abs(min(data_es$control_acc, na.rm = TRUE)),
    linetype   = "dashed"
  ) +
  geom_hline(
    yintercept = meta_primary$beta[[1]],
    linetype   = "dashed",
    linewidth  = 1
  ) +
  geom_hline(
    yintercept = 0,
    linetype   = "dotted"
  ) +
  geom_point(
    aes(
      size = 1/vi
    ),
    shape = 1,
    alpha = .25
  ) +
  geom_line(
    data = as.data.frame(pred_control_acc),
    aes(
      x = control_acc,
      y = pred
    ),
    linetype  = "solid",
    alpha     = .80,
    color     = "darkred",
    linewidth = 1
  ) +
  geom_line(
    data = as.data.frame(pred_control_acc),
    aes(
      x = control_acc,
      y = ci.lb
    ),
    linetype  = "dashed",
    alpha     = .80,
    color     = "darkred",
    linewidth = 1
  ) +
  geom_line(
    data = as.data.frame(pred_control_acc),
    aes(
      x = control_acc,
      y = ci.ub
    ),
    linetype  = "dashed",
    alpha     = .80,
    color     = "darkred",
    linewidth = 1
  ) +
  scale_x_continuous(
    breaks = seq(0, 1, .10)
  ) +
  scale_y_continuous(
    breaks = seq(-4, 8, 1),
    limits = c(-4, 8)
  ) +
  labs(
    x = "Control Accuracy",
    y = "Effect size"
  ) +
  guides(
    size = "none"
  ) +
  theme_classic()

save_plot("figures/mema_control-accuracy-plot.png", 
          scatter_control_acc,
          base_height = 6, base_width = 6)

# Control accuracy quadratic visualization -------------------------------------

## Create prediction line

pred_control_acc_sq <- predict(meta_quad, 
                               newmods = cbind(
                                 rep(0, 100),
                                 rep(0, 100),
                                 rep(0, 100),
                                 rep(0, 100),
                                 rep(0, 100),
                                 seq(min(data_es$control_acc, na.rm = TRUE),
                                     max(data_es$control_acc, na.rm = TRUE), 
                                     length.out = 100),
                                 seq(min(data_es$control_acc, na.rm = TRUE),
                                     max(data_es$control_acc, na.rm = TRUE), 
                                     length.out = 100)^2,
                                 rep(0, 100),
                                 rep(0, 100),
                                 rep(0, 100),
                                 rep(0, 100)
                               ))

pred_control_acc_sq$control_acc <- seq(min(data_es$control_acc, na.rm = TRUE),
                                       max(data_es$control_acc, na.rm = TRUE), 
                                       length.out = 100) - min(data_es$control_acc, na.rm = TRUE)

# Visualization of control accuracy

scatter_control_acc_sq <-
  ggplot(data_es,
         aes(
           x = control_acc - min(control_acc, na.rm = TRUE),
           y = yi
         )) +
  geom_vline(
    xintercept = abs(min(data_es$control_acc, na.rm = TRUE)),
    linetype   = "dashed"
  ) +
  geom_hline(
    yintercept = meta_primary$beta[[1]],
    linetype   = "dashed",
    linewidth  = 1
  ) +
  geom_hline(
    yintercept = 0,
    linetype   = "dotted"
  ) +
  geom_point(
    aes(
      size = 1/vi
    ),
    shape = 1,
    alpha = .25
  ) +
  geom_line(
    data = as.data.frame(pred_control_acc_sq),
    aes(
      x = control_acc,
      y = pred
    ),
    linetype  = "solid",
    alpha     = .80,
    color     = "darkred",
    linewidth = 1
  ) +
  geom_line(
    data = as.data.frame(pred_control_acc_sq),
    aes(
      x = control_acc,
      y = ci.lb
    ),
    linetype  = "dashed",
    alpha     = .80,
    color     = "darkred",
    linewidth = 1
  ) +
  geom_line(
    data = as.data.frame(pred_control_acc_sq),
    aes(
      x = control_acc,
      y = ci.ub
    ),
    linetype  = "dashed",
    alpha     = .80,
    color     = "darkred",
    linewidth = 1
  ) +
  scale_x_continuous(
    breaks = seq(0, 1, .10)
  ) +
  scale_y_continuous(
    breaks = seq(-4, 8, 1),
    limits = c(-4, 8)
  ) +
  labs(
    x = "Control Accuracy",
    y = "Effect size"
  ) +
  guides(
    size = "none"
  ) +
  theme_classic()


save_plot("figures/mema_control-accuracy-quadratic-plot.png", 
          scatter_control_acc_sq,
          base_height = 6, base_width = 6)

# Visualization: Predicted vs. residuals ---------------------------------------

plot_pred_res <- 
  ggplot(data_es,
         aes(
           x    = pred,
           y    = resid,
           size = 1/vi
         )) +
  geom_point(
    shape = 1,
    alpha = .25
  ) +
  geom_hline(
    yintercept = 0,
    linetype   = "dotted"
  ) +
  geom_smooth(
    method  = "lm",
    formula = "y ~ x",
    se      = FALSE
  ) +
  guides(
    size = "none"
  ) +
  labs(
    x = "Predicted",
    y = "Standardized residual"
  ) +
  scale_y_continuous(
    breaks = seq(-4, 20, 2)
  ) +
  theme_classic()

save_plot("figures/mema_pred-res-plot.png", 
          plot_pred_res,
          base_height = 6, base_width = 6)

# QQ-Plot of residuals ---------------------------------------------------------

qqplot_residuals <- 
ggplot(data_es,
       aes(
         sample = yi
       )) +
  stat_qq(
    shape = 1,
    alpha = .33
  ) +
  geom_qq_line(
    linetype = "dashed"
  ) +
  scale_y_continuous(
    breaks = seq(-4, 20, 2)
  ) +
  scale_x_continuous(
    breaks = seq(-3, 3, 1)
  ) +
  labs(
    y = "Standardized residual",
    x = "Normal distribution quantile"
  ) +
  theme_classic()

save_plot("figures/mema_qqplot-residuals.png",
          qqplot_residuals,
          base_height = 6, base_width = 6)

# Visualization: Publication history -------------------------------------------

plot_timeline <- 
  ggplot(data_es,
         aes(
           x = year_raw,
           y = yi,
           size = 1/vi
         )) +
  geom_quasirandom(
    shape = 1,
    alpha = .25
  ) +
  geom_hline(
    yintercept = meta_primary$beta[[1]],
    linetype   = "dashed",
    linewidth  = 1
  ) +
  geom_hline(
    yintercept = 0,
    linetype   = "dotted"
  ) +
  guides(
    size = "none"
  ) +
  scale_y_continuous(
    breaks = seq(-4, 20, 1),
    limits = c(-4, 8)
  ) +
  scale_x_continuous(
    breaks = seq(1975, 2025, 5)
  ) +
  labs(
    x = "Publication year",
    y = "Effect size"
  ) +
  theme_classic()

save_plot("figures/mema_timeline-plot.png", 
          plot_timeline,
          base_height = 6, base_width = 12)

# Visualization: Albatross -----------------------------------------------------

source("R/mema_albatross.R")

# Post-Exposure Retention Interval visualization -------------------------------

# Create prediction line

pred_control_ret <- predict(meta_primary, 
                            newmods = cbind(
                              rep(0, 100), # Post event retention
                              seq(min(data_es$postexposure_retention_interval, na.rm = TRUE),
                                  max(data_es$postexposure_retention_interval, na.rm = TRUE), 
                                  length.out = 100), # Post exposure retention
                              rep(0, 100), # Pre event warning
                              rep(0, 100), # Post event warning
                              rep(0, 100), # Post exposure warning
                              rep(0, 100), # Control accuracy
                              rep(0, 100), # Post event testing
                              rep(0, 100), # Post exposure testing
                              rep(0, 100), # Publication year
                              rep(0, 100)  # Prereg
                            ))

pred_control_ret$postexposure_retention_interval <- 
  seq(min(data_es$postexposure_retention_interval, na.rm = TRUE),
      max(data_es$postexposure_retention_interval, na.rm = TRUE),
      length.out = 100)

pred_control_ret$ret_days <- pred_control_ret$postexposure_retention_interval/24

# Visualization of control accuracy

scatter_retention_60 <-
  ggplot(data_es,
         aes(
           x = postexposure_retention_interval,
           y = yi
         )) +
  geom_hline(
    yintercept = meta_primary$beta[[1]],
    linetype   = "dashed",
    linewidth  = 1
  ) +
  geom_hline(
    yintercept = 0,
    linetype   = "dotted"
  ) +
  geom_point(
    aes(
      size = 1/vi
    ),
    shape = 1,
    alpha = .15
  ) +
  geom_line(
    data = as.data.frame(pred_control_ret),
    aes(
      x = postexposure_retention_interval,
      y = pred
    ),
    linetype  = "solid",
    alpha     = .80,
    color     = "darkred",
    linewidth = 1
  ) +
  geom_line(
    data = as.data.frame(pred_control_ret),
    aes(
      x = postexposure_retention_interval,
      y = ci.lb
    ),
    linetype  = "dashed",
    alpha     = .80,
    color     = "darkred",
    linewidth = 1
  ) +
  geom_line(
    data = as.data.frame(pred_control_ret),
    aes(
      x = postexposure_retention_interval,
      y = ci.ub
    ),
    linetype  = "dashed",
    alpha     = .80,
    color     = "darkred",
    linewidth = 1
  ) +
  scale_x_continuous(
    breaks = c(min(data_es$postexposure_retention_interval, na.rm = TRUE),
               seq(24 * 6, 24 * 60, length.out = 10),
               max(data_es$postexposure_retention_interval, na.rm = TRUE)),
    labels = c(min(data_es$postexposure_retention_interval, na.rm = TRUE),
               seq(24 * 6, 24 * 60, length.out = 10),
               max(data_es$postexposure_retention_interval, na.rm = TRUE))/24,
    limits = c(0, 24 * 65)
  ) +
  scale_y_continuous(
    breaks = seq(-4, 8, 1),
    limits = c(-4, 8)
  ) +
  labs(
    x = "Post-Exposure Retention Interval (Days, Truncated)",
    y = "Effect size"
  ) +
  guides(
    size = "none"
  ) +
  theme_classic()

scatter_retention_full <-
  ggplot(data_es,
         aes(
           x = postexposure_retention_interval,
           y = yi
         )) +
  geom_hline(
    yintercept = meta_primary$beta[[1]],
    linetype   = "dashed",
    linewidth  = 1
  ) +
  geom_hline(
    yintercept = 0,
    linetype   = "dotted"
  ) +
  geom_point(
    aes(
      size = 1/vi
    ),
    shape = 1,
    alpha = .15
  ) +
  geom_line(
    data = as.data.frame(pred_control_ret),
    aes(
      x = postexposure_retention_interval,
      y = pred
    ),
    linetype  = "solid",
    alpha     = .80,
    color     = "darkred",
    linewidth = 1
  ) +
  geom_line(
    data = as.data.frame(pred_control_ret),
    aes(
      x = postexposure_retention_interval,
      y = ci.lb
    ),
    linetype  = "dashed",
    alpha     = .80,
    color     = "darkred",
    linewidth = 1
  ) +
  geom_line(
    data = as.data.frame(pred_control_ret),
    aes(
      x = postexposure_retention_interval,
      y = ci.ub
    ),
    linetype  = "dashed",
    alpha     = .80,
    color     = "darkred",
    linewidth = 1
  ) +
  scale_x_continuous(
    breaks = c(min(data_es$postexposure_retention_interval, na.rm = TRUE),
               seq(50, 350, 50) * 24),
    labels = c(min(data_es$postexposure_retention_interval, na.rm = TRUE),
               seq(50, 350, 50) * 24)/24,
    limits = c(0, max(data_es$postexposure_retention_interval, na.rm = TRUE))
  ) +
  scale_y_continuous(
    breaks = seq(-4, 8, 1),
    limits = c(-4, 8)
  ) +
  labs(
    x = "Post-Exposure Retention Interval (Days, Full)",
    y = "Effect size"
  ) +
  guides(
    size = "none"
  ) +
  theme_classic()

save_plot("figures/mema_retention-60-days-plot.png", 
          scatter_retention_60,
          base_height = 6, base_width = 6)

save_plot("figures/mema_retention-full-plot.png", 
          scatter_retention_full,
          base_height = 6, base_width = 6)

# Post-Exposure Warning Visualizations -----------------------------------------

pred_warn <- predict(meta_primary, 
                     newmods = cbind(
                       rep(0, 3), # Post event retention
                       rep(0, 3), # Post exposure retention
                       rep(0, 3), # Pre event warning
                       rep(0, 3), # Post event warning
                       c(0, 1, 2), # Post exposure warning
                       rep(0, 3), # Control accuracy
                       rep(0, 3), # Post event testing
                       rep(0, 3), # Post exposure testing
                       rep(0, 3), # Publication year
                       rep(0, 3)  # Prereg
                     ))

pred_warn$postexposure_warning <- c(0, 1, 2)

# Post-exposure warning visualization

scatter_warning <-
  ggplot(data_es,
         aes(
           x = as.factor(postexposure_warning),
           y = yi
         )) +
  geom_hline(
    yintercept = meta_primary$beta[[1]],
    linetype   = "dashed",
    linewidth  = 1
  ) +
  geom_hline(
    yintercept = 0,
    linetype   = "dotted"
  ) +
  geom_quasirandom(
    aes(
      size = 1/vi
    ),
    shape = 1,
    alpha = .15
  ) +
  geom_errorbar(
    data = as.data.frame(pred_warn),
    inherit.aes = FALSE,
    aes(
      x    = as.factor(postexposure_warning),
      ymin = ci.lb,
      ymax = ci.ub
    ),
    width     = .33,
    linewidth = 1,
    color     = "darkred"
  ) +
  geom_point(
    data = as.data.frame(pred_warn),
    inherit.aes = FALSE,
    aes(
      x = as.factor(postexposure_warning),
      y = pred
    ),
    shape = 16,
    size  = 3,
    color = "darkred"
  ) +
  geom_line(
    data = as.data.frame(pred_warn),
    inherit.aes = FALSE,
    aes(
      x = as.factor(postexposure_warning),
      y = pred,
      group = 1
    ),
    linewidth = 0.80,
    color     = "darkred"
  ) +
  scale_y_continuous(
    breaks = seq(-4, 8, 1),
    limits = c(-4, 8)
  ) +
  labs(
    x = "Post-Exposure Warnings",
    y = "Effect size"
  ) +
  guides(
    size = "none"
  ) +
  theme_classic()

save_plot("figures/mema_warning-plot.png", 
          scatter_warning,
          base_height = 6, base_width = 6)

# Preregistration Visualization ------------------------------------------------

pred_reg  <- predict(meta_primary, 
                     newmods = cbind(
                       rep(0, 2), # Post event retention
                       rep(0, 2), # Post exposure retention
                       rep(0, 2), # Pre event warning
                       rep(0, 2), # Post event warning
                       rep(0, 2), # Post exposure warning
                       rep(0, 2), # Control accuracy
                       rep(0, 2), # Post event testing
                       rep(0, 2), # Post exposure testing
                       rep(0, 2), # Publication year
                       c(0, 1)    # Prereg
                     ))

pred_reg$preregistered <- c(0, 1)

# Pre-registration visualization

scatter_prereg <-
  ggplot(data_es,
         aes(
           x = as.factor(preregistered),
           y = yi
         )) +
  geom_hline(
    yintercept = meta_primary$beta[[1]],
    linetype   = "dashed",
    linewidth  = 1
  ) +
  geom_hline(
    yintercept = 0,
    linetype   = "dotted"
  ) +
  geom_quasirandom(
    aes(
      size = 1/vi
    ),
    shape = 1,
    alpha = .25
  ) +
  geom_errorbar(
    data = as.data.frame(pred_reg),
    inherit.aes = FALSE,
    aes(
      x    = as.factor(preregistered),
      ymin = ci.lb,
      ymax = ci.ub
    ),
    width     = .33,
    linewidth = 1,
    color     = "darkred"
  ) +
  geom_point(
    data = as.data.frame(pred_reg),
    inherit.aes = FALSE,
    aes(
      x = as.factor(preregistered),
      y = pred
    ),
    shape = 16,
    size  = 3,
    color = "darkred"
  ) +
  geom_line(
    data = as.data.frame(pred_reg),
    inherit.aes = FALSE,
    aes(
      x = as.factor(preregistered),
      y = pred,
      group = 1
    ),
    linewidth = 0.80,
    color     = "darkred"
  ) +
  scale_y_continuous(
    breaks = seq(-4, 8, 1),
    limits = c(-4, 8)
  ) +
  scale_x_discrete(
    labels = c("No", "Yes")
  ) +
  labs(
    x = "Preregistered",
    y = "Effect size"
  ) +
  guides(
    size = "none"
  ) +
  theme_classic()

save_plot("figures/mema_prereg-plot.png", 
          scatter_prereg,
          base_height = 6, base_width = 6)

# Post-Event Testing Visualization ---------------------------------------------

pred_res  <- predict(meta_primary, 
                     newmods = cbind(
                       rep(0, 7), # Post event retention
                       rep(0, 7), # Post exposure retention
                       rep(0, 7), # Pre event warning
                       rep(0, 7), # Post event warning
                       rep(0, 7), # Post exposure warning
                       rep(0, 7), # Control accuracy
                       0:6,       # Post event testing
                       rep(0, 7), # Post exposure testing
                       rep(0, 7), # Publication year
                       rep(0, 7)  # Prereg
                     ))

pred_res$postevent_recall <- 0:6

# Pre-registration visualization

scatter_res <-
  ggplot(data_es,
         aes(
           x = postevent_recall,
           y = yi
         )) +
  geom_hline(
    yintercept = meta_primary$beta[[1]],
    linetype   = "dashed",
    linewidth  = 1
  ) +
  geom_hline(
    yintercept = 0,
    linetype   = "dotted"
  ) +
  geom_point(
    aes(
      size = 1/vi
    ),
    shape = 1,
    alpha = .25
  ) +
  geom_line(
    data = as.data.frame(pred_res),
    inherit.aes = FALSE,
    aes(
      x = postevent_recall,
      y = pred,
      group = 1
    ),
    linewidth = 0.80,
    color     = "darkred"
  ) +
  geom_line(
    data = as.data.frame(pred_res),
    inherit.aes = FALSE,
    aes(
      x = postevent_recall,
      y = ci.lb,
      group = 1
    ),
    linewidth = 0.80,
    color     = "darkred",
    linetype  = "dashed"
  ) +
  geom_line(
    data = as.data.frame(pred_res),
    inherit.aes = FALSE,
    aes(
      x = postevent_recall,
      y = ci.ub,
      group = 1
    ),
    linewidth = 0.80,
    color     = "darkred",
    linetype  = "dashed"
  ) +
  scale_y_continuous(
    breaks = seq(-4, 8, 1),
    limits = c(-4, 8)
  ) +
  scale_x_continuous(
    breaks = 0:6
  ) +
  labs(
    x = "Post-Event Tests",
    y = "Effect size"
  ) +
  guides(
    size = "none"
  ) +
  theme_classic()

save_plot("figures/mema_res-plot.png", 
          scatter_prereg,
          base_height = 6, base_width = 6)


# Unstandardized effect size back-transformation -------------------------------

source("R/mema_predicted-proportion.R")

# Visualization: Grids ---------------------------------------------------------

# Heterogeneity and performance grid

grid_upper <- plot_grid(funnel_plot, albatross,
                        qqplot_residuals, plot_pred_res,
                        labels = c("a", "b", "c", "d"),
                        nrow = 2)

grid_plot <- plot_grid(grid_upper,
                       plot_timeline,
                       labels = c("", "e"),
                       nrow = 2,
                       rel_heights = c(1, .5))

save_plot("figures/mema_performance-grid-full.png",
          grid_plot, 
          base_height = 14,
          base_width  = 9)

# Moderator grid

mod_grid_upper <- plot_grid(scatter_retention_60, scatter_retention_full,
                            scatter_control_acc, prediction_prop_plot,
                            scatter_warning, scatter_res,
                            labels = c("a", "b", "c", "d", "e", "f"),
                            nrow = 3)

# mod_grid_plot  <- plot_grid(mod_grid_upper,
#                             plot_timeline,
#                             labels = c("", "e"),
#                             nrow = 2,
#                             rel_heights = c(1, .5))

save_plot("figures/mema_moderator-grid-full.png",
          mod_grid_upper, 
          base_height = 14,
          base_width  = 9)

