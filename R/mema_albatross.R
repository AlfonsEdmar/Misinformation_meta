################################################################################

#  Misinformation Meta-analysis - Albatross Plot

################################################################################

# Utility functions ------------------------------------------------------------

albatross_cont_smd <- function(p, smd) {
  
  n <- ((8 + smd^2) / (2 * smd^2)) * qnorm(p)^2
  
  return(n)
  
}

# Data -------------------------------------------------------------------------

data_es$p     <- pnorm(data_es$yi/sqrt(data_es$vi))

data_es <- data_es %>% 
  mutate(
    p_log = case_when(
      p - .50 < 0 ~  log10(p),
      p - .50 > 0 ~ -log10(1 - p)
    )
  )

p_range <- sort(
  
  c(seq(1, 
        exp(-35), 
        length.out = 1001), 
    .9999, .99999, .999999, .99999999, .9999999999999, 1 - exp(-35), 1 - exp(-40),
    .0001, .00001, .000001, .00000001, .0000000000001, exp(-35))
  
)

p_range <- p_range[p_range != 1]

contour_data <- data.frame(
  p     = p_range,
  n_010 = albatross_cont_smd(p_range, 0.10),
  n_030 = albatross_cont_smd(p_range, 0.30),
  n_050 = albatross_cont_smd(p_range, 0.50),
  n_080 = albatross_cont_smd(p_range, 0.80),
  n_100 = albatross_cont_smd(p_range, 1.00),
  n_150 = albatross_cont_smd(p_range, 1.50),
  n_200 = albatross_cont_smd(p_range, 2.00)
)

contour_data_long <- contour_data %>% 
  pivot_longer(
    cols      = c("n_010", "n_030", "n_050", "n_080", "n_100", "n_150", "n_200"),
    names_to  = "smd",
    values_to = "n"
  )

contour_data_long <- contour_data_long %>% 
  filter(
    n >= 1
  )

contour_data_long <- contour_data_long %>% 
  mutate(
    p_log = case_when(
      p - .50 < 0 ~  log10(p),
      p - .50 > 0 ~ -log10(1 - p)
    )
  )

# Visualization ----------------------------------------------------------------

albatross <- 
ggplot(data_es %>% 
         filter(p_log != Inf),
       aes(
         y = log10(n_mi + n_control)^2,
         x = p_log
       )) +
  geom_line(
    data = contour_data_long,
    aes(
      group    = smd,
      linetype = smd,
      y        = log10(n)^2,
      x        = p_log
    ),
    alpha = .50
  ) +
  geom_point(
    size  = 1,
    shape = 1,
    alpha = .25
  ) +
  scale_y_continuous(
    limits = c(0, 10.1),
    breaks = log10(c(1, 50, 200, 500, 1500))^2,
    labels = c(1, 50, 200, 500, 1500)
  ) +
  scale_x_continuous(
    limits = c(-18, 18),
    breaks = c( log(2*c(1e-08, 1e-05, 1e-03, 1e-02, .05, .5)), 
               -log(2*c(.05, 1e-02, 1e-03, 1e-05, 1e-08))),
    labels = c("1e-08", "1e-05", ".001", ".01", ".05", "1",
              ".05", ".01", ".001", "1e-05", "1e-08")
  ) +
  scale_linetype_discrete(
    labels = c("0.10",
               "0.30",
               "0.50",
               "0.80",
               "1.00",
               "1.50",
               "2.00")
  ) +
  labs(
    x        = "p-value",
    y        = "Sample size",
    linetype = "Effect size"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 60,
                               hjust = 1)
  )

save_plot("figures/mema_albatross-plot.png", 
          albatross,
          base_height = 6, base_width = 6)
