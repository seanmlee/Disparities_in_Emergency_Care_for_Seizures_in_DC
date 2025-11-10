

# load libraries ---------------------------------------------------------------
library(tidyverse)
library(ggbeeswarm)
library(ggsignif)


# preds ------------------------------------------------------------------------
critval <- 1.96

newdata <- expand.grid(
  group = c("A", "B", "C"),
  age = mean(ed$age),
  sex = "Male",
  substance_abuse = "No",
  neuromodulation = "No",
  hx_head_trauma = "No",
  hx_psych_trauma = "No",
  prior_eeg = "No",
  seizure_captured = "No",
  admitted_monitored = "No",
  baseline_asm = "1",
  total_meds_baseline = mean(ed$total_meds_baseline)
)

preds <- predict(
  mod_duration_ed_no, 
  newdata = newdata, 
  type = "link",
  se.fit = TRUE
)

fit_response <- preds$fit

upr_response <- preds$fit + (critval * preds$se.fit)

lwr_response <- preds$fit - (critval * preds$se.fit)

logit_to_prob <- function(x) {
  exp(x)  # Inverse logit function
}

fit_response <- logit_to_prob(fit_response)
upr_response <- logit_to_prob(upr_response)
lwr_response <- logit_to_prob(lwr_response)

fit <- as.data.frame(
  
  cbind(
    newdata,
    fit_response, 
    upr_response,
    lwr_response
  )
  
)


# plot -------------------------------------------------------------------------
plot_duration_ed_glm <-

fit %>%
  
  ggplot(
    aes(
      x = group, 
      y = fit_response,
      group = 1
    )
  ) +
  
  geom_point(
    data = ed_no, 
    mapping = aes(
      x = group, 
      y = duration_ed
    ),
    pch = 21,                    # allows fill + border
    fill = alpha("grey50", 0.1), # semi-transparent interior
    color = "black",             # bold stroke color
    stroke = 0.2,                # thickness of outline
    size = 1,                    # point size
    position = position_jitter(width = 0.05)
  ) +
  
  geom_errorbar(
    aes(
      ymin = lwr_response,
      ymax = upr_response
    ),
    alpha = 1,
    fill = "black",
    width = 0.4
    
  ) +

  geom_point(
    size = 8,
    pch = 21,
    fill = "white"
  ) +
  
  geom_text(
    aes(label = round(fit_response, 0)),
    vjust = 0.5,
    hjust = 0.5,
    size = 3.5
  ) +
  
  ggtitle("a)") +
  
  xlab("") +
  
  ylab("Duration in ED") +
  
  scale_x_discrete(
    labels = c("White \nWards 0-6", "Black \nWards 0-6", "Black \nWards 7-8")
  ) +
  
  scale_y_continuous(
    limits = c(0, 31),
    breaks = seq(0, 30, 10)
  ) +
  
  theme_bw() +
  
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 15, face = "bold"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  ) +
  
  geom_signif(
    
    y_position = 24.6, 
    xmin = 1.015,
    xmax = 2,
    size = 0.075,
    annotation = paste0("p=", format(round(summary(mod_duration_ed_no)$coefficients[2, "Pr(>|t|)"], 3), nsmall = 3)), 
    tip_length = c(0.158, 0.001),
    color = "black",
    vjust = -0.25,
    textsize = 4
    
  ) +
  
  geom_signif(
    
    y_position = 28.5, 
    xmin = 0.985,
    xmax = 3,
    size = 0.075,
    annotation = paste0("p=", format(round(summary(mod_duration_ed_no)$coefficients[3, "Pr(>|t|)"], 3), nsmall = 3)),
    tip_length = c(0.199, 0.136),
    color = "black",
    vjust = -0.225,
    textsize = 4
    
  )

plot_duration_ed_glm

ggsave(
  "out/plot_duration_ed_no.tiff",
  dpi = 300,
  height = 3,
  width = 4
)
