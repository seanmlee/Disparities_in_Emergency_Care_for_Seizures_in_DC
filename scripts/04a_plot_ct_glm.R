

# load libraries ---------------------------------------------------------------
library(tidyverse)
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
  mod_ct_no, 
  newdata = newdata, 
  type = "response",
  se.fit = TRUE
)

fit_response <- preds$fit

upr_response <- preds$fit + (critval * preds$se.fit)

lwr_response <- preds$fit - (critval * preds$se.fit)

fit <- as.data.frame(
  
  cbind(
    newdata,
    fit_response, 
    upr_response,
    lwr_response
  )
  
)


# plot -------------------------------------------------------------------------
plot_ct_glm <- 
  
  fit %>%
  
  ggplot(
    aes(
      x = group, 
      y = fit_response,
      group = 1,
      ymin = lwr_response,
      ymax = upr_response
    )
  ) +
  
  geom_errorbar(
    width = 0.4
  ) +
  
  geom_point(
    size = 10,
    pch = 21,
    fill = "white"
  ) +
  
  geom_text(
    aes(label = scales::percent(fit_response, accuracy = 1)),
    vjust = 0.5,
    hjust = 0.5,
    size = 3.5
  ) +
  
  ggtitle("a)") +
  
  xlab("") +
  
  ylab("Probability of CT") +
  
  scale_x_discrete(
    labels = c("White \nWards 0-6", "Black \nWards 0-6", "Black \nWards 7-8")
  ) +
  
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.25),
    labels = scales::percent_format(accuracy = 1)
  ) +
  
  theme_bw() +
  
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 15, face = "bold"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  ) +
  
  geom_signif(
    
    y_position = 0.5, 
    xmin = 1.025,
    xmax = 2,
    size = 0.075,
    annotation = paste0("p=", format(round(summary(mod_ct_no)$coefficients[2, "Pr(>|z|)"], 3), nsmall = 3)), 
    tip_length = c(0.3, 0.45),
    color = "black",
    vjust = -0.25,
    textsize = 4
    
  ) +
  
  geom_signif(
    
    y_position = 0.7,
    xmin = 0.975,
    xmax = 3,
    size = 0.075,
    annotation = paste0("p=", format(round(summary(mod_ct_no)$coefficients[3, "Pr(>|z|)"], 3), nsmall = 3)),
    tip_length = c(0.89, 0.675),
    color = "black",
    vjust = -0.25,
    textsize = 4
    
  )

plot_ct_glm

ggsave(
  "out/plot_ct_no.tiff",
  dpi = 300,
  height = 3,
  width = 4
)
