

# libraries --------------------------------------------------------------------
library(patchwork)


# combine ----------------------------------------------------------------------
plot_duration_ed_combined <- plot_duration_ed_glm + plot_duration_ed_glmm + plot_layout(ncol = 2)

plot_duration_ed_combined

ggsave(
  "out/plot_duration_ed_combined.tiff",
  plot = plot_duration_ed_combined,
  dpi = 300,
  height = 3,
  width = 8
)
