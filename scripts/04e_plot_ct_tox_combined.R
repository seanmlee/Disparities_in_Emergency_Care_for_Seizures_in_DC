

# libraries --------------------------------------------------------------------
library(patchwork)


# combine ----------------------------------------------------------------------
plot_ct_tox_combined <- plot_ct_glm + plot_ct_glmm + plot_tox_glm + plot_tox_glmm + plot_layout(ncol = 2)

plot_ct_tox_combined

# optional: save to file
ggsave(
  "out/plot_ct_tox_combined.tiff",
  plot = plot_ct_tox_combined,
  dpi = 300,
  height = 6,
  width = 8
)
