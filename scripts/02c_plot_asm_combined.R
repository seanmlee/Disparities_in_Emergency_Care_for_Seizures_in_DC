

# libraries --------------------------------------------------------------------
library(patchwork)


# combine ----------------------------------------------------------------------
plot_asm_combined <- plot_asm_glm + plot_asm_glmm + plot_layout(ncol = 2)

plot_asm_combined

ggsave(
  "out/plot_asm_combined.tiff",
  plot = plot_asm_combined,
  dpi = 300,
  height = 3,
  width = 8
)
