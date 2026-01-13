# Fig. 6

#### Setup ####
source("./src/utils.R")
setup(use_stan=FALSE, is_rmd=FALSE)
library(cowplot)

#### Data ####
path = "./data/fgcms.csv"
df = load_and_preprocess_data(path, group_list=c("R", "M"))
Y = df$log_FGCMs

#### Posterior predictive checks & Posteriors ####
COLOR = "#67A2A0"

fit_RM01 = readRDS(file = './output/model-output/lmm_RM.RData')

p1 = vis_posterior_predictive_check(fit_RM01, Y, "Model-RM", I=100, color=COLOR)
p2 = vis_slope_posteriors(fit_RM01, "Model-RM", xlim_factor = 2.0, color=COLOR)

p1 = p1 + theme(axis.title.y = element_text(margin = margin(r = 30))) + theme(plot.margin = unit(c(5, 10, 10, 5), "mm")) + NULL
p2 = p2 + theme(axis.title.y = element_text(margin = margin(r = 30))) + theme(plot.margin = unit(c(5, 10, 10, 5), "mm")) + NULL

# Arrange two plots (p1 and p2) in a single row with labels
p = cowplot::plot_grid(
  p1, p2,
  labels = c("(a)", "(b)"),
  label_fontfamily = "Helvetica",
  label_size = 18,                  # Set font size of labels
  nrow = 1, ncol = 2,               # Define layout as 1 row, 2 columns
  label_x = c(-0.03, -0.03),        # Adjust x positions of labels
  label_y = c(1.03, 1.03),          # Adjust y positions of labels
  rel_widths = c(1, 1),             # Set relative widths of plots
  rel_heights = c(1)                # Set relative heights of plots
)

# Apply additional styling to the combined plot
p = p + theme(
  plot.margin = margin(10, 10, 5, 10, unit = "mm"),          # Adjust plot margins
  plot.background = element_rect(fill = "white", color = NA) # Set background color as white
)

print(p)
ggsave(filename="./output/figure-for-paper/fig_06.png", plot=p, dpi=600, w=10, h=5)
ggsave(filename="./output/figure-for-paper/fig_06.pdf", plot=p, w=10, h=5)

# remove all objects
remove(list = ls())
