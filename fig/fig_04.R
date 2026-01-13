# Fig. 4

#### Setup ####
source("./src/utils.R")
setup(use_stan=FALSE, is_rmd=FALSE)
library("cowplot")

#### Data ####
path = "./data/fgcms.csv"
df = load_and_preprocess_data(path, group_list=c("R"))

#### Posterior distribution ####
# load stan fit objects
fit_R01 = readRDS(file = './output/model-output/lmm_R01.RData') # load model output
fit_R02 = readRDS(file = './output/model-output/lmm_R02.RData') # load model output

p3 = vis_slope_posteriors(fit_R01, "Model-R1", xlim_factor = 1.0)
p4 = vis_slope_posteriors(fit_R02, "Model-R2", xlim_factor = 1.0)

p3 = p3 + theme(axis.title.y = element_text(margin = margin(r = 30))) + theme(plot.margin = unit(c(5, 20, 5, 5), "mm")) + NULL
p4 = p4 + theme(axis.title.y = element_text(margin = margin(r = 30))) + theme(plot.margin = unit(c(5, 20, 5, 5), "mm")) + NULL

# Arrange plots p3 and p4 in a single row with labels
p = cowplot::plot_grid(
  p3, p4,
  labels = c("(a)", "(b)"),
  label_fontfamily = "Helvetica",
  label_x = c(-0.03, -0.03), # Adjust x positions of labels
  label_y = c(1.03, 1.03),   # Adjust y positions of labels
  label_size = 18,           # Adjust font size of labels
  nrow = 1, ncol = 2,        # Define layout as 1 row, 2 columns
  align = "hv",              # Align horizontally and vertically
  rel_widths = c(1, 1),      # Set relative widths of plots
  rel_heights = c(1)         # Set relative heights of plots
)

# Apply additional styling to the combined plot
p = p + theme(
  plot.margin = margin(10, 10, 5, 10, unit = "mm"),          # Adjust plot margins
  plot.background = element_rect(fill = "white", color = NA) # Set background color as white
)

print(p)
ggsave(filename="./output/figure-for-paper/fig_04.png", plot=p, dpi=600, w=11, h=7)
ggsave(filename="./output/figure-for-paper/fig_04.pdf", plot=p, w=11, h=7)

# remove all objects
remove(list = ls())
