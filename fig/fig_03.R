# Fig. 3

#### Setup ####
source("./src/utils.R")
setup(use_stan=FALSE, is_rmd=FALSE)
library("cowplot")

#### Data ####
# data
path = "./data/fgcms.csv"
df = load_and_preprocess_data(path, group_list=c("R"))
Y = df$log_FGCMs

#### Posterior Predictive Checks ####
# load stan fit objects
fit_R01 = readRDS(file = './output/model-output/lmm_R01.RData') # load model output
fit_R02 = readRDS(file = './output/model-output/lmm_R02.RData') # load model output

p1 = vis_posterior_predictive_check(fit_R01, Y, "Model-R1", I=100)
p2 = vis_posterior_predictive_check(fit_R02, Y, "Model-R2", I=100)

p1 = p1 + theme(axis.title.y = element_text(margin = margin(r = 30))) + theme(plot.margin = unit(c(5, 20, 5, 5), "mm")) + NULL
p2 = p2 + theme(axis.title.y = element_text(margin = margin(r = 30))) + theme(plot.margin = unit(c(5, 20, 5, 5), "mm")) + NULL

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

ggsave(filename="./output/figure-for-paper/fig_03.png", plot=p, dpi=600, w=11, h=5)
ggsave(filename="./output/figure-for-paper/fig_03.pdf", plot=p, w=11, h=5)
# remove all objects
remove(list = ls())
