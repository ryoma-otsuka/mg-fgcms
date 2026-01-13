# Fig. S1

#### Setup ####
source("./src/utils.R")
setup(use_stan=FALSE, is_rmd=FALSE)
library(cowplot)

#### Data ####
# volume
df = read.csv("./data/experiment-data/1g8ml_05g5ml.csv")
# print(df)
df_volume = prep_df_slope(df, exp_type='volume')

# filter
df = read.csv("./data/experiment-data/no_filter_filter.csv")
df_filter = prep_df_slope(df, exp_type='filter')

# nest
df = read.csv("./data/experiment-data/trail_nest.csv")
df_nest = prep_df_slope(df, exp_type='nest')

#### Slope plot ####
common_y_lim = c(0, 1300) 
common_y_lim_log_scale = c(3.5, 8.5)

df2 = df_volume
df2$Condition = factor(df2$Condition, levels = c("1.00g/8ml", "0.50g/5ml"))
p1 = vis_slope_plot(df2, x_labels=c("1.00 g/8.0 ml", "0.50 g/5.0 ml"), y_lim=common_y_lim)
p4 = vis_slope_plot(df2, x_labels=c("1.00 g/8.0 ml", "0.50 g/5.0 ml"), y_lim=common_y_lim_log_scale, log_scale=TRUE)

df2 = df_filter
df2$Condition = factor(df2$Condition, levels = c("No_filter", "Filter"))
p2 = vis_slope_plot(df2, x_labels=c("No filter", "Filter"), y_lim=common_y_lim)
p5 = vis_slope_plot(df2, x_labels=c("No filter", "Filter"), y_lim=common_y_lim_log_scale, log_scale=TRUE)

df2 = df_nest
df2$Condition = factor(df2$Condition, levels = c("Trail", "Nest"))
p3 = vis_slope_plot(df2, x_labels=c("Trail", "Nest"), y_lim=common_y_lim)
p6 = vis_slope_plot(df2, x_labels=c("Trail", "Nest"), y_lim=common_y_lim_log_scale, log_scale=TRUE)

# Adjust y-label and margin
p1 = p1 + theme(axis.title.y = element_text(margin = margin(r = 40))) + theme(plot.margin = unit(c(5, 10, 10, 5), "mm")) + NULL
p2 = p2 + theme(axis.title.y = element_text(margin = margin(r = 40))) + theme(plot.margin = unit(c(5, 10, 10, 5), "mm")) + NULL
p3 = p3 + theme(axis.title.y = element_text(margin = margin(r = 40))) + theme(plot.margin = unit(c(5, 10, 10, 5), "mm")) + NULL

p4 = p4 + theme(axis.title.y = element_text(margin = margin(r = 40))) + theme(plot.margin = unit(c(5, 10, 10, 5), "mm")) + NULL
p5 = p5 + theme(axis.title.y = element_text(margin = margin(r = 40))) + theme(plot.margin = unit(c(5, 10, 10, 5), "mm")) + NULL
p6 = p6 + theme(axis.title.y = element_text(margin = margin(r = 40))) + theme(plot.margin = unit(c(5, 10, 10, 5), "mm")) + NULL

# print(p1)
# print(p2)
# print(p3)

# Arrange six plots (p1 to p6) in a 2x3 grid with labels
p = cowplot::plot_grid(
  p1, p2, p3, p4, p5, p6,
  labels = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)"),
  label_fontfamily = "Helvetica",
  label_size = 18,                                     # Set font size of labels
  nrow = 2, ncol = 3,                                  # Define layout as 2 rows, 3 columns
  label_x = c(-0.05, -0.05, -0.05),                    # Adjust x positions of labels
  label_y = c(1.08, 1.08, 1.08),                       # Adjust y positions of labels
  rel_widths = c(1.0, 1.0, 1.0),                       # Set relative widths of plots
  rel_heights = c(1, 1),                               # Set relative heights of plots
  align = "hv"                                         # Align horizontally and vertically
)

# Apply additional styling to the combined plot
p = p + theme(
  plot.margin = margin(10, 10, 5, 10, unit = "mm"),          # Adjust plot margins
  plot.background = element_rect(fill = "white", color = NA) # Set background color as white
)

# print(p)
ggsave(filename="./output/figure-for-paper/fig_s01.png", plot=p, dpi=600, w=13, h=7.5)
ggsave(filename="./output/figure-for-paper/fig_s01.pdf", plot=p, w=13, h=7.5)

# remove all objects
remove(list = ls())
