# Fig. 2
# Post defecation changes in FGCMs concentrations

#### Setup ####
source("./src/utils.R")
setup(use_stan=FALSE, is_rmd=FALSE)
library(cowplot)

#### Data ####
df = read.csv("./data/post_defecation.csv")

#### Line plot ####
p1 = ggplot(data = df) +
  geom_line(aes(x=Time, y=Value), col="#1E88E5", linewidth=1.0) +
  geom_point(aes(x=Time, y=Value), col="#1E88E5", size=3.0) +
  facet_wrap( ~ ID, ncol=5) +
  scale_x_continuous(breaks = seq(from=0, to=14, by=2)) +
  scale_y_continuous(breaks = seq(from=0, to=1600, by=400)) +
  coord_cartesian(xlim=c(1.5, 14.5), ylim=c(0, 1500)) +
  labs(x="Time after defecation (h)", y="fGCMs (ng/g wet)") +
  theme(panel.spacing.x = unit(2,"line")) +
  theme(panel.spacing.y = unit(2,"line")) +
  theme(panel.grid.minor.x = element_blank()) +
  theme(panel.grid.minor.y = element_blank()) +
  theme(text = element_text(family = "Arial")) +
  theme(strip.text = element_text(family = "Arial")) +
  NULL

df$log_Value = log(df$Value)
p2 = ggplot(data = df) +
  geom_line(aes(x=Time, y=log_Value), col="#1E88E5", linewidth=1.0) +
  geom_point(aes(x=Time, y=log_Value), col="#1E88E5", size=3.0) +
  facet_wrap( ~ ID, ncol=5) +
  scale_x_continuous(breaks = seq(from=0, to=14, by=2)) +
  scale_y_continuous(breaks = seq(from=0, to=12, by=1)) +
  coord_cartesian(xlim=c(1.5, 14.5), ylim=c(3.5, 9.5)) +
  labs(x="Time after defecation (h)", y="log-fGCMs (ng/g wet)") +
  theme(panel.spacing.x = unit(2,"line")) +
  theme(panel.spacing.y = unit(2,"line")) +
  theme(panel.grid.minor.x = element_blank()) +
  theme(panel.grid.minor.y = element_blank()) +
  theme(text = element_text(family = "Arial")) +
  theme(strip.text = element_text(family = "Arial")) +
  NULL

p1 = p1 + theme(axis.title.y = element_text(margin = margin(r = 30))) + theme(plot.margin = unit(c(5, 10, 5, 5), "mm")) + NULL
p2 = p2 + theme(axis.title.y = element_text(margin = margin(r = 30))) + theme(plot.margin = unit(c(5, 10, 5, 5), "mm")) + NULL

# Arrange two plots (p1 and p2) in a single column with labels
p = cowplot::plot_grid(
  p1, p2,
  labels = c("(a)", "(b)"),
  label_fontfamily = "Arial",
  label_size = 18,                  # Set font size of labels
  nrow = 2, ncol = 1,               # Define layout as 2 rows, 1 column
  label_x = c(-0.03),               # Adjust x position of labels
  label_y = c(1.05, 1.05),          # Adjust y positions of labels
  rel_widths = c(1),                # Set relative width of plots
  rel_heights = c(1, 1),            # Set relative heights of plots
  align = "hv"                      # Align horizontally and vertically
)

# Apply additional styling to the combined plot
p = p + theme(
  plot.margin = margin(10, 10, 5, 10, unit = "mm"),          # Adjust plot margins
  plot.background = element_rect(fill = "white", color = NA) # Set background color as white
)

ggsave(filename="./output/figure-for-paper/fig_s02.png", plot=p, dpi=600, w=14, h=10)
ggsave(filename="./output/figure-for-paper/fig_s02.svg", plot=p, w=14, h=10)

# remove all objects
remove(list = ls())
