# Fig. S4

# source("../src/utils.R")
# setup(use_stan=FALSE, is_rmd=TRUE)
library(cowplot)
library(grid)

df_raw = read.csv("./data/fgcms.csv")

df = df_raw %>%
  dplyr::select(-c(Sample_ID)) %>%  # remove a colum of Date
  dplyr::mutate(FGCMs = FGCMs_ng_g_wet) %>%
  dplyr::mutate(log_FGCMs=log(FGCMs))  # add a new column (log transformed FGCMs concentration)
str(df)
length(df[,1])

# histogram
p1 = ggplot(df, aes(FGCMs)) + 
  geom_histogram(binwidth=100, alpha=0.9, col="transparent", fill="#56B4E9") +
  labs(x="fGCMs (ng/g wet)", y="Count") +
  NULL
print(p1)

# histogram without outliers
df1 = subset(df, FGCMs < 8000)
p2 = ggplot(df1, aes(FGCMs)) +
  geom_histogram(binwidth=20, alpha=0.9, col="transparent", fill="#56B4E9") +
  labs(x="fGCMs (ng/g wet)", y="Count") +
  NULL
print(p2)

# log_fGCM
p3 = ggplot(df, aes(log_FGCMs)) +
  geom_histogram(binwidth=0.1, alpha=0.9, col="transparent", fill="#56B4E9") +
  labs(x="log-fGCMs (ng/g wet)", y="Count") +
  NULL

# log_fGCM
p4 = ggplot(df1, aes(log_FGCMs)) +
  geom_histogram(binwidth=0.1, alpha=0.9, col="transparent", fill="#56B4E9") +
  labs(x="log-fGCMs (ng/g wet)", y="Count") +
  NULL

p1 = p1 + theme(axis.title.y = element_text(margin = margin(r = 30))) + theme(plot.margin = unit(c(5, 10, 10, 5), "mm")) + NULL
p2 = p2 + theme(axis.title.y = element_text(margin = margin(r = 30))) + theme(plot.margin = unit(c(5, 10, 10, 5), "mm")) + NULL
p3 = p3 + theme(axis.title.y = element_text(margin = margin(r = 30))) + theme(plot.margin = unit(c(5, 10, 10, 5), "mm")) + NULL
p4 = p4 + theme(axis.title.y = element_text(margin = margin(r = 30))) + theme(plot.margin = unit(c(5, 10, 10, 5), "mm")) + NULL

# Arrange four plots (p1, p2, p3, p4) in a 2x2 grid with labels
p = cowplot::plot_grid(
  p1, p2, p3, p4,
  labels = c("(a)", "(b)", "(c)", "(d)"),  
  label_fontfamily = "Helvetica",
  label_size = 18,                         # Set font size of labels
  nrow = 2, ncol = 2,                      # Define layout as 2 rows, 2 columns
  label_x = c(-0.03, -0.03, -0.03, -0.03), # Adjust x positions of labels
  label_y = c(1.03, 1.03, 1.03, 1.03),     # Adjust y positions of labels
  rel_widths = c(1, 1),                    # Set relative width of plots
  rel_heights = c(1, 1),                   # Set relative heights of plots
  align = "hv"                             # Align horizontally and vertically
)

# Apply additional styling to the combined plot
p = p + theme(
  plot.margin = margin(10, 10, 5, 10, unit = "mm"),           # Adjust plot margins
  plot.background = element_rect(fill = "white", color = NA)  # Set background color as white
)

print(p)
ggsave(filename="./output/figure-for-paper/fig_s04.png", plot=p, dpi=600, w=10, h=8)
ggsave(filename="./output/figure-for-paper/fig_s04.pdf", plot=p, w=10, h=8)
