# Fig. 7

#### Setup ####
source("./src/utils.R")
setup(use_stan=FALSE, is_rmd=FALSE)
library(cowplot)

COLOR = "#1E88E5"
COLOR_BOX = COLOR
COLOR_VIOLIN = "#555555"

common_theme <- theme(
  text = element_text(family = "Helvetica"),
  axis.title = element_text(size = 16),
  axis.text = element_text(size = 14)
)

##### Data ####
path = "./data/fgcms.csv"
df = load_and_preprocess_data(path, group_list=c("R", "M"))

group_names = c(
  "M" = "Mubare",
  "R" = "Rushegura"
)

# Sex
table(df$Group, df$Sex)
set.seed(558)
p = ggplot(data=df, aes(x=Sex , y=log_FGCMs, color=Group)) +
  geom_violin(col=NA, fill=COLOR_VIOLIN, alpha=0.2, width=0.5, scale="width", position=position_nudge(x=-0.0)) + 
  geom_boxplot(outlier.colour=NA, size=0.6, width=0.1, position=position_nudge(x=0.2)) +
  geom_jitter(width=0.1, size=1.5, alpha=0.6, shape=21) +
  scale_color_manual(values=c("#009E73", "#1E88E5")) +
  coord_cartesian(ylim=c(3.5, 7.5)) +
  facet_grid(~ Group + Age_Category) +
  labs(x='', y="log-fGCMs (ng/g wet)") +
  theme(panel.spacing.x = unit(2,"line")) +
  theme(
    plot.margin = unit(c(10, 10, 5, 10), "mm"), # Adjust plot margins
    plot.background = element_rect(fill = "white", color = NA),  # Set background color as white
    legend.position = "none"
  ) +
  theme(panel.grid.major.x = element_blank()) +
  common_theme +
  NULL

print(p)
ggsave(filename="./output/figure-for-paper/fig_07.png", plot=p, dpi=600, w=12, h=5)
ggsave(filename="./output/figure-for-paper/fig_07.pdf", plot=p, w=12, h=5)

# remove all objects
remove(list = ls())
