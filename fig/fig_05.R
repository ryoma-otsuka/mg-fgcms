# Fig. 5

#### Setup ####
source("./src/utils.R")
setup(use_stan=FALSE, is_rmd=FALSE)
library(cowplot)

#### Data ####
path = "./data/fgcms.csv"
df = load_and_preprocess_data(path, group_list=c("R"))

#### p1, p2, p3 ####
COLOR = "#1E88E5"
COLOR_BOX = COLOR
COLOR_VIOLIN = "#555555"
set.seed(558)

common_theme <- theme(
  text = element_text(family = "Helvetica"),
  axis.title = element_text(size = 16),
  axis.text = element_text(size = 14)
)

# Sex
p1 = ggplot(data=df, mapping=aes(x=Sex , y=log_FGCMs, colour=Sex)) +
  geom_violin(col=NA, fill=COLOR_VIOLIN, alpha=0.2, width=0.5, scale="width", position=position_nudge(x=-0.0)) + 
  geom_boxplot(col=COLOR_BOX, outlier.colour=NA, size=0.6, width=0.1, position=position_nudge(x=0.2)) +
  geom_jitter(width=0.1, size=1.5, alpha=0.6, col=COLOR, shape=21) +
  coord_cartesian(ylim=c(3.5, 7.5)) +
  labs(x="Sex", y="log-fGCMs (ng/g wet)") +
  theme(panel.grid.major.x = element_blank()) + 
  common_theme +
  NULL

# Age Category
p2 = ggplot(data=df, aes(x=Age_Category , y=log_FGCMs, colour=Age_Category)) +
  geom_violin(col=NA, fill=COLOR_VIOLIN, alpha=0.2, width=0.5, scale="width", position=position_nudge(x=-0.0)) + 
  geom_boxplot(col=COLOR_BOX, outlier.colour=NA, size=0.6, width=0.1, position=position_nudge(x=0.2)) +
  geom_jitter(width=0.1, size=1.5, alpha=0.6, col=COLOR, shape=21) +
  coord_cartesian(ylim=c(3.5, 7.5)) +
  labs(x='Age Category', y="log-fGCMs (ng/g wet)") +
  theme(panel.grid.major.x = element_blank()) + 
  common_theme +
  NULL

# Mean temperature
p3 = ggplot(data=df, aes(x=Mean_temp , y=log_FGCMs)) +
  geom_point(shape=21, size=1.5, col=COLOR) +
  labs(x="Mean temperature", y="log-fGCMs (ng/g wet)") +
  coord_cartesian(xlim=c(16, 22), ylim=c(3.5, 7.5)) +
  theme(legend.position="none") +
  theme(panel.grid.major.x = element_line(colour = "grey90")) +
  common_theme +
  NULL

p1 = p1 + theme(axis.title.y = element_text(margin = margin(r = 30))) + theme(plot.margin = unit(c(5, 10, 5, 5), "mm")) + NULL
p2 = p2 + theme(axis.title.y = element_text(margin = margin(r = 30))) + theme(plot.margin = unit(c(5, 10, 5, 5), "mm")) + NULL
p3 = p3 + theme(axis.title.y = element_text(margin = margin(r = 30))) + theme(plot.margin = unit(c(5, 10, 5, 5), "mm")) + NULL

#### p4, p5 | Tourist number ####

# get mcmc samples
fit_R01 = readRDS(file = './output/model-output/lmm_R01.RData') # load model output
ms = rstan::extract(fit_R01)

# New data for plot
X_new = min(df$Tourist_number_2d):max(df$Tourist_number_2d) # 6:21
# print(X_new)
N_X = length(X_new)
N_mcmc = length(ms$lp__)

set.seed(1234)
mu_new_mcmc = as.data.frame(matrix(nrow=N_mcmc, ncol=N_X))
# print(mu_new_mcmc)
y_new_mcmc = as.data.frame(matrix(nrow=N_mcmc, ncol=N_X))
mean_3 = mean(df$Mean_temp)
sd_3 = sd(df$Mean_temp)
sd_4 = sd(df$Tourist_number_2d)
for (i in 1:N_X) {
  mu_new_mcmc[,i] = median(ms$b[,1]) * 0 + median(ms$b[,2]) * 0 + median(ms$b[,3]) / sd_3 * mean_3 + ms$b[,4] / sd_4 * X_new[i] + ms$a # regression
  y_new_mcmc[,i] = rnorm(n=N_mcmc, mean=mu_new_mcmc[,i], sd=ms$sigma_Y) # prediction
}

# Regression dataframe
probs = c(0.015, 0.055, 0.5, 0.945, 0.985)
qua_est = apply(mu_new_mcmc, 2, quantile, prob=probs)
df_est = data.frame(X_new=X_new, t(qua_est))
colnames(df_est) = c('X_new', paste0('p', probs*100))
# head(df_est)

# Prediction dataframe
probs = c(0.015, 0.055, 0.5, 0.945, 0.985)
qua_pred = apply(y_new_mcmc, 2, quantile, prob=probs)
df_pred = data.frame(X_new=X_new, t(qua_pred))
colnames(df_pred) = c('X_new', paste0('p', probs*100))
# head(df_pred)

COLOR = "#1E88E5"
COLOR_MED_LINE = COLOR
# COLOR_MED_LINE = "#333333"

# base plot
p = ggplot() +
  geom_point(data=df, aes(x=Tourist_number_2d, y=log_FGCMs),shape=21, size=1.5, col=COLOR) +
  scale_x_continuous(breaks=seq(from=0, to=30, by=5)) +
  coord_cartesian(xlim=c(4.0, 23), ylim=c(3.5, 7.5)) +
  labs(x="Number of tourists", y="log-fGCMs (ng/g wet)") +
  theme(legend.position="none") +
  theme(panel.grid.major.x = element_line(colour = "grey90")) +
  common_theme +
  NULL

# Regression
# Median + 89% & 97% Bayesian Credible Interval
p_est = p +
  geom_ribbon(data=df_est, mapping=aes(x=X_new, min=p1.5, max=p98.5), alpha=1/9, fill=COLOR) +   # 97% BPI
  geom_ribbon(data=df_est, mapping=aes(x=X_new, min=p5.5, max=p94.5), alpha=2/9, fill=COLOR) +   # 89% BPI
  geom_line(data=df_est, mapping=aes(x=X_new, y=p50), col=COLOR_MED_LINE, size=1.2) + # Median
  NULL

# Prediction
# Median + 89% & 97% Bayesian Prediction Interval
p_pred = p +
  geom_ribbon(data=df_pred, mapping=aes(x=X_new, min=p1.5, max=p98.5), alpha=1/9, fill=COLOR) +  # 97% BPI
  geom_ribbon(data=df_pred, mapping=aes(x=X_new, min=p5.5, max=p94.5), alpha=2/9, fill=COLOR) +  # 89% BPI
  geom_line(data=df_pred, mapping=aes(x=X_new, y=p50), col=COLOR_MED_LINE, size=1.2) + # Median
  NULL

p_est = p_est + theme(axis.title.y = element_text(margin = margin(r = 30))) + theme(plot.margin = unit(c(5, 10, 5, 5), "mm")) + NULL
p_pred = p_pred + theme(axis.title.y = element_text(margin = margin(r = 30))) + theme(plot.margin = unit(c(5, 10, 5, 5), "mm")) + NULL
p4 = p_est
p5 = p_pred
# print(p4)
# print(p5)

#### p6 | Tourist visit ####
# Tourist visit
COLOR = "#1E88E5"
COLOR_BOX = COLOR
COLOR_VIOLIN = "#555555"

# Tourist visit Log
set.seed(1234)
df$Tourist_visit_2d = factor(df$Tourist_visit_2d, levels=c("1", "2", "3"))
p = ggplot(data=df, aes(x=Tourist_visit_2d , y=log_FGCMs, colour=Tourist_visit_2d)) +
  geom_violin(col=NA, fill=COLOR_VIOLIN, alpha=0.2, width=0.5, scale="width", position=position_nudge(x=-0.0)) + 
  geom_boxplot(col=COLOR_BOX, outlier.colour=NA, size=0.6, width=0.1, position=position_nudge(x=0.2)) +
  geom_jitter(width=0.1, size=1.5, alpha=0.6, col=COLOR, shape=21) +
  coord_cartesian(ylim=c(3.5, 7.5)) +
  labs(x="Number of tourist visits", y="log-fGCMs (ng/g wet)") +
  theme(legend.position="none") +
  theme(
    plot.margin = unit(c(10, 10, 5, 10), "mm"), # Adjust plot margins
    plot.background = element_rect(fill = "white", color = NA)  # Set background color as white
  ) + 
  theme(panel.grid.major.x = element_blank()) +
  common_theme +
  NULL

p = p + theme(
  plot.margin = unit(c(10, 10, 5, 10), "mm"), # Adjust plot margins
  plot.background = element_rect(fill = "white", color = NA)  # Set background color as white
)
p6 = p
# print(p6)

#### Fig. 05 ####
# Arrange multiple plots (p1 to p6) in a grid with labels
p = cowplot::plot_grid(
  p1, p2, p3, p4, p5, p6,
  labels = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)"),
  label_fontfamily = "Helvetica",
  label_x = c(-0.03, -0.03, -0.03),                   # Adjust x positions of labels
  label_y = c(1.03, 1.03, 1.03),                      # Adjust y positions of labels
  label_size = 18,                                    # Set font size of labels
  ncol = 3, nrow = 2,                                 # Define layout as 2 rows, 3 columns
  align = "hv",                                       # Align horizontally and vertically
  rel_widths = c(1.0, 1.0, 1.0),                      # Set relative widths of plots
  rel_heights = c(1.0, 1.0)                           # Set relative heights of plots
)

# Apply additional styling to the combined plot
p = p + theme(
  plot.margin = margin(10, 10, 5, 10, unit = "mm"),          # Adjust plot margins
  plot.background = element_rect(fill = "white", color = NA) # Set background color as white
)

print(p)
ggsave(filename="./output/figure-for-paper/fig_05.png", plot=p, dpi=600, w=15, h=9)
ggsave(filename="./output/figure-for-paper/fig_05.pdf", plot=p, w=15, h=9)

# remove all objects
remove(list = ls())

