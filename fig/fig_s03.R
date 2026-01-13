# Fig. S3
# Parallelism

source("./src/utils.R")
setup(use_stan=FALSE, is_rmd=FALSE)

df = read.csv(file = "./data/parallelism_data.csv")
print(df) 

p = plot_parallelism_test(df, y="Absorbance", ncol=5)
ggsave(filename="./output/figure-for-paper/fig_s03.png", plot=p, dpi=600, w=12, h=3)
ggsave(filename="./output/figure-for-paper/fig_s03.pdf", plot=p, w=12, h=3)