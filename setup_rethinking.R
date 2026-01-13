# setup_rethinking.R

# install function
install_if_missing <- function(pkg, install_cmd) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    eval(parse(text = install_cmd))
  }
}

# Run
install_if_missing("remotes", 'install.packages("remotes")')
install_if_missing("cmdstanr", 'install.packages("cmdstanr")')
install_if_missing("rethinking", 'remotes::install_github("rmcelreath/rethinking", INSTALL_opts = "--no-multiarch")')

# load
library(remotes)
library(cmdstanr)
library(rethinking)