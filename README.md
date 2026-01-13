# mg-fgcms

This repository contains the data and code for the following paper:
> Otsuka, R., Kalema-Zikusoka, G., Yamakoshi, G., & Kinoshita, K. (2026). Tourism impacts on fecal glucocorticoid metabolites of free-ranging mountain gorillas in Bwindi Impenetrable National Park, Uganda. *bioRxiv*. [Preprint].

## Project Overview
This study investigates how tourism affects the stress levels (measured via fecal glucocorticoid metabolites, fGCMs) of mountain gorillas in Bwindi using Bayesian multilevel models.

## Repository Structure
```text
.
├── data/           # Data used in this study
├── models/         # Stan model files (.stan)
├── notebooks/      # R Markdown files
├── fig/            # R scripts for visualizations
├── renv.lock       # R environment lockfile
├── run_model_r1.R
├── run_model_r2.R
├── run_model_rm.R
└── setup_rethinking.R
```

## Requirements

| OS/Software  | Version       |
| :----------- | :------------ |
| Windows      | 11 (64 bit)   |
| R            | 4.1.3         |
| Stan         | 2.21.7        |

### Environment Setup
1. Clone this repository.
2. Open the project in RStudio.
3. Restore the environment using renv:
```R
renv::restore()
```
4. Install the rethinking package:
```R
source("setup_rethinking.R")
```
- [setup_rethinking.R](./setup_rethinking.R)  

## Data
All data files used in this paper are available in the [./data/](./data/) directory.

> [!NOTE]
> If you use this data in your research, please cite the original paper listed in the [Reference](#reference) section

## Usage
### Bayesian Statistical Modelling
Main models (Model-R1, R2, and RM) are defined in:
- [./models/multilevel_linear_model.stan]("./models/multilevel_linear_model.stan").   

To run the analysis, execute the following scripts:
```R
source("run_model_r1.R")
source("run_model_r2.R")
source("run_model_rm.R")
```

### Convergence diagnosis and model comparison
Analysis of MCMC chains, model diagnostics (e.g., R-hat), and model comparison (e.g., WAIC) can be found in:
- [./notebooks/40_models.Rmd]("./notebooks/40_models.Rmd").

### Supplementary Experiment S1-3
- Model: [./models/bayesian_paired_t_test.stan]("./models/bayesian_paired_t_test.stan") 
- Notebooks: [./notebooks/ex_s01.Rmd]("./notebooks/ex_s01.Rmd") to [./notebooks/ex_s03.Rmd]("./notebooks/ex_s04.Rmd")   

### Visualization
Scripts for generating figures are located in the [./fig/]("./fig/") directory.

## Author
**Ryoma Otsuka** | [Website](URL) | [GitHub](URL) | [Google Scholar](URL) |

## Reference

```
@article{otsuka2026fgcms,
  author    = {Otsuka, Ryoma and Kalema-Zikusoka, Gladys and Yamakoshi, Gen and Kinoshita, Kodzue},
  title     = {Tourism impacts on fecal glucocorticoid metabolites of free-ranging mountain gorillas in {Bwindi Impenetrable National Park}, {Uganda}},
  elocation-id = {2026.XX.XX.XXXXXX},
  year      = {2026},
  doi       = {10.1101/2026.XX.XX.XXXXXX},
  publisher = {Cold Spring Harbor Laboratory},
  journal   = {bioRxiv},
  URL       = {https://www.biorxiv.org/content/early/2026/01/06/2026.XX.XX.XXXXXX},
  note      = {Preprint}
}
```
