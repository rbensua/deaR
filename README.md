
# deaR: Data Envelopment Analysis in R

[![CRAN Status](https://www.r-pkg.org/badges/version/deaR)](https://CRAN.R-project.org/package=deaR)

## Overview

`deaR` is an R package designed for conducting Data Envelopment Analysis (DEA). It offers a comprehensive suite of tools for both conventional and fuzzy DEA models, enabling users to assess the efficiency of Decision Making Units (DMUs) across various sectors, including economics, finance, healthcare, and public administration. The package supports:

- **Conventional DEA models**: Radial, non-radial, additive, Slack-Based Measure (SBM), etc.
- **Fuzzy DEA models**: Addressing uncertainty in data using models like Kao-Liu, Guo-Tanaka, and possibilistic approaches.
- **Advanced analyses**: Super-efficiency models, cross-efficiency analysis, Malmquist productivity index, bootstrapping methods, and metafrontier analysis.

## Features

- **Conventional DEA Models**: Implement standard models such as CCR (Charnes, Cooper, and Rhodes) and BCC (Banker, Charnes, and Cooper) with customizable returns to scale and orientations.
- **Fuzzy DEA Models**: Incorporate uncertainty in data using:
  - **Kao-Liu model**: Handles fuzzy inputs and outputs using α-cuts.
  - **Guo-Tanaka model**: Considers fuzziness in both input and output data.
  - **Possibilistic model**: Utilizes possibility distributions to manage data uncertainty.
- **Super-Efficiency Models**: Rank efficient DMUs beyond the efficiency frontier.
- **Cross-Efficiency Analysis**: Evaluate DMUs from multiple perspectives.
- **Malmquist Productivity Index**: Measure productivity changes over time.
- **Bootstrapping Methods**: Estimate confidence intervals for efficiency scores.
- **Metafrontier Analysis**: Compare efficiency across different groups.
- **Customization**: Handle non-controllable, non-discretionary, and undesirable variables.
- **Visualization**: Provide novel graphical representations for visualizing results.

## Installation

Install the stable version from CRAN:

```r
install.packages("deaR")
```

Or install the development version from GitHub:

```r
# install.packages("devtools")
devtools::install_github("your-github-username/deaR")
```

## Basic Usage

### Load the Package

```r
library(deaR)
```

### Import a Dataset

`deaR` provides built-in datasets for testing. For example:

```r
data("Fortune500")
head(Fortune500)
```

### Define DEA Data

```r
data_dea <- make_deadata(Fortune500, ni = 3, no = 2)
```

### Run a Basic DEA Model (CCR Model)

```r
model <- model_basic(data_dea, rts = "crs", orientation = "io")
summary(model)
```

### Extract Efficiency Scores

```r
efficiencies(model)
```

## Fuzzy DEA Models

`deaR` supports several fuzzy DEA models to handle imprecision and uncertainty in data.

### Kao-Liu Model

```r
data("Leon2003")
data_fuzzy <- make_deadata_fuzzy(Leon2003,
                                 inputs.mL = 2,
                                 inputs.dL = 3,
                                 outputs.mL = 4,
                                 outputs.dL = 5)
result_kaoliu <- modelfuzzy_kaoliu(data_fuzzy,
                                   kaoliu_modelname = "basic",
                                   alpha = seq(0, 1, by = 0.1),
                                   orientation = "io",
                                   rts = "vrs")
efficiencies(result_kaoliu)
```

### Possibilistic Model

```r
data("Leon2003")
data_fuzzy <- make_deadata_fuzzy(Leon2003,
                                 inputs.mL = 2,
                                 inputs.dL = 3,
                                 outputs.mL = 4,
                                 outputs.dL = 5)
result_poss <- modelfuzzy_possibilistic(data_fuzzy,
                                        h = seq(0, 1, by = 0.1),
                                        orientation = "io",
                                        rts = "vrs")
efficiencies(result_poss)
```

## Advanced Features

- **Super-Efficiency Models**: Rank efficient DMUs beyond the efficiency frontier.
- **Cross-Efficiency Analysis**: Evaluate DMUs from multiple perspectives.
- **Bootstrapping Methods**: Estimate confidence intervals for efficiency scores.
- **Malmquist Productivity Index**: Measure productivity changes over time.
- **Metafrontier Analysis**: Compare efficiency across different groups.

## Contributions

Contributions are welcome! Please feel free to open an issue or submit a pull request.

## Citation

If you use `deaR` in your research, please cite:

> Coll-Serrano, V., Bolós, V. J., & Benítez Suárez, R. (2022). deaR: Conventional and Fuzzy Data Envelopment Analysis. R package version 1.3.2. [CRAN](https://CRAN.R-project.org/package=deaR)

## License

This package is licensed under the MIT License.
