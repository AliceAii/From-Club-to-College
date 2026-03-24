# From Club to College: The Role of High School Science Clubs in Shaping Students' STEM Major Aspirations

**Authors:** Shuhan (Alice) Ai & Kevin Eagan

## Overview

This repository contains the R code used in the paper *"From Club to College: The Role of High School Science Clubs in Shaping Students' STEM Major Aspirations"*, published in *Research in Higher Education*.

Using data from the High School Longitudinal Study of 2009 (HSLS:09), this study examines whether participation in science clubs/groups between ninth and 11th grade is associated with students' intentions to pursue a STEM major in college. The study employs a quasi-experimental design with propensity score–based inverse probability of treatment weighting (IPTW) and doubly robust estimation.

## Research Questions

1. What individual and contextual factors are associated with students' likelihood of participating in science clubs/groups during high school?
2. Do students who participated in science clubs/groups between ninth and 11th grade demonstrate a greater likelihood of intending to pursue a STEM major in college compared to peers with similar demographic and academic backgrounds who did not participate?

## Data

This study uses restricted-use data from the [High School Longitudinal Study of 2009 (HSLS:09)](https://nces.ed.gov/surveys/hsls09/), sponsored by the National Center for Education Statistics (NCES). The data are not included in this repository due to licensing restrictions. Researchers can apply for access through NCES.

The analytic sample includes **9,720 students** from **800 high schools** who had complete data on key variables and non-zero longitudinal sample weights (W3W1W2STU).

## Repository Structure

```
├── README.md
├── 00_data_preparation.R      # Read raw HSLS:09 files, recode/clean variables, impute missing data (MICE)
├── 01_setup.R                  # Load packages, read imputed .rds files, apply effect coding, helper functions
├── 02_descriptive_analysis.R   # Descriptive statistics (Table 1), correlations, naive estimates
├── 03_propensity_score.R       # Propensity score estimation, common support, trimming (Table 2, Figure 1)
├── 04_iptw_balance.R           # IPTW construction, covariate balance checks (Love plot)
├── 05_outcome_models.R         # Doubly robust ATE/ATT/ATC logistic regression (Tables 3–4, Appendices)
└── 06_sensitivity_analysis.R   # Sensitivity analysis via sensemakr (robustness values)

```


## Analytic Approach

| Step | Script | Description |
|------|--------|-------------|
| **Data Preparation** | `00_data_preparation.R` | Read raw HSLS:09 SPSS/Stata files; select and recode all analytic variables (treatment, outcomes, covariates); check missingness; impute missing values via MICE with stochastic single imputation; export `.rds` analytic files |
| **Setup** | `01_setup.R` | Load R packages, read imputed `.rds` files, apply effect coding for race, create factor and dummy variables, define helper functions for coefficient tables |
| **Descriptive Analysis** | `02_descriptive_analysis.R` | Compare participant vs. non-participant groups on all baseline covariates (Table 1); estimate naive (unweighted) logistic regression and multilevel models |
| **Propensity Score Estimation** | `03_propensity_score.R` | Fit logistic regression to estimate propensity scores (Table 2); check VIF for multicollinearity; visualize common support on the logit scale (Figure 1); trim extreme propensity scores |
| **IPTW & Balance** | `04_iptw_balance.R` | Compute ATE, ATT, and ATC inverse probability weights; combine with normalized survey weights; assess balance using standardized mean differences and Love plots |
| **Outcome Models** | `05_outcome_models.R` | Estimate nested logistic regression models (unconditional → background → competence/perception → full doubly robust); report ATE, ATT, ATC estimates (Table 4); compare model fit via AIC/BIC |
| **Sensitivity Analysis** | `06_sensitivity_analysis.R` | Re-estimate outcomes as linear probability models; compute robustness values (RV) using `sensemakr` to quantify how strong an unobserved confounder must be to nullify the treatment effect |

## Key Findings

- Science club participants were approximately **40% more likely** to intend to pursue a STEM major in college (OR = 1.386, *p* < 0.001) compared to non-participants with similar backgrounds.
- The effect remained robust across multiple model specifications (naive, weighted, doubly robust).
- Sensitivity analysis indicated that an unobserved confounder would need to explain at least **6.38%** of the residual variance in both treatment and outcome to reduce the ATE to zero.

## Software & Packages

All analyses were conducted in **R**. Key packages include:

- **Propensity score & weighting:** `WeightIt`, `cobalt`, `MatchIt`
- **Survey design:** `survey`
- **Modeling:** `lme4`, `lmerTest`, `WeMix`, `marginaleffects`
- **Sensitivity analysis:** `sensemakr`
- **Robust standard errors:** `sandwich`, `lmtest`, `car`
- **Data import & wrangling:** `haven`, `tidyverse`, `ggplot2`, `patchwork`
- **Tables:** `table1`, `tableone`, `flextable`, `knitr`
- **Missing data:** `mice`, `naniar`

## Citation

> Ai, S., & Eagan, K. (2026). From club to college: The role of high school science clubs in shaping students' STEM major aspirations. *Research in Higher Education*.

## License

This repository is provided for academic reproducibility purposes. The code is available under the [MIT License](https://opensource.org/licenses/MIT). The HSLS:09 data are subject to NCES restricted-use data license agreements.
