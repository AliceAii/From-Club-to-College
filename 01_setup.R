
# ---- 1. Load packages -------------------------------------------------------

library(tidyverse)
library(skimr)
library(flextable)
library(table1)
library(tableone)
library(knitr)
library(MatchIt)
library(WeightIt)
library(cobalt)          # love plots for balance diagnostics
library(marginaleffects)
library(sensemakr)       # sensitivity analysis
library(patchwork)
library(car)
library(naniar)
library(mice)
library(haven)
library(lm.beta)         # standardized beta coefficients
library(lme4)            # mixed-effects / multilevel models
library(lmtest)
library(survey)          # survey weights & robust SEs
library(broom)           # tidy model output
library(merTools)
library(lmerTest)
library(WeMix)           # MLM with survey weights
library(sandwich)
library(corrr)

# ---- 2. Read data -----------------------------------------------------------

# NOTE: Update these paths to match your local data directory.
data.sciid      <- readRDS("data/hslssicclub_datasciid2.rds")
data.stemmajor  <- readRDS("data/hslssicclub_datastemmajor2.rds")

# ---- 3. Effect coding for race ----------------------------------------------

contrasts(data.sciid$x1race)      <- contr.sum(7)
contrasts(data.stemmajor$x1race)  <- contr.sum(7)

# ---- 4. Create categorical versions of informal science vars ----------------
# 1 = Never, 2 = Rarely, 3 = Sometimes, 4 = Often

data.stemmajor <- data.stemmajor %>%
  mutate(
    fs1smuseum = factor(s1smuseum, levels = 1:4,
                        labels = c("Never", "Rarely", "Sometimes", "Often")),
    fs1sbooks  = factor(s1sbooks,  levels = 1:4,
                        labels = c("Never", "Rarely", "Sometimes", "Often")),
    fs1webinfo = factor(s1webinfo, levels = 1:4,
                        labels = c("Never", "Rarely", "Sometimes", "Often"))
  )

# ---- 5. Create numeric versions of key variables ----------------------------

data.stemmajor$sciclubgroup.num <- ifelse(
  data.stemmajor$sciclubgroup == "Treatment", 1,
  ifelse(data.stemmajor$sciclubgroup == "Control", 0, NA)
)

data.stemmajor$collegeSTEMmajorasp.num <- ifelse(
  data.stemmajor$collegeSTEMmajorasp == "Yes", 1,
  ifelse(data.stemmajor$collegeSTEMmajorasp == "No", 0, NA)
)

# ---- 6. Helper: create dummy variables for correlation analysis -------------

create_dummy_variables <- function(data, cat_vars) {
  data_with_dummies <- data
  for (var in cat_vars) {
    if (var %in% names(data)) {
      levels <- sort(unique(data[[var]][!is.na(data[[var]])]))
      for (level in levels) {
        dummy_name <- paste0(var, "_", level)
        data_with_dummies[[dummy_name]] <- ifelse(data[[var]] == level, 1, 0)
        data_with_dummies[[dummy_name]][is.na(data[[var]])] <- NA
      }
    }
  }
  return(data_with_dummies)
}

categorical_vars <- c("x1freelunch", "a1stucolor", "x1ap",
                       "x1race", "x1sesq5", "x1stuedexpct")

data.stemmajor <- create_dummy_variables(data.stemmajor, categorical_vars)

# ---- 7. Helper functions: coefficient tables --------------------------------

# Linear model coefficient table
tidy.coeftable <- function(model, digits = 3) {
  model_summary <- summary(model)
  b    <- model_summary$coefficients[, "Estimate"]
  se   <- model_summary$coefficients[, "Std. Error"]
  pval <- model_summary$coefficients[, "Pr(>|t|)"]
  beta <- lm.beta(model)$standardized.coefficients

  model_data <- model$model
  y_var      <- names(model_data)[1]
  y_numeric  <- if (is.factor(model_data[[y_var]])) {
    as.numeric(model_data[[y_var]]) - 1
  } else {
    model_data[[y_var]]
  }

  corr <- rep(NA, length(b)); names(corr) <- names(b)
  for (i in 2:length(b)) {
    var_name <- names(b)[i]
    if (var_name %in% names(model_data)) {
      corr[i] <- cor(model_data[[var_name]], y_numeric, use = "complete.obs")
    } else {
      for (col_name in names(model_data)) {
        if (startsWith(var_name, col_name) && is.factor(model_data[[col_name]])) {
          corr[i] <- cor(as.numeric(model_data[[col_name]]), y_numeric,
                         use = "complete.obs")
          break
        }
      }
    }
  }

  get_stars <- function(p) {
    if (is.na(p)) return("")
    else if (p < 0.001) return("***")
    else if (p < 0.01)  return("**")
    else if (p < 0.05)  return("*")
    else if (p < 0.1)   return(" ")
    else return("")
  }

  data.frame(
    Variable    = names(b),
    Correlation = round(corr, digits),
    Coefficient = round(b, digits),
    SE          = round(se, digits),
    Beta        = round(beta, digits),
    p_value     = round(pval, digits),
    Sig         = sapply(pval, get_stars),
    row.names   = NULL
  )
}

# Logistic model coefficient table
tidy.logit.table <- function(model, digits = 3) {
  model_summary <- summary(model)
  b    <- model_summary$coefficients[, "Estimate"]
  se   <- model_summary$coefficients[, "Std. Error"]
  pval <- model_summary$coefficients[, "Pr(>|z|)"]
  OR   <- exp(b)

  model_data <- model$model
  y_var      <- names(model_data)[1]
  y_numeric  <- if (is.factor(model_data[[y_var]])) {
    as.numeric(model_data[[y_var]])
  } else {
    model_data[[y_var]]
  }

  corr <- rep(NA, length(b)); names(corr) <- names(b)
  for (i in 2:length(b)) {
    var_name <- names(b)[i]
    if (var_name %in% names(model_data)) {
      corr[i] <- cor(model_data[[var_name]], y_numeric, use = "complete.obs")
    } else {
      for (col_name in names(model_data)) {
        if (startsWith(var_name, col_name) && is.factor(model_data[[col_name]])) {
          corr[i] <- cor(as.numeric(model_data[[col_name]]), y_numeric,
                         use = "complete.obs")
          break
        }
      }
    }
  }

  get_stars <- function(p) {
    if (is.na(p)) return("")
    else if (p < 0.001) return("***")
    else if (p < 0.01)  return("**")
    else if (p < 0.05)  return("*")
    else if (p < 0.1)   return(" ")
    else return("")
  }

  data.frame(
    Variable    = names(b),
    Correlation = round(corr, digits),
    Coefficient = round(b, digits),
    SE          = round(se, digits),
    OR          = round(OR, digits),
    p_value     = round(pval, digits),
    Sig         = sapply(pval, get_stars)
  )
}

cat("Setup complete.\n")
cat("  Science identity sample: n =", nrow(data.sciid), "\n")
cat("  STEM major sample:       n =", nrow(data.stemmajor), "\n")
