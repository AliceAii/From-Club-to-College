
# ---- 1. Normalize survey weights and compute IPTW --------------------------

data_trimmed.stemmajor <- data_trimmed.stemmajor %>%
  mutate(
    W3W1W2STU_norm = W3W1W2STU / mean(W3W1W2STU, na.rm = TRUE),
    D = ifelse(sciclubgroup == "Treatment", 1, 0),
    # ATE weights
    w.ate = (D * (1 / pscore)) + ((1 - D) * (1 / (1 - pscore))),
    # ATT weights
    w.att = D + ((1 - D) * (pscore / (1 - pscore))),
    # ATC weights
    w.atc = (D * ((1 - pscore) / pscore)) + (1 - D),
    # Combined IPTW × survey weights
    w.ate_W3W1W2STU = w.ate * W3W1W2STU_norm,
    w.att_W3W1W2STU = w.att * W3W1W2STU_norm,
    w.atc_W3W1W2STU = w.atc * W3W1W2STU_norm
  )

table(data_trimmed.stemmajor$sciclubgroup)

# ---- 2. Covariate balance check (ATE weights) ------------------------------

covars <- c(
  "x1sciid", "stemoccasp9th",
  "x1control", "x1freelunch", "a1stucolor", "x1ap",
  "a1mspdintrst", "a1msmentor", "c1scholarship", "c1pursue",
  "x1race", "x1sex", "x1sesq5", "x1txmscr", "x1sciint", "x1stuedexpct",
  "x1schoolbel", "x1schooleng",
  "s1alg1m09", "s1alg2m09", "s1tgeom09", "s1sfall09",
  "s1smuseum", "s1sbooks", "s1webinfo"
)

bal_ate.stemmajor <- bal.tab(
  x         = data_trimmed.stemmajor[, covars],
  treat     = data_trimmed.stemmajor$D,
  weights   = data_trimmed.stemmajor$w.ate_W3W1W2STU,
  estimand  = "ATE",
  s.d.denom = "pooled",
  m.threshold = 0.10,
  un        = TRUE
)
bal_ate.stemmajor

# Love plot
love.plot(
  bal_ate.stemmajor,
  stats      = "mean.diffs",
  abs        = FALSE,
  binary     = "std",
  thresholds = c(m = 0.10),
  colors     = c("#ee84a8", "#71bced"),
  title      = "Covariate Balance after IPTW (ATE) for STEM Major"
) + theme_bw()

# ---- 3. Post-weighting descriptive statistics (Table 1 weighted) -----------

covars.stemmajor <- c(covars, "collegeSTEMmajorasp")

design.stemmajor <- svydesign(ids = ~1,
                               weights = ~w.ate_W3W1W2STU,
                               data = data_trimmed.stemmajor)

table_weighted.stemmajor <- svyCreateTableOne(
  vars   = covars.stemmajor,
  strata = "sciclubgroup",
  data   = design.stemmajor
)
print(table_weighted.stemmajor)
