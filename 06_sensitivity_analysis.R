
# ---- 0. Prepare binary outcome as numeric -----------------------------------

data_trimmed.stemmajor$collegeSTEMmajorasp.num <- ifelse(
  data_trimmed.stemmajor$collegeSTEMmajorasp == "Yes", 1,
  ifelse(data_trimmed.stemmajor$collegeSTEMmajorasp == "No", 0, NA)
)

# ---- 1. DR model with IPTW × survey weights (main specification, M6) -------

lpm_ate_stemmajor <- lm(
  collegeSTEMmajorasp.num ~
    stemoccasp9th +
    sciclubgroup +
    x1control + x1freelunch + a1stucolor + x1ap +
    a1mspdintrst + a1msmentor + c1scholarship + c1pursue +
    x1race + x1sex + x1sesq5 +
    x1txmscr + x1sciint + x1stuedexpct +
    x1schoolbel + x1schooleng +
    s1alg1m09 + s1alg2m09 + s1tgeom09 + s1sfall09 +
    s1smuseum + s1sbooks + s1webinfo,
  data = data_trimmed.stemmajor,
  weights = w.atc_W3W1W2STU
)

sen_m6 <- sensemakr(
  model                  = lpm_ate_stemmajor,
  treatment              = "sciclubgroupTreatment",
  benchmark_covariates   = "stemoccasp9thSTEM",
  kd    = 0.25:0.75,
  ky    = 0.25:0.75,
  q     = 1,
  alpha = 0.05,
  reduce = TRUE
)
summary(sen_m6)
plot(sen_m6)

# ---- 2. Unconditional model with IPTW × survey weights ---------------------

lpm_ate_stemmajor0 <- lm(
  collegeSTEMmajorasp.num ~
    stemoccasp9th +
    sciclubgroup,
  data = data_trimmed.stemmajor,
  weights = w.atc_W3W1W2STU
)

sen_m4 <- sensemakr(
  model                  = lpm_ate_stemmajor0,
  treatment              = "sciclubgroupTreatment",
  benchmark_covariates   = "stemoccasp9thSTEM",
  kd    = 0.25:0.75,
  ky    = 0.25:0.75,
  q     = 1,
  alpha = 0.05,
  reduce = TRUE
)
summary(sen_m4)
plot(sen_m4)

# ---- 3. DR model without any weights (unweighted, M2) ----------------------

lpm_ate_stemmajor_unw <- lm(
  collegeSTEMmajorasp.num ~
    stemoccasp9th +
    sciclubgroup +
    x1control + x1freelunch + a1stucolor + x1ap +
    a1mspdintrst + a1msmentor + c1scholarship + c1pursue +
    x1race + x1sex + x1sesq5 +
    x1txmscr + x1sciint + x1stuedexpct +
    x1schoolbel + x1schooleng +
    s1alg1m09 + s1alg2m09 + s1tgeom09 + s1sfall09 +
    s1smuseum + s1sbooks + s1webinfo,
  data = data_trimmed.stemmajor
)

sen_m2 <- sensemakr(
  model                  = lpm_ate_stemmajor_unw,
  treatment              = "sciclubgroupTreatment",
  benchmark_covariates   = "stemoccasp9thSTEM",
  kd    = 0.25:0.75,
  ky    = 0.25:0.75,
  q     = 1,
  alpha = 0.05,
  reduce = TRUE
)
summary(sen_m2)
plot(sen_m2)

# ---- 4. DR model with IPTW only (no survey weights, M5) --------------------

lpm_ate_stemmajor_nosw <- lm(
  collegeSTEMmajorasp.num ~
    stemoccasp9th +
    sciclubgroup +
    x1control + x1freelunch + a1stucolor + x1ap +
    a1mspdintrst + a1msmentor + c1scholarship + c1pursue +
    x1race + x1sex + x1sesq5 +
    x1txmscr + x1sciint + x1stuedexpct +
    x1schoolbel + x1schooleng +
    s1alg1m09 + s1alg2m09 + s1tgeom09 + s1sfall09 +
    s1smuseum + s1sbooks + s1webinfo,
  data = data_trimmed.stemmajor,
  weights = w.atc
)

sen_m5 <- sensemakr(
  model                  = lpm_ate_stemmajor_nosw,
  treatment              = "sciclubgroupTreatment",
  benchmark_covariates   = "stemoccasp9thSTEM",
  kd    = 0.25:0.75,
  ky    = 0.25:0.75,
  q     = 1,
  alpha = 0.05,
  reduce = TRUE
)
summary(sen_m5)
plot(sen_m5)
