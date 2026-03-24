
# ---- 1. Sample overview -----------------------------------------------------

cat("Science identity sample: n =", nrow(data.sciid),
    "students in", length(unique(data.sciid$SCH_ID)), "schools\n")
cat("STEM major sample:       n =", nrow(data.stemmajor),
    "students in", length(unique(data.stemmajor$SCH_ID)), "schools\n")

# Treatment / control split
autofit(as_flextable(table(data.sciid$sciclubgroup)))
autofit(as_flextable(table(data.stemmajor$sciclubgroup)))

# ---- 2. Descriptive tables by treatment group (Table 1) ---------------------

descr_stemmajor <- table1(
  ~ collegeSTEMmajorasp +
    x1sciid + stemoccasp9th +
    x1control + x1freelunch + a1stucolor + x1ap +
    a1mspdintrst + a1msmentor + c1scholarship + c1pursue +
    x1race + x1sex + x1sesq5 + x1txmscr + x1sciint + x1stuedexpct +
    x1schoolbel + x1schooleng +
    s1alg1m09 + s1alg2m09 + s1tgeom09 + s1sfall09 +
    s1smuseum + s1sbooks + s1webinfo | as.factor(sciclubgroup),
  data = data.stemmajor
)
descr_stemmajor

# ---- 3. Bivariate correlations with treatment and outcome -------------------

dummy_vars    <- names(data.stemmajor)[grepl("_", names(data.stemmajor))]

# Correlations with treatment
selected_vars1 <- c(dummy_vars, "sciclubgroup.num")
cor_results1   <- data.stemmajor[selected_vars1] %>%
  correlate() %>%
  focus(sciclubgroup.num)
print(cor_results1)

# Correlations with outcome
selected_vars2 <- c(dummy_vars, "collegeSTEMmajorasp.num")
cor_results2   <- data.stemmajor[selected_vars2] %>%
  correlate() %>%
  focus(collegeSTEMmajorasp.num)
print(cor_results2)

# ---- 4. Naive estimates (no PS, no survey weights) --------------------------

# Unconditional naive estimate — STEM major (logistic)
m000 <- glm(collegeSTEMmajorasp ~ sciclubgroup,
            data = data.stemmajor, family = "binomial")
summary(m000)
hc2_m000 <- vcovHC(m000, type = "HC2")
coeftest(m000, vcov = hc2_m000)

# Covariate-adjusted naive estimate — STEM major (logistic)
m001 <- glm(
  collegeSTEMmajorasp ~
    stemoccasp9th +
    sciclubgroup +
    x1control + x1freelunch + a1stucolor + x1ap +
    a1mspdintrst + a1msmentor + c1scholarship + c1pursue +
    x1race + x1sex + x1sesq5 + x1txmscr + x1sciint + x1stuedexpct +
    x1schoolbel + x1schooleng +
    s1alg1m09 + s1alg2m09 + s1tgeom09 + s1sfall09 +
    s1smuseum + s1sbooks + s1webinfo,
  data = data.stemmajor, family = "binomial"
)
table.m001 <- tidy.logit.table(m001)
knitr::kable(table.m001, digits = 3,
             caption = "Unweighted logistic regression: STEM major aspiration")

# Multilevel naive estimate — STEM major (logistic GLMM)
m002 <- glmer(
  collegeSTEMmajorasp ~
    stemoccasp9th +
    sciclubgroup +
    x1control + x1freelunch + a1stucolor + x1ap +
    a1mspdintrst + a1msmentor + c1scholarship + c1pursue +
    x1race + x1sex + x1sesq5 + x1txmscr + x1sciint + x1stuedexpct +
    x1schoolbel + x1schooleng +
    s1alg1m09 + s1alg2m09 + s1tgeom09 + s1sfall09 +
    s1smuseum + s1sbooks + s1webinfo +
    (1 | SCH_ID),
  data = data.stemmajor, family = binomial(link = "logit")
)
summary(m002)
