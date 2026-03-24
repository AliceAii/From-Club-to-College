
# ---- 1. Unconditional outcome model with ATE weights ------------------------

ate_stemma <- glm(
  collegeSTEMmajorasp ~
    stemoccasp9th +
    sciclubgroup,
  data = data_trimmed.stemmajor, family = binomial(),
  weights = w.ate_W3W1W2STU
)
table.ate_stemma <- tidy.logit.table(ate_stemma)
knitr::kable(table.ate_stemma, digits = 3,
             caption = "ATE weighted logistic regression: unconditional model")

# ---- 2. Nested model — Background block ------------------------------------

ate_stemma2 <- glm(
  collegeSTEMmajorasp ~
    stemoccasp9th +
    sciclubgroup +
    x1control + x1freelunch + a1stucolor + x1ap +
    a1mspdintrst + a1msmentor + c1scholarship + c1pursue +
    x1race + x1sex + x1sesq5,
  data = data_trimmed.stemmajor, family = binomial(),
  weights = w.ate_W3W1W2STU
)
table.ate_stemma2 <- tidy.logit.table(ate_stemma2)
knitr::kable(table.ate_stemma2, digits = 3,
             caption = "Nested ATE model: Background block")

# ---- 3. Nested model — Competence / Perception / Experiences block ----------

ate_stemma3 <- glm(
  collegeSTEMmajorasp ~
    stemoccasp9th +
    sciclubgroup +
    x1control + x1freelunch + a1stucolor + x1ap +
    a1mspdintrst + a1msmentor + c1scholarship + c1pursue +
    x1race + x1sex + x1sesq5 +
    x1txmscr + x1sciint + x1stuedexpct +
    x1schoolbel + x1schooleng,
  data = data_trimmed.stemmajor, family = binomial(),
  weights = w.ate_W3W1W2STU
)
table.ate_stemma3 <- tidy.logit.table(ate_stemma3)
knitr::kable(table.ate_stemma3, digits = 3,
             caption = "Nested ATE model: Competence/Perception/Experiences block")

# ---- 4. Doubly robust ATE estimate (Table 4) --------------------------------

ate_dr_stemma <- glm(
  collegeSTEMmajorasp ~
    stemoccasp9th +
    sciclubgroup +
    x1control + x1freelunch + a1stucolor + x1ap +
    a1mspdintrst + a1msmentor + c1scholarship + c1pursue +
    x1race + x1sex + x1sesq5 +
    x1txmscr + x1sciint + x1stuedexpct +
    x1schoolbel + x1schooleng +
    s1alg1m09 + s1alg2m09 + s1tgeom09 + s1sfall09 +
    s1smuseum + s1sbooks + s1webinfo,
  data = data_trimmed.stemmajor, family = binomial(),
  weights = w.ate_W3W1W2STU
)
table.ate_dr_stemma <- tidy.logit.table(ate_dr_stemma)
knitr::kable(table.ate_dr_stemma, digits = 3,
             caption = "Doubly robust ATE: STEM major aspiration (Table 4)")

# ---- 5. Doubly robust ATT estimate -----------------------------------------

att_dr_stemma <- glm(
  collegeSTEMmajorasp ~
    stemoccasp9th +
    sciclubgroup +
    x1control + x1freelunch + a1stucolor + x1ap +
    a1mspdintrst + a1msmentor + c1scholarship + c1pursue +
    x1race + x1sex + x1sesq5 +
    x1txmscr + x1sciint + x1stuedexpct +
    x1schoolbel + x1schooleng +
    s1alg1m09 + s1alg2m09 + s1tgeom09 + s1sfall09 +
    s1smuseum + s1sbooks + s1webinfo,
  data = data_trimmed.stemmajor, family = binomial(),
  weights = w.att_W3W1W2STU
)
table.att_dr_stemma <- tidy.logit.table(att_dr_stemma)
knitr::kable(table.att_dr_stemma, digits = 3,
             caption = "Doubly robust ATT: STEM major aspiration")

# ---- 6. Doubly robust ATC estimate -----------------------------------------

atc_dr_stemma <- glm(
  collegeSTEMmajorasp ~
    stemoccasp9th +
    sciclubgroup +
    x1control + x1freelunch + a1stucolor + x1ap +
    a1mspdintrst + a1msmentor + c1scholarship + c1pursue +
    x1race + x1sex + x1sesq5 +
    x1txmscr + x1sciint + x1stuedexpct +
    x1schoolbel + x1schooleng +
    s1alg1m09 + s1alg2m09 + s1tgeom09 + s1sfall09 +
    s1smuseum + s1sbooks + s1webinfo,
  data = data_trimmed.stemmajor, family = binomial(),
  weights = w.atc_W3W1W2STU
)
table.atc_dr_stemma <- tidy.logit.table(atc_dr_stemma)
knitr::kable(table.atc_dr_stemma, digits = 3,
             caption = "Doubly robust ATC: STEM major aspiration")

# ---- 7. ATE without survey weights (Appendix D) ----------------------------

ate_dr_stemma_nosw <- glm(
  collegeSTEMmajorasp ~
    stemoccasp9th +
    sciclubgroup +
    x1control + x1freelunch + a1stucolor + x1ap +
    a1mspdintrst + a1msmentor + c1scholarship + c1pursue +
    x1race + x1sex + x1sesq5 +
    x1txmscr + x1sciint + x1stuedexpct +
    x1schoolbel + x1schooleng +
    s1alg1m09 + s1alg2m09 + s1tgeom09 + s1sfall09 +
    s1smuseum + s1sbooks + s1webinfo,
  data = data_trimmed.stemmajor, family = binomial(),
  weights = w.ate
)
table.ate_dr_stemma_nosw <- tidy.logit.table(ate_dr_stemma_nosw)
knitr::kable(table.ate_dr_stemma_nosw, digits = 3,
             caption = "ATE weighted (no survey weights): STEM major (Appendix D)")

# ---- 8. Model comparison (AIC / BIC) ----------------------------------------

AIC(ate_stemma, ate_stemma2, ate_stemma3, ate_dr_stemma)
BIC(ate_stemma, ate_stemma2, ate_stemma3, ate_dr_stemma)
