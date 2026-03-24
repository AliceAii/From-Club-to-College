
# ---- 1. Propensity score model (STEM major sample) --------------------------

pscore.m.stemmajor <- glm(
  sciclubgroup ~
    x1sciid + stemoccasp9th +
    x1control + x1freelunch + a1stucolor + x1ap +
    a1mspdintrst + a1msmentor + c1scholarship + c1pursue +
    x1race + x1sex + x1sesq5 + x1txmscr + x1sciint + x1stuedexpct +
    x1schoolbel + x1schooleng +
    s1alg1m09 + s1alg2m09 + s1tgeom09 + s1sfall09 +
    s1smuseum + s1sbooks + s1webinfo,
  data = data.stemmajor, family = "binomial"
)

# Table 2: Propensity score model results
table.pscore.m.stemmajor <- tidy.logit.table(pscore.m.stemmajor)
knitr::kable(table.pscore.m.stemmajor, digits = 3,
             caption = "Logistic regression predicting science club participation (Table 2)")

# Store predicted propensity scores
data.stemmajor <- data.stemmajor %>%
  mutate(
    pscore       = predict(pscore.m.stemmajor, type = "response"),
    pscorelogodd = predict(pscore.m.stemmajor)
  )

# ---- 2. VIF check -----------------------------------------------------------

vif_values <- vif(pscore.m.stemmajor)
print(vif_values)

# ---- 3. Common support visualisation (Figure 1) ----------------------------

data.stemmajor <- data.stemmajor %>%
  mutate(sciclubgroup_plot = factor(
    sciclubgroup.num,
    levels = c(0, 1),
    labels = c("Non-participants", "Participants")
  ))

# Trimming cutoffs on the logit scale
logit_lower <- log(0.023 / (1 - 0.023))
logit_upper <- log(0.623 / (1 - 0.623))

fig1 <- ggplot(data.stemmajor,
               aes(x = pscorelogodd, y = sciclubgroup_plot,
                   color = sciclubgroup)) +
  geom_jitter(height = 0.2, alpha = 0.4, size = 1) +
  geom_vline(xintercept = logit_lower, linetype = "dashed",
             color = "red", linewidth = 0.8) +
  geom_vline(xintercept = logit_upper, linetype = "dashed",
             color = "red", linewidth = 0.8) +
  scale_color_manual(values = c("#FFB81C", "#005587"),
                     labels = c("Non-participants", "Participants")) +
  scale_x_continuous(
    breaks = seq(floor(min(data.stemmajor$pscorelogodd, na.rm = TRUE)),
                 ceiling(max(data.stemmajor$pscorelogodd, na.rm = TRUE)),
                 by = 0.5)
  ) +
  labs(title = " ", x = "Logit Score", y = NULL,
       color = "Science Clubs/Groups") +
  theme_minimal() +
  theme(plot.title = element_text(size = 10))

fig1

ggsave("output/Fig1_commonsupport.png", plot = fig1,
       width = 7, height = 3, dpi = 1200, bg = "white")

# ---- 4. Trim data to common support region ---------------------------------

data_trimmed.stemmajor <- data.stemmajor %>%
  filter(pscore >= 0.023 & pscore <= 0.623)

cat("Cases trimmed:", nrow(data.stemmajor) - nrow(data_trimmed.stemmajor), "\n")
table(data_trimmed.stemmajor$sciclubgroup)
