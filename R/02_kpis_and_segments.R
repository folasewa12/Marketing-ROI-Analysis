# =============================================================================
# 02_kpis_and_segments.R  -  headline KPIs and segment performance
# =============================================================================

# ---- Headline KPIs -----------------------------------------------------------
# Portfolio ROMI is calculated from totals (total revenue vs total spend),
# not by averaging transaction-level ratios.
kpis <- marketing %>%
  summarise(
    transactions    = n(),
    total_revenue   = sum(purchase),
    total_spend     = sum(marketing),
    revenue_per_pound = total_revenue / total_spend,
    portfolio_romi  = (total_revenue - total_spend) / total_spend
  )
print(kpis)

# ---- Segment table with effect size ------------------------------------------
# For each segment: volume share, revenue share, average sale, ROMI from totals,
# and eta-squared = share of variation in purchase value the segment explains.
segment_summary <- function(var) {
  fit  <- aov(as.formula(paste("purchase ~", var)), data = marketing)
  ss   <- summary(fit)[[1]][["Sum Sq"]]
  eta2 <- ss[1] / sum(ss)
  p    <- summary(fit)[[1]][["Pr(>F)"]][1]

  marketing %>%
    group_by(level = .data[[var]]) %>%
    summarise(transactions = n(),
              avg_purchase = mean(purchase),
              revenue      = sum(purchase),
              spend        = sum(marketing),
              .groups = "drop") %>%
    mutate(segment       = var,
           share_volume  = transactions / sum(transactions),
           share_revenue = revenue / sum(revenue),
           romi          = (revenue - spend) / spend,
           eta_squared   = eta2,
           anova_p       = p) %>%
    select(segment, level, transactions, share_volume, share_revenue,
           avg_purchase, romi, eta_squared, anova_p)
}

segments <- bind_rows(lapply(c("campaign", "product", "buyer", "promotions", "quarter"),
                             segment_summary))
print(segments, n = Inf)
write.csv(segments, file.path(table_dir, "segment_performance.csv"), row.names = FALSE)

# Pairwise campaign comparisons
print(TukeyHSD(aov(purchase ~ campaign, data = marketing)))
