# =============================================================================
# 03_return_on_spend.R  -  average vs marginal return, and drivers of purchase value
# =============================================================================

# ---- Why transaction-level ROMI "falls" as spend rises -----------------------
# ROMI = purchase / marketing - 1. Purchase value is almost constant here, so
# ROMI is close to 1 / marketing and must fall as spend rises. This is arithmetic,
# not evidence of diminishing returns.
cor(marketing$romi, 1 / marketing$marketing)   # ~0.98

# ---- Marginal return: what does the NEXT £1 of spend bring in? -------------
simple_model <- lm(purchase ~ marketing, data = marketing)

# Spend deciles: average spend, average purchase and ROMI in each tenth
spend_deciles <- marketing %>%
  mutate(decile = ntile(marketing, 10)) %>%
  group_by(decile) %>%
  summarise(avg_spend = mean(marketing), avg_purchase = mean(purchase), .groups = "drop") %>%
  mutate(romi = (avg_purchase - avg_spend) / avg_spend)
print(spend_deciles)
write.csv(spend_deciles, file.path(table_dir, "spend_deciles.csv"), row.names = FALSE)

# Elasticity: % change in purchase value for a 1% change in spend
elasticity_model <- lm(log(purchase) ~ log(marketing), data = marketing)

# ---- Drivers of purchase value (all levers together) ------------------------
full_model <- lm(purchase ~ marketing + commissions + campaign + promotions +
                   product + buyer + quarter, data = marketing)
summary(full_model)

model_table <- tidy(full_model, conf.int = TRUE)
write.csv(model_table, file.path(table_dir, "full_model_coefficients.csv"), row.names = FALSE)

return_summary <- tibble(
  measure = c("Average revenue per £1 of spend",
              "Marginal revenue per extra £1 (spend only)",
              "Marginal revenue per extra £1 (all controls)",
              "Spend elasticity of purchase value"),
  value   = c(kpis$revenue_per_pound,
              coef(simple_model)[["marketing"]],
              coef(full_model)[["marketing"]],
              coef(elasticity_model)[[2]])
)
print(return_summary)
write.csv(return_summary, file.path(table_dir, "average_vs_marginal_return.csv"), row.names = FALSE)
