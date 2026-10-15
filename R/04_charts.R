# =============================================================================
# 04_charts.R  -  charts used in the README and summary
# =============================================================================

# ---- 1. Average vs marginal return -------------------------------------------
fig_return <- return_summary %>%
  filter(measure != "Spend elasticity of purchase value") %>%
  mutate(measure = factor(measure, levels = rev(measure))) %>%
  ggplot(aes(value, measure)) +
  geom_col(fill = "#2a78d6", width = 0.6) +
  geom_text(aes(label = dollar(value, prefix = "£", accuracy = 0.01)), hjust = -0.15) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "The next £1 of spend returns far less than the average £1",
       x = "Revenue per £1 of marketing spend", y = NULL) +
  theme_report()
save_plot(fig_return, "average_vs_marginal_return", height = 3.5)

# ---- 2. Campaign volume vs revenue share --------------------------------------
fig_share <- segments %>%
  filter(segment == "campaign") %>%
  select(level, share_volume, share_revenue) %>%
  pivot_longer(-level, names_to = "measure", values_to = "share") %>%
  mutate(measure = recode(measure, share_volume = "Share of transactions",
                          share_revenue = "Share of revenue")) %>%
  ggplot(aes(level, share, fill = measure)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = percent(share, accuracy = 0.1)),
            position = position_dodge(width = 0.8), vjust = -0.4, size = 3.2) +
  scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = c("#2a78d6", "#eb6834")) +
  labs(title = "Twitter's revenue lead comes from volume, not bigger sales",
       x = NULL, y = NULL, fill = NULL) +
  theme_report()
save_plot(fig_share, "campaign_volume_vs_revenue")

# ---- 3. Size of each effect, holding everything else constant ---------------
effects <- model_table %>%
  filter(term %in% c("campaigntwitter", "campaigninstagram", "campaigntiktok",
                     "promotionsyes", "quarterQ4", "productsuv")) %>%
  mutate(label = recode(term,
                        campaigntwitter   = "Twitter (vs Facebook)",
                        campaigninstagram = "Instagram (vs Facebook)",
                        campaigntiktok    = "TikTok (vs Facebook)",
                        promotionsyes     = "Promotion running",
                        quarterQ4         = "Quarter 4 (vs Q1)",
                        productsuv        = "SUV (vs Sedan)"),
         label = reorder(label, estimate))

fig_effects <- ggplot(effects, aes(estimate, label)) +
  geom_vline(xintercept = 0, colour = "grey60") +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.2, orientation = "y") +
  geom_point(size = 3, colour = "#2a78d6") +
  labs(title = "Every segment effect is under 0.3% of an average £49,198 sale",
       x = "Difference in purchase value per transaction (£), 95% CI", y = NULL) +
  theme_report()
save_plot(fig_effects, "segment_effects")

# ---- 4. Spend deciles ---------------------------------------------------------
fig_deciles <- ggplot(spend_deciles, aes(avg_spend, avg_purchase)) +
  geom_line(colour = "#2a78d6", linewidth = 1) +
  geom_point(colour = "#2a78d6", size = 2.5) +
  scale_y_continuous(labels = dollar_format(prefix = "£")) +
  scale_x_continuous(labels = dollar_format(prefix = "£")) +
  labs(title = "Purchase value rises only slightly across spend deciles",
       x = "Average marketing spend per transaction", y = "Average purchase value") +
  theme_report()
save_plot(fig_deciles, "spend_deciles")
