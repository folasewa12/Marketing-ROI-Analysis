# =============================================================================
# 01_clean_data.R  -  import, validate and prepare the transaction data
# =============================================================================
# Output: `marketing` - 44,359 clean transactions (from 49,287 raw rows)

raw <- read_excel(data_file)
skim(raw)

marketing <- raw %>%
  # Negative commissions are not valid
  filter(commissions >= 0) %>%
  # Drop rows missing a category we analyse
  filter(!is.na(product), !is.na(promotions), !is.na(campaign), !is.na(buyer)) %>%
  # Drop invalid category codes found during profiling
  filter(product != "false", campaign != "soon", promotions != "tank") %>%
  mutate(
    buyer      = recode(buyer, two = "couple"),      # "two" and "couple" are the same segment
    across(c(campaign, product, buyer, promotions), factor),
    quarter    = cut(period, breaks = c(0, 3, 6, 9, 12), labels = c("Q1", "Q2", "Q3", "Q4")),
    romi       = (purchase - marketing) / marketing  # transaction-level ROMI
  )

# Data notes recorded during validation:
#  * Purchase value barely varies (mean ~ £49,198, SD ~ £181), so small £ differences
#    can be statistically significant without being commercially meaningful.
#  * About 24% of `period` values are exactly 12, which suggests values may be capped.

message("Clean transactions: ", nrow(marketing), " of ", nrow(raw))
