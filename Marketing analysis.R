##############################################
# Marketing ROI & Effectiveness Analysis
# Author: AFOLASEWA DUDUYEMI
# Date: 26/12/2025
# Purpose: Analyze marketing spend, revenue, and ROI
##############################################

### 1. Load Libraries ###
library(readxl)
library(ggplot2)
library(dplyr)
library(skimr)

### 2. Set Working Directory ###
setwd("C:/Users/folas/OneDrive/Documents/Working Directory")

### 3. Load Data ###
Marketing <- read_excel("ev_20.xlsx")

# Quick look at data
str(Marketing)
summary(Marketing)
skim(Marketing)

### 4. Data Cleaning & Transformation ###

# Remove negative commissions
Marketing <- Marketing %>% filter(commissions >= 0)

# Remove missing values in key columns
Marketing <- Marketing %>%
  filter(!is.na(product),
         !is.na(promotions),
         !is.na(campaign),
         !is.na(buyer))

# Remove invalid entries
Marketing <- Marketing %>% filter(product != "false",
                                  campaign != "soon", 
                                  promotions != "tank")

# Recode buyer segments
Marketing$buyer <- recode_factor(Marketing$buyer, 'two' = "couple")

# Create ROMI metrics
Marketing <- Marketing %>%
  mutate(
    ROMI = (purchase - marketing) / marketing,
    ROMI_pct = ROMI * 100
  )

### 5. Baseline Summary ###
baseline <- Marketing %>%
  summarise(
    n = n(),
    total_marketing = sum(marketing, na.rm=TRUE),
    total_purchase = sum(purchase, na.rm=TRUE),
    avg_marketing = mean(marketing, na.rm=TRUE),
    avg_purchase = mean(purchase, na.rm=TRUE),
    avg_ROMI = mean(ROMI, na.rm=TRUE)
  )
baseline

### 6. Data Visualization ###

### 6a. Custom Themes & Palettes ###
theme_report <- theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

campaign_palette <- c(
  "fbook" = "#1877F2",
  "instagram" = "#E4405F",
  "tiktok" = "#000000",
  "twitter" = "#1DA1F2"
)

product_palette <- c(
  "sedan" = "#4E79A7",
  "suv" = "#59A14F",
  "sport" = "#E15759"
)

buyer_palette <- c(
  "single" = "#6B6ECF",
  "couple" = "#9C9EDE",
  "family" = "#CEDB9C"
)


#### 6b. Distribution of KPIs ####
# Purchase Revenue
ggplot(Marketing, aes(purchase)) + 
  geom_histogram(binwidth = 100, fill = "#1b7837", color = "black") +
  labs(title = "Distribution of Purchase Revenue", x = "Purchase (£)", y = "Count") +
  theme_report

# ROMI
ggplot(Marketing, aes(ROMI)) + 
  geom_histogram(binwidth = 1, fill = "darkgreen", color = "black") +
  labs(title = "Distribution of ROMI", x = "ROMI", y = "") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "transparent"), 
        text = element_text(size = 9, family = "sans", face = "bold"))

#### 6c. Campaign Analysis ####
# Average Purchase by Campaign
df_avg_campaign <- Marketing %>%
  group_by(campaign) %>%
  summarise(avg_purchase = mean(purchase, na.rm=TRUE))

ggplot(df_avg_campaign, aes(campaign, avg_purchase, fill = campaign)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = round(avg_purchase, 0)), vjust = -0.5, size = 3.2) +
  scale_fill_manual(values = campaign_palette) +
  labs(title = "Average Purchase Value by Campaign", x = "Campaign", y = "Avg Purchase (£)") +
  theme_report

# Total Purchase by Campaign
df_total_campaign <- Marketing %>%
  group_by(campaign) %>%
  summarise(total_purchase = sum(purchase))

ggplot(df_total_campaign, aes(campaign, total_purchase / 1e6, fill = campaign)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = round(total_purchase / 1e6, 0)), vjust = -0.5, size = 3.2) +
  scale_fill_manual(values = campaign_palette) +
  labs(title = "Total Purchase Revenue by Campaign", x = "Campaign", y = "Total Purchase (£ millions)") +
  theme_report

#### 6d. Product Analysis ####
# Average & Total Purchase by Product
df_avg_product <- Marketing %>%
  group_by(product) %>%
  summarise(avg_purchase = mean(purchase))
df_total_product <- Marketing %>%
  group_by(product) %>%
  summarise(total_purchase = sum(purchase))

# Plot code similar to above, using product_palette

#### 6e. Buyer Analysis ####
# Average & Total Purchase by Buyer Segment
df_avg_buyer <- Marketing %>%
  group_by(buyer) %>%
  summarise(avg_purchase = mean(purchase))
df_total_buyer <- Marketing %>%
  group_by(buyer) %>%
  summarise(total_purchase = sum(purchase))

# Plot code similar to above, using buyer_palette

#### 6f. Marketing vs Purchase ####
ggplot(Marketing, aes(marketing, purchase)) +
  geom_point(alpha = 0.25, color = "#2166ac") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Marketing Spend vs Purchase Revenue", x = "Marketing (£)", y = "Purchase (£)") +
  theme_report

#### 6g. Marketing vs ROMI ####
ggplot(Marketing, aes(marketing, ROMI)) +
  geom_point(alpha = 0.25, color = "#2166ac") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Marketing Spend vs ROMI", x = "Marketing (£)", y = "ROMI") +
  theme_report

### 7. Statistical Analysis ###

# Regression: Purchase ~ Marketing + Commissions
model_purchase <- lm(purchase ~ marketing + commissions, data = Marketing)
summary(model_purchase)

# Regression: ROMI ~ Marketing
model_romi <- lm(ROMI ~ marketing, data = Marketing)
summary(model_romi)

# ANOVA: Campaign effects on Purchase
anova_campaign <- aov(purchase ~ campaign, data = Marketing)
summary(anova_campaign)
TukeyHSD(anova_campaign)

# Combined Predictive Model
final_model <- lm(purchase ~ marketing + commissions + campaign + promotions + product, data = Marketing)
summary(final_model)

