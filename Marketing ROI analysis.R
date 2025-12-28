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
    plot.title = element_text(face = "bold", size = 12, color = "black"),  
    axis.title = element_text(face = "bold", color = "black"),             
    axis.text = element_text(color = "black"),                             
    panel.background = element_rect(fill = "white", color = NA),           
    plot.background = element_rect(fill = "white", color = NA),            
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )


campaign_palette <- c(
  "fbook" = "#1877F2",
  "instagram" = "#E4405F",
  "tiktok" = "#808080",
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


#### 6b. Distribution of KPI's ####
# Purchase Revenue
ggplot(Marketing, aes(purchase)) + 
  geom_histogram(binwidth = 100, fill = "#1b7837", color = "black") +
  labs(title = "Distribution of Purchase Revenue", x = "Purchase (£)", y = "Count") +
  theme_report

ggsave(
  "Charts/Distribution of Purchase Value.png",
  width = 8,
  height = 5,
  dpi = 300)

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
  coord_cartesian(
    ylim = c(
      min(df_avg_campaign$avg_purchase) * 0.999,
      max(df_avg_campaign$avg_purchase) * 1.001
    )) + 
  scale_fill_manual(values = campaign_palette) +
  labs(title = "Average Purchase Value by Campaign", x = "Campaign", y = "Avg Purchase (£)") +
  theme_report 

ggsave(
  "Charts/Average Purchase by Campaign.png",
  width = 8,
  height = 5,
  dpi = 300)

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

ggsave(
  "Charts/Total Purchase by Campaign.png",
  width = 8,
  height = 5,
  dpi = 300)

#### 6d. Product Analysis ####
# Average & Total Purchase by Product
df_avg_product <- Marketing %>%
  group_by(product) %>%
  summarise(avg_purchase = mean(purchase))

ggplot(df_avg_product, aes(x = product, y = avg_purchase, fill = product)) +
  geom_col() +
  geom_text(aes(label = round(avg_purchase, 0)), vjust = -0.5) + # add labels for clarity
  labs(title="Average Purchase by Product", y="Average Purchase") +
  scale_fill_manual(values = product_palette) +
  coord_cartesian(ylim = c(min(df_avg_product$avg_purchase)*0.999, max(df_avg_product$avg_purchase)*1.001)) + 
  theme_report 

ggsave(
  "Charts/Average Purchase by Product.png",
  width = 8,
  height = 5,
  dpi = 300)

### Total

df_total_product <- Marketing %>%
  group_by(product) %>%
  summarise(total_purchase = sum(purchase))

ggplot(df_total_product, aes(x = product, y = total_purchase / 1e6, fill = product)) +
  geom_col(show.legend = FALSE) +
  geom_text(
    aes(label = round(total_purchase / 1e6, 0)),
    vjust = -0.3,
    size = 3.5
  ) +
  labs(
    title = "Total Purchase by Product",
    x = "Product",
    y = "Total Purchase (£ millions)"
  ) +
  scale_fill_manual(values = product_palette) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15))  
  ) +
  theme_report

ggsave(
  "Charts/Total Purchase by Products.png",
  width = 8,
  height = 5,
  dpi = 300)

#### 6e. Buyer Analysis ####
# Average & Total Purchase by Buyer Segment
df_avg_buyer <- Marketing %>%
  group_by(buyer) %>%
  summarise(avg_purchase = mean(purchase))

ggplot(df_avg_buyer, aes(x = buyer, y = avg_purchase, fill = buyer)) +
  geom_col() +
  geom_text(aes(label = round(avg_purchase, 0)), vjust = -0.5) + # add labels for clarity
  labs(title="Average Purchase by Buyer Segment", y="Average Purchase £") +
  scale_fill_manual(values = buyer_palette) +
  coord_cartesian(ylim = c(min(df_avg_buyer$avg_purchase)*0.999, max(df_avg_buyer$avg_purchase)*1.001)) + 
  theme_report

ggsave(
  "Charts/Average Purchase by Buyer.png",
  width = 8,
  height = 5,
  dpi = 300)

## Total 
df_total_buyer <- Marketing %>%
  group_by(buyer) %>%
  summarise(total_purchase = sum(purchase))

ggplot(df_total_buyer, aes(x = buyer, y = total_purchase / 1e6, fill = buyer)) +
  geom_col(show.legend = FALSE) +
  geom_text(
    aes(label = round(total_purchase / 1e6, 0)),
    vjust = -0.5,
    size = 3.2
  ) +
  scale_fill_manual(values = buyer_palette) +
  labs(
    title = "Total Purchase Revenue by Buyer Segment",
    x = "Buyer Segment",
    y = "Total Purchase (£ millions)"
  ) +
  theme_report

ggsave(
  "Charts/Total Purchase by Buyer.png",
  width = 8,
  height = 5,
  dpi = 300)


#### 6f. Marketing vs Purchase ####
ggplot(Marketing, aes(marketing, purchase)) +
  geom_point(alpha = 0.25, color = "#2166ac") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Marketing Spend vs Purchase Revenue", x = "Marketing (£)", y = "Purchase (£)") +
  theme_report

ggsave(
  "Charts/Purchase and Marketing Spend.png",
  width = 8,
  height = 5,
  dpi = 300)


#### 6g. Marketing vs ROMI ####
ggplot(Marketing, aes(marketing, ROMI)) +
  geom_point(alpha = 0.25, color = "#2166ac") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Marketing Spend vs ROMI", x = "Marketing (£)", y = "ROMI") +
  theme_report

ggsave(
  "Charts/ROMI and Marketing Spend.png",
  width = 8,
  height = 5,
  dpi = 300)


### 6g. Campaign vs ROMI ####
#### Is Marketing actually profitable using ROMI ### 

df_romi_campaign <- Marketing %>%
  group_by(campaign) %>%
  summarise(avg_ROMI = mean(ROMI, na.rm=TRUE))


ggplot(df_romi_campaign, aes(campaign, avg_ROMI, fill = campaign)) +
  geom_col(show.legend = FALSE) +
  geom_text(
    aes(label = scales::percent(avg_ROMI, accuracy = 0.1)),
    vjust = -0.5,
    size = 3.2
  ) + scale_fill_manual(values = campaign_palette) +
  labs(
    title = "Average Return on Marketing Investment by Campaign",
    x = "Campaign",
    y = "Average ROMI"
  ) +
  coord_cartesian(
    ylim = c(
      min(df_romi_campaign$avg_ROMI) * 0.999,
      max(df_romi_campaign$avg_ROMI) * 1.001
    )
  ) +
  theme_report

ggsave(
  "Charts/ROMI and Campaign.png",
  width = 8,
  height = 5,
  dpi = 300)

### 6h. Group the period into quarters ####

Marketing$quarter <- cut(
  Marketing$period,
  breaks = c(0, 3, 6, 9, 12),
  labels = c("Q1", "Q2", "Q3", "Q4")
)

df_quarter <- Marketing %>%
  group_by(quarter) %>%
  summarise(avg_purchase = mean(purchase, na.rm = TRUE))

ggplot(df_quarter, aes(x = quarter, y = avg_purchase, group = 1)) +
  geom_line(color = "#4E79A7", linewidth = 1) +
  geom_point(color = "#4E79A7", size = 3) +
  geom_text(aes(label = round(avg_purchase, 0)), vjust = -0.5) +
  labs(
    title = "Purchase Trend by Quarter",
    y = "Average Purchase"
  ) +
  theme_report


ggsave(
  "Charts/Purchase and Quarterly Period.png",
  width = 8,
  height = 5,
  dpi = 300)


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

