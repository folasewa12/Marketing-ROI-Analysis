Marketing ROI & Effectiveness Analysis
How does marketing spend drive purchase value and profitability?

BUSINESS QUESTION

How does marketing spend influence purchase value and profitability, and which marketing activities deliver the most efficient returns on investment?

PROJECT OVERVIEW

Marketing spend is often increased to drive revenue, but higher spend does not always result in higher profitability. 
This project evaluates whether marketing investments effectively drive purchase value and whether they generate efficient returns.

Using transactional marketing data, this analysis examines:

* Revenue performance across campaigns, products, and buyer segments

* Marketing efficiency using Return on Marketing Investment (ROMI)

* Evidence of diminishing returns as marketing spend increases

* The goal is to support data-driven marketing budget allocation by balancing revenue growth with efficiency.

DATA AND KEY PERFORMANCE INDICATOR (KPI) 

The dataset consists of transactional marketing records, where each observation represents a marketing exposure linked to a purchase outcome.
Key variables include:
* Purchase value (revenue)
* Marketing spend
* Commissions
* Campaign (Facebook, Instagram, Twitter, TikTok)
* Product category (SUV, Sedan, Sport)
* Buyer type (Single, Couple, Family)
* Promotion status
* Time (grouped into quarters)

After data cleaning and validation, 44,359 observations were retained from the original ~49,000 records.

Key Performance Indicators (KPIs)

* Purchase Value — primary revenue KPI
* Return on Marketing Investment (ROMI)

ROMI = (Purchase − Marketing Spend) / Marketing Spend

These KPIs provide a balanced view of value creation and spend efficiency.

ANALYTICAL APPROACH 
~ Exploratory Data Analysis (EDA)
  * Revenue breakdown by campaign, product, buyer type, and time
   * Distribution analysis of purchase values
~ Relationship analysis
  * Marketing spend vs purchase value
   * Marketing spend vs ROMI
~ Statistical modelling
* Multiple linear regression
 * ANOVA with post-hoc Tukey tests
   * Multivariate predictive modelling with multiple marketing levers

KEY INSIGHTS 
* Marketing spend and commissions have statistically significant positive effects on purchase value.
* Revenue differences across campaigns are driven primarily by transaction volume, not transaction size.
* Twitter generates both high revenue and the highest ROMI.
* Higher marketing spend is associated with declining ROMI, indicating diminishing marginal returns.
* Promotions are associated with lower purchase value after controlling for other factors.

RECOMMENDATIONS 
* Prioritise efficiency over pure revenue growth by incorporating ROMI into budget decisions.
* Reallocate spend toward higher-ROMI campaigns, particularly Twitter.
* Monitor diminishing returns and avoid uniform budget increases across all campaigns.
* Use volume-driven campaigns strategically, pairing scale with efficiency checks.
* Reassess promotional strategies, as promotions may reduce transaction value.
These actions can improve profitability without increasing total marketing spend.

STATISTICAL METHODS USED 
* Multiple Linear Regression
* One-way ANOVA
* Tukey HSD post-hoc testing
* Predictive regression modelling

~ All statistical results were validated using significance testing (p < 0.001 where applicable).

TOOLS USED 
~ R
* tidyverse
  * ggplot2
    * stats
* Data cleaning & transformation
* Regression analysis & ANOVA
* Business-focused data storytelling

 HOW TO RUN THE CODE 
* Clone this repository: git clone https://github.com/folasewa12/marketing-roi-analysis.git
* Open the project in RStudio.
* Install required packages: install.packages(c("tidyverse"))
~ Run the scripts in the following order:
* data_cleaning.R
 * eda_visualisations.R
   * statistical_analysis.R

Outputs include cleaned datasets, charts, and model summaries.