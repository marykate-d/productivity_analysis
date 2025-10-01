# Productivity Project: Predicting Employee Performance in the Garment Industry

This project uses machine learning techniques to predict whether a team of garment workers met their daily productivity targets based on operational, time, and workforce-related features.

## 📊 Overview

- **Goal:** Predict if a team met their productivity target using a classification approach.
- **Modeling Techniques:** Naive Bayes, k-Nearest Neighbors (kNN), Random Forest, and XGBoost.
- **Best Model:** A tuned Random Forest, selected based on balanced accuracy and ROC AUC metrics.

## 📁 Data

- **Source:** [UCI Machine Learning Repository – Garment Worker Productivity Dataset](https://archive.ics.uci.edu/dataset/597/productivity+prediction+of+garment+employees)
- **Observations:** 1,197 rows, across 12 teams.
- **Features:** Includes number of workers, overtime, incentives, idle time, SMV (Standard Minute Value), and more.
- **Target Variable:** `goal_met` — a binary variable created for this project:
  - `1` = team met their productivity goal
  - `0` = team did not meet their goal

## 🧠 Key Research Questions

1. **Do higher incentives lead to better productivity?**
2. **Is there an interaction between team size and work in progress?**
3. **Are style changes significant in predicting productivity outcomes?**

## 🔍 Methodology

- Data preprocessing:
  - Removal of non-predictive features (`date`, `department`, `day`)
  - Normalization and dummy coding
  - Handling class imbalance via upsampling

- Modeling:
  - Trained four models using cross-validation
  - Tuned the best-performing ones (Random Forest and XGBoost)
  - Evaluated final model on a held-out test set

## 🏆 Results

- **Best model:** Random Forest (tuned)
  - **Balanced Accuracy:** 0.87 (test set)
  - **ROC AUC:** 0.90 (test set)
- **Most important features:**
  - Incentive amount
  - Standard Minute Value (SMV)
  - Number of workers
  - Work in progress (WIP)
  - Team identifier (e.g., Team 10 had lower performance)

## 📌 Insights

- **Incentives** significantly increase the likelihood of goal achievement—especially around the average incentive level.
- **Standard Minute Value (SMV)** is inversely related to goal completion; longer tasks decrease the odds.
- **Team size** helps, but its interaction with WIP did not show a strong dependency.
- **Style changes**, contrary to expectation, were not highly predictive.
- **Team-specific patterns** (e.g., Team 10 underperformance) may reveal deeper operational issues.

## 📚 References

- Imran, A. A., Rahim, M. S., & Ahmed, T. (2021). Mining the productivity data of the garment industry. *International Journal of Business Intelligence and Data Mining, 19*(3), 319–342.
- Balla, I., Rahayu, S., & Purnama, J. J. (2021). *Garment employee productivity prediction using Random Forest*. Techno Nusa Mandiri, 18(1), 49–54.
- [UCI Machine Learning Repository - Garment Productivity Dataset](https://archive.ics.uci.edu/dataset/597/productivity+prediction+of+garment+employees)

## 🛠 Tools & Libraries

- **R**
- tidyverse, tidymodels, recipes, themis
- glmnet, kknn, DALEX, DALEXtra

---

### 📄 Output Format

- Final report generated as a **PDF** using Quarto.

---

## 📌 Author

Project developed as part of **Behavioral Data Science II** coursework. For academic, educational, and demonstration purposes.

