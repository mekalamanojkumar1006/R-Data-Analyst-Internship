# ============================================================
# R DATA ANALYST INTERNSHIP - WEEK 3
# Employee Attrition Prediction using Logistic Regression
# ============================================================

# ------------------------------------------------------------
# 1. Load Packages
# ------------------------------------------------------------

library(ggplot2)
library(caret)
library(pROC)

# ------------------------------------------------------------
# 2. Dataset Preparation
# ------------------------------------------------------------

dim(hr_clean)
str(hr_clean)

# Check missing values
colSums(is.na(hr_clean))

# Attrition distribution
table(hr_clean$Attrition)
prop.table(table(hr_clean$Attrition)) * 100

# ------------------------------------------------------------
# 3. Train-Test Split
# ------------------------------------------------------------

set.seed(123)

train_index <- createDataPartition(
  hr_clean$Attrition,
  p = 0.80,
  list = FALSE
)

train_data <- hr_clean[train_index, ]
test_data  <- hr_clean[-train_index, ]

# ------------------------------------------------------------
# 4. Logistic Regression Model
# ------------------------------------------------------------

logistic_model <- glm(
  Attrition ~ Age +
    BusinessTravel +
    Department +
    DistanceFromHome +
    Education +
    EnvironmentSatisfaction +
    JobInvolvement +
    JobLevel +
    JobRole +
    JobSatisfaction +
    MaritalStatus +
    MonthlyIncome +
    NumCompaniesWorked +
    OverTime +
    PercentSalaryHike +
    PerformanceRating +
    RelationshipSatisfaction +
    StockOptionLevel +
    TotalWorkingYears +
    TrainingTimesLastYear +
    WorkLifeBalance +
    YearsAtCompany +
    YearsInCurrentRole +
    YearsSinceLastPromotion +
    YearsWithCurrManager,
  data = train_data,
  family = binomial
)

summary(logistic_model)

# ------------------------------------------------------------
# 5. Prediction
# ------------------------------------------------------------

pred_prob <- predict(
  logistic_model,
  newdata = test_data,
  type = "response"
)

pred_class <- ifelse(
  pred_prob >= 0.5,
  "Yes",
  "No"
)

pred_class <- factor(
  pred_class,
  levels = c("No", "Yes")
)

# ------------------------------------------------------------
# 6. Confusion Matrix
# ------------------------------------------------------------

conf_matrix <- confusionMatrix(
  pred_class,
  test_data$Attrition,
  positive = "Yes"
)

print(conf_matrix)

# ------------------------------------------------------------
# 7. ROC Curve and AUC
# ------------------------------------------------------------

roc_model <- roc(
  response = test_data$Attrition,
  predictor = pred_prob,
  levels = c("No", "Yes"),
  direction = "<"
)

auc_value <- auc(roc_model)

cat("ROC-AUC:", round(as.numeric(auc_value), 4), "\n")

# Save ROC curve
png(
  "week3/visualizations/01_roc_curve.png",
  width = 1000,
  height = 700
)

plot(
  roc_model,
  main = "ROC Curve - Employee Attrition Prediction",
  legacy.axes = TRUE
)

abline(
  a = 0,
  b = 1,
  lty = 2
)

dev.off()

# ------------------------------------------------------------
# 8. Confusion Matrix Visualization
# ------------------------------------------------------------

cm_table <- as.data.frame(
  table(
    Actual = test_data$Attrition,
    Predicted = pred_class
  )
)

cm_plot <- ggplot(
  cm_table,
  aes(
    x = Predicted,
    y = Actual,
    fill = Freq
  )
) +
  geom_tile() +
  geom_text(
    aes(label = Freq),
    size = 8
  ) +
  labs(
    title = "Confusion Matrix - Employee Attrition Prediction",
    x = "Predicted Attrition",
    y = "Actual Attrition",
    fill = "Count"
  ) +
  theme_minimal()

print(cm_plot)

ggsave(
  "week3/visualizations/02_confusion_matrix.png",
  cm_plot,
  width = 8,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------
# 9. Variable Importance
# ------------------------------------------------------------

model_coef <- summary(logistic_model)$coefficients

importance_df <- data.frame(
  Variable = rownames(model_coef),
  Estimate = model_coef[, "Estimate"]
)

importance_df <- importance_df[
  importance_df$Variable != "(Intercept)",
]

importance_df$Importance <- abs(
  importance_df$Estimate
)

importance_df <- importance_df[
  order(
    importance_df$Importance,
    decreasing = TRUE
  ),
]

top10_importance <- head(
  importance_df,
  10
)

importance_plot <- ggplot(
  top10_importance,
  aes(
    x = reorder(Variable, Importance),
    y = Importance
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 10 Variables Influencing Employee Attrition",
    x = "Variable",
    y = "Absolute Coefficient"
  ) +
  theme_minimal()

print(importance_plot)

ggsave(
  "week3/visualizations/03_variable_importance.png",
  importance_plot,
  width = 10,
  height = 7,
  dpi = 300
)

# ------------------------------------------------------------
# 10. Model Performance
# ------------------------------------------------------------

accuracy <- as.numeric(
  conf_matrix$overall["Accuracy"]
)

sensitivity <- as.numeric(
  conf_matrix$byClass["Sensitivity"]
)

specificity <- as.numeric(
  conf_matrix$byClass["Specificity"]
)

precision <- as.numeric(
  conf_matrix$byClass["Pos Pred Value"]
)

f1_score <- as.numeric(
  conf_matrix$byClass["F1"]
)

auc_score <- as.numeric(
  auc_value
)

performance_df <- data.frame(
  Metric = c(
    "Accuracy",
    "Sensitivity",
    "Specificity",
    "Precision",
    "F1 Score",
    "ROC-AUC"
  ),
  Value = c(
    accuracy,
    sensitivity,
    specificity,
    precision,
    f1_score,
    auc_score
  )
)

performance_df$Percentage <- round(
  performance_df$Value * 100,
  2
)

print(performance_df)

# Save performance results
write.csv(
  performance_df,
  "week3/model_performance.csv",
  row.names = FALSE
)

# ------------------------------------------------------------
# 11. Save Top Variables
# ------------------------------------------------------------

write.csv(
  importance_df,
  "week3/variable_importance.csv",
  row.names = FALSE
)

# ------------------------------------------------------------
# 12. Final Output Check
# ------------------------------------------------------------

cat("\n============================================\n")
cat("WEEK 3 ANALYSIS COMPLETED SUCCESSFULLY\n")
cat("============================================\n")

cat("Accuracy    :", round(accuracy * 100, 2), "%\n")
cat("Sensitivity :", round(sensitivity * 100, 2), "%\n")
cat("Specificity :", round(specificity * 100, 2), "%\n")
cat("Precision   :", round(precision * 100, 2), "%\n")
cat("F1 Score    :", round(f1_score * 100, 2), "%\n")
cat("ROC-AUC     :", round(auc_score, 4), "\n")

cat("\nFiles created:\n")

print(
  list.files(
    "week3",
    recursive = TRUE
  )
)