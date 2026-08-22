
# ============================================================
# WEEK 2 - HR ATTRITION ANALYSIS
# R Data Analyst Internship
# ============================================================

# Load package
library(ggplot2)

# ------------------------------------------------------------
# 1. Check existing cleaned dataset
# ------------------------------------------------------------

dim(hr_clean)
str(hr_clean)

# ------------------------------------------------------------
# 2. Overall Attrition
# ------------------------------------------------------------

attrition_table <- table(hr_clean$Attrition)
attrition_percentage <- prop.table(attrition_table) * 100

print(attrition_table)
print(attrition_percentage)

# ------------------------------------------------------------
# 3. Department vs Attrition
# ------------------------------------------------------------

table_department <- table(
  hr_clean$Department,
  hr_clean$Attrition
)

print(table_department)

print(
  prop.table(table_department, margin = 1) * 100
)

print(chisq.test(table_department))

# ------------------------------------------------------------
# 4. OverTime vs Attrition
# ------------------------------------------------------------

table_overtime <- table(
  hr_clean$OverTime,
  hr_clean$Attrition
)

print(table_overtime)

print(
  prop.table(table_overtime, margin = 1) * 100
)

print(chisq.test(table_overtime))

# ------------------------------------------------------------
# 5. BusinessTravel vs Attrition
# ------------------------------------------------------------

table_travel <- table(
  hr_clean$BusinessTravel,
  hr_clean$Attrition
)

print(table_travel)

print(
  prop.table(table_travel, margin = 1) * 100
)

print(chisq.test(table_travel))

# ------------------------------------------------------------
# 6. JobRole vs Attrition
# ------------------------------------------------------------

table_jobrole <- table(
  hr_clean$JobRole,
  hr_clean$Attrition
)

print(table_jobrole)

print(
  prop.table(table_jobrole, margin = 1) * 100
)

print(chisq.test(table_jobrole))

# ------------------------------------------------------------
# 7. MaritalStatus vs Attrition
# ------------------------------------------------------------

table_marital <- table(
  hr_clean$MaritalStatus,
  hr_clean$Attrition
)

print(table_marital)

print(
  prop.table(table_marital, margin = 1) * 100
)

print(chisq.test(table_marital))

# ------------------------------------------------------------
# 8. Gender vs Attrition
# ------------------------------------------------------------

table_gender <- table(
  hr_clean$Gender,
  hr_clean$Attrition
)

print(table_gender)

print(
  prop.table(table_gender, margin = 1) * 100
)

print(chisq.test(table_gender))

# ------------------------------------------------------------
# 9. Numerical Variables vs Attrition
# ------------------------------------------------------------

print(
  t.test(
    JobSatisfaction ~ Attrition,
    data = hr_clean
  )
)

print(
  t.test(
    WorkLifeBalance ~ Attrition,
    data = hr_clean
  )
)

print(
  t.test(
    Age ~ Attrition,
    data = hr_clean
  )
)

print(
  t.test(
    YearsAtCompany ~ Attrition,
    data = hr_clean
  )
)

print(
  t.test(
    TotalWorkingYears ~ Attrition,
    data = hr_clean
  )
)

print(
  t.test(
    MonthlyIncome ~ Attrition,
    data = hr_clean
  )
)

# ------------------------------------------------------------
# 10. Group Means
# ------------------------------------------------------------

print(
  tapply(
    hr_clean$MonthlyIncome,
    hr_clean$Attrition,
    mean
  )
)

print(
  tapply(
    hr_clean$MonthlyIncome,
    hr_clean$Attrition,
    median
  )
)

print(
  tapply(
    hr_clean$Age,
    hr_clean$Attrition,
    mean
  )
)

print(
  tapply(
    hr_clean$WorkLifeBalance,
    hr_clean$Attrition,
    mean
  )
)

# ------------------------------------------------------------
# 11. Correlation Analysis
# ------------------------------------------------------------

correlation_data <- hr_clean[, c(
  "Age",
  "MonthlyIncome",
  "TotalWorkingYears",
  "YearsAtCompany",
  "YearsInCurrentRole",
  "YearsSinceLastPromotion",
  "YearsWithCurrManager"
)]

correlation_matrix <- cor(correlation_data)

print(
  round(correlation_matrix, 2)
)

# ------------------------------------------------------------
# 12. Week 2 Key Findings
# ------------------------------------------------------------

cat("\n================ WEEK 2 KEY FINDINGS ================\n")

cat("\nOverall Attrition Rate:\n")
cat("16.12% of employees left the company.\n")

cat("\nOverTime:\n")
cat("Employees working overtime had 30.53% attrition.\n")
cat("Employees not working overtime had 10.44% attrition.\n")

cat("\nDepartment:\n")
cat("Sales had 20.63% attrition.\n")
cat("Human Resources had 19.05% attrition.\n")
cat("Research & Development had 13.84% attrition.\n")

cat("\nBusiness Travel:\n")
cat("Frequent travelers had 24.91% attrition.\n")

cat("\nJob Role:\n")
cat("Sales Representatives had the highest attrition at 39.76%.\n")

cat("\nMarital Status:\n")
cat("Single employees had 25.53% attrition.\n")

cat("\nJob Satisfaction:\n")
cat("Mean satisfaction for employees who stayed: 2.78.\n")
cat("Mean satisfaction for employees who left: 2.47.\n")

cat("\nWork-Life Balance:\n")
cat("Mean work-life balance for employees who stayed: 2.78.\n")
cat("Mean work-life balance for employees who left: 2.66.\n")

cat("\nAge:\n")
cat("Mean age for employees who stayed: 37.56.\n")
cat("Mean age for employees who left: 33.61.\n")

cat("\nYears at Company:\n")
cat("Mean years for employees who stayed: 7.37.\n")
cat("Mean years for employees who left: 5.13.\n")

cat("\nTotal Working Years:\n")
cat("Mean working years for employees who stayed: 11.86.\n")
cat("Mean working years for employees who left: 8.24.\n")

cat("\n======================================================\n")

