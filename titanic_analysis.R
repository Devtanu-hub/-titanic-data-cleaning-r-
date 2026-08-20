############################################################
# Week 1 Task: Data Cleaning & Preliminary Analysis in R
# Dataset : Titanic Passenger Data (titanic_raw.csv)
# Author  : Data Analytics Intern
############################################################

## 0. SETUP -------------------------------------------------
required_pkgs <- c("dplyr", "tidyr", "ggplot2", "corrplot", "naniar", "scales")
new_pkgs <- required_pkgs[!(required_pkgs %in% installed.packages()[,"Package"])]
if(length(new_pkgs)) install.packages(new_pkgs)

library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot)
library(naniar)
library(scales)

titanic <- read.csv("titanic_raw.csv", stringsAsFactors = FALSE)

## 1. INITIAL INSPECTION -------------------------------------
str(titanic)
summary(titanic)
dim(titanic)
head(titanic, 8)

## 2. MISSING VALUE ASSESSMENT --------------------------------
colSums(is.na(titanic))
round(colMeans(is.na(titanic)) * 100, 1)

# Visualize missingness
gg_miss_var(titanic) + labs(title = "Missing Values by Column")
ggsave("plots/01_missing_values.png", width = 7, height = 4.5)

## 3. DATA CLEANING --------------------------------------------

# 3a. Cabin has 75.5% missing -> too sparse to impute reliably.
#     Convert to a binary indicator instead of dropping the signal entirely.
titanic <- titanic %>%
  mutate(CabinKnown = ifelse(is.na(Cabin), 0, 1)) %>%
  select(-Cabin)

# 3b. Embarked has only 2 missing values -> impute with the mode
mode_embarked <- names(sort(table(titanic$Embarked), decreasing = TRUE))[1]
titanic$Embarked[is.na(titanic$Embarked)] <- mode_embarked

# 3c. Age has ~20% missing -> impute with the median WITHIN Pclass x Sex
#     subgroups (more accurate than a single global median)
titanic <- titanic %>%
  group_by(Pclass, Sex) %>%
  mutate(Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age)) %>%
  ungroup() %>%
  mutate(Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age)) # safety net

# Confirm no missing values remain
colSums(is.na(titanic))

## 4. OUTLIER DETECTION (IQR METHOD) ----------------------------
iqr_bounds <- function(x) {
  q1 <- quantile(x, 0.25); q3 <- quantile(x, 0.75)
  iqr <- q3 - q1
  c(lower = q1 - 1.5 * iqr, upper = q3 + 1.5 * iqr)
}

fare_bounds <- iqr_bounds(titanic$Fare)
age_bounds  <- iqr_bounds(titanic$Age)

n_fare_out <- sum(titanic$Fare < fare_bounds["lower"] | titanic$Fare > fare_bounds["upper"])
n_age_out  <- sum(titanic$Age  < age_bounds["lower"]  | titanic$Age  > age_bounds["upper"])

cat("Fare outliers:", n_fare_out, sprintf("(%.1f%%)\n", n_fare_out/nrow(titanic)*100))
cat("Age outliers :", n_age_out,  sprintf("(%.1f%%)\n", n_age_out/nrow(titanic)*100))

# Boxplots before/after treatment
ggplot(titanic, aes(y = Fare)) + geom_boxplot(fill = "#DD8452") +
  labs(title = "Fare - Before Capping")
ggsave("plots/02a_fare_before.png", width = 4, height = 4.5)

# Winsorize (cap) rather than delete, to preserve sample size
titanic$Fare_capped <- pmin(pmax(titanic$Fare, fare_bounds["lower"]), fare_bounds["upper"])

ggplot(titanic, aes(y = Fare_capped)) + geom_boxplot(fill = "#55A868") +
  labs(title = "Fare - After IQR Capping")
ggsave("plots/02b_fare_after.png", width = 4, height = 4.5)

## 5. NORMALIZATION ------------------------------------------
min_max <- function(x) (x - min(x)) / (max(x) - min(x))
z_score <- function(x) (x - mean(x)) / sd(x)

titanic <- titanic %>%
  mutate(
    Age_norm         = min_max(Age),
    Fare_capped_norm = min_max(Fare_capped),
    Age_z            = z_score(Age),
    Fare_capped_z     = z_score(Fare_capped)
  )

## 6. ENCODING CATEGORICAL VARIABLES ---------------------------
titanic <- titanic %>%
  mutate(
    Sex_encoded = ifelse(Sex == "female", 1, 0),
    Pclass      = factor(Pclass, levels = c(1,2,3), ordered = TRUE)
  )

embarked_dummies <- model.matrix(~ Embarked - 1, data = titanic)
titanic <- cbind(titanic, embarked_dummies)

write.csv(titanic, "titanic_cleaned.csv", row.names = FALSE)

## 7. EXPLORATORY DATA ANALYSIS --------------------------------

# 7a. Age distribution
ggplot(titanic, aes(x = Age)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "#4C72B0", alpha = 0.8) +
  geom_density(color = "black") +
  labs(title = "Distribution of Passenger Age (Post-Imputation)")
ggsave("plots/03_age_distribution.png", width = 7, height = 4.5)

# 7b. Survival rate by class & sex
surv_tab <- titanic %>%
  group_by(Pclass, Sex) %>%
  summarise(SurvivalRate = mean(Survived), .groups = "drop")

ggplot(surv_tab, aes(x = Pclass, y = SurvivalRate, fill = Sex)) +
  geom_col(position = "dodge") +
  labs(title = "Survival Rate by Passenger Class and Sex", y = "Survival Rate")
ggsave("plots/04_survival_by_class_sex.png", width = 7, height = 4.5)

# 7c. Correlation matrix
num_vars <- titanic %>%
  transmute(Survived, Pclass = as.numeric(Pclass), Age, SibSp, Parch,
            Fare_capped, CabinKnown)
corr_mat <- cor(num_vars)
round(corr_mat, 2)

png("plots/05_correlation_heatmap.png", width = 700, height = 600, res = 120)
corrplot(corr_mat, method = "color", addCoef.col = "black",
         type = "upper", tl.col = "black", number.cex = 0.8)
dev.off()

# 7d. Fare distribution by class
ggplot(titanic, aes(x = Pclass, y = Fare_capped, fill = Pclass)) +
  geom_boxplot() +
  labs(title = "Capped Fare Distribution by Passenger Class", y = "Fare (capped)")
ggsave("plots/06_fare_by_class.png", width = 7, height = 4.5)

# 7e. Embarkation counts
ggplot(titanic, aes(x = Embarked)) +
  geom_bar(fill = "#8172B2") +
  labs(title = "Passenger Count by Port of Embarkation")
ggsave("plots/07_embarked_counts.png", width = 7, height = 4.5)

## 8. KEY SUMMARY STATISTICS -----------------------------------
cat("Overall survival rate:", round(mean(titanic$Survived)*100,1), "%\n")
cat("Female survival rate :", round(mean(titanic$Survived[titanic$Sex=="female"])*100,1), "%\n")
cat("Male survival rate   :", round(mean(titanic$Survived[titanic$Sex=="male"])*100,1), "%\n")
cat("Class 1 survival rate:", round(mean(titanic$Survived[titanic$Pclass==1])*100,1), "%\n")
cat("Class 3 survival rate:", round(mean(titanic$Survived[titanic$Pclass==3])*100,1), "%\n")
cat("Median age  :", median(titanic$Age), "\n")
cat("Median fare :", median(titanic$Fare_capped), "\n")

############################################################
# END OF SCRIPT
############################################################
