
library(e1071)        
library(modeest)     
library(ggplot2)      


options(scipen = 999)

data <- read.csv("C:/assingment 1/supervised_dataset_40.csv")

head(data)

hist(data$Age, main = "Histogram of Age", col = "skyblue", xlab = "Age")
hist(data$Income, main = "Histogram of Income", col = "lightgreen", xlab = "Income")
hist(data$Work_Hours, main = "Histogram of Work Hours", col = "salmon", xlab = "Work Hours")


plot(density(data$Age), main = "Density of Age", col = "blue", lwd = 2)
plot(density(data$Income), main = "Density of Income", col = "darkgreen", lwd = 2)
plot(density(data$Work_Hours), main = "Density of Work Hours", col = "red", lwd = 2)


age_density <- density(data$Age)
plot(age_density, main = "Age Distribution with Mean, Median, Mode", col = "purple", lwd = 2)
abline(v = mean(data$Age), col = "blue", lty = 2, lwd = 2)
abline(v = median(data$Age), col = "green", lty = 2, lwd = 2)
abline(v = mfv(data$Age), col = "red", lty = 2, lwd = 2)
legend("topright", legend = c("Mean", "Median", "Mode"),
       col = c("blue", "green", "red"), lty = 2, lwd = 2)
cat("\n--- Age ---\n")
cat("Mean   :", format(mean(data$Age), big.mark = ","), "\n")
cat("Median :", format(median(data$Age), big.mark = ","), "\n")
cat("Mode   :", format(mfv(data$Age), big.mark = ","), "\n")
age_skew <- skewness(data$Age)
cat("Skewness of Age:", round(age_skew, 3),
    ifelse(age_skew > 0.5, "→ Right-skewed",
           ifelse(age_skew < -0.5, "→ Left-skewed", "→ Approximately symmetric")), "\n")


income_density <- density(data$Income)
plot(income_density, main = "Income Distribution with Mean, Median, Mode", col = "purple", lwd = 2)
abline(v = mean(data$Income), col = "blue", lty = 2, lwd = 2)
abline(v = median(data$Income), col = "green", lty = 2, lwd = 2)
abline(v = mfv(data$Income), col = "red", lty = 2, lwd = 2)
legend("topright", legend = c("Mean", "Median", "Mode"),
       col = c("blue", "green", "red"), lty = 2, lwd = 2)
cat("\n--- Income ---\n")
cat("Mean   :", format(mean(data$Income), big.mark = ","), "\n")
cat("Median :", format(median(data$Income), big.mark = ","), "\n")
cat("Mode   :", format(mfv(data$Income), big.mark = ","), "\n")
income_skew <- skewness(data$Income)
cat("Skewness of Income:", round(income_skew, 3),
    ifelse(income_skew > 0.5, "→ Right-skewed",
           ifelse(income_skew < -0.5, "→ Left-skewed", "→ Approximately symmetric")), "\n")


hours_density <- density(data$Work_Hours)
plot(hours_density, main = "Work Hours Distribution with Mean, Median, Mode", col = "purple", lwd = 2)
abline(v = mean(data$Work_Hours), col = "blue", lty = 2, lwd = 2)
abline(v = median(data$Work_Hours), col = "green", lty = 2, lwd = 2)
abline(v = mfv(data$Work_Hours), col = "red", lty = 2, lwd = 2)
legend("topright", legend = c("Mean", "Median", "Mode"),
       col = c("blue", "green", "red"), lty = 2, lwd = 2)
cat("\n--- Work Hours ---\n")
cat("Mean   :", format(mean(data$Work_Hours), big.mark = ","), "\n")
cat("Median :", format(median(data$Work_Hours), big.mark = ","), "\n")
cat("Mode   :", format(mfv(data$Work_Hours), big.mark = ","), "\n")
hours_skew <- skewness(data$Work_Hours)
cat("Skewness of Work Hours:", round(hours_skew, 3),
    ifelse(hours_skew > 0.5, "→ Right-skewed",
           ifelse(hours_skew < -0.5, "→ Left-skewed", "→ Approximately symmetric")), "\n")


barplot(table(data$Education), col = "orange", main = "Education Levels", ylab = "Count")
barplot(table(data$Gender), col = "cyan", main = "Gender Distribution", ylab = "Count")
barplot(table(data$Purchase), col = "lightpink", main = "Purchase Decision", ylab = "Count")


ggplot(data, aes(x = Education, y = Income, fill = Education)) +
  geom_boxplot() +
  labs(title = "Box Plot: Income by Education",
       x = "Education", y = "Income") +
  theme_minimal()


plot(data$Age, data$Income, main = "Scatterplot: Age vs Income",
     xlab = "Age", ylab = "Income", col = "darkblue", pch = 19)


ggplot(data, aes(x = Purchase, y = Work_Hours, fill = Purchase)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white") +
  labs(title = "Violin Plot: Work Hours by Purchase",
       x = "Purchase", y = "Work Hours") +
  theme_minimal()
