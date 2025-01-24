## Install and load the mada package
if (!requireNamespace("mada", quietly = TRUE)) {
  install.packages("mada")
}
library(mada)

## Define data
Id <- c("Alejandro et al 2023", "Odusam et al 2023_1", "Odusam et al 2023_2", "Zhu et al 2022", 
        "Tang et al 2023_1", "Tang et al 2023_2", "Tang et al 2023_3", "Liu et al 2023",
        "Dhinagar et al 2023", "Pan et al 2022", "Khatri et al 2024_1", "Khatri et al 2024_2", 
        "Khan et al 2024", "Aghdam et al 2024_1", "Aghdam et al 2024_2", "Huang et al 2023")

TP <- c(89, 99, 94, 82, 98, 92, 97, 99, 83, 94, 88, 91, 98, 96, 97, 71)
FP <- c(26, 1, 3, 12, 1, 2, 2, 1, 16, 5, 5, 1, 2, 3, 3, 2)
FN <- c(11, 1, 6, 18, 2, 8, 3, 1, 17, 6, 12, 9, 2, 4, 3, 29)
TN <- c(74, 99, 97, 88, 99, 98, 98, 99, 84, 95, 95, 99, 98, 97, 97, 98)

## Create a data frame for mada
mada_data <- data.frame(TP = TP, FN = FN, FP = FP, TN = TN)

## Perform bivariate meta-analysis using mada
meta_analysis <- mada::reitsma(mada_data)

## Calculate sensitivity and specificity
specificity <- TN / (TN + FP)
sensitivity <- TP / (TP + FN)
one_minus_specificity <- 1 - specificity

## Initialize the plot
plot(
  one_minus_specificity, sensitivity, 
  xlim = c(0, 1), ylim = c(0, 1), 
  xlab = "1 - specificity", ylab = "Sensitivity", 
  main = "SROC Curve", pch = 19, col = "green"
)

## Add the SROC curve
curve_points <- mada::sroc(meta_analysis, type = "ruttergatsonis")  # Calculate SROC points
lines(curve_points[, 1], curve_points[, 2], col = "blue", lwd = 2)  # Add the curve

## Add AUC and SE(AUC) to the top-right corner
vcov_matrix <- vcov(meta_analysis)
logit_sens_var <- vcov_matrix[1, 1]
logit_spec_var <- vcov_matrix[2, 2]
auc_value <- exp(-0.5 * (logit_sens_var + logit_spec_var))
auc_se <- sqrt((auc_value^2) * (logit_sens_var + logit_spec_var) / 2)

## Display AUC and SE(AUC)
text(
  x = 0.95, y = 0.95, 
  labels = paste0("AUC = ", round(auc_value, 4), "\nSE(AUC) = ", round(auc_se, 4)), 
  adj = c(1, 1), cex = 1.2, col = "black"
)

