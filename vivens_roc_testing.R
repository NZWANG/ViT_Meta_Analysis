# Load required libraries
library(pROC)
library(ggplot2)

# Generate Sample Data
set.seed(123)
n <- 1000
data <- data.frame(
  Actual = sample(c(0, 1), n, replace = TRUE),
  SVM = runif(n),
  NB = runif(n),
  RF = runif(n),
  MLP = runif(n),
  XGB = runif(n),
  LR = runif(n),
  GBDT = runif(n),
  AdaBoost = runif(n),
  DecisionTree = runif(n),
  AWS_OWS = runif(n)
)

# Calculate ROC and AUC for each model
models <- colnames(data)[-1]
roc_curves <- lapply(models, function(model) {
  roc_obj <- roc(data$Actual, data[[model]])
  list(
    model = model,
    roc = roc_obj,
    auc = auc(roc_obj)
  )
})

# Plot ROC Curves
plot(roc_curves[[1]]$roc, col = "red", lwd = 2, main = "Receiver Operating Characteristic", legacy.axes = TRUE)
for (i in 2:length(roc_curves)) {
  plot(roc_curves[[i]]$roc, col = rainbow(length(roc_curves))[i], lwd = 2, add = TRUE)
}
abline(a = 0, b = 1, col = "blue", lty = 2)

# Add legend
legend("bottomright", legend = sapply(roc_curves, function(x) paste0("ROC curve (AUC_", x$model, " = ", round(x$auc, 3), ")")), 
       col = c("red", rainbow(length(roc_curves))), lwd = 2, cex = 0.8)
