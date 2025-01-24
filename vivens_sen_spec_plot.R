## Load necessary libraries
detach("package:mada", unload = TRUE)
library(meta)

## Define data
Id <- c("Alejandro et al 2023", "Odusam et al 2023_1", "Odusam et al 2023_2", "Zhu et al 2022", 
        "Tang et al 2023_1", "Tang et al 2023_2", "Tang et al 2023_3", "Liu et al 2023",
        "Dhinagar et al 2023", "Pan et al 2022", "Khatri et al 2024_1", "Khatri et al 2024_2", 
        "Khan et al 2024", "Aghdam et al 2024_1", "Aghdam et al 2024_2", "Huang et al 2023")

TP <- c(89, 99, 94, 82, 98, 92, 97, 99, 83, 94, 88, 91, 98, 96, 97, 71)
FP <- c(26, 1, 3, 12, 1, 2, 2, 1, 16, 5, 5, 1, 2, 3, 3, 2)
FN <- c(11, 1, 6, 18, 2, 8, 3, 1, 17, 6, 12, 9, 2, 4, 3, 29)
TN <- c(74, 99, 97, 88, 99, 98, 98, 99, 84, 95, 95, 99, 98, 97, 97, 98)

## Add a continuity correction to prevent zero proportions
TP <- TP + 0.0000001 # 0.5
FN <- FN + 0.0000001
FP <- FP + 0.0000001
TN <- TN + 0.0000001
g <- c(0,0,0,0,1,1,1,1,0,1,0,1,1,0,1,1)

## Create data frame
##dta_ViT <- data.frame(Id, TP, FP, FN, TN)

dta_ViT <- data.frame(Id,TP,FP,FN,TN,g)

## Sensitivity analysis (use the "PRAW" model for raw proportions)
sensitivity_logit <- metaprop(
  event = dta_ViT$TP,
  n = dta_ViT$TP + dta_ViT$FN,
  sm = "PRAW",  # Raw proportion for sensitivity
  method.ci = "NAsm",  # Normal approximation confidence intervals
  studlab = dta_ViT$Id, 
  byvar=dta_ViT$g,    # This function introduce sub-group-analysis
  comb.fixed = FALSE,  # Disable fixed effects model
  comb.random = TRUE,  # Enable random effects model
  verbose = TRUE
)

## Print sensitivity results
print(sensitivity_logit, digits = 3)

## Plot forest plot for sensitivity
forest(
  sensitivity_logit,
  digits = 3,
  rightcols = c("effect", "ci"),
  xlab = "Sensitivity",
  col.square = "black",
  col.diamond = "darkblue",
  col.diamond.lines = "black",
  digits.se = 3,
  digits.zval = 3,
  digits.pval = 3,
  pval.method = "italic",
  label.left = "Lower limit",
  label.right = "Upper limit",
  overall = TRUE,  # Hide fixed effect model
  refline = 0.94  # Add vertical dotted line at 0.94 (example value; replace as needed)
)

## Specificity analysis (use the "PRAW" model for raw proportions)
specificity_logit <- metaprop(
  event = dta_ViT$TN,
  n = dta_ViT$TN + dta_ViT$FP,
  sm = "PRAW",  # Raw proportion for specificity
  method.ci = "NAsm",  # Normal approximation confidence intervals
  studlab = dta_ViT$Id,
  byvar=dta_ViT$g,    # This function introduce sub-group-analysis
  comb.fixed = FALSE,  # Disable fixed effects model
  comb.random = TRUE,  # Enable random effects model
  verbose = TRUE
)

## Print specificity results
print(specificity_logit, digits = 3)

## Plot forest plot for specificity
forest(
  specificity_logit,
  digits = 3,
  rightcols = c("effect", "ci"),
  xlab = "Specificity",
  col.square = "black",
  col.diamond = "darkblue",
  col.diamond.lines = "black",
  digits.se = 3,
  digits.zval = 3,
  digits.pval = 3,
  pval.method = "italic",
  label.left = "Lower limit",
  label.right = "Upper limit",
  overall = TRUE,  # Hide fixed effect model
  refline = 0.94  # Add vertical dotted line at 0.94 (example value; replace as needed)
)

