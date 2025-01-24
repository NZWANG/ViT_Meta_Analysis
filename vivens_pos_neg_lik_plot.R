## Load necessary library
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

## Model: 0=ViT model alone; 1=Hybrid of ViT
g <- c(0, 0, 0, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 1, 1)

## Create data frame
dta_ViT <- data.frame(Id, TP, FP, FN, TN, g)

## Calculate sensitivity and specificity
Sensitivity <- TP / (TP + FN)
Specificity <- TN / (TN + FP)

## Calculate positive and negative likelihood ratios
LR_Positive <- Sensitivity / (1 - Specificity)
LR_Negative <- (1 - Sensitivity) / Specificity

## Add calculated values to the data frame
dta_ViT$LR_Positive <- LR_Positive
dta_ViT$LR_Negative <- LR_Negative

## Meta-analysis for positive likelihood ratio
meta_LR_Positive <- metagen(
  TE=log(dta_ViT$LR_Positive), # Log-transform for meta-analysis
  seTE=sqrt((1 / TP) + (1 / FN) + (1 / FP) + (1 / TN)), # Standard error for log(LR+)
  studlab=Id,
  comb.fixed=FALSE,
  comb.random=TRUE,
  sm="RR" # Log risk ratio corresponds to log LR+ in this context
)

## Meta-analysis for negative likelihood ratio
meta_LR_Negative <- metagen(
  TE=log(dta_ViT$LR_Negative), # Log-transform for meta-analysis
  seTE=sqrt((1 / TP) + (1 / FN) + (1 / FP) + (1 / TN)), # Standard error for log(LR-)
  studlab=Id,
  comb.fixed=FALSE,
  comb.random=TRUE,
  sm="RR" # Log risk ratio corresponds to log LR-
)

## Print meta-analysis results for LR+
print(meta_LR_Positive, digits=3)

## Print meta-analysis results for LR-
print(meta_LR_Negative, digits=3)

## Forest plot for positive likelihood ratio
forest(
  meta_LR_Positive,
  xlab="Positive LR",
  digits=3,
  col.square="purple",
  col.diamond="magenta",
  col.diamond.lines="black",
  label.left="Lower limit",
  label.right="Upper limit"
)

## Forest plot for negative likelihood ratio
forest(
  meta_LR_Negative,
  xlab="Negative LR",
  digits=3,
  col.square="purple",
  col.diamond="magenta",
  col.diamond.lines="black",
  label.left="Lower limit",
  label.right="Upper limit"
)

