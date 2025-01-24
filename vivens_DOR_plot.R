## This code plot the forestplot for Diagnostic Odd Ratio (DOR) of a diagnostic device##
detach("package:mada", unload = TRUE)
library(meta)
Id <- c("Alejandro et al 2023","Odusam et al 2023_1","Odusam et al 2023_2","Zhu et al 2022","Tang et al 2023_1","Tang et al 2023_2","Tang et al 2023_3","Liu et al 2023",
        "Dhinagar et al 2023","Pan et al 2022","Khatri et al 2024_1","Khatri et al 2024_2","Khan et al 2024","Aghdam et al 2024_1","Aghdam et al 2024_2","Huang et al 2023")

TP <- c(89,99,94,82,98,92,97,99,83,94,88,91,98,96,97,71)


FP <- c(26,1,3,12,1,2,2,1,16,5,5,1,2,3,3,2)

FN <- c(11,1,6,18,2,8,3,1,17,6,12,9,2,4,3,29)

TN <- c(74,99,97,88,99,98,98,99,84,95,95,99,98,97,97,98)

## from Shim, S. R., Kim, S. J., & Lee, J. (2019). Diagnostic test accuracy: application and practice using R software. Epidemiology and health, 41, e2019007. https://doi.org/10.4178/epih.e2019007##
##Model 0=ViT model alone; 1= Hybrid of ViT##
g <- c(0,0,0,0,1,1,1,1,0,1,0,1,1,0,1,1)

##dta_ViT <- data.frame(Id,TP,FP,FN,TN)
dta_ViT <- data.frame(Id,TP,FP,FN,TN,g)
## Perform meta-analysis for DOR
DOR_model <- metabin(
  TP, TP + FP, FN, FN + TN, 
  sm = "OR",                          # Odds Ratio (logit scale)
  comb.fixed = FALSE,                 # Disable fixed effects model
  comb.random = TRUE,                 # Enable random effects model
  method = "Inverse",                 # Inverse variance method
  studlab = Id,                       # Study labels
  byvar = g,                          # Subgroup analysis by g
  data = dta_ViT
)

## Print model summary
print(DOR_model)

## Plot forest plot with colors
forest(
  DOR_model,
  digits = 5,
  rightcols = c("effect", "ci"),
  xlab = "Diagnostic Odds Ratio",
  col.square = "black",               # Individual study estimates in black
  col.diamond = "blue",               # Pooled estimate in blue
  col.diamond.lines = "black",        # Diamond lines in black
  refline = 1                         # Vertical dotted line at 1 (neutral value for odds ratio)
)

## Perform meta-regression
metareg(DOR_model, g, method.tau = "REML", digits = 3)
