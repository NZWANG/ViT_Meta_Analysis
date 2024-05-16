## This code plot the funnelplot of a diagnostic device##
## Require you to load metafor library##
library(metafor)
Id <- c("Alejandro et al. 2023","Odusam et al. 2023_1","Odusam et al. 2023_2","Zhu et al. 2022","Tang et al. 2023_1","Tang et al. 2023_2","Tang et al. 2023_3","Liu et al. 2023",
	"Dhinagar et al. 2023","Pan et al. 2022","Khatri et al. 2024_1","Khatri et al. 2024_2","Khan et al. 2024","Aghdam et al. 2024_1","Aghdam et al. 2024_2","Huang et al. 2023")

TP <- c(89,99,94,82,98,92,97,99,83,94,88,91,98,96,97,71)


FP <- c(26,1,3,12,1,2,2,1,16,5,5,1,2,3,3,2)

FN <- c(11,1,6,18,2,8,3,1,17,6,12,9,2,4,3,29)

TN <- c(74,99,97,88,99,98,98,99,84,95,95,99,98,97,97,98)


##Model 0=ViT model alone; 1= Hybrid of ViT##
##g <- c(0,0,0,0,1,1,1,1,0,1,0,1,1,0,1,1)
dta_ViT <- data.frame(Id,TP,FP,FN,TN)
### calculate log risk ratios and corresponding sampling variances
dta_ViT <- escalc(measure="RR", ai=TP, bi=TN, ci=FP, di=FN, data= dta_ViT)
### random-effects model
res <- rma(yi, vi, data= dta_ViT)
### standard funnel plot
funnel(res)

