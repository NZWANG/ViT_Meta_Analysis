## This code plot the forestplot for both Sensitivity and Specificity of a diagnostic device ##
library(meta)
Id <- c("Alejandro et al. 2023","Odusam et al. 2023_1","Odusam et al. 2023_2","Zhu et al. 2022","Tang et al. 2023_1","Tang et al. 2023_2","Tang et al. 2023_3","Liu et al. 2023",
	"Dhinagar et al. 2023","Pan et al. 2022","Khatri et al. 2024_1","Khatri et al. 2024_2","Khan et al. 2024","Aghdam et al. 2024_1","Aghdam et al. 2024_2","Huang et al. 2023")

TP <- c(89,99,94,82,98,92,97,99,83,94,88,91,98,96,97,71)


FP <- c(26,1,3,12,1,2,2,1,16,5,5,1,2,3,3,2)

FN <- c(11,1,6,18,2,8,3,1,17,6,12,9,2,4,3,29)

TN <- c(74,99,97,88,99,98,98,99,84,95,95,99,98,97,97,98)

## from Shim, S. R., Kim, S. J., & Lee, J. (2019). Diagnostic test accuracy: application and practice using R software. Epidemiology and health, 41, e2019007. https://doi.org/10.4178/epih.e2019007##
##Model 0=ViT model alone; 1= Hybrid of ViT##
g <- c(0,0,0,0,1,1,1,1,0,1,0,1,1,0,1,1)

#dta_ViT <- data.frame(Id,TP,FP,FN,TN)
dta_ViT <- data.frame(Id,TP,FP,FN,TN,g)
sensitivity_logit <- metaprop(dta_ViT$TP, dta_ViT$TP+ dta_ViT$FN, 
                              comb.fixed=FALSE,comb.random=TRUE,sm="PLOGIT",method.ci="CP",
                              studlab=dta_ViT$Id,byvar=dta_ViT$g)
print(sensitivity_logit, digits=3)
forest(sensitivity_logit, digits=3,rightcols=c("effect","ci"),xlab="Sensitivity")

specificity_logit <- metaprop(dta_ViT$TN, dta_ViT$TN+ dta_ViT$FP, 
                              comb.fixed=FALSE,comb.random=TRUE,sm="PLOGIT",method="Inverse",method.ci="CP",
                              studlab=dta_ViT$Id,byvar=dta_ViT$g)
print(specificity_logit, digits=3)
forest(specificity_logit, digits=3,rightcols=c("effect","ci"),xlab="Specificity")

