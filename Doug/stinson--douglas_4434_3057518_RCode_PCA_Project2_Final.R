## Read in Data

Project_2_Data=read.csv(file.choose())

## Subset Traditional Dataset for PCA

Traditional <- cbind(Project_2_Data[0], Project_2_Data[5:12])

## Create Standardization Function

standardize <- function(x) {((x - mean(x))/sd(x))}

## Standardize Traditional

scaled.traditional = data.frame(apply(Traditional,2,function(x) ((x - mean(x))/sd(x))))

## Plot Pairwise Scatterplots of Traditional

pairs(Traditional)
pairs(scaled.traditional)

## Perform PCA on Traditional

trad.pca <- princomp(Traditional, scores=TRUE, cor=TRUE)
summary(trad.pca)

## Print Scree Plot for Traditional

plot(trad.pca)

## Print BiPlot for Traditional

biplot(trad.pca)

## Print Loading for Traditional

trad.pca$loadings

## Subset Imaging Dataset for PCA

Imaging <- cbind(Project_2_Data[0], Project_2_Data[13:27])

## Standardize Imaging

scaled.imaging = data.frame(apply(Imaging,2,function(x) ((x - mean(x))/sd(x))))

## Plot Pairwise Scatterplots of Imaging

pairs(Imaging)
pairs(scaled.imaging)

## Perform PCA on Imaging

imaging.pca <- princomp(Imaging, scores=TRUE, cor=TRUE)
summary(imaging.pca)

## Print Scree Plot for Imaging

plot(imaging.pca)

## Print BiPlot for Imaging

biplot(imaging.pca)

## Print Loading for Traditional

imaging.pca$loadings

## Create Data Frames for Traditional LM and PCR

Trad.FEV1pp <- cbind(Project_2_Data[0], Project_2_Data[5:12], Project_2_Data[28])

Trad.FVCpp <- cbind(Project_2_Data[0], Project_2_Data[5:12], Project_2_Data[29])

Trad.FEV1_FVC <- cbind(Project_2_Data[0], Project_2_Data[5:12], Project_2_Data[30])

## MSE Function 
mse <- function(error) {mean(error^2)} 

## RMSE Function
rmse <- function(error) {sqrt(mean(error^2))}

## lm using Trad.FVE1pp

trad.FEV1pp.lm <- lm(FEV1pp~., data=Trad.FEV1pp)
summary(trad.FEV1pp.lm)
mse(trad.FEV1pp.lm$residuals)
rmse(trad.FEV1pp.lm$residuals)

## lm using Trad.FVCpp

trad.FVCpp.lm <- lm(FVCpp~., data=Trad.FVCpp)
summary(trad.FVCpp.lm)
mse(trad.FVCpp.lm$residuals)
rmse(trad.FVCpp.lm$residuals)

## lm using Trad.FEV1_FVC

trad.FEV1_FVC.lm <- lm(FEV1_FVC~., data=Trad.FEV1_FVC)
summary(trad.FEV1_FVC.lm)
mse(trad.FEV1_FVC.lm$residuals)
rmse(trad.FEV1_FVC.lm$residuals)

## Create Data Frames for Imaging LM and PCR

Imaging.FEV1pp <- cbind(Project_2_Data[0], Project_2_Data[13:27], Project_2_Data[28])

Imaging.FVCpp <- cbind(Project_2_Data[0], Project_2_Data[13:27], Project_2_Data[29])

Imaging.FEV1_FVC <- cbind(Project_2_Data[0], Project_2_Data[13:27], Project_2_Data[30])

## lm using Imaging.FVE1pp

imaging.FEV1pp.lm <- lm(FEV1pp~., data=Imaging.FEV1pp)
summary(imaging.FEV1pp.lm)
mse(imaging.FEV1pp.lm$residuals)
rmse(imaging.FEV1pp.lm$residuals)

## lm using Imaging.FVCpp

imaging.FVCpp.lm <- lm(FVCpp~., data=Imaging.FVCpp)
summary(imaging.FVCpp.lm)
mse(imaging.FVCpp.lm$residuals)
rmse(imaging.FVCpp.lm$residuals)

## lm using Imaging.FEV1_FVC

imaging.FEV1_FVC.lm <- lm(FEV1_FVC~., data=Imaging.FEV1_FVC)
summary(imaging.FEV1_FVC.lm)
mse(imaging.FEV1_FVC.lm$residuals)
rmse(imaging.FEV1_FVC.lm$residuals)

## PCR using Trad.FEV1pp

library(pls)
pcr.trad.FEV1pp=pcr(Trad.FEV1pp$FEV1pp~., data=Trad.FEV1pp, scale=TRUE, validation="CV")
summary(pcr.trad.FEV1pp)
R2(pcr.trad.FEV1pp)
MSEP(pcr.trad.FEV1pp)
RMSEP(pcr.trad.FEV1pp)

validationplot(pcr.trad.FEV1pp, val.type="MSEP")

## PCR using Trad.FVCpp

pcr.trad.FVCpp=pcr(Trad.FVCpp$FVCpp~., data=Trad.FVCpp, scale=TRUE, validation="CV")
summary(pcr.trad.FVCpp)
R2(pcr.trad.FVCpp)
MSEP(pcr.trad.FVCpp)
RMSEP(pcr.trad.FVCpp)

validationplot(pcr.trad.FVCpp, val.type="MSEP")

## PCR using Trad.FEV1_FVC

pcr.trad.FEV1_FVC=pcr(Trad.FEV1_FVC$FEV1_FVC~., data=Trad.FEV1_FVC, scale=TRUE, validation="CV")
summary(pcr.trad.FEV1_FVC)
R2(pcr.trad.FEV1_FVC)
MSEP(pcr.trad.FEV1_FVC)
RMSEP(pcr.trad.FEV1_FVC)

validationplot(pcr.trad.FEV1_FVC, val.type="MSEP")

## PCR using Imaging.FEV1pp

pcr.imaging.FEV1pp=pcr(Imaging.FEV1pp$FEV1pp~., data=Imaging.FEV1pp, scale=TRUE, validation="CV")
summary(pcr.imaging.FEV1pp)
R2(pcr.imaging.FEV1pp)
MSEP(pcr.imaging.FEV1pp)
RMSEP(pcr.imaging.FEV1pp)

validationplot(pcr.imaging.FEV1pp, val.type="MSEP")

## PCR using Imaging.FVCpp

pcr.imaging.FVCpp=pcr(Imaging.FVCpp$FVCpp~., data=Imaging.FVCpp, scale=TRUE, validation="CV")
summary(pcr.imaging.FVCpp)
R2(pcr.imaging.FVCpp)
MSEP(pcr.imaging.FVCpp)
RMSEP(pcr.imaging.FVCpp)

validationplot(pcr.imaging.FVCpp, val.type="MSEP")

## PCR using Imaging.FEV1_FVC

pcr.imaging.FEV1_FVC=pcr(Imaging.FEV1_FVC$FEV1_FVC~., data=Imaging.FEV1_FVC, scale=TRUE, validation="CV")
summary(pcr.imaging.FEV1_FVC)
R2(pcr.imaging.FEV1_FVC)
MSEP(pcr.imaging.FEV1_FVC)
RMSEP(pcr.imaging.FEV1_FVC)

validationplot(pcr.imaging.FEV1_FVC, val.type="MSEP")