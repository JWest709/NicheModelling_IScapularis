# This is the standard workflow within RStudio for sequence seven of the primary dataset 

install.package('plyr')
install.package('usdm')
install.package('caret')
install.package('biomod2') 
install.package('gridExtra')
install.package('dplyr')
install.package('CENFA')
install.package('ENMeval')
install.package('sdm')
installAll()

library(sdm)
library(rgdal)
library(raster)
library(plyr)
library(usdm)
library(caret)
library(biomod2)
library(ENMeval)
library(gridExtra)
library(dplyr)
library(CENFA)
library(corrplot)


Ixodes_Scapularis_Shapefile <- readOGR("C:/Users/jwestcot/AGIS9000/Ixodes_Scapularis/Shapefile","Ixodes_Scapularis")
Raster_Stack <- stack(List_Rasters)
vifcor_Rasters <- vifcor(Raster_Stack, th = 0.7)
Rasters_Present <- exclude(Raster_Stack,vifcor_Rasters) 
PearsonR <- layerStats(Raster_Stack, 'pearson', na.rm=T)
Corr_Matrix = PearsonR$'pearson correlation coefficient'
sdmData_GLM_GAM_GLMPoly <- sdmData(formula = I_Scap~., train= Ixodes_Scapularis_Shapefile, predictors=Rasters_Present, bg = list(n=10000, method='gRandom'))
IScap_Shapefile_Turned_DF <- as.data.frame(Ixodes_Scapularis_Shapefile)
Background_Grid_MDA_FDA <- BIOMOD_FormatingData(resp.var = rep(1, nrow(IScap_Shapefile_Turned_DF)),
                                                expl.var = Rasters_Present,
                                                resp.xy = IScap_Shapefile_Turned_DF[,c("coords.x1","coords.x2")], # colnames of coords in DF
                                                resp.name = "Ixodes_Scapularis", # species name in DF
                                                PA.strategy = "sre",
                                                PA.nb.rep = 1,
                                                PA.nb.absences = 1100,
                                                na.rm = FALSE)


Background_MDA_FDA <- cbind(sp = c(rep(1,nrow(IScap_Shapefile_Turned_DF)),rep(0, 1100)), Background_Grid_MDA_FDA@coord)


names(Background_MDA_FDA)[1] <- names(IScap_Shapefile_Turned_DF)[1]


coordinates(Background_MDA_FDA) <- 2:3
sdmData_MDA_FDA <- sdmData(formula = I_Scap~., train= Background_MDA_FDA)
Background_Grid_MARS <- BIOMOD_FormatingData(resp.var = rep(1, nrow(IScap_Shapefile_Turned_DF)),
                                             expl.var = Rasters_Present,
                                             resp.xy = IScap_Shapefile_Turned_DF[,c("coords.x1","coords.x2")], 
                                             resp.name = "Ixodes_Scapularis",
                                             PA.strategy = "disk",
                                             PA.dist.min = 220000,
                                             PA.nb.rep = 1,
                                             PA.nb.absences = 1100,
                                             na.rm = FALSE)


Background_MARS <- cbind(sp = c(rep(1,nrow(IScap_Shapefile_Turned_DF)),rep(0, 1100)), Background_Grid_MARS@coord)


names(Background_MARS)[1] <- names(IScap_Shapefile_Turned_DF)[1]


coordinates(Background_MARS) <- 2:3
sdmData_MARS <- sdmData(formula = I_Scap~., train= Background_MARS)
Background_Grid_RPART_CART_BRT_RF_SVM_RBF_MLP <- BIOMOD_FormatingData(resp.var = rep(1, nrow(IScap_Shapefile_Turned_DF)),
                                                                      expl.var = Rasters_Present,
                                                                      resp.xy = IScap_Shapefile_Turned_DF[,c("coords.x1","coords.x2")], 
                                                                      resp.name = "Ixodes_Scapularis",
                                                                      PA.strategy = "sre",
                                                                      PA.nb.rep = 1,
                                                                      PA.nb.absences = 3425,
                                                                      na.rm = FALSE)


Background_RPART_CART_BRT_RF_SVM_RBF_MLP <- cbind(sp = c(rep(1,nrow(IScap_Shapefile_Turned_DF)),rep(0, 3425)), Background_Grid_RPART_CART_BRT_RF_SVM_RBF_MLP@coord)


names(Background_RPART_CART_BRT_RF_SVM_RBF_MLP)[1] <- names(IScap_Shapefile_Turned_DF)[1]


coordinates(Background_RPART_CART_BRT_RF_SVM_RBF_MLP) <- 2:3
sdmData_RPART_CART_BRT_RF_SVM_RBF_MLP <- sdmData(formula = I_Scap~., train= Background_RPART_CART_BRT_RF_SVM_RBF_MLP)
DF_GLM_GAM_GLMPoly <- as.data.frame(sdmData_GLM_GAM_GLMPoly)


DF_MDA_FDA <- as.data.frame(sdmData_MDA_FDA)


DF_MARS <- as.data.frame(sdmData_MARS)


DF_RPART_CART_BRT_RF_SVM_RBF_MLP <- as.data.frame(sdmDaa_RPART_CART_BRT_RF_SVM_RBF_MLP)


DF_GLM_GAM_GLMPoly <- subset(DF_GLM_GAM_GLMPoly, select = -rID)
DF_GLM_GAM_GLMPoly <- subset(DF_GLM_GAM_GLMPoly, select = -coords.x1)
DF_GLM_GAM_GLMPoly <- subset(DF_GLM_GAM_GLMPoly, select = -coords.x2)


DF_MDA_FDA <- subset(DF_MDA_FDA, select = -rID)
DF_MDA_FDA <- subset(DF_MDA_FDA, select = -coords.x1)
DF_MDA_FDA <- subset(DF_MDA_FDA, select = -coords.x2)


DF_MARS <- subset(DF_MARS, select = -rID)
DF_MARS <- subset(DF_MARS, select = -coords.x1)
DF_MARS <- subset(DF_MARS, select = -coords.x2)


DF_RPART_CART_BRT_RF_SVM_RBF_MLP <- subset(DF_RPART_CART_BRT_RF_SVM_RBF_MLP, select = -rID)
DF_RPART_CART_BRT_RF_SVM_RBF_MLP <- subset(DF_RPART_CART_BRT_RF_SVM_RBF_MLP, select = -coords.x1)
DF_RPART_CART_BRT_RF_SVM_RBF_MLP <- subset(DF_RPART_CART_BRT_RF_SVM_RBF_MLP, select = -coords.x2)


DF_GLM_GAM_GLMPoly$Ixodes_Scapularis <- as.factor(DF_GLM_GAM_GLMPoly$Ixodes_Scapularis)
DF_MDA_FDA$Ixodes_Scapularis <- as.factor(DF_MDA_FDA$Ixodes_Scapularis)
DF_MARS$Ixodes_Scapularis <- as.factor(DF_MARS$Ixodes_Scapularis)
DF_RPART_CART_BRT_RF_SVM_RBF_MLP$Ixodes_Scapularis <- as.factor(DF_RPART_CART_BRT_RF_SVM_RBF_MLP$Ixodes_Scapularis)


levels(DF_GLM_GAM_GLMPoly$Ixodes_Scapularis) <- c("A", "P") # A = Absence, P = Presence
levels(DF_MDA_FDA$Ixodes_Scapularis) <- c("A", "P")
levels(DF_MARS$Ixodes_Scapularis) <- c("A", "P")
levels(DF_RPART_CART_BRT_RF_SVM_RBF_MLP$Ixodes_Scapularis) <- c("A", "P")


fitControl <- trainControl(method = "repeatedcv",
                           number = 5,
                           repeats = 8,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary,
                           search = "random") 


GAM_Fit <- caret::train(Ixodes_Scapularis~., data = DF_GLM_GAM_GLMPoly, 
                        method = "gam",
                        metric = "ROC",
                        tuneLength = 50,
                        trControl = fitControl)
GLMPoly_Fit <- caret::train(Ixodes_Scapularis~., data = DF_GLM_GAM_GLMPoly, 
                            method = "gam",
                            metric = "ROC",
                            tuneLength = 50,
                            trControl = fitControl)


MDA_Fit <- caret::train(Ixodes_Scapularis~., data = DF_MDA_FDA, 
                        method = "mda",
                        metric = "ROC",
                        tuneLength = 50,
                        trControl = fitControl)
FDA_Fit <- caret::train(Ixodes_Scapularis~., data = DF_MDA_FDA, 
                        method = "fda",
                        metric = "ROC",
                        tuneLength = 50,
                        trControl = fitControl)


MARS_Fit <- caret::train(Ixodes_Scapularis~., data = DF_MARS,, 
                         method = "bagEarth",
                         metric = "ROC",
                         tuneLength = 50,
                         trControl = fitControl)


RPART_Fit <- caret::train(Ixodes_Scapularis~., data = DF_RPART_CART_BRT_RF_SVM_RBF_MLP, 
                          method = "rpart",
                          metric = "ROC",
                          tuneLength = 50,
                          trControl = fitControl)
BRT_Fit <- caret::train(Ixodes_Scapularis~., data = DF_RPART_CART_BRT_RF_SVM_RBF_MLP, 
                        method = "gbm",
                        metric = "ROC",
                        tuneLength = 50,
                        trControl = fitControl)
RF_Fit <- caret::train(Ixodes_Scapularis~., data = DF_RPART_CART_BRT_RF_SVM_RBF_MLP, 
                       method = "RRF",
                       metric = "ROC",
                       tuneLength = 50,
                       trControl = fitControl)
SVM_Fit <- caret::train(Ixodes_Scapularis~., data = DF_RPART_CART_BRT_RF_SVM_RBF_MLP, 
                        method = "svmPoly",
                        metric = "ROC",
                        tuneLength = 50,
                        trControl = fitControl)
RBF_Fit <- caret::train(Ixodes_Scapularis~., data = DF_RPART_CART_BRT_RF_SVM_RBF_MLP, 
                        method = "rbfDDA",
                        metric = "ROC",
                        tuneLength = 50,
                        trControl = fitControl)
MLP_Fit <- caret::train(Ixodes_Scapularis~., data = DF_RPART_CART_BRT_RF_SVM_RBF_MLP, 
                        method = "mlpML",
                        metric = "ROC",
                        tuneLength = 50,
                        trControl = fitControl)


SDM_GLM_GAM_GLMPoly <- sdm(Ixodes_Scapularis~.,data=sdmData_GLM_GAM_GLMPoly,methods=c('glm','gam','glmpoly'), replication = c('sub','boot','cv'), cv.folds = 5,n=4,test.percent=20, modelSettings = list(gam=list(select = FALSE, method = 'GCV.Cp')))

SDM_MARS <- sdm(Ixodes_Scapularis~.,data=sdmData_MARS,methods=c('mars'), replication = c('sub','boot','cv'), cv.folds = 5,n=4,test.percent=20, modelSettings = list(mars=list(nprune = 2, degree = 2)))

SDM_MDA_FDA <- sdm(Ixodes_Scapularis~.,data=sdmData_MDA_FDA,methods=c('mda','fda'), replication = c('sub','boot','cv'), cv.folds = 5,n=4,test.percent=20, modelSettings = list(mda=list(subclasses = 47),fda = list(degree = 2, nprune = 13)))

SDM_RPART_CART_BRT_RF_SVM_RBF_MLP <- sdm(Ixodes_Scapularis~.,data=sdmData_RPART_CART_BRT_RF_SVM_RBF_MLP,methods=c('rpart','cart','brt','svm','rbf','mlp'), replication = c('sub','boot','cv'), cv.folds = 5,n=4,test.percent=20, modelSettings = list(rpart=list(cp = 9.735202e-05), brt = list(n.trees = 1311, interaction.depth = 9, shrinkage = 0.01702334, n.minobsinnode = 20),rf = list(mtry = 5, coefReg = 0.7684281, coefImp = 0.2433694),svm = list(degree = 3, scale = 0.3088387, C = 112.9314),rbf = list(negativeThreshold = 0.08416721), mlp = list(layer1 = 19, layer2 = 14, layer3 = 4)))

gui(SDM_GLM_GAM_GLMPoly)  # user interface to see results of models

gui(SDM_MARS)

gui(SDM_MDA_FDA)

gui(SDM_RPART_CART_BRT_RF_SVM_RBF_MLP) 

eval_glm_gam_glmpoly <- lapply(c("SDM_GLM_GAM_GLMPoly"),
                               function(x) { getEvaluation(get(x),wtest='test.dep',stat=c('AUC','COR','Deviance','obs.prevalence',
                                                                                          'threshold','TSS','Kappa','sensitivity','specificity','NMI','phi','ppv','npv','ccr','prevalence'),opt=2) } )
n <- 28 # number of models in each algorithm
algo_glm_gam_glmpoly <- c('GLM','GAM','GLMPoly') ######whatever one you want
eval_algo_glm_gam_glmpoly <- data.frame(algo = c(rep(algo_glm_gam_glmpoly, each=n)),bind_rows(eval_glm_gam_glmpoly))
good_models_glm_gam_glmpoly <- eval_algo_glm_gam_glmpoly[eval_algo_glm_gam_glmpoly[,"AUC"] >= 0.9 & eval_algo_glm_gam_glmpoly[,"TSS"] >= 0.88 & eval_algo_glm_gam_glmpoly[,"Kappa"] >= 0.82,]
good_models_glm_gam_glmpoly

eval_mars <- lapply(c("SDM_MARS"),
                    function(x) { getEvaluation(get(x),wtest='test.dep',stat=c('AUC','COR','Deviance','obs.prevalence',
                                                                               'threshold','TSS','Kappa','sensitivity','specificity','NMI','phi','ppv','npv','ccr','prevalence'),opt=2) } )
n <- 28 # number of models in each algorithm
algo_mars <- c('MARS') 
eval_algo_mars <- data.frame(algo = c(rep(algo_mars, each=n)),bind_rows(eval_mars))
good_models_mars <- eval_algo_mars[eval_algo_mars[,"AUC"] >= 0.9 & eval_algo_mars[,"TSS"] >= 0.72 & eval_algo_mars[,"Kappa"] >= 0.67,]
good_models_mars

eval_mda_fda <- lapply(c("SDM_MDA_FDA"),
                       function(x) { getEvaluation(get(x),wtest='test.dep',stat=c('AUC','COR','Deviance','obs.prevalence',
                                                                                  'threshold','TSS','Kappa','sensitivity','specificity','NMI','phi','ppv','npv','ccr','prevalence'),opt=2) } )
n <- 28 # number of models in each algorithm
algo_mda_fda <- c('MDA','FDA') ######whatever one you want
eval_algo_mda_fda <- data.frame(algo = c(rep(algo_mda_fda, each=n)),bind_rows(eval_mda_fda))
good_models_mda_fda <- eval_algo_mda_fda[eval_algo_mda_fda[,"AUC"] >= 0.9 & eval_algo_mda_fda[,"TSS"] >= 0.88 & eval_algo_mda_fda[,"Kappa"] >= 0.82,]
good_models_mda_fda

eval_rpart_cart_brt_rf_svm_rbf_mlp <- lapply(c("SDM_RPART_CART_BRT_RF_SVM_RBF_MLP),
function(x) { getEvaluation(get(x),wtest='test.dep',stat=c('AUC','COR','Deviance','obs.prevalence',
'threshold','TSS','Kappa','sensitivity','specificity','NMI','phi','ppv','npv','ccr','prevalence'),opt=2) } )
n <- 28 # number of models in each algorithm
algo_rpart_cart_brt_rf_svm_rbf_mlp <- c('RPART','CART','BRT','RF','SVM','RBF','MLP') ######whatever one you want
eval_algo_rpart_cart_brt_rf_svm_rbf_mlp <- data.frame(algo = c(rep(algo_, each=n)),bind_rows(eval_mda_fda))
good_models_rpart_cart_brt_rf_svm_rbf_mlp <- eval_algo_rpart_cart_brt_rf_svm_rbf_mlp[eval_algo_rpart_cart_brt_rf_svm_rbf_mlp[,"AUC"] >= 0.9 & eval_algo_rpart_cart_brt_rf_svm_rbf_mlp[,"TSS"] >= 0.88 & eval_algo_rpart_cart_brt_rf_svm_rbf_mlp[,"Kappa"] >= 0.82,]
good_models_rpart_cart_brt_rf_svm_rbf_mlp

ID_GLM <- good_models_glm_gam_glmpoly[good_models_glm_gam_glmpoly$algo =="GLM",]
ID_GAM <- good_models_glm_gam_glmpoly[good_models_glm_gam_glmpoly$algo =="GAM",]
ID_GLMPoly <- good_models_glm_gam_glmpoly[good_models_glm_gam_glmpoly$algo =="GLMPoly",]

ID_MARS <- good_models_mars[good_models_mars$algo =="MARS",]

ID_MDA <- good_models_mda_fda[good_models_mda_fda$algo =="MDA",]
ID_FDA <- good_models_mda_fda[good_models_mda_fda$algo =="FDA",]

ID_RPART <- good_models_rpart_cart_brt_rf_svm_rbf_mlp[good_models_rpart_cart_brt_rf_svm_rbf_mlp$algo =="RPART",]
ID_CART <- good_models_rpart_cart_brt_rf_svm_rbf_mlp[good_models_rpart_cart_brt_rf_svm_rbf_mlp$algo =="CART",]
ID_BRT <- good_models_rpart_cart_brt_rf_svm_rbf_mlp[good_models_rpart_cart_brt_rf_svm_rbf_mlp$algo =="BRT",]
ID_RF <- good_models_rpart_cart_brt_rf_svm_rbf_mlp[good_models_rpart_cart_brt_rf_svm_rbf_mlp$algo =="RF",]
ID_SVM <- good_models_rpart_cart_brt_rf_svm_rbf_mlp[good_models_rpart_cart_brt_rf_svm_rbf_mlp$algo =="SVM",]
ID_RBF <- good_models_rpart_cart_brt_rf_svm_rbf_mlp[good_models_rpart_cart_brt_rf_svm_rbf_mlp$algo =="RBF",]
ID_MLP <- good_models_rpart_cart_brt_rf_svm_rbf_mlp[good_models_rpart_cart_brt_rf_svm_rbf_mlp$algo =="MLP",]

List_Rasters_SSP585_2011_2040 <- list.files(path = "D:/Westcott/Raster_Data/Sequence_7/SSP585/2011_2040/SQ/PJ/Msk", pattern = 'tif$', full.names = T)
List_Rasters_SSP585_2041_2070 <- list.files(path = "D:/Westcott/Raster_Data/Sequence_7/SSP585/2041-2070/Sq/PJ/Msk", pattern = 'tif$', full.names = T)
List_Rasters_SSP585_2071_2100 <- list.files(path = "D:/Westcott/Raster_Data/Sequence_7/SSP585/2071-2100/Sq\PJ\Msk", pattern = 'tif$', full.names = T)

Rasters_SSP585_2011_2040 <- stack(List_Rasters_SSP585_2011_2040)
Rasters_SSP585_2041_2070 <- stack(List_Rasters_SSP585_2041_2070)
Rasters_SSP585_2071_2100 <- stack(List_Rasters_SSP585_2071_2100)

List_Rasters_SSP370_2011_2040 <- list.files(path = "D:/Westcott/Raster_Data/Sequence_7/2011_2040/SQ/PJ/Msk", pattern = 'tif$', full.names = T)
List_Rasters_SSP370_2041_2070 <- list.files(path = "D:/Westcott/Raster_Data/Sequence_7/2041-2070/Sq/PJ/Msk", pattern = 'tif$', full.names = T)
List_Rasters_SSP370_2071_2100 <- list.files(path = "D:/Westcott/Raster_Data/Sequence_7/2071-2100/Sq\PJ\Msk", pattern = 'tif$', full.names = T)

Rasters_SSP370_2011_2040 <- stack(List_Rasters_SSP370_2011_2040)
Rasters_SSP370_2041_2070 <- stack(List_Rasters_SSP370_2041_2070)
Rasters_SSP370_2071_2100 <- stack(List_Rasters_SSP370_2071_2100)

Ensemble_GAM <- ensemble(SDM_GLM_GAM_GLMPoly, Rasters_Present, filename = "IS_CB_S7_GAM.tif", setting=list(method = 'unweighted', id = ID_GAM$modelID))
Ensemble_GAM_SSP585_2011_2040 <- ensemble(SDM_GLM_GAM_GLMPoly, Rasters_SSP585_2011_2040, filename = "IS_CB_S7_GAM_SSP585_2011_2040.tif", setting=list(method = 'unweighted', id = ID_GAM$modelID))
Ensemble_GAM_SSP585_2041_2070 <- ensemble(SDM_GLM_GAM_GLMPoly, Rasters_SSP585_2041_2070, filename = "IS_CB_S7_GAM_SSP585_2041_2070.tif", setting=list(method = 'unweighted', id = ID_GAM$modelID))
Ensemble_GAM_SSP585_2071_2100 <- ensemble(SDM_GLM_GAM_GLMPoly, Rasters_SSP585_2071_2100, filename = "IS_CB_S7_GAM_SSP585_2071_2100.tif", setting=list(method = 'unweighted', id = ID_GAM$modelID))
Ensemble_GAM_SSP370_2011_2040 <- ensemble(SDM_GLM_GAM_GLMPoly, Rasters_SSP370_2011_2040, filename = "IS_CB_S7_GAM_SSP370_2011_2040.tif", setting=list(method = 'unweighted', id = ID_GAM$modelID))
Ensemble_GAM_SSP370_2041_2070 <- ensemble(SDM_GLM_GAM_GLMPoly, Rasters_SSP370_2041_2070, filename = "IS_CB_S7_GAM_SSP370_2041_2070.tif", setting=list(method = 'unweighted', id = ID_GAM$modelID))
Ensemble_GAM_SSP370_2071_2100 <- ensemble(SDM_GLM_GAM_GLMPoly, Rasters_SSP370_2071_2100, filename = "IS_CB_S7_GAM_SSP370_2071_2100.tif", setting=list(method = 'unweighted', id = ID_GAM$modelID))

Ensemble_GLMPoly <- ensemble(SDM_GLM_GAM_GLMPoly, Rasters_Present, filename = "IS_CB_S7_GLMPoly.tif", setting=list(method = 'unweighted', id = ID_GLMPoly$modelID))
Ensemble_GLMPoly_SSP585_2011_2040 <- ensemble(SDM_GLM_GAM_GLMPoly, Rasters_SSP585_2011_2040, filename = "IS_CB_S7_GLMPoly_SSP585_2011_2040.tif", setting=list(method = 'unweighted', id = ID_GLMPoly$modelID))
Ensemble_GLMPoly_SSP585_2041_2070 <- ensemble(SDM_GLM_GAM_GLMPoly, Rasters_SSP585_2041_2070, filename = "IS_CB_S7_GLMPoly_SSP585_2041_2070.tif", setting=list(method = 'unweighted', id = ID_GLMPoly$modelID))
Ensemble_GLMPoly_SSP585_2071_2100 <- ensemble(SDM_GLM_GAM_GLMPoly, Rasters_SSP585_2071_2100, filename = "IS_CB_S7_GLMPoly_SSP585_2071_2100.tif", setting=list(method = 'unweighted', id = ID_GLMPoly$modelID))
Ensemble_GLMPoly_SSP370_2011_2040 <- ensemble(SDM_GLM_GAM_GLMPoly, Rasters_SSP370_2011_2040, filename = "IS_CB_S7_GLMPoly_SSP370_2011_2040.tif", setting=list(method = 'unweighted', id = ID_GLMPoly$modelID))
Ensemble_GLMPoly_SSP370_2041_2070 <- ensemble(SDM_GLM_GAM_GLMPoly, Rasters_SSP370_2041_2070, filename = "IS_CB_S7_GLMPoly_SSP370_2041_2070.tif", setting=list(method = 'unweighted', id = ID_GLMPoly$modelID))
Ensemble_GLMPoly_SSP370_2071_2100 <- ensemble(SDM_GLM_GAM_GLMPoly, Rasters_SSP370_2071_2100, filename = "IS_CB_S7_GLMPoly_SSP370_2071_2100.tif", setting=list(method = 'unweighted', id = ID_GLMPoly$modelID))

Ensemble_MDA <- ensemble(SDM_MDA_FDA, Rasters_Present, filename = "IS_CB_S7_MDA.tif", setting=list(method = 'unweighted', id = ID_MDA$modelID))
Ensemble_MDA_SSP585_2011_2040 <- ensemble(SDM_MDA_FDA, Rasters_SSP585_2011_2040, filename = "IS_CB_S7_MDA_SSP585_2011_2040.tif", setting=list(method = 'unweighted', id = ID_MDA$modelID))
Ensemble_MDA_SSP585_2041_2070 <- ensemble(SDM_MDA_FDA, Rasters_SSP585_2041_2070, filename = "IS_CB_S7_MDA_SSP585_2041_2070.tif", setting=list(method = 'unweighted', id = ID_MDA$modelID))
Ensemble_MDA_SSP585_2071_2100 <- ensemble(SDM_MDA_FDA, Rasters_SSP585_2071_2100, filename = "IS_CB_S7_MDA_SSP585_2071_2100.tif", setting=list(method = 'unweighted', id = ID_MDA$modelID))
Ensemble_MDA_SSP370_2011_2040 <- ensemble(SDM_MDA_FDA, Rasters_SSP370_2011_2040, filename = "IS_CB_S7_MDA_SSP370_2011_2040.tif", setting=list(method = 'unweighted', id = ID_MDA$modelID))
Ensemble_MDA_SSP370_2041_2070 <- ensemble(SDM_MDA_FDA, Rasters_SSP370_2041_2070, filename = "IS_CB_S7_MDA_SSP370_2041_2070.tif", setting=list(method = 'unweighted', id = ID_MDA$modelID))
Ensemble_MDA_SSP370_2071_2100 <- ensemble(SDM_MDA_FDA, Rasters_SSP370_2071_2100, filename = "IS_CB_S7_MDA_SSP370_2071_2100.tif", setting=list(method = 'unweighted', id = ID_MDA$modelID))

Ensemble_FDA <- ensemble(SDM_SDM_MDA_FDA, Rasters_Present, filename = "IS_CB_S7_FDA.tif", setting=list(method = 'unweighted', id = ID_FDA$modelID))
Ensemble_FDA_SSP585_2011_2040 <- ensemble(SDM_MDA_FDA, Rasters_SSP585_2011_2040, filename = "IS_CB_S7_FDA_SSP585_2011_2040.tif", setting=list(method = 'unweighted', id = ID_FDA$modelID))
Ensemble_FDA_SSP585_2041_2070 <- ensemble(SDM_MDA_FDA, Rasters_SSP585_2041_2070, filename = "IS_CB_S7_FDA_SSP585_2041_2070.tif", setting=list(method = 'unweighted', id = ID_FDA$modelID))
Ensemble_FDA_SSP585_2071_2100 <- ensemble(SDM_MDA_FDA, Rasters_SSP585_2071_2100, filename = "IS_CB_S7_FDA_SSP585_2071_2100.tif", setting=list(method = 'unweighted', id = ID_FDA$modelID))
Ensemble_FDA_SSP370_2011_2040 <- ensemble(SDM_MDA_FDA, Rasters_SSP370_2011_2040, filename = "IS_CB_S7_FDA_SSP370_2011_2040.tif", setting=list(method = 'unweighted', id = ID_FDA$modelID))
Ensemble_FDA_SSP370_2041_2070 <- ensemble(SDM_MDA_FDA, Rasters_SSP370_2041_2070, filename = "IS_CB_S7_FDA_SSP370_2041_2070.tif", setting=list(method = 'unweighted', id = ID_FDA$modelID))
Ensemble_FDA_SSP370_2071_2100 <- ensemble(SDM_MDA_FDA, Rasters_SSP370_2071_2100, filename = "IS_CB_S7_FDA_SSP370_2071_2100.tif", setting=list(method = 'unweighted', id = ID_FDA$modelID))

Ensemble_RPART <- ensemble(SDM_RPART_CART_BRT_RF_SVM_RBF_MLP, Rasters_Present, filename = "IS_CB_S7_RPART.tif", setting=list(method = 'unweighted', id = ID_RPART$modelID))
Ensemble_RPART_SSP585_2011_2040 <- ensemble(SDM_RPART_CART_BRT_RF_SVM_RBF_MLP, Rasters_SSP585_2011_2040, filename = "IS_CB_S7_RPART_SSP585_2011_2040.tif", setting=list(method = 'unweighted', id = ID_RPART$modelID))
Ensemble_RPART_SSP585_2041_2070 <- ensemble(SDM_RPART_CART_BRT_RF_SVM_RBF_MLP, Rasters_SSP585_2041_2070, filename = "IS_CB_S7_RPART_SSP585_2041_2070.tif", setting=list(method = 'unweighted', id = ID_RPART$modelID))
Ensemble_RPART_SSP585_2071_2100 <- ensemble(SDM_RPART_CART_BRT_RF_SVM_RBF_MLP, Rasters_SSP585_2071_2100, filename = "IS_CB_S7_RPART_SSP585_2071_2100.tif", setting=list(method = 'unweighted', id = ID_RPART$modelID))
Ensemble_RPART_SSP370_2011_2040 <- ensemble(SDM_RPART_CART_BRT_RF_SVM_RBF_MLP, Rasters_SSP370_2011_2040, filename = "IS_CB_S7_RPART_SSP370_2011_2040.tif", setting=list(method = 'unweighted', id = ID_RPART$modelID))
Ensemble_RPART_SSP370_2041_2070 <- ensemble(SDM_RPART_CART_BRT_RF_SVM_RBF_MLP, Rasters_SSP370_2041_2070, filename = "IS_CB_S7_RPART_SSP370_2041_2070.tif", setting=list(method = 'unweighted', id = ID_RPART$modelID))
Ensemble_RPART_SSP370_2071_2100 <- ensemble(SDM_RPART_CART_BRT_RF_SVM_RBF_MLP, Rasters_SSP370_2071_2100, filename = "IS_CB_S7_RPART_SSP370_2071_2100.tif", setting=list(method = 'unweighted', id = ID_RPART$modelID))

Ensemble_CART <- ensemble(SDM_RPART_CART_BRT_RF_SVM_RBF_MLP, Rasters_Present, filename = "IS_CB_S7_RPART.tif", setting=list(method = 'unweighted', id = ID_CART$modelID))
Ensemble_CART_SSP585_2011_2040 <- ensemble(SDM_RPART_CART_BRT_RF_SVM_RBF_MLP, Rasters_SSP585_2011_2040, filename = "IS_CB_S7_CART_SSP585_2011_2040.tif", setting=list(method = 'unweighted', id = ID_CART$modelID))
Ensemble_CART_SSP585_2041_2070 <- ensemble(SDM_RPART_CART_BRT_RF_SVM_RBF_MLP, Rasters_SSP585_2041_2070, filename = "IS_CB_S7_CART_SSP585_2041_2070.tif", setting=list(method = 'unweighted', id = ID_CART$modelID))
Ensemble_CART_SSP585_2071_2100 <- ensemble(SDM_RPART_CART_BRT_RF_SVM_RBF_MLP, Rasters_SSP585_2071_2100, filename = "IS_CB_S7_CART_SSP585_2071_2100.tif", setting=list(method = 'unweighted', id = ID_CART$modelID))
Ensemble_CART_SSP370_2011_2040 <- ensemble(SDM_RPART_CART_BRT_RF_SVM_RBF_MLP, Rasters_SSP370_2011_2040, filename = "IS_CB_S7_CART_SSP370_2011_2040.tif", setting=list(method = 'unweighted', id = ID_CART$modelID))
Ensemble_CART_SSP370_2041_2070 <- ensemble(SDM_RPART_CART_BRT_RF_SVM_RBF_MLP, Rasters_SSP370_2041_2070, filename = "IS_CB_S7_CART_SSP370_2041_2070.tif", setting=list(method = 'unweighted', id = ID_CART$modelID))
Ensemble_CART_SSP370_2071_2100 <- ensemble(SDM_RPART_CART_BRT_RF_SVM_RBF_MLP, Rasters_SSP370_2071_2100, filename = "IS_CB_S7_CART_SSP370_2071_2100.tif", setting=list(method = 'unweighted', id = ID_CART$modelID))

Ensemble_BRT <- ensemble(SDM_RPART_CART_BRT_RF_SVM_RBF_MLP, Rasters_Present, filename = "IS_CB_S7_BRT.tif", setting=list(method = 'unweighted', id = ID_BRT$modelID))
Ensemble_BRT_SSP585_2011_2040 <- ensemble(SDM_RPART_CART_BRT_RF_SVM_RBF_MLP, Rasters_SSP585_2011_2040, filename = "IS_CB_S7_BRT_SSP585_2011_2040.tif", setting=list(method = 'unweighted', id = ID_BRT$modelID))
Ensemble_BRT_SSP585_2041_2070 <- ensemble(SDM_RPART_CART_BRT_RF_SVM_RBF_MLP, Rasters_SSP585_2041_2070, filename = "IS_CB_S7_BRT_SSP585_2041_2070.tif", setting=list(method = 'unweighted', id = ID_BRT$modelID))
Ensemble_BRT_SSP585_2071_2100 <- ensemble(SDM_RPART_CART_BRT_RF_SVM_RBF_MLP, Rasters_SSP585_2071_2100, filename = "IS_CB_S7_BRT_SSP585_2071_2100.tif", setting=list(method = 'unweighted', id = ID_BRT$modelID))
Ensemble_BRT_SSP370_2011_2040 <- ensemble(SDM_RPART_CART_BRT_RF_SVM_RBF_MLP, Rasters_SSP370_2011_2040, filename = "IS_CB_S7_BRT_SSP370_2011_2040.tif", setting=list(method = 'unweighted', id = ID_BRT$modelID))
Ensemble_BRT_SSP370_2041_2070 <- ensemble(SDM_RPART_CART_BRT_RF_SVM_RBF_MLP, Rasters_SSP370_2041_2070, filename = "IS_CB_S7_BRT_SSP370_2041_2070.tif", setting=list(method = 'unweighted', id = ID_BRT$modelID))
Ensemble_BRT_SSP370_2071_2100 <- ensemble(SDM_RPART_CART_BRT_RF_SVM_RBF_MLP, Rasters_SSP370_2071_2100, filename = "IS_CB_S7_BRT_SSP370_2071_2100.tif", setting=list(method = 'unweighted', id = ID_BRT$modelID))

Ensemble_RF <- ensemble(SDM_RPART_CART_BRT_RF_SVM_RBF_MLP, Rasters_Present, filename = "IS_CB_S7_RF.tif", setting=list(method = 'unweighted', id = ID_RF$modelID))
Ensemble_RF_SSP585_2011_2040 <- ensemble(SDM_RPART_CART_BRT_RF_SVM_RBF_MLP, Rasters_SSP585_2011_2040, filename = "IS_CB_S7_RF_SSP585_2011_2040.tif", setting=list(method = 'unweighted', id = ID_RF$modelID))
Ensemble_RF_SSP585_2041_2070 <- ensemble(SDM_RPART_CART_BRT_RF_SVM_RBF_MLP, Rasters_SSP585_2041_2070, filename = "IS_CB_S7_RF_SSP585_2041_2070.tif", setting=list(method = 'unweighted', id = ID_RF$modelID))
Ensemble_RF_SSP585_2071_2100 <- ensemble(SDM_RPART_CART_BRT_RF_SVM_RBF_MLP, Rasters_SSP585_2071_2100, filename = "IS_CB_S7_RF_SSP585_2071_2100.tif", setting=list(method = 'unweighted', id = ID_RF$modelID))
Ensemble_RF_SSP370_2011_2040 <- ensemble(SDM_RPART_CART_BRT_RF_SVM_RBF_MLP, Rasters_SSP370_2011_2040, filename = "IS_CB_S7_RF_SSP370_2011_2040.tif", setting=list(method = 'unweighted', id = ID_RF$modelID))
Ensemble_RF_SSP370_2041_2070 <- ensemble(SDM_RPART_CART_BRT_RF_SVM_RBF_MLP, Rasters_SSP370_2041_2070, filename = "IS_CB_S7_RF_SSP370_2041_2070.tif", setting=list(method = 'unweighted', id = ID_RF$modelID))
Ensemble_RF_SSP370_2071_2100 <- ensemble(SDM_RPART_CART_BRT_RF_SVM_RBF_MLP, Rasters_SSP370_2071_2100, filename = "IS_CB_S7_RF_SSP370_2071_2100.tif", setting=list(method = 'unweighted', id = ID_RF$modelID))

Ensemble_SVM <- ensemble(SDM_RPART_CART_BRT_RF_SVM_RBF_MLP, Rasters_Present, filename = "IS_CB_S7_SVM.tif", setting=list(method = 'unweighted', id = ID_SVM$modelID))
Ensemble_SVM_SSP585_2011_2040 <- ensemble(SDM_RPART_CART_BRT_RF_SVM_RBF_MLP, Rasters_SSP585_2011_2040, filename = "IS_CB_S7_SVM_SSP585_2011_2040.tif", setting=list(method = 'unweighted', id = ID_SVM$modelID))
Ensemble_SVM_SSP585_2041_2070 <- ensemble(SDM_RPART_CART_BRT_RF_SVM_RBF_MLP, Rasters_SSP585_2041_2070, filename = "IS_CB_S7_SVM_SSP585_2041_2070.tif", setting=list(method = 'unweighted', id = ID_SVM$modelID))
Ensemble_SVM_SSP585_2071_2100 <- ensemble(SDM_RPART_CART_BRT_RF_SVM_RBF_MLP, Rasters_SSP585_2071_2100, filename = "IS_CB_S7_SVM_SSP585_2071_2100.tif", setting=list(method = 'unweighted', id = ID_SVM$modelID))
Ensemble_SVM_SSP370_2011_2040 <- ensemble(SDM_RPART_CART_BRT_RF_SVM_RBF_MLP, Rasters_SSP370_2011_2040, filename = "IS_CB_S7_SVM_SSP370_2011_2040.tif", setting=list(method = 'unweighted', id = ID_SVM$modelID))
Ensemble_SVM_SSP370_2041_2070 <- ensemble(SDM_RPART_CART_BRT_RF_SVM_RBF_MLP, Rasters_SSP370_2041_2070, filename = "IS_CB_S7_SVM_SSP370_2041_2070.tif", setting=list(method = 'unweighted', id = ID_SVM$modelID))
Ensemble_SVM_SSP370_2071_2100 <- ensemble(SDM_RPART_CART_BRT_RF_SVM_RBF_MLP, Rasters_SSP370_2071_2100, filename = "IS_CB_S7_SVM_SSP370_2071_2100.tif", setting=list(method = 'unweighted', id = ID_SVM$modelID))

Ensemble_RBF <- ensemble(SDM_RPART_CART_BRT_RF_SVM_RBF_MLP, Rasters_Present, filename = "IS_CB_S7_RBF.tif", setting=list(method = 'unweighted', id = ID_RBF$modelID))
Ensemble_RBF_SSP585_2011_2040 <- ensemble(SDM_RPART_CART_BRT_RF_SVM_RBF_MLP, Rasters_SSP585_2011_2040, filename = "IS_CB_S7_RBF_SSP585_2011_2040.tif", setting=list(method = 'unweighted', id = ID_RBF$modelID))
Ensemble_RBF_SSP585_2041_2070 <- ensemble(SDM_RPART_CART_BRT_RF_SVM_RBF_MLP, Rasters_SSP585_2041_2070, filename = "IS_CB_S7_SVM_SSP585_2041_2070.tif", setting=list(method = 'unweighted', id = ID_RBF$modelID))
Ensemble_RBF_SSP585_2071_2100 <- ensemble(SDM_RPART_CART_BRT_RF_SVM_RBF_MLP, Rasters_SSP585_2071_2100, filename = "IS_CB_S7_SVM_SSP585_2071_2100.tif", setting=list(method = 'unweighted', id = ID_RBF$modelID))
Ensemble_RBF_SSP370_2011_2040 <- ensemble(SDM_RPART_CART_BRT_RF_SVM_RBF_MLP, Rasters_SSP370_2011_2040, filename = "IS_CB_S7_RBF_SSP370_2011_2040.tif", setting=list(method = 'unweighted', id = ID_RBF$modelID))
Ensemble_RBF_SSP370_2041_2070 <- ensemble(SDM_RPART_CART_BRT_RF_SVM_RBF_MLP, Rasters_SSP370_2041_2070, filename = "IS_CB_S7_RBF_SSP370_2041_2070.tif", setting=list(method = 'unweighted', id = ID_RBF$modelID))
Ensemble_RBF_SSP370_2071_2100 <- ensemble(SDM_RPART_CART_BRT_RF_SVM_RBF_MLP, Rasters_SSP370_2071_2100, filename = "IS_CB_S7_RBF_SSP370_2071_2100.tif", setting=list(method = 'unweighted', id = ID_RBF$modelID))

Ensemble_MLP <- ensemble(SDM_RPART_CART_BRT_RF_SVM_RBF_MLP, Rasters_Present, filename = "IS_CB_S7_MLP.tif", setting=list(method = 'unweighted', id = ID_MLP$modelID))
Ensemble_MLP_SSP585_2011_2040 <- ensemble(SDM_RPART_CART_BRT_RF_SVM_RBF_MLP, Rasters_SSP585_2011_2040, filename = "IS_CB_S7_MLP_SSP585_2011_2040.tif", setting=list(method = 'unweighted', id = ID_MLP$modelID))
Ensemble_MLP_SSP585_2041_2070 <- ensemble(SDM_RPART_CART_BRT_RF_SVM_RBF_MLP, Rasters_SSP585_2041_2070, filename = "IS_CB_S7_MLP_SSP585_2041_2070.tif", setting=list(method = 'unweighted', id = ID_MLP$modelID))
Ensemble_MLP_SSP585_2071_2100 <- ensemble(SDM_RPART_CART_BRT_RF_SVM_RBF_MLP, Rasters_SSP585_2071_2100, filename = "IS_CB_S7_MLP_SSP585_2071_2100.tif", setting=list(method = 'unweighted', id = ID_MLP$modelID))
Ensemble_MLP_SSP370_2011_2040 <- ensemble(SDM_RPART_CART_BRT_RF_SVM_RBF_MLP, Rasters_SSP370_2011_2040, filename = "IS_CB_S7_MLP_SSP370_2011_2040.tif", setting=list(method = 'unweighted', id = ID_MLP$modelID))
Ensemble_MLP_SSP370_2041_2070 <- ensemble(SDM_RPART_CART_BRT_RF_SVM_RBF_MLP, Rasters_SSP370_2041_2070, filename = "IS_CB_S7_MLP_SSP370_2041_2070.tif", setting=list(method = 'unweighted', id = ID_MLP$modelID))
Ensemble_MLP_SSP370_2071_2100 <- ensemble(SDM_RPART_CART_BRT_RF_SVM_RBF_MLP, Rasters_SSP370_2071_2100, filename = "IS_CB_S7_MLP_SSP370_2071_2100.tif", setting=list(method = 'unweighted', id = ID_MLP$modelID))

write.csv(ID_GAM,"C:/Users/jwestcot/Permission_Check/Black_Legged_Tick/Ixodes_Scapularis/R/Ixodes_Scapularis/Ixodes_Scapularis_Scenario_Seven/Ixodes_Scapularis_Scenario_Seven/Tables/IS_CB_S7_GAM_ID.csv")
write.csv(ID_GLMPoly,"C:/Users/jwestcot/Permission_Check/Black_Legged_Tick/Ixodes_Scapularis/R/Ixodes_Scapularis/Ixodes_Scapularis_Scenario_Seven/Ixodes_Scapularis_Scenario_Seven/Tables/IS_CB_S7_GLMPoly_ID.csv")
write.csv(ID_MDA,"C:/Users/jwestcot/Permission_Check/Black_Legged_Tick/Ixodes_Scapularis/R/Ixodes_Scapularis/Ixodes_Scapularis_Scenario_Seven/Ixodes_Scapularis_Scenario_Seven/Tables/IS_CB_S7_MDA_ID.csv")
write.csv(ID_FDA,"C:/Users/jwestcot/Permission_Check/Black_Legged_Tick/Ixodes_Scapularis/R/Ixodes_Scapularis/Ixodes_Scapularis_Scenario_Seven/Ixodes_Scapularis_Scenario_Seven/Tables/IS_CB_S7_FDA_ID.csv")
write.csv(ID_RPART,"C:/Users/jwestcot/Permission_Check/Black_Legged_Tick/Ixodes_Scapularis/R/Ixodes_Scapularis/Ixodes_Scapularis_Scenario_Seven/Ixodes_Scapularis_Scenario_Seven/Tables/IS_CB_S7_RPART_ID.csv")
write.csv(ID_CART,"C:/Users/jwestcot/Permission_Check/Black_Legged_Tick/Ixodes_Scapularis/R/Ixodes_Scapularis/Ixodes_Scapularis_Scenario_Seven/Ixodes_Scapularis_Scenario_Seven/Tables/IS_CB_S7_CART_ID.csv")
write.csv(ID_BRT,"C:/Users/jwestcot/Permission_Check/Black_Legged_Tick/Ixodes_Scapularis/R/Ixodes_Scapularis/Ixodes_Scapularis_Scenario_Seven/Ixodes_Scapularis_Scenario_Seven/Tables/IS_CB_S7_BRT_ID.csv")
write.csv(ID_RF,"C:/Users/jwestcot/Permission_Check/Black_Legged_Tick/Ixodes_Scapularis/R/Ixodes_Scapularis/Ixodes_Scapularis_Scenario_Seven/Ixodes_Scapularis_Scenario_Seven/Tables/IS_CB_S7_RF_ID.csv")
write.csv(ID_SVM,"C:/Users/jwestcot/Permission_Check/Black_Legged_Tick/Ixodes_Scapularis/R/Ixodes_Scapularis/Ixodes_Scapularis_Scenario_Seven/Ixodes_Scapularis_Scenario_Seven/Tables/IS_CB_S7_SVM_ID.csv")
write.csv(ID_RBF,"C:/Users/jwestcot/Permission_Check/Black_Legged_Tick/Ixodes_Scapularis/R/Ixodes_Scapularis/Ixodes_Scapularis_Scenario_Seven/Ixodes_Scapularis_Scenario_Seven/Tables/IS_CB_S7_RBF_ID.csv")
write.csv(ID_MLP,"C:/Users/jwestcot/Permission_Check/Black_Legged_Tick/Ixodes_Scapularis/R/Ixodes_Scapularis/Ixodes_Scapularis_Scenario_Seven/Ixodes_Scapularis_Scenario_Seven/Tables/IS_CB_S7_MLP_ID.csv")

RC_GAM <- rcurve(SDM_GLM_GAM_GLMPoly, id = c(32,42,50,51,56), gg=T, mean=T, confidence=T, main = 'Response Curve')+ theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),text=element_text(size=10))
RC_GLMPoly <- rcurve(SDM_GLM_GAM_GLMPoly, id = c(70,79,84), gg=T, mean=T, confidence=T, main = 'Response Curve')+ theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),text=element_text(size=10))
RC_MDA <- rcurve(SDM_MDA_FDA, id = c(70,79,84), gg=T, mean=T, confidence=T, main = 'Response Curve')+ theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),text=element_text(size=10))
RC_MDA <- rcurve(SDM_MDA_FDA, id = c(8,26), gg=T, mean=T, confidence=T, main = 'Response Curve')+ theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),text=element_text(size=10))
RC_FDA <- rcurve(SDM_MDA_FDA, id = c(31,32,39,53), gg=T, mean=T, confidence=T, main = 'Response Curve')+ theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),text=element_text(size=10))
RC_RPART <- rcurve(SDM_RPART_CART_BRT_RF_SVM_RBF_MLP, id = c(10,11), gg=T, mean=T, confidence=T, main = 'Response Curve')+ theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),text=element_text(size=10))
RC_CART <- rcurve(SDM_RPART_CART_BRT_RF_SVM_RBF_MLP, id = c(30,32,34,38,39,40,43,50), gg=T, mean=T, confidence=T, main = 'Response Curve')+ theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),text=element_text(size=10))
RC_BRT <- rcurve(SDM_RPART_CART_BRT_RF_SVM_RBF_MLP, id = c(62,63,67,68,71,78), gg=T, mean=T, confidence=T, main = 'Response Curve')+ theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),text=element_text(size=10))
RC_RF <- rcurve(SDM_RPART_CART_BRT_RF_SVM_RBF_MLP, id = c(90,91,92,95,99,105), gg=T, mean=T, confidence=T, main = 'Response Curve')+ theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),text=element_text(size=10))
RC_SVM <- rcurve(SDM_RPART_CART_BRT_RF_SVM_RBF_MLP, id = c(118,123,133), gg=T, mean=T, confidence=T, main = 'Response Curve')+ theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),text=element_text(size=10))
RC_RBF <- rcurve(SDM_RPART_CART_BRT_RF_SVM_RBF_MLP, id = c(145,146,150,155,158,160,164,165), gg=T, mean=T, confidence=T, main = 'Response Curve')+ theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),text=element_text(size=10))
RC_MLP <- rcurve(SDM_RPART_CART_BRT_RF_SVM_RBF_MLP, id = c(179,183,189), gg=T, mean=T, confidence=T, main = 'Response Curve')+ theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),text=element_text(size=10))

RC_BIND <- rbind(RC_GAM$data,RC_GLMPoly$data,RC_MDA$data,RC_FDA$data,RC_RPART$data,RC_CART$data,RC_BRT$data,RC_RF$data,RC_SVM$data,RC_RBF$data,RC_MLP$data)

RC_V_BIO1 <- RC_BIND[RC_BIND$variable=="Band_1.1",]
RC_V_BIO16 <- RC_BIND[RC_BIND$variable=="Band_1.2",]
RC_V_BIO17 <- RC_BIND[RC_BIND$variable=="Band_1.3",]


VARIMP_GAM <- getVarImp(SDM_GLM_GAM_GLMPoly, id = c(32,42,50,51,56), wtest = 'test.dep')
VARIMP_GLMPoly <- getVarImp(SDM_GLM_GAM_GLMPoly, id = c(70,79,84), wtest = 'test.dep')
VARIMP_MDA <- getVarImp(SDM_MDA_FDA, id = c(8,26), wtest = 'test.dep')
VARIMP_FDA <- getVarImp(SDM_MDA_FDA, id = c(31,32,39,53), wtest = 'test.dep')
VARIMP_RPART <- getVarImp(SDM_RPART_CART_BRT_RF_SVM_RBF_MLP, id = c(10,11), wtest = 'test.dep')
VARIMP_CART <- getVarImp(SDM_RPART_CART_BRT_RF_SVM_RBF_MLP, id = c(30,32,34,38,39,40,43,50), wtest = 'test.dep')
VARIMP_BRT <- getVarImp(SDM_RPART_CART_BRT_RF_SVM_RBF_MLP, id = c(62,63,67,68,71,78), wtest = 'test.dep')
VARIMP_RF <- getVarImp(SDM_RPART_CART_BRT_RF_SVM_RBF_MLP, id = c(90,91,92,95,99,105), wtest = 'test.dep')
VARIMP_SVM <- getVarImp(SDM_RPART_CART_BRT_RF_SVM_RBF_MLP, id = c(118,123,133), wtest = 'test.dep')
VARIMP_RBF <- getVarImp(SDM_RPART_CART_BRT_RF_SVM_RBF_MLP, id = c(145,146,150,155,158,160,164,165), wtest = 'test.dep')
VARIMP_MLP <- getVarImp(SDM_RPART_CART_BRT_RF_SVM_RBF_MLP, id = c(179,183,189), wtest = 'test.dep')

Mean_VI_Bio1 <- (VARIMP_GAM@varImportanceMean$corTest$corTest[1]+VARIMP_GLMPoly@varImportanceMean$corTest$corTest[1]+VARIMP_MDA@varImportanceMean$corTest$corTest[1]+VARIMP_FDA@varImportanceMean$corTest$corTest[1]+VARIMP_RPART@varImportanceMean$corTest$corTest[1]+VARIMP_CART@varImportanceMean$corTest$corTest[1]+VARIMP_BRT@varImportanceMean$corTest$corTest[1]+VARIMP_RF@varImportanceMean$corTest$corTest[1]+VARIMP_SVM@varImportanceMean$corTest$corTest[1]+VARIMP_RBF@varImportanceMean$corTest$corTest[1]+VARIMP_MLP@varImportanceMean$corTest$corTest[1])/11
Mean_VI_Bio16 <- (VARIMP_GAM@varImportanceMean$corTest$corTest[2]+VARIMP_GLMPoly@varImportanceMean$corTest$corTest[2]+VARIMP_MDA@varImportanceMean$corTest$corTest[2]+VARIMP_FDA@varImportanceMean$corTest$corTest[2]+VARIMP_RPART@varImportanceMean$corTest$corTest[2]+VARIMP_CART@varImportanceMean$corTest$corTest[2]+VARIMP_BRT@varImportanceMean$corTest$corTest[2]+VARIMP_RF@varImportanceMean$corTest$corTest[2]+VARIMP_SVM@varImportanceMean$corTest$corTest[2]+VARIMP_RBF@varImportanceMean$corTest$corTest[2]+VARIMP_MLP@varImportanceMean$corTest$corTest[2])/11
Mean_VI_Bio17 <- (VARIMP_GAM@varImportanceMean$corTest$corTest[3]+VARIMP_GLMPoly@varImportanceMean$corTest$corTest[3]+VARIMP_MDA@varImportanceMean$corTest$corTest[3]+VARIMP_FDA@varImportanceMean$corTest$corTest[3]+VARIMP_RPART@varImportanceMean$corTest$corTest[3]+VARIMP_CART@varImportanceMean$corTest$corTest[3]+VARIMP_BRT@varImportanceMean$corTest$corTest[3]+VARIMP_RF@varImportanceMean$corTest$corTest[3]+VARIMP_SVM@varImportanceMean$corTest$corTest[3]+VARIMP_RBF@varImportanceMean$corTest$corTest[3]+VARIMP_MLP@varImportanceMean$corTest$corTest[3])/11

PLOT_BIO1 <- RC_V_BIO1 %>% ggplot(aes(Value, Response)) + ylim(0,1) +
geom_text(x = ((max(RC_V_BIO1$Value)-min(RC_V_BIO1$Value))*0.2 + min(RC_V_BIO1$Value)), y = 0.9,
label = paste0("Variable Importance = ",round(Mean_VI_Bio1, 4)), parse = F) +
labs(x = "Mean Annual Precipitation (Bio1)", y = "Probability") +
geom_smooth(color="Black", span = 0.50, method = "loess", method.args = list(degree=1))
PLOT_BIO16 <- RC_V_BIO16 %>% ggplot(aes(Value, Response)) + ylim(0,1) +
geom_text(x = ((max(RC_V_BIO16$Value)-min(RC_V_BIO16$Value))*0.2 + min(RC_V_BIO16$Value)), y = 0.9,
label = paste0("Variable Importance = ",round(Mean_VI_Bio16, 4)), parse = F) +
labs(x = "Mean Monthly Precipitation: Wettest Quarter (Bio16)", y = "Probability") +
geom_smooth(color="Black", span = 0.50, method = "loess", method.args = list(degree=1))
PLOT_BIO17 <- RC_V_BIO17 %>% ggplot(aes(Value, Response)) + ylim(0,1) +
geom_text(x = ((max(RC_V_BIO17$Value)-min(RC_V_BIO17$Value))*0.2 + min(RC_V_BIO17$Value)), y = 0.9,
label = paste0("Variable Importance = ",round(Mean_VI_Bio17, 4)), parse = F) +
labs(x = "Mean Monthly Precipitation: Warmest Quarter (Bio17)", y = "Probability") +
geom_smooth(color="Black", span = 0.50, method = "loess", method.args = list(degree=1))
