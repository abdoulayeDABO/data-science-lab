install.packages("mlr", dependencies = TRUE)
library(mlr)
library(tidyverse)

data(diabetes, package = "mclust")
diabetesTib <- as_tibble(diabetes)
summary(diabetesTib)
head(diabetesTib)

ggplot(diabetesTib, aes(glucose, insulin, col = class)) +
  geom_point() +
  theme_bw()
ggplot(diabetesTib, aes(sspg, insulin, col = class)) +
  geom_point() +
  theme_bw()
ggplot(diabetesTib, aes(sspg, glucose, col = class)) +
  geom_point() +
  theme_bw()


# Building a machine learning model with the mlr package has three main stages:
# 1 Define the task. The task consists of the data and what we want to do with it. In
# this case, the data is diabetesTib, and we want to classify the data with the
# class variable as the target variable.
# 2 Define the learner. The learner is simply the name of the algorithm we plan to use,
# along with any additional arguments the algorithm accepts.
# 3 Train the model. This stage is what it sounds like: you pass the task to the learner,
# and the learner generates a model that you can use to make future predictions.
# TIP This may seem unnecessarily cumbersome, but splitting the task, learner,
# and model into different stages is very useful. It means we can define a single
# task and apply multiple learners to it, or define a single learner and test it
# with multiple different tasks.

#listLearners()$class
diabetesTask <- makeClassifTask(data = diabetesTib, target = "class")
diabetesTask
knn <- makeLearner("classif.knn", par.vals = list("k" = 2))
knnModel <- train(knn, diabetesTask)
knnPred <- predict(knnModel, newdata = diabetesTib)
performance(knnPred, measures = list(mmce, acc))

#############
# Hold out cv
#############
holdout <- makeResampleDesc(method = "Holdout", split = 2/3, stratify = TRUE)
holdoutCV <- resample(learner = knn, task = diabetesTask, resampling = holdout, measures = list(mmce, acc))
holdoutCV$aggr

# Nouvelle description : 10% test, pas de stratification
holdout_desc_10 <- makeResampleDesc(
  method = "Holdout",
  split = 0.9,        # 90% train, 10% test
  stratify = FALSE    # Pas de stratified sampling
)
holdout_desc_10
holdoutCV <- resample(learner = knn, task = diabetesTask, resampling = holdout_desc_10, measures = list(mmce, acc))
calculateConfusionMatrix(holdoutCV$pred, relative = TRUE)


###############
# kfold cv
###############
kFold <- makeResampleDesc(method = "RepCV", folds = 10, reps = 50, stratify = TRUE)
kFoldCV <- resample(learner = knn, task = diabetesTask, resampling = kFold, measures = list(mmce, acc))
kFoldCV$measures.test
kFoldCV$aggr
calculateConfusionMatrix(kFoldCV$pred, relative = TRUE)


########
# loo cv
########
LOO <- makeResampleDesc(method = "LOO")
LOOCV <- resample(learner = knn, task = diabetesTask, resampling = LOO, measures = list(mmce, acc))
LOOCV$aggr
calculateConfusionMatrix(LOOCV$pred, relative = TRUE)


#################
# CV with Tunning
#################

# inner <- makeResampleDesc("CV")
# outer <- makeResampleDesc("RepCV", folds = 10, reps = 5)
# knnWrapper <- makeTuneWrapper("classif.knn", resampling = inner,
#                               par.set = knnParamSpace,
#                               control = gridSearch)
# cvWithTuning <- resample(knnWrapper, diabetesTask, resampling = outer)
# cvWithTuning
# 
# outerHoldout <- makeResampleDesc("Holdout", split = 2/3, stratify = TRUE)
# knnWrapper <- makeTuneWrapper("classif.knn", resampling = inner,
#                               par.set = knnParamSpace,
#                               control = gridSearch)
# holdoutCVWithTuning <- resample(knnWrapper, irisTask,
#                                 resampling = outerHoldout)
# holdoutCVWithTuning
# 
# outerKfold <- makeResampleDesc("CV", iters = 5, stratify = TRUE)
# kFoldCVWithTuning <- resample(knnWrapper, irisTask,
#                               resampling = outerKfold)
# kFoldCVWithTuning
# resample(knnWrapper, irisTask, resampling = outerKfold)
# kSamples <- map_dbl(1:10, ~resample(
#   knnWrapper, irisTask, resampling = outerKfold)$aggr
# )
# hSamples <- map_dbl(1:10, ~resample(
#   knnWrapper, irisTask, resampling = outerHoldout)$aggr
# )
# hist(kSamples, xlim = c(0, 0.11))
# hist(hSamples, xlim = c(0, 0.11))

knnParamSpace <- makeParamSet(makeDiscreteParam("k", values = 1:10))
gridSearch <- makeTuneControlGrid()
cvForTuning <- makeResampleDesc("RepCV", folds = 10, reps = 20)
tunedK <- tuneParams("classif.knn", task = diabetesTask, resampling = cvForTuning, par.set = knnParamSpace, control = gridSearch)
knnTuningData <- generateHyperParsEffectData(tunedK)
plotHyperParsEffect(knnTuningData, x = "k", y = "mmce.test.mean", plot.type = "line") + theme_bw()

# Now we can train our final model, using our tuned value of k:
tunedKnn <- setHyperPars(makeLearner("classif.knn"), par.vals = tunedK$x)
tunedKnnModel <- train(tunedKnn, diabetesTask)

# Prediction
newDiabetesPatients <- tibble(glucose = c(82, 108, 300),
                              insulin = c(361, 288, 1052),
                              sspg = c(200, 186, 135))
newPatientsPred <- predict(tunedKnnModel, newdata = newDiabetesPatients)
getPredictionResponse(newPatientsPred)


