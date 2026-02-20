##################
# knn on iris data
##################
install.packages("mlr", dependencies = TRUE)
install.packages("ggplot2")
install.packages("tidyverse")
library(mlr)
library(tidyverse)

data(iris)
head(iris)
str(iris)
irisTask <- makeClassifTask(data = iris, target = "Species")
knnParamSpace <- makeParamSet(makeDiscreteParam("k", values = 1:25))
gridSearch <- makeTuneControlGrid()
cvForTuning <- makeResampleDesc("RepCV", folds = 10, reps = 20)
tunedK <- tuneParams("classif.knn", task = irisTask,
                     resampling = cvForTuning,
                     par.set = knnParamSpace,
                     control = gridSearch)
# tunedK
# tunedK$x
knnTuningData <- generateHyperParsEffectData(tunedK)
plotHyperParsEffect(knnTuningData, x = "k", y = "mmce.test.mean", plot.type = "line") + theme_bw()
tunedKnn <- setHyperPars(makeLearner("classif.knn"), par.vals = tunedK$x)
tunedKnnModel <- train(tunedKnn, irisTask)

# Predict unseen data
newFlowers <- tibble(
  Sepal.Length = c(5.1, 5.9, 6.9),
  Sepal.Width  = c(3.5, 3.0, 3.1),
  Petal.Length = c(1.4, 4.2, 5.4),
  Petal.Width  = c(0.2, 1.5, 2.1)
)

prediction <- predict(tunedKnnModel, newdata = newFlowers)
getPredictionResponse(prediction)