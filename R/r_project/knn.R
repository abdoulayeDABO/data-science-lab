###########################
# Install and load packages
###########################
install.packages("mlr", dependencies = TRUE)
library(mlr)
library(tidyverse)
library(ggplot2)
library(patchwork)



#########################################
# Loading and exploring the diabetes data 
#########################################
data(diabetes, package = "mclust")
View(diabetes)
diabetesTib <- as_tibble(diabetes)
diabetesTib
summary(diabetesTib)
head(diabetesTib)


palette_class <- c(
  "Chemical" = "#7B5EA7",
  "Normal"   = "#1B998B",
  "Overt"    = "#F2C94C"
)


theme_ds <- function() {
  theme_bw(base_size = 11) +
    theme(
      axis.title        = element_text(face = "bold"),
      panel.grid.minor  = element_blank(),
      legend.position   = "bottom"  
    )
}


p1 <- ggplot(diabetesTib, aes(glucose, insulin, col = class)) +
  geom_point(alpha = 0.75, size = 2.2) +
  scale_color_manual(values = palette_class, name = "Class") +
  guides(color = guide_legend(
    override.aes   = list(size = 4, alpha = 1),
    title.position = "top",
    title.hjust    = 0.5
  )) +
  labs(x = "Glucose (mg/dL)", y = "Insulin (µU/mL)") +
  theme_ds()

p2 <- ggplot(diabetesTib, aes(sspg, insulin, col = class)) +
  geom_point(alpha = 0.75, size = 2.2) +
  scale_color_manual(values = palette_class, name = "Class") +
  guides(color = guide_legend(
    override.aes   = list(size = 4, alpha = 1),
    title.position = "top",
    title.hjust    = 0.5
  )) +
  labs(x = "SSPG (mg/dL)", y = "Insulin (µU/mL)") +
  theme_ds()

p3 <- ggplot(diabetesTib, aes(sspg, glucose, col = class)) +
  geom_point(alpha = 0.75, size = 2.2) +
  scale_color_manual(values = palette_class, name = "Class") +
  guides(color = guide_legend(
    override.aes   = list(size = 4, alpha = 1),
    title.position = "top",
    title.hjust    = 0.5
  )) +
  labs(x = "SSPG (mg/dL)", y = "Glucose (mg/dL)") +
  theme_ds()


(p1 + p2) / (p3 + plot_spacer()) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title    = "Analyse exploratoire du dataset Diabetes",
    subtitle = "Comparaison des classes : Chemical · Normal · Overt",
    caption  = "Source : Reaven & Miller (1979)"
  ) &
  theme(
    # Légende unique
    legend.position   = "bottom",
    legend.title      = element_text(face = "bold", size = 11),
    legend.text       = element_text(size = 11),
    legend.key.size   = unit(1.1, "lines"),
    legend.background = element_rect(fill = "white", colour = "grey85",
                                     linewidth = 0.4),
    legend.margin     = margin(6, 12, 6, 12),
    # Titre global
    plot.title        = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle     = element_text(size = 12, hjust = 0.5, color = "grey40",
                                     margin = margin(b = 8)),
    plot.caption      = element_text(size = 9, color = "grey60")
  )


####################
# Building the model
####################
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

diabetesTask <- makeClassifTask(data = diabetesTib, target = "class")
#diabetesTask

knn <- makeLearner("classif.knn", par.vals = list("k" = 2))
#knn
# How to list all of mlr’s algorithms
# The mlr package has a large number of machine learning algorithms that we can give
# to the makeLearner() function, more than I can remember without checking! To list
# all the available learners, simply use
# listLearners()$class
# Or list them by function:
# listLearners("classif")$class
# listLearners("regr")$class
# listLearners("cluster")$class

knnModel <- train(knn, diabetesTask)
#knnModel

knnPred <- predict(knnModel, newdata = diabetesTib)
#knnPred

performance(knnPred, measures = list(mmce, acc))


#####################################################################
# Using cross-validation to tell if we’re overfitting or underfitting
#####################################################################


##########################
# Holdout cross-validation
##########################

holdout <- makeResampleDesc(method = "Holdout", split = 2/3, stratify = TRUE)
holdoutCV <- resample(learner = knn, task = diabetesTask, resampling = holdout, measures = list(mmce, acc))
holdoutCV$aggr

# CALCULATING A CONFUSION MATRIX
calculateConfusionMatrix(holdoutCV$pred, relative = TRUE)

# Relative confusion matrix (normalized by row/column):
#       predicted
# true       Chemical  Normal    Overt     -err.-   
#   Chemical 0.92/0.73 0.08/0.04 0.00/0.00 0.08     
#   Normal   0.08/0.13 0.92/0.96 0.00/0.00 0.08     
#   Overt    0.18/0.13 0.00/0.00 0.82/1.00 0.18     
#   -err.-        0.27      0.04      0.00 0.10    

# Nouvelle description : 10% test, pas de stratification
holdout_desc_10 <- makeResampleDesc(
  method = "Holdout",
  split = 0.9,        # 90% train, 10% test
  stratify = FALSE    # Pas de stratified sampling
)
holdout_desc_10
holdoutCV <- resample(learner = knn, task = diabetesTask, resampling = holdout_desc_10, measures = list(mmce, acc))
calculateConfusionMatrix(holdoutCV$pred, relative = TRUE)


#########################
# K-fold cross-validation
#########################

kFold <- makeResampleDesc(method = "RepCV", folds = 10, reps = 50, stratify = TRUE)
kFoldCV <- resample(learner = knn, task = diabetesTask, resampling = kFold, measures = list(mmce, acc))
kFoldCV$measures.test
kFoldCV$aggr
calculateConfusionMatrix(kFoldCV$pred, relative = TRUE)


################################
# Leave-one-out cross-validation
################################

LOO <- makeResampleDesc(method = "LOO")
LOOCV <- resample(learner = knn, task = diabetesTask, resampling = LOO, measures = list(mmce, acc))
LOOCV$aggr
calculateConfusionMatrix(LOOCV$pred, relative = TRUE)



###############################
# Tuning k to improve the model
###############################

knnParamSpace <- makeParamSet(makeDiscreteParam("k", values = 1:10))
gridSearch <- makeTuneControlGrid()
cvForTuning <- makeResampleDesc("RepCV", folds = 10, reps = 20)
tunedK <- tuneParams("classif.knn", task = diabetesTask, resampling = cvForTuning, par.set = knnParamSpace, control = gridSearch)
knnTuningData <- generateHyperParsEffectData(tunedK)
plotHyperParsEffect(knnTuningData, x = "k", y = "mmce.test.mean", plot.type = "line") + theme_bw()

# Now we can train our final model, using our tuned value of k:
tunedKnn <- setHyperPars(makeLearner("classif.knn"), par.vals = tunedK$x)
tunedKnnModel <- train(tunedKnn, diabetesTask)

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


############
# Prediction
############

newDiabetesPatients <- tibble(glucose = c(82, 108, 300),
                              insulin = c(361, 288, 1052),
                              sspg = c(200, 186, 135))
newPatientsPred <- predict(tunedKnnModel, newdata = newDiabetesPatients)
getPredictionResponse(newPatientsPred)


