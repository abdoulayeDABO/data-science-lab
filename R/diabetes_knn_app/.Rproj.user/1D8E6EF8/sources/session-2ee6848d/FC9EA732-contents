# ============================================
# ÉTAPE 1 : Sauvegarder ton modèle entraîné
# Exécute ce script UNE SEULE FOIS
# ============================================

library(mlr)
library(tidyverse)
library(mclust)

# Charger et préparer les données
data(diabetes, package = "mclust")
diabetesTib <- as_tibble(diabetes)

# Créer la task
diabetesTask <- makeClassifTask(data = diabetesTib, target = "class")

# Tuning du paramètre k
knnParamSpace <- makeParamSet(makeDiscreteParam("k", values = 1:10))
gridSearch    <- makeTuneControlGrid()
cvForTuning   <- makeResampleDesc("RepCV", folds = 10, reps = 20)

tunedK <- tuneParams(
  "classif.knn",
  task       = diabetesTask,
  resampling = cvForTuning,
  par.set    = knnParamSpace,
  control    = gridSearch
)

# Entraîner le modèle final avec le meilleur k
tunedKnn      <- setHyperPars(makeLearner("classif.knn"), par.vals = tunedK$x)
tunedKnnModel <- train(tunedKnn, diabetesTask)

# ✅ Sauvegarder le modèle dans un fichier .rds
saveRDS(tunedKnnModel, "diabetes_knn_model.rds")

cat("✅ Modèle sauvegardé dans : diabetes_knn_model.rds\n")
cat("📌 Meilleur k trouvé :", tunedK$x$k, "\n")
