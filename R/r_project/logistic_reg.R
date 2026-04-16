################################################################################
# PROJET : MODÉLISATION DE LA SURVIE DES PASSAGERS DU TITANIC
# Méthode : Régression logistique binaire — Framework MLR
# Données : titanic_train / titanic_test (package {titanic})
################################################################################


# ==============================================================================
# 1. ENVIRONNEMENT DE TRAVAIL
# ==============================================================================

library(mlr)        # Machine Learning en R — gestion des tâches, apprenants, CV
library(tidyverse)  # Manipulation de données (dplyr, tidyr) et visualisation (ggplot2)

install.packages("titanic")
data(titanic_train, package = "titanic")
titanicTib <- as_tibble(titanic_train)


# ==============================================================================
# 2. PRÉPARATION ET NETTOYAGE DES DONNÉES
# ==============================================================================
# — Conversion des variables catégorielles en facteurs
# — Ingénierie de variable : FamSize = nombre de membres de la famille à bord
# — Réduction du jeu de données aux variables pertinentes pour la modélisation

fctrs <- c("Survived", "Sex", "Pclass")

titanicClean <- titanicTib %>%
  mutate_at(.vars = fctrs, .funs = factor) %>%
  mutate(FamSize = SibSp + Parch) %>%
  select(Survived, Pclass, Sex, Age, Fare, FamSize)


# ==============================================================================
# 3. ANALYSE EXPLORATOIRE (EDA)
# ==============================================================================

# Passage en format long pour faciliter la visualisation facettée
titanicUntidy <- gather(titanicClean, key = "Variable", value = "Value", -Survived)

# ------------------------------------------------------------------------------
# 3.1 Variables continues — Distribution selon la survie (graphiques en violon)
#     Quantiles affichés : Q1 / Médiane / Q3
# ------------------------------------------------------------------------------
titanicUntidy %>%
  filter(Variable != "Pclass" & Variable != "Sex") %>%
  ggplot(aes(Survived, as.numeric(Value))) +
  facet_wrap(~ Variable, scales = "free_y") +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
  theme_bw()

# ------------------------------------------------------------------------------
# 3.2 Variables catégorielles — Effectifs selon la survie (barres groupées)
#     position = "dodge"  → comparaison des effectifs absolus
#     position = "stack"  → visualisation des volumes cumulés
#     position = "fill"   → comparaison des proportions (taux de survie relatif)
# ------------------------------------------------------------------------------
titanicUntidy %>%
  filter(Variable == "Pclass" | Variable == "Sex") %>%
  ggplot(aes(Value, fill = Survived)) +
  facet_wrap(~ Variable, scales = "free_x") +
  geom_bar(position = "dodge") +
  theme_bw()

titanicUntidy %>%
  filter(Variable == "Pclass" | Variable == "Sex") %>%
  ggplot(aes(Value, fill = Survived)) +
  facet_wrap(~ Variable, scales = "free_x") +
  geom_bar(position = "stack") +
  theme_bw()

titanicUntidy %>%
  filter(Variable == "Pclass" | Variable == "Sex") %>%
  ggplot(aes(Value, fill = Survived)) +
  facet_wrap(~ Variable, scales = "free_x") +
  geom_bar(position = "fill") +
  theme_bw()


# ==============================================================================
# 4. GESTION DES VALEURS MANQUANTES
# ==============================================================================

titanicClean$Age[1:60]       # Inspection des 60 premières observations
sum(is.na(titanicClean$Age)) # Quantification des valeurs manquantes sur Age

# Imputation par la moyenne — méthode simple adaptée à ce contexte
imp <- impute(titanicClean, cols = list(Age = imputeMean()))
sum(is.na(imp$data$Age))     # Contrôle post-imputation — résultat attendu : 0


# ==============================================================================
# 5. MODÉLISATION — RÉGRESSION LOGISTIQUE BINAIRE
# ==============================================================================

# Définition de la tâche de classification supervisée (variable cible : Survived)
titanicTask <- makeClassifTask(data = imp$data, target = "Survived")

# Instanciation de l'apprenant avec sortie probabiliste
logReg       <- makeLearner("classif.logreg", predict.type = "prob")

# Entraînement du modèle sur l'ensemble d'apprentissage complet
logRegModel  <- train(logReg, titanicTask)


# ==============================================================================
# 6. ÉVALUATION — VALIDATION CROISÉE RÉPÉTÉE (RepCV)
# ==============================================================================
# — 10 folds × 50 répétitions — stratification sur la variable cible
# — L'imputation est encapsulée dans le pipeline pour prévenir toute fuite
#   de données entre les plis (data leakage)
#
# Métriques retenues :
#   acc → Taux de classification correcte (accuracy)
#   fpr → Taux de faux positifs (False Positive Rate)
#   fnr → Taux de faux négatifs (False Negative Rate)

logRegWrapper <- makeImputeWrapper("classif.logreg",
                                   cols = list(Age = imputeMean()))

kFold <- makeResampleDesc(method   = "RepCV",
                          folds    = 10,
                          reps     = 50,
                          stratify = TRUE)

logRegwithImpute <- resample(logRegWrapper,
                             titanicTask,
                             resampling = kFold,
                             measures   = list(acc, fpr, fnr))


# ==============================================================================
# 7. INTERPRÉTATION DES PARAMÈTRES DU MODÈLE
# ==============================================================================

logRegModelData <- getLearnerModel(logRegModel)

coef(logRegModelData)  # Coefficients bruts (échelle log-odds)

# Conversion en Odds Ratios avec intervalles de confiance à 95%
# Lecture : OR > 1 → facteur favorable à la survie
#           OR < 1 → facteur défavorable à la survie
exp(cbind(Odds_Ratio = coef(logRegModelData),
          confint(logRegModelData)))


# ==============================================================================
# 8. PRÉDICTION SUR LE JEU DE TEST
# ==============================================================================

data(titanic_test, package = "titanic")
titanicNew <- as_tibble(titanic_test)

# Prétraitement identique au jeu d'entraînement
# Note : la variable Survived est absente du jeu de test par construction
titanicNewClean <- titanicNew %>%
  mutate_at(.vars = c("Sex", "Pclass"), .funs = factor) %>%
  mutate(FamSize = SibSp + Parch) %>%
  select(Pclass, Sex, Age, Fare, FamSize)

# Application du modèle entraîné — retourne classes prédites et probabilités
predict(logRegModel, newdata = titanicNewClean)
