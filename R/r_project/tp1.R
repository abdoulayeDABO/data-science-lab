# ========================================
# TD ARBRES DE DÉCISION - Package rpart
# ========================================
library(rpart)
library(rpart.plot)

# Lecture des données
data = read.table('./data/arbre.txt', header = TRUE, sep = '\t')
data

# Création de l'arbre de décision
arbreDecision <- rpart(Jouer ~ Temps + Temperature + Humidite + Venteux, data = data, method = "class", control = rpart.control( minsplit = 1, minbucket = 1 ))
rpart.plot(arbreDecision)

summary(arbreDecision)
printcp(arbreDecision)

