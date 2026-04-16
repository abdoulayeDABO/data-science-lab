# ============================================================
# PROJET 2 - Exercice 1 : Arbre de décision - Play Golf
# ============================================================

# ── 0. DONNÉES ───────────────────────────────────────────────

golf_train <- data.frame(
  Day         = c("07-05","07-06","07-07","07-09","07-10",
                  "07-12","07-14","07-15","07-20","07-21",
                  "07-22","07-23","07-26","07-30"),
  Temperature = c("hot","hot","hot","cool","cool","mild","cool",
                  "mild","mild","mild","hot","mild","cool","mild"),
  Outlook     = c("sunny","sunny","overcast","rain","overcast","sunny",
                  "sunny","rain","sunny","overcast","overcast","rain","rain","rain"),
  Humidity    = c("high","high","high","normal","normal","high","normal",
                  "normal","normal","high","normal","high","normal","high"),
  Windy       = c(FALSE,TRUE,FALSE,FALSE,TRUE,FALSE,FALSE,
                  FALSE,TRUE,TRUE,FALSE,TRUE,TRUE,FALSE),
  PlayGolf    = c("no","no","yes","yes","yes","no","yes",
                  "yes","yes","yes","yes","no","no","yes"),
  stringsAsFactors = FALSE
)

golf_test <- data.frame(
  Day         = c("today","tomorrow"),
  Temperature = c("cool","mild"),
  Outlook     = c("sunny","sunny"),
  Humidity    = c("normal","normal"),
  Windy       = c(FALSE,FALSE),
  stringsAsFactors = FALSE
)

cat("═══════════════════════════════════════\n")
cat("   APERÇU DES DONNÉES\n")
cat("═══════════════════════════════════════\n")
cat(sprintf("Dimensions : %d lignes x %d colonnes\n", nrow(golf_train), ncol(golf_train)))
print(golf_train)


# ══════════════════════════════════════════════════════════
# 1. FONCTION ENTROPIE
# H(S) = - Σ p_i * log2(p_i)
# ══════════════════════════════════════════════════════════

entropie <- function(y) {
  freq <- table(y)
  prob <- freq / length(y)
  prob <- prob[prob > 0]       # supprimer les zéros (évite log2(0))
  -sum(prob * log2(prob))      # valeur retournée
}

H_total <- entropie(golf_train$PlayGolf)

cat("\n═══════════════════════════════════════\n")
cat("   ENTROPIE DE LA RACINE\n")
cat("═══════════════════════════════════════\n")
cat("  Distribution : yes=9, no=5, Total=14\n")
cat("  Formule : H(S) = -(9/14)*log2(9/14) - (5/14)*log2(5/14)\n")
cat(sprintf("  H(S) = %.4f bits\n", H_total))


# ══════════════════════════════════════════════════════════
# 2. GAIN D'INFORMATION
# Gain(S,A) = H(S) - Σ (|Sv|/|S|) * H(Sv)
# ══════════════════════════════════════════════════════════

gain_info <- function(data, attribut, cible = "PlayGolf") {
  S         <- data[[cible]]
  H_S       <- entropie(S)
  n         <- length(S)
  vals      <- unique(data[[attribut]])
  H_pondere <- 0
  details   <- list()
  
  for (v in vals) {
    Sv    <- S[data[[attribut]] == v]
    poids <- length(Sv) / n
    H_Sv  <- entropie(Sv)
    H_pondere          <- H_pondere + poids * H_Sv
    details[[as.character(v)]] <- list(n = length(Sv), H = H_Sv, poids = poids)
  }
  
  gain <- H_S - H_pondere
  list(gain = gain, details = details, H_S = H_S, H_pondere = H_pondere)
}

attributs <- c("Outlook", "Temperature", "Humidity", "Windy")

cat("\n═══════════════════════════════════════\n")
cat("   GAIN D'INFORMATION PAR ATTRIBUT\n")
cat("═══════════════════════════════════════\n")

gains <- sapply(attributs, function(a) {
  res <- gain_info(golf_train, a)
  cat(sprintf("\n▶ %s\n", a))
  for (v in names(res$details)) {
    d <- res$details[[v]]
    cat(sprintf("   %-10s : n=%2d, poids=%.3f, H=%.4f bits\n",
                v, d$n, d$poids, d$H))
  }
  cat(sprintf("   H_ponderee         = %.4f bits\n", res$H_pondere))
  cat(sprintf("   Gain(%-12s) = %.4f - %.4f = %.4f bits\n",
              a, res$H_S, res$H_pondere, res$gain))
  res$gain
})

cat("\n═══════════════════════════════════════\n")
cat("   RÉSUMÉ DES GAINS (trié)\n")
cat("═══════════════════════════════════════\n")
gains_df <- data.frame(Attribut = attributs, Gain = round(gains, 4))
gains_df <- gains_df[order(-gains_df$Gain), ]
print(gains_df, row.names = FALSE)
cat(sprintf("\n  ★ Meilleur attribut racine : %s (Gain = %.4f bits)\n",
            gains_df$Attribut[1], gains_df$Gain[1]))


# ══════════════════════════════════════════════════════════
# 3. CONSTRUCTION DE L'ARBRE avec rpart
# ══════════════════════════════════════════════════════════

if (!require(rpart))      install.packages("rpart")
if (!require(rpart.plot)) install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

# Conversion en facteurs
golf_train$Temperature <- as.factor(golf_train$Temperature)
golf_train$Outlook     <- as.factor(golf_train$Outlook)
golf_train$Humidity    <- as.factor(golf_train$Humidity)
golf_train$Windy       <- as.factor(golf_train$Windy)
golf_train$PlayGolf    <- as.factor(golf_train$PlayGolf)

arbre <- rpart(
  PlayGolf ~ Temperature + Outlook + Humidity + Windy,
  data    = golf_train,
  method  = "class",
  parms   = list(split = "information"),
  control = rpart.control(minsplit = 1, minbucket = 1, cp = 0)
)

cat("\n═══════════════════════════════════════\n")
cat("   STRUCTURE DE L'ARBRE\n")
cat("═══════════════════════════════════════\n")
print(arbre)

# Visualisation de l'arbre
rpart.plot(arbre,
           type  = 4,
           extra = 104,
           main  = "Arbre de décision - Play Golf",
           under = TRUE)


# ══════════════════════════════════════════════════════════
# 4. ERREUR DU MODÈLE (méthode Holdout)
# Erreur = Nb prédictions incorrectes / Nb total instances
# ══════════════════════════════════════════════════════════

pred_train <- predict(arbre, golf_train, type = "class")
nb_erreurs <- sum(pred_train != golf_train$PlayGolf)
erreur     <- nb_erreurs / nrow(golf_train)

cat("\n═══════════════════════════════════════\n")
cat("   ERREUR DU MODÈLE\n")
cat("═══════════════════════════════════════\n")
cat("  Formule : Erreur = Nb_erreurs / Nb_total\n")
cat(sprintf("  Erreur = %d / %d = %.4f  (%.1f%%)\n",
            nb_erreurs, nrow(golf_train), erreur, erreur * 100))
cat(sprintf("  Précision = %.4f  (%.1f%%)\n", 1 - erreur, (1 - erreur) * 100))

cat("\n  Matrice de confusion :\n")
print(table(Prédit = pred_train, Réel = golf_train$PlayGolf))


# ══════════════════════════════════════════════════════════
# 5. PRÉDICTIONS HOLDOUT (today & tomorrow)
# ══════════════════════════════════════════════════════════

golf_test$Temperature <- as.factor(golf_test$Temperature)
golf_test$Outlook     <- as.factor(golf_test$Outlook)
golf_test$Humidity    <- as.factor(golf_test$Humidity)
golf_test$Windy       <- as.factor(golf_test$Windy)

pred_test              <- predict(arbre, golf_test, type = "class")
golf_test$PlayGolf_pred <- pred_test

cat("\n═══════════════════════════════════════\n")
cat("   PRÉDICTIONS HOLDOUT\n")
cat("═══════════════════════════════════════\n")
print(golf_test[, c("Day","Temperature","Outlook","Humidity","Windy","PlayGolf_pred")])
