# ============================================
# DÉPLOIEMENT - API REST avec Plumber
# Modèle KNN - Diabète
# ============================================
# Lancement :
#   library(plumber)
#   pr("plumber_api.R") %>% pr_run(port = 8000)
# ============================================

library(plumber)
library(mlr)
library(tidyverse)

# Charger le modèle au démarrage de l'API
model <- readRDS("diabetes_knn_model.rds")

#* @apiTitle API Diabète - KNN
#* @apiDescription Prédit la classe diabétique d'un patient (Normal / Chemical / Overt)

# ── Endpoint de prédiction ──────────────────

#* Prédire la classe d'un patient
#* @param glucose Taux de glucose (mg/dL)
#* @param insulin Taux d'insuline (µU/mL)
#* @param sspg Valeur SSPG (mg/dL)
#* @get /predict
function(glucose, insulin, sspg) {
  
  # Convertir les paramètres en numérique
  new_patient <- tibble(
    glucose = as.numeric(glucose),
    insulin = as.numeric(insulin),
    sspg    = as.numeric(sspg)
  )
  
  # Prédiction
  pred   <- predict(model, newdata = new_patient)
  classe <- as.character(getPredictionResponse(pred))
  
  # Retourner résultat JSON
  list(
    status    = "success",
    input     = list(glucose = glucose, insulin = insulin, sspg = sspg),
    prediction = classe,
    interpretation = switch(classe,
      "Normal"   = "Aucun signe de diabète détecté",
      "Chemical" = "Diabète chimique - tests supplémentaires recommandés",
      "Overt"    = "Diabète avéré - prise en charge médicale nécessaire"
    )
  )
}

# ── Endpoint de santé ───────────────────────

#* Vérifier que l'API est active
#* @get /health
function() {
  list(
    status  = "ok",
    message = "API KNN Diabète opérationnelle",
    model   = "KNN (mlr)",
    classes = c("Normal", "Chemical", "Overt")
  )
}
