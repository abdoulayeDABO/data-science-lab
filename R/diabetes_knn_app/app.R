# ============================================================
# DÉPLOIEMENT PRO — Application Shiny
# Modèle KNN Diabète · Sans icônes · Haut contraste
# ============================================================

library(shiny)
library(mlr)
library(tidyverse)

model <- readRDS("diabetes_knn_model.rds")

ui <- fluidPage(
  title = "DiabAI · Clinical Decision Support",
  
  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=DM+Serif+Display&family=DM+Sans:opsz,wght@9..40,400;9..40,500;9..40,600;9..40,700&display=swap"),
    
    tags$style(HTML("

*, *::before, *::after { box-sizing: border-box; margin: 0; padding: 0; }

:root {
  --bg:         #F4F5F9;
  --surface:    #FFFFFF;
  --border:     #D1D5E0;
  --text:       #0D1117;
  --text-sec:   #3D4451;
  --muted:      #5A6270;
  --accent:     #0047CC;
  --accent-dk:  #003399;
  --accent-lt:  #E0EAFF;
  --accent-tx:  #002B99;

  --normal-bg:  #D6F5E8;
  --normal-bd:  #3CA86B;
  --normal-tx:  #0A5C2E;

  --chem-bg:    #FEF0D0;
  --chem-bd:    #C87D0E;
  --chem-tx:    #6B3C00;

  --overt-bg:   #FDDEDE;
  --overt-bd:   #C42B2B;
  --overt-tx:   #6B0000;

  --radius:     12px;
  --shadow:     0 1px 4px rgba(0,0,0,.07), 0 4px 18px rgba(0,0,0,.07);
}

html, body {
  background: var(--bg);
  font-family: 'DM Sans', sans-serif;
  color: var(--text);
  font-size: 15px;
  line-height: 1.6;
  min-height: 100vh;
}

/* ── Shell ──────────────────────────────────────────────── */
.app-shell {
  max-width: 1080px;
  margin: 0 auto;
  padding: 0 24px 64px;
}

/* ── Header ─────────────────────────────────────────────── */
.app-header {
  display: flex;
  align-items: center;
  justify-content: space-between;
  padding: 30px 0 30px;
  border-bottom: 2px solid var(--border);
  margin-bottom: 36px;
}
.brand-name {
  font-family: 'DM Serif Display', serif;
  font-size: 1.6em;
  letter-spacing: -.5px;
  color: var(--text);
}
.brand-sub {
  font-size: .78em;
  color: var(--muted);
  margin-top: 1px;
}
.header-badge {
  background: var(--accent-lt);
  color: var(--accent-tx);
  font-size: .72em;
  font-weight: 700;
  padding: 6px 16px;
  border-radius: 999px;
  border: 1px solid #B0C8FF;
  letter-spacing: .6px;
}

/* ── Cards ──────────────────────────────────────────────── */
.card {
  background: var(--surface);
  border: 1.5px solid var(--border);
  border-radius: var(--radius);
  box-shadow: var(--shadow);
  padding: 28px;
}
.card-title {
  font-size: .68em;
  font-weight: 700;
  letter-spacing: 1.4px;
  text-transform: uppercase;
  color: var(--muted);
  margin-bottom: 24px;
}

/* ── Fields ─────────────────────────────────────────────── */
.field-group { margin-bottom: 22px; }
.field-label {
  display: flex;
  justify-content: space-between;
  align-items: baseline;
  font-size: .84em;
  font-weight: 600;
  color: var(--text);
  margin-bottom: 8px;
}
.field-range {
  font-size: .76em;
  color: var(--muted);
  font-weight: 400;
}

.shiny-input-container { margin-bottom: 0 !important; }
.form-control {
  width: 100% !important;
  border: 2px solid var(--border) !important;
  border-radius: 9px !important;
  padding: 10px 14px !important;
  font-family: 'DM Sans', sans-serif !important;
  font-size: .94em !important;
  color: var(--text) !important;
  background: var(--bg) !important;
  font-weight: 500 !important;
  transition: border-color .15s, box-shadow .15s !important;
}
.form-control:focus {
  border-color: var(--accent) !important;
  box-shadow: 0 0 0 3px rgba(0,71,204,.12) !important;
  outline: none !important;
  background: #fff !important;
}

.range-bar {
  height: 4px;
  background: var(--border);
  border-radius: 999px;
  margin-top: 8px;
  position: relative;
  overflow: hidden;
}
.range-fill {
  position: absolute; left: 0; top: 0; height: 100%;
  background: var(--accent);
  border-radius: 999px;
  transition: width .3s ease;
}

/* ── Button ─────────────────────────────────────────────── */
.btn-predict {
  width: 100%;
  background: var(--accent) !important;
  color: #FFFFFF !important;
  border: none !important;
  border-radius: 9px !important;
  padding: 13px 24px !important;
  font-family: 'DM Sans', sans-serif !important;
  font-size: .95em !important;
  font-weight: 700 !important;
  letter-spacing: .4px !important;
  cursor: pointer !important;
  margin-top: 8px !important;
  transition: background .15s, transform .12s, box-shadow .15s !important;
  box-shadow: 0 2px 10px rgba(0,71,204,.35) !important;
}
.btn-predict:hover {
  background: var(--accent-dk) !important;
  transform: translateY(-1px) !important;
  box-shadow: 0 4px 18px rgba(0,71,204,.4) !important;
}
.btn-predict:active { transform: translateY(0) !important; }

/* ── Info Banner ─────────────────────────────────────────── */
.info-banner {
  background: var(--accent-lt);
  border: 1.5px solid #B0C8FF;
  border-radius: 9px;
  padding: 11px 14px;
  font-size: .78em;
  color: var(--accent-tx);
  font-weight: 500;
  margin-top: 18px;
  line-height: 1.6;
}

/* ── Divider ─────────────────────────────────────────────── */
.divider { border: none; border-top: 1.5px solid var(--border); margin: 22px 0; }

/* ── Result Empty ────────────────────────────────────────── */
.result-empty {
  display: flex; flex-direction: column;
  align-items: center; justify-content: center;
  gap: 10px; min-height: 190px;
  color: var(--muted); font-size: .88em;
  font-weight: 500;
}
.empty-dot {
  width: 40px; height: 40px;
  border: 3px solid var(--border);
  border-radius: 50%;
}

/* ── Result Card ─────────────────────────────────────────── */
.result-card { animation: fadeUp .3s cubic-bezier(.16,1,.3,1); }
@keyframes fadeUp {
  from { opacity: 0; transform: translateY(10px); }
  to   { opacity: 1; transform: translateY(0); }
}

.result-chip {
  display: inline-block;
  padding: 5px 16px;
  border-radius: 999px;
  border: 2px solid;
  font-size: .72em;
  font-weight: 700;
  letter-spacing: 1px;
  text-transform: uppercase;
  margin-bottom: 14px;
}
.chip-Normal   { background: var(--normal-bg); border-color: var(--normal-bd); color: var(--normal-tx); }
.chip-Chemical { background: var(--chem-bg);   border-color: var(--chem-bd);   color: var(--chem-tx); }
.chip-Overt    { background: var(--overt-bg);  border-color: var(--overt-bd);  color: var(--overt-tx); }

.result-class {
  font-family: 'DM Serif Display', serif;
  font-size: 3em;
  line-height: 1.05;
  margin-bottom: 10px;
  letter-spacing: -.5px;
}
.class-Normal   { color: var(--normal-tx); }
.class-Chemical { color: var(--chem-tx); }
.class-Overt    { color: var(--overt-tx); }

.result-desc {
  font-size: .87em;
  color: var(--text-sec);
  max-width: 330px;
  margin: 0 auto 24px;
  line-height: 1.75;
  font-weight: 400;
}

/* ── Risk Meter ──────────────────────────────────────────── */
.risk-meter { margin-top: 6px; }
.risk-label {
  display: flex; justify-content: space-between;
  font-size: .74em; color: var(--text-sec);
  margin-bottom: 7px; font-weight: 600;
}
.risk-track {
  height: 9px; background: var(--border);
  border-radius: 999px; overflow: hidden;
  border: 1px solid #C5CAD8;
}
.risk-fill {
  height: 100%; border-radius: 999px;
  transition: width .7s cubic-bezier(.16,1,.3,1);
}

/* ── Stats Grid ──────────────────────────────────────────── */
.stats-grid {
  display: grid;
  grid-template-columns: repeat(3, 1fr);
  gap: 12px;
}
.stat-box {
  background: var(--bg);
  border: 1.5px solid var(--border);
  border-radius: 10px;
  padding: 16px 10px;
  text-align: center;
}
.stat-val {
  font-size: 1.5em;
  font-weight: 700;
  color: var(--text);
  line-height: 1.1;
}
.stat-name {
  font-size: .72em;
  color: var(--text-sec);
  font-weight: 600;
  margin-top: 4px;
  letter-spacing: .2px;
}
.stat-unit {
  font-size: .65em;
  color: var(--muted);
  font-weight: 400;
}

/* ── Footer ──────────────────────────────────────────────── */
.app-footer {
  margin-top: 48px;
  padding-top: 18px;
  border-top: 1.5px solid var(--border);
  display: flex; align-items: center;
  justify-content: space-between;
  font-size: .74em;
  color: var(--muted);
  font-weight: 500;
}
    "))
  ),
  
  div(class = "app-shell",
      
      # ── Header ─────────────────────────────────────────────
      div(class = "app-header",
          div(
            div(class = "brand-name", "Diabete prediction model"),
            div(class = "brand-sub", "Clinical Decision Support — KNN Classifier")
          ),
          div(class = "header-badge", "RESEARCH USE ONLY")
      ),
      
      fluidRow(
        
        # ── LEFT : Formulaire ───────────────────────────────────
        column(4,
               div(class = "card",
                   div(class = "card-title", "Patient Biomarkers"),
                   
                   div(class = "field-group",
                       div(class = "field-label",
                           span("Glucose"),
                           span(class = "field-range", "60 – 353 mg/dL")),
                       numericInput("glucose", NULL, value = 100, min = 60, max = 353, step = 1),
                       div(class = "range-bar",
                           div(class = "range-fill", id = "bar-glucose", style = "width:14%;"))
                   ),
                   
                   div(class = "field-group",
                       div(class = "field-label",
                           span("Insulin"),
                           span(class = "field-range", "45 – 1600 µU/mL")),
                       numericInput("insulin", NULL, value = 300, min = 45, max = 1600, step = 1),
                       div(class = "range-bar",
                           div(class = "range-fill", id = "bar-insulin", style = "width:16%;"))
                   ),
                   
                   div(class = "field-group",
                       div(class = "field-label",
                           span("SSPG"),
                           span(class = "field-range", "10 – 748 mg/dL")),
                       numericInput("sspg", NULL, value = 150, min = 10, max = 748, step = 1),
                       div(class = "range-bar",
                           div(class = "range-fill", id = "bar-sspg", style = "width:19%;"))
                   ),
                   
                   hr(class = "divider"),
                   actionButton("predict_btn", "Run classification", class = "btn-predict"),
                   
                   div(class = "info-banner",
                       "Modèle KNN — mclust::diabetes (145 obs.) — Usage recherche uniquement."
                   )
               )
        ),
        
        # ── RIGHT : Résultats ────────────────────────────────────
        column(8,
               
               div(class = "card",
                   style = "text-align:center; padding:36px 28px; margin-bottom:16px;",
                   uiOutput("result_panel")
               ),
               
               div(class = "card",
                   div(class = "card-title", "Valeurs soumises"),
                   div(class = "stats-grid",
                       div(class = "stat-box",
                           div(class = "stat-val", textOutput("val_glucose", inline = TRUE)),
                           div(class = "stat-name", "Glucose", tags$span(class = "stat-unit", " mg/dL"))
                       ),
                       div(class = "stat-box",
                           div(class = "stat-val", textOutput("val_insulin", inline = TRUE)),
                           div(class = "stat-name", "Insulin", tags$span(class = "stat-unit", " µU/mL"))
                       ),
                       div(class = "stat-box",
                           div(class = "stat-val", textOutput("val_sspg", inline = TRUE)),
                           div(class = "stat-name", "SSPG", tags$span(class = "stat-unit", " mg/dL"))
                       )
                   )
               )
        )
      ),
      
      div(class = "app-footer",
          span("DiabAI — KNN + mlr + Shiny"),
          span("mclust::diabetes — 145 observations — 3 classes")
      )
  ),
  
  tags$script(HTML("
    function pct(v, mn, mx) {
      return Math.round(Math.max(0, Math.min(100, (v - mn) / (mx - mn) * 100)));
    }
    function bindBar(inputId, barId, mn, mx) {
      var el = document.getElementById(inputId);
      if (!el) return;
      el.addEventListener('input', function() {
        var b = document.getElementById(barId);
        if (b) b.style.width = pct(+this.value, mn, mx) + '%';
      });
    }
    document.addEventListener('DOMContentLoaded', function() {
      bindBar('glucose', 'bar-glucose', 60, 353);
      bindBar('insulin', 'bar-insulin', 45, 1600);
      bindBar('sspg',    'bar-sspg',    10, 748);
    });
  "))
)

# ── SERVER ────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  output$val_glucose <- renderText(input$glucose)
  output$val_insulin <- renderText(input$insulin)
  output$val_sspg    <- renderText(input$sspg)
  
  result <- eventReactive(input$predict_btn, {
    new_pt <- tibble(
      glucose = as.numeric(input$glucose),
      insulin = as.numeric(input$insulin),
      sspg    = as.numeric(input$sspg)
    )
    pred <- predict(model, newdata = new_pt)
    as.character(getPredictionResponse(pred))
  })
  
  output$result_panel <- renderUI({
    
    if (input$predict_btn == 0) {
      return(div(class = "result-empty",
                 div(class = "empty-dot"),
                 div("Entrez les biomarqueurs du patient"),
                 div(style = "font-size:.8em; color:#8A93A8; font-weight:400;",
                     "puis cliquez Run Classification")
      ))
    }
    
    cls <- result()
    
    cfg <- switch(cls,
                  "Normal"   = list(
                    chip  = "chip-Normal",
                    label = "NORMAL",
                    cname = "class-Normal",
                    desc  = "Aucun signe de diabète détecté. Les biomarqueurs sont dans les limites normales.",
                    risk  = 12,
                    color = "#1A7A45",
                    track = "#C2EDD5"
                  ),
                  "Chemical" = list(
                    chip  = "chip-Chemical",
                    label = "CHEMICAL",
                    cname = "class-Chemical",
                    desc  = "Diabète chimique détecté. Des examens complémentaires sont fortement recommandés.",
                    risk  = 60,
                    color = "#9A5A00",
                    track = "#FAD98A"
                  ),
                  "Overt"    = list(
                    chip  = "chip-Overt",
                    label = "OVERT",
                    cname = "class-Overt",
                    desc  = "Diabète avéré. Une prise en charge médicale immédiate est nécessaire.",
                    risk  = 92,
                    color = "#A01010",
                    track = "#F5AAAA"
                  )
    )
    
    div(class = "result-card",
        div(class = paste("result-chip", cfg$chip), cfg$label),
        div(class = paste("result-class", cfg$cname), cls),
        div(class = "result-desc", cfg$desc),
        div(class = "risk-meter",
            div(class = "risk-label",
                span("Indice de risque estimé"),
                span(paste0(cfg$risk, " / 100"))
            ),
            div(class = "risk-track", style = paste0("background:", cfg$track, ";"),
                div(class = "risk-fill",
                    style = paste0("width:", cfg$risk, "%; background:", cfg$color, ";"))
            )
        )
    )
  })
}

shinyApp(ui = ui, server = server)