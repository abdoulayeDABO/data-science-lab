options(stringsAsFactors = F)
options(scipen = 100)

# Installer les paquages
install.packages("questionr")

# Charger les libs
library(ggplot2)
library(questionr)

# Charger les donnees
data(rp2018)

# Filtre
rp <- filter(
  rp2018,
  departement %in% c("Oise", "Rhône", "Hauts-de-Seine", "Lozère", "Bouches-du-Rhône")
)

rp["cadres"]

ggplot(rp) +
  geom_histogram(aes(x = cadres)) 

ggplot(rp) +
  geom_point(
    aes(x = dipl_sup, y = cadres),
    color = "darkgreen", size = 3, alpha = 0.3
  )

ggplot(rp) +
  geom_boxplot(
    aes(x = departement, y = maison),
    fill = "wheat", color = "tomato4"
  )

ggplot(rp) +
  geom_bar(aes(x = departement))


data("economics")
economics

ggplot(economics) +
  geom_line(aes(x = date, y = unemploy))


ggplot(rp) +
  geom_point(
    aes(x = dipl_sup, y = cadres, color = departement, size = pop_tot)
  )

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, shape = Species)) +
  geom_point() +
  theme_bw()
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) +
  geom_point() + theme_bw()


#########################
# Read from file and plot
#########################

install.packages("readxl")
library(readxl)
library(ggplot2)
data <- read_excel("./data/SaleData.xlsx")
data <- read.csv("./data/StudentPerformanceFactors.csv", stringsAsFactors = FALSE)
head(data)
 
 # Scatter plot avec deux variables numériques
 ggplot(data, aes(x = Hours_Studied, y = Previous_Scores, shape = Gender)) +
   geom_point(alpha = 0.6, size = 2) +
   theme_bw() +
   labs(title = "Relation heures d'étude vs scores",
        x = "Heures d'étude",
        y = "Scores précédents")

 ggplot(data, aes(x = Distance_from_Home, y = Previous_Scores, fill = Gender)) +
   geom_boxplot() +
   theme_bw() +
   labs(title = "Scores selon la distance du domicile",
        x = "Distance du domicile",
        y = "Scores précédents")

f = function(z){ 1 / (1+exp(-z)) }
f(0)

x <- seq(-5, 5, by=0.1)
y <- sapply(x, f)
df <- data.frame(x=x, y=y)

ggplot(data.frame(x,y), aes(x = x, y = y)) + 
  geom_line(colour = "blue") +
  theme_bw() +
  labs(title = "Fonction logistique") +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed")
          
