install.packages("tidyverse")
library(tidyverse)

# install.packages(c("tibble", "dplyr", "ggplot2", "tidyr", "purrr"))
# library(tibble)
# library(dplyr)
# library(ggplot2)
# library(tidyr)
# library(purrr)


# myDf <- data.frame(x = 1:4,y = c("london", "beijing", "las vegas", "berlin"))
# dfToTib <- as_tibble(myDf)
# dfToTib

# read.csv()
# read_table()

# myDf <- data.frame(x = 1:4,y = c("london", "beijing", "las vegas", "berlin"), stringsAsFactors = FALSE)
# myDf
# str(myDf)


# data(starwars)
# starwars
# as.data.frame(starwars)
# 
# 
# sequentialTib <- tibble(nItems = c(12, 45, 107),
#                         cost = c(0.5, 1.2, 1.8),
#                         totalWorth = nItems * cost)
# sequentialTib


data("mtcars")

cars_data <- as_tibble(mtcars)
summary(cars_data)
cars_data
mtcars %>% select(-qsec, -vs)
mtcars %>% filter(cyl != 8)

# carb comme variable numérique
ggplot(mtcars, aes(x = drat, y = wt, color = carb)) +
  geom_point(size = 3) +
  labs(title = "carb numérique")

# carb comme facteur (catégories)
ggplot(mtcars, aes(x = drat, y = wt, color = as.factor(carb))) +
  geom_point(size = 3) +
  labs(title = "carb comme facteur")


tidyCarsData <- gather(
  cars_data, 
  key = "variable",       # Nom de la colonne clé
  value = "valeur",       # Nom de la colonne valeur
  vs, am, gear, carb      # Colonnes à rassembler
)

tidyCarsData


# Exercise 7
# Use a function from the purrr package to return a logical vector indicating whether the
# sum of the values in each column of the mtcars dataset is greater than 1,000.


s = map(mtcars, sum) 
l <- map_lgl(s, ~ . > 1000) # by substituting ~ for function(.).
l

#####################################

library(tibble)
library(tidyr)
patientData <- tibble(Patient = c("A", "B", "C"),
                      Month0 = c(21, 17, 29),
                      Month3 = c(20, 21, 27),
                      Month6 = c(21, 22, 23))
patientData

tidyPatientData <- gather(patientData, key = Month,
                          value = BMI, -Patient)
tidyPatientData

spread(tidyPatientData, key = Month, value = BMI)


####################################

listOfNumerics <- list(a = rnorm(5),
                       b = rnorm(9),
                       c = rnorm(10))
listOfNumerics

elementLengths <- vector("list", length = 3)
# elementLengths
# 
# for(i in seq_along(listOfNumerics)) {
#   elementLengths[[i]] <- length(listOfNumerics[[i]])
# }
# elementLengths

map(listOfNumerics, length)
len = map_int(listOfNumerics, length)
map_df(listOfNumerics, length)

par(mfrow = c(1, 3))
walk(listOfNumerics, hist)
