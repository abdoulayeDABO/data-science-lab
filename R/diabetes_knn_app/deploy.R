install.packages('rsconnect')
install.packages("dplyr")  # Package R

rsconnect::setAccountInfo(name='abdoulayedabo',
                          token='93C870E10F87F5F18B45B1B5AD3DEC3F',
                          secret='wfqM6gt9U5iOwO2vYEs9oxMtOdCMOqjXXiEnEWUJ')

library(rsconnect)
rsconnect::deployApp('/home/mdialloc19/Bureau/Abdoulaye/python/data-science-lab/R/diabetes_knn_app')