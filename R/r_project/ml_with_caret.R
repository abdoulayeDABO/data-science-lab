# Helper packages
library(dplyr)     # for data manipulation
library(ggplot2)   # for awesome graphics

# Modeling process packages
library(rsample)   # for resampling procedures
library(caret)     # for resampling and model training
library(h2o)       # for resampling and model training

# h2o set-up 
h2o.no_progress()  # turn off h2o progress bars
h2o.init()         # launch h2o


# Ames housing data
ames <- AmesHousing::make_ames()
ames.h2o <- as.h2o(ames)

# Job attrition data
churn <- rsample::attrition %>% 
  mutate_if(is.ordered, .funs = factor, ordered = FALSE)
churn.h2o <- as.h2o(churn)



# Using base R
# set.seed(123)  # for reproducibility
# index_1 <- sample(1:nrow(ames), round(nrow(ames) * 0.7))
# train_1 <- ames[index_1, ]
# test_1  <- ames[-index_1, ]

# # Using caret package
# set.seed(123)  # for reproducibility
# index_2 <- createDataPartition(ames$Sale_Price, p = 0.7, 
#                                list = FALSE)
# train_2 <- ames[index_2, ]
# test_2  <- ames[-index_2, ]
# 
# # Using rsample package
# set.seed(123)  # for reproducibility
# split_1  <- initial_split(ames, prop = 0.7)
# train_3  <- training(split_1)
# test_3   <- testing(split_1)
# 
# # Using h2o package
# split_2 <- h2o.splitFrame(ames.h2o, ratios = 0.7, 
#                           seed = 123)
# train_4 <- split_2[[1]]
# test_4  <- split_2[[2]]


# # Sale price as function of neighborhood and year sold
# model_fn(Sale_Price ~ Neighborhood + Year_Sold,
#          data = ames)
# 
# # Variables + interactions
# model_fn(Sale_Price ~ Neighborhood + Year_Sold +
#            Neighborhood:Year_Sold, data = ames)
# 
# # Shorthand for all predictors
# model_fn(Sale_Price ~ ., data = ames)
# 
# # Inline functions / transformations
# model_fn(log10(Sale_Price) ~ ns(Longitude, df = 3) +
#            ns(Latitude, df = 3), data = ames)


# Use separate inputs for X and Y
# features <- c("Year_Sold", "Longitude", "Latitude")
# model_fn(x = ames[, features], y = ames$Sale_Price)
# 
# 
# model_fn(
#   x = c("Year_Sold", "Longitude", "Latitude"),
#   y = "Sale_Price",
#   data = ames.h2o
# )

# Example using h2o
# h2o.cv <- h2o.glm(
#   x = x, 
#   y = y, 
#   training_frame = ames.h2o,
#   nfolds = 10  # perform 10-fold CV
# )
# 
# 

# Stratified sampling with the rsample package
set.seed(123)
split <- initial_split(ames, prop = 0.7, 
                       strata = "Sale_Price")
ames_train  <- training(split)
ames_test   <- testing(split)

# Specify resampling strategy
cv <- trainControl(
  method = "repeatedcv", 
  number = 10, 
  repeats = 5
)

# Create grid of hyperparameter values
hyper_grid <- expand.grid(k = seq(2, 25, by = 1))

# Tune a knn model using grid search
knn_fit <- train(
  Sale_Price ~ ., 
  data = ames_train, 
  method = "knn", 
  trControl = cv, 
  tuneGrid = hyper_grid,
  metric = "RMSE"
)

# Print and plot the CV results
knn_fit

## RMSE was used to select the optimal model using the smallest value.
## The final value used for the model was k = 7.
ggplot(knn_fit)
