Project 2
================
Nermin Bibic
7/11/2021

-   [Read Data and Split Into Training & Test
    Sets](#read-data-and-split-into-training--test-sets)
-   [Predictive Modeling](#predictive-modeling)
    -   [Linear Regression With Main Effects & 1st Order
        Interactions](#linear-regression-with-main-effects--1st-order-interactions)
    -   [Fit a random forest model on training
        dataset](#fit-a-random-forest-model-on-training-dataset)

## Read Data and Split Into Training & Test Sets

``` r
# Read and clean data
dayData <- read_csv("day.csv")
dayData <- filter(dayData, weekday == 1)
dayData <- select(dayData, -c(instant, weekday))
dayData$dteday = as.Date(dayData$dteday, format = "%Y-%m-%d")

# Split data to train and test sets
set.seed(1)
dayIndex <- createDataPartition(dayData$cnt, p = 0.7, list = FALSE)
dayTrain <- dayData[dayIndex, ]
dayTest <- dayData[-dayIndex, ]

# Convert categorical variables to factors
cols <- c("season", "yr", "mnth", "holiday", "workingday", "weathersit")
dayTrain[cols] <- lapply(dayTrain[cols], factor)
dayTest[cols] <- lapply(dayTest[cols], factor)
```

## Predictive Modeling

Prior to the models fit using linear regression, the first group member
should provide a short but thorough explanation of the idea of a linear
regression model.

### Linear Regression With Main Effects & 1st Order Interactions

Prior to each ensemble model, you should provide a short but reasonably
thorough explanation of the ensemble model you are using (so one for
each group member).

### Fit a random forest model on training dataset

Random forest modeling involves multiple decision trees that form an
ensemble. Random forest models are the same idea as bagging, but it
makes bagged trees predictions more correlated with a smaller reduction
in variance from aggregation. Since the random forest model selects a
subset of predictors randomly, good predictors won’t dominate the tree
fits. If there is a very strong predictor, every bootstrap tree will
likely use it for the first split. Here we are fitting a random forest
model for the train set.

``` r
# Set up fold number (10) for the cross validation and the repeated times (5) of the whole CV 
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
# Set seed for reproducible
set.seed(123)
# Fit the random forest model with cnt as response and all other variables as
# predictors on training set
rfFit <- train(cnt ~ ., data = dayTrain,
               method = "rf",
               preProcess = c("center", "scale"),
               trControl = trctrl)
```

    ## Error in eval(expr, p): object 'dayTrain' not found

``` r
rfFit
```

    ## Error in eval(expr, envir, enclos): object 'rfFit' not found
