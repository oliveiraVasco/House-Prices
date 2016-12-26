source("pre_analysis.R")
source("pre_processing.R")
source("regression.R")
source("random_sample.R")

# File reading ------------------------
data <- read.csv("data/train.csv",
                 header = TRUE,
                 sep = ",")

data.info <- read.csv("feature_approach.csv", 
                      header = TRUE, 
                      sep = ",")

# Data Segmentation --------------------

train.percentage <- 0.7

smp.indexes <- GenerateSample(nrow(data), train.percentage)

train.data <- SegmentTrainingSample(data, smp.indexes)

cv.data <- SegmentCrossValidation(data, smp.indexes)

train.house.price <- train.data[ ,ncol(train.data)]

train.all.features <- train.data[ ,2:(ncol(train.data)-1)]

# Data Ploting --------------------------

#FeaturePloting(data[ ,2:(ncol(data)-1)], data[ ,ncol(data)])

rm (data)

# Feature Processing --------------------

data.regression <- RegressionMatrix(train.all.features,
                                    train.house.price, 
                                    data.info)

# Least Squares Estimation ---------------

regression.result <- RegressionFunction(data.regression)


