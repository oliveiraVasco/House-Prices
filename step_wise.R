source("random_sample.R")
source("regression_tools.R")


Rmsle <- function(y , y.estimated)
{
  # Computes the root mean square logarithmic error
  #
  # Args:
  #   y: One dimensional object with observed values of y
  #   y.estimated: One dimensional object with estimated values of y
  # 
  # Returns:
  #   rmsle: root mean square logarithmic error
  #
  
  rmsle <- sqrt( (1/length(y)) * sum((log(y + 1) - log(y.estimated + 1))^2) )

  return (rmsle)
}

FeatureAnalysis <- function(train.data, cross.validation.data)
{
  # Removes each feature and estimates the models. Computes the error outside the sample
  #
  # Args: 
  #   train.data: Data for training the model
  #   cross.validation.data: Data for cross validation of the model
  #
  # Returns:
  #   feature.rmsle: One dimensional object with the rmsle for the feature removed
  #
  
  feature.rmsle <- c()
  for (i in 2:ncol(train.data))
  {
    print(paste("    Cross Validation", i))
    # Removing i feature
    temp.train.data <- train.data[ ,-i]
    temp.cv <- cross.validation.data[ ,-i]
    
    # Removing linear dependent features
    remove.indexes <- LinearDependence(temp.train.data, TRUE)
    if (length(remove.indexes) > 0)
    {
      temp.train.data <- temp.train.data[ ,-remove.indexes]
      temp.cv <- temp.cv[ ,-remove.indexes]
    }

    # Regression Predictions and RMSLE
    regression <- RegressionFunction (temp.train.data, FALSE)
    predictions <- predict(regression, temp.cv[ ,-1])
    indicator <- Rmsle(temp.cv[ ,1], predictions)
    feature.rmsle <- append(feature.rmsle, indicator)
  }
  return (feature.rmsle)
}

CrossValidationStepWise <- function(data.regression, n.sample.generations, train.percentage)
{
  # Step wise backwards. Tests which feature is the best to remove based on FeatureAnalysis.
  #
  # Args:
  #   data.regression: Two dimensional object with y on the first column and features on the other
  #   n.sample.generations: Number os random generated samples on each step choice
  #   train.percentage: Percentage of data used to train.
  #
  # Returns:
  #   model.information: Returns the formula of the best model
  #
  
  number.features <- ncol(data.regression) - 1
  vr <- number.features
  model.information <- data.frame()
  while (vr > 5) # Not less then 5 feature
  {
    print(paste("VR on position", vr))
    temp.error <- c(0)
    for (i in 1:n.sample.generations)
    {
      print(paste("  Generation", i))
      smp.indexes <- GenerateSample(nrow(data.regression), train.percentage)
      train.data <- SegmentTrainingSample(data.regression, smp.indexes)
      cross.validation.data <- SegmentCrossValidation(data.regression, smp.indexes)
      
      feature.rmsle <- FeatureAnalysis(train.data, cross.validation.data)
      
      temp.error <- temp.error + feature.rmsle
    }
    temp.error <- temp.error / n.sample.generations
    
    remove.feature <- which.min(temp.error) + 1
    
    data.regression <- data.regression[ ,-remove.feature]
    
    temp.names <- colnames(data.regression)
    model.information[(number.features - vr + 1), 1] <- paste(temp.names[2:length(temp.names)], collapse = " + ")
    model.information[(number.features - vr + 1), 2] <- min(temp.error)
    
    vr <- vr - 1
  }
  best.model <- which.min(model.information[ ,2])
  best.formula <- paste(colnames(data.regression), " ~ ", model.information[best.model,1], sep = "")
  best.formula <- as.formula(best.formula)
  return (best.formula)
}


