source("random_sample.R")
source("regression_tools.R")
require(doMC)

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

FeatureAnalysisBackward <- function(train.data, cross.validation.data)
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
  
  number.features <- ncol(train.data) - 1 
  feature.rmsle <- array(data = 0, dim = number.features)
  #for (i in 2:(number.features+1))
  feature.rmsle <- foreach (i = 2:(number.features+1), .combine = c) %dopar%
  {
    print(paste("    Cross Validation", i))
    # Removing i feature
    temp.train.data <- train.data[ ,-i]
    temp.cv <- cross.validation.data[ ,-i]

    # Regression Predictions and RMSLE
    regression <- RegressionFunction (temp.train.data, FALSE)
    predictions <- Prediction(regression$coefficients, temp.cv[ ,-1])
    indicator <- Rmsle(exp(temp.cv[ ,1]), exp(predictions))
    feature.rmsle[i-1] <- indicator
  }
  return (feature.rmsle)
}

CrossValidationStepWiseBackward <- function(data.regression, n.sample.generations, train.percentage, number.cores)
{
  # Step wise backwards. Tests which feature is the best to remove based on FeatureAnalysisBackward.
  #
  # Args:
  #   data.regression: Two dimensional object with y on the first column and features on the other
  #   n.sample.generations: Number os random generated samples on each step choice
  #   train.percentage: Percentage of data used to train.
  #
  # Returns:
  #   best.formula: Returns the formula of the best model
  #
  
  registerDoMC(cores = number.cores)
  
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
      
      feature.rmsle <- FeatureAnalysisBackward(train.data, cross.validation.data)
      
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


FeatureAnalysisForward <- function(train.data, cross.validation.data, model.feature)
{
  # Tests each feature and estimates the models. Computes the error outside the sample
  #
  # Args: 
  #   train.data: Data for training the model
  #   cross.validation.data: Data for cross validation of the model
  #   model.feature: Best features collected on previous instances
  #
  # Returns:
  #   feature.rmsle: One dimensional object with the rmsle for the feature removed
  #
  
  number.features <- ncol(train.data) - 1 
  feature.rmsle <- array(data = NA, dim = number.features)
  #for (i in 2:(number.features+1))
  feature.rmsle <- foreach (i = 2:(number.features+1), .combine = c) %dopar%
  {
    if (!(i %in% model.feature))
    {
      print(paste("    Cross Validation", i))
      # Removing i feature
      temp.train.data <- train.data[ ,append(1, append(model.feature,i))]
      temp.cv <- cross.validation.data[ ,append(1, append(model.feature,i))]
      
      # Regression Predictions and RMSLE
      regression <- RegressionFunction (temp.train.data, FALSE)
      predictions <- Prediction(regression$coefficients, temp.cv[ ,-1])
      indicator <- Rmsle(exp(temp.cv[ ,1]), exp(predictions))
      feature.rmsle[i-1] <- indicator
    }
  }
  return (feature.rmsle)
}

CrossValidationStepWiseForward <- function(data.regression, n.sample.generations, train.percentage, number.cores, limit.features)
{
  # Step wise forward. Tests which feature is the best to add.
  #
  # Args:
  #   data.regression: Two dimensional object with y on the first column and features on the other
  #   n.sample.generations: Number os random generated samples on each step choice
  #   train.percentage: Percentage of data used to train.
  #
  # Returns:
  #   model.information: Returns the information about models tested
  #
  
  registerDoMC(cores = number.cores)
  
  number.features <- ncol(data.regression) - 1
  model.information <- data.frame()
  model.feature <- c()
  vr <- 1
  while (vr <= limit.features)
  {
    print(paste("VR on position", vr))
    temp.error <- c(0)
    for (i in 1:n.sample.generations)
    {
      print(paste("  Generation", i))
      smp.indexes <- GenerateSample(nrow(data.regression), train.percentage)
      train.data <- SegmentTrainingSample(data.regression, smp.indexes)
      cross.validation.data <- SegmentCrossValidation(data.regression, smp.indexes)
      
      feature.rmsle <- FeatureAnalysisForward(train.data, cross.validation.data, model.feature)
      
      temp.error <- temp.error + feature.rmsle
    }
    temp.error <- temp.error / n.sample.generations
    
    add.feature <- which.min(temp.error) + 1
    
    model.feature <- append(model.feature, add.feature)
    
    temp.names <- colnames(data.regression)[model.feature]
    model.information[vr, 1] <- paste(temp.names[2:length(temp.names)], collapse = " + ")
    model.information[vr, 2] <- min(temp.error, na.rm = TRUE)
    
    vr <- vr + 1
  }
  return (model.information)
}


