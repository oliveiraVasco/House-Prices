require (lmtest)
require (nlme)

RegressionFunction <- function(data.regression, gls)
{
  # Generation of formula and estimation of OLS or GLS according to gls parameter
  #
  # Args: 
  #   regression.data: Object with Y on the first column and features on the others
  #   gls: True to estimate gls instead of ols
  #
  # Returns:
  #   regression.result: Generalized/Ordinary least squares object
  
  feature.names <- colnames(data.regression)
  
  regression.formula <- paste(feature.names[2:length(feature.names)],
                              collapse = " + ")
  
  regression.formula <- paste(feature.names[1], " ~ ",
                              regression.formula, sep = "")
  
  regression.formula <- as.formula(regression.formula)
  
  if (gls == FALSE)
  {
    regression.result <- lm(regression.formula, 
                            data = data.regression)    
  }
  else
  {
    regression.result <- gls(regression.formula, 
                            data = data.regression)    
  }
  
  return (regression.result)
}

LinearDependence <- function(data.regression, indexes)
{
  # Detects NA elements on a regression and removes it from the inital dataset
  #
  # Args:
  #   data.regression: Two dimensional object with y on the first column and features on the others
  #   indexes: If true returns indexes of NA coefficients. If False returns the new dataset
  #
  # Return:
  #   data.regression: Dataset without linear dependent features (if indexes == FALSE)
  #   linear.depence: Indexes with NA coefficients (if indexes == TRUE)
  #
  
  regression.result <- RegressionFunction(data.regression, FALSE)
  
  linear.dependece <- which(is.na(regression.result$coefficients))
  
  if (indexes == TRUE)
    return (linear.dependece)
  
  if (length(linear.dependece) > 0)
    data.regression <- data.regression[ ,-linear.dependece]
  
  return (data.regression)
}

RegressionEstimationType <- function(data.regression)
{
  # Estimation of OLS. In the presence of heteroscedasticity returns the estimation of GLS
  #
  # Args:
  #   data.regression: Two dimensional object with y on the first column and features on the others
  # 
  # Returns:
  #   regression.result: Object with OLS or GLS estimation
  #
  
  regression.result <- RegressionFunction(data.regression, FALSE)
  
  if (bptest(regression.result)$p.value < 0.05) # Rejects homoskedastic hypothesis
  {
    regression.result <- RegressionFunction(data.regression, TRUE)
  }
  return (regression.result)
}

Prediction <- function(coeffs, cv.features)
{
  # Computes model predictions. Ignores linear dependent features and adds a columen to compute intercept.
  #
  # Args:
  #   coeffs: One dimensional object with all model coefficients
  #   cv.features: Two dimensional object with features on columns (without y)
  #
  # Returns:
  #   predictions: One dimensional object with predicted values
  #
  coeffs[is.na(coeffs)]<-0 # Ignore linear dependence
  cv.features <- cbind2(rep(1, nrow(cv.features)), cv.features)
  pred <- as.matrix(cv.features) %*% coeffs
  return (pred)
}

