RegressionFunction <- function(regression.data)
{
  # Generation of formula and estimation of least square 
  #
  # Args: 
  #   regression.data: Object with Y on the first column and features on the others
  #
  # Returns:
  #   regression.result: Least squares object
  
  feature.names <- colnames(regression.data)
  regression.formula <- paste(feature.names[1], 
                             " ~ ", 
                             feature.names[2]) 
  for (i in 3:length(feature.names))
  {
    regression.formula <- paste(regression.formula, 
                               " + ", 
                               feature.names[i])
  }
  regression.formula <- as.formula(regression.formula)
  regression.result <- lm(regression.formula, 
                         data = regression.data)
  return (regression.result)
}