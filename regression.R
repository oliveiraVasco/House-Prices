RegressionFunction <- function(data.regression)
{
  # Generation of formula and estimation of least square 
  #
  # Args: 
  #   regression.data: Object with Y on the first column and features on the others
  #
  # Returns:
  #   regression.result: Least squares object
  
  feature.names <- colnames(data.regression)
  
  regression.formula <- paste(feature.names[2:length(feature.names)],
                              collapse = " + ")
  
  regression.formula <- paste(feature.names[1], " ~ ",
                              regression.formula, sep = "")
  
  regression.formula <- as.formula(regression.formula)
  
  regression.result <- lm(regression.formula, 
                          data = data.regression)
  
  return (regression.result)
}