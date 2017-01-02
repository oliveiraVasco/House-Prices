require(fmsb)
StepWiseMulticolinearity <- function(data.regression)
{
  # This function returns the variance inflation factor for each feature
  #
  # Args:
  #   data.regression: Object with data ready for regression ( 1st column with y)
  #
  # Returns:
  #   vif.values: One dimensional object with VIF for all features
  #
  
  feature.names <- colnames(data.regression)
  vif.values <- matrix(data = 0, nrow = (length(feature.names)-1), ncol = 1)
  
  for(i in 2:length(feature.names))
  {
    adapted.formula <- paste(feature.names[-i], collapse=" + ")
    adapted.formula <- paste(feature.names[i], " ~ ", adapted.formula)
    adapted.formula <- as.formula(adapted.formula)
    vif.values[(i-1), 1] <-  VIF(lm(adapted.formula, data=data.regression))
  }
  vif.values <- as.data.frame(vif.values)
  rownames(vif.values) <- feature.names[2:length(feature.names)]
  return (vif.values)
}