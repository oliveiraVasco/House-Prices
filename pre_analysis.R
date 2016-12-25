FeaturePloting <- function(all.features, house.price)
{
  # Plots all features with house price on Y axis and
  # and the feature on X axis
  # 
  # Args:
  #   all.features: Object with features on columns
  #   house.price: One dimension object with housing prices data
  #
  
  i <- 1
  while (i <= (ncol(all.features)))
  {
    plot(all.features[ ,i],
         house.price,
         type = "p",
         xlab = paste("value: ", i," ", colnames(all.features)[i]))
    i <- i+1
  }
  
}

FeatureAnalysis <- function(all.features)
{
  # Collects the number of unique elements and the existence of NA
  # on each feature. Saves the result on a csv file.
  #
  # Args:
  #   all.features: Object with features on the columns
  # 
  features <- data.frame()
  i <- 1
  while (i <= (ncol(all.features)))
  {
    features[i,1] <- length(unique(all.features[ ,i]))
    features[i,2] <- any(is.na(all.features[ ,i]))
    i <- i+1
  }
  
  colnames(features) <- c("NTypes", "NA?")
  rownames(features) <- colnames(all.features)
  write.csv(features, file = "features.csv")
}
