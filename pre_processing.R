DummyConstructor <- function(feature, na, name)
{
  # Takes a feature and decompose each unique element into a dummy
  # 
  # Args:
  #   feature: One dimensional object to convert into dummy
  #   na: Boolean for the existence of NA elements
  #   name: Name of the feature
  #
  # Return:
  #   output.matrix: Object with each unique element on each column
  #
  elements <- unique(feature)
  nd <- length(elements) - 1
  
  output.matrix <- matrix(data = 0 , 
                         nrow = length(feature), 
                         ncol = nd)
  names <- c()
  if (na == FALSE)
  {
    for (i in 1:nd)
    {
      output.matrix[,i] <- as.integer( feature == elements[i] )
      names[i] <- paste(name, 
                       "_", 
                       gsub(' ', 
                            '_',
                            as.character(elements[i])),
                       sep = "")
    }      
  }
  else
  {
    output.index <- 1
    feature <- as.matrix(feature)
    feature[is.na(feature)] <- FALSE
    feature <- as.array(feature)
    nd <- nd + 1
    for (i in 1:nd)
    {
      if (!is.na(elements[i]))
      {
        output.matrix[ ,output.index] <- as.integer( feature == elements[i] )
        names[output.index] <- paste(name, 
                                    "_", 
                                    gsub(' ', 
                                         '_', 
                                         as.character(elements[i])), 
                                    sep = "") 
        output.index <- output.index + 1
      }
    }
  }
  
  output.matrix <- as.data.frame(output.matrix)
  colnames(output.matrix) <- names
  return (output.matrix)
}

NASpecialLevel <- function(feature, name)
{
  # Corrects or mutes level feature with NA elements
  #
  # Args:
  #   feature: One dimensional object with feature data
  #   name: Name of the feature
  # 
  # Return:
  #   result: Corrected data
  #   NA: No correction available
  
  if (name == "LotFrontage" || name == "MasVnrArea")
  {
    result <- feature[is.na(feature)]<-0
    return (result)
  }
  else if ( name == "GarageYrBlt")
  {
    return (NA)
  }
  return (NA)
}

InputConstruction <- function(all.features, data.info)
{
  # Generates the estimation data according with the information provided
  # 
  # Args:
  #   all.features: All features avaiable
  #   data.info: Information about the features (generate level or dummy)
  #
  # Returns:
  #   x.matrix: X matrix ready for estimation
  #
  x.matrix <- data.frame()
  for ( i in 1:nrow(data.info))
  {
    if (data.info[i,4] == "Dummy")
      feature.data <- DummyConstructor(all.features[ ,i], data.info[i,3], data.info[i,1])
    else
    {
      if (data.info[i,3] == FALSE)
        feature.data <- as.data.frame(all.features[ ,i])
      else
      {
        feature.data <- as.data.frame( NASpecialLevel(all.features[ ,i], 
                                                     data.info[i,1]) )
      }
      colnames(feature.data) <- gsub(' ', 
                                     '_', 
                                     colnames(all.features)[i] )
    }
    
    if (!anyNA(feature.data))
    {
      if (i == 1)
        x.matrix <- feature.data
      else
        x.matrix <- cbind2(x.matrix, feature.data)
    }
  }
  return (x.matrix)
}


RegressionMatrix <- function(all.features, house.price, data.info)
{
  # Generates the data ready for least square function
  #
  # Args:
  #   all.feature: Object with features on its columns
  #   house.price: One dimension object with house price
  #   data.info: Information collected from feature_approach.csv file
  #
  # Return:
  #   regression.data: Object ready for regression. Y on the first column and level and dummy features on the others.
  #
  x.matrix <- InputConstruction(all.features, data.info)
  house.price <- as.data.frame(house.price)
  colnames(house.price) <- "house.price"
  regression.data <- cbind2(house.price, x.matrix)
  return (regression.data)
}
