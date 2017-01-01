DummyConstruction <- function(feature, name)
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
  
  if (nd == 0)
    return (NA)
  
  output.matrix <- matrix(data = 0 , 
                          nrow = length(feature), 
                          ncol = nd)
  na <- anyNA(feature)
  names <- c()
  if (na == FALSE)
  {
    for (i in 1:nd)
    {
      output.matrix[,i] <- as.integer( feature == elements[i] )
      names[i] <- paste(name, "_", 
                        gsub(' ', '_', as.character(elements[i])),
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
        names[output.index] <- paste(name,"_", 
                                     gsub(' ', '_', as.character(elements[i])), 
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
  
  correction1 <- c("LotFrontage", "MasVnrArea")
  mute <- c("GarageYrBlt")
  
  if ( name %in% mute)
  {
    return (NA)
  }
  else if ( name %in% correction1 || anyNA(feature) )
  {
    feature[is.na(feature)]<-0
    return (feature)
  }
  return (feature)
}

LevelSquares <- function(level.data, level.names)
{
  # Computes the cross prodcut between all level features
  #
  # Args:
  #   level.data: Two dimensional object with features on columns
  #   level.names: One dimensional obeject with all level features names
  #
  # Returns:
  #   level.data: Two dimensional obeject with level features and cros product ( and colnames)
  #
  
  nd <- ncol(level.data)
  for (i in 1:nd)
  {
    for (j in i:nd)
    {
      level.data <- cbind2(level.data, 
                           as.numeric(level.data[ ,i]) * as.numeric(level.data[ ,j]))
      level.names <- append(level.names, 
                            paste(level.names[i], "_X_", level.names[j], sep=""))
    }
  }
  colnames(level.data) <- level.names
  return (level.data)
}

LevelConstruction <- function(all.features, level.indexes, data.info)
{
  # Groups all the level features in one data.frame
  # By grouping features in the beginning of the data.frame it's easier to generate cross products
  #
  # Args:
  #   all.features: All features avaiable
  #   level.indexes: One dimensional object with indexes of all level features
  #   data.info: Information about the features (generate level or dummy)
  #
  # Return:
  #   level.data: All level data corrected 
  #
  
  nd <- length(level.indexes)
  level.data <- NA
  level.names <- c()
  for (i in 1:nd)
  {
    index <- level.indexes[i]
    addData <- NASpecialLevel(all.features[ ,index],
                              data.info[index,1])
    
    if (!anyNA(addData))
    {
      level.names <- append(level.names,
                            paste(data.info[index,1]))
      if (anyNA(level.data))
        level.data <- as.data.frame(addData)  
      else
        level.data <- cbind2(level.data,
                             as.data.frame(addData))
    } 
  }
  #colnames(level.data) <- level.names # Uncomment when LevelSquares is not invoked
  level.data <- LevelSquares(level.data, level.names)
  print(paste("Number of level:", ncol(level.data)))
  return (level.data)
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
  level.indexes <- c()
  for ( i in 1:nrow(data.info))
  {
    if (data.info[i,2] == "Dummy")
    {
      feature.data <- DummyConstruction(all.features[ ,i],
                                        data.info[i,1])
      if (!anyNA(feature.data))
      {
        if (i == 1)
          x.matrix <- feature.data
        else
          x.matrix <- cbind2(x.matrix, feature.data)        
      }
    }
    else if (data.info[i,2] == "Level")
      level.indexes <- append(level.indexes, i)
  }
  print(paste("Number of Dummy:", ncol(x.matrix)))
  level.data <- LevelConstruction(all.features,
                                  level.indexes, data.info)
  x.matrix <- cbind2(level.data, x.matrix)
  print(paste("Total variables:", ncol(x.matrix)))
  return (x.matrix)
}

AppendYToRegressionMatrix <- function(regression.data, y)
{
  # Appends Y values to the X matrix
  #
  # Args:
  #   regression.data: All regressible features ( on columns)
  #   y: One dimensional object with y values
  #
  # Returns:
  #   regression.data: Y on the first column and features on the others
  #
  
  y <- as.data.frame(y)
  colnames(y) <- "housePrice"
  regression.data <- cbind2(y, regression.data) 
  return (regression.data)
}
