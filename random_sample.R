GenerateSample <- function(size, percentage)
{
  # Generates indexes of random the new random sample
  #
  # Args:
  #   size: Dimension of overall sample
  #   percentage: Percentage of sample for training
  #
  # Return:
  #   random.indexes: Array with random sample indexes
  #
  
  number.elements <- round(percentage * size)
  random.indexes <- sample(size, number.elements)
  return (random.indexes)
}

SegmentTrainingSample <- function(data, indexes)
{
  # Segments the data for training 
  #
  # Args:
  #   data: Two dimensional object with features on columns
  #   indexes: One dimensional object with indexes for training
  #
  # Return:
  #   train.data: Training data segmented
  #
  
  train.data = data[indexes, ]
  return (train.data)
}

SegmentCrossValidation <- function(data, indexes)
{
  # Segments data for cross validation
  # 
  # Args:
  #   data: Two dimensional object with features on columns
  #   indexes: One dimensional object with indexes for training
  #
  # Return:
  #   cv.data: Cross-validation data segmented
  #
  
  all.indexes <- 1:nrow(data)
  occurrences <- all.indexes %in% indexes
  cv.data <- data[!occurrences, ]
  return (cv.data)
}
