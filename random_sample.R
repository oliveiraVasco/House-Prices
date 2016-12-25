GenerateRandomSample <- function(data, dimension)
{
  # Generates a random sample from a set of data
  #
  # Args:
  #   data: Data to be bootstraped
  #   dimension: Number of rows of the output
  #
  # Return:
  #   smp: Random sample
  nr <- nrow(data)
  smp <- data[sample(nr, dimension), ]
  return (smp)
}