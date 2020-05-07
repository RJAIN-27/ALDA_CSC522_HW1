###########
# HW1
# Mention your team details here
# Srujana Rachakonda (srachak)
# Rajshree Jain (rjain27)
#
############


# You may use the following librarie(s):
require(plyr)
# If you get an error when running these lines, 
# make sure to install the respective libraries

# read data matrix
read_data <- function(path = "./iris.csv") {
  # Note 1: DO NOT change the function arguments.
  # Input: path: type: string, output: a matrix containing data from iris.csv
  # Write code here to read the csv file as a data frame and return it.
  return(read.csv(path))
}

# Part 1: Distance Measurement
calculate_euclidean <- function(p, q) {
  # Input: p, q are numeric vectors of the same length
  # output: a single value of type double, containing the euclidean distance between p and q.
  combined_matrix <- rbind(p,q)
  euclidean_distance <- dist(combined_matrix, method="euclidean")
  return(euclidean_distance)
  
}

calculate_cosine <- function(p, q) {
  # Input: p, q are numeric vectors of the same length
  # output: a single value of type double, containing the cosine distance between p and q.
  dot_product <- sqrt(sum(p^2)) * sqrt(sum(q^2))
  cosine_distance <- sum(p*q) / dot_product
  return(cosine_distance)
}

calculate_l_inf <- function(p, q) {
  # Input: p, q are numeric vectors of the same length
  # output: a single value of type double, containing the l_inf distance between p and q.
  return(max(abs(p-q)))
  
}

# Part 2: principal Component Analysis
principal_component_analysis <- function(data, n){
  # Input: data: the Iris dataframe, with 4 numeric attributes and a 5th nominal class variable
  #        n: the number of the principle component to calculate (e.g. 1 for first principal component)
  # output: a 1 x 4 vector of type double, containing the weights (eigenvector) of the 
  # nth principal component of the dataset.
  data <- subset(data, select = -c(5))
  result <- prcomp(data, scale = FALSE)
  output_vector <- as.numeric(result$rotation[,n])
  return(output_vector)
  
}

principal_component_calculation <- function(p, component_weights){
  # Input: p is a numeric vector of of length n, e.g. representing a row from the original dataset.
  #        component_weights is a vector length n, containing the weights of a principal component
  #        (e.g. the output from running principal_component_analysis)
  # Output: a single value of type double, containing the first principal component value of the sample.
  return(sum(p * component_weights))
}

pc1_distance <- function(p, q, component_weights) {
  # Input: p, q are numeric vectors of of length n, e.g. representing rows from the original dataset.
  #        component_weights is a vector length n, containing the weights of a principal component
  #        (e.g. the output from running principal_component_analysis)
  # output: a single value of type double, containing the distance between p and q, projected onto 
  # the first principal component (i.e. |PC1_p - PC1_q|)
  return(abs(principal_component_calculation(p,component_weights) - principal_component_calculation(q,component_weights)))
}
