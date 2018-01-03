#part 1 parameter estimation for dugen
#returns a vector containing (a,b)


convert_file_to_vector <- function(path) {
  
  values <- read.table(path, sep = ',')
  vec <- c()
  for(value in values) {
    vec <- c(vec, value)
  }
  
  return(vec)
  
}

#convert_file_to_vector("myfile.txt")


dugen_mle <- function(path) {
  
  samples = convert_file_to_vector(path)
  
  b = max(samples)
  a = min(samples)
  interval = c(a, b)
  return(interval)
}


#dugen_mle(c(1,4,6,2,5,8,4,3,2,67,4,7))

#part 2 parameter estimation for cugen
#returns a vector containing (a,b)

cugen_mle <- function(path) {
  
  samples = convert_file_to_vector(path)
  
  b = 1
  a = 0
  interval <- c(a, b)
  return(interval)
}

#part 3 parameter estimation for bernuli

brgen_mle <- function(path) {
  
  samples = convert_file_to_vector(path)
  print(samples)
  number_of_elements = length(samples)
  samples <- table(samples)
  
  number_of_one = samples[names(samples) == 1]
  
  return (number_of_one / number_of_elements)
  
}

#brgen_mle(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))

#part 4 parameter estimation for binomial
#returns parameteres as (n,p)

bigen_mle <- function(path) {
  
  samples = convert_file_to_vector(path)
  
  expected <- mean(samples)
  variance <- mean(samples^2) - (mean(samples))^2
  len <- length(samples)
  
  p = 1 - (variance/(expected*len))
  n = expected/p
  parameters <- c(n, p)
  return(parameters)
  
}

#part 5 parameter estimation for geometric

gegen_mle <- function(path) {
  
  samples = convert_file_to_vector(path)
  
  p = 1 / mean(samples)
  return(p)
  
}


#part 6 parameter estimation for exponential

expgen_mle <- function(path) {
  
  samples = convert_file_to_vector(path)
  
  lambda = 1 / mean(samples)
  
  return(lambda)
  
}

#expgen_mle("myfile.txt")


#part 7 parameter estimation for gamma
#returns parameters as (alpha,betta)

gagen_mle <- function(path) {
  
  samples = convert_file_to_vector(path)
  
  expected = mean(samples)
  variance = mean(samples^2) - expected^2
  
  betta = expected/variance
  alpha = expected^2 / variance
  parameters <- c(alpha, betta)
  
  return(parameters)
  
}

#part 8 parameter estimation for poisson

pogen_mle <- function(path) {
  
  samples = convert_file_to_vector(path)
  
  lambda = mean(samples)
  return(lambda)
  
}


#part 9 parameter estimation for normal
#returns a vector containing (expected, variance)

nogen_mle <- function(path) {
  
  samples = convert_file_to_vector(path)
  
  number_of_elements <- length(samples)
  expected = mean(samples)
  
  number = 0
  for(element in samples) {
    number = number + (element-expected)^2
  }
  
  variance = number / number_of_elements
  
  expected_variance <- c(expected, variance)
  
  return(expected_variance)
}

#nogen_mle(c(1,2,3,4,5,9))
