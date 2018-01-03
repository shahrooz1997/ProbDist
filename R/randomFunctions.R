#Adding packages
if("digest" %in% rownames(installed.packages()) == FALSE)
{install.packages("digest")}

library(digest)

#Functions gets an integer from (-2^31) to (2^31-1)
#and produces another random integer from (-2^31) to (2^31-1)
singleRandom <- function(number){
  tempStr <- c(sha1(number))
  #print(substr(tempStr,1,1))
  Tchar <- substr(tempStr,1,1)
  if(Tchar=="8")
    Tchar <- "-0"
  else if(Tchar=="9")
    Tchar <- "-1"
  else if(Tchar=="a" || Tchar=="A")
    Tchar <- "-2"
  else if(Tchar=="b" || Tchar=="B")
    Tchar <- "-3"
  else if(Tchar=="c" || Tchar=="C")
    Tchar <- "-4"
  else if(Tchar=="d" || Tchar=="D")
    Tchar <- "-5"
  else if(Tchar=="e" || Tchar=="E")
    Tchar <- "-6"
  else if(Tchar=="f" || Tchar=="F")
    Tchar <- "-7"
  
  HexStr <- sprintf("%s%s",Tchar,substr(tempStr,2,8)) #cat(Tchar,substr(tempStr,2,8),sep="")
  #print(HexStr)
  #print(substr(tempStr,2,8))
  return(strtoi(HexStr,16))
}

##STEP 1
#Using SHA-1 to generate random variables
rgenerator <-function(seed , length = 100){
  final_out <- singleRandom(c(seed))
  seed <- final_out
  if(length < 2)
    return(final_out)
  for(i in 2:length){
    result <- singleRandom(seed)
    seed <- result
    final_out <- c(final_out,result)
  }
  return(final_out)
}

##STEP 2
#A function that produces random variables between two two numbers (including
#those numbers too
dugen <- function(start,end,length=100,seed=1){
  Rvec = rgenerator(seed,length)
  rangeLen = abs(end - start) + 1
  Rvec <- (Rvec %% rangeLen) + start
  return(Rvec)
}

##STEP 3
#A function that give one number between 0 and 1 with the corresponding seed
# cugen <- function(seed){
#   Rnum <- abs(singleRandom(seed))/(2^31)
#   return(Rnum)
# }

#the same function without the need of input "seed"
cugen <- function(seed = NULL){
  if(is.null(seed)){
    seed <- as.numeric(Sys.time())*1000
  }
  Rnum <- abs(singleRandom(seed)/(2^31))
  return(Rnum)
}


##STEP 4
# A function for generating bernoulli distribution
brgen <- function(p){
  if(cugen() <= p){
    return(1)
  }
  else{
    return(0)
  }
}
# test bern :::::: test passed
test.bern <- function(){
  one <- 0
  zero <- 0
  for (i in 1:10000){
    if(brgen(0.7) == 1){
      one <- one + 1
    }
    else{
      zero <- zero + 1
    }
  }
  print(one / (zero + one))
}


##step 5
# A function for generating binomial distribution
bigen <- function(n, p)
{
  x <- 0
  
  for(i in c(1:n))
  {
    if(cugen() <= p)
    {
      x <- x + 1
    }
  }
  
  return(x);
}


##STEP 6
# A function for generating Geometric distribution
gegen <- function(p){
  sum <- 0
  temp <- p
  i <- 0
  myrandnum = cugen()
  while(TRUE){
    if(sum < myrandnum && myrandnum < temp){
      return(i+1)
    }
    i <- i + 1
    sum <- temp
    temp <- temp + p * (1 - p) ^ (i)
  }
}

##STEP 7
# A function for generating Exponential Distribution
expgen <- function(lambda){
  return((-1/lambda) * log(cugen()))
}


##step 8
# A function for generating Gamma distribution
gagen <- function(lambda, k)
{
  x = 0
  
  for(i in c(1, k))
    x = x + expgen(lambda)
  
  return(x)
}


##step 9
# A function for generating poisson distribution
pogen <- function(lambda, t = 1)
{
  count <- 0
  spent.time <- 0
  
  while(spent.time < t)
  {
    spent.time <- spent.time + expgen(lambda)
    count <- count + 1
  }
  
  return(count-1)
}

##STEP 10
# A function for generating Normal Distribution
# Please set the number of poissons first otherwise we use the default: 100
nogen <- function(u, s){
  if(s <= 0){
    print("error: s is zero or negetive")
    return()
  }
  num.of.pois <- 100
  #lambda.of.pois <- 10
  i <- 0
  ret <- 0
  while(i < num.of.pois){
    ret <- ret + pogen(s)
    i <- i + 1
  }
  ret <- ret / num.of.pois
  # N(s, s / num.of.pois)
  ret <- ret - s
  # N(0, s / num.of.pois)
  ret <- ret * sqrt(num.of.pois)
  # N(0, s)
  ret <- ret + u
  # N(u, s)
  return(ret)
  #ret <- ret / sqrt(num.of.pois)
  #ret <- ret - ((sqrt(num.of.pois)) * s)
  #return(ret + u)
  #return((ret / sqrt(num.of.pois)) - (sqrt(num.of.pois) * s)  + u)
  #return((sqrt(s) * ret / sqrt(num.of.pois * lambda.of.pois)) - (sqrt(num.of.pois * lambda.of.pois * s))  + u)
}

# for testing nogen function:::::

# sum <- 0
# for(i in 1:100){
#   sum <- sum + nogen(100, 5)
#   
#   print(sum)
# }


