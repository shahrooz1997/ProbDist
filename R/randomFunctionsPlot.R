#STEP 11
#First we need to define the Rand_Gen functions!
#source('randomFunctions.R')

library(ggplot2)


#function creating plot for dugen
dugen_plot <- function(start, end){
  value <- c()
  for(i in 1:10000){
    value <- c(value, dugen(start, end, 1, i))
  }
  ggplot() + aes(value) + geom_histogram(colour='black', fill='blue')
}

#dugen_plot(1, 10)


#function creating plot for cugen
cugen_plot <- function(){
  value <- c()
  for(i in 1:10000){
    value <- c(value, cugen())
  }

  ggplot() + aes(value) + geom_histogram( fill='red')
  #value

}

#cugen_plot()


#function creating plot for brgen
brgen_plot <- function(p){
  value <- c()
  for(i in 1:10000){
    value <- c(value, brgen(p))
  }

  ggplot() + aes(value) + geom_histogram(binwidth=1, colour='black', fill='green')
}

#brgen_plot(0.6)


#function creating plot for bigen
bigen_plot <- function(n, p){
  value <- c()
  for(i in 1:10000){
    value <- c(value, bigen(n, p))
  }

  ggplot() + aes(value) + geom_histogram(colour='black', fill='orange')
}

#bigen_plot(10, 0.7)


#function creating plot for gegen
gegen_plot <- function(p){
  value <- c()
  for(i in 1:10000){
    value <- c(value, gegen(p))
  }

  ggplot() + aes(value) + geom_histogram(binwidth=1, colour='black', fill='pink')
}

#gegen_plot(0.5)


#function creating plot for expgen
expgen_plot <- function(lambda){
  value <- c()
  for(i in 1:20000){
    value <- c(value, expgen(lambda))
  }

  ggplot() + aes(value) + geom_histogram(colour='black', fill='brown')
}

#expgen_plot(4)


#function creating plot for gagen
gagen_plot <- function(lambda, k){
  value <- c()
  for(i in 1:20000){
    value <- c(value, gagen(lambda, k))
  }

  ggplot() + aes(value) + geom_histogram(colour='black', fill='purple')
}

#gagen_plot(1, 7.5)
#for test

#function creating plot for pogen
pogen_plot <- function(lambda, t=1){
  value <- c()
  for(i in 1:10000){
    value <- c(value, pogen(lambda, t=1))
  }

  ggplot() + aes(value) + geom_histogram(colour='black', fill='cyan')
}

#pogen_plot(1)


#function creating plot for nogen
nogen_plot <- function(u, s){
  value <- c()
  for(i in 1:10000){
    value <- c(value, nogen(u, s))
  }

  ggplot() + aes(value) + geom_histogram(colour='black', fill='coral')
}

#nogen_plot(2, 0.5)
