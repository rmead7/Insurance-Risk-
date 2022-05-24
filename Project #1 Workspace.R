## Project #1 ##
#  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
# Question #1:#
# Analyzing the size of the claims X.
#a.) Analytically calculate the PDF (CDF) of X, E[X], Var(X), skewness, and excess kurtosis. 
# It might be easier if you fix a = 5 but leave b arbitrary when do the calculations then 
# substitute for b.

#declaring variables
pf <- function(x){((5)*(200000^5))/((x+200000)^(5+1))}

#packages
library('cubature')

#Expected Value
moment_1 <- function(x) {((5)*(200000^5)*(x))/((x+200000)^(6))}
expected_value <- adaptIntegrate(moment_1,lowerLimit = 0,upperLimit = Inf)
expected_value$integral

#Variance
moment_2<- function(x) {(((5)*(200000^5)*(x^2))/(x+200000)^(5+1))-(((5)*(200000^5)*(x))/(x+200000)^(5+1))^2}
variance <- adaptIntegrate(moment_2,lowerLimit = 0,upperLimit = Inf)
theovar <- variance$integral - expected_value$integral^2
theovar

#Skewness
moment_3 <- function(x) {((5)*(200000^5)*(x^3))/((x + 200000)^(5+1))}
skewness <- adaptIntegrate(moment_3,lowerLimit = 0, upperLimit = Inf)
skewness$integral

#Excess Kurtosis
moment_4 <- function(x) {((5)*(200000^5)*(x^4))/((x + 200000)^6)}
kurtosis <- adaptIntegrate(moment_4,lowerLimit = 0,upperLimit = Inf)
kurtosis$integral

excess_kurtosis <- kurtosis$integral - 3
excess_kurtosis

#  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#

#b.) Run a simulation using 10,000 samples of X using (1) and generate a histogram and
# compare with a graph of the density in (1). If needed you can increase the number of
# samples.

#packages
library('fBasics')
library('Pareto')
library('actuar')

#simulation
a <- 5
b <- 200000
n <- 1000
samples <- 10000
x <- seq(1:samples)
p <- rpareto(1:samples,a,b)

#Graphics
plot(p,main = "Pareto Distribution \n 10,000 Samples", xlab = "Number of Simulations",
     ylab = "Density",type = "line")
hist(p,breaks = 300,main = "Pareto Distribution Histogram \n 10,000 Samples", xlab = "Claims",
     ylab = "Frequency")
curve(dPareto(x,a,b),type="l",add=TRUE,col='red')

#  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#

#c.) Compute the sample mean, variance, skewness, and kurtosis of your simulation and 
# compare with your results in (a). Are they consistent?

#packages
install.packages('moments')
library('moments')

#Moments 
mo_1 <- moment(p,order = 1)
mo_1

#Second Moment
mo_2 <- moment(p, order = 2)
mo_2

#Variance
expvar <- mo_2 - (mo_1)^2
expvar

#Third Moment
mo_3 <- moment(p, order = 3)
mo_3

#Verifying Moments with Calculations
mean(p)
var(p)
skewness(p)
k <- kurtosis(p)
ek <- k-3 
ek

#  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#

#Simulation the company’s assets Z(t) for five years.
#a.) Plot a five different sample paths with proper labels.

#declaring variables
initial <- 1000000
premium <- n*5000
t <- seq(1:5)
n.steps <- 5
n.indiv <- 5

z <- matrix(NA, nrow = n.indiv, ncol = n.steps)
z[,1] <- initial


for(i in 2:n.steps){
  z[,i] <- z[,i-1]+(premium*1000)-rpareto(t,a,b)*rbinom(5,1000,0.1)
  
  print(i)
}

plot(NA, ylim=c(min(z),max(z)),xlim = c(1,n.steps),cex.main=1.5,xlab = "Time", ylab = "Assets", 
     main = "Five Sample Paths \n of Company Assets")
for (i in 1:nrow(z)) {
  lines(z[i,], col = rainbow(n.indiv)[i])
  
}

#b.) Using 10,000 sample paths, estimate the probability that the company goes bankrupt. 
#If needed you can increase the number of sample paths.
#declaring variables
initial <- 1000000
premium <- 5000
t <- seq(1:5)
n.steps <- 5
n.indiv <- 30000


z_1 <- matrix(NA, nrow = n.indiv, ncol = n.steps)
z_1[,1] <- initial


for(i in 2:n.steps){
  z_1[,i] <- z_1[,i-1]+(premium*1000)-rpareto(t,a,b)*rbinom(5,1000,0.1)
  
  print(i)
}

plot(NA, ylim=c(min(z_1),max(z_1)),xlim = c(1,n.steps),cex.main=1.5,xlab = "Time", ylab = "Assets", 
     main = "10,000 Sample Paths \n of Company Assets")
for (i in 1:nrow(z_1)) {
  lines(z_1[i,], col = "black")
  
}

#declaring variables
n.steps <- 5
w <- rpareto(1000,a,b)
prob <- rbinom(1000,1,0.1)
total_claims <- sum(w*prob)
n.iter_2 <- 10000
assets_2 <- rep(0,(n.steps+1))
assets_2[1] <- start
sim_2 <- matrix(0,(n.steps+1),n.iter_2)

#simulation
for (i in 1:n.iter_2) {
  for (j in 2:(n.steps+1)) {
    if (assets_2[j-1] <= 0){
      assets_2[j] = 0
    }
    else
    {
      claims <- rpareto(1000,a,b)*rbinom(1000,1,0.1)
      total_claims <- sum(claims)
      assets_2[j] <- max(assets_2[j-1]+prem-total_claims,0)
    }
  }
  sim_2[,i] <- assets_2
}

#graphics
plot(NA, ylim=c(min(sim_2),max(sim_2)),xlim = c(1,n.steps+1),cex.main=1.5,xlab = "Time", ylab = "Total Assets", main = "10,000 Sample Paths")
for (i in 1:ncol(sim_2)) {
  lines(sim_2[,i], col = rainbow(n.iter_2)[i])
}
hist(sim_2, xlab = "Total Assets", main = "10,000 Simulations \n of a Companies Total Assets")
hist_info <- hist(sim_2,breaks = 6000000)
hist_info
total <- sum(hist_info$counts)
total

14257
