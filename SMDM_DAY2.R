##Binomial Distribution

##probability 
x = 0:7
prob = dbinom(x,size = 7,prob = 0.6)
data.frame(x,prob)

##cumilative proba

cumProb = pbinom(x,size = 7,prob = 0.6)
data.frame(x,prob,cumProb)

##poisson distri
dpois(x = 4, lambda = 3)

1-ppois(3,3)

###normal distribution
mean = 0.295
sigma = 0.025

pnorm(0.280,mean,sigma)

1-pnorm(0.350,mean,sigma)
pnorm(0.350,mean,sigma,lower.tail = FALSE)

pnorm(0.350,mean,sigma) - pnorm(0.260,mean,sigma)

qnorm(0.99,mean = 10.01, sd = 0.06)
