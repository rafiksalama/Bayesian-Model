data = rbinom(1000,size = 1,prob = 0.2)
theta = runif(100,0,1)
alternativetheta = seq(0.1,0.4,length=10)

#Bayes Factor = Ratio of Probability of Data given the parameter * probabiliy of the parmater in model 1 vs model 2
prob <- function(d,n,p){choose(n,d)*(p^d)*((1-p)^(n-d))}

likelihoodm1 = prob(sum(data),1000,seq(0,1,length=1000))
likelihoodm2 = prob(sum(data),1000,0.2)

sum(likelihoodm1*1/1000)/sum(likelihoodm2)
