a = rnorm(100,10,1)
b = a * runif(100,0.1,0.2)
plot(a,b)
print(cor(a,b))

#So for any two vectors, you can estimate the posterior distribution of the B parameter
#let us assume that the prior distribution of the B parameter will generally follow a normal distibution 
#around the mean of the parameter
#The likelihood of the parameter will be simply the likelihood of the data given this parameter
#This will be the P(Y|B) = probability that Y is equal to X*B and hence we can measure
#this using the residual variation so, P(Y-X*B==0) and we can do this for every variable and 
#look at the sum of the log

prior_mean = 0.415
prior_sd = 10
prior_data = seq(0,1,length=1e04)
likelihood <- function(Y,X,B)
{
  sum(dnorm((Y-(X*B)),0,1,log = T))
}

posterior = vector()
for(i in 1:length(prior_data))
{
  posterior[i] = likelihood(b,a,prior_data[i]) + dnorm(prior_data[i],prior_mean,prior_sd)
}
mat = as.matrix(cbind(prior_data,posterior))
mat[is.infinite(as.numeric(mat[,2])),] = 0
mat[,2] = exp(mat[,2])/sum(exp(mat[,2]))
map = mat[mat[,2]==max(mat[,2]),1]
plot(a,b)
abline(lm(b~0+a))
points(a,a*map)
plot(mat)