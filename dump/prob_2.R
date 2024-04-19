#### Problem 2 ####

library(MASS)
data = mvrnorm(n=100,mu=c(0,0),Sigma = diag(rep(1,2)))


theta = c(0,0)
theta_new = c(0,0)

f_0 = function(t){
  sum = 0
  for(i in 1:100){
    sum = sum + sqrt(sum((data[i,]-t)^2))
  }
  return(sum)
}


optim(c(0,0),f_0)
