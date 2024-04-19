#### Problem 1 ####

library(ddalpha)


data = mvrnorm(n=100,mu=c(0,0),Sigma = diag(rep(1,2)))

depth_1 = function(x){
  return(-depth.halfspace(x,data))
}

depth_2 = function(x){
  depth.simplicial(c(0,0),data)
}




optim(c(0,0),depth_1) ## Half-space median
optim(c(0,0),depth_2) ## Simplicial median

