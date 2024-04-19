#### Problem 4 #####

library(MASS)
library(ddalpha)

m = 20
n = 30

data_1 = mvrnorm(n=m,mu=c(5,5),Sigma = diag(rep(1,2)))
data_2 = mvrnorm(n=n,mu=c(-5,-5),Sigma = diag(rep(1,2)))

calc_t1 <- function(X,Y){
  #forming the data cloud
  Z <- NULL
  for(i in 1:nrow(X)){
    for(j in 1:nrow(Y)){
      Z <- rbind(Z, X[i,] - Y[j,]) 
    }  
  }
  
  return(1 - depth.spatial(rep(0,ncol(Z)),Z))
}

repetition = 100000
val = numeric(0)

for(i in 1:repetition){
  cat(i,"\n")
  val = c(val,calc_t1(data_1,data_2[sample(nrow(data_2)),]))
}

val_true = calc_t1(data_1,data_2)


cat("p-value",length(which(val_true<val))/length(val))
