#### Problem 3 ####
library(MASS)
library(e1071)

m = 100
n = 150

data_1 = mvrnorm(n=m,mu=c(1,1),Sigma = diag(rep(1,2)))
data_2 = mvrnorm(n=n,mu=c(-1,-1),Sigma = diag(rep(1,2)))

counter = 0

while(TRUE){
  cdata_1 = data_1[chull(data_1),]
  cdata_2 = data_2[chull(data_2),]
  
  label_1 = rep(1, nrow(cdata_1))   
  label_2 = rep(-1, nrow(cdata_2))
  
  data = rbind(cdata_1,cdata_2)
  label = c(label_1,label_2)
  
  svm_result = svm(x = data, y = label, kernel = "linear",type = 'C-classification')
  predicted_labels = predict(svm_result,data) 
  print(predicted_labels)
  misclassified_indices <- which(predicted_labels != label)
  if(length(misclassified_indices) == 0 && counter == 0){
    cat("Already separable!!! relux koro\n")
    break
  } else if(length(misclassified_indices) == 0 && counter > 0){
  
    cat("Convex hull is linearly separable and points removed is:",counter,"\nCazz relux koro\n")  
    break
  } else{
    # misclassified_points <- data[misclassified_indices, ]
    # if(length(misclassified_indices <= m) >= length(misclassified_indices > m)){
    #   i = misclassified_indices[misclassified_indices<=m][0]
    #   data_1 = data_1[-i,]
    #   counter = counter + 1
    #   m = m - 1
    # } else {
    #   i = misclassified_indices[misclassified_indices>m][0] - m
    #   data_2 = data_2[-i,]
    #   counter = counter + 1
    #   n = n - 1
    # }
    
    i = misclassified_indices[1]
    if(i <= nrow(cdata_1)){
      data_1 = data_1[-chull(data_1)[i],]
    } else {
      data_2 = data_2[-chull(data_2)[i-nrow(cdata_1)],]
    }
    counter = counter + 1
  }
  cat(counter,"\n")
}





