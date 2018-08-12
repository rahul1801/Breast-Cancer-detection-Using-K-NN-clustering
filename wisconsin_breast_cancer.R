data = cancer
str(data)
data = subset(data,select = -`id`)
table(data$diagnosis)
data$diagnosis = factor(data$diagnosis,levels = c("B","M"),labels = c("Benign","Malignant"))
round(prop.table(table(data$diagnosis))*100,digits = 1)
summary(data[c('radius_mean','area_mean','smoothness_mean')])
normalize = function(x) {
  return((x - min(x))/(max(x) - min(x)))
}
data_normalized = as.data.frame(lapply(data[2:31], normalize))
summary(data_normalized)
names(data_normalized[0])
data_train = data_normalized[1:469,]
data_test = data_normalized[470:569,]
data_train_labels = data[1:469,1,drop = T]
data_test_labels = data[470:569,1,drop = T]
install.packages("class")
library(class)
data_test_pred = knn(train = data_train,test = data_test,cl = data_train_labels,k = 21)
install.packages("gmodels")
library(gmodels)
CrossTable(x = data_test_labels,y = data_test_pred,prop.chisq = F)
data_z = as.data.frame(scale(data[-1]))
summary(data_z)
data_train_z = data_z[1:469,]
data_test_z = data_z[470:569,]
data_train_labels_z = data[1:469,1,drop = T]
data_test_labels_z = data[470:569,1,drop = T]
data_test_pred_z = knn(train = data_train_z,test = data_test_z,cl = data_train_labels_z,k = 21)
CrossTable(x = data_test_labels_z,y = data_test_pred_z,prop.chisq = F)
