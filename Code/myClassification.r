#### Add Packages ####
install.packages("party")
install.packages("class")
library(class)
library(party)

#### Read the pre-processed data ####
bcw_c <- readRDS("./Data/bcw_processed.Rda")
set.seed(9218)

#### Divide the data 70/30 ####
trainidx<-sample(nrow(bcw_c), floor(nrow(bcw_c) * 0.70))
bcw_train<-bcw_c[trainidx,] #Accessing all trainidx rows
bcw_test<-bcw_c[-trainidx,] #Accessing all rows except trainidx

paste("Size of Training data: ", nrow(bcw_train))
paste("Size of Testing data: ", nrow(bcw_test))

#### Tree classification ####
bcw_formula <- Class ~ .
bcw_ctree <- ctree(bcw_formula, data = bcw_train)

png(filename = "./Plot/Ctree_Default.png",width = 1280, height = 720)
plot(bcw_ctree, type = "simple")
dev.off()

ctree_pred <- predict(bcw_ctree, newdata = bcw_test[,1:9])

#### Evaluation #####

#Function to generate performance figures from a confusion matrix
calculatePerformance <- function(confu_matrix) {
  N = sum(confu_matrix)
  TP_TN = diag(confu_matrix)
  Actual = apply(confu_matrix, 1, sum)
  Predicted = apply(confu_matrix, 2, sum)
  
  accuracy = sum(TP_TN) / N
  
  precision = TP_TN/Predicted
  recall = TP_TN/Actual
  f1 = 2 * (precision * recall) / (precision + recall)
  return(data.frame(accuracy,precision, recall, f1))
}

confu_m = as.matrix(table(Actual = bcw_test[,10], Predicted = ctree_pred))
calculatePerformance(confu_m)

#### Change ctree parameters ####
bcw_ctree2 <- ctree(bcw_formula, data = bcw_train, 
                    controls = ctree_control(testtype = "MonteCarlo"))
png(filename = "./Plot/Ctree_MonteCarlo.png", width = 1080, height = 720)
plot(bcw_ctree2, type = "simple")
dev.off()

bcw_ctree3 <- ctree(bcw_formula, data = bcw_train, 
                    controls = ctree_control(testtype = "MonteCarlo", mincriterion = 0.97))
png(filename = "./Plot/Ctree_MonteCarlo_97.png", width = 1080, height = 720)
plot(bcw_ctree3, type = "simple")
dev.off()

bcw_ctree4 <- ctree(bcw_formula, data = bcw_train, 
                    controls = ctree_control(testtype = "MonteCarlo", mincriterion = 0.97, maxdepth = 3))
png(filename = "./Plot/Ctree_MonteCarlo_97_max3.png", width = 1080, height = 720)
plot(bcw_ctree4, type = "simple")
dev.off()

ctree_pred4 <- predict(bcw_ctree4, newdata = bcw_test[,1:9])

confu_m4 = as.matrix(table(Actual = bcw_test[,10], Predicted = ctree_pred4))
calculatePerformance(confu_m4)


#### KNN ####
perf_results <- vector(mode = "list", length = 5)
for( k in 1:5) {
  knn_pred <- knn(train = bcw_train[,1:9], test = bcw_test[,1:9], cl = bcw_train[,10], k)
  perf <- calculatePerformance(as.matrix(table(Actual = bcw_test[,10], Predicted = knn_pred)))
  perf_results[[k]] <- perf
}

perf_results
