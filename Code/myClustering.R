#### Read the pre-processed data ####
bcw <- readRDS("./Data/bcw_processed.Rda")
set.seed(9218)
#exclude Class column
bcw_features <- bcw[,1:9]

#### k-means with k = 2 ####
k = 2
km.results <- kmeans(bcw_features, k)
jpeg(filename = paste("./Plot/", k, "_Clusters.jpeg", sep = ""), quality = 90)
plot(bcw_features[,c("Clump.Thickness", "Uniformity.of.Cell.Size")], col = km.results$cluster)
points(km.results$centers[,c("Clump.Thickness", "Uniformity.of.Cell.Size")], col = 1:2, pch = 8, cex = 2)
title(paste("k = ", k, sep = ""))
dev.off()

#plot original data with Class
jpeg(filename = "./Plot/Orig_Clusters.jpeg", quality = 90)
plot(bcw[,c("Clump.Thickness", "Uniformity.of.Cell.Size")], col = bcw$Class, main="Breast Cancer by Class")
dev.off()

#show the confusion matrix for 2 clusters against the original data
as.matrix(table(Actual = bcw$Class, Clusters = km.results$cluster))

#keep all SSE results in a vector for later analysis
results <- vector(mode = "list", length = 4)
names(results) <- c("2", "3", "4", "5")
results[[as.character(k)]] <- km.results$tot.withinss

#### K-means with k =3,4,5 ####
for (k in 3:5) {
  
  km.results <- kmeans(bcw_features, k)
  
  jpeg(filename = paste("./Plot/", k, "_Clusters.jpeg", sep = ""), quality = 90)
  plot(bcw_features[,c("Clump.Thickness", "Uniformity.of.Cell.Size")], col = km.results$cluster)
  points(km.results$centers[,c("Clump.Thickness", "Uniformity.of.Cell.Size")], col = 1:k, pch = 8, cex = 2)
  title(paste("k = ", k, sep = ""))
  dev.off()
  
  results[[as.character(k)]] <- km.results$tot.withinss
}

#### How SSE changes with K ####
jpeg(filename = "./Plot/SSE_vs_K.jpeg")
plot(x = 2:5, y = results, xlab = "K", ylab = "SSE",
     col = rgb(1,0,0,1), type = "s", main = "SSE vs K")
dev.off()




#### Hierarchical clustering ####


hClusters <- hclust(dist(bcw_features))

png(filename = "./Plot/H_C_Dendro_Default.png", width = 1600, height = 900)
plot(hClusters, main = "Default Dendrogram")
dev.off()

for (K in 2:5) {
  jpeg(filename = paste("./Plot/", K, "_H_Clusters.jpeg", sep = ""), quality = 90)
  hC_K <- cutree(hClusters, k = K)
  plot(bcw_features[,c("Clump.Thickness", "Uniformity.of.Cell.Size")], col = hC_K)
  title(paste("Number of clusters = ", K, sep = ""))
  dev.off()
}

# Check the confusion matrix for 2 H clusters against the original data
as.matrix(table(Actual = bcw$Class, Clusters = cutree(hClusters, k = 2)))


#### Try different hierarchical clustering methods ####
png(filename = "./Plot/H_C_Dendro_Single.png", width = 1600, height = 900)
plot(hclust(dist(bcw_features), method = "single"), main = "Single Dendrogram")
dev.off()

png(filename = "./Plot/H_C_Dendro_complete.png", width = 1600, height = 900)
plot(hclust(dist(bcw_features), method = "complete"), main = "Complete Dendrogram")
dev.off()

png(filename = "./Plot/H_C_Dendro_Avg.png", width = 1600, height = 900)
plot(hclust(dist(bcw_features), method = "average"), main = "Average Dendrogram")
dev.off()

