#DANA Clustering Project
# Team 06

#Amit Verma 		(100329477)
#Sparsh Sharma 	(100329093)
#Simardeep Kaur 	(100329207)
#Charanjeet Singh	(100329356)
#--------------------------------------------------------------------------------------------------#
#Hierarchical Clustering
#Reading file
data<-read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/00292/Wholesale customers data.csv")
str(data)
#Data structure and preparation
df<-data[3:8]

# Standardize the data
dfscaled<-scale(df)

# Show the first 6 rows
head(dfscaled,nrow=5)

# Similarity Measures
#Compute dissimilarity matrix
res.dist <- dist(dfscaled, method = "euclidean")

## ------------------------------------------------------------------------
as.matrix(res.dist)[1:5,1:5]

# Correlation based distance
library("factoextra")
dist.cor <- get_dist(dfscaled, method = "pearson")
# Display a subset
round(as.matrix(dist.cor)[1:3, 1:3], 1)


## ------------------------------------------------------------------------
res.hc <- hclust(d = res.dist, method = "ward.D")

#Dendrogram
library("factoextra")
fviz_dend(res.hc, cex = 0.5)

#Verify the cluster tree
# Compute cophentic distance
res.coph <- cophenetic(res.hc)

# Correlation between cophenetic distance and the original distance
cor(res.dist, res.coph)

#Execute the hclust() function again using the average linkage method. Next, call
#cophenetic() to evaluate the clustering solution.
res.hc2 <- hclust(res.dist, method = "average")
cor(res.dist, cophenetic(res.hc2))

#Cut tree into 3 groups
grp <- cutree(res.hc, k = 2)
head(grp, n = 4)

# Number of members in each cluster
table(grp)

# Get the names for the members of cluster 1
rownames(df)[grp == 1]

# Cut in 2 groups and color by groups
fviz_dend(res.hc, k = 2, # Cut in 2 groups
          cex = 0.5, # label size
          k_colors = c("#00AFBB", "#E7B800"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE)

#Clusterplot
fviz_cluster(list(data = dfscaled, cluster = grp),
             palette = c("#8476c3", "#b4005b"), 
             ellipse.type = "convex", # Concentration ellipse
             repel = FALSE, # Avoid label overplotting (slow)
             show.clust.cent = FALSE, ggtheme = theme_minimal())


#Comparing Dendrogram
library(dendextend)

# Compute distance matrix
res.dist <- dist(dfscaled, method = "euclidean")

# Compute 2 hierarchical clusterings
hc1 <- hclust(res.dist, method = "average")
hc2 <- hclust(res.dist, method = "ward.D")

# Create two dendrograms
dend1 <- as.dendrogram (hc1)
dend2 <- as.dendrogram (hc2)

# Create a list to hold dendrograms
dend_list <- dendlist(dend1, dend2)

#Visual comparison of two dendrograms
tanglegram(dend1, dend2,
           highlight_distinct_edges = FALSE, # Turn-off dashed lines
           common_subtrees_color_lines = TRUE, # Turn-off line colors
           common_subtrees_color_branches = TRUE, # Color common branches
           main = paste("entanglement =", round(entanglement(dend_list), 2)))

#Correlation matrix between a list of dendrograms
# Cophenetic correlation matrix
cor.dendlist(dend_list, method = "cophenetic") 

# Baker correlation matrix
cor.dendlist(dend_list, method = "baker")

# Create multiple dendrograms by chaining
dend1 <- dfscaled %>% dist %>% hclust("complete") %>% as.dendrogram
dend2 <- dfscaled %>% dist %>% hclust("single") %>% as.dendrogram
dend3 <- dfscaled %>% dist %>% hclust("average") %>% as.dendrogram
dend4 <- dfscaled %>% dist %>% hclust("centroid") %>% as.dendrogram

#-------------------------------------
tanglegram(dend2, dend4,
           highlight_distinct_edges = FALSE, # Turn-off dashed lines
           common_subtrees_color_lines = TRUE, # Turn-off line colors
           common_subtrees_color_branches = TRUE, # Color common branches
           main = paste("entanglement =", round(entanglement(dend_list), 2)))



# Compute correlation matrix
dend_list <- dendlist("Complete" = dend1, "Single" = dend2,
                      "Average" = dend3, "Centroid" = dend4)

cors <- cor.dendlist(dend_list)
# Print correlation matrix
round(cors, 2)

# Visualize the correlation matrix using corrplot package
library(corrplot)
corrplot(cors, "pie", "lower")

#Visualizing dendrograms
fviz_dend(res.hc, k = 2, # Cut in 2 groups
          cex = 0.5, # label size
          k_colors = c("#00AFBB", "#E7B800"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE, # Add rectangle around groups
          rect_border = c("#2E9FDF",  "#E7B800"),
          rect_fill = TRUE)

#Circular Dendrogram
fviz_dend(res.hc, cex = 0.5, k = 2,
          k_colors = "jco", type = "circular")

#phylogenic dendrogram
require("igraph")
fviz_dend(res.hc, k = 2, k_colors = "jco",
          type = "phylogenic", repel = FALSE)

#Heatmap
library(plotly)
# Default plot
heatmap(dfscaled, scale = "none")

#Enhanced heat maps: heatmap.2()
library("gplots")
heatmap.2(dfscaled, scale = "none", col = bluered(100),
          trace = "none", density.info = "none")

# Pretty heat maps: pheatmap()
library("pheatmap")
pheatmap(dfscaled, cutree_rows = 4)

#Determining the Optimal Number of Clusters
library(factoextra)
library(NbClust)

# Elbow method
fviz_nbclust(dfscaled, kmeans, method = "wss") +
  geom_vline(xintercept = 2, linetype = 2)+
  labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(dfscaled, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# Gap statistic
fviz_nbclust(dfscaled, kmeans, nstart = 25, method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")

#Clustering analysis
# Hierarchical clustering
hc.res <- eclust(dfscaled, "hclust", k = 2, hc_metric = "euclidean",hc_method = "ward.D", graph = FALSE)

# Visualize dendrograms
fviz_dend(hc.res, show_labels = FALSE,
          palette = "jco", as.ggplot = TRUE)

#Cluster Validation with Silhouette measure
km.res <- eclust(dfscaled, "kmeans", k = 2, nstart = 25, graph = FALSE)
fviz_silhouette(km.res, palette = "jco",
                ggtheme = theme_classic())

# Silhouette information
silinfo <- km.res$silinfo
names(silinfo)

# Silhouette widths of each observation
head(silinfo$widths[, 1:3], 10)

# Silhouette width of observation
sil <- km.res$silinfo$widths[, 1:3]

# Objects with negative silhouette
neg_sil_index <- which(sil[, 'sil_width'] < 0)
sil[neg_sil_index, , drop = FALSE]

#Cluster Validation with Dunn index
library(fpc)

# Statistics for k-means clustering
km_stats <- cluster.stats(dist(dfscaled), km.res$cluster)

# Dun index
km_stats$dunn

#Choosing the Best Clustering Algorithms
library("clValid")

# Compute clValid
clmethods <- c("hierarchical","kmeans","pam")
intern <- clValid(dfscaled, nClust = 2:6,
                  clMethods = clmethods, validation = "internal")
# Summary
summary(intern)

#-----------------------------------------------------------------------------------------------------#
# Partitional Clustering
#Reading the file
wheat_dataset<-read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/00236/seeds_dataset.txt",header=F)
head(wheat_dataset)
str(wheat_dataset)

#Taking appropriate columns
wheat_data<-wheat_dataset[ ,-8]
#Renaming columns
colnames(wheat_data)<-c('Area','Perimeter','Compactness','Length_Of_Kernel','Width_Of_Kernel','Asym_Coeff','Length_Kernel_Groove')
head(wheat_data)

library('factoextra')
#Scaling the data
wheat_data <- scale(wheat_data)
#Elbow Method
fviz_nbclust(wheat_data, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)



# Computing k-means with k = 3
set.seed(123)
wheat_km_res <- kmeans(wheat_data, 3, nstart = 25)
# Print the results
print(wheat_km_res)


#Taking aggregate mean
aggregate(wheat_data, by=list(cluster=wheat_km_res$cluster), mean)


#To perform Principal Component Analysis (PCA) and to plot data points according to the first two principal components coordinates
fviz_cluster(wheat_km_res, data = wheat_data,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
             ellipse.type = "euclid", 
             star.plot = TRUE, 
             repel = TRUE, 
             ggtheme = theme_minimal()
)

library('cluster')
#Performing PAM on dataset
wheat_pam_res <- pam(wheat_data, 2)
print(wheat_pam_res)

library('NbClust')
library('factoextra')
library('cluster')
#Silhouette Method
fviz_nbclust(wheat_data, pam, method = "silhouette")+
  theme_classic()

#Performing PCA and plotting points
fviz_cluster(wheat_pam_res, palette = c("#00AFBB", "#FC4E07"),  ellipse.type = "t",   repel = TRUE, 
             ggtheme = theme_classic())

heatmap(wheat_data, scale = "none")

# Computing Hopkins statistic for dataset
wheat_res <- get_clust_tendency(wheat_data, n = nrow(wheat_data)-1, graph = FALSE)
wheat_res$hopkins_stat


#Dissimilarity matrix (DM)
fviz_dist(dist(wheat_data), show_labels = FALSE)+
  labs(title = "Wheat data")


set.seed(123)
fviz_nbclust(wheat_data, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")

library('NbClust')
nb <- NbClust(wheat_data, distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "kmeans")

#Visualize Nb
fviz_nbclust(nb)

library(fpc)
# Statistics for k-means clustering
wheat_km_stats <- cluster.stats(dist(wheat_data),  wheat_km_res$cluster)
# Dun index
wheat_km_stats$dunn


# Compute cluster stats
seed_type <- as.numeric(wheat_dataset$Seed_Type)
clust_stats <- cluster.stats(d = dist(wheat_dataset), seed_type,wheat_km_res$cluster)
# Corrected Rand index
clust_stats$corrected.rand
# Computing VI index
clust_stats$vi

library('clValid')
# Compute clValid
clmethods <- c("hierarchical","kmeans","pam")
wheat_valid <- clValid(wheat_data, nClust = 2:6, 
                       clMethods = clmethods, validation = "internal")
# Summary
summary(wheat_valid)


# Stability measures
clmethods <- c("hierarchical","kmeans","pam")
wheat_stab <- clValid(wheat_data, nClust = 2:6, clMethods = clmethods, 
                      validation = "stability")
# Display only optimal Scores
optimalScores(wheat_stab)


