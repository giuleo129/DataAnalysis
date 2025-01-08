scaled_df <- df[,c(4,5,6,8,9,10)]
scaled_df <- as.data.frame(scaled_df)
numeric_df <- df[,c(4,5,6,8,9,10)]
scaled_df <- apply(scaled_df,2,scale)
pairs(scaled_df, pch = 21, gap = 0, main = "scaled")

random_df = apply(df[c(4,5,6,8,9,10)], 2, function(x){runif(length(x), min(x), (max(x)))}) 
random_df = as.data.frame(random_df) 
scaled_random_df = scale(random_df) 
pairs(scaled_random_df, pch = 21, gap = 0, main = "random")

set.seed(123)
hopkins(scaled_df) 
hopkins(scaled_random_df)

dist.eucl <- dist(scaled_df, method = "euclidean")
round(as.matrix(dist.eucl)[1:5, 1:5], 2)
dist.man <- dist(scaled_df, method = "manhattan")
round(as.matrix(dist.man)[1:5, 1:5], 2)

fviz_dist(dist.eucl, show_labels=F)
fviz_dist(dist.man, show_labels=F)

# ------------------ #
# Possible distances #
# ------------------ #

#optimal HIERARCHICAL
fviz_nbclust(scaled_df, hcut, method = "wss") + geom_vline(xintercept = 3, linetype = 2) + labs(subtitle = "Hierarchical: Elbow method")
fviz_nbclust(scaled_df, hcut, method = "silhouette") + labs(subtitle = "Hierarchical: Silhouette method")
fviz_nbclust(scaled_df, hcut, method = "gap_stat", nboot = 500) + labs(subtitle = "Hierarchical: Gap statistic method")

#optimal K-MEDOIDS
fviz_nbclust(scaled_df, cluster::pam, method = "wss") + geom_vline(xintercept = 3, linetype = 2) + labs(subtitle = "K-Medoids: Elbow method")
fviz_nbclust(scaled_df, cluster::pam, method = "silhouette") + labs(subtitle = "K-Medoids: Silhouette method")
fviz_nbclust(scaled_df, cluster::pam, method = "gap_stat", nboot = 500) + labs(subtitle = "K-Medoids: Gap statistic method")

#optimal k-MEANS
fviz_nbclust(NbClust(scaled_df, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans"))

clust.methods <- c("hierarchical","kmeans","pam")
internal <- clValid(scaled_df,nClust = 2:6,clMethods = clust.methods,validation = "internal")
summary(internal)

stability <- clValid(scaled_df,nClust = 2:6, clMethods = clust.methods, validation = "stability")
summary(stability)

#""""AGGOLERATIVE METHOD""""""" dissimilarity matrix
d.euclidean <- dist(scaled_df, method = "euclidean")
d.manhattan <- dist(scaled_df, method = "manhattan")

eu.single <- hclust(d = d.euclidean, method = "single") 
fviz_dend(eu.single, k = 2, cex = 0.5, k_colors = c("darkblue", "orange"), color_labels_by_k = TRUE,rect = TRUE, show_labels = FALSE, main="Single linkage method - Euclidean")
cor(d.euclidean, cophenetic(eu.single))
grp <- cutree(eu.single, k = 2 )
table(grp)

eu.single3 <- hclust(d = d.euclidean, method = "single") 
fviz_dend(eu.single, k = 3, cex = 0.5, k_colors = c("darkblue", "orange","red"), color_labels_by_k = TRUE,rect = TRUE, show_labels = FALSE, main="Single linkage method - Euclidean")
cor(d.euclidean, cophenetic(eu.single))
grp <- cutree(eu.single3, k = 3 )
table(grp)

man.single <- hclust(d = d.manhattan, method = "single")
fviz_dend(man.single,k=2, cex = 0.5, k_colors = c("darkorange","forestgreen"), color_labels_by_k = TRUE,rect = TRUE, show_labels = FALSE, main="Single linkage method - Manhattan")
cor(d.manhattan, cophenetic(man.single))
grp <- cutree(man.single, k = 2 )
table(grp)

eu.av <- hclust(d = d.euclidean, method = "average") 
fviz_dend(eu.av, k = 2, cex = 0.5, k_colors = c("darkblue", "orange"), color_labels_by_k = TRUE,rect = TRUE, show_labels = FALSE, main="Average linkage method - Euclidean")
cor(d.euclidean, cophenetic(eu.av))
grp <- cutree(eu.av, k = 2 )
table(grp)

eu.cen <- hclust(d = d.euclidean, method = "centroid") 
fviz_dend(eu.cen, k = 2, cex = 0.5, k_colors = c("purple","green"), color_labels_by_k = TRUE,rect = TRUE, show_labels = FALSE, main="Average linkage method - Euclidean")
cor(d.euclidean, cophenetic(eu.cen))
grp <- cutree(eu.cen, k = 2 )
table(grp)

eu.war <- hclust(d = d.euclidean, method = "ward.D2") 
fviz_dend(eu.war, k = 2, cex = 0.5, k_colors = c("blue","green"), color_labels_by_k = TRUE,rect = TRUE, show_labels = FALSE, main="Ward.D2 linkage method - Euclidean")
cor(d.euclidean, cophenetic(eu.war))
grp <- cutree(eu.war, k = 2 )
table(grp)

pairs(scaled_df, gap=0,cex = 1, pch=grp, col=c("#00AFBB", "#E7B800")[grp])

fviz_cluster(list(data = numeric_df, cluster = grp),
             palette = c("#2E9FDF", "#FC4E07"),
             ellipse.type = "convex", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             show.clust.cent = FALSE, ggtheme = theme_minimal())

#""""DIVISIVE""""""
res.diana <- diana(x = numeric_df, stand = TRUE, metric = "euclidean", )
fviz_dend(res.diana, cex = 0.6, k = 2, main = "Divisive method - Euclidean", show_labels = FALSE)
cor(d.euclidean, cophenetic(res.diana))
grp <- cutree(res.diana, k = 2 )
table(grp)

pairs(numeric_df, gap=0,cex = 1, pch=grp, col=c("darkorange", "forestgreen")[grp])


# Silhouette method for PAM (look at the maximum)

fviz_nbclust(scaled_df, pam, method = "silhouette")+
  theme_classic()

## Computing PAM clustering

# Compute PAM with K = 2

pam.res <- pam(x = scaled_df,k = 2, diss = F, metric = "euclidean" )
print(pam.res)

# Adding the point classifications to the original data

dd <- cbind(numeric_df, cluster = pam.res$cluster)
head(dd, n = 8)

## Accessing to the results of pam() function

# Cluster medoids: New Mexico and Nebraska

pam.res$medoids

# Cluster numbers

head(pam.res$clustering)

## Visualizing PAM clusters

# on the original space

cl <- pam.res$clustering
pairs(numeric_df, gap=0, pch=cl, col=c("#00AFBB", "#FC4E07")[cl])

# on the space of the first 2 PCs

fviz_cluster(pam.res,
             palette = c("#00AFBB", "#FC4E07"), # color palette
             ellipse.type = "t", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_classic()
)
pam.res$clusinfo

fviz_nbclust(scaled_df, kmeans, nstart = 25, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)

## Computing $K$-means clustering

# Compute K-means with K = 4

set.seed(123)
km.res <- kmeans(scaled_df, 2, nstart = 25)

## Print the results

print(km.res)

# Mean of the original variables for each cluster

aggregate(numeric_df, by=list(cluster=km.res$cluster), mean)

# Adding the point classifications to the original data

dd <- cbind(numeric_df, cluster = km.res$cluster)
head(dd)

## Accessing to the results of kmeans() function

# Cluster number for each of the observations

km.res$cluster

# Cluster size

km.res$size

# Cluster means

round(km.res$centers,2)

## Visualizing K-means clusters in the original space

cl <- km.res$cluster
pairs(numeric_df, gap=0, pch=cl, col=c("#2E9FDF", "#FC4E07")[cl])

## Visualizing K-means clusters

fviz_cluster(km.res, 
             data = numeric_df,
             palette = c("#2E9FDF", "#FC4E07"),
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)
