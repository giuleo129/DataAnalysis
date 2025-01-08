#""""""PCA""""""""""""""""
df_cor <- df[,c(4,5,6,8,9,10)]
plot <- pairs.panels(df_cor, hist.col="lightblue", density = TRUE, ellipses = FALSE)
cor_matrix <- cor(df_cor)
corrplot(cor_matrix,type = "upper",tl.col = "black", tl.srt = 45,tl.cex =0.7)

summary(scaled_df)

#eigen_calculating
df_cov <- cov(scaled_df)
df_eigen <- eigen(df_cov)
(df_eigen$values)

# Extract the loadings
(phi <- df_eigen$vectors[,1:5]) # round brackets are used to print at video
phi <- -phi
row.names(phi) <- c("in_spotify_playlist", "streams", "bpm", "danceability","energy","liveness")
colnames(phi) <- c("PC1", "PC2","PC3","PC4","PC5")
phi

# Calculate Principal Component scores
PC1 <- scaled_df %*% phi[,1]
PC2 <- scaled_df %*% phi[,2]
PC3 <- scaled_df %*% phi[,3]
# Create the data frame with Principal Component scores
PC <- data.frame(ID = row.names(df), PC1, PC2, PC3)
head(PC)

# Plot Principal Components for song
ggplot(PC, aes(PC1, PC2)) + modelr::geom_ref_line(h = 0) + modelr::geom_ref_line(v = 0) + geom_text(aes(label = ID), size = 3) + xlab("First Principal Component") + ylab("Second Principal Component") + ggtitle("Scores-1PC and 2PC")
ggplot(PC, aes(PC1, PC3)) + modelr::geom_ref_line(h = 0) + modelr::geom_ref_line(v = 0) + geom_text(aes(label = ID), size = 3) + xlab("First Principal Component") + ylab("Third Principal Component") + ggtitle("Scores-1PC and 3PC")
ggplot(PC, aes(PC2, PC3)) + modelr::geom_ref_line(h = 0) + modelr::geom_ref_line(v = 0) + geom_text(aes(label = ID), size = 3) + xlab("Second Principal Component") + ylab("Third Principal Component") + ggtitle("Scores-2PC and 3PC")

# Proportion of Variance Explained (PVE)
PVE <- df_eigen$values/sum(df_eigen$values)
cumsum(round(PVE, 3))

# scree plot
PVEplot <- qplot(c(1:6), PVE) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("PVE") +
  ggtitle("Scree Plot") +
  ylim(0, 1)
PVEplot

# Cumulative PVE plot
cumPVE <- qplot(c(1:6), cumsum(PVE)) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab(NULL) + 
  ggtitle("Cumulative Scree Plot") +
  ylim(0,1)

grid.arrange(PVEplot, cumPVE, ncol = 2)
head(round(phi[,1:4], digits = 2))

pca_result <- princomp(scaled_df)
biplot(pca_result)
view(df)
