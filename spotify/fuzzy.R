#### FkM algorithm ####
fkm <- FKM(X = numeric_df, m = 1.2, RS = 50, stand = 1, index = "SIL.F")
summary(fkm)
Hraw(fkm$X, fkm$H)
pairs(scaled_df, col = fkm$clus[, 1], pch = 16, cex = 0.8, gap = 0)
plot.fclust(x = fkm, pca = TRUE)

#### FkMed algorithm ####
fkmed <- FKM.med(X = scaled_df, m = 1.2, RS = 50, stand = 1, index = "SIL.F")
summary(fkmed)
Hraw(scaled_df, fkmed$H)
pairspairspairs(scaled_df, col = fkmed$clus[, 1], pch = 16, cex = 0.8, gap = 0)
plot.fclust(x = fkmed, pca = TRUE)

