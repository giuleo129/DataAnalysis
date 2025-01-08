#"""""""""Model-Based""""""""""
model <- Mclust(scaled_df, G = 1:10, modelNames = NULL)
plot(model$BIC)

summary(model$BIC)
summary(model)

head(round(model$z, 6), 15)    # Probability to belong to a given cluster
head(model$classification, 15) # Cluster assignment of each observation

pairs(scaled_df, gap=0, pch = 16, col = model$classification)
plot(model)

probabilities <- as.matrix(round(model$z, 5)) # Probability to belong to a given cluster
colnames(probabilities) <- c("pi1", "pi2", "pi3", "pi4", "pi5")
classification <- as.matrix(model$classification) # Cluster assignment of each observation
colnames(classification) <- "Classification"
uncertainty <- as.matrix(round(model$uncertainty, 5)) # Classification Uncertainty
colnames(uncertainty) <- "Uncertainty"
# Combining the overall information in one single matrix for a more orderly display of data
model_output <- cbind(probabilities, classification, uncertainty)
head(model_output, 30)

fviz_mclust(model, "classification", geom = "point", pointsize = 1.5, palette = "jco")
fviz_mclust(model, "uncertainty", palette = "jco")

# Detecting the most uncertain units in terms of classification with their PCs scores
PC <- prcomp(scaled_df, scale. = TRUE)
PC <- PC$x

cbind(PC[model_output[,"Uncertainty"] > 0.50, 1:2], Uncertainty = model_output[model_output[,"Uncertainty"] > 0.50, 6], Classification = model_output[model_output[,"Uncertainty"] > 0.50, 7])
model_output[model_output[,"Uncertainty"] > 0.50, 1:7]
view(spotify)
