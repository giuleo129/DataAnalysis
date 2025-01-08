set.seed(123)

#import "spotify_dataset"
spotify <- read.csv("C:/Users/leona/Desktop/Università/DATA SCIENCE/data analysis (6 cfu)/REPORT/spotify-2023.csv")

#create a new data-set with the selected column and 250 observation
df <- spotify[c(1:250),c(1,2,4,7,9,15,17,18,20,23)]

options(scipen = 999) #to remove the scientific notation
df$streams <- as.numeric(df$streams) #streams to numeric
df$mode <- as.factor(df$mode) #mode to factor
df <- df %>%
  rename(danceability=danceability_.,energy=energy_.,liveness=liveness_.) %>%
  mutate(across(c(danceability, energy, liveness), ~ . * 0.01))
view(df)

dim(df) #to show the dimensions
attach(df) #to set the variables available
str(df) #to give a first look to our data-set
summary(df)

scaled_df <- df[,c(4,5,6,8,9,10)]
scaled_df <- apply(scaled_df,2,scale)
boxplot(scaled_df)



