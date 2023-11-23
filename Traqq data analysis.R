library(readxl)
install.packages("dplyr")
library(dplyr)
library(ggplot2)
library(cluster)
library(tidyr)

### PART 1 - Clean the data

# Type Conversion
Traqq$nevoCode <- as.character(Traqq$nevoCode)
SVT$nevocode <- as.character(SVT$nevocode)

# Combine two database(Traqq+SVT)
Data <- left_join(Traqq, SVT, by = c("nevoCode" = "nevocode"))
Data$nevocode <- NULL
Data
write.csv(Data,'Data.csv')

## figure out the missing data!!!

# Create a data set which only contain taste intensity (delete the nutrient)
Data2 <- Data[, c(1, 16:ncol(Data))]
Data2
str(Data2)


### PART 2 - Get the mean taste intensity

# Calculating averages for each participant(get the mean taste intensity values for each participant)
Mean <- Data2 %>%
  group_by(participantCodename) %>%
  summarise(across(everything(), mean, na.rm = TRUE))
Mean

# Set the first column to the row name(the first column is 'id' so we cannot take it into cluster analysis)
Mean2<- Mean
Mean2 <- as.data.frame(Mean2)
rownames(Mean2) <- Mean2[,1]

# Delete the original first column which is now the row name
Mean2 <- Mean2[,-1]
Mean2
write.csv(Mean2,'aaaa.csv')


### PART 3 - Hierarchical cluster

# Calculate the distance matrix
distance_matrix <- dist(Mean2, method = "euclidean")

# Perform hierarchical clustering
hc <- hclust(distance_matrix, method = "ward.D2")

# Tree diagram
plot(hc)

# Cutting tree diagrams to create clusters
group <- cutree(hc, k = 6) # try different k values

#Finding the most suitable k value (compare the average profile width, which is closer to 1)
  # Calculating Contour Objects
  silhouette_values <- silhouette(group, dist(Mean2))
  
  # Get the average profile width
  average_sil_width <- mean(silhouette_values[, "sil_width"])
  average_sil_width

# Adding clustering results to raw data
Mean2$cluster <- as.factor(group)

# print result
head(Mean2)
write.csv(Mean2,'Mean2.csv')


### PART 4 - 
cluster<- Mean2 %>%
  group_by(cluster) %>%
  summarise(across(c(1, 2, 3, 4, 5, 6), 
                   list(mean = ~mean(., na.rm = TRUE), 
                        sd = ~sd(., na.rm = TRUE))))