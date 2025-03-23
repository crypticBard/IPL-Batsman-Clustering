 ############################################################################
## KMeans Clustering of IPL players
## In the previous script we created aggregated lifetime data for each player  
## In this script we see how to perform KMeans Clustering 
##  WE perform these steps - 
## 1. Load aggregate data 
## 2. Try to reduce the dimension so that Kmeans algorithm performs better
##    We do this by removing highly correlated columns (one for each pair)
## 3. Scale each column so that no one column dominates others 
## 4. Perform Kmeans Analysis with 3,4,5 centres 
## 5. Create a dataframe showing players in each cluster and compare results 
 

## Note : Here we have no way of telling how good the clustering is. We just 
##          assess subjectively by what we know about each player
##      : Some clustering functions,  once trained,  also allow to  
##        predict cluster for unseen players. R's base KMean does not.
##        Even then, you should  keep some player data aside for this, to learn  


library(dplyr)

############################################################################
## 1. Load Data using dget 
############################################################################
df<- dget(file = "batter_lifetime_final.rd")
View(df)
head(df)

sapply(df, class)
glimpse(df)

############################################################################
## 2. Remove any columns you can 
############################################################################

# if any two columns are too highly correlated, we can remove one of them 
# this should improve results
df
dim(df)
# correlation matrix is a matrix of correlation between every 2 columns
# cor() finds this correlation
# we'll leave out the first column which is player's names
correlation_mat<- cor(df[,2:12])

# making a heatmap would be useful
heatmap(correlation_mat) #darker cells mean higher correlation
View(correlation_mat)

#----------- A function to  remove highly correlated columns (skip) -----------
#  this is the standard way of removing high correlation columns 
# without using any libraries 
# Strategy will be same in python too 
# This function is taken from the internet, so you can just accept it as is

removeHighCorreatedColumns<- function(data, cutoff=0.85){
  #get Correlation matrix
  correlationMatrix<- cor(data)
  # Get upper triangle  excluding diagonal 
  upperTriangle <- upper.tri(correlationMatrix, diag = FALSE)
  # Find indices of highly correlated pairs (absolute value)
  highlyCorrelatedIndices <- which(abs(correlationMatrix[upperTriangle]) > cutoff)
  # Convert indices to column numbers
  highlyCorrelatedColumns <- unique(which(upperTriangle, 
                                          arr.ind = 
                                            TRUE)[highlyCorrelatedIndices, 2])
  # Remove highly correlated columns
  data_filtered <- data[, -highlyCorrelatedColumns]
  return( data_filtered)
}

# -------------------------------------------------------------------------

# df[,-1] means drop first column df[-1, ] means drop first row
df_filtered<- removeHighCorreatedColumns( data = df[,-1],cutoff = 0.85 )
dim(df_filtered) 

#obviously we need to add batter column back to this smaller 
df_filtered$batter<- df$batter

############################################################################
## 3. Scale Each Column around its mean
############################################################################

# It is alway better to do this, even if a particular function does not need this .
# Here's how you can scale the numerical columns
# WE are  using a new dataframe and retaining the original data  
df_filtered_scaled<- df_filtered
dim(df_filtered_scaled)
df_filtered_scaled[,-9]<- scale(df_filtered_scaled[,-9]) #9th column is batter

#compare scaled df with original 
df_filtered[20,]
df_filtered_scaled[20,]


# you can see that all these operations preserve all the rows and their order 
# this is a must, because row = record = observation 
# Also this allows us to append original data  to this new data, if we need  
df %>% dim()
df_filtered %>% dim()
df_filtered_scaled %>% dim()
View(df_filtered)

df_filtered %>% names()

# KMeans requires numeric columns
df_numeric<-  df_filtered[,1:8]
# so we were able to drop 3 columns 
cor(df_numeric) %>% heatmap

############################################################################
## 4. Perform K MEans Clustering 
############################################################################
# now let's use these numeric columns to find clusters
# Set a seed to get the same result every time 
set.seed(2048)
km5<- kmeans(x=df_numeric, centers = 5)
km5
km5$centers
km5$cluster
# the order is same as the original df, we can set the  2 columns side by side 
data.frame( batter= df_filtered$batter, clust5=km5$cluster ) %>% View()

# you can see some groups have very few members
km5$cluster %>% table()

# Now find clusters for scaled data 
##########

df_numeric_scaled <- df_filtered_scaled[,-9]
names(df_filtered_scaled)
names(df_numeric_scaled)
km5_scaled <- kmeans(x=df_numeric_scaled,5)

km5_scaled
km5_scaled$cluster

clusterdf_scaled<- data.frame(batter=df_filtered$batter,clust5_scaled=km5_scaled$cluster)
View(clusterdf_scaled)


plot(df_filtered,col=clusterdf_scaled$clust5_scaled)
#########

clusterdf<- data.frame( batter= df_filtered$batter, clust5=km5$cluster ) 
View( clusterdf )




############# END 
