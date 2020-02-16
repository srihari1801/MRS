#links <- read.csv("links.csv")
movies <- read.csv("movies.csv",stringsAsFactors=FALSE)
ratings <- read.csv("ratings.csv")
#tags <- read.csv("tags.csv")

library(recommenderlab)
library(ggplot2)

genres <- as.data.frame(movies$genres, stringsAsFactors=FALSE)
library(data.table)
genres2 <- as.data.frame(tstrsplit(genres[,1], '[|]', 
                                   type.convert=TRUE), 
                         stringsAsFactors=FALSE)
colnames(genres2) <- c(1:10)

genre_list <- c("Action", "Adventure", "Animation", "Children", 
                "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                "Film-Noir", "Horror", "Musical", "Mystery","Romance",
                "Sci-Fi", "Thriller", "War", "Western")

genre_matrix <- matrix(0,10330,18) 
genre_matrix[1,] <- genre_list 
colnames(genre_matrix) <- genre_list
for (i in 1:nrow(genres2)) {
  for (c in 1:ncol(genres2)) {
    genmat_col = which(genre_matrix[1,] == genres2[i,c])
    genre_matrix[i+1,genmat_col] <- 1
  }
}

genre_matrix2 <- as.data.frame(genre_matrix[-1,], stringsAsFactors=FALSE) 
for (c in 1:ncol(genre_matrix2)) {
  genre_matrix2[,c] <- as.integer(genre_matrix2[,c])
} 
years <- as.data.frame(movies$title, stringsAsFactors=FALSE)
library(data.table)
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
years <- as.data.frame(substr(substrRight(substrRight(years$`movies$title`, 6),5),1,4))

search_matrix <- cbind(movies[,1], substr(movies[,2],1,nchar(movies[,2])-6), years, genre_matrix2)
colnames(search_matrix) <- c("movieId", "title", "year", genre_list)

write.csv(search_matrix, "search.csv")
search_matrix <- read.csv("search.csv", stringsAsFactors=FALSE)

subset(search_matrix, Action == 1 & year == 1995)$title

binaryratings <- ratings



for (i in 1:nrow(binaryratings)){
  if (binaryratings[i,3] > 3){
    binaryratings[i,3] <- 1
  }
  else{
    binaryratings[i,3] <- -1
  }
}


for (i in 1:ncol(binaryratings2)){
  binaryratings2[which(is.na(binaryratings2[,i]) == TRUE),i] <- 0
}
binaryratings2 = binaryratings2[,-1] 

movieIds <- length(unique(movies$movieId)) #10329
ratingmovieIds <- length(unique(ratings$movieId)) #10325
movies2 <- movies[-which((movies$movieId %in% ratings$movieId) == FALSE),]
rownames(movies2) <- NULL

genre_matrix3 <- genre_matrix2[-which((movies$movieId %in% ratings$movieId) == FALSE),]
rownames(genre_matrix3) <- NULL



result = matrix(0,18,668) 
for (c in 1:ncol(binaryratings2)){
  for (i in 1:ncol(genre_matrix3)){
    result[i,c] <- sum((genre_matrix3[,i]) * (binaryratings2[,c])) 
  }
}

for (c in 1:ncol(result)){
  for (i in 1:nrow(result)){
    if (result[i,c] < 0){
      result[i,c] <- 0
    }
    else {
      result[i,c] <- 1
    }
  }
}



library(reshape2)
#Create ratings matrix. Rows = userId, Columns = movieId
ratingmat <- dcast(ratings, userId~movieId, value.var = "rating", na.rm=FALSE)
ratingmat <- as.matrix(ratingmat[,-1]) #remove userIds



library(recommenderlab)
#Convert rating matrix into a recommenderlab sparse matrix
ratingmat <- as(ratingmat, "realRatingMatrix")


similarity_users <- similarity(ratingmat[1:4, ], 
                               method = "cosine", 
                               which = "users")
as.matrix(similarity_users)
image(as.matrix(similarity_users), main = "User similarity")


similarity_items <- similarity(ratingmat[, 1:4], method =
                                 "cosine", which = "items")
as.matrix(similarity_items)
image(as.matrix(similarity_items), main = "Item similarity")

vector_ratings <- as.vector(ratingmat@data)
unique(vector_ratings) 

table_ratings <- table(vector_ratings) 
table_ratings

vector_ratings <- factor(vector_ratings)

qplot(vector_ratings) + 
  ggtitle("Distribution of the ratings")

views_per_movie <- colCounts(ratingmat) 

table_views <- data.frame(movie = names(views_per_movie),
                          views = views_per_movie) 
table_views <- table_views[order(table_views$views, 
                                 decreasing = TRUE), ] 

ggplot(table_views[1:6, ], aes(x = movie, y = views)) +
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_x_discrete(labels=subset(movies2, movies2$movieId == table_views$movie)$title) +
  ggtitle("Number of views of the top movies")

image(ratingmat, main = "Heatmap of the rating matrix") # hard to read-too many dimensions
image(ratingmat[1:10, 1:15], main = "Heatmap of the first rows and columns")
image(ratingmat[rowCounts(ratingmat) > quantile(rowCounts(ratingmat), 0.99),
                 colCounts(ratingmat) > quantile(colCounts(ratingmat), 0.99)], 
      main = "Heatmap of the top users and movies")


ratingmat_norm <- normalize(ratingmat)
image(ratingmat_norm[rowCounts(ratingmat_norm) > quantile(rowCounts(ratingmat_norm), 0.99),
                colCounts(ratingmat_norm) > quantile(colCounts(ratingmat_norm), 0.99)], 
      main = "Heatmap of the top users and movies")

recommender_model <- Recommender(ratingmat_norm, 
                                 method = "UBCF", 
                                 param=list(method="Cosine",nn=30))

model_details <- getModel(recommender_model)
model_details$data

recom <- predict(recommender_model, 
                 ratingmat[1], 
                 n=10) 

recom



recom_list <- as(recom, 
                 "list") 
recom_result <- matrix(0,10)
for (i in 1:10){
  recom_result[i] <- as.character(subset(movies, 
                                         movies$movieId == as.integer(recom_list[[1]][i]))$title)
}


evaluation_scheme <- evaluationScheme(ratingmat, 
                                      method="cross-validation", 
                                      k=5, given=3, 
                                      goodRating=5) 
evaluation_results <- evaluate(evaluation_scheme, 
                               method="UBCF", 
                               n=c(1,3,5,10,15,20))
eval_results <- getConfusionMatrix(evaluation_results)[[1]]

