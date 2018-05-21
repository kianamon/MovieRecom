#Set the working directory:
setwd("/Users/Kianamon/R/project")
#####################################################################################
#libraries in use:
library(knitr)
library(httr)
library(readr)
library(dplyr)
library(tidyr)
library(XML)
library(ggplot2)
library(stringr)
library(lubridate)
library(grid)
library(caret)
library(ranger)
library(rpart)
library()
library()
#####################################################################################
#check for missing packages and install them:
list.of.packages <- c("knitr", "httr", "readr", "dplyr", "tidyr", "XML",
                      "ggplot2", "stringr", "lubridate", "grid", "caret")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
#####################################################################################
#downloading the two main data sets:
#All the movies from IMDB website:
GET("https://raw.githubusercontent.com/kianamon/MovieRecom/master/movies.csv", 
    write_disk("movies.csv", overwrite = TRUE))
df_movies <- read_csv("movies.csv")
#All the movies that I have watched:
GET("https://raw.githubusercontent.com/kianamon/MovieRecom/master/kiana_watchlist.csv", 
    write_disk("kianamovies.csv", overwrite = TRUE))
df_kianamovies <- read_csv("kianamovies.csv")
#####################################################################################
#renaming the columns for convenience:
names(df_kianamovies)[names(df_kianamovies) == 'IMDb Rating'] <- 'Score'
names(df_kianamovies)[names(df_kianamovies) == 'Runtime (mins)'] <- 'Duration'
names(df_kianamovies)[names(df_kianamovies) == 'Your Rating'] <- 'KianaRating'
names(df_kianamovies)[names(df_kianamovies) == 'Title Type'] <- 'Type'
names(df_kianamovies)[names(df_kianamovies) == 'Num Votes'] <- 'Num_Votes'

names(df_movies)[names(df_movies) == 'movie_title'] <- 'Title'
names(df_movies)[names(df_movies) == 'movie_imdb_link'] <- 'URL'
names(df_movies)[names(df_movies) == 'num_voted_users'] <- 'Num_Votes'
names(df_movies)[names(df_movies) == 'movie_title'] <- 'Title'
names(df_movies)[names(df_movies) == 'director_name'] <- 'Directors'
names(df_movies)[names(df_movies) == 'imdb_score'] <- 'Score'
names(df_movies)[names(df_movies) == 'duration'] <- 'Duration'
names(df_movies)[names(df_movies) == 'genres'] <- 'Genres'
#####################################################################################
#we have a big data set, we train our model for the kiana data set and apply the results 
#to the big data set and calculate the likability of the movie for the big data set.
#The output of the system is the Likability of each movie.
movies1 <- df_movies %>%
  select(Title, URL, Score, Duration, Genres, Directors, Num_Votes)
kiana <- df_kianamovies %>%
  select(Title, URL, Score, Duration, Genres, Directors, Num_Votes, KianaRating)
#examining user's watched movies to see a pattern:
#I am definig a factor as Likability to calculate a score for each movie,
#Likability goes from 0 to 100 which is the probabilty of me liking a movie
df_kiana <- kiana %>%
  mutate(Likability = KianaRating*10) %>%
  select(-KianaRating)
#####################################################################################
#from now on, we are only working with df_kiana data set:
#first we save the data set to a csv file:
#saving the data frame in a csv file
write.table(df_kiana,  
            paste0("/Users/Kianamon/R/project/kiana.csv"), row.names=F)
write.table(movies1,  
            paste0("/Users/Kianamon/R/project/bigsetofmovies.csv"), row.names=F)
#####################################################################################
#now we should manipulate the data to do the analysis:
df_kiana1 <- as.data.frame(df_kiana) %>% 
  separate(Genres, into = paste("Genres", 1:4, sep = ""), sep = ",")
head(df_kiana1)
df_kianatrain <- df_kiana1 
#  select(-KianaRating, -URL, -Title) 
df_kianatrain$Genres2[is.na(df_kianatrain$Genres2)] <- df_kianatrain$Genres1
df_kianatrain$Genres3[is.na(df_kianatrain$Genres3)] <- df_kianatrain$Genres1
df_kianatrain$Genres4[is.na(df_kianatrain$Genres4)] <- df_kianatrain$Genres1
df_kianatrain$Directors[is.na(df_kianatrain$Directors)] <- "Kiana"
df_kianatrain$Genres1 <- as.factor(df_kianatrain$Genres1)
df_kianatrain$Genres2 <- as.factor(df_kianatrain$Genres2)
df_kianatrain$Genres3 <- as.factor(df_kianatrain$Genres3)
df_kianatrain$Genres4 <- as.factor(df_kianatrain$Genres4)
#df_kianatrain$Directors <- as.factor(df_kianatrain$Directors)
df_kianatrain$Likability[is.na(df_kianatrain$Likability)] <- 50
df_kianatrain$Duration[is.na(df_kianatrain$Duration)] <- 100
colSums(is.na(df_kianatrain))
#I hace decided to solve the problem only based on the 4 primary genres that are provided.
#####################################################################################
#caret
set.seed(1234)
myControl1 <- trainControl(method = "cv", number = 5)
grid <- expand.grid(alpha = 1, lambda = 0)
modcaret <- train(Likability ~ .-URL-Title, df_kianatrain, method = "glmnet",
family = "multinomial", trControl = myControl1,
tuneGrid = grid, metric = "Accuracy")
modcaret$results$Accuracy

#caret2
set.seed(1234)
trControl <- trainControl(method = "cv", number = 5)
grid <- expand.grid(alpha = 1, lambda = seq(0, 1.5, length = 201))
modcaret2 <- train(Likability ~ .-URL-Title, df_kianatrain, method = "glmnet", tuneGrid = grid, 
             trControl = trControl, metric = "RMSE", preProcess = c("center", "scale"))
par(mar = c(4, 4, 0, 0))
plot(modcaret2)
Beta <- coef(modcaret2$finalModel, 0.75)
R2 <- modcaret2$results$Rsquared[which(grid$lambda == 0.75)]
1 - (1 - R2) * (nrow(df_kianatrain) - 1)/(nrow(df_kianatrain) - sum(Beta != 0) - 1)


#glmnet:
library(glmnet)
x <- as.matrix(select(df_kianatrain, -URL-Title))
y <- df_kianatrain$Likability
mod_glmnet <- glmnet(x, y, family = "multinomial", lambda = 0)
pred_glmnet <- predict(mod_glmnet, x, type = "response")
head(pred_glmnet[, , 1])
predglmnet <- predict(mod_glmnet, x, type = "class")
table(predglmnet)
confusionMatrix(predglmnet, df_kianatrain$Likability)



#tree based
myControl2 <- trainControl(method = "cv", number = 5, classProbs = TRUE)
grid <- expand.grid(cp = c(0, 0.1, 0.3, 0.6))
modrpart <- train(Likability ~ .-URL-Title, df_kianatrain, method = "rpart", trControl = myControl2, 
    tuneGrid = grid, metric = "Accuracy")
modrpart$results

#grid search
set.seed(1234)
k = 5
folds <- createFolds(df_kianatrain$Likability, k, list = FALSE)
mygrid <- expand.grid(minsplit = c(1, 2, 5, 10, 20), maxdepth = c(1, 2, 3, 4, 5), 
cp = c(0, 0.01, 0.1, 0.3, 0.5, 1))
mod_accuracy <- rep(0, nrow(mygrid))
for (i in 1:nrow(mygrid)) {
    acc <- rep(0, k)
    for (j in 1:k) {
        kiana_train <- df_kianatrain[folds != j, ]
        kiana_test <- df_kianatrain[folds == j, ]
        mod <- rpart(Likability ~ .-URL-Title, df_kianatrain, 
        control = list(minsplit = mygrid$minsplit[i], 
            maxdepth = mygrid$maxdepth[i], cp = mygrid$cp[i]))
        pred <- predict(mod, kiana_test, type = "class")
        cm <- confusionMatrix(pred, kiana_test$Likability)
        acc[j] <- cm$overall["Accuracy"]
    }
    mod_accuracy[i] <- mean(acc)
    print(paste0("Iteration ", i, " out of ", nrow(mygrid), " completed."))
    flush.console()
}
bst.mod <- which.max(mod_accuracy)
mygrid[bst.mod, ]
mod_accuracy[bst.mod]

#ranger
modranger <- ranger(Likability ~ .-URL-Title, df_kianatrain)
modranger
modranger$confusion.matrix
#####################################################################################
#Testing the model on the big set of all the movies:
