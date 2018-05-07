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
#####################################################################################
#check for missing packages and install them:
list.of.packages <- c("knitr", "httr", "readr", "dplyr", "tidyr", "XML",
                      "ggplot2", "stringr", "lubridate", "grid")
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
#cleaning the datasets:
names(df_movies)
names(df_kianamovies)
allmovies <- df_movies %>%
  select(movie_title, movie_imdb_link, imdb_score, genres, duration, 
         color, content_rating, country, language, director_name, 
         plot_keywords, title_year, duration, num_voted_users)
#renaming the columns for convenience:
names(df_kianamovies)[names(df_kianamovies) == 'IMDb Rating'] <- 'Score'
names(df_kianamovies)[names(df_kianamovies) == 'Runtime (mins)'] <- 'duration'
names(df_kianamovies)[names(df_kianamovies) == 'Your Rating'] <- 'KianaRating'
names(df_kianamovies)[names(df_kianamovies) == 'Title Type'] <- 'Type'
names(df_kianamovies)[names(df_kianamovies) == 'Num Votes'] <- 'Num_Votes'

names(allmovies)[names(allmovies) == 'movie_title'] <- 'Title'
names(allmovies)[names(allmovies) == 'movie_imdb_link'] <- 'URL'
names(allmovies)[names(allmovies) == 'num_voted_users'] <- 'Num_Votes'
names(allmovies)[names(allmovies) == 'movie_title'] <- 'Title'
names(allmovies)[names(allmovies) == 'director_name'] <- 'Directors'
#####################################################################################
#examining user's watched movies to see a pattern:
#I am definig a factor as Likability to calculate a score for each movie,
#Likability goes from 0 to 100 which is the probabilty of me liking a movie
df_kiana <- df_kianamovies %>%
  select(Title, URL, Type, Score, duration, Year, Genres, Num_Votes
         , Directors, KianaRating) %>%
  mutate(Likability = KianaRating*10)
#####################################################################################
ggplot(data=df_kiana, aes(x = Year, fill = Likability)) +
  geom_bar(position = "stack") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
