#############################################################
# Create edx set, validation set, and submission file
#############################################################
## Section provided by course
# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")


# Validation set will be 10% of MovieLens data

set.seed(1)
index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-index,]
temp <- movielens[index,]

# Make sure userId and movieId in validation set are also in edx set

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)
############################################################################################################
############################################################################################################

# Separates edx set into train and test sets

set.seed(1)
edx_test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
edx_train <- edx[-edx_test_index,]
edx_test <- edx[edx_test_index,]


#Confirm movieId and userId in edx_test are also in edx_trian, then add removed movieId to edx_train

edx_test <- edx_test %>% 
  semi_join(edx_train, by = "movieId") %>%
  semi_join(edx_train, by = "userId")

removed <- edx_test %>% 
  anti_join(edx_train, by = "movieId")

edx_train <- rbind(edx_train, removed)


######################################################################################################################################
########### Data exploration
############################################################################################################################################33

# Dataset structure:
str(edx)
head(edx)

#Check missing values
summary(edx)

# NUmber of star ratings
edx %>% count(rating) %>% arrange(desc(n))

# Unique user and movie count
edx %>%
  summarise(n_users = n_distinct(userId), n_movies = n_distinct(movieId)) %>%
  print.data.frame()

# Ratings Hsstogram:
edx %>%
  ggplot(aes(rating)) +
  geom_histogram(binwidth = 0.5, color = "black", fill = "blue") +
  scale_x_discrete(limits = c(seq(0.5,5,0.5))) +
  scale_y_continuous(breaks = c(seq(0, 3000000, 500000))) +
  ggtitle("Rating distribution")

# Mean rating (users wminimum 100 ratings):
edx %>% 
  group_by(userId) %>% 
  filter(n() >= 100) %>%
  summarise(mean_rating = mean(rating)) %>% 
  ggplot(aes(mean_rating)) + 
  geom_histogram(bins = 30, color = "black", fill = "blue") +
  xlab("Mean movie rating") +
  ylab("Number of users") +
  ggtitle("Mean movie rating per user") +
  scale_x_discrete(limits = c(seq(0.5,5,0.5)))

# Large variations seen in  number of movies rated by user:
edx %>% count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black", fill = "blue") + 
  scale_x_log10() + 
  xlab("Number of ratings") +
  ylab("Number of users") +
  ggtitle("Number of ratings per user")

# Movies with 1 rating
edx %>% 
  group_by(movieId) %>%
  summarise(count = n()) %>% 
  filter(count == 1) %>% count(count) 

# Ratings per movie
edx %>% 
  count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black", fill = "blue") + 
  xlab("Number of ratings") +
  ylab("Movie count") +
  scale_x_log10() + 
  ggtitle("Number of ratings per movie")

#####################################################################################################################################################
############Models
##################################################################################################################################################
RMSE <- function(valid_set, pred_set){
  sqrt(mean((valid_set - pred_set)^2))
}

########################Average
###############################
mu <- mean(edx_train$rating)
naive_rmse <- RMSE(edx_test$rating, mu)
predictions <- rep(2.5, nrow(edx_test))
RMSE(edx_test$rating, predictions)

rmse_results <- data_frame(method = "Average", RMSE = naive_rmse)
#fit <- lm(rating ~ as.factor(userId), data = movielens)


#################################Movie Effect
##########################################
mu <- mean(edx_train$rating)
movie_avgs <- edx_train %>%
  group_by(movieId) %>%
  summarize(bi = mean(rating - mu))

predicted_ratings <- mu + edx_test %>%
  left_join(movie_avgs, by = 'movieId') %>%
  .$bi
model1_rmse <- RMSE(predicted_ratings, edx_test$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method = "Movie Effect Model",
                                     RMSE = model1_rmse))
rmse_results %>% knitr::kable()

#lm(rating ~ as.factor(movieId) + as.factor(userId))

####################################Movie and User Effect
#########################################################
user_avgs <- edx_train %>%
  left_join(movie_avgs, by = 'movieId') %>%
  group_by(userId) %>%
  summarize(bu = mean(rating - mu - bi))

predicted_ratings <- edx_test %>%
  left_join(movie_avgs, by = 'movieId') %>%
  left_join(user_avgs, by = 'userId') %>%
  mutate(pred = mu + bi + bu) %>%
  .$pred
model2_rmse <- RMSE(predicted_ratings, edx_test$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method = "Movie + User Effects Model",
                                     RMSE = model2_rmse))
rmse_results %>% knitr::kable()


####################################Movie + User + Genre Effect
#########################################################
gen_avgs <- edx_train %>%
  left_join(movie_avgs, by = 'movieId') %>%
  left_join(user_avgs, by = 'userId') %>%
  group_by(genres) %>%
  summarize(bg = mean(rating - mu - bi - bu))

predicted_ratings <- edx_test %>%
  left_join(movie_avgs, by = 'movieId') %>%
  left_join(user_avgs, by = 'userId') %>%
  left_join(gen_avgs, by = 'genres') %>%
  mutate(pred = mu + bi + bu + bg) %>%
  .$pred
model22_rmse <- RMSE(predicted_ratings, edx_test$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method = "Movie + User + Genre Effects Model",
                                     RMSE = model22_rmse))
rmse_results %>% knitr::kable()

########################################## Regularized Movie and User
#############################################################################

lambdas <- seq(0, 10, 0.25)

rmses <- sapply(lambdas, function(l){
  mu <- mean(edx_train$rating)
  bi <- edx_train %>% 
    group_by(movieId) %>%
    summarise(bi = sum(rating - mu)/(n()+l))
    
  bu <- edx_train %>% 
    left_join(bi, by="movieId") %>%
    group_by(userId) %>%
    summarise(bu = sum(rating - bi - mu)/(n()+l))
  
  predicted_ratings <- 
    edx_test %>% 
    left_join(bi, by = "movieId") %>%
    left_join(bu, by = "userId") %>%
    mutate(pred = mu + bi + bu) %>%
    .$pred
  
  return(RMSE(predicted_ratings, edx_test$rating))
})

qplot(lambdas, rmses) 

# Lambda with lowest RMSE
lambda <- lambdas[which.min(rmses)]
lambda
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie + User Effect Model - test dataset",  
                                     RMSE = min(rmses)))
rmse_results %>% knitr::kable()

##################################################################################################################################################
###############  Model Against the Validation Set
##################################################################################################################################################

# Calculate movie effect with lambda penalty
b_i <- edx %>% group_by(movieId) %>% summarise(b_i = sum(rating - mu)/(n()+lambda))

# Calculate user effect with lambda penalty
b_u <- edx %>% left_join(b_i, by="movieId") %>% group_by(userId) %>% summarise(b_u = sum(rating - b_i - mu)/(n()+lambda))

#Validation set prediction ratings
predicted_ratings <- validation %>% left_join(b_i, by = "movieId") %>% left_join(b_u, by = "userId") %>% mutate(pred = mu + b_i + b_u) %>% .$pred

# RMSE
validation_rmse <- RMSE(predicted_ratings, validation$rating)

# Append the results
rmse_results <- bind_rows(rmse_results,data_frame(method = "Regularized Movie + User Effect Model - Validation dataset",RMSE = validation_rmse))
rmse_results %>% knitr::kable()

