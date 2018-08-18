# Sankalp PAthak Roll No. RN-DDA1720076
# Beer Recommendations Assignment
#Required Libraries
library(dplyr)
library(recommenderlab)
library(ggplot2)

# Data loading
setwd("F:/BA/IIIT Upgrad/Domian Elective - E commerse/Assignment")
list.files(pattern = "csv")

beer<-read.csv("beer_data.csv", header = T, na.strings = c("", "NA"))
View(head(beer))
colnames(beer)
# [1] "beer_beerid"        "review_profilename" "review_overall"
nrow(beer) # 475984

#Data quality check
sum(is.na(beer)) # 0

# Empty userid or beerid 

sum(beer$beer_beerid == 0 | is.na(beer$beer_beerid) ) # 0
sum(beer$review_profilename == 0 | is.na(beer$review_profilename) ) # 100

# Removing empty user ids 

beer<-dplyr:: filter(beer, (!is.na(beer$review_profilename )))

# Out of range review as in description it is mentioned that review ranges between 1-5.

sum(beer$review_overall < 1 | beer$review_overall > 5) # 6
dplyr:: filter(beer, ((beer$review_overall < 1 | beer$review_overall > 5)))
#     beer_beerid review_profilename review_overall
# 1        3806           beernut7              0
# 2        3804           beernut7              0
# 3        3810           beernut7              0
# 4        3787           beernut7              0
# 5        3822           beernut7              0
# 6        3786           beernut7              0

# Removing zero values from review 

beer<-dplyr:: filter(beer, ((beer$review_overall  > 0 )))
nrow(beer) # 475878

#Rectifying issues with lower case upper case if any 
beer$review_profilename<-tolower(beer$review_profilename)

# duplicate values

sum(duplicated(beer)) # 580

# As only 580 entries are duplicate i.e same user reviewed same beer
# and gave same ratings. 

sum(duplicated(beer[,c('beer_beerid', 'review_profilename')])) # 1422

sum(duplicated(beer[,c('beer_beerid', 'review_profilename')]))-sum(duplicated(beer)) #842

(sum(duplicated(beer[,c('beer_beerid', 'review_profilename')]))/ nrow(beer))*100 # 0.29%

# For 842 times, user has reviewed same beer but give different ratings. 
# However number is very small as compared to total observations. 
# While preparing the final matrix, these duplicated review of same beer by user
# may create problem (like aggregating the review leading to averaged values
# like 3.125 rather than 1, 1.5, 2..) it is decied to remove those observations. 

beer_cln<- beer[!duplicated(beer[,c('beer_beerid', 'review_profilename')]),]

nrow(beer_cln)-nrow(beer) #-1422
View(head(beer_cln))

# Unique values for each attribute
length(unique(beer_cln$review_profilename)) # 22497

length(unique(beer_cln$beer_beerid)) # 40302

length(unique(beer_cln$review_overall)) # 9

ratings<-c(unique(beer_cln$review_overall))

# What are the unique values of ratings?
ratings
#  3.0 4.0 3.5 4.5 2.5 5.0 2.0 1.0 1.5

#	Visualise the rating values and notice:
boxplot( beer_cln$review_overall)

hist(beer_cln$review_overall, xlab= "Overall Review", 
     main = paste("Histogram of" , "Overall Review"),
     ylab = "count", col = 1, probability = T )
#lines(density(beer_cln$review_overall))


# Most of the user give 3.5 or more ratings making it left skewed plot. 
# The average beer ratings
mean(beer_cln$review_overall) # [1] 3.814
quantile(beer_cln$review_overall)

# 0%  25%  50%  75% 100% 
# 1.0  3.5  4.0  4.5  5.0 

#	The average user ratings

#	The average number of ratings given to the beers 
beer_avr_rating<- beer_cln %>% 
  dplyr :: group_by(beer_beerid) %>% 
  dplyr::summarize(Mean = mean(review_overall))

# Rating distribution
plot(beer_avr_rating)

#	The average number of ratings given by the users

user_avr_rating<- beer_cln %>% 
  dplyr :: group_by(review_profilename) %>% 
  dplyr::summarize(MeanU = mean(review_overall))

# Rating distribution 
plot(beer_avr_rating)

# Removing unecessary objects 
rm(beer, temp, user_avr_rating, ratings)

###EDA###
#1. To identify minimum number of review

# aggregating on number of review

beer_cln_agg<- beer_cln %>% group_by(beer_beerid) %>% summarize (n())
colnames(beer_cln_agg)<- c("beer_beerid", "Review_Count")
# beer_cln_agg$BeerId <- as.factor(beer_cln_agg$BeerId)
summary(beer_cln_agg)


# It is observed that most of the beer id have more than 5 reviews.
# Lets plot the data for better estimate. 

# Plots to get an estimate of minimum number of review required

beer_cln_agg_plot <- ggplot(beer_cln_agg, aes(sample = Review_Count))
beer_cln_agg_plot + stat_qq() # QQ Plot
# As can be seen from plot, number of review surges at around 30-100 reviews. 
# Lets dig further. 

# As most of the review count lies in last quartile, we will take closed look at it.

quantile(beer_cln_agg$Review_Count, probs = seq(0.75, 1, 0.005))

#75%   75.5%     76%   76.5%     77%   77.5%     78%   78.5%     79%   79.5%     80%   80.5% 
#  5.000   5.000   6.000   6.000   6.000   6.000   6.000   7.000   7.000   7.000   7.000   8.000 
#81%   81.5%     82%   82.5%     83%   83.5%     84%   84.5%     85%   85.5%     86%   86.5% 
#  8.000   8.000   9.000   9.000  10.000  10.000  11.000  11.000  12.000  12.000  13.000  14.000 
#87%   87.5%     88%   88.5%     89%   89.5%     90%   90.5%     91%   91.5%     92%   92.5% 
#  15.000  15.000  16.000  17.000  19.000  20.000  21.000  23.000  24.000  26.000  28.000  31.000 
#93%   93.5%     94%   94.5%     95%   95.5%     96%   96.5%     97%   97.5%     98%   98.5% 
#  34.000  37.000  41.000  45.445  51.000  57.000  65.000  75.000  86.000 100.000 121.000 149.000 
# 99%   99.5%    100% 
# 195.000 291.495 977.000

# Finding out relation between number of reviews and average review. 

cor(beer_cln_agg$Review_Count, beer_avr_rating$Mean)

plot( beer_avr_rating$Mean, beer_cln_agg$Review_Count)

axis(side=2, at=seq(0, 1000, by=50))
# 0.06279793
# Almost no correlation between number of reviews and average rating. 

# After viewing QQ plot, scatter plot and detailed observation of quantile, I decied minimum number
# reviews a 25. 
N <- 25

# Selecting beer ID with minimum number of review as N. 
beer_cln_N <- dplyr::filter(beer_cln_agg, beer_cln_agg$Review_Count > N)
# 6538 obs
colnames(beer_cln_agg)<- c("beer_beerid", "Review_Count")

# Removing unnecessary objects
rm(beer_avr_rating, beer_cln_agg, beer_cln_agg_plot)

# Preparing pre final dataframe for with minimum number of review per beer

beer_temp<- semi_join(beer_cln, beer_cln_N)

# Caclulating minimum number of review per user
beer_user_agg<- beer_temp %>% group_by(review_profilename) %>% summarize (n())
colnames(beer_user_agg)<- c("review_profilename", "Review_Count")
summary(beer_user_agg)

# Selecting the minimum number of review per user
boxplot(beer_user_agg$Review_Count)

# Selecting M - Minimum number of review per user

beer_user_N <- dplyr::filter(beer_user_agg, beer_user_agg$Review_Count > N)
beer_final <- semi_join(beer_temp, beer_user_N)

beer_user_N_agg<- beer_final_temp %>% group_by(review_profilename) %>% summarize (n())
summary(beer_user_M_agg)
min(beer_user_N_agg$`n()`)


# Cleaning memory

rm(beer_cln, beer_cln_N)


#############################################################################

# Converting to realmatrix

beer_final <- as.data.frame(beer_final)
# beer_final$beer_beerid <- as.factor(beer_final$beer_beerid)
# beer_final$review_profilename <- as.factor(beer_final$review_profilename)
# beer_final$review_overall <- as.numeric(beer_final$review_overall)
colnames(beer_final)
# [1] "beer_beerid"        "review_profilename" "review_overall" 

# Reordering columns 

beer_final<- beer_final[c("review_profilename", "beer_beerid", "review_overall")]
summary(beer_final)


beer_rating_matrix <- as(beer_final, "realRatingMatrix") 

# Reviewing the matrix
dimnames(beer_rating_matrix)

rowCounts(beer_rating_matrix)

colCounts(beer_rating_matrix)

rowMeans(beer_rating_matrix)
#--------------------------Understand users and ratings----------#

# Visualizing ratings
library(ggplot2)
qplot(getRatings(beer_rating_matrix), binwidth = 1, 
      main = "Histogram of ratings", xlab = "Rating")

summary(getRatings(beer_rating_matrix)) # Skewed to the right

qplot(getRatings(normalize(beer_rating_matrix, method = "Z-score")),
      main = "Histogram of normalized ratings", xlab = "Rating") 

summary(getRatings(normalize(beer_rating_matrix, method = "Z-score"))) # seems better


qplot(rowCounts(beer_rating_matrix), binwidth = 10, 
      main = "Beer Rated on average", 
      xlab = "# of users", 
      ylab = "# of beers rated")
#Most users rate less number of beers.
#Very few users have rated many beers 


# Similarity matrix for first 10 users

similar_users <- similarity(beer_rating_matrix[1:10, ],
                            method = "cosine",
                            which = "users")

as.matrix(similar_users)

#Visualise similarity matrix
image(as.matrix(similar_users), main = "User similarity")

#Inferecne : NA values in similarity for users. 


#How similar are the first 10items are with each other

similar_items <- similarity(beer_rating_matrix[,1:10 ],
                            method = "cosine",
                            which = "items")
as.matrix(similar_items)

image(as.matrix(similar_items), main = "Item similarity")

# Inference : Not may users among first 10 are similar to each other. 

### Recommender model development

# Defining evaluation schemes : 
# 1. Split
# As minimum number of review is 25, given : 20 ~80% of minimum

scheme_split <- evaluationScheme(beer_rating_matrix, method = "split", train = .9,
                               k = 1, given = 20, goodRating = 4)


# 2. Cross validation

scheme_cv <- evaluationScheme(beer_rating_matrix, method = "cross-validation" , train = .9,
                              k = 5 , given = 20, goodRating = 4)

# Models - Split

# creation of recommender model based on ubcf

Rec_ubcf_split <- Recommender(getData(scheme_split, "train"), "UBCF")

# creation of recommender model based on ibcf for comparison

Rec_ibcf_split <- Recommender(getData(scheme_split, "train"), "IBCF")

# Models - CV

# creation of recommender model based on ubcf

Rec_ubcf_cv <- Recommender(getData(scheme_cv, "train"), "UBCF")

# creation of recommender model based on ibcf for comparison

Rec_ibcf_cv <- Recommender(getData(scheme_cv, "train"), "IBCF")

#Comparision
# making predictions on the test data set
# UBCF - Split 

p_ubcf_split <- predict(Rec_ubcf_split, getData(scheme_split, "known"), type="ratings")

# IBCF - split 

p_ibcf_split <- predict(Rec_ibcf_split, getData(scheme_split, "known"), type="ratings")

# UBCF - cv

p_ubcf_cv <- predict(Rec_ubcf_cv, getData(scheme_cv, "known"), type="ratings")

# IBCF - split 

p_ibcf_cv <- predict(Rec_ibcf_cv, getData(scheme_cv, "known"), type="ratings")


# obtaining the error metrics for all four approaches and comparing them
error_ubcf_split <-calcPredictionAccuracy(p_ubcf_split, getData(scheme_split, "unknown"))

error_ibcf_split <-calcPredictionAccuracy(p_ibcf_split, getData(scheme_split, "unknown"))

error_ubcf_cv <-calcPredictionAccuracy(p_ubcf_cv, getData(scheme_cv, "unknown"))

error_ibcf_cv <-calcPredictionAccuracy(p_ibcf_cv, getData(scheme_cv, "unknown"))


error <- rbind(error_ubcf_split, error_ibcf_split, error_ubcf_cv, error_ibcf_cv)
rownames(error) <- c("UBCF-SPLIT","IBCF- SPLIT", "UBCF-CV", "IBCF-CV")
min(error)

#Selected model is Rec_ibcf_cv


# Prediction for customer 
Rec_beer <- function(customer_key) {
 R <- predict(Rec_ibcf_cv, beer_final[which(review_profilename== customer_key), ], n=10)
 return(as (R, "list"))
}

# Recomendations for user : "cokes", "genog" & "giblet"
Rec_beer("cokes")
Rec_beer("genog")
Rec_beer("giblet")


####################################END OF RECOMMEDNER ENGINE##################








