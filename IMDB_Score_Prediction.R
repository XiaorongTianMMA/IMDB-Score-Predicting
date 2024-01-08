library(boot)

imdb = read.csv("/Users/sissi.w./Desktop/McGill/Courses/MGSC661/Group/training_1030.csv")
attach(imdb)
# View(imdb)

test = read.csv("/Users/sissi.w./Desktop/McGill/Courses/MGSC661/Group/test_1030.csv")
# View(test)


#### Outliers ####

Q1 <- quantile(imdb$nb_news_articles_log, 0.25) 
Q3 <- quantile(imdb$nb_news_articles_log, 0.75) 
# Calculating the IQR 
IQR_value <- IQR(imdb$nb_news_articles_log) 
# Identifying the rows where the value column is an outlier 
outlier_rows <- which(imdb$nb_news_articles_log < (Q1 - 1.5*IQR_value) | imdb$nb_news_articles_log > (Q3 + 1.5*IQR_value)) 
# Printing the rows with outliers 
print(outlier_rows)

Q1 <- quantile(imdb$movie_meter_IMDBpro_norm, 0.25) 
Q3 <- quantile(imdb$movie_meter_IMDBpro_norm, 0.75) 
# Calculating the IQR 
IQR_value <- IQR(imdb$movie_meter_IMDBpro_norm) 
# Identifying the rows where the value column is an outlier 
outlier_rows2 <- which(imdb$movie_meter_IMDBpro_norm < (Q1 - 1.5*IQR_value) | imdb$movie_meter_IMDBpro_norm > (Q3 + 1.5*IQR_value)) 
# Printing the rows with outliers 
print(outlier_rows2)

new_imdb=imdb[-c(817,722,597,545,1112),]
new_imdb = new_imdb[-outlier_rows2,]
new_imdb = new_imdb[-outlier_rows,]


#### Model ####

fit = glm(imdb_score~
            nb_news_articles_log+
            movie_meter_IMDBpro_norm+
            action+
            thriller+
            musical+
            horror+
            drama+
            animation+
            crime+
            comedy+
            documentary+
            biography+
            animation_universal+
            avg_distributor_imdb_score+
            country_dummy_USA+
            duration_0_100+
            duration_100_150+
            disney_animation+
            academy_months+
            poly(director_film_running_avaerge, 2)+
            cinematographer_film_running_avaerge+
            actor1_film_running_avaerge+
            actor2_film_running_avaerge+
            actor3_film_running_avaerge+
            maturity_rating_G+
            maturity_rating_PG+
            director_max, data=new_imdb)

mse=cv.glm(new_imdb, fit, K=10)$delta[1]
mse

summary(fit)

r_squared <- 1 - sum(fit$residuals^2) / sum((new_imdb$imdb_score - mean(new_imdb$imdb_score))^2)
print(r_squared)


#### Predict on test dataset
predict(fit,test)

predictions <- predict(fit, test)
results <- data.frame(Movie_Title = test$movie_title, Predicted_Score = predictions)
print(results)

# library(stargazer)
# stargazer(results, type="html", out="results.html", title="Predicted Scores of Movies", digits=5, rownames=FALSE)



