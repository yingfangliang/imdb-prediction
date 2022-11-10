# Load Required libraries
load.libraries <- c('gridExtra', 'corrplot', 'GGally', 'ggplot2', 'dplyr','car','boot','plm','lmtest','ggpubr', 'methods', 'splines','stargazer', 'caTools')
install.lib <- load.libraries[!load.libraries %in% installed.packages()]
for(libs in install.lib) install.packages(libs, dependences = TRUE)
sapply(load.libraries, require, character = TRUE)

imdb = read.csv("IMDB_data.csv")
head(imdb)

# Confirm missing values - There are no missing values in the data
colSums(is.na(imdb))

# Separate categorical and numerical predictors into separate datasets
cat_var <- names(imdb)[which(sapply(imdb, is.character))]
numeric_var <- names(imdb)[which(sapply(imdb, is.numeric))]

imdb_cont <- imdb[numeric_var]
imdb_cat <- imdb[cat_var]

# DATASET PREPROCESSING ---------------------------------------
# Dropping columns with high cardinality or no patterns
newdf = subset(imdb, select = c('imdbScore',
                                'movieBudget',
                                'duration',
                                'maturityRating',
                                'nbNewsArticles',
                                'director',
                                'actor1_starMeter',
                                'actor2_starMeter',
                                'actor3_starMeter',
                                'nbFaces',
                                'action',
                                'adventure',
                                'scifi',
                                'thriller',
                                'musical',
                                'romance',
                                'western',
                                'sport',
                                'horror',
                                'drama',
                                'war',
                                'animation',
                                'crime',
                                'movieMeter_IMDBpro',
                                'colourFilm'
))

# Calculate avg rating for each director
a = newdf %>%
  group_by(director) %>%
  summarise(dirimdbrating = mean(imdbScore))

# Join df with original to assign avg rating
imdbdf = inner_join(newdf, a, by = 'director')

# Calculate rank and binning based on rank and filtering down to COLOUR movies only
imdbdf = imdbdf %>% mutate(directorRank = dense_rank(desc(dirimdbrating)))  %>%
  mutate(ranking_bin = ntile(directorRank, n=10)) %>%
  filter(colourFilm =='Color')

# Save bins assigned to directors for test set transformation
dirbins = imdbdf %>% select(director, ranking_bin) %>% 
  group_by(director) %>%
  summarise(ranking_bin = max(ranking_bin))
attach(imdbdf)

# Convert star meter to avg of all 3
avgStarMeter <- (actor1_starMeter+actor2_starMeter+actor3_starMeter)/3
imdbdf$avgStarMeter <- avgStarMeter 

LogavgStarMeter <- log(avgStarMeter)
imdbdf$LogavgStarMeter <- LogavgStarMeter 

imdbdf$maturityRating

# Bucket maturity rating into required buckets
levels(imdbdf$maturityRating) <- c(levels(imdbdf$maturityRating), "Others")
imdbdf$maturityRating[(imdbdf$maturityRating != 'R')&(imdbdf$maturityRating != 'PG-13')&(imdbdf$maturityRating != 'PG')] <- 'Others' 

# Drop columns no longer required
imdbdf <- subset(imdbdf, select = -c(director, dirimdbrating, directorRank,colourFilm, actor1_starMeter,  actor2_starMeter, actor3_starMeter,LogavgStarMeter))

# FIND IDEAL DEGREES FOR EACH PREDICTOR ---------------------------------------
# 1. Poly

# movieBudget variable 
regmB = lm(imdbScore~movieBudget, data = imdbdf)
regmB2 = lm(imdbScore~poly(movieBudget,2), data = imdbdf)
regmB3 = lm(imdbScore~poly(movieBudget,3), data = imdbdf)
regmB4 = lm(imdbScore~poly(movieBudget,4), data = imdbdf)
regmB5 = lm(imdbScore~poly(movieBudget,5), data = imdbdf)
anova(regmB, regmB2, regmB3, regmB4, regmB5) 
# d = 1 
# stargazer(regmB, regmB2, regmB3, regmB4, regmB5, type = 'html')

# duration variable 
regDuration = lm(imdbScore~duration, data = imdbdf)
regDuration2 = lm(imdbScore~poly(duration,2), data = imdbdf)
regDuration3 = lm(imdbScore~poly(duration,3), data = imdbdf)
regDuration4 = lm(imdbScore~poly(duration,4), data = imdbdf)
regDuration5 = lm(imdbScore~poly(duration,5), data = imdbdf)
anova(regDuration, regDuration2, regDuration3, regDuration4, regDuration5) 
# d = 2
# stargazer(regDuration, regDuration2, regDuration3, regDuration4, regDuration5, type = 'html')

# nbNewsArticles variable
regNews1 = lm(imdbScore~nbNewsArticles, data = imdbdf)
regNews2 = lm(imdbScore~poly(nbNewsArticles,2), data = imdbdf)
regNews3  = lm(imdbScore~poly(nbNewsArticles,3), data = imdbdf)
regNews4  = lm(imdbScore~poly(nbNewsArticles,4), data = imdbdf)
regNews5 = lm(imdbScore~poly(nbNewsArticles,5), data = imdbdf) 
regNews6 = lm(imdbScore~poly(nbNewsArticles,6), data = imdbdf) 
anova(regNews1, regNews2, regNews3, regNews4, regNews5, regNews6) 
# d = 5 
# stargazer(regNews1, regNews2, regNews3, regNews4, regNews5, regNews6, type = 'html')

# avgStarMeter variable
regst1 = lm(imdbScore~avgStarMeter, data = imdbdf)
regst2 = lm(imdbScore~poly(avgStarMeter,2), data = imdbdf)
regst3  = lm(imdbScore~poly(avgStarMeter,3), data = imdbdf)
regst4  = lm(imdbScore~poly(avgStarMeter,4), data = imdbdf)
regst5 = lm(imdbScore~poly(avgStarMeter,5), data = imdbdf) 
regst6 = lm(imdbScore~poly(avgStarMeter,6), data = imdbdf) 
anova(regst1, regst2, regst3, regst4, regst5, regst6)  
# d = 2 
# stargazer(regst1, regst2, regst3, regst4, regst5, regst6, type = 'html')

# movieMeter_IMDBpro variable
regMeterPro1 = lm(imdbScore~movieMeter_IMDBpro, data = imdbdf )
regMeterPro2 = lm(imdbScore~poly(movieMeter_IMDBpro ,2), data = imdbdf)
regMeterPro3  = lm(imdbScore~poly(movieMeter_IMDBpro ,3), data = imdbdf)
regMeterPro4  = lm(imdbScore~poly(movieMeter_IMDBpro ,4), data = imdbdf)
regMeterPro5 = lm(imdbScore~poly(movieMeter_IMDBpro ,5), data = imdbdf) 
regMeterPro6 = lm(imdbScore~poly(movieMeter_IMDBpro ,6), data = imdbdf) 
anova(regMeterPro1, regMeterPro2, regMeterPro3, regMeterPro4, regMeterPro5, regMeterPro6) # d = 3 
# stargazer(regMeterPro1, regMeterPro2, regMeterPro3, regMeterPro4, type = 'html')

# Multiple Polynomial Regression 
regPoly = lm(imdbScore~nbFaces+movieBudget+poly(duration, 2)+poly(nbNewsArticles, 3)+avgStarMeter+
               poly(movieMeter_IMDBpro ,3), data = imdbdf)  
summary(regPoly)

# 2. Spline

# movieBudget variable
# Create 5 knots and divide data into 6 equally spaced  regions 
k1 = quantile(movieBudget, 1/6, data = imdbdf) 
k2 = quantile(movieBudget, 2/6, data = imdbdf) 
k3 = quantile(movieBudget, 3/6, data = imdbdf) 
k4 = quantile(movieBudget, 4/6, data = imdbdf) 
k5 = quantile(movieBudget, 5/6, data = imdbdf)

# Run the regression for movieBudget 
reg_mBS1  = lm(imdbScore~bs(movieBudget, knots = c(k1, k2, k3, k4, k5), degree = 1), data = imdbdf) # linear spline
reg_mBS2  = lm(imdbScore~bs(movieBudget, knots = c(k1, k2, k3, k4, k5), degree = 2), data = imdbdf) # quadratic spline
reg_mBS3  = lm(imdbScore~bs(movieBudget, knots = c(k1, k2, k3, k4, k5), degree = 3), data = imdbdf) # cubic spline
reg_mBS4  = lm(imdbScore~bs(movieBudget, knots = c(k1, k2, k3, k4, k5), degree = 4), data = imdbdf) # quartic spline
reg_mBS5  = lm(imdbScore~bs(movieBudget, knots = c(k1, k2, k3, k4, k5), degree = 5), data = imdbdf) # quintic poly spline
# stargazer(reg_mBS1, reg_mBS2, reg_mBS3, reg_mBS4, reg_mBS5, type = 'html')

# duration variable
# Create 5 knots and divide data into 6 equally spaced  regions 
k1 = quantile(duration, 1/6) 
k2 = quantile(duration, 2/6) 
k3 = quantile(duration, 3/6) 
k4 = quantile(duration, 4/6) 
k5 = quantile(duration, 5/6)

# Run the regression for duration
reg_ds1  = lm(imdbScore~bs(duration, knots = c(k1, k2, k3, k4, k5), degree = 1)) # linear spline
reg_ds2  = lm(imdbScore~bs(duration, knots = c(k1, k2, k3, k4, k5), degree = 2)) # quadratic spline
reg_ds3  = lm(imdbScore~bs(duration, knots = c(k1, k2, k3, k4, k5), degree = 3)) # cubic spline
reg_ds4  = lm(imdbScore~bs(duration, knots = c(k1, k2, k3, k4, k5), degree = 4)) # quartic spline
reg_ds5 = lm(imdbScore~bs(duration, knots = c(k1, k2, k3, k4, k5), degree = 5)) # quintic poly spline
stargazer(reg_ds1, reg_ds2, reg_ds3, reg_ds4, reg_ds5, type = 'html')

# nbNewsArticles variable
# Create 5 knots and divide data into 6 equally spaced  regions 
k1 = quantile(nbNewsArticles, 1/6) 
k2 = quantile(nbNewsArticles, 2/6) 
k3 = quantile(nbNewsArticles, 3/6) 
k4 = quantile(nbNewsArticles, 4/6) 
k5 = quantile(nbNewsArticles, 5/6)

# Run the regression for nbNewsArticles
reg_NewsS1  = lm(imdbScore~bs(nbNewsArticles, knots = c(k1, k2, k3, k4, k5), degree = 1)) # linear spline
reg_NewsS2  = lm(imdbScore~bs(nbNewsArticles, knots = c(k1, k2, k3, k4, k5), degree = 2)) # quadratic spline
reg_NewsS3  = lm(imdbScore~bs(nbNewsArticles, knots = c(k1, k2, k3, k4, k5), degree = 3)) # cubic spline
reg_NewsS4  = lm(imdbScore~bs(nbNewsArticles, knots = c(k1, k2, k3, k4, k5), degree = 4)) # quartic spline
reg_NewsS5  = lm(imdbScore~bs(nbNewsArticles, knots = c(k1, k2, k3, k4, k5), degree = 5)) # quintic poly spline
stargazer(reg_NewsS1, reg_NewsS2, reg_NewsS3, reg_NewsS4, reg_NewsS5, type = 'html')

# movieMeter_IMDBpro variable
# Create 5 knots and divide data into 6 equally spaced  regions 
k1 = quantile(movieMeter_IMDBpro, 1/6) 
k2 = quantile(movieMeter_IMDBpro, 2/6) 
k3 = quantile(movieMeter_IMDBpro, 3/6) 
k4 = quantile(movieMeter_IMDBpro, 4/6) 
k5 = quantile(movieMeter_IMDBpro, 5/6)

# Run the regression for movieMeter_IMDBpro
reg_Mpro1  = lm(imdbScore~bs(movieMeter_IMDBpro, knots = c(k1, k2, k3, k4, k5), degree = 1)) # linear spline
reg_Mpro2  = lm(imdbScore~bs(movieMeter_IMDBpro, knots = c(k1, k2, k3, k4, k5), degree = 2)) # quadratic spline
reg_Mpro3  = lm(imdbScore~bs(movieMeter_IMDBpro, knots = c(k1, k2, k3, k4, k5), degree = 3)) # cubic spline
reg_Mpro4  = lm(imdbScore~bs(movieMeter_IMDBpro, knots = c(k1, k2, k3, k4, k5), degree = 4)) # quartic spline
reg_Mpro5  = lm(imdbScore~bs(movieMeter_IMDBpro, knots = c(k1, k2, k3, k4, k5), degree = 5)) # quintic poly spline
# stargazer(reg_Mpro1, reg_Mpro2, reg_Mpro3, reg_Mpro4, reg_Mpro5, type = 'html')


# movieBudget variable
# Create 2 knots and divide data into 6 equally spaced regions 
k1 = quantile(movieBudget, 1/3) 
k2 = quantile(movieBudget, 2/3) 
reg_mBS1  = lm(imdbScore~bs(movieBudget, knots = c(k1, k2), degree = 1)) # linear spline
reg_mBS2  = lm(imdbScore~bs(movieBudget, knots = c(k1, k2), degree = 2)) # quadratic spline
reg_mBS3  = lm(imdbScore~bs(movieBudget, knots = c(k1, k2), degree = 3)) # cubic spline
reg_mBS4  = lm(imdbScore~bs(movieBudget, knots = c(k1, k2), degree = 4)) # quartic spline
reg_mBS5  = lm(imdbScore~bs(movieBudget, knots = c(k1, k2), degree = 5)) # quintic poly spline
# stargazer(reg_mBS1, reg_mBS2, reg_mBS3, reg_mBS4, reg_mBS5, type = 'html')

# duration variable
# Create 2 knots and divide data into 6 equally spaced regions 
k1 = quantile(duration, 1/3) 
k2 = quantile(duration, 2/3) 
reg_ds1  = lm(imdbScore~bs(duration, knots = c(k1, k2), degree = 1)) # linear spline
reg_ds2  = lm(imdbScore~bs(duration, knots = c(k1, k2), degree = 2)) # quadratic spline
reg_ds3  = lm(imdbScore~bs(duration, knots = c(k1, k2), degree = 3)) # cubic spline
reg_ds4  = lm(imdbScore~bs(duration, knots = c(k1, k2), degree = 4)) # quartic spline
reg_ds5 = lm(imdbScore~bs(duration, knots = c(k1, k2), degree = 5)) # quintic poly spline
# stargazer(reg_ds1, reg_ds2, reg_ds3, reg_ds4, reg_ds5, type = 'html')

# nbNewsArticles variable
# Create 2 knots and divide data into 6 equally spaced regions 
k1 = quantile(nbNewsArticles, 1/3) 
k2 = quantile(nbNewsArticles, 2/3) 
reg_NewsS1  = lm(imdbScore~bs(nbNewsArticles, knots = c(k1, k2), degree = 1)) # linear spline
reg_NewsS2  = lm(imdbScore~bs(nbNewsArticles, knots = c(k1, k2), degree = 2)) # quadratic spline
reg_NewsS3  = lm(imdbScore~bs(nbNewsArticles, knots = c(k1, k2), degree = 3)) # cubic spline
reg_NewsS4  = lm(imdbScore~bs(nbNewsArticles, knots = c(k1, k2), degree = 4)) # quartic spline
reg_NewsS5  = lm(imdbScore~bs(nbNewsArticles, knots = c(k1, k2), degree = 5)) # quintic poly spline
# stargazer(reg_NewsS1, reg_NewsS2, reg_NewsS3, reg_NewsS4, reg_NewsS5, type = 'html')


# movieMeter_IMDBpro variable
# Create 2 knots and divide data into 6 equally spaced regions 
k1 = quantile(movieMeter_IMDBpro, 1/3) 
k2 = quantile(movieMeter_IMDBpro, 2/3) 
reg_Mpro1  = lm(imdbScore~bs(movieMeter_IMDBpro, knots = c(k1, k2), degree = 1)) # linear spline
reg_Mpro2  = lm(imdbScore~bs(movieMeter_IMDBpro, knots = c(k1, k2), degree = 2)) # quadratic spline
reg_Mpro3  = lm(imdbScore~bs(movieMeter_IMDBpro, knots = c(k1, k2), degree = 3)) # cubic spline
reg_Mpro4  = lm(imdbScore~bs(movieMeter_IMDBpro, knots = c(k1, k2), degree = 4)) # quartic spline
reg_Mpro5  = lm(imdbScore~bs(movieMeter_IMDBpro, knots = c(k1, k2), degree = 5)) # quintic poly spline
# stargazer(reg_Mpro1, reg_Mpro2, reg_Mpro3, reg_Mpro4, reg_Mpro5, type = 'html')

# Columns that are Not Factors, taken as numeric predictors - "action","adventure","scifi","thriller","musical","romance","western","sport","horror","drama","war","animation","crime",
# Convert to factors
cat_var <- c("maturityRating","ranking_bin")
for (a in cat_var)
  eval(parse(text=paste("imdbdf$", a, "=as.factor(imdbdf$", a, ")", sep="")))
sapply(imdbdf, is.factor)

# Remove outliers - refer to appendix for additional information
imdbdf = imdbdf[-c(11,183,383,477,1529,954,1748),]

# Save transformed datset
#write.csv(imdbdf,"/Users/utkarshnagpal/Downloads/MMA/MGSC - 661 - Multivariate Statistics/IMDB Team Project/imdbdf.csv", row.names = FALSE)

# Final transformed dataset
attach(imdbdf)


# MODEL BUILDING ---------------------------------------
model1 = glm(imdbScore~ movieBudget +poly(duration, 2) + maturityRating +poly(nbNewsArticles,2) +nbFaces + action + adventure+scifi +thriller + musical + romance + western +sport +horror + drama + war + animation +
              crime +ranking_bin + poly(movieMeter_IMDBpro,2)+avgStarMeter, data=imdbdf)
summary(model1)


model2 = glm(imdbScore~ movieBudget +poly(duration, 3) + maturityRating +poly(nbNewsArticles,3) +nbFaces + action + adventure+scifi +thriller + musical + romance + western +sport +horror + drama + war + animation +
              crime +ranking_bin + movieMeter_IMDBpro+poly(avgStarMeter,2), data=imdbdf)
summary(model2)

model3 = glm(imdbScore~ movieBudget +poly(duration, 2) + maturityRating +poly(nbNewsArticles,4) +nbFaces + action + adventure+scifi +thriller + musical + romance + western +sport +horror + drama + war + animation +
              crime +ranking_bin + movieMeter_IMDBpro+poly(avgStarMeter,3), data=imdbdf)
summary(model3)

model4 = glm(imdbScore~ movieBudget +poly(duration, 2) + maturityRating +poly(nbNewsArticles,4) +nbFaces + action + adventure+scifi +thriller + musical + romance + western +sport +horror + drama + war + animation +
              crime +ranking_bin +poly(avgStarMeter,3), data=imdbdf)
summary(model4)


model5 = glm(imdbScore~ movieBudget +poly(duration, 3) +poly(nbNewsArticles,3) +nbFaces + action + adventure+scifi +thriller + musical + romance + western +sport +horror + drama + war + animation +
              crime +ranking_bin +poly(avgStarMeter,2), data=imdbdf)
summary(model5)

model6 = glm(imdbScore~ movieBudget +poly(duration, 2) +poly(nbNewsArticles,3) +nbFaces + action + adventure+scifi +thriller + musical + romance + western +sport +horror + drama + war + animation +
              crime +ranking_bin +poly(avgStarMeter,2), data=imdbdf)
summary(model6)


model7 = glm(imdbScore~ movieBudget +poly(duration, 2) +poly(nbNewsArticles,3) +nbFaces + action + adventure+scifi +thriller + musical + romance + western +sport +horror + drama + war + animation +
              crime +ranking_bin, data=imdbdf)
summary(model7)

model8 = glm(imdbScore~ movieBudget +poly(duration, 2) +poly(nbNewsArticles,3) +nbFaces + action +scifi +thriller + musical + romance + western +sport +horror + drama + war + animation +
              crime +ranking_bin, data=imdbdf)
summary(model8)

model9 = glm(imdbScore~ movieBudget +poly(duration, 2) +poly(nbNewsArticles,3) +nbFaces + action +scifi +thriller + romance + western +sport +horror + drama + war + animation +
              crime +ranking_bin, data=imdbdf)
summary(model9)


model10 = glm(imdbScore~ movieBudget +poly(duration, 2) +poly(nbNewsArticles,3) +nbFaces + action +scifi +thriller + romance + western +sport +horror + drama + war + animation +
              crime, data=imdbdf)
summary(model10)

model11 = glm(imdbScore~ movieBudget +poly(duration, 2) +poly(nbNewsArticles,3) +nbFaces + western + drama + animation + ranking_bin, data=imdbdf)
summary(model11)


# Remove Heteroskedasticity from selected model
coeftest(model11, vcov=vcovHC(model2, type="HC1"))
summary(model11)


# CROSS-VALIDATION and MODEL SELECTION ---------------------------------------

# Train Test split
sample=sample.split(imdbdf$imdbScore, SplitRatio=0.8)
train_set=subset(imdbdf, sample==TRUE)
test_set=subset(imdbdf, sample==FALSE)

actual=test_set$imdbScore
prediction=predict(model11, test_set)
squared_error=(actual-prediction)^2
MSE=mean(squared_error)
MSE

# LOOCV
mse=cv.glm(imdbdf, model11)$delta[1]
mse

# K-Fold
mse=cv.glm(imdbdf, model11, K=5)$delta[1]
mse

# PREDICTIONS - PROCESSING TEST SET ---------------------------------------
testdata = read.csv("test_data_IMDB.csv")
testdf = subset(testdata, select = c('imdbScore',
                                'movieBudget',
                                'duration',
                                'maturityRating',
                                'nbNewsArticles',
                                'director',
                                'actor1_starMeter',
                                'actor2_starMeter',
                                'actor3_starMeter',
                                'nbFaces',
                                'action',
                                'adventure',
                                'scifi',
                                'thriller',
                                'musical',
                                'romance',
                                'western',
                                'sport',
                                'horror',
                                'drama',
                                'war',
                                'animation',
                                'crime',
                                'movieMeter_IMDBpro',
                                'colourFilm'
))

# Join test set with director ranking bins 
testdf = left_join(testdf, dirbins, by = 'director')
  
# Set default bin to 5  
testimdb = testdf %>% mutate(ranking_bin = ifelse(is.na(ranking_bin), 5, ranking_bin))

# Drop columns no longer required
testimdb <- subset(testimdb, select = -c(director,colourFilm, actor1_starMeter,  actor2_starMeter, actor3_starMeter))
cat_var <- c("maturityRating","ranking_bin")
for (a in cat_var)
  eval(parse(text=paste("testimdb$", a, "=as.factor(testimdb$", a, ")", sep="")))
sapply(testimdb, is.factor)

# ACTUAL PREDICTIONS ---------------------------------------
prediction=predict(model11, testimdb)
print(prediction)

# APPENDIX ---------------------------------------

# 0. Attach original dataset
attach(imdb)

remove = c("movieTitle","movieID","imdbLink","releaseDay","releaseMonth","releaseYear","language","country","distributor","actor1","actor2","actor3","genres","plotKeywords","cinematographer","productionCompany") 
imdb = imdb[!(names(imdb) %in% remove)]

attr <- names(imdb)
attr = attr[-c(1)]
categorical <- c("maturityRating","aspectRatio","director","colourFilm","action","adventure","scifi","thriller","musical","romance","western","sport","horror","drama","war","animation","crime")
numerical <- attr[which(!attr %in% categorical)]

for (a in attr){
  if (a %in% categorical){
    eval(parse(text=paste("imdb$", a, "=as.factor(imdb$", a, ")", sep="")))
  }
}
sapply(imdb, is.factor)

# 1. Check for Non-linearity - only numerical predictors
reg = lm(imdbScore~movieBudget+duration+nbNewsArticles+actor1_starMeter+actor2_starMeter+actor3_starMeter+nbFaces+movieMeter_IMDBpro)
#residualPlots(reg)

# 2. Check for Heteroskedasticity
pValue <- list()
for (a in numerical){
  eval(parse(text=paste("reg = lm(imdbScore~",a,")",sep="")))
  pValue <- append(pValue, ncvTest(reg)$p)
}

pValue <- data.frame(pValue)
colnames(pValue) <- numerical
pValue < 0.05

# 3. Outliers
for (a in numerical){
  eval(parse(text=paste("reg = lm(imdbScore~",a,")",sep="")))
  print(a)
  print(outlierTest(reg))
}
outlierTest(reg)

# 4. Validation Test
sample=sample.split(imdbdf$imdbScore, SplitRatio=0.5)
train_set=subset(imdbdf, sample==TRUE)
test_set=subset(imdbdf, sample==FALSE)

# Validation Approaches
# model 1
actual=test_set$imdbScore
prediction=predict(model1, test_set)
squared_error=(actual-prediction)^2
MSE=mean(squared_error)
MSE

# model 2
actual=test_set$imdbScore
prediction=predict(model2, test_set)
squared_error=(actual-prediction)^2
MSE=mean(squared_error)
MSE

# model 3
actual=test_set$imdbScore
prediction=predict(model3, test_set)
squared_error=(actual-prediction)^2
MSE=mean(squared_error)
MSE

# model 4
actual=test_set$imdbScore
prediction=predict(model4, test_set)
squared_error=(actual-prediction)^2
MSE=mean(squared_error)
MSE

# model 5
actual=test_set$imdbScore
prediction=predict(model5, test_set)
squared_error=(actual-prediction)^2
MSE=mean(squared_error)
MSE

# model 6
actual=test_set$imdbScore
prediction=predict(model6, test_set)
squared_error=(actual-prediction)^2
MSE=mean(squared_error)
MSE

# model 7
actual=test_set$imdbScore
prediction=predict(model7, test_set)
squared_error=(actual-prediction)^2
MSE=mean(squared_error)
MSE

# model 8
actual=test_set$imdbScore
prediction=predict(model8, test_set)
squared_error=(actual-prediction)^2
MSE=mean(squared_error)
MSE

# model 9
actual=test_set$imdbScore
prediction=predict(model9, test_set)
squared_error=(actual-prediction)^2
MSE=mean(squared_error)
MSE

# model 10
actual=test_set$imdbScore
prediction=predict(model10, test_set)
squared_error=(actual-prediction)^2
MSE=mean(squared_error)
MSE

# model 11
actual=test_set$imdbScore
prediction=predict(model11, test_set)
squared_error=(actual-prediction)^2
MSE=mean(squared_error)
MSE

# 5. LOOCV Test

# model 1
mse=cv.glm(imdbdf, model1)$delta[1]
mse

# model 2
mse=cv.glm(imdbdf, model2)$delta[1]
mse

# model 3
mse=cv.glm(imdbdf, model3)$delta[1]
mse

# model 4
mse=cv.glm(imdbdf, model4)$delta[1]
mse

# model 5
mse=cv.glm(imdbdf, model5)$delta[1]
mse

# model 6
mse=cv.glm(imdbdf, model6)$delta[1]
mse

# model 7
mse=cv.glm(imdbdf, model7)$delta[1]
mse

# model 8
mse=cv.glm(imdbdf, model8)$delta[1]
mse

# model 9
mse=cv.glm(imdbdf, model9)$delta[1]
mse

# model 10
mse=cv.glm(imdbdf, model10)$delta[1]
mse

# model 11
mse=cv.glm(imdbdf, model11)$delta[1]
mse
