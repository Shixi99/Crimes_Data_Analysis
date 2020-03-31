# Import the libraries ----
library(tidyverse)
library(caTools)
library(car)
library(e1071)
library(glue)
library(highcharter)
library(plotly)
library(dplyr)
library(stats)
library(h2o)

# Import the dataset ----
df <- read_csv('C://Users//dell//Downloads//crimes.csv')
df %>% glimpse()

# Data preparation ----

# Outliers
num_varss <- df %>% 
  select(-ViolentCrimesPerPop) %>% 
  select_if(is.numeric) %>% 
  names()
num_varss


# Check normality 
num_varss <- df %>% 
  select_if(is.numeric) %>% 
  names()
num_varss

par(mfrow=c(2, 2))  # divide graph area in 2columns & 2rows (number of variables)

for (i in 1:length(num_varss)) {
  var_name = num_varss[i]
  plot(density(df[[num_varss[i]]]),
       main=glue('{enexpr(var_name)}'), 
       ylab="Frequency", 
       sub=paste("Skewness:", round(e1071::skewness(df[[ num_varss[i] ]]), 2)))  
  polygon(density(df[[num_varss[i]]]), col="red")
}

# Correlation
hchart(cor(df %>% 
             mutate_if(is.character,as.factor) %>% 
             mutate_if(is.factor,as.numeric)) %>% 
         round(.,2),label = T)

# Transforming Data
df <- cbind(scale(df[-4]),df[4]) %>% as.data.frame()


# Linear Regression Diagnostics ----
glm <- glm(ViolentCrimesPerPop ~ ., data = df)

glm %>% summary()

glm %>% plot()

# Multicollinrarity
vif <- glm %>% vif() %>% as.data.frame()  
vif$Variable <- vif %>% rownames()
vif <- vif %>% rename(GVIF = '.')
vif %>% 
  select(Variable,GVIF) %>% 
  arrange(desc(GVIF)) 

df2 <- df %>% 
  select(-TotalPctDiv,-FemalePctDiv ,-MalePctDivorce ,-PctFam2Par,-PctKids2Par, -PctYoungKids2Par)

# Splitting the df into the Train set and Test set
set.seed(123)
split <- df2$ViolentCrimesPerPop %>% sample.split(SplitRatio = 0.8)
train <- df2 %>% subset(split == TRUE)
test <- df2 %>% subset(split == FALSE)


# GLM ----
glm <- glm(ViolentCrimesPerPop ~ ., data = train)
glm %>% summary()


# Select a significant variables with Stepwise Algorithm
step <- glm %>% stats::step()
step$call # copy past

glm2 <- glm(formula = ViolentCrimesPerPop ~ PctEmplProfServ + PctOccupManu + 
              PctOccupMgmtProf + MalePctNevMarr + PctTeen2Par + PctWorkMomYoungKids + 
              PctWorkMom + NumIlleg + PctIlleg + NumImmig + PctImmigRec5, 
            data = train)
glm2 %>% summary()

# --------------------------------------------------------
vif2 <- glm2 %>% vif() %>% as.data.frame()  
vif2$Variable <- vif2 %>% rownames()
vif2 <- vif2 %>% rename(GVIF = '.')
vif2 %>% 
  select(Variable,GVIF) %>% 
  arrange(desc(GVIF)) 

# --------------------------------------------------------
df3 <- df2 %>% 
  select(-PctWorkMom)

set.seed(123)
split <- df3$ViolentCrimesPerPop %>% sample.split(SplitRatio = 0.8)
train <- df3 %>% subset(split == TRUE)
test <- df3 %>% subset(split == FALSE)


# GLM ----
glm2 <- glm(ViolentCrimesPerPop ~ ., data = train)
glm2 %>% summary()


# Select a significant variables with Stepwise Algorithm
step <- glm2 %>% stats::step()
step$call # copy past

glm3 <- glm(formula = ViolentCrimesPerPop ~ PctEmplProfServ + PctOccupManu + 
              PctOccupMgmtProf + MalePctNevMarr + PersPerFam + PctTeen2Par + 
              PctWorkMomYoungKids + NumIlleg + PctIlleg + NumImmig + PctImmigRec5, 
            data = train)
glm3 %>% summary()

vif3 <- glm3 %>% vif() %>% as.data.frame()  
vif3$Variable <- vif3 %>% rownames()
vif3 <- vif3 %>% rename(GVIF = '.')
vif3 %>% 
  select(Variable,GVIF) %>% 
  arrange(desc(GVIF)) 

# -------------------------------------------------------------------------------
df4 <- df3 %>% 
  select(-PctIlleg)

set.seed(123)
split <- df4$ViolentCrimesPerPop %>% sample.split(SplitRatio = 0.8)
train <- df4 %>% subset(split == TRUE)
test <- df4 %>% subset(split == FALSE)

glm4 <- glm(ViolentCrimesPerPop ~ ., data = train)
glm4 %>% summary()


# Select a significant variables with Stepwise Algorithm
step <- glm4 %>% stats::step()
step$call # copy past

glm4 <- glm(formula = ViolentCrimesPerPop ~ PctEmplProfServ + PersPerFam + 
              PctTeen2Par + PctWorkMomYoungKids + NumIlleg + PctImmigRec5, 
            data = train)

vif4 <- glm4 %>% vif() %>% as.data.frame()  
vif4$Variable <- vif4 %>% rownames()
vif4 <- vif4 %>% rename(GVIF = '.')
vif4 %>% 
  select(Variable,GVIF) %>% 
  arrange(desc(GVIF)) 

# -------------------------------------------------------------------------------

# Predicting the Test set results
y_pred <- glm4 %>% predict(test %>% select('PctTeen2Par','NumIlleg','PersPerFam','PctImmigRec5','PctEmplProfServ','PctWorkMomYoungKids'))


# Model evaluation ----
residuals = test$ViolentCrimesPerPop - y_pred

#Calculate Root Mean Square Error
RMSE = sqrt(mean(residuals^2))

y_test_mean = mean(test$ViolentCrimesPerPop)

#Calculate total sum of squares
tss =  sum((test$ViolentCrimesPerPop - y_test_mean)^2)

#Calculate residual sum of squares
rss =  sum(residuals^2)

#Calculate R-squared
R2  =  1 - (rss/tss)

#Calculate Adjusted R-squared
n <- train %>% nrow() #sample size
k <- 6 #number of independent variables
Adjusted_R2 = 1-(1-R2)*((n-1)/(n-k-1))
Adjusted_R2 <- paste0(round(Adjusted_R2*100, 1),"%")

tibble(RMSE = round(RMSE,2),
       Adjusted_R2)


# Plotting actual & predicted ----
my_data <- as.data.frame(cbind(predicted = y_pred,
                               observed = test$ViolentCrimesPerPop))


# Plot predictions & test data
g <- my_data %>% ggplot(aes(predicted, observed)) + 
  geom_point(color = "darkred") + 
  geom_smooth(method=lm) + 
  ggtitle(glue('Multiple Linear Regression --> Adjusted R2 = {enexpr(Adjusted_R2)}')) +
  xlab("Predecited Power Output ") + 
  ylab("Observed Power Output") + 
  theme(plot.title = element_text(color="darkgreen",size=16,hjust=0.5),
        axis.text.y = element_text(size=12), 
        axis.text.x = element_text(size=12),
        axis.title.x = element_text(size=14), 
        axis.title.y = element_text(size=14))

g %>% ggplotly()


# -------------------------------------------------------------------
# -------------------------------------------------------------------

# h2o Model

h2o.init()

h2o_data <- as.h2o(df)

# Splitting the data
h2o_data <- h2o.splitFrame(h2o_data,ratios = c(0.7,0.15),seed=123)
train<-h2o_data[[1]]
validation<-h2o_data[[2]]
test<-h2o_data[[3]]

outcome <- 'ViolentCrimesPerPop' # y - variable which we want to predict
features <- df %>% select(-ViolentCrimesPerPop) %>% names() # by using x variables predict y 

# Fitting h2o model
model <- h2o.automl(
  x = features,
  y = outcome,
  training_frame    = train,
  validation_frame  = validation,
  leaderboard_frame = test,
  stopping_metric = "RMSE", #classification modeldirese auc
  seed = 123,
  max_runtime_secs = 360)


# Predicting the Test set results
y_pred <- model %>% h2o.predict(newdata = test) %>% as.data.frame()
y_pred


# Model evaluation ----
test_set <- test %>% as.data.frame()
residuals = test_set$ViolentCrimesPerPop - y_pred$predict

#Calculate Root Mean Square Error
RMSE = sqrt(mean(residuals^2))

y_test_mean = mean(test_set$ViolentCrimesPerPop)

#Calculate total sum of squares
tss = sum((test_set$ViolentCrimesPerPop - y_test_mean)^2)

#Calculate residual sum of squares
rss = sum(residuals^2)

#Calculate R-squared
R2 = 1 - (rss/tss)
R2

#Calculate Adjusted R-squared
n <- df %>% nrow() #sample size
k <- 6 #number of independent variables
Adjusted_R2 = 1-(1-R2)*((n-1)/(n-k-1))
Adjusted_R2 <- paste0(round(Adjusted_R2*100, 1),"%")

tibble(RMSE = round(RMSE),
       Adjusted_R2)

p3 <- df(movies, aes(x=))

p3 + geom_histogram(binwidth = 10)

