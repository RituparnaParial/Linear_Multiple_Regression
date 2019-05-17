#-----------LINEAR REGRESSION-------------------#
# example uses "women" dataset
# that contains height and weight of 15 women aged 30 - 39

str(women)


simple_linear_model <- lm(weight ~ height, data = women)
simple_linear_model
# shows the intercept and beta coefficient for height variable
# ie weight = -87.52 + 3.45 x height

plot(women$height,
     women$weight, 
     xlab = "Height (inches)",
     ylab = "Weight (lbs)",
     main = "Scatter plot showing regression line for weight
     predicted from height")


abline(simple_linear_model)

summary(simple_linear_model)
confint(simple_linear_model)

# cor measures the association between 2 variables
# ranges from -1 which is a perfect -ve correlation 
# to +1 which is a perfect +ve correlation
# a value close to zero indicates a weak relationship
# a low correlation (-0.2< x < 0.2) suggests that much
# of the variation of the outcome variable is not explained
# by the predictor
# In such case we should then look at better predictor
# variables

cor(women$height, women$weight)

# Lets examine the goodness of fit

summary(simple_linear_model)

# Cars dataset
install.packages("car")
library(car)
str(cars)
head(cars)

# visualise if there is any relationship between the
# independent and dependent variables
scatter.smooth(x = cars$speed, 
               y = cars$dist,
               xlab = "Car Speed",
               ylab = "Stopping Distance",
               main = "Distance against Speed")

par(mfrow = c(1,2)) # Divides graph area into 2 columns
boxplot(cars$speed, main = "Car Speed", 
        sub = paste("Outlier rows ", boxplot.stats(cars$speed)$out))

boxplot(cars$dist, main = "Stopping distance", 
        sub = paste("Outlier rows ", boxplot.stats(cars$dist)$out))

install.packages("e1071")
library(e1071)
par(mfrow = c(1,2)) 
plot(density(cars$speed), 
     main = "Density plot : speed",
     ylab = "Frequency",
     sub = paste("Skewness :", 
                 round(e1071::skewness(cars$speed), 2)))

# lets fill in the area under the density plot
polygon(density(cars$speed), col = "red")


plot(density(cars$dist), 
     main = "Density plot : distance",
     ylab = "Frequency",
     sub = paste("Skewness :", 
                 round(e1071::skewness(cars$dist), 2)))

# lets fill in the area under the density plot
polygon(density(cars$dist), col = "red")

cor(cars$speed, cars$dist)

# Build linear regression model on full data
linearmodel <- lm(cars$dist ~ cars$speed)
print(linearmodel) # Shows coeffiecients
model_summary <- summary(linearmodel)


# Calculate model coefficients 

model_coefficients <- model_summary$coefficients
model_coefficients

AIC(linearmodel)
BIC(linearmodel)
# -----------------------------
set.seed(200)

# select a random sample from 1 to all records
# in cars, with 80% of rows
total_records <- sample(1:nrow(cars), 0.8*nrow(cars))

training_data <- cars[total_records,]
training_data

testing_data <- cars[-total_records,]
testing_data

# Build the model using the training data
lr_model <- lm(dist ~ speed, data = training_data)

# model summary
summary(lr_model)

# Predict distance from testing data
dist_predicted <- predict(lr_model, testing_data)

# make an actuals v/s predicted dataframe
actuals_preds <- data.frame(actuals = testing_data$dist,
                            predicted = dist_predicted)

actuals_preds

cor_accuracy <- cor(actuals_preds$actuals, actuals_preds$predicted)
cor_accuracy

# Checking min max accuracy
min_max_accuracy <- mean(apply(actuals_preds, 1, min) 
                         / apply(actuals_preds, 1, max))
min_max_accuracy


# ....................................#
# Multiple Linear Regression
# We use the ":" symbol to indicate an interaction between the
# predictor variables
multiple_linear_model <- lm(mpg ~ hp + wt + hp:wt, data = mtcars)
summary(multiple_linear_model)

# We can visualise interaction using the effects() function
# This means that we can change the value for weight and
# view the changes graphically

install.packages("effects")
library(effects)
plot(effect("hp:wt", multiple_linear_model,, 
            list(wt = c(2.2, 3.2, 4.2))), multiline = TRUE)

# Evaluate the statistical assumption using the plot() function
par(mfrow = c(2,2))
plot(multiple_linear_model)

install.packages("gvlma")
library(gvlma)
gvmodel <- gvlma(multiple_linear_model)
summary(gvmodel)