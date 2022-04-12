# Logistic Regression

# Importing the dataset
dataset = read.csv('breast_cancer.csv')
dataset = dataset[-1]

# Encoding the target feature as factor
dataset$Class = factor(dataset$Class, levels = c(2, 4))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Class, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
training_set[,1:9] = scale(training_set[,1:9])
test_set[,1:9] = scale(test_set[,1:9])

# Fitting Logistic Regression to the Training set
classifier = glm(formula = Class ~ .,
                 family = binomial,
                 data = training_set)

# Predicting the Test set results
prob_pred = predict(classifier, type = 'response', newdata = test_set[-10])
y_pred = ifelse(prob_pred > 0.5, 1, 0)


## Validate the model

# Making the Confusion Matrix
cm = table(test_set[, 10], y_pred)


library(caret)


#repeated K-fold cross-validation

# setting seed to generate a
# reproducible random sampling
set.seed(125)

# defining training control as
# repeated cross-validation and
# value of K is 10 and repetition is 3 times
train_control <- trainControl(method = "repeatedcv",
                              number = 10, repeats = 3)

# training the model by assigning sales column
# as target variable and rest other column
# as independent variable
model <- train(Class ~., data = training_set,
               method = "glm",
               trControl = train_control)

# printing model performance metrics
# along with other details
print(model)

