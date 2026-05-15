install.packages("dplyr")
install.packages("ggplot2")
install.packages("caret")
install.packages("randomForest")
install.packages("rpart")
library(dplyr)
library(ggplot2)
library(caret)
library(randomForest)
library(rpart)
head(smartfit_dataset)
str(smartfit_dataset)
summary(smartfit_dataset)
ggplot(smartfit_dataset, aes(x = Size)) +
  geom_bar(fill = "skyblue") +
  ggtitle("Distribution of Clothing Sizes")
library(ggplot2)
library(ggplot2)
install.packages("ggplot2")
library(ggplot2)
ggplot(smartfit_dataset, aes(x = Size)) +
  geom_bar(fill = "skyblue") +
  ggtitle("Distribution of Clothing Sizes")
ggplot(smartfit_dataset,
       aes(x = Height_cm,
           y = Weight_kg,
           color = Size)) +
  geom_point(size = 3) +
  ggtitle("Height vs Weight by Clothing Size")
ggplot(smartfit_dataset,
       aes(x = Brand,
           fill = Size)) +
  geom_bar(position = "dodge") +
  ggtitle("Brand-wise Clothing Size Distribution")
ggplot(smartfit_dataset,
       aes(x = Gender,
           fill = Gender)) +
  geom_bar() +
  ggtitle("Gender Distribution")
numeric_data <- smartfit_dataset[, c(
  "Height_cm",
  "Weight_kg",
  "Chest_cm",
  "Waist_cm",
  "Hip_cm")]
cor(numeric_data)
smartfit_dataset$Gender <-
  as.factor(smartfit_dataset$Gender)

smartfit_dataset$Brand <-
  as.factor(smartfit_dataset$Brand)

smartfit_dataset$Category <-
  as.factor(smartfit_dataset$Category)

smartfit_dataset$Size <-
  as.factor(smartfit_dataset$Size)
set.seed(123)

trainIndex <- createDataPartition(
  smartfit_dataset$Size,
  p = 0.8,
  list = FALSE)

trainData <- smartfit_dataset[trainIndex, ]

testData <- smartfit_dataset[-trainIndex, ]
set.seed(123)

trainIndex <- createDataPartition(
  smartfit_dataset$Size,
  p = 0.8,
  list = FALSE)

trainData <- smartfit_dataset[trainIndex, ]

testData <- smartfit_dataset[-trainIndex, ]
library(caret)
install.packages("caret")
library(caret)
set.seed(123)

trainIndex <- createDataPartition(
  smartfit_dataset$Size,
  p = 0.8,
  list = FALSE)

trainData <- smartfit_dataset[trainIndex, ]

testData <- smartfit_dataset[-trainIndex, ]
dim(trainData)
dim(testData)
model <- rpart(
  Size ~ Height_cm + Weight_kg +
    Chest_cm + Waist_cm + Hip_cm,
  data = trainData,
  method = "class")
predictions <- predict(
  model,
  testData,
  type = "class")
confusionMatrix(
  predictions,
  testData$Size)
smartfit_dataset <- read.csv(
  "C:/Users/Drishti/OneDrive/Desktop/smartfit_dataset.csv")
dim(smartfit_dataset)
smartfit_dataset <- read.csv(
  "C:/Users/Drishti/OneDrive/Desktop/smartfit_dataset.csv")
dim(smartfit_dataset)
str(smartfit_dataset)
summary(smartfit_dataset)
ggplot(smartfit_dataset,
       aes(x = Size,
           fill = Size)) +
  geom_bar() +
  ggtitle("Distribution of Clothing Sizes")
ggplot(smartfit_dataset,
       aes(x = Height_cm,
           y = Weight_kg,
           color = Size)) +
  geom_point(size = 3) +
  ggtitle("Height vs Weight by Clothing Size")
ggplot(smartfit_dataset,
       aes(x = Brand,
           fill = Size)) +
  geom_bar(position = "dodge") +
  ggtitle("Brand-wise Size Distribution")
numeric_data <- smartfit_dataset[, c(
  "Height_cm",
  "Weight_kg",
  "Chest_cm",
  "Waist_cm",
  "Hip_cm",
  "Age",
  "BMI")]

correlation_matrix <- cor(numeric_data)

print(correlation_matrix)
smartfit_dataset$Gender <-
  as.factor(smartfit_dataset$Gender)

smartfit_dataset$Brand <-
  as.factor(smartfit_dataset$Brand)

smartfit_dataset$Category <-
  as.factor(smartfit_dataset$Category)

smartfit_dataset$Size <-
  as.factor(smartfit_dataset$Size)

set.seed(123)

trainIndex <- createDataPartition(
  smartfit_dataset$Size,
  p = 0.8,
  list = FALSE)

trainData <- smartfit_dataset[trainIndex, ]

testData <- smartfit_dataset[-trainIndex, ]

dim(trainData)
dim(testData)
dim(trainData)

model <- rpart(
  Size ~ Height_cm + Weight_kg +
    Chest_cm + Waist_cm + Hip_cm +
    Age + BMI,
  data = trainData,
  method = "class")

predictions <- predict(
  model,
  testData,
  type = "class")
confusionMatrix(
  predictions,
  testData$Size)
rpart.plot(model)
rpart.plot(model)

model <- rpart(
  Size ~ Height_cm + Weight_kg +
    Chest_cm + Waist_cm + Hip_cm +
    Age + BMI,
  data = trainData,
  method = "class")





















new_person <- data.frame(
  Height_cm = 175,
  Weight_kg = 72,
  Chest_cm = 98,
  Waist_cm = 84,
  Hip_cm = 99,
  Age = 22,
  BMI = 23.5
)

predict(model, new_person, type = "class")



new_person <- data.frame(
  Height_cm = 158,
  Weight_kg = 50,
  Chest_cm = 84,
  Waist_cm = 68,
  Hip_cm = 90,
  Age = 20,
  BMI = 20
)

predict(model, new_person, type = "class")

new_person <- data.frame(
  Height_cm = 185,
  Weight_kg = 92,
  Chest_cm = 110,
  Waist_cm = 98,
  Hip_cm = 108,
  Age = 25,
  BMI = 28
)

predict(model, new_person, type = "class")



new_person <- data.frame(
  Height_cm = 185,
  Weight_kg = 92,
  Chest_cm = 110,
  Waist_cm = 98,
  Hip_cm = 108,
  Age = 25,
  BMI = 28
)
predict(model, new_person, type = "class")


model <- rpart(
  Size ~ Height_cm + Weight_kg +
    Chest_cm + Waist_cm + Hip_cm +
    Age + BMI + Gender +
    Brand + Category,
  
  data = trainData,
  method = "class"
)
predictions <- predict(
  model,
  testData,
  type = "class"
)

confusionMatrix(
  predictions,
  testData$Size
)
new_person <- data.frame(
  Height_cm = 175,
  Weight_kg = 72,
  Chest_cm = 98,
  Waist_cm = 84,
  Hip_cm = 99,
  Age = 22,
  BMI = 23.5,
  Gender = "Male",
  Brand = "Zara",
  Category = "T-shirt"
)
predict(model, new_person, type = "class")
smartfit_dataset$Gender <- as.factor(smartfit_dataset$Gender)

smartfit_dataset$Brand <- as.factor(smartfit_dataset$Brand)

smartfit_dataset$Category <- as.factor(smartfit_dataset$Category)
set.seed(123)

trainIndex <- createDataPartition(
  smartfit_dataset$Size,
  p = 0.8,
  list = FALSE
)

trainData <- smartfit_dataset[trainIndex, ]

testData <- smartfit_dataset[-trainIndex, ]
model <- rpart(
  Size ~ Height_cm + Weight_kg +
    Chest_cm + Waist_cm + Hip_cm +
    Age + BMI + Gender +
    Brand + Category,
  
  data = trainData,
  method = "class"
)
predictions <- predict(
  model,
  testData,
  type = "class"
)

confusionMatrix(
  predictions,
  testData$Size
)
new_person <- data.frame(
  Height_cm = 175,
  Weight_kg = 72,
  Chest_cm = 98,
  Waist_cm = 84,
  Hip_cm = 99,
  Age = 22,
  BMI = 23.5,
  Gender = "Male",
  Brand = "Zara",
  Category = "T-shirt"
)

predict(model, new_person, type = "class")

new_person <- data.frame(
  Height_cm = 158,
  Weight_kg = 50,
  Chest_cm = 84,
  Waist_cm = 68,
  Hip_cm = 90,
  Age = 20,
  BMI = 20,
  Gender = "Female",
  Brand = "Cider",
  Category = "Top"
)

predict(model, new_person, type = "class")

new_person <- data.frame(
  Height_cm = 174,
  Weight_kg = 70,
  Chest_cm = 98,
  Waist_cm = 84,
  Hip_cm = 100,
  Age = 22,
  BMI = 23,
  Gender = "Male",
  Brand = "Uniqlo",
  Category = "T-shirt"
)

predict(model, new_person, type = "class")

new_person <- data.frame(
  Height_cm = 185,
  Weight_kg = 92,
  Chest_cm = 112,
  Waist_cm = 98,
  Hip_cm = 108,
  Age = 25,
  BMI = 28,
  Gender = "Male",
  Brand = "HRX",
  Category = "Hoodie"
)

predict(model, new_person, type = "class")

new_person <- data.frame(
  Height_cm = 167,
  Weight_kg = 66,
  Chest_cm = 96,
  Waist_cm = 78,
  Hip_cm = 104,
  Age = 23,
  BMI = 24,
  Gender = "Female",
  Brand = "Zara",
  Category = "Dress"
)

predict(model, new_person, type = "class")

new_person <- data.frame(
  Height_cm = 178,
  Weight_kg = 80,
  Chest_cm = 106,
  Waist_cm = 90,
  Hip_cm = 104,
  Age = 21,
  BMI = 25,
  Gender = "Male",
  Brand = "Bonkers",
  Category = "Oversized T-shirt"
)

predict(model, new_person, type = "class") 


