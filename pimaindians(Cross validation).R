################# Changement du dossier de travail
setwd("C:/Users/mazou/Desktop/TP_ML")
getwd()

################# Chargement des donn�es
pimaindians <- read.table("data_PimaIndiansDiabetes.csv", sep = ";", header=TRUE)
str(pimaindians)

################# Packages
install.packages("caret")
install.packages("e1071")
library(caret)

################# Partitionnement des donn�es
set.seed(42)

# data de train et data de test
index <- createDataPartition(pimaindians$diabetes, p=.8, list=FALSE, times=1)
index

train_df <- pimaindians[index, ]
test_df <- pimaindians[-index, ]

train_df$diabetes[train_df$diabetes == 1] <- "Malade"
train_df$diabetes[train_df$diabetes == 0] <- "Sain"

test_df$diabetes[test_df$diabetes == 1] <- "Malade"
test_df$diabetes[test_df$diabetes == 0] <- "Sain"

train_df$diabetes <- as.factor(train_df$diabetes)
test_df$diabetes <- as.factor(test_df$diabetes)

class(train_df$diabetes)
class(test_df$diabetes)

################# Cross validation avec 10 partitions(Folds) sur un mod�le de r�gression logistique
ctrlspecs <- trainControl(method = "cv",
                          number = 10,
                          savePredictions = "all",
                          classProbs = TRUE)
set.seed(42)

# Sp�cification de la r�gr�ssion logsitique 
model <- train(diabetes ~ .,
               data = train_df,
               method = "glm",
               family = "binomial",
               trControl = ctrlspecs)
print(model)
summary(model)
varImp(model)

# Sp�cification de la r�gr�ssion logsitique sans les variables triceps et insuline
model <- train(diabetes ~ pregnant + glucose + pressure + mass + pedigree + age,
               data = train_df,
               method = "glm",
               family = "binomial",
               trControl = ctrlspecs)
print(model)
summary(model)
varImp(model)

# Application du mod�le sur l'ensemble de test 
predictions <- predict(model, newdata = test_df)
predictions
confusionMatrix(data=predictions, test_df$diabetes)

