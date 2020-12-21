################# Changement du dossier de travail
setwd("C:/Users/mazou/Desktop/TP_ML")
getwd()


################# Chargement des données
pimaindians <- read.table("data_PimaIndiansDiabetes.csv", sep = ";", header=TRUE)
str(pimaindians)



################# Split des données en data de train(70%) et de test (30%)

# Vérification de l'équilibre des groupes
table(pimaindians$diabetes)
library(ggplot2)
ggplot(pimaindians) +geom_bar(aes(as.factor(diabetes))) +xlab("Diabetes") +theme_bw()

# Nombre d'observations
n <- nrow(pimaindians)

# indices des individus dans l'échantillon d'apprentissage
train_index <- sample(x =1:n,size = round(0.7 * n),replace =FALSE)

# train et test sets
train_data <-pimaindians[train_index,]
test_data <-pimaindians[-train_index,]
nrow(train_data)
nrow(test_data)


################# Régression linéaire
# modèle de régression linéaire
lin_reg <- lm(diabetes ~ .,data =train_data)

# R2
summary(lin_reg)$r.squared

# graph résidus vs valeurs prédites
plot(lin_reg,which=1)

# graph quantiles vs écart-type
plot(lin_reg,which=2)

# graph variance vs valeurs prédites
plot(lin_reg,which=3)

# réponse : valeurs prédites vs valeurs observées
plot(train_data$diabetes, predict(lin_reg),xlab ="observed value",
     ylab ="predicted value")


################# Régression logistique

