################# Changement du dossier de travail
setwd("C:/Users/mazou/Desktop/TP_ML")
getwd()


################# Chargement des donn�es
pimaindians <- read.table("data_PimaIndiansDiabetes.csv", sep = ";", header=TRUE)
str(pimaindians)



################# Split des donn�es en data de train(70%) et de test (30%)

# V�rification de l'�quilibre des groupes
table(pimaindians$diabetes)
library(ggplot2)
ggplot(pimaindians) +geom_bar(aes(as.factor(diabetes))) +xlab("Diabetes") +theme_bw()

# Nombre d'observations
n <- nrow(pimaindians)

# indices des individus dans l'�chantillon d'apprentissage
train_index <- sample(x =1:n,size = round(0.7 * n),replace =FALSE)

# train et test sets
train_data <-pimaindians[train_index,]
test_data <-pimaindians[-train_index,]
nrow(train_data)
nrow(test_data)


################# R�gression lin�aire
# mod�le de r�gression lin�aire
lin_reg <- lm(diabetes ~ .,data =train_data)

# R2
summary(lin_reg)$r.squared

# graph r�sidus vs valeurs pr�dites
plot(lin_reg,which=1)

# graph quantiles vs �cart-type
plot(lin_reg,which=2)

# graph variance vs valeurs pr�dites
plot(lin_reg,which=3)

# r�ponse : valeurs pr�dites vs valeurs observ�es
plot(train_data$diabetes, predict(lin_reg),xlab ="observed value",
     ylab ="predicted value")


################# R�gression logistique
