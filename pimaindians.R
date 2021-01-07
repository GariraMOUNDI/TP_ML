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
# modèle de régression logistique
log_reg <- glm(diabetes ~ .,data =train_data,family="binomial")
coef(log_reg)
summary(log_reg)

# probabilités prédites
hat_pi <- predict(log_reg, newdata= test_data, type="response")

# valeurs prédites (threshold 50%)
hat_diabetes <- as.integer(hat_pi > 0.5)
hat_diabetes

# Matrice de confusion
table(hat_diabetes, test_data$diabetes)
# taux d'erreur (taux de mal classé)
erreur <- sum(hat_diabetes != test_data$diabetes)/length(test_data$diabetes)
erreur
# accuracy = 1 - taux d'erreur
accuracy <- sum(hat_diabetes == test_data$diabetes)/length(test_data$diabetes)
accuracy
# sensibilité (taux vrai positif)
sensibilite <- sum(hat_diabetes == 1 & test_data$diabetes == 1)/sum(test_data$diabetes == 1)
sensibilite
# taux faux négatif = 1 - sensibility
faux_negatif <- 1 - sensibilite
faux_negatif
# specificité (taux vrai négatif)
specificite <- sum(hat_diabetes == 0 & test_data$diabetes == 0)/sum(test_data$diabetes == 0)
specificite
# taux faux positif = 1 - specificity
faux_positif <- 1 - specificite
faux_positif

################# Optimisation du modèle de régréssion logistique
model_opt <- step(log_reg, direction="backward")
model_opt
summary(model_opt)

# probabilités prédites
hat_pi <- predict(model_opt, newdata= test_data, type="response")
hat_pi

# valeurs prédites (threshold 50%)
hat_diabetes <- as.integer(hat_pi > 0.5)
hat_diabetes

# Matrice de confusion
table(hat_diabetes, test_data$diabetes)
# taux d'erreur (taux de mal classé)
erreur <- sum(hat_diabetes != test_data$diabetes)/length(test_data$diabetes)
erreur
# accuracy = 1 - taux d'erreur
accuracy <- sum(hat_diabetes == test_data$diabetes)/length(test_data$diabetes)
accuracy
# sensibilité (taux vrai positif)
sensibilite <- sum(hat_diabetes == 1 & test_data$diabetes == 1)/sum(test_data$diabetes == 1)
sensibilite
# taux faux négatif = 1 - sensibility
faux_negatif <- 1 - sensibilite
faux_negatif
# specificité (taux vrai négatif)
specificite <- sum(hat_diabetes == 0 & test_data$diabetes == 0)/sum(test_data$diabetes == 0)
specificite
# taux faux positif = 1 - specificity
faux_positif <- 1 - specificite
faux_positif

################# Courbe ROC
library(pROC)
# Aire en dessous de la courbe ROC
auc <- auc(test_data$diabetes, hat_pi)
auc
# Objet ROC
roc_obj <- roc(test_data$diabetes, hat_pi)
roc_obj
# Plot de la courbe ROC
plot(roc_obj)
# Meilleur threshold
best_coord <- coords(roc_obj, "best", "threshold")
best_coord
best_threshold <- best_coord$threshold
best_threshold

################# Résultat avec le meilleur threshold
# valeurs prédites (threshold 50%)
hat_diabetes <- as.integer(hat_pi > best_threshold)
hat_diabetes

# Matrice de confusion
table(hat_diabetes, test_data$diabetes)
# taux d'erreur (taux de mal classé)
erreur <- sum(hat_diabetes != test_data$diabetes)/length(test_data$diabetes)
erreur
# accuracy = 1 - taux d'erreur
accuracy <- sum(hat_diabetes == test_data$diabetes)/length(test_data$diabetes)
accuracy
# sensibilité (taux vrai positif)
sensibilite <- sum(hat_diabetes == 1 & test_data$diabetes == 1)/sum(test_data$diabetes == 1)
sensibilite
# taux faux négatif = 1 - sensibility
faux_negatif <- 1 - sensibilite
faux_negatif
# specificité (taux vrai négatif)
specificite <- sum(hat_diabetes == 0 & test_data$diabetes == 0)/sum(test_data$diabetes == 0)
specificite
# taux faux positif = 1 - specificity
faux_positif <- 1 - specificite
faux_positif
