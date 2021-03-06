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
# mod�le de r�gression logistique
log_reg <- glm(diabetes ~ .,data =train_data,family="binomial")
coef(log_reg)
summary(log_reg)

# probabilit�s pr�dites
hat_pi <- predict(log_reg, newdata= test_data, type="response")

# valeurs pr�dites (threshold 50%)
hat_diabetes <- as.integer(hat_pi > 0.5)
hat_diabetes

# Matrice de confusion
table(hat_diabetes, test_data$diabetes)
# taux d'erreur (taux de mal class�)
erreur <- sum(hat_diabetes != test_data$diabetes)/length(test_data$diabetes)
erreur
# accuracy = 1 - taux d'erreur
accuracy <- sum(hat_diabetes == test_data$diabetes)/length(test_data$diabetes)
accuracy
# sensibilit� (taux vrai positif)
sensibilite <- sum(hat_diabetes == 1 & test_data$diabetes == 1)/sum(test_data$diabetes == 1)
sensibilite
# taux faux n�gatif = 1 - sensibility
faux_negatif <- 1 - sensibilite
faux_negatif
# specificit� (taux vrai n�gatif)
specificite <- sum(hat_diabetes == 0 & test_data$diabetes == 0)/sum(test_data$diabetes == 0)
specificite
# taux faux positif = 1 - specificity
faux_positif <- 1 - specificite
faux_positif

################# Optimisation du mod�le de r�gr�ssion logistique
model_opt <- step(log_reg, direction="backward")
model_opt
summary(model_opt)

# probabilit�s pr�dites
hat_pi <- predict(model_opt, newdata= test_data, type="response")
hat_pi

# valeurs pr�dites (threshold 50%)
hat_diabetes <- as.integer(hat_pi > 0.5)
hat_diabetes

# Matrice de confusion
table(hat_diabetes, test_data$diabetes)
# taux d'erreur (taux de mal class�)
erreur <- sum(hat_diabetes != test_data$diabetes)/length(test_data$diabetes)
erreur
# accuracy = 1 - taux d'erreur
accuracy <- sum(hat_diabetes == test_data$diabetes)/length(test_data$diabetes)
accuracy
# sensibilit� (taux vrai positif)
sensibilite <- sum(hat_diabetes == 1 & test_data$diabetes == 1)/sum(test_data$diabetes == 1)
sensibilite
# taux faux n�gatif = 1 - sensibility
faux_negatif <- 1 - sensibilite
faux_negatif
# specificit� (taux vrai n�gatif)
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

################# R�sultat avec le meilleur threshold
# valeurs pr�dites (threshold 50%)
hat_diabetes <- as.integer(hat_pi > best_threshold)
hat_diabetes

# Matrice de confusion
table(hat_diabetes, test_data$diabetes)
# taux d'erreur (taux de mal class�)
erreur <- sum(hat_diabetes != test_data$diabetes)/length(test_data$diabetes)
erreur
# accuracy = 1 - taux d'erreur
accuracy <- sum(hat_diabetes == test_data$diabetes)/length(test_data$diabetes)
accuracy
# sensibilit� (taux vrai positif)
sensibilite <- sum(hat_diabetes == 1 & test_data$diabetes == 1)/sum(test_data$diabetes == 1)
sensibilite
# taux faux n�gatif = 1 - sensibility
faux_negatif <- 1 - sensibilite
faux_negatif
# specificit� (taux vrai n�gatif)
specificite <- sum(hat_diabetes == 0 & test_data$diabetes == 0)/sum(test_data$diabetes == 0)
specificite
# taux faux positif = 1 - specificity
faux_positif <- 1 - specificite
faux_positif
