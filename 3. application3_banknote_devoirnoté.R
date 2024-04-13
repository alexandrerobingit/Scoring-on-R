rm(list=ls(all=TRUE))

library(readr)
billets <- read_delim("3. Exercice_billets_banknote.csv",delim = ";", escape_double = FALSE, trim_ws = TRUE)

attach(billets)

#On cherche à classer un nouveau billet de dimensions : Length=214.90, Left=130.12, Right=129.96 par trois méthodes :
# (i) Analyse Discriminante Probabiliste sous un modèle Gaussien hétéroscédastique  
# (ii) Analyse Discriminante Probabiliste sous un modèle Gaussien homoscédastique  
# (iii) Régression Logistique

new = data.frame(Length=c(214.90),Left=c(130.12),Right=c(129.96))

#### 1/ A quelle classe et avec quelle probabilité chacune des méthodes 
### (i), (ii), (iii) affecte-t-elle le nouveau billet ?

#ADP sous modèle Gaussien hétéroscédastique (variances differentes entre les classes)
library(Rmixmod)

learn = mixmodLearn(data=billets[,2:4],knownLabels=as.factor(Status),models=mixmodGaussianModel(listModels = c("Gaussian_pk_Lk_Ck")),criterion=c('BIC'))
learn #BIC = 610.8178

prednew = mixmodPredict(data=new,classificationRule=learn['bestResult'])
prednew # partition 1, donc counterfeit, avec proba de 60,28%

#ADP sous modèle Gaussien homoscédastique
learn = mixmodLearn(data=billets[,2:4],knownLabels=as.factor(Status),models=mixmodGaussianModel(listModels = c("Gaussian_pk_L_C")),criterion=c('BIC'))
learn #BIC = 594.7856

prednew = mixmodPredict(data=new,classificationRule=learn['bestResult'])
prednew # partition 2, donc genuine, avec proba de 50,01%

# Regression lineaire

rule=glm(as.factor(Status)~Length+Left+Right, family=binomial(link='logit'))
rule
#intercept = 157.92
#coeff length = 2.967
#coeff left = -2
#coeff right = -4.118
#Beta_hat = (2.6 ; -0.2 ; -4.118)
#Alpha_hat = 157.92
# DONC S(x)= 2.967*length - 2*left - 4.118*right + 157.92
rule$coefficients

############# Affectation
score=predict(rule,new)
score
# score > 0, le billet appartient à la classe 1

score = predict(rule, billets[,2:4])
table(score<0, Status)
#donc le billet est classé en genuine

proba=predict(rule,new,type = "response")
proba
# avec une proba de 50,39%

#### 2/ Quel est le meilleur des deux modèles (i) et (ii) selon BIC ?

# modèle hétéroscédastique puisque l'on minimise le BIC

#### 3/ Quelle est l'erreur de classement de chacune des méthodes 
### (i), (ii), (iii) si l'échantillon d'appprentissage est pris pour échantillon de test ?

##################################
# (i) ADP modèle Gaussien hetero
##################################

learn = mixmodLearn(data=billets[,2:4],knownLabels=as.factor(Status),models=mixmodGaussianModel(listModels = c("Gaussian_pk_Lk_Ck")),criterion=c('BIC'))
learn

pred = mixmodPredict(data=billets[,2:4],classificationRule=learn['bestResult'])

table(pred@partition,Status)
# contrefait = classe 1
# officiel = classe 2
(15+16)/200
#erreur de classement = (18+16)/200 = 15.5%

##################################
# (ii) ADP modele Gaussien homo
##################################

# Gaussian based ADP
learn = mixmodLearn(data=billets[,2:4],knownLabels=as.factor(Status),models=mixmodGaussianModel(listModels = c("Gaussian_pk_L_C")),criterion=c('BIC'))
learn

pred = mixmodPredict(data=billets[,2:4],classificationRule=learn['bestResult'])

table(pred@partition,Status)
# contrefacon = classe 1
# officiel = classe 2
############# erreur de classement :
(19+17)/(200) # 18%

###########################
# (iii) Reg Logistique
###########################

############# Erreur de classement
rule=glm(as.factor(trainz)~.,data=trainx, family=binomial(link='logit')) 
score = predict(rule, new=trainx)
table(score<0,trainz) # false = score positif => pour nous c'est la classe 1
# un score negatif donne les billets contrefaits

score = predict(rule,billets[,2:4])
table(score<0,Status)

#### erreur de RL = (19+17)/(200)
(19+17)/(200) # ===> 18%
