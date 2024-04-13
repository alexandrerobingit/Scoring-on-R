rm(list=ls(all=TRUE))

mydata = data.frame(sal=c(1.7,2.2,3.4,3.5),age=c(28,38,43,54),classe=c('A','B','B','A'))
attach(mydata) # permet de detacher les variables, plus besoin de mettre >mydata$sal, mais juste sal
sal
plot(sal,age, col=as.factor(classe))
points(2.1,34,col='green')

rule=glm(as.factor(classe)~sal+age, family=binomial(link='logit'))# classe en fonction de salaire et age
#rule contient l'estimation de Beta et Alpha
rule
#intercept = 1.16
#coeff salaire = 2.6
#coeff age = -0.2
#Beta_hat = (2.6 ; -0.2)
#Alpha_hat = 1.16
# DONC S(x)= 2.6*salaire - 0.2*age + 1.16 
#reste des explications page 1 seance 4

rule$coefficients
abline(a=1.16/0.2 , b=2.6/0.2)# frontiere de classement
#on observe que 2 B sont de part et d'autre de la frontière, 1 erreur sur 4, erreur de classement = 1/4 = 25%


#calcul pour M Martin
score=predict(rule,new=data.frame(sal=c(2.1),age=c(34)))
score
#ce score est negatif, on affecte M Martin à la classe 2

score=predict(rule, new=mydata[,1:2])
score

#est ce que les scores coincident avec les classes réelles ?
table(score<0,classe) #dde a r de construire la table de contingence selon deux criteres qualitatifs
# 2 scores neg dans A, les 2 en SudOuest, 1 neg dans B, celui en SudEst, le pos dans B, celui en NordEst
#erreur de classement de 25


############## a faire, erreur de classement RL, AD Gaussienne heteroscedastique, ADG homoscédastique

#################### commence par erreur de classement avec RL
library(Rmixmod)
data("finance")
mydata=finance
trainx=mydata[mydata$Year=='2002',3:6]
trainz=mydata[mydata$Year=='2002',2]

testx=mydata[mydata$Year=='2003',3:6]
testz=mydata[mydata$Year=='2003',2]

rule=glm(as.factor(trainz)~.,data=trainx, family=binomial(link='logit')) 
#~.,data=trainx veut dire qu'on oprend toutes les variables dans le jeu de données trainx

score = predict(rule, new=trainx) #predit la regle sur les données d'apprentissage
score

#objectif : classer les firmes de 2003 (leur vraie classe est dans testz)
# avant ça on veut savoir si bankrupty c'est classe 1 estimee ou classe 2 estimee

#tableau de contingence
table(score<0,trainz)
# FALSE = score positif, pour nous c'est la classe 1
#les indivs en erreur de classement 40 et 37
# healthy = classe 1 = score > 0
# bankruptcy = classe 2 = score <0

#mtn on retourne dans l'echantillon de test
score = predict(rule, new=testx) #predit la regle sur les données test
score

#tableau de contingence
table(score<0,testz)
#mtn on sait que le FALSE c'est la classe 'healthy'
#donc 57 et 68 sont les mal classés

#### erreur de RL = (57+68)/(57+68+163+173)
(57+68)/(57+68+163+173) # ===> 27%

##################### erreur de classement Gaussienne heteroscedastique

# Gaussian based ADP
learn = mixmodLearn(data=trainx,knownLabels=as.factor(trainz),models=mixmodGaussianModel(listModels = c("Gaussian_pk_Lk_C")),criterion=c('BIC'))
learn

new=trainx
pred = mixmodPredict(data=new,classificationRule=learn['bestResult'])

table(pred@partition,trainz)
# bankruptcy = classe 1
# healthy = classe 2

new=testx
pred = mixmodPredict(data=new,classificationRule=learn['bestResult'])

table(pred@partition,testz)

#erreur de classement :
(58+54)/(162+54+58+187)
# ===> 0,25

##################### erreur de classement Gaussienne homoscedastique

# Gaussian based ADP
learn = mixmodLearn(data=trainx,knownLabels=as.factor(trainz),models=mixmodGaussianModel(listModels = c("Gaussian_pk_L_C")),criterion=c('BIC'))
learn

new=trainx
pred = mixmodPredict(data=new,classificationRule=learn['bestResult'])

table(pred@partition,trainz)
# bankruptcy = classe 1
# healthy = classe 2

new=testx
pred = mixmodPredict(data=new,classificationRule=learn['bestResult'])

table(pred@partition,testz)

#erreur de classement :
(60+64)/(160+60+64+177)
# ===> 0,27

#### le meilleur est l'heteroscedastique