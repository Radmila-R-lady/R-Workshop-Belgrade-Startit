#######################RLadies: Visestruka linearna regresija###############
#########################Radmila VELICKOVIC#####################################
#########################20.2.2019, Startit, Beograd#########################


# UVOD ------------------------------------------------------------

#Linearna regresija odgovara na pitanje da li varijabla Y zavisi od X,
#odnosno koje varijable X uticu na Y


#engleski termini za nezavisne i zavisnu promenljivu
#X1,X2, Xn-independent variable, regressors, predictors, explanatory variables,covariates
#Y-dependent variable, regressand, explained variable  

#linearna regresija sluzi za modeliranje tj. objasnjenje nekog fenomena
#i za predvidanje

#u visestrukoj regresiji preliminarna analiza je jako vazna jer interakcija 
#izmedju varijabli moze biti sakrivena 

#dijagnostikovanje modela je od izuzetnog znacaja zato sto pretpostavke
#u visestrukoj regresiji mogu biti preksene

#jedna od pretpostavki je da su objasnjavajuce promenljive nezavisne. 
#Ali, sta ako izmedu nekih od njih postoji (visoka) korelacija? 

#Y=B0+B1+...+Bn

#multiple linear regression

getwd()
setwd("C:/Users/Rada/POSM laptop/1. STATISTICS masters material/R_Treninzi")

houses<-read.table("houses.txt",header=T)

View(houses)
attach(houses)
dim(houses)

summary(houses)
cor(houses)
pairs(houses)


#fitting the model using all possible variables (backward method)
model1=lm(area~price+bed+bath.+renovation)
model1
summary(model1)

#fitted values
fitted(fit)

#residuals
residuals(fit)

par(mfrow=c(2,2))
plot(model1)


#we removed least significant variable bed)
model2=lm(price~area+bath.+renovation)
summary(model2)
par(mfrow=c(2,2))
plot(model2)

#we remove bath as non significant
model3=lm(price~area+bed)
summary(model3)
par(mfrow=c(2,2))
plot(model3)

#ostali paketi za regresione modele
#glmnet
#essurvey paket ima funkcije za regresiju sa weights namenjene
#za ess podatke europeansocialsurvey.org



# clean up and close!

rm(list=ls())
q()