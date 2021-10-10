#importation de la base de données

data = read.csv2("covid.csv", sep= ",")


#installation des packages necessaire

library(funModeling)
if(!require(psych)){install.packages("psych")}
if(!require(lmtest)){install.packages("lmtest")}
if(!require(boot)){install.packages("boot")}
if(!require(rcompanion)){install.packages("rcompanion")}
library(caTools)
library(ggplot2)
library(cowplot)
library(ROCR)
library(tidyverse)
library(broom)

#la préparation et la netoyage des différent parametres pour la regression logistique

data$sex = ifelse(data$sex== 2 ,0,1)
data$patient_type = ifelse(data$patient_type== 2 ,0,1)
data$intubed = ifelse(data$intubed== 2 ,0,1)
data$pneumonia = ifelse(data$pneumonia== 2 ,0,1)
data$pregnancy = ifelse(data$pregnancy== 2 ,0,1)
data$diabetes = ifelse(data$diabetes== 2 ,0,1)
data$copd = ifelse(data$copd== 2 ,0,1)
data$asthma = ifelse(data$asthma== 2 ,0,1)
data$inmsupr = ifelse(data$inmsupr== 2 ,0,1)
data$hypertension = ifelse(data$hypertension== 2 ,0,1)
data$other_disease = ifelse(data$other_disease== 2 ,0,1)
data$cardiovascular = ifelse(data$cardiovascular== 2 ,0,1)
data$obesity = ifelse(data$obesity== 2 ,0,1)
data$renal_chronic = ifelse(data$renal_chronic== 2 ,0,1)
data$tobacco = ifelse(data$tobacco== 2 ,0,1)
data$contact_other_covid = ifelse(data$contact_other_covid== 2 ,0,1)
data$covid_res = ifelse(data$covid_res== 1 ,1,0)
data$icu = ifelse(data$icu== 2 ,0,1)
data = na.omit(data[2:20])

View(data)

#division de la base pour deux parties (apprentissage et test)

spl = sample.split(data,SplitRatio = 0.8)
train = subset(data,spl == TRUE)
test = subset(data,spl == FALSE)


#le premier modele avec tout les variable de la base

model1 = glm(covid_res~sex+patient_type+intubed+pneumonia+age+pregnancy
             +diabetes+copd+asthma+inmsupr+hypertension+other_disease+cardiovascular+
               obesity+renal_chronic+tobacco+contact_other_covid+icu, family = binomial, data = train)

#resumer

summary(model1)

#la modification de modele 1 par la direction "backward"

step(model1,trace = TRUE,direction = "backward")


#le modele generer par la direction "backward"

model2 = glm(covid_res ~ intubed + pneumonia + age + diabetes + copd + 
               inmsupr + other_disease + cardiovascular + obesity + renal_chronic + 
               tobacco + contact_other_covid, family = binomial, data = train)
summary(model2)


#deuxieme modele avec l'elimination de tout les variables sauf beta0

model3 = glm(covid_res~1,data=train)

#la modification de modele 2 par la direction "forward"

step(model3,scope = ~.+sex+patient_type+intubed+pneumonia+age+pregnancy+diabetes+
       copd+asthma+inmsupr+hypertension+other_disease+cardiovascular+obesity+renal_chronic+
       tobacco+contact_other_covid+icu, direction = "forward", trace = TRUE)

#le modele generer par la direction "forward"

model4 = glm(covid_res ~ pneumonia + age + obesity + other_disease + 
               copd + renal_chronic + contact_other_covid + cardiovascular + 
               inmsupr + intubed + tobacco + diabetes + icu, data = data)
summary(model4)


#r_quard pour les deux modeles

nagelkerke(model2)

nagelkerke(model4)


#les coffetients des variables

exp(coefficients(model2))


predicted.data <- data.frame(
  probability.of.covid=model2$fitted.values,
  covid=train$covid_res)
predicted.data <- predicted.data[
  order(predicted.data$probability.of.covid, decreasing=FALSE),]
predicted.data$rank <- 1:nrow(predicted.data)

## Lastly, we can plot the predicted probabilities for each sample having
## covid and color by whether or not they actually had covid
ggplot(data=predicted.data, aes(x=rank, y=probability.of.covid)) +
  geom_point(aes(fill=train$covid_res),color="purple" , alpha=1, shape=4, stroke=2) +
  xlab("Index") +
  ylab("Predicted probability of getting covid")



#predict 50% si < 0.5 => 0 sinon => 1

res = predict(model2, test, type = "response")


#table de test avec le pourcentage de "acurracy"

(table(ActualValue=test$covid_res,PredictedValue=res > 0.5))
cat("acurracy is ",((1315+1713)*100)/(1315+1052+791+1713))
RocrPred=prediction(res,test$covid_res)
RocrPrefor=performance(RocrPred,"tpr","fpr")
plot(RocrPrefor,colorize=T,print.cuttofs.at=seq(0.1,by=0.02))


#Vérification de la linéarité entre le logit (sortie) et les autres variables 

theme_set(theme_classic())
probabilities <- predict(model2, type = "response")
mydata <- train %>%
  dplyr::select_if(is.numeric) 
predictors <- colnames(mydata)
mydata <- mydata %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)
ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")
#Calcule la distance de cook 
plot(model2, which = 4, id.n = 3)


#pour une nouvelle test

donn_test <- data.frame(intubed= 0,pneumonia= 0,age= 22,diabetes = 0,copd = 0,
                        inmsupr = 0,other_disease= 0,cardiovascular= 1,obesity=0 ,
                        renal_chronic= 0,tobacco= 0,contact_other_covid= 1)
predict(model2, donn_test, type = "response")
