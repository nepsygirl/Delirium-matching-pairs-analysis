#Here are the things , that should be known before knowing the aspects of prepensity score. These are the proof that by ausing propensity score we may not reduce the subjects 
#tha may be important for us to determine who should receive the treatment 

POD_dataset <- read.csv("C:/Users/pushp/Dokoumente/BA/Logist/POD_dataset.csv")
#ADJUSTING REGRESSION
#adjust the exposure variable and demographics
#which of the demographics variable causing confounding is verry difficult to analyze! So its most fo the time is age!
#2*2 table
library(tableone)
require(tableone)
tab0 <- CreateTableOne(vars ="intervention",
                        data = POD_dataset,
                        strata = "POD")
print(tab0, showAllLevels = TRUE)


#adjust the exposure variable.What is the odds ratio that i got from outcome
#also without adjjsuting for variables i am getting p value less than 0,05
library(Publish)
library(prodlim)
fit0 <- glm(I(POD == "1")~intervention,
             family = binomial, data = POD_dataset)
require(Publish)
publish(fit0)

#adjust the exposure variable + demographics
#intervention is statiscally significant because p value is less that 0,05.#
#This means, wether or not a subjects recive the treatment, that impacts the risk
#of getting POD
#why adding adjustment variables ? so that i can find the relation between the 
# POD and interventon with the help of adjustment variables

library(prodlim)
library(Publish)
fit1 <- glm(I(POD == "1")~intervention + age + bloodpressure
            family = binomial , data = POD_dataset)
publish(fit1)

#adjust the exposure variable + adjustment variables.I want to calculate the adjustment , to know the Confounder?  (proove assumption 1) 
#odds Ratio give me the  the true effect of the intervention variable that was  of my primary interest 
library(prodlim)
library(Publish)
baselinevars <- c("age", "gender", "frailty", "bloodpressure" ,"surgery" , "anae_duration" , "clinic")
out.formula <- as.formula(paste("I (POD == 1)" , "~intervention +",
                                 paste(baselinevars,
                                       collapse = "+")))
out.formula
fit2 <- glm(out.formula , family = binomial , data = POD_dataset)
publish(fit2)
 
#how to see weather we are satisfied with the assumption or not. There are couple of ways. 
#check diagnostics plot.
plot(fit2 , which = 1)
#how to check with the help of the plot now #this picture is not so clear
plot(fit2 , which = 3) # this plot  is not so helpful for me to determine either my Assumptions holds or not. I mean , when we work in real dataset , we might nic tsee 
#the actual relationship between predicted values and variables. 
#IF i am running a regression just adding all the covariates , I am not considering model-specification terms like the interaction  , Polynomial terms which could 
#be useful to determine the true relationship of Outcome of interest.That implies , we may not get the true relationship.

#MAin target ::: To find out the true model that holds the Assumptions

#Motivation: PS Model use only one variable  to match, so with that may be able to find out the true relationship of outcome of interest.Above Code are not from PS Model
#this is just to show, that the output are not that much satisfied to predict the treatment effect
#So from here i am starting the PS Model.

#i should do many matchigns hier!!not using ps score , instead just using many covariats , and show that we may loose many subjects!!!So PS is the best way to do



#PS Score 
#Step 1 : Specify the propensity score model to estimate prpensity scores, and fit the model. Which of the variables should i have to use in PS Model?
#We are fitting a regreesion,to get a probablity of being exposed to treatment 
ps.formula <- as.formula(paste("I(intervention == '1')" , "~" ,
                               paste(baselinevars, collapse = "+")))
ps.formula #Prediction will be done at the last.PS.formula is just our model. We are definfing our model first
#coef of PS Model fit in not concern 
#Model can be rich 
#While PS has balancing property, PS is unknown and needs to be estimatedd
#fit logistics regression to estimate the propensity score
PS.fit <- glm(ps.formula , family ="binomial" ,
               data = POD_dataset)
#extract estimated propensity scores from the fit
POD_dataset$PS <- predict(PS.fit,
                           newdata = POD_dataset , type = "response")
plot(POD_dataset$PS)
 
 #summarize PS Score
 summary(POD_dataset$PS)
 #summarize PS by exposure group
 tapply(POD_dataset$PS, POD_dataset$'intervention',summary)
 
 #So hier i got the PS score, but i am not sure about the Covriates wether that could the best Covariates that has effect on both the OutCome and Teatment
 #trying to estimate the ps score. But But  the best  balance is the ultimate goal for PS Score!!!!
 #onething to remember. When i am using the PS 

 #Step 2 : PS Matching
 #Calculating Threshold for Runnig a matching regression
 logitPS <- log(1/POD_dataset$PS - 1)
 #logit of the PS Score
 .2*sd(logitPS)#suggested in the literature. We are determining the 2 standard deaviateion of logit of PS Score 
 #since PS is continous variable, 1:1 variable is not that much suitable one. Because finding exact one can be difficult.We may not find the sufficient mathching 
 #because as per my distribution treted PS median are far different than control PS.So exact might not work here.
 
 #Since using 2 sd deviation , my threshold is too high , so i am using the 1 standard deaviation so that my threshold will  be less
 
0.1*sd(logitPS) #We are using this.These are the cutpoint(threshhold) using 1 standard deaviation of PS Score, because the erson with PS of 0,1 should 
#match with the person having PS 0,9. So need threshold.suggested from the literature first 
#chossing too strict PS has unintendend consequeces.
#using 1 sd , my theshold is 0,10342. That means , if one person ofcontrol has the PS score of 0,1 and another person of the treted group has the PS score of the 
#0,19. These pairs should be considerd as the pair having saming ps 



#Step 2: to do match---> slecting the best match
#MAtch using estimates propensity score 
#nearest -neighbor matching-->it would simply selct one and delete other like in the pictur.
#without repalcement 
#with caliper = 1*SD logit of propensity score 
#with 1:1 ratio(pair matching)

#it is very essential for us to set the seed so that it reproduce my own result 
#set seed (value) where value specifies the initial value of the random number seed. Syntax: set.seed(123) 
# the above line,123 is set as the random number value. The main point of using the seed is to be able to reproduce a
# particular sequence of 'random' numbers.

#method nearest by using dsitance(threshold)
set.seed(123)
library("MatchIt")
match.obj<-matchit(formula = ps.formula,data = POD_dataset,
                     distance = 'logit',
                     method = "nearest" , replace = FALSE,
                     caliper = .1*sd(logitPS),ratio = 1)
#see matchit funktion option hier 
#httpst:/www.rdocumentation.org/packages/MatchIT/versions/1.0-1/topics/matchit
POD_dataset$PS <- match.obj$distance
summary(match.obj$distance)
match.obj

#method nearest by not using distance(threshold)
#set.seed(123)
library("MatchIt")
match.obj<-matchit(formula = ps.formula,data = POD_dataset,
                   distance = POD_dataset$PS,    #using estiamated PS and insert in matchit funktion
                   method = "nearest" , replace = FALSE,
                   caliper = .1*sd(logitPS),ratio = 1)
#see matchit funktion option hier 
#httpst:/www.rdocumentation.org/packages/MatchIT/versions/1.0-1/topics/matchit #
POD_dataset$PS <- match.obj$distance
summary(match.obj$distance)
match.obj
summary(match.obj)




#method exact by not using the thershhold point
library("MatchIt")
data("POD_dataset", package = "MatchIt")
match.obj <- matchit(formula = ps.formula,data = POD_dataset, method = "exact")
summary(match.obj)
#plot(summary(m.out))

#method exact by not using the thershhold point
library("MatchIt")
data("POD_dataset", package = "MatchIt")
match.obj <- matchit(formula = ps.formula,data = POD_dataset, method = "full")
summary(match.obj)
#matched(ESS) for effective sample size.

#Taking a closer look at the matches 
#Ref : https:/lists.gking.harvard.edu/pipermail/mathcit/2013-October/000559.html
#the distribution is exaclty similar looking , not the same one.
#i tried with the second one

library("MatchIt")
matches <- as.data.frame(match.obj$match.matrix)
colnames(matches) <- c("matched_unit")
matches$matched_unit <- as.numeric(
  as.character(matches$matched_unit))
matches$treated_unit <-as.numeric(rownames(matches))
matches.only <- matches[!is.na(matches$matched_unit),]
head(matches.only)
POD_dataset[POD_dataset$id  %in%
              as.numeric(matches.only[1,]),]

#this is the third pair 
library("MatchIt")
matches <- as.data.frame(match.obj$match.matrix)
colnames(matches) <- c("matched_unit")
matches$matched_unit <- as.numeric(
  as.character(matches$matched_unit))
matches$treated_unit <-as.numeric(rownames(matches))
matches.only <- matches[!is.na(matches$matched_unit),]
head(matches.only)
POD_dataset[POD_dataset$id  %in%
              as.numeric(matches.only[3,]),]

#this is the fifth pair
library("MatchIt")
matches <- as.data.frame(match.obj$match.matrix)
colnames(matches) <- c("matched_unit")
matches$matched_unit <- as.numeric(
  as.character(matches$matched_unit))
matches$treated_unit <-as.numeric(rownames(matches))
matches.only <- matches[!is.na(matches$matched_unit),]
head(matches.only)
POD_dataset[POD_dataset$id  %in%
              as.numeric(matches.only[5,]),]
                         
#In regression, "multicollinearity" refers to predictors that are correlated with other predictors
# Multicollinearity occurs when your model includes multiple factors that are correlated not just to your response variable, but also to each other

#Assesing balance and overlap
#package plyr and ggplt2
boxplot(PS~ intervention , data = POD_dataset,
        lwd =2 , ylab = 'PS')
stripchart(PS~ intervention == '1',vertical = TRUE,
           data = POD_dataset, method = "jitter",
           add = TRUE , pch = 20 , col =' blue')

#Vizualization for assesing overlap issues after matching
plot(match.obj , type = "hist")

#Step 3: Most important thing wether the matching that i have done using nearest method.skeleton(
#was good enough or not.

#Assessment of Balance :Better than regression model.Matching should be successfull by
#the criteria of ASMD
#Compare the similarity of baseline chaacteristics between treated and untreated subjecs in the prpensity scorre-matche
#sample
#in this case, we compare SMD <0,1. 0,1 is made up point suggested in many paper 
#in some literature, other generous values (0,25) are proposed to see wether the assembly less than 0,1 or not.
#only in the matched data not in the whole data 
library(tableone)
library(MatchIt)
library(vctrs) #installed whenever error
matched.data <- match.data(match.obj)
tablm <- CreateTableOne(vars = baselinevars,
                 data = matched.data, strata = "intervention",    #doing only in the mathed data not the wholedata
                 includeNA = TRUE,
                 test = TRUE, smd = TRUE)

print(tablm, showAllLevels = FALSE,smd = TRUE, test = FALSE)

smd.res <- ExtractSmd(tablm)
t(round(smd.res,2)) 
#this for only the second pair.The assembly less than 0,1 is hier 
#succesfull. That means assesment of balance using the smd less than 0,1  is succcessful by the criteria of 
#smd.Because evry covariate has smd less than 0,1

library(tableone)
library(vctrs) #installed whenever error
matched.data <- match.data(match.obj)
tablm <- CreateTableOne(vars = baselinevars,
                        data = matched.data, strata = "intervention",    #doing only in the mathed data not the wholedata
                        includeNA = TRUE,
                        test = TRUE, smd = TRUE)

print(tablm, showAllLevels = FALSE, smd = TRUE, test = FALSE)

smd.res <- ExtractSmd(tablm)
t(round(smd.res,5)) #smd estimates the amount by which the experimental intervention changes 
#the outcome on average compared with the control.


#Compare Similarity of baseline Characteristics between treated and untreated
#subjects in the matched sample
#in this case,i compare SMD<0,1 or not
#in some literature,other generous values (0,25) purposed
#this for only the third pair.The assembly less than 0,1 is hier 
#succesfull. That means assesment of balance using the smd less than 0,1  is succcessful by the criteria of 
#smd.Because evry covariate has smd less than 0,1


#p value is not relevant hier 
# to check assesemnent by the critera of Variance ratio
#variance ratios ~ 1 means 
#eual variances in groups 
#group balance
#could vary from 1/2 to 2
#other cutpoints are suggested asa well as (0,8 to 1,2)
#Rubin suggested very strict cutpoint 

library(cobalt) # to get the variance ratio
require(cobalt)
batlab.res <- bal.tab(x = match.obj , data = POD_dataset,
                      treat = POD_dataset$intervention,
                      disp.v.ratio = TRUE)
batlab.res$Balance$V.Ratio.Adj

#i did balance asssesment after matching in two diff criteria , one is ASMD , and the other is Variance Ratio
#Many Researchhers suggested using ASMD to chack the balance assesment.




#Step 4: outcocome modeling 

#Some flixibilty in choosing outcome model
     #-considered  independent of exposure modeling 
     #some purpose doubt robust approach
    #adjusting imbalance covoriates only?

#Estimate the effect of treatment on outcomes using propensity score-matched sample
#The average treatment effect can be expressed in logits, as an odds ratio, 
#as relative risk, or as risk difference. 

#Odds ratios provide a reasonable approximation of the relative risk
##odds Ratio give me the  the true effect of the intervention variable on an outcome
#which is my primary interest 

library(Publish)
library(prodlim)
fit3 <- glm(I(POD == "1") ~ intervention,
            family = binomial , data = matched.data)
publish(fit3)

#I am adding  more covariates that i think that it is associated with the outcome aswell
#these covariates doesnot have to be exaclty the same covariates that i have
#put in my exposure modeling, however some of the literature suggested,i should 
#put the same variable that i put in my treatment modelling. But 
#generally i should adjust the variable that is associated with the outcome


library(Publish)
library(prodlim)
out.formula <- I(POD == "1") ~ intervention + anae_duration + gender + age + bloodpressure + frailty + complication
            
fit3b <- glm(out.formula,
            family = binomial , data = matched.data)
summary(fit3b)
publish(fit3b)

#this for the whole data -> just to compare

library(Publish)
library(prodlim)
out.formula <- I(POD == "1") ~ intervention + anae_duration + gender + age + bloodpressure + frailty + complication

fit3c <- glm(out.formula,
             family = binomial , data = POD_dataset)
summary(fit3c)
publish(fit3c)


#A
#in regression adjusted method also we get the similar odds ratio
#then why are we doing all these complicated methods i.e using propensity 
#score matching method.

#nobody is claiming that propensity score is better than the regression method
#but using propensity score modeling method there is good checks and balance
#mechanism. In the propensity score i am checking balance , overlap. But 
#here i can actually identify  the problem and i can take appropriate step to 
#to analyse that problem. 

#in normal regression adjusted method, we just predict the value that we got from OR.
#no facility to check and balance 

#REPORTING GUIDELINE
#How variables selected 
   #why do i choose these variables?
#Model Selection
   #logistic vs machine learning
#Overlap vs Balance
     #numeric and visual

#we dont need weight

#Propensity score was calculated based on a fitting a regression model
# with the key risk factors of some covariates.
#ps score-> for subject 2 -> 16% chances that subject 2 is receiving the treatment 

#impact of intervention on an outcome 
#complication shows 9.17 that means odds ratio of 9.17 shows a much a stronger 
#association with the outcome.


#the intercept which is the mean of control group



 

 
 
 













 