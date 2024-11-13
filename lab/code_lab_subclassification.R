setwd("~/Dropbox/P8122/causal_inference_2021/week_7_8")


x <- read.table("./lab/gardasil.dat.txt", header = T)
head(x)

#Y: Completed
#A: PracticeType

x$PracticeType <- as.factor(x$PracticeType)

summary(x$PracticeType)
#0 pediatrics
#1 family practice 
#2 OB-GYN


x$PracticeType_bin <- as.numeric(as.numeric(x$PracticeType)>1)

#Association

reg <- glm(Completed~PracticeType_bin,data=x, family = binomial)
summary(reg)



#C: Age, Race, InsuranceType, MedAssist, LocationType
x$Race <- as.factor(x$Race)

#Propensity score

ps.model<-glm(PracticeType_bin~Age +  Race +   as.factor(InsuranceType) + as.factor(Location) ,data=x, family = binomial)
summary(ps.model)

x$ps <- predict(ps.model, type="response") #gets the propensity scores for each unit, based on the model


# install R package for visualization of overlap
attach(x)
library(personalized)
prop.func <- function(x, trt)
{
  # fit propensity score model
  propens.model <- glm(PracticeType_bin~Age +  as.factor(Race) +   as.factor(InsuranceType) + as.factor(Location) ,data=x, family = binomial)
  pi.x <- predict(propens.model, type = "response")
  pi.x
}

check.overlap(x = x,
              trt = x$PracticeType_bin,
              propensity.func = prop.func)


# now add density plot with histogram
check.overlap(x = x,
              trt = x$PracticeType_bin,
              type = "both",
              propensity.func = prop.func)

#eliminate non comparable cases and generate the analytic data

#eliminate controls for whom the P(A=1|C) is less that the min(P(A=1|C)) found in the treated group
min(ps[x$PracticeType_bin==1])
ps[which(x$PracticeType_bin==0)]<= min(ps[x$PracticeType_bin==1])

length(ps[which(x$PracticeType_bin==0)]<= min(ps[x$PracticeType_bin==1]))
#eliminate treated for whom the P(A=1|C) is greater that the max(P(A=1|C)) found in the control group
max(ps[x$PracticeType_bin==0])
ps[which(x$PracticeType_bin==1)]>= max(ps[x$PracticeType_bin==0])

length(ps[which(x$PracticeType_bin==1)]>= max(ps[x$PracticeType_bin==0]))

data = x[ps>=min(ps[x$PracticeType_bin==1]) & ps <= max(ps[x$PracticeType_bin==0]),] 
dim(x)
dim(data)


### refitting propensity score model
ps.model<-glm(PracticeType_bin~Age +  as.factor(Race) +   as.factor(InsuranceType) + as.factor(Location) ,data=data, family = binomial)
summary(ps.model)

data$ps <- predict(ps.model, type="response") #gets the propensity scores for each unit, based on the model
check.overlap(x = data,
              trt = data$PracticeType_bin,
              propensity.func = prop.func)
check.overlap(x = data,
              trt = data$PracticeType_bin,
              type = "both",
              propensity.func = prop.func)


library(tableone)
x$InsuranceType <-as.factor(x$InsuranceType)
x$Location <-as.factor(x$Location)
vars <- c("Age" , "Race", "InsuranceType" ,"Location")

## Construct a table
tabpresub <- CreateTableOne(vars = vars, strata = "PracticeType_bin", data = x, test = FALSE)

data$InsuranceType<-as.factor(data$InsuranceType)
data$Location<-as.factor(data$Location)
tabpresub_pret <- CreateTableOne(vars = vars, strata = "PracticeType_bin", data = x, test = FALSE)
tabpresub_postt <- CreateTableOne(vars = vars, strata = "PracticeType_bin", data = data, test = FALSE)

tabpresub
## Show table with SMD

print(tabpresub_pret, smd = TRUE)

print(tabpresub_postt, smd = TRUE)


data = x[ps>=(min(ps[x$PracticeType==1])-0.055) & ps <= (max(ps[x$PracticeType==0]+0.055)),] 
dim(x)
dim(data)
names(data)

ps.model<-glm(PracticeType_bin~Age +  Race +   as.factor(InsuranceType) + as.factor(Location) ,data=data, family = binomial)
summary(ps.model)

data$ps <- predict(ps.model, type="response") #gets the propensity scores for each unit, based on the model



################# SUBCLASSIFICATION ##########


#creating subclasses
subclass.breaks = quantile(ps, c(.20, .40, .60, .80)) # bins (initial try - modify as needed)
subclass = data$ps
subclass = as.numeric(data$ps>subclass.breaks[1])
subclass[which(data$ps>subclass.breaks[1]& data$ps<=subclass.breaks[2])]<- 1
subclass[which(data$ps>subclass.breaks[2]&data$ps<=subclass.breaks[3])]<- 2
subclass[which(data$ps>subclass.breaks[3])]<- 3
#looking at sample sizes within each subclass

#creating subclasses
subclass.breaks = quantile(data$ps, c(.25, .50, .65)) 
subclass = data$ps
subclass = as.numeric(data$ps>subclass.breaks[1])
subclass[which(data$ps>subclass.breaks[1]& data$ps<=subclass.breaks[2])]<- 1
subclass[which(data$ps>subclass.breaks[2]&data$ps<=subclass.breaks[3])]<- 2
subclass[which(data$ps>subclass.breaks[3])]<- 3


#looking at propensity scores within subclasses
prop.func <- function(x, trt)
{
  
  data$ps[which(data$ps <= subclass.breaks[1])]
}

check.overlap(x = data[which(data$ps <=subclass.breaks[1]),],
              trt = data$PracticeType_bin[which(data$ps <= subclass.breaks[1])],
              type = "both",
              propensity.func = prop.func)


prop.func <- function(x, trt)
{
 
  data$ps[which(data$ps>subclass.breaks[1]&data$ps<=subclass.breaks[2])]
}

check.overlap(x = data[which(data$ps>subclass.breaks[1]&data$ps<=subclass.breaks[2]),],
              trt = data$PracticeType_bin[which(data$ps>subclass.breaks[1]&data$ps<=subclass.breaks[2])],
              type = "both",
              propensity.func = prop.func)

prop.func <- function(x, trt)
{
  
  data$ps[which(data$ps>subclass.breaks[2]&data$ps<=subclass.breaks[3])]
}

check.overlap(x = data[which(data$ps>subclass.breaks[2]&data$ps<=subclass.breaks[3]),],
              trt = data$PracticeType_bin[which(data$ps>subclass.breaks[2]&data$ps<=subclass.breaks[3])],
              type = "both",
              propensity.func = prop.func)



 prop.func <- function(x, trt)
 {
   
   data$ps[which(data$ps>subclass.breaks[3])]
 }
 #data$ps <-ps
 check.overlap(x = data[which(data$ps>subclass.breaks[3]),],
               trt = data$PracticeType_bin[which(data$ps>subclass.breaks[3])],
               type = "both",
               propensity.func = prop.func)
 
 

 
#looking at sample sizes within each subclass
table(data$PracticeType_bin, subclass)

#looking at overall covariate balance
names(data)
X = data[, !(colnames(data) %in% c("PracticeType_bin","Completed","PracticeType","Shots"))]
head(X)
#X = subset.data(data, keep = c(VARIABLES TO KEEP))

vars <- c("Age" , "Race", "InsuranceType" ,"Location")

tab_s0 <- CreateTableOne(vars = vars, strata = "PracticeType_bin", data = data[which(subclass==0),], test = FALSE)
tab_s1 <- CreateTableOne(vars = vars, strata = "PracticeType_bin", data = data[which(subclass==1),], test = FALSE)
tab_s2 <- CreateTableOne(vars = vars, strata = "PracticeType_bin", data = data[which(subclass==2),], test = FALSE)
tab_s3 <- CreateTableOne(vars = vars, strata = "PracticeType_bin", data = data[which(subclass==3),], test = FALSE)

## Show table with SMD


print(tab_s0, smd = TRUE)
print(tab_s1, smd = TRUE)
print(tab_s2, smd = TRUE)
print(tab_s3, smd = TRUE)


#AVERAGE CAUSAL EFFECT WITHIN STRATA
ACE0 <- mean(data$Completed[which(subclass==0 & data$PracticeType==1)])-mean(data$Completed[which(subclass==0 & data$PracticeType==0)])
ACE1 <- mean(data$Completed[which(subclass==1 & data$PracticeType==1)])-mean(data$Completed[which(subclass==1 & data$PracticeType==0)])
ACE2 <- mean(data$Completed[which(subclass==2 & data$PracticeType==1)])-mean(data$Completed[which(subclass==2 & data$PracticeType==0)])
ACE3 <- mean(data$Completed[which(subclass==3 & data$PracticeType==1)])-mean(data$Completed[which(subclass==3 & data$PracticeType==0)])

ace <- (nrow(data[which(subclass==0),])/nrow(data))*ACE0+
  (nrow(data[which(subclass==1),])/nrow(data))*ACE1+
  (nrow(data[which(subclass==2),])/nrow(data))*ACE2+
(nrow(data[which(subclass==3),])/nrow(data))*ACE3


v01 <- var(data$Completed[which(subclass==0 & data$PracticeType==1)])
v00 <- var(data$Completed[which(subclass==0 & data$PracticeType==0)])

v11 <- var(data$Completed[which(subclass==1 & data$PracticeType==1)])
v10 <- var(data$Completed[which(subclass==1 & data$PracticeType==0)])

v21 <- var(data$Completed[which(subclass==2 & data$PracticeType==1)])
v20 <- var(data$Completed[which(subclass==2 & data$PracticeType==0)])

v31 <- var(data$Completed[which(subclass==3 & data$PracticeType==1)])
v30 <- var(data$Completed[which(subclass==3 & data$PracticeType==0)])


n0 <- nrow(data[which(subclass==0),])
n1 <- nrow(data[which(subclass==1),])
           n2 <- nrow(data[which(subclass==2),])
                      n3 <- nrow(data[which(subclass==3),])
                                                       
           n01 <- nrow(data[which(subclass==0& data$PracticeType==1),])
           n11 <- nrow(data[which(subclass==1& data$PracticeType==1),])
                                            n21 <- nrow(data[which(subclass==2& data$PracticeType==1),])
                                            n31 <- nrow(data[which(subclass==3& data$PracticeType==1),])
                                            n00 <- nrow(data[which(subclass==0& data$PracticeType==0),])
                                        
                                            n10 <- nrow(data[which(subclass==1& data$PracticeType==0),])
                                            n20 <- nrow(data[which(subclass==2& data$PracticeType==0),])
                                            n30 <- nrow(data[which(subclass==3& data$PracticeType==0),])
                                            
                                            varace <-(n1)^2/nrow(data)^2*((v11/n11)+(v10/n10))+
  
  (n2)^2/nrow(data)^2*((v21/n21)+(v20/n20))+
  (n3)^2/nrow(data)^2*((v31/n31)+(v30/n30))+
  (n0)^2/nrow(data)^2*((v01/n01)+(v00/n00))

sdace<-sqrt(varace)

CIL=ace-sdace*2
CIU=ace+sdace*2

print(CIL)
print(CIU)

###next we calculate a regural regression results adjusting for confounding
reg_adj <- glm(Completed~PracticeType_bin+ Age +Race +InsuranceType + LocationType,data=x, family = binomial)
summary(reg_adj)



