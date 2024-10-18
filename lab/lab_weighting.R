##############################
#Propensity Score Weighting  
##############################
library(survey)
library("MatchIt")
data("lalonde")
?lalonde

boots <- 1000
b.holder1 <- rep(NA)
b.holder2 <- rep(NA)
b.holder3 <- rep(NA)

n<-nrow(lalonde)

for (i in 1:boots) {
  S.b <- sample(1:n, size = n, replace = TRUE)
  data.b <- lalonde[S.b, ]
pprobs <- predict(glm(treat ~ age + educ + as.factor(race) + nodegree + married + re74 + re75,data=data.b,family=binomial(link = "logit")),type="response")
est.w <- ifelse(data.b$treat == 1, 1/pprobs, 1/(1 - pprobs))

psw1 <- svyglm(re78 ~ treat + age + educ + as.factor(race) + nodegree + married + re74 + re75 + 
                         I(age^2) + I(educ^2) + re74 + nodegree:re74,design = svydesign(~ 1, weights = ~ est.w,data=data.b))
#summary(psw1)
b.holder1[i]<-psw1$coefficients[2]

pprobs <- predict(glm(treat ~ age + educ + as.factor(race) + nodegree + married + re74 +re75
                      + I(age^2) + nodegree:re74,data=data.b,family=binomial(link = "logit")),type="response")
est.w <- ifelse(data.b$treat == 1, 1/pprobs, 1/(1 - pprobs))

psw2 <- svyglm(re78 ~ treat + age + educ +as.factor(race) + nodegree + married + re74 + re75 + 
                 I(age^2) + I(educ^2) + re74 + nodegree:re74,design = svydesign(~ 1, weights = ~ est.w,data=data.b))
#summary(psw2)
b.holder2[i]<-psw2$coefficients[2]

psw3 <- svyglm(re78 ~ treat,design = svydesign(~ 1, weights = ~ est.w,data=data.b))
#summary(psw2)
b.holder3[i]<-psw3$coefficients[2]

}

#conditional effects
mean(b.holder1)
mean(b.holder2)
#marginal effect
mean(b.holder3)

quantile(b.holder1,probs = c(0.025,0.975))
quantile(b.holder2,probs = c(0.025,0.975))
quantile(b.holder3,probs = c(0.025,0.975))

