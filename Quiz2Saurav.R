##############Question 1###############################

BNK = read.csv(file.choose())
BNK$Experience = BNK$YrsPrior + (95 - BNK$YrHired)
plot(BNK$Salary, BNK$Experience)
bnkmodel = lm(Salary ~ Experience, data=BNK)
newEmp = data.frame(Experience=20)
newSal = predict(bnkmodel, newEmp)
par(mfrow=c(2,2))
plot(bnkmodel)
predict(bnkmodel, newEmp, interval = "confidence", level = 0.95)

BNK$newSal = (BNK$Salary - 40000)
newSal.Boxcox = caret::BoxCoxTrans(BNK$newSal)
print(newSal.Boxcox)
BNK = cbind(BNK, "newSalboxcox" = predict(newSal.Boxcox, BNK$Salary))
lm.boxcox = lm(newSal.boxcox ~ Experience, data=BNK)
prednewEmp = predict(lm.boxcox, newEmp)
invBoxCox = function(x, lambda)
  if(lambda == 0) exp(x) else (lambda*x + 1)^(1/lambda)

predSal = invBoxCox(prednewEmp,0.3)

####################Question 2################################

cereal = read.csv(file.choose())
cereal = cereal[, -1]
pc = princomp(cereal, cor = TRUE)
pc$loadings[,1:3]

###################Question3################################

wine = read.csv(file.choose())
wine$QGoodBad = ifelse(wine$quality >= 6, 1, 0)
set.seed(1234)
wine = wine[sample(nrow(wine)),]
wine$QGoodBad = as.factor(wine$QGoodBad)
wine.training = wine[1:3900,]
wine.testing = wine[3901:4900,]
head(wine.training)
head(wine.testing)
lrmodel = glm(QGoodBad ~ ., family="binomial", data=wine.training)
wine.pred = predict(lrmodel, wine.testing, type = "response")
summary(lrmodel)
mean(wine.pred == wine.test$QGoodBad)
NB = naiveBayes(QGoodBad ~ ., data = wine.training)
NB.pred = predict(NB, wine.testing)
table(NB.pred, wine.testing$QGoodBad)
mean(NB.pred == wine.test$QGoodBad)



#######################Question 4#################################

credit = read.csv(file.choose())
head(credit)
set.seed(9500)
credit = credit[sample(nrow(credit)),]
credit$risk = NULL
bal = credit$Balance
low = quantile(bal,0.2)
mid = quantile(bal, 0.8)

for(i in 1:400)
{
  if(credit$Balance[i] > mid)
  {
    credit$risk[i] = "High"
  }
  else if(credit$Balance[i] > low && credit$Balance[i] <= mid)
  {
    credit$risk[i] = "Middle"
  }
  else if(credit$Balance[i] <= low)
  {
    credit$risk[i] = "Low"
  }
}
table(credit$risk)

credit = data.frame(credit)
credit$risk = as.factor(credit$risk)
credit.training = credit[1:300,]
credit.test = credit[301:400,]

NB = naiveBayes(risk ~ ., data = credit.training)
summary(NB)
NBpred = predict(NB, newdata = credit.test)

table(NBpred, credit.test$risk)
mean(NBpred == credit.test$risk)

###################Question5############################################

catalog = read.csv(file.choose())
lm1 = lm(AmountSpent ~ ., data = catalog)
summary(lm1)

#removing Customer Column
catalog = catalog[,-1]
lm2 = lm(AmountSpent ~ ., data = catalog)
summary(lm2)

#removing Married
catalog = catalog[,-3]
lm3 = lm(AmountSpent ~ ., data = catalog)
summary(lm3)
#removing Age
catalog = catalog[,-1]
lm4 = lm(AmountSpent ~ ., data = catalog)
summary(lm4)

#removing OwnHome
catalog = catalog[,-8]
lm5 = lm(AmountSpent ~ ., data = catalog)
summary(lm5)

#removing Gender
catalog = catalog[,-1]
lm6 = lm(AmountSpent ~ ., data = catalog)
summary(lm6)

#removing PrevSpent
catalog = catalog[,-4]
lm7 = lm(AmountSpent ~ ., data = catalog)
summary(lm7)

#removing PrevCust
catalog = catalog[,-3]
lm8 = lm(AmountSpent ~ ., data = catalog)
summary(lm8)

#removing Children
catalog = catalog[,-2]
lm9 = lm(AmountSpent ~ ., data = catalog)
summary(lm9)

###################################Question 6##########################


razor = read.csv(file.choose())
razor$logQuantity = log(razor$Quantity)
expmodel = lm(logQuantity ~ Price, data = razor)
summary(expmodel)

newrazor = data.frame(Price = 2.75)
predict(expmodel, newrazor)
