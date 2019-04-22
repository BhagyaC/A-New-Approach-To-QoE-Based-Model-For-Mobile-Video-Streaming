#loading data and setting variables
testdata<-test
exp<-testdata$Experience
rat<-testdata$rating
tot<-0
summ<-0
#calculating total no of levels(MOS levels)
num<-nrow(testdata)
#estimating the total fraction of support
for (i in 1:num) {
  tot<-tot+rat[i]
}
tot
#calculating the user expectation(alpha)
for (i in 1:num) {
  summ<-summ+rat[i]*exp[i]
}
alpo=summ/tot
#taking the ceiling value as reference in our model
alpn=ceiling(alpo)
#calcutating the difference btw ceiling value and user expectation and later used in error correction
kk<-alpn-alpo
#the numerical rating is converted to categorical before multinominal regression
testdata$fact<-factor(testdata$Experience)
#defining the reference as the alpha and also considering reference as alpha-1 later will be used in error correction part
testdata$out<-relevel(testdata$fact, ref = alpn)
testdata$out1<-relevel(testdata$fact, ref = alpn-1)
library(nnet)
#exectuting multinominal logistic regression for both alpha and alpha-1 and obtaining intercepts for both reference level
mymodel<-multinom(out~rat,data = testdata)
summary(mymodel)$coefficients
mymodell<-multinom(out1~rat,data = testdata)
summary(mymodell)$coefficients
#taking the difference between the intecepts and fraction of support obtained for both aplha and alpha-1(error correction)
k11<- (summary(mymodel)$coefficients-summary(mymodell)$coefficients)
View(k11)
#the values obtained by regression is atomic for mathematical operation in error correction part converting it to recursive
is.recursive(k11)
ktest<-as.data.frame(k11)
#executing data correction
ktest<-(ktest/100)*kk
#feeding the corrected values back to the model
mymodel$coefficients<-ktest

mymodel$coefficients
#predict the values with error correction
predict(mymodel,testdata,type = "prob")
#estimating confustion matrix to compare prediction of model with actual data
cm<-table(predict(mymodel),testdata$fact)
print(cm)
#chi-square analysis with null hypothesis objective matrices are dependent to subjective parameters and considering significant level as .05 
#if the p value is greater than the significant level the null hypothesis is accepted
chisq.test(test)
