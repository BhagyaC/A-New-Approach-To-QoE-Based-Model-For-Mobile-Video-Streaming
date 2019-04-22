#generalised linear modeling considering logit function as a link function
# step 1: finding the over all user experience on the basis of collected mos of each parameter
try1<-testobj1
i<-0
k<-0
for (k in 1:2) {
  for (i in 1:5) {
    testobj1[k,i]<-testobj1[k,i]*testsub[1,i]
  }
}
testobj1
i<-0
k<-0
for (k in 1:2) {
  for (i in 1:5) {
    testobj1[k,i]<-testobj1[k,i]/testsub[1,4]
  }
}
testobj1
tmul1<-0
tmul2<-0
for (k in 1:5) {
    tmul1<-tmul1+testobj1[1,k]
    tmul2<-tmul2+testobj1[2,k]
}
ttmul1<-0

ttmul2<-0

for (k in 1:5) {
  ttmul1<-ttmul1+try1[1,k]
  ttmul2<-ttmul2+try1[2,k]
}
#the user experience calculated added to the objective matrices 
xx<-tmul1/ttmul1
yy<-tmul2/ttmul2
try1$usrexp<-0
try1[1,6]<-xx
try1[2,6]<-yy
#modeling using glm is done for each parameter seperatly and the final result is averaged for the entire model prediction
#modeling and prediction of sr
mlasts<- glm(formula= usrexp ~ sr,data =try1, family=binomial(link = "logit"))
summary(mlasts)
mlasts$coefficients
coeftabsr<-coefficients(mlasts)
coeftabsr
coeftabsr[1]
betasr0<-coeftabsr[1]
betasr1<-coeftabsr[2]
#the table predc contains the objective value for sr whoes user expectation have to be predicted
foprsr<-betasr0+betasr1*pred[1,1]
predfsr<-exp(foprsr)/(1+exp(foprsr))
predfsr
#modeling and prediction for ir
mlastir<- glm(formula= usrexp ~ ir,data =try1, family=binomial(link = "logit"))
summary(mlastir)
mlastir$coefficients
coeftabir<-coefficients(mlastir)
coeftabir
coeftabir[1]
betair0<-coeftabir[1]
betair1<-coeftabir[2]
#the table predc contains the objective value for ir whoes user expectation have to be predicted
foprir<-betair0+betair1*pred[1,2]
pred[2,2]
predfir<-exp(foprir)/(1+exp(foprir))
predfir
#modeling and prediction for abe
mlastabe<- glm(formula= usrexp ~ abe,data =try1, family=binomial(link = "logit"))
summary(mlastabe)
mlastabe$coefficients
coeftababe<-coefficients(mlastabe)
coeftababe
coeftababe[1]
betaabe0<-coeftababe[1]
betaabe1<-coeftababe[2]
#the table predc contains the objective value for abe whoes user expectation have to be predicted
foprabe<-betaabe0+betaabe1*pred[1,3]
predfabe<-exp(foprabe)/(1+exp(foprabe))
predfabe
#modeling and prediction for vci
mlastvci<- glm(formula= usrexp ~ vci,data =try1, family=binomial(link = "logit"))
summary(mlastvci)
mlastvci$coefficients
coeftabvci<-coefficients(mlastvci)
coeftabvci
coeftabvci[1]
betavci0<-coeftabvci[1]
betavci1<-coeftabvci[2]
#the table predc contains the objective value for vci whoes user expectation have to be predicted
foprvci<-betavci0+betavci1*pred[1,4]
predfvci<-exp(foprvci)/(1+exp(foprvci))
predfvci
#modeling and prediction for bsr
mlastbsr<- glm(formula= usrexp ~ bsr,data =try1, family=binomial(link = "logit"))
summary(mlastbsr)
mlastbsr$coefficients
coeftabbsr<-coefficients(mlastbsr)
coeftabbsr
coeftabbsr[1]
betabsr0<-coeftabbsr[1]
betabsr1<-coeftabbsr[2]
#the table predc contains the objective value for bsr whoes user expectation have to be predicted
foprbsr<-betabsr0+betabsr1*pred[1,5]
predfbsr<-exp(foprbsr)/(1+exp(foprbsr))
predfbsr

#so the user expectation for every parameter is predicted using the model now the over all user expectation can be calculated using weighted mean
ovrexpb<-predfsr*testsub[1]+predfir*testsub[2]+predfabe*testsub[3]+predfvci*testsub[4]+predfbsr*testsub[3]
tet<-predfabe+predfir+predfsr+predfbsr+predfvci
tet
ovrexp<-ovrexpb/tet
#hence the over all expected user expectation is predicted
ovrexp
#so the user expectation for every parameter is predicted using the model now the over all user expectation can be calculated using mean and then ranging it to a scale of 1-5
mn<-tet/5
rmn<-1+4*(mn)
#hence the over all expected user expectation is predicted
rmn

