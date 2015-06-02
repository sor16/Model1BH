##figure 3

data=read.table('15.txt')
names(data)=c("W","Q")
data$Q=as.numeric(RC$y)
data$l_m=l_m
data$fit=X_m%*%mu
seq=seq(2000,20000,5)
quantyp1=yp1[,seq]
quantyp2=yp2[,seq]
quantyp3=yp3[,seq]
quantyp4=yp4[,seq]
quantmatrix=t(cbind(quantyp1,quantyp2,quantyp3,quantyp4))
prctile=t(apply(quantmatrix, 2, quantile, probs = c(0.025, 0.975),  na.rm = TRUE))
data$lower=prctile[,1]
data$upper=prctile[,2]
g=ggplot(data)+geom_line(mapping=aes(l_m,fit))+theme_bw()+geom_point(mapping=aes(l_m,Q))+geom_line(mapping=aes(l_m,lower),linetype="dashed")+
geom_line(mapping=aes(l_m,upper),linetype="dashed")

##figure 2
data$resid=(RC$y-X_m%*%mu)/sqrt(exp(t_m[2]))

f=ggplot(data)+geom_point(aes(l_m,resid))+geom_abline(intercept = 0, slope = 0)+
  geom_abline(intercept = 2, slope = 0,linetype="dashed")+geom_abline(intercept = -2, slope = 0,linetype="dashed")+ylim(-4,4)

