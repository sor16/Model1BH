##figure 3 rating curve logscale with conf interval

data=data.frame(W=RC$w,Q=RC$y)
data$l_m=l_m
data$fit=X_m%*%mu

simdata=data.frame(fit=as.matrix(seq(min(X_m%*%mu),max(X_m%*%mu),length.out=1000)))
simdata$fitl_m = as.matrix(seq(min(data$l_m),max(data$l_m),length.out=1000))
simdata$Wfit=as.matrix(seq(min(log(data$W)),max(log(data$W)),length.out=1000))                   
seq=seq(2000,20000,5)
quantypo1=ypo1[,seq]
quantypo2=ypo2[,seq]
quantypo3=ypo3[,seq]
quantypo4=ypo4[,seq]
quantmatrix=t(cbind(quantypo1,quantypo2,quantypo3,quantypo4))
prctile=t(apply(quantmatrix, 2, quantile, probs = c(0.025, 0.975),  na.rm = TRUE))
data$lower=prctile[,1]
data$upper=prctile[,2]

rclog=ggplot(data)+geom_line(data=simdata, mapping=aes(fitl_m,fit))+theme_bw()+geom_point(mapping=aes(l_m,Q))+geom_line(mapping=aes(l_m,lower),linetype="dashed")+
geom_line(mapping=aes(l_m,upper),linetype="dashed")

##figure 2 residual log scale
data$residlog=(data$Q-data$fit)/sqrt(exp(t_m[2,]))

rcleiflog=ggplot(data)+geom_point(aes(l_m,residlog),color="red")+theme_bw()+geom_abline(intercept = 0, slope = 0)+
  geom_abline(intercept = 2, slope = 0,linetype="dashed")+geom_abline(intercept = -2, slope = 0,linetype="dashed")+ylim(-4,4)+
  ylab(expression(epsilon[i]))

##figure 6 rating curve real scale with conf interval
rcraun=ggplot(data)+theme_bw()+geom_point(aes(exp(Q),exp(l_m)))+geom_line(data=simdata, aes(exp(fit),exp(fitl_m)))+geom_line(aes(exp(lower),exp(l_m)),linetype="dashed")+
  geom_line(aes(exp(upper),exp(l_m)),linetype="dashed")

##residual realscale
data$residraun=(exp(data$Q)-exp(data$fit))/sqrt(exp(t_m[2,]))
rcleifraun=ggplot(data)+geom_point(aes(exp(l_m),residraun),color="red")+theme_bw()+geom_abline(intercept = 0, slope = 0)+
  geom_abline(intercept = exp(2), slope = 0,linetype="dashed")+geom_abline(intercept = -exp(2), slope = 0,linetype="dashed")+
  ylim(-8,8)+ylab(expression(epsilon[i]))

