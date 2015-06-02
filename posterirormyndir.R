##figure 3 rating curve logscale with conf interval

data=data.frame(W=0.01*RC$w, Q=RC$y)
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
rclog=ggplot(data)+geom_line(mapping=aes(l_m,fit))+theme_bw()+geom_point(mapping=aes(l_m,Q))+geom_line(mapping=aes(l_m,lower),linetype="dashed")+
geom_line(mapping=aes(l_m,upper),linetype="dashed")

##figure 2 residual log scale
data$residlog=(expRC$y-X_m%*%mu)/sqrt(exp(t_m[2,]))

rcleiflog=ggplot(data)+geom_point(aes(l_m,residlog),color="red")+theme_bw()+geom_abline(intercept = 0, slope = 0)+
  geom_abline(intercept = 2, slope = 0,linetype="dashed")+geom_abline(intercept = -2, slope = 0,linetype="dashed")+ylim(-4,4)+
  ylab(expression(epsilon[i]))

##figure 6 rating curve real scale with conf interval
rcreal=ggplot(data)+theme_bw()+geom_point(aes(exp(Q),W,col))+geom_line(aes(exp(fit),W))+geom_line(aes(exp(lower),W),linetype="dashed")+
  geom_line(aes(exp(upper),W),linetype="dashed")

##residual realscale
data$residraun=(exp(RC$y)-exp(X_m%*%mu))/sqrt(exp(t_m[2,]))
rcleifraun=ggplot(data)+geom_point(aes(exp(l_m),residraun),color="red")+theme_bw()+geom_abline(intercept = 0, slope = 0)+
  geom_abline(intercept = exp(2), slope = 0,linetype="dashed")+geom_abline(intercept = -exp(2), slope = 0,linetype="dashed")+
  ylim(-8,8)+ylab(expression(epsilon[i]))

