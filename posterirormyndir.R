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
g=ggplot(data,aes(l_m,fit))+geom_line()+theme_bw()+geom_point(data=data,mapping=aes(l_m,Q))+geom_line(mapping=aes(l_m,lower))+
geom_line(mapping=aes(l_m,upper))
