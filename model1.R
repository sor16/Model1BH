library(stats)
library(ggplot2)

Nit=20000
dataset=15

#Prior Parameters
RC=list()
RC$mu_a=3.20;
RC$mu_b=2.29;
RC$sig_a=sqrt(1.21);
RC$sig_b=sqrt(0.48);
RC$p_ab=-0.61;
RC$Sig_x=rbind(c(RC$sig_a^2, RC$p_ab*RC$sig_a*RC$sig_b), c(RC$p_ab*RC$sig_a*RC$sig_b, RC$sig_b^2))

#RC.mu_x=[RC.mu_a RC.mu_b]';
#Breyting fra Matlab koda, bylt (') verdur t()

RC$mu_x=as.matrix(c(RC$mu_a, RC$mu_b))

#inv() i matlab er solve i R

RC$Sig_xinv=solve(RC$Sig_x);
RC$Lx=chol(RC$Sig_x);
RC$mu_c=1.9;
RC$Sinvmu=RC$Sig_xinv%*%RC$mu_x;



# axel/begin/26.05.15



#%import data from text file that has water level measurements in cm in left
#%column and corresponding discharge measurements in m^3/s in right column

#axel: 

wq = as.matrix(read.table('15.txt'))

#wq=importdata([num2str(dataset) '.txt']);

#Sölvi pæling
#qwdata=read.table('15.txt')
#names(qwdata)=c("W","Q")
#qwdata$Qlog=log(qwdata$Q)
#RC$qwdata=qwdata
################
RC$y=as.matrix(log(wq[,2]));
RC$w=0.01*wq[,1]; #to meters 
RC$w_tild=RC$w-RC$w[1];
RC$n=length(RC$y);

#axel/end/26.05.15/virkar


#axel/begin/27.05.15


#axel: teiknar upp punktana Q á móti H
# figure(1);hold off;
# scatter(RC.w,wq(:,2),'.');

H=RC$w
Q=wq[,2]
dat=data.frame(H,Q)


ggplot(dat,aes(x=H,y=Q))+geom_point(shape=1)+theme_bw()

#axel/end/27.05.15/virkar

#axel/begin/28.05.15


#Dens =@(t)-DensEvalm11(t,RC);
Dens <- function(th){ Denseval11(th,RC)$pmin}
Densmin=optim(par=c(0,0),Dens,hessian=TRUE)
Densmin$value=t_m 
Densmin$value=H

#axel/begin/28.05.15

# 
# 
# [t_m,~,~,~,~,H]=fminunc(Dens,zeros(2,1));
# 
# 
l_m=as.matrix(log(RC$w_tild+exp(t_m[1]))); #samanburdur stodst

X_m=cbind(matrix(1,nrow(l_m),ncol(l_m)),l_m); #samanburdur stodst

L=t(chol(RC$Sig_xinv+t(X_m)%*%X_m/exp(t_m[2]))); #samanburdur stodst

mu=solve(t(L),(solve(L,(RC$Sinvmu+t(X_m)%*%RC$y/exp(t_m[2]))))); #samanburdur stodst


# hold on

plot(RC$w,exp(X_m*mu)); #axel: nota ggplot2? 


v_temp=X_m%*%solve(RC$Sig_xinv+t(X_m)%*%X_m/exp(t_m[2]))%*%t(X_m) #samanburdur stodst


varappr=as.matrix(diag(v_temp)+exp(t_m[2])); #samanburdur stodst
                   
                   #axel/end/28.05.15

#              %[norminv(0.025,0,sqrt(varappr)) norminv(0.975,0,sqrt(varappr))]
#              
#              %Ã–ryggisbil Ã¡ logskala fyrir mÃ¦lingar (empirical bayes)
#              %Xm*mu %<- mÃ¶t y_hat 
#              
#              %axel: ég kommentaði út Xm*mu hér fyrir ofan
#              
#              [X_m*mu+norminv(0.025,0,sqrt(varappr)) X_m*mu+norminv(0.975,0,sqrt(varappr))]
#              
#              
#              %LH=chol(H)'/0.42;
# LH=chol(H)'/(2.38/sqrt(2));
#              %LH=chol(H)';
