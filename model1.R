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
t_m=Densmin$par
H=Densmin$hessian

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

plot(RC$w,exp(X_m%*%mu),type="l"); #axel: nota ggplot2? 


v_temp=X_m%*%solve(RC$Sig_xinv+t(X_m)%*%X_m/exp(t_m[2]))%*%t(X_m) #samanburdur stodst


varappr=as.matrix(diag(v_temp)+exp(t_m[2])); #samanburdur stodst
                   
                   #axel/end/28.05.15

#              %[norminv(0.025,0,sqrt(varappr)) norminv(0.975,0,sqrt(varappr))]
#              
#              %Ã–ryggisbil Ã¡ logskala fyrir mÃ¦lingar (empirical bayes)
#              %Xm*mu %<- mÃ¶t y_hat 
#              
#              %axel: ég kommentaði út Xm*mu hér fyrir ofan
#              axel: norminv sama og qnorm í R
#              
#              [X_m*mu+norminv(0.025,0,sqrt(varappr)) X_m*mu+norminv(0.975,0,sqrt(varappr))]

confinterval= cbind(X_m%*%mu+qnorm(0.025,0,sqrt(varappr)),X_m%*%mu+qnorm(0.975,0,sqrt(varappr))) #samanburdur stodst

LH=t(chol(H))/(2.38/sqrt(2)) #Hvadan kemur thessi tala?? 2.38

#              %LH=chol(H)'/0.42;
# LH=chol(H)'/(2.38/sqrt(2));
#              %LH=chol(H)';
# t1=zeros(4,Nit); %axel: skipun í R er matrix(0,4,Nit)
# t2=zeros(4,Nit);
# t3=zeros(4,Nit);
# t4=zeros(4,Nit);

t1=matrix(0,4,Nit)
t2=matrix(0,4,Nit)
t3=matrix(0,4,Nit)
t4=matrix(0,4,Nit)

#axel/begin/02.06.15



for(j in 1:4){
  t_old=t_m
  t=matrix(0,4,Nit)
  
  t_temp=NULL
  yp_temp=NULL
  ypo_temp=NULL
  
  
  Dens<-Denseval11(t_old,RC)
  p_old=Dens$p
  x_old=Dens$x
  yp_old=Dens$yp
  ypo_old=Dens$ypo
  D_old=Dens$D
  
  for(i in 1:Nit){
    t_new=t_old+solve(t(LH),as.matrix(rnorm(2,0,1)))
    
    Densnew<-Denseval11(t_new,RC)
    p_new=Densnew$p
    x_new=Densnew$x
    yp_new=Densnew$yp
    ypo_new=Densnew$ypo
    D_new=Densnew$D
    
    logR=p_new-p_old
    if (logR>log(runif(1))){
        t_old=t_new
        x_old=x_new
        p_old=p_new
        yp_old=yp_new
        ypo_old=ypo_new
        D_old=D_new
    }
    t=rbind(t_temp,c(t_old,x_old)) #skoda thetta, mogulega adra adferd (th[,i]=as.matrix(2,1))
    t_temp=t
    yp=cbind(yp_temp,yp_old)
    yp_temp=yp
    ypo=cbind(ypo_temp,ypo_old)
    ypo_temp=ypo
    
    D[i]=D_old
  }
  
  if(j==1){
    t1=t
    yp1=yp
    ypo1=ypo
    D1=D
  } else if(j==2){
    t2=t
    yp2=yp
    ypo2=ypo
    D2=D
  } else if(j==3){
    t3=t
    yp3=yp
    ypo3=ypo
    D3=D
  } else if(j==4){
    t4=t
    yp4=yp
    ypo4=ypo
    D4=D
  }
}


Dhat=-2*sum(log(dlnorm(exp(RC$y),X_m%*%mu,sqrt(exp(t_m[2])))))
Davg=mean(c(D1[seq(2000,20000,5)],D2[seq(2000,20000,5)],D3[seq(2000,20000,5)],D4[seq(2000,20000,5)]))
pd=Davg-Dhat
DIC=Dhat+2*pd
B=1/(mean(0.5*c(D1[seq(2000,20000,5)],D2[seq(2000,20000,5)],D3[seq(2000,20000,5)],D4[seq(2000,20000,5)])))

c(Dhat, Davg, DIC, pd, B) #afhverju thessi vigur?

# Dhat=-2*sum(log(lognpdf(exp(RC.y),X_m*mu,sqrt(exp(t_m(2))))));
# Davg=mean([D1(2000:5:20000) D2(2000:5:20000) D3(2000:5:20000) D4(2000:5:20000)]);
# pd=Davg-Dhat;
# DIC=Dhat+2*pd;
# B=1/mean(exp(0.5*[D1(2000:5:20000) D2(2000:5:20000) D3(2000:5:20000) D4(2000:5:20000)]));
# [Dhat Davg DIC pd B]
