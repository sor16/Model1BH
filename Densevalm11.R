#Sölvi begin 26.may
Denseval11 <- function(tr,RC){
  
  #hugsanlega önnur breytunöfn
  l=log(RC$w_tild+exp(tr[1]))
  
  
  #l=RC$wtild %>% + exp(t(1)) %>% log()
  X=matrix(nrow=length(l),c(rep(1,length(l)),l))
  L=t(chol(RC$Sig_xinv + (t(X) %*% X)/exp(tr[2])))
  
  
  #L=(RC$Sig_xinv+as.matrix(X)%*%X/exp(t(2))) %>% chol() %>% t();
  q=(RC$Sinvmu+t(X)*RC$y/exp(tr[2]))/L;
  #Sölvi end
  
  
  #Sölvi begin 27.mai
  p=0.5*sum(q^2)+log(L[1,1])+log(L[2,2]) 
  -0.5*sum(RC$y^2)/exp(tr[2])-RC$n*tr[2]/2 
  +t[1]-exp(t[1])*RC$mu_c-tr[2]
  
  x=(q+as.matrix(rnorm(2)))/t[L]
  
  yp=X %*% x;
  
  ypo=yp+as.matrix(rnorm(RC$n,sd=sqrt(exp(t[2]))));
  #ypo=rnorm(RC$n,sd=sqrt(exp(t(2)))) %>% as.matrix() %>% + yp;
  D=-2*sum(log(dlnorm(exp(RC$y),X*x,sqrt(exp(t[2])))));
  #D=dlnorm(exp(RC$y),mean=yp,sd=sqrt(exp(t(2)))) %>% log() %>% sum()*(-2)
  
  #Skilar p,x,yp,ypo,D
}