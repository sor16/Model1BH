#Sölvi begin 26.may
Denseval11 <- function(t,RC)
  #hugsanlega önnur breytunöfn
  l=log(RC$w_tild+exp(t(1))
  X=rep(1,length(l),l];
  
  L=RC$Sig_xinv+as.matrix(X)%*%X/exp(t(2)) %>% chol() %>% t();
  q=L\(RC$Sinvmu+as.matrix(X)*RC$y/exp(t(2)));
#Sölvi end
  
#Skilar p,x,yp,ypo,D