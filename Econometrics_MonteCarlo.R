n <- 1000


a1=0.2;
a2=0.95;
b1=0.5;
p=0;
parameters<-t(c(a1,a2,1,b1))
chow_result=0
for(k in 1:500){
  parameters<-t(c(a1,a2,1,b1))
  z=rnorm(n, mean = 0, sd = 0.2)
  y<-c(parameters%*%c(1,1,z[1],0))
  
  for(i in 2:n){
    y[i]=parameters%*%c(1,y[i-1],z[i],z[i-1])
  }
  
  parameters<-t(c(a1*1.1,a2*0.97,1,b1*0.95))
  
  bp=300
  for(i in bp:n){
    y[i]=parameters%*%c(1,y[i-1],z[i],z[i-1])
  }
  
  #"Nyblom-Hansen""Chow"
  r<-sctest(y[2:1000]~1+y[1:999]+z[1:999], type ="Nyblom-Hansen", h = 0.15,alt.boundary = FALSE, functional = c("max", "range","maxL2", "meanL2"), from = 0.15,
         to = NULL, point=0.7, asymptotic = FALSE)
  
  if (r$p.value<0.05) chow_result=chow_result+1
  p[k]=r$p.value

}
chow_result/500

scus.seat <- gefp(y[2:1000]~1+y[1:999]+z[1:999])
sctest(scus.seat, functional = supLM(from=0.2))
plot(scus.seat, functional = supLM(from=0.2))
plot(y,type='l')
