n <- 1000


a1=0.2;
a2=1;
a3=0.5;
#parameters<-t(c(a1,a2,1,b1))
x=matrix(runif(n*3),n,3)
y=0

chow_result=0
for(k in 1:500){
  parameters<-c(a1,a2,a3,1)
  z=rnorm(n)
  xx<-cbind(x[1:1000,1],x[1:1000,2],x[1:1000,3],z[1:1000])
  y[1:1000]<-c(xx%*%parameters)
  
  parameters<-c(a1*2,a2*0.5,a3*0.7,1)
  bp=0
  for (i in 1:10){
   bp=bp+rpois(1, lambda=500)
     if(bp>0 && bp <1000){
       xx<-cbind(x[bp:1000,1],x[bp:1000,2],x[bp:1000,3],z[bp:1000])
       y[bp:1000]<-c(xx%*%parameters)
     }
  }
  
  r<-sctest(y~x[1:1000,1]+x[1:1000,2]+x[1:1000,3], type ="Nyblom-Hansen", h = 0.15,alt.boundary = FALSE, functional = c("max", "range","maxL2", "meanL2"), from = 0.15,
            to = NULL, asymptotic = FALSE)
  
  if (r$p.value<0.05) chow_result=chow_result+1
  
}
chow_result/500

scus.seat <- gefp(y~x[1:1000,1]+x[1:1000,2]+x[1:1000,3])
sctest(scus.seat, functional = supLM(from=0.2))
plot(scus.seat, functional = supLM(from=0.2))


