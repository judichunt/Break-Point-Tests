#sp500<-SPY[1800:2598,4]
#q=as.numeric(sp500)
#q=diff(q,1)
library(quantmod)
SSE <- getSymbols("000001.SS",auto.assign=FALSE) 
sh=0
sh<-SSE[1800:2527,4]
sh=as.numeric(sh)
s=diff(sh,1)




r<-sctest(s[2:728]~1+s[1:727], type ="Nyblom-Hansen", h = 0.15,alt.boundary = FALSE, functional = c("max", "range","maxL2", "meanL2"), from = 0.15,
           to = NULL, point=0.35, asymptotic = FALSE)

r$p.value
scus.seat <- gefp(s[2:728]~1+s[1:727])
sctest(scus.seat, functional = supLM(from=0.2))
plot(scus.seat, functional = supLM(from=0.2))

SSE_Composite_Index=SSE[1800:2527,4]
