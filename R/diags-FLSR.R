

.diagFLSR=function(object, i=NULL) {
require(plyr)

    x <- ssb(object)
    y <- rec(object)
     yHat <- predict(object)
     residual <- residuals(object)
 
     dmns <- dimnames(x)
 
     residualLag <- FLQuant(NA, dimnames=dimnames(residual))
     residualLag[,-dim(residual)[2]] <- residual[,-1]
 
     qq. <- qqnorm(c(residual),plot.it=FALSE)
     qqx <- FLQuant(qq.$x,dimnames=dimnames(residual))
     qqy <- FLQuant(qq.$y,dimnames=dimnames(residual))
 
     ssb <- FLQuant(seq(0, max(x,na.rm=T), length.out=dim(x)[2]), dimnames=dimnames(x))
     rec <- predict(object, ssb=ssb)

     res <- model.frame(FLQuants(ssb=x, obs=y, hat=yHat, residual=residual, residualLag=residualLag,
                                 qqx=qqx, qqy=qqy, rec=rec, ssb.=ssb))
     
     qqpar <- diags:::qqLine(c(qqx),c(qqy))[c("a","b")]
 
     res <-data.frame(res,qqHat=c(qqpar["a"]*qqx+qqpar["b"]))
     
     return(res)
   }

if (!isGeneric("diags")) setGeneric('diags',   function(object,method,...) standardGeneric('diags'))

setMethod('diags',  signature(object='FLSR',method="missing"), function(object,method,...) .diagFLSR(object,method,...))
