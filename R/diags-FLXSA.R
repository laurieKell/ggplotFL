
.diagFLXSA=function(object,i=NULL) {
require(plyr) 
     fn <- function(object,i) {
       x <- index(object)[[i]]
       yHat <- index.hat(object)[[i]]
       residual <- index.res(object)[[i]]
 
       #
       dmns <- dimnames(x)
       y <- stock.n(object)[dmns$age,dmns$year]
 
       #
       residualLag      =FLQuant(NA,dimnames=dimnames(residual))
       residualLag[,-dim(residual)[2]] <- residual[,-1]
       qq. <- qqnorm(c(residual),plot.it=FALSE)
       qqx <- FLQuant(qq.$x,dimnames=dimnames(residual))
       qqy <- FLQuant(qq.$y,dimnames=dimnames(residual))
 
       #
       res <- model.frame(FLQuants(stock.n=x, obs=y, hat=yHat, residual=residual,
                                   residualLag=residualLag, qqx=qqx, qqy=qqy))
 
       return(res)
     }
 
     #
     if (is.null(i)) {
 
       nms <- index.name(object)
       res <- mdply(data.frame(x=1:length(nms)),
         function(x,object)
           data.frame(index=nms[x],fn(object,x)), object=object)
     } else
       res <- fn(object, i)
 
     return(res[,-1])}
 
if (!isGeneric("diags")) setGeneric('diags',   function(object,method,...) standardGeneric('diags.'))

setMethod('diags', signature(object='FLAssess',method="missing"),  function(object,method="missing",...) .diagFLXSA(object,method,...))
