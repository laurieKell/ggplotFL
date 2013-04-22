# ggplotFL/R/diags.R
# 
# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC, Laurie Kell, ICCAT
# $Id:  $

## local function to calculated expected QQ line
qqLine <- function(x,y){ 
  qtlx <- quantile(x, prob=c(0.25,0.75), na.rm=T)
  qtly <- quantile(y, prob=c(0.25,0.75), na.rm=T)
      
  a <- (qtly[1]- qtly[2]) / (qtlx[1] - qtlx[2])
  b <- qtly[1] - qtlx[1] * a
      
  res <- c(a,b)
        
  names(res) <- NULL
  names(res) <- c("a","b")

 return(res)}

 fnDiags=function(res){
      res$residualLag <- c(res$residual[-1],NA)
     
      qq.     <- qqnorm(res$residual,plot.it=FALSE,na.rm=T)
      res$qqx <- qq.$x
      res$qqy <- qq.$y
      
      qqpar <- qqLine(qq.$x,qq.$y)[c("a","b")]

      res$qqHat=qqpar["a"]*res$qqx+qqpar["b"]
      
      res}
 