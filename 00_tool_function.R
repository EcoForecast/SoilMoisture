ciEnvelope <- function(x,ylo,yhi,...){
  polygon(cbind(c(x, rev(x), x[1]), c(ylo, rev(yhi),
                                      ylo[1])), border = NA,...) 
}

col.alpha <- function(col,alpha=1){
  rgb = col2rgb(col)
  rgb(rgb[1],rgb[2],rgb[3],alpha*255,maxColorValue=255)
}

## weighted quantile
wtd.quantile <- function(x,wt,q){ 
  ord <- order(x)
  wstar <- cumsum(wt[ord])/sum(wt)
  qi <- findInterval(q,wstar); qi[qi<1]=1;qi[qi>length(x)]=length(x)
  return(x[ord[qi]])
}
