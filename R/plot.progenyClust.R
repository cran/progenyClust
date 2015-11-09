plot.progenyClust <-
function(x,errorbar=FALSE,xlab='number of clusters',ylab='progeny score',...){
  if(x$method=='gap'){
    if(errorbar==T & dim(x$score)[1]>1){
      Hmisc::errbar(x$ncluster[-c(1,length(x$ncluster))],x$mean.gap,yplus=x$sd.gap+x$mean.gap,yminus=x$mean.gap-x$sd.gap,xlab=xlab,ylab=ylab,...)
    }else{
      plot(x$ncluster[-c(1,length(x$ncluster))],x$mean.gap,xlab=xlab,ylab=ylab,...)
    }
  }else if(x$method=='score'){
    if(errorbar==T & dim(x$score)[1]>1){
      Hmisc::errbar(x$ncluster,x$mean.score,yplus=x$sd.score+x$mean.score,yminus=x$mean.score-x$sd.score,xlab=xlab,ylab=ylab,...)
    }else{
      plot(x$ncluster,x$mean.score,xlab=xlab,ylab=ylab,...)
    }
  }else{
    par(mfrow=c(2,1))
    if(errorbar==T & dim(x$score)[1]>1){
      Hmisc::errbar(x$ncluster[-c(1,length(x$ncluster))],x$mean.gap,yplus=x$sd.gap+x$mean.gap,yminus=x$mean.gap-x$sd.gap,xlab=xlab,ylab='gap',xlim=c(min(x$ncluster),max(x$ncluster)),...)
      Hmisc::errbar(x$ncluster,x$mean.score,yplus=x$sd.score+x$mean.score,yminus=x$mean.score-x$sd.score,xlab=xlab,ylab='score',...)
    }else{
      plot(x$ncluster[-c(1,length(x$ncluster))],x$mean.gap,xlab=xlab,ylab='gap',xlim=c(min(x$ncluster),max(x$ncluster)),...)
      plot(x$ncluster,x$mean.score,xlab=xlab,ylab='score',...)
    }
  }
}
