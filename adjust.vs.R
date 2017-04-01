adjust.vs <- function(dat,adjacent,method="BH",alpha=0.20,symm.prob=0.5){
  p <- dim(dat)[2]
  selection <- matrix(0,p,p)
  for(a in 1:p){
    lfit <- summary(lm(dat[,a]~dat[,-a]))
    pval <- lfit$coefficients[,4][-1]
    adjust.selected <-  which(p.adjust(pval,method = method)<=alpha)
    attr(adjust.selected, "names") <- NULL
    selection[a,-a][adjust.selected] <- 1
  }
  selection <- symm.matrix(selection,prob=symm.prob)
  overall.fdr <- sum(adjacent==0&selection==1)/max(sum(selection),1)
  overall.power <- sum(adjacent==1&selection==1)/sum(adjacent)
  #cat("overall.fdr",overall.fdr,"\n")
  #cat("overall.power",overall.power,"\n")
  return(list(fdr=overall.fdr,power=overall.power))
}

