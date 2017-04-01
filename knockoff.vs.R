knockoff.vs <- function(dat,adjacent,fdr=0.20,statistic=knockoff.stat.lasso_difference,symm.prob=0.5){
  p <- dim(dat)[2]
  selection <- matrix(0,p,p)
  for(a in 1:p){
    filter <- knockoff.filter(dat[,-a], dat[,a], fdr = 0.20, statistic = statistic  ,
    # knockoff.stat.fs knockoff.stat.fs_omp knockoff.stat.lasso_difference
    # knockoff.stat.lasso_signed_max
    threshold = "knockoff+", knockoffs="equicorrelated",normalize = TRUE, randomize = FALSE)
    selection[a,-a][filter$selected] <- 1
  }
  selection <- symm.matrix(selection,prob=symm.prob)
  overall.fdr <- sum(adjacent==0&selection==1)/max(sum(selection),1)
  overall.power <- sum(adjacent==1&selection==1)/sum(adjacent)
  #cat("overall.fdr",overall.fdr,"\n")
  #cat("overall.power",overall.power,"\n")
  return(list(fdr=overall.fdr,power=overall.power))
}
