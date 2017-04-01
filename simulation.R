library(mvtnorm)
library(huge)
n=500
p=100
type <- "random"
huge.tmp <- huge.generator(n,p,graph=type,v=0.3,prob = 0.5)
sigma <- huge.tmp$sigma
adjacent <- huge.tmp$theta
knock.stat <- c(
  knockoff.stat.fs,
  knockoff.stat.fs_omp,
  knockoff.stat.lasso_difference,
  knockoff.stat.lasso_signed_max
)

st <- Sys.time()
times <- 600
perf.fdr <- perf.power <- matrix(0,times,12)
colnames(perf.fdr) <- colnames(perf.power) <- c(p.adjust.methods,paste("knockoff","stat",1:4,sep=""))
for(i in 1:times){
  dat <- rmvnorm(n,mean=rep(0,p),sigma = sigma)
  for(j in 1:length(p.adjust.methods)){
    vs.result <- adjust.vs(dat,adjacent,method=p.adjust.methods[j])
    perf.fdr[i,j] <- vs.result$fdr
    perf.power[i,j] <- vs.result$power
  }
  for(j in 1:length(knock.stat)){
  vs.result <- knockoff.vs(dat,adjacent,statistic=knock.stat[[j]])
  perf.fdr[i,8+j] <- vs.result$fdr
  perf.power[i,8+j] <- vs.result$power
  }

}

apply(perf.fdr,2,mean)
apply(perf.power,2,mean)

cat("running time:")
Sys.time()-st
    
