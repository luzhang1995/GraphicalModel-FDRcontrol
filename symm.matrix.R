symm.matrix <- function(mat,prob){
	p <- dim(mat)[1]
  symm.min <- -abs(skewpart(mat))+symmpart(mat)
  symm.max <-  abs(skewpart(mat))+symmpart(mat)
  rchoice <- matrix(0,p,p)
  rchoice[lower.tri(rchoice)] <- rbinom(p*(p-1)/2,1,prob=prob)
  rchoice[upper.tri(rchoice)] <- rchoice[lower.tri(rchoice)]
  return(rchoice*symm.max+(1-rchoice)*symm.min)
}
