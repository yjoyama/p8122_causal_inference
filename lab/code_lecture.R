

A <- c(rep(1,8),rep(0,8))
Y <- c(50,62,48,55,58,61,58,56,48,46,54,45,53,46,53,48)
T_stat <- mean(Y[A == 1]) - mean(Y[A == 0])
T_stat

library(remotes)
install_version("ri", "0.9")
library(ri)

Abold <- genperms(A,maxiter = 12870)

Abold <- genperms(A)

Abold[, 1:6]


rdist <- rep(NA, times = ncol(Abold))
for (i in 1:ncol(Abold)) {
  A_tilde <- Abold[, i]
  rdist[i] <- mean(Y[A_tilde == 1]) - mean(Y[A_tilde == 0])
}
rdist
hist(rdist)

# p-value
pval <- mean(rdist >= T_stat)
pval
quant <- quantile(rdist,probs = 1-pval)
hist(rdist)
abline(v = quant,col="red")


# CI


#grid<-seq(-3,12, by=0.1)
grid<-seq(-3,12, by=0.05)
p.ci<-rep(NA,length(grid))
rdist <- rep(NA, times = ncol(Abold))
for (i in 1:length(grid)){
for (k in 1:ncol(Abold)) {
  A_tilde <- Abold[, k]
  rdist[k] <- mean(Y[A_tilde == 1]) - mean(Y[A_tilde == 0])+grid[i]
}
  p.ci[i]<-mean(rdist >= T_stat)
}

cbind(p.ci,grid)#select values you would not reject (2.25,11.45)

grid[which(0.049<p.ci & p.ci<0.05)]
(6.75+ 6.80 +6.85+ 6.90 +6.95)/5

?invert.ci

perms.ci <- genperms(A,maxiter = 12870) ## all possible permutations of assignment to treatment
probs.ci <- genprobexact(A) ## assuming complete randomization

c(invert.ci(Y,A,probs.ci,perms.ci,0.025),invert.ci(Y,A,probs.ci,perms.ci,0.975))

