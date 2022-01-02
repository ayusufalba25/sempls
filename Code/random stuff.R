tE
pC
pC + (pC %*% pC) + (pC %*% pC %*% pC)

dp <- pC
for(i in 1:nrow(pC)){
  pr <- dp
  for(j in 1:i){
    pr <- pr %*% dp
  }
  dp <- dp + pr
}
dp

cor(ecsi$data, ecsi$factor_scores)
plsLoadings(ecsi)
ecsi$outer_loadings
