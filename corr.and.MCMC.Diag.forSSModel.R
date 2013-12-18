cor1 = cor(out[,2:4])
sprintf('The correlation coefficient between tau_add and tau_gcc is %f',cor1[2])
sprintf('The correlation coefficient between tau_add and tau_ndvi is %f',cor1[3])
sprintf('The correlation coefficient between tau_gcc and tau_ndvi is %f',cor1[6])

# MCMC diagnostics
# MCMC diagnostics
tmp1 <- jags.out[[1]]
tmp2 <- jags.out[[2]]
tmp3 <- jags.out[[3]]
tmp1 <- tmp1[,c(2,3,4,5000,5020,5040,5060,5080,5100)]
tmp2 <- tmp2[,c(2,3,4,5000,5020,5040,5060,5080,5100)]
tmp3 <- tmp3[,c(2,3,4,5000,5020,5040,5060,5080,5100)]
tmp_list <- mcmc.list(tmp1,tmp2,tmp3)
plot(tmp_list,trace=TRUE,density=TRUE,smooth=FALSE,auto.layout=TRUE,ask=dev.interactive())