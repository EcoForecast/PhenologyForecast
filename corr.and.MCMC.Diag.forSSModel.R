corr.and.MCMC.Diag.forSSModel <- function(jags.out.all.years.array) {
source("global_input_parameters.R")
  ##### MCMC diagnostics
pheno.state.fromallchains <- jags.out.all.years.array[,5,]
### hardcoding number of chains for now
length.of.chain <- length(pheno.state.fromallchains)/3
first.chain <-pheno.state.fromallchains[1:length.of.chain]
second.chain <-pheno.state.fromallchains[(length.of.chain+1):(2*length.of.chain)]
third.chain <-pheno.state.fromallchains[(2*length.of.chain +1):(3*length.of.chain)]
chains.mcmc <- mcmc.list(as.mcmc(first.chain),as.mcmc(second.chain),as.mcmc(third.chain))
plot(chains.mcmc,trace=TRUE,density=TRUE,smooth=FALSE,auto.layout=TRUE)

}
