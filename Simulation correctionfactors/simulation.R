################################################################################
## Simulation of the correction factors of MCD.diff and MCD.org for           ##
## given variogram estimations                                                ##
################################################################################

library(RobVario)

#############
## Parameters
## grid sizes
grids <- c("nx" = 15, "ny" = 15)

## variogram models
variograms <- "spherical"

## parameters of the variogram models
params.variogram <- c("var" = 0.4, "scale" = 5)

## nugget effects
nugget <- c(0.1)

## Parameters for the anisotropy
aniso.params <- c("angle" = 3*pi/8, "ratio" = 2)


## 1. Simulate the data
datas <- simulate_grf(gridsize = grids, variogram = variograms,
                      param.variogram = params.variogram, nugget = nugget,
                      aniso.param = aniso.params, n.it = 1000,
                      seed = 14354)

save(datas, file = "simulation correctionfactors/data.RData")

# 2. Calculate the variogram for each simulated data set
varogs.re <- lapply(1:1000, function(l){
  data <- cbind(datas$data[,l], datas$grid)
  vario <- variogram_est(data, hmax = c(7,7,5,5),
                         estimator = c("mcd.diff", "mcd.org"))})

save(varogs.re, file = "simulation correctionfactors/variogram-reweighted.RData")

varogs <- lapply(1:1000, function(l){
  data <- cbind(datas$data[,l], datas$grid)
  vario <- variogram_est(data, hmax = c(7,7,5,5),
                         estimator = c("mcd.diff", "mcd.org"),
                         reweighting = FALSE)})

save(varogs, file = "simulation correctionfactors/variogram.RData")

# 3.Simulate correction factors for the reweighted estimator and the true variogram model
load("simulation correctionfactors/variogram-reweighted.RData")
corrs.sph.re <- list()
for(i in 1:1000){
  corrs.sph.re[[i]] <- simulate_correctionfactors(gridsize = grids, varogs.re[[i]], n.it = 100, estimator = c("mcd.diff", "mcd.org"))
  print(i)
}

save(corrs.sph.re, file = "simulation correctionfactors/corr-reweighted-sph.RData")

# Auswertung
load(file = "simulation correctionfactors/corr-reweighted-sph.RData")
c.sph.re <- matrix(rep(0, 8), ncol = 2)
for(i in 1:1000){
  c.sph.re <- c.sph.re + corrs.sph.re[[i]]$correctionfactors
}
round(c.sph.re/1000, 2)


# 4.Simulate correction factors for the unreweighted estimator and the true variogram model
load("simulation correctionfactors/variogram.RData")
corrs.sph <- list()
for(i in 1:1000){
  corrs.sph[[i]] <- simulate_correctionfactors(gridsize = grids, varogs.re[[i]], n.it = 100, estimator = c("mcd.diff", "mcd.org"),
                                               reweighting = FALSE)
  print(i)
}

save(corrs.sph, file = "simulation correctionfactors/corr-sph.RData")

# Auswertung
load(file = "simulation correctionfactors/corr-sph.RData")
c.sph <- matrix(rep(0, 8), ncol = 2)
for(i in 1:1000){
  c.sph <- c.sph + corrs.sph[[i]]$correctionfactors
}
round(c.sph/1000, 2)


# 5.Simulate correction factors for the reweighted estimator and the false variogram model
load("simulation correctionfactors/variogram-reweighted.RData")
corrs.exp.re <- list()
for(i in 1:1000){
  corrs.exp.re[[i]] <- simulate_correctionfactors(gridsize = grids, varogs.re[[i]], n.it = 100, estimator = c("mcd.diff", "mcd.org"),
                                                  variogram = "Exp")
  print(i)
}

save(corrs.exp.re, file = "simulation correctionfactors/corr-reweighted-exp.RData")


# 6.Simulate correction factors for the estimator and the false variogram model
load("simulation correctionfactors/variogram.RData")
corrs.exp <- list()
for(i in 1:1000){
  corrs.exp[[i]] <- simulate_correctionfactors(gridsize = grids, varogs.re[[i]], n.it = 100, estimator = c("mcd.diff", "mcd.org"),
                                               reweighting = FALSE, variogram = "Exp")
  print(i)
}

save(corrs.exp, file = "simulation correctionfactors/corr-exp.RData")
