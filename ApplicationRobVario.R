################################################################################
## Application: RobVario package                                              ##
################################################################################

## packages
library(RobVario)
library(ggplot2)

#########################
## 1. simulated data sets

# Simulation of a GRF without outliers
dat.non <- simulate_grf(gridsize = c("nx" = 15, "ny" = 15),
                        variogram = "spherical",
                        param.variogram = c("beta" = 0.4, "r" = 5),
                        nugget = 0.1,
                        aniso.param = c("theta" = 3*pi/8, "b" = 2),
                        n.it = 1,
                        seed = 123)

# simulation of a GRF with a nearly squared outlier block
dat.sq <- simulate_grf(gridsize = c("nx" = 15, "ny" = 15),
                       variogram = "spherical",
                       param.variogram = c("beta" = 0.4, "r" = 5),
                       nugget = 0.1,
                       aniso.param = c("theta" = 3*pi/8, "b" = 2),
                       out.type = "block",
                       block.type = "square",
                       dist.outlier = rnorm,
                       amount = 0.1,
                       param.outlier = c("mu" = 3, "sd" = 1),
                       mixture = TRUE,
                       n.it = 1,
                       seed = 234)

# visualisation
dat.non <- cbind(dat.non$data, dat.non$grid)

plot.non <- ggplot(dat.non, mapping = aes(x = x, y = y, color = data.1)) +
  geom_point(size = 4) +
  scale_color_viridis_c() +
  coord_equal() +
  labs(title = "GRF without outliers") +
  theme_minimal(base_size = 15)

dat.sq <- cbind(dat.sq$data, dat.sq$grid)

plot.sq <- ggplot(dat.sq, mapping = aes(x = x, y = y, color = data.1)) +
  geom_point(size = 4) +
  scale_color_viridis_c() +
  coord_equal() +
  labs(title = "GRF with an outlier block") +
  theme_minimal(base_size = 15)

## intern benötigt
library(patchwork)
plot.non | plot.sq
ggsave("C:/Users/paulg/Sciebo2/Diss/Grafiken/RobVario/GRFs.pdf", width = 22, height = 9, units = "cm")


# variogram estimation in the four main directions
varog.non <- variogram_est(dat.non,
                           hmax = c(7,7),
                           direction = c("S-N", "E-W"),
                           estimator = c("matheron", "genton", "mcd.diff"),
                           reweighting = TRUE)

varog.sq <- variogram_est(dat.sq,
                          hmax = c(7,7),
                          direction = c("S-N", "E-W"),
                          estimator = c("matheron", "genton", "mcd.diff"),
                          reweighting = TRUE)

# simulation of correction factors for MCD.diff and MCD.org
corr.non <- simulate_correctionfactors(gridsize = c(15,15),
                                       variogram.est = varog.non,
                                       n.it = 1000,
                                       estimator = c("mcd.diff"),
                                       reweighting = TRUE,
                                       variogram = "Sph")


corr.sq <- simulate_correctionfactors(gridsize = c(15,15),
                                      variogram.est = varog.sq,
                                      n.it = 1000,
                                      estimator = c("mcd.diff"),
                                      reweighting = TRUE,
                                      variogram = "Sph")

cbind(corr.non$correctionfactors, corr.sq$correctionfactors)
#     mcd.diff mcd.diff
# S-N 1.089130 1.162065
# E-W 1.200355 1.210355


# multiplication of mcd.diff with the simulated correction factors
for(d in 1:2){
  varog.non[[d]][which(varog.non[[d]]$estimator == "mcd.diff"),"variogram"] <-  varog.non[[d]][which(varog.non[[d]]$estimator == "mcd.diff"),"variogram"] * corr.non$correctionfactors[d]

  varog.sq[[d]][which(varog.sq[[d]]$estimator == "mcd.diff"),"variogram"] <-  varog.sq[[d]][which(varog.sq[[d]]$estimator == "mcd.diff"),"variogram"] * corr.sq$correctionfactors[d]
}

# visualisation
varog.non <- lapply(1:2, function(l){
  cbind(varog.non[[l]], "direction" = names(varog.non)[l])
})
varog.non <- do.call(rbind, varog.non)
varog.non$lag <- sqrt(varog.non$lag.x^2 + varog.non$lag.y^2)


p.varog.non <- ggplot(varog.non, mapping = aes(x = lag, y = variogram, color = estimator, linetype = direction)) +
  geom_point(size = 4) +
  geom_line(linewidth = 1.2) +
  labs(title = "GRF without outliers") +
  theme_minimal(base_size = 15)


varog.sq <- lapply(1:2, function(l){
  cbind(varog.sq[[l]], "direction" = names(varog.sq)[l])
})
varog.sq <- do.call(rbind, varog.sq)
varog.sq$lag <- sqrt(varog.sq$lag.x^2 + varog.sq$lag.y^2)

p.varog.sq <- ggplot(varog.sq, mapping = aes(x = lag, y = variogram, color = estimator, linetype = direction)) +
  geom_point(size = 4) +
  geom_line(linewidth = 1.2) +
  labs(title = "GRF without outliers") +
  theme_minimal(base_size = 15)

## intern benötigt
(p.varog.non | p.varog.sq) + plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
ggsave("C:/Users/paulg/Sciebo2/Diss/Grafiken/RobVario/Variograms.pdf", width = 22, units = "cm")



####################
## 2. Satellite data

load("ndvi.RData")

# block permutation test
test.block <- isotropy_test(data, lagmat = rbind(c(1,0), c(0,1), c(1,1), c(1,-1)), A = rbind(c(1,-1,0,0),c(0,0,1,-1)),
                            estimator = "all", method = "blockpermutation", window.dims = c(6,6), B = 1000)

# subsampling test
test.sub <- isotropy_test(data, lagmat = rbind(c(1,0), c(0,1), c(1,1), c(1,-1)), A = rbind(c(1,-1,0,0),c(0,0,1,-1)),
                            estimator = "all", method = "subsampling", window.dims = c(8,8), edge.sub = TRUE)

res <- rbind(c("matheron" = test.block$matheron$p.value, "genton" = test.block$genton$p.value, "mcd" = test.block$MCD$p.value),
             c("matheron" = test.sub$matheron$p.value, "genton" = test.sub$genton$p.value, "mcd" = test.sub$MCD$p.value))
rownames(res) <- c("blockpermutation", "subsampling")
round(res, 2)
#                  matheron genton  mcd
# blockpermutation     0.00   0.01 0.01
# subsampling          0.03   0.09 0.05
