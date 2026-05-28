################################################################################
## Estimation of variograms                                                   ##
################################################################################

library(robustbase)
load( file = "PV-Data/Solardaten/Clean_Data2.Rdata")
load(file = "PV-Data/Solardaten/Postcode_Distances.RData")

## split for the postcodes
data.split <- split(data, data$Postcode)


#############
## 1. Isotrop: Specific engery yield, upto 120km
mat.sey <- matrix(nrow = length(unique(data.split[[1]]$date)), ncol = 1:120)
gen.sey <- matrix(nrow = length(unique(data.split[[1]]$date)), ncol = 1:120)
for(t in 1:length(unique(data.split[[1]]$date))){
  for(d in 1:120){
    diff.postcodes <- postcods_dists[[d]]

    data.perdiff <- lapply(1:ncol(diff.postcodes), function(x){
      p1 <- data.split[[diff.postcodes[1,x]]][which(data.split[[diff.postcodes[1,x]]]$date == data.split[[1]]$date[t]),]
      p2 <- data.split[[diff.postcodes[2,x]]][which(data.split[[diff.postcodes[2,x]]]$date == data.split[[1]]$date[t]),]
      return(list(p1, p2))
    } )

    diff <- lapply(data.perdiff, function(x){
      as.vector(outer(x[[1]], x[[2]], FUN = "-"))
    })
    diff <- unlist(diff)

    matheron[t, d] <- mean(diff^2)
    genton[t, d] <- Qn(diff)^2
  }
}


plot(names(postcods_dists), matheron[1,], type = "l")
plot(names(postcods_dists), genton[1,], type = "l")
matheron[1,]
genton[1,]

