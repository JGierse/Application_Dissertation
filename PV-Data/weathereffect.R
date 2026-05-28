################################################################################
## estimate the daily weather effect                                          ##
################################################################################

load(file = "PV-Data/Solardaten/Clean_Data.Rdata")

## prepare data
data.day <- split(data, data$date)

## estimate the dayeffect (using only the clean data)
mut <- rep(0, length(data.day))  # (mean: using only the clean data)
mut.rob <- rep(0, length(data.day)) # (median: using all data)

for(i in 1:length(data.day)){
  mut[i] <- mean(data.day[[i]][which(data.day[[i]]$quality == 1), "day_norm"])
  mut.rob[i] <- median(data.day[[i]]$day_norm)
  mut2[i] <- mean(data.day[[i]]$day_norm)

}
names(mut) <- names(data.day)
names(mut.rob) <- names(data.day)

save(mut, mut.rob, file = "PV-Data/Solardaten/weathereffect.RData")

## weather of the day
sunny <- quantile(mut, 0.75)
cloudy <- quantile(mut, 0.5)
heavycloudy <- quantile(mut, 0.25)

## add a column weather
for(d in 1:length(data.day)){
  if(mut[d] > sunny){
    data.day[[d]]$weather <- "sunny"
  }
  if(mut[d] > cloudy & mut[d] <= sunny){
    data.day[[d]]$weather <- "partly cloudy"
  }
  if(mut[d] > heavycloudy & mut[d] <= cloudy){
    data.day[[d]]$weather <- "mostly cloudy"
  }
  if(mut[d] <= heavycloudy){
    data.day[[d]]$weather <- "heavy rain"
  }
}

## add a column: corrected data for weather effect
for(d in 1:length(data.day)){
  data.day[[d]]$error <- data.day[[d]]$day_norm/mut[d]
}


data <- do.call(rbind, data.day)
save(data, file = "PV-Data/Solardaten/Clean_Data2.RData")
