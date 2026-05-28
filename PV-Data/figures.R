################################################################################
#### Graphs
################################################################################

library(ggplot2)
library(patchwork)

##############################
## map of the postcodes
load(file = "PV-Data/Solardaten/postcodes.RData")
poa.plot <- ggplot(poa.postcodes) +
  geom_sf(aes(fill = as.factor(POA_CODE21))) +   # POA_CODE ist ein Beispiel-Feld; ggf. Feldname anpassen
  theme_minimal() +
  labs(title = "Australien: Postal Areas (POA)",
       fill = "Postcode (POA)") + theme(legend.position = "none")
poa.plot
ggsave("PV-Data/Graphs/postcodes.pdf")


#################
## Scatter plots

load(file = "PV-Data/Solardaten/Clean_Data.Rdata")

## 1. households in the same postcode
## postcode 2048 (Stanmore), households 40, 99
data.40 <- data[data$Customer == 40, ]
data.99 <- data[data$Customer == 99, ]
# prepare for ggplot
data.scat1 <- as.data.frame(cbind("h40" = data.40$day_norm, "h99" = data.99$day_norm))
scat1 <- ggplot(data.scat1, mapping = aes(x = h40, y = h99)) + geom_point() +  theme_minimal(base_size = 15) +
         labs(x = "specific yield in kWh/kWp", y = "specific energy in kWh/kWp",
              title = "household 40 vs. household 99")

# postecode 2066 (Lane Cove), households 96, 159
data.96 <- data[data$Customer == 96, ]
data.159 <- data[data$Customer == 159, ]
# prepare for ggplot
data.scat2 <- as.data.frame(cbind("h96" = data.96$day_norm, "h159" = data.159$day_norm))
scat2 <- ggplot(data.scat2, mapping = aes(x = h96, y = h159)) + geom_point() +  theme_minimal(base_size = 15) +
  labs(x = "specific yield in kWh/kWp", y = "specific energy in kWh/kWp",
       title = "household 96 vs. household 159")
(scat1|scat2) + plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
ggsave("PV-Data/Graphs/Corr-near.pdf", width = 30, units = "cm")


## 2. hoseholds far away
## distance: 184 km, households 279, 239
data.279 <- data[data$Customer == 279, ]
data.239 <- data[data$Customer == 239, ]
# prepare for ggplot
data.scat3 <- as.data.frame(cbind("h279" = data.279$day_norm, "h239" = data.239$day_norm))
scat3 <- ggplot(data.scat3, mapping = aes(x = h279, y = h239)) + geom_point() +  theme_minimal(base_size = 15) +
  labs(x = "specific yield in kWh/kWp", y = "specific energy in kWh/kWp",
       title = "dhousehold 279 vs. household 239")


## distance: 184 km, households 194, 239
data.194 <- data[data$Customer == 194, ]
# prepare for ggplot
data.scat4 <- as.data.frame(cbind("h194" = data.194$day_norm, "h239" = data.239$day_norm))
scat4 <- ggplot(data.scat4, mapping = aes(x = h194, y = h239)) + geom_point() +  theme_minimal(base_size = 15) +
  labs(x = "specific yield in kWh/kWp", y = "specific energy in kWh/kWp",
       title = "household 194 vs. household 239")

(scat3|scat4) + plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
ggsave("PV-Data/Graphs/Corr-far.pdf", width = 30, units = "cm")


####################################################
## Scatter plots: corrected for daily weather effect

load(file = "PV-Data/Solardaten/Clean_Data2.RData")

#### 1. households in the same postcode
## postcode 2048 (Stanmore), households 40, 99
data.40 <- data[data$Customer == 40, ]
data.99 <- data[data$Customer == 99, ]
# prepare for ggplot
data.scat1 <- as.data.frame(cbind("h40" = data.40$error, "h99" = data.99$error))
scat1 <- ggplot(data.scat1, mapping = aes(x = h40, y = h99)) + geom_point() +  theme_minimal(base_size = 15) +
  labs(x = "estimated error", y = "estimated error",
       title = "household 40 vs. household 99")

# postecode 2066 (Lane Cove), households 96, 159
data.96 <- data[data$Customer == 96, ]
data.159 <- data[data$Customer == 159, ]
# prepare for ggplot
data.scat2 <- as.data.frame(cbind("h96" = data.96$error, "h159" = data.159$error))
scat2 <- ggplot(data.scat2, mapping = aes(x = h96, y = h159)) + geom_point() +  theme_minimal(base_size = 15) +
  labs(x = "estimated error", y = "estimated error",
       title = "household 96 vs. household 159")
(scat1|scat2) + plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
ggsave("PV-Data/Graphs/Corr-near-error.pdf", width = 30, units = "cm")


## 2. hoseholds far away
## distance: 184 km, households 279, 239
data.279 <- data[data$Customer == 279, ]
data.239 <- data[data$Customer == 239, ]
# prepare for ggplot
data.scat3 <- as.data.frame(cbind("h279" = data.279$error, "h239" = data.239$error))
scat3 <- ggplot(data.scat3, mapping = aes(x = h279, y = h239)) + geom_point() +  theme_minimal(base_size = 15) +
  labs(x = "estimated error", y = "estimated error",
       title = "dhousehold 279 vs. household 239")


## distance: 184 km, households 194, 239
data.194 <- data[data$Customer == 194, ]
# prepare for ggplot
data.scat4 <- as.data.frame(cbind("h194" = data.194$error, "h239" = data.239$error))
scat4 <- ggplot(data.scat4, mapping = aes(x = h194, y = h239)) + geom_point() +  theme_minimal(base_size = 15) +
  labs(x = "estimated error", y = "estimated error",
       title = "household 194 vs. household 239")

(scat3|scat4) + plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
ggsave("PV-Data/Graphs/Corr-far-error.pdf", width = 30, units = "cm")
