################################################################################
#### Graphs
################################################################################


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

## 1. households in the same postcode


## 2. hoseholds far away


