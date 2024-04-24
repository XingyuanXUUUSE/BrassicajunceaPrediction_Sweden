#Bias Control
occ <- clcjuncea
colnames(occ) <- c("species", "decimalLatitude","decimalLongitude")
bias.out <- calculate_bias(x = occ, gaz = NULL)
summary(bias.out)
plot(bias.out)

proj <- project_bias(bias.out)
map<-map_bias(proj, type = "log_sampling_rate")