
# Preparación de datos ----------------------------------------------------
 ## aqui solo se cargan la base de datos para despues ser trabajada


library(data.table)

fraildata <- fread("data-raw/frailhrv_d.csv")

save(fraildata, file = "data/fraildata.RData")

