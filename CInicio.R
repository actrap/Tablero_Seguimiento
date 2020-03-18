a <- "C:/Users/larcos/M_Empresarial/Analisis/Ventas/Aplicaciones/Tablero_Seguimiento/"
#a <- "C:/Users/vmorenoa/Documents/R proyectos/pruebas/"
b <- "/srv/shiny-server/tablero_seguimiento/"
#b <- "/srv/shiny-server/pruebas/"

direcc <- if_else(substr(getwd(),1,1) == "C", a , b)

# Carga la base actual
load(paste0(direcc,"insumos/a_p/actual_uso_inicio.RDATA"))