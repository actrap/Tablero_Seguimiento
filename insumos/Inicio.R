library(readxl)
library(dplyr)
library(data.table)
library(dtplyr)
library(htmltools)
library(stringr)
library(tidyr)
library(lubridate)

#a <- "E:/Archivos_LARCOS/Aplicaciones/Comparativo_SACTEL/"
a <- "C:/Users/larcos/M_Empresarial/Analisis/Ventas/Aplicaciones/Comparativo_SACTEL/"
#b <- "/srv/shiny-server/tablero_seguimiento/"
b <- "/srv/shiny-server/pruebas/"


dire1c <- if_else(substr(getwd(),1,1) == "/", b , a)
dire2c <- paste0(dire1c,"insumos/")

if( dire1c == b ){
  Sys.setlocale(category = "LC_TIME" ,"es_MX.UTF-8")
  Sys.setlocale(category = "LC_COLLATE" ,"es_MX.UTF-8")
  dire2c <- "/home/estrategia/"
}

dia01 <- if_else(strftime(Sys.Date(),format = "%A") %in% c("Monday","lunes"),Sys.Date()-5,Sys.Date()-1 )
dia02 <- dia01-6

Tprod <- subset(subset(as.data.table(read.csv(paste0(dire1c,"insumos/catalogo/Productos.csv"), colClasses = "character") ), select = -Familia.de.productos), select = -Fuente)
names(Tprod) <- c("Producto de mayor ponderacion","Nombre_corto_Producto","Familia","Negocio")
etapasopp <- as.data.table(read.csv(paste0(dire1c,"insumos/catalogo/Etapa.csv"), encoding = "UTF-8"))
names(etapasopp) <- c("Etapa","Etap_Camp")
DivSec <- as.data.table(read_excel(paste0(dire1c,"insumos/catalogo/DivSec.xlsx")))
Fam_Neg <- unique(data.table("Nombre_corto_Producto" = Tprod$Nombre_corto_Producto,"Familia" = Tprod$Familia, "Negocio" = Tprod$Negocio))
total_campos <- as.data.table(read_excel(paste0(dire1c,"insumos/catalogo/Campos_disponibles.xlsx"), col_types = "text"))
a_campa <- as.data.table(read_excel(paste0(dire1c,"insumos/catalogo/Campañas.xlsx")), col_types = c("text","text","date"))
p_campa <- as.data.table(read_excel(paste0(dire1c,"insumos/catalogo/Producto-Campaña.xlsx"), col_types = "text"))

NoPEx <- c("Experiencia y Habilidades","Gestión Comercial","Precio")
c <- as.data.table(read_excel(paste0(dire2c,"Reclasificación_Postmortem.xlsx"), sheet = "Hoja1", col_types = "text"))
PM <- unique(subset(c, select = c("ID","Clasificación_EAE","Subclasificación_EAE","Conclusión de cierre","Competidor ganador")))

UComp <- unique(PM$`Competidor ganador`[!(PM$`Competidor ganador` %in% c("0","N/A") | is.na(PM$`Competidor ganador`)) ])
UComp <- UComp[order(UComp)]

Giro_Vt <- as.data.table(read_excel( paste0(dire1c,"insumos/catalogo/Giro_Vertical.xlsx") ))
G_x_C <- as.data.table(read_excel( paste0(dire1c,"insumos/catalogo/Giro_x_CUC.xlsx") ))
Vtcal <- as.character(unique(Giro_Vt$Vertical),unique(G_x_C$Vertical))
Vtcal <- Vtcal[ !(Vtcal %in% c("Sin especificar","Agroindustria","Servicios","Medios","Construcción","Industria") )]
Vtcal <- Vtcal[order(Vtcal)]

Proy <- c("A","B","C")
Etap <- c("Nueva",as.character(unique(etapasopp$Etap_Camp)))

MeDi <- data.table(`Div / Sect 2` = DivSec$`División / Sector`, Mercado = DivSec$Mercado)[order(-Mercado)]


Divs <- as.character(unique(DivSec$`División / Sector`))
Meri <- as.character(unique(MeDi$Mercado))
Fari <- as.character(unique(Fam_Neg$Familia))
Neri <- as.character(unique(Fam_Neg$Negocio))


campos_incluye_T.Proy <- c("Cliente","División / Sector","Nombre de la oportunidad","Descripción de la Oportunidad","Etapa","Paso siguiente","Fecha paso siguiente",
                           "Total de Proyecto","Importe Anual","Producto mayor ponderacion","Productos en OPP","Tipo","Clasificación","Creación","Cierre","ID","Ejecutivo","Gerencia","Giro","Servicios","Importe","sucursales","suc_ganadas","Negocio","Descripción del cierre")

campos_para_cerradas <- c("Cliente","División / Sector","Nombre de la oportunidad","Conclusión de cierre","Clasificación_EAE","Subclasificación_EAE","Competidor ganador",
                          "Total de Proyecto","Importe Anual","Producto mayor ponderacion","Productos en OPP","Cierre","ID","Etapa","Ejecutivo","Gerencia","Servicios","Importe")

campos_en_casos <- c("Cliente","División / Sector","Nombre de la oportunidad","Total de Proyecto","Precio Objetivo","Número del caso",
                     "Estado","Asignado_Finanzas","Fecha de apertura","Fecha de Proyecto","Fecha Cierre del Caso","Productos en OPP",
                     "Tipo","Clasificación","Creación","Cierre","ID","Etapa")


campos_Ten_Ganancia <- c("Div / Sect 2","RAZON_SOCIAL_CORP","RAZON_SOCIAL","Gerencia","Nombre del producto")

############# Casos Finanzas
casos_01 <-as.data.table(read_excel(paste0(dire1c,"insumos/Casos_01_Sactel.xlsx") ))
casos_02 <-as.data.table(read_excel(paste0(dire1c,"insumos/Casos_02_Finanzas.xlsx") ))
f_reporte <- max(as.Date(casos_02$`FECHA REPORTE`))

casos_03 <- merge(casos_01,subset(casos_02, select = c("Número del caso","STATUS","COMENTARIO","ASIGNADO")), by = "Número del caso", all.x = TRUE)
casos_03$COMENTARIO[is.na(casos_03$COMENTARIO)] <- casos_03$`Razón de Respuesta`[is.na(casos_03$COMENTARIO)]

y1 <- c('Casos : Finanzas - Privado', 'Casos: Finanzas - Gobierno/EP', 'ANDREA BERENICE DURAN BECERRIL', 'IRVIN PEREDA MEJIA', 'MONICA GUTIERREZ MARQUEZ', 'CESAR ALEJANDRO HAMED GONZALEZ','DANIEL MORALES ROMERO')

x <- subset(casos_03, `Propietario del caso` %in% y1 & !is.na(casos_03$`Identificador de la Oportunidad`), select = c("Identificador de la Oportunidad","Número del caso","Estado","STATUS","Abierto") )

y1 <- x[Abierto == 1, .(Caso = max(`Número del caso`) ) , by = `Identificador de la Oportunidad`]
y2 <- x[Estado == "Aprobado", .(Caso = max(`Número del caso`) ) , by = `Identificador de la Oportunidad`]
y2 <- y2[!(y2$`Identificador de la Oportunidad` %in% y1$`Identificador de la Oportunidad`)]
y2 <- rbind(y1,y2)
y3 <- x[Estado == "Cerrado", .(Caso = max(`Número del caso`) ) , by = `Identificador de la Oportunidad`]
y3 <- y3[!(y3$`Identificador de la Oportunidad` %in% y2$`Identificador de la Oportunidad`)]
y3 <- rbind(y2,y3)
y4 <- x[Estado == "Rechazado", .(Caso = max(`Número del caso`) ) , by = `Identificador de la Oportunidad`]
y4 <- y4[!(y4$`Identificador de la Oportunidad` %in% y3$`Identificador de la Oportunidad`)]
y4 <- rbind(y3,y4)

y5 <- merge(y4, subset(casos_03, select = c("Número del caso","Descripción","Estado","STATUS","COMENTARIO","Precio Objetivo","Antigüedad (Días)","Fecha de apertura","Fecha de cierre","Abierto","ASIGNADO")), by.x = "Caso" , by.y = "Número del caso", all.x = TRUE )

u1 <- !(is.na(y5$STATUS) | y5$Estado %in% c("Aprobado","Rechazado") | y5$STATUS == "Gobierno" )
y5$Estado_Final <- y5$Estado
y5$Estado_Final[u1] <- y5$STATUS[u1]
y5$Estado_Final[y5$Estado_Final == "Cerrado"] <- "Rechazado"
y5$Estado_Final[y5$Estado_Final == "Nuevo"] <- "Actualizar"
y5$Estado_Final[y5$Estado_Final == "En Progreso"] <- "Trabajar"
y5$Estado_Final[y5$Estado_Final == "Escalado"] <- "En Revisión"
y5$Estado_Final[y5$Estado_Final == "Liberado"] <- "Aprobado"
y5$Rango_Caso <- as.character(NA)
y5[is.na(y5$ASIGNADO) & y5$Abierto == 1]$ASIGNADO <- " Sin Asignado"

names(y5) <- c("Número del caso","Identificador de la oportunidad","Descripción del Caso","Estado_Sactel","STATUS_Finanzas","Comentario","Precio Objetivo","Antigüedad_Caso","Fecha de apertura","Fecha_Cierre_Caso","Caso_Abierto","Asignado_Finanzas","Estado_Final","Rango_Caso")

casos_Final <- y5

#####

Gerencias_Salud <- c("FARMACIAS (CADENAS)","GRUPOS HOSPITALARIOS Y LABORATORIOS","QUIMICO FARMACEUTICO","SEGURIDAD, SOCIAL Y SALUD")
Gerencias_EGrupo <- c("EMPRESAS GRUPO FINANCIERO","EMPRESAS GRUPO SERVICIOS Y TURISMO","EMPRESAS GRUPO ENERGETICOS","EMPRESAS GRUPO INDUSTRIA")
filiales <- "FILIALES TELMEX"

#### Carga Base de Oportunidades y hace ajustes

load(paste0(dire1c,"insumos/a_p/actual_uso.RDATA"))

ejec_gcia <- unique(subset(p3_cliente, select = c("Propietario de la cuenta","Gerencia")))
names(ejec_gcia) <- c("Propietario de oportunidad","Gerencia")
Geri <- as.character(unique(ejec_gcia$Gerencia))


Base_Y0 <- unique(subset(opps_total, select = c("Identificador de la oportunidad", "Nombre del producto", "Familia de productos","Precio total","Cantidad")))
Base_X0 <- uni_ks_opps

##### Agrega Oportunidades Manuales------------
manuales <- Base_X0[0,]
y1 <- as.data.table(read_xlsx(paste0(dire1c,"insumos/Carga_Manual_OPP.xlsx") ))
y2 <- merge(y1[!is.na(`Identificador de la oportunidad`)], Base_X0[,.N,by = `Identificador de la oportunidad` ], by = "Identificador de la oportunidad", all.x = TRUE)
cliente <- subset(y2, is.na(N), select = -N)
manuales <- rbind(manuales, cliente) #oportunidades que no están identificadas y se suman 
# Actualiza el nombre del cliente, div y gerencia con los datos que hay en la base
cliente <- as.data.table(read_excel(paste0(dire1c,"insumos/Visitas_Paso3.xlsx") ))[CUC %in% as.character(unique(manuales$CUC))]
cliente$`Div / Sect 2`[cliente$`Div / Sect 2` == "Otros"] <- cliente$`División / Sector`[cliente$`Div / Sect 2` == "Otros"]
client3 <- data.frame("CUC" = cliente$CUC,"nomi"=cliente$`Nombre de la cuenta`,"divi"= cliente$`Div / Sect 2`,"geri"=cliente$Gerencia)
manuales <- merge(manuales,client3, by = "CUC", all.x = TRUE)
manuales[!is.na(divi)]$`Nombre de la cuenta` <- manuales[!is.na(divi)]$nomi
manuales[!is.na(divi)]$`Div / Sect 2` <- manuales[!is.na(divi)]$divi
manuales[!is.na(divi)]$Gerencia <- manuales[!is.na(divi)]$geri
Base_X0 <- rbind(Base_X0, manuales[,1:ncol(Base_X0)] )
manuales <- data.table("Identificador de la oportunidad" = manuales$`Identificador de la oportunidad`, "Nombre del producto" = manuales$`Producto de mayor ponderacion`, "Familia de productos" = manuales$`Familia producto de mayor ponderación`, "Precio total" = manuales$`Importe Anual`, "Cantidad" = 1 )
Base_Y0 <- rbind(Base_Y0, manuales)
# Las Opp encontradas les actualiza la Descripción y la Campaña
y3 <- data.table("Identificador de la oportunidad" = y2[!is.na(y2$N)]$`Identificador de la oportunidad`,"D2" = y2[!is.na(y2$N)]$Descripción,"C2" = y2[!is.na(y2$N)]$`Origen de la campaña principal`,"E2" = y2[!is.na(y2$N)]$`Descripción del cierre`)
Base_X0 <- merge(Base_X0, y3, by = "Identificador de la oportunidad", all.x = TRUE)
Base_X0[!is.na(D2)]$Descripción <- paste(Base_X0[!is.na(D2)]$D2, Base_X0[!is.na(D2)]$Descripción)
Base_X0[!is.na(C2)]$`Origen de la campaña principal` <- Base_X0[!is.na(C2)]$C2
Base_X0[!is.na(E2)]$`Descripción del cierre` <- paste(Base_X0[!is.na(E2)]$E2, Base_X0[!is.na(E2)]$`Descripción del cierre`)
Base_X0 <- subset(Base_X0, select = -D2)
Base_X0 <- subset(Base_X0, select = -C2)
Base_X0 <- subset(Base_X0, select = -E2)
#####-----------------

### Complementa la tabla Y0 con Productos
Base_Y0 <- merge(Base_Y0, Tprod, by.x = "Nombre del producto", by.y = "Producto de mayor ponderacion", all.x = TRUE)

### Crea bandera de Estatus del Paso Siguiente
Base_X0$Estatus_paso_siguiente <- if_else(as.Date(Base_X0$`Fecha de ejecución del paso siguiente`) < Sys.Date(),"Vencido","Vigente")

# Agrega el Mercado y cruza con los casos
Base_X0 <- merge(Base_X0, MeDi, by ="Div / Sect 2")
Base_X0 <- merge(Base_X0, casos_Final, by ="Identificador de la oportunidad", all.x = TRUE)
Base_X0[Caso_Abierto %in% c(0,1)]$Rango_Caso <- ifelse(Base_X0[Base_X0$Caso_Abierto %in% c(0,1)]$Antigüedad_Caso > 20,"4. Más de 20 días",ifelse(Base_X0[Base_X0$Caso_Abierto %in% c(0,1)]$Antigüedad_Caso > 10,"3. De 11 a 20 días",ifelse(Base_X0[Base_X0$Caso_Abierto %in% c(0,1)]$Antigüedad_Caso > 5,"2. De 6 a 10 días","1. Menos de 5 días")))

# marca Etapas y estatus de Postmortem analizado por Ramón Ramírez
Base_X0 <- merge(Base_X0, etapasopp, by ="Etapa", sort = FALSE )
#Base_X0[Etapa == "Prospectación (FP)"]$Etap_Camp <- "Prospección"
#Base_X0[Etapa %in% c("Oportunidad", "Aproximación","Contacto") ]$Etap_Camp <- "Temprana"
Base_X0 <- merge(Base_X0, PM, by.x ="Identificador de la oportunidad", by.y = "ID", all.x = TRUE, sort = FALSE)
  # identifica las oportunidades que no tienen "Conclusión de cierre" pero si tienen "Descripción de Cierre"
c <- is.na(Base_X0$`Conclusión de cierre`) & !is.na(Base_X0$`Descripción del cierre`)
Base_X0[c]$`Conclusión de cierre` <- Base_X0[c]$`Descripción del cierre`
Base_X0[Etapa == "PERDIDA" &  !(Clasificación_EAE %in% NoPEx)]$Etap_Camp <- "No exitosa"


Div_Gerencia <- unique(data.table(Gerencia = Base_X0$Gerencia,`Div / Sect 2` = Base_X0$`Div / Sect 2`))[order(-`Div / Sect 2`)]
ctes <- unique(subset(Base_X0, Cerrado == 0)$`Nombre de la cuenta`)

## Cambia de Campaña según producto
x <- as.character(unique(p_campa$`Campaña Reporte`))
Base_X0 <- merge(Base_X0, a_campa[,1:2], by.x = "Origen de la campaña principal", by.y = "Campaña_Sactel", all.x = TRUE)
u1 <- names(Base_X0)
names(Base_X0) <- c(u1[1:length(u1)-1],"Campaña_Prod")
for(i in 1:length(x)){
  Base_X0[Etap_Camp != "Prospección" & Campaña_Prod == x[i]]$Campaña_Prod <- NA
  c <- as.character(p_campa[`Campaña Reporte` == x[i]]$Producto)
  u1 <- as.character(unique(Base_Y0[`Nombre del producto` %in% c]$`Identificador de la oportunidad`))
  y1 <- min(as.Date(a_campa[`Campaña Reporte` == x[i]]$`Fecha de inicio`))
  Base_X0[`Identificador de la oportunidad` %in% u1 & is.na(Campaña_Prod) & as.Date(strftime(`Fecha de cierre`,"%Y-%m-01")) >= y1 ]$Campaña_Prod <- x[i]
}

## Cambia las fechas a tipo Date con Lubridate
Base_X0$`Fecha de creación` <- ymd(Base_X0$`Fecha de creación`)
Base_X0$`Fecha de ejecución del paso siguiente` <- ymd(Base_X0$`Fecha de ejecución del paso siguiente`)
Base_X0$`Fecha de cierre` <- ymd(Base_X0$`Fecha de cierre`)

a_campa <- a_campa[,1:2]

######3 Especial para Campaña de Sucursales ----------------
y1 <- "sucursales a blindar"
u1 <- Base_X0$Descripción
y2 <- regexpr(y1,u1,ignore.case = TRUE)
Base_X0$sucursales <- as.numeric(gsub(",","",str_extract(substr(u1,y2,nchar(u1)),"\\d+\\,*\\d*")))
Base_X0$sucursales[y2 == -1] <- NA

y1 <- "sucursales"
u1 <- Base_X0$`Descripción del cierre`
y2 <- regexpr(y1,u1,ignore.case = TRUE)
Base_X0$suc_ganadas <- as.numeric(gsub(",","",str_extract(substr(u1,y2,nchar(u1)),"\\d+\\,*\\d*")))
Base_X0$suc_ganadas[y2 == -1] <- NA
Base_X0[ Etapa == "GANADA" & is.na(suc_ganadas) & !is.na(sucursales)]$suc_ganadas <- Base_X0[ Etapa == "GANADA" & is.na(suc_ganadas) & !is.na(sucursales)]$sucursales

######################################-------------------

#Actualiza Giro por el CUC
Base_X0 <- merge(Base_X0,subset(G_x_C,select = c("CUC","Vertical")), by="CUC", all.x = TRUE)
Base_X0[!is.na(Vertical)]$Giro <- Base_X0[!is.na(Vertical)]$Vertical
Base_X0 <- subset(Base_X0, select = -Vertical)

####### Agrega el campo "Mercado" en la base de Tenencia
#aTen <- merge(aTen, MeDi, by = "Div / Sect 2", all.x = TRUE)
#cTen <- merge(cTen, MeDi, by = "Div / Sect 2", all.x = TRUE)

load(paste0(dire1c,"insumos/a_p/cTen_actual.RDATA"))

###### archivo de Metas en unidades
Met2019 <- as.data.table(read_excel(paste0(dire1c,"insumos/Meta 2019 en unidades.xlsx")))
Meta19 <- as.data.table(gather(subset(Met2019,!is.na(Gerencia), select = -`% Part.`),"Nombre_corto_Producto","CANTIDAD_FINAL",4:(ncol(Met2019)-1) ))
Meta19 <- merge(Meta19, unique(subset(Tprod, select = -`Producto de mayor ponderacion`)), by.x = "Nombre_corto_Producto", by.y = "Nombre_corto_Producto", all.x = TRUE)
b <- unique(subset(Tprod, select = c("Familia","Negocio")))
names(b) <- c("Familia","NEG")
Meta19 <- merge(Meta19,b, by.x = "Nombre_corto_Producto", by.y = "Familia", all.x = TRUE)
Meta19[is.na(Familia)]$Familia <- Meta19[is.na(Familia)]$Nombre_corto_Producto
Meta19[is.na(Negocio)]$Negocio <- Meta19[is.na(Negocio)]$NEG
Meta19[is.na(Negocio)]$Negocio <- Meta19[is.na(Negocio)]$Nombre_corto_Producto
Meta19 <- subset(Meta19, select = -NEG)
Meta19$suma_campo <- NA
#Meta19$Fech_inventario <- as.Date("01-12-2018","%d-%m-%Y")

#Meta19 <- rbind(subset(aTen, !is.na(Gerencia) & Fech_inventario == max(Meta19$Fech_inventario), select = names(Meta19)), Meta19)

rm(a,b,i,Met2019,casos_01,casos_02,casos_03,x,y1,y2,y3,y4,y5,u1,c,p_campa,client3,cliente,manuales,opps_total,uni_ks_opps)

### función que cruza los estatus - etapas e identifica cuales opp son nuevas
f_cruza <- function(x0, fechas, TProd){
  # Crea el campo -Días en Temprana- como D_Temprana
  x0$D_Temprana <- ifelse(is.na(x0$`Días en Oportunidad`),0,x0$`Días en Oportunidad`) + ifelse(is.na(x0$`Días en Aproximación`),0,x0$`Días en Aproximación`) + ifelse(is.na(x0$`Días en Contacto`),0,x0$`Días en Contacto`)
  
  # marca las nuevas
  x0$Nueva <- 0
  x0$vivI <- as.Date(x0$`Fecha de Temprana`)
  x0[x0$vivI > fechas[1] & x0$vivI <= fechas[2] ]$Nueva <- 1
  x0$vive <- as.Date(x0$`Fecha de cierre`)
  
  # Pone el nombre corto usado en el catálogo -TProd-
  paso0F <- merge(x0, TProd, all.x = TRUE, by = "Producto de mayor ponderacion")
  paso0F$Familia[is.na(paso0F$Familia)] <- "Otros"
  paso0F$Negocio[is.na(paso0F$Negocio)] <- "Otros"
  
  # marca la etapa final
  paso00 <- subset(x0, select = c("Identificador de la oportunidad","Etap_Camp","vive","Cerrado","vivI"))
  paso00$EtapF <- paso00$Etap_Camp
  paso00[ Cerrado == 1 & (vive < fechas[1] | vive > fechas[2]) ]$EtapF <- " "
  paso00[ Cerrado == 0 & vivI > fechas[2] ]$EtapF <- " "
  
  paso01 <- subset(paso00, select = c("Identificador de la oportunidad","EtapF"))
  paso0F <- merge(paso0F, paso01, by ="Identificador de la oportunidad" , all.x = TRUE, sort = FALSE )
  
  # Señala las OPP -Inicial-
  paso0F$inicio <- " "
  paso0F$inicio[ paso0F$vivI <= fechas[1] & ( paso0F$Cerrado == 0 | paso0F$vive >= fechas[1] ) ] <- "Inicial"
  
  # Señala Opps -Final-
  paso0F$f1nal <- " "
  paso0F$f1nal[ paso0F$vivI <= fechas[2] & ( paso0F$Cerrado == 0 | paso0F$vive > fechas[2] ) ] <- "Final"
  
  # Filtra las oportunidades que nos interesan
  #paso1F <- subset(paso0F, EtapF != " " | inicio == "Inicial" | f1nal == "Final" | Nueva == 1)
  paso1F <- paso0F
  
  #Inicializa Cantidad e Importe
  paso1F$Cant.1 <- 1
  #paso1F$Serv.1 <- 0
  #paso1F$Impo.1 <- 0
  
  return(paso1F)
}
#########

### Función que le cambia los nombres de los campos
f_campos <- function(x1, total_campos, etapas, Tabla_1_PR, Tabla_2_DV){
  
  campo_by_PR <- names(Tabla_1_PR)[1]
  agrupador_PR <- names(Tabla_1_PR)[2]
  
  campo_by_DV <- names(Tabla_2_DV)[1]
  agrupador_DV <- names(Tabla_2_DV)[2]
  
  # Establece los campos llave
  x1$llaveP <- paste(x1[[campo_by_PR]],x1[[agrupador_PR]])
  x1$llaveD <- paste(x1[[campo_by_DV]],x1[[agrupador_DV]])
  
  x2 <- x1 # subset(x1, x1$EtapF %in% etapas)
  
  # Calcula los días en etapas indicadas
  x2$Dias_Tabla <- 0
  if( is.null(etapas) ){ x2$Dias_Tabla <- x2$D_Temprana + ifelse(is.na(x2$`Días en Proyecto`),0,x2$`Días en Proyecto`) + ifelse(is.na(x2$`Días en Propuesta`),0,x2$`Días en Propuesta`) }
  
  if("Temprana" %in% etapas ) x2$Dias_Tabla <- x2$Dias_Tabla + x2$D_Temprana
  if("Proyecto" %in% etapas ) x2$Dias_Tabla <- x2$Dias_Tabla + ifelse(is.na(x2$`Días en Proyecto`),0,x2$`Días en Proyecto`)
  if("Propuesta" %in% etapas ) x2$Dias_Tabla <- x2$Dias_Tabla + ifelse(is.na(x2$`Días en Propuesta`),0,x2$`Días en Propuesta`)
  
  # Establece los rangos y asigna en la variable bandera
  x2$bandera <-"Más de 15 Días"
  x2$bandera[x2$Dias_Tabla <= 15 ] <- "7 - 15 Días"
  x2$bandera[x2$Dias_Tabla < 7 ] <- "< 7 Días"
  
  # hace el cambio de nombre de los campos
  a <- x2[order(-`Total de Proyecto`, -`Importe Anual`)]
  b <- data.frame("Campo" = names(a))
  c <- merge(b,total_campos, by = "Campo", sort = FALSE, all.x = TRUE)
  names(a) <- as.character(c$Final)
  
  return(a)
}
############

#Función para hacer las tablas por División y Producto
f_tabla <- function(x2, y2, fechas, Tabla_ag, nivel = 2, camb_etapa = FALSE, et_actual = FALSE, campo_suma, v2sta, Tenencia, Metas ){
  
  campo_by <- names(Tabla_ag)[1]
  agrupador <- names(Tabla_ag)[2]
  Tabla_ag$llave <- paste(Tabla_ag[[campo_by]],Tabla_ag[[agrupador]])
  x1 <- x2
 
  if(agrupador %in% c("Mercado","Div / Sect 2","Gerencia")){
    x1$llave <- x1$llaveD
  }
  else{
    x1$llave <- x1$llaveP
  }
  
  if(names(x2)[3] != names(y2)[3] && campo_suma %in% c("Servicios","Importe")){x1 <- y2}
  x1$suma_campo <- x1[[campo_suma]]
  
  if( nivel == 1 ){
    x1$llave <- paste(x1[[agrupador]])
    Tabla_ag$llave <- paste(Tabla_ag[[agrupador]])
    #Tenencia$llave <- paste(Tenencia[[agrupador]])
    #Metas$llave <- paste(Metas[[agrupador]])
    Tabla_ag[[campo_by]] <- NA
    Tabla_ag <- unique(Tabla_ag)
  }
  
  if(v2sta == 1){
      a  <- x1[ inicio == "Inicial", .(Inicial=sum(suma_campo)) , by = llave]
      b2 <- merge(a, x1[Nueva == 1, .(Nueva=sum(suma_campo)) , by = llave], by = "llave", all = TRUE)
      b3 <- merge(b2, x1[EtapF == "Ganada", .(Ganada=sum(suma_campo)) , by = llave], by = "llave", all = TRUE)
      b4 <- merge(b3, x1[EtapF == "No exitosa", .("No Exitosa"=sum(suma_campo)) , by = llave], by = "llave", all = TRUE)
      b5 <- merge(b4, x1[EtapF == "Perdida", .(Perdida=sum(suma_campo)) , by = llave], by = "llave", all = TRUE)
      b1 <- merge(b5, x1[f1nal == "Final", .(Final=sum(suma_campo)) , by = llave], by = "llave", all = TRUE)
      b1$b1anco <- 0
      
      b8 <- b1
      caso <- " "
      
      encabezado <- c("agrupador","llave","Fam","Inicial","Nueva","Ganada","No exitosa","Perdida","Final"," ")
          
      if( et_actual ){
        caso <- "detalle"
        bX_1 <- x1[x1$EtapF == "Temprana"] %>% group_by(llave) %>% summarise(Temprana = sum(suma_campo), Dilación_1 = sum(x1$D_Temprana, na.rm = TRUE) ) 
        b6 <- merge(b1, bX_1, by = "llave", all = TRUE)
        b6$Dilación_1 <- b6$Dilación_1/b6$Temprana
        
        b7 <- merge(b6, x1[x1$EtapF == "Proyecto"] %>% group_by(llave) %>% summarise(Proyecto = sum(suma_campo), Dilación_2 = sum(x1$`Días en Proyecto`, na.rm = TRUE)), 
                           by = "llave", all = TRUE)
        b7$Dilación_2 <- b7$Dilación_2/b7$Proyecto
        
        b8 <- merge(b7, x1[x1$EtapF == "Propuesta"] %>% group_by(llave) %>% summarise(Propuesta = sum(suma_campo), Dilación_3 = sum(x1$`Días en Propuesta`, na.rm = TRUE)), 
                           by = "llave", all = TRUE)
        b8$Dilación_3 <- b8$Dilación_3/b8$Propuesta
      }
      #####
      
      if(camb_etapa){
        #u1 <- x1[as.Date(x1$`Fecha de Temprana`) >= fechas[1] & as.Date(x1$`Fecha de Temprana`)<= fechas[2]] %>% group_by(llave) %>% summarise(E.Temprana = .N)
        u2 <- x1[as.Date(x1$`Fecha de Proyecto`) >= fechas[1] & as.Date(x1$`Fecha de Proyecto`)<= fechas[2]] %>% group_by(llave) %>% summarise(E.Proyecto = sum(suma_campo))
        u3 <- merge(u2,x1[as.Date(x1$`Fecha de Propuesta`) >= fechas[1] & as.Date(x1$`Fecha de Propuesta`)<= fechas[2]] %>% group_by(llave) %>% summarise(E.Propuesta = sum(suma_campo)), by = "llave", all = TRUE)
        b8 <- merge(b8, u3, by = "llave", all = TRUE) 
      }
      
  } #cierra el if visita == 1
  
  if(v2sta == 2){
    if( nivel == 1 ){
      Tenencia$llave <- paste(Tenencia[[agrupador]])
      Metas$llave <- paste(Metas[[agrupador]])
    }
    
    if( nivel == 2 ){
      Tenencia$llave <- paste(Tenencia[[campo_by]], Tenencia[[agrupador]])
      Metas$llave <- paste(Metas[[campo_by]], Metas[[agrupador]])
    }
    
    finv <-  max(as.Date(names(Tenencia),"%Y-%m-%d"), na.rm = TRUE)
    
    if(campo_suma %in% c("Servicios")){
      Tenencia$suma_campo <- Tenencia[[as.character(finv)]]
      Tenencia$suma_GANA <- Tenencia$ganAn
      Metas$suma_campo <- Metas$CANTIDAD_FINAL
    }
    else{
      Tenencia$suma_campo <- NA
      Tenencia$suma_GANA <- NA
      Metas$suma_campo <- NA
    }
    
    
     
    T1 <- Tenencia[ !is.na(Mercado),.(Inventario = sum(suma_campo)), by = llave]
    G1 <- merge(T1, Tenencia[ !is.na(Mercado),.(Ganancia = sum(suma_GANA)), by = llave], by = "llave", all = TRUE)
    b0 <- merge(G1, Metas[,.(Meta = sum(suma_campo)), by = llave], by = "llave", all = TRUE)
    b0$Avance <- 0
    b0$b1anco <- 0
    b1 <- merge(b0, x1[Etap_Camp %in% c("Temprana","Proyecto","Propuesta"), .(Oportunidades=sum(suma_campo)) , by = llave], by = "llave", all = TRUE)
    b2 <- merge(b1, x1[Etap_Camp %in% c("Proyecto","Propuesta"), .(Avanzadas=sum(suma_campo)) , by = llave], by = "llave", all = TRUE)
    b2$b2anco <- 0
    m1 <- Sys.Date()
    b3 <- merge(b2, x1[Etap_Camp %in% c("Proyecto","Propuesta") & strftime(Cierre,"%m-%Y") == strftime(m1,"%m-%Y"), .(mes1=sum(suma_campo)) , by = llave], by = "llave", all = TRUE)
    m2 <- as.Date(paste0(year(m1+32),"-",month(m1+32),"-01"),"%Y-%m-%d")
    b4 <- merge(b3, x1[Etap_Camp %in% c("Proyecto","Propuesta") & strftime(Cierre,"%m-%Y") == strftime(m2,"%m-%Y"), .(mes2=sum(suma_campo)) , by = llave], by = "llave", all = TRUE)
    m3 <- as.Date(paste0(year(m2+32),"-",month(m2+32),"-01"),"%Y-%m-%d")
    b5 <- merge(b4, x1[Etap_Camp %in% c("Proyecto","Propuesta") & strftime(Cierre,"%m-%Y") == strftime(m3,"%m-%Y"), .(mes3=sum(suma_campo)) , by = llave], by = "llave", all = TRUE)
    m4 <- as.Date(paste0(year(m3+32),"-",month(m3+32),"-01"),"%Y-%m-%d")
    b6 <- merge(b5, x1[Etap_Camp %in% c("Proyecto","Propuesta") & strftime(Cierre,"%m-%Y") == strftime(m4,"%m-%Y"), .(mes4=sum(suma_campo)) , by = llave], by = "llave", all = TRUE)
    
    b8 <- b6
    caso <- " "
    
    encabezado <- c("agrupador","llave","Fam",paste("Inv.",strftime(finv,"%b-%y")),"Ganancia","Meta","Avance","bl1","Total Opp","Proy-Prop","bl2",strftime(m1,"%b-%y"),strftime(m2,"%b-%y"),strftime(m3,"%b-%y"),strftime(m4,"%b-%y"))
  } #cierra el if vista == 2
  
  b <- merge(Tabla_ag, b8, by = "llave")
  b <- b[order(b[[agrupador]])]
  b <- data.table(agrupador = b[[agrupador]], subset(b, select = as.character(names(b)[grep(agrupador,names(b), invert = TRUE)][1:(ncol(b)-1)])) )
  
  
  if( nrow(b)>1 ){
    c1 <- unique(b$agrupador)
    s <- ncol(b)
    k <- b[0,]
    for(i in 1:length(c1)){
      c2 <- b[b$agrupador == c1[i]]
      c3 <- data.frame(agrupador = c1[i], llave = c1[i], Div = " ",t(apply( c2[,4:s],2, function(x)sum(x, na.rm = TRUE))) )
      c2[,1] <- " "
      if(caso == "detalle"){
        c3$Dilación_1 <- as.numeric( x1[x1[[agrupador]] == c1[i] & x1$EtapF == "Temprana"] %>% summarise(sum(D_Temprana, na.rm = TRUE) ) )/ c3$Temprana
        c3$Dilación_2 <- as.numeric( x1[x1[[agrupador]] == c1[i] & x1$EtapF == "Proyecto"] %>% summarise(sum(`Días en Proyecto`, na.rm = TRUE) ) )/ c3$Proyecto
        c3$Dilación_3 <- as.numeric( x1[x1[[agrupador]] == c1[i] & x1$EtapF == "Propuesta"]%>% summarise(sum(`Días en Propuesta`, na.rm = TRUE) ) )/ c3$Propuesta
      }
      names(c3) <- names(c2)
      
      if(c1[i] %in% c("Otros","7. Otros")){c4 <- c3}else{c4 <- rbind(c3,c2)}
      #if(c1[i] == "Otros"){c4 <- c3}
      #if(c1[i] != "Otros"){c4 <- rbind(c3,c2)}
      k <- rbind(k,c4)
      
    }
    if(length(c1)>1){
      c5 <- data.frame(agrupador = "TOTAL", llave = "TOTAL", Div = " ",t(apply(b[,4:s],2,function(x)sum(x,na.rm = TRUE))) )
      if(caso == "detalle"){
        c5$Dilación_1 <- as.numeric( x1[x1$EtapF == "Temprana"] %>% summarise(sum(D_Temprana, na.rm = TRUE) ) )/ c5$Temprana
        c5$Dilación_2 <- as.numeric( x1[x1$EtapF == "Proyecto"] %>% summarise(sum(`Días en Proyecto`, na.rm = TRUE) ) )/ c5$Proyecto
        c5$Dilación_3 <- as.numeric( x1[x1$EtapF == "Propuesta"]%>% summarise(sum(`Días en Propuesta`, na.rm = TRUE) ) )/ c5$Propuesta
      }
      names(c5) <- names(b)
      k <- rbind(k,c5)
    }
    
    b <- k
    
  }
  
  if( v2sta == 2 ){
    b$b2anco <- NA
    b$Avance <- b$Ganancia/b$Meta
  }
  b$b1anco <- NA
  
  
  if(caso == "detalle"){
    encabezado <- c(encabezado,"Temprana","Dila","Proyecto","Dila ","Propuesta","Dila  ")
  }
  
  if(camb_etapa){
    encabezado <- c(encabezado,"E.Proyecto","E.Propuesta") 
  }
  
  names(b) <- encabezado
  
  if( nivel == 1 ){
    b <- subset(b, agrupador != " ")
  }
  
 
  
  return(b)
}
#######

#### Función para tabla de Dilación
f_2_dila <- function(x2, Tabla_ag, nivel = 2){
  
  campo_by <- names(Tabla_ag)[1]
  agrupador <- names(Tabla_ag)[2]
  
  Tabla_ag$llave <- paste(Tabla_ag[[campo_by]],Tabla_ag[[agrupador]])
  x2$llave <- x2$llaveP
  if(agrupador %in% c("Mercado","Div / Sect 2")){
    x2$llave <- x2$llaveD
  }
  
  a_01 <- x2[bandera == "< 7 Días"] %>% group_by(llave) %>% summarise("< 7 Días" = .N)
  a_02 <- merge(a_01, x2[bandera == "7 - 15 Días"] %>% group_by(llave) %>% summarise("7 - 15 Días" = .N), by = "llave", all = TRUE)
  a_03 <- merge(a_02, x2[bandera == "Más de 15 Días"] %>% group_by(llave) %>% summarise("Más de 15 Días" = .N), by = "llave", all = TRUE)
  a_04 <- merge(a_03, x2 %>% group_by(llave) %>% summarise("Total" = .N), by = "llave", all = TRUE)
  
  b <- merge(Tabla_ag, a_04, by = "llave")
  b <- b[order(b[[agrupador]])]
  b <- data.table(agrupador = b[[agrupador]], subset(b, select = as.character(names(b)[grep(agrupador,names(b), invert = TRUE)][1:(ncol(b)-1)])) )
  
  
  
  if( nrow(b)>1){
    c1 <- unique(b$agrupador)
    s <- ncol(b)
    k <- b[0,]
    for(i in 1:length(c1)){
      c2 <- b[b$agrupador == c1[i]]
      c3 <- data.frame(agrupador = c1[i], llave = c1[i], Div = " ",t(apply( c2[,4:s],2, function(x)sum(x, na.rm = TRUE))) )
      c2[,1] <- " "
      
      names(c3) <- names(c2)
      
      if(c1[i] == "Otros"){c4 <- c3}
      if(c1[i] != "Otros"){c4 <- rbind(c3,c2)}
      k <- rbind(k,c4)
      
    }
    if(length(c1)>1){
      c5 <- data.frame(agrupador = "TOTAL", llave = "TOTAL" , Div = " ",t(apply(b[,4:s],2,function(x)sum(x,na.rm = TRUE))) )
      names(c5) <- names(b)
      k <- rbind(k,c5)
    }
    
    b <- k
    
  }
  
  names(b) <- c("agrupador","llave","Fam","< 7 Días","7 - 15 Días","Más de 15 Días","Total")
  
  
  if( nivel == 1 ){
    b <- subset(b, agrupador != " ")
  }
  
  
  return(b)
  
}
#######

# Para hacer el encabezado de las tablas por DD y Producto
f_encabezado <- function(fecha, camb_etapa = FALSE, et_actual = FALSE, v2sta, titulos){
  
  if( v2sta == 2){
      encabezado <- htmltools::withTags(table(
        class = 'display',
        thead(
          tr(
            th(rowspan = 2, colspan = 2, ' ' ),
            th(rowspan = 1, colspan = 4, HTML('<center>Situación Actual</center>')),
            th(rowspan = 2, colspan = 1, ' '),
            th(rowspan = 1, colspan = 2, HTML('<center>Opportunidades en Proceso</center>')),
            th(rowspan = 2, colspan = 1, ' '),
            th(rowspan = 1, colspan = 4, HTML('<center>Cierre Estimado</center>'))
            # c("agrupador","llave","Fam",paste("Inv.",strftime(finv,"%b-%y")),"Ganancia","Meta","Avance","bl1","Total Opp","Proyecto, Propuesta","bl2",strftime(m1,"%b-%y"),strftime(m2,"%b-%y"),strftime(m3,"%b-%y"),strftime(m4,"%b-%y"))
          ),
          tr(
            th(titulos[4]),
            th(titulos[5]),
            th(titulos[6]),
            th(titulos[7]),
            th("Total"),
            th("Proy/Prop"),
            th(titulos[12]),
            th(titulos[13]),
            th(titulos[14]),
            th(titulos[15])
          )
        )
      ))
     return(encabezado)
  }
  
  if(fecha[2] > fecha[1]+1){
    abc <- paste0('<center>Opps del ', strftime(fecha[1]+1,format = "%d de %B ")," al ", strftime(fecha[2],format = "%d de %B %Y"),"</center>")
  }
  else{
    abc <- paste0('<center>Oportunidades del ', strftime(fecha[2],format = "%d de %B %Y"),"</center>")
  }
  
  if( et_actual && camb_etapa){
    encabezado <- htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(rowspan = 2, colspan = 2, ' ' ),
          th(rowspan = 2, paste0('Inicial: ',strftime(fecha[1],format = "%d de %B"))),
          th(colspan = 5, HTML( abc )),
          th(rowspan = 2, colspan = 1, ' '),
          th(colspan = 2, HTML('<center>Temprana</center>')),
          th(colspan = 2, HTML('<center>Proyecto</center>')),
          th(colspan = 2, HTML('<center>Propuesta</center>')),
          th(colspan = 2, HTML('<center>Cambio de Etapa</center>'))
        ),
        tr(
          th('Nueva'),
          th('Ganada'),
          th('No Exitosa'),
          th('Perdida'),
          th('Final'),
          th('Cant.'),
          th('Dila'),
          th('Cant.'),
          th('Dila'),
          th('Cant'),
          th('Dila'),
          th('Temprana -> Proyecto'),
          th('Proyecto -> Propuesta')
        )
      )
    ))
  }
  else if( et_actual && !camb_etapa ){
       encabezado <- htmltools::withTags(table(
          class = 'display',
          thead(
            tr(
              th(rowspan = 2, colspan = 2, ' ' ),
              th(rowspan = 2, paste0('Inicial: ',strftime(fecha[1],format = "%d de %B %Y"))),
              th(colspan = 5, HTML( abc )),
              th(rowspan = 2, colspan = 1, ' '),
              th(colspan = 2, HTML('<center>Temprana</center>')),
              th(colspan = 2, HTML('<center>Proyecto</center>')),
              th(colspan = 2, HTML('<center>Propuesta</center>'))
            ),
            tr(
              th('Nueva'),
              th('Ganada'),
              th('No Exitosa'),
              th('Perdida'),
              th('Final'),
              th('Cant.'),
              th('Dila'),
              th('Cant.'),
              th('Dila'),
              th('Cant'),
              th('Dila')
            )
          )
        ))
  }
  else if(camb_etapa && !et_actual){
    encabezado <- htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(rowspan = 2, colspan = 2, ' ' ),
          th(rowspan = 2, paste0('Inicial: ',strftime(fecha[1],format = "%d de %B %Y"))),
          th(colspan = 5, HTML( abc )),
          th(rowspan = 2, colspan = 1, ' '),
          th(colspan = 2, HTML('<center>Cambio de Etapa</center>'))
        ),
        tr(
          th('Nueva'),
          th('Ganada'),
          th('No Exitosa'),
          th('Perdida'),
          th('Final'),
          th('Temprana -> Proyecto'),
          th('Proyecto -> Propuesta')
        )
      )
    ))
    
  }
  else{
    encabezado <- htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(rowspan = 2, colspan = 2, ' ' ),
          th(rowspan = 2, paste0('Inicial: ',strftime(fecha[1],format = "%d de %B %Y"))),
          th(colspan = 5, HTML( abc )),
          th(rowspan = 2, colspan = 1, ' ')
        ),
        tr(
          th('Nueva'),
          th('Ganada'),
          th('No Exitosa'),
          th('Perdida'),
          th('Final')
       )
      )
    ))
  }
    
return(encabezado)
}
#####

#### Función para tabla de Casos Finanzas
f_3_Casos <- function(x1, Tabla_ag, Horz_columna, sta_casos, fechas, campo_suma){
  
  campo_by <- names(Tabla_ag)[1]
  agrupador <- names(Tabla_ag)[2]
  
  
  x1 <- x1[is.na(x1$`Fecha Cierre del Caso`) | (as.Date(x1$`Fecha Cierre del Caso`) >= fechas[1] & as.Date(x1$`Fecha Cierre del Caso`) <= fechas[2])]
  
  if( sta_casos == "Abiertos"){
    x2 <- x1[ Caso_Abierto == 1]
  }
  else{
    x2 <- x1[ Caso_Abierto == 0]
  }
  
  
  Tabla_ag$llave <- paste(Tabla_ag[[campo_by]],Tabla_ag[[agrupador]])
  x2$llave <- x2$llaveP
  if(agrupador %in% c("Mercado","Div / Sect 2")){
    x2$llave <- x2$llaveD
  }
  
  x2$suma_campo <- x2[[campo_suma]]
  
  u1 <- unique(x2[[Horz_columna]])
  u1 <- u1[!is.na(u1)]
  u1 <- u1[order(u1)]
  
  a1 <- x2[x2[[Horz_columna]] == u1[1] ] %>% group_by(llave) %>% summarise( i = sum(suma_campo, na.rm = TRUE ))
  names(a1) <- c("llave",u1[1])
  
  for(i in 2:length(u1)){
    campo_uso <- names(a1)
    a1 <- merge(a1, x2[x2[[Horz_columna]] == u1[i] ] %>% group_by(llave) %>% summarise( i = sum(suma_campo, na.rm = TRUE )) , by = "llave", all = TRUE )
    names(a1) <- c(campo_uso,u1[i])
  }
  
  #Hace la columna "TOTAL"
  campo_uso <- names(a1)
  a1 <- merge(a1, x2[ !is.na(x2[[Horz_columna]]) ] %>% group_by(llave) %>% summarise( i = sum(suma_campo, na.rm = TRUE )) , by = "llave", all = TRUE )
  names(a1) <- c(campo_uso,"TOTAL")
  
  
  b <- merge(Tabla_ag, a1, by = "llave")
  b <- b[order(b[[agrupador]])]
  b <- data.table(agrupador = b[[agrupador]], subset(b, select = as.character(names(b)[grep(agrupador,names(b), invert = TRUE)][1:(ncol(b)-1)])) )
  
  
  
  if( nrow(b)>1){
    c1 <- unique(b$agrupador)
    s <- ncol(b)
    k <- b[0,]
    for(i in 1:length(c1)){
      c2 <- b[b$agrupador == c1[i]]
      c3 <- data.frame(agrupador = c1[i], llave = c1[i], Div = " ",t(apply( c2[,4:s],2, function(x)sum(x, na.rm = TRUE))) )
      c2[,1] <- " "
      
      names(c3) <- names(c2)
      
      if(c1[i] == "Otros"){c4 <- c3}
      if(c1[i] != "Otros"){c4 <- rbind(c3,c2)}
      k <- rbind(k,c4)
      
    }
    if(length(c1)>1){
      c5 <- data.frame(agrupador = "TOTAL", llave = "TOTAL" , Div = " ",t(apply(b[,4:s],2,function(x)sum(x,na.rm = TRUE))) )
      names(c5) <- names(b)
      k <- rbind(k,c5)
    }
    
    b <- k
    
  }
  
  return(b)
  
}
#######

#### Función para tabla de Campañas
f_campania <- function(x02, campa_mostrar, campo_suma, Tipo ){
  
  # Dejar solo las opps que están en las campañas deseadas, como llave queda el nombre de la Campaña
  x1 <- merge(x02, campa_mostrar, by.x = "Campaña", by.y = "Campaña_Sactel", all.x = TRUE)
  a1 <- unique(data.table("llave" = campa_mostrar$`Campaña Reporte`))
  ultimo <- names(x1)[ncol(x1)]
  x1$llave <- x1[[ultimo]]

  # establece el campo que se usará para sumar 
  x1$suma_campo <- x1[[campo_suma]]
  
  bI <- merge(a1, x1[, .("Clientes Prospectos" = sum(suma_campo)) , by = Campaña_Prod], by.x = "llave", by.y = "Campaña_Prod", all.x = TRUE)
  b0 <- merge(bI, x1[Etap_Camp == "Prospección", .("Pendiente Visitar" = sum(suma_campo)) , by = llave], by = "llave", all.x = TRUE)
  
  if( Tipo == "T"){
    # Se crean las columnas de la tabla del Periodo
    b1 <- merge(b0, x1[EtapF == "Temprana", .(Temprana = sum(suma_campo)) , by = Campaña_Prod], by.x = "llave", by.y = "Campaña_Prod", all.x = TRUE)
    b2 <- merge(b1, x1[EtapF == "Proyecto", .(Proyecto = sum(suma_campo)) , by = Campaña_Prod], by.x = "llave", by.y = "Campaña_Prod", all.x = TRUE)
    b3 <- merge(b2, x1[EtapF == "Propuesta", .(Propuesta = sum(suma_campo)) , by = Campaña_Prod], by.x = "llave", by.y = "Campaña_Prod", all.x = TRUE)
    b4 <- merge(b3, x1[EtapF == "No exitosa", .("No exitosa"= sum(suma_campo)) , by = Campaña_Prod], by.x = "llave", by.y = "Campaña_Prod", all.x = TRUE)
    b5 <- merge(b4, x1[EtapF == "Perdida", .(Perdida = sum(suma_campo)) , by = Campaña_Prod], by.x = "llave", by.y = "Campaña_Prod", all.x = TRUE)
    b  <- merge(b5, x1[EtapF == "Ganada", .(Ganada = sum(suma_campo)) , by = Campaña_Prod], by.x = "llave", by.y = "Campaña_Prod", all.x = TRUE)
  }
  if( Tipo == "R"){
    # Se crean las columnas de la tabla Resumen 
    b1 <- merge(b0, x1[Etap_Camp %in% c("Temprana","Proyecto","Propuesta"), .("En Proceso" = sum(suma_campo)) , by = Campaña_Prod], by.x = "llave", by.y = "Campaña_Prod", all.x = TRUE)
    b2 <- merge(b1, x1[Etap_Camp == "No exitosa", .("No exitosa"= sum(suma_campo)) , by = Campaña_Prod], by.x = "llave", by.y = "Campaña_Prod", all.x = TRUE)
    b3 <- merge(b2, x1[Etap_Camp == "Perdida", .(Perdida = sum(suma_campo)) , by = Campaña_Prod], by.x = "llave", by.y = "Campaña_Prod", all.x = TRUE)
    b4 <- merge(b3, x1[Etap_Camp %in% c("No exitosa","Perdida","Ganada"), .(Cerradas = sum(suma_campo)), by = Campaña_Prod], by.x = "llave", by.y = "Campaña_Prod", all.x = TRUE)
    b  <- merge(b4, x1[Etap_Camp == "Ganada", .(Ganada = sum(suma_campo)) , by = Campaña_Prod], by.x = "llave", by.y = "Campaña_Prod", all.x = TRUE)
  }
  
  
  # Agrega una fila de Total 
  s <- ncol(b)
  c3 <- data.frame(llave = "TOTAL", t(apply( b[,2:s],2, function(x)sum(x, na.rm = TRUE))) )
  names(c3) <- names(b)
  b <- rbind(b,c3)
  
  
  # Agrega el % de Avance de las Campañas
  if( Tipo == "R"){
    b$Avance <- b$Cerradas/b$`Clientes Prospectos`
    b$Eficacia <- b$Ganada/b$Cerradas
    b <- subset(b, select = -Cerradas)
  }
  
  # Cambia el nombre de la columna llave a "Campaña"
  s <- ncol(b)
  names(b) <- c("Campaña",names(b)[2:s])
  
  return(b)
  
}
#######

#### Función para tabla de Vertical x Campaña
f_Vcampania <- function(x02, Giro_Vt, campa_mostrar, campo_suma){
  
  # Dejar solo las opps que están en los Giros
  x1 <- merge(x02, Giro_Vt, by = "Giro")
  fuera <- c("Sin especificar","Agroindustria","Servicios","Medios","Construcción","Industria")
  
  a1 <- as.character(unique(campa_mostrar$`Campaña Reporte`))
  b1 <- data.table("Vertical" = unique(Giro_Vt$Vertical[!(Giro_Vt$Vertical %in% fuera)]))
  
  # establece el campo que se usará para sumar 
  x1$suma_campo <- x1[[campo_suma]]
  
  # Se crean las columnas de la tabla
  b <- b1
  for(i in 1:length(a1)){
    b <- merge(b, x1[ Campaña_Prod == a1[i],.( VSUMA = sum(suma_campo)), by = Vertical], by = "Vertical", all.x = TRUE)
    names(b) <- c(names(b)[1:ncol(b)-1],a1[i])
  }
  
  #Crea Columna de Total 
  b <- merge(b, x1[ Campaña_Prod %in% a1,.( Total = sum(suma_campo) ), by = Vertical], by = "Vertical", all.x = TRUE)
  
  c1 <- nrow(b)
  s <- ncol(b)
  
  # Agrega una fila de Total 
  if( c1 >1){
    c3 <- data.frame(llave = "TOTAL", t(apply( b[,2:s],2, function(x)sum(x, na.rm = TRUE))) )
    
    names(c3) <- names(b)
    
    b <- rbind(b,c3)
  }
  
  # Agrega el % de Avance de las Campañas
  #b$Avance <- 1 - b$Prospección/b$`Clientes en Campaña`
  
  # Cambia el nombre de la columna llave a "Campaña"
  #names(b) <- c("Campaña",names(b)[1:s+1])
  
  return(b)
  
}
#######
