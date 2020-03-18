library(readxl)
library(dplyr)
library(data.table)
library(dtplyr)
library(tidyr)
library(htmltools)
library(stringr)
library(tidyr)
library(lubridate)

rm(list = ls())

#a <- "C:/Users/larcos/M_Empresarial/Analisis/Ventas/Aplicaciones/Tablero_Seguimiento/"
a <- "C:/Users/vmorenoa/Documents/R proyectos/pruebas/"
#b <- "/srv/shiny-server/tablero_seguimiento/"
b <- "/srv/shiny-server/pruebas/"

direcc <- if_else(substr(getwd(),1,1) == "C", a , b)

#a <- "C:/Users/larcos/M_Empresarial/Analisis/Ventas/Aplicaciones/Tablero_Seguimiento/insumos/"
a <- "C:/Users/vmorenoa/Documents/R proyectos/pruebas/ArchivosContinuos/"
b <- "/srv/archivosDWH/"

direcc_opp <- if_else(substr(getwd(),1,1) == "C", a , b)

#a <- "C:/Users/larcos/M_Empresarial/Analisis/Ventas/Aplicaciones/Tablero_Seguimiento/insumos/"
a <- "C:/Users/vmorenoa/Documents/R proyectos/pruebas/insumos/"
b <- "/home/infocecor/"
direc2 <- if_else(substr(getwd(),1,1) == "C", a , b)

dire2c <- paste0(direcc,"insumos/")

if( substr(getwd(),1,1) != "C" ){
  Sys.setlocale(category = "LC_TIME" ,"es_MX.UTF-8")
  Sys.setlocale(category = "LC_COLLATE" ,"es_MX.UTF-8")
  dire2c <- "/home/estrategia/"
}

#############################
freducir <- function(x){
  y <- as.character(unique(x[,1]))
  
  k <- subset(x, x[,1] == y[1]) 
  b <- paste0(k[1,2],"($",format(floor(k[1,4]),big.mark = ",", scientific = FALSE), " Cant. " ,format(k[1,3],big.mark = ",", scientific = FALSE),")")
  if (nrow(k)>1){
    for (j in 2:nrow(k)){
      b <- paste(b,paste0(k[j,2],"($",format(floor(k[j,4]),big.mark = ",", scientific = FALSE), " Cant. " ,format(k[j,3],big.mark = ",", scientific = FALSE),")"), sep = "; ")
    } }## Cierra for y cierra If
  
  a <- data.frame("ID" = k[1,1], "resumen" = b)
  if(length(y)>1)
    for(i in 2:length(y))
    {
      k <- subset(x, x[,1] == y[i]) 
      b <- paste0(k[1,2],"($",format(floor(k[1,4]),big.mark = ",", scientific = FALSE), " Cant. " ,format(k[1,3],big.mark = ",", scientific = FALSE),")")
      if (nrow(k)>1){
        for (j in 2:nrow(k)){
          b <- paste(b,paste0(k[j,2],"($",format(floor(k[j,4]),big.mark = ",", scientific = FALSE), " Cant. " ,format(k[j,3],big.mark = ",", scientific = FALSE),")"), sep = "; ")
        } }## Cierra for y cierra If
      s <- data.frame("ID"=k[1,1],"resumen" = b)
      a <- rbind(a,s)
    }
  return(a)
}
##########################

### Para Reducir los "Asignado" en las Visitas
f_Asignado <- function(x){
  CamBase <- names(x)[1]
  sepa <- "; "
  y <- as.character(unique(x[[CamBase]]))
  
  k <- subset(x, x[[CamBase]] == y[1])
  b <- paste0(k[1,2],"(",k[1,3],")")
  if (nrow(k)>1){
    for (j in 2:nrow(k)){
      b <- paste0(b,sepa,k[j,2],"(",k[j,3],")")
    }
  }## Cierra for y cierra If
  
  a <- data.frame("ID" = k[1,1], "resumen" = b)
  if(length(y)>1)
    for(i in 2:length(y)){
      k <- subset(x, x[[CamBase]] == y[i]) 
      b <- paste0(k[1,2],"(",k[1,3],")")
      if (nrow(k)>1){
        for (j in 2:nrow(k)){
          b <- paste0(b,sepa, k[j,2],"(",k[j,3],")")
        } }## Cierra for y cierra If
      s <- data.frame("ID"= k[1,1],"resumen" = b)
      a <- rbind(a,s)
    }
  names(a) <- c("llave","Asistentes")
  return(a)
}
##########################

# Carga los archivos de Visitas
p1_visitas <- unique(as.data.table(read_excel(paste0(direcc,"insumos/Visitas_Paso1.xlsx") )))
p2_visitas <- unique(as.data.table(read_excel(paste0(direcc,"insumos/Visitas_Paso2.xlsx") )))
p3_cliente <- unique(as.data.table(read_excel(paste0(direcc,"insumos/Visitas_Paso3.xlsx") )))
p4_usuarios <- unique(as.data.table(read_excel(paste0(direcc,"insumos/usuarios_sactel.xlsx") )))
p4_usr2 <- p4_usuarios[,.(Perfil = max(`Perfil: Nombre`)), by = `Nombre completo`]
p2_visitas <- merge(p2_visitas, p4_usr2, by.x = "Asignado", by.y = "Nombre completo", all.x = TRUE)

#names(p4_usr2) <- c("Asignado","Perfil")
p3_cliente$`Div / Sect 2`[p3_cliente$`Div / Sect 2` == "Otros"] <- p3_cliente$`División / Sector`[p3_cliente$`Div / Sect 2` == "Otros"]
p3_cliente$l2ave <- paste(p3_cliente$`División / Sector`,p3_cliente$`Nombre de la cuenta`)

p2_visitas <- merge(p2_visitas, unique(subset(p3_cliente, select = c("Nombre de la cuenta","Div / Sect 2","Gerencia","Propietario de la cuenta","Giro"))), by.x = "Compañía / Cuenta", by.y = "Nombre de la cuenta")

b1_1 <- unite(p1_visitas, l2ave, -`Identificador de la oportunidad`, sep = " ")

a <- names(p2_visitas)[!(names(p2_visitas) %in% c("Asignado","Función asignada","Perfil"))]
b2_1 <- unite(p2_visitas,llave, a, sep = " ", remove = FALSE)
b2_2 <- subset(b2_1, select = c("llave","Asignado","Perfil"))
b2_3 <- unique(subset(b2_1, select = c("llave",a)))
b2_4 <- f_Asignado(b2_2)
b2_5 <- merge(b2_3, b2_4, by = "llave")

b2_5$l2ave <- paste(b2_5$Inicio, b2_5$Fin, b2_5$`Compañía / Cuenta`, b2_5$Asunto, b2_5$`Resultado de la actividad`,b2_5$`Relacionado con`)

visitas <- merge(b2_5, b1_1, by = "l2ave", all.x = TRUE)[!is.na(`Compañía / Cuenta`)]
#visitas$l2ave <- paste(visitas$`Div / Sect 2`,visitas$`Compañía / Cuenta`)
#visitas <- merge(visitas, unique(subset(p3_cliente, Estatus == "Activa", select = c("l2ave","Div / Sect 2","Gerencia"))), by = "l2ave", all.x = TRUE)

visitas$l2ave <- paste(visitas$Inicio, visitas$`Compañía / Cuenta`, visitas$Asunto)


vis1_R <- visitas[,.( Inicio = min(Inicio), Eventos = .N, Estado = min(Estado), Cliente = min(`Compañía / Cuenta`), "Div / Sect 2" = min(`Div / Sect 2`), Gerencia = min(Gerencia), "Propietario de la cuenta" = min(`Propietario de la cuenta`) ), by = l2ave]
vis1_R$Fecha <- as.Date(substr(vis1_R$Inicio,1,10),"%Y-%m-%d")
vis1_R$Hora <- substr(vis1_R$Inicio,12,16)
vis1_R[Hora == "00:00"]$Hora <- "Todo el día"

RollvtasTI <- c("Ventas Consultivas")
#Rollcomercial <- c("Gerente Ventas","Ejecutivo Ventas")
#RollScitum <- c("Ventas Consultivas Scitum","SCITUM")

u <- subset(b2_2, Perfil %in% RollvtasTI)
w <- unique(u$llave)
y <- subset(visitas, llave %in% w)
lzve <- unique(y$l2ave)


# Carga los archivos de oportunidades detectados

campos_tot<-c("CUC","Identificador de la oportunidad","Descripción","Clasificación del proyecto","ID_TIPO_REGISTRO_OPORTUNIDAD",
  "Tipo de registro de la oportunidad","ID de 18","Tipo de Tecnología","Ponderación","Gerencia","OPP_ESTRATEGICA_ORIGINAL",
  "¿Estratégica?","Unidad de Negocio","Giro","Origen de la campaña principal","Nombre de la cuenta","Nombre de la oportunidad",
  "Propietario de oportunidad","Tipo de proyecto","Paso siguiente","Razón de pérdida","Etapa","Clasificación","Probabilidad (%)",
  "División / Sector","Div / Sect 2","Área / Subsector","Segmento","ID_JEFE_INMEDIATO","Nombre de jefe inmediato","Antigüedad",
  "Familia producto de mayor ponderación","Producto de mayor ponderacion","Familia de productos","Nombre del producto","Razón ganada",
  "Importe Anual","Total de Proyecto","Precio total","Cantidad","Días en Oportunidad","Días en Contacto","Días en Propuesta",
  "Días en Prospectacion FP","Días en Aproximación","Días en Proyecto","Fecha de Propuesta","Fecha de Aproximación","Fecha de Proyecto",
  "Fecha de Oportunidad","Fecha de Contacto","Fecha de Temprana","Fecha de ejecución del paso siguiente","Fecha de cierre",
  "Fecha de creación","Comprometida para Roadmap","Cerrado","Descripción del cierre")

opps_total<-as.data.table(fread(paste0(direcc_opp,"SACTEL_OPPS_TOTAL.txt"), quote = "", header = FALSE, sep = "|", stringsAsFactors = F))

names(opps_total)<-campos_tot 

opps_total$`Fecha de Propuesta`<-as.Date(mdy_hms(opps_total$`Fecha de Propuesta`))
opps_total$`Fecha de Aproximación`<-as.Date(mdy_hms(opps_total$`Fecha de Aproximación`))
opps_total$`Fecha de Proyecto`<-as.Date(mdy_hms(opps_total$`Fecha de Proyecto`))
opps_total$`Fecha de Oportunidad`<-as.Date(mdy_hms(opps_total$`Fecha de Oportunidad`))
opps_total$`Fecha de Contacto`<-as.Date(mdy_hms(opps_total$`Fecha de Contacto`))
opps_total$`Fecha de Temprana`<-as.Date(mdy_hms(opps_total$`Fecha de Temprana`))
opps_total$`Fecha de ejecución del paso siguiente`<-as.Date(mdy_hms(opps_total$`Fecha de ejecución del paso siguiente`))
opps_total$`Fecha de cierre`<-as.Date(mdy_hms(opps_total$`Fecha de cierre`))
opps_total$`Fecha de creación`<-as.Date(mdy_hms(opps_total$`Fecha de creación`)-hours(ifelse(month(mdy_hms(opps_total$`Fecha de creación`)) %in% c(4:10),5,6)))

# Para la fecha de temprana (Temporal)

opps_total$`Fecha de Temprana`<-NA
opps_total[!is.na(`Fecha de Oportunidad`)]$`Fecha de Temprana`<-opps_total[!is.na(`Fecha de Oportunidad`)]$`Fecha de Oportunidad`
opps_total[is.na(`Fecha de Oportunidad`)]$`Fecha de Temprana`<-opps_total[is.na(`Fecha de Oportunidad`)]$`Fecha de creación`

opps_total<-subset(opps_total,  select = -c(ID_JEFE_INMEDIATO,OPP_ESTRATEGICA_ORIGINAL,ID_TIPO_REGISTRO_OPORTUNIDAD))

campos_uni<-c("CUC","Nombre de la oportunidad","Propietario de oportunidad","Tipo de proyecto","Paso siguiente","Razón de pérdida",
  "Etapa","División / Sector","Área / Subsector","ID_JEFE_INMEDIATO","Nombre de jefe inmediato","Fecha de creación","Días en Prospectacion FP",
  "Días en Aproximación","Días en Proyecto","OPP_ESTRATEGICA_ORIGINAL","¿Estratégica?","Familia producto de mayor ponderación",
  "Total de Proyecto","Fecha de Proyecto","Fecha de Temprana","Origen de la campaña principal","Identificador de la oportunidad",
  "Nombre de la cuenta","Descripción","Clasificación del proyecto","ID_TIPO_REGISTRO_OPORTUNIDAD","Tipo de registro de la oportunidad",
  "Fecha de ejecución del paso siguiente","Cerrado","Importe Anual","Div / Sect 2","Gerencia","Segmento","Fecha de cierre",
  "Días en Oportunidad","Días en Contacto","Días en Propuesta","Producto de mayor ponderacion","Razón ganada","Giro","Fecha de Propuesta",
  "Descripción del cierre","Productos_OPP")

uni_ks_opps <-as.data.table(fread(paste0(direcc_opp,"SACTEL_OPP_UNICAS.txt"), quote = "", header = FALSE, sep = "|", stringsAsFactors = F))
if(nrow(uni_ks_opps)<100000)stop("archivo muy pequeño")

names(uni_ks_opps)<-campos_uni

uni_ks_opps<-subset(uni_ks_opps,select=-c(ID_JEFE_INMEDIATO,OPP_ESTRATEGICA_ORIGINAL,ID_TIPO_REGISTRO_OPORTUNIDAD))
uni_ks_opps$vive<-0
CRoad<-unique(opps_total[,c("Identificador de la oportunidad","Comprometida para Roadmap")])
uni_ks_opps<-merge(uni_ks_opps,CRoad, by = "Identificador de la oportunidad")

uni_ks_opps$`Fecha de Propuesta`<-as.Date(mdy_hms(uni_ks_opps$`Fecha de Propuesta`))
uni_ks_opps$`Fecha de Proyecto`<-as.Date(mdy_hms(uni_ks_opps$`Fecha de Proyecto`))
uni_ks_opps$`Fecha de ejecución del paso siguiente`<-as.Date(mdy_hms(uni_ks_opps$`Fecha de ejecución del paso siguiente`))
uni_ks_opps$`Fecha de cierre`<-as.Date(mdy_hms(uni_ks_opps$`Fecha de cierre`))
uni_ks_opps$`Fecha de creación`<-as.Date(mdy_hms(uni_ks_opps$`Fecha de creación`)-hours(ifelse(month(mdy_hms(uni_ks_opps$`Fecha de creación`)) %in% c(4:10),5,6)))
uni_ks_opps$`Fecha de Temprana`<-as.Date(mdy_hms(uni_ks_opps$`Fecha de Temprana`))

# Para la fecha de temprana (Temporal)

uni_ks_opps<-merge(uni_ks_opps,unique(opps_total[,c("Identificador de la oportunidad","Fecha de Oportunidad")]))
uni_ks_opps$`Fecha de Temprana`<-NA
uni_ks_opps[!is.na(`Fecha de Oportunidad`)]$`Fecha de Temprana`<-uni_ks_opps[!is.na(`Fecha de Oportunidad`)]$`Fecha de Oportunidad`
uni_ks_opps[is.na(`Fecha de Oportunidad`)]$`Fecha de Temprana`<-uni_ks_opps[is.na(`Fecha de Oportunidad`)]$`Fecha de creación`
uni_ks_opps<-subset(uni_ks_opps, select = -`Fecha de Oportunidad`)

#Agrega productos nuevos encontrados en la base al catálogo -SACTEL-
Tprod <- as.data.table(read.csv(paste0(direcc,"insumos/catalogo/Productos.csv"),fileEncoding = "UTF-8-BOM"))

a <- unique(data.table(Nombre.del.producto = opps_total$`Nombre del producto`, Familia.de.productos = opps_total$`Familia de productos`))
a <- a[Nombre.del.producto!=""]
c <- a$Nombre.del.producto %in% Tprod$Nombre.del.producto
a <- a[!c]
if(nrow(a)>0){
  a$Nombre_corto_Producto <- a$Nombre.del.producto
  a$Familia <- "Otros"; a$Negocio <- "7. Otros"; a$Fuente<-"SACTEL"
  write.table(a, file = paste0(direcc,"insumos/catalogo/Productos.csv"), append = TRUE, row.names = FALSE, col.names = FALSE, sep = ",")
}

# Lee de oportunidades CECOR

tipos <- c("text","text","text","text","text","text","text","text","text","text","text","date","text","numeric","text","text","text","text","text","text","numeric","numeric","numeric","numeric","text","text","text","text","text","text","numeric","date","date","numeric","numeric","numeric","numeric","numeric","numeric","numeric","text","text","text","text","numeric","text","numeric","date","date","date","date","date","text","text","text","text")

a_p_carga <- as.data.table(read_xlsx( paste0(direc2, "FunnelCecor.xlsx"), col_types = tipos )) #, n_max = b_p_carga))

a_p_carga$`Fecha de Propuesta`<-ymd(a_p_carga$`Fecha de Propuesta`)
a_p_carga$`Fecha de Proyecto`<-ymd(a_p_carga$`Fecha de Proyecto`)
a_p_carga$`Fecha de Temprana`<-ymd(a_p_carga$`Fecha de Temprana`)
a_p_carga$`Fecha de ejecución del paso siguiente`<-ymd(a_p_carga$`Fecha de ejecución del paso siguiente`)
a_p_carga$`Fecha de cierre`<-ymd(a_p_carga$`Fecha de cierre`)
a_p_carga$`Fecha de creación`<-ymd(a_p_carga$`Fecha de creación`)
a_p_carga$`Fecha de Aproximación`<-ymd(a_p_carga$`Fecha de Aproximación`)
a_p_carga$`Fecha de Oportunidad`<-ymd(a_p_carga$`Fecha de Oportunidad`)
a_p_carga$`Fecha de Contacto`<-ymd(a_p_carga$`Fecha de Contacto`)

a_p_carga$`Fecha de Temprana` <- a_p_carga$`Fecha de creación`

x <- data.frame("id" = a_p_carga$`Identificador de la oportunidad`, "Producto" = a_p_carga$`Nombre del producto`,"Cantidad" =a_p_carga$Cantidad, "Importe" = a_p_carga$`Precio total`)

a_p_carga$Productos_OPP <- paste0(x$Producto,"($",format(floor(x$Importe),big.mark = ",", scientific = FALSE), " Cant. " ,format(x$Cantidad,big.mark = ",", scientific = FALSE),")")
a_p_carga$vive <- 0

a_p_carga$Gerencia <- a_p_carga$`Área / Subsector`

#Agrega productos nuevos encontrados en la base al catálogo -CECOR-
Tprod <- as.data.table(read.csv(paste0(direcc,"insumos/catalogo/Productos.csv"),fileEncoding = "UTF-8-BOM"))

a <- unique(data.table(Nombre.del.producto = a_p_carga$`Nombre del producto`, Familia.de.productos = a_p_carga$`Familia de productos`))
a <- a[Nombre.del.producto!=""]
c <- a$Nombre.del.producto %in% Tprod$Nombre.del.producto
a <- a[!c]
if(nrow(a)>0){
  a$Nombre_corto_Producto <- a$Nombre.del.producto
  a$Familia <- "Otros"; a$Negocio <- "7. Otros"; a$Fuente<-"CECOR"
  write.table(a, file = paste0(direcc,"insumos/catalogo/Productos.csv"), append = TRUE, row.names = FALSE, col.names = FALSE, sep = ",")
}

# Toma las tablas actuales y pega información de CECOR
uni_ks_opps$FUENTE <- "Empresarial"
uni_ks_opps$`Canal Que Genera` <- ""

#Pega la Base de Cecor en la de SACTEL
campos <- names(uni_ks_opps)
uni_ks_opps <- rbind(uni_ks_opps, unique(subset(a_p_carga, select = campos)), use.names=FALSE )

campos <- names(opps_total)
opps_total <- rbind(opps_total, subset(a_p_carga, select = campos), use.names=FALSE )


# Actualiza visitas con los productos asociados -Dentro de las OPPs-

visitas <- merge(visitas, subset(uni_ks_opps, select = c("Identificador de la oportunidad","Productos_OPP")), by="Identificador de la oportunidad", all.x = TRUE)
visitas[is.na(`Productos adicionales`) & !is.na(Productos_OPP)]$`Productos adicionales` <- visitas[is.na(`Productos adicionales`) & !is.na(Productos_OPP)]$Productos_OPP
visitas <- subset(visitas, select = -Productos_OPP)

## Termina Carga.R

## -----------------

## Archivo Inicio.R

dia01 <- if_else(strftime(Sys.Date(),format = "%A") %in% c("Monday","lunes"),Sys.Date()-3,Sys.Date()-1 )
dia02 <- dia01-6

Tprod <- subset(subset(as.data.table(read.csv(paste0(direcc,"insumos/catalogo/Productos.csv"), colClasses = "character") ), select = -Familia.de.productos), select = -Fuente)
names(Tprod) <- c("Producto de mayor ponderacion","Nombre_corto_Producto","Familia","Negocio")
etapasopp <- as.data.table(read.csv(paste0(direcc,"insumos/catalogo/Etapa.csv"), fileEncoding = "UTF-8-BOM"))
DivSec <- as.data.table(read_excel(paste0(direcc,"insumos/catalogo/DivSec.xlsx")))
Fam_Neg <- unique(data.table("Nombre_corto_Producto" = Tprod$Nombre_corto_Producto,"Familia" = Tprod$Familia, "Negocio" = Tprod$Negocio))
total_campos <- as.data.table(read_excel(paste0(direcc,"insumos/catalogo/Campos_disponibles.xlsx"), col_types = "text"))
a_campa <- as.data.table(read_excel(paste0(direcc,"insumos/catalogo/Campañas.xlsx")), col_types = c("text","text","date"))
p_campa <- as.data.table(read_excel(paste0(direcc,"insumos/catalogo/Producto-Campaña.xlsx"), col_types = "text"))

NoPEx <- c("Experiencia y Habilidades","Gestión Comercial","Precio")
c <- as.data.table(read_excel(paste0(dire2c,"Reclasificación_Postmortem.xlsx"), sheet = "Hoja1", col_types = "text"))
PM <- unique(subset(c, select = c("ID","Clasificación_EAE","Subclasificación_EAE","Conclusión de cierre","Competidor ganador")))

UComp <- unique(PM$`Competidor ganador`[!(PM$`Competidor ganador` %in% c("0","N/A") | is.na(PM$`Competidor ganador`)) ])
UComp <- UComp[order(UComp)]

Giro_Vt <- as.data.table(read_excel( paste0(direcc,"insumos/catalogo/Giro_Vertical.xlsx") ))
G_x_C <- as.data.table(read_excel( paste0(direcc,"insumos/catalogo/Giro_x_CUC.xlsx") ))
Vtcal <- as.character(unique(Giro_Vt$Vertical),unique(G_x_C$Vertical))
Vtcal <- Vtcal[ !(Vtcal %in% c("Sin especificar","Agroindustria","Servicios","Medios","Construcción","Industria") )]
Vtcal <- Vtcal[order(Vtcal)]

Proy <- c("A","B","C","MULTINACIONAL")
Etap <- c("Nueva",as.character(unique(etapasopp$Etap_Camp)))

MeDi <- data.table(`Div / Sect 2` = DivSec$`División / Sector`, Mercado = DivSec$Mercado)[order(-Mercado)]


Divs <- as.character(unique(DivSec$`División / Sector`))
Meri <- as.character(unique(MeDi$Mercado))
Fari <- as.character(unique(Fam_Neg$Familia))
Neri <- as.character(unique(Fam_Neg$Negocio))


campos_incluye_T.Proy <- c("Cliente","División / Sector","Nombre de la oportunidad","Descripción de la Oportunidad","Etapa","Paso siguiente","Fecha paso siguiente",
                           "Total de Proyecto","Importe Anual","Producto mayor ponderacion","Productos en OPP","Tipo","Clasificación","Creación","Cierre","ID","Ejecutivo",
                           "Gerencia","Giro","Servicios","Importe","sucursales","suc_ganadas","enlaces","enlaces_ganados","Negocio","Descripción del cierre","Meses")

campos_para_cerradas <- c("Cliente","División / Sector","Nombre de la oportunidad","Razón de pérdida","Conclusión de cierre","Clasificación_EAE","Subclasificación_EAE","Competidor ganador",
                          "Total de Proyecto","Importe Anual","Producto mayor ponderacion","Productos en OPP","Cierre","ID","Etapa","Ejecutivo","Gerencia","Servicios","Importe","Meses")

campos_en_casos <- c("Cliente","División / Sector","Nombre de la oportunidad","Total de Proyecto","Precio Objetivo","Número del caso",
                     "Estado","Asignado_Finanzas","Fecha de apertura","Fecha de Proyecto","Fecha Cierre del Caso","Productos en OPP",
                     "Tipo","Clasificación","Creación","Cierre","ID","Etapa")


campos_Ten_Ganancia <- c("Div / Sect 2","RAZON_SOCIAL_CORP","RAZON_SOCIAL","Gerencia","Nombre del producto")

############# Casos Finanzas
casos_01 <-as.data.table(read_excel(paste0(direcc,"insumos/Casos_01_Sactel.xlsx") ))
casos_02 <-as.data.table(read_excel(paste0(direcc,"insumos/Casos_02_Finanzas.xlsx") ))
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

#### Base de Oportunidades y ajustes

ejec_gcia <- unique(subset(p3_cliente, select = c("Propietario de la cuenta","Gerencia")))
names(ejec_gcia) <- c("Propietario de oportunidad","Gerencia")
Geri <- as.character(unique(ejec_gcia$Gerencia))


Base_Y0 <- unique(subset(opps_total, select = c("Identificador de la oportunidad", "Nombre del producto", "Familia de productos","Precio total","Cantidad")))
Base_X0 <- uni_ks_opps

##### Agrega Oportunidades Manuales------------
manuales <- Base_X0[0,]
y1 <- as.data.table(read_excel(paste0(direcc,"insumos/Carga_Manual_OPP.xlsx") ))
y1$`Fecha de Propuesta`<-ymd(y1$`Fecha de Propuesta`)
y1$`Fecha de Aproximación`<-ymd(y1$`Fecha de Aproximación`)
y1$`Fecha de Proyecto`<-ymd(y1$`Fecha de Proyecto`)
y1$`Fecha de Oportunidad`<-ymd(y1$`Fecha de Oportunidad`)
y1$`Fecha de Contacto`<-ymd(y1$`Fecha de Contacto`)
y1$`Fecha de Temprana`<-ymd(y1$`Fecha de Temprana`)
y1$`Fecha de ejecución del paso siguiente`<-ymd(y1$`Fecha de ejecución del paso siguiente`)
y1$`Fecha de cierre`<-ymd(y1$`Fecha de cierre`)
y1$`Fecha de creación`<-ymd(y1$`Fecha de creación`)
y2 <- merge(y1[!is.na(`Identificador de la oportunidad`)], Base_X0[,.N,by = `Identificador de la oportunidad` ], by = "Identificador de la oportunidad", all.x = TRUE)
cliente <- subset(y2, is.na(N), select = -N)
#cliente$FUENTE<-"SACTEL"
#cliente$`Canal Que Genera`<-""
cliente<-subset(cliente,select=names(manuales))
manuales <- rbind(manuales, cliente, use.names=FALSE) #oportunidades que no están identificadas y se suman 
# Actualiza el nombre del cliente, div y gerencia con los datos que hay en la base
cliente <- as.data.table(read_excel(paste0(direcc,"insumos/Visitas_Paso3.xlsx") ))[CUC %in% as.character(unique(manuales$CUC))]
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

### Complementa la tabla Y con Productos
Base_Y0 <- merge(Base_Y0, Tprod, by.x = "Nombre del producto", by.y = "Producto de mayor ponderacion", all.x = TRUE)

### Crea bandera de Estatus del Paso Siguiente
Base_X0$Estatus_paso_siguiente <- if_else(as.Date(Base_X0$`Fecha de ejecución del paso siguiente`) < Sys.Date(),"Vencido","Vigente")

# Agrega el Mercado y cruza con los casos
Base_X0 <- merge(Base_X0, MeDi, by ="Div / Sect 2")
Base_X0 <- merge(Base_X0, casos_Final, by ="Identificador de la oportunidad", all.x = TRUE)
Base_X0[Caso_Abierto %in% c(0,1)]$Rango_Caso <- ifelse(Base_X0[Base_X0$Caso_Abierto %in% c(0,1)]$Antigüedad_Caso > 20,"4. Más de 20 días",ifelse(Base_X0[Base_X0$Caso_Abierto %in% c(0,1)]$Antigüedad_Caso > 10,"3. De 11 a 20 días",ifelse(Base_X0[Base_X0$Caso_Abierto %in% c(0,1)]$Antigüedad_Caso > 5,"2. De 6 a 10 días","1. Menos de 5 días")))

# marca Etapas y estatus de Postmortem analizado por Ramón Ramírez
Base_X0 <- merge(Base_X0, etapasopp, by ="Etapa", sort = FALSE )
#Base_X0$Etap_Camp <- Base_X0$Etapa
#Base_X0[Etapa == "Prospectación (FP)"]$Etap_Camp <- "Prospección"
#Base_X0[Etapa %in% c("Oportunidad", "Aproximación","Contacto") ]$Etap_Camp <- "Temprana"
Base_X0 <- merge(Base_X0, PM, by.x ="Identificador de la oportunidad", by.y = "ID", all.x = TRUE, sort = FALSE)
# identifica las oportunidades que no tienen "Conclusión de cierre" pero si tienen "Descripción de Cierre"
c <- is.na(Base_X0$`Conclusión de cierre`) & !is.na(Base_X0$`Descripción del cierre`)
Base_X0[c]$`Conclusión de cierre` <- Base_X0[c]$`Descripción del cierre`
Base_X0[Etapa == "PERDIDA" &  !(Clasificación_EAE %in% NoPEx)]$Etap_Camp <- "No exitosa"


Div_Gerencia <- unique(data.table(Gerencia = Base_X0$Gerencia,`Div / Sect 2` = Base_X0$`Div / Sect 2`))[order(-`Div / Sect 2`)]
ctes <- unique(subset(Base_X0[FUENTE=="Empresarial"], Cerrado == 0)$`Nombre de la cuenta`)

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
#Base_X0$`Fecha de creación` <- ymd(Base_X0$`Fecha de creación`)
#Base_X0$`Fecha de ejecución del paso siguiente` <- ymd(Base_X0$`Fecha de ejecución del paso siguiente`)
#Base_X0$`Fecha de cierre` <- ymd(Base_X0$`Fecha de cierre`)

a_campa <- a_campa[,1:2]

######3 Especial para Campaña de Sucursales ----------------

y1 <- "sucursales a blindar"
u1 <- as.character(Base_X0$Descripción)
y2 <- regexpr(y1,u1,ignore.case = TRUE)
Base_X0$sucursales <- as.numeric(gsub(",","",str_extract(substr(u1,y2,nchar(u1)),"\\d+\\,*\\d*")))
Base_X0$sucursales[y2 == -1] <- NA

y1 <- "sucursales"
u1 <- as.character(Base_X0$`Descripción del cierre`)
y2 <- regexpr(y1,u1,ignore.case = TRUE)
Base_X0$suc_ganadas <- as.numeric(gsub(",","",str_extract(substr(u1,y2,nchar(u1)),"\\d+\\,*\\d*")))
Base_X0$suc_ganadas[y2 == -1] <- NA
Base_X0[ Etapa == "GANADA" & is.na(suc_ganadas) & !is.na(sucursales)]$suc_ganadas <- Base_X0[ Etapa == "GANADA" & is.na(suc_ganadas) & !is.na(sucursales)]$sucursales

######3 Especial para Campaña de Fortalecimiento 2020 ----------------

y1 <- "enlaces"
u1 <- as.character(Base_X0$Descripción)
y2 <- regexpr(y1,u1,ignore.case = TRUE)
Base_X0$enlaces <- as.numeric(gsub(",","",str_extract(substr(u1,y2,nchar(u1)),"\\d+\\,*\\d*")))
Base_X0$enlaces[y2 == -1] <- NA

y1 <- "[:alpha:][:alnum:][:digit:][-][:digit:]{4}[-][:digit:]{4}"
Base_X0$enlaces_ganados <- str_count(Base_X0$`Descripción del cierre`,y1)
Base_X0[is.na(Campaña_Prod) | !(Campaña_Prod=="Fortalecimiento Conectividad 2020" & Etap_Camp=="Ganada")]$enlaces_ganados<-NA

######################################-------------------

#Actualiza Giro por el CUC
Base_X0 <- merge(Base_X0,subset(G_x_C,select = c("CUC","Vertical")), by="CUC", all.x = TRUE)
Base_X0[!is.na(Vertical)]$Giro <- Base_X0[!is.na(Vertical)]$Vertical
Base_X0 <- subset(Base_X0, select = -Vertical)

p3_cliente <- merge(p3_cliente,subset(G_x_C,select = c("CUC","Vertical")), by="CUC", all.x = TRUE)
p3_cliente[!is.na(Vertical)]$Giro <- p3_cliente[!is.na(Vertical)]$Vertical
p3_cliente <- subset(p3_cliente, select = -Vertical)

#### Bases de Inventario y Facturación

####### Agrega el campo "Mercado" en la base de Tenencia
#aTen <- merge(aTen, MeDi, by = "Div / Sect 2", all.x = TRUE)
#cTen <- merge(cTen, MeDi, by = "Div / Sect 2", all.x = TRUE)

load(paste0(direcc,"insumos/a_p/cTen_actual.RDATA"))
load(paste0(direcc,"insumos/a_p/base_FacMen_actual.RDATA"))
load(paste0(direcc,"insumos/a_p/actual_operacion.RDATA"))

## Reasinga el mercado a casos casos inconsistentes

cTen_[Mercado=="3. Pyme" & `Div / Sect 2` %in% c("FINANCIERO","SERVICIOS Y TURISMO","PUBLICO Y PARAESTATAL",
  "EMPRESAS GRUPO","GOBIERNO","INDUSTRIA Y SALUD")]$Mercado<-"1. Telecorp"
cTen_[Merca2 %in% c("3. Pyme","4. CECOR") & `Div / Sect 2` %in% c("FINANCIERO","SERVICIOS Y TURISMO",
   "PUBLICO Y PARAESTATAL","EMPRESAS GRUPO","GOBIERNO","INDUSTRIA Y SALUD")]$Merca2<-"1. Telecorp"

## Agrega el nuemero de sucursales y las verticales a la base de inventarios

cSuc <- as.data.table(read_xlsx(paste0(direcc,"insumos/CUC-E_suc.xlsx")))
cTen_ <- merge(cTen_[!is.na(ganMe)],cSuc[,c("CUC_E","suc")],by="CUC_E",all.x = TRUE)
cTen_ <- merge(cTen_,Giro_Vt, by="Giro", all.x = TRUE)
cTen_[is.na(Vertical)]$Vertical<-"OtrasV"
cTen_[is.na(Giro)]$Giro<-"OtrasV"
cTen_$uso <- "I"
#cTen_$`F2019-01-01` <- 0
#cTen_$`F2019-02-01` <- 0
#cTen_$`F2019-03-01` <- 0
#cTen_$`F2019-04-01` <- 0
#cTen_$`F2019-05-01` <- 0
#cTen_$`F2019-06-01`	<- 0
#cTen_$`F2019-07-01`	<- 0
#cTen_$`F2019-08-01`	<- 0
#cTen_$`F2019-09-01`	<- 0
#cTen_$`F2019-10-01`	<- 0
#cTen_$`F2019-11-01`	<- 0
#cTen_$`F2019-12-01`	<- 0
#cTen_$Facturacion <- 0
#cTen_$Meta <- 0

### Pega la facturación con los usos "F" y "P"

#cFact <- as.data.table(read_xlsx(paste0(direcc,"insumos/FACT_PARCHE.xlsx")))

#cTen_ <- rbind(cTen_,cFact)

colu <- names(bFM2)[!(names(bFM2) %in% names(cTen_))]
cTen_ <- cTen_[,(colu):=0]
colu <- names(cTen_)[!(names(cTen_) %in% names(bFM2))]
bFM2 <- bFM2[,(colu):=NA]

cTen_ <- rbind(cTen_,bFM2)


###### archivo de Metas en unidades
Met2019 <- as.data.table(read_excel(paste0(direcc,"insumos/Meta 2019 en unidades.xlsx")))
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


### función que cruza los estatus - etapas e identifica cuales opp son nuevas
f_cruza <- function(x0, fechas, TProd){
  # Crea el campo -Días en Temprana- como D_Temprana
  x0$D_Temprana <- ifelse(is.na(x0$`Días en Oportunidad`),0,x0$`Días en Oportunidad`) + ifelse(is.na(x0$`Días en Aproximación`),0,x0$`Días en Aproximación`) + ifelse(is.na(x0$`Días en Contacto`),0,x0$`Días en Contacto`)
  
  # marca las nuevas
  x0$Nueva <- 0
  x0$vivI <- as.Date(x0$`Fecha de Temprana`)
  x0[x0$vivI >= fechas[1] & x0$vivI <= fechas[2] ]$Nueva <- 1
  x0$vive <- as.Date(x0$`Fecha de cierre`)
  
  # Pone el nombre corto usado en el catálogo -TProd-
  paso0F <- merge(x0, TProd, all.x = TRUE, by = "Producto de mayor ponderacion")
  paso0F$Familia[is.na(paso0F$Familia)] <- "Otros"
  paso0F$Negocio[is.na(paso0F$Negocio)] <- "7. Otros"
  
  # marca la etapa final
  paso00 <- subset(x0, select = c("Identificador de la oportunidad","Etap_Camp","vive","Cerrado","vivI","Fecha de Proyecto","Fecha de Propuesta"))
  paso00$EtapF <- "Arranca"
  paso00[ vivI > fechas[2] ]$EtapF <- " "
  paso00[ Cerrado == 1 & vive < fechas[1] ]$EtapF <- " " # | vive > fechas[2]
  #paso00[ Cerrado == 0 & vivI > fechas[2] ]$EtapF <- " "
  
  paso00[Etap_Camp == "Prospección"]$EtapF <- "Prospección"
  paso00[EtapF == "Arranca" & Cerrado == 1 & vive <= fechas[2]]$EtapF <- paso00[EtapF == "Arranca" & Cerrado == 1 & vive <= fechas[2]]$Etap_Camp
  paso00[EtapF == "Arranca" & `Fecha de Propuesta` <= fechas[2]]$EtapF <- "Propuesta"
  paso00[EtapF == "Arranca" & `Fecha de Proyecto` <= fechas[2]]$EtapF <- "Proyecto"
  paso00[EtapF == "Arranca" & vivI <= fechas[2]]$EtapF <- "Temprana"
  
  paso01 <- subset(paso00, select = c("Identificador de la oportunidad","EtapF"))
  paso0F <- merge(paso0F, paso01, by ="Identificador de la oportunidad" , all.x = TRUE, sort = FALSE )
  
  # Señala las OPP -Inicial-
  paso0F$inicio <- " "
  paso0F$inicio[ paso0F$vivI < fechas[1] & paso0F$EtapF !=" " ] <- "Inicial"
  
  # Señala Opps -Final-
  paso0F$f1nal <- " "
  paso0F$f1nal[ paso0F$vivI <= fechas[2] & paso0F$EtapF %in% c("Temprana","Proyecto","Propuesta") ] <- "Final"
  
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
  if(length(u1)>1){
    for(i in 2:length(u1)){
      campo_uso <- names(a1)
      a1 <- merge(a1, x2[x2[[Horz_columna]] == u1[i] ] %>% group_by(llave) %>% summarise( i = sum(suma_campo, na.rm = TRUE )) , by = "llave", all = TRUE )
      names(a1) <- c(campo_uso,u1[i])
    }
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


#Agrega los Gerentes de Mesa de Control
b <- as.data.table(read_xlsx(paste0(direcc,"insumos/MDControl/Catalogo_Responsales.xlsx")))
b <- unique.data.frame(b[,1:2])
Base_X0 <- merge(Base_X0, b, all.x = TRUE, by.x="División / Sector", by.y="DD_Sec")


#Temporal para agregar campos de OPP´s
b <- as.data.table(read_xlsx(paste0(direcc,"insumos/OPPS_complemento.xlsx"), col_types = c("text",rep("date",5),rep("text",4),"numeric") ))

Base_X0 <- merge(Base_X0, b, all.x = TRUE, by = "Identificador de la oportunidad")
Base_X0$`Clasificación del proyecto`[!is.na(Base_X0$`Núm. De PGLA`)] <- "MULTINACIONAL"
Base_X0$`Clasificación del proyecto`[!is.na(Base_X0$Países)] <- "MULTINACIONAL"

Base_X0$`Fecha de publicación convocatoria` <- as.Date(Base_X0$`Fecha de publicación convocatoria`)
Base_X0$`Fecha de adquisición de bases` <- as.Date(Base_X0$`Fecha de adquisición de bases`)
Base_X0$`Fecha de juntas de aclaraciones` <- as.Date(Base_X0$`Fecha de juntas de aclaraciones`)
Base_X0$`Fecha de presentación/apertura` <- as.Date(Base_X0$`Fecha de presentación/apertura`)
Base_X0$`Fecha de fallo` <- as.Date(Base_X0$`Fecha de fallo`)


# Elimina variables que no van al RData
a<-direcc

rm(a_p_carga,b,b1_1,b2_1,b2_2,b2_3,b2_4,b2_5,c,campos_tot,campos_uni,casos_01,casos_02,casos_03,client3,cliente,
  CRoad,cSuc,direc2,direcc,direcc_opp,f_Asignado,freducir,i,manuales,Met2019,opps_total,p_campa,p1_visitas,p2_visitas,
  p4_usr2,tipos,u,u1,uni_ks_opps,w,x,y,y1,y2,y3,y4,y5)

# Crea archivos RDATA
save.image(paste0(a,"/insumos/a_p/OPP_",Sys.Date(),".RDATA"))
#rm(archivoss,i)
save.image(paste0(a,"/insumos/a_p/actual_uso_inicio.RDATA"))


#### Para Mesa de Control, reinicia el kanban los días viernes antes de las 8
if(hour(Sys.time()) < 8 & strftime(Sys.time(),"%A") == "jueves"){
  MDCom <- fread(paste0(a,"insumos/MDControl/comentaMDC.csv"), sep = ",", colClasses = c(rep("character",15)))
  MDCom$kanban <- "FALSE"
  write.csv(MDCom, paste0(a,"insumos/MDControl/comentaMDC.csv"), row.names = FALSE)
}