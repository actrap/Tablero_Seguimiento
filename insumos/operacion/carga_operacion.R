library(readxl)
library(dplyr)
library(data.table)
library(dtplyr)
library(tidyr)
library(htmltools)
library(stringr)
library(tidyr)
library(lubridate)

a <- "C:/Users/larcos/M_Empresarial/Analisis/Ventas/Aplicaciones/Comparativo_SACTEL/"
#a <- "C:/Users/vmorenoa/Documents/R proyectos/pruebas/"
#b <- "/srv/shiny-server/tablero_seguimiento/"
b <- "/srv/shiny-server/pruebas/"

direcc <- if_else(substr(getwd(),1,1) == "C", a , b)

# Carga los archivos de Operación
setwd(paste0(direcc,"insumos/operacion/"))
arch_oper <- dir()
m <- regexpr("\\d\\d\\d\\d\\.",arch_oper)
b0 <- regmatches(arch_oper,m-1)
b1 <- substr(unique(b0),2,5)
m <- fread("Periodos_cargados.txt", colClasses = "character")
c <- b1[!(b1 %in% m$Periodo)]

load(paste0(direcc,"insumos/a_p/actual_operacion.RDATA"))

if(length(c)>0){
  catalogo_operacion <- as.data.table(read_xlsx("Catalogo_Operacion.xlsx"))
  cat_ope_2 <- unique(subset(catalogo_operacion, select = c("TORRE+SERVICE+TELMEX_PRODUCT","PODUCTO DIRECCIÓN")))
  
  for(i in 1:length(c)){
    #incidentes <- as.data.table(read_xlsx(paste0(direcc,"insumos/operacion/INCIDENTES 2001.xlsx")))
    #company <- as.data.table(read_xlsx(paste0(direcc,"insumos/operacion/COMPANY 2001.xlsx")))
    circuitos <- fread(paste0("CIRCUITOS ",c[i],".csv"), sep = ",")
    names(circuitos) <- c("TORRE1",names(circuitos)[-1])
    circuitos$`TORRE+SERVICE+TELMEX_PRODUCT` <- paste0(circuitos$TORRE1,"_",circuitos$SERVICE,"_",circuitos$TELMEX_PRODUCT)
    circuitos <- merge(circuitos, cat_ope_2, by = "TORRE+SERVICE+TELMEX_PRODUCT")
    
    equipamiento <- subset(fread(paste0("EQUIPAMIENTO ",c[i],".csv"), sep = ","),select = -COMPANY)
    names(equipamiento) <- c("TORRE1",names(equipamiento)[-1])
    equipamiento$`TORRE+SERVICE+TELMEX_PRODUCT` <- paste0(equipamiento$TORRE1,"_",equipamiento$SERVICE,"_",equipamiento$TELMEX_PRODUCT)
    equipamiento <- merge(equipamiento, cat_ope_2, by = "TORRE+SERVICE+TELMEX_PRODUCT", all.x = TRUE)
    equipamiento$TOTAL_EXTENSIONS <- as.numeric(gsub(",","",equipamiento$TOTAL_EXTENSIONS))
    equipamiento$TOTAL_LICENSES <- as.numeric(gsub(",","",equipamiento$TOTAL_LICENSES))
    equipamiento$EQ_TOTAL <- fifelse(is.na(equipamiento$TOTAL_EXTENSIONS) | equipamiento$TOTAL_EXTENSIONS == 0,fifelse(is.na(equipamiento$TOTAL_LICENSES) | equipamiento$TOTAL_LICENSES == 0,1,equipamiento$TOTAL_LICENSES), equipamiento$TOTAL_EXTENSIONS)
    
    equipamiento$EQ_TIPO <- fifelse(equipamiento$`PODUCTO DIRECCIÓN` =="AUDIOCONFERENCIA TELMEX","SALAS (Participantes)",
                                   fifelse(equipamiento$SUBTYPE =="SOFTPHONE","TELEFONO",
                                           fifelse(equipamiento$SUBTYPE =="TELEPHONE ADAPTER","ACCESORIOS", 
                                                   fifelse(substr(equipamiento$SUBTYPE,1,9)=="TELEPHONE","TELEFONO",
                                                           fifelse(substring(equipamiento$SUBTYPE, first = nchar(equipamiento$SUBTYPE)-8) =="TELEPHONE","TELEFONO",
                                                                   fifelse(equipamiento$TELMEX_PRODUCT=="SENALIZACION DIGITAL","DISPOSITIVOS",
                                                                           fifelse(equipamiento$TYPE %in% c("OFFICERESOURCES","CENTERCOMPUTER"),"EQUIPO DE COMPUTO",
                                                                                   fifelse(equipamiento$TYPE %in% c("INFRASTRUCTURE","PERSONNEL")|(equipamiento$TYPE=="STOCK" & equipamiento$SUBTYPE=="EQUIPMENT"),"OTROS",
                                                                                           fifelse(equipamiento$SUBTYPE %in% c("ANTI-VIRUS / SECURITY","UTILITY SOFTWARE","OPERATING SYSTEM") | equipamiento$TYPE=="SOFTWARELIC","LICENCIAS",
                                                                                                   fifelse(equipamiento$TYPE %in% c("PERIPHERAL","HAND HELD DEVICES","STOCK") | equipamiento$SUBTYPE %in% c("MICROPHONE", "SCREEN", "REMOTE CONTROL","SPEAKERS","CABLE","DISPLAY CART","TOUCH DEVICE"),"ACCESORIOS","DISPOSITIVOS")
                                                                                           )
                                                                                   )
                                                                           )
                                                                   )
                                                           )
                                                   )
                                           )
                                   )
    )
    
    
    sitios <- fread(paste0("SITIOS ",c[i],".csv"), sep = ",")
    sitios$`TORRE+SERVICE+TELMEX_PRODUCT` <- paste0(sitios$TORRE,"_",sitios$SERVICE,"_",sitios$TELMEX_PRODUCT)
    sitios <- merge(sitios, cat_ope_2, by = "TORRE+SERVICE+TELMEX_PRODUCT")
    sitios$tipo <- "sitios" 
    
    c1 <- circuitos[,.N, by= .(CUC,`PODUCTO DIRECCIÓN`, SERVICE_TYPE)]
    names(c1) <- c("CUC","Producto","Tipo","Cantidad")
    s1 <- sitios[,.(valor = length(unique(LOCATION_CODE))), by = .(CUC, `PODUCTO DIRECCIÓN`,tipo) ]
    names(s1) <- c("CUC","Producto","Tipo","Cantidad")
    b1 <- equipamiento[,.(valor = sum(EQ_TOTAL)), by =.(CUC, `PODUCTO DIRECCIÓN`, EQ_TIPO)]
    names(b1) <- c("CUC","Producto","Tipo","Cantidad")
    
    b2 <- rbind(c1,s1,b1)
    b2$periodo <- as.Date(paste(c[i],"01")," %y%m%d")
    b2$Categoria <- ""
    b2[Tipo %in% c("DISPOSITIVOS") & !(Producto %in% c("CLOUDPBX","COMUNICACIONES UNIFICADAS AVANZADAS"))]$Categoria <- "Dispositivos"
    b2[Tipo %in% c("TELEFONO") & Producto %in% c("CLOUDPBX","COMUNICACIONES UNIFICADAS AVANZADAS")]$Categoria <- "Teléfonos"
    b2[Tipo %in% c("I-DATA","IDE","TKIP","RPS","RPV","IDN BAJO DEMANDA","IDE BAJO DEMANDA","ADSL","VDSL") ]$Categoria <- "Conectividad"
    b2[Tipo %in% c("sitios") ]$Categoria <- "Sitios"
      
    fwrite(data.frame(c[i]),"Periodos_cargados.txt",append = TRUE)
    inv_opera <- rbind(inv_opera,b2)
  }
  inv2 <- spread(inv_opera,periodo,Cantidad)
  names(inv2) <- c(names(inv2)[1:4],paste0("O",names(inv2)[-(1:4)]))
}

setwd(paste0(direcc,"insumos/a_p/"))
rm(b0,c1,s1,b1,b2,catalogo_operacion,cat_ope_2,m,equipamiento,sitios,circuitos,arch_oper,a,b,c,direcc,i)
save.image("actual_operacion.RDATA")
