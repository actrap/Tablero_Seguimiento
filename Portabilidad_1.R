library(shinydashboard)
library(dtplyr)
library(dplyr)
library(data.table)
library(DT)
library(readxl)


#a <- "C:/Users/larcos/M_Empresarial/Analisis/Ventas/Aplicaciones/Comparativo_SACTEL/"
#b <- "/srv/shiny-server/tablero_seguimiento/"

#direcc <- if_else(substr(getwd(),1,1) == "C", a , b)


porta <- as.data.table(read_excel(paste0(direcc,"insumos/portabilidad.xlsx") ))

porta$semana <- strftime(porta$`Fecha de alerta`+86400, format = "%W-%y")
porta$mes <- strftime(porta$`Fecha de alerta`+86400, format = "%m-%y")
porta$año <- strftime(porta$`Fecha de alerta`+86400, format = "%Y")

hoy <- c(strftime(Sys.Date(), format = "%W-%y"),strftime(Sys.Date(), format = "%m-%y"),strftime(Sys.Date(), format = "%Y"))

Meri <- c("TELECORP","DIVISIONES")

###############
f_filtra <- function(Tabla, MDO){
  
  y1 <- subset(Tabla, Mercado %in% MDO & CLASIF == "MDO EMP" & `Inicio Referencia` %in% c("C0","C1","C2","C3","C4","C5"))
  
  return(y1)
  
}



################3 función para hacer las tablas
f2_tabla <- function( Tabla, agrupador ){
  
  y1 <- Tabla  
  
  if(agrupador == "Mercado") {
    y1$llave <- paste(y1$Mercado,y1$`Div Atención`)
    a0  <- unique(subset(y1, select = c("llave","Mercado","Div Atención")))[order(-Mercado)]
    ban <- 3
  }
  else
  {
    y1$llave <- paste(y1$RANGO_DIL_F_LIBvsCORTE)
    a0  <- unique(subset(y1, select = c("llave",agrupador)))[order(llave)]
    ban <- 2
  }
  
  
  a1  <- y1[.("METRO"), .(Metro =.N) , keyby = llave, on="SUB_DIVIS 2"]
  a2  <- y1[.("NORTE"), .(Norte =.N) , keyby = llave, on="SUB_DIVIS 2"]
  a3  <- y1[.("OCCIDENTE"), .(Occidente =.N) , keyby = llave, on="SUB_DIVIS 2"]
  a4  <- y1[.("SUR"), .(Sur =.N) , keyby = llave, on="SUB_DIVIS 2"]
  a5  <- y1[.("TELNOR"), .(Telnor =.N) , keyby = llave, on="SUB_DIVIS 2"]
  a6  <- y1[, .(Total =.N) , keyby = llave]
  
  af <- a0[a1[a2[a3[a4[a5[a6[a0[,1], on="llave"], on="llave"], on="llave"], on="llave"], on="llave"], on="llave"], on="llave"]
  
  b <- subset(af, select = -llave)
  
  ########### hace el TOTAL
  s <- ncol(b)
  c5 <- data.frame( t(apply(b[,ban:s],2,function(x)sum(x,na.rm = TRUE))) )
  c6 <- data.frame(agrupador = "TOTAL")
  
  #######  
  if(agrupador == "Mercado"){
    c1 <- unique(b[[agrupador]])
    k <- b[0,]
    for(i in 1:length(c1)){
      c2 <- b[b[[agrupador]] == c1[i]]
      c3 <- data.table(agrupador = c1[i], Div = " ",t(apply( c2[,3:s],2, function(x)sum(x, na.rm = TRUE))) )
      c2[,1] <- " "
      names(c3) <- names(c2)
      c4 <- rbind(c3,c2)
      k <- rbind(k,c4)
      
    }
    c6 <- data.frame(agrupador = "TOTAL", Div = " ")
    b <- k
  }
  
  c4 <- cbind(c6,c5)
  names(c4) <- names(b)
  b <- rbind(b,c4) #Pega el total
  
  return(b)
}
#######


BajoDemandaUI <- function(id){
  ns <- NS(id)
  
  uiOutput(ns("kps"))
  
}

BajoDemanda <- function(input, output, session){
  
  
  output$kps <- renderUI({
    ns <- session$ns
    tagList(
      h2("Reporte de Servicios IDE Pendientes de entrega"),
      fluidRow(
        column( width = 8,
                box( width = 12, title = "Servicios por División de Atención / Div de Instalación", div(style = 'overflow-x: scroll',h6(dataTableOutput(ns("sale_01"))))),
                box( width = 12, title = "Servicios por tiempo de Dilación", div(style = 'overflow-x: scroll',h6(dataTableOutput(ns("sale_02")))))
        ),
        column( width = 4,
                box( width = 12, title = "Clientes con más servicios pendientes", div(style = 'overflow-x: scroll',h6(dataTableOutput(ns("sale_03")))))  
        )
      )
    )
  })
  
  Tabla_01 <- reactive( f2_tabla( f_filtra(IDE, Meri), "Mercado") )
  
  output$sale_01 <- renderDataTable( datatable(Tabla_01(), rownames = FALSE, selection = 'none',
                                               options = list(pageLength = 16, dom = 't', ordering = FALSE )) %>% 
                                       formatCurrency(3:8, currency="", digits=0) %>% formatStyle(2, textAlign = "right") %>% formatStyle(1, textAlign = "left") %>%
                                       formatStyle("Mercado", target = "row",
                                                   background = styleEqual(c(Meri,'TOTAL'),c(as.character(rep('gray',length(Meri))),'steelblue')), 
                                                   color = styleEqual(c(Meri,'TOTAL'),c(as.character(rep('white',length(Meri))),'white')) )
  )
  
  Tabla_02 <- reactive( f2_tabla( f_filtra(IDE, Meri), "RANGO_DIL_F_LIBvsCORTE") )
  
  output$sale_02 <- renderDataTable( datatable(Tabla_02(), rownames = FALSE, selection = 'none',
                                               options = list(pageLength = 16, dom = 't', ordering = FALSE )) %>% 
                                       formatCurrency(2:8, currency="", digits=0) %>% formatStyle(2, textAlign = "right") %>% formatStyle(1, textAlign = "left") %>%
                                       formatStyle("RANGO_DIL_F_LIBvsCORTE", target = "row",
                                                   background = styleEqual(c(Meri,'TOTAL'),c(as.character(rep('gray',length(Meri))),'steelblue')), 
                                                   color = styleEqual(c(Meri,'TOTAL'),c(as.character(rep('white',length(Meri))),'white')) )
  )
  
  output$sale_03 <- renderDataTable( datatable(f_filtra(IDE, Meri)[,.(Servicios = .N), by=EMPRESA_PTA][order(-Servicios)][1:15,], rownames = FALSE, selection = 'none',
                                               options = list(pageLength = 16, dom = 't', ordering = FALSE ))
                                     
  )
  
  
}
