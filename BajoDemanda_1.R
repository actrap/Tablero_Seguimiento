library(shinydashboard)
library(dtplyr)
library(dplyr)
library(data.table)
library(DT)
library(readxl)
library(tidyr)


###############
f_filtra <- function(Tabla, MDO, servicio, BjDe, TipoM){
  
  if(servicio == ""){ servicio <- c("IDE","RPV") }
  if(TipoM == "Total" ){ TipoM <- unique(Tabla$TPR_N_AGRUP)}
  
  y1 <- subset(Tabla, Mercado %in% MDO & CLASIF == "MDO EMP" & `Inicio Referencia` %in% BjDe & `Tipo de Producto` %in% servicio & TPR_N_AGRUP %in% TipoM)
  y1$llavM <- paste(y1$Mercado,y1$`Div Atención`)
  y1$llavD <- paste(y1$RANGO_DIL_F_LIBvsCORTE)
  
  return(y1)
  
}

################ función para hacer las tablas
f2_tabla <- function( Tabla, agrupador ){
  
  y1 <- Tabla  
  
  if(agrupador == "Mercado") {
    y1$llave <- y1$llavM
    a0  <- unique(subset(y1, select = c("llave","Mercado","Div Atención")))[order(-Mercado)][!is.na(llave)]
    ban <- 4
  }
  else
  {
    y1$llave <- y1$llavD
    a0  <- unique(subset(y1, select = c("llave",agrupador)))[order(llave)][!is.na(llave)]
    ban <- 3
  }
  
  A1 <- y1[, .(Cantidad = .N), by = c("llave","SUB_DIVIS 2")]
  A2 <- spread(A1, `SUB_DIVIS 2`, Cantidad, fill = 0 )
  A3 <- A1[, .(Total = sum(Cantidad)), keyby = llave]
  b <- a0[A2[A3[a0[,1], on="llave"], on = "llave"], on = "llave"]
  
  #a1  <- y1[.("METRO"), .(METRO =.N) , keyby = llave, on="SUB_DIVIS 2"]
  #a2  <- y1[.("NORTE"), .(NORTE =.N) , keyby = llave, on="SUB_DIVIS 2"]
  #a3  <- y1[.("OCCIDENTE"), .(OCCIDENTE =.N) , keyby = llave, on="SUB_DIVIS 2"]
  #a4  <- y1[.("SUR"), .(SUR =.N) , keyby = llave, on="SUB_DIVIS 2"]
  #a5  <- y1[.("TELNOR"), .(Telnor =.N) , keyby = llave, on="SUB_DIVIS 2"]
  #a6  <- y1[, .(Total =.N) , keyby = llave]
  
  #b <- a0[a1[a2[a3[a4[a5[a6[a0[,1], on="llave"], on="llave"], on="llave"], on="llave"], on="llave"], on="llave"], on="llave"]
  
  #b <- subset(af, select = -llave)
  
  ########### hace el TOTAL
  s <- ncol(b)
  c5 <- data.frame( t(apply(b[,ban:s],2,function(x)sum(x,na.rm = TRUE))) )
  c6 <- data.frame(llave = "TOTAL", agrupador = "TOTAL")
  
  #######  
    if(agrupador == "Mercado"){
      c1 <- unique(b[[agrupador]])
      k <- b[0,]
        for(i in 1:length(c1)){
          c2 <- b[b[[agrupador]] == c1[i]]
          c3 <- data.table(llave = c1[i], agrupador = c1[i], Div = " ",t(apply( c2[,ban:s],2, function(x)sum(x, na.rm = TRUE))) )
          c2[,2] <- " "
          names(c3) <- names(c2)
          c4 <- rbind(c3,c2)
          k <- rbind(k,c4)
          
        }
      c6 <- data.frame(llave = "TOTAL", agrupador = "TOTAL", Div = " ")
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

BajoDemanda <- function(input, output, session, direcc){
  
  ns <- session$ns
  
  IDE <- as.data.table(read_excel(paste0(direcc,"insumos/IDE_BajoDemanda.xlsx"), col_types = "text"))
  MeSu <- c("TELECORP","DIVISIONES")
  B1De <- c("C0","C1","C2","C3","C4","C5")
  B2De <- as.character(unique(IDE$`Inicio Referencia`))
  B2De <- B2De[order(B2De)]
  TipM <- as.character(unique(IDE$TPR_N_AGRUP))
  TipM <- TipM[order(TipM)]
  campos_BD_mostrar <- c("EMPRESA_PTA","Div Atención","TPR_N_AGRUP","POB_B","Tipo de Producto","VELOCIDAD","MES_ENTREGA","RAZON_LEA_PPU_PPC")
  campos_BD_cambia <- c("Empresa","Div Atención","Movimiento","Población","Producto","Velocidad","Mes Entrega","Razón")
  T200p <- c(0,7)
  
  output$kps <- renderUI({
    tagList(
      h2(paste("Reporte de Servicios","Pendientes de entrega")),
      fluidRow(
        column(width = 3, materialSwitch(ns("I_BD_01"), label = "Filtrar Candidatos a Bajo Demanda", value = FALSE,  status = "primary")  ), 
        column(width = 3, selectInput(ns("Ser_01"), "Tipo de Servicio", choices = c("","IDE","RPV"))),
        column(width = 3, selectInput(ns("TiM_01"), "Tipo de Movimiento", choices = c("Total",TipM), selected = "Altas"))
      ),
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
  
  BjDF <- reactive( ifelse(input$I_BD_01,return(B1De),return(B2De)) )
  
  observeEvent(input$I_BD_01,{ if(input$I_BD_01) updateSelectInput(session, "Ser_01", selected = "IDE") })
  
  BA3F <- reactive( f_filtra( IDE, MeSu, input$Ser_01, BjDF(), input$TiM_01) ) 
  
  Tabla_01 <- reactive( f2_tabla( BA3F() , "Mercado") )
  
  output$sale_01 <- renderDataTable( datatable( subset(Tabla_01(), select = -llave), rownames = FALSE, selection = 'none',
                                               options = list(pageLength = 16, dom = 't', ordering = FALSE )) %>% 
                                       formatCurrency(3:8, currency="", digits=0) %>% formatStyle(2, textAlign = "right") %>% formatStyle(1, textAlign = "left") %>%
                                       formatStyle("Mercado", target = "row",
                                                   background = styleEqual(c(MeSu,'TOTAL'),c(as.character(rep('gray',length(MeSu))),'steelblue')), 
                                                   color = styleEqual(c(MeSu,'TOTAL'),c(as.character(rep('white',length(MeSu))),'white')) )
  )
  
  ###### Para seleccionar dentro de la tabla por DD
  observeEvent(input$sale_01_cell_clicked, {
    info = input$sale_01_cell_clicked
    a1 <- names(Tabla_01())[info$col+2]
    b1 <- as.character(Tabla_01()$llave[info$row])
    k <- BA3F()
    
    if( is.null(info$value) || info$col %in% c(0:1)  ) return()
    
    if(b1 != "TOTAL"){
      k <- subset( k, llavM == b1 | llavD == b1 )
      #b1 <- paste(b1,a1)
      b1 <- paste("de", as.character(Tabla_01()[info$row,3]),"en",a1 )
    }
    
    if( a1 != "Total"){ SERV_Pselec <- k[`SUB_DIVIS 2` == a1] }
    else{ SERV_Pselec <- k }
    
    #campos_mostrar <- c("EMPRESA_PTA","Div Atención","TPR_N_AGRUP","POB_B","Tipo de Producto","VELOCIDAD","MES_ENTREGA","RAZON_LEA_PPU_PPC","OBSERVS_LEA_PPU_PPC")
    
    #fecha_campos <- c("Fecha paso siguiente","Creación","Cierre") 
    #fecha_rep <- c(rep('toLocaleDateString',3))
    
    
    showModal(modalDialog(
      title = paste("Detalle de",info$value,"Servicios",b1),
      h6(dataTableOutput(ns("OPPs_Tabla"))),
      footer = modalButton("Salir"),
      size = "l",
      easyClose = TRUE
    ))
    
    
    output$OPPs_Tabla <- renderDataTable( datatable( subset(SERV_Pselec, select = campos_BD_mostrar), colnames = campos_BD_cambia , rownames = FALSE, extensions = c('FixedColumns','KeyTable','Buttons'),
                                                     options = list(pageLength = 5, dom = 'Bplti', ordering = TRUE, autoWidth = TRUE, buttons = c('excel','copy'),
                                                                    keys = TRUE, scrollX = TRUE, scrollY = TRUE, fixedColumns = list(leftColumns = 2),
                                                                    columnDefs = list(list(width = '200px', targets = T200p )) )) #%>%
                                          #formatCurrency(c("Total de Proyecto","Importe Anual"), digits = 0) %>% formatDate(fecha_campos,fecha_rep)
    )
    
    
  })
  ######## Termina para seleccionar en la tabla por DD
  
  
  Tabla_02 <- reactive( f2_tabla( BA3F(), "RANGO_DIL_F_LIBvsCORTE") )
  
  output$sale_02 <- renderDataTable( datatable(subset(Tabla_02(), select = -llave), rownames = FALSE, selection = 'none',
                                               options = list(pageLength = 16, dom = 't', ordering = FALSE )) %>% 
                                       formatCurrency(2:8, currency="", digits=0) %>% formatStyle(2, textAlign = "right") %>% formatStyle(1, textAlign = "left") %>%
                                       formatStyle("RANGO_DIL_F_LIBvsCORTE", target = "row",
                                                   background = styleEqual(c(MeSu,'TOTAL'),c(as.character(rep('gray',length(MeSu))),'steelblue')), 
                                                   color = styleEqual(c(MeSu,'TOTAL'),c(as.character(rep('white',length(MeSu))),'white')) )
  )
  
  
  ###### Para seleccionar dentro de la tabla Dilación
  observeEvent(input$sale_02_cell_clicked, {
    info = input$sale_02_cell_clicked
    a1 <- names(Tabla_02())[info$col+2]
    b1 <- as.character(Tabla_02()$llave[info$row])
    k <- BA3F()
    
    if( is.null(info$value) || info$col %in% c(0)  ) return()
    
    if(b1 != "TOTAL"){
      k <- subset( k, llavM == b1 | llavD == b1 )
      #b1 <- paste(b1,a1)
      b1 <- paste("de", as.character(Tabla_02()[info$row,2]),"en",a1 )
    }
    
    if( a1 != "Total"){ SERV_Pselec <- k[`SUB_DIVIS 2` == a1] }
    else{ SERV_Pselec <- k }
    
    
    
    #fecha_campos <- c("Fecha paso siguiente","Creación","Cierre") 
    #fecha_rep <- c(rep('toLocaleDateString',3))
    
    
    showModal(modalDialog(
      title = paste("Detalle de",info$value,"Servicios",b1),
      h6(dataTableOutput(ns("OPPs_Tabla"))),
      footer = modalButton("Salir"),
      size = "l",
      easyClose = TRUE
    ))
    
    
    output$OPPs_Tabla <- renderDataTable( datatable( subset(SERV_Pselec, select = campos_BD_mostrar), colnames = campos_BD_cambia , rownames = FALSE, extensions = c('FixedColumns','KeyTable','Buttons'),
                                                     options = list(pageLength = 5, dom = 'Bplti', ordering = TRUE, autoWidth = TRUE, buttons = c('excel','copy'),
                                                                    keys = TRUE, scrollX = TRUE, scrollY = TRUE, fixedColumns = list(leftColumns = 2),
                                                                    columnDefs = list(list(width = '200px', targets = T200p )) )) #%>%
                                          #formatCurrency(c("Total de Proyecto","Importe Anual"), digits = 0) %>% formatDate(fecha_campos,fecha_rep)
    )
    
    
  })
  ######## Termina para seleccionar en la tabla por Dilación
  
  output$sale_03 <- renderDataTable( datatable(BA3F()[,.(Servicios = .N), by=EMPRESA_PTA][order(-Servicios)][1:15,], rownames = FALSE, selection = 'none',
                                               options = list(pageLength = 16, dom = 't', ordering = FALSE ))
                                       
  )
  
  
  
  
}