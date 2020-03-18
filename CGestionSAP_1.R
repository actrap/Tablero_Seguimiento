library(dtplyr)
library(dplyr)
library(data.table)
library(shinyWidgets)


# Para hacer el encabezado de las tablas por DD y Producto
f_encabez_SAP <- function(){
  
  encabezado <- htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(rowspan = 2, colspan = 2, ' ' ),
        th(colspan = 2, HTML('<center>Ganados</center>')),
        th(rowspan = 2, colspan = 1, ' '),
        th(colspan = 5, HTML('<center>En Proceso</center>'))
      ),
      tr(
        th("Clientes"),
        th("Opps"),
        th("Clientes"),
        th("Total Opps"),
        th("Pendiente Visitar"),
        th("Temprana"),
        th("Avanzada")
      )
    )
  ))
  return(encabezado)
}

#####

# Para generar la tabla a mostrar
f_SAP_Tabla <- function(x2, xC1, Tabla_ag, x_comp){
  
  GG <- as.character(unique(xC1$Gerencia))
  
  campo_by <- names(Tabla_ag)[1]
  agrupador <- names(Tabla_ag)[2]
  
  Tabla_ag$l3ave <- paste(Tabla_ag[[campo_by]],Tabla_ag[[agrupador]])
  x2$l3ave <- paste(x2[[campo_by]],x2[[agrupador]])
  x_comp$l3ave <- paste(x_comp[[campo_by]],x_comp[[agrupador]])
  
  if(campo_by == "Gerencia"){
    Tabla_ag <- merge(Tabla_ag,unique(subset(x_comp, select = c("l3ave","Gerente"))), by = "l3ave", all.x = TRUE)
    Tabla_ag$Gerente <- paste("  ->",Tabla_ag$Gerente)
  }
  else{Tabla_ag$Gerente <- " "}
  
  x2$suma_campo <- 1

  p1_G <- x2[Etapa == "GANADA",.(Opp_G = .N), by = .(l3ave, `Nombre de la cuenta`)]
  p2_F <- x2[!Etapa %in% c("GANADA","PERDIDA"),.(Opp_F = .N), by = .(l3ave, `Nombre de la cuenta`)]
  p2_F <- merge(p2_F, x2[Etap_Camp=="Prospección", .(Prospec_F = .N), by = `Nombre de la cuenta`], by = "Nombre de la cuenta", all.x = TRUE)
  p2_F <- merge(p2_F, x2[Etap_Camp=="Temprana", .(Temprana_F = .N), by = `Nombre de la cuenta`], by = "Nombre de la cuenta", all.x = TRUE)
  p2_F <- merge(p2_F, x2[Etap_Camp %in% c("Proyecto","Propuesta"), .(Avanzada_F = .N), by = `Nombre de la cuenta`], by = "Nombre de la cuenta", all.x = TRUE)

  t1 <- p1_G[,.(Cte_G = .N, Opp_G = sum(Opp_G)), by = l3ave]
  t1$blanc2 <- 0
  
  t2 <- p2_F[,.(Cte_F = .N, Opp_F = sum(Opp_F), Opp_P = sum(Prospec_F, na.rm = TRUE), Opp_T = sum(Temprana_F, na.rm = TRUE), Opp_A = sum(Avanzada_F, na.rm = TRUE) ), by = l3ave]
  
  tF <- merge(t1,t2, all = TRUE, by = "l3ave")
  
  b <- merge(Tabla_ag, tF, by = "l3ave")
  b <- b[order(b[[agrupador]])]
  b <- data.table(agrupador = b[[agrupador]], subset(b, select = as.character(names(b)[grep(agrupador,names(b), invert = TRUE)][1:(ncol(b)-1)])) )
  
  if( nrow(b)>1){
    c1 <- unique(b$agrupador)
    s <- ncol(b)
    k <- b[0,]
    for(i in 1:length(c1)){
      c2 <- b[b$agrupador == c1[i]]
      c3 <- data.frame(agrupador = c1[i], l3ave = c1[i], Div = " ", Gte = " ", t(apply( c2[,5:s],2, function(x)sum(x, na.rm = TRUE))) )
      c2$agrupador <- c2$Gerente
      names(c3) <- names(c2)
      c4 <- rbind(c3,c2)
      k <- rbind(k,c4)
    }
    if(length(c1)>1){
      c5 <- data.frame(agrupador = "TOTAL", llave = "TOTAL", Div = " ", Gte = " ", t(apply(b[,5:s],2,function(x)sum(x,na.rm = TRUE))) )
      names(c5) <- names(b)
      k <- rbind(k,c5)
    }
    b <- k
  }
  
  b <- subset(b,select = -Gerente)
  b$blanc2 <- NA

  return(b)
}
#######

# Función UI del módulo
fGestionSAPUI <- function(id){ns <- NS(id)
  uiOutput(ns("kbp")) 
}

# Función "Server" del módulo
fGestionSAP <- function(input, output, session, direcc, OPPS_SAP, OPTotal, Div_var, Mer_ar, Vt_01){ 
  
  compromiso <- as.data.table(read_excel(paste0(direcc,"insumos/Compromiso_vta_2019.xlsx"), sheet = "Tabla"))[!is.na(Gerencia)]
  compromiso <- merge(compromiso, Div_Gerencia, by = "Gerencia", all.x = TRUE)
  compromiso <- merge(compromiso, MeDi, by = "Div / Sect 2", all.x = TRUE)
  
  Vertical<-Giro_Vt
  Vertical$bco1<-"TOTAL"
  Vertical<-Vertical[,c(2,3,1)]
  
  Giro_Vt_Agr<-reactive({x<-unique(Vertical[,c(1,2)])
    ifelse(!is.null(Vt_01()),x<-Giro_Vt,x<-unique(Vertical[,c(1,2)]))
    return(x)
  })
  
  ns <- session$ns
  
## Función que trae el UI  
  output$kbp <- renderUI( 
    tagList(
      fluidRow(
        column(width = 9, h2('Gestión de Aplicaciones SAP') ),
        #column(width = 4, h2(htmlOutput(ns("texft_salida"))) ),
        column(width = 3, prettyRadioButtons(ns("vista"),"",c("Canal"="mer","Vertical"="ver"), inline = TRUE),icon = icon("check"), status = "info")
        ), 
      div(style = 'overflow-x: scroll',h6(dataTableOutput(ns("salida_fort"))))
    )
  )
  
  output$texft_salida <- renderText( paste("<center>Día",Sys.Date()-as.Date("2019-10-07")))

## Definición de Tabla a mostrar
  
  OPPS_SAP1<-reactive(merge(OPPS_SAP(),Vertical,by="Giro",all.x = TRUE))
  
  Tab_Agr<-reactive({x<-Div_var()
    if(!is.null(input$vista)){
      if(input$vista=="ver"){x<-Giro_Vt_Agr()}
      if(input$vista=="mer"){x<-Div_var()}
    }
    return(x)
  })
  
  Tabla_SAP <- reactive(f_SAP_Tabla(OPPS_SAP1(), OPTotal(), Tab_Agr(), compromiso ))
  
  output$salida_fort <- renderDataTable(datatable(subset(Tabla_SAP(),select = -l3ave), 
    rownames = FALSE, 
    extensions = c('Buttons'), selection = 'none', container = f_encabez_SAP(), 
    options = list(pageLength = nrow(Tabla_SAP()), dom = 'Bt', ordering = FALSE, buttons = c('excel','copy') ) ) 
    %>% formatStyle(c(3:4,6:10), cursor = 'pointer') 
    %>% formatStyle(2, textAlign = "right") 
    %>% formatStyle(1, textAlign = "left") 
    %>% formatCurrency(3:10, currency = "", digits=0) 
    %>% formatStyle(1, target = "row",
        backgroundColor = styleEqual(c(Mer_ar(),as.character(unique(Giro_Vt$Vertical)),'TOTAL'),
          c(as.character(rep('gray',length(Mer_ar())+length(as.character(unique(Giro_Vt$Vertical))))),'steelblue')), 
        color = styleEqual(c(Mer_ar(),as.character(unique(Giro_Vt$Vertical)),'TOTAL'),
          c(as.character(rep('white',length(Mer_ar())+length(as.character(unique(Giro_Vt$Vertical))))),'white')))
  )
  
  ###### Para seleccionar dentro de la tabla de Visitas
  observeEvent(input$salida_fort_cell_clicked, {
    info = input$salida_fort_cell_clicked
    a1 <- names(Tabla_SAP())[info$col+2]
    a2 <- substr(a1,nchar(a1),nchar(a1))
    e1 <- names(Tab_Agr())[1]
    e2 <- names(Tab_Agr())[2]
    b1 <- as.character(Tabla_SAP()$l3ave[info$row])
    k <- OPPS_SAP1()
    k$l3ave <- paste(k[[e1]],k[[e2]])
    
    if( is.null(info$value) || info$col %in% c(0:1,4)) return()
    
    if(b1 != "TOTAL"){
      k <- subset( k, l3ave == b1 | k[[e2]] == b1 | Gerencia == b1 | Mercado == b1 )
      b1 <- paste("de", as.character(Tabla_SAP()[[e1]][info$row]) )
    }
    
    if( a2 == "G") {OPPS_Pselec <- k[Etap_Camp == "Ganada"]}
    if( a2 == "F") {OPPS_Pselec <- k[Cerrado == 0]}
    if( a2 == "P") {OPPS_Pselec <- k[Etap_Camp %in% c("Prospección")]}
    if( a2 == "T") {OPPS_Pselec <- k[Etap_Camp %in% c("Temprana")]}
    if( a2 == "A") {OPPS_Pselec <- k[Etap_Camp %in% c("Proyecto","Propuesta")]}
    
    
    OPPS_selec <- f_campos(OPPS_Pselec, total_campos, NULL, Fam_Neg[,2:3], MeDi)
      
    campos_mostrar <- c("Cliente","División / Sector","Nombre de la oportunidad","Descripción de la Oportunidad","Etapa","Paso siguiente","Fecha paso siguiente",
                                                 "Total de Proyecto","Importe Anual","Productos en OPP","Creación","Cierre","ID","Ejecutivo","Gerencia")
    
    fecha_campos <- c("Fecha paso siguiente","Creación","Cierre") 
    fecha_rep <- c(rep('toLocaleDateString',3))
    
    
    showModal(modalDialog(
      title = paste("Detalle de",info$value,"Clientes",b1),
      h6(dataTableOutput(ns("OPPs_Tabla"))),
      footer = modalButton("Salir"),
      size = "l",
      easyClose = TRUE
    ))
    
    
    output$OPPs_Tabla <- renderDataTable(datatable(subset(OPPS_selec, select = campos_mostrar)[order(Cliente, -`Total de Proyecto`, -`Importe Anual`)], 
      rownames = FALSE, extensions = c('FixedColumns','KeyTable','Buttons'), 
      options = list(pageLength = 5, lengthMenu = c(5,20,100,500), dom = 'Bplti', 
      ordering = TRUE, autoWidth = TRUE, buttons = c('excel','copy'), keys = TRUE, scrollX = TRUE, scrollY = TRUE, 
      fixedColumns = list(leftColumns = 2), columnDefs = list(list(width = '200px', targets = c(3,5,9))))) 
      %>% formatCurrency(c("Total de Proyecto","Importe Anual"), digits = 0) 
      %>% formatDate(fecha_campos,fecha_rep)
    )
  })
  ######## Termina para seleccionar en la tabla para la campaña Fortalecimiento 2020
  
} # Fin de la función "Server"

