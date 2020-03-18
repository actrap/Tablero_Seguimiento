library(dtplyr)
library(dplyr)
library(data.table)
library(shinyWidgets)


# Para hacer el encabezado de las tablas por DD y Producto
f_encabez_fort <- function(){
  
  encabezado <- htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(rowspan = 2, colspan = 2, ' ' ),
        th(colspan = 3, HTML('<center>Ganados</center>')),
        th(rowspan = 2, colspan = 1, ' '),
        th(colspan = 5, HTML('<center>En Proceso</center>'))
      ),
      tr(
        th("Clientes"),
        th("Enlaces"),
        th("Opps"),
        th("Clientes"),
        th("Enlaces"),
        th("Total Opps"),
        th("Temprana"),
        th("Avanzada")
      )
    )
  ))
  return(encabezado)
}

#####

# Para generar la tabla a mostrar
f_fort_Tabla <- function(x2, xC1, Tabla_ag, x_comp){
  
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

  p1_G <- x2[Etapa == "GANADA",.(enlaces_G = sum(enlaces_ganados, na.rm = TRUE), Opp_G = .N), by = .(l3ave, `Nombre de la cuenta`)]
  p2_F <- x2[!Etapa %in% c("GANADA","PERDIDA"),.(enlaces_F = sum(enlaces, na.rm = TRUE), Opp_F = .N), by = .(l3ave, `Nombre de la cuenta`)]
  p2_F <- merge(p2_F, x2[Etap_Camp=="Temprana", .(Temprana_F = .N), by = `Nombre de la cuenta`], by = "Nombre de la cuenta", all.x = TRUE)
  p2_F <- merge(p2_F, x2[Etap_Camp %in% c("Proyecto","Propuesta"), .(Avanzada_F = .N), by = `Nombre de la cuenta`], by = "Nombre de la cuenta", all.x = TRUE)
  

  t1 <- p1_G[,.(Cte_G = .N, enlaces_G=sum(enlaces_G, na.rm = TRUE), Opp_G = sum(Opp_G)), by = l3ave]
  t1$blanc2 <- 0
  
  t2 <- p2_F[,.(Cte_F = .N, enlaces_F = sum(enlaces_F, na.rm = TRUE), Opp_F = sum(Opp_F), Opp_T = sum(Temprana_F, na.rm = TRUE), Opp_A = sum(Avanzada_F, na.rm = TRUE) ), by = l3ave]
  
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
ffortalecimientoUI <- function(id){ns <- NS(id)
  uiOutput(ns("kbp")) 
}

# Función "Server" del módulo
ffortalecimiento <- function(input, output, session, direcc, OPPS_fort, OPTotal, Div_var, Mer_ar ){ 
  
  comproFort <- as.data.table(read_excel(paste0(direcc,"insumos/Compromiso_vta_2019.xlsx"), sheet = "Tabla"))[!is.na(Gerencia)]
  comproFort <- merge(comproFort, Div_Gerencia, by = "Gerencia", all.x = TRUE)
  comproFort <- merge(comproFort, MeDi, by = "Div / Sect 2", all.x = TRUE)

  ns <- session$ns
  
## Función que trae el UI  
  output$kbp <- renderUI( 
    tagList(
      fluidRow(
        column(width = 7, h2('Fortalecimiento de Conectividad 2020') ),
        column(width = 5, h2(htmlOutput(ns("texft_salida"))) )
        ), 
      div(style = 'overflow-x: scroll',h6(dataTableOutput(ns("salida_fort"))))
    )
  )
  
  output$texft_salida <- renderText( paste("<center>Día",Sys.Date()-as.Date("2019-10-07")))

## Definición de Tabla a mostrar
  
  Tabla_fort <- reactive( f_fort_Tabla(OPPS_fort(), OPTotal(), Div_var(), comproFort ) )

  output$salida_fort <- renderDataTable(datatable(subset(Tabla_fort(),select = -l3ave), 
    rownames = FALSE, 
    extensions = c('Buttons'), selection = 'none', container = f_encabez_fort(), 
    options = list(pageLength = nrow(Tabla_fort()), dom = 'Bt', ordering = FALSE, buttons = c('excel','copy') ) ) 
    %>% formatStyle(c(3:5,7:11), cursor = 'pointer') 
    %>% formatStyle(2, textAlign = "right") 
    %>% formatStyle(1, textAlign = "left") 
    %>% formatCurrency(3:11, currency = "", digits=0) 
    %>% formatStyle(1, target = "row",
        backgroundColor = styleEqual(c(Mer_ar(),'TOTAL'),c(as.character(rep('gray',length(Mer_ar()))),'steelblue')), 
        color = styleEqual(c(Mer_ar(),'TOTAL'),c(as.character(rep('white',length(Mer_ar()))),'white')))
  )
  
  ###### Para seleccionar dentro de la tabla de Visitas
  observeEvent(input$salida_fort_cell_clicked, {
    info = input$salida_fort_cell_clicked
    a1 <- names(Tabla_fort())[info$col+2]
    a2 <- substr(a1,nchar(a1),nchar(a1))
    e1 <- names(Div_var())[1]
    e2 <- names(Div_var())[2]
    b1 <- as.character(Tabla_fort()$l3ave[info$row])
    k <- OPPS_fort()
    k$l3ave <- paste(k[[e1]],k[[e2]])
    
    if( is.null(info$value) || info$col %in% c(0:1,3,5,7)) return()
    
    if(b1 != "TOTAL"){
      k <- subset( k, l3ave == b1 | k[[e2]] == b1 | Gerencia == b1 | Mercado == b1 )
      b1 <- paste("de", as.character(Tabla_fort()[[e1]][info$row]) )
    }
    
    if( a2 == "G") {OPPS_Pselec <- k[Etap_Camp == "Ganada"]}
    if( a2 == "F") {OPPS_Pselec <- k[Cerrado == 0]}
    if( a2 == "T") {OPPS_Pselec <- k[Etap_Camp %in% c("Temprana")]}
    if( a2 == "A") {OPPS_Pselec <- k[Etap_Camp %in% c("Proyecto","Propuesta")]}
    
    
    OPPS_selec <- f_campos(OPPS_Pselec, total_campos, NULL, Fam_Neg[,2:3], MeDi)
      
    campos_mostrar <- c("Cliente","División / Sector","Nombre de la oportunidad","Descripción de la Oportunidad","Etapa","Paso siguiente","Fecha paso siguiente",
                        "Total de Proyecto","Importe Anual","Productos en OPP","Creación","Cierre","ID","Ejecutivo","Gerencia","enlaces","enlaces_ganados")
    
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

