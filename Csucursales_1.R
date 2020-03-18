library(dtplyr)
library(dplyr)
library(data.table)
library(shinyWidgets)


# Para hacer el encabezado de las tablas por DD y Producto
f_encabez_CS <- function(actual){
  
  encabezado <- htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(rowspan = 2, colspan = 2, ' ' ),
        th(colspan = 2, HTML('<center>Compromiso</center>')),
        th(rowspan = 2, colspan = 1, ' '),
        th(colspan = 3, HTML('<center>Cumplimiento</center>')),
        th(rowspan = 2, colspan = 1, ' '),
        th(colspan = 5, HTML('<center>En Proceso</center>'))
      ),
      tr(
        th("Clientes"),
        th("Sucursales"),
        th("Clientes"),
        th("Sucursales"),
        th(actual[9]),
        #th(actual[6]),
        th("Clientes"),
        th("Sucursales"),
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
f_CS_Tabla <- function(x2, xC1, Tabla_ag, x_comp){
  
  x2 <- subset(x2, !Etapa %in% c("PROSPECTACION (FP)") )
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
  else{
    Tabla_ag$Gerente <- " "
  }
    
  
  x2$suma_campo <- 1
  
  
  p1_G <- x2[Etapa == "GANADA",.(tot_suc = sum(sucursales, na.rm = TRUE), tot_sgan=sum(suc_ganadas, na.rm = TRUE)), by = .(l3ave, `Nombre de la cuenta`)]
  p2_F <- x2[!Etapa %in% c("GANADA","PERDIDA"),.(Opp_F = .N, tot_suc = sum(sucursales, na.rm = TRUE), tot_sgan=sum(suc_ganadas, na.rm = TRUE)), by = .(l3ave, `Nombre de la cuenta`)]
  p2_F <- merge(p2_F, x2[Etap_Camp=="Temprana", .(Temprana_F = .N), by = `Nombre de la cuenta`], by = "Nombre de la cuenta", all.x = TRUE)
  p2_F <- merge(p2_F, x2[Etap_Camp %in% c("Proyecto","Propuesta"), .(Avanzada_F = .N), by = `Nombre de la cuenta`], by = "Nombre de la cuenta", all.x = TRUE)
  
  t0 <- x_comp[Gerencia %in% GG, .(Cte_C = sum(Clientes, na.rm = TRUE), Suc_C = sum(Sucursales, na.rm = TRUE)), by = l3ave]
  t0$blanc1 <- 0
  t1 <- p1_G[,.(Cte_G = .N, Suc_G = sum(tot_sgan)), by = l3ave]
  t1$Avance <- 0
  t1$blanc2 <- 0
  
  t2 <- p2_F[,.(Cte_F = .N, Suc_F = sum(tot_suc), Opp_F = sum(Opp_F), Opp_T = sum(Temprana_F, na.rm = TRUE), Opp_A = sum(Avanzada_F, na.rm = TRUE) ), by = l3ave]
  
  t1 <- merge(t0,t1, all = TRUE, by = "l3ave")
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
  b$blanc1 <- NA
  b$blanc2 <- NA
  b$Avance <- b$Suc_G/b$Suc_C
  
  #encabezado <- c("agrupador","l3ave",campo_by,"Programadas","Realizadas","Avance"," ",strftime(d1,format = "%a %d"),strftime(d2,format = "%a %d"),strftime(d3,format = "%a %d"),strftime(d4,format = "%a %d"),strftime(d5,format = "%a %d"))
  
  #names(b) <- encabezado
  
  return(b)
}
#######

# Función UI del módulo
fsucursalUI <- function(id){
  ns <- NS(id)
  uiOutput(ns("kbp")) 
  
}

# Función "Server" del módulo
fsucursal <- function(input, output, session, direcc, OPPS_CS, OPTotal, Div_var, Mer_ar ){ 
  
  compromiso <- as.data.table(read_excel(paste0(direcc,"insumos/Compromiso_vta_2019.xlsx"), sheet = "Tabla"))[!is.na(Gerencia)]
  compromiso <- merge(compromiso, Div_Gerencia, by = "Gerencia", all.x = TRUE)
  compromiso <- merge(compromiso, MeDi, by = "Div / Sect 2", all.x = TRUE)
  
  #OPPS_CS <- reactive({
  #  ex3 <- c("INDUSTRIA MONTERREY","SERVICIOS MONTERREY","FINANCIERO Y SERVICIOS MONTERREY FINANZAS")
  #  x2 <- OPPT_CS()
  #  #x2[Gerencia %in% ex3]$`Div / Sect 2` <- x2[Gerencia %in% ex3]$Gerencia
  #  return(x2)
  #})
  
  ns <- session$ns
  
  p5_usuarios <- p4_usuarios[!Cargo %in% c("BAJA-X","BAJA", NA)]
  usuarios <- unique(p5_usuarios$`Nombre completo`)
  usuarios <- usuarios[order(usuarios)]
  
  output$kbp <- renderUI( 
    tagList(
      fluidRow(
        column(width = 7, h2('Conectividad Total de Sucursales') ),
        column(width = 5, h2(htmlOutput(ns("tex_salida"))) ) 
        ), 
      div(style = 'overflow-x: scroll',h6(dataTableOutput(ns("salida_CS1"))))
    )
  )
  
  
  # Trae las personas que son del mismo equipo de trabajo del usuario seleccionado
  #persona <- eventReactive(input$Nom_1ej,{
  #  if(input$Nom_1ej == ""){ return("Sin selección")}
  #  else{ return( subset(p5_usuarios, `Gestor: Nombre completo` %in% as.character(p5_usuarios[`Nombre completo` == input$Nom_1ej]$`Gestor: Nombre completo`), select = c("Nombre completo","Gestor: Nombre completo") ) )}
  #})
  #jefe <- eventReactive(persona(),{
  #  if(!is.data.frame(persona())){return("")}else{return(as.character(persona()[1,2]))}
  #})
  
  output$tex_salida <- renderText( paste("<center>Día", 100 - (as.Date("2019-10-15") - Sys.Date()), "de 100</center>") )
  
  T2bla <- reactive({ 
    if( "Gerencia" %in% names(Div_var()) ){return( Div_var() )}else{return( Div_var() )}
  })
  
  
  Tabla_CS1 <- reactive( f_CS_Tabla(OPPS_CS(), OPTotal(), T2bla(), compromiso ) )
  
  #a_seleccionar <- eventReactive(Tabla_CS1(),{
  #  if(!is.data.frame(persona())){return(0)}else{return(grep(input$Nom_1ej, as.character(Tabla_CS1()$`Nombre completo`)))}
  #})
  
  output$salida_CS1 <- renderDataTable( datatable(subset(Tabla_CS1(), select = -l3ave), rownames = FALSE, extensions = c('Buttons'), selection = 'none', container = f_encabez_CS(actual = names(Tabla_CS1()) ), #selection = list(target = 'row', selected = c(a_seleccionar()) ),
                                                 options = list(pageLength = nrow(Tabla_CS1()), dom = 'Bt', ordering = FALSE, buttons = c('excel','copy') ) ) %>% 
                                         formatStyle(c(6,10,12:14), cursor = 'pointer') %>% formatStyle(2, textAlign = "right") %>% formatStyle(1, textAlign = "left") %>% 
                                         formatCurrency(3:14, currency = "", digits=0) %>% 
                                         formatPercentage("Avance", digits = 1) %>%
                                         formatStyle(1, target = "row",
                                                     backgroundColor = styleEqual(c(Mer_ar(),'TOTAL'),c(as.character(rep('gray',length(Mer_ar()))),'steelblue')), 
                                                     color = styleEqual(c(Mer_ar(),'TOTAL'),c(as.character(rep('white',length(Mer_ar()))),'white') ))
                                       
                                       
  )
  
  ###### Para seleccionar dentro de la tabla de Visitas
  observeEvent(input$salida_CS1_cell_clicked, {
    info = input$salida_CS1_cell_clicked
    a1 <- names(Tabla_CS1())[info$col+2]
    a2 <- substr(a1,nchar(a1),nchar(a1))
    e1 <- names(T2bla())[1]
    e2 <- names(T2bla())[2]
    b1 <- as.character(Tabla_CS1()$l3ave[info$row])
    k <- OPPS_CS()
    k$l3ave <- paste(k[[e1]],k[[e2]])
    
    if( is.null(info$value) || info$col %in% c(0:4,6:8,10)  ) return()
    
    if(b1 != "TOTAL"){
      k <- subset( k, l3ave == b1 | k[[e2]] == b1 | Gerencia == b1 | Mercado == b1 )
      b1 <- paste("de", as.character(Tabla_CS1()[[e1]][info$row]) )
    }
    
    #if( a2 == "C") {OPPS_selec <- subset(k, Fecha >= fechas()[1] & Fecha <= fechas()[2])}
    if( a2 == "G") {OPPS_Pselec <- k[Etapa == "GANADA"]}
    if( a2 == "F") {OPPS_Pselec <- k[Cerrado == 0]}
    if( a2 == "T") {OPPS_Pselec <- k[Etap_Camp=="Temprana"]}
    if( a2 == "A") {OPPS_Pselec <- k[Etap_Camp %in% c("Proyecto","Propuesta")]}
    
    
    OPPS_selec <- f_campos(OPPS_Pselec, total_campos, NULL, Fam_Neg[,2:3], MeDi)
      
    campos_mostrar <- c("Cliente","División / Sector","Nombre de la oportunidad","Descripción de la Oportunidad","sucursales","Etapa","Paso siguiente","Fecha paso siguiente",
                                                 "Total de Proyecto","Importe Anual","Productos en OPP","Creación","Cierre","ID","Ejecutivo","Gerencia","suc_ganadas")
    
    fecha_campos <- c("Fecha paso siguiente","Creación","Cierre") 
    fecha_rep <- c(rep('toLocaleDateString',3))
    
    
    showModal(modalDialog(
      title = paste("Detalle de",info$value,"Clientes",b1),
      h6(dataTableOutput(ns("OPPs_Tabla"))),
      footer = modalButton("Salir"),
      size = "l",
      easyClose = TRUE
    ))
    
    
    output$OPPs_Tabla <- renderDataTable( datatable( subset(OPPS_selec, select = campos_mostrar)[order(Cliente, -`Total de Proyecto`, -`Importe Anual`)], rownames = FALSE, extensions = c('FixedColumns','KeyTable','Buttons'),
                                                     options = list(pageLength = 5, dom = 'Bplti', ordering = TRUE, autoWidth = TRUE, buttons = c('excel','copy'),
                                                                    keys = TRUE, scrollX = TRUE, scrollY = TRUE, fixedColumns = list(leftColumns = 2),
                                                     columnDefs = list(list(width = '200px', targets = c(3,6,10) )) )) %>%
                                            formatCurrency(c("Total de Proyecto","Importe Anual"), digits = 0) %>% formatDate(fecha_campos,fecha_rep)
    )
    
    
  })
  ######## Termina para seleccionar en la tabla por Visitas
  
} # Fin de la función "Server" de Visitas


