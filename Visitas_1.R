library(dtplyr)
library(dplyr)
library(data.table)
library(shinyWidgets)



# Para hacer el encabezado de las tablas por DD y Producto
f_encabez_visit <- function(fecha, actual){
  
  if(fecha[2] > fecha[1]+1){
    abc <- paste0('<center>Visitas del ', strftime(fecha[1],format = "%d de %B ")," al ", strftime(fecha[2],format = "%d de %B %Y"),"</center>")
  }
  else{
    abc <- paste0('<center>Visitas del ', strftime(fecha[2],format = "%d de %B %Y"),"</center>")
  }
  
  encabezado <- htmltools::withTags(table(
  class = 'display',
    thead(
        tr(
          th(rowspan = 2, colspan = 2, ' ' ),
          th(colspan = 3, HTML( abc )),
          th(rowspan = 2, colspan = 1, ' '),
          th(colspan = 5, HTML('<center>Siguientes visitas</center>'))
        ),
        tr(
          th(actual[4]),
          th(actual[5]),
          th(actual[6]),
          th(actual[8]),
          th(actual[9]),
          th(actual[10]),
          th(actual[11]),
          th(actual[12])
        )
    )
    ))
  return(encabezado)
}
#####

# Para generar la tabla a mostrar
f_viTabla <- function(x2, Tabla_ag, fechas){
  
  campo_by <- names(Tabla_ag)[1]
  agrupador <- names(Tabla_ag)[2]
  Tabla_ag$l3ave <- paste(Tabla_ag[[campo_by]],Tabla_ag[[agrupador]])
  
  #x2$l3ave <- paste(x2[[campo_by]],x2[[agrupador]])
  x2$suma_campo <- 1
  
  x1 <- subset(x2, Fecha >= fechas[1] & Fecha <= fechas[2])
  
  a  <- x1[ , .(Programadas = sum(suma_campo)) , by = l3ave]
  a <- merge(a, x1[Estado == "Realizada", .(Realizadas = sum(suma_campo)) , by = l3ave], by = "l3ave", all = TRUE)
  a$Avance <- 0
  a$blanco <- 0
  
  d1 <- fechas[2] + if_else(strftime(fechas[2]+1,format = "%A") %in% c("Saturday","sábado"),3, if_else(strftime(fechas[2]+1,format = "%A") %in% c("Sunday","domingo"),2,1))
  a <- merge(a, x2[Fecha == d1, .("D1"=sum(suma_campo)) , by = l3ave], by = "l3ave", all = TRUE)
  
  d2 <- d1 + if_else(strftime(d1+1,format = "%A") %in% c("Saturday","sábado"),3, if_else(strftime(d1+1,format = "%A") %in% c("Sunday","domingo"),2,1))
  a <- merge(a, x2[Fecha == d2, .("D2"=sum(suma_campo)) , by = l3ave], by = "l3ave", all = TRUE)
  
  d3 <- d2 + if_else(strftime(d2+1,format = "%A") %in% c("Saturday","sábado"),3, if_else(strftime(d2+1,format = "%A") %in% c("Sunday","domingo"),2,1))
  a <- merge(a, x2[Fecha == d3, .("D3"=sum(suma_campo)) , by = l3ave], by = "l3ave", all = TRUE)
  
  d4 <- d3 + if_else(strftime(d3+1,format = "%A") %in% c("Saturday","sábado"),3, if_else(strftime(d3+1,format = "%A") %in% c("Sunday","domingo"),2,1))
  a <- merge(a, x2[Fecha == d4, .("D4"=sum(suma_campo)) , by = l3ave], by = "l3ave", all = TRUE)
  
  d5 <- d4 + if_else(strftime(d4+1,format = "%A") %in% c("Saturday","sábado"),3, if_else(strftime(d4+1,format = "%A") %in% c("Sunday","domingo"),2,1))
  a <- merge(a, x2[Fecha == d5, .("D5"=sum(suma_campo)) , by = l3ave], by = "l3ave", all = TRUE)
  
  b <- merge(Tabla_ag, a, by = "l3ave")
  b <- b[order(b[[agrupador]])]
  b <- data.table(agrupador = b[[agrupador]], subset(b, select = as.character(names(b)[grep(agrupador,names(b), invert = TRUE)][1:(ncol(b)-1)])) )
  
  if( nrow(b)>1){
    c1 <- unique(b$agrupador)
    s <- ncol(b)
    k <- b[0,]
    for(i in 1:length(c1)){
      c2 <- b[b$agrupador == c1[i]]
      c3 <- data.frame(agrupador = c1[i], l3ave = c1[i], Div = " ",t(apply( c2[,4:s],2, function(x)sum(x, na.rm = TRUE))) )
      c2[,1] <- " "
      names(c3) <- names(c2)
      c4 <- rbind(c3,c2)
      k <- rbind(k,c4)
    }
    if(length(c1)>1){
      c5 <- data.frame(agrupador = "TOTAL", llave = "TOTAL", Div = " ",t(apply(b[,4:s],2,function(x)sum(x,na.rm = TRUE))) )
      names(c5) <- names(b)
      k <- rbind(k,c5)
    }
    b <- k
  }
    b$blanco <- NA
    b$Avance <- b$Realizadas/b$Programadas
  
  
  encabezado <- c("agrupador","l3ave",campo_by,"Programadas","Realizadas","Avance"," ",strftime(d1,format = "%a %d"),strftime(d2,format = "%a %d"),strftime(d3,format = "%a %d"),strftime(d4,format = "%a %d"),strftime(d5,format = "%a %d"))
  
  names(b) <- encabezado
  
  return(b)
}
#######

# Genera la base con las visitas de "x" usuarios
f_Base_Vis <- function(x1_visR, x2_usr, usrs, fechas){
  u <- as.character(usrs$`Nombre completo`)
  l <- length(u)
  b <- x1_visR[0,]
  b$`Nombre completo` <- NA
  x2_usr$Fecha <- as.Date(x2_usr$Fin)
  x2_usr <- subset(x2_usr, Fecha >= fechas[1] & Fecha <= fechas[2]+7)
  for(i in 1:l){
    w <- as.character(x2_usr[grep(u[i],x2_usr$Asistentes)]$l2ave)
    k <- x1_visR[l2ave %in% w]
    k$`Nombre completo` <- u[i]
    b <- rbind(b,k)
  }
  return(b)
}
#######

# Función UI del módulo
fvisitasUI <- function(id){
  ns <- NS(id)
  uiOutput(ns("kps"))
  
}

# Función "Server" del módulo
fvisitas <- function(input, output, session, usf, fechas, Tabla, Mer_ar, Ct_01, Co_01, DD_01, Ge_01, Vt_01){
  ns <- session$ns
  campos_visitas <- c("l2ave",names(visitas)[!(names(visitas) %in% names(vis1_R))])
  v2isitas <- subset(visitas, select = campos_visitas)
  p5_usuarios <- p4_usuarios[!Cargo %in% c("BAJA-X","BAJA", NA)]
  usuarios <- unique(p5_usuarios$`Nombre completo`)
  usuarios <- usuarios[order(usuarios)]
  ejec_gcia <- unique(subset(p3_cliente, select = c("Propietario de la cuenta","Gerencia")))
  
  
  # Genera el UI a mostrar
  output$kps <- renderUI({
    tagList(
      
      fluidRow(
        column(width = 7, h2(textOutput(ns("titulo"))) ),
        column(width = 5, style = "margin-top: 25px;", selectInput(ns("Nom_ej"), label = "Buscar un Ejecutivo para mostrar sus visitas", multiple = FALSE, choices = c("",usuarios) ) )
      ),  
      h6(awesomeRadio(ns("viVC_1"), label = "", inline = TRUE, checkbox = TRUE, status = "primary", choices = c("Comercial","Ventas TI","Comercial + Ventas TI"), selected = "Ventas TI" )),
      div(style = 'overflow-x: scroll',h6(dataTableOutput(ns("salida_V1"))))
    )
  })
  
  #filtro de Vista del usuario -A que DD tiene acceso-
  V02_x <- reactive({
    x <- vis1_R[!is.na(Estado)]
    if(usf$DD != "TODO"){ x <- subset(x, `Div / Sect 2` %in% unlist(strsplit(usf$DD,"; ")) | Gerencia %in% unlist(strsplit(usf$DD,"; "))  ) }
    return(x)
  })
  
  # Trae las personas que son del mismo equipo de trabajo del usuario seleccionado
  persona <- eventReactive(input$Nom_ej,{
    if(input$Nom_ej == ""){ return("Sin selección")}
    else{ return( subset(p5_usuarios, `Gestor: Nombre completo` %in% as.character(p5_usuarios[`Nombre completo` == input$Nom_ej]$`Gestor: Nombre completo`), select = c("Nombre completo","Gestor: Nombre completo") ) )}
  })
  jefe <- eventReactive(persona(),{
    if(!is.data.frame(persona())){return("")}else{return(as.character(persona()[1,2]))}
    })
  T2bla <- reactive({ 
    if( !is.data.frame(persona()) ){if(!is.null(Ge_01()))return(ejec_gcia)else{return(Tabla())} }else{return( persona() )}
  })
  
  # Filtro por usuarios en caso de seleccionar alguno
  v03_P <- reactive({
    if( !is.data.frame(persona()) ){ return(V02_x())}
    else{return(f_Base_Vis( x1_visR = V02_x(), x2_usr = v2isitas, usrs = persona(), fechas = fechas() ) )}
  })
  
  # filto de visitas por "Filtros"
  vis2_R <- reactive({
    x <- v03_P()
    # filtro por Cliente
    if( !is.null(Ct_01()) ) { x <- subset(x, Cliente %in% Ct_01() ) }
    #Filtros por Coord, DD, Ger
    if( !is.null(Co_01()) || !is.null(DD_01()) || !is.null(Ge_01())){
      if( Co_01() != "" && !(Co_01() %in% c("Salud","Sin Emp.Gpo","Sin F.TMX")) ) { x <- subset(x, `Div / Sect 2` %in% as.character(subset(DivSec, Coordinación %in% Co_01())$`División / Sector`) ) }
      if( Co_01() == "Salud" ) { x <- subset(x, Gerencia  %in% Gerencias_Salud  ) }
      if( Co_01() == "Sin Emp.Gpo" ) {x <- subset(x, !(Gerencia %in% Gerencias_EGrupo) )}
      if( Co_01() == "Sin F.TMX" ) {x <- subset(x, Cliente != filiales ) }
      
      if( !is.null(DD_01() ) ) { x <- subset(x, `Div / Sect 2` %in% DD_01() ) }
      if( !is.null(Ge_01() ) ) { x <- subset(x, Gerencia %in% Ge_01() ) }
    }
    # Filtro Vertical elegida
    if( !is.null(Vt_01() ) ) { x <- subset(x, Cliente %in% unique(subset(Base_X0, Giro %in% as.character(subset(Giro_Vt, Vertical %in% Vt_01())$Giro) )$`Nombre de la cuenta`) ) }
    
    return(x)
  })
  
  # Crea el título del Reporte
  output$titulo <- renderText( paste("Reporte Visitas",ifelse( input$viVC_1 == "Comercial", "equipo Comercial",ifelse(input$viVC_1 == "Ventas TI","equipo Ventas TI","Comercial + Ventas TI" )) ) )
  
  
  # Filtra las OOP de Ventas Consultivas 
  v_final <- reactive({
    x <- vis2_R()
    if( input$viVC_1 == "Comercial" ){ x <- subset(x, l2ave %in% as.character(v2isitas[grepl("Ventas",v2isitas$Asistentes)]$l2ave) ) }
    if( input$viVC_1 == "Ventas TI" ){ x <- subset(x, l2ave %in% lzve) }
    campo_by <- names(T2bla())[1]
    agrupador <- names(T2bla())[2]
    if(agrupador %in% c("Mercado","Gestor: Nombre completo")){ x <- merge(x, T2bla(), by = campo_by ) }
    x$l3ave <- paste(x[[campo_by]],x[[agrupador]])
    return( x )
  })
  
  
    
  Tabla_V1 <- reactive( f_viTabla(v_final(), T2bla(), fechas() ) )
  
  a_seleccionar <- eventReactive(Tabla_V1(),{
    if(!is.data.frame(persona())){return(0)}else{return(grep(input$Nom_ej,as.character(Tabla_V1()$`Nombre completo`)))}
  })
  
  output$salida_V1 <- renderDataTable( datatable(subset(Tabla_V1(), select = -l3ave), rownames = FALSE, selection = list(target = 'row', selected = c(a_seleccionar()) ), extensions = c('Buttons'), container = f_encabez_visit(fecha = fechas(), actual = names(Tabla_V1()) ),
                                                 options = list(pageLength = nrow(Tabla_V1()),dom = 'Bt', ordering = FALSE, buttons = c('excel','copy'))) %>% 
                                         formatStyle(c(3,4,7:11), cursor = 'pointer') %>% formatStyle(2, textAlign = "right") %>% formatStyle(1, textAlign = "left") %>% 
                                         formatPercentage("Avance", digits = 1) %>%
                                         formatCurrency(c(3,4,7:11), currency = "", digits=0) %>%
                                         formatStyle(1, target = "row",
                                                     backgroundColor = styleEqual(c(Mer_ar(),jefe(),'TOTAL'),c(as.character(rep('gray',length(Mer_ar())+1)),'steelblue')), 
                                                     color = styleEqual(c(Mer_ar(),jefe(),'TOTAL'),c(as.character(rep('white',length(Mer_ar())+1)),'white') ))
                                         
                                         
  )
  
  ###### Para seleccionar dentro de la tabla de Visitas
  observeEvent(input$salida_V1_cell_clicked, {
    info = input$salida_V1_cell_clicked
    a1 <- names(Tabla_V1())[info$col+2]
    a2 <- names(T2bla())[1]
    b1 <- as.character(Tabla_V1()$l3ave[info$row])
    k <- v_final()
    
    
    if (is.null(info$value) || info$col %in% c(0:1,4:5) || ( as.character(Tabla_V1()$agrupador[info$row]) != "TOTAL" && b1 == " " ) ) return()
     
    if(b1 != "TOTAL"){
      if(names(T2bla())[2] %in% c("Mercado")){
        k <- subset( k, l3ave == b1 | k[[a2]] == b1 | Gerencia == b1 | Mercado == b1 | "Propietario de la cuenta" == b1)
      }
      else{
        k <- subset( k, l3ave == b1 | k[[a2]] == b1 | Gerencia == b1 | "Propietario de la cuenta" == b1)
      }
      b1 <- paste("de", as.character(Tabla_V1()[[a2]][info$row]) )
    }
    
    if( a1 == "Programadas") {OPPS_selec <- subset(k, Fecha >= fechas()[1] & Fecha <= fechas()[2])}
    if( a1 == "Realizadas") {OPPS_selec <- subset(k, Estado == "Realizada" & Fecha >= fechas()[1] & Fecha <= fechas()[2])}
    if( substr(a1,1,3) %in% c("lun", "mar","mié","jue","vie") ) {
      if( a1 == strftime(fechas()[2] + info$col - 5, format = "%a %d") ){
          OPPS_selec <- subset(k, Fecha == (fechas()[2] + info$col - 5) )
      }
      else{
          OPPS_selec <- subset(k, Fecha == (fechas()[2] + info$col - 3) )
      }
    }
    
    
    OPPS_selec <- merge(OPPS_selec, v2isitas, by = "l2ave")
      campos_mostrar <- c("Fecha","Hora","Cliente","Div / Sect 2","Estado","Descripcion","Resultado de la actividad","Contacto","Asistentes","Productos adicionales","Eventos")
      fecha_campos <- c("Fecha") 
      fecha_rep <- c(rep('toLocaleDateString',1))
    
    
    showModal(modalDialog(
      title = paste("Detalle de",info$value,"Visitas",a1,b1),
      h6(dataTableOutput(ns("OPPs_Tabla"))),
      footer = modalButton("Salir"),
      size = "l",
      easyClose = TRUE
    ))
    
    
    output$OPPs_Tabla <- renderDataTable( datatable( subset(OPPS_selec, select = campos_mostrar)[order(Fecha, Hora)], rownames = FALSE, extensions = c('FixedColumns','KeyTable','Buttons'),
                                                     options = list(pageLength = 5, dom = 'Bplti', ordering = TRUE, autoWidth = TRUE, buttons = c('excel','copy'),
                                                                    keys = TRUE, scrollX = TRUE, scrollY = TRUE, fixedColumns = list(leftColumns = 2))) %>%
                                                                    formatDate(fecha_campos,fecha_rep)
    )
    
  
  })
  ######## Termina para seleccionar en la tabla por Visitas
  
} # Fin de la función "Server" de Visitas


