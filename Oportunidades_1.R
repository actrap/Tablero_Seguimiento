library(readxl)
library(dtplyr)
library(dplyr)
library(data.table)
library(shinyWidgets)
library(htmltools)
library(DT)
library(lubridate)

#a <- "C:/Users/larcos/M_Empresarial/Analisis/Ventas/Aplicaciones/Tablero_Seguimiento/"
a <- "C:/Users/vmorenoa/Documents/R proyectos/pruebas/"
#b <- "/srv/shiny-server/tablero_seguimiento/"
b <- "/srv/shiny-server/pruebas/"

dire1c <- if_else(substr(getwd(),1,1) == "/", b , a)

fico <- as.data.table(read_xlsx(paste0(dire1c,"insumos/catalogo/FiCo.xlsx")))

Predeterminadas<-c("Canal/Sucesos del periodo","Producto de MP/Sucesos del periodo",
  "Producto de MP/Cierre Estimado", "Producto de MP/Dilación","Vertical/Campaña")

Objetos<-c("Oportunidades","Clientes","Servicios","Importe","Total de Proyecto")

fcontaru <- function(x){length(na.omit(unique(x)))}

f_encabez_Opp <- function(fecha, Ry, colu, Rp){
  if (fecha[2] > fecha[1]) { abc <- paste0("<center> Sucesos del ", strftime(fecha[1], format = "%d de %B "), 
    " al ", strftime(fecha[2], format = "%d de %B %Y"), "</center>")
  }
  else {abc <- paste0("<center>Oportunidades del ", strftime(fecha[2], format = "%d de %B %Y"), "</center>")}  
  if(Ry=="Etap1"){
    encabezado <- htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(rowspan = 2, colspan = 3, HTML('<center>',"",'</center>')),
          th(rowspan = 2, colspan = 1, HTML(paste0("<center>", strftime(fecha[1], format = "%d de %B"),"<br>inició con:</center>"))),
          th(colspan = 4, HTML(abc)),
          th(rowspan = 2, colspan = 1, HTML(paste0("<center>", strftime(fecha[2], format = "%d de %B"),"<br>finaliza con:</center>")))
        ),
        tr(
          th(HTML('<center>',"Nueva",'<center>')),
          th(HTML('<center>',"Ganada",'<center>')),
          th(HTML('<center>',"No exitosa",'<center>')),
          th(HTML('<center>',"Perdida",'<center>'))#,
          #th(HTML('<center>',"Final",'<center>'))
        )
      )
    ))
  }
  if(Ry=="Etap2"){
    abc <- paste("<center>Estado de las opps al ", strftime(max(fecha), format = "%d de %B de %Y"), "<center>")
    encabezado <- htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(rowspan = 2, colspan = 3, HTML('<center>',"",'<center>')),
          th(colspan = 4, HTML(abc)),
          th(rowspan = 2, HTML('<center>',"Total",'<center>'))
        ),
        tr(
          th(HTML('<center>',"Prospección",'<center>')),
          th(HTML('<center>',"Temprana",'<center>')),
          th(HTML('<center>',"Proyecto",'<center>')),
          th(HTML('<center>',"Propuesta",'<center>'))
        )
      )
    ))
  }
  if(Ry=="Dila"){
    encabezado <- htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(colspan = 3, HTML('<center>',"",'<center>')),
          th(HTML('<center>',"< 7 Días",'<center>')),
          th(HTML('<center>',"7 - 15 Días",'<center>')),
          th(HTML('<center>',"Más de 15 Días",'<center>')),
          th(HTML('<center>',"Total",'<center>'))
        )
      )
    ))
  }
  if(Ry=="Funnel"){
    abc <- paste("<center>Oportunidades abiertas al ", strftime(max(fecha), format = "%d de %B de %Y"), "<center>")
    encabezado <- htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(rowspan = 2, colspan = 3, HTML('<center>',"",'<center>')),
          th(colspan = 4, HTML(abc))
        ),
        tr(
          th(HTML('<center>',"Clientes",'<center>')),
          th(HTML('<center>',"Oportunidades",'<center>')),
          th(HTML('<center>',"Servicios",'<center>')),
          th(HTML('<center>',"Importe",'<center>'))
        )
      )
    ))
  }
  if(Ry=="Clos"){
    encabezado <- htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(rowspan = 2, colspan = 3, HTML('<center>',"",'<center>')),
          th(colspan = 2, HTML('<center>',"Oportunidades en Proceso",'<center>')),
          th(rowspan = 2, colspan = 1, HTML('<center>',"",'<center>')),
          th(colspan = 4, HTML('<center>',"Cierre Estimado",'<center>'))
        ),
        tr(
          th(HTML('<center>',"Total",'<center>')),
          th(HTML('<center>',"Proy/Prop",'<center>')),
          th(HTML('<center>',as.character(colu[3]),'<center>')),
          th(HTML('<center>',as.character(colu[4]),'<center>')),
          th(HTML('<center>',as.character(colu[5]),'<center>')),
          th(HTML('<center>',as.character(colu[6]),'<center>'))
        )
      )
    ))
  }
  if(Ry=="Camp"){
    ifelse(Rp==Predeterminadas[5],
      {abc <- "<center>Oportunidades desde el inicio de la campaña</center>"},
      {abc <- paste("<center>Oportunidades desde el inicio de la campaña hasta el ", strftime(max(fecha), format = "%d de %B de %Y"),"</center>")}
    )
    encabezado <- htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(rowspan = 2, colspan = 3, HTML('<center>',"",'<center>')),
          th(colspan = 7, HTML(abc)),
          th(rowspan = 2, colspan = 1, HTML('<center>',"Total",'<center>'))
        ),
        tr(
          th(HTML('<center>',as.character(colu[1]),'<center>')),
          th(HTML('<center>',as.character(colu[2]),'<center>')),
          th(HTML('<center>',as.character(colu[3]),'<center>')),
          th(HTML('<center>',as.character(colu[4]),'<center>')),
          th(HTML('<center>',as.character(colu[5]),'<center>')),
          th(HTML('<center>',as.character(colu[6]),'<center>')),
          th(HTML('<center>',as.character(colu[7]),'<center>'))
        )
      )
    ))
  }
  if(Ry=="Prod"){
    abc <- paste("<center>Oportunidades abiertas al ", strftime(max(fecha), format = "%d de %B de %Y"), "<center>")
    encabezado <- htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(rowspan = 2, colspan = 3, HTML('<center>',"",'<center>')),
          th(colspan = length(unique(Tprod$Negocio)), HTML(abc)),
          th(rowspan = 2, HTML('<center>',"Registros únicos",'<center>'))
        ),
        tr(
          th(HTML('<center>',as.character(colu[1]),'<center>')),
          th(HTML('<center>',as.character(colu[2]),'<center>')),
          th(HTML('<center>',as.character(colu[3]),'<center>')),
          th(HTML('<center>',as.character(colu[4]),'<center>')),
          th(HTML('<center>',as.character(colu[5]),'<center>')),
          th(HTML('<center>',as.character(colu[6]),'<center>')),
          th(HTML('<center>',as.character(colu[7]),'<center>'))
        )
      )
    ))
  }
  return(encabezado)
}

# Función UI del módulo

foppsUI <- function(id){ns <- NS(id)
uiOutput(ns("Opps"))}

# Función "Server" del módulo

fopps <- function(input, output, session, usf, Base_P3, Base_02, Base_Y2, b_Ten, M_ta, FCH_R, Et_01, Ct_01, DD_01, 
  Ge_01, Vt_01, Ng_01, Fm_01, Pd_01, clean, back)
  {ns<-session$ns
  output$Opps<-renderUI({
    tagList(
    fluidRow(column(width = 10, h2(renderText("Reporte de Oportunidades"))),
      column(width = 2, awesomeCheckbox(ns("Opp"), "Detalle de Opp's", value = FALSE))),
    fluidRow(column(width = 3, selectInput(ns("Prede"), "Tablas Predeterminadas", choices =c("",Predeterminadas), selected = Predeterminadas[1])),
      column(width = 3, selectInput(ns("Row"), "1. Filas:", choices = c("",names(fico)[2:ncol(fico)]))),
      column(width = 3, selectInput(ns("Col"), "2. Columnas:", choices = "")),
      column(width = 3, selectInput(ns("Obj"), "3. Elementos a mostrar:", choices = Objetos))
      ),
    fluidRow(h6(dataTableOutput(ns("salida_C1"))))
  )})
  
  # Actualiza "Clientes" a Falso para no mostrar detalles
  
  observeEvent(clean(),{updateMaterialSwitch(session, "Opp", value = FALSE)})
  observeEvent(back(),{updateMaterialSwitch(session, "Opp", value = FALSE)})
  
  #Actualiza las opciones disponibles según la selección del usuario
  
  actualiza_campos2 <- function(x){
    if(x %in% Predeterminadas){
      updateSelectInput(session, "Col", choices = "")
      updateSelectInput(session, "Row", selected = "")
    }
    if(x %in% names(fico)){
      updateSelectInput(session, "Col", choices = fico[fico[[x]]==1]$Columnas)
      updateSelectInput(session, "Prede", selected = "")
    }
  }
  
  observeEvent(input$Prede, actualiza_campos2(input$Prede))
  observeEvent(input$Row, actualiza_campos2(input$Row))

  #Función para crear las tablas
  
  Base_03 <- reactive({
    
    a <- input$Prede
    x <- input$Row
    y <- input$Col
    z <- input$Obj
    base <- Base_02()
    
    if(a==Predeterminadas[5] && x==""){base <- Base_P3()}
    if(y==fico$Columnas[7] && a==""){campos <- c("ID",names(Base_02())[!(names(Base_02()) %in% names(Base_Y2()))])
      base_alt <- subset(Base_02(), select = campos)
      base <- merge(Base_Y2(),base_alt,by="ID")
    }
    
    #Para evitar el error que surge con los nombres de las capañas
    
    base[`Campaña_Prod`=="Atracción de Clientes Centro de Datos"]$`Campaña_Prod` <- "Atraccion de Clientes Centro de Datos"
    base[`Campaña_Prod`=="Atracción de Clientes Nube Híbrida"]$`Campaña_Prod` <- "Atraccion de Clientes Nube Hibrida"
    base[`Campaña_Prod`=="Gestión de Aplicaciones SAP"]$`Campaña_Prod` <- "Gestion de Aplicaciones SAP"
    base[`Campaña_Prod`=="Actualización de conmutadores"]$`Campaña_Prod` <- "Actualizacion de conmutadores"
    
    # Crea banderas paras las filas y columnas para las tablas predetrminadas
    if(a!=""){if(a==Predeterminadas[1]){Ry<-"Etap1" 
      Rx<-"Sec"}
    else if(a==Predeterminadas[2]){Ry<-"Etap1"
      Rx<-"Prod1"}
    else if(a==Predeterminadas[3]){Ry<-"Clos"
      Rx<-"Prod1"}
    else if(a==Predeterminadas[4]){Ry<-"Dila"
      Rx<-"Prod1"}
    else if(a==Predeterminadas[5]){Ry<-"Camp"
      Rx<-"Ver"}
    }
    
    # Crea banderas para las columas seleccionadas
    if(y!=""){if(y==fico$Columnas[1]){Ry<-"Etap1"}
      else if(y==fico$Columnas[2]){Ry<-"Etap2"}
      else if(y==fico$Columnas[3]){Ry<-"Clos"}
      else if(y==fico$Columnas[4]){Ry<-"Dila"}
      else if(y==fico$Columnas[5]){Ry<-"Camp"}
      else if(y==fico$Columnas[6]){Ry<-"Funnel"}
      else if(y==fico$Columnas[7]){Ry<-"Prod"}
    }
    
    # Asigna las columnas seleccionadas
    switch(Ry,
      Etap1={base$col<-as.character(base$EtapF)
        colu<-c("Inicial","Nuevas","Ganada","No exitosa","Perdida","Final")},
      Etap2={base$col<-as.character(base$EtapF)
        colu<-c("Prospección","Temprana","Proyecto","Propuesta","Total")},
      Dila={base$col<-as.character(base$bandera)
        colu<-c("< 7 Días","7 - 15 Días","Más de 15 Días","Total")},
      Camp={base$col<-as.character(base$`Campaña_Prod`)
        #colu<-c(as.character(unique(a_campa$`Campaña Reporte`)),"Total")},
        colu<-c("Atraccion de Clientes Centro de Datos","Atraccion de Clientes Nube Hibrida","Gestion de Aplicaciones SAP",
        "Actualizacion de conmutadores","Sucursales","Ciberseguridad (Industria)","Fortalecimiento Conectividad 2020","Total")},
      Clos={base$col<-as.character(format(base$Cierre,"%b %y"))
        base[!(EtapF %in% c("Proyecto","Propuesta"))]$col<-NA
        colu<-c("Final","ProyProp",format(as.Date(paste0(year(Sys.Date()),"-",month(Sys.Date()),"-01"))+months(0:3),"%b %y"))},
      Funnel={base$col<-as.character(base$f1nal)
        colu<-c("Clt","Opp","Srv","Imp")},
      Prod={base$col<-base$Negocio
        base[f1nal!="Final"]$col<-NA
        colu<-c(sort(unique(Tprod$Negocio)),"Total")}
    )
    #base[,colu] <- NULL
    for(i in 1:length(colu)){base[col==colu[i],colu[i]]<-"1"}
    # Crea algunas columnas manualmente
    if(Ry=="Etap1"){
      base[Nueva==1]$Nuevas <- "1"
      base[inicio=="Inicial"]$Inicial <- "1"
      base[f1nal=="Final"]$Final <- "1"
    }
    if(Ry %in% c("Etap2","Dila","Camp","Prod")){base[col %in% colu]$Total <- "1"}
    if(Ry=="Clos"){
      base[f1nal=="Final"]$Final <- "1"
      base[EtapF %in% c("Proyecto","Propuesta")]$ProyProp <- "1"
    }
    if(Ry=="Funnel"){base[f1nal=="Final",colu] <- "1"}
    
    # Crea banderas para las filas
    if(x!=""){if(x==names(fico)[2]){Rx<-"Sec"}
      else if(x==names(fico)[3]){Rx<-"Ver"}
      else if(x==names(fico)[4]){Rx<-"Prod1"}
      else if(x==names(fico)[5]){Rx<-"Prod2"}
      else if(x==names(fico)[6]){Rx<-"Cam"}
    }
    # Asigna las filas
    switch(Rx,
      Sec={b<-base
        ifelse(!is.null(Ct_01()),{
          b$R1<-b$Cliente
          b$R2<-b$Ejecutivo},
          ifelse(!is.null(Ge_01()),{
            b$R1<-b$Gerencia
            b$R2<-b$Cliente
            #b$R2<-paste0(b$Ejecutivo,"-",b$Cliente)
            },ifelse(!is.null(DD_01()) && DD_01()!="",{
              b$R1<-b$`División / Sector`
              b$R2<-b$Gerencia},{
              b$R1<-b$Mercado
              b$R2<-b$`División / Sector`}
      )))},
      Ver={b<-merge(base, Giro_Vt, by="Giro")
        ifelse(!is.null(Vt_01()),{
          b$R1<-b$Vertical
          b$R2<-b$Giro},{
          b$R1<-"Total"
          b$R2<-b$Vertical}
      )},
      Cam={b<-base[!is.na(`Campaña_Prod`)]
        b$R1<-"Total"
        b$R2<-b$`Campaña_Prod`},
      Prod1={b<-base
      ifelse(!is.null(Fm_01()),{
        b$R1<-b$Familia
        b$R2<-b$Nombre_corto_Producto},
        ifelse(!is.null(Ng_01()) && Ng_01()!="",{
          b$R1<-b$Negocio
          b$R2<-b$Familia},{
          b$R1<-"Total"
          b$R2<-b$Negocio
      }))},
      Prod2={c<-c("ID",names(base)[!(names(base) %in% names(Base_Y2()))])
      #c<-c("ID","col",colu)
      b<-merge(subset(base, select = c), Base_Y2(), by="ID")
      ifelse(!is.null(Fm_01()),{
        b$R1<-b$Familia
        b$R2<-b$Nombre_corto_Producto},
        ifelse(!is.null(Ng_01()) && Ng_01()!="",{
          b$R1<-b$Negocio
          b$R2<-b$Familia},{
          b$R1<-"Total"
          b$R2<-b$Negocio
      }))},
    )
    
    b$R0<-"Total"
    
    # Asigna los elementos a mostrar
    if(!is.null(z)){
      if(z==Objetos[1]){b$Rz<-b$ID}
      else if(z==Objetos[2]){b$Rz<-b$Cliente}
      else if(z==Objetos[3]){b$Rz<-b$Servicios}
      else if(z==Objetos[4]){b$Rz<-b$Importe}
      else if(z==Objetos[5]){b$Rz<-b$`Total de Proyecto`}
    }
    
    #Reemplaza las banderas por los objetos a mostrar
    if(Ry=="Funnel"){
      b[b[[colu[1]]]=="1"][[colu[1]]] <- b[b[[colu[1]]]=="1"]$Cliente
      b[b[[colu[2]]]=="1"][[colu[2]]] <- b[b[[colu[2]]]=="1"]$ID
      b[b[[colu[3]]]=="1"][[colu[3]]] <- b[b[[colu[3]]]=="1"]$Servicios
      b[b[[colu[4]]]=="1"][[colu[4]]] <- b[b[[colu[4]]]=="1"]$Importe
      for(i in 3:length(colu)){b[[colu[i]]]<-as.numeric(b[[colu[i]]])}
    }
    else{
      for(i in 1:length(colu)){b[b[[colu[i]]]=="1"][[colu[i]]] <- b[b[[colu[i]]]=="1"]$Rz}
      if(z %in% Objetos[3:5]){for(i in 1:length(colu)){b[[colu[i]]]<-as.numeric(b[[colu[i]]])}}
    }
    
    bl<-list(b, colu, Ry, Rx)
    
    return(bl)
  })
  
  vista.opp <<- eventReactive(input$Prede, return(Base_03()[[4]]))
  vista.opp <<- eventReactive(input$Row, return(Base_03()[[4]]))

  Basef <- reactive({
    
    b <- subset(Base_03()[[1]], select=c("R0","R1","R2","Rz",Base_03()[[2]]))
    colu <- Base_03()[[2]]
    Ry <- Base_03()[[3]]
    Rx <- Base_03()[[4]]
    z <- input$Obj
    
    if(Ry=="Funnel"){
      b0 <- merge(b[, lapply(.SD, fcontaru), by = R0, .SDcols = colu[1:2]],
        b[, lapply(.SD, sum, na.rm=T), by = R0, .SDcols = colu[3:4]], by="R0")
      b0$R1 <- "Total"
      b0$R2 <- ""
      b0$llave <- paste0(b0$R0,"x")
      b1 <- merge(b[, lapply(.SD, fcontaru), by = .(R0,R1), .SDcols = colu[1:2]],
        b[, lapply(.SD, sum, na.rm=T), by = .(R0,R1), .SDcols = colu[3:4]], by=c("R0","R1"))
      b1$R2 <- ""
      b1$llave <- paste0(b1$R0,b1$R1)
      b2 <- merge(b[, lapply(.SD, fcontaru), by = .(R0,R1,R2), .SDcols = colu[1:2]],
        b[, lapply(.SD, sum, na.rm=T), by = .(R0,R1,R2), .SDcols = colu[3:4]], by=c("R0","R1","R2"))
      b2$llave <- paste0(b2$R0,b2$R1,b2$R2)
      b2$R1 <- ""
    }
    else{
      if(!is.null(z)){
        if(z %in% Objetos[1:2]){
          b0 <- b[, lapply(.SD, fcontaru), by = R0, .SDcols = colu]
          b0$R1 <- "Total"
          b0$R2 <- ""
          b0$llave <- paste0(b0$R0,"x")
          b1 <- b[, lapply(.SD, fcontaru), by = .(R0,R1), .SDcols = colu]
          b1$R2 <- ""
          b1$llave <- paste0(b1$R0,b1$R1)
          b2 <- b[, lapply(.SD, fcontaru), by = .(R0,R1,R2), .SDcols = colu]
          b2$llave <- paste0(b2$R0,b2$R1,b2$R2)
          b2$R1 <- ""
        }
        else if(z %in% Objetos[3:5]){
          b0 <- b[, lapply(.SD, sum, na.rm=T), by = R0, .SDcols = colu]
          b0$R1 <- "Total"
          b0$R2 <- ""
          b0$llave <- paste0(b0$R0,"x")
          b1 <- b[, lapply(.SD, sum, na.rm=T), by = .(R0,R1), .SDcols = colu]
          b1$R2 <- ""
          b1$llave <- paste0(b1$R0,b1$R1)
          b2 <- b[, lapply(.SD, sum, na.rm=T), by = .(R0,R1,R2), .SDcols = colu]
          b2$llave <- paste0(b2$R0,b2$R1,b2$R2)
          b2$R1 <- ""
        }
      }
    }
    
    if(nrow(b0)>0){
      if(b1[1,"R1"]=="Total"){b <- rbind(b0,b2)[order(llave)]}
      else {b <- rbind(b0,b1,b2)[order(llave)]}
    }
    else{b <- data.table(R1="Sin Información",R2=NA)
      b <- b[,(colu):=NA]
    }
    
    #Dar formato a la tabla
    
    b$bco1 <- NA
    b$bco2 <- NA
    
    if(Ry=="Clos"){b <- subset(b, select = c("R1","R2","bco1", colu[1:2], "bco2", colu[3:6]))}
    else {b <- subset(b, select = c("R1","R2","bco1",colu))}
    
    return(b)
    
  })
  
  #Muestra la tabla agrupada o el detalle de las oportunidades filtradas
  
  observeEvent(input$Opp,
    if(input$Opp){
      
      if((length(Et_01()) < 3 & ("Ganada" %in% Et_01() | "Perdida" %in% Et_01() | "No exitosa" %in% Et_01())))
        {campos_mostrar <- campos_para_cerradas
        fecha_campos <- c("Cierre") 
        fecha_rep <- c(rep('toLocaleDateString',1))
        targ200 <- c(3,4,10)} 
      else {campos_mostrar <- campos_incluye_T.Proy
        fecha_campos <- c("Fecha paso siguiente","Creación","Cierre") 
        fecha_rep <- c(rep('toLocaleDateString',3))
        targ200 <- c(3,5,10,25)}
      
      moneda_campos <- c("Total de Proyecto","Importe Anual","Importe")
      moneda_simbolo <- "$"
      
      output$salida_C1 <- renderDataTable(datatable(subset(Base_02(), select = campos_mostrar), 
        rownames = FALSE, extensions = c('FixedColumns','KeyTable','Buttons'),
        options = list(pageLength = min(25, nrow(Base_02())), dom = 'Bplti', ordering = TRUE, autoWidth = TRUE, 
        buttons = c('excel','copy'), keys = TRUE, scrollX = TRUE, scrollY = TRUE, 
        fixedColumns = list(leftColumns = 2), columnDefs = list(list(width = '200px', targets = targ200 )))) 
        %>% formatCurrency(moneda_campos, currency = moneda_simbolo, digits = 0) 
        %>% formatDate(fecha_campos,fecha_rep)
      )
    }
    else {
      
      moneda <- eventReactive(input$Obj,{ifelse(input$Obj %in% Objetos[4:5],"$","")})
      sub_agr <- reactive(c(unique(Basef()[R1!=""]$R1)))
      
      observeEvent(input$Col,
        if(input$Col!=fico$Columnas[6]){
          output$salida_C1 <- renderDataTable(datatable(Basef(), rownames = FALSE, extensions = c('Buttons'),
            container = f_encabez_Opp(fecha = FCH_R(), Ry = Base_03()[[3]], colu = Base_03()[[2]], Rp = input$Prede),
            options = list(pageLength = nrow(Basef()), dom = 'Bti', ordering = TRUE, buttons = c('excel','copy')))
            %>% formatStyle(c(3:length(names(Basef()))+1), cursor = 'pointer') 
            %>% formatCurrency(3:length(names(Basef()))+1, currency = moneda(), digits=0)
            %>% formatStyle(2, textAlign = "right") 
            %>% formatStyle(1, textAlign = "left")  
            %>% formatStyle("R1", target = "row", 
              background = styleEqual(c("xx",sub_agr()),c(as.character(rep('gray',length(sub_agr()))),'steelblue')), 
              color = styleEqual(c(sub_agr()),c(as.character(rep('white',length(sub_agr()))))))
          )
        }
        else{
          output$salida_C1 <- renderDataTable(datatable(Basef(), rownames = FALSE, extensions = c('Buttons'),
            container = f_encabez_Opp(fecha = FCH_R(), Ry = Base_03()[[3]], colu = Base_03()[[2]], Rp = input$Prede),
            options = list(pageLength = nrow(Basef()), dom = 'Bti', ordering = TRUE, buttons = c('excel','copy')))
            %>% formatStyle(c(3:length(names(Basef()))+1), cursor = 'pointer')
            %>% formatCurrency('Imp', currency = "$", digits = 0)
            %>% formatRound(c('Clt','Opp','Srv'),0) 
            %>% formatStyle(2, textAlign = "right") 
            %>% formatStyle(1, textAlign = "left")  
            %>% formatStyle("R1", target = "row", 
              background = styleEqual(c("xx",sub_agr()),c(as.character(rep('gray',length(sub_agr()))),'steelblue')), 
              color = styleEqual(c(sub_agr()),c(as.character(rep('white',length(sub_agr()))))))
          )
        }
    )}
  )
  
  #Para seleccionar dentro de la tabla
  
  dato_elegido_opp <<- eventReactive(input$salida_C1_cell_clicked, {
    info <- input$salida_C1_cell_clicked
    if(!is.null(info$value) && info$col %in% c(0,1)){return(Basef()$R2[info$row])}
  })
  
  #Mostrar una ventana emergente con el detalle de las opps
  
  observeEvent(input$salida_C1_cell_clicked, {
    
    info = input$salida_C1_cell_clicked
    
    if(is.null(info$value))return()
    
    Ry <- Base_03()[[3]]
    Rx <- Base_03()[[4]]
    ry <- names(Basef())[info$col+1]
    r1 <- as.character(Basef()$R1[info$row])
    r2 <- as.character(Basef()$R2[info$row])
    op <- as.numeric(Basef()[info$row,5])
    b <- Base_03()[[1]]
    
    if(input$Opp==F){
      if(!is.null(info$col)){b <- b[!is.na(b[[ry]])]}
      if(!is.null(info$row)){
        if(r1!=""){ifelse(r1=="Total",
          b <- b,
          b <- b[R1==r1]
        )}
        else{b <- b[R2==r2]}
      }
    
      if(!is.null(info$value) && info$col %in% c(2:length(names(Basef())))){
        titulo <- ifelse(Ry=="Funnel",
          paste("Detalle de", round(op), "Oportunidades en", ifelse(r1=="", as.character(r2), as.character(r1))),
          paste("Detalle de", round(info$value), as.character(input$Obj), "en", as.character(ry), "de", ifelse(r1=="", as.character(r2), as.character(r1)))
        )
        showModal(modalDialog( 
          title = titulo,
          h6(dataTableOutput(ns("BaseSM"))),
          footer = modalButton("Salir"),
          size = "l",
          easyClose = TRUE
        ))
      }
      
      ifelse(ry %in% c("Ganada", "No exitosa","Perdida"),
        {campos_mostrar <- campos_para_cerradas
        fecha_campos <- c("Cierre") 
        fecha_rep <- c(rep('toLocaleDateString',1))}, 
        {campos_mostrar <- campos_incluye_T.Proy
        fecha_campos <- c("Fecha paso siguiente","Creación","Cierre") 
        fecha_rep <- c(rep('toLocaleDateString',3))}
      )
      moneda_campos <- c("Total de Proyecto","Importe Anual","Importe")
      moneda_simbolo <- "$"
      targ200 <- c(3,4)
      
      if((Ry=="Prod" || Rx=="Prod2") && is.null(Pd_01())){
        campos_mostrar <- campos_mostrar[!(campos_mostrar %in% c("Servicios","Importe","Negocio"))]
        moneda_campos <- c("Total de Proyecto","Importe Anual")
      }
    
      output$BaseSM <- renderDataTable(datatable(unique(subset(b, select = campos_mostrar)), 
        rownames = FALSE, extensions = c('FixedColumns','KeyTable','Buttons'),
        options = list(pageLength = 5, dom = 'Bplti', ordering = TRUE, autoWidth = TRUE, buttons = c('excel','copy'),
          keys = TRUE, scrollX = TRUE, scrollY = TRUE, fixedColumns = list(leftColumns = 2),
          columnDefs = list(list(width = '200px', targets = targ200 )))) 
        %>% formatCurrency(moneda_campos, currency = moneda_simbolo, digits = 0) 
        %>% formatDate(fecha_campos,fecha_rep)
      )
    }
      
  })
  
  
  
  
  
  
  
  
  
  
  
}

  