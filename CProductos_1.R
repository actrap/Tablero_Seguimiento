library(readxl)
library(dtplyr)
library(dplyr)
library(data.table)
library(shinyWidgets)
library(htmltools)
library(lubridate)
library(DT)

#a <- "E:/Archivos_LARCOS/Aplicaciones/Tablero_Seguimiento/"
a <- "C:/Users/larcos/M_Empresarial/Analisis/Ventas/Aplicaciones/Tablero_Seguimiento/"
#a <- "C:/Users/vmorenoa/Documents/R proyectos/tablero_seguimiento/"
#a <- "C:/Users/vmorenoa/Documents/R proyectos/pruebas/"
b <- "/srv/shiny-server/tablero_seguimiento/"
#b <- "/srv/shiny-server/pruebas/"

dire1c <- if_else(substr(getwd(),1,1) == "/", b , a)

#Crear la base de fechas validas de corte

FCorte<-data.frame(fec1=as.Date("2020-01-01")-months(c(0:25)))
#FCorte<-data.frame(fec1=as.Date(paste0(year(Sys.Date()),"-",month(Sys.Date())-1,"-01"))-months(c(0:(month(Sys.Date())+12))))
FCorte$fec2<-format(FCorte$fec1,"%B %Y")
FCorte$fec<-c(1:nrow(FCorte))

# Para hacer el encabezado de las tablas por DD y Producto
f_encabez_PROD <- function(fecha){
  encabezado <- htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(colspan = 3, HTML('<center>',paste("Mensual: ",fecha),'<center>')),
        th(rowspan = 2, colspan = 1, HTML('<center>',"",'<center>')),
        th(colspan = 3, HTML('<center>Acumulada</center>')),
        th(rowspan = 2, colspan = 1, ' '),
        th(colspan = 5, HTML('<center>',as.character(fecha),'</center>'))
      ),
      tr(
        th(HTML('<center>',"Altas",'<center>')),
        th(HTML('<center>',"Bajas",'<center>')),
        th(HTML('<center>',"Ganancia",'<center>')),
        th(HTML('<center>',"Altas",'<center>')),
        th(HTML('<center>',"Bajas",'<center>')),
        th(HTML('<center>',"Ganancia",'<center>')),
        th(HTML('<center>',"Inventario",'<center>')),
        th(HTML('<center>',"Sucursales",'<center>')),
        th(HTML('<center>',"Facturación",'<center>')),
        th(HTML('<center>',"Clientes",'<center>')),
        th(HTML('<center>',"%",'<center>'))
      )
    )
  ))
  return(encabezado)
}

# Para hacer el encabezado de la pestaña de facturacion 1

f_encabez_fact1 <- function(fecha3){
  encabezado <- htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(rowspan = 2, colspan = 1, HTML('<center>',paste("Facturación: ",fecha3),'<center>')),
        th(colspan = 3, HTML('<center>Acumulada</center>'))
      ),
      tr(
        th(HTML('<center>',"Facturación",'<center>')),
        th(HTML('<center>',"Presupuesto",'<center>')),
        th(HTML('<center>',"% Cumplimiento",'<center>'))
      )
    )
  ))
  return(encabezado)
}

# Para hacer el encabezado de la pestaña de facturacion 2

f_encabez_PROD2 <- function(fecha2){
  encabeza2 <- htmltools::withTags(table(
    class = 'display', thead( tr(
      th(HTML('<center>',paste("Facturación",fecha2),'<center>')),
      th(HTML('<center>',paste("Facturación",FCorte$fec2[FCorte$fec==FCorte$fec[FCorte$fec2==fecha2]+1]),'<center>')),
      th(HTML('<center>',"Variación mes actual vs mes anterior",'<center>')),
      th(HTML('<center>',"Crecimiento % mes actual vs mes anterior",'<center>')),
      th(HTML('<center>',paste("Facturación",FCorte$fec2[FCorte$fec==FCorte$fec[FCorte$fec2==fecha2]+12]),'<center>')),
      th(HTML('<center>',"Variación mes actual vs mismo mes año anterior",'<center>')),
      th(HTML('<center>',"Crecimiento % mes actual vs mismo mes año anterior",'<center>')),
      th(HTML('<center>',"Producto",'<center>')),
      th(HTML('<center>',paste("Facturación acumulada a",fecha2),'<center>')),
      th(HTML('<center>',paste("Facturación acumulada a",FCorte$fec2[FCorte$fec==FCorte$fec[FCorte$fec2==fecha2]+12]),'<center>')),
      th(HTML('<center>',paste("Variación acumulada",fecha2,"vs",FCorte$fec2[FCorte$fec==FCorte$fec[FCorte$fec2==fecha2]+12]),'<center>')),
      th(HTML('<center>',paste("Crecimiento acumulado a",fecha2,"vs acumulada",FCorte$fec2[FCorte$fec==FCorte$fec[FCorte$fec2==fecha2]+12]),'<center>')),
      th(HTML('<center>',paste("Presupuesto",year(FCorte$fec1[FCorte$fec2==fecha2])),'<center>')),
      th(HTML('<center>',"% de Avance",'<center>')),
      th(HTML('<center>',paste("Numero de clientes a",fecha2),'<center>')),
      th(HTML('<center>',paste("Inventario de servicios a",fecha2),'<center>')),
      th(HTML('<center>',paste("Funnel de ventas a",fecha2),'<center>'))
      )
    )
  ))
  return(encabeza2)
}

# Función UI del módulo

fproductoUI <- function(id){ns <- NS(id)
  uiOutput(ns("Prod"))
}

# Función "Server" del módulo

fproducto <- function(input, output, session, usf, Ct_01, Co_01, DD_01, Ge_01, Vt_01, Ng_01, Fm_01, Pd_01, txt_01, clean, back){
  ns<-session$ns
  output$Prod<-renderUI({
    tagList(
      fluidRow(column(width = 7, h2(renderText("Reporte Avión"))),
        column(width = 5, prettyRadioButtons(ns("vista"),"Vista:",c("Canal"="mer","Producto"="prod","Vertical"="ver"), inline = TRUE,icon = icon("check"), status = "info"))
        #,
        #column(width = 2, style = "margin-top: 25px;", align="center", actionButton(ns("clean1"),"Limpiar filtros"))
      ),
      wellPanel( style = "background-color: #ffffff;", tabsetPanel( type = "tabs", tabPanel("Inventario", fluidRow(
        column(width = 5),
        column(width = 3, awesomeCheckbox(ns("Cliente"), "Detalle de Clientes", value = FALSE)),
        column(width = 2, selectInput(ns("base1"), "Canal", multiple = FALSE, choices = c("Empresarial","Pyme","CECOR","Todo"))),
        column(width = 2, selectInput(ns("FecMes"), "Mes de corte", multiple = FALSE, choices = FCorte$fec2[1:25]))
      ),
      div(style = 'overflow-x: scroll',h6(dataTableOutput(ns("salida_V1"))))
      ),
    tabPanel("Facturación 1", fluidRow(
      #column(width = 3, style = "margin-top: 25px;", align="center", prettySwitch(ns("base3"), "Incluir Pyme", status = "primary", fill=TRUE, bigger = TRUE)),
      column(width = 4, selectInput(ns("FecMes3"), "Mes de corte", multiple = FALSE, choices = FCorte$fec2[c(1:4,6)]))
    ),
    div(style = 'overflow-x: scroll',h6(dataTableOutput(ns("salida_V3"))))
    ),
    tabPanel("Facturación 2", 
      fluidRow(column(width = 4, selectInput(ns("FecMes2"), "Mes de corte", multiple = FALSE, choices = FCorte$fec2[c(2,3,4,6:8)])),
      column(width = 4, selectInput(ns("Merca2"), "Canal:", multiple = FALSE, choices = c("Total","1. Telecorp","2. División","3. Pyme")))         
    ),
    div(style = 'overflow-x: scroll',h6(dataTableOutput(ns("salida_V2"))))
    ))
  ))
})
  
# Actualiza "Clientes" a Falso para no mostrar detalles
  
  observeEvent(clean(),{updateMaterialSwitch(session, "Cliente", value = FALSE)})
  observeEvent(back(),{updateMaterialSwitch(session, "Cliente", value = FALSE)})
  
# Reacción al botón de -limpiar filtros-
    
  #boton1limpiar <<- eventReactive(input$clean1,return(input$clean1))

# Filtro: Mercado

  fTen<-reactive({
    #base<-merge(cTen_[!is.na(ganMe)],as.data.table(read_excel(paste0(dire1c,"insumos/CUC-E_sitios.xlsx"))),by ="CUC_E",all.x = TRUE)
    #base$Sitios<-as.numeric(base$Sitios)
    #base<-merge(base,Giro_Vt, by="Giro", all.x = TRUE)
    #base[is.na(Vertical)]$Vertical<-"OtrasV"
    #base[is.na(Giro)]$Giro<-"OtrasV"
    base<-cTen_[!is.na(Merca2)]
    base<-base[`Div / Sect 2`!="SIN ASIGNAR"]
    #Reasinga el mercado a casos casos inconsistentes
    #base[Mercado=="3. Pyme" & `Div / Sect 2` %in% c("FINANCIERO","SERVICIOS Y TURISMO","PUBLICO Y PARAESTATAL",
    #  "EMPRESAS GRUPO","GOBIERNO","INDUSTRIA Y SALUD")]$Mercado<-"1. Telecorp"
    #base[Merca2 %in% c("3. Pyme","4. CECOR") & `Div / Sect 2` %in% c("FINANCIERO","SERVICIOS Y TURISMO",
    #  "PUBLICO Y PARAESTATAL","EMPRESAS GRUPO","GOBIERNO","INDUSTRIA Y SALUD")]$Merca2<-"1. Telecorp"
    if(!is.null(input$base1)){
      if(input$base1=="Empresarial"){y<-c("1. Telecorp","2. División")}
      if(input$base1=="Pyme"){y<-c("3. Pyme","4. CECOR")}
      if(input$base1=="CECOR"){y<-"4. CECOR"}
      if(input$base1=="Todo"){y<-c("1. Telecorp","2. División","3. Pyme","4. CECOR")}
    }
    x<-base[Merca2 %in% y]
    return(x)
  })

# Filtro de Vista del usuario -A que DD tiene acceso y cuales son los casos validos de la tabla-
  
  TTen0 <- reactive({x<-fTen()
    x$Familia<-as.character(x$Familia)
    x$Negocio<-as.character(x$Negocio)
    x[is.na(x$Negocio)]$Negocio <- "7. Otros"
    x[is.na(x$`Div / Sect 2`)]$`Div / Sect 2` <- "SIN ASIGNAR"
    if(usf$DD != "TODO"){ x <- subset(x, `Div / Sect 2` %in% unlist(strsplit(usf$DD,"; ")) | Gerencia %in% unlist(strsplit(usf$DD,"; ")))}
    return(x)
  })
  
  vista.inventario <<- eventReactive(input$vista, return(input$vista))
  
  TTen1 <- reactive({
    x<-TTen0()
    
    if(!is.null(input$vista)){
      if(input$vista=="mer"){ifelse(!is.null(Ct_01()),{
        x$x0<-x$`Nombre de la cuenta`
        x$x1<-x$RAZON_SOCIAL
      },ifelse(!is.null(Ge_01()),{
        x$x0<-x$Gerencia
        x$x1<-x$`Nombre de la cuenta`
      },ifelse(!is.null(DD_01()),{
        x$x0<-x$`Div / Sect 2`
        x$x1<-x$Gerencia},{
          x$x0<-x$Mercado
          x$x1<-x$`Div / Sect 2`
        })))}
      if(input$vista=="prod"){ifelse(!is.null(Fm_01()),{
        x$x0<-x$Familia
        x$x1<-x$Nombre_corto_Producto
      },ifelse(!is.null(Ng_01()) && Ng_01()!="",{
        x$x0<-x$Negocio
        x$x1<-x$Familia},{
          x$x0<-"Total"
          x$x1<-x$Negocio
        }))}
      if(input$vista=="ver"){
        x$x0<-"Total"
        x$x1<-x$Vertical}
    }
    
    #Detalle de Clientes
    
    if(input$Cliente){x$x0<-"Total"
    x$x1<-x$`Nombre de la cuenta`}
    
    return(x)
  })
  

# Filtros de División/Sector sobre la base Tenencia de servicios

  TTen2 <- reactive({
    x <- TTen1()
    
    # Cuentas
    if( !is.null(Ct_01()) ) { x <- subset(x, x$`Nombre de la cuenta` %in% Ct_01() ) }
    
    # Filtros por División, Coordinación y Gerencia
    if( !is.null(Co_01()) || !is.null(DD_01()) || !is.null(Ge_01())){
      if( Co_01() != "" && !(Co_01() %in% c("Salud","Sin Emp.Gpo","Sin F.TMX")) ) { x <- subset(x, x$`Div / Sect 2` %in% as.character(subset(DivSec, DivSec$Coordinación %in% Co_01())$`División / Sector`) ) }
      if( Co_01() == "Salud" ) { x <- subset(x, x$Gerencia  %in% Gerencias_Salud  ) }
      if( Co_01() == "Sin Emp.Gpo" ) {x <- subset(x, !(x$Gerencia %in% Gerencias_EGrupo) )}
      if( Co_01() == "Sin F.TMX" ) {x <- subset(x, x$`Nombre de la cuenta` != filiales ) }
      
      if( !is.null(DD_01()) ) { x <- subset(x, x$`Div / Sect 2` %in% DD_01() ) }
      if( !is.null(Ge_01()) ) { x <- subset(x, x$Gerencia %in% Ge_01() ) }
    }
    # Filtro Vertical elegida
    if( !is.null(Vt_01()) ) { x <- subset(x, x$Giro %in% as.character( subset(Giro_Vt, Giro_Vt$Vertical %in% Vt_01())$Giro) ) }        
    
    # texto en campos
    #if( !is.null(txt_01()) ) { x <- x[grepl(txt_01(), x$`Nombre de la cuenta`, ignore.case = FALSE)]}
    
    return(x)
  })  

# Filtros de Producto sobre la base Tenencia de servicios

TTen3 <- reactive({
  x <- TTen2()
  if( !is.null(Ng_01()) || !is.null(Fm_01()) || !is.null(Pd_01())){
    if( Ng_01() != "" ) {x <- subset(x, x$Negocio %in% Ng_01())}
    if( !is.null(Fm_01())) {x <- subset(x, x$Familia %in% Fm_01())}
    if( !is.null(Pd_01())) {x <- subset(x, x$Nombre_corto_Producto %in% Pd_01())}
  }
  return(x)
})  

#Filtro: mes de corte
  
  TTen4 <-reactive({
    x <- TTen3()
    monAc <- FCorte$fec1[FCorte$fec2=="enero 2020"]
    #monAc <- FCorte$fec1[FCorte$fec2==format(Sys.Date()-months(2),"%B %Y")]
    mon <- FCorte$fec1[FCorte$fec2=={input$FecMes}]
    x$inv <- x[[as.character(monAc)]]
    if(mon!=monAc){
      x$inv <- x[[as.character(mon)]]
      if(as.character(year(mon))<"2019"){x$ganAn<-NA}
      else{x$ganAn <- x[[as.character(mon)]]-x[[paste0(year(mon)-1,"-12-01")]]}
      if(mon=="2018-01-01"){x$ganMe<-NA}
      else{x$ganMe <- x[[as.character(mon)]]-x[[as.character(mon-months(1))]]}
    }
    x$CUC_inv<-ifelse(x$inv>0,x$CUC,NA)
    x$CUC_E_inv<-ifelse(x$inv>0,x$CUC_E,NA)
    x[,c("bAn","aAn")]<-x$ganAn
    x[,c("bMe","aMe")]<-x$ganMe
    x$bAn[which(x$bAn>0)]<-0
    x$aAn[which(x$aAn<0)]<-0
    x$bMe[which(x$bMe>0)]<-0
    x$aMe[which(x$aMe<0)]<-0
    
    x$fac<-NA
    #if(mon<ymd("2020-01-01")){ifelse(mon<ymd("2019-01-01"),x$fac<-NA,x$fac<-x[[paste0("F",as.character(mon))]])}
    ifelse(mon<ymd("2019-01-01"),x$fac<-NA,x$fac<-x[[paste0("F",as.character(mon))]])
    
    return(x)
  })
  
  sub_agr <- reactive(unique(TTen4()$x0))

  #Funcion de la tabla a mostrar
  
  f_inv <- function(x){
    #Obtener los subtotales
    y<-rbind(
    #Subtotal según el primer filtro
    merge(
      subset(x[,.(aMe=sum(aMe, na.rm = TRUE), bMe=(-1)*sum(bMe, na.rm = TRUE), ganMe=sum(ganMe, na.rm = TRUE),
      aAn=sum(aAn, na.rm = TRUE), bAn=(-1)*sum(bAn, na.rm = TRUE), ganAn=sum(ganAn, na.rm = TRUE), inv=sum(inv, na.rm = TRUE), 
      fac=sum(fac, na.rm = TRUE), clte1=length(na.omit(unique(CUC_inv))), cltE1=length(na.omit(unique(CUC_E_inv))),
      x1=x0,llave=paste0(x0,"__")), by=.(x0)], select = -c(x0,x1)), 
      merge(TTen2()[uso=="I",.(clte0=length(na.omit(unique(CUC))), cltE0=length(na.omit(unique(CUC_E))), x1=x0,llave=paste0(x0,"__")),by=.(x0)], 
        subset(unique(x[,c("x0","x1","CUC","CUC_E","suc")])[,.(suc=sum(suc, na.rm = TRUE), llave=paste0(x0,"__")),by=.(x0)], select = -c(x0)),
        by="llave", all = TRUE),
      by="llave", all = TRUE),
    #Subtotal según el segundo filtro
    merge(subset(x[,.(aMe=sum(aMe, na.rm = TRUE), bMe=(-1)*sum(bMe, na.rm = TRUE), ganMe=sum(ganMe, na.rm = TRUE),
      aAn=sum(aAn, na.rm = TRUE), bAn=(-1)*sum(bAn, na.rm = TRUE), ganAn=sum(ganAn, na.rm = TRUE), inv=sum(inv, na.rm = TRUE), 
      fac=sum(fac, na.rm = TRUE), clte1=length(na.omit(unique(CUC_inv))), cltE1=length(na.omit(unique(CUC_E_inv))),
      llave=paste0(x0,"__",x1)), by=.(x0,x1)], select = -c(x0,x1)),
      merge(TTen2()[uso=="I",.(clte0=length(na.omit(unique(CUC))), cltE0=length(na.omit(unique(CUC_E))), llave=paste0(x0,"__",x1)),by=.(x0,x1)], 
        subset(unique(x[,c("x0","x1","CUC","CUC_E","suc")])[,.(suc=sum(suc, na.rm = TRUE), llave=paste0(x0,"__",x1)),by=.(x0,x1)], select = -c(x0,x1)),
        by="llave", all = TRUE),
      by="llave", all = TRUE),
    #Subtotal (total)
    y1<-merge(x[x1!="OtrasV",.(aMe=sum(aMe, na.rm = TRUE), bMe=(-1)*sum(bMe, na.rm = TRUE), ganMe=sum(ganMe, na.rm = TRUE),
      aAn=sum(aAn, na.rm = TRUE), bAn=(-1)*sum(bAn, na.rm = TRUE), ganAn=sum(ganAn, na.rm = TRUE), inv=sum(inv, na.rm = TRUE), 
      fac=sum(fac, na.rm = TRUE), clte1=length(na.omit(unique(CUC_inv))), cltE1=length(na.omit(unique(CUC_E_inv))),
      llave="zz__")],
      merge(TTen2()[uso=="I" & x1!="OtrasV",.(clte0=length(na.omit(unique(CUC))), cltE0=length(na.omit(unique(CUC_E))), x0="zz", x1="TOTAL", llave="zz__")],
        unique(x[x1!="OtrasV",c("CUC","CUC_E","suc")])[,.(suc=sum(suc, na.rm = TRUE), llave="zz__")],
        by="llave", all = TRUE),
      by="llave", all = TRUE)
    )
    y<-y[!is.na(x1)]
    y[is.na(inv)]$inv<-0
    #Cambiar el orden en que se muestra la tabla a partir del filtro de gerencia
    ifelse({!is.null(Ct_01())|!is.null(Ge_01())},y<-y[order(y$x0,-y$inv,-y$clte1,y$llave)],y<-y[order(llave)])
    #Para la vista de producto muestra solo los productos activos en los filtros seleccionados
    if(input$vista=="prod"){y<-y[!is.na(clte1)]
      ifelse(!is.null(Ct_01()), y$cltE0<-y1[,cltE0], y$clte0<-y1[,clte0])}
    #Columnas de clientes con inventario / clientes registrados
    ifelse(!is.null(Ct_01()), y$clte <-paste(formatC(ifelse(is.na(y$cltE1), 0, y$cltE1), big.mark = ","), "/", 
      formatC(y$cltE0,big.mark = ",")), 
      y$clte <-paste(formatC(ifelse(is.na(y$clte1), 0, y$clte1), big.mark = ","), "/", formatC(y$clte0, big.mark = ",")))
    ifelse(!is.null(Ct_01()), y$cltep<-y$cltE1/y$cltE0, y$cltep<-y$clte1/y$clte0)  
    #Columnas a mostrar
    y$bco<-NA
    y=y[, c('x0','aMe','bMe','ganMe','x1','aAn','bAn','ganAn','bco','inv','suc','fac','clte','cltep')]
    if(y[1,'x1']=="Total"){y<-y[-c(1),]} #Para quitar una linea innecesaria en la vista de producto
    if(is.null(Vt_01())){y<-y[x1!="OtrasV"]} #Para ocultar la fila de "Otras Verticales" en la vista de Verticales
    if(input$Cliente){y<-y[order(x0,-inv,x1)]} #Para ordenar la vista del detalle de clientes
    y[nrow(y),'x0']<-"TOTAL" #Para Etiquetar la ultima linea como TOTAL
    return(y)
  }
  
  Tab <- reactive(f_inv(TTen4()))
  
  #Tabla Salida
  
  output$salida_V1 <- renderDataTable(datatable(subset(Tab(),select = -c(x0)), rownames = FALSE, 
    extensions = c('KeyTable','Buttons'), container = f_encabez_PROD(fecha = input$FecMes), 
    options = list(pageLength = nrow(Tab()), dom = 'Bti', ordering = FALSE, autoWidth = FALSE, 
      buttons = c('excel','copy'), keys = TRUE, scrollX = TRUE, scrollY = TRUE,
      language = list(zeroRecords = "No hay información disponible para las opciones específicadas")              ))
    %>% formatRound(c('aMe','bMe','ganMe','aAn','bAn','ganAn','inv','suc'),0) 
    %>% formatCurrency('fac',3, currency = "$", digits = 0)
    %>% formatPercentage('cltep',digits = 1)
    %>% formatStyle(c(4,10,12), textAlign = "center")
    %>% formatStyle(4, backgroundColor = 'steelblue', color = 'white')
    %>% formatStyle(4, target = 'row', backgroundColor = styleEqual(c(sub_agr(),"TOTAL"),
          c(as.character(rep('gray',length(sub_agr()))),'steelblue')),
          color = styleEqual(c(sub_agr(),"TOTAL"),c(as.character(rep('white',length(sub_agr())+1)))))
    )
  
  #Para seleccionar dentro de la tabla
  
  dato_elegido <<- eventReactive(input$salida_V1_cell_clicked, {
    info <- input$salida_V1_cell_clicked
    return(Tab()$x1[info$row])
  })
  
  ## Pestaña de facturacion 1
  
  DivSec2 <- DivSec 
  DivSec2$`Div / Sect` <- DivSec2$`División / Sector` 
  DivSec2[`División / Sector` %in% c("NORTE 1", "NORTE 2")]$`Div / Sect` <-"NORTE"
  DivSec2[`División / Sector` %in% c("SUR 1", "SUR 2")]$`Div / Sect` <-"SUR"
  DivSec2[`División / Sector` %in% c("OCCIDENTE 1", "OCCIDENTE 2")]$`Div / Sect` <-"OCCIDENTE"
  
  #Filtro: Mes de corte
  
  B1fact<-reactive({
    x<-as.data.table(read_excel(paste0(dire1c,"insumos/AvionFact/Fact/base_202001.xlsx")))
    if(input$FecMes3=="agosto 2019"){x<-as.data.table(read_excel(paste0(dire1c,"insumos/AvionFact/Fact/base_201908.xlsx")))}
    if(input$FecMes3=="octubre 2019"){x<-as.data.table(read_excel(paste0(dire1c,"insumos/AvionFact/Fact/base_201910.xlsx")))}
    if(input$FecMes3=="noviembre 2019"){x<-as.data.table(read_excel(paste0(dire1c,"insumos/AvionFact/Fact/base_201911.xlsx")))}
    if(input$FecMes3=="diciembre 2019"){x<-as.data.table(read_excel(paste0(dire1c,"insumos/AvionFact/Fact/base_201912.xlsx")))}
    if(input$FecMes3=="enero 2020"){x<-as.data.table(read_excel(paste0(dire1c,"insumos/AvionFact/Fact/base_202001.xlsx")))}
    x$ID_PROD<-substr(x$ID,1,8)
    Tprod <- subset(as.data.table(read.csv(paste0(dire1c,"insumos/catalogo/Productos.csv"), fileEncoding="UTF-8-BOM")), select = -Familia.de.productos)
    Ten_prod <- as.data.table(read_excel(paste0(dire1c,"insumos/catalogo/Producto_Tenencia.xlsx"), col_types = "text"))
    Ten_prod <- subset(merge(Ten_prod,Tprod, by.x ="Nombre del producto", by.y ="Nombre.del.producto"), select = c("ID_PROD","Nombre del producto","Nombre_corto_Producto","Familia","Negocio"))
    x <- merge(x, Ten_prod, by ="ID_PROD", all.x = TRUE)
    x <- x[x$Negocio!="7. Otros"]
    x <- subset(x, select = c("ID_PROD","Mercado","Div / Sect","Facturacion","Presupuesto","Nombre del producto","Nombre_corto_Producto","Familia","Negocio"))
    return(x)
  })
  
# Filtros sobre la base de facturación (pestaña: facturación 1)
  
  B1fact1 <- reactive({
    x <- B1fact()
    # Filtros por División, Coordinación y Gerencia
    if( !is.null(Co_01()) || !is.null(DD_01()) || !is.null(Ge_01())){
      if( Co_01() != "" && !(Co_01() %in% c("Salud","Sin Emp.Gpo","Sin F.TMX")) ) { x <- subset(x, x$`Div / Sect` %in% as.character(subset(DivSec2, DivSec2$Coordinación %in% Co_01())$`Div / Sect`) ) }
      if( !is.null(DD_01()) ) { x <- subset(x, x$`Div / Sect` %in% DivSec2[`División / Sector` %in% DD_01()]$`Div / Sect`) }
    }
    # Productos
    if( !is.null(Ng_01()) || !is.null(Fm_01()) || !is.null(Pd_01())){
      if( Ng_01() != "" ) {x <- subset(x, x$Negocio %in% Ng_01())}
      if( !is.null(Fm_01())) {x <- subset(x, x$Familia %in% Fm_01())}
      if( !is.null(Pd_01())) {x <- subset(x, x$Nombre_corto_Producto %in% Pd_01())}
    }
    return(x)
  })
  
  B1fact2 <- reactive({
    x<-B1fact1()
    switch(input$vista,
      mer = {x$x0<-x$Mercado
        x$x1<-x$`Div / Sect`
      },
      prod = ifelse(!is.null(Fm_01()),{
        x$x0<-x$Familia
        x$x1<-x$Nombre_corto_Producto
      },ifelse(!is.null(Ng_01()) && Ng_01()!="",{
        x$x0<-x$Negocio
        x$x1<-x$Familia},{
        x$x0<-"Total"
        x$x1<-x$Negocio
      })),{
        x$x0<-x$Mercado
        x$x1<-x$`Div / Sect`
      })
    return(x)
  })
  
  #Funcion de la tabla a mostrar
  
  f_inv3 <- function(x){
    y<-rbind(
      x[,.(factu=sum(Facturacion),presu=sum(Presupuesto),x1=x0,llave=paste0(x0,"__")),by=.(x0)], 
      x[,.(factu=sum(Facturacion),presu=sum(Presupuesto),llave=paste0(x0,"__",x1)),by=.(x0,x1)],
      x[,.(x0="TOTAL",x1="TOTAL",factu=sum(Facturacion),presu=sum(Presupuesto),llave="zz__")]
    )[order(llave)]
    y$cumpli<-(y$factu/y$presu)
    y=y[,c('x0','x1','factu','presu','cumpli')]
    if(y[1,'x1']=="Total"){y<-y[-c(1),]}
    return(y)
  }
  
  Tab3 <- reactive(f_inv3(B1fact2()))
  sub_agr3 <- reactive(unique(Tab3()$x0))
  
  #Tabla Salida
    
  output$salida_V3 <- renderDataTable(datatable(subset(Tab3() ,select = -x0), rownames = FALSE, 
    extensions = c('KeyTable','Buttons'), container = f_encabez_fact1(fecha = input$FecMes3), 
    options = list(pageLength = nrow(Tab3()), dom = 'Bt', ordering = TRUE, autoWidth = FALSE, 
    buttons = c('excel','copy'), keys = TRUE, scrollX = TRUE, scrollY = TRUE))
    %>% formatCurrency(c('factu','presu'), 3, currency = "$", digits = 0)
    %>% formatPercentage('cumpli',digits = 1)
    %>% formatStyle(1, textAlign = "center")
    %>% formatStyle(1, backgroundColor = 'steelblue', color = 'white')
    %>% formatStyle(1, target = 'row', backgroundColor = styleEqual(c(sub_agr3()),c(as.character(rep('gray',length(sub_agr3())-1)),'steelblue')),
        color = styleEqual(c(sub_agr3()),c(as.character(rep('white',length(sub_agr3()))))))
  )
  
  #Para seleccionar dentro de la tabla
  
  dato_elegido3 <<- eventReactive(input$salida_V3_cell_clicked, {
    info <- input$salida_V3_cell_clicked
    return(Tab3()$x1[info$row])
  })

  ## Pestaña de facturacion 2
  
  #Filtro: Mes de corte
  
  Bfact<-reactive({
    x<-as.data.table(read_excel(paste0(dire1c,"insumos/AvionFact/AvionFact_1912.xlsx")))
    if(input$FecMes2=="diciembre 2019"){x<-as.data.table(read_excel(paste0(dire1c,"insumos/AvionFact/AvionFact_1912.xlsx")))}
    if(input$FecMes2=="noviembre 2019"){x<-as.data.table(read_excel(paste0(dire1c,"insumos/AvionFact/AvionFact_1911.xlsx")))}
    if(input$FecMes2=="octubre 2019"){x<-as.data.table(read_excel(paste0(dire1c,"insumos/AvionFact/AvionFact_1910.xlsx")))}
    if(input$FecMes2=="agosto 2019"){x<-as.data.table(read_excel(paste0(dire1c,"insumos/AvionFact/AvionFact_1908.xlsx")))}
    if(input$FecMes2=="julio 2019"){x<-as.data.table(read_excel(paste0(dire1c,"insumos/AvionFact/AvionFact_1907.xlsx")))}
    if(input$FecMes2=="junio 2019"){x<-as.data.table(read_excel(paste0(dire1c,"insumos/AvionFact/AvionFact_1906.xlsx")))}
    return(x)
  })
  
  #Filtro: Mercado
  
  Bfact2<-reactive({
    x<-Bfact()
    if(input$Merca2=="Total"){x<-x[x0=="Total"]}
    if(input$Merca2=="1. Telecorp"){x<-x[x0=="1. Telecorp"]}
    if(input$Merca2=="2. División"){x<-x[x0=="2. División"]}
    if(input$Merca2=="3. Pyme"){x<-x[x0=="3. Pyme"]}
    return(x)
  })
  
  output$salida_V2 <- renderDataTable(datatable(subset(Bfact2(), select = -x0), rownames = FALSE, 
    extensions = c('KeyTable','Buttons'), container = f_encabez_PROD2(fecha2 = input$FecMes2),
    options = list(pageLength = 50, dom = 'BT', ordering = TRUE, autoWidth = TRUE, 
      buttons = c('excel','copy'), keys = TRUE, scrollX = TRUE, scrollY = TRUE))
  %>% formatCurrency(c(1,2,3,5,6,9,10,11,13), 3, currency = "$", digits = 0)
  %>% formatRound(c(15:17),0)
  %>% formatStyle(8, textAlign = "center")
  %>% formatPercentage(c(4,7,12,14),0)
  %>% formatStyle(8, backgroundColor = 'steelblue', color = 'white')
  %>% formatStyle(8, target = 'row', backgroundColor = styleEqual(c("Total","1. Telecorp","2. División","3. Pyme"),c('steelblue','steelblue','steelblue','steelblue')),
                  color = styleEqual(c("Total","1. Telecorp","2. División","3. Pyme"),c('white','white','white','white')))
  )
    
  
    
  
  
  
}
  
