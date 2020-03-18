library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(readxl)
library(dtplyr)
library(data.table)
library(dplyr)
library(DT)



rm(list = ls())


#a <- "E:/Archivos_LARCOS/Aplicaciones/Tablero_Seguimiento/"
a <- "C:/Users/larcos/M_Empresarial/Analisis/Ventas/Aplicaciones/Tablero_Seguimiento/"
#a <- "C:/Users/vmorenoa/Documents/R proyectos/tablero_seguimiento/"
#a <- "C:/Users/vmorenoa/Documents/R proyectos/pruebas/"
b <- "/srv/shiny-server/tablero_seguimiento/"
#b <- "/srv/shiny-server/pruebas/"

direcc <- if_else(substr(getwd(),1,1) == "/", b , a)

Sys.setlocale(category = "LC_TIME" ,"es_MX.UTF-8")
Sys.setlocale(category = "LC_COLLATE" ,"es_MX.UTF-8")

#source(paste0(direcc,"insumos/Inicio.R"), encoding = "UTF-8")
#source(paste0(direcc,"CInicio.R"), encoding = "UTF-8")
source(paste0(direcc,"BajoDemanda_1.R"), encoding = "UTF-8")
source(paste0(direcc,"vigencia_1.R"), encoding = "UTF-8")
source(paste0(direcc,"Visitas_1.R"), encoding = "UTF-8")
source(paste0(direcc,"Csucursales_1.R"), encoding = "UTF-8")
source(paste0(direcc,"CCentroDatos_1.R"), encoding = "UTF-8")
source(paste0(direcc,"CForta2020_1.R"), encoding = "UTF-8")
source(paste0(direcc,"CGestionSAP_1.R"), encoding = "UTF-8")
source(paste0(direcc,"CProductos_1.R"), encoding = "UTF-8")
source(paste0(direcc,"MesaDeControl_1.R"), encoding = "UTF-8")
source(paste0(direcc,"Oportunidades_1.R"), encoding = "UTF-8")


dato_elegido <<- function(){}
dato_elegido3 <<- function(){}
vista.inventario <<- function(){}
Datos_MDC <<- function(){}
vista.opp <<- function(){}

################

shinyServer(function(input, output,session) {
  
  # Carga los datos actuales y lee los usuarios válidos
  source(paste0(direcc,"CInicio.R"), encoding = "UTF-8")
  usuarios <-  fread(paste0(direcc,"uso/","usuarios.csv"), sep = ",", encoding = "Latin-1")
  
  showModal(modalDialog( 
    textInput("usr1","Usuario"),
    passwordInput("pwd1","Contraseña"),
    title = "Bienvenido",
    footer = tagList(actionButton("ok", "Entrar"))
  ))
  
  dato <- eventReactive(input$ok,{return(as.character(paste0(input$usr1,"#$%",input$pwd1)))})
  
  observeEvent(dato(),{removeModal()
    validar <- as.character(paste0(usuarios$usuario,"#$%",usuarios$contraseña))
    if( !(dato() %in% validar) ){
      fwrite(data.frame(input$usr1,format(Sys.time(), "%d-%b-%Y %X"),0),file = paste0(direcc,"uso/log.txt"), append = TRUE)
      showModal(modalDialog( 
        textInput("usr1","Usuario"),
        passwordInput("pwd1","Contraseña"),
        title = "Bienvenido",
        footer = tagList( HTML('<p align="center"><font color = red>usuario y/o password incorrecto <br></font></p>'),
       #modalButton("Cancelar"),
          actionButton("ok", "Entrar"))
      ))
    } ##cierra el IF del modal
    else
    {usf <- subset(usuarios, usuario %in% input$usr1)
      fwrite(data.frame(input$usr1,format(Sys.time(), "%d-%b-%Y %X"),1),file = paste0(direcc,"uso/log.txt"), append = TRUE)
      #Validación de vigencia de la contrseña
      if( as.Date(usf$pwd_update, "%d/%m/%Y") <= Sys.Date() - Vig_Pwd){
        output$PM3 <- renderUI( menuItem("Contraseña Actualizada",tabName = "UsR", icon = icon("exclamation-triangle")) )
        callModule(vigencia, "Vig_01", direcc, usf$usuario)
        updateTabItems(session,"Menu","UsR")}
      output$usrFR <- renderText({ 
        paste0('<H1 align ="center">Tablero de Seguimiento Ventas Consultivas</H1>',
               '<br><H1 align ="center"><font color = blue>',usf$Nombre,'</font></H1>', 
               '<br><H1 align ="center">ha ingresado correctamente</H1>',
               '<br><H3 align = "center">Para ocultar/mostrar el menú de opciones presiona el ícono con esta imagen ',icon("bars"),'</H3>') 
      })
      
#### Carga de módulos      
      
      if(usf$Visitas == 1){
        callModule(fvisitas,"V_D1", usf, reactive(input$FCH_R), reactive(Div_Var()), reactive(Mer_ar()), reactive(input$Ct_01), reactive(input$Co_01), reactive(input$DD_01), reactive(input$Ge_01), reactive(input$Vt_01) )
      }
      
      if(usf$Sucursales == 1){
        callModule(fsucursal,"CSuc", direcc, reactive(Base_X1()[Campaña_Prod == "Sucursales"]),reactive(Base_X1()), reactive(Div_Var()), reactive(Mer_ar()))
      }
      
      if(usf$CDatos == 1){
        callModule(fcentrodatos,"CDat", direcc, reactive(Base_X1()[Campaña_Prod == "Atracción de Clientes Centro de Datos"]),reactive(Base_X1()), reactive(Div_Var()), reactive(Mer_ar()))
      }
      
      if(usf$Forta2020 == 1){
        callModule(ffortalecimiento,"CForta", direcc, reactive(Base_X1()[Campaña_Prod == "Fortalecimiento Conectividad 2020"]),reactive(Base_X1()), reactive(Div_Var()), reactive(Mer_ar()))
      }
      
      if(usf$SAP == 1){
        callModule(fGestionSAP,"CSAP", direcc, reactive(Base_X1()[Campaña_Prod == "Gestión de Aplicaciones SAP"]),
          reactive(Base_X1()), reactive(Div_Var()), reactive(Mer_ar()), reactive(input$Vt_01))
      }
      
      if(usf$IDE_Pendientes == 1){ 
        output$MDely <- renderUI( menuItem("Enlaces Pendientes", tabName = "Pi2", icon = icon("file")) )
        callModule(BajoDemanda, "B_D1", direcc)
      }
      
      if(usf$Productos == 1){
        output$MProd <- renderUI(menuItem("Inventario", tabName = "TabProd", icon = icon("plane")))
        callModule(fproducto,"CProd", usf, reactive(input$Ct_01), reactive(input$Co_01), reactive(input$DD_01), 
                   reactive(input$Ge_01), reactive(input$Vt_01), reactive(input$Ng_01), reactive(input$Fm_01), reactive(input$Pd_01),
                   reactive(input$txt_01), reactive(input$clean), reactive(input$back)
        )
      }
      
      callModule(MesaDeControl,"MMDC", usf, direcc, reactive(Base_P3()[!(Cerrado == 1 & Cierre < input$FCH_R[1])]) )
      
      
      if(usf$Opps == 1){
        callModule(fopps,"COpps", usf, reactive(Base_P3()), reactive(Base_02()), reactive(Base_Y2()), 
                   reactive(b_Ten()), reactive(M_ta()), reactive(input$FCH_R), reactive(input$Et_01), reactive(input$Ct_01), 
                   reactive(input$DD_01), reactive(input$Ge_01), reactive(input$Vt_01), reactive(input$Ng_01), 
                   reactive(input$Fm_01), reactive(input$Pd_01), reactive(input$clean), reactive(input$back)
        )
      } 
      
#### las variables principales para -Oportunidades- y la respectiva condición para que se muestre
      
      output$PM1 <- renderMenu(
            sidebarMenu(id="tab_menu",
              menuItem("Menú", icon = icon("swatchbook"), 
                menuItemOutput("MProd"),
                menuItemOutput("MDely"),
                menuItem("Oportunidades", tabName = "S_0", icon = icon("file-alt"))
                ))
      )
      
      output$PM2 <- renderMenu(    
                     menuItem("Filtros", icon = icon("filter"),        
                        menuItem("filtros General", uiOutput("filtros_0"), startExpanded = TRUE),
                        menuItem("filtros División", uiOutput("filtros_1"), startExpanded = TRUE),
                        menuItem("filtros Producto", uiOutput("filtros_2"), startExpanded = TRUE),
                        menuItem("filtros Postmortem", uiOutput("filtros_3"), startExpanded = TRUE),
                        menuItem("filtros Campañas", uiOutput("filtros_4"), startExpanded = TRUE),
                     startExpanded = TRUE)
      )
      

#### Filtra las OPPs de la DD que tiene acceso
      if(usf$DD != "TODO"){ Base_X0 <- subset(Base_X0, Base_X0$`Div / Sect 2` %in% unlist(strsplit(usf$DD,"; ")) | Base_X0$Gerencia %in% unlist(strsplit(usf$DD,"; "))  ) }

#### Crea el select para clientes en el sideMenu
      output$ParaMenu1 <- renderUI(selectInput("Ct_01", label = "Cliente", multiple = TRUE, choices = ctes ))
      
#### Crea las seccines de Filtros            
      output$filtros_0 <- renderUI({
        tagList(
          dateRangeInput("FCH_R", label = "Indica el Periodo", weekstart = 1, start = dia02, end = dia01, min = "2017-01-01", max = strftime(Sys.Date(),format = "%Y-%m-%d"), format = "dd/M/yy", separator = " al ", language = "es"),
          #selectInput("Ct_01", label = "Cliente", multiple = TRUE, choices = ctes ),
          selectInput("Vt_01", label = "Vertical", multiple = TRUE, choices = Vtcal),
          numericRangeInput("Imp_1", label = HTML("Total Proyecto: <font size=1>rango en $MDP</font>"), value = c(0,9999), separator = " - "),
          fluidRow(
            awesomeCheckbox("RoM_1", label = "Comprometida Roadmap", value = FALSE),
            awesomeCheckbox("A_FPR", label = "Filtrar Fecha Presentación", value = FALSE)
          ),
          uiOutput("B_FPR")
        )
      })
      
      observeEvent(input$A_FPR,{
        if(input$A_FPR){
          output$B_FPR <- renderUI(dateRangeInput("FPR_MC", label = NULL, weekstart = 1, format = "dd/M/yy", separator = " al ", language = "es", start =input$FCH_R[1], end =input$FCH_R[2] ))
        }
        else{ output$B_FPR <- renderUI("") }
      })

      output$filtros_1 <- renderUI({
        tagList(
          selectInput("Co_01", label = "Coordinación", multiple = FALSE, choices = c("",as.character(unique(DivSec$Coordinación)),"Salud","Sin Emp.Gpo","Sin F.TMX")),
          selectInput("DD_01", label = "División", multiple = TRUE, choices = c("",Divs)),
          selectInput("Ge_01", label = "Gerencia", multiple = TRUE, choices = c("",as.character(Div_Gerencia$Gerencia)))
        )
      })
      
      output$filtros_2 <- renderUI({
        tagList(
          selectInput("Ng_01", label = "Negocio", multiple = FALSE, choices = c("",Neri[order(Neri)]) ),
          selectInput("Fm_01", label = "Familia Producto", multiple = TRUE, choices = Fari[order(Fari)] ),
          selectInput("Pd_01", label = "Producto", multiple = TRUE, choices = unique(Fam_Neg$Nombre_corto_Producto, na.rm=FALSE)[order(unique(Fam_Neg$Nombre_corto_Producto, na.rm=FALSE))]),
          h5(awesomeRadio("PD_12", label = "", checkbox = TRUE, inline = TRUE, choices = c("Producto en la Opp","Producto Mayor Ponderación"), selected = "Producto en la Opp" ))
        ) 
      })
      
      output$filtros_3 <- renderUI({
        tagList(
          awesomeCheckboxGroup( inputId = "PM_C1", label = "Clasificación", inline = TRUE,
                                choices = c("Experiencia y Habilidades", "Gestión Comercial", "Precio"), selected = NULL ),
          selectInput("PM_G2", label = "Competidor", multiple = TRUE, choices = UComp )
        ) 
      })
      
      output$filtros_4 <- renderUI({
        tagList(
          selectInput("Cp_01", label = "Campaña", multiple = TRUE, choices = as.character(unique(a_campa$`Campaña Reporte`)) ),
          materialSwitch("Pro_01", label = "Mostrar Prospección", value = FALSE, inline = FALSE, status = "primary")
        ) 
      })

#### Crea el UI de salida principal de Oportunidades
  
  output$s_hoja_0 <- renderUI({
    tagList(fluidRow(htmlOutput("hilo_CP"),
      column(width = 11, wellPanel( style = "background-color: #ffffff;",tabsetPanel( id ="tab_OPPS", type = "tabs",
        if(usf$Opps == 1){tabPanel("Oportunidades",foppsUI("COpps"),icon = icon("file-signature"))},
        if(usf$Visitas == 1){tabPanel("Visitas",icon = icon("calendar-alt"),fvisitasUI("V_D1"))},
        tabPanel("Mesa de Control",MesaDeControlUI("MMDC"), icon = icon("clipboard-check") ),
        tabPanel("Casos Finanzas", icon = icon("file-invoice-dollar"),
                 fluidRow(column(width = 5, h6(awesomeRadio("sel_casos","Vista", inline = TRUE, checkbox = TRUE, choices = c("Dilación_del_Caso","Estado","Asignado_Finanzas"), selected = "Estado" ))),
                          column(width = 4, h6(paste("Reporte Finanzas al", strftime(f_reporte,"%d-%b-%Y") ))),
                          column(width = 3, h6(awesomeRadio("sta_casos","Casos a Mostrar", inline = TRUE, checkbox = TRUE, choices = c("Abiertos","Cerrados"), selected = "Abiertos" )))), 
                 div(style = 'overflow-x: scroll',h6(dataTableOutput("sale_finanzas")))),
        tabPanel("Campañas", icon = icon("list-alt"), navbarPage( "", #type = "tabs",
          tabPanel("Resumen", 
            fluidRow(column(width = 6 ),
            column(width = 6, h6(awesomeRadio("E_Camp_3","", inline = TRUE, checkbox = TRUE, choices = c("Cantidad","Servicios","Importe","Total de Proyecto"), selected = "Cantidad" )) )),
            h4(paste("Avance de Campañas" )),
            div(style = 'overflow-x: scroll',h6(dataTableOutput("sale_2campania")))),
          if(usf$CDatos == 1){tabPanel("Centros de Datos",fcentrodatosUI("CDat"))},
          if(usf$Sucursales == 1){tabPanel("Sucursales",fsucursalUI("CSuc"))},
          if(usf$SAP == 1){tabPanel("Gestion de Aplicaciones SAP",fGestionSAPUI("CSAP"))},
          if(usf$Forta2020 == 1){tabPanel("Fortalecimiento 2020",ffortalecimientoUI("CForta"))}
        ))
        ))),
      column(width = 1, h6(uiOutput("ft_02")))
    ),
    htmlOutput("hilo_Opp"))
  })
      
#### Crea los filtros que se muestran del lado derecho en Oportunidades       
      output$ft_02 <- renderUI({
        fluidRow(
          selectInput("Py_01", label = "Clasificación Proyecto", multiple = TRUE, choices = Proy),
          selectInput("Et_01", label = "Etapa", multiple = TRUE, choices = Etap),
          selectInput("Dl_01", label = "Dilación", multiple = TRUE, choices = c("< 7 Días","7 - 15 Días","Más de 15 Días")),
         #textInput("txt_01", label = "Buscar Palabra"),  
          selectInput("PS_V1", label = "Paso Siguiente", multiple = FALSE, choices = c("Todos","Vencido","Vigente"), selected = "Todos" ),
          selectInput("Ft_V1", label = "Fuente de Información", multiple = FALSE, choices = c("Empresarial","CECOR","TODO"), selected = "Empresarial" ),
          awesomeCheckbox("MC_01", label ="Filtrar OPP para Mesa de Control", value = FALSE)
         #actionButton("clean","limpiar filtros")
        )
      })
      
#### Reacción al botón de -limpiar filtros-
      
      limpiar<-function(){
        updateSelectInput(session, "Ct_01", selected = "" ) #Nombre de Cliente
        updateSelectInput(session, "Co_01", selected = "" ) #Coordinación
        updateSelectInput(session, "DD_01", selected = "" ) #División
        updateSelectInput(session, "Ge_01", selected = "" ) #Gerencia
        updateSelectInput(session, "Ng_01", selected = "" ) #Negocio de Producto
        updateSelectInput(session, "Fm_01", selected = "" ) #Familia de Producto
        updateSelectInput(session, "Pd_01", selected = "" ) #Nombre Producto
        updateSelectInput(session, "Et_01", selected = "" ) #Etapa 
        updateSelectInput(session, "Py_01", selected = "" ) #Tipo de Proyecto
        updateSelectInput(session, "Dl_01", selected = "" ) #Dilación
        updateSelectInput(session, "Vt_01", selected = "" ) #Verticales 
        updateNumericRangeInput(session,"Imp_1", label = HTML("Total Proyecto: <font size=1>rango en $MDP</font>"), value = c(0,9999)) #Total de Proyecto entre...
        updateAwesomeRadio(session,"PD_12", selected ="Producto en la Opp" ) #Producto -Mayor ponderación ó En la Opp-
        updateAwesomeRadio(session,"E_Camp_1", selected ="Cantidad" )
        updateAwesomeRadio(session,"E_Camp_2", selected ="Cantidad" )
        updateAwesomeRadio(session,"E_Camp_3", selected ="Cantidad" )
        updateAwesomeRadio(session,"E_Camp_4", selected ="Cantidad" )
        updateAwesomeCheckbox(session,"RoM_1", value = FALSE) #En RoadMap
        updateAwesomeCheckboxGroup(session,"PM_C1", selected = "") #Postmortem -Clasificación-
        updateSelectInput(session,"PM_G2", selected = "") #Postmortem -Competidor-
        updateTextInput(session,"txt_01", value = "") #Texto
        updateSelectInput(session, "Vt_01", selected = "" ) #Verticales 
        updateSelectInput(session, "PS_V1", selected = "Todos" ) #Paso Siguiente
        updateMaterialSwitch(session,"Pro_01", value = FALSE) #Mostrar Prospección
        updateSelectInput(session, "Ft_V1", selected = "Empresarial" ) #Fuente de Información
        updateAwesomeCheckbox(session, "MC_01", value = FALSE) # Mesa de Control
	      updateAwesomeCheckbox(session,"A_FPR", value = FALSE) #Regresa Fechas de Presentación
      }
      
      observeEvent(input$clean,limpiar())
      
### Hilo de Filtros Activos
      
      output$hilo_Opp <- renderText(paste("<b>Filtros activos:</b>  <br>",
        paste(" <b>Fuente de Información:</b> ", paste(input$Ft_V1,collapse = ", "),";"),
        if(!is.null(input$Vt_01)){paste(" <b>Vertical:</b> ", paste(input$Vt_01,collapse = ", "),";")},
        if(input$Imp_1[1]!= 0 || input$Imp_1[2]!= 9999)
          {paste(" <b>Total Proyecto:</b> ", paste0("Entre ", format(input$Imp_1[1], big.mark = ","), " y ", format(input$Imp_1[2], big.mark = ",")," MDP;"))},
        if(input$RoM_1){paste(" <b>Roadmap:</b> "," Si;")},
        if(input$Co_01 != ""){paste(" <b>Coordinación:</b> ", paste(input$Co_01,collapse = ", "),";")},
        if(!is.null(input$DD_01)){paste(" <b>División:</b> ", paste(input$DD_01,collapse = ", "),";")},
        if(!is.null(input$Ge_01)){paste(" <b>Gerencia:</b> ", paste(input$Ge_01,collapse = ", "),";")},
        if(!is.null(input$Ct_01)){paste(" <b>Cliente:</b> ", paste(input$Ct_01,collapse = ", "),";")},
        if(input$Ng_01 != ""){paste(" <b>Negocio:</b> ", paste(input$Ng_01,collapse = ", "),";")},
        if(!is.null(input$Fm_01)){paste(" <b>Familia:</b> ", paste(input$Fm_01,collapse = ", "),";")},
        if(!is.null(input$Pd_01)){paste(" <b>Producto:</b> ", paste(input$Pd_01,collapse = ", "),";")},
        if(!is.null(input$Cp_01)){paste(" <b>Campaña:</b> ", paste(input$Cp_01,collapse = ", "),";")},
        if(input$Pro_01){paste(" <b>Prospección:</b> "," Si;")},
        if(!is.null(input$Py_01)){paste(" <b>Proyecto:</b> ", paste(input$Py_01,collapse = ", "),";")},
        if(!is.null(input$Et_01)){paste(" <b>Etapa:</b> ", paste(input$Et_01,collapse = ", "),";")},
        if(!is.null(input$Dl_01)){paste(" <b>Dilación:</b> ", paste(input$Dl_01,collapse = ", "),";")},
        #if(input$PS_V1!="Todos"){paste(" <b>Paso Siguiente:</b> ", paste(input$PS_V1,collapse = ", "),";")},
        if(!is.null(input$MC_01)){if(input$MC_01){paste(" <b>Mesa de Control:</b> "," Si;")}}
      ))
      

####  Para hacer filtros al elegir un campo en la tabla -Click en un registro-
      
      actualiza_campos<-function(nvo.valor){
          if(length(nvo.valor)==0)return()
          
          if( nvo.valor %in% Divs ){ updateSelectInput(session, "DD_01", selected = nvo.valor ) }#División
          if( nvo.valor %in% Geri ){ updateSelectInput(session, "Ge_01", selected = nvo.valor ) }#Gerencia
          if( nvo.valor %in% ctes ){ updateSelectInput(session, "Ct_01", selected = nvo.valor ) }#Nombre de Cliente
          
          if( nvo.valor %in% as.character(unique(Tprod$Nombre_corto_Producto)) ){ updateSelectInput(session, "Pd_01", selected = nvo.valor ) }#Nombre Producto
          if( nvo.valor %in% Fari ){ updateSelectInput(session, "Fm_01", selected = nvo.valor ) }#Familia de Producto
          if( nvo.valor %in% Neri ){ updateSelectInput(session, "Ng_01", selected = nvo.valor ) }#Negocio de Producto
        
          if( nvo.valor %in% Vtcal){ updateSelectInput(session, "Vt_01", selected = nvo.valor ) }#Vertical
      }
      
      observeEvent( dato_elegido(), actualiza_campos( dato_elegido() ))
      observeEvent( dato_elegido3(), actualiza_campos( dato_elegido3() ))
      observeEvent( dato_elegido_opp(), actualiza_campos( dato_elegido_opp() ))

### Función para el botón 'Regresar'
      regresa_campos<-function( tab.actual ){
        
        if(length(tab.actual)==0)return()
        x <- TRUE
        if(tab.actual %in% c("Div / Sector","mer","Sec")){
          if( x && !is.null(input$Ct_01) ){ x <- FALSE; updateSelectInput(session, "Ct_01", selected = "" ) }#Nombre de Cliente
          if( x && !is.null(input$Ge_01) ){ x <- FALSE; updateSelectInput(session, "Ge_01", selected = "" ) }#Gerencia
          if( x && !is.null(input$DD_01) ){ x <- FALSE; updateSelectInput(session, "DD_01", selected = "" ) }#División
        }
        if(tab.actual %in% c("Producto Mayor Ponderación","prod","Prod1","Prod2")){
          if( x && !is.null(input$Pd_01) ){ x <- FALSE; updateSelectInput(session, "Pd_01", selected = "" ) }#Nombre Producto
          if( x && !is.null(input$Fm_01) ){ x <- FALSE; updateSelectInput(session, "Fm_01", selected = "" ) }#Familia de Producto
          if( x && !is.null(input$Ng_01) ){ x <- FALSE; updateSelectInput(session, "Ng_01", selected = "" ) }#Negocio de Producto
        }
        if(tab.actual %in% c("ver","Ver")){
          if( x && !is.null(input$Vt_01) ){ x <- FALSE; updateSelectInput(session, "Vt_01", selected = "" ) }#Nombre Vertical
        }
      }
      
      tab.activo <- reactive(
        if(input$tab_menu == "S_0"){ifelse(input$tab_OPPS=="Oportunidades",return(vista.opp()),return(input$tab_OPPS))}
        else {return(vista.inventario())}
      )  
      
      observeEvent( input$back, regresa_campos( tab.activo() ))
      
#### Para actualizar los -Select Input- al elegir algo
      observeEvent(input$Co_01,{
        if( input$Co_01 == "" ){
          updateSelectInput(session, "DD_01", choices = c("", Divs)  )
        }
        else
        {
          #updateSelectInput(session, "DD_01", choices = c("", as.character(subset(DivSec, Coordinación %in% input$Co_01)$División...Sector))  )
          u <- as.character( unique(Base_X1()$`Div / Sect 2`) )
          u <- u[order(u)]
          updateSelectInput(session, "DD_01", choices = c("", u )  )
        }   
      })
      
      observeEvent(input$Ng_01,{
        if( input$Ng_01 == "" ){
          updateSelectInput(session, "Fm_01", choices = Fari[order(Fari)]  )
          updateSelectInput(session, "Pd_01", choices = unique(Fam_Neg$Nombre_corto_Producto, na.rm=FALSE)[order(unique(Fam_Neg$Nombre_corto_Producto, na.rm=FALSE))])
        }
        else
        {
          updateSelectInput(session, "Fm_01", choices = unique(subset(Fam_Neg, Negocio %in% input$Ng_01 )$Familia)  )
          updateSelectInput(session, "Pd_01", choices = unique(subset(Fam_Neg, Negocio %in% input$Ng_01 )$Nombre_corto_Producto)  )
        }   
      })
      
      observeEvent(input$Fm_01,{
        if( !is.null(input$Fm_01) ){
          
          updateSelectInput(session, "Pd_01", choices = unique(subset(Fam_Neg, Familia %in% input$Fm_01 )$Nombre_corto_Producto)  )
        }
        else
        {
          
          updateSelectInput(session, "Pd_01", choices = unique(Fam_Neg$Nombre_corto_Producto)  )
        }  
      })
      
      observeEvent(input$DD_01,{
        if(is.null(input$DD_01)){
         updateSelectInput(session,"Ge_01", choices = c("",as.character(Div_Gerencia$Gerencia)) ) 
        }
        else{
          updateSelectInput(session,"Ge_01", choices = c("",as.character(subset(Div_Gerencia, Div_Gerencia$`Div / Sect 2` %in% input$DD_01)$Gerencia)) )
        }
      })
      
      observeEvent(input$PD_12,{
          if(input$PD_12 == "Producto Mayor Ponderación"){
            updateSelectInput(session,"Pd_01", label = "Producto Mayor Ponderación"  )
            }
          else{
            updateSelectInput(session,"Pd_01", label = "Producto en la Opp"  )
          }
      })
      
      observeEvent(input$E_Camp_1,{
        updateAwesomeRadio(session,"E_Camp_2", selected =input$E_Camp_1 )
        updateAwesomeRadio(session,"E_Camp_3", selected =input$E_Camp_1 )
        updateAwesomeRadio(session,"E_Camp_4", selected =input$E_Camp_1 )
      })
     
      observeEvent(input$E_Camp_2,{
        updateAwesomeRadio(session,"E_Camp_1", selected =input$E_Camp_2 )
        updateAwesomeRadio(session,"E_Camp_3", selected =input$E_Camp_2 )
        updateAwesomeRadio(session,"E_Camp_4", selected =input$E_Camp_2 )
      })
      
      observeEvent(input$E_Camp_3,{
        updateAwesomeRadio(session,"E_Camp_1", selected =input$E_Camp_3 )
        updateAwesomeRadio(session,"E_Camp_2", selected =input$E_Camp_3 )
        updateAwesomeRadio(session,"E_Camp_4", selected =input$E_Camp_3 )
      })
      
      observeEvent(input$E_Camp_4,{
        updateAwesomeRadio(session,"E_Camp_1", selected =input$E_Camp_4 )
        updateAwesomeRadio(session,"E_Camp_2", selected =input$E_Camp_4 )
        updateAwesomeRadio(session,"E_Camp_3", selected =input$E_Camp_4 )
      })
      
      vi2sta <- reactive(if(is.null(input$V_Camp_2))return(1)else return(grep(input$V_Camp_2, c("Sucesos del periodo","Proximos a cerrar"))))
      
      observeEvent(input$V_Camp_1,{
        updateAwesomeRadio(session,"V_Camp_2", selected =input$V_Camp_1 )
      })
      
      observeEvent(input$V_Camp_2,{
        updateAwesomeRadio(session,"V_Camp_1", selected =input$V_Camp_2 )
        if(vi2sta() == 2) updateAwesomeRadio(session,"E_Camp_2", selected = "Servicios" )
      })
      
      
#### Base con la que se va a trabajar y aplicación de filtros
      
      Base01 <- eventReactive(input$FCH_R ,{ return(f_cruza(Base_X0[Cerrado == 0 | !is.na(Campaña_Prod) | `Fecha de Temprana` >= input$FCH_R[1] | `Fecha de cierre` >= input$FCH_R[1] ], input$FCH_R, Tprod)) })
      
      Base_X1 <- reactive({
        x <- Base01()
        y <- Base_Y0
        ## Filtro de Mesa de Control
        if(!is.null(input$MC_01)){if(input$MC_01){x <- subset(x, FUENTE != "CECOR" & (`Clasificación del proyecto` == "A" | `¿Estratégica?` == 1 | `Importe Anual`>= 10000000) ) }}
        
	      #Filtro Fechas de Presentación/apertura
        if(input$A_FPR){ x <- subset(x, `Fecha de presentación/apertura`>= input$FPR_MC[1] & `Fecha de presentación/apertura`<= input$FPR_MC[2])}
        
        # Filtros generales
        if( !is.null(input$Ct_01) ) { x <- subset(x, x$`Nombre de la cuenta` %in% input$Ct_01 ) }
        if( !is.null(input$Py_01) ) { x <- subset(x, x$`Clasificación del proyecto` %in% input$Py_01 ) }
        
        # Filtra las etapas
        if( !is.null(input$Et_01) ){
          if( "Nueva" %in% input$Et_01 ){ x <- subset(x, x$EtapF %in% input$Et_01 | x$Nueva == 1) }
          else{ x <- subset(x, x$EtapF %in% input$Et_01 ) }
        }
        
        # Filtros por División, Coordinación y Gerencia
        if( !is.null(input$Co_01) || !is.null(input$DD_01) || !is.null(input$Ge_01)){
            if( input$Co_01 != "" && !(input$Co_01 %in% c("Salud","Sin Emp.Gpo","Sin F.TMX")) ) { x <- subset(x, x$`Div / Sect 2` %in% as.character(subset(DivSec, Coordinación %in% input$Co_01)$`División / Sector`) ) }
            if( input$Co_01 == "Salud" ) { x <- subset(x, x$Gerencia  %in% Gerencias_Salud  ) }
            if( input$Co_01 == "Sin Emp.Gpo" ) {x <- subset(x, !(x$Gerencia %in% Gerencias_EGrupo) )}
            if( input$Co_01 == "Sin F.TMX" ) {x <- subset(x, x$`Nombre de la cuenta` != filiales ) }
              
            if( !is.null(input$DD_01) ) { x <- subset(x, x$`Div / Sect 2` %in% input$DD_01 ) }
            if( !is.null(input$Ge_01) ) { x <- subset(x, x$Gerencia %in% input$Ge_01 ) }
        }
        # Filtro Vertical elegida
        if( !is.null(input$Vt_01) ) { x <- subset(x, x$Giro %in% as.character( subset(Giro_Vt, Giro_Vt$Vertical %in% input$Vt_01)$Giro) ) }
        
        # Importe de Total del Proyecto
        x <- subset(x, x$`Total de Proyecto` >= input$Imp_1[1]*1000000 & x$`Total de Proyecto` <= input$Imp_1[2]*1000000)
        
        # Filtros Postmortem
        if( !is.null(input$PM_C1) ) { x <- subset(x, x$Clasificación_EAE %in% input$PM_C1 & x$Cerrado == 1) }
        if( !is.null(input$PM_G2) ) { x <- subset(x, x$`Competidor ganador` %in% input$PM_G2 & x$Cerrado == 1) }
        
        # texto en campos
        if( !is.null(input$txt_01) ) { x <- x[grepl(input$txt_01, x$`Identificador de la oportunidad`, ignore.case = TRUE) | grepl(input$txt_01, x$Descripción, ignore.case = TRUE) | grepl(input$txt_01, x$`Nombre de la oportunidad`, ignore.case = TRUE) | grepl(input$txt_01, x$Productos_OPP, ignore.case = TRUE) | grepl(input$txt_01, x$`Nombre de la cuenta`, ignore.case = TRUE) | grepl(input$txt_01, x$`Paso siguiente`, ignore.case = TRUE)] }
        
        # Comprometida para Roadmap -RoM_1-
        if( input$RoM_1 ) { x <- subset(x, x$`Comprometida para Roadmap` == 1)}
        
        # Considera Producto de la Opp o Producto Mayor Ponderación
        if( !is.null(input$Ng_01) || !is.null(input$Fm_01) || !is.null(input$Pd_01) || !is.null(input$PD_12) ){
            if(input$PD_12 == "Producto Mayor Ponderación"){
              if( input$Ng_01 != "" ) { x <- subset(x, x$Negocio %in% input$Ng_01 ) }
              if( !is.null(input$Fm_01) ) { x <- subset(x, x$Familia %in% input$Fm_01 ) }
              if( !is.null(input$Pd_01) ) { x <- subset(x, x$Nombre_corto_Producto %in% input$Pd_01 ) }
            }
            
            y <- subset(y, y$`Identificador de la oportunidad` %in% as.character(x$`Identificador de la oportunidad`) )
            
            if( input$Ng_01 != "" ) { y <- subset(y, y$Negocio %in% input$Ng_01 ) }
            if( !is.null(input$Fm_01) ) { y <- subset(y, y$Familia %in% input$Fm_01 ) }
            if( !is.null(input$Pd_01) ) { y <- subset(y, y$Nombre_corto_Producto %in% input$Pd_01 ) }
            
            if(input$PD_12 == "Producto en la Opp"){  
              x <- subset(x, x$`Identificador de la oportunidad` %in% as.character(y$`Identificador de la oportunidad`) )
            }
        }
        
        #Filtra Campañas elgidas
        if( !is.null(input$Cp_01) ){x <- subset(x, x$Campaña_Prod %in% input$Cp_01 ) }
        
        #Incorpora Servicios e Importe
        w <- y[, .(Serv.1=sum(Cantidad, na.rm = TRUE), Impo.1=sum(`Precio total`, na.rm = TRUE)) , by = `Identificador de la oportunidad`] 
        x <- merge(x, w, by = "Identificador de la oportunidad", all.x = TRUE )
        
        ### Filtro de Paso Siguiente
        if( !is.null(input$PS_V1) ){
          if(input$PS_V1 != "Todos"){x <- subset(x, x$Estatus_paso_siguiente == input$PS_V1)}
        }
        
        # Filtro de Fuente de Información
        if( !is.null(input$Ft_V1) ){if(input$Ft_V1 != "TODO"){x <- subset(x, x$FUENTE == input$Ft_V1)} }
        
        return(x)
        
      })
      
      # Filtros sobre la base Tenencia de servicios
      b_Ten <- reactive({
        x <- cTen_[!is.na(Mercado)]
        au <- names(Fam_Var())
        x$llaveP <- paste(x[[au[1]]],x[[au[2]]])
        # Cuentas
        if( !is.null(input$Ct_01) ) { x <- subset(x, x$`Nombre de la cuenta` %in% input$Ct_01 ) }
        # Filtros por División, Coordinación y Gerencia
        if( !is.null(input$Co_01) || !is.null(input$DD_01) || !is.null(input$Ge_01)){
          if( input$Co_01 != "" && !(input$Co_01 %in% c("Salud","Sin Emp.Gpo","Sin F.TMX")) ) { x <- subset(x, x$`Div / Sect 2` %in% as.character(subset(DivSec, Coordinación %in% input$Co_01)$`División / Sector`) ) }
          if( input$Co_01 == "Salud" ) { x <- subset(x, x$Gerencia  %in% Gerencias_Salud  ) }
          if( input$Co_01 == "Sin Emp.Gpo" ) {x <- subset(x, !(x$Gerencia %in% Gerencias_EGrupo) )}
          if( input$Co_01 == "Sin F.TMX" ) {x <- subset(x, x$`Nombre de la cuenta` != filiales ) }
          
          if( !is.null(input$DD_01) ) { x <- subset(x, x$`Div / Sect 2` %in% input$DD_01 ) }
          if( !is.null(input$Ge_01) ) { x <- subset(x, x$Gerencia %in% input$Ge_01 ) }
        }
        # Filtro Vertical elegida
        if( !is.null(input$Vt_01) ) { x <- subset(x, x$Giro %in% as.character( subset(Giro_Vt, Giro_Vt$Vertical %in% input$Vt_01)$Giro) ) }
        
        # Productos
        if( !is.null(input$Ng_01) || !is.null(input$Fm_01) || !is.null(input$Pd_01) ){
          if( input$Ng_01 != "" ) { x <- subset(x, x$Negocio %in% input$Ng_01 ) }
          if( !is.null(input$Fm_01) ) { x <- subset(x, x$Familia %in% input$Fm_01 ) }
          if( !is.null(input$Pd_01) ) { x <- subset(x, x$Nombre_corto_Producto %in% input$Pd_01 ) }
        }
        return(x)
      })
      
    # M_ta <- reactive({
    #   x <- Meta19
    #   # Cuentas
    #   # if( !is.null(input$Ct_01) ) { x <- subset(x, x$`Nombre de la cuenta` %in% input$Ct_01 ) }
    #   # Filtros por División, Coordinación y Gerencia
    #   if( !is.null(input$Co_01) || !is.null(input$DD_01) || !is.null(input$Ge_01)){
    #     if( input$Co_01 != "" && !(input$Co_01 %in% c("Salud","Sin Emp.Gpo","Sin F.TMX")) ) { x <- subset(x, x$`Div / Sect 2` %in% as.character(subset(DivSec, Coordinación %in% input$Co_01)$`División / Sector`) ) }
    #     if( input$Co_01 == "Salud" ) { x <- subset(x, x$Gerencia  %in% Gerencias_Salud  ) }
    #     if( input$Co_01 == "Sin Emp.Gpo" ) {x <- subset(x, !(x$Gerencia %in% Gerencias_EGrupo) )}
    #     #if( input$Co_01 == "Sin F.TMX" ) {x <- subset(x, x$`Nombre de la cuenta` != filiales ) }
    #     
    #     if( !is.null(input$DD_01) ) { x <- subset(x, x$`Div / Sect 2` %in% input$DD_01 ) }
    #     if( !is.null(input$Ge_01) ) { x <- subset(x, x$Gerencia %in% input$Ge_01 ) }
    #   }
    #   # Filtro Vertical elegida
    #   #if( !is.null(input$Vt_01) ) { x <- subset(x, x$Giro %in% as.character( subset(Giro_Vt, Giro_Vt$Vertical %in% input$Vt_01)$Giro) ) }
    #   
    #   # Productos
    #   if( !is.null(input$Ng_01) || !is.null(input$Fm_01) || !is.null(input$Pd_01) ){
    #     if( input$Ng_01 != "" ) { x <- subset(x, x$Negocio %in% input$Ng_01 ) }
    #     if( !is.null(input$Fm_01) ) { x <- subset(x, x$Familia %in% input$Fm_01 ) }
    #     if( !is.null(input$Pd_01) ) { x <- subset(x, x$Nombre_corto_Producto %in% input$Pd_01 ) }
    #   }
    #   return(x)
    # })
    # 
    ### Para la tabla por División / Sector
      
      Div_Var <- reactive({ x <- MeDi
                            if(!is.null(input$Ge_01) || !is.null(input$DD_01) ){
                              if(!is.null(input$DD_01)  ){ x <- Div_Gerencia }
                              if(!is.null(input$Ge_01) ){ x <- ejec_gcia }
                            }
                            return(x)  
        })
      
      Mer_ar <- reactive({  x <- Meri
                            if(!is.null(input$Ge_01) || !is.null(input$DD_01) ){
                              if(!is.null(input$DD_01)  ){ x <- Divs }
                              if(!is.null(input$Ge_01) ){ x <- Geri }
                            }
                            return(x) 
        })
       
      Fam_Var <- reactive( if( (input$Ng_01 == ""  || is.null(input$Ng_01)) && is.null(input$Fm_01) && is.null(input$Pd_01) ){unique(Fam_Neg[,2:3])}else{unique(Fam_Neg[,1:2])}  )
      Fari_ar <- reactive( if( (input$Ng_01 == ""  || is.null(input$Ng_01)) && is.null(input$Fm_01) && is.null(input$Pd_01) ){Neri}else{Fari}  )
      
      
    Base_P2 <- eventReactive(Base_X1(),{ return(f_campos(Base_X1(), total_campos, input$Et_01, Fam_Var(), Div_Var())) })
      
    ### Filtro de dilación       
      Base_P3 <- reactive({
        x <- Base_P2()
        if( !is.null(input$Dl_01) ){ x <- subset(x, x$bandera %in% input$Dl_01 ) }
        x <- x[order(-`Total de Proyecto`, -`Importe Anual`)]
        return(x)
      })
      
    ### Quita oportunidades en Prospectación y las que no se usan
      Base_02 <- reactive({ 
        x <- Base_P3()
        if((is.null(input$Pro_01) || !input$Pro_01) && !("Prospección" %in% input$Et_01)){ x <- subset(x, Etap_Camp != "Prospección")}
        x <- subset(x, EtapF != " " | inicio == "Inicial" | f1nal == "Final" | Nueva == 1)
        x <- x[order(-`Total de Proyecto`, -`Importe Anual`)]
        return(x)
      })
      
      Base_Y2 <- eventReactive(Base_02(),{
        y <- Base_Y0
        campo_by <- names(Fam_Var())[1]
        agrupador <- names(Fam_Var())[2]
        names(y) <- c("Nombre del producto","ID","Familia de productos","Importe","Servicios","Nombre_corto_Producto","Familia","Negocio")
        
        # Considera Producto de la Opp 
        if( !is.null(input$Ng_01) || !is.null(input$Fm_01) || !is.null(input$Pd_01) ){
            if( input$Ng_01 != "" ) { y <- subset(y, y$Negocio %in% input$Ng_01 ) }
            if( !is.null(input$Fm_01) ) { y <- subset(y, y$Familia %in% input$Fm_01 ) }
            if( !is.null(input$Pd_01) ) { y <- subset(y, y$Nombre_corto_Producto %in% input$Pd_01 ) }
        }
        
        x <- subset(Base_02(), select = c("ID","inicio","Nueva","f1nal","EtapF","Etap_Camp","Fecha de Proyecto","Fecha de Propuesta","Cierre"))
        w <- merge(y, x , by = "ID")
        w$llave <- paste(w[[campo_by]],w[[agrupador]])

        return(w)
      })
      
      output$hilo_CP <- renderText(paste("<center> <b>Proy Tipo A:</b> ", format(nrow(Base_02()[Clasificación == "A" & f1nal == "Final"]), big.mark = ","),"Opps", paste0("$",format(sum(Base_02()[Clasificación == "A" & f1nal == "Final"]$`Importe Anual`, na.rm = TRUE), big.mark = ",")),
                                              " // ","<b>Proy Tipo B:</b> ", format(nrow(Base_02()[Clasificación == "B" & f1nal == "Final"]), big.mark = ","),"Opps", paste0("$",format(sum(Base_02()[Clasificación == "B" & f1nal == "Final"]$`Importe Anual`, na.rm = TRUE), big.mark = ",")),
                                              " // ","<b>Proy Tipo C:</b> ", format(nrow(Base_02()[Clasificación == "C" & f1nal == "Final"]), big.mark = ","),"Opps", paste0("$",format(sum(Base_02()[Clasificación == "C" & f1nal == "Final"]$`Importe Anual`, na.rm = TRUE), big.mark = ",")),
                                          "</center>"    ))
      
      moneda <- eventReactive(input$E_Camp_3,{ifelse(input$E_Camp_3 %in% c("Importe","Total de Proyecto"),"$","")})
      
      porcentaje <- eventReactive(vi2sta(),{ifelse(vi2sta() == 1," ","Avance")})
      

#### Tablas de Casos Finanzas
      
      Tabla_41 <- reactive( f_3_Casos(Base_02(), Div_Var(), input$sel_casos, input$sta_casos, input$FCH_R, input$E_Camp_3) ) 
      
      
      output$sale_finanzas <- renderDataTable( datatable(subset( Tabla_41(), select = -llave ), rownames = FALSE,  selection = list(target = 'cell', mode = 'single'), extensions = c('Buttons'),
                                                     options = list(pageLength = nrow(Tabla_41()), dom = 'Bt', ordering = FALSE, buttons = c('excel','copy') )) %>% formatStyle(c(3:5), cursor = 'pointer') %>%
                                             formatCurrency(3:ncol(Tabla_41()), currency= moneda(), digits=0) %>% formatStyle(2, textAlign = "right") %>% formatStyle(1, textAlign = "left") %>%
                                             formatStyle("agrupador", target = "row",
                                                         background = styleEqual(c(Mer_ar(),'TOTAL'),c(as.character(rep('gray',length(Mer_ar()))),'steelblue')), 
                                                         color = styleEqual(c(Mer_ar(),'TOTAL'),c(as.character(rep('white',length(Mer_ar()))),'white') ))
      )
      
      ###### Para seleccionar debtro de la tabla de Casos Finanzas
      observeEvent(input$sale_finanzas_cell_clicked, {
        info = input$sale_finanzas_cell_clicked
        a1 <- names(Tabla_41())[info$col+2]
        b1 <- as.character(Tabla_41()$llave[info$row])
        
        
        if (is.null(info$value) || info$col %in% c(0:1) ) return()
        
        if(input$sta_casos == "Abiertos"){
          k <- Base_02()[ Caso_Abierto == 1 ]
        }
        else{
          k <- Base_02()[ Caso_Abierto == 0 ]
          k <- k[is.na(k$`Fecha Cierre del Caso`) | (as.Date(k$`Fecha Cierre del Caso`) >= input$FCH_R[1] & as.Date(k$`Fecha Cierre del Caso`) <= input$FCH_R[2])]
        }
        
        
        if(b1 == "TOTAL"){
          b1 <- "TOTAL"
        }
        else{
          k <- subset( k, llaveD == b1 | `División / Sector` == b1 | Mercado == b1 )
          b1 <- paste("de", as.character(Tabla_41()$`Div / Sect 2`[info$row]) )
        }
        
        
        ifelse(a1 == "TOTAL", OPPS_selec <- k, OPPS_selec <- subset(k, k[[input$sel_casos]] == a1 ) )
        
        
        campos_mostrar <- campos_en_casos
        fecha_campos <- c("Fecha de apertura","Fecha de Proyecto","Creación","Cierre","Fecha Cierre del Caso") 
        fecha_rep <- c(rep('toLocaleDateString',5))
        targ200 <- c(11)
        
        showModal(modalDialog( 
          title = paste("Detalle de",info$value,"Casos '",a1,"'",b1),
          h6(dataTableOutput("OPPs_Tabla")),
          footer = modalButton("Salir"),
          size = "l",
          easyClose = TRUE
        ))
        
        
        output$OPPs_Tabla <- renderDataTable( datatable( subset(OPPS_selec, select = campos_mostrar), rownames = FALSE, extensions = c('FixedColumns','KeyTable'),
                                                         options = list(pageLength = 5, dom = 'plti', ordering = TRUE, autoWidth = TRUE,
                                                                        keys = TRUE, scrollX = TRUE, scrollY = TRUE, fixedColumns = list(leftColumns = 2),
                                                                        columnDefs = list(list(width = '200px', targets = targ200 )) )) %>%
                                                formatCurrency(c("Total de Proyecto","Precio Objetivo"), digits = 0) %>% formatDate(fecha_campos,fecha_rep)
        )
        
      })
######## Termina para seleccionar en la tabla de Finanzas
      

## Tabla de Campañas
      Tabla_52 <- reactive( f_campania(Base_P3(), campa_mostrar = a_campa, campo_suma = input$E_Camp_3, Tipo = "R") )
      
      
      output$sale_2campania <- renderDataTable( datatable(Tabla_52(), rownames = FALSE,  selection = list(target = 'cell', mode = 'single'),
                                                         options = list(pageLength = nrow(Tabla_52()), dom = 't', ordering = FALSE)) %>% formatStyle(c(2:7), cursor = 'pointer') %>%
                                                 formatCurrency(2:8, currency = moneda(), digits=0) %>% formatStyle(1, textAlign = "left") %>% formatPercentage(c("Avance","Eficacia"), digits = 1) %>%
                                                 formatStyle("Clientes Prospectos", background = 'steelblue', color = 'white' ) %>%
                                                 formatStyle("Campaña", target = "row", background = styleEqual('TOTAL','steelblue'), 
                                                             color = styleEqual('TOTAL','white') ) 
      )
      ###### Para seleccionar debtro de la tabla de Campaña
      observeEvent(input$sale_2campania_cell_clicked, {
        info = input$sale_2campania_cell_clicked
        a1 <- names(Tabla_52())[info$col+1]
        b1 <- as.character(Tabla_52()$Campaña[info$row])
        
        
        if (is.null(info$value) || info$col %in% c(0,11) ) return()
        
        k <- merge(Base_P3(), a_campa, by.x = "Campaña", by.y = "Campaña_Sactel", all.x = TRUE)
        
        
        if(b1 == "TOTAL"){
          b1 <- "TOTAL"
        }
        else{
          k <- subset( k, Campaña_Prod == b1 )
          b1 <- paste("de", b1 )
        }
        
        
        
        ifelse(a1 == "Clientes Prospectos", OPPS_selec <- k, OPPS_selec <- subset(k, k$Etap_Camp == a1 ) )
        #if(a1 == "Cargados en Campaña"){ OPPS_selec <- subset(k, !is.na(`Campaña Reporte`) ) }
        if(a1 == "Pendiente Visitar"){ OPPS_selec <- subset(k, k$Etap_Camp == "Prospección" ) }
        if(a1 == "En Proceso"){ OPPS_selec <- subset(k, k$Etap_Camp %in% c("Temprana","Proyecto","Propuesta") ) }
        #if(a1 == "Total Cerradas"){ OPPS_selec <- subset(k, k$Etapa %in% c("Ganada","Perdida") ) }
        
        campos_mostrar <- campos_incluye_T.Proy
        fecha_campos <- c("Fecha paso siguiente","Creación","Cierre") 
        fecha_rep <- c(rep('toLocaleDateString',3))
        targ200 <- c(3,5,10,24)
        
        showModal(modalDialog( 
          title = paste("Detalle de",info$value," Oportunidades '",a1,"'",b1),
          h6(dataTableOutput("OPPs_Tabla")),
          footer = modalButton("Salir"),
          size = "l",
          easyClose = TRUE
        ))
        
        
        output$OPPs_Tabla <- renderDataTable( datatable( subset(OPPS_selec, select = campos_mostrar), rownames = FALSE, extensions = c('FixedColumns','KeyTable','Buttons'),
                                                         options = list(pageLength = 5,lengthMenu = c(5,20,100,500), dom = 'Bplti', ordering = TRUE, autoWidth = TRUE, buttons = c('excel','copy'),
                                                                        keys = TRUE, scrollX = TRUE, scrollY = TRUE, fixedColumns = list(leftColumns = 2),
                                                                        columnDefs = list(list(width = '200px', targets = targ200 )) )) %>%
                                                formatCurrency(c("Total de Proyecto","Importe Anual", "Importe"), digits = 0) %>% formatDate(fecha_campos,fecha_rep)
        )
        
      })
######## Termina para seleccionar en la tabla Campañas    


    }# cierra el else de acceso - cuando si entran a la aplicacción-
    
  }) # Observe Event

})### Cierra Server
