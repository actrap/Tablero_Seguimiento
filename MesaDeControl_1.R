library(shinydashboard)
library(shinyWidgets)
library(dtplyr)
library(dplyr)
library(data.table)
library(DT)
library(readxl)
library(tidyr)

### Campos que se muestran en el listado de de OPP´s
camposG_MDC <- c('ID','Cliente',
                 'Nombre de la oportunidad',
                 'División / Sector', 
                 'GerenteMC',
                 'Tipo de concurso',
                 'Etapa',
                 'EjecutivoMC'
)
### Campos mostrados en el Detalle
campos1_MDC <- c('Tipo de concurso',
                 'División / Sector',
                 'Descripción de la Oportunidad',
                 'Paso siguiente', 
                 'Fecha paso siguiente',
                 'Productos en OPP',
                 'Creación', 
                 'Cierre',
                 'Etapa', 
                 'Tipo',
                 'Clasificación',
                 'Publicación convocatoria',
                 'Juntas de aclaraciones',
                 'Presentación/apertura',
                 'Fecha de fallo',
                 'Importe Anual', 'Total de Proyecto'
)
###Campos mostrados para Kanban
campKanban <- c('ID','División / Sector',
                'Cliente',
                'Tipo de concurso',
                'Nombre de la oportunidad',
                'Descripción de la Oportunidad',
                'Comentarios', 
                'Meses',
                'Total de Proyecto',
                'Publicación convocatoria',
                'Creación', 
                'Juntas de aclaraciones',
                'Presentación/apertura',
                'Fecha de fallo'
)

Et0 <- "Etapa MC"
Et1 <- "Proveedor Actual"
Et2 <- "Competidor"
Et3 <- "Estatus Técnico"
Et4 <- "Estatus Administrativo"
Et5 <- "Estatus Jurídico"
Et6 <- "Estatus Compras"
Et7 <- "Estatus Delivery"

eta_validos <- c("En Proceso","En espera de decisión","Regresa a Cambios","Cancelada", "Declinada", "Descalificada", "Desierta","Ganada","Perdida")

MesaDeControlUI <- function(id){
  ns <- NS(id)
  uiOutput(ns("kps"))
  
}

MesaDeControl <- function(input, output, session, usf, direcc, baseOPP){
  ns <- session$ns
  
  MCompleto <- fread(paste0(direcc,"insumos/MDControl/comentaMDC.csv"), sep = ",", colClasses = c(rep("character",15)))
  MCompleto$fecha <- as.Date(MCompleto$fecha)
  MCompleto <- MCompleto[order(-fecha,-hora)]
  MDCom <- MCompleto[!duplicated(MCompleto, by = "ID" )]
  names(MDCom) <- c("ID","EjecutivoMC",Et1,Et2,Et3,Et4,Et5,Et6,Et7,"kanban",Et0,"ComentariosMC","usuario","fecha","hora")
  Gte_Eje_MDC <- as.data.table(read_xlsx(paste0(direcc,"insumos/MDControl/Catalogo_Responsales.xlsx")))
  baseMDC <- reactive({
    x <- merge(baseOPP(), MDCom, by = "ID", all.x = TRUE)
    x <- x[Fuente != "CECOR" & (Clasificación == "A" | `¿Estratégica?` == 1 | `Importe Anual`>= 10000000 | !is.na(EjecutivoMC) )]
    return(x[order(-Creación)])
  })
  ### UI esquema general de Inicio
  output$kps <- renderUI({
    tagList(
      navbarPage("",id = ns("MDC_menu"),
           tabPanel("Lista de Oportunidades", value = "panel2", h6(dataTableOutput(ns("OMDC2")))),
           tabPanel("Detalle de OPP", value = "panel3", uiOutput(ns("SMCD3"))),
           tabPanel("Kanban", value = "panel4",
                    fluidRow(column(width = 10, a(HTML('<strong><div align="center">SEGUIMIENTO PROPUESTAS EMPRESARIALES</div></strong>')) ), column(width = 2,actionButton(ns("refreshkan"),label = "Actualizar"))),
                    fluidRow(h6(dataTableOutput(ns("KMDC4")))),
                    textOutput(ns("p1p2")))
      )#cierra WellPanel
    ) #cierra tagList
  })
  
  ### Listado de Oportunidades que están en la Mesa de Control
  output$OMDC2 <- renderDataTable(datatable(subset(baseMDC(), select = camposG_MDC),
                                            rownames = FALSE, filter = "top", selection = 'none', extensions = c('Buttons'), #'KeyTable',
                                            options = list(pageLength = 10, dom = 'Bpti', ordering = TRUE, autoWidth = FALSE,
                                                           scrollX = TRUE, scrollY = TRUE, buttons = c('excel','copy'),# keys = TRUE, 
                                                           language = list(zeroRecords = "No hay información disponible para las opciones específicadas"))
  ))
  ### Actualiza el numero de OPP al elegir un registro de la lista de Oportunidades  
  observeEvent(input$OMDC2_cell_clicked,{
    info = input$OMDC2_cell_clicked
    #if( is.null(info$value) )return()
    ID_OPP <- baseMDC()$ID[info$row]
    updateTextInput(session,"ID_MDC_OPP", value = ID_OPP)
    updateNavbarPage(session,"MDC_menu", selected = "panel3" )
  })
  
  ### Crea las vistas del Detalle de la oportunidad elegida o buscada
  output$SMCD3 <- renderUI({
    tagList(
      fluidRow(
        column(width = 3, 
               textInput(ns("ID_MDC_OPP"), label = "Número de OPP", placeholder = "OPP-1234-123456")
        ),
        column(width = 4,
               htmlOutput(ns("CTE_NOM"))
        ),
        column(width = 5,
               htmlOutput(ns("NOM_OPP"))
        )
      ),
      fluidRow(
        column(width = 5, h6(dataTableOutput(ns("T1MDC")))),
        column(width = 3, h6(dataTableOutput(ns("T2MDC")))),
        box(width = 4, title ="Mesa de Control" ,uiOutput(ns("Datos_MDC")),uiOutput(ns("Boton")))
      )  
    ) #Cierra tagList 
  })
  
  ### Se guarda en una variable el vector de la OPP elegida 
  Reg_buscado <- eventReactive(input$ID_MDC_OPP,{ 
    x <- baseOPP()[ID == input$ID_MDC_OPP]
    x$`División / Sector` <- paste0(x$`División / Sector`," / ", x$Ejecutivo)
    return(x) })
  
  ### Se cargan los comentarios de MDC sobre la OPP elegida
  CReg_MDC <- eventReactive(input$ID_MDC_OPP,{return(MDCom[ID == input$ID_MDC_OPP])})
  
  ### Datos que se actualizan al cambiar la OPP Elegida
  observeEvent(Reg_buscado(),{
    output$CTE_NOM <- reactive(paste("<b>Cliente:</b>",br(),Reg_buscado()$Cliente)) #,br(),"<b>Importe Anual:</b>$",format(Reg_buscado()$`Importe Anual`, scientific=FALSE, big.mark=",")) )
    output$NOM_OPP <- reactive(paste("<b>Nombre de la Oportunidad:</b>",br(),Reg_buscado()$`Nombre de la oportunidad`)) #,br(),"<b>Total de Proyecto:</b>$",format(Reg_buscado()$`Total de Proyecto`, scientific=FALSE, big.mark=",")) )
    
    tabla0 <- subset(Reg_buscado(), select = campos1_MDC)
    ## Cambia la Etapa a "Character"
    tabla0$Etapa <- as.character(tabla0$Etapa)
    ## Cambia formatos de Date a "Character"
    tabla0$`Fecha paso siguiente` <- format(tabla0$`Fecha paso siguiente`,"%d/%b/%y")
    tabla0$Cierre <- format(tabla0$Cierre,"%d/%b/%y")
    tabla0$Creación <- format(tabla0$Creación,"%d/%b/%y")
    tabla0$`Publicación convocatoria` <- format(tabla0$`Publicación convocatoria`,"%d/%b/%y")
    tabla0$`Juntas de aclaraciones` <- format(tabla0$`Juntas de aclaraciones`,"%d/%b/%y")
    tabla0$`Presentación/apertura` <- format(tabla0$`Presentación/apertura`,"%d/%b/%y")
    tabla0$`Fecha de fallo` <- format(tabla0$`Fecha de fallo`,"%d/%b/%y")
    ## Cambia formatos de Numeric a "Character" 
    tabla0$`Importe Anual` <- ifelse(nrow(tabla0) == 0,0,paste0("$",format(tabla0$`Importe Anual`, scientific=FALSE, big.mark=",")))
    tabla0$`Total de Proyecto` <- ifelse(nrow(tabla0) == 0,0,paste0("$",format(tabla0$`Total de Proyecto`, scientific=FALSE, big.mark=",")))
    ## Caso Finanzas 
    ##  tabla0$`Número del caso` <- as.character(tabla0$`Número del caso`)
    
    tabla1 <- gather(tabla0,"Campo","valor",1:length(campos1_MDC))
    
    output$T1MDC <- renderDataTable(datatable(tabla1[1:6,],
                                              rownames = FALSE, colnames = "", #extensions = c('KeyTable','Buttons'), 
                                              options = list(pageLength = 10, dom = 't', ordering = FALSE, autoWidth = FALSE,
                                                             #buttons = c('excel','copy'), keys = TRUE, scrollX = TRUE, scrollY = TRUE,
                                                             language = list(zeroRecords = "No hay información disponible para las opciones específicadas"))
    ))
    
    output$T2MDC <- renderDataTable(datatable(tabla1[7:17,],
                                              rownames = FALSE, colnames = "", #extensions = c('KeyTable','Buttons'), 
                                              options = list(pageLength = 13, dom = 't', ordering = FALSE, autoWidth = FALSE,
                                                             #buttons = c('excel','copy'), keys = TRUE, scrollX = TRUE, scrollY = TRUE,
                                                             language = list(zeroRecords = "No hay información disponible para las opciones específicadas"))
    ))
    
    output$Datos_MDC <- renderUI(
      dataTableOutput(ns("T3MDC"))
    )
    
    ## Verifica si el usuario tiene permisos de edición en el campo MDC_edt    
    output$Boton <- renderUI(
      ifelse(usf$MDC_edt == 1, return(fluidRow( column(width = 8), column(width = 4, actionButton(ns("Editar"),"Editar") ) )),return("") )
    )
    
  }) #Cierra Observe de Registro_buscado
  
  ### Cuadro de Mesa de Control  
  output$T3MDC <- renderDataTable({
    x <- subset(subset(subset(CReg_MDC(), select = -ID ), select = -fecha), select = -hora)
    x$kanban <- ifelse(as.logical(x$kanban),"SI","NO")
    y <- datatable(subset(gather(x,"Campo","valor",-usuario), select = -usuario), escape = FALSE,
                   rownames = FALSE, colnames = "", #extensions = c('KeyTable','Buttons'), 
                   options = list(pageLength = 12, dom = 't', ordering = FALSE, autoWidth = FALSE,
                                  language = list(zeroRecords = "No hay información disponible para las opciones específicadas")) )
    return(y)               
  })
  
  ### Cuando selecciona "Editar"  
  observeEvent(input$Editar,{
    output$Boton <- renderUI(
      return(fluidRow( column(width = 8), column(width = 4, actionButton(ns("Guardar"),"Guardar") ) ))
    )
    
    usr_validos <- reactive(ifelse(usf$MDC_transferencias == 1,return(unique(Gte_Eje_MDC$EjecutivoMC)),return(Gte_Eje_MDC[DD_Sec == Reg_buscado()$`División / Sector_General`]$EjecutivoMC)))
    opciones <- reactive(unique(c(CReg_MDC()$EjecutivoMC, usr_validos())))
    opc1onET <- reactive(unique(c(CReg_MDC()[[Et0]], eta_validos)))
    Vkanban <- reactive({ 
      x <- as.logical(CReg_MDC()$kanban)
      if(nrow(CReg_MDC())==0){x <- FALSE}
      if(is.na(x)){x <- FALSE}
      return(x)
    })
    
    output$Datos_MDC <- renderUI(tagList(
      selectInput(ns("EjMC"),"Ejecutivo Mesa de Control:", choices = opciones(), selected = CReg_MDC()$EjecutivoMC ),
      textInput(ns("ProA"), Et1, value = CReg_MDC()[[Et1]] ),
      textInput(ns("Cope"), Et2, value = CReg_MDC()[[Et2]] ),
      textInput(ns("EsTe"), Et3, value = CReg_MDC()[[Et3]] ),
      textInput(ns("EsAd"), Et4, value = CReg_MDC()[[Et4]] ),
      textInput(ns("EsJu"), Et5, value = CReg_MDC()[[Et5]] ),
      textInput(ns("EsCo"), Et6, value = CReg_MDC()[[Et6]] ),
      textInput(ns("EsDe"), Et7, value = CReg_MDC()[[Et7]] ),
      prettyToggle( ns("KaBa"), label_on = "Poner en Kanban", label_off = "No va en Kanban", outline = TRUE, plain = TRUE,
        icon_on = icon("thumbs-up"), icon_off = icon("thumbs-down"), value = Vkanban() ),
      selectInput(ns("EtMC"), Et0, choices = opc1onET(), selected = CReg_MDC()[[Et0]] ),
      textInput(ns("Come"),"Comentarios MC", value = CReg_MDC()$ComentariosMC )
    ))
    
  }) #Cierra el observe de Editar
  
  ### Al presionar el botón "Guardar"
  observeEvent(input$Guardar,{
    ## Guarda las variables en el data frame
    renglon <- data.table()
    if( input$ID_MDC_OPP %in% MDCom$ID){
      MDCom[ID == input$ID_MDC_OPP]$EjecutivoMC <- input$EjMC
      MDCom[ID == input$ID_MDC_OPP][[Et0]] <- input$EtMC
      MDCom[ID == input$ID_MDC_OPP][[Et1]] <- input$ProA
      MDCom[ID == input$ID_MDC_OPP][[Et2]] <- input$Cope
      MDCom[ID == input$ID_MDC_OPP][[Et3]] <- input$EsTe
      MDCom[ID == input$ID_MDC_OPP][[Et4]] <- input$EsAd
      MDCom[ID == input$ID_MDC_OPP][[Et5]] <- input$EsJu
      MDCom[ID == input$ID_MDC_OPP][[Et6]] <- input$EsCo
      MDCom[ID == input$ID_MDC_OPP][[Et7]] <- input$EsDe
      MDCom[ID == input$ID_MDC_OPP]$usuario <- usf$usuario
      MDCom[ID == input$ID_MDC_OPP]$fecha <- Sys.Date()
      MDCom[ID == input$ID_MDC_OPP]$hora <- strftime(Sys.time(),"%H:%M")
      MDCom[ID == input$ID_MDC_OPP]$kanban <- as.character(input$KaBa)
      MDCom[ID == input$ID_MDC_OPP]$ComentariosMC <- input$Come
      renglon <- MDCom[ID == input$ID_MDC_OPP]
      renglon$fecha <- strftime(Sys.Date(),"%Y/%m/%d")
    }
    else{
      x <- data.table()
      x$ID <- Reg_buscado()$ID
      x$EjecutivoMC <- input$EjMC
      x$E1 <- input$ProA
      x$E2 <- input$Cope
      x$E3 <- input$EsTe
      x$E4 <- input$EsAd
      x$E5 <- input$EsJu
      x$E6 <- input$EsCo
      x$E7 <- input$EsDe
      x$kanban <- as.character(input$KaBa)
      x$E0 <- input$EtMC
      x$ComentariosMC <- input$Come
      x$usuario <- usf$usuario
      x$fecha <- Sys.Date()
      x$hora <- strftime(Sys.time(),"%H:%M")
      names(x) <- names(MDCom)
      renglon <- x
      renglon$fecha <- strftime(Sys.Date(),"%Y/%m/%d")
      MDCom <- rbind(MDCom,x)
    }
    ## Guarda el renglón que se creó
    fwrite(renglon, file = paste0(direcc,"insumos/MDControl/comentaMDC.csv"), append = TRUE)
    updateTextInput(session,"ID_MDC_OPP", value = "" )
    MDCom <<- MDCom
    output$Boton <- renderUI(
      return(fluidRow( column(width = 8), column(width = 4, actionButton(ns("Editar"),"Editar") ) ))
    )
    
  }) #Cierra el observe de Guardar
  
###Crea el Kanban
  output$KMDC4 <- renderDataTable({
    x <- MDCom[as.logical(kanban) == TRUE]
    u <- data.table("ID"=as.character(),"Comentarios"=as.character())
    if(nrow(x)>0){
      u <- data.table("ID"= x$ID, 
                      "Comentarios" = paste0("1. ",strong(Et1),": ",x[[Et1]],"\r\n",br(),"2. ",strong(Et2),": ",x[[Et2]],"\r\n",br(),"3. ",strong(Et3),": ",x[[Et3]],"\r\n",br(),
                                             "4. ",strong(Et4),": ",x[[Et4]],"\r\n",br(),"5. ",strong(Et5),": ",x[[Et5]],"\r\n",br(),"6. ",strong(Et6),": ",x[[Et6]],"\r\n",br(),
                                             "7. ",strong(Et7),": ",x[[Et7]]) 
      )
    }
    x <- subset(merge(u,baseOPP()), select = campKanban)[order(`Presentación/apertura`)]
    y <- datatable(x, rownames = FALSE, extensions = c('FixedHeader','FixedColumns','KeyTable','Buttons'), 
                   escape = c(5), colnames = c('OPP' = 'ID','Publicación de Bases' = 'Publicación convocatoria', 'Creación Sactel' = 'Creación','Entrega Propuesta' = 'Presentación/apertura','Fallo' = 'Fecha de fallo'),
                   options = list(pageLength = 5, dom = 'Bplti', ordering = FALSE, autoWidth = TRUE, 
                                  buttons = c('excel','copy'), fixedHeader = TRUE, keys = TRUE, 
                                  scrollX = TRUE, scrollY = TRUE, #fixedColumns = list(leftColumns = 2),
                                  columnDefs = list(list(width = '200px', targets = c(4,5,6) )),
                                  language = list(zeroRecords = "No hay información disponible para las opciones específicadas")
                   )
    ) %>% formatCurrency(c(8), currency = "$", digits = 0) %>% 
      formatDate(c(9:13),c(rep('toLocaleDateString',5)))
    
    return(y)
  })
  
observeEvent(input$refreshkan,{
  output$KMDC4 <- renderDataTable({
    x <- MDCom[as.logical(kanban) == TRUE]
    u <- data.table("ID"=as.character(),"Comentarios"=as.character())
    if(nrow(x)>0){
      u <- data.table("ID"= x$ID, 
                      "Comentarios" = paste0("1. ",strong(Et1),": ",x[[Et1]],"\r\n",br(),"2. ",strong(Et2),": ",x[[Et2]],"\r\n",br(),"3. ",strong(Et3),": ",x[[Et3]],"\r\n",br(),
                                             "4. ",strong(Et4),": ",x[[Et4]],"\r\n",br(),"5. ",strong(Et5),": ",x[[Et5]],"\r\n",br(),"6. ",strong(Et6),": ",x[[Et6]],"\r\n",br(),
                                             "7. ",strong(Et7),": ",x[[Et7]]) 
      )
    }
    x <- subset(merge(u,baseOPP()), select = campKanban)[order(`Presentación/apertura`)]
    y <- datatable(x, rownames = FALSE, extensions = c('FixedHeader','FixedColumns','KeyTable','Buttons'), 
                   escape = c(5), colnames = c('OPP' = 'ID','Publicación de Bases' = 'Publicación convocatoria', 'Creación Sactel' = 'Creación','Entrega Propuesta' = 'Presentación/apertura','Fallo' = 'Fecha de fallo'),
                   options = list(pageLength = 5, dom = 'Bplti', ordering = FALSE, autoWidth = TRUE, 
                                  buttons = c('excel','copy'), fixedHeader = TRUE, keys = TRUE, 
                                  scrollX = TRUE, scrollY = TRUE, #fixedColumns = list(leftColumns = 2),
                                  columnDefs = list(list(width = '200px', targets = c(4,5,6) )),
                                  language = list(zeroRecords = "No hay información disponible para las opciones específicadas")
                   )
    ) %>% formatCurrency(c(9), currency = "$", digits = 0) %>% 
      formatDate(c(10:14),c(rep('toLocaleDateString',5)))
    
    return(y)
  })
})

observeEvent(input$KMDC4_cell_clicked,{
  info = input$KMDC4_cell_clicked
  
  if( !is.null(info$value) && info$col == 0 ){
    updateTextInput(session,"ID_MDC_OPP", value = as.character(info$value) )
    updateNavbarPage(session,"MDC_menu", selected = "panel3" )
    }

})

} #Cierra función "server"