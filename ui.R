library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)

#a <- "E:/Archivos_LARCOS/Aplicaciones/Tablero_Seguimiento/"
a <- "C:/Users/larcos/M_Empresarial/Analisis/Ventas/Aplicaciones/Tablero_Seguimiento/"
#a <- "C:/Users/vmorenoa/Documents/R proyectos/pruebas/"
b <- "/srv/shiny-server/tablero_seguimiento/"
#b <- "/srv/shiny-server/pruebas/"

direcc <- if_else(substr(getwd(),1,1) == "/", b , a)

source(paste0(direcc,"BajoDemanda_1.R"))
source(paste0(direcc,"vigencia_1.R"))
#source(paste0(direcc,"Visitas_1.R"))
#source(paste0(direcc,"Csucursales_1.R"))
#source(paste0(direcc,"CCentroDatos_1.R"))
#source(paste0(direcc,"CForta2020_1.R"))
#source(paste0(direcc,"CGestionSAP_1.R"))
source(paste0(direcc,"CProductos_1.R"))
source(paste0(direcc,"Oportunidades_1.R"))

shinyUI(
  dashboardPage(skin = "blue", title="Reporte Ventas Consultivas TI", 
    dashboardHeader(title = "Reporte Ventas TI"),
    dashboardSidebar(disable = FALSE, sidebarMenu(id = "Menu", menuItem("Bienvenido", tabName = "Mi0"),
      uiOutput("ParaMenu1"),
      sidebarSearchForm(textId = "txt_01", buttonId = "searchbtn", label = "Buscar Palabra..."),
      fluidRow(
        column(8, actionBttn("clean","Limpiar filtros", icon = icon("eraser"), style = "gradient", color = "primary", size = "xs")),
        column(4, actionBttn( inputId = "back", label = NULL, style = "material-circle", color = "primary", icon = icon("undo"), size = "xs" ))
      ),
      menuItemOutput("PM1"),
      menuItemOutput("PM2"), 
      menuItemOutput("PM3"),
      menuItemOutput("PM4")
    )),## Cierra el SideBar
    dashboardBody(tabItems(
      tabItem(tabName = "Mi0", htmlOutput("usrFR")),
      tabItem(tabName = "S_0", uiOutput("s_hoja_0")),
      tabItem(tabName = "Pi2", BajoDemandaUI("B_D1")),
      tabItem(tabName = "UsR", vigenciaUI("Vig_01")),
     #tabItem(tabName = "visi", fvisitasUI("V_D1")),
     #tabItem(tabName = "CS_1", fsucursalUI("CSuc")),
     #tabItem(tabName = "CCD_1", fcentrodatosUI("CDat")),
     #tabItem(tabName = "Pi4", MapaitUI("Mat1")),
      tabItem(tabName = "TabProd", fproductoUI("CProd"))
    )) #cierra el Body
  )#cierre dashboardPage
)