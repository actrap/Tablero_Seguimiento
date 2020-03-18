
minusculas <- c('a','b','c','d','e','f','g','h','i','j','k','l','m','n','ñ','o','p','q','r','s','t','u','v','w','x','y','z')
mayusculas <- c('A','B','C','D','E','F','G','H','I','J','K','L','M','N','Ñ','O','P','Q','R','S','T','U','V','W','X','Y','Z')
numeros <- c('0','1','2','3','4','5','6','7','8','9')
caracteres <- c("#","$","%","&","/","(",")","=","?","¿","¡","!","|","+","-","_","*",".",",","{","}","[","]","<",">")
min_long <- 9 #longitud mínima

Vig_Pwd <- 365 #Días de vigencia de contraseña

vigenciaUI <- function(id){
  ns <- NS(id)
  
  uiOutput(ns("sVig"))
  
}

vigencia <- function(input, output, session, direcc, usr_entrada){
  
  ns <- session$ns
  
  output$sVig <- renderUI({
      showModal(modalDialog(
          #passwordInput(ns("npwd1"),"Nueva Contraseña"),
          #passwordInput(ns("npwd2"),"Repite nueva Contraseña"),
          HTML("Recuerda que la contraseña debe contener al menos:<br>"),
          HTML(paste("- mínimo",min_long,"caracteres,<br>")),
          HTML("- una letra mayúscula,<br>"),
          HTML("- una letra minúscula,<br>"),
          HTML("- un número,<br>"),
          HTML("- un caracter especial (#, $, %, &, /, etc.)<br>"),
          title = "Tu contraseña ha caducado y es necesario que la cambies",
          footer = actionButton(ns("ok_02"), "Seguir"),
          size = "m",
          fade = FALSE
      ))
  })
    
  
  observeEvent(input$ok_02,{
        removeModal()
        valida <- "ok"; tipo <- "ok"
        if( is.null(input$pwd1) ){ valida <- "error"; tipo <- " "; PWD1 <- "abc"; PWD2 <- "abc"} else{PWD1 <- as.character(input$pwd1); PWD2 <- as.character(input$pwd2)}
        if( valida != "error" && PWD1 != PWD2 ){ valida <- "error"; tipo <- "Debes poner la misa contraseña en ambos espacios" }
        palabra <- unlist(strsplit(PWD1, "", fixed = TRUE))
        
        if( valida != "error" && length(palabra) < min_long ) { valida <- "error"; tipo <- paste("Longitud minima de",min_long,"caracteres") }
        if( valida != "error" && !any(minusculas %in% palabra) ){ valida <- "error"; tipo <- "Usar al menos una letra minúscula" }
        if( valida != "error" && !any(mayusculas %in% palabra) ){ valida <- "error"; tipo <- "usar al menos una letra mayúscula" }
        if( valida != "error" && !any(numeros %in% palabra) ){ valida <- "error"; tipo <- "usar al menos un número" }
        if( valida != "error" && !any(caracteres %in% palabra) ){ valida <- "error"; tipo <- "usar al menos un caracter especial" }
        
        if(valida == "error"){
            showModal(modalDialog(
                passwordInput(ns("pwd1"),"Nueva Contraseña"),
                passwordInput(ns("pwd2"),"Repite nueva Contraseña"),
                HTML(paste('<p align="center"><font color = red>Recuerda cumplir con los criterios:',tipo,'</font></p>')),
                HTML("la contraseña debe contener al menos:<br>"),
                HTML(paste("- mínimo",min_long,"caracteres,<br>")),
                HTML("- una letra mayúscula,<br>"),
                HTML("- una letra minúscula,<br>"),
                HTML("- un número,<br>"),
                HTML("- un caracter especial (#, $, %, &, /, etc.)<br>"),
                title = "Actualizar contraseña",
                footer = actionButton(ns("ok_02"), "Actualizar"),
                size = "m",
                fade = TRUE
            ))
        }
        
        if(valida == "ok"){
          usuarios <-  fread(paste0(direcc,"uso/","usuarios.csv"), sep = ",", encoding = "Latin-1")
          usuarios[usuario == usr_entrada ][["contraseña"]] <- as.character(input$pwd1)
          usuarios[usuario == usr_entrada ][["pwd_update"]] <- strftime(Sys.Date(), format = "%d/%m/%Y")
          write.csv(usuarios,paste0(direcc,"uso/","usuarios.csv"), fileEncoding = "latin1", row.names = FALSE)
        }
    
    
  })
  
  

  
}  