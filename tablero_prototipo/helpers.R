## FUNCIONES PARA PROCESAR DATOS

genera_resumen <- function(df){
  #'@param df Un dataframe, eventualmente agrupado por subgrupos
  #'@returns Un dataframe resumido, con las categorias POBLACION, ECONOMICAMENTE_ACTIVES, CUENTAPROPISTAS_PROFESIONALES, CUENTAPROPISTAS_NO_PROFESIONALES.
  #'@details El objetivo de esta funcion es agilizar la escritura de la preparación de resumenes que muestren la evolución por año y trimestre de las poblaciones de interés. Pasandole un tibble agrupado, se puede ver la evolución de subgrupos de poblaciones en términos de las variables de la EP.
  df %>%
    summarise(
      'POBLACION'=sum(PONDERA),
      'OCUPADES' = sum(
        PONDERA * 
          (ESTADO == 'OCUPADE'),na.rm=TRUE),
      'ECONOMICAMENTE_ACTIVES' = sum(
        PONDERA * 
          (ESTADO == 'OCUPADE' | ESTADO == 'DESOCUPADE'),na.rm=TRUE),
      'CUENTAPROPISTAS' = sum(
        PONDERA * 
          (CATEGORIA_OCUPACION == 'CUENTAPROPISTA'),na.rm=TRUE),
      'CUENTAPROPISTAS_PROFESIONALES' = sum(
        PONDERA * 
          (CATEGORIA_OCUPACION == 'CUENTAPROPISTA' & ES_PROFESIONAL),na.rm=TRUE),
      'CUENTAPROPISTAS_NO_PROFESIONALES' = sum(
        PONDERA * 
          (CATEGORIA_OCUPACION == 'CUENTAPROPISTA' & !ES_PROFESIONAL),na.rm=TRUE),
      'TFSR' = sum(
        PONDERA * 
          (CATEGORIA_OCUPACION == 'TRABAJADORE FLIAR S.R.'),na.rm=TRUE),
      "RESTO_CUENTAPROPISTAS" = CUENTAPROPISTAS - CUENTAPROPISTAS_NO_PROFESIONALES - TFSR,
      "ASALARIADOS_REGISTRADOS" = sum(
        PONDERA * (
          CATEGORIA_OCUPACION == 'ASALARIADE' & REGISTRACION == 1),na.rm=TRUE
        ),
      "ASALARIADOS_NOREGISTRADOS" = sum(
        PONDERA * (
          CATEGORIA_OCUPACION == 'ASALARIADE' & REGISTRACION == 2),na.rm=TRUE
      ),
      "PATRONES" = sum(
        PONDERA * (
          CATEGORIA_OCUPACION == 'PATRON'), na.rm=TRUE
        ),
      "EP" = CUENTAPROPISTAS_NO_PROFESIONALES + TFSR
      ) %>% 
    return()
}
view(individual_03.hoy)
############################
## FUNCIONES PARA EL TABLERO
############################

genera_aes_cantTrabEP_plot <- function(input){
  #' @description Esta función se encarga de generar el objeto estético para el plot cantTrabEP_plot
  if(input$separar_sexos){ # Si separamos por sexo
      if(input$separar_zonas){ # Si separamos por región
        
        aes_plot <- aes_(
          x = ~ FECHA,
          y = ~ (ECON_NUCLEO)/1e6, 
          group = as.formula( paste('~paste( SEXO,', input$variable_zona, ')' )), 
          color = as.formula( paste('~', input$variable_zona )),
          shape = ~ SEXO)
        
      }else{ # Si no separamos por región
        
        aes_plot <- aes_(
          x = ~ FECHA,
          y = ~ (ECON_NUCLEO)/1e6, 
          group = ~ SEXO, 
          shape = ~ SEXO)
        
      }
    }else{ # Si no separamos por sexo
      if(input$separar_zonas){ # Si separamos por región
        
        aes_plot <- aes_(
          x = ~ FECHA,
          y = ~ (ECON_NUCLEO)/1e6, 
          group = as.formula(paste('~',input$variable_zona)), 
          color = as.formula(paste('~',input$variable_zona)))
        
      }else{ # Si no separamos por región
        
        aes_plot <- aes_(
          x = ~ FECHA,
          y = ~ (ECON_NUCLEO)/1e6)
        
      }
    }
  return(aes_plot)
}

genera_grouping_vars_cantTrabEP_plot <- function(input){
  #'@description Esta función genera el agrupamiento necesario para el plot cantTrabEP_plot
  # if(input$separar_sexos){ # Si separamos por sexo
  # 
  #   if(input$separar_zonas){ # Si separamos por zona
  # 
  #     grouping_vars <- quos('SEXO','YEAR','TRIMESTER', input$variable_zona)
  # 
  #     if(input$agregar_ocupados){ #Si separamos por ocupación
  # 
  #       grouping_vars <- quos('SEXO','YEAR','TRIMESTER', input$variable_zona, "EP", "RESTO_CUENTAPROPISTAS", "ASALARIADOS_REGISTRADOS", "ASALARIADOS_NOREGISTRADOS", "PATRONES")
  # 
  #     }
  # 
  #   }else{
  # 
  #     grouping_vars <- quos('SEXO','YEAR','TRIMESTER')
  # 
  #     if(input$agregar_ocupados){ #Si separamos por ocupación
  # 
  #       grouping_vars <- quos('SEXO','YEAR','TRIMESTER', "EP", "RESTO_CUENTAPROPISTAS", "ASALARIADOS_REGISTRADOS", "ASALARIADOS_NOREGISTRADOS", "PATRONES")
  # 
  #     }
  # 
  #   }
  # }else {
  #   if(input$separar_zonas){ # Si separamos por zona
  # 
  #     grouping_vars <- quos('YEAR','TRIMESTER', input$variable_zona)
  # 
  #     if(input$agregar_ocupados){ #Si separamos por ocupación
  # 
  #       grouping_vars <- quos('YEAR','TRIMESTER', input$variable_zona, "EP", "RESTO_CUENTAPROPISTAS", "ASALARIADOS_REGISTRADOS", "ASALARIADOS_NOREGISTRADOS", "PATRONES")
  # 
  #     }
  # 
  #   }else{
  # 
  #     if(input$agregar_ocupados){ #Si separamos por ocupación
  # 
  #       grouping_vars <- quos('YEAR','TRIMESTER', "EP", "RESTO_CUENTAPROPISTAS", "ASALARIADOS_REGISTRADOS", "ASALARIADOS_NOREGISTRADOS", "PATRONES")
  # 
  #     }else{ # Si no separamos
  # 
  #       grouping_vars <- quos('YEAR','TRIMESTER')
  # 
  #     }
  # 
  #   }
  # }
 
  
  #########################################################
  
  ###POR LAS DUDAS DEJO ACÁ LO QUE ESTABA ESCRITO ANTES####
  # if(input$separar_sexos){ # Si separamos por sexo
  # 
  #   if(input$separar_zonas){
  # 
  #     grouping_vars <- quos('SEXO','YEAR','TRIMESTER', input$variable_zona)
  # 
  #   }else{
  # 
  #     grouping_vars <- quos('SEXO','YEAR','TRIMESTER')
  # 
  #   }
  # }else {
  #   if(input$separar_zonas){
  # 
  #     grouping_vars <- quos('YEAR','TRIMESTER', input$variable_zona)
  # 
  #   }else{
  # 
  #     grouping_vars <- quos('YEAR','TRIMESTER')
  # 
  #   }
  # }

  
  ########################################################
  
  ### INTENTO DE NICO POR HACER MENOS LÍNEAS. Cuando intentaba correr la aplicación me decía que había un vector vacío, al cambiar el codigo anterior por el que escribí arriba, dicho problema desapareció.

  ### Ya funciona bien las primeras dos lineas, lo de ocupados todavia no porque no esta implementado el grafico

  agrupar <- c("YEAR", "TRIMESTER")
  if(input$separar_sexos){
    agrupar <- c(agrupar, "SEXO")
  }
  if(input$separar_zonas){
    agrupar<- c(agrupar, input$variable_zona)
  }
  if(input$agregar_ocupados){
    agrupar <- c(agrupar, "EP", "RESTO_CUENTAPROPISTAS", "ASALARIADOS_REGISTRADOS", "ASALARIADOS_NOREGISTRADOS", "PATRONES")
  }
  grouping_vars <- quos(agrupar) 
  return(grouping_vars) 
}
