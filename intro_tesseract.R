#() INSTALANDO PAQUETES Y CONFIGURANDO EL SERVIDOR PARA SELENIUM
if (!"RSelenium" %in% installed.packages()[,"Package"]){install.packages("RSelenium")}

library(RSelenium)
fdriver <- RSelenium::rsDriver(browser = c('chrome'),
                               port = 43533L, 
                               chromever = '103.0.5060.53',  
                               verbose = FALSE)


driver<-fdriver[['client']]



if (!"tidyverse" %in% installed.packages()[,"Package"]){install.packages("tidyverse")}
if (!"lubridate" %in% installed.packages()[,"Package"]){install.packages("lubridate")}
if (!"rvest" %in% installed.packages()[,"Package"]){install.packages("rvest")}
if (!"magick" %in% installed.packages()[,"Package"]){install.packages("magick")}
if (!"tesseract" %in% installed.packages()[,"Package"]){install.packages("tesseract")}
if (!"stringr" %in% installed.packages()[,"Package"]){install.packages("stringr")}
if (!"dplyr" %in% installed.packages()[,"Package"]){install.packages("dplyr")}
library(tidyverse)
library(lubridate)
library(rvest)
library(magick)
library(stringr)
library(tesseract)
library(dplyr)
tesseract_download('spa')

#especificar parámetros
inicio <- 2019
final <- 2022
years <- as.character(c(inicio:final))

####################################
#####1. Automatizar las descargas
####################################

#Es muy complicado descargar los archivos en una carpeta específica, por lo que se descarga automáticamente
#en la carpeta de /Downloads.
#En caso ya haya corrido anteriormente el código, lo ideal es verificar que en la carpeta de descargas 
#no se encuentren archivos pasados que pueden ocasionar conflictos con el presente código

setwd("C:/Users/luisc/Downloads")
file.remove('EjecucionPresupuestaria.pdf')

for (i in seq_along(years)) {
  file.remove(str_c('EjecucionPresupuestaria (',i,').pdf'))
} 

# entrar a la pagina
driver$maxWindowSize()
driver$navigate('https://sigep.sigma.gob.bo/sigep_publico/faces/SFprEjecucionPresupuestaria')
Sys.sleep(5)

#descargar 1 de prueba y verificar que esté en Downloads
descarga_prueba<-driver$findElement(using='xpath','//*[@id="pt1:id2019::text"]')
descarga_prueba$clickElement()
Sys.sleep(5)

#si esta en downloads, continuamos
for (i in seq_along(years)) {

  x <- years[i] #seleccionamos el año
  descargar<-driver$findElement(using='xpath', str_c('//*[@id="pt1:id',x,'::text"]'))
  descargar$clickElement()
  Sys.sleep(6)
}

driver$close()

####################################
#####2. Scrapear los pdfs descargados
####################################

pre_presup <- list()

#por defecto, nuestras descargas se encuentran en la carpeta Downloads,
#sin embargo, nuestro ambiente estará definido en nuestra carpeta de trabajo
script.path <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(script.path)

#loop para scrapear con OCR
for (i in seq_along(years)) {
  x <- years[i] #seleccionamos el año
  
  #convertimos el pdf
  pngfile <- pdftools::pdf_convert(str_c("C:/Users/luisc/Downloads/EjecucionPresupuestaria (",i,").pdf"), 
                                   dpi = 600)
  
  pngfile <- pngfile[1] #solo importa la primera página, donde estan los ministerios
  
  #probamos consistencia
  text <- tesseract::ocr(pngfile)
  cat(text)
  
  # RECOLECTAR LA DATA DE LA PAG 1
  raw_img <- image_read(pngfile)
  image_ggplot(raw_img) #ok
  
  ## naive OCR
  naive <- raw_img %>% 
    image_crop(geometry_area(0, 0, 110, 45)) %>% 
    ocr()
  naive #Ahora incluye los saltos de líneas
  
  #B&W image
  raw_img %>% 
    image_quantize(colorspace = "gray") %>% 
    image_ggplot()
  
  #take off "gray" areas
  no_grid <- raw_img %>% 
    image_quantize(colorspace = "gray") %>% 
    image_transparent(color = "white", fuzz=20) %>% 
    image_background("white") 
  
  image_ggplot(no_grid)
  
  #contener el formato
  
  no_grid_crop <- no_grid %>% 
    image_crop(geometry_area(0,0,300, 955))
  
  no_grid_crop %>% 
    image_ggplot()
  
  #---------------
  #DOS OPERACIONES:
  #---------------
  
  #1. Nombres (solo toma lado izquierdo)
  nombres <- no_grid_crop %>% 
    image_crop(geometry_area(1400,0,0, 0)) 
  
  nombres %>%
    image_ggplot()
  
  #2. Cifras (presupuesto y ejecución)
  cifras <- no_grid_crop %>% 
    image_crop(geometry_area(0,0,2000, 0)) 
  
  cifras %>%
    image_ggplot()
  
  #############################################
  #OCR
  
  no_grid_crop %>% 
    image_ocr()
  
  #1
  nombres %>% 
    image_ocr()
  #2
  cifras %>% 
    image_ocr()
  # Es mejor usar el paquete de español
  instituciones <- nombres %>% 
    image_quantize(colorspace = 'gray') %>% 
    image_threshold() %>% 
    image_crop(geometry_area(0,0,0, 0)) %>% 
    ocr(engine = "spa")
  
  instituciones
  
  #Engine 2: NUMBERS
  num_only <- tesseract::tesseract(
    options = list(tessedit_char_whitelist = c(",.0123456789"))
  )
  
  numeros <- cifras %>% 
    image_quantize(colorspace = 'gray') %>% 
    image_threshold() %>% 
    image_crop(geometry_area(0,0,0, 0)) %>% 
    ocr(engine = num_only) 
  numeros
  
  ##############################################
  # tibbles
  ##############################################
  
  #nombres tibbles
  nombres_tibble <- instituciones %>% 
    str_split(pattern = "\n") %>% 
    unlist() %>%
    tibble(data = .) 
  
  nombres_tibble
  #limpieza
  data1 <- nombres_tibble[!apply(is.na(nombres_tibble) | nombres_tibble == "", 1, 
                                 all),]
  #################
  
  # numeros tibbles
  numeros_tibble <- numeros %>% 
    str_split(pattern = "\n") %>% 
    unlist() %>%
    tibble(data = .) 
  
  numeros_tibble
  #limpieza
  data2 <- numeros_tibble[!apply(is.na(numeros_tibble) | numeros_tibble == "", 1, 
                                 all),]
  
  data3 <- data2 %>% 
    mutate(across(where(is.character), str_trim))
  
  data3 <-data3 %>% 
    separate(
      data, 
      into = c("Ppto. Aprobado", "Crédito vigente", "Comprometido", "Devengado",
               "Pagado", "Saldo x Devengar", "Deuda flotante"), 
      sep = c(" ")
    ) 
  
  # Unir y obtener el df
  presup <- cbind(data1, data3)
  presup$year <- x
  
  #dentro de la lista
  pre_presup[[i]]<-presup
  
  }

# convertimos la lista en df
presup_total <-bind_rows(pre_presup) %>% select(year, everything())

#aplicamos formato numeric
nums <- c("Ppto. Aprobado", "Crédito vigente", "Comprometido", "Devengado",
  "Pagado", "Saldo x Devengar", "Deuda flotante")

presup_total[nums] <- lapply(presup_total[nums], gsub, pattern = ",", replacement = "")

#limpiamos la data
presup_total <- presup_total[grepl("Ministerio", presup_total$data),]

#guardamos
write_rds(presup_total,'data_final/presupuesto_bol.rds')
  


