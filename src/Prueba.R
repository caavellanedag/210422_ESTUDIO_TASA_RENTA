#
# Autor(es): Camilo Avellaneda
# Mantenimiento: Camilo Avellaneda
# Fecha creación: 22/04/2021
#==============================================

x<-10
if(!("input" %in% list.files())){dir.create("input")}
if(!("output" %in% list.files())){dir.create("output")}
if(!("src" %in% list.files())){dir.create("src")}
if(!("docs" %in% list.files())){dir.create("docs")}

require(pacman)
p_load(tidyverse,openxlsx,here,haven)
cat("labels=function(x)format(x, big.mark = \".\", scientific = FALSE) ",
    file = "src/Funciones.R")



"input/BASE_HIST_17_20_CONSOLI_FINAL_25022021.tab"

getwd()

base<-read.table("input/BASE_HIST_17_20_CONSOLI_FINAL_25022021.tab", header = T, sep = "\t", fill = TRUE)
base$PRECIO<-as.numeric(as.character(str_replace_all(base$PRECIO,c("\\$"="",","=""))))
base$IDENTIFICADOR<-as.character(base$IDENTIFICADOR)
names(base)
base<-base %>% mutate_at(c("IDENTIFICADOR","CODIGO_UPZ","NOMBRE_UPZ","CODIGO_BARRIO","NOMBRE_BARRIO",
                           "NOMBRE_LOCALIDAD","CODIGO_LOCALIDAD","BARMANPRE"),as.character) 
base$FECHA<-as.Date(base$FECHA,format="%d/%m/%Y")

nrow(base)
n_fail<-sum(base$IDENTIFICADOR=="" | is.na(base$IDENTIFICADOR) | is.na(base$PRECIO) | base$BARMANPRE=="" | is.na(base$BARMANPRE))
base_filtrada<-base[!(base$IDENTIFICADOR=="" | is.na(base$IDENTIFICADOR) | is.na(base$PRECIO) | base$BARMANPRE=="" | is.na(base$BARMANPRE)),]
base_filtrada<-as.data.table(base_filtrada)
