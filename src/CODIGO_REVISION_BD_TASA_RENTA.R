#
# Autor(es): Camilo Avellaneda
# Mantenimiento: Camilo Avellaneda
# Fecha creación: 22/04/2021
#==============================================

if(!("input" %in% list.files())){dir.create("input")}
if(!("output" %in% list.files())){dir.create("output")}
if(!("src" %in% list.files())){dir.create("src")}
if(!("docs" %in% list.files())){dir.create("docs")}

require(pacman)
p_load(tidyverse, openxlsx, here, haven, data.table, RecordLinkage, IAcsSPCR, qcc, matrixcalc)
options(scipen = 999)
# cat("labels=function(x)format(x, big.mark = \".\", scientific = FALSE) ",
#     file = "src/Funciones.R")
source("src/Funciones.R")
source("src/Genera_mapa_sectores_Bta.R")
# table(base$OUTLAYER)
# sum( is.na(base$PRECIO) | base$PRECIO==0)
# resumen<-base %>% group_by(OUTLAYER) %>% count() %>% ungroup()

base <- fread("input/BASE_HIST_RENTAS.tab", header = T, sep = "\t")
base$PRECIO <- as.numeric(as.character(str_replace_all(base$PRECIO,c("\\$"="",","=""))))
base$IDENTIFICADOR <- as.character(base$IDENTIFICADOR)

base <- base %>% mutate_at(c("IDENTIFICADOR","CODIGO_UPZ","NOMBRE_UPZ","CODIGO_BARRIO","NOMBRE_BARRIO",
                           "NOMBRE_LOCALIDAD","CODIGO_LOCALIDAD","BARMANPRE"),as.character) 
base$FECHA <- as.Date(base$FECHA,format="%d/%m/%Y")

# list_df <- list(base[FECHA>="2020-03-24" & FECHA<="2020-11-13",.N,by=.(NOMBRE_FUENTE)],
# base[FECHA>="2020-03-24" & FECHA<="2020-11-13",.N,by=.(TIPO_INMUEBLE,CLASE_PREDIO)],
# base[FECHA>="2020-03-24" & FECHA<="2020-11-13",.N,by=.(TIPO_INMUEBLE)],
# base[FECHA>="2020-03-24" & FECHA<="2020-11-13",.N,by=.(CLASE_PREDIO)],
# base[FECHA>="2020-03-24" & FECHA<="2020-11-13" & TIPO_INMUEBLE %in% c("APARTAMENTO","CASA"),.N,by=.(ESTRATO)])
# 
# labels<-c("NOMBRE_FUENTE","TIPO_CLASE_PREDIO","TIPO","CLASE","ESTRATO_SOLO_APT_CASA")
# wb<-createWorkbook("Camilo Avellaneda")
# map2(list_df,labels,~Sheet(.x,.y))
# saveWorkbook(wb,"output/210426_RESUMEN_OFERTAS_MARZO_NOVIEMBRE_2020.xlsx",overwrite=TRUE)

base_filtrada <- base[TIPO_INMUEBLE %in% c("APARTAMENTO","CASA")]
base_filtrada <- base_filtrada[,CODIGO_BARRIO := str_pad(CODIGO_BARRIO, width = 6, side = "left", pad = "0")]
base_filtrada <- base_filtrada[,BARMANPRE := str_pad(BARMANPRE, width = 10, side = "left", pad = "0")]

N_APTO_CASA <- base_filtrada[, .N]
N_BARMANPRE <- sum(base_filtrada$BARMANPRE=="" | is.na(base_filtrada$BARMANPRE))
N_AC <- sum(is.na(base_filtrada$AREA_CONSTRUIDA) | base_filtrada$AREA_CONSTRUIDA==0) 
N_AT <- sum((base_filtrada$TIPO_INMUEBLE!="APARTAMENTO" & (is.na(base_filtrada$AREA_DE_TERRENO) | base_filtrada$AREA_DE_TERRENO==0)))
#base_filtrada <- base_filtrada[!(base_filtrada$TIPO_INMUEBLE!="APARTAMENTO" & is.na(base_filtrada$AREA_DE_TERRENO)) & !is.na(base_filtrada$AREA_CONSTRUIDA),]
base_filtrada <- base_filtrada[!(base_filtrada$TIPO_INMUEBLE!="APARTAMENTO" & (is.na(base_filtrada$AREA_DE_TERRENO) | base_filtrada$AREA_DE_TERRENO==0))]
base_filtrada <- base_filtrada[!(is.na(base_filtrada$AREA_CONSTRUIDA) | base_filtrada$AREA_CONSTRUIDA==0)]
N_RURAL <- base_filtrada[substring(CODIGO_BARRIO,1,1) != "0", .N]
N_ALCOBAS <- base_filtrada[NUMERO_ALCOBAS == 0, .N]
N_BANOS <- base_filtrada[NUMERO_BA_OS == 0, .N]
N_ESTRATO <- base_filtrada[ESTRATO == 0 | is.na(ESTRATO), .N]

base_filtrada <- base_filtrada[substring(base_filtrada$CODIGO_BARRIO,1,1) == "0", ]
base_filtrada <- base_filtrada[NUMERO_ALCOBAS > 0]
base_filtrada <- base_filtrada[NUMERO_BA_OS > 0]
base_filtrada <- base_filtrada[AREA_CONSTRUIDA > 0]
base_filtrada <- base_filtrada[ESTRATO > 0]
base_filtrada <- base_filtrada[(AREA_DE_TERRENO>0 & CLASE_PREDIO == "N") | (CLASE_PREDIO != "N")]
base_filtrada <- base_filtrada[!(base_filtrada$BARMANPRE=="" | is.na(base_filtrada$BARMANPRE)),]




#table(base$NOMBRE_FUENTE)


base_filtrada <- base_filtrada[,CODIGO_BARRIO_OR := CODIGO_BARRIO]
base_filtrada[,key := paste0(AÑO,"_",CLASE_PREDIO,"_", ESTRATO, "_", TIPO_OFERTA)]

lista_separada <- base_filtrada %>% split(.$key) %>% map(Fill_na) 
lista_2 <- map(lista_separada, ~Agrupa_barrios(.x, cutoff = 10))

df_clean <- map_dfr(lista_2,function(.x){
  lista_3 <- .x  %>% split(.$CODIGO_BARRIO_NEW) 
  results <- map(lista_3, make_control_chart)
  df_clean <- map2_dfr(lista_3, results,
                       function(.x, .y){
                         if(is.null(.y$outlier)){
                           .x
                         }else{
                           .x[-.y$outlier,]
                         }
                       })
  return(df_clean)
})




base_filtrada[,.N,by=.(TIPO_INMUEBLE)][order(TIPO_INMUEBLE),LABELS:=labels(N)][N>2000] %>% 
  ggplot()+geom_col(aes(x=fct_reorder(TIPO_INMUEBLE,N,.desc=TRUE),y=N),fill="red",position="dodge")+
  geom_text(aes(x=fct_reorder(TIPO_INMUEBLE,N,.desc=TRUE),y=N,label=LABELS),vjust=-0.6,position = position_dodge(width = 1))+ylab("N?mero de ofertas")+xlab("AÑO")+
  scale_y_continuous(labels=labels)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


base_filtrada[,.N,by=.(AÑO,TIPO_INMUEBLE)][order(AÑO,TIPO_INMUEBLE),LABELS:=labels(N)][N>2000] %>% 
  ggplot()+geom_col(aes(x=fct_reorder(TIPO_INMUEBLE,N,.desc=TRUE),y=N),fill="red",position="dodge")+
  geom_text(aes(x=fct_reorder(TIPO_INMUEBLE,N,.desc=TRUE),y=N,label=LABELS),vjust=-0.6,position = position_dodge(width = 1))+ylab("N?mero de ofertas")+xlab("AÑO")+
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE))+facet_wrap(~AÑO,scales="free")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


base_filtrada[TIPO_INMUEBLE %in% c("APARTAMENTO","CASA"),.N,by=.(TIPO_INMUEBLE,ESTRATO)][order(TIPO_INMUEBLE,ESTRATO),LABELS:=labels(N)] %>% 
  ggplot()+geom_col(aes(x=ESTRATO,y=N),fill="red",position="dodge")+
  geom_text(aes(x=ESTRATO,y=N,label=LABELS),vjust=-0.6,position = position_dodge(width = 1))+ylab("N?mero de ofertas")+xlab("ESTRATO")+
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE))+
  scale_x_continuous(breaks=0:6)+
  facet_wrap(~TIPO_INMUEBLE,scales="free")



