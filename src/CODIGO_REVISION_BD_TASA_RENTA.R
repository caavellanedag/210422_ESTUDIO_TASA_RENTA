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
p_load(tidyverse,openxlsx,here,haven,data.table)
# cat("labels=function(x)format(x, big.mark = \".\", scientific = FALSE) ",
#     file = "src/Funciones.R")
source("src/Funciones.R")

table(base$OUTLAYER)
sum( is.na(base$PRECIO) | base$PRECIO==0)
resumen<-base %>% group_by(OUTLAYER) %>% count() %>% ungroup()
base <- fread("input/BASE_HIST_17_20_CONSOLI_FINAL_25022021.tab", header = T, sep = "\t")
base$PRECIO <- as.numeric(as.character(str_replace_all(base$PRECIO,c("\\$"="",","=""))))
base$IDENTIFICADOR <- as.character(base$IDENTIFICADOR)

base <- base %>% mutate_at(c("IDENTIFICADOR","CODIGO_UPZ","NOMBRE_UPZ","CODIGO_BARRIO","NOMBRE_BARRIO",
                           "NOMBRE_LOCALIDAD","CODIGO_LOCALIDAD","BARMANPRE"),as.character) 
base$FECHA <- as.Date(base$FECHA,format="%d/%m/%Y")

list_df<-list(base[FECHA>="2020-03-24" & FECHA<="2020-11-13",.N,by=.(NOMBRE_FUENTE)],
  base[FECHA>="2020-03-24" & FECHA<="2020-11-13",.N,by=.(TIPO_INMUEBLE,CLASE_PREDIO)],
base[FECHA>="2020-03-24" & FECHA<="2020-11-13",.N,by=.(TIPO_INMUEBLE)],
base[FECHA>="2020-03-24" & FECHA<="2020-11-13",.N,by=.(CLASE_PREDIO)],
base[FECHA>="2020-03-24" & FECHA<="2020-11-13" & TIPO_INMUEBLE %in% c("APARTAMENTO","CASA"),.N,by=.(ESTRATO)])


labels<-c("NOMBRE_FUENTE","TIPO_CLASE_PREDIO","TIPO","CLASE","ESTRATO_SOLO_APT_CASA")
wb<-createWorkbook("Camilo Avellaneda")
map2(list_df,labels,~Sheet(.x,.y))
saveWorkbook(wb,"output/210426_RESUMEN_OFERTAS_MARZO_NOVIEMBRE_2020.xlsx",overwrite=TRUE)

n_fail <- sum(base$IDENTIFICADOR=="" | is.na(base$IDENTIFICADOR) | is.na(base$PRECIO) | base$BARMANPRE=="" | is.na(base$BARMANPRE))
base_filtrada <- base[!(base$IDENTIFICADOR=="" | is.na(base$IDENTIFICADOR) | is.na(base$PRECIO) | base$BARMANPRE=="" | is.na(base$BARMANPRE)),]
base_filtrada <- as.data.table(base_filtrada)
sum(is.na(base$BARMANPRE))

N_AC <- sum(is.na(base_filtrada$AREA_CONSTRUIDA))
N_AT <- sum((base_filtrada$TIPO_INMUEBLE!="APARTAMENTO" & is.na(base_filtrada$AREA_DE_TERRENO)))
base_filtrada <- base_filtrada[!(base_filtrada$TIPO_INMUEBLE!="APARTAMENTO" & is.na(base_filtrada$AREA_DE_TERRENO)) & !is.na(base_filtrada$AREA_CONSTRUIDA),]



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

table(base$NOMBRE_FUENTE)

base_filtrada <- as.data.table(base_filtrada)
base_filtrada <- base_filtrada[TIPO_INMUEBLE %in% c("APARTAMENTO","CASA")]
