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
p_load(tidyverse, openxlsx, here, haven, data.table, RecordLinkage, IAcsSPCR, qcc,
       matrixcalc, nlme, haven, sf)
options(scipen = 999)
# cat("labels=function(x)format(x, big.mark = \".\", scientific = FALSE) ",
#     file = "src/Funciones.R")
source("src/Funciones.R")
source("src/Genera_mapa_sectores_Bta.R")
# table(base$OUTLAYER)
# sum( is.na(base$PRECIO) | base$PRECIO==0)
# resumen<-base %>% group_by(OUTLAYER) %>% count() %>% ungroup()

base <- fread("input/BASE_HIST_RENTAS.tab", header = T, sep = "\t")
resumen_base_catastral <- read_sas("input/base_predial_resumen_17_20.sas7bdat")

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
#base_filtrada <- base_filtrada[NUMERO_ALCOBAS > 0]
#base_filtrada <- base_filtrada[NUMERO_BA_OS > 0]
base_filtrada <- base_filtrada[AREA_CONSTRUIDA > 0]
base_filtrada <- base_filtrada[ESTRATO > 0]
#base_filtrada <- base_filtrada[(AREA_DE_TERRENO>0 & CLASE_PREDIO == "N") | (CLASE_PREDIO != "N")]
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



## Solamente para generar la ilustración de las cartas de control

ilustracion_qcc <- lista_2[[1]] %>%
  filter(CODIGO_BARRIO_NEW == lista_2[[1]]$CODIGO_BARRIO_NEW[1]) %>% 
  make_control_chart()

##


lista_separada <- df_clean[, -"CODIGO_BARRIO_NEW"] %>% split(.$key) 
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


df_clean_2 <- df_clean[,c("CODIGO_BARRIO", "NOMBRE_BARRIO", "CODIGO_LOCALIDAD", "NOMBRE_LOCALIDAD",
            "CLASE_PREDIO", "ESTRATO", "AÑO", "TIPO_OFERTA", "VALOR_M2_INTEGRAL",
            "VALOR_M2_TERRENO", "PRECIO", "ANTIGUEDAD_INMUEBLE", "NUMERO_GARAJES", "NUMERO_ALCOBAS",
            "AREA_CONSTRUIDA", "AREA_DE_TERRENO")]

df_clean_3 <- df_clean_2[, list(N = .N, PRECIO_MEAN =  mean(VALOR_M2_INTEGRAL)),
           by = .(CODIGO_BARRIO, NOMBRE_BARRIO, CODIGO_LOCALIDAD, NOMBRE_LOCALIDAD,
                  CLASE_PREDIO, ESTRATO, AÑO, TIPO_OFERTA)]

df_clean_4 <- df_clean_3 %>% data.table::dcast(formula = CODIGO_BARRIO + NOMBRE_BARRIO + CODIGO_LOCALIDAD + 
                       NOMBRE_LOCALIDAD + CLASE_PREDIO + ESTRATO + AÑO ~ TIPO_OFERTA, 
                     value.var = c("PRECIO_MEAN", "N"), fun.aggregate = mean, fill = NA)


df_clean_5 <- merge(df_clean_4, resumen_base_catastral, 
      by.x = c("CODIGO_BARRIO", "CLASE_PREDIO", "ESTRATO", "AÑO"),
      by.y = c("CODIGO_BARRIO", "CLASE_PREDIO", "CODIGO_ESTRATO", "VIGENCIA"))

df_clean_5 <- df_clean_5[, PRECIO_MEAN_VENTA := ifelse(is.na(PRECIO_MEAN_VENTA), VALOR_INTEGRAL, PRECIO_MEAN_VENTA)]

#df_clean_4$VENTA %>% is.na() %>% sum()
#df_clean_4$ARRIENDO %>% is.na() %>% sum()


df_clean_6 <- df_clean_5[!is.na(PRECIO_MEAN_ARRIENDO) | N_VENTA > 5]

df_input_1 <- df_clean[TIPO_OFERTA == "ARRIENDO",
                       .(MEAN_NEW_ARRIENDO = mean(VALOR_M2_INTEGRAL)),
         by = .(CODIGO_BARRIO, CLASE_PREDIO, AÑO, ESTRATO)]

df_input_2 <- df_clean[TIPO_OFERTA == "ARRIENDO",
                       .(MEAN_ARRIENDO_LOC = mean(VALOR_M2_INTEGRAL)),
                       by = .(CODIGO_LOCALIDAD, CLASE_PREDIO, AÑO, ESTRATO)]

df_clean_7 <- df_clean_6 %>% merge(df_input_1, by = c("CODIGO_BARRIO", "CLASE_PREDIO", "AÑO", "ESTRATO"), all.x = TRUE) %>% 
  merge(df_input_2, by = c("CODIGO_LOCALIDAD", "CLASE_PREDIO", "AÑO", "ESTRATO"), all.x = TRUE)

df_clean_8 <- df_clean_7[, PRECIO_MEAN_ARRIENDO := ifelse(is.na(PRECIO_MEAN_ARRIENDO) & is.na(MEAN_NEW_ARRIENDO),
                                         MEAN_ARRIENDO_LOC, ifelse(is.na(PRECIO_MEAN_ARRIENDO), 
                                                                 MEAN_NEW_ARRIENDO, PRECIO_MEAN_ARRIENDO))]

df_clean_9 <- df_clean_8[PRECIO_MEAN_VENTA/PRECIO_MEAN_ARRIENDO >= 100 & PRECIO_MEAN_VENTA/PRECIO_MEAN_ARRIENDO < 300]
# df_clean_8 <- df_clean_8[, ESTRATO := factor(ESTRATO)]
# df_clean_8 <- df_clean_8[, AÑO := factor(AÑO)]

df_clean_9[, c("ESTRATO", "AÑO") := list(factor(ESTRATO), factor(AÑO))]
df_clean_apto <- df_clean_9[CLASE_PREDIO == "P"]
df_clean_casa <- df_clean_9[CLASE_PREDIO == "N"]

model_matrix <- model.matrix(PRECIO_MEAN_VENTA ~  ESTRATO + CODIGO_LOCALIDAD + AÑO, data = df_clean_apto) 
df_clean_apto_2 <- df_clean_apto %>% cbind(model_matrix)
paste0(paste0("PRECIO_MEAN_ARRIENDO:",str_subset(names(df_clean_apto_2), "ESTRATO\\d|CODIGO_LOCALIDAD\\d|AÑO\\d")),
       collapse = " + ")


df_clean_apto_2 %>% names()

fit <- lm(data = df_clean_apto, PRECIO_MEAN_VENTA ~ PRECIO_MEAN_ARRIENDO + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD +
            PRECIO_MEAN_ARRIENDO:ESTRATO + 
            PRECIO_MEAN_ARRIENDO:AÑO -1)


fit_lme <- lme(PRECIO_MEAN_VENTA ~ -1 + PRECIO_MEAN_ARRIENDO + 
                 PRECIO_MEAN_ARRIENDO:ESTRATO2 + PRECIO_MEAN_ARRIENDO:ESTRATO3 + 
                 PRECIO_MEAN_ARRIENDO:ESTRATO4 + PRECIO_MEAN_ARRIENDO:ESTRATO5 + 
                 PRECIO_MEAN_ARRIENDO:ESTRATO6 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD10 + 
                 PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD11 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD12 + 
                 PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD13 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD14 + 
                 PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD15 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD16 + 
                 PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD17 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD18 + 
                 PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD19 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD2 + 
                 PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD3 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD4 + 
                 PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD5 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD6 + 
                 PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD7 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD8 + 
                 PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD9 + PRECIO_MEAN_ARRIENDO:AÑO2018 + 
                 PRECIO_MEAN_ARRIENDO:AÑO2019 + PRECIO_MEAN_ARRIENDO:AÑO2020,
               data = df_clean_apto_2, 
               random = ~ -1 + PRECIO_MEAN_ARRIENDO|CODIGO_BARRIO)

fit_lmer <- lmer(PRECIO_MEAN_VENTA ~ -1 + PRECIO_MEAN_ARRIENDO + 
                   PRECIO_MEAN_ARRIENDO:ESTRATO2 + PRECIO_MEAN_ARRIENDO:ESTRATO3 + 
                   PRECIO_MEAN_ARRIENDO:ESTRATO4 + PRECIO_MEAN_ARRIENDO:ESTRATO5 + 
                   PRECIO_MEAN_ARRIENDO:ESTRATO6 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD10 + 
                   PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD11 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD12 + 
                   PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD13 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD14 + 
                   PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD15 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD16 + 
                   PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD17 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD18 + 
                   PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD19 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD2 + 
                   PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD3 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD4 + 
                   PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD5 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD6 + 
                   PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD7 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD8 + 
                   PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD9 + PRECIO_MEAN_ARRIENDO:AÑO2018 + 
                   PRECIO_MEAN_ARRIENDO:AÑO2019 + PRECIO_MEAN_ARRIENDO:AÑO2020 + 
                   (1 | CODIGO_BARRIO), data = df_clean_apto_2)



inf <- influence(fit_lmer, obs = TRUE)
dfbetas <- dfbetas(inf) 
check_dfbetas <- dfbetas > 6/sqrt(nrow(df_clean_apto_2))

Cook <- cooks.distance(inf)

df_clean_apto_2 <- df_clean_apto %>% cbind(model_matrix)

df_clean_apto_2 <- df_clean_apto_2 %>% cbind(data.frame(Cook = Cook, Dfbetas = rowSums(check_dfbetas)))

df_clean_apto_3 <- df_clean_apto_2[(Cook < 4 * ncol(dfbetas(inf))/nrow(df_clean_apto_2))]

df_clean_apto_3 <- df_clean_apto_3[Dfbetas == 0]


fit_2 <- lm(data = df_clean_apto_3, PRECIO_MEAN_VENTA ~ PRECIO_MEAN_ARRIENDO + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD +
            PRECIO_MEAN_ARRIENDO:ESTRATO + 
            PRECIO_MEAN_ARRIENDO:AÑO -1)


fit_lme_2 <- lme(PRECIO_MEAN_VENTA ~ -1 + PRECIO_MEAN_ARRIENDO + 
                 PRECIO_MEAN_ARRIENDO:ESTRATO2 + PRECIO_MEAN_ARRIENDO:ESTRATO3 + 
                 PRECIO_MEAN_ARRIENDO:ESTRATO4 + PRECIO_MEAN_ARRIENDO:ESTRATO5 + 
                 PRECIO_MEAN_ARRIENDO:ESTRATO6 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD10 + 
                 PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD11 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD12 + 
                 PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD13 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD14 + 
                 PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD15 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD16 + 
                 PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD17 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD18 + 
                 PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD19 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD2 + 
                 PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD3 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD4 + 
                 PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD5 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD6 + 
                 PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD7 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD8 + 
                 PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD9 + PRECIO_MEAN_ARRIENDO:AÑO2018 + 
                 PRECIO_MEAN_ARRIENDO:AÑO2019 + PRECIO_MEAN_ARRIENDO:AÑO2020,
               data = df_clean_apto_3, 
               random = ~ -1 + PRECIO_MEAN_ARRIENDO|CODIGO_BARRIO)

ranef_df <- ranef(fit_lme_2) %>% as.data.frame() %>% rownames_to_column() %>% as.data.table()
ranef_df <- ranef_df[, -c("rowname", "PRECIO_MEAN_ARRIENDO")]

df_clean_apto_4 <- df_clean_apto_3[, c("Fitted", "Residuals") := list(fitted(fit_lme_2), residuals(fit_lme_2))]

plot_fitted_residuals <-  df_clean_apto_4 %>% 
  ggplot(aes(Fitted, Residuals)) + geom_point(colour = "red4", alpha = 0.5, shape = 18, size = 3) + 
  theme_bw() + xlab("Valores ajustados del modelo") + ylab("Residuales del modelo")+
  scale_y_continuous(labels = labels, limits = c(-2*10e5, 2*10e5)) + 
  scale_x_continuous(labels = labels)+
  geom_hline(yintercept = 0, colour = "black", linetype = "twodash", size = 1.5)
    

plot_boxplot_estrato_vigencia <- df_clean_apto_4 %>% 
    ggplot() + geom_boxplot(aes(ESTRATO, Residuals), fill = "darkorange", alpha = 0.8) +
    facet_wrap(~AÑO, scales = "free") + 
    geom_hline(yintercept = 0, colour = "darkred", size = 1.3, linetype = "twodash")+
    theme_bw() + xlab("Estrato") + ylab("Residuales del modelo")
 
plot_normal_residuals <- ggplot(df_clean_apto_4) + 
  geom_histogram( aes(Residuals), fill = "red2", alpha = 0.8, colour = "black") +
  theme_bw() + xlab("Residuales del modelo") + ylab("Frecuencia")
plot_normal_ranef <- ranef_df %>% ggplot() + 
  geom_histogram(aes(RAN_EFFECT), fill = "red2", alpha = 0.8, colour = "black") +
  theme_bw() + xlab("Efectos aleatorios") + ylab("Frecuencia")
    



fit_lme_to_print <- lme(PRECIO_MEAN_VENTA ~ -1 + PRECIO_MEAN_ARRIENDO,
                        data = df_clean_apto_3, 
                 random = ~ -1 + PRECIO_MEAN_ARRIENDO|CODIGO_BARRIO)

fit_lme_to_print_2 <- lme(PRECIO_MEAN_VENTA ~ -1 + PRECIO_MEAN_ARRIENDO +
                          PRECIO_MEAN_ARRIENDO:ESTRATO2 + PRECIO_MEAN_ARRIENDO:ESTRATO3 + 
                          PRECIO_MEAN_ARRIENDO:ESTRATO4 + PRECIO_MEAN_ARRIENDO:ESTRATO5 + 
                          PRECIO_MEAN_ARRIENDO:ESTRATO6,
                        data = df_clean_apto_3, 
                        random = ~ -1 + PRECIO_MEAN_ARRIENDO|CODIGO_BARRIO)
fit_lme_to_print_3 <- lme(PRECIO_MEAN_VENTA ~ -1 + PRECIO_MEAN_ARRIENDO +
                            PRECIO_MEAN_ARRIENDO:ESTRATO2 + PRECIO_MEAN_ARRIENDO:ESTRATO3 + 
                            PRECIO_MEAN_ARRIENDO:ESTRATO4 + PRECIO_MEAN_ARRIENDO:ESTRATO5 + 
                            PRECIO_MEAN_ARRIENDO:ESTRATO6 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD10 + 
                            PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD11 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD12 + 
                            PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD13 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD14 + 
                            PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD15 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD16 + 
                            PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD17 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD18 + 
                            PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD19 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD2 + 
                            PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD3 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD4 + 
                            PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD5 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD6 + 
                            PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD7 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD8 + 
                            PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD9,
                          data = df_clean_apto_3, 
                          random = ~ -1 + PRECIO_MEAN_ARRIENDO|CODIGO_BARRIO)

fit_lme_to_print_4 <- lme(PRECIO_MEAN_VENTA ~ -1 + PRECIO_MEAN_ARRIENDO + 
                   PRECIO_MEAN_ARRIENDO:ESTRATO2 + PRECIO_MEAN_ARRIENDO:ESTRATO3 + 
                   PRECIO_MEAN_ARRIENDO:ESTRATO4 + PRECIO_MEAN_ARRIENDO:ESTRATO5 + 
                   PRECIO_MEAN_ARRIENDO:ESTRATO6 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD10 + 
                   PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD11 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD12 + 
                   PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD13 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD14 + 
                   PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD15 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD16 + 
                   PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD17 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD18 + 
                   PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD19 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD2 + 
                   PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD3 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD4 + 
                   PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD5 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD6 + 
                   PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD7 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD8 + 
                   PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD9 + PRECIO_MEAN_ARRIENDO:AÑO2018 + 
                   PRECIO_MEAN_ARRIENDO:AÑO2019 + PRECIO_MEAN_ARRIENDO:AÑO2020,
                 data = df_clean_apto_3, 
                 random = ~ -1 + PRECIO_MEAN_ARRIENDO|CODIGO_BARRIO)

anova(fit_lme_to_print, fit_lme_to_print_2, fit_lme_to_print_3, fit_lme_to_print_4)


df_fixed_coef <- data.frame(Coeficiente = summary(fit_lme_2)$coef$fixed) %>%
  rownames_to_column() %>% as.data.table()

df_fixed_coef <- df_fixed_coef[, c("ESTRATO", "CODIGO_LOCALIDAD", "AÑO", "Intercepto") :=
list(str_replace_all(str_extract(rowname, "ESTRATO\\d"), "ESTRATO", ""),
     str_replace_all(str_extract(rowname, "CODIGO_LOCALIDAD\\d+"), "CODIGO_LOCALIDAD", ""),
     str_replace_all(str_extract(rowname, "AÑO\\d+"), "AÑO", ""),
     ifelse(rowname == "PRECIO_MEAN_ARRIENDO", 1, NA))]



Tabla_apto_final <- df_clean_apto_3[, c("CODIGO_BARRIO", "CODIGO_LOCALIDAD", "AÑO", "ESTRATO")] %>% unique() %>% 
  mutate(Intercepto = 1) %>% 
  merge(ranef_df, by = "CODIGO_BARRIO", all.x = TRUE) %>% 
  merge(df_fixed_coef[, Coef_all := Coeficiente][, c("Intercepto", "Coef_all")], by = "Intercepto", all.x = TRUE) %>% 
  merge(df_fixed_coef[, Coef_est := Coeficiente][, c("ESTRATO", "Coef_est")], by = "ESTRATO", all.x = TRUE) %>% 
  merge(df_fixed_coef[, Coef_loc := Coeficiente][, c("CODIGO_LOCALIDAD", "Coef_loc")], by = "CODIGO_LOCALIDAD", all.x = TRUE) %>%
  merge(df_fixed_coef[, Coef_year := Coeficiente][, c("AÑO", "Coef_year")], by = "AÑO", all.x = TRUE)  %>% 
  mutate_at(c("Coef_all", "Coef_est", "Coef_loc", "Coef_year"), replace_na_zero) %>% 
  mutate(TASA_RENTA = Coef_all + Coef_est + Coef_loc + Coef_year + RAN_EFFECT) %>% arrange(TASA_RENTA)


df_clean_apto_3[CODIGO_LOCALIDAD == 16] %>%
  #inner_join(Tabla_apto_final[ TASA_RENTA < 100, c("CODIGO_BARRIO", "ESTRATO", "AÑO")]) %>% 
  dplyr::select(PRECIO_MEAN_ARRIENDO, PRECIO_MEAN_VENTA, N_ARRIENDO, N_VENTA, Fitted, Residuals) %>% 
  mutate(TASA_RENTA = PRECIO_MEAN_VENTA/PRECIO_MEAN_ARRIENDO) %>% View()





model_matrix <- model.matrix(PRECIO_MEAN_VENTA ~  ESTRATO + CODIGO_LOCALIDAD + AÑO, data = df_clean_casa) 
df_clean_casa_2 <- df_clean_casa %>% cbind(model_matrix)
paste0(paste0("PRECIO_MEAN_ARRIENDO:",str_subset(names(df_clean_casa_2), "ESTRATO\\d|CODIGO_LOCALIDAD\\d|AÑO\\d")),
       collapse = " + ")


fit_casa <- lm(data = df_clean_casa, PRECIO_MEAN_VENTA ~ PRECIO_MEAN_ARRIENDO + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD +
            PRECIO_MEAN_ARRIENDO:ESTRATO + 
            PRECIO_MEAN_ARRIENDO:AÑO -1)


fit_lme_casa <- lme(PRECIO_MEAN_VENTA ~ -1 + PRECIO_MEAN_ARRIENDO + 
                 PRECIO_MEAN_ARRIENDO:ESTRATO2 + PRECIO_MEAN_ARRIENDO:ESTRATO3 + 
                 PRECIO_MEAN_ARRIENDO:ESTRATO4 + PRECIO_MEAN_ARRIENDO:ESTRATO5 + 
                 PRECIO_MEAN_ARRIENDO:ESTRATO6 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD10 + 
                 PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD11 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD12 + 
                 PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD13 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD14 + 
                 PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD15 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD16 + 
                 PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD17 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD18 + 
                 PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD19 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD2 + 
                 PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD3 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD4 + 
                 PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD5 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD6 + 
                 PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD7 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD8 + 
                 PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD9 + PRECIO_MEAN_ARRIENDO:AÑO2018 + 
                 PRECIO_MEAN_ARRIENDO:AÑO2019 + PRECIO_MEAN_ARRIENDO:AÑO2020,
               data = df_clean_casa_2, 
               random = ~ -1 + PRECIO_MEAN_ARRIENDO|CODIGO_BARRIO)

fit_lmer_casa <- lmer(PRECIO_MEAN_VENTA ~ -1 + PRECIO_MEAN_ARRIENDO + 
                   PRECIO_MEAN_ARRIENDO:ESTRATO2 + PRECIO_MEAN_ARRIENDO:ESTRATO3 + 
                   PRECIO_MEAN_ARRIENDO:ESTRATO4 + PRECIO_MEAN_ARRIENDO:ESTRATO5 + 
                   PRECIO_MEAN_ARRIENDO:ESTRATO6 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD10 + 
                   PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD11 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD12 + 
                   PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD13 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD14 + 
                   PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD15 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD16 + 
                   PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD17 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD18 + 
                   PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD19 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD2 + 
                   PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD3 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD4 + 
                   PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD5 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD6 + 
                   PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD7 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD8 + 
                   PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD9 + PRECIO_MEAN_ARRIENDO:AÑO2018 + 
                   PRECIO_MEAN_ARRIENDO:AÑO2019 + PRECIO_MEAN_ARRIENDO:AÑO2020 + 
                   (1 | CODIGO_BARRIO), data = df_clean_casa_2)



inf <- influence(fit_lmer_casa, obs = TRUE)
dfbetas <- dfbetas(inf) 
check_dfbetas <- dfbetas > 6/sqrt(nrow(df_clean_casa_2))

Cook <- cooks.distance(inf)

df_clean_casa_2 <- df_clean_casa %>% cbind(model_matrix)

df_clean_casa_2 <- df_clean_casa_2 %>% cbind(data.frame(Cook = Cook, Dfbetas = rowSums(check_dfbetas)))

df_clean_casa_3 <- df_clean_casa_2[(Cook < 4 * ncol(dfbetas(inf))/nrow(df_clean_apto_2))]

df_clean_casa_3 <- df_clean_casa_3[Dfbetas == 0]


fit_2_casa <- lm(data = df_clean_casa_3, PRECIO_MEAN_VENTA ~ PRECIO_MEAN_ARRIENDO + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD +
              PRECIO_MEAN_ARRIENDO:ESTRATO + 
              PRECIO_MEAN_ARRIENDO:AÑO -1)


fit_lme_2_casa <- lme(PRECIO_MEAN_VENTA ~ -1 + PRECIO_MEAN_ARRIENDO + 
                   PRECIO_MEAN_ARRIENDO:ESTRATO2 + PRECIO_MEAN_ARRIENDO:ESTRATO3 + 
                   PRECIO_MEAN_ARRIENDO:ESTRATO4 + PRECIO_MEAN_ARRIENDO:ESTRATO5 + 
                   PRECIO_MEAN_ARRIENDO:ESTRATO6 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD10 + 
                   PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD11 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD12 + 
                   PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD13 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD14 + 
                   PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD15 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD16 + 
                   PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD17 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD18 + 
                   PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD19 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD2 + 
                   PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD3 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD4 + 
                   PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD5 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD6 + 
                   PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD7 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD8 + 
                   PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD9 + PRECIO_MEAN_ARRIENDO:AÑO2018 + 
                   PRECIO_MEAN_ARRIENDO:AÑO2019 + PRECIO_MEAN_ARRIENDO:AÑO2020,
                 data = df_clean_casa_3, 
                 random = ~ -1 + PRECIO_MEAN_ARRIENDO|CODIGO_BARRIO)

ranef_df_casa <- ranef(fit_lme_2_casa) %>% as.data.frame() %>% rownames_to_column() %>% as.data.table()
ranef_df_casa <- ranef_df_casa[, c("CODIGO_BARRIO", "RAN_EFFECT") := list(rowname, PRECIO_MEAN_ARRIENDO)]
ranef_df_casa <- ranef_df_casa[, -c("rowname", "PRECIO_MEAN_ARRIENDO")]

df_clean_casa_4 <- df_clean_casa_3[, c("Fitted", "Residuals") := list(fitted(fit_lme_2_casa), residuals(fit_lme_2_casa))]

plot_fitted_residuals_casa <-  df_clean_casa_4 %>% 
  ggplot(aes(Fitted, Residuals)) + geom_point(colour = "red4", alpha = 0.5, shape = 18, size = 3) + 
  theme_bw() + xlab("Valores ajustados del modelo") + ylab("Residuales del modelo")+
  scale_y_continuous(labels = labels, limits = c(-2*10e5, 2*10e5)) + 
  scale_x_continuous(labels = labels)+
  geom_hline(yintercept = 0, colour = "black", linetype = "twodash", size = 1.5)


plot_boxplot_estrato_vigencia_casa <- df_clean_casa_4 %>% 
  ggplot() + geom_boxplot(aes(ESTRATO, Residuals), fill = "darkorange", alpha = 0.8) +
  facet_wrap(~AÑO, scales = "free") + 
  geom_hline(yintercept = 0, colour = "darkred", size = 1.3, linetype = "twodash")+
  theme_bw() + xlab("Estrato") + ylab("Residuales del modelo")

plot_normal_residuals_casa <- ggplot(df_clean_casa_4) + 
  geom_histogram( aes(Residuals), fill = "red2", alpha = 0.8, colour = "black") +
  theme_bw() + xlab("Residuales del modelo") + ylab("Frecuencia")
plot_normal_ranef_casa <- ranef_df_casa %>% ggplot() + 
  geom_histogram(aes(RAN_EFFECT), fill = "red2", alpha = 0.8, colour = "black") +
  theme_bw() + xlab("Efectos aleatorios") + ylab("Frecuencia")




fit_lme_to_print_casa <- lme(PRECIO_MEAN_VENTA ~ -1 + PRECIO_MEAN_ARRIENDO,
                        data = df_clean_casa_3, 
                        random = ~ -1 + PRECIO_MEAN_ARRIENDO|CODIGO_BARRIO)

fit_lme_to_print_2_casa <- lme(PRECIO_MEAN_VENTA ~ -1 + PRECIO_MEAN_ARRIENDO +
                            PRECIO_MEAN_ARRIENDO:ESTRATO2 + PRECIO_MEAN_ARRIENDO:ESTRATO3 + 
                            PRECIO_MEAN_ARRIENDO:ESTRATO4 + PRECIO_MEAN_ARRIENDO:ESTRATO5 + 
                            PRECIO_MEAN_ARRIENDO:ESTRATO6,
                          data = df_clean_casa_3, 
                          random = ~ -1 + PRECIO_MEAN_ARRIENDO|CODIGO_BARRIO)
fit_lme_to_print_3_casa <- lme(PRECIO_MEAN_VENTA ~ -1 + PRECIO_MEAN_ARRIENDO +
                            PRECIO_MEAN_ARRIENDO:ESTRATO2 + PRECIO_MEAN_ARRIENDO:ESTRATO3 + 
                            PRECIO_MEAN_ARRIENDO:ESTRATO4 + PRECIO_MEAN_ARRIENDO:ESTRATO5 + 
                            PRECIO_MEAN_ARRIENDO:ESTRATO6 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD10 + 
                            PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD11 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD12 + 
                            PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD13 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD14 + 
                            PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD15 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD16 + 
                            PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD17 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD18 + 
                            PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD19 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD2 + 
                            PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD3 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD4 + 
                            PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD5 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD6 + 
                            PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD7 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD8 + 
                            PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD9,
                          data = df_clean_casa_3, 
                          random = ~ -1 + PRECIO_MEAN_ARRIENDO|CODIGO_BARRIO)

fit_lme_to_print_4_casa <- lme(PRECIO_MEAN_VENTA ~ -1 + PRECIO_MEAN_ARRIENDO + 
                            PRECIO_MEAN_ARRIENDO:ESTRATO2 + PRECIO_MEAN_ARRIENDO:ESTRATO3 + 
                            PRECIO_MEAN_ARRIENDO:ESTRATO4 + PRECIO_MEAN_ARRIENDO:ESTRATO5 + 
                            PRECIO_MEAN_ARRIENDO:ESTRATO6 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD10 + 
                            PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD11 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD12 + 
                            PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD13 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD14 + 
                            PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD15 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD16 + 
                            PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD17 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD18 + 
                            PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD19 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD2 + 
                            PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD3 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD4 + 
                            PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD5 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD6 + 
                            PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD7 + PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD8 + 
                            PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD9 + PRECIO_MEAN_ARRIENDO:AÑO2018 + 
                            PRECIO_MEAN_ARRIENDO:AÑO2019 + PRECIO_MEAN_ARRIENDO:AÑO2020,
                          data = df_clean_casa_3, 
                          random = ~ -1 + PRECIO_MEAN_ARRIENDO|CODIGO_BARRIO)

anova(fit_lme_to_print_casa, fit_lme_to_print_2_casa, fit_lme_to_print_3_casa, fit_lme_to_print_4_casa)


df_fixed_coef_casa <- data.frame(Coeficiente = summary(fit_lme_2)$coef$fixed) %>%
  rownames_to_column() %>% as.data.table()

df_fixed_coef_casa <- df_fixed_coef_casa[, c("ESTRATO", "CODIGO_LOCALIDAD", "AÑO", "Intercepto") :=
                                 list(str_replace_all(str_extract(rowname, "ESTRATO\\d"), "ESTRATO", ""),
                                      str_replace_all(str_extract(rowname, "CODIGO_LOCALIDAD\\d+"), "CODIGO_LOCALIDAD", ""),
                                      str_replace_all(str_extract(rowname, "AÑO\\d+"), "AÑO", ""),
                                      ifelse(rowname == "PRECIO_MEAN_ARRIENDO", 1, NA))]



Tabla_casa_final <- df_clean_casa_3[, c("CODIGO_BARRIO", "CODIGO_LOCALIDAD", "AÑO", "ESTRATO")] %>% unique() %>% 
  mutate(Intercepto = 1) %>% 
  merge(ranef_df_casa, by = "CODIGO_BARRIO", all.x = TRUE) %>% 
  merge(df_fixed_coef_casa[, Coef_all := Coeficiente][, c("Intercepto", "Coef_all")], by = "Intercepto", all.x = TRUE) %>% 
  merge(df_fixed_coef_casa[, Coef_est := Coeficiente][, c("ESTRATO", "Coef_est")], by = "ESTRATO", all.x = TRUE) %>% 
  merge(df_fixed_coef_casa[, Coef_loc := Coeficiente][, c("CODIGO_LOCALIDAD", "Coef_loc")], by = "CODIGO_LOCALIDAD", all.x = TRUE) %>%
  merge(df_fixed_coef_casa[, Coef_year := Coeficiente][, c("AÑO", "Coef_year")], by = "AÑO", all.x = TRUE)  %>% 
  mutate_at(c("Coef_all", "Coef_est", "Coef_loc", "Coef_year"), replace_na_zero) %>% 
  mutate(TASA_RENTA = Coef_all + Coef_est + Coef_loc + Coef_year + RAN_EFFECT) %>% arrange(TASA_RENTA)




Tabla_estrato_casa <- Tabla_casa_final[, c("AÑO", "ESTRATO", "Coef_all", "Coef_est", "Coef_year")
                 ][, "TASA_RENTA" := Coef_all + Coef_est + Coef_year] %>% unique()

Tabla_estrato_casa[order(ESTRATO, AÑO)]


Tabla_estrato_apto <- Tabla_apto_final[, c("AÑO", "ESTRATO", "Coef_all", "Coef_est", "Coef_year")
][, "TASA_RENTA" := Coef_all + Coef_est + Coef_year] %>% unique()

Tabla_estrato_apto[order(ESTRATO, AÑO)]



df_clean_casa_3 %>% ggplot(aes(PRECIO_MEAN_ARRIENDO, PRECIO_MEAN_VENTA, color = AÑO)) + 
  geom_point() +
  facet_wrap(~ ESTRATO, scales = "free_x") +
  geom_smooth(method = "lm", se = F) +
  xlab("Promedio de arriendo") + ylab("Promedio de venta") + 
  scale_x_continuous(labels = labels) +
  scale_y_continuous(labels = labels) +
  theme_bw() +
  scale_color_brewer(palette = "Set1")

df_clean_apto_3 %>% ggplot(aes(PRECIO_MEAN_ARRIENDO, PRECIO_MEAN_VENTA, color = AÑO)) + 
  geom_point() +
  facet_wrap(~ ESTRATO, scales = "free_x") +
  geom_smooth(method = "lm", se = F) +
  xlab("Promedio de arriendo") + ylab("Promedio de venta") + 
  scale_x_continuous(labels = labels) +
  scale_y_continuous(labels = labels) +
  theme_bw() +
  scale_color_brewer(palette = "Set1")
  


Sectores_shp <- sf::st_read("input/Base_Dic2020/Sectores/Sectores1712.shp")
Sectores_shp <- Sectores_shp %>% as('Spatial')  %>% 
  spTransform(CRS("+proj=longlat +datum=WGS84")) 

Sectores_shp_sf <- st_as_sf(Sectores_shp)
Sectores_shp_sf_new <- st_as_sf(Sectores_shp)
Sectores_shp_sf <- merge(Sectores_shp_sf, Tabla_apto_final[AÑO == 2020],
                         by.x = "SECSECT_ID", by.y = "CODIGO_BARRIO", all.x = FALSE)
Sectores_shp_sf <- Sectores_shp_sf[substring(Sectores_shp_sf$SECSECT_ID, 1, 1) == "0",]


Sectores_shp_sf %>% filter(AÑO == "2020") %>%
  ggplot() + geom_sf(aes(fill = TASA_RENTA), color = "white") + 
  facet_wrap(~ESTRATO) + theme_bw()+
  scale_fill_viridis_c()




## Se va a realizar la ejecución del modelo para apartamentos 

fit <- lm(data = df_clean_apto, VENTA ~ ARRIENDO + ARRIENDO:CODIGO_LOCALIDAD +
                             ARRIENDO:ESTRATO + 
                             ARRIENDO:AÑO -1)

fit_lme <- lme(VENTA ~ -1 + ARRIENDO +ARRIENDO:ESTRATO2 +
                 ARRIENDO:ESTRATO3 + ARRIENDO:ESTRATO4 + 
                 ARRIENDO:ESTRATO5 + ARRIENDO:ESTRATO6 + 
                 ARRIENDO:CODIGO_LOCALIDAD10 + ARRIENDO:CODIGO_LOCALIDAD11 +
                 ARRIENDO:CODIGO_LOCALIDAD12 + ARRIENDO:CODIGO_LOCALIDAD13 +
                 ARRIENDO:CODIGO_LOCALIDAD14 + ARRIENDO:CODIGO_LOCALIDAD15 + ARRIENDO:CODIGO_LOCALIDAD16 + 
                 ARRIENDO:CODIGO_LOCALIDAD17 + ARRIENDO:CODIGO_LOCALIDAD18 + ARRIENDO:CODIGO_LOCALIDAD19 + 
                 ARRIENDO:CODIGO_LOCALIDAD2 + ARRIENDO:CODIGO_LOCALIDAD3 + ARRIENDO:CODIGO_LOCALIDAD4 +
                 ARRIENDO:CODIGO_LOCALIDAD5 + ARRIENDO:CODIGO_LOCALIDAD6 + ARRIENDO:CODIGO_LOCALIDAD7 + 
                 ARRIENDO:CODIGO_LOCALIDAD8 + ARRIENDO:CODIGO_LOCALIDAD9 + 
                 ARRIENDO:AÑO2018 + ARRIENDO:AÑO2019 + ARRIENDO:AÑO2020, data = df_clean_apto_2, 
               random = ~ -1 + ARRIENDO|CODIGO_BARRIO)

fit_lmer <- lmer(VENTA ~ -1 + ARRIENDO +ARRIENDO:ESTRATO2 +
                 ARRIENDO:ESTRATO3 + ARRIENDO:ESTRATO4 + 
                 ARRIENDO:ESTRATO5 + ARRIENDO:ESTRATO6 + 
                 ARRIENDO:CODIGO_LOCALIDAD10 + ARRIENDO:CODIGO_LOCALIDAD11 +
                 ARRIENDO:CODIGO_LOCALIDAD12 + ARRIENDO:CODIGO_LOCALIDAD13 +
                 ARRIENDO:CODIGO_LOCALIDAD14 + ARRIENDO:CODIGO_LOCALIDAD15 + ARRIENDO:CODIGO_LOCALIDAD16 + 
                 ARRIENDO:CODIGO_LOCALIDAD17 + ARRIENDO:CODIGO_LOCALIDAD18 + ARRIENDO:CODIGO_LOCALIDAD19 + 
                 ARRIENDO:CODIGO_LOCALIDAD2 + ARRIENDO:CODIGO_LOCALIDAD3 + ARRIENDO:CODIGO_LOCALIDAD4 +
                 ARRIENDO:CODIGO_LOCALIDAD5 + ARRIENDO:CODIGO_LOCALIDAD6 + ARRIENDO:CODIGO_LOCALIDAD7 + 
                 ARRIENDO:CODIGO_LOCALIDAD8 + ARRIENDO:CODIGO_LOCALIDAD9 + 
                 ARRIENDO:AÑO2018 + ARRIENDO:AÑO2019 + ARRIENDO:AÑO2020 + (1 | CODIGO_BARRIO), data = df_clean_apto_2)



inf <- influence(fit_lmer, obs = TRUE)
dfbetas <- dfbetas(inf)


Cook <- cooks.distance(inf)
rowSums(dfbetas >= 2/sqrt(nrow(df_clean_apto_2))) > 0
nrow(df_clean_apto_2)
rowSums(dfbetas >= 2/sqrt(nrow(df_clean_apto_2)))
df_clean_apto_3 <- df_clean_apto_2[Cook < 4 * ncol(dfbetas(inf))/nrow(df_clean_apto_2), ]




##install.packages('spDataLarge', repos='https://nowosad.github.io/drat/', type='source')
sum(is.na(df_clean_apto$VENTA) | is.na(df_clean_apto_2$ARRIENDO))
df_clean_4[AÑO == 2017 & ESTRATO == 1]


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






Sectores_shp <- sf::st_read("input/Base_Dic2020/Sectores/Sectores1712.shp")
Sectores_shp <- Sectores_shp %>% as('Spatial')  %>% 
  spTransform(CRS("+proj=longlat +datum=WGS84")) 

Sectores_shp_sf <- ?st_as_sf(Sectores_shp)
Sectores_shp_sf <- merge(Sectores_shp_sf, df_clean_apto_2,
                         by.x = "SECSECT_ID", by.y = "CODIGO_BARRIO", all = TRUE)


  Sectores_shp_sf %>% filter(AÑO == "2019") %>%
  ggplot() + geom_sf(aes(fill = VENTA), color = "white") + 
  facet_wrap(~ESTRATO) + theme_bw()+
  scale_fill_viridis_c()


df_clean_apto_2[, .N, by = .(AÑO, CLASE_PREDIO, ESTRATO, VENTA_NA)]



