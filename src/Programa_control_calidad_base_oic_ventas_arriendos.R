library(tidyverse)
library(targets)
library(openxlsx)
library(renv)

source("src/Funciones.R")

tar_make()
restore()
tar_load(base_filtrada)


base_oic <- read.xlsx("input/Ofertas_OIC_2017_2020_vivienda.xlsx", sheet = "consolidado")

base_oic$IDENTIFICADOR <- as.character(base_oic$OFT_CODIGO)
base_oic_2 <- base_oic %>% 
  pivot_longer(names_to = "TIPO_OFERTA", values_to = "PRECIO", cols = c("VR_INICIAL_ARRIENDO", "VR_INICIAL_VENTA")) 

base_oic_2$AREA_CONSTRUIDA <- base_oic_2$AREA_CONSTRUIDA_SIIC
base_oic_2$VALOR_M2_INTEGRAL <- base_oic_2$PRECIO/base_oic_2$AREA_CONSTRUIDA
base_oic_2$ESTRATO <- base_oic_2$CODIGO_ESTRATO_SIIC
base_oic_2$CLASE_PREDIO <- base_oic_2$CLASE_PREDIO_SIIC
base_oic_2$TIPO_OFERTA <- str_replace_all(base_oic_2$TIPO_OFERTA, c("VR_INICIAL_" = ""))
base_oic_2$IDENTIFICADOR <- as.character(base_oic_2$OFT_CODIGO)
base_oic_2$BARMANPRE <- paste0(base_oic_2$CODIGO_BARRIO, base_oic_2$CODIGO_MANZANA, base_oic_2$CODIGO_PREDIO)
base_oic_2$ANO <- base_oic_2$Vigencia

base_final <-  base_oic_2 %>% dplyr::select("PRECIO", "VALOR_M2_INTEGRAL", "AREA_CONSTRUIDA", "CODIGO_BARRIO",
  "ESTRATO",
  "CLASE_PREDIO",
  "TIPO_OFERTA",
  "IDENTIFICADOR",
  "ANO",
  "TIPO_OFERTA",
  "BARMANPRE", 
  "NOMBRE_BARRIO",
  "CODIGO_LOCALIDAD",
  "NOMBRE_LOCALIDAD") %>% 
  mutate(MARCA = "Test") %>% 
  rbind(base_filtrada %>% 
                           dplyr::select("PRECIO", "VALOR_M2_INTEGRAL", "AREA_CONSTRUIDA", "CODIGO_BARRIO",
                                "ESTRATO",
                                "CLASE_PREDIO",
                                "TIPO_OFERTA",
                                "IDENTIFICADOR",
                                "ANO",
                                "TIPO_OFERTA",
                                "BARMANPRE",
                                "NOMBRE_BARRIO",
                                "CODIGO_LOCALIDAD",
                                "NOMBRE_LOCALIDAD") %>% 
          mutate(MARCA = "Train"))


base_3 <- depura_base(base_final)
base_final_2 <- clean_bd(base_3)

base_oic %>%
  inner_join(base_final_2[MARCA == "Test", "IDENTIFICADOR"], by = "IDENTIFICADOR") %>% 
  write.xlsx(file = "input/210809_Ofertas_OIC_2017_2020_vivienda.xlsx", sheet = "consolidado")




