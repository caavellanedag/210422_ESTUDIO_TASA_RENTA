if(!require('pacman')){install.packages('pacman')}
p_load(janitor, tidyverse, openxlsx, here, haven, 
  data.table, RecordLinkage,  qcc, nlme, 
  haven, sf, lme4, influence.ME)


labels <- function(x)format(x, big.mark = ".", scientific = FALSE) 

check_all_equal <- function(.x){return(all(.x == mean(.x)))}


Sheet<-function(.x, .y, wb){
  style_number<-createStyle(numFmt = "#,##0")
  style_body <- createStyle(fontSize = 9, fontName = "Arial",
                            halign = "center", border= "TopBottomLeftRight",valign="center",wrapText = TRUE)
  style_head<-createStyle(fontSize = 10, fontColour = "white",fontName = "Arial",textDecoration = "bold",fgFill = "steelblue4",halign="center", border= "TopBottomLeftRight")
  addWorksheet(wb,sheetName=.y)
  writeData(wb,sheet=.y,x=.x)
  addStyle(wb, sheet = .y,style=style_body,stack=TRUE,rows = 2:(nrow(.x)+1), cols = 1:ncol(.x),gridExpand = TRUE)
  addStyle(wb, sheet = .y,style=style_head,stack=TRUE,rows = 1, cols = 1:ncol(.x),gridExpand = TRUE)
  width_vec <- apply(.x, 2, function(x) max(nchar(as.character(x)) + 2, na.rm = TRUE)) 
  width_names<-nchar(as.character(colnames(.x)))+7
  width<-apply(cbind(width_vec,width_names),1,max)
  setColWidths(wb,  sheet = .y, cols=1:ncol(.x), widths = width)
}


make_control_chart <- function(.x){
  #if(unique(.x$CLASE_PREDIO) == "P"){
    cols_all <- c("PRECIO", "VALOR_M2_INTEGRAL", "ANTIGUEDAD_INMUEBLE", #"MED_PUNTAJE", "ANTIGUEDAD_INMUEBLE", "NUMERO_ALCOBAS", 
                  "AREA_CONSTRUIDA")
  
    cols_all <- c("PRECIO", "VALOR_M2_INTEGRAL", "ANTIGUEDAD_INMUEBLE",#"MED_PUNTAJE", "ANTIGUEDAD_INMUEBLE", "AREA_DE_TERRENO","NUMERO_ALCOBAS", 
                  "AREA_CONSTRUIDA")
  
  
  df <- .x %>% dplyr::select(cols_all)
  if(sum(map_lgl(df, check_all_equal)) > 0){
    df <- df %>% dplyr::select(-names(df)[map_lgl(df, check_all_equal)])  
  }
  if(nrow(df) == 0){
    list <- list(points = NULL,
                 limits = NULL,
                 outlier = NULL,
                 df_all = .x %>% dplyr::select(cols_all))
  }else{
    cols_all_2 <- names(df)
    x <- df %>% as.matrix()
    if(class(try(solve(cov(x)), silent = TRUE)) == "try-error"){
      k <- 1
      x <- df[, 1:k] %>% as.matrix()
      while(class(try(solve(cov(x)), silent = TRUE)) != "try-error" & k <= length(cols_all_2)){
        x <- df[, 1:k] %>% as.matrix()
        k <- k + 1
      }
      k <- k - 2
      df <- df[, 1:k]
    }
    df <- df %>% as.matrix()
    
    q <- mqcc(df, type = "T2.single", confidence.level = 0.9, plot = FALSE)
    
    list <- list(points = q$statistics,
                 limits = q$limits,
                 outlier = q$violations$beyond.limits,
                 df_all = df)
  }
  return(list)
}


make_plot_qcc <- function(.x){
  df_to_plot <- data.table(label = 1:length(.x$points),
                           points = .x$points) 
  
  df_to_plot <- df_to_plot[, out := ifelse(label %in% .x$outlier,"out","in")]
  
  p <- df_to_plot %>% ggplot()+geom_point(aes(label, points, color = out), size = 2.5)+
    geom_line(aes(label, points))+
    geom_hline(yintercept = .x$limits, linetype = "dashed")+
    theme_bw()+
    scale_color_manual(values = c("black", "red"))+
    guides(color = FALSE)+xlab("Oferta")+ylab("Estadística calculada")
  
  return(p)
}



Agrupa_barrios <- function(.y, cutoff){
  Muy_pocos <- TRUE
  k <- 1
  while(Muy_pocos){
    if(k == 1){
      summary <- .y[,.N, by = .(CODIGO_BARRIO)]
      Not_enough <- summary[N <= cutoff]
      Enough <- summary[N > cutoff]
      SECTORES <- summary$CODIGO_BARRIO
      N <- summary$N 
      Similarity_codes <- map_dfr(Not_enough$CODIGO_BARRIO,
                                  ~Encuentra_barrio(.x, SECTORES, N))
      for(.x in Similarity_codes$CODIGO_BARRIO){
        #.x <- "002620"
        if(.x %in% Similarity_codes$CODIGO_BARRIO_NEW){
          Similarity_codes[CODIGO_BARRIO_NEW == .x, "CODIGO_BARRIO_NEW"] <- Similarity_codes[CODIGO_BARRIO == .x, "CODIGO_BARRIO_NEW"]
        }
      }
      .y <- merge(.y,Similarity_codes, by = "CODIGO_BARRIO",all.x = TRUE)
      
      .y[, "CODIGO_BARRIO_NEW" := ifelse(is.na(CODIGO_BARRIO_NEW), CODIGO_BARRIO, CODIGO_BARRIO_NEW)] 
      summary <- .y[,.N, by = .(CODIGO_BARRIO_NEW)]
    }else{
      summary <- .y[,.N, by = .(CODIGO_BARRIO_NEW)]
      Not_enough <- summary[N <= cutoff]
      Enough <- summary[N > cutoff]
      SECTORES <- summary$CODIGO_BARRIO_NEW
      N <- Enough$N
      Similarity_codes <- map_dfr(Not_enough$CODIGO_BARRIO_NEW,
                                  ~Encuentra_barrio(.x, SECTORES, N))
      for(.x in Similarity_codes$CODIGO_BARRIO){
        if(.x %in% Similarity_codes$CODIGO_BARRIO_NEW){
          Similarity_codes[CODIGO_BARRIO_NEW == .x, "CODIGO_BARRIO_NEW"] <- Similarity_codes[CODIGO_BARRIO == .x, "CODIGO_BARRIO_NEW"]
        }
      }
      Similarity_codes[, "CODIGO_BARRIO_NEW.y" :=  CODIGO_BARRIO_NEW]
      Similarity_codes <- Similarity_codes[,-"CODIGO_BARRIO_NEW"]
      .y <- merge(.y,Similarity_codes, by.x="CODIGO_BARRIO_NEW",
                  by.y = "CODIGO_BARRIO",all.x = TRUE)
      .y[, "CODIGO_BARRIO_NEW.y" := ifelse(is.na(CODIGO_BARRIO_NEW.y), CODIGO_BARRIO_NEW, CODIGO_BARRIO_NEW.y)] 
      .y <- .y[,-"CODIGO_BARRIO_NEW"]
      .y <- .y[, "CODIGO_BARRIO_NEW" :=  CODIGO_BARRIO_NEW.y]
      .y <- .y[,-"CODIGO_BARRIO_NEW.y"]
      #.y <- merge(.y,Similarity_codes, by = "CODIGO_BARRIO",all.x = TRUE)
      summary <- .y[,.N, by = .(CODIGO_BARRIO_NEW)]
      
    }
    Muy_pocos <- any(summary$N < cutoff)
    k <- k+1
  }
  return(.y)
}

Encuentra_barrio <- function(.x, SECTORES, N){
  #for(.x in unique(Not_enough$CODIGO_BARRIO)){
  #.x <- Not_enough$CODIGO_BARRIO[1]
  DISTANCES <- levenshteinSim(.x, SECTORES)
  df_distances <- data.table(SECTORES, DISTANCES, N)
  df_distances <- df_distances[DISTANCES < 1]
  n_row <- 0
  k <- 5
  while(n_row == 0){
    df_distances_2 <- df_distances[substring(SECTORES,1,k) == substring(.x,1,k)]
    n_row <- nrow(df_distances_2)
    k <- k - 1 
  }
  
  df_distances_2 <- df_distances_2 %>% arrange(-DISTANCES, -N)
  optim <- df_distances_2[1, "SECTORES"]
  #data.frame(CODIGO_BARRIO = .x, CODIGO_NEW = optim)
  #}
  return(data.table(CODIGO_BARRIO = .x, CODIGO_BARRIO_NEW = as.character(optim)))
}





Fill_na <- function(.z){
  .z %>% mutate_at(c("NUMERO_GARAJES", "NUMERO_ALCOBAS"), replace_na_zero) %>% 
    mutate_at(c("AREA_DE_TERRENO","AREA_CONSTRUIDA", 
                "MED_PUNTAJE", 
                "ANTIGUEDAD_INMUEBLE"), replace_na_mean)
}

replace_na_mean <- function(x){
  nafill(x, fill = mean(x, na.rm = TRUE))
}
replace_na_zero <- function(x){
  nafill(x, fill = 0)
}


depura_base <- function(base){
  base <- clean_names(base)
  names(base) <- toupper(names(base))
  base$PRECIO <- as.numeric(as.character(str_replace_all(base$PRECIO,c("\\$"="",","=""))))
  base$IDENTIFICADOR <- as.character(base$IDENTIFICADOR)
  
  base <- base %>% mutate_at(c("IDENTIFICADOR","CODIGO_UPZ","NOMBRE_UPZ","CODIGO_BARRIO","NOMBRE_BARRIO",
                               "NOMBRE_LOCALIDAD","CODIGO_LOCALIDAD","BARMANPRE"),as.character) 
  base$FECHA <- as.Date(base$FECHA,format="%d/%m/%Y")
  base$CAT_EDAD <- cut(base$ANTIGUEDAD_INMUEBLE, breaks = c(0, 5, 10, 20, max(base$ANTIGUEDAD_INMUEBLE)), include.lowest = TRUE)
  base_filtrada <- base[,CODIGO_BARRIO := str_pad(CODIGO_BARRIO, width = 6, side = "left", pad = "0")]
  base_filtrada <- base_filtrada[,BARMANPRE := str_pad(BARMANPRE, width = 10, side = "left", pad = "0")]
  base_filtrada <- base_filtrada[substring(base_filtrada$CODIGO_BARRIO,1,1) == "0", ]
  base_filtrada <- base_filtrada[AREA_CONSTRUIDA > 0 & !(is.na(base_filtrada$AREA_CONSTRUIDA))]
  base_filtrada <- base_filtrada[!(base_filtrada$BARMANPRE=="" | is.na(base_filtrada$BARMANPRE)),]
  
  return(base_filtrada)
}



clean_bd <- function(base){
  base_filtrada <- base %>% as.data.table()
  base_filtrada <- base_filtrada[,CODIGO_BARRIO_OR := CODIGO_BARRIO]
  base_filtrada[, key := paste0(ANO,"_",CLASE_PREDIO,"_", ESTRATO, "_", TIPO_OFERTA)]
  
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
  return(df_clean)
}





resumen_mean_bd <- function(bd){
  df_clean <- bd
  df_clean_2 <- df_clean[,c("CODIGO_BARRIO", "NOMBRE_BARRIO", "CODIGO_LOCALIDAD", "NOMBRE_LOCALIDAD",
                            "CLASE_PREDIO", "ESTRATO", "ANO", "TIPO_OFERTA", "CAT_EDAD", "VALOR_M2_INTEGRAL", "PRECIO")]
  
  df_clean_3 <- df_clean_2[, list(N = .N, PRECIO_MEAN =  mean(VALOR_M2_INTEGRAL)),
                           by = .(CODIGO_BARRIO, NOMBRE_BARRIO, CODIGO_LOCALIDAD, NOMBRE_LOCALIDAD,
                                  CLASE_PREDIO, ESTRATO, ANO, TIPO_OFERTA, CAT_EDAD)]
  
  df_clean_4 <- df_clean_3 %>% data.table::dcast(formula = CODIGO_BARRIO + NOMBRE_BARRIO + CODIGO_LOCALIDAD + 
                                                   NOMBRE_LOCALIDAD + CLASE_PREDIO + ESTRATO + ANO + CAT_EDAD ~ TIPO_OFERTA, 
                                                 value.var = c("PRECIO_MEAN", "N"), fun.aggregate = mean, fill = NA)
  
  df_clean_4 <- df_clean_4[!is.na(PRECIO_MEAN_ARRIENDO) | N_VENTA > 5]
  return(df_clean_4)
}

imputar_venta_arriendo <- function(bd, bd_completa, resumen_base_catastral){
  df_clean_4 <- bd
  df_clean_5 <- merge(df_clean_4, resumen_base_catastral, 
                      by.x = c("CODIGO_BARRIO", "CLASE_PREDIO", "ESTRATO", "ANO", "CAT_EDAD"),
                      by.y = c("CODIGO_BARRIO", "CLASE_PREDIO", "CODIGO_ESTRATO", "VIGENCIA", "CAT_EDAD"))
  
  df_clean_5 <- df_clean_5[, PRECIO_MEAN_VENTA := ifelse(is.na(PRECIO_MEAN_VENTA), VALOR_INTEGRAL, PRECIO_MEAN_VENTA)]
  
  
  
  df_input_1 <- bd_completa[TIPO_OFERTA == "ARRIENDO",
                            .(MEAN_NEW_ARRIENDO = mean(VALOR_M2_INTEGRAL)),
                            by = .(CODIGO_BARRIO, CLASE_PREDIO, ESTRATO, CAT_EDAD)]
  
  df_input_2 <- bd_completa[TIPO_OFERTA == "ARRIENDO",
                            .(MEAN_ARRIENDO_LOC = mean(VALOR_M2_INTEGRAL)),
                            by = .(CODIGO_LOCALIDAD, CLASE_PREDIO, ESTRATO, CAT_EDAD)]
  
  df_clean_7 <- df_clean_5 %>% merge(df_input_1, by = c("CODIGO_BARRIO", "CLASE_PREDIO", "ESTRATO", "CAT_EDAD"), all.x = TRUE) %>% 
    merge(df_input_2, by = c("CODIGO_LOCALIDAD", "CLASE_PREDIO", "ESTRATO", "CAT_EDAD"), all.x = TRUE)
  
  df_clean_8 <- df_clean_7[, PRECIO_MEAN_ARRIENDO := ifelse(is.na(PRECIO_MEAN_ARRIENDO) & is.na(MEAN_NEW_ARRIENDO),
                                                            MEAN_ARRIENDO_LOC, ifelse(is.na(PRECIO_MEAN_ARRIENDO), 
                                                                                      MEAN_NEW_ARRIENDO, PRECIO_MEAN_ARRIENDO))]
  df_clean_8 <- df_clean_8[, -c("MEAN_NEW_ARRIENDO", "MEAN_ARRIENDO_LOC")]
  return(df_clean_8)
}


excluir_tasa_arriendo <- function(bd){
  bd <- bd[, c("ESTRATO", "ANO") := list(factor(ESTRATO), factor(ANO))]
  bd_in <- bd[PRECIO_MEAN_VENTA/PRECIO_MEAN_ARRIENDO >= 100 & PRECIO_MEAN_VENTA/PRECIO_MEAN_ARRIENDO < 300]  
  bd_out <- bd[PRECIO_MEAN_VENTA/PRECIO_MEAN_ARRIENDO < 100 | PRECIO_MEAN_VENTA/PRECIO_MEAN_ARRIENDO > 300]  
  return(list(bd_in = bd_in, bd_out = bd_out))
}


make_model_matrix <- function(bd, CLASE){
  df_clean_9 <- bd
  df_clean_apto <- df_clean_9[CLASE_PREDIO == CLASE]
  model_matrix <- model.matrix(PRECIO_MEAN_VENTA ~  ESTRATO + CODIGO_LOCALIDAD + ANO + CAT_EDAD, data = df_clean_apto) 
  df_clean_apto_2 <- df_clean_apto %>% cbind(model_matrix)
  return(df_clean_apto_2)
}

prune_df_to_model <- function(make_model_matrix){
  fit_lmer <- lm(PRECIO_MEAN_VENTA ~ -1 + PRECIO_MEAN_ARRIENDO + 
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
                     PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD9 + PRECIO_MEAN_ARRIENDO:ANO2018 + 
                     PRECIO_MEAN_ARRIENDO:ANO2019 + PRECIO_MEAN_ARRIENDO:ANO2020, data = make_model_matrix)
  
   dfbetas <- dfbetas(fit_lmer)
   check_dfbetas <- dfbetas > 6/sqrt(nrow(make_model_matrix))
 
   Cook <- cooks.distance(fit_lmer)
  
   make_model_matrix <- make_model_matrix[(Cook < 4 * ncol(dfbetas)/nrow(make_model_matrix)) & rowSums(check_dfbetas) == 0]
   
   return(make_model_matrix)
}


get_anova_table <- function(bd){
  fit_lme_to_print <- lme(PRECIO_MEAN_VENTA ~ -1 + PRECIO_MEAN_ARRIENDO,
                          data = bd, 
                          random = ~ -1 + PRECIO_MEAN_ARRIENDO|CODIGO_BARRIO)
  
  fit_lme_to_print_2 <- lme(PRECIO_MEAN_VENTA ~ -1 + PRECIO_MEAN_ARRIENDO +
                              PRECIO_MEAN_ARRIENDO:ESTRATO2 + PRECIO_MEAN_ARRIENDO:ESTRATO3 + 
                              PRECIO_MEAN_ARRIENDO:ESTRATO4 + PRECIO_MEAN_ARRIENDO:ESTRATO5 + 
                              PRECIO_MEAN_ARRIENDO:ESTRATO6,
                            data = bd, 
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
                            data = bd, 
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
                              PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD9 + PRECIO_MEAN_ARRIENDO:ANO2018 + 
                              PRECIO_MEAN_ARRIENDO:ANO2019 + PRECIO_MEAN_ARRIENDO:ANO2020,
                            data = bd, 
                            random = ~ -1 + PRECIO_MEAN_ARRIENDO|CODIGO_BARRIO)
  
  return(anova(fit_lme_to_print, fit_lme_to_print_2, fit_lme_to_print_3, fit_lme_to_print_4))
}



final_model <- function(bd){
  lme(PRECIO_MEAN_VENTA ~ -1 + PRECIO_MEAN_ARRIENDO + 
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
        PRECIO_MEAN_ARRIENDO:CODIGO_LOCALIDAD9 + PRECIO_MEAN_ARRIENDO:ANO2018 + 
        PRECIO_MEAN_ARRIENDO:ANO2019 + PRECIO_MEAN_ARRIENDO:ANO2020,
      data = bd, 
      random = ~ -1 + PRECIO_MEAN_ARRIENDO|CODIGO_BARRIO)
}
fixed_coef_to_datatable <- function(fit_model){
  df_fixed_coef <- data.frame(Coeficiente = summary(fit_model)$coef$fixed) %>%
    rownames_to_column() %>% as.data.table()
  
  df_fixed_coef <- df_fixed_coef[, c("ESTRATO", "CODIGO_LOCALIDAD", "ANO", "Intercepto") :=
                                   list(str_replace_all(str_extract(rowname, "ESTRATO\\d"), "ESTRATO", ""),
                                        str_replace_all(str_extract(rowname, "CODIGO_LOCALIDAD\\d+"), "CODIGO_LOCALIDAD", ""),
                                        str_replace_all(str_extract(rowname, "ANO\\d+"), "ANO", ""),
                                        ifelse(rowname == "PRECIO_MEAN_ARRIENDO", 1, NA))]
  return(df_fixed_coef)
}

random_coef_to_datatable <- function(fit_model){
  ranef_df <- ranef(fit_model) %>% as.data.frame() %>% rownames_to_column() %>% as.data.table()
  ranef_df <- ranef_df[, c("CODIGO_BARRIO", "RAN_EFFECT") := list(rowname, PRECIO_MEAN_ARRIENDO)]
  ranef_df <- ranef_df[, -c("rowname", "PRECIO_MEAN_ARRIENDO")]
  return(ranef_df)
}



Tabla_final_tasas <- function(bd_model, fixed_df, random_df){
  Tabla_apto_final <- bd_model[, c("CODIGO_BARRIO", "CODIGO_LOCALIDAD", "ANO", "ESTRATO")] %>% unique() %>% 
    mutate(Intercepto = 1) %>% 
    merge(random_df, by = "CODIGO_BARRIO", all.x = TRUE) %>% 
    merge(fixed_df[, Coef_all := Coeficiente][, c("Intercepto", "Coef_all")], by = "Intercepto", all.x = TRUE) %>% 
    merge(fixed_df[, Coef_est := Coeficiente][, c("ESTRATO", "Coef_est")], by = "ESTRATO", all.x = TRUE) %>% 
    merge(fixed_df[, Coef_loc := Coeficiente][, c("CODIGO_LOCALIDAD", "Coef_loc")], by = "CODIGO_LOCALIDAD", all.x = TRUE) %>%
    merge(fixed_df[, Coef_year := Coeficiente][, c("ANO", "Coef_year")], by = "ANO", all.x = TRUE)  %>% 
    mutate_at(c("Coef_all", "Coef_est", "Coef_loc", "Coef_year"), replace_na_zero) %>% 
    mutate(TASA_RENTA = Coef_all + Coef_est + Coef_loc + Coef_year + RAN_EFFECT) %>% arrange(TASA_RENTA)
  return(Tabla_apto_final)
}



Check_residuals <- function(bd, fit_model){
  df_clean_apto_4 <- bd[, c("Fitted", "Residuals") := list(fitted(fit_model), residuals(fit_model))]
  
  plot_fitted_residuals <-  df_clean_apto_4 %>% 
    ggplot(aes(Fitted, Residuals)) + geom_point(colour = "red4", alpha = 0.5, shape = 18, size = 3) + 
    theme_bw() + xlab("Valores ajustados del modelo") + ylab("Residuales del modelo")+
    scale_y_continuous(labels = labels, limits = c(-1*10^6, 1*10^6)) + 
    scale_x_continuous(labels = labels)+
    geom_hline(yintercept = 0, colour = "black", linetype = "twodash", size = 1.5)
  
  
  plot_boxplot_estrato_vigencia <- df_clean_apto_4 %>% 
    ggplot() + geom_boxplot(aes(ESTRATO, Residuals), fill = "darkorange", alpha = 0.8) +
    facet_wrap(~ ANO, scales = "free") + 
    geom_hline(yintercept = 0, colour = "darkred", size = 1.3, linetype = "twodash")+
    theme_bw() + xlab("Estrato") + ylab("Residuales del modelo")
  
  plot_normal_residuals <- ggplot(df_clean_apto_4) + 
    geom_histogram( aes(Residuals), fill = "red2", alpha = 0.8, colour = "black") +
    theme_bw() + xlab("Residuales del modelo") + ylab("Frecuencia")
  
  ranef_df <- ranef(fit_model) %>% as.data.frame() %>% rownames_to_column() %>% as.data.table()
  ranef_df <- ranef_df[, c("CODIGO_BARRIO", "RAN_EFFECT") := list(rowname, PRECIO_MEAN_ARRIENDO)]
  ranef_df <- ranef_df[, -c("rowname", "PRECIO_MEAN_ARRIENDO")]
  
  plot_normal_ranef <- ranef_df %>% ggplot() + 
    geom_histogram(aes(RAN_EFFECT), fill = "red2", alpha = 0.8, colour = "black") +
    theme_bw() + xlab("Efectos aleatorios") + ylab("Frecuencia")
  
  scatterplot_estrato <- bd %>% ggplot(aes(PRECIO_MEAN_VENTA, PRECIO_MEAN_ARRIENDO, color = ANO)) + 
    geom_point() +
    facet_wrap(~ ESTRATO, scales = "free_x") +
    geom_smooth(method = "lm", se = F) +
    xlab("Promedio de arriendo") + ylab("Promedio de venta") + 
    scale_x_continuous(labels = labels) +
    scale_y_continuous(labels = labels) +
    theme_bw() +
    scale_color_brewer(palette = "Set1")
  
  return(
    list(plot_fitted_residuals = plot_fitted_residuals,
         plot_boxplot_estrato_vigencia = plot_boxplot_estrato_vigencia,
         plot_normal_residuals = plot_normal_residuals,
         plot_normal_ranef = plot_normal_ranef,
         scatterplot_estrato = scatterplot_estrato)
  )
}


get_summary_tables <- function(tabla_tasa_final){
  tabla_tasa_final_est <- tabla_tasa_final[, c("ANO", "ESTRATO", "Coef_all", "Coef_est", "Coef_year")
  ][, c("A", "TASA_RENTA") := list(ANO, round(Coef_all + Coef_est + Coef_year, 2))][
    , "TASA_RENTA_2" := round(1/TASA_RENTA, 4)] %>% unique()
  
  
  tabla_tasa_final_est <- tabla_tasa_final_est[order(ESTRATO, A), c("A","ESTRATO", "TASA_RENTA", "TASA_RENTA_2")]
  
  tabla_tasa_final_loc <- tabla_tasa_final[, c("ANO", "CODIGO_LOCALIDAD", "Coef_all", "Coef_loc", "Coef_year")
  ][, c("A","CODIGO_LOCALIDAD", "TASA_RENTA") := list(ANO, as.numeric(as.character(CODIGO_LOCALIDAD)), round(Coef_all + Coef_loc + Coef_year, 2))][
    , "TASA_RENTA_2" := round(1/TASA_RENTA, 4)] %>% unique()
  
  tabla_tasa_final_loc <- tabla_tasa_final_loc[order(CODIGO_LOCALIDAD, A), c("A","CODIGO_LOCALIDAD", "TASA_RENTA", "TASA_RENTA_2")]
  

  return(list(tabla_tasa_final_loc = tabla_tasa_final_loc,
              tabla_tasa_final_est = tabla_tasa_final_est
  ))
}


export_requirements <- function(df_clean, df_clean_final, table_taxes_PH_final, table_taxes_NPH_final){
  df_clean <- as.data.table(df_clean)
  df_clean_to_print <- df_clean[CODIGO_BARRIO %in% df_clean_final$bd_out$CODIGO_BARRIO]
  wb <- createWorkbook("Camilo Avellaneda")
  Sheet(df_clean_final$bd_out, "SECTORES_OUT", wb)
  Sheet(df_clean_to_print, "PREDIOS_SECTORES_OUT", wb)
  Sheet(table_taxes_PH_final, "PH_Final", wb)
  Sheet(table_taxes_NPH_final, "NPH_Final", wb)
  saveWorkbook(wb, file = paste0("output/", str_replace_all(Sys.Date(), c("2021" = "21", "-" = "")), "_RESULTADOS_PROGRAMA_TASA_RENTA_SECTORES.xlsx"), overwrite = TRUE)
}

