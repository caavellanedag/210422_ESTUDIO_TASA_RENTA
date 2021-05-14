labels=function(x)format(x, big.mark = ".", scientific = FALSE) 

check_all_equal <- function(.x){return(all(.x == mean(.x)))}



make_control_chart <- function(.x){
  if(unique(.x$CLASE_PREDIO) == "P"){
    cols_all <- c("PRECIO", "VALOR_M2_INTEGRAL", "MED_PUNTAJE", "ANTIGUEDAD_INMUEBLE", 
                  "AREA_CONSTRUIDA", "NUMERO_ALCOBAS")
  }else{
    cols_all <- c("PRECIO", "VALOR_M2_INTEGRAL", "MED_PUNTAJE", "ANTIGUEDAD_INMUEBLE", 
                  "AREA_CONSTRUIDA", "AREA_DE_TERRENO","NUMERO_ALCOBAS")
  }
  
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
