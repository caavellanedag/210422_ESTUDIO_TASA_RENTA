library(openxlsx)
library(tidyverse)


Sheet<-function(.x, .y, wb){
  style_number<-createStyle(numFmt = "#,##0")
  style_body <- createStyle(fontSize = 9, fontName = "Arial",
                            halign = "center", border= "TopBottomLeftRight",valign="center",wrapText = TRUE)
  style_head<-createStyle(fontSize = 10, fontColour = "white",
                          fontName = "Arial",textDecoration = "bold",fgFill = "red4",halign="center", border= "TopBottomLeftRight")
  style_number <- createStyle(numFmt = "#,##0")
  style_percentage <- createStyle(numFmt="PERCENTAGE")
  
  addWorksheet(wb,sheetName=.y)
  writeData(wb,sheet=.y,x=.x, startRow = 3)
  addStyle(wb, sheet = .y,style=style_body,stack=TRUE,rows = 4:(nrow(.x)+4), cols = 1:ncol(.x),gridExpand = TRUE)
  addStyle(wb, sheet = .y,style=style_head,stack=TRUE,rows = 3, cols = 1:ncol(.x),gridExpand = TRUE)
  width_vec <- apply(.x, 2, function(x) max(nchar(as.character(x)) + 2, na.rm = TRUE)) 
  width_names<-nchar(as.character(colnames(.x)))+7
  width<-apply(cbind(width_vec,width_names),1,max)
  setColWidths(wb,  sheet = .y, cols=1:ncol(.x), widths = width)
  
  addStyle(wb, sheet = .y,
           style=style_number,
           stack=TRUE,
           rows = 4:(nrow(.x)+5),
           cols = str_which(names(.x), "^AREA|^N|^PREDIOS|^AV|^VALOR|^AVALUO|^MEDIA|^MEDIANA|ERROR_TOTAL|NUM_PREDIOS|DIF|VR"),
           gridExpand = TRUE)
  
  addStyle(wb, sheet = .y,
           style = style_percentage,
           stack = TRUE,
           rows = 4:(nrow(.x)+5),
           cols = str_which(names(.x), "^VAR|TCR|^TASA"),
           gridExpand = TRUE)
  
  
}


write_excel <- function(tabla, name, label, num){
  tabla_2 <- read.xlsx(tabla)
  Sheet(tabla_2, name, wb)
  writeFormula(wb, "Índice", startRow = 9 + num, startCol = 3, 
               x = makeHyperlinkString(sheet = name, row = 3, col = 1, text = label))
}




wb <- createWorkbook("Camilo Avellaneda")
addWorksheet(wb, "Índice")
insertImage(wb, "Índice",
            "input/logo.png", width = 3.5,height = 1.5,
            startRow = 1,startCol = 1,units = "in",dpi = 300)

# addWorksheet(wb, "Diccionario")
# insertImage(wb, "Diccionario",
#             "input/logo.png", width = 3.5,height = 1.5,
#             startRow = 1,startCol = 1,units = "in",dpi = 300)
# writeData(wb, "Diccionario", diccionario, startRow = 9, startCol= 3)

wb_1 <- loadWorkbook("output/210809_BASE_OIC_ARRIENDO_VENTA.xlsx")
Tablas <- getSheetNames("output/210809_BASE_OIC_ARRIENDO_VENTA.xlsx")
labels <- paste0("Tabla ", 1:length(Tablas), ": ")
labels <- paste0(labels, c("Base de ofertas utilizadas para la validación de la metodología de tasas de capitalización de renta en las vigencias 2019 y 2020.",
                           "Errores medianos de valores integrales y de tasas de capitalización, según estrato, clase de predio y localidad.",
                           "Errores medianos de valores integrales y de tasas de capitalización, según clase de predio.",
                           "Errores medianos de valores integrales y de tasas de capitalización, según localidad.",
                           "Errores medianos de valores integrales y de tasas de capitalización, según estrato.")
)
names <- paste0("T.", 1:length(labels))

pmap(list(Tablas, names, labels, 1:length(labels)), function(tabla, name, label, num){
  #tabla_2 <- read.xlsx(tabla)
  tabla_2 <- read.xlsx(wb_1, sheet = tabla)
  Sheet(tabla_2, name, wb)
  writeData(wb, name, label, startRow=2, startCol=1)
  writeFormula(wb, "Índice", startRow = 9 + num, startCol = 3, 
               x = makeHyperlinkString(sheet = name, row = 3, col = 1, text = label))
  
})

saveWorkbook(wb,
             paste0("output/",
                    str_replace_all(Sys.Date(), c("-" = "", "2021" = "21")), "_RESULTADOS_REVISION_BASE_OIC_VENTA_ARRIENDO.xlsx"),
             overwrite = TRUE)
