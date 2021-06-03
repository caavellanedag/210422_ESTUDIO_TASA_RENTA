library(targets)
library(tarchetypes)
tar_option_set(packages = c("janitor", "tidyverse", "openxlsx", "here", "haven", 
                            "data.table", "RecordLinkage",  "qcc", "nlme", 
                            "haven", "sf", "lme4", "influence.ME", "scales"))

source("src/Funciones.R")

list(
  tar_target(raw_data_file, "input/BASE_HIST_RENTAS.tab", format = "file"),
  tar_target(raw_bd_catastral, "input/base_predial_resumen_17_20.sas7bdat", format = "file"),
  tar_target(resumen_base_catastral, read_sas(raw_bd_catastral)),
  tar_target(base, fread(raw_data_file, header = T, sep = "\t")),
  tar_target(base_apto_casa, base[TIPO_INMUEBLE %in% c("APARTAMENTO","CASA") & ESTRATO > 0]),
  tar_target(base_filtrada, depura_base(base_apto_casa)),
  tar_target(df_clean, clean_bd(base_filtrada)),
  tar_target(df_clean_resumen, resumen_mean_bd(df_clean)),
  tar_target(df_clean_imputar, imputar_venta_arriendo(df_clean_resumen,
                                                      df_clean, 
                                                      resumen_base_catastral)),
  tar_target(df_clean_final, excluir_tasa_arriendo(df_clean_imputar)),
  
  tar_target(df_to_model_PH, make_model_matrix(df_clean_final$bd_in, CLASE = "P")),
  tar_target(bd_pruned_to_model_PH, prune_df_to_model(df_to_model_PH)),
  tar_target(anova_table_PH, get_anova_table(bd_pruned_to_model_PH)),
  tar_target(final_model_PH, final_model(bd_pruned_to_model_PH)),
  tar_target(ranef_df_PH, random_coef_to_datatable(final_model_PH)),
  tar_target(df_fixed_coef_PH, fixed_coef_to_datatable(final_model_PH)),
  tar_target(list_plots_model_PH, Check_residuals(bd_pruned_to_model_PH, final_model_PH)),
  tar_target(table_taxes_PH_final, Tabla_final_tasas(bd_pruned_to_model_PH, df_fixed_coef_PH, ranef_df_PH)),
  tar_target(list_whole_tables_PH, get_summary_tables(table_taxes_PH_final)),
  
  tar_target(df_to_model_NPH, make_model_matrix(df_clean_final$bd_in, CLASE = "N")),
  tar_target(bd_pruned_to_model_NPH, prune_df_to_model(df_to_model_NPH)),
  tar_target(anova_table_NPH, get_anova_table(bd_pruned_to_model_NPH)),
  tar_target(final_model_NPH, final_model(bd_pruned_to_model_NPH)),
  tar_target(ranef_df_NPH, random_coef_to_datatable(final_model_NPH)),
  tar_target(df_fixed_coef_NPH, fixed_coef_to_datatable(final_model_NPH)),
  tar_target(list_plots_model_NPH, Check_residuals(bd_pruned_to_model_NPH, final_model_NPH)),
  tar_target(table_taxes_NPH_final, Tabla_final_tasas(bd_pruned_to_model_NPH, df_fixed_coef_NPH, ranef_df_NPH)),
  tar_target(list_whole_tables_NPH, get_summary_tables(table_taxes_NPH_final)),
  tar_render(report, "reports/210428_PROGRAMA_DOCUMENTO_TASA_RENTA.Rmd"),
  tar_target(export, export_requirements(df_clean, df_clean_final, 
                                         table_taxes_PH_final, 
                                         table_taxes_NPH_final,
                                         df_clean_resumen, df_clean_imputar))
)





