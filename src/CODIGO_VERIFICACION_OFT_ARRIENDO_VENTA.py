# -*- coding: utf-8 -*-
"""
#
# Autor(es): Camilo Avellaneda
# Mantenimiento: Camilo Avellaneda
# Fecha creación : 06/05/2021
#==============================================
@author: cavellaneda
"""
%reset -f
# -*- coding: utf-8 -*-
import os
import matplotlib.pyplot as plt
import seaborn as sns
import numpy as np
import pandas as pd
import re as re
import math
import operator
import statistics
import matplotlib.ticker as mtick
import matplotlib.ticker as tkr
from datetime import datetime

date = datetime.today().strftime('%Y-%m-%d').replace("-", "").replace("2021","21")

sns.set_style("whitegrid")

def COLUMNS_UPPERCASE(dataframe):
    for k in dataframe.columns:
        dataframe=dataframe.rename(columns={k:k.upper().replace(" ","_")})
    return dataframe

pd.options.display.float_format = '{:.2f}'.format
path = os.getcwd()
#print(path)
#print(type(path))

Tasas_sectores_PH = pd.read_excel("Z:/Trabajo/Catastro/210422_ESTUDIO_TASA_RENTA/output/210714_RESULTADOS_PROGRAMA_TASA_RENTA_SECTORES.xlsx",
              sheet_name = "PH_Sectores")

Tasas_sectores_NPH = pd.read_excel("Z:/Trabajo/Catastro/210422_ESTUDIO_TASA_RENTA/output/210714_RESULTADOS_PROGRAMA_TASA_RENTA_SECTORES.xlsx",
              sheet_name = "NPH_Sectores")

Tasas_loc_PH = pd.read_excel("Z:/Trabajo/Catastro/210422_ESTUDIO_TASA_RENTA/output/210714_RESULTADOS_PROGRAMA_TASA_RENTA_SECTORES.xlsx",
              sheet_name = "PH_Localidades")

Tasas_loc_NPH = pd.read_excel("Z:/Trabajo/Catastro/210422_ESTUDIO_TASA_RENTA/output/210714_RESULTADOS_PROGRAMA_TASA_RENTA_SECTORES.xlsx",
              sheet_name = "NPH_Localidades")

Tasas_loc_NPH["OFT_TIPO_INMUEBLE"] = "CASA"
Tasas_loc_PH["OFT_TIPO_INMUEBLE"] = "APARTAMENTO"

Tasas_sectores_NPH["OFT_TIPO_INMUEBLE"] = "CASA"
Tasas_sectores_PH["OFT_TIPO_INMUEBLE"] = "APARTAMENTO"

Tasas_loc = pd.concat([Tasas_loc_NPH, Tasas_loc_PH], axis = 0)
Tasas_sectores = pd.concat([Tasas_sectores_NPH, Tasas_sectores_PH], axis = 0)

BASE_OFT = pd.read_excel("Z:/Trabajo/Catastro/210422_ESTUDIO_TASA_RENTA/input/Ofertas_OIC_2017_2020_vivienda.xlsx",
              sheet_name="consolidado")

BASE_OFT = BASE_OFT.drop(["no_loc_no_estrato", "TRC_OIC", "Venta_ estimado", "año_SC_estrato",
               "TRC_localidad", "año_localidad_estrato", "TCR_Sector", "año_localidad_estrato"], axis = 1)

BASE_OFT = COLUMNS_UPPERCASE(BASE_OFT)

BASE_OFT["CODIGO_BARRIO"] = BASE_OFT.CODIGO_BARRIO.astype(str).apply(lambda x: x.zfill(6))

Tasas_loc = Tasas_loc.rename(columns = {"TASA_RENTA": "TASA_LOCALIDAD"})
Tasas_sectores = Tasas_sectores.rename(columns = {"TASA_RENTA": "TASA_SECTOR"})
Tasas_sectores["CODIGO_BARRIO"] = Tasas_sectores.CODIGO_BARRIO.astype(str).apply(lambda x: x.zfill(6))

BASE_OFT_2 = pd.merge(BASE_OFT, Tasas_sectores.drop(["CODIGO_LOCALIDAD"], axis = 1), how = "left",
         left_on = ["OFT_TIPO_INMUEBLE", "VIGENCIA", "CODIGO_BARRIO", "CODIGO_ESTRATO_SIIC"],
         right_on = ["OFT_TIPO_INMUEBLE", "ANO", "CODIGO_BARRIO", "ESTRATO"])

BASE_OFT_3 = pd.merge(BASE_OFT_2.drop(["ESTRATO", "ANO"], axis = 1), Tasas_loc, how = "left",
         left_on = ["OFT_TIPO_INMUEBLE", "VIGENCIA", "CODIGO_LOCALIDAD", "CODIGO_ESTRATO_SIIC"],
         right_on = ["OFT_TIPO_INMUEBLE", "ANO", "CODIGO_LOCALIDAD", "ESTRATO"])
BASE_OFT_3 = BASE_OFT_3.drop(["ESTRATO", "ANO"], axis = 1)

BASE_OFT_3['TCR_ESTADISTICA'] = BASE_OFT_3["TASA_SECTOR"]
BASE_OFT_3.loc[BASE_OFT_3["TASA_SECTOR"].isna(), 'TCR_ESTADISTICA'] = BASE_OFT_3["TASA_LOCALIDAD"]
#BASE_OFT_3.loc[BASE_OFT_3["TCR_ESTADISTICA"].isna(),
               #["CODIGO_BARRIO", "CODIGO_LOCALIDAD", "CODIGO_ESTRATO_SIIC", "OFT_TIPO_INMUEBLE", "VIGENCIA"]]
               
BASE_OFT_4 = BASE_OFT_3[(BASE_OFT_3.OFT_TIPO_INMUEBLE.isin(["CASA", "APARTAMENTO"])) & (~BASE_OFT_3.TCR_ESTADISTICA.isna())]
BASE_OFT_4["TCR_OIC"] = BASE_OFT_4["VR_INICIAL_ARRIENDO"] / BASE_OFT_4["VR_INICIAL_VENTA"]
#BASE_OFT.loc[BASE_OFT["TCR_Sector"].isna(), ]
#BASE_OFT["TRC_localidad"]
#BASE_OFT["TRC_OIC"]

BASE_OFT_5 = BASE_OFT_4.loc[(BASE_OFT_4['TCR_OIC'] >= 0.0033) & (BASE_OFT_4['TCR_OIC'] <= 0.01)]
BASE_OFT_5["VENTA_ESTIMADA"] = BASE_OFT_5["VR_INICIAL_ARRIENDO"] / BASE_OFT_5["TCR_ESTADISTICA"]

BASE_OFT_5["DIFERENCIAS"] = abs(BASE_OFT_5['VR_INICIAL_VENTA']-BASE_OFT_5['VENTA_ESTIMADA'])/BASE_OFT_5.AREA_CONSTRUIDA_SIIC
BASE_OFT_5["DIFERENCIAS_TCR"] = abs(BASE_OFT_5['TCR_ESTADISTICA']-BASE_OFT_5['TCR_OIC'])


SUMMARY = BASE_OFT_5.groupby(["OFT_TIPO_INMUEBLE", "NOMBRE_LOCALIDAD", "CODIGO_ESTRATO_SIIC"]).\
    agg({"CODIGO_RESTO":[("NUM_PREDIOS", "count")], 
         "DIFERENCIAS":[("ERROR_TOTAL_MEDIO", "median")],
         "DIFERENCIAS_TCR":[("DIF_TCR", "median")]})
SUMMARY.columns = SUMMARY.columns.droplevel(0)
SUMMARY = SUMMARY.reset_index()



SUMMARY_CP = BASE_OFT_5.groupby(["OFT_TIPO_INMUEBLE"]).\
    agg({"CODIGO_RESTO":[("NUM_PREDIOS", "count")], 
         "DIFERENCIAS":[("ERROR_TOTAL_MEDIO", "median")],
         "DIFERENCIAS_TCR":[("DIF_TCR", "median")]})
SUMMARY_CP.columns = SUMMARY_CP.columns.droplevel(0)
SUMMARY_CP = SUMMARY_CP.reset_index()

SUMMARY_LOC = BASE_OFT_5.groupby(["NOMBRE_LOCALIDAD"]).\
    agg({"CODIGO_RESTO":[("NUM_PREDIOS", "count")], 
         "DIFERENCIAS":[("ERROR_TOTAL_MEDIO", "median")],
         "DIFERENCIAS_TCR":[("DIF_TCR", "median")]})
SUMMARY_LOC.columns = SUMMARY_LOC.columns.droplevel(0)
SUMMARY_LOC = SUMMARY_LOC.reset_index()

SUMMARY_EST = BASE_OFT_5.groupby(["CODIGO_ESTRATO_SIIC"]).\
    agg({"CODIGO_RESTO":[("NUM_PREDIOS", "count")], 
         "DIFERENCIAS":[("ERROR_TOTAL_MEDIO", "median")],
         "DIFERENCIAS_TCR":[("DIF_TCR", "median")]})
SUMMARY_EST.columns = SUMMARY_EST.columns.droplevel(0)
SUMMARY_EST = SUMMARY_EST.reset_index()
 
g = sns.scatterplot(x = 'TCR_OIC', y = 'TCR_ESTADISTICA',
              hue = "OFT_TIPO_INMUEBLE",
              data = BASE_OFT_5);

writer = pd.ExcelWriter("Z:/Trabajo/Catastro/210422_ESTUDIO_TASA_RENTA//output/"+date+"_BASE_OIC_ARRIENDO_VENTA.xlsx", engine='xlsxwriter')
BASE_OFT_5.to_excel(writer, sheet_name='BASE_FINAL',index=False)
SUMMARY.to_excel(writer, sheet_name = 'RESUMEN',index=False)
SUMMARY_CP.to_excel(writer, sheet_name = 'RESUMEN_CP',index=False)
SUMMARY_LOC.to_excel(writer, sheet_name = 'RESUMEN_LOC',index=False)
SUMMARY_EST.to_excel(writer, sheet_name = 'RESUMEN_EST',index=False)
#sample_25.to_excel(writer, sheet_name='MUESTRA_LOTES_25',index=False)
writer.save()

g = sns.scatterplot(x = 'VR_FINAL_VENTA', y = 'Venta_ estimado',
              hue = "OFT_TIPO_INMUEBLE",
              data = BASE_OFT_2.loc[BASE_OFT_2["OFT_TIPO_INMUEBLE"] == "APARTAMENTO"]);









SUMMARY_AREA_2=MARCO_FINAL.loc[(~MARCO_FINAL["LIMINF_AREA"].isna()) & (~MARCO_FINAL["LIMSUP_AREA"].isna()),["MODELO_0","BARMANPRE","CODIGO_USOP","MARCAS_AREA"]].\
        drop_duplicates().groupby(["MODELO_0","BARMANPRE","CODIGO_USOP"]).agg({"MARCAS_AREA":[("COUNT_PER_BMP","count")]})
        SUMMARY_AREA_2.columns = SUMMARY_AREA_2.columns.droplevel(0)
        SUMMARY_AREA_2=SUMMARY_AREA_2.reset_index()
 
help(np.corrcoef)
np.corrcoef(BASE_OFT_2.loc[~ BASE_OFT_2['Venta_ estimado'].isna(), 'VR_FINAL_VENTA'],
            BASE_OFT_2.loc[~ BASE_OFT_2['Venta_ estimado'].isna(), 'Venta_ estimado'])





fig, ax = plt.subplots(1, 1)
sns.boxplot(y = "DIFERENCIAS", data = BASE_OFT_3)
labels = ['%.5f' % float(t.get_text()) for t in ax.get_xticklabels()]
ax.set_xticklabels(labels)
plt.ticklabel_format(style='plain', axis='y')
sns.plt.ticklabel_format(style='plain', axis='y',useOffset=False)


pd_df['CAT_DIFERENCIA'] = "VACIO"
pd_df.loc[pd_df['Time'].between(0, 30, inclusive=False), 'CAT_DIFERENCIA'] = 'Easy'
pd_df.loc[pd_df['Time'].between(30, 60, inclusive=True), 'CAT_DIFERENCIA'] = 'Medium'
#BASE_OFT_3.loc[BASE_OFT_3["DIFERENCIAS"].between(-2350*10**6, -50*10**6, inclusive=False)]
#BASE_OFT_3.loc[BASE_OFT_3["DIFERENCIAS"].between(-50*10**6, -50*10**6, inclusive=False)]
#BASE_OFT_3.loc[BASE_OFT_3["DIFERENCIAS"].between(-30*10**6, -50*10**6, inclusive=False)]
#BASE_OFT_3.loc[BASE_OFT_3["DIFERENCIAS"].between(-15*10**6, -50*10**6, inclusive=False)]
#BASE_OFT_3.loc[BASE_OFT_3["DIFERENCIAS"].between(-5*10**6, 0, inclusive=False)]
BASE_OFT_3.loc[BASE_OFT_3["DIFERENCIAS"].between(0, 3*10**6, inclusive=False)]
BASE_OFT_3.loc[BASE_OFT_3["DIFERENCIAS"].between(3*10**6, 5*10**6, inclusive=False)]
BASE_OFT_3.loc[BASE_OFT_3["DIFERENCIAS"].between(5*10**6, 10*10**6, inclusive=False)]
BASE_OFT_3.loc[BASE_OFT_3["DIFERENCIAS"].between(10*10**6, 15*10**6, inclusive=False)]
BASE_OFT_3.loc[BASE_OFT_3["DIFERENCIAS"].between(15*10**6, 30*10**6, inclusive=False)]
BASE_OFT_3.loc[BASE_OFT_3["DIFERENCIAS"].between(30*10**6, 50*10**6, inclusive=False)]
BASE_OFT_3.loc[BASE_OFT_3["DIFERENCIAS"].between(50*10**6, 1000*10**6, inclusive=False)]
-50*10**6
-30*10**6
-15*10**6
-5*10**6
0
5*10**6
15*10**6
30*10**6
50*10**6

np.quantile(BASE_OFT_3.loc[BASE_OFT_3["OFT_TIPO_INMUEBLE"] == "APARTAMENTO", "DIFERENCIAS"],
            q = [0.05, 0.1, 0.25, 0.4, 0.45, 0.5, 0.55, 0.6, 0.75, 0.9, 0.95])/1000000
help(plt.scatter)
plt.scatter(BASE_OFT['TRC_OIC'], BASE_OFT['TCR_ESTADISTICA'], alpha=0.2,
             cmap='viridis', hue = BASE_OFT["OFT_TIPO_INMUEBLE"], s = 500)
plt.show()
plt.xlabel(iris.feature_names[0])
plt.ylabel(iris.feature_names[1]);

BASE_OFT.columns