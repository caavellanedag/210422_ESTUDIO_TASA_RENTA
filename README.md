# Estudio de tasa de renta

## Depuración y validación de ofertas 

* El primer filtro considera únicamente predios cuyo tipo es apartamento o casa (1072252).
* Predios sin barmanpre 63.615
* Predios sin área construida 169.392
* Casas sin área de terreno 77.690
* N. Baños 30.457
* Predios rurales 8.692
* Predios sin estrato 157.171

Restante 625.646.


El conjunto de registros correspondiente a las ofertas contiene información desde el primero de enero de 2017 hasta el 31 de diciembre de 2020. Considerando que se cuenta con datos, por un lado de ventas y arriendo, mientras que por otra parte predios en propiedad horizontal (PH) y no propiedad horizontal (NPH), además de diferentes estratos, se van a conformar diferentes agrupaciones de manera inicial. Las agrupaciones dependen del año de recolección del dato, clase de predio (PH y NPH), estrato y tipo de oferta (arriendo y venta). Es decir que, por ejemplo, las ofertas de arriendo en PH no se van a comparar con ofertas en arriendo en NPH, ni entre vigencias, ni entre estratos, ya que de antemano se espera que tengan un comportamiento diferente. Una vez se conforman estas agrupaciones, lo ideal es realizar comparaciones entre ofertas cercanas. Estas comparaciones se realizan con el objetivo de determinar si la oferta es similar en sus características a ofertas, debido a que en otro caso la información recolectada en ese registro presentaría anomalías y debería reportarse como atípica, al mismo tiempo que ser excluida de los análisis a desarrollar. 

Una manera de determinar registros próximos geográficamente, es seleccionando todos los que se encuentren en el mismo sector catastral, pero un primer inconveniente surge cuando en un sector solamente se encuentre una oferta o un número insuficiente de ofertas para realizar la comparación. Por este motivo, se observa la necesidad de realizar aglomeraciones más grandes de tal forma que se tengan grupos con un número suficiente de información para realizar la validación. Es importante mencionar que si el número de registros supera el umbral determinado, no se requiere una agrupación más grande y se va a aplicar el protocolo de comparación únicamente con las ofertas que se encuentran en ese barrio. 

Con el objetivo de tener agrupaciones de ofertas lo suficientemente grandes para tener información y realizar la comparación, pero de tal forma que no se agregue demasiado, considerando la singularidad de cada sector, en este documento se propone agrupar los sectores de acuerdo a su codificación. Se esperaría que los sectores cuya codificación es igual a diferencia del último dígito son más cercanos que dos sectores que tienen diferencias en sus dos últimos dígitos. Por ejemplo, se esperaría que los sectores "001101" y "001102" sean próximos geográficamente hablando. De esta manera, el algoritmo propuesto busca estas coincidencias a partir de la distancia de Levenshtein \cite{}, la cual es un valor entre 0 y 1. Esta distancia toma un valor de 0 cuando las dos codificaciones no tienen dígitos en común en las mismas posiciones y toma el valor de 1 cuando son exactamente iguales. El algoritmo realiza la busqueda iterativa de sectores similares, iniciando por los últimos dígitos, de forma que si no encuentra sectores con el mismo código y último dígito diferente, procede a buscar coincidencias exceptuando los dos últimos dígitos y así sucesivamente hasta superar el umbral definido. Una vez el algoritmo encuentra agrupaciones tal que todas superan dicho valor predefinido se detiene y no continua con el proceso de agregación. 

Hablar del umbral

* Realizar las agrupaciones de manera iterativa mediante la distancia de Levenshtein entre los código de barrio.
* Las edades, puntajes y área en NA, se cambian por la mediana correspondiente a la agrupación
* El número de garajes y de alcobas en NA se cambia por 0.
* Una vez se tenga la información depurada y las agrupaciones conformadas, para cada combinación de año, clase de predio, estrato y tipo de oferta, se realiza:
	+ Para cada agrupación de sectores (o sectores en su defecto) se elabora una carta de control de calidad multivariada.
	+ Se utiliza una carta de control multivariada, debido a que cada predio tiene valores de precio, área, edad y puntaje, las cuales también pueden presentar atipicidades.
	+ Un predio puede tener un precio y área dentro de rangos aceptables por separado, pero al observarlos de manera conjunta puede que la conclusión sea diferente. De esta manera, dentro del proceso de control de calidad se hace uso de la relación entre variables.
	+ Las cartas de control, por lo general utilizan dos fases. La primer fase es de estimación, mientras que la segunda fase utiliza las estimaciones previas para determinar puntos atipicos. En este caso, ya que no se van a tener observaciones adicionales dentro de cada grupo y como el objetivo principal no es realizar el control de calidad de la información, si no que es un objetivo secundario, se procede sin tener en cuenta las dos fases. Esto quiere decir que se va a considerar una única fase, donde los predios que se determinen fuera de los límites de control se van a considerar como atípicos y van a ser excluidos. 
	+ Como muchos procesos estadísticos, la realización de la carta de control multivariada requiere que la matriz de información sea de rango completo, lo que quiere decir que una de las variables no sea combinación lineal de las otras, lo cual sería redundante. De esta forma, dentro del proceso de realización del algoritmo si la matriz no es de rango completo se descartan variables hasta obtener una matriz no singular de acuerdo al siguiente orden: "NUMERO_ALCOBAS", "AREA_DE_TERRENO", "AREA_CONSTRUIDA", "ANTIGUEDAD_INMUEBLE", "MED_PUNTAJE", "VALOR_M2_INTEGRAL" y "PRECIO".

Una vez se determinen los registros atipicos se procede con el resto del análisis.



