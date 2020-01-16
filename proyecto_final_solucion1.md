---
title: "Carga inicial datos: Physionet 2012 UCI data mortality"
author: "Arturo González, Jun De Wu, Ítria Mascaró"
time:  "`Sys.Date()`"
output: 
  html_document: 
    keep_md: yes
    number_sections: yes
    toc: yes
  pdf_document: 
    number_sections: yes
    toc: yes
---



# Introducción: Physionet 2012 UCI data mortality


En el concurso del congreso ["Computers in  Cardiology" (ahora "Computing in Cardiology") del año 2012](https://physionet.org/content/challenge-2012/) propuso un  caso de estudio como reto: *Predicción de la tasa de mortalidad de los pacientes de una UCI*

Resto de años mas recientes 

* https://physionet.org/content/challenge-2018/
* https://physionet.org/content/challenge-2019/



##   Enlaces de interés

[**HR**: Heart Rate bpm beats per minut](https://en.wikipedia.org/wiki/Heart_rate)

[**GCS**: Glasgow Comma Score (scale 3-15)](https://en.wikipedia.org/wiki/Glasgow_Coma_Scale)

[**RespRate**:  Respiration rate (bpm) breaths for one minute](https://en.wikipedia.org/wiki/Respiratory_rate)


#  Ingesta de datos


## Modelo de datos 


```r
# Cargamos los datos
path = "data_basic_physionet/set-a/"# path training
# Creamos un vector con los nombres de los archivos
lista_pacientes_set_a = dir(path) # lista  ficheros pacientes 
# Printamos número de archivos que leemos
length(lista_pacientes_set_a) # número pacientes en training
```

```
## [1] 4000
```

```r
# Mostramos como ejemplo el nombre del documento 1 de los datos 
lista_pacientes_set_a[1]
```

```
## [1] "132539.txt"
```
## Descripción de las variables

- `Edad`

- `Género`

- `Tipo UCI`

- `SAPSI` ("Simplified Acute Physiology Score" estima la probabilitat de mortalitat del pacient d'UCI.)

- `SOFA` ("Sequential Organ Failure Assessment"  Es utilizado para seguir el estado del paciente durante su estadía en la Unidad de UC.)

- `LOS`

- `Survival`

- `In-hospital-death`


```
data_paciente_132539=read_csv("data_basic_physionet/set-a/132539.txt", col_types =cols(Time=col_time(format="%M:%S"),Parameter=col_character(),
Value=col_double()))
str(data_paciente_132539)
glimpse(data_paciente_132539)
class(data_paciente_132539)
head(data_paciente_132539,30)
```

## Carga set_a


```r
# lista path's  a cada  ficjero de paciente
list_files = paste0(path,lista_pacientes_set_a)
# Función leer paciente
# Leemos el tiempo como carácter y después haremos un ajuste que nos lo simplifique todo a minutos.  
leer_paciente = function(file){read_csv(file, col_types = cols(Time = col_character(),
                                                               Parameter = col_character(),
                                                               Value = col_double())) %>%
# Separamos las horas de los minutos de la columna Time para acto seguido poner una sola columna 
# llamada Time_min sólo con los minutos en que se tomaron los datos.
                              separate(Time,into = c("H","M"),sep = ":") %>% 
                              mutate(Time_Minutes = as.numeric(H)*60+as.numeric(M)) %>% 
                              select(Time_Minutes,Parameter,Value)}

#leer_paciente(list_files[1])

raw_data = lapply(list_files,leer_paciente) # lista de los datos por paciente

#extraer perfiles "RecordID" "Age"  "Gender"   "Height"   "Weight"   "ICUType" 
perfil = function(data_paciente){
            data_paciente %>%
            filter(Parameter %in% c("RecordID","Age","Gender","Height","ICUType","Weight")) %>%
            select(-Time_Minutes) %>%
            distinct(Parameter,.keep_all = TRUE) %>%
            spread(Parameter,Value) }
## ejemplo
#perfil(data_paciente_132539)
## Guardo  todos los datos  del perfil de cada paciente
perfiles =  lapply(raw_data,perfil) %>%
            bind_rows() %>%
            select(RecordID, Age, Gender, Height,Weight,ICUType)

glimpse(perfiles) # Printamos la funcion
```

```
## Observations: 4,000
## Variables: 6
## $ RecordID <dbl> 132539, 132540, 132541, 132543, 132545, 132547, 13254...
## $ Age      <dbl> 54, 76, 44, 68, 88, 64, 68, 78, 64, 74, 64, 71, 66, 8...
## $ Gender   <dbl> 0, 1, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 1, 1, 1, 1,...
## $ Height   <dbl> -1.0, 175.3, -1.0, 180.3, -1.0, 180.3, 162.6, 162.6, ...
## $ Weight   <dbl> -1.0, 76.0, 56.7, 84.6, -1.0, 114.0, 87.0, 48.4, 60.7...
## $ ICUType  <dbl> 4, 2, 3, 3, 3, 1, 3, 3, 3, 2, 3, 2, 3, 1, 1, 2, 3, 3,...
```

```r
# Arreglamos el error de time
serie_UCI_parameter <- function(paciente,parameters){ paciente %>%
                                                      arrange(Parameter,Time_Minutes) %>%
                                                      filter(Parameter %in% parameters) %>%
                                                      add_column(RecordID = paciente[1,3]$Value) } 
##ejemplo
parameters = c("HR","RespRate","GCS")
serie_paciente1 = serie_UCI_parameter(raw_data[[1]],parameters)
serie_paciente1
```

```
## # A tibble: 92 x 4
##    Time_Minutes Parameter Value RecordID
##           <dbl> <chr>     <dbl>    <dbl>
##  1            7 GCS          15   132539
##  2          217 GCS          15   132539
##  3          457 GCS          15   132539
##  4          697 GCS          15   132539
##  5          937 GCS          15   132539
##  6         1177 GCS          15   132539
##  7         1417 GCS          15   132539
##  8         1657 GCS          15   132539
##  9         1897 GCS          14   132539
## 10         2137 GCS          15   132539
## # ... with 82 more rows
```

```r
# paso TODOS los parámetros y  apilo 
parameters=c("Albumin","ALP","ALT","AST","Bilirubin","BUN","Cholesterol","Creatinine","DiasABP","FiO2","GCS","Glucose","HCO3","HCT","HR","K","Lactate","Mg","MAP","MechVent","Na","NIDiasABP","NIMAP","NISysABP","PaCO2","PaO2","pH","Platelets","RespRate","SaO2","SysABP","Temp","TropI","TropT","Urine","WBC")
series_parameters = lapply(raw_data,FUN=function(x) serie_UCI_parameter(x,parameters)) %>%
                    bind_rows()

glimpse(series_parameters) # Printamos la función
```

```
## Observations: 1,606,254
## Variables: 4
## $ Time_Minutes <dbl> 637, 1987, 637, 1987, 7, 217, 457, 697, 937, 1177...
## $ Parameter    <chr> "BUN", "BUN", "Creatinine", "Creatinine", "GCS", ...
## $ Value        <dbl> 13.0, 8.0, 0.8, 0.7, 15.0, 15.0, 15.0, 15.0, 15.0...
## $ RecordID     <dbl> 132539, 132539, 132539, 132539, 132539, 132539, 1...
```




## En resumen  tenemos


```r
#set-a
glimpse(perfiles)
```

```
## Observations: 4,000
## Variables: 6
## $ RecordID <dbl> 132539, 132540, 132541, 132543, 132545, 132547, 13254...
## $ Age      <dbl> 54, 76, 44, 68, 88, 64, 68, 78, 64, 74, 64, 71, 66, 8...
## $ Gender   <dbl> 0, 1, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 1, 1, 1, 1,...
## $ Height   <dbl> -1.0, 175.3, -1.0, 180.3, -1.0, 180.3, 162.6, 162.6, ...
## $ Weight   <dbl> -1.0, 76.0, 56.7, 84.6, -1.0, 114.0, 87.0, 48.4, 60.7...
## $ ICUType  <dbl> 4, 2, 3, 3, 3, 1, 3, 3, 3, 2, 3, 2, 3, 1, 1, 2, 3, 3,...
```

```r
glimpse(series_parameters)
```

```
## Observations: 1,606,254
## Variables: 4
## $ Time_Minutes <dbl> 637, 1987, 637, 1987, 7, 217, 457, 697, 937, 1177...
## $ Parameter    <chr> "BUN", "BUN", "Creatinine", "Creatinine", "GCS", ...
## $ Value        <dbl> 13.0, 8.0, 0.8, 0.7, 15.0, 15.0, 15.0, 15.0, 15.0...
## $ RecordID     <dbl> 132539, 132539, 132539, 132539, 132539, 132539, 1...
```



## Unificar: series, perfiles y scores

Nos faltan los scores clásicos que se utilizan eb las ICU. Estos ewstán el fichero Outcome-a.txt para el set-a




```r
scoresApath = "data_basic_physionet/Outcomes-a.txt"
scoresA = read_csv(scoresApath)
```

```
## Parsed with column specification:
## cols(
##   RecordID = col_double(),
##   `SAPS-I` = col_double(),
##   SOFA = col_double(),
##   Length_of_stay = col_double(),
##   Survival = col_double(),
##   `In-hospital_death` = col_double()
## )
```

```r
glimpse(scoresA)
```

```
## Observations: 4,000
## Variables: 6
## $ RecordID            <dbl> 132539, 132540, 132541, 132543, 132545, 13...
## $ `SAPS-I`            <dbl> 6, 16, 21, 7, 17, 14, 14, 19, 11, 14, 15, ...
## $ SOFA                <dbl> 1, 8, 11, 1, 2, 11, 4, 8, 0, 6, 2, 7, 2, 7...
## $ Length_of_stay      <dbl> 5, 8, 19, 9, 4, 6, 9, 6, 17, 8, 13, 7, 22,...
## $ Survival            <dbl> -1, -1, -1, 575, 918, 1637, -1, 5, 38, -1,...
## $ `In-hospital_death` <dbl> 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, ...
```

```r
Scores_perfilesA = inner_join(perfiles,scoresA,"RecordID")
glimpse(Scores_perfilesA)
```

```
## Observations: 4,000
## Variables: 11
## $ RecordID            <dbl> 132539, 132540, 132541, 132543, 132545, 13...
## $ Age                 <dbl> 54, 76, 44, 68, 88, 64, 68, 78, 64, 74, 64...
## $ Gender              <dbl> 0, 1, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, ...
## $ Height              <dbl> -1.0, 175.3, -1.0, 180.3, -1.0, 180.3, 162...
## $ Weight              <dbl> -1.0, 76.0, 56.7, 84.6, -1.0, 114.0, 87.0,...
## $ ICUType             <dbl> 4, 2, 3, 3, 3, 1, 3, 3, 3, 2, 3, 2, 3, 1, ...
## $ `SAPS-I`            <dbl> 6, 16, 21, 7, 17, 14, 14, 19, 11, 14, 15, ...
## $ SOFA                <dbl> 1, 8, 11, 1, 2, 11, 4, 8, 0, 6, 2, 7, 2, 7...
## $ Length_of_stay      <dbl> 5, 8, 19, 9, 4, 6, 9, 6, 17, 8, 13, 7, 22,...
## $ Survival            <dbl> -1, -1, -1, 575, 918, 1637, -1, 5, 38, -1,...
## $ `In-hospital_death` <dbl> 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, ...
```


### Extracción factores de las series 

genero una tabla con resumenes de  las variables por paciente: media, desviación típica 


```r
series_summary =  series_parameters %>%
                  group_by(RecordID,Parameter) %>%
                  summarise(count = n(),mean = mean(Value,na.rm = TRUE),
                            sd = sd(Value,na.rm=TRUE)) %>%
                  gather(Stat, Value, count:sd) %>%
                  ungroup() %>%
                  transmute(RecordID,ParameterStat = paste0(Parameter,"_",Stat),Value) %>%
                  spread(ParameterStat, Value)
```



```r
data_tidy = Scores_perfilesA %>% inner_join(series_summary)
```

```
## Joining, by = "RecordID"
```

## LIMPIEZA NA's

Si quisiéramos limpiar los NA's de una variable cualquiera, por ejemplo `Lactate_count`, podríamos realizar las siguientes instrucciones en R:


```r
# aux te marcará las filas donde existen NA's en la variables Lactate_count
aux = which(is.na(data_tidy$Lactate_count))
# El número de NA's en la variable 
length(aux)
```

```
## [1] 1814
```

```r
# Quitamos las filas donde aparecen NA's 
data_tidy2 = data_tidy[-aux,]
```

Pero hay un problema. Si hacemos esta limpieza para todas las variables de golpe, nos quedaremos sin datos, ya que en cada fila siempre hay un NA. ¿Cómo podemos hacer la limpieza de forma óptima?

Empezamos quitando las variables con más NA's, que son:

- `ALP`

- `Bilirubina`

- `Lactate`

- `Resp Rate`

Ya que nos hemos creado una función que nos filtre todas las variables que superen o no el umbral de 1500 NA's:


```r
#Definimos una función que nos cuente el número de NA's en una variable x
contar_nas = function(x){
  sum(is.na(x))
}

#Definimos un programa que la variable x marque TRUE cuando tiene menos de 1500 NA's
pocos_nas = function(x){
  for(i in 1:length(x)){
    if(contar_nas(x) >= 1500){
      return(FALSE)
    }
    else{
      return(TRUE)
    }
  }
}


#Aplicamos la función de TRUE o FALSE para saber qué variables pasan el umbral de 1500 NA's y cuáles no
seleccion_pocos_nas = data_tidy %>%
  apply( 2, pocos_nas)


# Guardamos sólo los nombres de las variables que en "<VARIABLE>_count" está por debajo los 1500 NA's
nombres = names(seleccion_pocos_nas[seleccion_pocos_nas])
nombres
```

```
##  [1] "RecordID"          "Age"               "Gender"           
##  [4] "Height"            "Weight"            "ICUType"          
##  [7] "SAPS-I"            "SOFA"              "Length_of_stay"   
## [10] "Survival"          "In-hospital_death" "BUN_count"        
## [13] "BUN_mean"          "BUN_sd"            "Creatinine_count" 
## [16] "Creatinine_mean"   "Creatinine_sd"     "DiasABP_count"    
## [19] "DiasABP_mean"      "DiasABP_sd"        "FiO2_count"       
## [22] "FiO2_mean"         "FiO2_sd"           "GCS_count"        
## [25] "GCS_mean"          "GCS_sd"            "Glucose_count"    
## [28] "Glucose_mean"      "Glucose_sd"        "HCO3_count"       
## [31] "HCO3_mean"         "HCO3_sd"           "HCT_count"        
## [34] "HCT_mean"          "HCT_sd"            "HR_count"         
## [37] "HR_mean"           "HR_sd"             "K_count"          
## [40] "K_mean"            "K_sd"              "MAP_count"        
## [43] "MAP_mean"          "MAP_sd"            "MechVent_count"   
## [46] "MechVent_mean"     "MechVent_sd"       "Mg_count"         
## [49] "Mg_mean"           "Mg_sd"             "Na_count"         
## [52] "Na_mean"           "Na_sd"             "NIDiasABP_count"  
## [55] "NIDiasABP_mean"    "NIDiasABP_sd"      "NIMAP_count"      
## [58] "NIMAP_mean"        "NIMAP_sd"          "NISysABP_count"   
## [61] "NISysABP_mean"     "NISysABP_sd"       "PaCO2_count"      
## [64] "PaCO2_mean"        "PaCO2_sd"          "PaO2_count"       
## [67] "PaO2_mean"         "PaO2_sd"           "pH_count"         
## [70] "pH_mean"           "pH_sd"             "Platelets_count"  
## [73] "Platelets_mean"    "Platelets_sd"      "SysABP_count"     
## [76] "SysABP_mean"       "SysABP_sd"         "Temp_count"       
## [79] "Temp_mean"         "Temp_sd"           "Urine_count"      
## [82] "Urine_mean"        "Urine_sd"          "WBC_count"        
## [85] "WBC_mean"          "WBC_sd"
```

```r
#Guardamos en un data set nuevo las variables que nos ha interesado seleccionar con el criterio de NA's
primera_seleccion = select(data_tidy, nombres)
```

## SELECCIÓN 10 VARIABLES INICIALES

El criterio de primera selección se basará en considerar las que tengan menos NA's como las más útiles para nuestro estudio.


```r
# DE LAS SELECCIONADAS, MIRAMOS LAS QUE TIENEN MENOS NA'S Y LAS REORDENAMOS PARA ELEGIR LAS 10 PRIMERAS
# Para hacerlo nos fijamos sólo en las variables: "<VARIABLE>_sd" mediante un select
seleccion_menor_nas = select(primera_seleccion, ends_with("sd")) %>% apply(2, contar_nas)
seleccion_menor_nas
```

```
##        BUN_sd Creatinine_sd    DiasABP_sd       FiO2_sd        GCS_sd 
##           109           107          1216          1353            63 
##    Glucose_sd       HCO3_sd        HCT_sd         HR_sd          K_sd 
##           320           146           128            61           243 
##        MAP_sd   MechVent_sd         Mg_sd         Na_sd  NIDiasABP_sd 
##          1225          1490           307           177           645 
##      NIMAP_sd   NISysABP_sd      PaCO2_sd       PaO2_sd         pH_sd 
##           649           642          1292          1295          1276 
##  Platelets_sd     SysABP_sd       Temp_sd      Urine_sd        WBC_sd 
##           159          1216            62           121           173
```

```r
# LAS REORDENAMOS PARA PODER COGER SÓLO LAS 10 PRIMERAS
nombres_pocos_nas = sort(seleccion_menor_nas)
nombres_pocos_nas
```

```
##         HR_sd       Temp_sd        GCS_sd Creatinine_sd        BUN_sd 
##            61            62            63           107           109 
##      Urine_sd        HCT_sd       HCO3_sd  Platelets_sd        WBC_sd 
##           121           128           146           159           173 
##         Na_sd          K_sd         Mg_sd    Glucose_sd   NISysABP_sd 
##           177           243           307           320           642 
##  NIDiasABP_sd      NIMAP_sd    DiasABP_sd     SysABP_sd        MAP_sd 
##           645           649          1216          1216          1225 
##         pH_sd      PaCO2_sd       PaO2_sd       FiO2_sd   MechVent_sd 
##          1276          1292          1295          1353          1490
```

```r
# COGEMOS LAS 10 PRIMERAS
nombres_def = nombres_pocos_nas[1:10]
nombres_def
```

```
##         HR_sd       Temp_sd        GCS_sd Creatinine_sd        BUN_sd 
##            61            62            63           107           109 
##      Urine_sd        HCT_sd       HCO3_sd  Platelets_sd        WBC_sd 
##           121           128           146           159           173
```

```r
# DE FORMA MANUAL, VOLVEMOS AL DATA SET ORIGINAL Y SELECCIONAMOS LAS 10 VARIABLES DE "nombres_def"
data_real_tidy = select(data_tidy, contains("RecordID"),  contains("Age"),  contains("Gender"),  contains("Height"),  contains("Weight"),  contains("ICUType"),  contains("SAPS-I"),  contains("SOFA"),  contains("Length_of_stay"),  contains("Survival"),  contains("In-hospital_death"), contains("HR"), contains("Temp"), contains("GCS"), contains("Creatinine"),  contains("BUN"),  contains("Urine"),  contains("HCT"),  contains("HCO3"),  contains("Platelets"),  contains("WBC"))

# Eliminamos los na's que todavía hay y vemos que esta vez no desaparecen todos los datos, 
# así pues habremos hecho una limpieza que habrá merecido la pena :)
drop_na(data_real_tidy)
```

```
## # A tibble: 3,676 x 41
##    RecordID   Age Gender Height Weight ICUType `SAPS-I`  SOFA
##       <dbl> <dbl>  <dbl>  <dbl>  <dbl>   <dbl>    <dbl> <dbl>
##  1   132539    54      0    -1    -1         4        6     1
##  2   132540    76      1   175.   76         2       16     8
##  3   132541    44      0    -1    56.7       3       21    11
##  4   132543    68      1   180.   84.6       3        7     1
##  5   132545    88      0    -1    -1         3       17     2
##  6   132547    64      1   180.  114         1       14    11
##  7   132548    68      0   163.   87         3       14     4
##  8   132551    78      0   163.   48.4       3       19     8
##  9   132555    74      1   175.   66.1       2       14     6
## 10   132556    64      0    -1    65         3       15     2
## # ... with 3,666 more rows, and 33 more variables: Length_of_stay <dbl>,
## #   Survival <dbl>, `In-hospital_death` <dbl>, HR_count <dbl>,
## #   HR_mean <dbl>, HR_sd <dbl>, Temp_count <dbl>, Temp_mean <dbl>,
## #   Temp_sd <dbl>, GCS_count <dbl>, GCS_mean <dbl>, GCS_sd <dbl>,
## #   Creatinine_count <dbl>, Creatinine_mean <dbl>, Creatinine_sd <dbl>,
## #   BUN_count <dbl>, BUN_mean <dbl>, BUN_sd <dbl>, Urine_count <dbl>,
## #   Urine_mean <dbl>, Urine_sd <dbl>, HCT_count <dbl>, HCT_mean <dbl>,
## #   HCT_sd <dbl>, HCO3_count <dbl>, HCO3_mean <dbl>, HCO3_sd <dbl>,
## #   Platelets_count <dbl>, Platelets_mean <dbl>, Platelets_sd <dbl>,
## #   WBC_count <dbl>, WBC_mean <dbl>, WBC_sd <dbl>
```

## SELECCIÓN 5 VARIABLES

Para elegir las 5 que nos aporten mas información a la hora de predecir las muertes, realizamos los siguientes boxplots y contrastes de hipótesis para encontrar aquellas que la muerte y la vida tengan más variación.


```r
# Hacemos contraste de hipótesis para saber si deberiamos considerar las
# varianzas iguales o diferentes para el contraste de medias que haremos a continuación.
test_varianzas = function(columna){
  # hip nula : varianzas iguales
  # hip alternativa : diferentes 
  x = var.test(columna[data_real_tidy$`In-hospital_death` == 0],
               columna[data_real_tidy$`In-hospital_death` == 1])
  alpha = x$p.value
  if(alpha < 0.05){
    return(FALSE)
  }
  else{
    return(TRUE)
  }
}

# Hacemos contraste de hipótesis para saber si las medias son iguales o no.
test_medias = function(columna){
  # hip nula : medias son iguales 
  # hip alternativa: se indica en alternative = c("two.sided", "less", "greater")
  x = t.test(columna[data_real_tidy$`In-hospital_death` == 0],
             columna[data_real_tidy$`In-hospital_death` == 1],
             var.equal = test_varianzas(columna)) # Aquí aplicamos la función anterior como habíamos dicho
  return(x$p.value)
}

respuesta_test = function(columna){
  x = t.test(columna[data_real_tidy$`In-hospital_death` == 0],
             columna[data_real_tidy$`In-hospital_death` == 1],
             var.equal = test_varianzas(columna)) # Aplicamos la función anterior para considerar o no var. iguales o diferentes
  alpha = x$p.value
  if(alpha < 0.05){
    print('Rechazamos: Las medias son diferentes')
  }
  else{
    print('Aceptamos: Las medias son iguales <-')
  }
}


# SELECCIÓN 5 VARIABLES MÁS RELEVANTES PARA PREDECIR LA MUERTE
# Para que podamos hacer los boxplot bien, antes debemos hacer factor la columna de "Muertes en el hospital"
data_real_tidy$`In-hospital_death` = as.factor(data_real_tidy$`In-hospital_death`)
# Creamos un array "vacio" que contendrá los valores del p.value del test de medias
p_valores = c(1:10)

# LATIDO CORAZON -------------------------------------------------------------------------
p1 <- data_real_tidy %>%
  ggplot() +
  geom_boxplot(aes( x = `In-hospital_death`, y = HR_mean)) +
  labs(x = " vive | muere", y = "Latido corazon")


# Hacemos el test de medias de cada una de las variables
print("Variable: LATIDO CORAZÓN")
```

```
## [1] "Variable: LATIDO CORAZÓN"
```

```r
# Guardamos el p_valor para más tarde considerar las 5 mejores como aquellas con el p_valor más bajo
p_valores[1] = test_medias(data_real_tidy$HR_mean)
# Imprimimos por pantalla la resolución del test de medias (teniendo el de varianzas ya resuelto dentro del de medias)
respuesta_test(data_real_tidy$HR_mean)
```

```
## [1] "Rechazamos: Las medias son diferentes"
```

```r
# TEMPERATURA ----------------------------------------------------------------------------
p2 <- data_real_tidy %>%
  ggplot() +
  geom_boxplot(aes( x = `In-hospital_death`, y = Temp_mean)) +
  labs(x = " vive | muere", y = "Temperatura")

print("Variable: TEMPERATURA")
```

```
## [1] "Variable: TEMPERATURA"
```

```r
p_valores[2] = test_medias(data_real_tidy$Temp_mean)
respuesta_test(data_real_tidy$Temp_mean)
```

```
## [1] "Rechazamos: Las medias son diferentes"
```

```r
# GLASGOW --------------------------------------------------------------------------------
p3 <- data_real_tidy %>%
  ggplot() +
  geom_boxplot(aes( x = `In-hospital_death`, y = GCS_mean)) +
  labs(x = " vive | muere", y = "Glasgow")

print("Variable: GLASGOW")
```

```
## [1] "Variable: GLASGOW"
```

```r
p_valores[3] = test_medias(data_real_tidy$GCS_mean)
respuesta_test(data_real_tidy$GCS_mean)
```

```
## [1] "Rechazamos: Las medias son diferentes"
```

```r
# CREATININA -----------------------------------------------------------------------------
p4 <- data_real_tidy %>%
  ggplot() +
  geom_boxplot(aes( x = `In-hospital_death`, y = Creatinine_mean)) +
  labs(x = " vive | muere", y = "Creatinina")

print("Variable: CREATININA")
```

```
## [1] "Variable: CREATININA"
```

```r
p_valores[4] = test_medias(data_real_tidy$Creatinine_mean)
respuesta_test(data_real_tidy$Creatinine_mean)
```

```
## [1] "Rechazamos: Las medias son diferentes"
```

```r
# NITROGENO EN SANGRE --------------------------------------------------------------------
p5 <- data_real_tidy %>%
  ggplot() +
  geom_boxplot(aes( x = `In-hospital_death`, y = BUN_mean)) +
  labs(x = " vive | muere", y = "Ni en sangre")

print("Variable: NITROGENO EN SANGRE")
```

```
## [1] "Variable: NITROGENO EN SANGRE"
```

```r
p_valores[5] = test_medias(data_real_tidy$BUN_mean)
respuesta_test(data_real_tidy$BUN_mean)
```

```
## [1] "Rechazamos: Las medias son diferentes"
```

```r
# ORINA ----------------------------------------------------------------------------------
p6 <- data_real_tidy %>%
  ggplot() +
  geom_boxplot(aes( x = `In-hospital_death`, y = Urine_mean)) +
  labs(x = " vive | muere", y = "Orina")

print("Variable: ORINA")
```

```
## [1] "Variable: ORINA"
```

```r
p_valores[6] = test_medias(data_real_tidy$Urine_mean)
respuesta_test(data_real_tidy$Urine_mean)
```

```
## [1] "Rechazamos: Las medias son diferentes"
```

```r
# HEMATOCRITO ----------------------------------------------------------------------------
p7 <- data_real_tidy %>%
  ggplot() +
  geom_boxplot(aes( x = `In-hospital_death`, y = HCT_mean)) +
  labs(x = " vive | muere", y = "Hematocrito")

print("Variable: HEMATOCRITO")
```

```
## [1] "Variable: HEMATOCRITO"
```

```r
p_valores[7] = test_medias(data_real_tidy$HCT_mean) # Se acepta que las medias son iguales, así HTC no nos interesa
respuesta_test(data_real_tidy$HCT_mean)
```

```
## [1] "Aceptamos: Las medias son iguales <-"
```

```r
# SERUM BICARBONADO ----------------------------------------------------------------------
p8 <- data_real_tidy %>%
  ggplot() +
  geom_boxplot(aes( x = `In-hospital_death`, y = HCO3_mean)) +
  labs(x = " vive | muere", y = "Serum bicarbonado")

print("Variable: SERUM BICARBONADO")
```

```
## [1] "Variable: SERUM BICARBONADO"
```

```r
p_valores[8] = test_medias(data_real_tidy$HCO3_mean)
respuesta_test(data_real_tidy$HCO3_mean)
```

```
## [1] "Rechazamos: Las medias son diferentes"
```

```r
# PLAQUETAS ------------------------------------------------------------------------------
p9 <- data_real_tidy %>%
  ggplot() +
  geom_boxplot(aes( x = `In-hospital_death`, y = Platelets_mean)) +
  labs(x = " vive | muere", y = "Plaquetas")

print("Variable: PLAQUETAS")
```

```
## [1] "Variable: PLAQUETAS"
```

```r
p_valores[9] = test_medias(data_real_tidy$Platelets_mean) 
respuesta_test(data_real_tidy$Platelets_mean) # Se acepta que las medias son iguales, así Platelets no nos interesa
```

```
## [1] "Aceptamos: Las medias son iguales <-"
```

```r
# GLOBULOS BLANCOS SANGRE ----------------------------------------------------------------
p10 <- data_real_tidy %>%
  ggplot() +
  geom_boxplot(aes( x = `In-hospital_death`, y = WBC_mean)) +
  labs(x = " vive | muere", y = "Globulos blancos")

print("Variable: GLOBULOS BLANCOS EN SANGRE")
```

```
## [1] "Variable: GLOBULOS BLANCOS EN SANGRE"
```

```r
p_valores[10] = test_medias(data_real_tidy$WBC_mean)
respuesta_test(data_real_tidy$WBC_mean)
```

```
## [1] "Rechazamos: Las medias son diferentes"
```

```r
# Imprimimos los boxplot de forma más compacta para ahorrar espacio
grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10, nrow = 2)
```

![](proyecto_final_solucion1_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

Hemos descubierto que Platelets y HTC no vale la pena considerarlos. Falta elegir de entre los 8 restantes, cuales son los que presentan mayor varianza. Ésto se puede ver mejor en los boxplot.

Lo que nos interesa ahora es coger de las que sí presentan diferencias, elegir las que el p-valor sea más bajo ya que serán aquellas que nos aportarán más información.


```r
# Creamos un array que nos guarde los nombres de las 10 variables con las que trabajamos
diez_variables = names(select(data_real_tidy, ends_with("mean")))

# Creamos una tibble que de cada variable nos diga su p-valor en el contraste de medias
tabla_p_valores = as_tibble(data.frame(diez_variables, p_valores))
tabla_p_valores
```

```
## # A tibble: 10 x 2
##    diez_variables  p_valores
##    <fct>               <dbl>
##  1 HR_mean          4.12e- 5
##  2 Temp_mean        6.32e- 4
##  3 GCS_mean         5.19e-39
##  4 Creatinine_mean  6.38e-11
##  5 BUN_mean         1.36e-28
##  6 Urine_mean       4.96e-10
##  7 HCT_mean         5.20e- 1
##  8 HCO3_mean        9.27e-12
##  9 Platelets_mean   2.31e- 1
## 10 WBC_mean         2.43e- 5
```

```r
# Las reordenamos de menor a mayor para quedarnos sólo con las 5 primeras (con el p-valor más bajo)
cinco_variables = arrange(tabla_p_valores,p_valores)[1:5,1]
cinco_variables
```

```
## # A tibble: 5 x 1
##   diez_variables 
##   <fct>          
## 1 GCS_mean       
## 2 BUN_mean       
## 3 HCO3_mean      
## 4 Creatinine_mean
## 5 Urine_mean
```

Ahora que ya sabemos los nombres de las 5 variables que nos ayudarán a hacer un estudio más preciso, creamos la tabla final de datos sólo con estas variables.


```r
# Selecionamos manualmente (como antes) las 5 variables que usaremos durante lo que queda de trabajo
data_real_tidy = select(data_tidy, contains("RecordID"),  contains("Age"),  contains("Gender"),  contains("Height"),  contains("Weight"),  contains("ICUType"),  contains("SAPS-I"),  contains("SOFA"),  contains("Length_of_stay"),  contains("Survival"),  contains("In-hospital_death"), contains("GCS"), contains("BUN"),  contains("Urine"), contains("HCO3"), contains("Creatinine"))

# Volvemos a hacer factor el `In-hospital_death`
data_real_tidy$`In-hospital_death` = as.factor(data_real_tidy$`In-hospital_death`)

# Vemos que hay dos pacientes que por error su género se indica como -1, así pues, quitamos estas filas
data_real_tidy = filter(data_real_tidy,Gender != -1)
# Quitamos los NA's que hayan podido quedar
data_real_tidy = drop_na(data_real_tidy)


# Mostramos por pantalla la tabla final
head(data_real_tidy) %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
  scroll_box(width = "910px")
```

<div style="border: 1px solid #ddd; padding: 5px; overflow-x: scroll; width:910px; "><table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:right;"> RecordID </th>
   <th style="text-align:right;"> Age </th>
   <th style="text-align:right;"> Gender </th>
   <th style="text-align:right;"> Height </th>
   <th style="text-align:right;"> Weight </th>
   <th style="text-align:right;"> ICUType </th>
   <th style="text-align:right;"> SAPS-I </th>
   <th style="text-align:right;"> SOFA </th>
   <th style="text-align:right;"> Length_of_stay </th>
   <th style="text-align:right;"> Survival </th>
   <th style="text-align:left;"> In-hospital_death </th>
   <th style="text-align:right;"> GCS_count </th>
   <th style="text-align:right;"> GCS_mean </th>
   <th style="text-align:right;"> GCS_sd </th>
   <th style="text-align:right;"> BUN_count </th>
   <th style="text-align:right;"> BUN_mean </th>
   <th style="text-align:right;"> BUN_sd </th>
   <th style="text-align:right;"> Urine_count </th>
   <th style="text-align:right;"> Urine_mean </th>
   <th style="text-align:right;"> Urine_sd </th>
   <th style="text-align:right;"> HCO3_count </th>
   <th style="text-align:right;"> HCO3_mean </th>
   <th style="text-align:right;"> HCO3_sd </th>
   <th style="text-align:right;"> Creatinine_count </th>
   <th style="text-align:right;"> Creatinine_mean </th>
   <th style="text-align:right;"> Creatinine_sd </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 132539 </td>
   <td style="text-align:right;"> 54 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> -1.0 </td>
   <td style="text-align:right;"> -1.0 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> -1 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:right;"> 13 </td>
   <td style="text-align:right;"> 14.923077 </td>
   <td style="text-align:right;"> 0.2773501 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 10.500000 </td>
   <td style="text-align:right;"> 3.535534 </td>
   <td style="text-align:right;"> 38 </td>
   <td style="text-align:right;"> 171.05263 </td>
   <td style="text-align:right;"> 171.87031 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 27.00000 </td>
   <td style="text-align:right;"> 1.4142136 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 0.7500000 </td>
   <td style="text-align:right;"> 0.0707107 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 132540 </td>
   <td style="text-align:right;"> 76 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 175.3 </td>
   <td style="text-align:right;"> 76.0 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 16 </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> -1 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 13.333333 </td>
   <td style="text-align:right;"> 3.2659863 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 18.333333 </td>
   <td style="text-align:right;"> 2.516611 </td>
   <td style="text-align:right;"> 41 </td>
   <td style="text-align:right;"> 151.56098 </td>
   <td style="text-align:right;"> 161.50976 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 22.33333 </td>
   <td style="text-align:right;"> 1.5275252 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 1.1000000 </td>
   <td style="text-align:right;"> 0.2645751 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 132541 </td>
   <td style="text-align:right;"> 44 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> -1.0 </td>
   <td style="text-align:right;"> 56.7 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 21 </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:right;"> 19 </td>
   <td style="text-align:right;"> -1 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:right;"> 13 </td>
   <td style="text-align:right;"> 5.923077 </td>
   <td style="text-align:right;"> 1.1875422 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 4.666667 </td>
   <td style="text-align:right;"> 2.886751 </td>
   <td style="text-align:right;"> 41 </td>
   <td style="text-align:right;"> 124.95122 </td>
   <td style="text-align:right;"> 93.21667 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 25.00000 </td>
   <td style="text-align:right;"> 1.0000000 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 0.3333333 </td>
   <td style="text-align:right;"> 0.0577350 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 132543 </td>
   <td style="text-align:right;"> 68 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 180.3 </td>
   <td style="text-align:right;"> 84.6 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 575 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:right;"> 14.944444 </td>
   <td style="text-align:right;"> 0.2357023 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 17.666667 </td>
   <td style="text-align:right;"> 6.806859 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 545.83333 </td>
   <td style="text-align:right;"> 224.95370 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 27.66667 </td>
   <td style="text-align:right;"> 0.5773503 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 0.7666667 </td>
   <td style="text-align:right;"> 0.1154701 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 132545 </td>
   <td style="text-align:right;"> 88 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> -1.0 </td>
   <td style="text-align:right;"> -1.0 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 17 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 918 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 15.000000 </td>
   <td style="text-align:right;"> 0.0000000 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 35.000000 </td>
   <td style="text-align:right;"> 14.142136 </td>
   <td style="text-align:right;"> 38 </td>
   <td style="text-align:right;"> 62.13158 </td>
   <td style="text-align:right;"> 39.38320 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 19.00000 </td>
   <td style="text-align:right;"> 1.4142136 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 1.0000000 </td>
   <td style="text-align:right;"> 0.0000000 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 132547 </td>
   <td style="text-align:right;"> 64 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 180.3 </td>
   <td style="text-align:right;"> 114.0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 14 </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 1637 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 8.666667 </td>
   <td style="text-align:right;"> 0.9759001 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 16.750000 </td>
   <td style="text-align:right;"> 1.707825 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 136.33333 </td>
   <td style="text-align:right;"> 221.44094 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 19.75000 </td>
   <td style="text-align:right;"> 0.9574271 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 0.9750000 </td>
   <td style="text-align:right;"> 0.2986079 </td>
  </tr>
</tbody>
</table></div>

## Descripción de las cinco variables seleccionadas

- `GCS` (Glasgow): 

- `BUN` (Nitrógeno en sangre):

- `HCO3` (Serum bicarbonado):

- `Creatinine` (Creatinina):

- `Urine` (orina):



# Análisis descriptivo multivariado

## Histogramas entre las variables cualitativas



```r
n = nrow(data_real_tidy)

data_cualitativa = select(data_real_tidy,Gender, ICUType, `In-hospital_death`)
data_cuantitativa = select(data_real_tidy, -RecordID, -Gender, -ICUType, -`In-hospital_death`)
data_cualitativa$Gender = as_factor(data_cualitativa$Gender)


  ggplot(data_cualitativa)+
  geom_bar(mapping = aes(x=ICUType,fill= Gender), position = "dodge")+
  labs(title = "UCI/Genero\n", x = "Tipo UCI", y = "Counts", fill = "Genero") +
  scale_fill_manual(labels = c("Mujer", "Homber"), values = c("hotpink2", "dodgerblue3")) 
```

![](proyecto_final_solucion1_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

```r
  ggplot(data_cualitativa)+
  geom_bar(mapping = aes(x=ICUType,fill= `In-hospital_death`), position = "dodge")+
  labs(title = "UCI/Muerte\n", x = "Tipo UCI", y = "Counts", fill = "Muertos en el hospital") +
  scale_fill_manual(labels = c("Vida", "Muerte"), values = c("khaki3", "gray48"))
```

![](proyecto_final_solucion1_files/figure-html/unnamed-chunk-13-2.png)<!-- -->

```r
  ggplot(data_cualitativa)+
  geom_bar(mapping = aes(x=Gender,fill= `In-hospital_death`), position = "dodge")+
  labs(title = "Muerte/Genero\n", x = "Genero", y = "Counts", fill = "Muertos en el hospital") +
   scale_fill_manual(labels = c("Vida", "Muerte"), values = c("khaki3", "gray48"))
```

![](proyecto_final_solucion1_files/figure-html/unnamed-chunk-13-3.png)<!-- -->

- **Gráfico1**: Vemos que la distribución de los pacientes respecto al tipo de UCI es que la Uci tipo 3 tiene más pacientes que las demás y que en todos los tipos hay más hombres que mujeres

- **Gráfico2**: En el gráfico 1 los pacientes de la UCI 2 so mas que los pacientes de la UCI 1, pero en cambio vemos que hay menos muerte en UCI 2 que en UCI 1. Esto puede significar menor probabilidad de muerte en la UCI 2.

- **Gráfico3**: Hay mas población de hombres que de mujeres pero las muertes se reparten de forma equitativa.


## ¿Qué variables influyen más mortalidad?


```r
contar_sofa = function(data){
  # El índice de SOFA sabemos que va de 0 a 24, creamos un array vacio para guardar el numero de pacientes que se corresponde a cada indice
  a = c(rep(0,25))
  b = c(rep(0,25))
for (i in 1:25){
  # El array `a` nos guarda en la posición i el número de personas que tienen el coeficiente SOFA en i-1
  a[i] = nrow(filter(data, SOFA == i-1))
  # El `b` guarda el número de personas que tienen el coeficiente i-1 y al mismo tiempo mueren en el hospital
  b[i] = nrow(filter(data,(SOFA == i-1)&(`In-hospital_death` == 1)))
}
  # Devolvemos el porcentaje de pacientes que mueren respecto al indice que tienen en SOFA
  return((b/a)*100)
}


muertes_cond_SOFA = tibble(SOFA = as.factor(c(0:24)), Proporcion = as.factor(contar_sofa(data_real_tidy)))
muertes_cond_SOFA
```

```
## # A tibble: 25 x 2
##    SOFA  Proporcion      
##    <fct> <fct>           
##  1 0     6.74157303370786
##  2 1     4.47284345047923
##  3 2     5.14705882352941
##  4 3     8.96860986547085
##  5 4     14.760147601476 
##  6 5     15.8914728682171
##  7 6     13.8613861386139
##  8 7     16.6666666666667
##  9 8     14.0161725067385
## 10 9     10.4895104895105
## # ... with 15 more rows
```

```r
  ggplot(muertes_cond_SOFA) +
  geom_bar(mapping = aes(x = SOFA, y = Proporcion), stat = "identity")
```

![](proyecto_final_solucion1_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

```r
  ggplot(data_real_tidy)+
  geom_bar(mapping = aes(x = SOFA, fill = `In-hospital_death`), position = "dodge")+
  labs(title = "SOFA/Muertes\n", x = "SOFA", y = "Counts", fill = "Muertos en el hospital") +
   scale_fill_manual(labels = c("Vida", "Muerte"), values = c("khaki3", "gray48"))
```

![](proyecto_final_solucion1_files/figure-html/unnamed-chunk-14-2.png)<!-- -->

```r
  ggplot(data_real_tidy)+
  geom_bar(mapping = aes(x = `SAPS-I`, fill = `In-hospital_death`), position = "dodge")+
  labs(title = "SAPS-I/Muertes\n", x = "SAPS-I", y = "Counts", fill = "Muertos en el hospital") +
   scale_fill_manual(labels = c("Vida", "Muerte"), values = c("khaki3", "gray48"))
```

![](proyecto_final_solucion1_files/figure-html/unnamed-chunk-14-3.png)<!-- -->

```r
  ggplot(data_real_tidy)+
  geom_bar(mapping = aes(x=`SAPS-I`,fill= `In-hospital_death`), position = "dodge")+
  labs(title = "SAPS-I/Muertes\n", x = "SAPS-I", y = "Counts", fill = "Muertos en el hospital") +
   scale_fill_manual(labels = c("Vida", "Muerte"), values = c("khaki3", "gray48"))
```

![](proyecto_final_solucion1_files/figure-html/unnamed-chunk-14-4.png)<!-- -->

```r
    ggplot(data_real_tidy)+
  geom_point(mapping = aes(x=Urine_mean,y=Urine_count,color= `In-hospital_death`))+
  labs(title = "Urine/Muertes\n", x = "Urine_mean", y = "Urine_count", color = "Muertos en el hospital") +
   scale_color_manual(labels = c("Vida", "Muerte"), values = c("khaki3", "gray48"))
```

![](proyecto_final_solucion1_files/figure-html/unnamed-chunk-14-5.png)<!-- -->

```r
    ggplot(data_real_tidy)+
  geom_point(mapping = aes(x=BUN_mean,y=BUN_count,color= `In-hospital_death`),position = "jitter")+
  labs(title = "BUN/Muertes\n", x = "Genero", y = "Counts", color = "Muertos en el hospital") +
   scale_color_manual(labels = c("Vida", "Muerte"), values = c("khaki3", "gray48"))
```

![](proyecto_final_solucion1_files/figure-html/unnamed-chunk-14-6.png)<!-- -->

```r
    ggplot(data_real_tidy)+
  geom_point(mapping = aes(x=GCS_mean,y=GCS_count,color= `In-hospital_death`),position = "jitter")+
  labs(title = "Muerte/Genero\n", x = "Genero", y = "Counts", color = "Muertos en el hospital") +
   scale_color_manual(labels = c("Vida", "Muerte"), values = c("khaki3", "gray48"))
```

![](proyecto_final_solucion1_files/figure-html/unnamed-chunk-14-7.png)<!-- -->

```r
    ggplot(data_real_tidy)+
  geom_point(mapping = aes(x=Creatinine_mean,y=Creatinine_count,color= `In-hospital_death`),position = "jitter")+
  labs(title = "Muerte/Genero\n", x = "Genero", y = "Counts", color = "Muertos en el hospital") +
   scale_color_manual(labels = c("Vida", "Muerte"), values = c("khaki3", "gray48"))
```

![](proyecto_final_solucion1_files/figure-html/unnamed-chunk-14-8.png)<!-- -->

```r
  ggplot(data_real_tidy)+
  geom_point(mapping = aes(x=HCO3_mean,y=HCO3_count,color= `In-hospital_death`),position = "jitter")+
  labs(title = "Muerte/Genero\n", x = "Genero", y = "Counts", color = "Muertos en el hospital") +
   scale_color_manual(labels = c("Vida", "Muerte"), values = c("khaki3", "gray48"))
```

![](proyecto_final_solucion1_files/figure-html/unnamed-chunk-14-9.png)<!-- -->

Las variables SOFA y SAPS-I son variables discretas y podemos usar geom_bar() para representarlas mediante histogramas. Pero las variables que hemos escogido son continuas y geom_bar() necesita que sean variables discretas, hemos decidido usar geom_point() para dibujar las muertes respecto de cada variable en el plano. 

## ¿El tipo de UCI marca muchas diferencias respecto el nivel de mortalidad?

Sin hacer :D

## Correlación entre variables

Empezamos realizando un análisis estadístico multivariante sin separar las variables por tipo de UCI. Las conclusiones que podamos sacar estarán más generalizadas respecto al estudio individual.

A la hora de calcular las correlaciones entre las variables por ejemplo, al variar entre los diferentes tipos de UCI's, los coeficientes de correlación variarán pero no cambiará respecto a qué variables están correlacionadas entre ellas.


```r
data_cualitativa = select(data_real_tidy, Gender, ICUType, `In-hospital_death`)
data_cuantitativa = select(data_real_tidy, -RecordID, -Gender, -ICUType, -`In-hospital_death`)


# Para mayor facilidad en el calculo de correlacion
mean_real_tidy = select(data_cuantitativa, -ends_with("count"), -ends_with("sd"))
#sd_real_tidy = select(data_cuantitativa, -ends_with("count"), -ends_with("mean"))


chart.Correlation(mean_real_tidy, histogram = TRUE, pch=19)
```

![](proyecto_final_solucion1_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

```r
#chart.Correlation(sd_real_tidy, histogram = TRUE, pch=19)

sapply(mean_real_tidy, function(x) sum(is.na(x)))
```

```
##             Age          Height          Weight          SAPS-I 
##               0               0               0               0 
##            SOFA  Length_of_stay        Survival        GCS_mean 
##               0               0               0               0 
##        BUN_mean      Urine_mean       HCO3_mean Creatinine_mean 
##               0               0               0               0
```

```r
cov(mean_real_tidy)
```

```
##                           Age      Height      Weight       SAPS-I
## Age              302.71325980   77.087487  -69.548109   24.5725082
## Height            77.08748697 7468.546482  626.555372   77.4717408
## Weight           -69.54810905  626.555372  939.937278   10.5862236
## SAPS-I            24.57250817   77.471741   10.586224   27.7631955
## SOFA               5.29101583   86.495128   24.751795   14.0151290
## Length_of_stay    -8.54945820   43.300768   29.281406    9.6823266
## Survival         796.34171479  536.800848 -508.578581   14.4105051
## GCS_mean           2.09479136  -24.953515  -14.533581   -9.4809216
## BUN_mean          81.34017446  -61.740551   55.321214   12.9333263
## Urine_mean      -525.40306820 -582.072723 -124.452393 -134.7410286
## HCO3_mean          0.07232343   -7.947188    2.531826   -4.0764027
## Creatinine_mean    0.69054871    1.798982    4.051446    0.9035275
##                        SOFA Length_of_stay     Survival    GCS_mean
## Age                5.291016      -8.549458    796.34171   2.0947914
## Height            86.495128      43.300768    536.80085 -24.9535150
## Weight            24.751795      29.281406   -508.57858 -14.5335810
## SAPS-I            14.015129       9.682327     14.41051  -9.4809216
## SOFA              16.838813       8.103618    -25.51845  -7.7030458
## Length_of_stay     8.103618     150.516640    250.61368  -9.5653997
## Survival         -25.518449     250.613676 132212.12026  51.7452061
## GCS_mean          -7.703046      -9.565400     51.74521  10.5444235
## BUN_mean          20.794966      23.484404    260.29502  -2.4928809
## Urine_mean      -126.971164    -154.549497   -917.65027  62.2277304
## HCO3_mean         -4.423335      -3.707300     39.86646   2.1710162
## Creatinine_mean    1.436546       1.141658     11.09028  -0.1782254
##                    BUN_mean  Urine_mean    HCO3_mean Creatinine_mean
## Age               81.340174  -525.40307   0.07232343       0.6905487
## Height           -61.740551  -582.07272  -7.94718833       1.7989825
## Weight            55.321214  -124.45239   2.53182646       4.0514460
## SAPS-I            12.933326  -134.74103  -4.07640268       0.9035275
## SOFA              20.794966  -126.97116  -4.42333486       1.4365461
## Length_of_stay    23.484404  -154.54950  -3.70730045       1.1416580
## Survival         260.295021  -917.65027  39.86646132      11.0902832
## GCS_mean          -2.492881    62.22773   2.17101616      -0.1782254
## BUN_mean         423.378690  -484.01866 -21.05106944      18.6221961
## Urine_mean      -484.018662 14078.15889  52.55320313     -26.0110171
## HCO3_mean        -21.051069    52.55320  16.54674398      -1.4896023
## Creatinine_mean   18.622196   -26.01102  -1.48960230       1.6358629
```

Variables más correlacionadas positivamente (las variables correlacionadas crecerán o decrecerán directamente proporcional):

- `BUN` (Nitrógeno en sangre) y `Creatinine`, con una covarianza de $18.62$

- `SAPS-I` ("Puntuación simplificada de fisiología aguda") y `SOFA` ("Evaluación secuencial de insuficiencia orgánica"), con una covarianza de $14.01$


Variables más correlacionadas negativamente (las variables correlacionadas crecerán o decrecerán inversamente proporcional):

- `SAPS-I` y `GCS` (Glasgow), con una covarianza de $-9.48$

- `SOFA` y `GCS`, con una covarianza de $-7.73$


Si haciendo estas gráficas diferenciando por el tipo de UCI no notamos diferencias significativas con la conclusión que hayamos podido sacar juntando todas las UCIS no añadiremos notaciones.

### UCI 1 - Cuidados coronarios


```r
# Seleccionamos los datos cuantitativos de la UCI 1
data_cuant1 = data_real_tidy %>% 
              filter(ICUType == 1) %>%
              select(-RecordID, -Gender, -ICUType, -`In-hospital_death`)

# Solo guardamos las columnas que nos dan las medias de las 5 variables
mean_data1 = select(data_cuant1, -ends_with("count"), -ends_with("sd"))


# Graficamos por pares las correlaciones de las variables
chart.Correlation(mean_data1, histogram = TRUE, pch=19)
```

![](proyecto_final_solucion1_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

```r
# Calculamos la matriz de varianzas-covarianzas
cov(mean_data1)
```

```
##                         Age      Height       Weight      SAPS-I
## Age              215.748979  -73.993816  -95.2168457   21.925026
## Height           -73.993816 7009.354555  627.1300694   32.609926
## Weight           -95.216846  627.130069  767.0937934    1.711360
## SAPS-I            21.925026   32.609926    1.7113601   28.769706
## SOFA               6.519873   41.217459   15.4049703   16.194111
## Length_of_stay    -1.339266   65.025433   14.5813288   10.587525
## Survival         567.696400 1325.334529 -518.9415140    9.651770
## GCS_mean           1.675787  -32.442845  -12.3980653  -11.578176
## BUN_mean          88.622024   24.928953   38.0336373   23.131541
## Urine_mean      -518.189126 -535.612401  -41.5880817 -241.583655
## HCO3_mean          3.854745  -28.373051   -0.8534206   -6.950612
## Creatinine_mean    1.042728    2.187376    5.1883523    1.475589
##                        SOFA Length_of_stay      Survival    GCS_mean
## Age                6.519873      -1.339266    567.696400   1.6757872
## Height            41.217459      65.025433   1325.334529 -32.4428450
## Weight            15.404970      14.581329   -518.941514 -12.3980653
## SAPS-I            16.194111      10.587525      9.651770 -11.5781759
## SOFA              17.331413       6.647377     -8.585376  -9.4376444
## Length_of_stay     6.647377      99.277929    154.369374  -5.7149708
## Survival          -8.585376     154.369374 142313.137018  79.7270564
## GCS_mean          -9.437644      -5.714971     79.727056  10.9366916
## BUN_mean          22.514428      38.072047    228.805634   0.1380971
## Urine_mean      -169.361828    -118.991798     10.323134 104.5325051
## HCO3_mean         -5.953283      -4.527243    -22.286229   4.2951041
## Creatinine_mean    1.469341       2.096670     27.759634  -0.2159081
##                     BUN_mean  Urine_mean   HCO3_mean Creatinine_mean
## Age               88.6220236  -518.18913   3.8547448       1.0427280
## Height            24.9289531  -535.61240 -28.3730506       2.1873760
## Weight            38.0336373   -41.58808  -0.8534206       5.1883523
## SAPS-I            23.1315410  -241.58365  -6.9506124       1.4755888
## SOFA              22.5144285  -169.36183  -5.9532829       1.4693406
## Length_of_stay    38.0720475  -118.99180  -4.5272427       2.0966700
## Survival         228.8056341    10.32313 -22.2862288      27.7596336
## GCS_mean           0.1380971   104.53251   4.2951041      -0.2159081
## BUN_mean         423.3695610  -582.14017 -12.3560229      16.4734637
## Urine_mean      -582.1401672 10546.07759  67.9105695     -28.5237029
## HCO3_mean        -12.3560229    67.91057  16.2816408      -1.0568553
## Creatinine_mean   16.4734637   -28.52370  -1.0568553       1.6356827
```

### UCI 2 - Recuperación cirugía cardíaca


```r
# Seleccionamos los datos cuantitativos de la UCI 1
data_cuant2 = data_real_tidy %>% 
              filter(ICUType == 2) %>%
              select(-RecordID, -Gender, -ICUType, -`In-hospital_death`)

# Solo guardamos las columnas que nos dan las medias de las 5 variables
mean_data2 = select(data_cuant2, -ends_with("count"), -ends_with("sd"))


# Graficamos por pares las correlaciones de las variables
chart.Correlation(mean_data2, histogram = TRUE, pch=19)
```

![](proyecto_final_solucion1_files/figure-html/unnamed-chunk-17-1.png)<!-- -->

```r
# Calculamos la matriz de varianzas-covarianzas
cov(mean_data2)
```

```
##                         Age       Height      Weight      SAPS-I
## Age              164.152590    -9.132644  -79.980639  20.4450781
## Height            -9.132644  1734.388134  236.269554  25.3553296
## Weight           -79.980639   236.269554  450.090409 -12.9519989
## SAPS-I            20.445078    25.355330  -12.951999  21.4881258
## SOFA               6.698900    32.055845    4.896950   9.0484016
## Length_of_stay     5.433109   -22.554384   -8.406753   5.1330483
## Survival         848.966376 -1329.883887 -680.981441 -32.8522718
## GCS_mean          -3.706823    -8.679242   -2.944719  -6.0957770
## BUN_mean          20.085387   -17.547063   21.612472   9.3681516
## Urine_mean      -133.949095    61.711519   11.429302 -38.0676658
## HCO3_mean         -5.250172    -7.423032    4.369940  -2.2146865
## Creatinine_mean   -1.180676    -1.334965    1.651347   0.5962937
##                        SOFA Length_of_stay      Survival    GCS_mean
## Age               6.6988999       5.433109    848.966376  -3.7068225
## Height           32.0558453     -22.554384  -1329.883887  -8.6792424
## Weight            4.8969496      -8.406753   -680.981441  -2.9447188
## SAPS-I            9.0484016       5.133048    -32.852272  -6.0957770
## SOFA             10.7906656       5.548122    -20.234393  -4.8716402
## Length_of_stay    5.5481223     134.922407    399.525450 -10.7818760
## Survival        -20.2343926     399.525450 181962.699063   4.4613586
## GCS_mean         -4.8716402     -10.781876      4.461359   8.1252938
## BUN_mean          8.4618281      31.664030    449.556232  -4.7887103
## Urine_mean      -43.9520280    -110.438015  -2972.046171  15.1046510
## HCO3_mean        -2.3830268      -5.293724   -105.267244   1.6928042
## Creatinine_mean   0.8844077       1.679038     19.638614  -0.3913367
##                    BUN_mean  Urine_mean    HCO3_mean Creatinine_mean
## Age               20.085387  -133.94910   -5.2501718      -1.1806764
## Height           -17.547063    61.71152   -7.4230323      -1.3349650
## Weight            21.612472    11.42930    4.3699404       1.6513467
## SAPS-I             9.368152   -38.06767   -2.2146865       0.5962937
## SOFA               8.461828   -43.95203   -2.3830268       0.8844077
## Length_of_stay    31.664030  -110.43801   -5.2937237       1.6790380
## Survival         449.556232 -2972.04617 -105.2672441      19.6386144
## GCS_mean          -4.788710    15.10465    1.6928042      -0.3913367
## BUN_mean         117.993277  -259.16657   -8.0903612       7.1349608
## Urine_mean      -259.166573  3119.44810   36.0374867     -18.0497577
## HCO3_mean         -8.090361    36.03749    7.2855473      -0.6405231
## Creatinine_mean    7.134961   -18.04976   -0.6405231       0.9241415
```

### UCI 3 - Medical UCI


```r
# Seleccionamos los datos cuantitativos de la UCI 1
data_cuant3 = data_real_tidy %>% 
              filter(ICUType == 3) %>%
              select(-RecordID, -Gender, -ICUType, -`In-hospital_death`)

# Solo guardamos las columnas que nos dan las medias de las 5 variables
mean_data3 = select(data_cuant3, -ends_with("count"), -ends_with("sd"))


# Graficamos por pares las correlaciones de las variables
chart.Correlation(mean_data3, histogram = TRUE, pch=19)
```

![](proyecto_final_solucion1_files/figure-html/unnamed-chunk-18-1.png)<!-- -->

```r
# Calculamos la matriz de varianzas-covarianzas
cov(mean_data3)
```

```
##                          Age      Height      Weight      SAPS-I
## Age              338.1787443  -65.878929  -92.982665   20.788803
## Height           -65.8789287 6119.104708  493.841813   11.867179
## Weight           -92.9826646  493.841813 1259.258724    9.937215
## SAPS-I            20.7888031   11.867179    9.937215   27.257823
## SOFA              -0.9313817   36.652703   26.864613   12.807054
## Length_of_stay   -15.8328762   54.107616   28.382624   10.791753
## Survival         538.9223728 -443.188215 -826.434135   -3.321337
## GCS_mean           4.5692273  -26.523003  -17.260022   -9.863847
## BUN_mean         111.9927913  117.206120  102.722667   20.114196
## Urine_mean      -612.5080542 -488.338820 -189.059084 -170.418370
## HCO3_mean          4.6745696  -14.053018    8.354389   -4.816185
## Creatinine_mean    0.5094761    9.746007    6.227786    1.300445
##                         SOFA Length_of_stay      Survival    GCS_mean
## Age               -0.9313817    -15.8328762    538.922373   4.5692273
## Height            36.6527030     54.1076158   -443.188215 -26.5230031
## Weight            26.8646127     28.3826241   -826.434135 -17.2600224
## SAPS-I            12.8070539     10.7917529     -3.321337  -9.8638469
## SOFA              17.6476195      8.5779774    -78.715022  -8.9620151
## Length_of_stay     8.5779774    149.7004025    414.695087  -8.4888734
## Survival         -78.7150223    414.6950868 120937.908047  24.2550373
## GCS_mean          -8.9620151     -8.4888734     24.255037  11.2593050
## BUN_mean          38.6178647     20.0951285     47.501267  -6.1560938
## Urine_mean      -178.6361243   -181.7240500    133.304214 101.5729187
## HCO3_mean         -6.1387481     -0.5018886    181.382533   1.6984615
## Creatinine_mean    2.3164708      1.2201590     -4.074937  -0.2097231
##                    BUN_mean  Urine_mean   HCO3_mean Creatinine_mean
## Age              111.992791  -612.50805   4.6745696       0.5094761
## Height           117.206120  -488.33882 -14.0530178       9.7460069
## Weight           102.722667  -189.05908   8.3543890       6.2277860
## SAPS-I            20.114196  -170.41837  -4.8161853       1.3004450
## SOFA              38.617865  -178.63612  -6.1387481       2.3164708
## Length_of_stay    20.095129  -181.72405  -0.5018886       1.2201590
## Survival          47.501267   133.30421 181.3825332      -4.0749371
## GCS_mean          -6.156094   101.57292   1.6984615      -0.2097231
## BUN_mean         680.178650  -789.58540 -34.2471331      30.8487869
## Urine_mean      -789.585396 20743.45997  89.3540761     -41.3665191
## HCO3_mean        -34.247133    89.35408  24.9007485      -2.5152207
## Creatinine_mean   30.848787   -41.36652  -2.5152207       2.6347895
```

### UCI 4 - Cirugía


```r
# Seleccionamos los datos cuantitativos de la UCI 1
data_cuant4 = data_real_tidy %>% 
              filter(ICUType == 4) %>%
              select(-RecordID, -Gender, -ICUType, -`In-hospital_death`)

# Solo guardamos las columnas que nos dan las medias de las 5 variables
mean_data4 = select(data_cuant2, -ends_with("count"), -ends_with("sd"))

# Graficamos por pares las correlaciones de las variables
chart.Correlation(mean_data4, histogram = TRUE, pch=19)
```

![](proyecto_final_solucion1_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

```r
# Calculamos la matriz de varianzas-covarianzas
cov(mean_data4)
```

```
##                         Age       Height      Weight      SAPS-I
## Age              164.152590    -9.132644  -79.980639  20.4450781
## Height            -9.132644  1734.388134  236.269554  25.3553296
## Weight           -79.980639   236.269554  450.090409 -12.9519989
## SAPS-I            20.445078    25.355330  -12.951999  21.4881258
## SOFA               6.698900    32.055845    4.896950   9.0484016
## Length_of_stay     5.433109   -22.554384   -8.406753   5.1330483
## Survival         848.966376 -1329.883887 -680.981441 -32.8522718
## GCS_mean          -3.706823    -8.679242   -2.944719  -6.0957770
## BUN_mean          20.085387   -17.547063   21.612472   9.3681516
## Urine_mean      -133.949095    61.711519   11.429302 -38.0676658
## HCO3_mean         -5.250172    -7.423032    4.369940  -2.2146865
## Creatinine_mean   -1.180676    -1.334965    1.651347   0.5962937
##                        SOFA Length_of_stay      Survival    GCS_mean
## Age               6.6988999       5.433109    848.966376  -3.7068225
## Height           32.0558453     -22.554384  -1329.883887  -8.6792424
## Weight            4.8969496      -8.406753   -680.981441  -2.9447188
## SAPS-I            9.0484016       5.133048    -32.852272  -6.0957770
## SOFA             10.7906656       5.548122    -20.234393  -4.8716402
## Length_of_stay    5.5481223     134.922407    399.525450 -10.7818760
## Survival        -20.2343926     399.525450 181962.699063   4.4613586
## GCS_mean         -4.8716402     -10.781876      4.461359   8.1252938
## BUN_mean          8.4618281      31.664030    449.556232  -4.7887103
## Urine_mean      -43.9520280    -110.438015  -2972.046171  15.1046510
## HCO3_mean        -2.3830268      -5.293724   -105.267244   1.6928042
## Creatinine_mean   0.8844077       1.679038     19.638614  -0.3913367
##                    BUN_mean  Urine_mean    HCO3_mean Creatinine_mean
## Age               20.085387  -133.94910   -5.2501718      -1.1806764
## Height           -17.547063    61.71152   -7.4230323      -1.3349650
## Weight            21.612472    11.42930    4.3699404       1.6513467
## SAPS-I             9.368152   -38.06767   -2.2146865       0.5962937
## SOFA               8.461828   -43.95203   -2.3830268       0.8844077
## Length_of_stay    31.664030  -110.43801   -5.2937237       1.6790380
## Survival         449.556232 -2972.04617 -105.2672441      19.6386144
## GCS_mean          -4.788710    15.10465    1.6928042      -0.3913367
## BUN_mean         117.993277  -259.16657   -8.0903612       7.1349608
## Urine_mean      -259.166573  3119.44810   36.0374867     -18.0497577
## HCO3_mean         -8.090361    36.03749    7.2855473      -0.6405231
## Creatinine_mean    7.134961   -18.04976   -0.6405231       0.9241415
```

Lo único que ha podido cambiar han sido los valores de los coeficientes de correlación y covarianzas. Pero efectivamente se han mantenido las mismas variables.

Con esta información concluimos que al hacer ACP podremos reducir dimensionalidad.


# Análisis analítico

## Análisis de componetes principales

Como ya hemos visto en las correlaciones, podremos reducir la dimensionalidad para poder realizar un ACP y así ver cuales son las componentes que más influyen en la mortalidad del paciente.

### UCI 1

```r
# Mediante la funcion prcomp realizamos un análisis de componentes principales y devuelve los pesos de las variables al hacer las diferentes reducciones de dimensionalidad
mean_data1.acp = prcomp(mean_data1[,4:12], scale = TRUE)
mean_data1.acp
```

```
## Standard deviations (1, .., p=9):
## [1] 1.7740835 1.2310177 0.9997282 0.9532648 0.9211491 0.8105793 0.6080716
## [8] 0.5522860 0.4987257
## 
## Rotation (n x k) = (9 x 9):
##                          PC1         PC2          PC3         PC4
## SAPS-I          -0.470379518  0.20628808 -0.035356999  0.04769118
## SOFA            -0.481584730  0.17071916  0.001441113  0.09764265
## Length_of_stay  -0.189773567 -0.14654774 -0.264491569 -0.91721876
## Survival        -0.004100646 -0.16089848 -0.933472299  0.29746034
## GCS_mean         0.410192827 -0.40937947  0.027987521  0.05050427
## BUN_mean        -0.263799944 -0.60808137  0.162401506  0.05686551
## Urine_mean       0.337595191  0.03799737 -0.102421066 -0.20398706
## HCO3_mean        0.291301124 -0.04562112  0.088399972 -0.05396394
## Creatinine_mean -0.272237053 -0.58314324  0.109358598  0.09006305
##                          PC5         PC6         PC7         PC8
## SAPS-I          -0.126031789  0.16605905  0.16309467 -0.72488059
## SOFA            -0.020225284  0.28800073  0.11151747  0.05637347
## Length_of_stay  -0.097645799 -0.12237854 -0.02237179 -0.02103620
## Survival        -0.092938568  0.03342138  0.01423875  0.06105137
## GCS_mean        -0.002125832 -0.23988082  0.18462980 -0.61617309
## BUN_mean        -0.028848488  0.10503143  0.64512111  0.26250255
## Urine_mean       0.529249760  0.72163632  0.14835011 -0.08909831
## HCO3_mean       -0.817177367  0.47934169 -0.06440837  0.02467476
## Creatinine_mean  0.129667788  0.23299062 -0.69558259 -0.10038005
##                         PC9
## SAPS-I           0.37034093
## SOFA            -0.79399456
## Length_of_stay  -0.07621993
## Survival         0.02360289
## GCS_mean        -0.43532923
## BUN_mean         0.18462403
## Urine_mean       0.04091862
## HCO3_mean       -0.00557890
## Creatinine_mean  0.02739766
```

Si empezamos imprimiendo la gráfica de los valores propios/varianzas entre el número de dimensión del ACP.


```r
fviz_eig(mean_data1.acp)
```

![](proyecto_final_solucion1_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

Mediante la regla del codo pensaríamos que el mayor salto está en la dimensión dos. Veamos qué información podemos obtener a partir de los valores propios.



```r
get_eigenvalue(mean_data1.acp)
```

```
##       eigenvalue variance.percent cumulative.variance.percent
## Dim.1  3.1473724        34.970804                    34.97080
## Dim.2  1.5154047        16.837830                    51.80863
## Dim.3  0.9994565        11.105072                    62.91371
## Dim.4  0.9087138        10.096820                    73.01053
## Dim.5  0.8485156         9.427952                    82.43848
## Dim.6  0.6570387         7.300430                    89.73891
## Dim.7  0.3697511         4.108346                    93.84725
## Dim.8  0.3050199         3.389110                    97.23636
## Dim.9  0.2487273         2.763637                   100.00000
```

Con sólo 2 componentes principales estaríamos explicando casi el 52% de la variación total (demasiado bajo). Hay diferentes criterios para determinar la dimensión más óptima para el análisis.

- Si miramos a partir de qué dimensión el valor propio és menor a 1, escogeríamos 3 componentes principales, pero sólo estaríamos contemplando el 62.92% de variación.

- Si decidimos directamente marcarnos que queremos explicar más del 85%, necesitaríamos hacer el ACP de dimensión 6.

Recordemos lo que hemos obtenido en el apartado anterior. Cuando hemos realizado el análisis estadístico multivariado, nos han salido 4 variables muy correlacionadas (dos a dos) y una correlacionada negativamente con una pareja de las anteriores. Así pues sería lógico que de 9 variables iniciales que estamos contemplando, quitaramos al menos 3 de ellas "juntando" aquellas que están correlacionadas.

Comprobemos que si realmente decidimos coger dimensión 6:

- Los pesos de las variables `SOFA` y `SAPS-I` deberían ir "a la par" (mismo signo y valores cercanos) y con cambio de signo respecto `GCS`.

- Y de igual manera con `Creatinine` y `BUN`.


```r
mean_data1.acp$rotation[,6]
```

```
##          SAPS-I            SOFA  Length_of_stay        Survival 
##      0.16605905      0.28800073     -0.12237854      0.03342138 
##        GCS_mean        BUN_mean      Urine_mean       HCO3_mean 
##     -0.23988082      0.10503143      0.72163632      0.47934169 
## Creatinine_mean 
##      0.23299062
```


```r
fviz_pca_var(mean_data1.acp,
col.var = "contrib",
gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE
)
```

![](proyecto_final_solucion1_files/figure-html/unnamed-chunk-24-1.png)<!-- -->

Al ver que todo nos cuadra, decidimos hacer el ACP de dimensión 6.


```r
fviz_pca_ind(mean_data1.acp,
col.ind = "cos2", #Color de acuerdo a la calidad de la representación.
gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
repel = TRUE # Para evitar que el texto se solape
)
```

![](proyecto_final_solucion1_files/figure-html/unnamed-chunk-25-1.png)<!-- -->

```r
fviz_pca_biplot(mean_data1.acp, repel = TRUE,
col.var = "#2E9FDF", # color para las variables
col.ind = "#696969" # color para las observaciones
)
```

![](proyecto_final_solucion1_files/figure-html/unnamed-chunk-25-2.png)<!-- -->

```r
data = filter(data_real_tidy, ICUType == 1)
data
```

```
## # A tibble: 548 x 26
##    RecordID   Age Gender Height Weight ICUType `SAPS-I`  SOFA
##       <dbl> <dbl>  <dbl>  <dbl>  <dbl>   <dbl>    <dbl> <dbl>
##  1   132547    64      1   180.  114         1       14    11
##  2   132570    84      1   170.  103.        1       14     7
##  3   132573    77      1   163.   90.1       1       12     3
##  4   132614    77      1   163.   59         1        8     3
##  5   132617    77      1   170.   75         1        8     5
##  6   132671    55      1   185.   96         1        4     0
##  7   132682    68      1    -1    78.9       1       20     8
##  8   132685    77      1   173.   81.8       1        6     1
##  9   132694    61      0   170.  165         1        9     4
## 10   132717    86      1   168.   73         1       12     4
## # ... with 538 more rows, and 18 more variables: Length_of_stay <dbl>,
## #   Survival <dbl>, `In-hospital_death` <fct>, GCS_count <dbl>,
## #   GCS_mean <dbl>, GCS_sd <dbl>, BUN_count <dbl>, BUN_mean <dbl>,
## #   BUN_sd <dbl>, Urine_count <dbl>, Urine_mean <dbl>, Urine_sd <dbl>,
## #   HCO3_count <dbl>, HCO3_mean <dbl>, HCO3_sd <dbl>,
## #   Creatinine_count <dbl>, Creatinine_mean <dbl>, Creatinine_sd <dbl>
```

```r
#autoplot(mean_data1.acp, data = filter(data_real_tidy, ICUType == 1), colour = data$`In-hospital_death`,
#         loadings = TRUE, loadings.colour = 'blue', loadings.label = TRUE, loadings.label.size = 3)
```

