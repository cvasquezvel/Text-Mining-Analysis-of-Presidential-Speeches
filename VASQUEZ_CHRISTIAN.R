#-----------------------------------#
#    TERCERA PRÁCTICA CALIFICADA    # 
#         TEXT MINING               #
#     Mg. Jesús Salinas Flores      # 
#-----------------------------------#

# Para limpiar el workspace, por si hubiera algun dataset 
# o información cargada
rm(list = ls())
graphics.off()

# Cambiar el directorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Otras opciones
options(scipen = 999)      # Eliminar la notación científica
options(digits = 3)      # Número de decimales


#-------------------#
# Minería de Textos #
#-------------------#

# browseURL("https://www.congreso.gob.pe/participacion/museo/congreso/mensajes-presidenciales/")

# Para cada discurso elegido, realice el análisis de textos que
# comprenda:
# - Limpieza de stopwords y lematización
# - Gráfico de frecuencias
# - Nube de palabras
# - Red de Términos
# - Gráfico del Análisis de Sentimientos
# Use el diccionario de stopwords y de sentimientos 
# proporcionados en clases

# Presente la comparación de ambos discursos que comprenda:
# - Análisis TF-IDF 
# - Gráfico que muestre las palabras más características de cada
#   discurso
# - Comparación usando la similaridad de Coseno
# - Comparación visual de las palabras
# - Gráfica de una Commonality Cloud
# - Gráfica de una Comparison Cloud

# Comente los resultados obtenidos en cada discurso y en el
# análisis comparativo

# Enviar a jsalinas@lamolina.edu.pe un solo archivo comprimido 
# que contenga:
# - Archivo script de R APELLIDO_NOMBRE.R con el código y 
#   el análisis.
# - Archivo que contenga el discurso analizado
# - Archivo(s) de stopwords usado 
# - Archivo de lematización (*.udpipe)
# - Archivo de sentimientos usado

# Plazo: jueves 02 de diciembre del 2021, 10:00 pm

# Examen Final: jueves 09 de diciembre 7:00 pm
#               Duración: 35 minutos
#               Aula virtual

# Desarrollo del exámen

library(pacman)
p_load(tidyverse, tidytext, wordcloud2, pdftools, patchwork,
       widyr, dplyr, magrittr, readxl, udpipe, coop, lsa,
       stopwords,
       tm, ggplot2, wordcloud, ggwordcloud, 
       tidyverse, tidytext, topicmodels, BiocManager, 
       graph, quanteda, quanteda.textplots)

# I. Primer discurso ----

# Ollanta Humala 2012

library(pdftools)
discurso1 <- pdf_text("mensaje-2012-oh.pdf")

discurso1

# Queda una cadena de caracteres por página (7 en total).
# Se une todo en una sola cadena de carácteres:

discurso1 <- paste(discurso1, collapse = " ")

# 1.1 Limpieza de stopwords y lematización ----

# Es necesario eliminar los encabezados y pie de página

# Buscando el encabezado de página
library(stringr)
str_count(discurso1, "MENSAJE DEL PRESIDENTE CONSTITUCIONAL DEL PERÚ")
str_count(discurso1, "COMANDANTE OLLANTA HUMALA TASSO")
str_count(discurso1, "ANTE EL CONGRESO NACIONAL")
str_count(discurso1, "Fuente: Diario Oficial El Peruano")

discurso1 <-  str_remove_all(discurso1, "MENSAJE DEL PRESIDENTE CONSTITUCIONAL DEL PERÚ")
discurso1 <-  str_remove_all(discurso1, "COMANDANTE OLLANTA HUMALA TASSO")
discurso1 <-  str_remove_all(discurso1, "ANTE EL CONGRESO NACIONAL")
discurso1 <-  str_remove_all(discurso1, "Fuente: Diario Oficial El Peruano")


discurso1

# Buscando los números de página
str_count(discurso1, "\n[:blank:]+[:digit:]+\n")

discurso1 <- str_replace_all(discurso1, "[:blank:]{2,}", " ")

# Buscando los números
str_count(discurso1, "[:digit:]")

discurso1 <- str_remove_all(discurso1, "[:digit:]")

discurso1

# Guardando como un archivo de texto plano
write_lines(discurso1, "Discurso Presidencial Ollanta_2012.txt")

# Lectura de nuevo txt
discurso1 <- scan("Discurso Presidencial Ollanta_2012.txt",
                  encoding = "UTF-8", what = "char",
                  sep = "\n")

discurso1

head(discurso1)

tail(discurso1)

library(tidytext)
library(dplyr)
library(tibble)
library(tm)

discurso1 <- tibble(discurso1) %>% 
  tidytext::unnest_tokens(Token, discurso1) %>% 
  dplyr::mutate(Token = removeNumbers(Token)) 

# Los signos de puntuación se remueven automáticamente

discurso1

library(readxl)
stopwords_es_1 <- readLines("stopwords.es-1.txt", encoding = "UTF-8")
stopwords_es_1 <- iconv(stopwords_es_1, to = "ASCII//TRANSLIT")

stopwords_es_2 <- tibble(Token = tm::stopwords(kind = "es"), Fuente="tm")
stopwords_es_3 <- tibble(Token = stopwords::stopwords(language = "es", source = "stopwords-iso"),
                         Fuente = "stopwords-iso")
stopwords_es_4 <- tibble(Token = stopwords::stopwords(language = "es", source = "snowball"),
                         Fuente = "snowball")
stopwords_es_5 <- tibble(Token = c(""), Fuente = "Mis StopWords")

stopwords_es   <- rbind(stopwords_es_1, stopwords_es_2, stopwords_es_3, 
                        stopwords_es_4, stopwords_es_5)
stopwords_es   <- stopwords_es[!duplicated(stopwords_es$Token), ]

remove(stopwords_es_1, stopwords_es_2, stopwords_es_3, 
       stopwords_es_4, stopwords_es_5)

# Removiendo los stopwords
discurso1 <- discurso1 %>% anti_join(stopwords_es)

# Lematización pre-entrenada del paquete udpipe
library(udpipe)
# udpipe_download_model("spanish")  # se hace una sola vez

model <- udpipe_load_model(file = "spanish-gsd-ud-2.5-191206.udpipe")

tidy_discurso1_lemma <- udpipe_annotate(model, 
                                        x = discurso1$Token,
                                        doc_id = discurso1$Token)

tidy_discurso1_lemma           <- as_tibble(tidy_discurso1_lemma)
names(tidy_discurso1_lemma)[7] <- "Token" 

tidy_discurso1_lemma <- tidy_discurso1_lemma %>% 
  anti_join(stopwords_es)

discurso1 <- tidy_discurso1_lemma  %>%
  filter(!is.na(Token)) %>%
  dplyr::select(Token)

discurso1

# 1.2 Gráfico de frecuencias ----

# Obteniendo frecuencias por palabra
discurso1_frecuencias <- discurso1 %>%
  dplyr::count(Token, sort = TRUE)

discurso1_frecuencias

# Gráfico de palabras más frecuentes
library(forcats)
library(ggplot2)
grafico1 <- discurso1_frecuencias %>% 
  top_n(10) %>% 
  ggplot() + 
  aes(x = fct_reorder(Token, n), y = n, fill = Token) + 
  geom_col() +
  labs(x = NULL, y = "Frecuencia", 
       title = "Mensaje Presidencial Ollanta - 2012" ) +
  theme_minimal() + 
  theme(legend.position = "none") +
  coord_flip()

grafico1

# La palabra más frecuente del 2012 fue año, seguido de gobierno

# 1.3. Nube de palabras ----

# Nube de palabras con el paquete wordcloud2
library(wordcloud2)

set.seed(2021)
wordcloud2(discurso1_frecuencias,
           size = 0.7,
           shape = 'circle')

# Las palabras más frecuentes de 2012 según el wordcloud fueron gobierno,
# año, mil y país

# 1.4. Red de Términos ----

# Lectura de nuevo txt
discurso1 <- scan("Discurso Presidencial Ollanta_2012.txt",
                  encoding = "UTF-8", what = "char",
                  sep = "\n")

# Convert Character Vector between Encodings
txt <- iconv(discurso1, "latin1", to = "ASCII//TRANSLIT")
txt

# Construcción del Corpus

library(tm)
corpus <- Corpus(VectorSource(txt)) # Si es un data.frame
corpus                              # usar DataframeSource()

# Un vector source interpreta cada elemento del vector 'x' como
# un documento

# Verificación del corpus
inspect(corpus)

# Impresión de una línea del corpus
writeLines(as.character(corpus[[100]]))  # paquete base
content(corpus[[100]])

corpus[[100]]$content   # Equivalente al anterior

corpus[[100]]$meta      # Para ver lo metadatos de un documento

corpus[[100]]$meta$id


# Limpieza del Corpus

# Muestra las transformaciones disponibles
getTransformations()

# [1] "removeNumbers" "removePunctuation" "removeWords" 
# [4] "stemDocument"  "stripWhitespace" 

# Llevar a minúsculas, tolower es una función de {base}
d  <- tm_map(corpus, content_transformer(tolower))
inspect(d)

# Remueve la puntuación
d <- tm_map(d, removePunctuation)
inspect(d)

# Stopwords o palabras vacías
# Las palabras vacias no brindan mensaje comunicacional.
# Ejemplos de palabras vacias: artículos, pronombres, 
#                              conectores, entre otras.

# Remover palabras vacías genericas (stopwords)
# usando el diccionario en espaÃ±ol que proporciona R
# stopwords("spanish")

# d <- tm_map(d, removeWords, stopwords("spanish"))
# inspect(d)

# Remover stopwords usando diccionario propio
# Carga archivo de palabras vacías personalizada y 
# lo convierte a ASCII
sw.es <- readLines("stopwords.es-1.txt", encoding = "UTF-8")
sw.es <- iconv(sw.es, to = "ASCII//TRANSLIT")
sw.es

# Remover palabras vacías personalizadas
d <- tm_map(d, removeWords, sw.es)
inspect(d)

# Quitar espacios en blanco
d  <- tm_map(d, stripWhitespace)
inspect(d)

# Remover números
d <- tm_map(d, removeNumbers)

# Guardar el documento limpio
d_limpio <- data.frame(word = as.character(d))
write.table(d_limpio,
           "Discurso Presidencial Ollanta-2012-limpio.txt",
           sep=" ")

inspect(d)

# Term Document Matrix (TDM)

# Crear Matriz de Términos con TermDocumentMatrix()
tdm <- TermDocumentMatrix(d)
tdm

inspect(tdm)

inspect(tdm[1:10, 1:5])

# Otra forma de crear Matriz de Términos con DocumentTermMatrix()
dtm <- DocumentTermMatrix(d)
dtm

inspect(dtm)

inspect(dtm[1:10, 1:5])

# Hasta aquí tenemos cargada una matriz con todos los términos 
# que aparecen en el discurso y filtrada por las palabras 
# vacías (stopwords). 

# En caso quiera eliminar las palabras "ano" y "anos"
# d <- tm_map(d, removeWords, c("ano","anos"))  

# Remueve las palabras señoras y señores
d <- tm_map(d, removeWords, c("seaoras","seaores"))  


# En caso quiera reemplazar las palabras "ano" y "anos"
#d <- tm_map(d, content_transformer(gsub), 
#            pattern = "\\bano\\b",    
#            replacement = "aÃ±o")

#d <- tm_map(d, content_transformer(gsub), 
#            pattern = "\\banos\\b", 
#            replacement = "aÃ±os")

#d <- tm_map(d, gsub, 
#            pattern = 'aÃ±os', 
#            replacement = 'aÃ±o')

# En caso quiera reemplazar las palabras "reformas" por "reforma"
# d <- tm_map(d, gsub, 
#             pattern = "reformas", 
#             replacement = "reforma")
# inspect(d)

# Instalar Rgraphviz
# source("https://bioconductor.org/biocLite.R")
# biocLite("Rgraphviz")
library(BiocManager)
# BiocManager::install("Rgraphviz")

library(graph)
library(Rgraphviz)

# Volver a generar el TDM
tdm <- tm::TermDocumentMatrix(d)
tdm

findFreqTerms(tdm, lowfreq = 4)

freq.terms <- findFreqTerms(tdm, lowfreq = 4)

windows()
Rgraphviz::plot(tdm, 
                term = freq.terms[1:10],
                corThreshold = 0.1, 
                weighting = T)

dev.off()

# De las 10 palabras más frecuentes en el 2012,
# las que guardaron relacion fueron congreso y república, luego no se hallaron
# relaciones

# 1.5. Gráfico del Análisis de Sentimientos ----

texto <- d_limpio
texto

# Para unir todo en una sola cadena de caracteres:
texto <- paste(texto, collapse = " ") 

texto

library(quanteda)
discurso       <- quanteda::corpus(texto)
discurso

summary(discurso)

tokeninfo <- summary(discurso)
tokeninfo

# Frecuencia de uso de las palabras
# Se trata de un análisis de texto en su forma mÃ¡s simple, en 
# el que los temas se cuentan y se llevan a la parte superior 
# en función de la frecuencia con la que se mencionan. Esto es
# ideal para identificar rÃ¡pidamente los temas comunes.
dfm_inaug <- corpus_subset(discurso) %>%
  dfm(remove = stopwords('spanish'), 
      remove_punct = TRUE, 
      remove_numbers = TRUE) %>%
  dfm_trim(min_termfreq = 5, verbose = FALSE)


dfm_inaug 

# Extracción de sentimientos de los discursos
# Sentimientos por tokens del discurso:
sentimiento <- tokens(discurso, remove_punct = TRUE)
sent        <- tokens_lookup(sentimiento, dictionary = data_dictionary_LSD2015[1:2])
head(sent)

# Tabla resumen de sumatoria de sentimientos
library(kableExtra)
df_sentiment <- dfm(sent)
kable(df_sentiment,"html", caption = "Resumen de Sentimientos extraidos") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = F, position = "center")

# Se detectó 114 palabras con sensasión de sentimiento positivo y 66 negativos
# en el discurso del 2012

# II. Primer discurso ----

# Ollanta Humala 2013

library(pdftools)
discurso2 <- pdf_text("mensaje-2013-oh.pdf")

discurso2

# Queda una cadena de caracteres por página (7 en total).
# Se une todo en una sola cadena de carácteres:

discurso2 <- paste(discurso2, collapse = " ")

# 2.1 Limpieza de stopwords y lematización ----

# Es necesario eliminar los encabezados y pie de página

# Buscando el encabezado de página
library(stringr)
str_count(discurso2, "MENSAJE DEL PRESIDENTE CONSTITUCIONAL DEL PERÚ")
str_count(discurso2, "COMANDANTE OLLANTA HUMALA TASSO")
str_count(discurso2, "ANTE EL CONGRESO NACIONAL")
str_count(discurso2, "Fuente: Diario Oficial El Peruano")

discurso2 <-  str_remove_all(discurso2, "MENSAJE DEL PRESIDENTE CONSTITUCIONAL DEL PERÚ")
discurso2 <-  str_remove_all(discurso2, "COMANDANTE OLLANTA HUMALA TASSO")
discurso2 <-  str_remove_all(discurso2, "ANTE EL CONGRESO NACIONAL")
discurso2 <-  str_remove_all(discurso2, "Fuente: Diario Oficial El Peruano")


discurso2

# Buscando los números de página
str_count(discurso2, "\n[:blank:]+[:digit:]+\n")

discurso2 <- str_replace_all(discurso2, "[:blank:]{2,}", " ")

# Buscando los números
str_count(discurso2, "[:digit:]")

discurso2 <- str_remove_all(discurso2, "[:digit:]")

discurso2

# Guardando como un archivo de texto plano
write_lines(discurso2, "Discurso Presidencial Ollanta_2013.txt")

# Lectura de nuevo txt
discurso2 <- scan("Discurso Presidencial Ollanta_2013.txt",
                  encoding = "UTF-8", what = "char",
                  sep = "\n")

discurso2

head(discurso2)

tail(discurso2)

library(tidytext)
library(dplyr)
library(tibble)
library(tm)

discurso2 <- tibble(discurso2) %>% 
  tidytext::unnest_tokens(Token, discurso2) %>% 
  dplyr::mutate(Token = removeNumbers(Token)) 

# Los signos de puntuación se remueven automáticamente

discurso2

library(readxl)
stopwords_es_1 <- readLines("stopwords.es-1.txt", encoding = "UTF-8")
stopwords_es_1 <- iconv(stopwords_es_1, to = "ASCII//TRANSLIT")

stopwords_es_2 <- tibble(Token = tm::stopwords(kind = "es"), Fuente="tm")
stopwords_es_3 <- tibble(Token = stopwords::stopwords(language = "es", source = "stopwords-iso"),
                         Fuente = "stopwords-iso")
stopwords_es_4 <- tibble(Token = stopwords::stopwords(language = "es", source = "snowball"),
                         Fuente = "snowball")
stopwords_es_5 <- tibble(Token = c(""), Fuente = "Mis StopWords")

stopwords_es   <- rbind(stopwords_es_1, stopwords_es_2, stopwords_es_3, 
                        stopwords_es_4, stopwords_es_5)
stopwords_es   <- stopwords_es[!duplicated(stopwords_es$Token), ]

remove(stopwords_es_1, stopwords_es_2, stopwords_es_3, 
       stopwords_es_4, stopwords_es_5)

# Removiendo los stopwords
discurso2 <- discurso2 %>% anti_join(stopwords_es)

# Lematización pre-entrenada del paquete udpipe
library(udpipe)
# udpipe_download_model("spanish")  # se hace una sola vez

model <- udpipe_load_model(file = "spanish-gsd-ud-2.5-191206.udpipe")

tidy_discurso2_lemma <- udpipe_annotate(model, 
                                        x = discurso2$Token,
                                        doc_id = discurso2$Token)

tidy_discurso2_lemma           <- as_tibble(tidy_discurso2_lemma)
names(tidy_discurso2_lemma)[7] <- "Token" 

tidy_discurso2_lemma <- tidy_discurso2_lemma %>% 
  anti_join(stopwords_es)

discurso2 <- tidy_discurso2_lemma  %>%
  filter(!is.na(Token)) %>%
  dplyr::select(Token)

discurso2

# 2.2 Gráfico de frecuencias ----

# Obteniendo frecuencias por palabra
discurso2_frecuencias <- discurso2 %>%
  dplyr::count(Token, sort = TRUE)

discurso2_frecuencias

# Gráfico de palabras más frecuentes
library(forcats)
library(ggplot2)
grafico2 <- discurso2_frecuencias %>% 
  top_n(10) %>% 
  ggplot() + 
  aes(x = fct_reorder(Token, n), y = n, fill = Token) + 
  geom_col() +
  labs(x = NULL, y = "Frecuencia", 
       title = "Mensaje Presidencial Ollanta - 2013" ) +
  theme_minimal() + 
  theme(legend.position = "none") +
  coord_flip()

grafico2

# La palabra más frecuente del 2012 fue millón, seguido de país

# 2.3. Nube de palabras ----

# Nube de palabras con el paquete wordcloud2
library(wordcloud2)

set.seed(2021)
wordcloud2(discurso2_frecuencias,
           size = 0.7,
           shape = 'circle')

# Las palabras más frecuentes de 2013 según el wordcloud fueron millón,
# año, inversión y país

# 2.4. Red de Términos ----

# Lectura de nuevo txt
discurso2 <- scan("Discurso Presidencial Ollanta_2013.txt",
                  encoding = "UTF-8", what = "char",
                  sep = "\n")

# Convert Character Vector between Encodings
txt <- iconv(discurso2, "latin1", to = "ASCII//TRANSLIT")
txt

# Construcción del Corpus

library(tm)
corpus <- Corpus(VectorSource(txt)) # Si es un data.frame
corpus                              # usar DataframeSource()

# Un vector source interpreta cada elemento del vector 'x' como
# un documento

# Verificación del corpus
inspect(corpus)

# Impresión de una línea del corpus
writeLines(as.character(corpus[[100]]))  # paquete base
content(corpus[[100]])

corpus[[100]]$content   # Equivalente al anterior

corpus[[100]]$meta      # Para ver lo metadatos de un documento

corpus[[100]]$meta$id


# Limpieza del Corpus

# Muestra las transformaciones disponibles
getTransformations()

# [1] "removeNumbers" "removePunctuation" "removeWords" 
# [4] "stemDocument"  "stripWhitespace" 

# Llevar a minúsculas, tolower es una función de {base}
d2  <- tm_map(corpus, content_transformer(tolower))
inspect(d2)

# Remueve la puntuación
d2 <- tm_map(d2, removePunctuation)
inspect(d2)

# Stopwords o palabras vacías
# Las palabras vacias no brindan mensaje comunicacional.
# Ejemplos de palabras vacias: artículos, pronombres, 
#                              conectores, entre otras.

# Remover palabras vacías genericas (stopwords)
# usando el diccionario en espaÃ±ol que proporciona R
# stopwords("spanish")

# d2 <- tm_map(d2, removeWords, stopwords("spanish"))
# inspect(d2)

# Remover stopwords usando diccionario propio
# Carga archivo de palabras vacías personalizada y 
# lo convierte a ASCII
sw.es <- readLines("stopwords.es-1.txt", encoding = "UTF-8")
sw.es <- iconv(sw.es, to = "ASCII//TRANSLIT")
sw.es

# Remover palabras vacías personalizadas
d2 <- tm_map(d2, removeWords, sw.es)
inspect(d2)

# Quitar espacios en blanco
d2  <- tm_map(d2, stripWhitespace)
inspect(d2)

# Remover números
d2 <- tm_map(d2, removeNumbers)

# Guardar el documento limpio
d_limpio2 <- data.frame(word = as.character(d2))
write.table(d_limpio2,
            "Discurso Presidencial Ollanta-2013-limpio.txt",
            sep=" ")

inspect(d2)

# Term Document Matrix (TDM)

# Crear Matriz de Términos con TermDocumentMatrix()
tdm <- TermDocumentMatrix(d2)
tdm

inspect(tdm)

inspect(tdm[1:10, 1:5])

# Otra forma de crear Matriz de Términos con DocumentTermMatrix()
dtm <- DocumentTermMatrix(d2)
dtm

inspect(dtm)

inspect(dtm[1:10, 1:5])

# Hasta aquí tenemos cargada una matriz con todos los términos 
# que aparecen en el discurso y filtrada por las palabras 
# vacías (stopwords). 

# En caso quiera eliminar las palabras "ano" y "anos"
# d2 <- tm_map(d2, removeWords, c("ano","anos"))  

# Remueve las palabras señoras y señores
d2 <- tm_map(d2, removeWords, c("seaoras","seaores"))  

# En caso quiera reemplazar las palabras "ano" y "anos"
#d2 <- tm_map(d2, content_transformer(gsub), 
#            pattern = "\\bano\\b",    
#            replacement = "aÃ±o")

#d2 <- tm_map(d2, content_transformer(gsub), 
#            pattern = "\\banos\\b", 
#            replacement = "aÃ±os")

#d2 <- tm_map(d2, gsub, 
#            pattern = 'aÃ±os', 
#            replacement = 'aÃ±o')

# En caso quiera reemplazar las palabras "reformas" por "reforma"
# d2 <- tm_map(d2, gsub, 
#             pattern = "reformas", 
#             replacement = "reforma")
# inspect(d2)

# Instalar Rgraphviz
# source("https://bioconductor.org/biocLite.R")
# biocLite("Rgraphviz")
library(BiocManager)
# BiocManager::install("Rgraphviz")

library(graph)
library(Rgraphviz)

# Volver a generar el TDM
tdm2 <- tm::TermDocumentMatrix(d2)
tdm2

findFreqTerms(tdm2, lowfreq = 4)

freq.terms2 <- findFreqTerms(tdm, lowfreq = 4)

windows()
Rgraphviz::plot(tdm2, 
                term = freq.terms2[1:10],
                corThreshold = 0.1, 
                weighting = T)

dev.off()

# De las 10 palabras más frecuentes en el 2013,
# las que guardaron relacion fueron congreso y república, luego esfuerzo, gracias
# e historia.

# 2.5. Gráfico del Análisis de Sentimientos ----

texto <- d_limpio2
texto

# Para unir todo en una sola cadena de caracteres:
texto <- paste(texto, collapse = " ") 

texto

library(quanteda)
discurso       <- quanteda::corpus(texto)
discurso

summary(discurso)

tokeninfo <- summary(discurso)
tokeninfo

# Frecuencia de uso de las palabras
# Se trata de un análisis de texto en su forma mÃ¡s simple, en 
# el que los temas se cuentan y se llevan a la parte superior 
# en función de la frecuencia con la que se mencionan. Esto es
# ideal para identificar rÃ¡pidamente los temas comunes.
dfm_inaug <- corpus_subset(discurso) %>%
  dfm(remove = stopwords('spanish'), 
      remove_punct = TRUE, 
      remove_numbers = TRUE) %>%
  dfm_trim(min_termfreq = 5, verbose = FALSE)


dfm_inaug 

# Extracción de sentimientos de los discursos
# Sentimientos por tokens del discurso:
sentimiento <- tokens(discurso, remove_punct = TRUE)
sent        <- tokens_lookup(sentimiento, dictionary = data_dictionary_LSD2015[1:2])
head(sent)

# Tabla resumen de sumatoria de sentimientos
library(kableExtra)
df_sentiment <- dfm(sent)
kable(df_sentiment,"html", caption = "Resumen de Sentimientos extraidos") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = F, position = "center")

# Se detectó 73 palabras con sentimiento positivo y 34 con sentimiento negativo
# en el discurso de 2013

# III. Comparación de discursos ----

# 3.1. Análisis TF-IDF ----

#----------------------------------------#
#### VI. Comparando los dos discursos ####
#----------------------------------------#

library(patchwork)

# Graficando ambos discursos de manera vertical (columnas) ----
grafico1 + grafico2

# Creando una columna de texto para diferenciar los discursos ----
discurso1_frecuencias <-  discurso1_frecuencias %>% 
  mutate(discurso = "Mensaje Presidencial Julio-2012")

head(discurso1_frecuencias)

discurso2_frecuencias <-  discurso2_frecuencias %>% 
  mutate(discurso = "Mensaje Presidencial Julio-2013")

head(discurso2_frecuencias)

# Uniendo ambos discursos -------------------------------------
library(dplyr)
discursos <- dplyr::bind_rows(discurso1_frecuencias, 
                                discurso2_frecuencias)

# Al inicio estan las palabras más frecuentes del discurso de 2012
head(discursos)

# Al final estÃ¡n las palabras menos frecuentes discurso de 2013
tail(discursos)

# Por un tema de facilidad de lectura de los resultados, 
# se reordenan las columnas para que el campo discurso 
# quede a la izquierda:
discursos <- discursos %>% 
  select(discurso, Token, n)

head(discursos)

# Análisis TF-IDF

# Usando la funciÃ³n bind_tf_idf() 
discursos <- discursos %>% 
  bind_tf_idf(Token, discurso, n)

head(discursos, 10)

# discurso                            Token         n      tf   idf tf_idf
# <chr>                               <chr>     <int>   <dbl> <dbl>  <dbl>
#   1 Mensaje Presidencial setiembre-2018 año          70 0.0132      0      0
# 2 Mensaje Presidencial setiembre-2018 gobierno     61 0.0115      0      0
# 3 Mensaje Presidencial setiembre-2018 país         52 0.00982     0      0
# 4 Mensaje Presidencial setiembre-2018 mil          46 0.00869     0      0
# 5 Mensaje Presidencial setiembre-2018 programa     43 0.00812     0      0
# 6 Mensaje Presidencial setiembre-2018 millón       42 0.00794     0      0
# 7 Mensaje Presidencial setiembre-2018 proyecto     38 0.00718     0      0
# 8 Mensaje Presidencial setiembre-2018 social       38 0.00718     0      0
# 9 Mensaje Presidencial setiembre-2018 nacional     37 0.00699     0      0
# 10 Mensaje Presidencial setiembre-2018 inversión    33 0.00623     0      0

# Verificando el TF
discursos %>% group_by(discurso) %>% count(discurso)  # Palabras diferentes

# Existió 1875 palabras diferentes en el discurso de 2012 y 1314 en 2013

discursos %>% group_by(discurso) %>% summarise(sum(n)) # Total de palabras

# Existió 5293 total de palabras en el discurso de 2012 y 2816 en 2013

# Las palabras características de cada discurso son las que
# tienen el TF-IDF más alto:
discursos %>% 
  arrange(desc(tf_idf))

# Las palabras que fueron más frecuentes en 2012 y a la vez en 2013 fueron
# conflicto, historia y recaudación.

# 3.2 Gráfico que muestre las palabras más características de cada discurso----


# Gráfico que muestra las palabras más características de cada
# discurso
discursos %>% 
  arrange(discurso, desc(tf_idf)) %>% 
  group_by(discurso) %>%
  top_n(10) %>% 
  ggplot() + 
  aes(x = reorder(Token, tf_idf), y=tf_idf, fill = discurso) +
  geom_col(show.legend = FALSE) + labs(x = "Palabras", y = "") +
  facet_wrap(~discurso, scales = "free") + 
  coord_flip()

# 3.3. Comparación usando la similaridad de Coseno ----

library(widyr)
comparacion_n <- discursos %>%
  pairwise_similarity(discurso, Token, n) %>%
  arrange(desc(similarity))
comparacion_n

# Existe una similitud del 81.4 % entre los discursos del 2012 y del 2013.

# 3.4. Comparación visual de las palabras ----

# Discurso de 2012
discurso1 <- scan("Discurso Presidencial Ollanta_2012.txt",
                  encoding = "UTF-8", what = "char",
                  sep = "\n")

discurso1_frecuencias <- tibble(discurso1) %>% 
  unnest_tokens(palabra, discurso1) %>% 
  mutate(palabra = removeNumbers(palabra)) %>%
  count(palabra, sort = TRUE)
discurso1_frecuencias

stopwords_es <- scan("stopwords.es-1.txt",
                     encoding = "UTF-8", what = "char",
                     sep = "\n")

stopwords_es <- tibble(palabra = stopwords_es)

discurso1_frecuencias <- discurso1_frecuencias %>% 
  anti_join(stopwords_es) %>% 
  anti_join(tibble(palabra = c("")))

# Discurso de 2013
discurso2 <- scan("Discurso Presidencial Ollanta_2013.txt",
                  encoding = "UTF-8", what = "char",
                  sep = "\n")

discurso2_frecuencias <-  tibble(discurso2) %>% 
  unnest_tokens(palabra, discurso2) %>% 
  mutate(palabra = removeNumbers(palabra)) %>%
  count(palabra, sort = TRUE)

discurso2_frecuencias <- discurso2_frecuencias %>% 
  anti_join(stopwords_es) %>%
  anti_join(tibble(palabra = c("")))

# Adicionando el campo discurso 
discurso1_frecuencias <-  discurso1_frecuencias %>% 
  mutate(discurso = "julio-2012")

discurso2_frecuencias <-  discurso2_frecuencias %>% 
  mutate(discurso = "julio-2013")

# Juntando ambos discursos 
discursos <- bind_rows(discurso1_frecuencias, 
                       discurso2_frecuencias)

# Gráfico final

comparacion_discursos <- discursos %>%
  pivot_wider(names_from = discurso, # spread()
              values_from = n,
              values_fill = list(n = 0))

comparacion_discursos   <- as.data.frame(comparacion_discursos)

library(ggrepel)
comparacion_discursos %>% filter(`julio-2012`+
                                   `julio-2013` > 10) %>%
  ggplot() +
  aes(`julio-2012`, `julio-2013`) +
  geom_abline(color = "gray50", size = 1.2, alpha = 0.8, lty = 2) +
  geom_text_repel(aes(label = palabra)) +
  coord_fixed()

# La pababra más usada en 2013 más, que a la vez fue muy usada en 2012.
# La palabra menos usada en 2013 fue mil, pero que a su vez fue medianamente
# usada en 2012.
# La palabra menos usada en 2012 fue dólares, pero que a su vez fue medianamente
# usada en 2013.


# 3.5. Gráfica de una Commonality Cloud ----

# Graficando una Commonality Cloud 
row.names(comparacion_discursos) <- comparacion_discursos$palabra
comparacion_discursos$palabra    <- NULL

comparacion_discursos <- as.matrix(comparacion_discursos)

library(wordcloud)
set.seed(2021)
commonality.cloud(comparacion_discursos, 
                  max.words = 100, 
                  colors = "blue")

# Lás malabras más comunes de ambos discursos fueron: más,
# gobierno país y millones, lo que hace referencia que en su gobierno
# se hizo cada vez más inversiones a través del país.

# 3.6 Gráfica de una Comparison Cloud --------

# Graficando una Comparison Cloud -----------------------------
windows()
set.seed(2021)
comparison.cloud(comparacion_discursos, 
                 colors = c("orange", "blue"),
                 max.words = 100)
dev.off()

# La palabra más común del 2012 fue mil, seguido de año.
# La palabra más común del 2013 fue millones, seguido de dolares.
# Los discursos del ex-presidente Ollanta Humala en 2012 y 2013
# Se concentraron en dar a conocer la inversión que hacía o
# Que iba a realizar.