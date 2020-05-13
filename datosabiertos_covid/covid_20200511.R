# Covid en Mexico
# @elvagodeldato

## Los acentos se omiten intencionalmente

# Librerias
library(dplyr)
library(ggplot2)
library(broom)
library(readr)
library(janitor)
library(tidyr)
library(readxl)
library(fdth)

# PATH

# load data
(covid <- read_csv(paste0(bases,'200511COVID19MEXICO.csv')))

# en bonito
covid <- covid %>% 
  clean_names()

# estas son las columnas que me interesa traducir
dic_cols <- c('origen' ,'sector','sexo' ,'tipo_paciente','intubado','neumonia','embarazo','habla_lengua_indig','diabetes','epoc','asma','inmusupr','hipertension','otra_com','cardiovascular','obesidad','renal_cronica','tabaquismo','otro_caso','migrante','nacionalidad','pais_nacionalidad','pais_origen','resultado','entidad_nac','entidad_res','municipio_res','uci')

# pusieron los diccionarios, obtenemos la etiqueta de datos
excel_sheets(paste0(cat,'Catalogos_0412.xlsx'))
my_workbook <- lapply(excel_sheets(paste0(cat,'Catalogos_0412.xlsx')),read_excel, path = paste0(cat,'Catalogos_0412.xlsx'))

# despues de darle una peinada obtuve los datos

# origen
origen <- pull(my_workbook[[1]][2])
names(origen) <- pull(my_workbook[[1]][1])

# sector
sector <- pull(my_workbook[[2]][2])
names(sector) <- pull(my_workbook[[2]][1])

# sexo
sexo <- pull(my_workbook[[3]][2])
names(sexo) <- pull(my_workbook[[3]][1])

# tipo_paciente
tipo_paciente <- pull(my_workbook[[4]][2])
names(tipo_paciente) <- pull(my_workbook[[4]][1])

# si_no
si_no <- pull(my_workbook[[5]][2])
names(si_no) <- pull(my_workbook[[5]][1])

# nacionalidad
nacionalidad <- pull(my_workbook[[6]][2])
names(nacionalidad) <- pull(my_workbook[[6]][1])

# resultado
resultado <- pull(my_workbook[[7]][2])
names(resultado) <- pull(my_workbook[[7]][1])

# entidades
entidades <- pull(my_workbook[[8]][2])
names(entidades) <- pull(my_workbook[[8]][1])

# municipios
municipios <- pull(my_workbook[[9]][2])
names(municipios) <- pull(my_workbook[[9]][1])

codes <- c('origen' = origen ,
           'sector'= sector,
           'sexo' = sexo,
           'tipo_paciente' = tipo_paciente,
           'intubado'= si_no,
           'neumonia'= si_no,
           'embarazo'= si_no,
           'habla_lengua_indig'= si_no,
           'diabetes'= si_no,
           'epoc'= si_no,
           'asma'= si_no,
           'inmusupr'= si_no,
           'hipertension'= si_no,
           'otra_com'= si_no,
           'cardiovascular'= si_no,
           'obesidad'= si_no,
           'renal_cronica'= si_no,
           'tabaquismo'= si_no,
           'otro_caso'= si_no,
           'migrante'= si_no,
           'nacionalidad'= nacionalidad,
           'pais_nacionalidad' = nacionalidad,
           'pais_origen' = nacionalidad,
           'resultado'= resultado,
           'entidad_nac'=entidades,
           'entidad_res'=entidades,
           'municipio_res'= municipios,
           'uci'=si_no)

for(col in dic_cols){
  covid[,col] <- sapply(covid[,col], function(code){codes[paste(col, '.', code, sep = '')]})
}

#View(covid)
glimpse(covid)

# bdd_confirmados
confirmados <- covid %>% 
  filter(resultado=='Positivo SARS-CoV-2')

# 36327 confirmados
dim(confirmados) 

# tipo de paciente
table(confirmados$tipo_paciente)

# histograma de edad sobre sexo
ggplot(confirmados,aes(x = edad, fill=sexo )) + 
  geom_histogram(alpha=0.6,binwidth=5,position = 'identity') +   
  scale_fill_manual(values = c("#20B2AA", "#D8BFD8"))+
  labs(
  x = 'Edad',
  y = 'Casos',
  title = 'Rangos de edad y sexo',
  caption = "Fuente: Elaborado por @elvagodeldato con información de Secretaría de Salud") +
  theme(plot.caption = element_text(color = "gray30"))

# histograma de edad sobre tipo_paciente
ggplot(confirmados,aes(x = edad, fill=tipo_paciente )) + 
  geom_histogram(alpha=0.6,binwidth=5,position = 'identity') +   
  scale_fill_manual(values = c("#20B2AA", "#D8BFD8"))+
  labs(
    x = 'Edad',
    y = 'Casos',
    title = 'Rangos de edad y tipo de paciente',
    caption = "Fuente: Elaborado por @elvagodeldato con información de Secretaría de Salud") +
  theme(plot.caption = element_text(color = "gray30"))

# en afan de emular el tablero con otros graficos anadi el siguiente
ggplot(confirmados,aes(y = resultado, x = tipo_paciente   )) + 
  geom_col(width = 0.2) +   
  #scale_fill_manual(values = c("#20B2AA", "#D8BFD8"))+
  labs(
    x = 'Tipo de paciente',
    y = 'Casos',
    title = 'Casos por tipo de paciente',
    caption = "Fuente: Elaborado por @elvagodeldato con información de Secretaría de Salud") +
  theme(plot.caption = element_text(color = "gray30"),
        axis.text.y = element_text(angle = 90,hjust = 0))



