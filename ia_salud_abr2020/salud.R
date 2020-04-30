# Recursos de salud 2018
# @elvagodeldato

## Los acentos se omiten intencionalmente

# Librerias
library(dplyr)
library(ggplot2)
library(broom)
library(readr)

# PATH

# Datos
bd <- read_csv(paste0(bases,'Recursos_Salud_2018.csv'))

# Guardo los nombres de las columnas me estreso sin datos curados
columnas <- as.data.frame(colnames(bd))
write_csv(columnas,paste0(bases,'nombre_col.csv'))

# BD sin nombres
bd <- read.csv(paste0(bases,'Recursos_Salud_2018.csv'),skip = 1, header = F)
glimpse(bd)

# Tipo de establecimiento por entidad federativa
bd %>% 
  mutate(contador = 1) %>% 
  group_by(V6, V12) %>% 
  summarise(unidades = sum(contador, na.rm = T))

# Numero de camas de cuidados intensivos
sum(bd[120], na.rm = T)

# Unidades de cuidades intensivos
bd_uci <- tbl_df(bd[bd$V35==1,])

# Proporcion de camas uci por tipo de institucion
(bd_uci %>%
  group_by(V4) %>% 
    select(V4,V120) %>% 
  summarise(n = sum(V120,na.rm = T)) %>% 
  mutate(freq = n/sum(n)*100))

# Proporcion de camas uci por entidad federativa
bd_uci_camas <- bd_uci %>%
    group_by(V6) %>% 
    select(V6,V120) %>% 
    summarise(n = sum(V120,na.rm = T)) %>% 
    mutate(freq = n/sum(n)*100) %>% 
    arrange(-freq)

# Nayarit el estado con menos camas de cuidados intensivos
bd_uci_camas[which.min(bd_uci_camas$freq),]

# Unidades con areas de hospitalizacion
bd_ah <- tbl_df(bd[bd$V33==1,])

# Proporcion de camas AH por entidad federativa por mil habitantes
View(bd_ah_camas <- bd_ah %>%
  group_by(V6) %>% 
  select(V6,V92) %>% 
  summarise(n = sum(V92,na.rm = T)) %>% 
  mutate(freq = n/sum(n)*100,
         cpormil = n/1000) %>% 
  arrange(-freq))
