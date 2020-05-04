# Recursos de salud 2018
# @elvagodeldato

## Los acentos se omiten intencionalmente

# Librerias
library(dplyr)
library(ggplot2)
library(broom)
library(readr)
library(janitor)
library(tidyr)

# PATH

# Datos de SSA
bd <- read_csv(paste0(bases,'Recursos_Salud_2018.csv'))

# Guardo los nombres de las columnas me estreso sin datos curados
columnas <- as.data.frame(colnames(bd))
#write_csv(columnas,paste0(bases,'nombre_col.csv'))

# BD sin nombres
bd <- read.csv(paste0(bases,'Recursos_Salud_2018.csv'),skip = 1, header = F)
glimpse(bd)

# Claves estados
bd %>% 
  select(V5) %>% 
  unique() %>% 
  as.list()

# Poblacion
pob <- read_csv(paste0(bases,'ind_dem_proyecciones.csv'))
glimpse(pob)

# Poblacion a mitad de year
(pmao <- pob %>% 
  filter(AO == 2018 & ENTIDAD != 'Repblica Mexicana') %>% 
  arrange(-POB_MIT_AO) %>% 
    select(ENTIDAD,CVE_GEO,POB_MIT_AO) %>% 
    unique() %>% 
    clean_names()) 

# Tipo de establecimiento por entidad federativa
bd %>% 
  select(V6,V12) %>% 
  mutate(contador = 1) %>% 
  group_by(V6, V12) %>% 
  summarise(unidades = sum(contador, na.rm = T)) %>% 
  clean_names() %>% 
  spread(v12,unidades) %>% 
  replace(is.na(.), 0) %>% 
  mutate(
    colsum=100* `HOSPITALIZACIXN`/(`CONSULTA EXTERNA` + `DE APOYO`+ `DE ASISTENCIA SOCIAL`+`HOSPITALIZACIXN`))

# Numero de camas de cuidados intensivos
sum(bd[120], na.rm = T)

# Unidades de cuidades intensivos
bd_uci <- tbl_df(bd[bd$V35==1,])

# Proporcion de camas uci por tipo de institucion
bd_uci %>%
  group_by(V4) %>% 
    #select("V4","V120") %>% 
  summarise(n = sum(V120,na.rm = T)) %>% 
  mutate(freq = n/sum(n)) %>%
  ggplot() + geom_col(aes(x= reorder(V4, -freq),y=freq)) +
  labs(
    x = 'Afiliación',
    y = 'Porcentaje de camas de cuidados intensivos',
    title = 'Camas de cuidados intensivos por afiliación',
    caption = "Fuente: Elaborado por @elvagodeldato con información de Secretaría de Salud"
    
  ) + scale_y_continuous(labels = scales::percent)

# Proporcion de camas uci por entidad federativa
(bd_uci_camas <- bd_uci %>%
    group_by(V6) %>% 
    #select(V6,V120) %>% 
    summarise(n = sum(V120,na.rm = T)) %>% 
    mutate(freq = n/sum(n)*100) %>% 
    arrange(-freq))

# Estadisticos del numero de camas UCI
bd_uci_camas %>% 
  summarise(prom=mean(n,na.rm = T),
            min = min(n,na.rm = T),
            max = max(n,na.rm= T),
            ds = sd(n,na.rm = T)) 


# Nayarit el estado con menos camas de cuidados intensivos
bd_uci_camas[which.min(bd_uci_camas$freq),]

# Unidades con areas de hospitalizacion
bd_ah <- tbl_df(bd[bd$V33==1,])

# Proporcion de camas AH por entidad federativa por mil habitantes
View(bd_ah_camas <- bd_ah %>%
  group_by(V6) %>% 
  select(V6,V92) %>% 
  summarise(n = sum(V92,na.rm = T)) %>% 
  mutate(freq = n/sum(n)*100) %>% 
  arrange(-freq))

ggplot(bd_ah_camas,aes(y = n)) + geom_boxplot()

bd_ah_camas <- merge(x = bd_ah_camas, y = unique(bd[,c('V6','V5')]), by = "V6", all.x=TRUE)
bd_ah_camas <- tbl_df(bd_ah_camas)

# Nombre de columnas
colnames(bd_ah_camas) 

# Lo cambiamos por comodidad al merge
names(bd_ah_camas)[names(bd_ah_camas) == 'V5'] <- 'CVE_GEO'

# Estadisticos del numero de camas AH
bd_ah_camas %>% 
  summarise(prom=mean(n,na.rm = T),
            min = min(n,na.rm = T),
            max = max(n,na.rm= T),
            ds = sd(n,na.rm = T))

# Densidad por mil
pmao2 <- pmao %>% 
  mutate(
    pobmil=POB_MIT_AO/1000
  )

# merge
bd_ah_camas <- left_join(bd_ah_camas,pmao2,by="CVE_GEO")
bd_ah_camas <- bd_ah_camas %>% 
  select(-ENTIDAD)

# Creamos la proporcion
bd_ah_camas$pmil <- bd_ah_camas$n/bd_ah_camas$pobmil

# Comparativo camas vs camas por mil
bd_ah_camas %>% 
  arrange(-n)

ggplot(bd_ah_camas,aes(x = reorder(V6, -n), y = n)) + geom_col() +  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + coord_flip()

# Notar sonora
bd_ah_camas2 <- bd_ah_camas %>% 
  arrange(-pmil)

ggplot(bd_ah_camas2,aes(x = reorder(V6, -pmil), y = pmil)) + geom_col() +  theme(axis.text.x = element_text(angle = 90, hjust = 1))  + coord_flip()


# Numero de medicos generales y especialistas
sum(bd[140], na.rm = T)==sum(bd[141:176], na.rm = T)

# Porcentaje de medicos especialistas y generales
bd %>% 
  select(V140:V176) %>% 
  summarise(
    total = sum(V140),
    mgyf = sum(V141,V142),
    esp = sum(V143,V144,V145,V146,V147,V148,V149,V150,V151,V152,V153,V154,V155,V156,V157,V158,V159,V160,V161,V162,V163,V164,V165,V166,V167,V168,V169,V170,V171,V172,V173,V174,V175,V176)
  ) %>% 
  mutate(
    pesp = esp/total*100,
    pgen = 100 - pesp
  )   
  
# Porcentaje de especialistas criticos
bd %>% 
  summarise(
    espcrit = sum(V146,V161,V165,V151,V171),
    otresp = sum(V143,V144,V145,V147,V148,V149,V150,V152,V153,V154,V155,V156,V157,V158,V159,V160,V162,V163,V164,V166,V167,V168,V169,V170,V172,V173,V174,V175,V176),
    pespcrit = espcrit/sum(espcrit+otresp)*100,
    potroesp = (100-pespcrit)
)


# Medicos por entidad federativa
medent <- bd %>% 
  group_by(V6) %>% 
  select(V6,V140:V176) %>% 
  summarise(
    medicos = sum(V140,na.rm = T)
  ) %>% 
  arrange(-medicos)

medent <- merge(x =medent , y = unique(bd[,c('V6','V5')]), by = "V6", all.x=TRUE)
medent <- tbl_df(medent)

# Lo cambiamos por comodidad al merge
names(medent)[names(medent) == 'V5'] <- 'CVE_GEO'

# merge
medent <- left_join(medent,pmao2,by="CVE_GEO")
medent <- medent %>% 
  select(-ENTIDAD)

# Medicos por cada mil habitantes
medent %>% 
  mutate(
    mmil=medicos/pobmil
)

# Medicos criticos por entidad federativa
bd %>% 
  select(V6,V146,V161,V165,V151,V171,V140) %>% 
  group_by(V6) %>% 
  #select(V6,V140:V176) %>% 
  summarise(
    total = sum(V140),
    medicosc = sum(V146,V161,V165,V151,V171,na.rm = T),
    internista = sum(V146),
    neumolo = sum(V161),
    urgen = sum(V165),
    anes = sum(V151),
    infec = sum(V171)
  ) %>% 
  arrange(-medicosc) 

# Base medicos
bdmed <- bd %>% 
  group_by(V6) %>% 
  select(V6,V140:V176) %>% 
  summarise_all(funs(sum))

#bdmed %>% 
#  select(V6,V140) %>% 
#  gather(key = 'esp', value = 'medicos', V140)


# Graficos


glimpse(bd)

