#
# Author: Georgina Jiménez
# Maintainers: Georgina Jiménez
# Municipios de la vacunación
# -------------------------------------------------
#vacunacion_municipios/import/input/import-data

options(scipen=999)
rm(list=ls())

require(pacman)
p_load(tidyverse, here, readxl, janitor,  writexl)

files <- list(vacunas=here("import/input/vacunacion_municipios.xlsx"),
              nomun=here("import/input/Nombres_Municipios.xlsx"),
              pob=here("import/input/poblacionQuinquenalCenso2020.csv"),
              pobreza=here("import/input/pobreza_coneval.csv"),
              casos_i=here("import/input/Casos_Diarios_Municipio_Confirmados_20210216.csv"),
              pres_i=here("import/input/presidencia.csv"),
              exceso=here("import/input/exceso_nacional.rds"),
              sinais=here("import/input/sinais-all.rds"),
              pres_out=here("import/output/presidencia.rds"),
              casos_out=here("import/output/casos_covid.rds"),
              pob_out=here("import/output/pobreza_coneval.rds"),
              pob_qui=here("import/output/poblacion.rds"),
              mun_vac=here("import/output/municipios_vacunacion.rds"),
              defunciones=here("import/output/defunciones.rds"))

#### Municipios donde están vacunando ####
vac <- read_xlsx(files$vacunas)
data <- read_xlsx(files$nomun)%>%
        left_join(vac)%>%
        mutate(vacunas=ifelse(is.na(vacunas), 0, vacunas))
tempo <- filter(vac, nom_mun=="Puerto Morelos")
data <- bind_rows(data, tempo)%>%
        mutate(cve_ent = formatC(as.integer(cve_ent), width = 2, flag = 0, format = "d"),
               cve_mun = formatC(as.integer(cve_mun), width = 3, flag = 0, format = "d"),
               inegi=paste0(cve_ent, cve_mun))%>%
        saveRDS(files$mun_vac)
rm(tempo, vac)

#### Población ####
data <- read.csv(files$pob)%>%
        mutate(cve_ent = formatC(as.integer(cve_ent), width = 2, flag = 0, format = "d"),
               cve_mun = formatC(as.integer(cve_mun), width = 3, flag = 0, format = "d"),
               inegi=paste0(cve_ent, cve_mun))%>%
        saveRDS(files$pob_qui)

#### Pobreza ####
data <- read.csv(files$pobreza)%>%
        mutate(inegi = formatC(as.integer(inegi), width = 5, flag = 0, format = "d"))%>%
        select(-poblacion)%>%
        saveRDS(files$pob_out)

#### Contagios ####
data <- read.csv(files$casos_i)%>%
        mutate(casos_covid=rowSums(.[4:400]))%>%
        select(cve_ent, casos_covid)%>%
        rename(inegi=cve_ent)%>%
        mutate(inegi = formatC(as.integer(inegi), width = 5, flag = 0, format = "d"))%>%
        saveRDS(files$casos_out)
              
#### Casos ####
data <- read.csv(files$casos_i)%>%
        mutate(casos_covid=rowSums(.[4:400]))%>%
        select(cve_ent, casos_covid)%>%
        rename(inegi=cve_ent)%>%
        mutate(inegi = formatC(as.integer(inegi), width = 5, flag = 0, format = "d"))%>%
        saveRDS(files$casos_out)

#### Voto por partido por municipio ####
data <- read.csv(files$pres_i)%>%
        clean_names()%>%
        rename(cve_ent=id_estado, cve_mun=id_municipio)%>%
        select(-c(nombre_estado, municipio, secciones, casillas))%>%
        mutate(cve_ent=as.numeric(cve_ent),
               cve_mun=as.numeric(cve_mun))%>%
        filter(!is.na(cve_mun))%>%
        filter(!is.na(cve_ent))%>%
        mutate(cve_ent = formatC(as.integer(cve_ent), width = 2, flag = 0, format = "d"),
               cve_mun = formatC(as.integer(cve_mun), width = 3, flag = 0, format = "d"),
               inegi=paste0(cve_ent, cve_mun))%>%
        saveRDS(files$pres_out)

#### Defunciones 2019 y 2020 ####
data <- readRDS(files$exceso)%>%
        group_by(inegi, sexo, year)%>%
        summarize(muertes=n())%>%
        ungroup()%>%
        mutate(sexo = case_when(sexo==1 ~ "Mujer",
                                sexo==2 ~ "Hombre",
                                T ~ NA_character_))
sinais <- readRDS(files$sinais)%>%
          filter(year==2019)%>%
          group_by(inegi, sexo, year)%>%
          summarize(muertes=n())%>%
          ungroup()

data <- bind_rows(data, sinais)%>%
        filter(!is.na(inegi))%>%
        filter(!is.na(sexo))%>%
        saveRDS(files$defunciones)


