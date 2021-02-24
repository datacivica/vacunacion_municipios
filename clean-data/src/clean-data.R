#
# Author: Georgina Jiménez
# Maintainers: Georgina Jiménez
# Municipios de la vacunación
# -------------------------------------------------
#vacunacion_municipios/clean-data/src/clean-data

options(scipen=999)
rm(list=ls())

require(pacman)
p_load(tidyverse, here, readxl, janitor,  writexl)

files <- list(exceso=here("import/input/exceso_nacional.rds"),
              pres_out=here("import/output/presidencia.rds"),
              casos_out=here("import/output/casos_covid.rds"),
              pob_out=here("import/output/pobreza_coneval.rds"),
              pob_qui=here("import/output/poblacion.rds"),
              mun_vac=here("import/output/municipios_vacunacion.rds"),
              defunciones=here("import/output/defunciones.rds"),
              data_f=here("clean-data/output/data-municipios.rds"),
              def_vac=here("clean-data/output/defun-vac.rds"),
              pob_vac=here("clean-data/output/pob-vac.rds"))

#### Una misma base con todo, menos población ####
data <- readRDS(files$mun_vac)

tempo <- readRDS(files$casos_out)

data <- left_join(data, tempo)

tempo <- readRDS(files$pob_out)%>%
         filter(year==2015)%>%
         select(inegi, per_pobr, per_pobrext)

data <- left_join(data, tempo)

tempo <- readRDS(files$pres_out)%>%
         mutate(anaya=pan+prd+mc+pan_prd_mc+pan_prd+pan_mc+prd_mc,
                amlo=morena+pt+es+pt_morena_es+pt_morena+pt_es+morena_es,
                anaya=anaya/total_votos*100,
                amlo=amlo/total_votos*100)%>%
          select(inegi, amlo, anaya, lista_nominal, total_votos)

data <- left_join(data, tempo)
saveRDS(data, files$data_f)

#### Defunciones y población van a quedar distinto####
data <- readRDS(files$defunciones)

tempo <- readRDS(files$mun_vac)

data <- left_join(data, tempo)%>%
        saveRDS(files$def_vac)

data <- readRDS(files$pob_qui)%>%
        left_join(tempo)%>%
        saveRDS(files$pob_vac)
        

