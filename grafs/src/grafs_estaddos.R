#
# Author: Georgina Jiménez
# Maintainers: Georgina Jiménez
# Municipios de la vacunación
# -------------------------------------------------
#vacunacion_municipios/grafs
options(scipen=999)
rm(list=ls())

require(pacman)
p_load(tidyverse, here, readxl, janitor,  writexl, ggalt, ggrepel, add2ggplot)

files <- list(data_f=here("clean-data/output/data-municipios.rds"),
              def_vac=here("clean-data/output/defun-vac.rds"),
              pob_vac=here("clean-data/output/pob-vac.rds"),
              logo=here("grafs/share/logo-dc.png"),
              pob_edo=here("grafs/output/pob-ent.jpg"),
              tam_edo=here("grafs/output/tam-ent.jpg"),
              casos_edo=here("grafs/output/casos-ent.jpg"),
              tablita=here("grafs/output/tablita.csv"))

#### Cosas para gráficas ####
tema <- theme_minimal() +
  theme(plot.title = element_text(size = 18, family = "Barlow Condensed", hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 14, family = "Barlow Condensed", hjust = 0.5),
        plot.caption = element_text(size = 12, family = "Barlow Condensed", hjust = 0, face = "italic"),
        axis.text = element_text(size = 12, family = "Barlow Condensed", face = "bold"),
        axis.title = element_text(size = 14, family = "Barlow Condensed"),
        legend.text = element_text(size = 12, family = "Barlow Condensed", hjust = 0.5),
        legend.title = element_text(size = 12, family = "Barlow Condensed", hjust = 0.5),
        strip.text = element_text(size = 14, face = "bold", family = "Barlow Condensed"))

colores <- c("#73d2de", "#8f2d56", "#ffbc42", "#d81159", "#218380")

add_dclogo <- function(graf, escala){
  graf_con_logo <- add_logo(
    plot_path = graf,
    logo_path = files$logo,
    logo_position = "bottom right",
    logo_scale = escala)
  
  magick::image_write(graf_con_logo, graf)
}

data2 <- readRDS(files$data_f)%>%
         mutate_at(vars(per_pobr, per_pobrext), as.numeric)
data <- readRDS(files$pob_vac)
#### Porcentaje de pobreza ####
data2%>%
  group_by(cve_ent, nom_ent, vacunas)%>%
  summarize(per_pobr=mean(per_pobr, na.rm=T))%>%
  ungroup%>%
  mutate(vacunas=gsub(0, "s_vacunas", vacunas),
         vacunas=gsub(1, "c_vacunas", vacunas))%>%
  pivot_wider(2:3, names_from="vacunas", values_from="per_pobr")%>%
  filter(!is.na(c_vacunas))%>%
  mutate(order=row_number(),
         dif=c_vacunas-s_vacunas)%>%
  ggplot(aes(x=s_vacunas, xend=c_vacunas, y=reorder(nom_ent, dif))) +
  geom_dumbbell(color="black", 
                size=2, 
                colour_x = "#73d2de",
                colour_xend = "#8f2d56", 
                size_x = 4.5, size_xend=4.5) + 
  geom_point(size=5, aes(x = s_vacunas, color = "Sin vacunas"))+
  geom_point(size=5, aes(x = c_vacunas, color = "Con vacunas"))+
   labs(title="Porcentaje promedio de pobreza en los municipios",
       subtitle="Según el estado del país y si van a ser vacunados o no",
       caption = "\n Fuente: Estimaciones de pobreza CONEVAL 2015 y https://coronavirus.gob.mx/vacunacion-covid/",
       x="\n", y="", fill = "") +
  tema+
  scale_color_manual(name = "", values = colores)
ggsave(files$pob_edo, width = 12, height = 8)
add_dclogo(graf = files$pob_edo, escala = 7)


#### Tamaño de la población por estado ####
data%>%
  group_by(vacunas, cve_ent, cve_mun, nom_mun, nom_ent)%>%
  summarize(pob_total=sum(total, na.rm=T))%>%
  ungroup()%>%
  group_by(cve_ent, nom_ent, vacunas)%>%
  summarize(pob_total=mean(pob_total, na.rm=T))%>%
  ungroup%>%
  mutate(vacunas=gsub(0, "s_vacunas", vacunas),
         vacunas=gsub(1, "c_vacunas", vacunas))%>%
  filter(!is.na(vacunas))%>%
  filter(!is.na(nom_ent))%>%
  pivot_wider(2:3, names_from="vacunas", values_from="pob_total")%>%
  filter(!is.na(c_vacunas))%>%
  mutate(order=row_number(),
         dif=c_vacunas-s_vacunas)%>%
  ggplot(aes(x=s_vacunas, xend=c_vacunas, y=reorder(nom_ent, dif))) + 
  geom_dumbbell(color="black", 
                size=2, 
                colour_x = "#73d2de",
                colour_xend = "#8f2d56", 
                size_x = 4.5, size_xend=4.5) + 
  geom_point(size=5, aes(x = s_vacunas, color = "Sin vacunas"))+
  geom_point(size=5, aes(x = c_vacunas, color = "Con vacunas"))+
           labs(title="Población promedio de los municipios",
                subtitle="Según el estado del país y si van a ser vacunados o no",
                caption = "\n Fuente: Censo 2020 y https://coronavirus.gob.mx/vacunacion-covid/",
                x="", y="", fill = "") +
           tema+
  scale_color_manual(name = "", values = colores)
ggsave(files$tam_edo, width = 12, height = 8)
add_dclogo(graf = files$tam_edo, escala = 7)


#### Número de casos por estado ####
data%>%
  group_by(vacunas, cve_ent, cve_mun, nom_mun, nom_ent)%>%
  summarize(pob_total=sum(total, na.rm=T))%>%
  ungroup()%>%
  filter(!is.na(vacunas))%>%
  left_join(data2)%>%
  mutate(tasa_casos=casos_covid/pob_total*100000)%>%
  group_by(cve_ent, nom_ent, vacunas)%>%
  summarize(tasa_casos=mean(tasa_casos, na.rm=T))%>%
  ungroup%>%
  mutate(vacunas=gsub(0, "s_vacunas", vacunas),
         vacunas=gsub(1, "c_vacunas", vacunas))%>%
  filter(!is.na(vacunas))%>%
  filter(!is.na(nom_ent))%>%
  pivot_wider(2:3, names_from="vacunas", values_from="tasa_casos")%>%
  filter(!is.na(c_vacunas))%>%
  mutate(dif=c_vacunas-s_vacunas)%>%
  ggplot(aes(x=s_vacunas, xend=c_vacunas, y=reorder(nom_ent, dif))) + 
  geom_dumbbell(color="black", 
                size=2, 
                colour_x = "#73d2de",
                colour_xend = "#8f2d56", 
                size_x = 4.5, size_xend=4.5) + 
  geom_point(size=5, aes(x = s_vacunas, color = "Sin vacunas"))+
  geom_point(size=5, aes(x = c_vacunas, color = "Con vacunas"))+
  labs(title="Tasa promedio de casos de COVID19 por cada 100mil habitantes",
       subtitle="Según el estado del país y si van a ser vacunados o no",
       caption = "\n Fuente: https://coronavirus.gob.mx/vacunacion-covid/",
       x="\n", y="", fill = "") +
  tema+
  scale_color_manual(name = "", values = colores)
ggsave(files$casos_edo, width = 12, height = 8)
add_dclogo(graf = files$casos_edo, escala = 7)


