#
# Author: Georgina Jiménez
# Maintainers: Georgina Jiménez
# Municipios de la vacunación
# -------------------------------------------------
#vacunacion_municipios/grafs
options(scipen=999)
rm(list=ls())

require(pacman)
p_load(tidyverse, here, readxl, janitor,  writexl, add2ggplot)

files <- list(data_f=here("clean-data/output/data-municipios.rds"),
              def_vac=here("clean-data/output/defun-vac.rds"),
              pob_vac=here("clean-data/output/pob-vac.rds"),
              logo=here("grafs/share/logo-dc.png"),
              pobreza=here("grafs/output/pobreza.jpg"),
              pob_pobreza=here("grafs/output/pob_pobreza.jpg"),
              scatter=here("grafs/output/scatter.jpg"),
              pobr_poblacion=here("grafs/output/pobreza_poblacion.jpg"),
              pobr_vejez=here("grafs/output/pobreza_vejez.jpg"),
              obradorismo=here("grafs/output/obradorismo.jpg"),
              pobr_casos=here("grafs/output/pobreza_caso.jpg"),
              casos=here("grafs/output/casos.jpg"),
              exceso=here("grafs/output/exceso.jpg"),
              pobr_exceso=here("grafs/output/pobr_exceso.jpg"),
              spine=here("grafs/output/spine.jpg"))

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

#Datos
data <- readRDS(files$pob_vac)
data2 <- readRDS(files$data_f)%>%
         mutate_at(vars(per_pobr, per_pobrext), as.numeric)

#### Pobreza ####
data2%>%
  mutate(quart_pob=ntile(per_pobr, 5))%>%
  filter(!is.na(quart_pob))%>%
  group_by(vacunas, quart_pob)%>%
  summarize(total=n())%>%
  ungroup()%>%
  group_by(vacunas)%>%
  mutate(den=sum(total, na.rm=T))%>%
  ungroup()%>%
  mutate(per=total/den*100,
         quart_pob=factor(quart_pob, levels=c(1:5), labels=c("Nada pobres",
                                                             "Poco pobres",
                                                             "Algo pobres",
                                                             "Bastante pobres",
                                                             "Muy pobres")),
         vacunas=factor(vacunas, levels=c(0:1), labels=c("No vacunas", "Vacunas")))%>%
  ungroup()%>%
  ggplot(aes(x=vacunas, y=per, fill=quart_pob)) +
  geom_bar(stat="identity", position="stack")+
  geom_text(aes(label = paste(round(per,1), "%")),
            position = position_stack(vjust = .5), vjust=-1, 
            size=5.5, color="black", family="Barlow Condensed")+
  labs(title="¿Qué tan pobres son los municipios donde empezaron a vacunar?",
       subtitle="Composición del grupo de municipios según quintil de pobreza",
       caption = "\n Fuente: Estimaciones de pobreza CONEVAL 2015 y https://coronavirus.gob.mx/vacunacion-covid/",
       x="\n", y="\n Quintil de pobreza del municipio \n", fill = "") +
  scale_fill_manual(values=colores)+
  tema+
  coord_flip()
ggsave(files$pobreza, width = 12, height = 8)
add_dclogo(graf = files$pobreza, escala = 7)

#### Pobreza vs tamaño de la población ####
data%>%
  group_by(vacunas, cve_ent, cve_mun, nom_mun, nom_ent)%>%
  summarize(pob=sum(total, na.rm=T))%>%
  ungroup()%>%
  filter(!is.na(vacunas))%>%
  ungroup()%>%
  left_join(data2)%>%
  mutate(vacunas=factor(vacunas, levels=c(0:1), labels=c("No vacunas", "Vacunas")))%>%
  ggplot(aes(x=pob, y=per_pobr, color=vacunas)) +
  geom_point(size=4)+
  labs(title = "Población y pobreza de los municipios donde empezaron a vacunar",
       x = "Población total", y = "Porcentaje de la población que vive en pobreza", color="",
       caption = "Fuente: Censo 2020, estimaciones de pobreza CONEVAL 2015 y https://coronavirus.gob.mx/vacunacion-covid/") +
  tema +
  theme(legend.position = "top")+
  scale_color_manual(values=colores)
ggsave(files$scatter, width = 12, height = 8)
add_dclogo(graf = files$pob_pobreza, escala = 7)


###a ver
data%>%
  group_by(vacunas, cve_ent, cve_mun, nom_mun, nom_ent)%>%
  summarize(pob_total=sum(total, na.rm=T))%>%
  ungroup()%>%
  filter(!is.na(vacunas))%>%
  left_join(data2)%>%
  mutate(tasa_casos=casos_covid/pob_total*100000,
         vacunas=factor(vacunas, levels=c(0:1), labels=c("No vacunas", "Vacunas")))%>%
  ggplot(aes(x=tasa_casos, y=per_pobr, color=vacunas)) +
  geom_point(size=4)+
  labs(title = "Tasa de casos de COVID y pobreza de los municipios donde empezaron a vacunar",
       x = "Tasa de casos", y = "Porcentaje de la población que vive en pobreza", color="",
       caption = "Fuente: Censo 2020, estimaciones de pobreza CONEVAL 2015 y https://coronavirus.gob.mx/vacunacion-covid/") +
  tema +
  theme(legend.position = "top")+
  scale_color_manual(values=colores)
  
  

