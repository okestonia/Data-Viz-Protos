#Piirikaubanduse mõju liiklusele
#Autor Annegrete Peek

#Paketid
library(dplyr)
library(data.table)
library(stringr)
library(geosphere)
library(ggplot2)

#04.06.18 päeva andmed, https://www2.politsei.ee/et/organisatsioon/analuus-ja-statistika/avaandmed.dot
liiklus <- fread("liiklusjarelevalve_2.csv", encoding = "UTF-8")

#Konverdin L-EST koordinaadid tavalisteks koordinaatideks, autor Taavi Unt
lest_geo <- function(x, y){
  # Konverteerib L-EST97 koordinaadid kaardirakendusele sobivaks (nn tavapärasteks koordinaatideks)
  # Kasutatud on maa-ameti veebilehel olevat php koodi:  
  # http://www.maaamet.ee/rr/geo-lest/files/geo-lest_function_php.txt
  
  a = 6378137
  F = 1 / 298.257222100883
  ESQ = F + F - F * F
  B0 = (57 + 31 / 60 + 3.194148 / 3600) / (180/pi)
  L0 = 24 / (180/pi)
  FN = 6375000
  FE = 500000
  B2 = (59 + 20 / 60) / (180/pi)
  B1 = 58 / (180/pi)
  xx = x - FN
  yy = y - FE
  t0 = sqrt((1 - sin(B0)) / (1 + sin(B0)) * ((1 + sqrt(ESQ) * sin(B0)) / (1 - sqrt(ESQ) * sin(B0))) ** sqrt(ESQ))
  t1 = sqrt((1 - sin(B1)) / (1 + sin(B1)) * ((1 + sqrt(ESQ) * sin(B1)) / (1 - sqrt(ESQ) * sin(B1))) ** sqrt(ESQ))
  t2 = sqrt((1 - sin(B2)) / (1 + sin(B2)) * ((1 + sqrt(ESQ) * sin(B2)) / (1 - sqrt(ESQ) * sin(B2))) ** sqrt(ESQ))
  m1 = cos(B1) / (1 - ESQ * sin(B1) * sin(B1)) ** 0.5
  m2 = cos(B2) / (1 - ESQ * sin(B2) * sin(B2)) ** 0.5
  n1 = (log(m1) - log(m2)) / (log(t1) - log(t2))
  FF = m1 / (n1 * t1 ** n1)
  p0 = a * FF * t0 ** n1
  p = (yy * yy + (p0 - xx) * (p0 - xx)) ** 0.5
  t = (p / (a * FF)) ** (1 / n1)
  FII = atan(yy / (p0 - xx))
  LON = FII / n1 + L0
  u = (pi / 2) - (2 * atan(t))
  LAT = (u + (ESQ / 2 + (5 * ESQ ** 2 / 24) + (ESQ ** 3 / 12) + (13 * ESQ ** 4 / 360)) * sin(2 * u) +
           ((7 * ESQ ** 2 / 48) + (29 * ESQ ** 3 / 240) + (811 * ESQ ** 4 / 11520)) * sin(4 * u) +
           ((7 * ESQ ** 3 / 120) + (81 * ESQ ** 4 / 1120)) * sin(6 * u) + (4279 * ESQ ** 4 / 161280) * sin(8 * u))
  LAT = LAT * 180/pi
  LON = LON * 180/pi
  
  return(data.frame(lat = LAT, lon = LON))
}

lest_geo_square <- function(x_sq, y_sq){
  # PPA andmestikus on sündmused paigutatud ruudustikku
  # Konverteerib ruudustiku LEST kooridnaadid tavapärasesse koordinaadistikku
  x_l = str_split_fixed(x_sq, "-", 2)
  x_l = data.frame(x_l, stringsAsFactors = FALSE)
  x_l[,1] = as.numeric(x_l[,1])
  x_l[,2] = as.numeric(x_l[,2])
  colnames(x_l) = c("x1","x2")
  y_l = str_split_fixed(y_sq, "-", 2)
  y_l = data.frame(y_l, stringsAsFactors = FALSE)
  y_l[,1] = as.numeric(y_l[,1])
  y_l[,2] = as.numeric(y_l[,2])
  colnames(y_l) = c("y1","y2")
  lat_lon1 = lest_geo(x_l$x1, y_l$y1)
  lat_lon2 = lest_geo(x_l$x2, y_l$y2)
  colnames(lat_lon1) = paste0(colnames(lat_lon1), "1")
  colnames(lat_lon2) = paste0(colnames(lat_lon2), "2")
  out = cbind(lat_lon1, lat_lon2)
  return(out)
}

add_coords <- function(dataset){
  # lisab andmestikule uute tunnustena konverteeritud koordinaadid
  return(cbind(dataset,lest_geo_square(dataset$Lest_X, dataset$Lest_Y)))
}

liiklus <- add_coords(liiklus)
#Panen kõik sündmused ruudu keskele ja jätan välja vaatlused, millel ei ole täielikke koordinaate
liiklus <- liiklus %>% mutate(lon = (lon1 + lon2)/2, lat = (lat1 + lat2)/2) %>% filter(!is.na(lat1))

#Eeldan, et baasaastad on 2013.-2015. aastad ja 2016. ja 2017. aasta on piirikaubanduse/alkoralli periood.
#Vaatan liiklusjärelevalve rikkumisi Ikla ja Valga piiripuktist 40km raadiuses oleval alal.
#Valga piiripunkt: lat 57.790229, lon 26.033000
#Ikla piiripunkt: lat 57.874603, lon 24.381502

#Arvutan kaugused
liiklus$kaugus_valga <- apply(liiklus[,c("lon", "lat")], 1, function(x) distm(c(x["lon"], x["lat"]), c(26.033000, 57.790229), fun = distHaversine))
liiklus$kaugus_ikla <- apply(liiklus[,c("lon", "lat")], 1, function(x) distm(c(x["lon"], x["lat"]), c(24.381502, 57.874603), fun = distHaversine))

table(liiklus$kaugus_ikla < 40000, liiklus$kaugus_valga < 40000) #Ei ole kattuvat piirkonda

#Piirkonna tunnus ja piirkonna sündmused
liiklus <- liiklus %>% 
  mutate(piirkond = case_when(kaugus_valga < 40000 ~ "Valga",
                              kaugus_ikla < 40000 ~ "Ikla",
                              TRUE ~ "Ülejäänud Eesti"))

#Liiklusrikkumised grupeerida ja kuupäevad agregeerida kuu tasemele
liiklus <- liiklus %>% mutate(rikkumine = case_when(Paragrahv == "§ 227." ~ "Kiiruseületamine",
                                                    Paragrahv == "§ 259." ~ "Liikleja (va mootorsõidukijuht) liiklusrikkumine",
                                                    Paragrahv %in% c("§ 224 '1.", "§ 224.", "§ 424.", "§ 424. '1") ~ "Juhtimine joobeseisundis",
                                                    TRUE ~ "Teised liiklusrikkumised"),
                              AASTA = year(ToimKpv))

#Kirjaldavad joonised
temp <- liiklus %>% group_by(AASTA, rikkumine, piirkond) %>% summarise(arv = n())
ggplot(temp, aes(AASTA, arv, color = rikkumine, group = rikkumine)) + 
  geom_line(size = 1.5) + 
  theme_bw(base_size = 18) + 
  facet_wrap(~ piirkond, scales = "free_y") +
  labs(title = "Liiklusrikkumiste arv on vähenenud.", 
       subtitle = "Kõigis kolmes piirkonnas on liiklusrikkumiste arv vähenenud. Isegi kui ülejäänud Eestis kiiruseületamiste arv kasvas, 
kahanes see stabiilselt piirialadel.", 
       x = NULL, 
       y = "Rikkumiste arv", 
       caption = "Andmed: PPA", 
       color = NULL) +
  scale_color_brewer(palette = "Set2") +
  theme(axis.line = element_line(colour = "#808080"),
        axis.ticks = element_line(colour = "#808080"),
        axis.text = element_text(colour = "#808080"),
        axis.title = element_text(colour = "#808080"),
        plot.subtitle = element_text(colour = "#808080"),
        plot.caption = element_text(colour = "#808080", size = 12),
        legend.text = element_text(colour = "#808080"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(colour = "#808080"),
        legend.position = "bottom")
temp <- temp %>% ungroup() %>%  mutate(piirkond = factor(ifelse(piirkond == "Ülejäänud Eesti", "Rest of Estonia", piirkond), levels = c("Ikla", "Valga", "Rest of Estonia")),
                        rikkumine = factor(case_when(rikkumine == "Juhtimine joobeseisundis" ~ "DUI",
                                              rikkumine == "Kiiruseületamine" ~ "Speeding",
                                              rikkumine == "Liikleja (va mootorsõidukijuht) liiklusrikkumine" ~ "Traffic violations by non-driver",
                                              TRUE ~ "Other traffic violations"), levels = c("DUI", "Speeding", "Traffic violations by non-driver", "Other traffic violations")))
ggplot(temp, aes(AASTA, arv, color = rikkumine, group = rikkumine)) + 
  geom_line(size = 1.5) + 
  theme_bw(base_size = 18) + 
  facet_wrap(~ piirkond, scales = "free_y") +
  labs(title = "Number of traffic violations has fallen.", 
       subtitle = "Number of traffic violations has fallen in all three areas. Even when number of speeding violations had risen in 
rest of Estonia it had fallen in the areas near Latvian border.", 
       x = NULL, 
       y = "Number of violations", 
       caption = "Data: PPA", 
       color = NULL) +
  scale_color_brewer(palette = "Set2") +
  theme(axis.line = element_line(colour = "#808080"),
        axis.ticks = element_line(colour = "#808080"),
        axis.text = element_text(colour = "#808080"),
        axis.title = element_text(colour = "#808080"),
        plot.subtitle = element_text(colour = "#808080"),
        plot.caption = element_text(colour = "#808080", size = 12),
        legend.text = element_text(colour = "#808080"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(colour = "#808080"),
        legend.position = "bottom")

#Liiklusõnnetuste andmed
#Andmed on http://kindlustus.maps.arcgis.com/apps/Viewer/index.html?appid=abd977aeea074631845cc67bfc3da87d lehelt kraabitud. 
onnetus <- fread("lkf.csv", encoding = "UTF-8") %>% mutate(time = as.Date(time, format("%Y-%m-%d %H:%M:%OS"))) %>% 
  filter(year(time) > 2012 & year(time) < 2018)
onnetus$kaugus_valga <- apply(onnetus[,c("lon", "lat")], 1, function(x) distm(c(x["lon"], x["lat"]), c(26.033000, 57.790229), fun = distHaversine))
onnetus$kaugus_ikla <- apply(onnetus[,c("lon", "lat")], 1, function(x) distm(c(x["lon"], x["lat"]), c(24.381502, 57.874603), fun = distHaversine))

table(onnetus$kaugus_ikla < 40000, onnetus$kaugus_valga < 40000) #Ei ole kattuvat piirkonda

#Piirkonna tunnus ja piirkonna sündmused
onnetus <- onnetus %>% 
  mutate(piirkond = case_when(kaugus_valga < 40000 ~ "Valga",
                              kaugus_ikla < 40000 ~ "Ikla",
                              TRUE ~ "Ülejäänud Eesti"),
         AASTA = year(time))
#Kontroll, kas kõikidel aastatel tundub jaotus sarnane
ggplot(onnetus, aes(x = lon, y = lat)) + 
  geom_point() + 
  theme_bw() +
  facet_wrap(~AASTA)


#Kirjaldavad joonised
temp <- onnetus %>% group_by(AASTA, piirkond) %>% summarise(arv = n())
ggplot(temp, aes(AASTA, arv, group = piirkond)) + 
  geom_line(size = 1.5, color = "#808080") + 
  theme_bw(base_size = 18) + 
  facet_wrap(~ piirkond, scales = "free_y") +
  labs(title = "Liiklusõnnetuse arv on kasvanud.", 
       subtitle = "Liiklusõnnetuste, kus on kaasatud kindlustus, arv on kõigis piirkondades pigem kasvanud.", 
       x = NULL, 
       y = "Õnnetuste arv", 
       caption = "Andmed: Eesti Liikluskindlustuse Fond", 
       color = NULL) +
  scale_color_brewer(palette = "Set2") +
  theme(axis.line = element_line(colour = "#808080"),
        axis.ticks = element_line(colour = "#808080"),
        axis.text = element_text(colour = "#808080"),
        axis.title = element_text(colour = "#808080"),
        plot.subtitle = element_text(colour = "#808080"),
        plot.caption = element_text(colour = "#808080", size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(colour = "#808080"))
temp <- temp %>% ungroup() %>%  mutate(piirkond = factor(ifelse(piirkond == "Ülejäänud Eesti", "Rest of Estonia", piirkond), levels = c("Ikla", "Valga", "Rest of Estonia")))
ggplot(temp, aes(AASTA, arv, group = piirkond)) + 
  geom_line(size = 1.5, color = "#808080") + 
  theme_bw(base_size = 18) + 
  facet_wrap(~ piirkond, scales = "free_y") +
  labs(title = "Number of traffic accidents has risen.", 
       subtitle = "Number of traffic accidents where insurance has been involved has risen.", 
       x = NULL, 
       y = "Number of accidents", 
       caption = "Data: Eesti Liikluskindlustuse Fond", 
       color = NULL) +
  scale_color_brewer(palette = "Set2") +
  theme(axis.line = element_line(colour = "#808080"),
        axis.ticks = element_line(colour = "#808080"),
        axis.text = element_text(colour = "#808080"),
        axis.title = element_text(colour = "#808080"),
        plot.subtitle = element_text(colour = "#808080"),
        plot.caption = element_text(colour = "#808080", size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(colour = "#808080"))
