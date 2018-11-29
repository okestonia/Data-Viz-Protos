#Äriregistri andmed
library(dplyr)
library(data.table)
library(stringr)
library(readxl)

#Äriregister seisuga 21.11.18
arireg <- fread("C:/Users/annegrete.peek/Desktop/Arireg/ettevotja_rekvisiidid_2018-11-21.csv", encoding = "UTF-8")
arireg <- arireg %>% 
  mutate(viimane = word(ads_normaliseeritud_taisaadress, -1),
         maja = ifelse(grepl("[[:digit:]]", viimane), viimane, NA),
         korter = ifelse(grepl("-", maja), gsub("^.*-", "", maja), NA))
arireg <- arireg %>% 
  mutate(korterita = ifelse(!is.na(korter), 
                            str_sub(ads_normaliseeritud_taisaadress, 1, str_length(ads_normaliseeritud_taisaadress) - str_length(korter) - 1), 
                            ads_normaliseeritud_taisaadress))

#Maksuvõlg seisuga 02.11.18
volg <- read_excel("C:/Users/annegrete.peek/Desktop/Arireg/maksuvolglaste_nimekiri.xlsx", skip = 2)
arireg <- arireg %>% mutate(volg = ifelse(ariregistri_kood %in% volg$Kood, 1, 0))

aadress <- arireg %>% group_by(korterita) %>% summarise(arv = n(), volg = sum(volg)) %>% ungroup() %>% arrange(-arv)

#Ehitised seisuga 24.11.18
ehitis <- fread("C:/Users/annegrete.peek/Desktop/Arireg/eh_ehitised_1994-01_2018-11-24.csv", encoding = "UTF-8") %>% filter(rajatis_hoone == "H") %>% 
  select(esmane_kasutus, nimetus, maakond, omavalitsus, taisaadress, lahiaadress, ehitisalune_pind, kasulik_pind, maht_bruto, suletud_netopind) 
ehitis2 <- ehitis %>% filter(taisaadress %in% arireg$korterita & taisaadress != "") %>% arrange(taisaadress, -ehitisalune_pind) %>% 
  group_by(taisaadress) %>% slice(1)
aadress <- aadress %>% left_join(ehitis2, by = c("korterita" = "taisaadress")) %>% unique()

aadress2 <- arireg %>% filter(!is.na(korter)) %>% group_by(ads_normaliseeritud_taisaadress) %>% summarise(arv = n(), volg = sum(volg)) %>% arrange(-arv)
