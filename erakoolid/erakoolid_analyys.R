## Erakoolide populaarsus
## Autor: Annegrete Peek
## Andmed: Haridus- ja Teadusministeerium, https://www.hm.ee/ehis/statistilised_tabelid/download.php?file=alus_yld_oppeasutused_oppurid.xlsx [24.08.2018]

#Vajalikud paketid
library(readxl)
library(dplyr)
library(ggplot2)
library(forcats)
library(gridExtra)
library(scales)
library(grid)

#Andmete sisse lugemine
kontakt <- read_xlsx("alus_yld_oppeasutused_oppurid.xlsx", sheet ="kontakt", skip = 6)
andmed13 <- read_xlsx("alus_yld_oppeasutused_oppurid.xlsx", sheet ="2013", skip = 9) %>% mutate(Aasta = 2013) %>% 
  select("Kooli nimi", "Kooli ID", "Omavalitsus", "Maakond", "Kooli liik", "Kooli tüüp", "Omandivorm", "1.", "2.", "3.", "4.", "5.", "6.", "7.", "8.", "9.", "G10", "G11", "G12", "Aasta") #544 kooli
andmed14 <- read_xlsx("alus_yld_oppeasutused_oppurid.xlsx", sheet ="2014", skip = 9) %>% mutate(Aasta = 2014) %>% 
  select("Kooli nimi", "Kooli ID", "Omavalitsus", "Maakond", "Kooli liik", "Kooli tüüp", "Omandivorm", "1.", "2.", "3.", "4.", "5.", "6.", "7.", "8.", "9.", "G10", "G11", "G12", "Aasta") #534 kooli
andmed15 <- read_xlsx("alus_yld_oppeasutused_oppurid.xlsx", sheet ="2015", skip = 9) %>% mutate(Aasta = 2015) %>% 
  select("Kooli nimi", "Kooli ID", "Omavalitsus", "Maakond", "Kooli liik", "Kooli tüüp", "Omandivorm", "1.", "2.", "3.", "4.", "5.", "6.", "7.", "8.", "9.", "G10", "G11", "G12", "Aasta") #527 kooli
andmed16 <- read_xlsx("alus_yld_oppeasutused_oppurid.xlsx", sheet ="2016", skip = 9) %>% mutate(Aasta = 2016) %>% 
  select("Kooli nimi", "Kooli ID", "Omavalitsus", "Maakond", "Kooli liik", "Kooli tüüp", "Omandivorm", "1.", "2.", "3.", "4.", "5.", "6.", "7.", "8.", "9.", "G10", "G11", "G12", "Aasta") #526 kooli
andmed17 <- read_xlsx("alus_yld_oppeasutused_oppurid.xlsx", sheet ="2017", skip = 9) %>% mutate(Aasta = 2017) %>% 
  select("Kooli nimi", "Kooli ID", "Omavalitsus", "Maakond", "Kooli liik", "Kooli tüüp", "Omandivorm", "1.", "2.", "3.", "4.", "5.", "6.", "7.", "8.", "9.", "G10", "G11", "G12", "Aasta") #522 kooli
andmed <- andmed13 %>% rbind(andmed14) %>% rbind(andmed15) %>%  rbind(andmed16) %>% rbind(andmed17) %>% filter(`Kooli nimi` != "Kokku", `Kooli tüüp` != "täiskasvanute gümnaasium") #Täiskasvanute gümnaasiumis ei ole üldiselt statsionaarset õpet
andmed[is.na(andmed)] <- 0
andmed <- andmed %>% mutate(pohikool_kokku = `1.` + `2.` + `3.` + `4.` + `5.` + `6.` + `7.` + `8.` + `9.`, gym_kokku = `G10` + `G11` + `G12`, kokku = pohikool_kokku + gym_kokku, erakool = ifelse(Omandivorm == "eraomand", 1, 0)) %>% 
  filter(kokku != 0)

##Erakoolide arv

#Aastate lõikes
table(andmed$erakool, andmed$Aasta)
mahud <- andmed %>% group_by(Aasta) %>% summarise(koolid_arv = sum(erakool), koolid_suh = round(mean(erakool)*100,1), opilased_arv = sum(kokku[erakool == 1]), opilased_suh = round(sum(kokku[erakool == 1])/sum(kokku),3)*100)
ggplot(mahud, aes(x = Aasta, ymin = 0)) + geom_line(aes(y = koolid_suh, color = "Erakoolide osakaal"), ymin = 0) + geom_line(aes(y = opilased_suh, color = "Erakoolide õpilaste osakaal")) + theme_bw() 
setdiff(andmed$`Kooli nimi`[andmed$Aasta == 2015 & andmed$erakool == 1], andmed$`Kooli nimi`[andmed$Aasta == 2016 & andmed$erakool == 1])
#Keila Kool muutus 2016. aastal munitsipaalkooliks. 
ggplot(mahud, aes(x = Aasta, ymin = 0)) + geom_line(aes(y = koolid_suh, color = "Erakoolide osakaal"), size = 2) + geom_line(aes(y = opilased_suh, color = "Erakoolide õpilaste osakaal"), size = 2) + theme_bw(base_size = 15) + 
  labs(title = "Erakoolide populaarsus on kasvanud. ", x = "", y = "%", caption = "Andmed: EHIS") + theme(legend.position = "bottom", legend.title = element_blank()) + scale_color_manual(values = c("gray50", "firebrick"))
ggplot(mahud, aes(x = Aasta, ymin = 0)) + geom_line(aes(y = koolid_suh, color = "Percentage of private schools"), size = 2) + geom_line(aes(y = opilased_suh, color = "Percentage of students in private schools"), size = 2) + theme_bw(base_size = 15) + 
  labs(title = "The popularity of private schools has risen.", x = "", y = "%", caption = "Data: EHIS") + theme(legend.position = "bottom", legend.title = element_blank()) + scale_color_manual(values = c("gray50", "firebrick"))


#Maakondade
maakonnad <- andmed %>% filter(Aasta == 2017) %>% mutate(Maakond = gsub(" maakond", "", Maakond)) %>% group_by(Maakond) %>% 
  summarise(koolid_arv = sum(erakool), koolid_suh = round(mean(erakool)*100,1), opilased_arv = sum(kokku[erakool == 1]), opilased_suh = round(sum(kokku[erakool == 1])/sum(kokku),3)*100) %>% 
  mutate(highlight = ifelse(Maakond == "Lääne-Viru", "1", "0"))
maakonnad$Maakond <- factor(maakonnad$Maakond, levels = maakonnad$Maakond[order(-maakonnad$koolid_suh)])
p1 <- ggplot(maakonnad[maakonnad$koolid_suh != 0,], aes(Maakond, koolid_suh, fill = highlight)) + geom_col() + theme_bw(base_size = 14) + labs(y = "Erakoolide osakaal (%)", x = "", caption = " ") + 
  scale_fill_manual(values = c("gray50", "firebrick")) + theme(legend.position = "none") + scale_y_continuous(limits = c(0,21), oob = rescale_none)
p1_eng <- ggplot(maakonnad[maakonnad$koolid_suh != 0,], aes(Maakond, koolid_suh, fill = highlight)) + geom_col() + theme_bw(base_size = 14) + labs(y = "Percentage of private schools", x = "", caption = " ") + 
  scale_fill_manual(values = c("gray50", "firebrick")) + theme(legend.position = "none") + scale_y_continuous(limits = c(0,21), oob = rescale_none)

maakonnad$Maakond <- factor(maakonnad$Maakond, levels = maakonnad$Maakond[order(-maakonnad$opilased_suh)])
p2 <- ggplot(maakonnad[maakonnad$opilased_suh != 0,], aes(Maakond, opilased_suh, fill = highlight)) + geom_col() + theme_bw(base_size = 14) + labs(y = "Erakoolis käivate laste osakaal (%)", x = "", caption = "Andmed: EHIS, õppeaasta 2017/2018") + 
  scale_fill_manual(values = c("gray50", "firebrick")) + theme(legend.position = "none") + scale_y_continuous(limits = c(0,21), oob = rescale_none)
p2_eng <- ggplot(maakonnad[maakonnad$opilased_suh != 0,], aes(Maakond, opilased_suh, fill = highlight)) + geom_col() + theme_bw(base_size = 14) + labs(y = "Percentage of private school students", x = "", caption = "Data: EHIS, 2017/2018 school year") + 
  scale_fill_manual(values = c("gray50", "firebrick")) + theme(legend.position = "none") + scale_y_continuous(limits = c(0,21), oob = rescale_none)
grid.arrange(p1, p2, ncol = 2, top = textGrob("Maakonniti erineb erakoolid populaarsus. Harjumaa erakoolides käib 8% õpilasi, Lääne-Virumaal 7% õpilastest, 
kuigi erakoolide osakaal on tunduvalt väiksem.", gp = gpar(fontsize = 20),x = unit(0, "lines"), y = unit(0, "lines"), hjust = 0, vjust = 0))
grid.arrange(p1_eng, p2_eng, ncol = 2, top = textGrob("The popularity varies by county. In Harju county there were 8% of students in private schools, 
in Lääne-Viru county there were 7% of students although percentage of private schools was much lower.", gp = gpar(fontsize = 20),x = unit(0, "lines"), y = unit(0, "lines"), hjust = 0, vjust = 0))


#Kooli tüüp
tyyp <- andmed %>% filter(Aasta == 2017) %>% group_by(`Kooli tüüp`) %>% 
  summarise(koolid_arv = sum(erakool), koolid_suh = round(mean(erakool)*100,1), opilased_arv = sum(kokku[erakool == 1]), opilased_suh = round(sum(kokku[erakool == 1])/sum(kokku),3)*100)

#Kooli liik
liik <- andmed %>% filter(Aasta == 2017) %>% group_by(`Kooli liik`) %>% 
  summarise(koolid_arv = sum(erakool), koolid_suh = round(mean(erakool)*100,1), opilased_arv = sum(kokku[erakool == 1]), opilased_suh = round(sum(kokku[erakool == 1])/sum(kokku),3)*100)

#Õppekeel
abi <- kontakt %>% select(`Kooli ID` = `EHIS ID`,`Kooli õppekeeled`)
keel <- andmed %>% filter(Aasta == 2017) %>% left_join(abi) %>% group_by(`Kooli õppekeeled`) %>% 
  summarise(koolid_arv = sum(erakool), koolid_suh = round(mean(erakool)*100,1), opilased_arv = sum(kokku[erakool == 1]), opilased_suh = round(sum(kokku[erakool == 1])/sum(kokku),3)*100)
