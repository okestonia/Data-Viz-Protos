#Veeproovid
#Annegrete Peek

#paketid
library(data.table)
library(dplyr)
library(ggplot2)
setwd("C:/Users/annegrete.peek/Dropbox/Sparkline/Avaandmed/Veeproovid")


#Andmete sisselugemine ja huvitava segmendi filtreerimine #20.04.18 kuupäeva seis
andmed <- fread("basseini_veeproovid.csv")
andmed <- andmed %>% select(id, bassein_id, bassein, proovivotu_aeg, proovivotu_eesmark, koha_nimetus = nimetus, 
                            hinnang, naitaja_nimetus = nimetus4, sisaldus, yhik, naitaja_hinnang = hinnang5) %>% unique() %>% 
  mutate(proovivotu_aeg = as.Date(as.POSIXct(proovivotu_aeg, format = "%d.%m.%Y %H:%M", tz = Sys.timezone()))) %>% 
  filter(proovivotu_aeg >= as.Date("2016-01-01") & proovivotu_aeg <= as.Date("2017-06-14"))

#Üks rida proovi kohta
proovid <- andmed %>% group_by(id, bassein_id, bassein, proovivotu_aeg, proovivotu_eesmark, koha_nimetus, hinnang) %>% 
  summarise(naitaja_halb = sum(naitaja_hinnang == "ei vasta nõuetele"), naitjate_arv = n())

#Kirjeldav analüüs
table(proovid$hinnang)/7496 #Iga viies veeproov ei vasta nõuetele
length(unique(proovid$bassein)) #386 erinevat basseini, mille alla lähevad ka mullivannid
sort(table(proovid$proovivotu_eesmark), decreasing = TRUE)/7496 # 58% Tellimustöö, 24% Enesekontrolli, 14% Plaaniline järelevalve
table(proovid$naitjate_arv) #näitajate arv ühes proovis
table(proovid$naitaja_halb[proovid$naitaja_halb != 0])/1554 #halbade näitajate arv ühes proovis
table(andmed$naitaja_nimetus, andmed$naitaja_hinnang) #mis näitajad kõige enam ei vasta nõutule

#Üks rida iga basseini kohta
kohad <- proovid %>% group_by(bassein) %>% summarise(pass = sum(hinnang == "vastab nõuetele"), fail = sum(hinnang == "ei vasta nõuetele"), n = n(), fail_pr = round(fail/n,2))
ggplot(kohad, aes(n, fail_pr)) + geom_point() #kas on seos proovide arvu ja nõutele mittevastavate proovide osakaalu vahel?

proovid <- proovid %>% mutate(eesmark2 = case_when(proovivotu_eesmark== "Tellimustöö" ~ "Tellimustöö",
                                                   proovivotu_eesmark== "Enesekontroll" ~ "Enesekontroll",
                                                   proovivotu_eesmark== "Plaaniline järelevalve" ~ "Plaaniline järelevalve",
                                                   TRUE ~ "Muu"))
table(proovid$eesmark2, proovid$hinnang) #Kas erineva eesmärgiga proovidel on nõuetele mittevastavate proovidehulk erinev? Plaanilisel järelevalvel on nõuetele mittevastavaid rohkem.
proovid %>% group_by(eesmark2) %>% summarise(mean(naitjate_arv)) #Kas eesmärk mõjutab näitajate arvu? 
proovid %>% group_by(kuu = month(proovivotu_aeg), aasta = year(proovivotu_aeg)) %>% summarise(osakaal = sum(hinnang == "ei vasta nõuetele")/n()) #Kas andmed on sessoonsed? 

#Vaba kloor
vaba_kloor <- andmed %>% filter(naitaja_nimetus == "Vaba kloor") %>% mutate(sisaldus = as.numeric(sisaldus)) 
sum(vaba_kloor$sisaldus > 2)/nrow(vaba_kloor) #2%
sum(vaba_kloor$sisaldus > 1.5)/nrow(vaba_kloor) #5%
sum(vaba_kloor$sisaldus < 0.5)/nrow(vaba_kloor) #11%
sum(vaba_kloor$sisaldus > 0.5 & vaba_kloor$sisaldus < 1.5)/nrow(vaba_kloor) #75%

vaba_kloor2 <- vaba_kloor %>% mutate(sisaldus = ifelse(sisaldus < 0.1, 0, ifelse(sisaldus < 1.1, signif(sisaldus, 1), signif(sisaldus, 2)))) %>% 
  group_by(sisaldus) %>% summarise(arv = n()) %>% ungroup() %>% mutate(varv = ifelse(sisaldus < 0.5 | sisaldus > 1.5, 1, 0))
ggplot(vaba_kloor2, aes(sisaldus, arv, fill = as.character(varv))) + 
   geom_bar(stat = "identity", width = 0.1) +
  scale_fill_manual("legend", values = c("1" = "firebrick", "0" = "lightgrey")) + 
  xlim(-0.05,2.05) + 
  theme_bw(base_size = 18) + 
  labs(title = "Basseinivees on vaba kloori parajalt... või liiga vähe.", 
       subtitle = "Basseinivees peaks olema 0,5 - 1,5 mg/l vaba kloori. Enamasti mitte nõudele vastamine tähendab, et vees on liiga vähe vaba kloori.",
       x = "Vaba kloori hulk vees (mg/l)",
       y = "Veeproovide arv",
       caption = "Andmed: Terviseamet") +
   annotate("text", x = 0.25, y = 70, label = "Liiga vähe", size = 8) +
   annotate("text", x = 1, y = 70, label = "Parajalt", size = 8) +
   annotate("text", x = 1.75, y = 70, label = "Liiga palju", size = 8) +
  theme(axis.line = element_line(colour = "#808080"),
        axis.ticks = element_line(colour = "#808080"),
        axis.text = element_text(colour = "#808080"),
        axis.title = element_text(colour = "#808080"),
        plot.subtitle = element_text(colour = "#808080"),
        plot.caption = element_text(colour = "#808080"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position = "none")

#Seotud kloor
seotud_kloor <- andmed %>% filter(naitaja_nimetus %in% c("Seotud kloor (<31 °C)", "Seotud kloor (>31 °C)")) %>% 
  mutate(sisaldus = as.numeric(sisaldus))
sum(seotud_kloor$sisaldus > 1, na.rm = TRUE)/nrow(seotud_kloor) #2%
nrow(subset(seotud_kloor, (sisaldus > 0.5 & naitaja_nimetus == "Seotud kloor (>31 °C)") | (sisaldus > 0.4 & naitaja_nimetus == "Seotud kloor (<31 °C)")))/nrow(seotud_kloor) 
#20% proovidest sisaldab liiga suurt hulka seotud kloori.

dat_text <- data.frame(
  label = c("Parajalt", "Parajalt", "Liiga palju", "Liiga palju"),
  naitaja_nimetus = c("Seotud kloor (>31 °C)", "Seotud kloor (<31 °C)", "Seotud kloor (>31 °C)", "Seotud kloor (<31 °C)"),
  x = c(0.4, 0.3, 0.62, 0.52),
  y = c(40, 40, 40, 40)
)

ggplot(seotud_kloor, aes(sisaldus)) + 
  stat_bin(binwidth = 0.1, center = 0.05, fill = "lightgrey") +
  stat_bin(data = subset(seotud_kloor, (sisaldus > 0.5 & naitaja_nimetus == "Seotud kloor (>31 °C)") | (sisaldus > 0.4 & naitaja_nimetus == "Seotud kloor (<31 °C)")), binwidth = 0.1, center = 0.05, fill = "firebrick") +
  xlim(0,1) +
  theme_bw() + 
  theme_bw(base_size = 18) + 
  labs(title = "Vees on palju seotud kloori.", 
       subtitle = "Basseinis ei tohiks olla seotud kloori rohkem kui 0,4 mg/l (<31 °C) või 0,5 mg/l (>31 °C). Iga viies proov ületab piirmäära.",
       x = "Seotud kloori hulk vees (mg/l)",
       y = "Veeproovide arv",
       caption = "Andmed: Terviseamet") +
  geom_text(dat_text, mapping = aes(x = x, y = y, label = label), size = 8) +
  theme(axis.line = element_line(colour = "#808080"),
        axis.ticks = element_line(colour = "#808080"),
        axis.text = element_text(colour = "#808080"),
        axis.title = element_text(colour = "#808080"),
        plot.subtitle = element_text(colour = "#808080"),
        plot.caption = element_text(colour = "#808080"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(colour = "#808080")) +
  facet_grid(~naitaja_nimetus)


##Plots in English
ggplot(vaba_kloor2, aes(sisaldus, arv, fill = as.character(varv))) + 
  geom_bar(stat = "identity", width = 0.1) +
  scale_fill_manual("legend", values = c("1" = "firebrick", "0" = "lightgrey")) + 
  xlim(-0.05,2.05) + 
  theme_bw(base_size = 18) + 
  labs(title = "There is enough (or too little) free clorine in the water.", 
       subtitle = "The required amount of free chlorine in pool water is 0.5-1.5 mg/l. If this is not met, in general, there is too little chlorine.",
       x = "The amount of free chlorine (mg/l)",
       y = "Number of water samples",
       caption = "Data: Health Board") +
  annotate("text", x = 0.25, y = 70, label = "Too little", size = 8) +
  annotate("text", x = 1, y = 70, label = "Enough", size = 8) +
  annotate("text", x = 1.75, y = 70, label = "Too much", size = 8) +
  theme(axis.line = element_line(colour = "#808080"),
        axis.ticks = element_line(colour = "#808080"),
        axis.text = element_text(colour = "#808080"),
        axis.title = element_text(colour = "#808080"),
        plot.subtitle = element_text(colour = "#808080"),
        plot.caption = element_text(colour = "#808080"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position = "none")

#Seotud kloor
seotud_kloor2 <- seotud_kloor %>% mutate(naitaja_nimetus = gsub("Seotud kloor", "Bounded chlorine", naitaja_nimetus))
dat_text <- data.frame(
  label = c("Enough", "Enough", "Too much", "Too much"),
  naitaja_nimetus = c("Bounded chlorine (>31 °C)", "Bounded chlorine (<31 °C)", "Bounded chlorine (>31 °C)", "Bounded chlorine (<31 °C)"),
  x = c(0.4, 0.3, 0.62, 0.52),
  y = c(40, 40, 40, 40)
)

ggplot(seotud_kloor2, aes(sisaldus)) + 
  stat_bin(binwidth = 0.1, center = 0.05, fill = "lightgrey") +
  stat_bin(data = subset(seotud_kloor2, (sisaldus > 0.5 & naitaja_nimetus == "Bounded chlorine (>31 °C)") | (sisaldus > 0.4 & naitaja_nimetus == "Bounded chlorine (<31 °C)")), binwidth = 0.1, center = 0.05, fill = "firebrick") +
  xlim(0,1) +
  theme_bw() + 
  theme_bw(base_size = 18) + 
  labs(title = "There is too much bounded chlorine.", 
       subtitle = "There shouldn't be more than 0.4 mg/l (<31 °C) or 0.5 mg/l (>31 °C) bounded chlorine in the pool water. Every fifth sample exceeds the limit.",
       x = "The amount of free chlorine (mg/l)",
       y = "Number of water samples",
       caption = "Data: Health Board") +
  geom_text(dat_text, mapping = aes(x = x, y = y, label = label), size = 8) +
  theme(axis.line = element_line(colour = "#808080"),
        axis.ticks = element_line(colour = "#808080"),
        axis.text = element_text(colour = "#808080"),
        axis.title = element_text(colour = "#808080"),
        plot.subtitle = element_text(colour = "#808080"),
        plot.caption = element_text(colour = "#808080"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(colour = "#808080")) +
  facet_grid(~naitaja_nimetus)
