#Maksuvõlgnike analüüs
#Autor Annegrete Peek

#paketid
library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)

#andmed
nov <- read_excel("maksuvolglaste_nimekiri_nov.xlsx", skip = 2) %>% mutate(pankrot = ifelse(grepl("PANKROTIS", Nimi), 1, 0))
dets <- read_excel("maksuvolglaste_nimekiri_dets.xlsx", skip = 2) %>% mutate(pankrot = ifelse(grepl("PANKROTIS", Nimi), 1, 0))
volg <- nov %>% full_join(dets, by = "Kood", suffix = c("_nov", "_dets")) %>% mutate(summa_muutus = Maksuvõlg_dets/Maksuvõlg_nov)
sum(is.na(volg$Nimi_nov)) #392 ettevõtet, keda novembris ei olnud
sum(is.na(volg$Nimi_dets)) #499 ettevõtet, keda detsembris ei olnud

#Kontrollime, kas nimed ja alguskuupäevad klapivad
table(volg$Nimi_nov == volg$Nimi_dets, useNA = "ifany") # 14 juhul nimed ei ole samad
volg %>% filter(Nimi_nov != Nimi_dets) %>% View() # 5 juhul on pankrotti sattunud, 1 on nüüd likvideerimisel, 8 on nime muutnud.
sum(volg$summa_muutus < 1, na.rm = TRUE) # 579 vähenes
sum(volg$summa_muutus == 1, na.rm = TRUE) # 2810 jäi samaks
sum(volg$summa_muutus > 1, na.rm = TRUE)


#Top 10,20,50,100 võlga dets
dets <- dets %>% 
  arrange(-Maksuvõlg)
temp <- dets %>%
  summarise(
    top10 = sum(head(Maksuvõlg,10)), 
    top20 = sum(head(Maksuvõlg,20)), 
    top50 = sum(head(Maksuvõlg,50)), 
    top100 = sum(head(Maksuvõlg,100))) %>% 
  reshape2::melt() %>% 
  mutate(grupp = "Kõik",
         grupp2 = "All")
sum(dets$Maksuvõlg) # 170508281
sum(dets$`Sellest vaidlustatud`) #4469157
sum(dets$pankrot) #276
sum(dets$Maksuvõlg[dets$pankrot == 1]) #31553909
table(year(head(dets$`Maksuvõla algus`, 100)))
table(year(head(dets$`Maksuvõla algus`, 100)) <= 2016) #86, 14

top <- dets %>% 
  filter(pankrot == 0) %>% 
  summarise(
    top10 = sum(head(Maksuvõlg,10)), 
    top20 = sum(head(Maksuvõlg,20)), 
    top50 = sum(head(Maksuvõlg,50)), 
    top100 = sum(head(Maksuvõlg,100))) %>% 
  reshape2::melt() %>% 
  mutate(grupp = "Pankrotita",
         grupp2 = "Without bankruptcy") %>% 
  rbind(temp)
dets %>% filter(pankrot == 0) %>% summarise(arv = sum(Maksuvõlg))

ggplot(top, aes(x = variable, y = value/1000000, fill = grupp)) + 
  geom_bar(position = "dodge", stat = "identity") +
  theme_bw(base_size = 18) + 
  scale_x_discrete(labels = c("10", "20", "50", "100")) +
  scale_fill_manual(values = c("#49667F", "#EEEEEE")) +
  labs(title = "10 suurimat maksuvõlgnikut on riigile võlgu rohkem kui 42 miljonit eurot.", 
       subtitle = "10 ja 100 suurimat võlga moodustavad vastavalt 25% ja 49% kajastatud võlast.",
       y = "Maksuvõlg (miljonites eurodes)",
       x = "Suurimat maksuvõlga",
       caption = "Andmed: Maksuamet") +
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
        legend.position = "bottom", 
        legend.title = element_blank(),
        legend.text = element_text(colour = "#808080"))

ggplot(top, aes(x = variable, y = value/1000000, fill = grupp2)) + 
  geom_bar(position = "dodge", stat = "identity") +
  theme_bw(base_size = 18) + 
  scale_x_discrete(labels = c("10", "20", "50", "100")) +
  scale_fill_manual(values = c("#49667F", "#EEEEEE")) +
  labs(title = "The top 10 tax debtors owe the government more than 42 million euros.", 
       subtitle = "The 10 and 100 largest debts represent 25% and 49% of the total debt, respectively.",
       y = "Tax Debt (EUR million)",
       x = "Largest debts",
       caption = "Data: Estonian Tax and Customs Board") +
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
        legend.position = "bottom", 
        legend.title = element_blank(),
        legend.text = element_text(colour = "#808080"))

#Kes kadusid ära ja kes tulid juurde? 
df1 <- dets %>% filter(!(Kood %in% nov$Kood)) %>% mutate(grupp = "Uued", grupp2 = "Newcomers")
df2 <- nov %>% filter(!(Kood %in% dets$Kood)) %>% mutate(grupp = "Kadunud", grupp2 = "Disappeared")
sum(df2$Maksuvõlg) #8054269
sum(df2$pankrot) #5
table(year(df2$`Maksuvõla algus`))
sum(df1$Maksuvõlg) #4699554
sum(df1$pankrot) #1
table(year(df1$`Maksuvõla algus`))

df <- df2 %>% rbind(df1)
ggplot(df, aes(Maksuvõlg, fill = grupp)) + 
  geom_density(alpha = 0.7) + 
  xlim(c(0, 100000)) +
  theme_bw(base_size = 18) + 
  scale_fill_manual(values = c("#808080", "#49667F")) +
  labs(title = "Uute ja kadunud võlgnike jaotused on väga sarnased.", 
       subtitle = "13 kadunud ja 4 uut võlga ei mahtunud joonisele, sest need on suuremad kui 100 000€.",
       y = "",
       x = "Maksuvõlg",
       caption = "Andmed: Maksuamet") +
  theme(axis.line = element_line(colour = "#808080"),
        axis.ticks = element_line(colour = "#808080"),
        axis.text = element_text(colour = "#808080"),
        axis.title = element_text(colour = "#808080"),
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.subtitle = element_text(colour = "#808080"),
        plot.caption = element_text(colour = "#808080"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom", 
        legend.title = element_blank(),
        legend.text = element_text(colour = "#808080"))

ggplot(df, aes(Maksuvõlg, fill = grupp2)) + 
  geom_density(alpha = 0.7) + 
  xlim(c(0, 100000)) +
  theme_bw(base_size = 18) + 
  scale_fill_manual(values = c("#808080", "#49667F")) +
  labs(title = "The distributions of newcomers and disappeared debtors are very similar.", 
       subtitle = "13 disappeared and 4 new debts are over 100 000€ and didn't fit into the graph.",
       y = "",
       x = "Tax Debt",
       caption = "Data: Estonian Tax and Customs Board") +
  theme(axis.line = element_line(colour = "#808080"),
        axis.ticks = element_line(colour = "#808080"),
        axis.text = element_text(colour = "#808080"),
        axis.title = element_text(colour = "#808080"),
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.subtitle = element_text(colour = "#808080"),
        plot.caption = element_text(colour = "#808080"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom", 
        legend.title = element_blank(),
        legend.text = element_text(colour = "#808080"))
