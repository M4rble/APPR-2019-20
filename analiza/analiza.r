# 4. faza: Analiza podatkov

logisticna <- function(x) { 100 / (1 + exp(-x)) }
logisticna_inv <- function(y) { log(y / (100 - y)) }
logisticna_breaks <- . %>% logisticna() %>% extended_breaks()() %>% logisticna_inv()

logisticna_trans <- trans_new("logistična funkcija", logisticna, logisticna_inv)



primerjava_rasti_log <- primerjava_rasti %>% mutate(delez=logisticna_inv(delez))

graf7 <- ggplot(primerjava_rasti_log %>% filter(drzava %in% c("Islandija", "Nizozemska", "Norveška", "Bolgarija", "Turčija", "Grčija")),
                aes(x=leto, y=delez, col=drzava)) +
  geom_point() + ggtitle("Primerjava rasti držav z največjo in najmanjšo rastjo") + 
  xlab("leto") + ylab("delež v %") + facet_grid(. ~ drzava) + stat_smooth(method="lm") +
  coord_trans(y=logisticna_trans) + scale_y_continuous(breaks=logisticna_breaks,
                                                       labels=logisticna) + 
  theme(axis.text.x=element_text(angle=45, vjust=0.5, hjust=1))


bolgarija = c('Bolgarija')
rasti_b <- primerjava_rasti_log %>% filter(drzava=="Bolgarija")
rasti_b$vrsta <- "podatki"
names(rasti_b)[names(rasti_b) == "delez"] <- "rast"
model7b <- lm(rast~leto, data=rasti_b)
pr <- predict(model7b, data.frame(leto=seq.int(2020, 2025, 1)))
napoved_rasti_b <- data.frame(leto = c(2020, 2021, 2022, 2023, 2024, 2025), rast = c(pr[1], pr[2], pr[3], pr[4], pr[5], pr[6]))
napoved_rasti_b$drzava <- bolgarija
napoved_rasti_b$vrsta <- "napoved"
skupne_rasti_b <- rbind(rasti_b, napoved_rasti_b)

turcija = c('Turčija')
rasti_t <- primerjava_rasti_log %>% filter(drzava=="Turčija")
rasti_t$vrsta <- "podatki"
names(rasti_t)[names(rasti_t) == "delez"] <- "rast"
model7t <- lm(rast~leto, data=rasti_t)
prt <- predict(model7t, data.frame(leto=seq.int(2020, 2025, 1)))
napoved_rasti_t <- data.frame(leto = c(2020, 2021, 2022, 2023, 2024, 2025), rast = c(prt[1], prt[2], prt[3], prt[4], prt[5], prt[6]))
napoved_rasti_t$drzava <- turcija
napoved_rasti_t$vrsta <- "napoved"
skupne_rasti_t <- rbind(rasti_t, napoved_rasti_t)

grcija = c('Grčija')
rasti_g <- primerjava_rasti_log %>% filter(drzava=="Grčija")
rasti_g$vrsta <- "podatki"
names(rasti_g)[names(rasti_g) == "delez"] <- "rast"
model7g <- lm(rast~leto, data=rasti_g)
prg <- predict(model7g, data.frame(leto=seq.int(2020, 2025, 1)))
napoved_rasti_g <- data.frame(leto = c(2020, 2021, 2022, 2023, 2024, 2025), rast = c(prg[1], prg[2], prg[3], prg[4], prg[5], prg[6]))
napoved_rasti_g$drzava <- grcija
napoved_rasti_g$vrsta <- "napoved"
skupne_rasti_g <- rbind(rasti_g, napoved_rasti_g)

napoved_rasti <- rbind(skupne_rasti_b, skupne_rasti_g, skupne_rasti_t)

graf7a <- ggplot(napoved_rasti, aes(x=leto, y=rast, col=drzava, shape=vrsta)) +
  geom_point() + ggtitle("Napoved rasti držav z največjo rastjo") + 
  xlab("leto") + ylab("delež v %") + facet_grid(. ~ drzava) + stat_smooth(method="lm") +
  coord_trans(y=logisticna_trans) + scale_y_continuous(breaks=logisticna_breaks,
                                                       labels=logisticna)

################################################################################################

primerjava_rasti_komunikacije_log <- primerjava_rasti_komunikacije %>% mutate(delez=logisticna_inv(delez))

graf8 <- ggplot(primerjava_rasti_komunikacije_log %>% filter(drzava %in% c("Latvija", "Estonija", "Danska", "Bolgarija", "Italija")),
                aes(x=leto, y=delez, col=drzava)) +
  geom_point() + ggtitle("Primerjava rasti držav z največjo in najmanjšo stopnjo komunikacije") + 
  xlab("leto") + ylab("delež v %") + facet_grid(. ~ drzava) + stat_smooth(method="lm") +
  coord_trans(y=logisticna_trans) + scale_y_continuous(breaks=logisticna_breaks,
                                                       labels=logisticna) + 
  theme(axis.text.x=element_text(angle=45, vjust=0.5, hjust=1))

komunikacija_b <- primerjava_rasti_komunikacije_log %>% filter(drzava=="Bolgarija")
komunikacija_b$vrsta <- "podatki"
names(komunikacija_b)[names(komunikacija_b) == "delez"] <- "rast"
model8b <- lm(rast~leto, data=komunikacija_b)
prb <- predict(model8b, data.frame(leto=seq.int(2020, 2025, 1)))
napoved_komunikacije_b <- data.frame(leto = c(2020, 2021, 2022, 2023, 2024, 2025), rast = c(prb[1], prb[2], prb[3], prb[4], prb[5], prb[6]))
napoved_komunikacije_b$drzava <- bolgarija
napoved_komunikacije_b$vrsta <- "napoved"
skupna_komunikacija_b <- rbind(komunikacija_b, napoved_komunikacije_b)

danska <- c('Danska')
komunikacija_d <- primerjava_rasti_komunikacije_log %>% filter(drzava=="Danska")
komunikacija_d$vrsta <- "podatki"
names(komunikacija_d)[names(komunikacija_d) == "delez"] <- "rast"
model8d <- lm(rast~leto, data=komunikacija_d)
prd <- predict(model8d, data.frame(leto=seq.int(2020, 2025, 1)))
napoved_komunikacije_d <- data.frame(leto = c(2020, 2021, 2022, 2023, 2024, 2025), rast = c(prd[1], prd[2], prd[3], prd[4], prd[5], prd[6]))
napoved_komunikacije_d$drzava <- danska
napoved_komunikacije_d$vrsta <- "napoved"
skupna_komunikacija_d <- rbind(komunikacija_d, napoved_komunikacije_d)

estonija <- c('Estonija')
komunikacija_e <- primerjava_rasti_komunikacije_log %>% filter(drzava=="Estonija")
komunikacija_e$vrsta <- "podatki"
names(komunikacija_e)[names(komunikacija_e) == "delez"] <- "rast"
model8e <- lm(rast~leto, data=komunikacija_e)
pre <- predict(model8e, data.frame(leto=seq.int(2020, 2025, 1)))
napoved_komunikacije_e <- data.frame(leto = c(2020, 2021, 2022, 2023, 2024, 2025), rast = c(pre[1], pre[2], pre[3], pre[4], pre[5], pre[6]))
napoved_komunikacije_e$drzava <- estonija
napoved_komunikacije_e$vrsta <- "napoved"
skupna_komunikacija_e <- rbind(komunikacija_e, napoved_komunikacije_e)

italija <- c('Italija')
komunikacija_i <- primerjava_rasti_komunikacije_log %>% filter(drzava=="Italija")
komunikacija_i$vrsta <- "podatki"
names(komunikacija_i)[names(komunikacija_i) == "delez"] <- "rast"
model8i <- lm(rast~leto, data=komunikacija_i)
pri <- predict(model8i, data.frame(leto=seq.int(2020, 2025, 1)))
napoved_komunikacije_i <- data.frame(leto = c(2020, 2021, 2022, 2023, 2024, 2025), rast = c(pri[1], pri[2], pri[3], pri[4], pri[5], pri[6]))
napoved_komunikacije_i$drzava <- italija
napoved_komunikacije_i$vrsta <- "napoved"
skupna_komunikacija_i <- rbind(komunikacija_i, napoved_komunikacije_i)

latvija <- c('Latvija')
komunikacija_l <- primerjava_rasti_komunikacije_log %>% filter(drzava=="Latvija")
komunikacija_l$vrsta <- "podatki"
names(komunikacija_l)[names(komunikacija_l) == "delez"] <- "rast"
model8l <- lm(rast~leto, data=komunikacija_l)
prl <- predict(model8l, data.frame(leto=seq.int(2020, 2025, 1)))
napoved_komunikacije_l <- data.frame(leto = c(2020, 2021, 2022, 2023, 2024, 2025), rast = c(prl[1], prl[2], prl[3], prl[4], prl[5], prl[6]))
napoved_komunikacije_l$drzava <- latvija
napoved_komunikacije_l$vrsta <- "napoved"
skupna_komunikacija_l <- rbind(komunikacija_l, napoved_komunikacije_l)

napoved_komunikacije <- rbind(skupna_komunikacija_b, skupna_komunikacija_d, skupna_komunikacija_e, 
                              skupna_komunikacija_i, skupna_komunikacija_l)

graf8a <- ggplot(napoved_komunikacije, aes(x=leto, y=rast, col=drzava, shape=vrsta)) +
  geom_point() + ggtitle("Napoved komunikacije državljanov z oblastmi") + 
  xlab("leto") + ylab("delež v %") + facet_grid(. ~ drzava) + stat_smooth(method="lm") +
  coord_trans(y=logisticna_trans) + scale_y_continuous(breaks=logisticna_breaks,
                                                       labels=logisticna) +
  theme(axis.text.x=element_text(angle=45, vjust=0.5, hjust=1))
