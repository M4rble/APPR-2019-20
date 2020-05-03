# 4. faza: Analiza podatkov

#uvoz tabel in grafov, iz obdelave podatkov

source("vizualizacija/vizualizacija.r", encoding="UTF-8")

primerjava_najvecje_razlike_rasti <- primerjava_rasti %>% 
                                     filter(drzava %in% c("Islandija", "Turčija"))

graf7 <- ggplot(primerjava_najvecje_razlike_rasti) + aes(x=leto, y=delez, col=drzava)  +
  geom_point() + ggtitle("Primerjava rasti držav z največjo in najmanjšo rastjo") + 
  xlab("leto") + ylab("delež v %") + facet_grid(. ~ drzava) + stat_smooth(method = "lm")

primerjava_rasti_tur <- primerjava_rasti %>% filter(drzava == "Turčija")
model7 <- lm(delez~leto, data=primerjava_rasti_tur)
pr <- predict(model7, data.frame(leto=seq.int(2020, 2025, 1)))
napoved_rasti <- data.frame(leto = c(2020, 2021, 2022, 2023, 2024, 2025), rast = c(pr[1], pr[2], pr[3], pr[4], pr[5], pr[6]))

primerjava_rasti_isl <- primerjava_rasti %>% filter(drzava == "Islandija")
model7i <- lm(log(delez)~leto, data=primerjava_rasti_isl)
pri <- predict(model7, data.frame(leto=seq.int(2020, 2025, 1)))
napoved_rasti_i <- data.frame(leto = c(2020, 2021, 2022, 2023, 2024, 2025), rast = c(pri[1], pri[2], pri[3], pri[4], pri[5], pri[6]))

primerjava_rasti_bol <- primerjava_rasti %>% filter(drzava == "Bolgarija")
model7b <- lm(delez~leto, data=primerjava_rasti_bol)
prb <- predict(model7b, data.frame(leto=seq.int(2020, 2025, 1)))
napoved_rasti_b <- data.frame(leto = c(2020, 2021, 2022, 2023, 2024, 2025), rast = c(prb[1], prb[2], prb[3], prb[4], prb[5], prb[6]))





















#podatki <- obcine %>% transmute(obcina, povrsina, gostota,
#                                gostota.naselij=naselja/povrsina) %>%
#  left_join(povprecja, by="obcina")
#row.names(podatki) <- podatki$obcina
#podatki$obcina <- NULL

# Število skupin
#n <- 5
#skupine <- hclust(dist(scale(podatki))) %>% cutree(n)
