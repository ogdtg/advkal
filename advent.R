library(ggplot2)

# Farben & Theme
gruen <- "#1F4D3A"; rot <- "#B3261E"; gold <- "#C9A227"; creme <- "#FBF7EF"; grau <- "#5E5E5E"
th <- theme_minimal(base_size = 30) +
  theme(plot.background = element_rect(fill = creme, colour = NA),
        panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        plot.title = element_text(face = "bold", colour = gruen, size = 44, margin = margin(b = 10)),
        plot.subtitle = element_text(colour = grau, size = 26, margin = margin(b = 30)),
        plot.caption = element_text(colour = grau, size = 18, hjust = 0, margin = margin(t = 25)),
        axis.title = element_blank(), axis.text = element_text(colour = grau),
        plot.margin = margin(50, 50, 40, 50),
        plot.title.position = "plot", plot.caption.position = "plot")
cap <- function(ds) paste0("Quelle: data.tg.ch – ", ds)
save <- function(p, f) ggsave(f, p, width = 10.8, height = 10.8, dpi = 100)

# 1 Christbäume nach Gemeinde 2025 (Fläche in Aren -> ha)
cb <- data.frame(g = c("Egnach","Hefenhofen","Berg (TG)","Amriswil","Kemmental","Bettwiesen","Kesswil","Salmsach"),
                 a = c(1612.24,1382.05,1260.81,1171.9,776.0,597.88,568.9,357.77))
cb$ha <- cb$a / 100
p1 <- ggplot(cb, aes(reorder(g, ha), ha)) +
  geom_col(fill = gruen, width = .7) +
  geom_text(aes(label = format(round(ha, 1), nsmall = 1, decimal.mark = ",")), hjust = -.2, size = 9, colour = gruen, fontface = "bold") +
  coord_flip(clip = "off") + scale_y_continuous(expand = expansion(c(0, .15))) +
  labs(title = "114,6 ha Christbäume im Thurgau",
       subtitle = "Christbaumkulturen 2025 in Hektaren – Top 8 Gemeinden\n(Kanton total: 104,6 ha 2023 → 114,6 ha 2025)",
       caption = cap("Landwirtschaftliche Kulturen und Bäume (div-la-3)")) +
  th + theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour = "#E6DFD0"))
save(p1, "01_christbaeume.png")

# 2 Festtagsbraten: Truten vs. Gänse, Index 2015 = 100
j <- 2015:2025
ga <- c(533,670,625,625,522,527,441,423,414,422,363)
tr <- c(9031,10350,14420,14420,12871,14175,14975,15093,15107,15122,15154)
d2 <- rbind(data.frame(j, v = tr/tr[1]*100, t = "Truten (Ausmast)"),
            data.frame(j, v = ga/ga[1]*100, t = "Gänse"))
p2 <- ggplot(d2, aes(j, v, colour = t)) +
  geom_hline(yintercept = 100, colour = "#CFC6B4") +
  geom_line(linewidth = 2.5) + geom_point(size = 4) +
  annotate("text", 2025.2, 168, label = "Truten\n+68 %", hjust = 0, colour = rot, size = 10, fontface = "bold", lineheight = .9) +
  annotate("text", 2025.2, 68, label = "Gänse\n−32 %", hjust = 0, colour = gold, size = 10, fontface = "bold", lineheight = .9) +
  scale_colour_manual(values = c("Truten (Ausmast)" = rot, "Gänse" = gold), guide = "none") +
  scale_x_continuous(breaks = c(2015, 2020, 2025), expand = expansion(c(.02, .2))) +
  coord_cartesian(clip = "off") +
  labs(title = "Festtagsbraten im Wandel",
       subtitle = "Tierbestände auf Thurgauer Landwirtschaftsbetrieben\nIndex 2015 = 100",
       caption = cap("Landwirtschaftliche Tierbestände (div-la-2)")) +
  th + theme(panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(colour = "#E6DFD0"))
save(p2, "02_festtagsbraten.png")

# 3 Weisse Weihnachten Frauenfeld (Schneehöhe 25.12.)
y <- c(1963:1980, 2011:2025)
s <- c(1,3,0,0,0,5,0,0,0,0,0,0,1,3,0,0,6,0, rep(0, 15))
d3 <- data.frame(y, s, per = ifelse(y < 2000, "1963–1980", "2011–2025"), weiss = s >= 1)
p3 <- ggplot(d3, aes(factor(y), 1, fill = weiss)) +
  geom_point(shape = 21, size = 15, colour = "#9DB3C7", stroke = 1.2) +
  geom_text(data = subset(d3, weiss), aes(label = s), vjust = 3.6, size = 7, colour = "#3A5A78") +
  facet_wrap(~per, ncol = 1, scales = "free_x") +
  scale_fill_manual(values = c(`TRUE` = "white", `FALSE` = "#DCE4EA"), guide = "none") +
  scale_x_discrete(labels = function(x) substr(x, 3, 4)) +
  labs(title = "Weisse Weihnachten?",
       subtitle = "Frauenfeld am 25. Dezember (weiss = Schnee, Zahl = cm)\n1963–80: 7 von 18 Jahren  |  2011–25: 0 von 15",
       caption = paste0(cap("MeteoSchweiz Frauenfeld (meteoschweiz-ogd-7)"), "\nLücke 1981–2010: keine Schneehöhenwerte im Datensatz")) +
  th + theme(axis.text.y = element_blank(), panel.grid = element_blank(),
             strip.text = element_text(size = 26, face = "bold", colour = gruen, hjust = 0),
             axis.text.x = element_text(size = 20))
save(p3, "03_weisse_weihnachten.png")

# 4 Marroni & Nüsse
d4 <- data.frame(j = factor(rep(2023:2025, 2)),
                 n = c(532, 662, 1012, 10174, 10386, 10625),
                 t = rep(c("Edelkastanien", "Nussbäume"), each = 3))
p4 <- ggplot(d4, aes(j, n, fill = j)) +
  geom_col(width = .65) +
  geom_text(aes(label = format(n, big.mark = "'")), vjust = -.5, size = 7.5, fontface = "bold", colour = gruen) +
  facet_wrap(~t, scales = "free_y") +
  scale_fill_manual(values = c("#C8B89A", "#9C7A4B", "#6B4A2B"), guide = "none") +
  scale_y_continuous(expand = expansion(c(0, .15)), labels = function(x) format(x, big.mark = "'")) +
  labs(title = "Heissi Marroni aus dem Thurgau?",
       subtitle = "Anzahl Bäume auf der landwirtschaftlichen Nutzfläche\nEdelkastanien fast verdoppelt (+90 % seit 2023)",
       caption = cap("Landwirtschaftliche Kulturen und Bäume (div-la-3)")) +
  th + theme(strip.text = element_text(size = 24, face = "bold", colour = gruen),
             panel.grid.major.y = element_line(colour = "#E6DFD0"))
save(p4, "04_marroni_nuesse.png")

# 5 Wer zieht den Schlitten? (2025)
d5 <- data.frame(t = c("Pferde, Ponys & Esel", "Damhirsche", "Alpakas", "Lamas", "Rentiere"),
                 n = c(5632, 584, 463, 86, 0))
p5 <- ggplot(d5, aes(reorder(t, n), n)) +
  geom_col(fill = c(gruen, rot, gold, gold, "#CFC6B4"), width = .7) +
  geom_text(aes(label = ifelse(n == 0, "keine erfasst", format(n, big.mark = "'"))), hjust = -.2, size = 10, fontface = "bold", colour = gruen) +
  coord_flip(clip = "off") + scale_y_continuous(expand = expansion(c(0, .2))) +
  labs(title = "Wer zieht den Schlitten?",
       subtitle = "Durchschnittsbestand 2025 auf Thurgauer\nLandwirtschaftsbetrieben",
       caption = cap("Landwirtschaftliche Tierbestände (div-la-2)")) +
  th + theme(axis.text.x = element_blank(), panel.grid = element_blank())
save(p5, "05_schlitten.png")

# 6 Bienenvölker (Bienenwachs-Kerzen)
bi <- c(8221,8391,8854,8854,8848,8995,9037,8918,8668,8392,8284)
d6 <- data.frame(j, bi)
p6 <- ggplot(d6, aes(j, bi)) +
  geom_line(colour = gold, linewidth = 2.5) + geom_point(colour = "#8A6D10", size = 4) +
  annotate("text", 2021, 9037, label = "Höchststand 2021\n9'037", vjust = -.4, size = 8, colour = "#8A6D10", fontface = "bold", lineheight = .9) +
  scale_x_continuous(breaks = c(2015, 2020, 2025)) +
  scale_y_continuous(labels = function(x) format(x, big.mark = "'")) +
  coord_cartesian(ylim = c(7000, 9600)) +
  labs(title = "8'284 Bienenvölker",
       subtitle = "…liefern Honig für die Guetzli und Wachs für die Kerzen.\nBienenvölker auf Thurgauer Landwirtschaftsbetrieben",
       caption = cap("Landwirtschaftliche Tierbestände (div-la-2)")) +
  th + theme(panel.grid.major.y = element_line(colour = "#E6DFD0"))
save(p6, "06_bienen.png")
