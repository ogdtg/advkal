source_lines <- readLines("advent.R")
eval(parse(text = source_lines[1:16]))   # Farben, Theme, cap(), save()
library(ggplot2)

# 7 Weihnachtsgeschichte im Siedlungsverzeichnis 2026
d7 <- data.frame(
  s = c("Himmelrich (Horn)", "Neuparadies (Schlatt)", "Engelswilen (Kemmental)", "Altparadies (Schlatt)",
        "Hirten (Homburg)", "Bethlehem (Homburg)", "Himmelreich (Weinfelden)", "Himmelrich (Tobel-Tägerschen)"),
  n = c(319, 198, 98, 45, 19, 18, 6, 6))
d7$hl <- grepl("Homburg", d7$s)
p7 <- ggplot(d7, aes(reorder(s, n), n, fill = hl)) +
  geom_col(width = .7) +
  geom_text(aes(label = n), hjust = -.25, size = 9, fontface = "bold", colour = gruen) +
  scale_fill_manual(values = c(`TRUE` = rot, `FALSE` = gruen), guide = "none") +
  coord_flip(clip = "off") + scale_y_continuous(expand = expansion(c(0, .15))) +
  labs(title = "Bethlehem liegt im Thurgau",
       subtitle = "Weihnachtliche Siedlungsnamen und Einwohner\n(31.12.2025) – Bethlehem und Hirten\nliegen beide in Homburg",
       caption = cap("Siedlungsverzeichnis des Kantons Thurgau 2026 (sk-stat-162)")) +
  th + theme(axis.text.x = element_blank(), panel.grid = element_blank(), axis.text.y = element_text(size = 24))
save(p7, "07_bethlehem.png")

# 8 Wintersportvereine
d8 <- data.frame(k = c("Eishockey", "Ski Alpin", "Langlauf", "Eiskunstlauf", "Curling", "Eisstock", "Bob"),
                 n = c(15, 7, 3, 2, 1, 1, 1))
p8 <- ggplot(d8, aes(reorder(k, n), n)) +
  geom_col(fill = "#3A5A78", width = .7) +
  geom_text(aes(label = n), hjust = -.4, size = 10, fontface = "bold", colour = "#3A5A78") +
  coord_flip(clip = "off") + scale_y_continuous(expand = expansion(c(0, .12))) +
  labs(title = "Wintersport im Flachland",
       subtitle = "30 von 729 Thurgauer Sportvereinen sind\nWintersportclubs – inklusive Bobclub Frauenfeld",
       caption = cap("Übersicht der Thurgauer Sportvereine (dek-spa-4)")) +
  th + theme(axis.text.x = element_blank(), panel.grid = element_blank())
save(p8, "08_wintersport.png")

# 9 Schätze im Boden
d9 <- data.frame(g = c("Eschenz", "Pfyn", "Schlatt", "Aadorf", "Wilen", "Arbon", "Felben-Wellhausen", "Hüttlingen"),
                 n = c(22, 9, 5, 3, 3, 2, 2, 2))
p9 <- ggplot(d9, aes(reorder(g, n), n)) +
  geom_col(fill = gold, width = .7) +
  geom_text(aes(label = n), hjust = -.4, size = 10, fontface = "bold", colour = "#8A6D10") +
  coord_flip(clip = "off") + scale_y_continuous(expand = expansion(c(0, .12))) +
  labs(title = "Schätze im Thurgauer Boden",
       subtitle = "66 archäologische Fundmeldungen mit Münzen\noder Schmuck – Top 8 Gemeinden",
       caption = cap("Archäologische Fundmeldungen ab 1988 (dek-aa-1)")) +
  th + theme(axis.text.x = element_blank(), panel.grid = element_blank())
save(p9, "09_schaetze.png")
