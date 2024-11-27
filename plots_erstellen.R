# Verkehsrsdaten 

library(tidyverse)
library(magick)

data_list <- lapply(2011:2024, function(jahr){
  data <- read_csv(paste0("https://ogdtg.ch/verkehrsdaten/total/",jahr,"_data.csv.gz"))
  
  
  data |> 
    group_by(code,name,strasse,datum) |> 
    summarise(anzahl = sum(anzahl))
  
})



data_list_df <- data_list |> bind_rows()


check <- data_list_df |> 
  group_by(code) |> 
  count()



verkehr_101 <- data_list_df |> 
  filter(code == "101") |> 
  mutate(christmas = case_when(
    day(datum) %in% c(24,25,26) & month(datum)==12 ~ "Weihnachtsfeiertage\n24.12-26.12",
    weekdays(datum) == "Sunday" ~ "Sonntage",
    TRUE ~ "Werktage"
  )) 



santa_pic <- image_read("santa.png")
image <- image_fill(santa_pic, 'none')
raster <- as.raster(image)

verkehr_xmas |> 
  ggplot(aes(x = fct_reorder(christmas,anzahl_mean),y = anzahl_mean)) +
  geom_bar(stat = "identity", fill = verkehr_xmas$color,color = "black")+
  annotation_raster(raster,xmin = 0.5,xmax = 1.5,ymin = 2000,ymax=6451,interpolate = TRUE)+
  theme_light()+
  labs(subtitle = "Durschschnittliche Anzahl gezählter Fahrzeuge pro Tag zwischen 2011 und 2023 auf der H 13 (Zählstelle 101, SCHLATT Kreuzung Paradies - Schaffhausen)",
       title = "Frohes Fest und freie Fahrt!",
       x= "",y = "Durschschnittliche Anzahl gezählter Fahrzeuge pro Tag") +
  theme(
    axis.text.x = element_text(size = 12, family = "Arial", color = "black"), # X-Achsen-Text
    text = element_text( family = "Arial", color = "black"),
    plot.title = element_text(face = "bold")
  ) 



# Voten im Grossen Rat

library(odsAPI)
voten <- get_dataset(dataset_id = "sk-stat-137")





voten_xmas <- voten |> 
  mutate(xmas_count = str_count(rede,"[w|W]eihnacht.*")) 



voten_xmas |> 
  mutate(week = month(datum,label = TRUE)) |> 
  group_by(week) |> 
  summarise(xmas = sum(xmas_count)) |> 
  ggplot(aes(week,xmas))+
  geom_bar(stat = "identity", fill = "#A73559",color = "black") +
  theme_light()+
  labs(subtitle ="Monatliche Gesamtanzahl des Wortes 'Weihnachten' in den Voten des Grossen Rats des Kantons Thurgau (2008–2024)",
       title = "Weihnachten kennt keinen Kalender im Grossen Rat des Kantons Thurgau",
       x= "",y = "Gesamtanzahl des Wortes 'Weihnachten' in den Voten des Grossen Rats") +
  theme(
    axis.text.x = element_text(size = 12, family = "Arial", color = "black"), # X-Achsen-Text
    text = element_text( family = "Arial", color = "black"),
    plot.title = element_text(face = "bold")
  ) +
  annotation_raster(raster,xmin = 11.2,xmax = 12.8,ymin = 17,ymax=22,interpolate = TRUE)