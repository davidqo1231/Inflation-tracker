library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(ggiraph)
library(htmlwidgets)
library(gt)
library(gtExtras)
library(purrr)
library(svglite)

# WRANGLING
Zoznam <- read.csv("https://raw.githubusercontent.com/davidqo1231/Inflation-tracker/main/cpiweights.csv", sep = ";") %>%
  mutate(Vaha = Vaha/10)

#Posledný mesiac v aktuálnom roku
last_month = as.Date("2022-12-01")

df_yoy <- read.csv("https://data.statistics.sk/api/v2/dataset/sp0029ms/all/1.,2.,3.,4.,5.,6.,7.,8.,9.,10.,11.,12./all/mj_romr?lang=sk&type=csv", sep = ";", header = FALSE) %>%
  slice(7:nrow(.)) %>%
  rename(Rok = 1, Mesiac = 2, Kod = 3, Jednotka = 4, YoY = 5) %>%
  mutate(Mesiac = as.numeric(Mesiac),
         YoY = as.numeric(YoY)) %>%
  arrange(Rok, Mesiac) %>%
  group_by(Kod) %>%
  mutate(Datum = as.Date(seq(ymd("2018-01-01"), ymd(last_month), by = "months"))) %>%
  ungroup() %>%
  mutate(YoY = YoY-100) %>%
  select(Datum, Kod, YoY) %>%
  drop_na()

df_mom <- read.csv("https://data.statistics.sk/api/v2/dataset/sp0029ms/all/1.,2.,3.,4.,5.,6.,7.,8.,9.,10.,11.,12./all/mj_predch_obdobie?lang=sk&type=csv", sep = ";", header = FALSE) %>%
  slice(7:nrow(.)) %>%
  rename(Rok = 1, Mesiac = 2, Kod = 3, Jednotka = 4, MoM = 5) %>%
  mutate(Mesiac = as.numeric(Mesiac),
         MoM = as.numeric(MoM)) %>%
  arrange(Rok, Mesiac) %>%
  group_by(Kod) %>%
  mutate(Datum = as.Date(seq(ymd("2018-01-01"), ymd(last_month), by = "months"))) %>%
  ungroup() %>%
  mutate(MoM = MoM-100) %>%
  select(Datum, Kod, MoM) %>%
  drop_na()

last_obs <- df_yoy$Datum[nrow(df_yoy)] 

df_last <- df_yoy %>%
  left_join(df_mom, by = c("Kod", "Datum")) %>%
  filter(Datum == last_obs) %>%
  mutate(YoY = round(YoY, digits = 1),
         MoM = round(MoM, digits = 1))


df_table <- df_yoy %>%
  left_join(df_mom, by = c("Kod", "Datum")) %>%
  # Zoberie len posledných 12 mesiacov od posledného pozorovania
  filter(Datum > last_obs %m-% months(12)) %>%
  mutate(YoY = round(YoY, digits = 1),
         MoM = round(MoM, digits = 1)) %>%
  group_by(Kod) %>%
  summarize(YoY_data = list(YoY),
            MoM_data = list(MoM), .groups = "drop")


df_final <- Zoznam %>%
  left_join(df_last, by = "Kod") %>%
  left_join(df_table, by = "Kod")

rm("df_last", "df_mom", "df_table", "df_yoy", "Zoznam", "last_month", "last_obs")


#
df <- df_final %>%
  filter(YoY > -20) %>%
  arrange(YoY)

df$right <- cumsum(df$Vaha) + 0*c(0:(nrow(df)-1))
df$left <- df$right - df$Vaha + 0.1

x.axis.labels <- seq(10, 100, 10) # positions of the subtle ticks
y.axis.labels <- seq(0,50,10)


make_table <- function(name) {
  df %>%
    #filter by name
    filter(Kod == name) %>%
    select(YoY, MoM, YoY_data) %>%
    gt() %>%
    cols_width(YoY ~ px(80),
                   MoM ~ px(90),
                   YoY_data ~ px(160),) %>%
    gt_plt_sparkline(YoY_data, 
                               type = "shaded", 
                               fig_dim = c(8, 45),
                               palette = c("white", "white", "green", "red", "#7FC8E8")) %>%
    cols_align(
      align = "center",
      columns = c("YoY", "MoM", "YoY_data")) %>%
    cols_label(
      YoY = "YoY (v %)",
      MoM = "MoM (v %)",
      YoY_data = "Dynamika (YoY v %)") %>%
    tab_footnote(
      footnote = "YoY - zmena oproti rovn. mesiacu minulého roka\n MoM - zmena oproti minulému mesiacu",
      locations = NULL,
      placement = c("right")) %>%
    opt_table_outline(style = "none") %>%
    tab_options(table.background.color = "black",
                table.font.color = "white") %>%
    #gtExtras::gt_theme_nytimes() %>%
    as_raw_html()  
  
}

df_table <- df %>%
  mutate(table = map(Kod, make_table)) %>%
  select(Popis, Kod, YoY, MoM, table) %>%
  mutate(Popis = gsub("(\\S* \\S* \\S* \\S* \\S*)","\\1<br>", Popis)) %>%
  mutate(table_full = paste("<center><b>",Popis,"</b></center>",table)) 
  

    

# PLOTTING

plot <- ggplot(df, aes(ymin = 0)) + 
  geom_rect_interactive(aes(xmin = left, xmax = right, ymax = YoY, tooltip = df_table$table_full, data_id = Kod, fill = Highlight)) +
  scale_fill_manual(values = c("Potraviny" = "#2EAAE1", "Energie a paliva" = "#7FC8E8", "Ostatne"= "#C8E7F5"), guide = FALSE ) +
  
  xlab("Podiel na výdavkoch domácností (kumulatívne v %)") + 
  ylab("") +
  theme_minimal() +
  theme(legend.position = "top",
        panel.background = element_rect(fill = 'white', colour = 'white'),
        plot.background = element_rect(fill = "white", colour = "white"),
        panel.grid.major.y = element_line(size = 0.2, colour = "#DCDCDC"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.x = element_text(vjust=5, size = 10, face = "bold", color = "#333333"),
        axis.text.y = element_blank(),
        text=element_text(family = "sans", size = 14, color = "#28231D"),
        plot.title = element_text(size=14, color="#333333", family = "sans", face = "bold"),
        plot.subtitle = element_text(size=10, color="#333333", family = "sans"),
        plot.caption = element_text(size=9, hjust=1, family = "sans")) +
  
  
  
  
  geom_hline(yintercept = 0, size = 0.4, linetype = 1, color = "#333333") +
  annotate(geom='text', x=x.axis.labels, y = -0.8, label=x.axis.labels, vjust=1.2, family = "sans", size = 4, color = "#686868") +
  #annotate(geom='segment', x=x.axis.labels, xend=x.axis.labels,  y = 0, yend=-0.5, size = 0.4, color = "#333333") +
  scale_y_continuous(position = "right") +
  coord_cartesian(ylim = c(-2, 51), xlim = c(0, 100), clip = "off") +
  
  annotate(geom='text', x=105, y = y.axis.labels, label = y.axis.labels, vjust=-0.3, hjust = 1, family = "sans", size = 4, color = "#686868") +
  
  annotate("rect", xmin = 4, xmax = 22.5, ymin = 27, ymax = 49, alpha = 1, fill = "white", color = "white") +
  annotate("rect", xmin = 5, xmax = 8.6248, ymin = 34, ymax = 45.7, alpha = 1, fill = "#7FC8E8") +
  
  annotate("rect", xmin = 45, xmax = 48.5, ymin = 52, ymax = 54, alpha = 1, fill = "#2EAAE1") +
  annotate('text', x=49, y = 53, label = "Potraviny", hjust = 0, family = "sans", size = 3, color = "#696969", fontface = 2) +
  annotate("rect", xmin = 58, xmax = 61.5, ymin = 52, ymax = 54, alpha = 1, fill = "#7FC8E8") +
  annotate('text', x=62, y = 53, label = "Energie a palivá", hjust = 0, family = "sans", size = 3, color = "#696969", fontface = 2) +
  annotate("rect", xmin = 75.5, xmax = 79, ymin = 52, ymax = 54, alpha = 1, fill = "#C8E7F5") +
  annotate('text', x=79.5, y = 53, label = "Ostatné", hjust = 0, family = "sans", size = 3, color = "#696969", fontface = 2) +
  
  annotate("segment", x = 10, xend = 10, y = 34, yend = 45.7, color = "#696969", size = 0.45) +
  annotate("segment", x = 9.3, xend = 10, y = 45.609, yend = 45.609, color = "#696969", size = 0.45) +
  annotate("segment", x = 9.3, xend = 10, y = 34.091, yend = 34.091, color = "#696969", size = 0.45) +
  annotate("segment", x = 5, xend = 8.6248, y = 32.6248, yend = 32.6248, color = "#696969", size = 0.45) +
  annotate("segment", x = 5.0883, xend = 5.0883, y = 32.6248, yend = 33.3, color = "#696969", size = 0.45) +
  annotate("segment", x = 8.545, xend = 8.545, y = 32.6248, yend = 33.3, color = "#696969", size = 0.45) +
  
  annotate('text', x=11.2, y = 39.85, label = "Cena za rok\n vzrástla o 11,7 %", hjust = 0, family = "sans", size = 2.7, color = "#696969") +
  annotate('text', x=5, y = 29.7, label = "3,7 % výdavkov dávajú\ndomácnosti na elektrinu", hjust = 0, family = "sans", size = 2.7, color = "#696969") +
  annotate('text', x=6.8124, y = 47.2, label = "Elektrina", family = "sans", size = 2.7, color = "#696969", fontface = 2) +
  
  
  #0,6752
  labs(caption="Dáta: ŠÚSR | Graf: IFP ",
       subtitle="Medziročný rast cien v decembri 2022 (v %)",
       title="Čo za uplynulý rok zdraželo najviac?", fill="")


tooltip_css <- "background-color:black;color:white;font-family:sans-serif;padding:10px;border-radius:5px;"

interactive<-girafe(ggobj=plot,  
                    options = list(opts_hover(css = "fill:white;stroke:#333333;cursor:pointer;"),
                                   opts_tooltip(css = tooltip_css),
                                   opts_sizing(rescale = TRUE, width = 1)),
                    width_svg=9, height_svg=5.25)

#save plot as HTML file
saveWidget(interactive, "inflation-tracker.html", selfcontained = T)


