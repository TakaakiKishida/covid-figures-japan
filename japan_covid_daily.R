setwd("~/Documents/GitHub/covid-figures-japan/data")

if (!require("pacman")) install.packages("pacman")
library(pacman) 

pacman::p_load(tidyverse, ggrepel, ggthemes)

# devtools::install_github("slowkow/ggrepel")
library(ggrepel)



# ------------------------------------
# import data

positive <- read_csv("MHLW/pcr_positive_daily.csv")
test <- read_csv("MHLW/pcr_tested_daily.csv")
severe <- read_csv("MHLW/severe_daily.csv")
prefec <- read_csv("NHK/nhk_news_covid19_prefectures_daily_data.csv")


# ------------------------------------
# processing

positive; test; severe
names(positive); names(test); names(severe)
tail(positive); tail(test); tail(severe)

d1 <- dplyr::left_join(positive, test, by = "日付")

cases <- dplyr::left_join(d1, severe, by = "日付")

cases <- cases %>% 
  dplyr::select(-X3, -X4) %>% 
  dplyr::rename(date = "日付",
                positive = "PCR 検査陽性者数(単日)",
                pcr = "PCR 検査実施件数(単日)",
                severe = "重症者数") %>% 
  dplyr::filter(date >= "2020/2/5") %>% 
  dplyr::mutate(prefec = "Total")
  


prefec
names(prefec)

prefec <- prefec %>% 
  dplyr::rename(date = "日付",
                positive = "各地の感染者数_1日ごとの発表数",
                prefec = "都道府県名") %>% 
  dplyr::filter(date >= "2020/2/5") %>% 
  dplyr::select(date, positive, prefec) %>% 
  dplyr::mutate(main = recode(prefec,
                               "北海道"   = 1,
                               "東京都"   = 1,
                               "神奈川県" = 1,
                               "千葉県"   = 1,
                               "埼玉県"   = 1,
                               "愛知県"   = 1,
                               "大阪府"   = 1,
                               "兵庫県"   = 1,
                               "福岡県"   = 1,
                               .default   = 0)) %>% 
  dplyr::mutate(prefec = recode(prefec,
                                "北海道"   = "Hokkaido",
                                "東京都"   = "Tokyo",
                                "神奈川県" = "Kanagawa",
                                "千葉県"   = "Chiba",
                                "埼玉県"   = "Saitama",
                                "愛知県"   = "Aichi",
                                "大阪府"   = "Osaka",
                                "兵庫県"   = "Hyogo",
                                "福岡県"   = "Fukuoka"))
                                # .default  = "")) %>% 


cases <- bind_rows(cases, prefec)

cases_main <- cases %>% 
  dplyr::filter(main == 1) %>% 
  dplyr::select(date, positive, prefec) 

cases_others <- cases %>% 
  dplyr::filter(main == 0) %>% 
  dplyr::select(date, positive, prefec) 



# ------------------------------------
# visualizing -- prep




# ------------------------------------
# visualizing -- ggplot
cases_fig1

cases_fig1 <- ggplot() + 
  geom_line(
    data = cases_main, 
    aes(x = date, y = positive,
        group = prefec,
        color = prefec)
        # group = forcats::fct_rev(prefec))
    ) +
  geom_line(
    data = cases_others, 
    aes(x = date, y = positive,
        # alpha = 0.5,
        group = prefec,
        color = "red")
    ) +
  
# cases_fig1

  geom_text_repel(
    data = cases_main %>% filter(date == "2021/3/27"), 
    aes(x = date, y = positive,
        label = prefec),
    # family = "Avenir Next Condensed",
    # nudge_x = 100,
    fontface = "bold",
    # size = 8,
    direction = "y",
    xlim = c("2021/3/27", NA),
    hjust = 0,
    segment.size = .7,
    segment.alpha = .5,
    segment.linetype = "dotted",
    box.padding = .4,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20
  ) +
  labs(title = "Daily Covid-19 Positive Cases in Japan",
       caption = "Source: Japan Broadcasting Corporation",
       x = "Date",
       y = "Positive Cases") +
  theme_minimal() + theme(panel.grid=element_blank()) +
  theme(text = element_text(family = "Optima"),
        plot.title = element_text(size = 24),
        plot.caption = element_text(hjust = 0),
        axis.text = element_text(color = "black", size = 14),
        axis.title.y = element_text(size = 18),
        axis.title.x = element_blank(),
        plot.background = element_rect(fill = "#f5f5f2"),
        legend.position = "none",
        plot.margin = margin(25, 25, 10, 25)) 
  
cases_fig1


  h_dash_lines_1 + h_dash_lines_2 + h_dash_lines_3 + 
  h_dash_lines_4 + h_dash_lines_5 +
  labs(title = "Age Distribution of Population in Japan, 1960-2019",
       # subtitle = "",
       caption = "Source: World Bank Open Data",
       x = "Year",
       y = "Population in Thousand") +
  scale_fill_manual(values = c("#8ba6b5", "#2695ab", "#135280"))+
  scale_y_continuous(limits = c(0, 130000),
                     expand = c(0, 0),
                     labels = scales::comma,
                     breaks = seq(0, 125000, 25000)) +
  scale_x_continuous(limits = c(1960, 2030),
                     expand = c(0, 0),
                     breaks = year_range) +
  theme_minimal() + theme(panel.grid=element_blank()) +
  theme(text = element_text(family = "Optima"),
        plot.title = element_text(size = 24),
        plot.caption = element_text(hjust = 0, size = 14),
        axis.text = element_text(color = "black", size = 14),
        axis.title.y = element_text(size = 18),
        axis.title.x = element_blank(),
        plot.background = element_rect(fill = "#f5f5f2"),
        legend.position = "none") + 
  
  # additional texts -- legend
  annotate(geom = "text", label= "0-14 years",
           x = 2024, y = 8000,
           color = "#135280", family = "Optima", size = 6) +
  annotate(geom = "text", label= "15-64 years",
           x = 2024.3, y = 55000,
           color = "#2695ab", family = "Optima", size = 6) +
  annotate(geom = "text", label= "65+ years",
           x = 2023.8, y = 110000,
           color = "#8ba6b5", family = "Optima", size = 6) + 
  # additional lines -- arrow both 
  annotate(geom = "segment", x = 2016.8, xend = 2016.8, 
           y = 0, yend = 15775, 
           arrow = grid::arrow(length = unit(0.2, "cm"), 
                               ends = "both"), color = "DimGray") +
  annotate(geom = "segment", x = 2016.8, xend = 2016.8, 
           y = 16600, yend = 92008, 
           arrow = grid::arrow(length = unit(0.2, "cm"), 
                               ends = "both"), color = "DimGray") +
  annotate(geom = "segment", x = 2016.8, xend = 2016.8, 
           y = 92508, yend = 126383, 
           arrow = grid::arrow(length = unit(0.2, "cm"), 
                               ends = "both"), color = "DimGray") +
  # additional texts -- numbers
  annotate(geom = "text", label= "15,875",
           x = 2016, y = 12000,
           color = "white", family = "Optima", size = 5) +
  annotate(geom = "text", label= "75,033",
           x = 2016, y = 88000,
           color = "white", family = "Optima", size = 5) +
  annotate(geom = "text", label= "35,357",
           x = 2016, y = 121500,
           color = "white", family = "Optima", size = 5) 
  
  

cases_fig1

setwd("~/Documents/GitHub/japan-pop-by-age/")
ggsave(popfig1, filename = "popfig1.png", 
       width = 10, height = 6)


# ------------------------------------
