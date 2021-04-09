setwd("~/Documents/GitHub/covid-figures-japan/data")

if (!require("pacman")) install.packages("pacman")
library(pacman) 

pacman::p_load(tidyverse, ggrepel, ggthemes, lubridate)

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
  dplyr::mutate(date = as.Date(date, format = "%Y/%m/%d"),
                prefec = "Total") %>% 
  dplyr::filter(date >= "2020-2-5") 


# cases[order(cases$date, format = "%Y-%m-%d"),]
# cases[order(as.Date(cases$date, format="%Y-%m-%d")),]

prefec
names(prefec)

prefec <- prefec %>% 
  dplyr::rename(date = "日付",
                positive = "各地の感染者数_1日ごとの発表数",
                prefec = "都道府県名") %>% 
  dplyr::mutate(date = as.Date(date, format = "%Y/%m/%d")) %>%
  dplyr::filter(date >= "2020-2-5") %>% 
  dplyr::select(date, positive, prefec) %>% 
  dplyr::mutate(main = recode(prefec,
                               # "北海道"   = 1,
                               "東京都"   = 1,
                               "神奈川県" = 1,
                               # "千葉県"   = 1,
                               # "埼玉県"   = 1,
                               # "愛知県"   = 1,
                               "大阪府"   = 1,
                               # "兵庫県"   = 1,
                               # "福岡県"   = 1,
                               .default   = 0)) %>% 
  dplyr::mutate(prefec = recode(prefec,
                                # "北海道"   = "Hokkaido",
                                "東京都"   = "Tokyo",
                                "神奈川県" = "Kanagawa",
                                # "千葉県"   = "Chiba",
                                # "埼玉県"   = "Saitama",
                                # "愛知県"   = "Aichi",
                                "大阪府"   = "Osaka")) 
                                # "兵庫県"   = "Hyogo",
                                # "福岡県"   = "Fukuoka"
                                # .default  = "")) %>% 

class(cases$date)
class(prefec$date)

# cases$date <- as.Date(cases$date, format = "%Y/%m/%d")
# prefec$date <- as.Date(prefec$date, format = "%Y/%m/%d")

cases <- bind_rows(cases, prefec)
cases$prefec

cases_main <- cases %>% 
  dplyr::filter(main == 1) %>% 
  dplyr::select(date, positive, prefec) %>% 
  tidyr::separate(col = date, 
                  into = c("year", "month", "day"), 
                  sep = "-", 
                  remove = FALSE)

cases_others <- cases %>% 
  dplyr::filter(main == 0) %>% 
  dplyr::select(date, positive, prefec) %>% 
  tidyr::separate(col = date, 
                  into = c("year", "month", "day"), 
                  sep = "-", 
                  remove = FALSE)



# ------------------------------------
# visualizing -- prep




# ------------------------------------
# visualizing -- ggplot
cases_fig1
resultcolors <- c("brown4", "dodgerblue1", "dodgerblue4")


cases_fig1 <- ggplot() + 
  geom_line(data = cases_main,
            aes(x = date, y = positive, 
                group = prefec, color = prefec)) +
  scale_color_manual(values = resultcolors) +
  # group = forcats::fct_rev(prefec))) +
  geom_line(data = cases_others,
            aes(x = date, y = positive, 
                group = prefec), 
            color = "black", alpha = 0.3) +
  geom_rect(aes(xmin = as.Date("2020-04-07"), xmax = as.Date("2020-05-25"),
                ymin = 0, ymax = 2500),
            fill = "darkgrey", color = NA, alpha = 0.4) +
  geom_rect(aes(xmin = as.Date("2021-01-08"), xmax = as.Date("2021-03-21"),
                ymin = 0, ymax = 2500),
            fill = "darkgrey", color = NA, alpha = 0.4) +
  # cases_fig1
  labs(title = "Daily Covid-19 Positive Cases in Japan",
       caption = "Source: Japan Broadcasting Corporation. Note: SoE refers to the State of Emergency.",
       x = "Date",
       y = "Positive Cases") +
  scale_x_date(limits = c(as.Date("2020-02-02"), as.Date("2021-05-31")),
               expand = c(0, 0),
               date_breaks = "2 month", 
               date_labels = "%b") +
  # scale_x_datetime(limits = lims, 
  #                  date_breaks = "month", 
  #                  date_labels = "%m-%d") +
  scale_y_continuous(limits = c(0, 2500),
                     breaks = seq(0, 2500, 500),
                     expand = c(0, 0),
                     # position = "right",
                     labels = scales::comma) +
  # scale_y_continuous(position = "right") +
  theme_minimal() + 
  theme(text = element_text(family = "Optima"),
        plot.title = element_text(size = 22),
        plot.caption = element_text(hjust = 0),
        axis.text = element_text(color = "black", size = 12),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_blank(),
        plot.background = element_rect(fill = "#f5f5f2"),
        # panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "grey80",
                                          linetype = "dashed"),
        legend.position = "none",
        plot.margin = margin(25, 25, 10, 25)) +
  geom_text_repel(data = cases_main %>% filter(date == "2021-03-27"), 
                  aes(x = date, y = positive, label = prefec, color = prefec),
                  family = "Avenir Next Condensed",
                  nudge_x = 100,
                  fontface = "bold",
                  direction = "y",
                  hjust = 0,
                  segment.size = 0.7,
                  segment.alpha = 0.5,
                  segment.linetype = "dotted",
                  box.padding = 0.4,
                  segment.curvature = -0.1,
                  segment.ncp = 3,
                  segment.angle = 20) +
  # text annotations
  annotate("text", x = as.Date("2020-05-01"), y = 2250,
           label = "1st SoE \n Apr 7 to May 25",
           family = "Avenir Next Condensed",
           size = 4,
           color = "grey40",
           hjust = 0.5) + 
  annotate("text", x = as.Date("2021-02-12"), y = 2250,
           label = "2nd SoE \n Jan 8 to Mar 21",
           family = "Avenir Next Condensed",
           size = 4,
           color = "grey40",
           hjust = 0.5) +
  annotate("text", x = as.Date("2020-12-25"), y = 2400,
           label = "2021",
           family = "Avenir Next Condensed",
           size = 5,
           color = "black",
           hjust = 0.5) +
  annotate("text", x = as.Date("2020-02-02"), y = 2400,
           label = "2020",
           family = "Avenir Next Condensed",
           size = 5,
           color = "black",
           hjust = 0)
  


cases_fig1




setwd("~/Documents/GitHub/covid-figures-japan/")
ggsave(cases_fig1, filename = "cases_fig1.png", width = 10, height = 6)
# ------------------------------------

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
  theme_minimal() + theme(panel.grid = element_blank()) +
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
  

# ------------------------------------
