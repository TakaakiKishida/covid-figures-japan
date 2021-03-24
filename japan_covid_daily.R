setwd("~/Documents/GitHub/japan-pop-by-age/data/")

if (!require("pacman")) install.packages("pacman")
library(pacman) 

pacman::p_load(tidyverse, ggrepel, ggthemes)



# ------------------------------------
# import data

pop0_14 <- read_csv("2019-ages-0-14.csv")
pop15_64 <- read_csv("2019-ages-15-64.csv")
pop65_above <- read_csv("2019-ages-65-above.csv")
poptotal <- read_csv("2019-ages-total.csv")



# ------------------------------------
# processing

# pop <- bind_rows(pop0_14, pop15_64, pop65_above, poptotal)
# category <- c("age-0-14", "age-15-64", "age-65-above", "age-total")
# 
# pop <- pop %>% 
#   dplyr::rename(cname = "Country Name") %>% 
#   dplyr::filter(cname == "Japan") %>% 
#   dplyr::mutate(category = category) %>% 
#   dplyr::relocate(cname, category) %>% 
#   dplyr::select(-"Indicator Name", -"Indicator Code", -"Country Code", -"2020")

# convert to long tables

pop_1 <- pop0_14 %>%
  dplyr::rename(cname = "Country Name") %>%
  dplyr::filter(cname == "Japan") %>%
  dplyr::mutate(category = "age-0-14") %>% 
  dplyr::relocate(category) %>% 
  dplyr::select(-"cname", -"Indicator Name", -"Indicator Code", -"Country Code", -"2020") %>% 
  pivot_longer(starts_with(c("19", "20")),
               names_to = "year",
               values_to = "population")

pop_2 <- pop15_64 %>%
  dplyr::rename(cname = "Country Name") %>%
  dplyr::filter(cname == "Japan") %>%
  dplyr::mutate(category = "age-15-64") %>% 
  dplyr::relocate(category) %>% 
  dplyr::select(-"cname", -"Indicator Name", -"Indicator Code", -"Country Code", -"2020") %>% 
  pivot_longer(starts_with(c("19", "20")),
               names_to = "year",
               values_to = "population")

pop_3 <- pop65_above %>%
  dplyr::rename(cname = "Country Name") %>%
  dplyr::filter(cname == "Japan") %>%
  dplyr::mutate(category = "age-65-above") %>% 
  dplyr::relocate(category) %>% 
  dplyr::select(-"cname", -"Indicator Name", -"Indicator Code", -"Country Code", -"2020") %>% 
  pivot_longer(starts_with(c("19", "20")),
               names_to = "year",
               values_to = "population")

pop_4 <- poptotal %>%
  dplyr::rename(cname = "Country Name") %>%
  dplyr::filter(cname == "Japan") %>%
  dplyr::mutate(category = "age-total") %>% 
  dplyr::relocate(category) %>% 
  dplyr::select(-"cname", -"Indicator Name", -"Indicator Code", -"Country Code", -"2020") %>% 
  pivot_longer(starts_with(c("19", "20")),
               names_to = "year",
               values_to = "population")

# as the result below suggests, total population may be incorrect
(pop_1$population + pop_2$population + pop_3$population) == pop_4$population


# append long data
popage <- bind_rows(pop_1, pop_2, pop_3)

is.numeric(popage$year)
is.numeric(popage$population)

popage$year <- as.numeric(popage$year)



# ------------------------------------
# visualizing -- prep

options(scipen = 999)
year_range <- c(1960, 1970, 1980, 1990, 2000, 2010, 2019)

h_dash_lines_1 <- geom_segment(
  aes(y = 25000, yend = 25000, 
      x = 1960, xend = 2019),
  color = "black", linetype = 'dashed', alpha = 0.1)

h_dash_lines_2 <- geom_segment(
  aes(y = 50000, yend = 50000, 
      x = 1960, xend = 2019),
  color = "black", linetype = 'dashed', alpha = 0.1)

h_dash_lines_3 <- geom_segment(
  aes(y = 75000, yend = 75000, 
      x = 1960, xend = 2019),
  color = "black", linetype = 'dashed', alpha = 0.1)

h_dash_lines_4 <- geom_segment(
  aes(y = 100000, yend = 100000, 
      x = 1960, xend = 2019),
  color = "black", linetype = 'dashed', alpha = 0.1)

h_dash_lines_5 <- geom_segment(
  aes(y = 125000, yend = 125000, 
      x = 1960, xend = 2019),
  color = "black", linetype = 'dashed', alpha = 0.1)

popage %>% filter(year == 2019)



# ------------------------------------
# visualizing -- ggplot

popfig1 <- ggplot() + 
  geom_area(data = popage, 
            aes(x = year, y = population / 1000,
                fill = forcats::fct_rev(category)),
                color = "white",
            alpha = 0.7) +
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
  
  

popfig1

setwd("~/Documents/GitHub/japan-pop-by-age/")
ggsave(popfig1, filename = "popfig1.png", 
       width = 10, height = 6)


# ------------------------------------
