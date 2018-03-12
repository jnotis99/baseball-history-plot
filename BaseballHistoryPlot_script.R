##  Data Visualization (QSS17) Winter 2018
##  Lab Session 3: Create Your own Data Visualization Project
##
##  Name: Joseph Notis
##  Date: February 17, 2018

# Initial settings --------------------------------------------------------

library(tidyverse)
library(rvest)
library(ggrepel)

# Read and wrangle data ---------------------------------------------------


web_table <- function(num_tbl, stats) {
  df <- temp[[num_tbl]] %>% 
    filter(Year != "Year") %>% 
    select(stats) %>% 
    mutate_if(is.character, as.numeric, stats)
  
  return(df)
}

middle_year <- function(start, end) {
  mid_year <- round(median(c(start, end)))
  return(mid_year)
}

Eras <- data.frame(
  Year = c(middle_year(1871, 1892) + 1, # The +1 adjusts to prevent the text from intersecting with the line in the plot
           middle_year(1893, 1919),
           middle_year(1920, 1946),
           middle_year(1947, 1968),
           middle_year(1969, 1992),
           middle_year(1993, 2012),
           2016),
  era_name = c("Pioneer Era",
               "Dead Ball Era",
               "Live Ball Era",
               "Golden Era",
               "Artificial Turf\nEra",
               "Steroid Era",
               "Moneyball\nEra")
)

sig_events = data.frame(
  Year = c(1910, 1920, 1941, 1945, 1969),
  event_name = c("First cork-based core\n in baseballs",
                 "Spitball outlawed",
                 "WWII\nstarts",
                 "WWII\nends",
                 "Pitcher's mound lowered\nto 10 inches"))

url <- "https://www.baseball-reference.com/leagues/MLB/bat.shtml#all_teams_standard_batting_totals"
temp <- read_html(url) %>% 
  html_nodes("table") %>% 
  html_table()

totals <- web_table(2, c("Year", "H", "2B", "3B", "HR")) %>% 
  group_by(Year) %>% 
  mutate(XBH = (`2B` + `3B` + HR)) %>%
  ungroup()

averages <- web_table(1, c("Year", "G", "H", "HR", "SO")) %>% 
  mutate(XBH = totals$XBH / G,
         pct_K = SO / 27,
         pct_XBH = XBH / H) %>%
  gather("Stat", "Ratio", 7:8) %>%
  mutate(Stat = ifelse(Stat == "pct_XBH",
                       "Percentage of Hits that were Extra-Base Hits per Game",
                       "Percentage of Outs that were Strikeouts per Game")) %>% 
  left_join(Eras, by = "Year") %>% 
  left_join(sig_events, by = "Year") %>% 
  mutate(sig_year = !is.na(event_name))

# Data visualization ------------------------------------------------------

ggplot(averages, aes(Year, Ratio)) +
  annotate("rect", xmin = 1893, xmax = 1919, ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "darkgray") +
  annotate("rect", xmin = 1947, xmax = 1968, ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "darkgray") +
  annotate("rect", xmin = 1993, xmax = 2012, ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "darkgray") +
  geom_label_repel(aes(label = event_name), 
                   direction = "y",
                   nudge_y = c(0.07, -0.05),
                   min.segment.length = 0.001,
                   size = 4,
                   na.rm = TRUE) +
  geom_line(size = 1.25) +
  geom_point(aes(size = sig_year)) +
  geom_text(aes(Year, 0.025, label = era_name), na.rm = TRUE, size = 4) +
  facet_wrap(~ Stat, nrow = 2) +
  scale_x_continuous(breaks = c(1871, 1893, 1919, 1947, 1968, 1993, 2012, 2017)) +
  scale_y_continuous(limits = c(0, 0.4),
                     labels = scales::percent) +
  labs(title = "Extra-Base Hit and Strikeout frequency throughout Professional Baseball",
       subtitle = paste("Source: Major League Baseball Batting Year-by-Year Averages and Totals, Baseball Reference"),
       y = "") +
  theme_minimal() +
  guides(size = FALSE) +
  theme(strip.text = element_text(size = 10),
        axis.text.x = element_text(size = 7)) +
  scale_size_manual(values = c(0.25, 2.5))

ggsave("LabSession03/figures/LabSession03_figure1.pdf", width = 13, height = 7.5)