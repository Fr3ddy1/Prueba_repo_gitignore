library(readr)
library(janitor)
library(dplyr)
library(ggplot2)
library(forcats)
library(glue)
library(ggtext)
library(cowplot)

data = read_csv("data/conmebol_team_stats.csv") %>% 
       mutate(xg_per_game = round(xG/PJ, 2),
              image = glue("<img src='logos/{Club}.png' width='50' />")) %>% 
       arrange(xg_per_game)

images = data$image

p = ggplot(data, aes(x = fct_reorder(image, xg_per_game), y = xg_per_game)) +
  geom_bar(stat = "identity", width = 0.93,
           fill = "#219ebc", alpha = 0.8) +
  geom_text(aes(label = xg_per_game), 
            size = 4, col = "white", hjust = 1.4) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 2.5), expand = c(0, 0)) +
  labs(x = "",
       y = "\nxG promedio por partido",
       title = "Ranking xG a favor promedio por partido",
       subtitle = "Clasificatorias Conmebol Qatar 2022\n", 
       caption = "Data: Instat") +
  scale_x_discrete(labels = images, expand = expansion(add = 0.07)) +
  theme_bw() +
  theme(plot.title = element_text(vjust = 0, hjust = 0.5),
        plot.subtitle = element_text(vjust = 0, hjust = 0.5),
        plot.margin = margin(2, 1.2, 1, 0.5, "cm"),
        text = element_text(size = 14),
        axis.text.y = element_markdown(size = 4),
        plot.background = element_rect(fill = "grey98"))

p2 = ggdraw() +
     draw_plot(p) +
     draw_image("logos/lpdt.png", x = 0.5, y = 0.45, hjust = 0.5, width = 0.15) +
     draw_image("logos/runrunes.png", x = 0.9, y = 0.45, hjust = 0.5, width = 0.15)

ggsave(plot = p2, filename = "outputs/bar_ranking.png", width = 11, height = 9)

