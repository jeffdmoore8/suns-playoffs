library(nbastatR)
library(tidyverse)
library(BasketballAnalyzeR)
library(ggimage)
library(cropcircles)
library(ggtext)
library(glue)
library(janitor)
library(htmltools)

Sys.setenv(VROOM_CONNECTION_SIZE=500072)

nba_teams_ids(teams = 'Phoenix Suns', team_ids = NULL, all_active_teams = F)


shots <- nbastatR::teams_shots(team_ids = 1610612756, seasons = 2023, season_types = c('Regular Season', 'Playoffs')) %>%
  filter(namePlayer %in% c('Kevin Durant', 'Mikal Bridges', 'Cameron Johnson')) %>%
  mutate(kd_flag = case_when(namePlayer=='Kevin Durant' ~ 'post',
         TRUE ~ 'pre'
  ),
  x = locationX/10-0,
  y = locationY/10-41.75
         ) %>%
  filter(y<0)

images <- data.frame(
  namePlayer = c("Kevin Durant","Mikal Bridges","Cameron Johnson"),
  label = c("Kevin Durant","Mikal Bridges","Cameron Johnson"),
  image = c("https://a.espncdn.com/combiner/i?img=/i/headshots/nba/players/full/3202.png",
            "https://a.espncdn.com/combiner/i?img=/i/headshots/nba/players/full/3147657.png",
            "https://a.espncdn.com/combiner/i?img=/i/headshots/nba/players/full/3138196.png")
)

#circle crop images
images$cropped<-cropcircles::circle_crop(images=images$image, to = here::here(paste0(images$label,'.png')), border_size = 1, border_colour = "whitesmoke")

drawNBAcourt(ggplot(data = shots),
             size = 0.5, col = 'grey20')+
  geom_point(mapping = aes(x=x, y=y, fill = typeEvent),

  color = 'white', shape = 21, size = 2.5, alpha = 0.8)+
  #backdrop image with fill to create border
  geom_image(data=images, mapping=aes(x=-20, y=6, image=cropped), color="#818990", size=0.16, asp=1/1.2)+
  #player image
  geom_image(data=images, mapping=aes(x=-20, y=6, image=cropped), size=0.15, asp=1/1.2)+
  #text per player with name, team, and stats
  # ggtext::geom_textbox(data=images, mapping=aes(x=5, y=6, label=text_label),
  #                      fill=NA, box.size=NA,
  #                      family=NA)+
  facet_wrap(~namePlayer, ncol=3)+
  coord_equal()+
  scale_fill_manual(values=rev(c("grey70","#ED254E")))+
  scale_y_continuous(expand=c(0.1,0.2))+
  #scale_fill_manual(values = c('#0E86BD', '#EE4C59'))+
  guides(fill = guide_legend(override.aes = list(size=5)))+
  labs(title = 'Suns Before/After KD', fill = 'Event', caption = 'Source: nbastatR')+
  theme(legend.position = "top",
        legend.title = element_text(face="bold", size=12),
        plot.margin = margin(t=20),
        legend.text = element_text(size=12),
        legend.margin = margin(rep(0,4)),
        strip.text = element_blank(),
        strip.background = element_blank(),
        legend.key = element_blank(),
        axis.text = element_blank(),
        legend.justification = "left",
        #text = element_text(family=font),
        panel.background = element_blank(),
        plot.title = element_text(face="bold", size=18),
        plot.subtitle = element_text(color="#818990", size=16, margin=margin(b=5)),
        panel.grid.minor=element_blank(),
        plot.caption = element_textbox_simple(hjust=0.01, color="#818990", margin=margin(b=10), size=10),
        # panel.grid.major = element_line(color="grey90", linewidth=0.3),
        axis.title=element_blank(),
        axis.ticks = element_blank())


ggsave('test.png', dpi = 'retina')
validate_nba_player_photos()
win_probability()
