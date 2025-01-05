library(ggplot2)
library(ggtext)
library(showtext)
library(sysfonts)
library(gdtools)
library(ggiraph)
library(dplyr)
library(ggimage)
library(patchwork)

register_gfont("Exo 2")
font_add_google("Exo 2", "Exo 2")
showtext_auto()

# This function is a modification of the original 'create_OPTA_pitch' function from package 'FCrSTATS/fc.rstats'. More info: https://rdrr.io/github/FCrSTATS/fc.rstats/man/create_OPTA_pitch.html
create_simple_pitch <- function (grass_colour = "#ffffff", line_colour = "#024b4a", 
                                 line_width = 0.7, background_colour = "#ffffff", 
                                 goal_colour = "#024b4a", padding = 2, arrow = F) 
{
  theme_blankPitch = function(size = 12) {
    theme(axis.text.x = element_blank(), axis.text.y = element_blank(), 
          axis.ticks.length = unit(0, "lines"), axis.title.x = element_blank(), 
          axis.title.y = element_blank(), legend.background = element_rect(fill = background_colour, 
                                                                           colour = NA), legend.key = element_rect(colour = background_colour, 
                                                                                                                   fill = background_colour), legend.key.size = unit(1.2, 
                                                                                                                                                                     "lines"), legend.text = element_text(size = size), 
          legend.title = element_text(size = size, face = "bold", 
                                      hjust = 0), strip.background = element_rect(colour = background_colour, 
                                                                                  fill = background_colour, linewidth = 0.5), panel.background = element_rect(fill = background_colour, 
                                                                                                                                                              colour = background_colour), panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), panel.spacing = element_blank(), 
          plot.background = element_blank(), plot.margin = unit(c(0, 
                                                                  0, 0, 0), "lines"), plot.title = element_text(size = size * 
                                                                                                                  1.2), strip.text.y = element_text(colour = background_colour, 
                                                                                                                                                    size = size, angle = 270), strip.text.x = element_text(size = size * 
                                                                                                                                                                                                             1))
  }
  p <- ggplot() + xlim(c(0 - padding, 100 + padding)) + ylim(c(0 - 
                                                                 padding, 100 + padding)) + theme_blankPitch()
  p <- p + geom_rect(aes(xmin = 0, xmax = 50, ymin = 0, ymax = 100), 
                     fill = NA, colour = line_colour, linewidth = line_width) + geom_rect(aes(xmin = 50, 
                                                                                              xmax = 100, ymin = 0, ymax = 100), fill = NA, colour = line_colour, linewidth = line_width) + 
    geom_rect(aes(xmin = 0, xmax = 17, ymin = 21.1, ymax = 78.9), 
              fill = grass_colour, colour = line_colour, linewidth = line_width) + geom_rect(aes(xmin = 83, 
                                                                                                 xmax = 100, ymin = 21.1, ymax = 78.9), fill = grass_colour, 
                                                                                             colour = line_colour, linewidth = line_width) + geom_segment(aes(x = 50, y = 0, 
                                                                                                                                                              xend = 50, yend = 100), colour = line_colour, linewidth = line_width)
  
  p <- p + geom_rect(aes(xmin = -1, ymin = 45.2, xmax = 0, 
                         ymax = 54.8), fill = grass_colour, colour = line_colour, linewidth = line_width) + 
    geom_rect(aes(xmin = 100, ymin = 45.2, xmax = 101, 
                  ymax = 54.8), fill = grass_colour, colour = line_colour, linewidth = line_width)
  
  p <- p + geom_rect(aes(xmin = 0, xmax = 5.8, ymin = 36.8, 
                         ymax = 63.2), fill = grass_colour, colour = line_colour, linewidth = line_width) + 
    geom_rect(aes(xmin = 94.2, xmax = 100, ymin = 36.8, 
                  ymax = 63.2), fill = grass_colour, colour = line_colour, linewidth = line_width) + 
    annotate("path", x = 50 + 8.7 * cos(seq(0, 2 * pi, 
                                            length.out = 100)), y = 50 + 8.7 * sin(seq(0, 
                                                                                       2 * pi, length.out = 100)), col = line_colour, linewidth = line_width) + 
    geom_point(aes(x = 11.5, y = 50), colour = line_colour, 
               size = 1) + geom_point(aes(x = 88.5, y = 50), 
                                      colour = line_colour, size = line_width) + geom_point(aes(x = 50, 
                                                                                                y = 50), colour = line_colour, size = line_width)
  p <- p + annotate("path", x = (11.5) + 9.2 * cos(seq(-0.3 * 
                                                         pi, 0.3 * pi, length.out = 500)), y = 50 + 9.2 * 
                      sin(seq(-0.3 * pi, 0.3 * pi, length.out = 500)), 
                    col = line_colour, linewidth = line_width) + annotate("path", x = (88.5) - 
                                                                            9.2 * cos(seq(-0.3 * pi, 0.3 * pi, length.out = 500)), 
                                                                          y = 50 - 9.2 * sin(seq(-0.3 * pi, 0.3 * pi, length.out = 500)), 
                                                                          col = line_colour, linewidth = line_width)
  
  if(arrow){
    p <- p + 
      geom_segment(aes(x = 40, y = -2, xend = 60, yend = -2),colour = line_colour, arrow = arrow(length = unit(0.1, "cm"), type="closed"))
  }
  
  return(p)
}

add_plot_title <- function(plot, text, title_margin = margin(0, 0, -10, 0)){
  p <- plot +
    labs(title = paste0("<img src='images/UEFA_Euro_2024_Logo.png' height=35 style='padding-right:10px'><span>", text, "</span>")) + 
    theme(plot.title = element_markdown(hjust = 0.5,
                                        fill = "#e2f2e9",
                                        family = "Exo 2",
                                        size = 23,
                                        margin = title_margin),
          plot.background = element_rect(color = "#e2f2e9", fill = "#e2f2e9"))
  
  return(p)
}

create_formations_plot <- function(data, highlighted_team = NULL, highlighted_position = NULL, hover_img = F){
  data$tooltip <- c(data$player_name)
  
  p <- create_simple_pitch(grass_colour = "#e2f2e9", 
                           background_colour = "#e2f2e9", 
                           line_colour = "black")
  
  if(hover_img){
    p <- p +
      geom_image(data = data, 
                 mapping = aes(x = 40, y = 90, 
                               image = "images/hover_me.png"),
                 size = 0.5)
  }
  
  if(!is.null(highlighted_team) & !is.null(highlighted_position)){
    hl_data <- data %>% filter(team_name == highlighted_team,
                               player_position == highlighted_position)
    nhl_data <- bind_rows(data %>% filter(team_name == highlighted_team,
                                          player_position != highlighted_position),
                          data %>% filter(team_name != highlighted_team))
  }
  else if(is.null(highlighted_team) & !is.null(highlighted_position)){
    hl_data <- data %>% filter(player_position == highlighted_position)
    nhl_data <- data %>% filter(player_position != highlighted_position)
  }
  else if(!is.null(highlighted_team) & is.null(highlighted_position)){
    hl_data <- data %>% filter(team_name == highlighted_team)
    nhl_data <- data %>% filter(team_name != highlighted_team)
  }
  
  if(!is.null(highlighted_team) | !is.null(highlighted_position)){
    p <- p +
      geom_point_interactive(data = hl_data, 
                             mapping = aes(x = x, y = y,
                                           tooltip = tooltip, data_id = tooltip,
                                           color = team_name, fill = team_name
                             ),
                             alpha = 1,
                             shape = 21, 
                             size = 8,
                             stroke = 1.5) +
      geom_text(data = hl_data, 
                mapping = aes(x = x, y = y, 
                              label = player_number),
                alpha = 1,
                size = 4,
                family = "Exo 2",
                fontface = "bold",
                color = ifelse(hl_data$team_name == "England", "#CD2626", "#FFD700")) +
      geom_point_interactive(data = nhl_data, 
                             mapping = aes(x = x, y = y,
                                           tooltip = tooltip, data_id = tooltip,
                                           color = team_name, fill = team_name
                             ),
                             alpha = 0.1,
                             shape = 21, 
                             size = 8,
                             stroke = 1.5) +
      geom_text(data = nhl_data, 
                mapping = aes(x = x, y = y, 
                              label = player_number),
                alpha = 0.1,
                size = 4,
                family = "Exo 2",
                fontface = "bold",
                color = ifelse(nhl_data$team_name == "England", "#CD2626", "#FFD700")) +
      scale_color_manual_interactive(values = c("Spain" = "#104E8B", "England" = "#27408B")) +
      scale_fill_manual_interactive(values = c("Spain" = "#FF0000", "England" = "#FFFFFF"))
  }
  else{
    p <- p +
      geom_point_interactive(data = data, 
                             mapping = aes(x = x, y = y,
                                           tooltip = tooltip, data_id = tooltip,
                                           color = team_name, fill = team_name
                             ),
                             shape = 21, 
                             size = 8,
                             stroke = 1.5) +
      geom_text(data = data, 
                mapping = aes(x = x, y = y, 
                              label = player_number),
                size = 4,
                family = "Exo 2",
                fontface = "bold",
                color = ifelse(data$team_name == "England", "#CD2626", "#FFD700")) +
      scale_color_manual_interactive(values = c("Spain" = "#104E8B", "England" = "#27408B")) +
      scale_fill_manual_interactive(values = c("Spain" = "#FF0000", "England" = "#FFFFFF")) 
  }
  
  p <- p +
    geom_image(data = data, 
               mapping = aes(x = 5, y = 94, 
                             image = "images/ESP.png"),
               size = 0.07) +
    geom_image(data = data, 
               mapping = aes(x = 95, y = 94, 
                             image = "images/ENG.png"),
               size = 0.07) +
    geom_text(data = data, 
              mapping = aes(x = 18, y = 94, 
                            label = "SPAIN"),
              size = 7,
              family = "Exo 2",
              color = "#EE0000") +
    geom_text(data = data, 
              mapping = aes(x = 78, y = 94, 
                            label = "ENGLAND"),
              size = 7,
              family = "Exo 2",
              color = "#27408B") +
    guides(color = "none",
           fill = "none")
  
  g <- girafe(ggobj = add_plot_title(p, " EURO 2024 Final"),
              width_svg = 7,
              height_svg = 6,
              options = list(
                opts_toolbar(
                  hidden = c("lasso_select", "lasso_deselect", "zoom_onoff",
                             "zoom_rect", "zoom_reset", "saveaspng"
                  )
                ),
                opts_sizing(rescale = F),
                opts_tooltip(use_fill = T,
                             css = paste0("font-family: Exo 2; padding:3pt; color:#000080; border-radius:5px")
                ),
                opts_hover(css = "stroke:black;stroke-width:3px;")
              )
  )
  
  return(g)
}

create_goal_sequence_plot <- function(data, rect_points = NULL){
  data$tooltip <- c(data$player_name)
  
  p <- create_simple_pitch(grass_colour = "#e2f2e9", 
                           background_colour = "#e2f2e9", 
                           line_colour = "black")
  
  if(length(rect_points) == 4){
    p <- p +
      geom_rect(aes(xmin = rect_points[[1]], xmax = rect_points[[2]], 
                    ymin = rect_points[[3]], ymax = rect_points[[4]]),
                fill = "#FF0000",
                alpha = 0.3)
  }
  
  p <- p +
    geom_segment(data = data %>% filter(event_type %in% c("Pass", "Carry", "Shot")),
                 mapping = aes(x = x, y = y,
                               xend = x_end, yend = y_end),
                 color = ifelse((data %>% filter(event_type %in% c("Pass", "Carry", "Shot")))$team_name == "England", "#27408B", "#104E8B"),
                 linetype = ifelse((data %>% filter(event_type %in% c("Pass", "Carry", "Shot")))$event_type %in% c("Pass", "Shot"), 1, 2),
                 arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +
    geom_segment(data = data %>% filter(event_type == "LoseMarker"),
                 mapping = aes(x = x, y = y,
                               xend = x_end, yend = y_end),
                 color = ifelse((data %>% filter(event_type == "LoseMarker"))$team_name == "England", "#CD2626", "#FF0000"),
                 linetype = 3,
                 linewidth = 0.5,
                 arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +
    geom_point_interactive(data = data %>% filter(event_type == "PlayerPos"), 
                           mapping = aes(x = x, y = y,
                                         tooltip = tooltip, data_id = tooltip,
                                         color = team_name, fill = team_name
                           ),
                           shape = 21, 
                           size = 8,
                           stroke = 1.5) +
    geom_text(data = data %>% filter(event_type == "PlayerPos"), 
              mapping = aes(x = x, y = y, 
                            label = player_number),
              size = 4,
              family = "Exo 2",
              fontface = "bold",
              color = ifelse((data %>% filter(event_type == "PlayerPos"))$team_name == "England", "#CD2626", "#FFD700")) +
    scale_color_manual_interactive(values = c("Spain" = "#104E8B", "England" = "#27408B")) +
    scale_fill_manual_interactive(values = c("Spain" = "#FF0000", "England" = "#FFFFFF"))  +
    guides(color = "none",
           fill = "none")
  
  g <- girafe(ggobj = add_plot_title(p, " EURO 2024 Final"),
              width_svg = 7,
              height_svg = 6,
              options = list(
                opts_toolbar(
                  hidden = c("lasso_select", "lasso_deselect", "zoom_onoff",
                             "zoom_rect", "zoom_reset", "saveaspng"
                  )
                ),
                opts_sizing(rescale = F),
                opts_tooltip(use_fill = T,
                             css = paste0("font-family: Exo 2; padding:3pt; color:#000080; border-radius:5px")
                ),
                opts_hover(css = "stroke:black;stroke-width:3px;")
              )
  )
  
  return(g)
}

create_xg_evolution_plot <- function(data){
  data$id <- rownames(data)
  data$tooltip <- c(paste0(data$minute, "' ", data$player_name, "\n ", 
                           "xG: ", data$xG))
  
  p <- ggplot(data) +
    geom_step(aes(minute, xG_accum,
                  color = team_name), 
              linewidth = 1) +
    geom_vline_interactive(data = data %>% 
                             filter(is_goal), 
                           aes(xintercept = minute, 
                               color = team_name,
                               tooltip = tooltip, 
                               data_id = id), 
                           linetype = "dashed", 
                           alpha = 0.3, 
                           linewidth = 0.6) +
    scale_color_manual(values = c("Spain" = "#FF0000", "England" = "#27408B")) +
    geom_image(data = data %>% 
                 filter(is_goal), 
               aes(x = minute, y = 1.8, image = "images/football-ball.png"), 
               asp = 3, 
               size = .012) +
    geom_text(data = data %>% 
                filter(is_goal), 
              mapping = aes(x = minute + 1.3, y = 1.8, 
                            label = result),
              size = 3,
              family = "Exo 2",
              fontface = "bold",
              color = ifelse((data %>% filter(is_goal))$team_name == "England", "#27408B", "#FF0000")) +
    geom_text(data = tail(data, 2) %>% filter(team_name == "Spain"), 
              mapping = aes(x = minute, y = xG_accum + 0.05, 
                            label = paste0("SPA: ", xG_accum)),
              size = 3.5,
              family = "Exo 2",
              fontface = "bold",
              color = "#FF0000") +
    geom_text(data = tail(data, 2) %>% filter(team_name == "England"), 
              mapping = aes(x = minute, y = xG_accum + 0.05, 
                            label = paste0("ENG: ", xG_accum)),
              size = 3.5,
              family = "Exo 2",
              fontface = "bold",
              color = "#27408B") +
    labs(x = "Minute",
         y = "Total xG") +
    scale_x_continuous(limits = c(45, 90), 
                       breaks = seq(45, 90, 5)) +
    ylim(0, 1.8) +
    theme_minimal() +
    theme(text = element_text(family = "Exo 2"),
          plot.background = element_rect(color = "#e2f2e9", fill = "#e2f2e9"),
          panel.background = element_rect(color = "#e2f2e9", fill = "#e2f2e9"),
          panel.grid.major = element_line(color = "#8FBC8F",
                                          linewidth = 0.1),
          panel.grid.minor = element_blank()) +
    guides(color = "none")
  
  return(p)
}

create_shotmap <- function(data, highlight = NULL){  
  data$id <- rownames(data)
  data$tooltip <- c(paste0(data$minute, "' ", data$player_name, "\n ", 
                           "xG: ", data$xG))
  
  data <- data %>% 
    mutate(is_goal = ifelse(is_goal, "Goal", "No goal"),
           x = x/1.05, 
           y = 100-(y/0.68)) %>% 
    filter(!is.na(xG))
  
  p <- create_simple_pitch(grass_colour = "#e2f2e9", 
                           background_colour = "#e2f2e9", 
                           line_colour = "black")
  
  p <- p +
    coord_flip(xlim = c(52.2, 99.5),
               ylim = c(4.4, 95.6))
  
  if(is.null(highlight)){
    p <- p +
      geom_point_interactive(data = data %>% 
                               filter(is_goal == "Goal"),
                             aes(x = x, 
                                 y = y, 
                                 color = team_name,
                                 fill = team_name,
                                 tooltip = tooltip, 
                                 data_id = id),
                             size = 5,
                             alpha = 0.7,
                             shape = "\u2605") +
      geom_point_interactive(data = data %>% 
                               filter(is_goal == "No goal"),
                             aes(x = x, 
                                 y = y, 
                                 color = team_name,
                                 fill = team_name,
                                 tooltip = tooltip, 
                                 data_id = id),
                             size = 4,
                             alpha = 0.7)
  }
  else{
    p <- p +
      geom_point_interactive(data = data[highlight,] %>% 
                               filter(is_goal == "Goal"),
                             aes(x = x, 
                                 y = y, 
                                 color = team_name,
                                 fill = team_name,
                                 tooltip = tooltip, 
                                 data_id = id),
                             size = 5,
                             alpha = 0.9,
                             shape = "\u2605") +
      geom_point_interactive(data = data[highlight,] %>% 
                               filter(is_goal == "No goal"),
                             aes(x = x, 
                                 y = y, 
                                 color = team_name,
                                 fill = team_name,
                                 tooltip = tooltip, 
                                 data_id = id),
                             size = 4,
                             alpha = 0.9)
  }
  
  p <- p +
    scale_color_manual(values = c("England" = "#27408B", "Spain" = "#FF0000")) +
    scale_fill_manual(values = c("England" = "#27408B", "Spain" = "#FF0000")) +
    theme(plot.background = element_rect(color = "#e2f2e9", fill = "#e2f2e9"),
          panel.background = element_rect(color = "#e2f2e9", fill = "#e2f2e9")) +
    guides(color = "none", 
           fill = "none", 
           alpha = "none",
           size = "none",
           shape = "none")
  
  return(p)
}

create_goalmouth_map <- function(data, highlight = NULL){
  p <- ggplot() +
    annotate("rect", xmin = 54.8, xmax = 55.3, ymin = 0, ymax = 2.2, fill = "white") +
    annotate("rect", xmin = 45.2, xmax = 44.7, ymin = 0, ymax = 2.2, fill = "white") +
    annotate("rect", xmin = 44.7, xmax = 55.3, ymin = 2, ymax = 2.2, fill = "white") +
    geom_segment(aes(x = -Inf, y = 0, xend = Inf, yend = 0), color = "black", linewidth = 0.6) +
    geom_segment(aes(x = 55.3, y = 2.2, xend = 44.7, yend = 2.2), color = "black", linewidth = 0.6) +
    geom_segment(aes(x = 54.8, y = 2, xend = 45.2, yend = 2), color = "black", linewidth = 0.6) +
    geom_segment(aes(x = 55.3, y = 0, xend = 55.3, yend = 2.2), color = "black", linewidth = 0.6) +
    geom_segment(aes(x = 54.8, y = 0, xend = 54.8, yend = 2), color = "black", linewidth = 0.6) +
    geom_segment(aes(x = 45.2, y = 0, xend = 45.2, yend = 2), color = "black", linewidth = 0.6) +
    geom_segment(aes(x = 44.7, y = 0, xend = 44.7, yend = 2.2), color = "black", linewidth = 0.6) +
    scale_x_reverse() +
    theme(plot.background = element_blank(),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank()) +
    xlim(61, 39) +
    ylim(-1, 6)
  
  data$id <- rownames(data)
  data$tooltip <- c(paste0(data$minute, "' ", data$player_name, "\n ", 
                           "xGOT: ", data$xGOT))
  
  data <- data %>% 
    mutate(is_goal = ifelse(is_goal, "Goal", "No goal"),
           g_y = g_y/0.68, 
           g_z = g_z) %>% 
    filter(!is.na(xGOT))
  
  if(is.null(highlight)){
    p <- p +
      geom_point_interactive(data = data %>% 
                               filter(is_goal == "Goal"),
                             aes(x = g_y, 
                                 y = g_z, 
                                 color = team_name,
                                 fill = team_name,
                                 tooltip = tooltip, 
                                 data_id = id),
                             size = 6,
                             alpha = 0.7,
                             shape = "\u2605") +
      geom_point_interactive(data = data %>% 
                               filter(is_goal == "No goal"),
                             aes(x = g_y, 
                                 y = g_z, 
                                 color = team_name,
                                 fill = team_name,
                                 tooltip = tooltip, 
                                 data_id = id),
                             size = 5,
                             alpha = 0.7) 
  }
  else{
    p <- p +
      geom_point_interactive(data = data[highlight,] %>% 
                               filter(is_goal == "Goal"),
                             aes(x = g_y, 
                                 y = g_z, 
                                 color = team_name,
                                 fill = team_name,
                                 tooltip = tooltip, 
                                 data_id = id),
                             size = 6,
                             alpha = 0.9,
                             shape = "\u2605") +
      geom_point_interactive(data = data[highlight,] %>% 
                               filter(is_goal == "No goal"),
                             aes(x = g_y, 
                                 y = g_z, 
                                 color = team_name,
                                 fill = team_name,
                                 tooltip = tooltip, 
                                 data_id = id),
                             size = 5,
                             alpha = 0.9) 
  }
  
  p <- p +
    scale_color_manual(values = c("England" = "#27408B", "Spain" = "#FF0000")) +
    scale_fill_manual(values = c("England" = "#27408B", "Spain" = "#FF0000")) +
    labs(caption = "Data: OPTA via FotMob") +
    theme(plot.background = element_rect(color = "#e2f2e9", fill = "#e2f2e9"),
          panel.background = element_rect(color = "#e2f2e9", fill = "#e2f2e9"),
          plot.caption = element_text(family = "Exo 2",
                                      face = "italic",
                                      color = "#8B8B83")) +
    guides(color = "none", 
           fill = "none", 
           alpha = "none",
           size = "none")
  
  return(p)
}

create_shot_dashboard <- function(data, highlight = NULL){
  layout <- "AAAAA
  BBBCC"
  
  g <- girafe(ggobj = add_plot_title(create_xg_evolution_plot(data), " EURO 2024 Final", margin(0, 0, 10, 0)) / create_shotmap(data, highlight) + create_goalmouth_map(data, highlight) + plot_layout(heights = c(1.5, 1),
                                                                                                                                                                                                      design = layout) &
                plot_annotation(theme = theme(plot.background = element_rect(color = "#e2f2e9", fill = "#e2f2e9"),
                                              panel.background = element_rect(color = "#e2f2e9", fill = "#e2f2e9"))),
              width_svg = 10,
              height_svg = 8,
              options = list(
                opts_toolbar(
                  hidden = c("lasso_select", "lasso_deselect", "zoom_onoff",
                             "zoom_rect", "zoom_reset", "saveaspng"
                  )
                ),
                opts_sizing(rescale = F),
                opts_tooltip(use_fill = T,
                             css = paste0("font-family: Exo 2; padding:3pt; color:#ffffff; border-radius:5px")
                ),
                opts_hover(css = "stroke:black;stroke-width:1px;"),
                opts_hover_inv(css = "opacity:0.2;")
              )
  )
  
  return(g)
}
