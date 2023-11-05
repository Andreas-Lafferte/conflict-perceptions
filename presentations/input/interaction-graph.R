ggpredict(model_6, terms = c("C_RATIO", "CLASS"), type = "fe") %>% 
  filter(group %in% c("1.Capitalistas", "9.Trabajadores no calificados")) %>% 
  plot(show.title = FALSE, 
       line.size = 1.1, 
       ci.style="ribbon", 
       alpha = 0.15)+
  scale_y_continuous(limits = c(0, 13), 
                     breaks = seq(0, 13, 2.5),
                     labels = formatter(nsmall = 1))+
  labs(x = "Ratio 80/20")+
  my_pretty_theme
  
ggsave(plot = last_plot(),
       filename = "C:/Users/Usuario/Documents/GitHub/thesis-presentations/examen-titulo/images/interaction-clasesmedias.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 33,
       height = 20)


windowsFonts(`Roboto Condensed` = windowsFont("Roboto Condensed"))

my_pretty_theme <- theme_minimal(base_family = "Roboto Condensed", base_size = 14) +
  theme(panel.grid.minor = element_blank(),
        # Bold, bigger title
        plot.title = element_text(face = "bold", size = rel(1.7)),
        # Plain, slightly bigger subtitle that is grey
        plot.subtitle = element_text(face = "italic", size = rel(0.85), color = "grey30"),
        # Italic, smaller, grey caption that is left-aligned
        plot.caption = element_text(face = "italic", size = rel(0.7), 
                                    color = "grey70", hjust = 0),
        # Bold legend titles
        legend.title = element_text(face = "bold", size = rel(1.1)),
        # Bold, slightly larger facet titles that are left-aligned for the sake of repetition
        strip.text = element_text(face = "bold", size = rel(0.7), hjust = 0.5),
        # Bold axis titles
        axis.title = element_text(face = "bold", size = rel(1.2)),
        # Add some space above the x-axis title and make it left-aligned
        axis.title.x = element_text(margin = margin(t = 5)),
        # Add some space to the right of the y-axis title and make it top-aligned
        axis.title.y = element_text(margin = margin(r = 5)),
        # Add a light grey background to the facet titles, with no borders
        strip.background = element_rect(fill = "grey90", color = NA),
        # Add a thin grey border around all the plots to tie in the facet titles
        panel.border = element_rect(color = "grey90", fill = NA),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13))
