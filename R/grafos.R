# Obtener los valores p del modelo

p_values <- p_value(model_4)

# Crear el gráfico con etiquetas numéricas
coefs <- broom.mixed::tidy(model_4, conf.int = TRUE)
coefs <- coefs %>% 
  filter(effect == "fixed")

coefs$p.value <- p_values$p
coefs <- coefs %>% 
  mutate(color = ifelse(estimate < 0, "Negative", "Positive"),
         sig = case_when(p.value <= 0.001 ~ "***",
                         p.value <= 0.01 ~ "**",
                         p.value <= 0.05 ~ "*",
                         TRUE ~ ""))

coefs[2:12,] %>% 
  mutate(term = case_when(str_detect(term, "class") ~ str_remove(term, "class"),
                              term == "unionSi" ~ "Miembro sindicato",
                              term == "z_top10" ~ "Participación Top 10% ingresos",
                              term == "corp_all" ~ "Índice Corporativismo"),
         term = factor(term, levels = rev(unique(term)))) %>% 
  ggplot(aes(x = estimate, y = term)) +
  geom_vline(aes(xintercept = 0), color = "black", size = 0.8, alpha = 0.8, linetype = "dotted") +
  geom_point(aes(x = estimate, color = color), size = 2.5) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high, color = color), 
                width = 0, size = 0.7, position = position_dodge2()) +
  scale_x_continuous(limits = c(-2, 1),
                     breaks = seq(-2,1,0.5)) +
  scale_color_manual(values = c("Negative" = "firebrick", "Positive" = "#084387")) +  # Asigne colores a valores positivos y negativos
  geom_text(aes(label = paste(round(estimate, 2), sig, sep = " "), color = color), 
            vjust = -0.5,
            size = 4) +
  theme_minimal()+
  theme(text = element_text(family = "Helvetica Neue"))



plot_model(model_4)

ggpredict(model_6_c, terms = c("c_top10", 
                             "class")) %>% 
  filter(group %in% c("1.Capitalistas", "9.Trabajadores no calificados")) %>% 
  plot() +
  theme_minimal()+
  theme(text = element_text(family = "Helvetica Neue"))

loadfonts()

theme_fancy <- function() {
  theme_minimal(base_family = "Poppins Medium", base_size = 12) +
    theme(panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "white", color = NA),
          plot.title = element_text(face = "bold"),
          axis.title = element_text(face = "bold"),
          strip.text = element_text(face = "bold", size = rel(0.8), hjust = 0),
          strip.background = element_rect(fill = "grey80", color = NA),
          legend.title = element_text(face = "bold"),
          plot.caption = element_text(size = rel(0.8), 
                                      hjust = 1))
}

my_pretty_theme <- function(){
  theme_minimal(base_family = "Helvetica Neue", base_size = 12) +
    theme(panel.grid.minor = element_blank(),
          # Bold, bigger title
          plot.title = element_text(face = "bold", size = rel(1.7)),
          # Plain, slightly bigger subtitle that is grey
          plot.subtitle = element_text(face = "italic", size = rel(0.85), color = "grey30"),
          # Italic, smaller, grey caption that is left-aligned
          plot.caption = element_text(size = rel(0.7), 
                                      hjust = 1),
          # Bold legend titles
          legend.title = element_text(face = "bold", size = rel(0.7)),
          # Bold, slightly larger facet titles that are left-aligned for the sake of repetition
          strip.text = element_text(face = "bold", size = rel(0.7), hjust = 0),
          # Bold axis titles
          axis.title = element_text(face = "bold", size = rel(0.85)),
          # Add some space above the x-axis title and make it left-aligned
          axis.title.x = element_text(margin = margin(t = 5)),
          # Add some space to the right of the y-axis title and make it top-aligned
          axis.title.y = element_text(margin = margin(r = 5)),
          # Add a light grey background to the facet titles, with no borders
          strip.background = element_rect(fill = "grey80", color = NA))
  
  
}



db %>% 
  group_by(country, year) %>% 
  summarise(promedio = round(mean(psci), digits = 1)) %>% 
  ggplot(aes(x = year, y = promedio, group = country)) +
  geom_hline(aes(yintercept = 3.9), linetype = "dashed", color = "#bc3032") +
  geom_line(aes(group = country)) +
  geom_point() +
  scale_x_continuous(breaks=seq(1999, 2019, 10)) +
  scale_y_continuous(labels = formatter(nsmall = 1)) +
  labs(x = "Año", 
       y = "Promedio PSCi",
       caption = "Fuente: Elaboración propia en base a ISSP (1999-2019)") +
  facet_wrap(.~country, ncol = 5, scales = "fixed") +
  theme_fancy()


ggsave(plot = last_plot(),
       filename = here("presentations","images","evol.png"),
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 33,
       height = 20)

theme_fancy <- function() {
  theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "white", color = NA),
          plot.title = element_text(face = "bold"),
          axis.title = element_text(face = "bold"),
          strip.text = element_text(face = "bold", size = rel(0.8), hjust = 0),
          strip.background = element_rect(fill = "grey80", color = NA),
          legend.title = element_text(face = "bold"),
          plot.caption = element_text(size = rel(0.8), 
                                      hjust = 1),
          axis.title.x = element_text(margin = margin(t = 5)),
          # Add some space to the right of the y-axis title and make it top-aligned
          axis.title.y = element_text(margin = margin(r = 5)))
}


db %>% 
  group_by(country, year) %>% 
  summarise(promedio = round(mean(psci), digits = 1)) %>% 
  ggplot(aes(x = year, y = promedio, group = 1)) +
  geom_hline(aes(yintercept = 3.9), linetype = "dashed", color = "#bc3032") +
  geom_line(aes(group = 1)) +
  geom_point() +
  scale_x_continuous(breaks=seq(1999, 2019, 10)) +
  scale_y_continuous(labels = formatter(nsmall = 1)) +
  labs(x = "Año", 
       y = "Promedio PSCi",
       caption = "Fuente: Elaboración propia en base a ISSP (1999-2019)") +
  facet_wrap(.~country, ncol = 5, scales = "fixed") +
  theme_fancy()


ggsave(plot = last_plot(),
       filename = here("presentations","images","f1.png"),
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 33,
       height = 20)
