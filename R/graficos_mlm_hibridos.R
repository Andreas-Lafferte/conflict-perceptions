## Graficos de modelos

loadfonts()

my_pretty_theme <- function(){
  theme_minimal(base_size = 16) +
    theme(panel.grid.minor = element_blank(),
          # Bold, bigger title
          plot.title = element_text(face = "bold", size = rel(1.7)),
          # Plain, slightly bigger subtitle that is grey
          plot.subtitle = element_text(face = "italic", size = rel(0.85), color = "grey20"),
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


graphs_m1 <- sjPlot::plot_model(model_1,
                                type = "re",
                                vline.color = "darkorange",
                                grid = F,
                                sort.est = "sort.all",
                                ci.lvl = .95,
                                colors = "black")


graphs_m1[[1]] +
  labs(title = "Efectos aleatorios para año-país (Intercepto)",
       y = "Coeficientes estimados") +
  my_pretty_theme() +
  theme(legend.position = "none",
        axis.text = element_text(face = "bold"),
        text = element_text(size = 18)) 

ggsave(plot = last_plot(),
       filename = here("presentations","images","modelo0_country_wave.png"),
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 33,
       height = 20) 

graphs_m1[[2]] +
  labs(title = "Efectos aleatorios para país (Intercepto)",
       y = "Coeficientes estimados") +
  my_pretty_theme() +
  theme(legend.position = "none",
        axis.text = element_text(face = "bold"),
        text = element_text(size = 18)) 

ggsave(plot = last_plot(),
       filename = here("presentations","images","modelo0_country.png"),
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 33,
       height = 20)

# Obtener los valores p del modelo
p_values <- p_value(model_2)

# Crear el gráfico con etiquetas numéricas
coefs <- broom.mixed::tidy(model_2, conf.int = TRUE)
coefs <- coefs %>% 
  filter(effect == "fixed")

coefs$p.value <- p_values$p
coefs <- coefs %>% 
  mutate(color = ifelse(estimate < 0, "Negative", "Positive"),
         sig = case_when(p.value <= 0.001 ~ "***",
                         p.value <= 0.01 ~ "**",
                         p.value <= 0.05 ~ "*",
                         TRUE ~ ""))

coefs[2:3,] %>% 
  mutate(term = case_when(term == "egpClase intermedia (III+IV)" ~ "Clase Intermedia (III+IV)",
                          term == "egpClase trabajadora (V+VI+VII)" ~ "Clase Trabajadora (III+IV)"),
         term = factor(term, levels = rev(unique(term)))) %>% 
  ggplot(aes(x = estimate, y = term)) +
  geom_vline(aes(xintercept = 0), color = "darkorange", size = 0.5, alpha = 0.8, linetype = "solid") +
  geom_point(aes(x = estimate, color = color), size = 3.5) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high, color = color), 
                width = 0, size = 1, position = position_dodge2()) +
  scale_x_continuous(limits = c(-1, 1),
                     breaks = seq(-1,1,0.5)) +
  scale_color_manual(values = c("Negative" = "black", "Positive" = "black")) +  # Asigne colores a valores positivos y negativos
  geom_text(aes(label = paste(round(estimate, 2), sig, sep = " "), color = color), 
            vjust = -1,
            hjust = -0.1,
            size = 5) +
  labs(x = "Coeficientes estimados",
       y = NULL) + 
  my_pretty_theme() +
  theme(legend.position = "none",
        axis.text = element_text(face = "bold"),
        text = element_text(size = 18)) 


ggsave(plot = last_plot(),
       filename = here("presentations","images","modelo2_mlm_hyb.png"),
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 33,
       height = 20)


# Obtener los valores p del modelo
p_values <- p_value(model_3)

# Crear el gráfico con etiquetas numéricas
coefs <- broom.mixed::tidy(model_3, conf.int = TRUE)
coefs <- coefs %>% 
  filter(effect == "fixed")

coefs$p.value <- p_values$p
coefs <- coefs %>% 
  mutate(color = ifelse(estimate < 0, "Negative", "Positive"),
         sig = case_when(p.value <= 0.001 ~ "***",
                         p.value <= 0.01 ~ "**",
                         p.value <= 0.05 ~ "*",
                         TRUE ~ ""))

coefs[c(15:19),] %>% 
  mutate(term = case_when(term == "top10_be" ~ "Participación 10% superior [BE]",
                          term == "top10_we" ~ "Participación 10% superior [WE]",
                          term == "mean_corp_all" ~ "Corporativismo",
                          term == "wave2009" ~ "Ola 2009",
                          term == "wave2019" ~ "Ola 2019"),
         term = factor(term, levels = rev(unique(term)))) %>% 
  ggplot(aes(x = estimate, y = term)) +
  geom_vline(aes(xintercept = 0), color = "darkorange", size = 0.5, alpha = 0.8, linetype = "solid") +
  geom_point(aes(x = estimate, color = color), size = 3.5) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high, color = color), 
                width = 0, size = 1, position = position_dodge2()) +
  scale_x_continuous(n.breaks = 5) +
  scale_color_manual(values = c("Negative" = "black", "Positive" = "black")) +  # Asigne colores a valores positivos y negativos
  geom_text(aes(label = paste(round(estimate, 2), sig, sep = " "), color = color), 
            vjust = -1,
            hjust = -0.1,
            size = 5) +
  labs(x = "Coeficientes estimados",
       y = NULL) + 
  my_pretty_theme() +
  theme(legend.position = "none",
        axis.text = element_text(face = "bold"),
        text = element_text(size = 18)) 

ggsave(plot = last_plot(),
       filename = here("presentations","images","modelo3_mlm_hyb.png"),
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 33,
       height = 20)

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

coefs[c(15:21),] %>% 
  mutate(term = case_when(str_detect(term, "egp") ~ str_remove(term, "egp"),
                          term == "top10_be" ~ "Participación 10% superior [BE]",
                          term == "top10_we" ~ "Participación 10% superior [WE]",
                          term == "mean_corp_all" ~ "Corporativismo",
                          term == "wave2009" ~ "Ola 2009",
                          term == "wave2019" ~ "Ola 2019",
                          term == "gdp_be" ~ "GDP [BE]",
                          term == "gdp_we" ~ "GDP [WE]"),
         term = factor(term, levels = rev(unique(term)))) %>% 
  ggplot(aes(x = estimate, y = term)) +
  geom_vline(aes(xintercept = 0), color = "darkorange", size = 0.5, alpha = 0.8, linetype = "solid") +
  geom_point(aes(x = estimate, color = color), size = 3.5) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high, color = color), 
                width = 0, size = 1, position = position_dodge2()) +
  scale_x_continuous(n.breaks = 5) +
  scale_color_manual(values = c("Negative" = "black", "Positive" = "black")) +  # Asigne colores a valores positivos y negativos
  geom_text(aes(label = paste(round(estimate, 2), sig, sep = " "), color = color), 
            vjust = -1,
            hjust = -0.1,
            size = 5) +
  labs(x = "Coeficientes estimados",
       y = NULL) + 
  my_pretty_theme() +
  theme(legend.position = "none",
        axis.text = element_text(face = "bold"),
        text = element_text(size = 18)) 

ggsave(plot = last_plot(),
       filename = here("presentations","images","modelo4_mlm_hyb.png"),
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 33,
       height = 20)


### Interacciones

# Obtener los valores p del modelo
p_values <- p_value(model_6)

# Crear el gráfico con etiquetas numéricas
coefs <- broom.mixed::tidy(model_6, conf.int = TRUE)
coefs <- coefs %>% 
  filter(effect == "fixed")

coefs$p.value <- p_values$p
coefs <- coefs %>% 
  mutate(color = ifelse(estimate < 0, "Negative", "Positive"),
         sig = case_when(p.value <= 0.001 ~ "***",
                         p.value <= 0.01 ~ "**",
                         p.value <= 0.05 ~ "*",
                         TRUE ~ ""))

coefs[c(22:25),] %>% 
  mutate(term = case_when(term == "egpClase intermedia (III+IV):top10_be" ~ "Clase Intermedia (III+IV) x Participación 10% superior [BE]",
                          term == "egpClase trabajadora (V+VI+VII):top10_be" ~ "Clase Trabajadora (III+IV) x Participación 10% superior [BE]",
                          term == "egpClase intermedia (III+IV):top10_we" ~ "Clase Intermedia (III+IV) x Participación 10% superior [WE]",
                          term == "egpClase trabajadora (V+VI+VII):top10_we" ~ "Clase Trabajadora (III+IV) x Participación 10% superior [WE]"),
         term = factor(term, levels = rev(unique(term)))) %>% 
  ggplot(aes(x = estimate, y = term)) +
  geom_vline(aes(xintercept = 0), color = "darkorange", size = 0.5, alpha = 0.8, linetype = "solid") +
  geom_point(aes(x = estimate, color = color), size = 3.5) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high, color = color), 
                width = 0, size = 1, position = position_dodge2()) +
  scale_x_continuous(n.breaks = 5) +
  scale_color_manual(values = c("Negative" = "black", "Positive" = "black")) +  # Asigne colores a valores positivos y negativos
  geom_text(aes(label = paste(round(estimate, 2), sig, sep = " "), color = color), 
            vjust = -1,
            hjust = 1,
            size = 5) +
  labs(x = "Coeficientes estimados",
       y = NULL) + 
  my_pretty_theme() +
  theme(legend.position = "none",
        axis.text = element_text(face = "bold"),
        text = element_text(size = 18)) 

ggsave(plot = last_plot(),
       filename = here("presentations","images","modelo6_mlm_hyb.png"),
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 33,
       height = 20)

marginaleffects::plot_slopes(model = model_6,
                             variables = "egp",
                             condition = "top10_be",
                             conf_level = .95) +
  geom_hline(yintercept = 0,
             color = "red",
             linetype = "dashed") +
  labs(x = "Participación 10% superior [BE]",
       y = "Efecto marginal de Clase Social",
       fill = NULL,
       caption = "Fuente: Elaboración propia en base a ISSP (1999-2019) y WID
       Intervalos de confianza al 95%") +
  my_pretty_theme() +
  theme(legend.position = "none")
  
ggsave(plot = last_plot(),
       filename = here("presentations","images","figure02.png"),
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 33,
       height = 20)


predictions(model_6,
            condition = c("top10_be", "egp"),
            newdata = datagrid(top10_be = seq(min(db$top10_be), max(db$top10_be), by= 0.1),
                               egp = levels(db$egp)),
            re.form = NA) %>% 
  ggplot(aes(y =estimate,x=top10_be, fill=egp, color=egp,ymin=conf.low, ymax=conf.high)) +
  geom_ribbon(alpha=0.8,size=1,linetype="solid",color="black") +
  geom_line(size=1,color="black") +
  geom_point(size=2,color="black") + 
  facet_wrap(~egp) +
  scale_fill_manual(values=c("#323232","#989898","#CCCCCC"))+
  scale_color_manual(values=c("#323232","#989898","#CCCCCC")) +
  scale_y_continuous(n.breaks = 10) +
  labs(x = "Participación 10% superior [BE]",
       y = "Perceived Social Conflict Index",
       fill = NULL,
       caption = "Fuente: Elaboración propia en base a ISSP (1999-2019) y WID
       Estimaciones predichas con intervalos de confianza al 95%") +
  theme(legend.position = "none")

ggsave(plot = last_plot(),
       filename = here("presentations","images","figure03.png"),
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 33,
       height = 20)

#########

marginaleffects::plot_slopes(model = model_6,
                             variables = "egp",
                             condition = "top10_be",
                             conf_level = .95) 

predictions(model_6,
            condition = c("top10_be", "egp"),
            newdata = datagrid(top10_be = seq(min(db$top10_be), max(db$top10_be), by= 0.1),
                               egp = levels(db$egp)),
            re.form = NA) %>% 
  ggplot(aes(y =estimate,x=top10_be, fill=egp, color=egp,ymin=conf.low, ymax=conf.high)) +
  geom_ribbon(alpha=0.8,size=1,linetype="solid",color="black") +
  geom_line(size=1,color="black") +
  geom_point(size=2,color="black") + 
  facet_wrap(~egp) +
  scale_fill_manual(values=c("#323232","#989898","#CCCCCC"))+
  scale_color_manual(values=c("#323232","#989898","#CCCCCC")) +
  scale_y_continuous(n.breaks = 10) +
  labs(x = "Participación 10% superior [BE]",
       y = "Perceived Social Conflict Index",
       fill = NULL,
       caption = "Fuente: Elaboración propia en base a ISSP (1999-2019) y WID
       Estimaciones predichas con intervalos de confianza al 95%") +
  theme(legend.position = "none")



ggsave(plot = last_plot(),filename = "figure02.png",device = "png",
       path = here::here("output/images"))

