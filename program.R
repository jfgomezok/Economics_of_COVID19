
# 1) Load packages -----------------------------------------------------------

library(tidyverse)
library(glue)
library(ggrepel)
library(tidytext)
library(ggtext)

# 2) Import Data -------------------------------------------------------------

# Main

weo_oct_2020 <- read.delim(
    file    = "data/WEOOct2020all.xls",
    skipNul = TRUE
  ) %>%
  as_tibble()

class(weo_oct_2020)
attributes(weo_oct_2020)
glimpse(weo_oct_2020)

# Auxiliar

weo_oct_2019 <- read.delim(
  file    = "data/WEOOct2019all.xls",
  skipNul = TRUE
) %>%
  as_tibble()

class(weo_oct_2019)
attributes(weo_oct_2019)
glimpse(weo_oct_2019)

source("aggregates.R")


# 3) Parse Data --------------------------------------------------------------


# Extract the estimates in a new tibble

estimations_after <- weo_oct_2020 %>% 
  select(ISO, Estimates.Start.After)


# Numbers are numbers

weo_oct_2020_1 <- weo_oct_2020 %>% 
  select(-X) %>% 
  mutate_at(vars(X1980:X2025), as.numeric)


weo_oct_2019_1 <- weo_oct_2019 %>%  
  mutate_at(vars(X1980:X2024), as.numeric)


# Tidy format (longitudianal panel)

weo_oct_2020_2 <- weo_oct_2020_1 %>% 
  select(-Estimates.Start.After) %>% 
  pivot_longer(
    cols = X1980:X2025,
    names_to = "year",
    values_to = "value"
  )


weo_oct_2019_2 <- weo_oct_2019_1 %>% 
  select(-Estimates.Start.After) %>% 
  pivot_longer(
    cols = X1980:X2024,
    names_to = "year",
    values_to = "value"
  )



# Remove de "X" of the years

weo_oct_2020_3 <- weo_oct_2020_2 %>% 
  mutate(year = as.integer(str_remove(year, "X")))


weo_oct_2019_3 <- weo_oct_2019_2 %>% 
  mutate(year = as.integer(str_remove(year, "X")))


# CSC Countries -----------------------------------------------------------

csc_countries <- c("ARG", "BRA", "CHL", "PRY", "URY")

weo_oct_2020_4 <- weo_oct_2020_3 %>% 
  filter(ISO %in% csc_countries) %>% 
  select(
    ISO = ISO,
    country = Country,
    variable_code = WEO.Subject.Code,
    variable = Subject.Descriptor,
    year,
    units = Units,
    scale = Scale,
    value
  )


weo_oct_2019_4 <- weo_oct_2019_3 %>% 
  filter(ISO %in% csc_countries) %>% 
  select(
    ISO = ISO,
    country = Country,
    variable_code = WEO.Subject.Code,
    variable = Subject.Descriptor,
    year,
    units = Units,
    scale = Scale,
    value
  )


variables <- weo_oct_2020_4 %>% count(variable) %>% pull(variable)





# 4) Visualizations ------------------------------------------------------


weo_oct_2020_4 %>% count(variable, variable_code, units, scale) %>% view()


# Test chart
weo_oct_2020_4 %>% 
  filter(
    variable == "Gross domestic product, constant prices",
    units == "Percent change",
    year >= 2019
  ) %>% 
  ggplot(aes(x = year, y = value, color = country)) +
  geom_line()
  
# Test chart
weo_oct_2020_4 %>% 
  filter(
    variable %in% c("Unemployment rate", "Gross domestic product, constant prices")
    & str_detect(units, "Percent")
    & year %in% c(2019, 2020)
  ) %>% 
  select(-units, -variable_code) %>% 
  pivot_wider(names_from = variable, values_from = "value") %>%
  ggplot() +
  geom_point(
    mapping = aes(
      x=`Gross domestic product, constant prices`,
      y = `Unemployment rate`,
      color = country,
      shape = as.factor(year)
    ),
    show.legend = FALSE,
    size = 3
  ) +
  geom_line(
    mapping = aes(
      x=`Gross domestic product, constant prices`,
      y = `Unemployment rate`,
      color = country
    ),
    show.legend = FALSE,
    arrow = arrow(
      ends= "first",
      type = "closed",
      length = unit(0.08, "inches")
    )
  ) +
  geom_text_repel(
    mapping = aes(
      x=`Gross domestic product, constant prices`,
      y = `Unemployment rate`,
      label = ifelse(year == 2019, glue("{ISO}:{year}"), year),
      color = country
    ),
    size = 3,
    show.legend = FALSE
  ) +
  scale_shape_manual(
    values = c(15,19)
  ) +
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0) +
  labs(
    x = "GDP Growth (constant prices)",
    y = "Unemployment Rate (Percent of total labor force)",
    title = "Impact of COVID19 in Economic Growth and Labour Market",
    subtitle = "2019 vs. 2020"
  ) +
  theme_minimal(base_size = 10) +
  theme(
      panel.grid.minor = element_blank()
  )




# Test chart
weo_oct_2020_4 %>% 
  filter(
    variable %in% c("Unemployment rate", "Gross domestic product, constant prices")
    & str_detect(units, "Percent")
    & year %in% c(2019, 2020)
  ) %>% 
  select(-units, -variable_code) %>% 
  group_by(ISO, variable) %>% 
  mutate(change = value - lag(value)) %>% 
  drop_na() %>% 
  select(-value) %>% 
  pivot_wider(names_from = variable, values_from = change) %>% 
  ggplot() +
  geom_point(
    mapping = aes(
      x = `Unemployment rate`,
      y = `Gross domestic product, constant prices`,
      color = country
      # shape = as.factor(year)
    ),
    show.legend = FALSE,
    size = 3
  ) +
  # geom_line(
  #   mapping = aes(
  #     x=`Gross domestic product, constant prices`,
  #     y = `Unemployment rate`,
  #     color = country
  #   ),
  #   show.legend = FALSE,
  #   arrow = arrow(
  #     ends= "first",
  #     type = "closed",
  #     length = unit(0.08, "inches")
  #   )
  # ) +
  geom_text_repel(
    mapping = aes(
      x = `Unemployment rate`,
      y = `Gross domestic product, constant prices`,
      label = glue("{ISO}"),
      color = country
    ),
    size = 3,
    show.legend = FALSE
  ) +
  # scale_shape_manual(
  #   values = c(15,19)
  # ) +
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0) +
  scale_x_continuous(position = "top") +
  labs(
    x = "% Change in Unemployment Rate",
    y = "% Change in GDP Growth (constant prices)",
    title = "Impact of COVID19 in Economic Growth and Labour Market",
    subtitle = "2019 vs. 2020"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.minor = element_blank()
  )

weo_oct_2020_4 %>% count(variable_code, variable, scale, units) %>% view()

est_oct_2020 <- weo_oct_2020_4 %>% 
  filter(
    year >= 2020
    & variable_code %in% c("LUR", "NGDP_RPCH", "GGXONLB_NGDP", "BCA_NGDPD", "PCPIPCH", "GGXWDG_NGDP")
  ) %>%
  group_by(variable_code, year) %>% 
  summarise(
    est_oct_2020 = mean(value, na.rm = TRUE)
  )

est_oct_2019 <- weo_oct_2019_4 %>% 
  filter(
    year >= 2020
    & variable_code %in% c("LUR", "NGDP_RPCH", "GGXONLB_NGDP", "BCA_NGDPD", "PCPIPCH", "GGXWDG_NGDP")
  ) %>%
  group_by(variable_code, year) %>% 
  summarise(
    est_oct_2019 = mean(value, na.rm = TRUE)
  )





# Plot Nº1 ----------------------------------------------------------------



data_aggregates <- estimations_a %>% 
  filter(year == 2020) %>% 
  select(
    country = group,
    variable,
    variable_code,
    est_oct_19 = est_oct_2019,
    est_oct_20 = est_oct_2020,
    covid19_impact
  )
  
csc <- weo_oct_2020_4 %>% 
  filter(
    year == 2020
    & variable_code %in% c("LUR", "NGDP_RPCH", "GGXONLB_NGDP", "BCA_NGDPD", "PCPIEPCH", "GGXWDG_NGDP")
  ) %>% 
  select(country, variable, est_oct_20 = value) %>% 
  full_join(
    weo_oct_2019_4 %>% 
      filter(
        year == 2020
        & variable_code %in% c("LUR", "NGDP_RPCH", "GGXONLB_NGDP", "BCA_NGDPD", "PCPIEPCH", "GGXWDG_NGDP")
      ) %>% 
      select(country, variable, variable_code, est_oct_19 = value)
  ) %>% 
  mutate(
    covid19_impact = est_oct_20 - est_oct_19
  ) %>% 
  group_by(variable, variable_code) %>% 
  summarise(
    est_oct_19 = mean(est_oct_19, na.rm = TRUE),
    covid19_impact = mean(covid19_impact, na.rm = TRUE),
    est_oct_20 = mean(est_oct_20, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  mutate(
    country = "CSC"
  )


p1 <- weo_oct_2020_4 %>% 
  filter(
    year == 2020
    & variable_code %in% c("LUR", "NGDP_RPCH", "GGXONLB_NGDP", "BCA_NGDPD", "PCPIEPCH", "GGXWDG_NGDP")
  ) %>% 
  select(country, variable, est_oct_20 = value) %>% 
  full_join(
    weo_oct_2019_4 %>% 
      filter(
        year == 2020
        & variable_code %in% c("LUR", "NGDP_RPCH", "GGXONLB_NGDP", "BCA_NGDPD", "PCPIEPCH", "GGXWDG_NGDP")
      ) %>% 
      select(country, variable, variable_code, est_oct_19 = value)
  ) %>% 
  mutate(
    covid19_impact = est_oct_20 - est_oct_19
  ) %>% 
  rbind(data_aggregates) %>% 
  rbind(csc) %>% 
  mutate(
    ISO = case_when(
      country == "Argentina" ~ "ARG",
      country == "Brazil" ~ "BRA",
      country == "Paraguay" ~ "PRY",
      country == "Uruguay" ~ "URU",
      country == "Chile" ~ "CHL",
      country == "CSC" ~ "CSC",
      str_detect(country, "Latin America") ~ "LAC",
      str_detect(country, "Emerging") ~ "EME",
      str_detect(country, "Advanced") ~ "ADE"
    ),
    variable_to_show = case_when(
      variable_code == "LUR" ~ "<b>Desempleo<sup>(3)</sup></b><br>Porcentaje de la PEA",
      variable_code == "NGDP_RPCH" ~ "<b>Crecimiento del PBI</b><br>Precios constantes",
      variable_code == "GGXONLB_NGDP" ~ "<b>Resultado Primario del Sector Público</b><br>Porcentaje del PBI",
      variable_code == "BCA_NGDPD" ~ "<b>Resultado Cuenta Corriente</b><br>Porcentaje del PBI",
      variable_code == "PCPIEPCH" ~ "<b>Inflación<sup>(4)</sup></b><br>Variación Interanual, fin de período",
      variable_code == "GGXWDG_NGDP" ~ "<b>Deuda Bruta del Gobierno General</b><br>Porcentaje del PBI"
    )
  ) %>% 
  group_by(variable_to_show) %>% 
  mutate(
    variable_to_show = as.factor(variable_to_show),
    ISO = reorder_within(ISO, covid19_impact, variable_to_show)
  ) %>% 
  # arrange(desc(covid19_impact), .by_group = TRUE) %>% 
  drop_na() %>% 
  ggplot() +
  geom_col(
    mapping = aes(x =covid19_impact , y = ISO, fill = variable_to_show),
    show.legend = FALSE,
    alpha = 0.5
  ) +
  geom_text(
    data = . %>% filter(covid19_impact <0),
    mapping = aes(x =covid19_impact , y = ISO, label = format(round(covid19_impact,1), decimal.mark = ",")),
    show.legend = FALSE,
    hjust = 0,
    # fontface = "bold",
    color = "black"
  ) +
  geom_text(
    data = . %>% filter(covid19_impact >0),
    mapping = aes(x =covid19_impact , y = ISO, label = glue("+{format(round(covid19_impact,1), decimal.mark = ',')}")),
    show.legend = FALSE,
    hjust = 1,
    # fontface = "bold",
    color = "black"
  ) +
  scale_y_reordered() +
  # geom_vline(
  #   data = mydata,
  #   mapping = aes(x=covid19_impact)
  #   # xintercept = covid19_impact
  # ) +
  facet_wrap(~variable_to_show, scales = "free") +
  labs(
    x = "", y = "", title = "<b>Gráfico Nº1: Impacto de COVID-19 en países del Cono Sur<sup>(1)</sup> en términos comparativos<sup>(2)</sup><br>respecto a variables seleccionadas</b>",
    subtitle = "Metodología de cálculo: Cambio en las estimaciones del FMI para 2020, Oct.2020 vs. Oct.2019",
    caption = "(1) CSC = Países del Cono Sur | CHL = Chile | BRA = Brazil | URU = Uruguay | ARG = Argentina | PRY = Paraguay
    <br>(2) EME = Emerging market and developing economies | ADE = Advanced economies | LAC = Latin America and the Caribbean
    <br>(3) No se disponen de datos para LAC y EME
    <br>(4) No se disponen de datos para ARG
    <br>Fuente: Elaboración propia en base al FMI" 
  ) +
  geom_vline(
    xintercept = 0,
    color = "grey90"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_textbox(hjust = 0),
    plot.caption = element_textbox(hjust = 0),
    strip.text.x = element_textbox(halign = 0.5),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(face = "bold"),
    axis.text.x = element_blank()
  )

p1

ggsave(plot = p1,  filename = "plot1.png", width = 9, height = 6)




# Plot Nº2 ----------------------------------------------------------------



estimations <- full_join(est_oct_2020, est_oct_2019) %>%
  mutate(
    covid19_impact = est_oct_2020 - est_oct_2019,
    variable_to_show = case_when(
      variable_code == "LUR" ~ "<b>Desempleo</b><br>Porcentaje de la PEA",
      variable_code == "NGDP_RPCH" ~ "<b>Crecimiento del PBI</b><br>Precios constantes",
      variable_code == "GGXONLB_NGDP" ~ "<b>Resultado Primario del Sector Público</b><br>Porcentaje del PBI",
      variable_code == "BCA_NGDPD" ~ "<b>Resultado Cuenta Corriente</b><br>Porcentaje del PBI",
      variable_code == "PCPIPCH" ~ "<b>Inflación<sup>(2)</sup></b><br>Variación Interanual, fin de período",
      variable_code == "GGXWDG_NGDP" ~ "<b>Deuda Bruta del Gobierno General</b><br>Porcentaje del PBI"
    )
  )


p2 <- ggplot() +
  geom_col(
    data = estimations,
    mapping = aes(x = year, y = covid19_impact, fill = variable_to_show),
    show.legend = FALSE,
    alpha = 0.5
  ) +
  geom_text(
    data = estimations %>% filter(covid19_impact >0),
    mapping = aes(
      x = year,
      y = covid19_impact,
      label = glue('+{format(round(covid19_impact,1), decimal.mark = ",")}')
    ),
    vjust = 1,
    # fontface = "bold",
    show.legend = FALSE
  ) +
  geom_text(
    data = estimations %>% filter(covid19_impact <0),
    mapping = aes(
      x = year,
      y = covid19_impact,
      label = format(round(covid19_impact,1), decimal.mark = ",")
    ),
    vjust = 0,
    # fontface = "bold",
    show.legend = FALSE
  ) +
  facet_wrap(~variable_to_show, scales = "free_y") +
  labs(
    x = "", y = "", title = "<b>Gráfico Nº2: Evolución tempral del Impacto de COVID-19 en países del Cono Sur<sup>(1)</sup> respecto <br>a variables seleccionadas",
    subtitle = "Metodología de cálculo: Cambio en las estimaciones del FMI para 2020, Oct.2020 vs. Oct.2019.",
    caption = "(1) Países del Cono Sur: Promedio simple de Chile, Brazil, Uruguay, Argentina, Paraguay.
    <br>(2) No se disponen de datos para ARG
    <br>Fuente: Elaboración propia en base al FMI" 
  ) +
  geom_hline(
    yintercept = 0,
    color = "grey90"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_textbox(hjust = 0),
    plot.caption = element_textbox(hjust = 0),
    strip.text.x = element_textbox(halign = 0.5),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.y = element_text(face = "bold")
  )


p2 

ggsave(filename = "plot2.png", plot = p2, width = 9, height = 6)
