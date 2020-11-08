


weo_a_oct_2020 <- read.delim(
  file    = "data/WEOOct2020alla.xls",
  skipNul = TRUE
) %>%
  as_tibble()



weo_a_oct_2019 <- read.delim(
  file    = "data/WEOOct2019alla.xls",
  skipNul = TRUE
) %>%
  as_tibble()


aggregates_of_interest <- c(205, 200, 110)

weo_a_oct_2020_1 <- weo_a_oct_2020 %>% 
  filter(
    WEO.Country.Group.Code %in% aggregates_of_interest
    & WEO.Subject.Code %in% c("LUR", "NGDP_RPCH", "GGXONLB_NGDP", "BCA_NGDPD", "PCPIEPCH", "GGXWDG_NGDP")
  ) %>% 
  mutate_at(vars(X1980:X2024), as.numeric) %>% 
  select(-Estimates.Start.After) %>% 
  pivot_longer(
    cols = X1980:X2024,
    names_to = "year",
    values_to = "value"
  ) %>% 
  mutate(year = as.integer(str_remove(year, "X"))) %>% 
  filter(year >= 2020) %>% 
  select(
    group = Country.Group.Name,
    variable_code = WEO.Subject.Code,
    variable = Subject.Descriptor,
    year,
    units = Units,
    scale = Scale,
    est_oct_2020 = value
  )




weo_a_oct_2019_1 <- weo_a_oct_2019 %>% 
  filter(
    WEO.Country.Group.Code %in% aggregates_of_interest
    & WEO.Subject.Code %in% c("LUR", "NGDP_RPCH", "GGXONLB_NGDP", "BCA_NGDPD", "PCPIEPCH", "GGXWDG_NGDP")
  ) %>% 
  mutate_at(vars(X1980:X2024), as.numeric) %>% 
  select(-Estimates.Start.After) %>% 
  pivot_longer(
    cols = X1980:X2024,
    names_to = "year",
    values_to = "value"
  ) %>% 
  mutate(year = as.integer(str_remove(year, "X"))) %>% 
  filter(year >= 2020) %>% 
  select(
    group = Country.Group.Name,
    variable_code = WEO.Subject.Code,
    variable = Subject.Descriptor,
    year,
    units = Units,
    scale = Scale,
    est_oct_2019 = value
  )



estimations_a <- full_join(weo_a_oct_2020_1, weo_a_oct_2019_1) %>%
  mutate(
    covid19_impact = est_oct_2020 - est_oct_2019
  )

