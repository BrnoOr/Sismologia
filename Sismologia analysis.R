pacman::p_load(
  tidyverse,
  rio,
  gg3D,
  plotly
)

data <- import('Sismologia_Chile_2000-2024.csv')

data %>% is.na() %>% colSums()

data %>% filter(!is.na(N))


data <- data %>% 
  mutate(lat = Latitud,
         lng = ifelse(is.na(Longitud),N,Longitud),
         prof = as.numeric(str_extract(Profundidad,"-?\\d+\\.\\d+|\\d+")),
         mag = Magnitud) %>% 
  select(V1,
         date_h =`Fecha UTC`,
         lat,
         lng,
         prof,
         mag
         )

# Eda ####

data %>% 
  ggplot(aes(x = prof)) +
  geom_bar()

data %>% 
  ggplot(aes(x = prof)) +
  geom_histogram(binwidth = 5)

data %>% 
  ggplot(aes(x = mag)) +
  geom_histogram(aes(y=..density..),binwidth = .1)+
  geom_density()

ggplotly(
  data %>% 
    ggplot(aes(x = lat,y = lng, z = mag))+
    geom_contour_filled()+
    theme_minimal()
)

ggplotly(
  data %>% 
    group_by(lat,lng) %>% 
    summarise(N = n()) %>% 
    ggplot(aes(x = lat,y = lng,z = N))+
    geom_contour_filled()+
    theme_minimal()
)
