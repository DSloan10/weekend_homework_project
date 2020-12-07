meteorite_data <- read_csv("meteorite_landings.csv") %>%clean_names

renamed_meteorite_data <-
  meteorite_data %>%
  rename("fell_or_found" = fall) %>% 
  rename("year_of_discovery" = year) 

q1_1_3_split <-
  renamed_meteorite_data %>%
  separate(geo_location, c("latitude", "longitud"), sep=",")

q1_1_3_split <-
  q1_1_3_split %>% 
  mutate(latitude = str_replace(latitude, "[(]", "")) %>%
  mutate(longitud = str_replace(longitud, "[)]", "")) 

q1_1_3_split <-
  q1_1_3_split %>%
  mutate(latitude = as.numeric(latitude)) %>% 
  mutate(longitud = as.numeric(longitud))

q1_1_3_split %>% 
  summarise(geo_nas = sum(is.na (c(latitude, longitud))))

q1_1_4_na_clean <-
  q1_1_3_split %>%
  mutate(latitude = coalesce(latitude, 0, na.rm = TRUE)) %>%
  mutate(longitud = coalesce(longitud, 0, na.rm = TRUE))

q1_1_5_remove_small_meteorites <-
  q1_1_4_na_clean %>%
  filter(mass_g >= 1000)

q1_1_6_arrange_by_year <-
  q1_1_5_remove_small_meteorites %>%
  arrange(year_of_discovery)

check_lat_long <- function(lat_long_num){
  lat_long_num %>% 
    verify(latitude >= -90 & latitude <= 90) %>%
    verify(longitud >= -180 & longitud <= 180)
  return(list(lat_long_num))
}

cleaned_meteorite_data <- q1_1_6_arrange_by_year

cleaned_meteorite_data %>% 
  write_csv(file = "cleaned_meteorite_data.csv")
