##libraries
library(tidycensus)
library(tidyverse)
library(sf)
library(tigris)
library(ggplot2)

options(tigris_use_cache = TRUE)
census_api_key("ef68c3ab06baab417502834cb453d32c2144f4da", install = TRUE, overwrite = TRUE)

tract_pop <- get_decennial(
  geography = "tract",
  variables = "P1_001N",
  state = "TX",
  year = 2020,
  geometry = TRUE
)

msa <- core_based_statistical_areas(cb = TRUE, year = 2020) %>%
  filter(NAME == "Austin-Round Rock-Georgetown, TX")

austin_tracts <- tract_pop %>%
  st_transform(st_crs(msa)) %>%
  st_intersection(msa)

austin_tracts <- austin_tracts %>%
  st_transform(5070) %>%
  mutate(
    population = value,
    area_km2 = as.numeric(st_area(geometry)) / 1000000,
    density_km2 = population / area_km2
  )

austin_tracts <- austin_tracts %>%
  mutate(
    landscape = case_when(
      density_km2 < 250 ~ "Exurban",
      density_km2 >= 250 & density_km2 < 550 ~ "Suburban low",
      density_km2 >= 550 & density_km2 < 800 ~ "Suburban high",
      density_km2 >= 800 & density_km2 < 1900 ~ "Urban low",
      density_km2 >= 1900 ~ "Urban high"
    )
  )

landscape_map <- ggplot(austin_tracts) +
  geom_sf(aes(fill = landscape), color = "white", linewidth = 0.05) +
  scale_fill_brewer(palette = "Spectral") +
  labs(
    title = "Austin Metropolitan Landscape Types",
    subtitle = "Population density classifications based on Hanberry thresholds",
    fill = "Landscape"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 8),
    legend.title = element_text(face = "bold"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank()
)
landscape_map

ggsave(
  "figures/austin_landscape_map.png",
  landscape_map,
  width = 10,
  height = 8,
  dpi = 300
)

##Median household income
income <- get_acs(
  geography = "tract",
  variables = "B19013_001",
  state = "TX",
  year = 2020,
  survey = "acs5",
  geometry = FALSE
)

austin_income <- austin_tracts %>%
  left_join(
    income %>%
      select(GEOID, estimate),
    by = "GEOID"
  ) %>%
  rename(median_income = estimate)

income_chart <- austin_income %>%
  st_drop_geometry() %>%
  group_by(landscape) %>%
  summarize(
    median_income = median(median_income, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = landscape, y = median_income, fill = landscape)) +
  geom_col() +
  scale_fill_brewer(palette = "Spectral") +
  labs(
    title = "Median Household Income by Landscape",
    x = "Landscape",
    y = "Median household income"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 15, hjust = 1)
  )

income_chart

ggsave(
  "figures/austin_income_chart.png",
  income_chart,
  width = 8,
  height = 6,
  dpi = 300
)

##second map
education <- get_acs(
  geography = "tract",
  variables = c(
    education_total = "B15003_001",
    bachelors = "B15003_022",
    masters = "B15003_023",
    professional = "B15003_024",
    doctorate = "B15003_025"
  ),
  state = "TX",
  year = 2020,
  survey = "acs5",
  geometry = FALSE
)

education_wide <- education %>%
  select(GEOID, variable, estimate) %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  mutate(
    college_total = bachelors + masters + professional + doctorate,
    college_share = college_total / education_total
  )

austin_education <- austin_tracts %>%
  left_join(education_wide, by = "GEOID")

college_map <- ggplot(austin_education) +
  geom_sf(aes(fill = college_share), color = NA) +
  scale_fill_viridis_c(
    labels = scales::percent,
    na.value = "gray90"
  ) +
  labs(
    title = "College Degree Attainment in Austin",
    subtitle = "Share of adults age 25+ by census tract",
    fill = "Share"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank()
  )
college_map

ggsave(
  "figures/austin_college_degree_map.png",
  college_map,
  width = 9,
  height = 7,
  dpi = 300
)

##population pyramids
age_data <- get_acs(
  geography = "tract",
  variables = c(
    male_under_18 = "B01001_003",
    male_18_24 = "B01001_007",
    male_25_34 = "B01001_011",
    male_35_44 = "B01001_013",
    male_45_54 = "B01001_015",
    male_55_64 = "B01001_017",
    male_65_plus = "B01001_020",
    female_under_18 = "B01001_027",
    female_18_24 = "B01001_031",
    female_25_34 = "B01001_035",
    female_35_44 = "B01001_037",
    female_45_54 = "B01001_039",
    female_55_64 = "B01001_041",
    female_65_plus = "B01001_044"
  ),
  state = "TX",
  county = c("Travis", "Williamson", "Hays", "Bastrop", "Caldwell"),
  year = 2020,
  survey = "acs5",
  geometry = FALSE
)

age_labels <- tibble(
  variable = c(
    "male_under_18", "male_18_24", "male_25_34", "male_35_44",
    "male_45_54", "male_55_64", "male_65_plus",
    "female_under_18", "female_18_24", "female_25_34", "female_35_44",
    "female_45_54", "female_55_64", "female_65_plus"
  ),
  sex = c(rep("Male", 7), rep("Female", 7)),
  age_group = rep(c(
    "Under 18", "18 to 24", "25 to 34", "35 to 44",
    "45 to 54", "55 to 64", "65+"
  ), 2)
)

tract_landscapes <- austin_tracts %>%
  st_drop_geometry() %>%
  select(GEOID, landscape) %>%
  mutate(
    broad_landscape = case_when(
      landscape %in% c("Urban low", "Urban high") ~ "Urban",
      landscape %in% c("Suburban low", "Suburban high") ~ "Suburban",
      TRUE ~ "Other"
    )
  )

pyramid_data <- age_data %>%
  select(GEOID, variable, estimate) %>%
  inner_join(age_labels, by = "variable") %>%
  left_join(tract_landscapes, by = "GEOID") %>%
  filter(broad_landscape %in% c("Urban", "Suburban")) %>%
  group_by(broad_landscape, sex, age_group) %>%
  summarize(population = sum(estimate, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    population_plot = if_else(sex == "Male", -population, population),
    age_group = factor(
      age_group,
      levels = c("Under 18", "18 to 24", "25 to 34", "35 to 44",
                 "45 to 54", "55 to 64", "65+")
    )
  )

pyramid_plot <- ggplot(pyramid_data, aes(x = age_group, y = population_plot, fill = sex)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ broad_landscape) +
  scale_y_continuous(labels = abs) +
  labs(
    title = "Urban and Suburban Population Pyramids",
    x = "Age group",
    y = "Population",
    fill = "Sex"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold")
  )
pyramid_plot

ggsave(
  "figures/austin_population_pyramids.png",
  pyramid_plot,
  width = 10,
  height = 7,
  dpi = 300
)
