# =============================================================================
# Richmond, VA MSA - Landscape Classification (Hanberry Table 4)
# Course assignment: 2020 Census + ACS 5-year (2016-2020)
# =============================================================================


library(tidycensus)
library(tidyverse)
library(sf)
library(tmap)
library(tigris)
library(scales)

# Put your Census API key here (only need to run once on your machine):
# census_api_key("YOUR_KEY_HERE", install = TRUE)

options(tigris_use_cache = TRUE)
sf::sf_use_s2(FALSE)   # avoids spherical-geometry headaches when filtering

# Make an output folder for figures
dir.create("figures", showWarnings = FALSE)

# =============================================================================
# 1. HANBERRY TABLE 4 THRESHOLDS  (Worldpop, people per km^2)
# =============================================================================
# Hanberry (2022) Table 4, Worldpop classification:
#   Wildlands     :    0 to    1
#   Inhabited     :    1 to   15
#   Exurban low   :   15 to  100
#   Exurban high  :  100 to  250
#   Suburban low  :  250 to  550
#   Suburban high :  550 to  800
#   Urban low     :  800 to 1900
#   Urban high    : > 1900
#
# Per assignment: keep both urban + both suburban classes, and roll
# Wildlands + Inhabited + Exurban low + Exurban high into a single "Exurban".
# Result: 5 landscapes.

classify_landscape <- function(density_km2) {
  case_when(
    density_km2 >  1900 ~ "Urban high",
    density_km2 >= 800  ~ "Urban low",
    density_km2 >= 550  ~ "Suburban high",
    density_km2 >= 250  ~ "Suburban low",
    TRUE                ~ "Exurban"
  )
}

landscape_levels <- c("Urban high","Urban low",
                      "Suburban high","Suburban low","Exurban")

landscape_palette <- c(
  "Urban high"    = "#67000d",
  "Urban low"     = "#cb181d",
  "Suburban high" = "#fd8d3c",
  "Suburban low"  = "#fdd49e",
  "Exurban"       = "#74c476"
)

# =============================================================================
# 2. GET 2020 CENSUS TRACT POPULATION FOR VIRGINIA
# =============================================================================
# Variable P1_001N = total population, decennial 2020.
# Richmond MSA is entirely inside VA, so we only need state = "VA".

va_tracts <- get_decennial(
  geography = "tract",
  variables = "P1_001N",
  state     = "VA",
  year      = 2020,
  sumfile   = "pl",
  geometry  = TRUE
) |>
  rename(pop = value)

# =============================================================================
# 3. GET RICHMOND MSA BOUNDARY AND FILTER TRACTS TO IT
# =============================================================================
# CBSAs come from tigris. Richmond, VA MSA = GEOID 40060.

cbsa_all <- core_based_statistical_areas(year = 2020, cb = TRUE)
richmond_msa <- cbsa_all |>
  filter(GEOID == "40060")   # "Richmond, VA Metro Area"

# Project to a planar CRS suitable for VA (NAD83 / Virginia South ftUS = 2284,
# or use UTM 18N = 26918). 26918 is fine for area calcs in meters.
va_tracts   <- st_transform(va_tracts,   26918)
richmond_msa <- st_transform(richmond_msa, 26918)

# Spatial filter: keep tracts whose centroids fall inside the MSA.
tract_centroids <- st_centroid(va_tracts)
inside <- st_within(tract_centroids, richmond_msa, sparse = FALSE)[,1]
rva <- va_tracts[inside, ]

# =============================================================================
# 4. CLASSIFY LANDSCAPES (HANBERRY)
# =============================================================================
rva <- rva |>
  mutate(
    area_km2    = as.numeric(st_area(geometry)) / 1e6,
    density_km2 = pop / area_km2,
    landscape   = factor(classify_landscape(density_km2),
                         levels = landscape_levels)
  )

# =============================================================================
# 5. MAP 1: SUB-GEOGRAPHIES ACROSS THE MSA
# =============================================================================
map_landscapes <- ggplot(rva) +
  geom_sf(aes(fill = landscape), color = "white", size = 0.05) +
  geom_sf(data = richmond_msa, fill = NA, color = "black", size = 0.6) +
  scale_fill_manual(values = landscape_palette, name = "Landscape") +
  labs(
    title    = "Richmond, VA MSA: Landscape classification (Hanberry 2022)",
    subtitle = "2020 Decennial Census, tract-level population density",
    caption  = "Source: U.S. Census Bureau"
  ) +
  theme_void(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

ggsave("figures/01_landscape_map.png",
       map_landscapes, width = 9, height = 7, dpi = 300)

# =============================================================================
# 6. ACS 5-YEAR (2016-2020) VARIABLES
# =============================================================================
# Pull a few useful variables. Adjust to your interests.
acs_vars <- c(
  median_income = "B19013_001",   # median household income
  median_age    = "B01002_001",   # median age
  pct_bach      = "B15003_022",   # bachelor's degree count (will normalize)
  pop_25plus    = "B15003_001",   # denominator for education
  median_rent   = "B25064_001"    # median gross rent
)

acs_rva <- get_acs(
  geography = "tract",
  variables = acs_vars,
  state     = "VA",
  year      = 2020,
  survey    = "acs5",
  output    = "wide",
  geometry  = FALSE
) |>
  mutate(pct_bachelors = 100 * pct_bachE / pop_25plusE) |>
  select(GEOID, median_income = median_incomeE,
         median_age = median_ageE,
         median_rent = median_rentE,
         pct_bachelors)

rva <- rva |> left_join(acs_rva, by = "GEOID")

# =============================================================================
# 7. CHART: median household income by landscape (boxplot)
# =============================================================================
chart_income <- rva |>
  st_drop_geometry() |>
  filter(!is.na(median_income)) |>
  ggplot(aes(landscape, median_income, fill = landscape)) +
  geom_boxplot(outlier.alpha = 0.4) +
  scale_fill_manual(values = landscape_palette, guide = "none") +
  scale_y_continuous(labels = label_dollar()) +
  labs(
    title = "Median household income by landscape, Richmond MSA",
    subtitle = "ACS 5-year, 2016-2020",
    x = NULL, y = "Median household income"
  ) +
  theme_minimal(base_size = 11)

ggsave("figures/02_income_by_landscape.png",
       chart_income, width = 8, height = 5, dpi = 300)

# =============================================================================
# 8. MAP 2: median household income (chloropleth)
# =============================================================================
map_income <- ggplot(rva) +
  geom_sf(aes(fill = median_income), color = NA) +
  scale_fill_viridis_c(option = "magma", labels = label_dollar(),
                       name = "Median HH income", na.value = "grey90") +
  labs(title = "Median household income by tract, Richmond MSA",
       caption = "ACS 2016-2020") +
  theme_void(base_size = 11)

ggsave("figures/03_income_map.png",
       map_income, width = 9, height = 7, dpi = 300)

# =============================================================================
# 9. POPULATION PYRAMIDS - urban vs. suburban
# =============================================================================
# Use B01001 (sex by age). We need each tract's urban/suburban label,
# so pull ACS at tract level then aggregate by landscape group.

rva_counties <- c(
  "Richmond city", "Henrico", "Chesterfield", "Hanover",
  "Powhatan", "Goochland", "New Kent", "Charles City",
  "Amelia", "Caroline", "Dinwiddie", "King William",
  "Prince George", "Sussex",
  "Petersburg city", "Hopewell city", "Colonial Heights city"
)

age_sex <- get_acs(
  geography = "tract",
  table     = "B01001",
  state     = "VA",
  county    = rva_counties,
  year      = 2020,
  survey    = "acs5",
  geometry  = FALSE
)

# Mapping of B01001 variable -> sex + age band
# Male:   B01001_003 ... _025;  Female: B01001_027 ... _049
age_lookup <- tribble(
  ~suffix,         ~age,
  "003","Under 5",  "004","5-9",     "005","10-14",   "006","15-17",
  "007","18-19",    "008","20",      "009","21",      "010","22-24",
  "011","25-29",    "012","30-34",   "013","35-39",   "014","40-44",
  "015","45-49",    "016","50-54",   "017","55-59",   "018","60-61",
  "019","62-64",    "020","65-66",   "021","67-69",   "022","70-74",
  "023","75-79",    "024","80-84",   "025","85+"
)

# Collapse into 5-year bands for a cleaner pyramid
age_bands <- c("0-14","15-24","25-34","35-44","45-54","55-64","65-74","75+")
band_for <- function(age) {
  case_when(
    age %in% c("Under 5","5-9","10-14")          ~ "0-14",
    age %in% c("15-17","18-19","20","21","22-24") ~ "15-24",
    age %in% c("25-29","30-34")                  ~ "25-34",
    age %in% c("35-39","40-44")                  ~ "35-44",
    age %in% c("45-49","50-54")                  ~ "45-54",
    age %in% c("55-59","60-61","62-64")          ~ "55-64",
    age %in% c("65-66","67-69","70-74")          ~ "65-74",
    age %in% c("75-79","80-84","85+")            ~ "75+"
  )
}

age_long <- age_sex |>
  mutate(
    suffix = str_extract(variable, "_(\\d{3})$") |> str_remove("_"),
    code   = as.integer(suffix),
    sex    = case_when(code >= 3  & code <= 25 ~ "Male",
                       code >= 27 & code <= 49 ~ "Female",
                       TRUE ~ NA_character_),
    suffix_norm = if_else(sex == "Female",
                          sprintf("%03d", code - 24),
                          suffix)
  ) |>
  filter(!is.na(sex)) |>
  left_join(age_lookup, by = c("suffix_norm" = "suffix")) |>
  filter(!is.na(age)) |>
  mutate(band = factor(band_for(age), levels = age_bands))

# Bring in landscape labels per tract
tract_landscape <- rva |> st_drop_geometry() |> select(GEOID, landscape)

age_long <- age_long |>
  left_join(tract_landscape, by = "GEOID") |>
  mutate(group = case_when(
    landscape %in% c("Urban high","Urban low")       ~ "Urban",
    landscape %in% c("Suburban high","Suburban low") ~ "Suburban",
    TRUE ~ NA_character_
  )) |>
  filter(!is.na(group))

pyramid_data <- age_long |>
  group_by(group, sex, band) |>
  summarise(pop = sum(estimate, na.rm = TRUE), .groups = "drop") |>
  mutate(pop_signed = if_else(sex == "Male", -pop, pop))

make_pyramid <- function(df, ttl) {
  ggplot(df, aes(x = band, y = pop_signed, fill = sex)) +
    geom_col() +
    coord_flip() +
    scale_y_continuous(labels = function(x) comma(abs(x))) +
    scale_fill_manual(values = c(Male = "#1f78b4", Female = "#e31a1c")) +
    labs(title = ttl, x = "Age band", y = "Population", fill = NULL) +
    theme_minimal(base_size = 11)
}

pyr_urban    <- make_pyramid(filter(pyramid_data, group == "Urban"),
                             "Population pyramid - Urban tracts (Richmond MSA)")
pyr_suburban <- make_pyramid(filter(pyramid_data, group == "Suburban"),
                             "Population pyramid - Suburban tracts (Richmond MSA)")

ggsave("figures/04_pyramid_urban.png",    pyr_urban,    width = 7, height = 5, dpi = 300)
ggsave("figures/05_pyramid_suburban.png", pyr_suburban, width = 7, height = 5, dpi = 300)

# =============================================================================
# 10. Save a clean tract dataset (optional, useful for the writeup)
# =============================================================================
st_write(rva, "richmond_tracts_classified.gpkg", delete_dsn = TRUE, quiet = TRUE)

message("Done. Figures written to ./figures/")