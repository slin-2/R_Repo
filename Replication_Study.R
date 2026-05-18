library(tidycensus)
library(tidyverse)
library(segregation)
library(tigris)
library(sf)
ca_acs_data <- get_acs(
  geography = "tract",
  variables = c(
    white = "B03002_003",
    black = "B03002_004",
    asian = "B03002_006",
    hispanic = "B03002_012"
  ),
  state = "CA",
  geometry = TRUE,
  year = 2019
)
# Use tidycensus to get urbanized areas by population with geometry,
# then filter for those that have populations of 750,000 or more
us_urban_areas <- get_acs(
  geography = "urban area",
  variables = "B01001_001",
  geometry = TRUE,
  year = 2019,
  survey = "acs1"
) %>%
  filter(estimate >= 750000) %>%
  transmute(urban_name = str_remove(NAME,
                                    fixed(", CA Urbanized Area (2010)")))
# Compute an inner spatial join between the California tracts and the
# urbanized areas, returning tracts in the largest California urban
# areas with the urban_name column appended
ca_urban_data <- ca_acs_data %>%
  st_join(us_urban_areas, left = FALSE) %>%
  select(-NAME) %>%
  st_drop_geometry()

mutual_within(
  data = ca_urban_data,
  group = "variable",
  unit = "GEOID",
  weight = "estimate",
  within = "urban_name",
  wide = TRUE
)
sf_local_seg <- ca_urban_data %>%
  filter(urban_name == "San Francisco--Oakland") %>%
  mutual_local(
    group = "variable",
    unit = "GEOID",
    weight = "estimate",
    wide = TRUE
  )

sf_tracts_seg <- tracts("CA", cb = TRUE, year = 2019) %>%
  inner_join(sf_local_seg, by = "GEOID")

sf_tracts_seg %>%
  ggplot(aes(fill = ls)) +
  geom_sf(color = NA) +
  coord_sf(crs = 26943) +
  scale_fill_distiller(palette = "RdPu", direction = 1) +
  theme_void() +
  labs(fill = "Local\nsegregation index")


------------------------------

  library(sf)
library(tidyverse)
library(tidycensus)
library(tigris)
library(tmap)
library(rmapshaper)
library(flextable)

# Bring in 2019-2023 census tract data using the Census API
ca.tracts <- get_acs(geography = "tract",
                     year = 2023,
                     variables = c(tpop = "B03002_001",
                                   white = "B03002_003", black = "B03002_004",
                                   asian = "B03002_006", hisp = "B03002_012"),
                     state = "CA",
                     survey = "acs5",
                     output = "wide",
                     geometry = TRUE)

# Calculate, rename and keep essential vars.
ca.tracts <- ca.tracts %>%
  mutate(pwhite = 100*(whiteE/tpopE), pasian = 100*(asianE/tpopE),
         pblack = 100*(blackE/tpopE), phisp = 100*(hispE/tpopE)) %>%
  rename(white = whiteE, asian = asianE, black = blackE,
         hisp = hispE, tpop = tpopE) %>%
  select(GEOID, tpop, pwhite, pasian, pblack, phisp,
         white, asian, black, hisp)

# Bring in city boundaries
pl <- places(state = "CA", year = 2023, cb = TRUE)

# Keep San Francisco
large.cities <- pl %>%
  filter(NAME == "San Francisco")

# Clip tracts in San Francisco
large.tracts <- ms_clip(target = ca.tracts,
                        clip = large.cities,
                        remove_slivers = TRUE)

# Join city info to tracts
large.tracts <- large.tracts %>%
  st_join(large.cities)

# Calculate citywide totals
large.tracts <- large.tracts %>%
  group_by(NAME) %>%
  mutate(whitec = sum(white), asianc = sum(asian),
         blackc = sum(black), hispc = sum(hisp),
         tpopc = sum(tpop))

# Location Quotient for San Francisco tracts
fresno.tracts <- large.tracts %>%
  filter(NAME == "San Francisco") %>%
  mutate(blklq = (black/tpop)/(blackc/tpopc),
         asnlq = (asian/tpop)/(asianc/tpopc),
         hisplq = (hisp/tpop)/(hispc/tpopc),
         whitelq = (white/tpop)/(whitec/tpopc))

# Histogram of Black LQ
fresno.tracts %>%
  ggplot() +
  geom_histogram(mapping = aes(x = blklq), na.rm = TRUE) +
  xlab("Black Location Quotient")

fresno.tracts %>%
  filter(!is.na(asnlq)) %>%
  tm_shape(unit = "mi") +
  tm_polygons(fill = "asnlq",
              fill.scale = tm_scale(style = "quantile", values = "BuGn"),
              fill.legend = tm_legend(title = "Asian Location Quotient For San Francisco"))