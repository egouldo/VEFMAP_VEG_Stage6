# Analysis of VEFMAP vegetation monitoring data

# See README.md for details of the VEFMAP program and this project and see
#    analysis/preregistration_template.Rmd for details of the analysis
#    implemented in this script

# Shortened script to load some summary figures for presentation purposes

# Authors: Jian Yen, Elliot Gould, Henry Wootton
# Contact: jdl.yen [at] gmail.com

# Date created: 16 November 2023
# Last updated: 16 April 2024

# load some packages (include all dependencies for renv tracking)
library(here)
library(qs)
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(aae.hydro)
library(ggplot2)
library(ggtext)
library(sessioninfo)

# load helper functions
source("R/utils.R")
source("R/data.R")

# load site info
site_info <- load_metadata(recompile = TRUE)

# first lets extract cover data from raw data files for each site
veg_cover <- bind_rows(
  mapply(
    \(sys, s67set) load_cover(
      system = sys,
      pilot = FALSE,
      recompile = TRUE,
      ar = FALSE,
      s6s7 = s67set
    ),
    sys = c(
      "Campaspe",
      "Wimmera",
      "Moorabool", 
      "Loddon",
      "Yarra", 
      "ThomsonMacalister",
      "Glenelg"
    ),
    s67set = c(TRUE, FALSE, TRUE, rep(FALSE, 3), TRUE),
    SIMPLIFY = FALSE
  )
)

# repeat for richness
veg_richness <- bind_rows(
  mapply(
    \(sys, s67set) load_richness(
      system = sys,
      pilot = FALSE,
      recompile = TRUE,
      s6s7 = s67set
    ),
    sys = c(
      "Campaspe",
      "Wimmera",
      "Moorabool", 
      "Loddon",
      "Yarra", 
      "ThomsonMacalister",
      "Glenelg"
    ),
    s67set = c(TRUE, FALSE, TRUE, rep(FALSE, 3), TRUE),
    SIMPLIFY = FALSE
  )
)

# load flow data and merge summary metrics with veg
flow <- bind_rows(
  lapply(
    c(
      "Campaspe",
      "Wimmera",
      "Moorabool", 
      "Loddon",
      "Yarra", 
      "ThomsonMacalister",
      "Glenelg"
    ),
    \(sys) load_flow(
      system = sys,
      pilot = FALSE,
      recompile = TRUE
    )
  )
)

# calculate some summary info
sys_lookup <- c(
  "Campaspe" = "Campaspe",
  "Burnt" = "Wimmera",
  "McKenzie" = "Wimmera",
  "Mt William" = "Wimmera",
  "Moorabool" = "Moorabool",
  "Sutherland" = "Moorabool",
  "Loddon" = "Loddon",
  "Tullaroop" = "Loddon",
  "TwelveMile" = "Loddon",
  "Watts" = "Yarra",
  "Yarra" = "Yarra",
  "Macalister" = "Thomson",
  "Thomson" = "Thomson",
  "Glenelg" = "Glenelg",
  "Wannon" = "Glenelg"
)

nsp <- veg_cover |>
  filter(hits > 0) |>
  group_by(waterbody, rec_group, origin) |>
  summarise(nspecies = length(unique(species))) |>
  ungroup() |>
  pivot_wider(
    id_cols = waterbody, 
    names_from = c(rec_group, origin),
    values_from = nspecies,
    names_prefix = "nspecies_",
    values_fill = 0
  ) |>
  mutate(
    native_species_total = nspecies_Aquatic_native + 
      nspecies_Emergent_native +
      nspecies_Riparian_native +
      nspecies_Terrestrial_native,
    exotic_species_total = nspecies_Aquatic_exotic + 
      nspecies_Emergent_exotic +
      nspecies_Riparian_exotic +
      nspecies_Terrestrial_exotic
  )
find_common_species <- function(sp, cov, n = 5) {
  cov <- tapply(cov, sp, mean)
  cov <- sort(cov, decreasing = TRUE)[seq_len(n)]
  cov <- cov[!is.na(cov)]
  paste(names(cov), collapse = ", ")
}
common_sp <- veg_cover |>
  mutate(cover = hits / npoint) |>
  group_by(waterbody, rec_group, origin) |>
  summarise(common_species = find_common_species(species, cover)) |>
  ungroup() |>
  pivot_wider(
    id_cols = waterbody,
    values_from = common_species,
    names_from = c(rec_group, origin),
    names_prefix = "common_"
  )

ave_cover <- veg_cover |>
  mutate(cover = 100 * (hits / npoint)) |>
  group_by(waterbody, site, transect, metres, survey, rec_group, origin) |>
  summarise(cover = sum(cover)) |>
  group_by(waterbody, rec_group, origin) |>
  summarise(cover = round(mean(cover), 1)) |>
  ungroup() |>
  pivot_wider(
    id_cols = waterbody, 
    names_from = c(rec_group, origin),
    values_from = cover,
    names_prefix = "average_cover_",
    values_fill = 0
  )
veg_summary <- veg_cover |>
  mutate(
    catchment = sys_lookup[waterbody],
    cover = hits / npoint
  ) |>
  group_by(catchment, waterbody) |> 
  summarise(
    nsite = length(unique(site)),
    ntransect = length(unique(paste(waterbody, site, transect, sep = "_"))),
    nsubtransect = length(unique(paste(waterbody, site, transect, metres, sep = "_"))),
    nsurvey = length(unique(paste(waterbody, site, survey, sep = "_"))),
    nvisit = length(unique(paste(waterbody, survey, sep = "_"))),
    survey_years = paste(sort(unique(survey_year)), collapse = ", ")
  ) |>
  ungroup() |>
  left_join(nsp, by = "waterbody") |>
  left_join(common_sp, by = "waterbody") |>
  left_join(ave_cover, by = "waterbody")
write.csv(veg_summary, file = "outputs/tables/veg-summary.csv")

veg_cover %>% 
  distinct(species, rec_group, origin) %>% 
  drop_na(rec_group, origin) %>% 
  write_csv("outputs/tables/species-grouping-list.csv")

# add a plot of a single site and some patterns
species_to_plot <- c(
  "Phragmites australis",
  # "Alternanthera denticulata",
  "Poa labillardierei var. labillardierei",
  "Oxalis pes-caprae",
  # "Vallisneria australis",
  "Juncus amabilis"#,
  # "Paspalum distichum"
)
spencer_mahd <- veg_cover |>
  filter(
    species %in% species_to_plot,
    site == "Spencer"
  ) |>
  left_join(
    site_info, 
    by = c("waterbody", "site", "transect", "metres")
  ) |>
  mutate(
    species = factor(species, level = species_to_plot[c(1, 4, 2, 3)]),
    species = forcats::fct_relabel(species, ~ gluedown::md_italic(.x) %>% 
                                     stringr::str_replace(" var. ", "_ var. _")),
    cover = 100 * (hits / npoint),
    baseflow_m_ahd = median(baseflow_m_ahd, na.rm = TRUE),
    springfresh_m_ahd = median(springfresh_m_ahd, na.rm = TRUE)
  ) |>
  ggplot(aes(y = cover, x = height_ahd)) +
  geom_point() +
  geom_vline(aes(xintercept = baseflow_m_ahd), col = "#2166AC") +
  geom_vline(aes(xintercept = springfresh_m_ahd), col = "#B2182B") +
  xlab("Elevation (mAHD)") +
  ylab("Percentage cover") +
  facet_wrap( ~ species) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        strip.text = ggtext::element_markdown())

spencer_flow <- fetch_hydro(
  406202,
  start = dmy("01-01-2016"), 
  end = dmy("31-12-2024"),
  variables = "depth"
)
spencer_flow2 <- flow |> filter(site == "Spencer")
spencer_flow3 <- tibble(
  system = "Campaspe",
  waterbody = "Campaspe",
  site = "Spencer",
  date_formatted = spencer_flow$date_formatted,
  water_level_m = spencer_flow$value,
  water_level_m_ahd = approx(
    x = spencer_flow2$water_level_m, 
    y = spencer_flow2$water_level_m_ahd,
    xout = spencer_flow$value
  )$y
)

metric <- calculate_metrics(spencer_flow3, site_info)
max_val_bs <- max(metric$days_above_baseflow)
max_val_sp <- max(metric$days_above_springfresh)
water_year_lu <- tibble(
  val = 1:16,
  label = c(
    "2017", "", "",
    "2018", "", "",
    "2019", "", "",
    "2020", "",
    "2021", "",
    "2022", "",
    "2023"
  )
)
water_year_lu <- mapply(
  \(x, y) x |> mutate(species = y),
  x = lapply(1:4, \(i, .x) .x, .x = water_year_lu),
  y = species_to_plot[c(1, 4, 2, 3)],
  SIMPLIFY = FALSE
)
water_year_lu <- bind_rows(water_year_lu)
max_label <- veg_cover |> 
  filter(site == "Spencer", species %in% species_to_plot) |>
  group_by(species) |>
  summarise(max_label = 100 * quantile(hits / npoint, probs = 0.99))
survey_mid <- veg_cover |> 
  filter(site == "Spencer", species %in% species_to_plot) |>
  group_by(survey_year) |>
  summarise(survey_mid = mean(survey))
spencer_time <- veg_cover |>
  filter(
    species %in% species_to_plot,
    site == "Spencer"
  ) |>
  left_join(
    site_info, 
    by = c("waterbody", "site", "transect", "metres")
  ) |>
  mutate(
    period = ifelse(survey == 1, "before_spring", period),
    period = ifelse(survey == 16, "before_spring", period)
  ) |>
  left_join(
    metric,
    by = c("system", "waterbody", "site", "survey_year", "period")
  ) |>
  left_join(max_label, by = "species") |>
  mutate(
    species = factor(species, level = species_to_plot[c(1, 4, 2, 3)]),
    cover = hits / npoint,
    days_above_baseflow = days_above_baseflow / (2 * max_val_bs),
    days_above_springfresh = days_above_springfresh / max_val_sp,
    survey = factor(survey),
    period = case_when(survey == 1 ~ fct_recode(period, "after_spring" = "before_spring",),
                       survey == 16 ~ fct_recode(period, "after_spring" = "before_spring"),
                       TRUE ~ period)
  ) |>
  group_by(survey, period, survey_year, species, max_label) |>
  summarise(
    mid = 100 * mean(cover),
    upper = 100 * quantile(cover, probs = 0.9),
    lower = 100 * quantile(cover, probs = 0.1),
    days_above_baseflow = 100 * median(days_above_baseflow, na.rm = TRUE),
    days_above_springfresh = 100 * median(days_above_springfresh, na.rm = TRUE)
  ) |>
  ungroup() |>
  mutate(
    water_year_label = water_year_lu$label[match(survey, water_year_lu$val)],
    period_clean = factor(
      period,
      levels = c("before_spring", "after_spring", "after_summer"),
      labels = c("Before spring", "After spring", "After summer")
    )
  ) |>
  left_join(survey_mid, by = "survey_year") |>
  mutate(species = forcats::fct_relabel(species, ~ gluedown::md_italic(.x) %>% 
                                          stringr::str_replace(" var. ", "_ var. _"))) |>
  ggplot(aes(y = mid, x = survey, shape = period_clean)) +
  geom_point() +
  geom_point(aes(y = days_above_baseflow), shape = "circle", col = "#2166AC") +
  geom_point(aes(y = 1.5 * days_above_springfresh), shape = "circle", col = "#B2182B") +
  geom_errorbar(aes(ymin = lower, ymax = upper)) +
  geom_text(aes(x = ifelse(survey_mid < 16, survey_mid, 16.5), y = 1.1 * max_label, label = water_year_label), size = 3.5) +
  geom_vline(xintercept = c(3.5, 6.5, 9.5, 11.5, 13.5, 15.5), linetype = "dashed") +
  xlab("Survey") +
  ylab("Percentage cover / relative days above threshold") +
  scale_shape(name = "") +
  scale_x_discrete(
    breaks = as.character(1:16), 
    limits = c(as.character(1:16), "16.5"), 
    labels = c(as.character(1:16))) +
  facet_wrap( ~ species, scales = "free") +
  theme_bw() +
  theme(legend.position = "bottom", 
        panel.grid = element_blank(),
        strip.text = ggtext::element_markdown())

ggsave(
  filename = "outputs/figures/spencer-cover.png",
  plot = spencer_mahd,
  device = ragg::agg_png,
  width = 8,
  height = 6,
  units = "in",
  dpi = 600
)
ggsave(
  filename = "outputs/figures/spencer-trend.png",
  plot = spencer_time,
  device = ragg::agg_png,
  width = 8,
  height = 6,
  units = "in",
  dpi = 600
)

# plot some of the exotic vs native patterns for different levels
native_vs_exotic_subtransect <- veg_cover |>
  mutate(cover = 100 * (hits / npoint)) |>
  group_by(waterbody, site, survey, transect, metres, survey_year, origin, rec_group) |>
  summarise(cover = sum(cover)) |>
  ungroup() |>
  mutate(level = "Sub-transect") |>
  pivot_wider(
    id_cols = c(waterbody, site, survey, transect, metres, survey_year, rec_group, level),
    values_from = cover,
    names_from = origin,
    values_fill = 0
  )
native_vs_exotic_transect <- veg_cover |>
  mutate(cover = 100 * (hits / npoint)) |>
  group_by(waterbody, site, survey, transect, survey_year, origin, rec_group) |>
  summarise(cover = sum(cover)) |>
  ungroup() |>
  mutate(level = "Transect", metres = NA) |>
  pivot_wider(
    id_cols = c(waterbody, site, survey, transect, metres, survey_year, rec_group, level),
    values_from = cover,
    names_from = origin,
    values_fill = 0
  ) |> 
  select(all_of(colnames(native_vs_exotic_subtransect)))
native_vs_exotic_site <- veg_cover |>
  mutate(cover = 100 * (hits / npoint)) |>
  group_by(waterbody, site, survey, survey_year, origin, rec_group) |>
  summarise(cover = sum(cover)) |>
  ungroup() |>
  mutate(level = "Site", transect = NA, metres = NA) |>
  pivot_wider(
    id_cols = c(waterbody, site, survey, transect, metres, survey_year, rec_group, level),
    values_from = cover,
    names_from = origin,
    values_fill = 0
  ) |> 
  select(all_of(colnames(native_vs_exotic_subtransect)))
native_vs_exotic <- bind_rows(
  native_vs_exotic_subtransect,
  native_vs_exotic_transect,
  native_vs_exotic_site
)

exotic_native_aquatic <- native_vs_exotic |>
  mutate(level = factor(level, levels = c("Sub-transect", "Transect", "Site"))) |>
  filter(rec_group == "Aquatic") |>
  ggplot(aes(x = exotic, y = native)) +
  geom_point() +
  facet_wrap( ~ level, scales = "free") +
  xlab("Cover of exotic species") +
  ylab("Cover of native species") +
  theme(legend.position = "bottom") +
  theme_bw() +
  theme(panel.grid = element_blank())

exotic_native_emergent <- native_vs_exotic |>
  mutate(level = factor(level, levels = c("Sub-transect", "Transect", "Site"))) |>
  filter(rec_group == "Emergent") |>
  ggplot(aes(x = exotic, y = native)) +
  geom_point() +
  facet_wrap( ~ level, scales = "free") +
  xlab("Cover of exotic species") +
  ylab("Cover of native species") +
  theme(legend.position = "bottom") +
  theme_bw() +
  theme(panel.grid = element_blank())

exotic_native_riparian <- native_vs_exotic |>
  mutate(level = factor(level, levels = c("Sub-transect", "Transect", "Site"))) |>
  filter(rec_group == "Riparian") |>
  ggplot(aes(x = exotic, y = native)) +
  geom_point() +
  facet_wrap( ~ level, scales = "free") +
  xlab("Cover of exotic species") +
  ylab("Cover of native species") +
  theme(legend.position = "bottom") +
  theme_bw() +
  theme(panel.grid = element_blank())

exotic_native_terrestrial <- native_vs_exotic |>
  mutate(level = factor(level, levels = c("Sub-transect", "Transect", "Site"))) |>
  filter(rec_group == "Terrestrial") |>
  ggplot(aes(x = exotic, y = native)) +
  geom_point() +
  facet_wrap( ~ level, scales = "free") +
  xlab("Cover of exotic species") +
  ylab("Cover of native species") +
  theme(legend.position = "bottom") +
  theme_bw() +
  theme(panel.grid = element_blank())

ggsave(
  filename = "outputs/figures/exotic-native-aquatic.png",
  plot = exotic_native_aquatic,
  device = ragg::agg_png,
  width = 7,
  height = 5,
  units = "in",
  dpi = 600
)
ggsave(
  filename = "outputs/figures/exotic-native-emergent.png",
  plot = exotic_native_emergent,
  device = ragg::agg_png,
  width = 7,
  height = 5,
  units = "in",
  dpi = 600
)
ggsave(
  filename = "outputs/figures/exotic-native-riparian.png",
  plot = exotic_native_riparian,
  device = ragg::agg_png,
  width = 7,
  height = 5,
  units = "in",
  dpi = 600
)
ggsave(
  filename = "outputs/figures/exotic-native-terrestrial.png",
  plot = exotic_native_terrestrial,
  device = ragg::agg_png,
  width = 7,
  height = 5,
  units = "in",
  dpi = 600
)

sessioninfo::session_info(to_file = "outputs/session-info.txt")
