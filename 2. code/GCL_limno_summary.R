# Script setup -----------------------------------------------------------


#install.packages("here") # use to access source file

# Common packages from source file
source(here::here("2. code", "0-source.R"))

# Install required packages
pkgs <- c(common_pkgs)
#install.packages(pkgs)


# Additional packages not in source
library(RColorBrewer)


# Function to create "season" variable in dataframes with a "date" variable
get_seasons <- function(x) {
  x |> 
    rename_with(~ tolower(str_extract(.x, "(D|d)ate")), matches("(D|d)ate")) |> 
    mutate(
      date = as.Date(date),
      year = date |> format("%Y"),
      month = date |> format("%B"),
      season = case_when(
        month %in% month.name[9:11] ~ "Fall",
        month %in% month.name[3:5] ~ "Spring",
        month %in% month.name[6:8] ~ "Summer",
        T ~ "Winter"
      )
    )
}


# Load 2020 - present data ------------------------------------------------


# Zooplankton data
new_zpl <- read_xlsx(
  here(
    "1. data",
    "Great Central Lake",
    "GCL-Productivity_all-lab-results.xlsx"
  ), 
  sheet = "ZPL"
) |> 
  mutate(
    taxon = case_when(
      grepl("Alona", Species) ~ "Alona",
      grepl("Asplancha", Species) ~ "Asplancha sp.",
      grepl("Bipalpus", Species) ~ "Bipalpus sp.",
      grepl("Bosmina", Species) ~ "Bosmina",
      grepl("Calanoid", Species) ~ "Calanoid",
      grepl("Ceriodaphnia", Species) ~ "Ceriodaphnia",
      grepl("Chironomid", Species) ~ "Chironomid",
      grepl("Chydor", Species) ~ "Chydorus",
      grepl("Cyclopoid", Species) ~ "Cyclopoid",
      grepl("Cyclops", Species) ~ "Cyclops",
      grepl("D(ia|ai)ptomus", Species) ~ "Diaptomus",
      grepl("Daphnia", Species) ~ "Daphnia",
      grepl("Diacyclops", Species) ~ "Diacyclops sp.",
      grepl("Diaphanosoma", Species) ~ "Diaphanosoma spp.",
      grepl("Epischura", Species) ~ "Epischura spp.",
      grepl("Holop", Species) ~ "Holopedium spp.",
      grepl("Kellicottia", Species) ~ "Kellicottia sp.",
      grepl("Leptodiaptomus", Species) ~ "Leptodiaptomus sp.",
      grepl("Naupl", Species) ~ "Nauplii",
      grepl("Polyparthra", Species) ~ "Polyarthra sp.",
      grepl("Polyphemus", Species) ~ "Polyphemus",
      grepl("Rotifer", Species) ~ "Rotifer",
      grepl("Scapholeberis", Species) ~ "Scapholeberis",
      grepl("Skistodiaptomus", Species) ~ "Skistodiaptomus sp.",
      T ~ Species
    ),
    Station = as.character(Station)
  ) |> 
  rename_with(~tolower(gsub("\\s\\(.*\\)", "", .x))) |> # Remove units from column names
  select(-`count date`) |> 
  get_seasons() |> 
  mutate(season = if_else(year == 2021 & season == "Winter", "Fall", season))


# Phytoplankton data
new_ppl <- read_xlsx(
  here(
    "1. data",
    "Great Central Lake",
    "GCL-Productivity_all-lab-results.xlsx"
  ), 
  sheet = "PPL"
) |> 
  rename_with(~tolower(gsub("\\s\\(.*\\)", "", .x))) |>   # Remove units from column names
  mutate(
    across(`unit density`:last_col(), as.numeric),
    # Re-classify edibility according to rules stated in Hyatt et al. 2016
    edibility = if_else(
      grepl("(M|m)icrocystis", taxon) |
        !is.na(`mean biomass- incl. mucilage`) |
        `mean max diameter- incl. mucilage/loricas` > 30 |
        `mean max length- incl. mucilage/loricas` > 30,
      "inedible",
      "edible"
    ),
    depth = if_else(grepl("1$", `client sample id`), 
                    "1, 3, 5 m", 
                    "20 m"),
    station = str_extract(`client sample id`, "(?<=GCL-S)\\d{1}"),
    expansion = `sub-sample volume` / max(`sub-sample volume`),
    adj_biomass = `total biomass`/expansion
  ) |> 
  get_seasons() 


# Chemistry data
new_chem <- read_xlsx(
  here(
    "1. data",
    "Great Central Lake",
    "GCL-Productivity_all-lab-results.xlsx"
  ), 
  sheet = "CHEM"
) |> 
  rename_with(~ tolower(str_extract(.x, "^\\w*\\b"))) |> 
  # Convert Nitrate, Nitrite, and Total Phosphorus from mg to µg
  # (Chlorophyll-a) is already in µg
  mutate(
    across(
      c(nitrate, nitrite, tp),
      \(x) if_else(
        str_detect(x, "<"),
        paste0("<", as.numeric(str_remove(x, "<"))*1000),
        as.character(as.numeric(x)*1000)
      )
    )
  ) |> 
  get_seasons()


# Load and groom historic data into workable format -----------------------


# Start with the zooplankton data.
# 2008-2012 are in a different format than 2013, so need to treat 2013
# separately and then combine once wrangled into a common format.

# 2008-2012 zoo data
zpl_2008_2012 <- map(
  2008:2012, 
  \(x) read_xlsx(
    here(
      "1. data", 
      "Great Central Lake",
      "Historic_2008-2013.xlsx"
    ), 
    sheet = paste0("ZPL", x)
  ) |> 
    mutate(across(everything(), as.character))
) |> 
  bind_rows() |>
  mutate(date = coalesce(SampleDate, Date)) |> 
  # Where "Source" exists, remove Don McQueen records. Howard Stiff provided 
  # cleaned values where densities are standardized to match the other years
  filter(!Source == "Don McQueen") |> 
  select(-SampleDate, -Date, -Source) |> 
  pivot_longer(
    cols = !c(date, Station, Depth, Metric),
    names_to = "Taxon",
    values_to = "value"
  ) |> 
  pivot_wider(
    names_from = Metric,
    values_from = value
  )


# 2013 zoo data
zpl_2013 <- read_xlsx(
  here(
    "1. data", 
    "Great Central Lake",
    "Historic_2008-2013.xlsx"
  ), 
  sheet = "ZPL2013"
) |> 
  select(Station, `Sample date`, contains("volums"), `Species code`:last_col()) |> 
  rename(
    "date" = "Sample date",
    "Density" = "Density (#/L)",
    "Biomass" = "Biomass (ug/L)"
  ) |> 
  mutate(across(everything(), as.character)) 


# Load species names lookup table from recent results file
zpl_spp_lu <- read_xlsx(
  here(
    "1. data",
    "Great Central Lake",
    "GCL-Productivity_all-lab-results.xlsx"
  ), 
  sheet = "Zoo_Species_LU"
) |> 
  select(SP_CODE,SP_NAME) |> 
  rename("Species code" = "SP_CODE", "Species" = "SP_NAME")
              

# Collate the 2008-2013 zooplankton data and standardize species names
hist_zpl <- bind_rows(zpl_2008_2012, zpl_2013) |> 
  left_join(zpl_spp_lu) |> 
  filter(!(is.na(Biomass) & is.na(Density))) |> 
  mutate(
    Taxon = coalesce(Taxon, Species),
    Taxon = case_when(
      grepl("Alona", Taxon) ~ "Alona",
      grepl("Bosmina", Taxon) ~ "Bosmina",
      grepl("Calanoid", Taxon) ~ "Calanoid",
      grepl("Ceriodaphnia", Taxon) ~ "Ceriodaphnia",
      grepl("Chironomid", Taxon) ~ "Chironomid",
      grepl("Chydor", Taxon) ~ "Chydorus",
      grepl("Cyclopoid", Taxon) ~ "Cyclopoid",
      grepl("Cyclops", Taxon) ~ "Cyclops",
      grepl("D(ia|ai)ptomus", Taxon) ~ "Diaptomus",
      grepl("Daphnia", Taxon) ~ "Daphnia",
      grepl("Diacyclops", Taxon) ~ "Diacyclops sp.",
      grepl("Diaphanosoma", Taxon) ~ "Diaphanosoma spp.",
      grepl("Epischura", Taxon) ~ "Epischura spp.",
      grepl("Holop", Taxon) ~ "Holopedium spp.",
      grepl("Kellicottia", Taxon) ~ "Kellicottia sp.",
      grepl("Leptodiaptomus", Taxon) ~ "Leptodiaptomus sp.",
      grepl("Naupl", Taxon) ~ "Nauplii",
      grepl("Polyphemus", Taxon) ~ "Polyphemus",
      grepl("Rotifer", Taxon) ~ "Rotifer",
      grepl("Scapholeberis", Taxon) ~ "Scapholeberis",
      grepl("Skistodiaptomus", Taxon) ~ "Skistodiaptomus sp.",
      T ~ Taxon
    ),
    across(everything(), parse_guess)
  ) |> 
  summarize(
    .by = c(Station, Depth, date, Taxon),
    across(Biomass:Density, \(x) sum(x, na.rm = TRUE))
  ) |> 
  mutate(
    Depth = if_else(is.na(Depth), 50, Depth),
    d_n = if_else(Depth == 25, "Night", "Day"), # Day vs. night samples
    Station = if_else(is.na(Station), "1, 2, 3, 4", Station),
    # Wet weights were used prior to 2012. Hyatt et al. (2013) 
    # use a conversion factor of 7 to compare.
    Biomass = if_else(date < as.Date("01-01-2012"), Biomass / 7, Biomass)
  ) |>  
  get_seasons() |> 
  rename_with(tolower)


# Next the phytoplankton data
hist_ppl <- map(
  2008:2012, # No phyto data available from 2013
  \(x) read_xlsx(
    here(
      "1. data", 
      "Great Central Lake",
      "Historic_2008-2013.xlsx"
    ), 
    sheet = paste0("PPL", x)
  ) |> 
    mutate(across(everything(), as.character))
) |> 
  bind_rows() |> 
  pivot_longer(
    contains("phyta"),
    names_to = "taxon",
    values_to = "biomass",
    values_transform = as.numeric
  ) |> 
  pivot_wider(
    names_from = Edibility,
    values_from = biomass
  ) |> 
  rename_with(tolower) |> 
  mutate(inedible = total - edible) |> 
  pivot_longer(
    total:last_col(),
    names_to = "edibility",
    values_to = "biomass"
  ) |> 
  mutate(
    depth = case_when(
      is.na(depth) | grepl("(\\d,)+", depth) ~ "1, 3, 5 m",
      .default = paste(depth, "m")
    ),
    station = "3, 4" # As reported in Hyatt et al. 2016
  ) |> 
  get_seasons()


# And now the chemistry data
hist_chem <- map(
  2008:2013,
  \(x) read_xlsx(
    here(
      "1. data", 
      "Great Central Lake",
      "Historic_2008-2013.xlsx"
    ), 
    sheet = paste0("CHEM", x)
  )|> 
    mutate(across(everything(), as.character))
) |> 
  bind_rows() |> 
  rename_with(~ tolower(str_extract(.x, "^\\w*\\b"))) |> 
  mutate(chlorophyll = coalesce(chlorophyll, chlorophyl)) |> 
  select(-chlorophyl) |> 
  get_seasons() |> 
  pivot_longer(
    nitrite:chlorophyll,
    names_to = "compound",
    values_to = "measurement",
    values_transform = as.numeric
  ) |> 
  mutate(station = "3, 4") # As reported in Hyatt et al. 2016
  # Note that values are averaged over stations 3 & 4, not summed


# Collate historic and recent data ----------------------------------------


# 2008-2013 and 2020+ zooplankton data
zpl <- new_zpl |> 
  select(station, date, taxon, biomass, density, year, month, season) |> 
  mutate(
    depth = 50,
    d_n = "Day"
  ) |> 
  bind_rows(hist_zpl)


# Join the new phytoplankton data with the historical data
ppl <- new_ppl |> 
  summarize(
    .by = c(phylum, date, depth, year, month, season, station, edibility),
    biomass = sum(adj_biomass, na.rm = TRUE)
  ) |> 
  mutate(
    lake = "GCL",
    taxon = phylum
  ) |> 
  bind_rows(hist_ppl) |> 
  mutate(year = as.integer(year)) |> 
  select(-phylum)


# Combine the new and historical chemistry data
chem <- new_chem |> 
  mutate(
    # replace sub-threshold quantities with NA
    across(everything(), ~ if_else(grepl("<", .x), .[NA], .x)), 
    depth = fct_collapse(
      as.character(depth),
      "1, 3, 5" = c("1", "3", "5"),
      "20" = "20"
    ),
    across(nitrate:tp, as.numeric),
    station = as.character(station)
  ) |>
  summarize(
    .by = c(date, station, depth, season, year, month),
    across(nitrate:tp, \(x) mean(x, na.rm = TRUE))
  ) |> 
  pivot_longer(
    nitrate:tp,
    names_to = "compound",
    values_to = "measurement"
  ) |> 
  bind_rows(hist_chem) |> 
  mutate(
    measurement = if_else(is.nan(measurement), NA_real_, measurement),
    year = as.numeric(year),
    season = factor(season, levels = c("Spring", "Summer", "Fall", "Winter"))
  )


# Save the cleaned data files
clean_data <- list(zpl = zpl, ppl = ppl, chem = chem)

if(FALSE) {
  write_xlsx(
    clean_data,
    path = here(
      "3. outputs",
      "Clean data",
      "Great Central Lake",
      paste0(
        "GCL_limno_cleaned-data_2008-", 
        max(clean_data[['chem']]$year),
        ".xlsx"
      )
    )
  )
}


# Plot combined data ------------------------------------------------------


# Colour palette for seasons
season_cols <- c("seagreen3", "goldenrod2", "darkorange2", "royalblue4")


# Zooplankton data time series
zpl_plot_data <- zpl |> 
  pivot_longer(biomass:density) |> 
  # Sum metrics across stations for each survey
  summarize(
    .by = c(year, season, taxon, name, date),
    value = sum(value, na.rm = TRUE)
  ) |> 
  # Take max from within each season
  summarize(
    .by = c(year, season, taxon, name),
    value = max(value, na.rm = TRUE)
  ) |>
  # Lump together taxa that have low biomass and density into "OTHER"
  mutate(
    taxon = fct_lump_n(
      taxon, 
      n = 7, 
      w = value, 
      ties.method = "first",
      other_level = "OTHER"
    ),
    year = as.integer(year)
  ) |> 
  # Expand dataframe to include all combinations of season and year
  add_row(
    year = 2008:2022, 
    season = "Winter", 
    taxon = "Cyclops", 
    name = "biomass",
    value = NA
  ) |> 
  complete(year, season, taxon, name) |> 
  # Adjust factor levels in season and taxon for nice display
  mutate(
    season = factor(season, levels = c("Spring", "Summer", "Fall", "Winter")),
    taxon = fct_reorder(taxon, value, na.rm = TRUE, .desc = TRUE) |> 
      fct_relevel("OTHER", after = Inf)
  )


# Zooplankton time series plot
(zpl_ts_p <- zpl_plot_data |> 
    ggplot(aes(season, value)) +
    facet_grid(
      name ~ year, 
      scales = "free_y",
      # Use "name" as y axis labels with units
      switch = "both",
      labeller = labeller(
        name = c(
          biomass = "Biomass (µg/L)",
          density = "Density (#/L)"
        )
      )
    ) +
    geom_bar(
      aes(fill = taxon), 
      stat = "identity", 
      position = "stack"
    ) +
    scale_fill_brewer(palette = "Set1") +
    coord_cartesian(expand = FALSE) +
    labs(y = NULL, x = NULL, fill = "Taxon") +
    theme(
      strip.placement = "outside",
      strip.background = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.text.x = element_text(angle = 55, hjust = 1),
      panel.spacing.x = unit(0, "lines"),
      panel.border = element_blank(),
      axis.line = element_line(colour = "black"),
      legend.position = "inside",
      legend.position.inside = c(0.5, 0.95),
      legend.justification.inside = c(0.5, 1),
      legend.background = element_rect(colour = "black")
    )
)


# Phytoplankton data time series
ppl_plot_data <- ppl |> 
  filter(
    !str_detect(station, "1|2"), # Historic data are only from stations 3 & 4
    !edibility == "total"
  ) |> 
  mutate(depth = fct_collapse(as.factor(depth), "c. 20 m" = c("20 m", "25 m"))) |> 
  # Sum biomass across stations for each survey
  summarize(
    .by = c(date, depth, edibility, taxon, year, season),
    biomass = sum(biomass, na.rm = TRUE)
  ) |> 
  # Average biomass across surveys for each season
  summarize(
    .by = c(depth, edibility, taxon, year, season),
    biomass = mean(biomass, na.rm = TRUE)
  ) |>
  # Add row for complete
  add_row(
    year = 2008, 
    season = "Winter", 
    taxon = "Bacillariophyta", 
    depth = "c. 20 m", 
    edibility = "edible",
    biomass = NA
  ) |> 
  complete(year, season, taxon, depth, edibility) |> 
  mutate(season = factor(season, levels = c("Spring", "Summer", "Fall", "Winter")))


# Phytoplankton time series plot
(ppl_ts_p <- ppl_plot_data |> 
    ggplot(aes(year, biomass)) +
    facet_wrap(
      ~ depth + edibility, 
      ncol = 1,
      strip.position = "right",
      scales = "free_y"
    ) +
    geom_col(
      aes(fill = season), 
      position = "dodge"
    ) +
    scale_fill_manual(values = season_cols) +
    scale_x_continuous(breaks = seq.int(min(ppl$year), max(ppl$year))) +
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.05)),
      labels = scales::label_number()
    ) +
    labs(y = "Phytoplankton biomass (µg/L)", fill = "Season") + 
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = c(0.3, 0.05),
      legend.justification = c(1, 0),
      legend.background = element_rect(colour = "black"),
      panel.spacing.y = unit(1, "lines")
    )
)


# Chemistry data time series
chem_plot_data <- chem |> 
  filter(!str_detect(station, "1|2")) |> # Historic data are only from stations 3 & 4
  mutate(depth = fct_collapse(
    depth, 
    "c. 20 m" = c("20", "25"),
    "1, 3, 5 m" = "1, 3, 5")
  ) |>
  summarize(
    .by = c(year, season, compound, depth),
    measurement = mean(measurement, na.rm = TRUE)
  ) |> 
  mutate(measurement = if_else(is.nan(measurement), 0, measurement)) |> 
  # Add row for complete
  add_row(
    year = 2008, 
    season = "Winter", 
    compound = "nitrite", 
    depth = "c. 20 m", 
    measurement = NA
  ) |> 
  complete(year, season, compound, depth) |> 
  mutate(season = factor(season, levels = c("Spring", "Summer", "Fall", "Winter")))


# Water chemistry time series plot
(chem_ts_p <- chem_plot_data |> 
    ggplot(aes(year, measurement)) +
    facet_grid(
      compound ~ depth,
      scales = "free_y",
      switch = "y",
      labeller = labeller(
        compound = c(
          chlorophyll = "Chlorophyll-a (µg/L)",
          nitrate = "Nitrate (mg/L)",
          nitrite = "Nitrite (mg/L)",
          tp = "Phosphorus (mg/L)"
        )
      )
    ) +
    geom_col(aes(fill = season), position = "dodge") +
    scale_x_continuous(breaks = seq.int(min(chem$year), max(chem$year))) +
    scale_fill_manual(values = season_cols) +
    coord_cartesian(expand = FALSE) +
    labs(y = NULL) +
    theme(
      strip.placement = "outside",
      strip.background.y = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.background = element_rect(colour = "black")
    )
)


# Export plots in print quality
ts_p_list <- list(
  "Zooplankton_biomass_density" = zpl_ts_p,
  "Phytoplankton_biomass" = ppl_ts_p,
  "Water_chemistry" = chem_ts_p
)


if(FALSE) { # Toggle "TRUE" to run
  ts_p_list |> 
    iwalk(
      \(x, name) ggsave(
        plot = x,
        filename = here(
          "3. outputs",
          "Plots",
          "Great Central Lake",
          paste0("GCL_limno_", name, "_time-series.png")
        ),
        width = 9,
        height = 7,
        units = "in",
        dpi = "print"
      )
    )
}
