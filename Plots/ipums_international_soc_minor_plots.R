library(haven)
library(tidyverse)
library(readxl)
library(iscoCrosswalks)

DL <- "~/Downloads"

ipumsi_file <- file.path(DL, "ipums3digitcode.dta")
acs_file <- file.path(DL, "acs_2010to2019.dta")
soc18_file <- file.path(DL, "soc_structure_2018.xlsx")
soc1018_file <- file.path(DL, "soc_2010_to_2018_crosswalk.xlsx")

output_dir <- file.path(DL, "ipumsi_us_soc_minor_plots")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# ISCO-08 to SOC-2010
isco_soc_frac <- iscoCrosswalks::isco08_soc10 %>%
  transmute(isco3 = as.character(isco08), soc10 = as.character(soc10)) %>%
  distinct() %>%
  group_by(isco3) %>%
  mutate(isco_fraction = 1 / n()) %>%
  ungroup()

# SOC-2010 -> SOC-2018. For each one-to-many SOC match, again use 1 / n.
xw1018 <- read_excel(
  soc1018_file,
  skip = 9,
  col_names = c("soc10_code", "soc10_title", "soc18_code", "soc18_title")
) %>%
  filter(str_detect(soc10_code, "^\\d{2}-\\d{4}$"),
         str_detect(soc18_code, "^\\d{2}-\\d{4}$")) %>%
  transmute(soc10 = str_remove(soc10_code, "-"),
            soc18 = str_remove(soc18_code, "-")) %>%
  distinct() %>%
  group_by(soc10) %>%
  mutate(soc_fraction = 1 / n()) %>%
  ungroup()

#employment multiplied with weight
isco3_to_soc18 <- isco_soc_frac %>%
  inner_join(xw1018, by = "soc10", relationship = "many-to-many") %>%
  transmute(
    isco3,
    minor_pref = str_sub(soc18, 1, 3),
    w = isco_fraction * soc_fraction
  ) %>%
  group_by(isco3, minor_pref) %>%
  summarise(w = sum(w), .groups = "drop")

# SOC-2018 minor-group labels
struct18 <- read_excel(
  soc18_file,
  skip = 8,
  col_names = c("major", "minor", "broad", "detailed", "title")
)

minor_names <- struct18 %>%
  filter(!is.na(minor)) %>%
  transmute(
    minor_pref = str_sub(str_remove(minor, "-"), 1, 3),
    minor_code = minor,
    minor_label = title
  ) %>%
  distinct()

stopifnot(!anyDuplicated(minor_names$minor_pref))

short_soc_label <- function(code, title) {
  keywords <- title %>%
    str_remove(" Occupations$") %>%
    str_replace_all("&", " ") %>%
    str_remove_all("(?i)\\b(first|line|of|and|the|other|miscellaneous)\\b") %>%
    str_remove_all("(?i)\\bworkers?\\b") %>%
    str_replace_all("(?i)\\bsupervisors?\\b", "supervisor") %>%
    str_replace_all("(?i)\\boperators?\\b", "operator") %>%
    str_squish() %>%
    str_split("\\s+") %>%
    map_chr(~ paste(head(.x[nzchar(.x)], 3), collapse = " ")) %>%
    str_to_title()
  
  paste(code, keywords)
}

# IPUMS International
#employed men aged 25-54 with ISCO08A three digit code.
ipumsi <- read_dta(
  ipumsi_file,
  col_select = c(country, year, perwt, sex, age, empstat, isco08a)
)

international_employed <- ipumsi %>%
  transmute(
    country = str_to_title(as.character(as_factor(country))),
    year = as.integer(year),
    weight = as.numeric(perwt),
    sex = as.numeric(sex),
    age = as.numeric(age),
    empstat = as.numeric(empstat),
    isco3 = sprintf("%03d", as.integer(isco08a))
  ) %>%
  filter(
    empstat == 1,
    sex == 1,
    age >= 25,
    age <= 54,
    weight > 0,
    !is.na(isco3),
    country != "United States"
  )

international_minor <- international_employed %>%
  inner_join(
    isco3_to_soc18,
    by = "isco3",
    relationship = "many-to-many"
  ) %>%
  filter(str_sub(minor_pref, 1, 2) != "55") %>%
  group_by(country, year, minor_pref) %>%
  summarise(emp = sum(weight * w), .groups = "drop") %>%
  group_by(country, year) %>%
  mutate(share = 100 * emp / sum(emp)) %>%
  ungroup() %>%
  select(country, year, minor_pref, share)

# IPUMS USA ACS
#civilian employed men aged 25-54.
acs <- read_dta(
  acs_file,
  col_select = c(year, perwt, sex, age, empstat, empstatd, occsoc)
)

acs_minor <- acs %>%
  transmute(
    year = as.integer(year),
    weight = as.numeric(perwt),
    sex = as.numeric(sex),
    age = as.numeric(age),
    empstat = as.numeric(empstat),
    empstatd = as.numeric(empstatd),
    minor_pref = str_sub(str_pad(trimws(occsoc), 6, pad = "0"), 1, 3)
  ) %>%
  filter(
    empstat == 1,
    empstatd %in% 10:12,
    sex == 1,
    age >= 25,
    age <= 54,
    weight > 0,
    minor_pref %in% minor_names$minor_pref,
    str_sub(minor_pref, 1, 2) != "55"
  ) %>%
  group_by(year, minor_pref) %>%
  summarise(emp = sum(weight), .groups = "drop") %>%
  group_by(year) %>%
  mutate(share = 100 * emp / sum(emp)) %>%
  ungroup() %>%
  transmute(year, minor_pref, country = "US", share)

# One US-versus-country plot per country-year with matching ACS data.
comparison_index <- international_minor %>%
  distinct(country, year) %>%
  inner_join(acs_minor %>% distinct(year), by = "year") %>%
  arrange(year, country)

pwalk(comparison_index, function(country, year) {
  other_country <- country
  sample_year <- year
  
  other <- international_minor %>%
    filter(country == other_country, year == sample_year) %>%
    transmute(minor_pref, country = other_country, share)
  
  us <- acs_minor %>%
    filter(year == sample_year) %>%
    select(minor_pref, country, share)
  
  plot_data <- bind_rows(us, other) %>%
    complete(
      minor_pref = minor_names$minor_pref,
      country = c("US", other_country),
      fill = list(share = 0)
    ) %>%
    left_join(minor_names, by = "minor_pref") %>%
    mutate(soc_label = short_soc_label(minor_code, minor_label)) %>%
    group_by(minor_pref) %>%
    mutate(us_share = share[country == "US"][1]) %>%
    ungroup() %>%
    arrange(desc(us_share), minor_pref) %>%
    mutate(
      soc_label = factor(soc_label, levels = unique(soc_label)),
      country = factor(country, levels = c("US", other_country))
    )
  
  SUBT <- paste(
    "Men aged 25-54, civilian employed.",
    paste0("IPUMS International ", sample_year,
           " and same-year U.S. ACS."),
    sep = "\n"
  )
  
  plot <- ggplot(plot_data, aes(x = soc_label, y = share, fill = country)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.72) +
    scale_fill_manual(values = c(US = "#50C2E5", setNames("#C9495E", other_country))) +
    scale_y_continuous(expand = expansion(mult = c(0, .06))) +
    labs(
      x = "SOC minor group",
      y = "% of employment",
      fill = NULL,
      title = paste("Employment share by SOC minor group: US vs", other_country),
      subtitle = SUBT
    ) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "top",
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 7),
      panel.grid.minor.x = element_blank(),
      plot.margin = margin(5.5, 5.5, 16, 5.5)
    )
  
  print(plot)
  
  safe_country <- paste(other_country, sample_year) %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", "_")
  
  ggsave(
    file.path(output_dir, paste0("us_", safe_country, "_soc_minor_share.png")),
    plot = plot,
    width = 32,
    height = 10,
    dpi = 160,
    limitsize = FALSE
  )
})
