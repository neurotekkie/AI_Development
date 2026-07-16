library(haven)
library(tidyverse)
library(readxl)
library(iscoCrosswalks)

DL <- "~/Downloads"

ipumsi_file <- file.path(DL, "cross_country_occupation.dta")
acs_file    <- file.path(DL, "acs_2010to2019.dta")
soc_file    <- file.path(DL, "soc_structure_2018.xlsx")

output_dir <- file.path(DL, "ipums_international_soc_major_plots")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# ISCO to SOC

xw8808 <- occupationcross::crosstable_isco08_isco88 %>%
  transmute(isco88 = str_pad(as.character(`ISCO-88 code`), 4, "left", "0"),
            isco08 = str_pad(as.character(`ISCO 08 Code`), 4, "left", "0")) %>%
  filter(str_detect(isco88, "^[0-9]{4}$"), str_detect(isco08, "^[0-9]{4}$"))

links_08_soc <- iscoCrosswalks::isco08_soc10 %>%
  transmute(isco3      = as.character(isco08),          # 3-digit codes
            major_code = str_sub(as.character(soc10), 1, 2))

isco1_to_soc_major <- xw8808 %>%
  transmute(isco1 = str_sub(isco88, 1, 1),              # ISCO-88 major group
            isco3 = str_sub(isco08, 1, 3)) %>%
  filter(isco1 %in% as.character(1:9)) %>%              # 0 armed forces. Hence excluded
  inner_join(links_08_soc, by = "isco3", relationship = "many-to-many") %>%
  count(isco1, major_code) %>%
  group_by(isco1) %>%
  mutate(fraction = n / sum(n)) %>%
  ungroup() %>%
  select(isco1, major_code, fraction)

# SOC major group

struct18 <- read_excel(
  soc_file,
  skip = 8,
  col_names = c("major", "minor", "broad", "detailed", "title")
)

major_names <- struct18 %>%
  filter(!is.na(major)) %>%
  transmute(
    major_code  = str_sub(major, 1, 2),
    major_label = str_remove(title, " Occupations$")
  ) %>%
  distinct()

civilian_majors <- setdiff(major_names$major_code, "55")

short_soc_label <- function(code, title) {
  keywords <- title %>%
    str_replace_all("&", " ") %>%
    str_remove_all("(?i)\\b(of|and|the|other|miscellaneous)\\b") %>%
    str_remove_all("[,;]") %>%
    str_squish() %>%
    str_split("\\s+") %>%
    map_chr(~ paste(head(.x[nzchar(.x)], 3), collapse = " ")) %>%
    str_to_title()
  
  paste(code, keywords)
}

# 25-54 prime age workforce men

ipumsi <- read_dta(
  ipumsi_file,
  col_select = c(country, year, perwt, sex, age, empstat, occisco)
)

international_raw <- ipumsi %>%
  transmute(
    country = str_to_title(as.character(as_factor(country))),
    year    = as.integer(year),
    weight  = as.numeric(perwt),
    isco1   = as.character(as.integer(occisco)),   # ISCO-88 major group. NA if missing
    sex     = as.numeric(sex),
    age     = as.numeric(age),
    empstat = as.numeric(empstat)
  ) %>%
  filter(
    empstat == 1,
    sex == 1,
    age >= 25,
    age <= 54,
    weight > 0,
    country != "United States"
  )
rm(ipumsi); invisible(gc())

# samples without occisco are skipped,
# unclassified workers (occisco 11/97/98/99 or missing) are excluded and
# reported so the renormalization is visible
coverage <- international_raw %>%
  group_by(country, year) %>%
  summarise(
    pct_classified   = 100 * sum(weight[isco1 %in% as.character(1:9)]) / sum(weight),
    pct_unclassified = 100 - pct_classified -
      100 * sum(weight[isco1 == "10"], na.rm = TRUE) / sum(weight),
    .groups = "drop"
  )

skipped <- coverage %>% filter(pct_classified == 0)
if (nrow(skipped) > 0)
  message("Samples without usable occisco (skipped): ",
          paste(paste(skipped$country, skipped$year), collapse = "; "))

droppy <- coverage %>% filter(pct_classified > 0, pct_unclassified >= 5)
if (nrow(droppy) > 0)
  message("Samples with >=5% unclassified employment (excluded & renormalized): ",
          paste(sprintf("%s %d (%.1f%%)", droppy$country, droppy$year,
                        droppy$pct_unclassified), collapse = "; "))

international_major <- international_raw %>%
  filter(isco1 %in% as.character(1:9)) %>%
  inner_join(isco1_to_soc_major, by = "isco1", relationship = "many-to-many") %>%
  group_by(country, year, major_code) %>%
  summarise(
    emp = sum(weight * fraction),
    .groups = "drop"
  ) %>%
  group_by(country, year) %>%
  mutate(share = 100 * emp / sum(emp)) %>%
  ungroup() %>%
  select(country, year, major_code, share)

# compare to the respective ACS year sample

acs <- read_dta(
  acs_file,
  col_select = c(year, perwt, sex, age, empstat, empstatd, occsoc)
)

acs_major <- acs %>%
  transmute(
    year       = as.integer(year),
    weight     = as.numeric(perwt),
    sex        = as.numeric(sex),
    age        = as.numeric(age),
    empstat    = as.numeric(empstat),
    empstatd   = as.numeric(empstatd),
    occsoc     = trimws(occsoc)
  ) %>%
  filter(
    empstat == 1,
    empstatd %in% 10:12,
    sex == 1,
    age >= 25,
    age <= 54,
    weight > 0,
    !occsoc %in% c("", "0")
  ) %>%
  mutate(major_code = str_sub(occsoc, 1, 2)) %>%   # first 2 digits = major group
  filter(major_code %in% civilian_majors) %>%
  group_by(year, major_code) %>%
  summarise(emp = sum(weight), .groups = "drop") %>%
  group_by(year) %>%
  mutate(share = 100 * emp / sum(emp)) %>%
  ungroup() %>%
  transmute(year, major_code, country = "US", share)
rm(acs); invisible(gc())

# Plots

comparison_index <- international_major %>%
  distinct(country, year) %>%
  inner_join(acs_major %>% distinct(year), by = "year") %>%
  arrange(year, country)

pwalk(comparison_index, function(country, year) {
  
  other_country <- country
  sample_year <- year
  
  other <- international_major %>%
    filter(country == other_country, year == sample_year) %>%
    transmute(major_code, country = other_country, share)
  
  us <- acs_major %>%
    filter(year == sample_year) %>%
    select(major_code, country, share)
  
  plot_data <- bind_rows(us, other) %>%
    complete(
      major_code = civilian_majors,
      country = c("US", other_country),
      fill = list(share = 0)
    ) %>%
    left_join(major_names, by = "major_code") %>%
    mutate(soc_label = short_soc_label(major_code, major_label)) %>%
    group_by(major_code) %>%
    mutate(us_share = share[country == "US"][1]) %>%
    ungroup() %>%
    arrange(desc(us_share), major_code) %>%
    mutate(
      soc_label = factor(soc_label, levels = unique(soc_label)),
      country = factor(country, levels = c("US", other_country))
    )
  
  unclass_pct <- coverage %>%
    filter(country == other_country, year == sample_year) %>%
    pull(pct_unclassified)
  
  SUBT <- paste(
    "Men aged 25–54, civilian employed.",
    paste0("IPUMS International ", sample_year,
           " and same-year U.S. ACS."),
    sep = "\n"
  )
  
  CAPT <- sprintf(paste0(
    "%s occupations = ISCO-88 major groups, allocated to SOC majors by ",
    "crosswalk-link shares. Unclassified occupations (%.1f%%) excluded."),
    other_country, unclass_pct)
  
  plot <- ggplot(plot_data, aes(x = soc_label, y = share, fill = country)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.72) +
    scale_fill_manual(
      values = c(US = "#50C2E5", setNames("#C9495E", other_country))
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, .06))) +
    labs(
      x = "SOC major group",
      y = "% of employment",
      fill = NULL,
      title = paste("Employment share by SOC major group: US vs", other_country),
      subtitle = SUBT,
      caption = CAPT
    ) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "top",
      axis.text.x = element_text(
        angle = 45,
        hjust = 1,
        vjust = 1,
        size = 9
      ),
      panel.grid.minor.x = element_blank(),
      plot.margin = margin(5.5, 5.5, 16, 5.5)
    )
  
  print(plot)
  
  safe_country <- paste(other_country, sample_year) %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", "_")
  
  ggsave(
    file.path(
      output_dir,
      paste0("us_", safe_country, "_soc_major_share.png")
    ),
    plot = plot,
    width = 16,
    height = 10,
    dpi = 160,
    limitsize = FALSE
  )
})
