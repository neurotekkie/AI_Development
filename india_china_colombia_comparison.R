library(haven)
library(tidyverse)
library(readxl)
library(iscoCrosswalks)
library(pulso)

DL <- "~/Downloads"

# I use the iscoCrosswalks package derived from the BLS ISCO-SOC table
isco08_soc10 <- iscoCrosswalks::isco08_soc10

crosswalk_frac <- isco08_soc10 %>%
  transmute(isco_code = as.character(isco08), soc_code = as.character(soc10)) %>%
  distinct() %>%
  group_by(isco_code) %>% mutate(fraction = 1 / n()) %>% ungroup()

xw1018 <- read_excel(file.path(DL, "soc_2010_to_2018_crosswalk.xlsx"), skip = 9,
                     col_names = c("soc10_code", "soc10_title",
                                   "soc18_code", "soc18_title")) %>%
  filter(str_detect(soc10_code, "^\\d{2}-\\d{4}$"),
         str_detect(soc18_code, "^\\d{2}-\\d{4}$")) %>%
  transmute(soc10 = str_remove(soc10_code, "-"),
            soc18 = str_remove(soc18_code, "-")) %>%
  distinct() %>%
  group_by(soc10) %>% mutate(frac = 1 / n()) %>% ungroup()

# ISCO-88 to 08
xw8808 <- occupationcross::crosstable_isco08_isco88 %>%
  transmute(isco88 = str_pad(as.character(`ISCO-88 code`), 4, "left", "0"),
            isco08 = str_pad(as.character(`ISCO 08 Code`), 4, "left", "0")) %>%
  filter(str_detect(isco88, "^\\d{4}$"), str_detect(isco08, "^\\d{4}$")) %>%
  distinct() %>%
  group_by(isco88) %>% mutate(frac88 = 1 / n()) %>% ungroup()

# ISCO-08 to SOC-2018
isco3_to_soc18 <- crosswalk_frac %>%
  inner_join(xw1018, by = c("soc_code" = "soc10"), relationship = "many-to-many") %>%
  transmute(isco3 = isco_code, soc18, w = fraction * frac)

to_soc18 <- function(df_isco3) {   # df: isco3, emp -> soc18, emp
  un <- anti_join(df_isco3, isco3_to_soc18, by = "isco3")
  list(
    soc18 = df_isco3 %>%
      inner_join(isco3_to_soc18, by = "isco3", relationship = "many-to-many") %>%
      group_by(soc18) %>% summarise(emp = sum(emp * w), .groups = "drop"),
    unmatched_pct = 100 * sum(un$emp) / sum(df_isco3$emp)
  )
}

# India data is PLFS (3 digit)
plfs <- read_dta(file.path(DL, "PLFS_Data_2022-22_STATA/cperv1.dta"))

in_isco3 <- plfs %>%
  transmute(
    nco_3digit = trimws(b5pt1q6_cperv1),
    status     = trimws(as.character(b5pt1q3_cperv1)),
    sex        = trimws(as.character(b4q5_cperv1)),   # 1=male, 2=female, 3=transgender
    age        = as.numeric(b4q6_perv1),
    weight     = mult_cperv1 /
      ifelse(trimws(nss_cperv1) == trimws(nsc_cperv1), 100, 200) /
      as.numeric(no_qtr_cperv1)
  ) %>%
  filter(status %in% c("11", "12", "21", "31", "41", "51"),
         sex == "1", age >= 25, age <= 54,
         !is.na(nco_3digit), nco_3digit != "") %>%
  count(isco3 = nco_3digit, wt = weight, name = "emp")

# China CFPS data is coded as ISCO-88, hence just need to switch to ISCO 08. Weight code is rswt_natcs22n.

cfps <- read_dta(file.path(DL, "CFPS2022Stata_EN/CFPS2022Stata_unzip_password_in_the_instructions/ecfps2022person_202410.dta"),
                 col_select = c(employ, rswt_natcs22n, qg303code_isco, gender, age))

cn_emp <- cfps %>%
  transmute(w    = as.numeric(rswt_natcs22n),
            i88a = as.numeric(qg303code_isco),
            employ = as.numeric(employ),
            male  = as.numeric(gender) == 1,     # gender: 1=male, 0=female
            age   = as.numeric(age)) %>%
  filter(employ == 1, w > 0, male, age >= 25, age <= 54) %>%
  mutate(isco88 = coalesce(ifelse(i88a > 0, i88a, NA))) #drop -1, -2, -8
message(sprintf("China: employed without occupation code (excluded): %.1f%%",
                100 * sum(cn_emp$w[is.na(cn_emp$isco88)]) / sum(cn_emp$w)))

cn_raw <- cn_emp %>%
  filter(!is.na(isco88)) %>%
  count(isco88 = sprintf("%04d", isco88), wt = w, name = "emp")

codes88 <- unique(xw8808$isco88)
cn88 <- cn_raw %>%
  mutate(units = map(isco88, function(cc) {
    if (cc %in% codes88) return(cc)
    codes88[str_starts(codes88, str_remove(cc, "0+$"))]
  }))

cn_isco3 <- cn88 %>%
  filter(lengths(units) > 0) %>%
  mutate(nunit = lengths(units)) %>%
  unnest(units) %>%
  transmute(isco88 = units, emp = emp / nunit) %>%
  inner_join(xw8808, by = "isco88", relationship = "many-to-many") %>%
  count(isco3 = str_sub(isco08, 1, 3), wt = emp * frac88, name = "emp")

# Colombia
#oficio_c8 is occupation
# gender is p3271. 1=man 
# age is p6040
co_ocu <- pulso_load(year = 2022, month = 1, module = "ocupados")
co_car <- pulso_load(year = 2022, month = 1, module = "caracteristicas_generales") %>%
  select(directorio, secuencia_p, orden, p3271, p6040)

co_isco3 <- co_ocu %>%
  inner_join(co_car, by = c("directorio", "secuencia_p", "orden")) %>%
  filter(fex_c18 > 0, !is.na(oficio_c8),
         p3271 == 1, p6040 >= 25, p6040 <= 54) %>%
  mutate(oficio = str_pad(as.character(oficio_c8), 4, "left", "0")) %>%
  count(isco3 = str_sub(oficio, 1, 3), wt = fex_c18, name = "emp")
#fex_c18 is the weight per person


# I downloaded ACS 2022
# perwt is the weight per person
# to restrict to civilian only, I use EMPSTAT 1 and EMPSTATD 10-12
us <- read_dta(file.path(DL, "acs 2022.dta"),
               col_select = c(perwt, sex, age, empstat, empstatd, occsoc)) %>%
  filter(as.numeric(empstat) == 1, as.numeric(empstatd) %in% 10:12,
         as.numeric(sex) == 1,                       # 1=male, 2=female
         as.numeric(age) >= 25, as.numeric(age) <= 54) %>%
  transmute(w = as.numeric(perwt), occsoc = trimws(occsoc)) %>%
  filter(!is.na(occsoc), !occsoc %in% c("", "0"))

# minor and major SOC codes
struct18 <- read_excel(file.path(DL, "soc_structure_2018.xlsx"), skip = 8,
                       col_names = c("major", "minor", "broad", "detailed", "title"))

major_names <- struct18 %>%
  filter(!is.na(major)) %>%
  transmute(major_code  = str_sub(major, 1, 2),
            major_label = str_remove(title, " Occupations$"))

# first 2 digits = major group, first 3 =minor group
minor_names <- struct18 %>%
  filter(!is.na(minor)) %>%
  transmute(minor_pref  = str_sub(str_remove(minor, "-"), 1, 3),
            minor_code  = minor,
            minor_label = title)
stopifnot(!any(duplicated(minor_names$minor_pref)))
stopifnot(all(str_detect(str_sub(us$occsoc, 1, 3), "^\\d{3}$")))

# drop all military occupations
soc18_by_country <- list(India = in_isco3, China = cn_isco3, Colombia = co_isco3) %>%
  imap(function(df, cty) {
    r <- to_soc18(df)
    message(sprintf("%s: ISCO 3-digit codes with no SOC mapping: %.2f%% of employment",
                    cty, r$unmatched_pct))
    r$soc18 %>% filter(str_sub(soc18, 1, 2) != "55")
  })

major_shares <- imap_dfr(soc18_by_country, ~.x %>%
                           group_by(major_code = str_sub(soc18, 1, 2)) %>%
                           summarise(share = 100 * sum(emp) / sum(.x$emp), .groups = "drop") %>%
                           mutate(country = .y))

minor_shares <- imap_dfr(soc18_by_country, ~.x %>%
                           group_by(minor_pref = str_sub(soc18, 1, 3)) %>%
                           summarise(share = 100 * sum(emp) / sum(.x$emp), .groups = "drop") %>%
                           mutate(country = .y))

us1 <- us %>% filter(str_sub(occsoc, 1, 2) != "55")
major_shares <- bind_rows(major_shares,
                          us1 %>% group_by(major_code = str_sub(occsoc, 1, 2)) %>%
                            summarise(share = 100 * sum(w) / sum(us1$w), .groups = "drop") %>%
                            mutate(country = "US"))
minor_shares <- bind_rows(minor_shares,
                          us1 %>% group_by(minor_pref = str_sub(occsoc, 1, 3)) %>%
                            summarise(share = 100 * sum(w) / sum(us1$w), .groups = "drop") %>%
                            mutate(country = "US"))

# ordered by U.S. share
CTY  <- c("US", "India", "China", "Colombia")   # bar order within each group
PAL  <- c(US = "#056875", India = "#50C2E5", China = "#C9495E", Colombia = "#D46600")
SUBT <- paste("Men aged 25–54, civilian employed.",
              "India: PLFS 2022. China: CFPS 2022.",
              "Colombia: GEIH 2022 January. US: ACS 2022 (IPUMS).",
              sep = "\n")


major_cmp <- major_shares %>%
  pivot_wider(names_from = country, values_from = share, values_fill = 0) %>%
  left_join(major_names, by = "major_code") %>%
  mutate(major_label = fct_reorder(major_label, US))

p_major <- major_cmp %>%
  pivot_longer(all_of(CTY), names_to = "country", values_to = "share") %>%
  mutate(country = factor(country, CTY)) %>%
  ggplot(aes(share, major_label, fill = country)) +
  geom_col(position = position_dodge2(reverse = TRUE), width = 0.8) +
  geom_text(aes(label = sprintf("%.1f", share)),
            position = position_dodge2(width = 0.8, reverse = TRUE),
            hjust = -0.15, size = 2.2) +
  scale_x_continuous(expand = expansion(mult = c(0, .09))) +
  scale_fill_manual(values = PAL, breaks = CTY) +
  labs(x = "% of employment", y = NULL, fill = NULL,
       title = "Employment share by SOC major group: US, India, China, Colombia",
       subtitle = SUBT) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top")

minor_cmp <- minor_shares %>%
  pivot_wider(names_from = country, values_from = share, values_fill = 0) %>%
  left_join(minor_names, by = "minor_pref") %>%
  mutate(lab = fct_reorder(str_wrap(paste(minor_code, minor_label), 50), US))


plot_minor <- function(dat, subtitle_head) {
  dat %>%
    pivot_longer(all_of(CTY), names_to = "country", values_to = "share") %>%
    mutate(country = factor(country, CTY)) %>%
    ggplot(aes(share, lab, fill = country)) +
    geom_col(position = position_dodge2(reverse = TRUE), width = 0.8) +
    geom_text(aes(label = sprintf("%.1f", share)),
              position = position_dodge2(width = 0.8, reverse = TRUE),
              hjust = -0.15, size = 2.2) +
    scale_x_continuous(expand = expansion(mult = c(0, .09))) +
    scale_fill_manual(values = PAL, breaks = CTY) +
    labs(x = "% of employment", y = NULL, fill = NULL,
         title = "Employment share by SOC minor group: US, India, China, Colombia",
         subtitle = paste(subtitle_head, SUBT, sep = "\n")) +
    theme_minimal(base_size = 10) +
    theme(legend.position = "top")
}

p_minor_all <- plot_minor(minor_cmp, "SOC Minor Groups")

print(p_major); print(p_minor_all)
ggsave(file.path(DL, "comparison_soc_major_share.png"),       p_major,     width = 10, height = 11, dpi = 160)
ggsave(file.path(DL, "comparison_soc_minor_share.png"),   p_minor_all, width = 10, height = 48, dpi = 160)

library(tidyverse)
#plot each country individually against the U.S.

output_dir <- file.path(DL, "pairwise_soc_plots")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

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

plot_pairwise_soc <- function(shares, names, code_col, title_col, group_name,
                              filename_suffix, width, label_size) {
  shares %>%
    inner_join(names, by = code_col) %>%
    filter(!is.na(.data[[title_col]])) %>%
    mutate(soc_label = short_soc_label(.data[[code_col]], .data[[title_col]])) %>%
    {
      plot_source <- .
      if (!"US" %in% plot_source$country) stop("Shares must include US.")
      countries <- plot_source %>% distinct(country) %>% pull(country) %>%
        setdiff("US") %>% sort()
      
      walk(countries, function(other_country) {
        plot_data <- plot_source %>%
          filter(country %in% c("US", other_country)) %>%
          complete(
            nesting(!!rlang::sym(code_col), !!rlang::sym(title_col), soc_label),
            country = c("US", other_country), fill = list(share = 0)
          ) %>%
          group_by(!!rlang::sym(code_col)) %>%
          mutate(us_share = share[country == "US"][1]) %>%
          ungroup() %>%
          arrange(desc(us_share), !!rlang::sym(code_col)) %>%
          mutate(
            soc_label = factor(soc_label, levels = unique(soc_label)),
            country = factor(country, levels = c("US", other_country))
          )
        
        plot <- ggplot(plot_data, aes(x = soc_label, y = share, fill = country)) +
          geom_col(position = position_dodge(width = 0.8), width = 0.72) +
          scale_fill_manual(values = c(US = "#50C2E5", setNames("#C9495E", other_country))) +
          scale_y_continuous(expand = expansion(mult = c(0, .06))) +
          labs(
            x = paste("SOC", group_name, "group"), y = "% of employment", fill = NULL,
            title = paste("Employment share by SOC", group_name, "group: US vs", other_country),
            subtitle = SUBT
          ) +
          theme_minimal(base_size = 11) +
          theme(
            legend.position = "top",
            axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = label_size),
            panel.grid.minor.x = element_blank(),
            plot.margin = margin(5.5, 5.5, 16, 5.5)
          )
        
        print(plot)
        safe_country <- other_country %>% str_to_lower() %>% str_replace_all("[^a-z0-9]+", "_")
        ggsave(
          file.path(output_dir, paste0("us_", safe_country, "_", filename_suffix, ".png")),
          plot = plot, width = width, height = 10, dpi = 160, limitsize = FALSE
        )
      })
    }
}

plot_pairwise_soc(
  minor_shares, minor_names, "minor_pref", "minor_label", "minor",
  "soc_minor_share", width = 32, label_size = 7
)
plot_pairwise_soc(
  major_shares, major_names, "major_code", "major_label", "major",
  "soc_major_share", width = 16, label_size = 9
)