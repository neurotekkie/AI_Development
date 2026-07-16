

library(tidyverse)
library(readxl)

downloads <- "/Users/karentao/Downloads"
output_dirs <- c(
  downloads,
  "/Users/karentao/MIT Dropbox/Karen Tao/AI_and_Development_MIT_FutureTech/Karen"
)
out_cols <- c("country", "native_code", "isco_or_soc_code", "code_type",
              "occupation_title", "task_statement")

write_outputs <- function(df, filename) {
  walk(output_dirs, ~ write_csv(df, file.path(.x, filename), na = ""))
}

us <- read_excel(file.path(downloads, "db_30_3_excel/Task Statements.xlsx"),
                 col_types = "text") %>%
  transmute(
    country = "United States",
    native_code = `O*NET-SOC Code`,
    isco_or_soc_code = `O*NET-SOC Code`,
    code_type = "O*NET-SOC",
    occupation_title = Title,
    task_statement = Task
  )

india <- read_csv(file.path(downloads, "india_2015_tasks.csv"),
                  col_types = cols(.default = col_character())) %>%
  transmute(
    country = "India",
    native_code = nco_code,
    isco_or_soc_code = str_sub(nco_code, 1, 4),
    code_type = "ISCO-08",
    occupation_title = occupation_title,
    task_statement = task
  )

china_isco <- read_csv(file.path(downloads, "zhiye_csco2022_isco08.csv"),
                       col_types = cols(.default = col_character())) %>%
  distinct(occ_code, .keep_all = TRUE) %>%
  select(occ_code, isco08)

china <- read_csv(file.path(downloads, "zhiye_dadian_2022_tasks_EN.csv"),
                  col_types = cols(.default = col_character())) %>%
  left_join(china_isco, by = "occ_code") %>%
  transmute(
    country = "China",
    native_code = occ_code,
    isco_or_soc_code = isco08,   # NA where the CSCO chain found no match
    code_type = "ISCO-08",
    occupation_title = name_en,
    task_statement = task_en
  )

colombia <- read_csv(file.path(downloads, "cuoc_2025/cuoc_funciones_EN.csv"),
                     col_types = cols(.default = col_character())) %>%
  transmute(
    country = "Colombia",
    native_code = cuoc_code,
    isco_or_soc_code = str_sub(cuoc_code, 1, 4),
    code_type = "ISCO-08",
    occupation_title = nombre_en,
    task_statement = funcion_en
  )

merged <- bind_rows(us, india, china, colombia) %>%
  filter(!is.na(task_statement), str_squish(task_statement) != "") %>%
  select(all_of(out_cols))

write_outputs(merged, "cross_country_tasks.csv")

tasks_per_occupation <- merged %>%
  count(country, native_code, isco_or_soc_code, code_type, occupation_title,
        name = "n_tasks") %>%
  arrange(country, native_code)

write_outputs(tasks_per_occupation,
               "cross_country_tasks_per_occupation.csv")

country_summary <- tasks_per_occupation %>%
  group_by(country) %>%
  summarise(
    n_occupations = n(),
    mean_tasks = round(mean(n_tasks), 1),
    median_tasks = median(n_tasks),
    min_tasks = min(n_tasks),
    max_tasks = max(n_tasks),
    occ_without_code = n_distinct(native_code[is.na(isco_or_soc_code)]),
    n_tasks = sum(n_tasks),
    .groups = "drop"
  ) %>%
  relocate(n_tasks, .after = n_occupations)

write_outputs(country_summary,
               "cross_country_task_summary.csv")

print(country_summary)
