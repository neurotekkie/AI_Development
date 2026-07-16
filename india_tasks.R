library(pdftools)
library(tidyverse)

vol_a <- "/Users/karentao/MIT Dropbox/Karen Tao/AI_and_Development_MIT_FutureTech/data/raw/India NCO Data/NCO_2015/National_Classification_of_Occupations_Vol_II-A-2015.pdf"
vol_b <- "/Users/karentao/MIT Dropbox/Karen Tao/AI_and_Development_MIT_FutureTech/data/raw/India NCO Data/NCO_2015/National_Classification_of_Occupations_Vol_II-B-2015.pdf"
output_dir <- "/Users/karentao/Downloads"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# start pattern
code_pattern <- "^\\d{4}\\.\\d{4}$"
block_end_pattern <- regex(
  "ISCO\\s*0?8\\s+Unit|Qualification\\s+Pack\\s+Details|National\\s+Occupational\\s+Standards",
  ignore_case = TRUE
)

clean_description <- function(text) {
  text <- str_replace_all(text, "[\\r\\n]+", " ") %>% str_squish()
  
  # Stop before lists of alternative designations or qualification content.
  stop_pattern <- paste(
    "\\bIs designated according to\\b",
    "\\bIs designated as\\b",
    "\\bIs designated\\b",
    "\\bMay be designated\\b",
    "\\bMay be known as\\b",
    "\\bAlso known as\\b",
    "\\bAlso called\\b",
    "\\bIncluded are\\s*:",
    "\\b(?:includes?|including)\\b[^.]{0,100}\\bsuch as\\s*:",
    "\\bMay specialize in\\b",
    "\\bQualification Pack\\b",
    "\\bQualification Pack Details\\b",
    "\\bNational Occupational Standards\\b",
    "NATIONAL CLASSIFICATION OF OCCUPATIONS",
    "\\bQP Code\\b",
    sep = "|"
  )
  
  str_split(text, regex(stop_pattern, ignore_case = TRUE), n = 2) %>%
    purrr::map_chr(1) %>%
    str_squish()
}

make_tasks <- function(descriptions) {
  lapply(descriptions, function(description) {
    sentences <- str_split(description, "(?<=[.!?])\\s+(?=[A-Z])")[[1]]
    sentences <- str_squish(sentences)
    sentences <- sentences[nzchar(sentences)]
    
    merged <- character()
    for (sentence in sentences) {
      short_fragment <- str_count(sentence, "\\S+") < 3
      if (short_fragment && length(merged) > 0) {
        merged[length(merged)] <- str_c(merged[length(merged)], sentence, sep = " ")
      } else {
        merged <- c(merged, sentence)
      }
    }
    merged
  })
}

# Delete first sentence
remove_intro_sentence <- function(description) {
  sentences <- make_tasks(description)[[1]]
  if (length(sentences) > 1) str_c(sentences[-1], collapse = " ") else description
}

# Remove the occupation title in task statement
strip_title_prefix <- function(text, title) {
  text <- str_squish(text)
  title <- str_squish(title)
  
  if (!is.na(title) && nzchar(title) &&
      startsWith(str_to_lower(text), str_to_lower(title))) {
    text <- str_sub(text, str_length(title) + 1)
  }
  
  text %>%
    str_remove("^[[:space:][:punct:]]+") %>%
    str_squish()
}

# Remove leading noun in the description

strip_leading_name_phrase <- function(text) {

  text <- str_squish(text)

  # Handle "include all other ... performing ..."
  inclusion <- str_locate(
    text,
    regex("\\binclude[s]?\\s+all\\s+other\\b.*?\\bperforming\\b",
          ignore_case = TRUE)
  )

  if (!is.na(inclusion[1, "start"]) &&
      inclusion[1, "start"] <= 120) {

    return(
      str_c(
        "perform",
        str_sub(text, inclusion[1, "end"] + 1)
      ) %>%
        str_squish()
    )
  }

  action_pattern <- paste(
    "may",
    "can",
    "must",
    "should",
    "include[s]?",
    "perform[s]?",
    "plan[s]?",
    "organize[s]?",
    "co-ordinate[s]?",
    "coordinate[s]?",
    "control[s]?",
    "direct[s]?",
    "supervise[s]?",
    "determine[s]?",
    "manage[s]?",
    "operate[s]?",
    "maintain[s]?",
    "develop[s]?",
    "prepare[s]?",
    "provide[s]?",
    "inspect[s]?",
    "install[s]?",
    "repair[s]?",
    "construct[s]?",
    "serve[s]?",
    "work[s]?",
    "head[s]?",
    "set[s]?",
    "act[s]?",
    sep = "|"
  )

  position <- str_locate(
    text,
    regex(
      paste0("\\b(", action_pattern, ")\\b"),
      ignore_case = TRUE
    )
  )

  # Only strip if the first verb occurs almost immediately after the
  # occupation name. Long noun phrases are likely legitimate titles.
  if (!is.na(position[1, "start"]) &&
      position[1, "start"] > 1 &&
      position[1, "start"] <= 20) {

    text <- str_sub(text, position[1, "start"])
  }

  str_squish(text)
}

# Reject PDF-extraction fragments rather than exporting them as tasks.
is_complete_task <- function(text) {
  words <- str_count(str_squish(text), "\\S+")
  words >= 3 &&
    !str_detect(text, regex("^(is|are|was|were|work)\\.?$", TRUE)) &&
    !str_detect(text, regex("\\b(is|are|was|were|and|or|but|of|to|with)\\.?$", TRUE))
}

# Reconstruct left and right columns independently. 
reconstruct_lines <- function(pdf_file) {
  pages <- pdf_data(pdf_file)

  purrr::imap_dfr(pages, function(tokens, page_number) {
    if (nrow(tokens) == 0) return(tibble())
    tokens %>%
      # Drop the running header (~y 37) and footer (~y 789 in II-B, ~y 802 in
      # II-A); body text sits between y ~78 and y ~765.
      filter(y > 60, y < 780) %>%
      mutate(column = if_else(x + width / 2 < 306, 1L, 2L)) %>%
      group_by(column, y) %>%
      arrange(x, .by_group = TRUE) %>%
      summarise(line = str_c(text, collapse = " "), .groups = "drop") %>%
      arrange(column, y) %>%
      mutate(pdf_page = as.integer(page_number))
  }) %>%
    arrange(pdf_page, column, y) %>%
    mutate(line = str_squish(line)) %>%
    filter(nzchar(line))
}

# The job description restates the occupation title as its opening subject,
# but title and description wrap at different points (the title is set in a
# larger face), so either accumulated string may be a prefix of the other:
#   title "Administrative Official, Union" + "Government"
#   description "Administrative Official, Union" / "Government serves in ..."
# When the wording differs outright (title "Legislators, Other" vs
# description "Elected Officials, Other include ..."), fall back on the
# vertical gap: lines within a block sit 14-18pt apart, the title/description
# boundary ~30pt apart.
begins_description <- function(line, title, gap) {
  title <- str_to_lower(str_squish(str_c(title, collapse = " ")))
  line <- str_to_lower(line)
  startsWith(line, title) || startsWith(title, line) ||
    (!is.na(gap) && gap >= 25)
}

# Walk the reconstructed lines one by one
walk_lines <- function(lines, volume) {
  entries <- vector("list", nrow(lines))
  n_entries <- 0L
  state <- "idle"
  code <- NA_character_
  title <- character()
  desc <- character()
  entry_page <- NA_integer_
  entry_column <- NA_integer_
  prev_page <- NA_integer_
  prev_column <- NA_integer_
  prev_y <- NA_real_

  save_entry <- function() {
    if (state == "desc" && length(desc) > 0) {
      n_entries <<- n_entries + 1L
      entries[[n_entries]] <<- tibble(
        nco_code = code,
        occupation_title = str_squish(str_c(title, collapse = " ")),
        job_description = str_c(desc, collapse = " "),
        volume = volume,
        pdf_page = entry_page,
        column = entry_column
      )
    }
  }

  for (i in seq_len(nrow(lines))) {
    line <- lines$line[i]
    page <- lines$pdf_page[i]
    column <- lines$column[i]
    y <- lines$y[i]

    # Vertical gap to the previous line, defined only within one column of
    # one page; across a column or page break the prefix test must decide.
    gap <- if (identical(page, prev_page) && identical(column, prev_column)) {
      y - prev_y
    } else {
      NA_real_
    }
    prev_page <- page
    prev_column <- column
    prev_y <- y

    if (str_detect(line, code_pattern)) {
      save_entry()
      code <- line
      title <- character()
      desc <- character()
      entry_page <- page
      entry_column <- column
      state <- "title"
    } else if (state == "title") {
      if (str_detect(line, block_end_pattern)) {
        state <- "idle"  # entry with no job description
      } else if (length(title) > 0 && begins_description(line, title, gap)) {
        desc <- line
        state <- "desc"
      } else if (length(title) >= 6) {
        state <- "idle"  # runaway accumulation: mis-parse, discard
      } else {
        title <- c(title, line)
      }
    } else if (state == "desc") {
      hit <- str_locate(line, block_end_pattern)
      if (!is.na(hit[1, "start"])) {
        # Keep any description words merged onto the metadata heading's line.
        leading_text <- str_squish(str_sub(line, 1, hit[1, "start"] - 1))
        if (nzchar(leading_text)) desc <- c(desc, leading_text)
        save_entry()
        state <- "idle"
      } else {
        desc <- c(desc, line)
      }
    }
  }

  bind_rows(entries[seq_len(n_entries)])
}

parse_volume <- function(pdf_file, volume) {
  reconstruct_lines(pdf_file) %>%
    walk_lines(volume)
}

occupations <- bind_rows(
  parse_volume(vol_a, "II-A"),
  parse_volume(vol_b, "II-B")
) %>%
  mutate(job_description = purrr::map_chr(job_description, clean_description)) %>%
  filter(
    nzchar(job_description),
    !str_detect(occupation_title, regex("qualification pack|qp details", TRUE)),
    !str_detect(job_description, regex("^qp nos reference|^qualification pack", TRUE))
  ) %>%
  distinct(nco_code, occupation_title, .keep_all = TRUE) %>%
  mutate(job_description = purrr::map_chr(job_description, remove_intro_sentence)) %>%
  mutate(job_description = purrr::map2_chr(job_description, occupation_title, strip_title_prefix)) %>%
  filter(!str_detect(job_description, regex("^is designated\\b", TRUE))) %>%
  arrange(nco_code)

tasks <- occupations %>%
  mutate(task = make_tasks(job_description)) %>%
  unnest_longer(task, indices_to = "task_number") %>%
  mutate(task = purrr::map2_chr(task, occupation_title, strip_title_prefix)) %>%
  mutate(task = purrr::map_chr(task, strip_leading_name_phrase)) %>%
  filter(
    !str_detect(task, regex("^is designated\\b", TRUE)),
    purrr::map_lgl(task, is_complete_task)
  ) %>%
  group_by(nco_code) %>%
  mutate(task_number = row_number()) %>%
  ungroup() %>%
  select(nco_code, occupation_title, task_number, task, volume, pdf_page)

write_csv(occupations, file.path(output_dir, "india_2015_occupations.csv"))
write_csv(tasks, file.path(output_dir, "india_2015_tasks.csv"))
