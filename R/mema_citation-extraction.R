################################################################################

# Misinformation meta-analysis: accessing citations

################################################################################

# loading packages -------------------------------------------------------------
library(rcrossref)
library(bib2df)

# loading data -----------------------------------------------------------------
read.csv(INSERT CORRECT PATH)

# Retrieving reference information from DOIs -----------------------------------

if (!file.exists("data/misinformation_citations.bib")) {
  
  doi_unique <- unique(raw$doi)
  
  citations <- cr_cn(
    dois = doi_unique,
    format = "bibtex",
    style = "apa"
  )
  
  writeLines(unlist(citations), "data/misinformation_citations.bib")
  
}

citation_data <- bib2df("data/misinformation_citations.bib")

colnames(citation_data) <- tolower(colnames(citation_data))

citation_data$category <- tolower(citation_data$category)

citation_clean <- citation_data %>% 
  select(
    bibtexkey, category, author, title, year, journal, doi
  )

raw_cite <- raw %>% 
  select(
    -title, -container, -record_type
  ) %>% 
  left_join(citation_clean, by = "doi") %>% 
  select(
    id_record, id_study, id_control, id_effect,
    authors, publication_year,
    bibtexkey, category, author, title, year, journal, doi,
    everything()
  )

## Transform author list column into a character column

raw <- raw %>% 
  select(
    -title, -container, -record_type
  ) %>% 
  left_join(citation_clean, by = "doi") %>% 
  select(
    id_record, id_study, id_control, id_effect,
    authors, publication_year,
    bibtexkey, category, author, title, year, journal, doi,
    everything()
  )

raw$author <- map_chr(raw_cite$author, function(x) {paste(unlist(x), sep = " ", collapse = "; ")})