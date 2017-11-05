library(tidyverse)
library(magrittr)
library(stringr)
# library(openxlsx)
library(httr)

api_key <- read_lines("scopus_api_key.txt")
query_string <- "(video game) AND (psychophysiology) AND cortisol"
query_string <- read_lines("query_string_2.txt")

# The scopus search API  uses a format like this
# http = "http://api.elsevier.com/content/search/scopus?start=25&count=25&query=%28video+game%29+AND+%28psychophysiology%29&apiKey=b75bc87dc8f893f4476a2108657d8080"

# BATCH DOWNLOAD SCOPUS INFO
start_index <- 0
items_per_page <- 25 # This is the maximum that API can handle
list_view <- "COMPLETE"

http <-
    paste0(
        "http://api.elsevier.com/content/search/scopus",
        "?start=", start_index,
        "&count=", items_per_page,
        "&query=", URLencode(query_string, reserved = TRUE),
        "&view=", list_view, # Complete search returns full author list and abstract
        "&apiKey=", api_key
    )

# Read the first page for essential information
newpage <- GET(http)
page_content <- newpage %>% content()
hits <- page_content$`search-results`$`opensearch:totalResults` %>% as.numeric() # All hits

# Collect all unique results through iteration
for (current_start in seq(items_per_page, hits, items_per_page)) {
    newpage <-
        httr::GET(
            paste0(
                "http://api.elsevier.com/content/search/scopus",
                "?start=", current_start,
                "&count=", 25,
                "&query=", URLencode(query_string, reserved = TRUE),
                "&view=", list_view, # Complete search returns full author list and abstract
                "&apiKey=", api_key
            )
        )
    
    page_content %<>% append(content(newpage))
    print(
        paste0(
            "Collecting data: ", current_start,
            " to ", current_start + items_per_page,
            " of ", hits
        )
    )
}

# Prepare contents for parsing. Keep only the entry element
simple_contents <- 
    page_content %>% 
    map(., "entry") %>%
    flatten()

# Parse the contents into a tibble
scopus_articles <- 
    tibble(
        doi = map(simple_contents, "prism:doi"),
        eid = map_chr(simple_contents, "eid"),
        sid = map_chr(simple_contents, "dc:identifier") %>% str_replace("SCOPUS_ID:",""),
        pmid = map(simple_contents, "pubmed-id"),
        title = map_chr(simple_contents, "dc:title"),
        journal = map_chr(simple_contents, "prism:publicationName"),
        authors = map(simple_contents, "author") %>% # Select the author element containing all authors
                    map_chr(~map_chr(.x, "authname") %>% # Select the authname elements
                    paste(collapse = "; ") %>%  # Make it one string
                    str_replace_all("[.]","")), # Replace dots to match pubmed dataset
        date = map_chr(simple_contents, "prism:coverDate"),
        year = str_replace(date, "(^\\d{4})-.*","\\1") %>% as.numeric(),
        month = str_replace(date, "^.*-(\\d{2})-.*$","\\1") %>% as.numeric(),
        day = str_replace(date, "^.*-.*-(\\d+$)","\\1") %>% as.numeric(),
        abstract =  map(simple_contents, "dc:description")
        ) %>% 
    rowwise() %>% # Elements containing NULL cannot be parsed well, so have to replace with NA
    mutate(
        doi = ifelse(is_null(doi), NA_character_, as.character(doi)),
        pmid = ifelse(is_null(pmid), NA_character_, as.character(pmid))
        ) %>% 
    select(-date)

