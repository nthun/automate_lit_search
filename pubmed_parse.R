library(tidyverse)
library(magrittr)
# devtools::install_github("nthun/easyPubMed") # I fixed a bug in the original code, so now this is from my own github repo
library(easyPubMed)
library(stringr)
library(openxlsx)
# source("table_articles_byAuth.R") # Bugfix of the original function from easyPubMed, if the github repo install would not work, try this.

# Define the query string. You should put together the query in the PubMed site, and just copy paste it here if unsure about the syntax
query_string <- read_lines("query_string.txt")

## SETUP FOLDER STRUCTURE TO RECEIVE DATA
# Create a xml directory if not exists
if (
    list.dirs(recursive = F) %>% 
    str_detect(., "pubmed_hits_xml") %>% 
    any(. == TRUE) %>% 
    not()) {dir.create("pubmed_hits_xml")}

# Create a csv directory if not exists    
if (
    list.dirs(recursive = F) %>% 
    str_detect(., "pubmed_hits_csv") %>% 
    any(. == TRUE) %>% 
    not()) {dir.create("pubmed_hits_csv")}

## BATCH DOWNLOAD ALL HITS IN XML FORMAT AND SAVE THEM TO DISK IN BATCHES
# This can take a _very_long time to run, but the results are saved locally, so it only has to be done once
output <- batch_pubmed_download(pubmed_query_string = query_string,
                               format = "xml",
                               batch_size = 150,
                               dest_file_prefix = "pubmed_hits_xml/internet_addiction_pubmed_")

pubmed_records <-
    tibble(filename = paste0("pubmed_hits_xml/" ,list.files(path = "pubmed_hits_xml/", ".xml") # get all xml files from the subdir
    )) %>%
    mutate(xml = map_chr(filename, read_file)) # Read all xml files (as character strings)to a data frame

## Batch parse the xmls to csv-s and save them to another library locally
# This can also take a long time, but again, the results are saved locally, so it only has to be done once
pwalk( # pwalk takes any number of parameters (..1, ..2, etc.) and iterates the function, but only for the side effects (in this case, creating files)
    list(pubmed_records$xml, pubmed_records$filename), # Define the parameters
    ~ table_articles_byAuth(
        ..1, # This parameter tells what to parse
        max_chars = 3000, # The maximum number of characters to parse from the abstract. I would like to have the full abstracts, so 3000 should be enough.
        included_authors = "all", # Get all authors in different rows
        dest_file = str_replace_all(..2, "xml", "csv") # save all to a different library
    )
)

# Prepare the final dataset from the saved csv-s
# The reason for this is to put all data together, and also to collapse author names. So this way, each row will correspond to one publication, while the author names are preserved
pubmed_articles <-
    tibble(filename = paste0("pubmed_hits_csv/" ,list.files(path = "pubmed_hits_csv/", ".csv"))) %>% # Read all csv files in subdir into a data frame
    mutate(df = map(filename, read_csv, na = c("","NA"))) %>% # Read all csvs into a nested dataframe 
    unnest(df) %>% # Unnest all data and bind by rows
    mutate(authors = paste(lastname, firstname, sep = ", ")) %>% # Make a single name var
    group_by(pmid, doi, title, abstract, year, month, day, journal) %>%
    summarise(authors = paste(authors, collapse = "; ")) %>% # Collapse author names
    ungroup() %>% 
    drop_na(pmid) %>%  # Drop all records without pmid (it is always an empty record in pubmed)
    filter(!duplicated(doi) & !duplicated(pmid) & !duplicated(title)) # Remove duplicates by doi, pmid, and title

# Write result as an xlsx
write.xlsx(pubmed_articles, "all_pubmed_hits.xlsx")

hits <- read.xlsx("all_pubmed_hits.xlsx")


