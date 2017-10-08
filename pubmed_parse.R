library(tidyverse)
devtools::install_github("nthun/easyPubMed")
library(easyPubMed)
library(stringr)
library(openxlsx)

query_string <- "
(problem Internet Use) OR (Problematic Internet Use) OR (Internet addiction) OR (Compulsive internet use) OR (pathological internet use) OR (excessive internet use) OR (internet dependence)OR  
(Computer game addiction) OR (Problem gaming) OR (Problematic Gaming) OR (Gaming addiction) OR (Gaming disorder) OR (Video game addiction) OR (Problematic video gaming) OR (Problematic online gaming) OR (Online gaming addiction) OR (pathological gaming) OR (excessive gaming) OR (videogame dependence)" 

pubmed_ids <- get_pubmed_ids(query_string)

article_list <- 
    query_results_xml %>% 
    articles_to_list()

article_df <- 
    map_df(article_list, article_to_df) %>% 
    as_tibble()

temp <- 
article_df %>% 
    mutate(authors = paste(lastname, firstname, sep = ", ")) %>% 
    group_by(pmid, doi, title, abstract, year, month, day, jabbrv, journal) %>% 
    summarise(authors = paste(authors, collapse = "; "),
              emails = paste(email, collapse = "; ")) %>% 
    drop_na(pmid)

### BATCH DOWNLOAD ALL HITS IN XML FORMAT AND SAVE THEM TO DISK IN BATCHES
# out.A <- batch_pubmed_download(pubmed_query_string = query_string,
#                                format = "xml",
#                                batch_size = 150,
#                                dest_file_prefix = "internet_addiction_pubmed_")



pubmed_records <-
    tibble(filename = paste0("pubmed_hits_xml/" ,list.files(path = "pubmed_hits_xml/", ".xml"))) %>% 
    mutate(xml = map_chr(filename, read_file))

# Parse the xmls to csvs and save them to a library
pwalk(list(pubmed_records$xml, pubmed_records$filename), ~table_articles_byAuth(..1, max_chars = 3000, included_authors = "all", dest_file = str_replace_all(..2, "xml","csv")))

pubmed_articles <-
    tibble(filename = paste0("pubmed_hits_csv/" ,list.files(path = "pubmed_hits_csv/", ".csv"))) %>% 
    mutate(df = map(filename, read_csv, na = c("","NA"))) %>% 
    unnest(df) %>% 
    mutate(authors = paste(lastname, firstname, sep = ", ")) %>% 
    group_by(pmid, doi, title, abstract, year, month, day, journal, jabbrv) %>%
    summarise(authors = paste(authors, collapse = "; ")) %>%
    ungroup() %>% 
    drop_na(pmid)


write.xlsx(pubmed_articles, "all_pubmed_hits.xlsx")

