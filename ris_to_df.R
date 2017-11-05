# Ris to df
library(tidyverse)
library(stringr)

# Prepare the ris file for processing
ris <- 
    read_file("psycInfo_1.ris") %>% 
    str_split("ER  - \\r\\n") %>% # Split file at certain points
    unlist()

# Parse the ris file to a tibble
psynet_articles <-
    tibble(
        psyid = str_match(ris, pattern = "ID  - (.*)") %>% .[,2],
        pmid = str_match(ris, pattern = "N1  - .* PMID: (\\d+)") %>% .[,2],
        doi = str_match(ris, pattern = "DO  - (.*)") %>% .[,2],
        journal = str_match(ris, pattern = "JO  - (.*)") %>% .[,2],
        authors = str_extract_all(ris, "AU  - (.*)") %>% 
                  map(~gsub(x = .x, "AU  - ", "") %>% 
                    paste(., collapse = "; ")) %>% 
                    unlist(),
        date = str_match(ris, pattern = "Y1  - (.*)") %>% .[,2],
        year = str_match(date, "^(\\d{4}).*") %>% .[,2] %>% as.numeric(),
        month = str_match(date, "^\\d{4}.(\\d{2}).*") %>% .[,2] %>% as.numeric(),
        day = str_match(date, "^\\d{4}.\\d{2}.(\\d{2})") %>% .[,2] %>% as.numeric(),
        title = str_match(ris, pattern = "T1  - (.*)") %>% .[,2],
        abstract = str_match(ris, pattern = "AB  - (.*)") %>% .[,2]
    ) %>% 
    select(-date)


