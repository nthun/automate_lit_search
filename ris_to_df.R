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
        source = "psycInfo",
        psyid = str_match(ris, pattern = "ID  - (.*)") %>% .[,2],
        pmid = str_match(ris, pattern = "N1  - .* PMID: (\\d+)") %>% .[,2],
        doi = str_match(ris, pattern = "DO  - (.*)") %>% .[,2],
        title = str_match(ris, pattern = "T1  - (.*)") %>% .[,2],
        abstract = str_match(ris, pattern = "AB  - (.*)") %>% .[,2],
        date = str_match(ris, pattern = "Y1  - (.*)") %>% .[,2],
        year = str_match(date, "^(\\d{4}).*") %>% .[,2] %>% as.numeric(),
        month = str_match(date, "^\\d{4}.(\\d{2}).*") %>% .[,2] %>% as.numeric(),
        day = str_match(date, "^\\d{4}.\\d{2}.(\\d{2})") %>% .[,2] %>% as.numeric(),
        journal = str_match(ris, pattern = "JO  - (.*)") %>% .[,2],
        authors = str_match_all(ris, "AU  - (.*)") %>% 
                    map(~paste(.x[,2], collapse = "; ")) %>% 
                    unlist()
    ) %>% 
    select(-date) %>% 
    drop_na(psyid)



