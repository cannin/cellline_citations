library(rvest)
library(httr)
library(stringr)

source("slugify.R")

#ccl_dat <- readRDS("cellosaurus.rds")
ccl_dat <- readRDS("cclp_search_info.rds")

# Base URL, 1951 is the year that the Hela cell line was created. 1951 is the starting year of the search
base_url <- "https://scholar.google.com/scholar?hl=en&as_ylo=1950&as_yhi=&as_sdt=0%2C33&q="
uastring <- "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2228.0 Safari/537.36"

# Additional search terms common to all searches 
search_prefix <- '-dissertation -thesis "cancer" "cell"'

# Cell lines with less than this many characters 
ncharLimit <- 4

# Sleep time in seconds 
time <- 60

#results_citations <- ccl_dat
#results_citations$citations <- NA_integer_

results_citations <- readRDS("results_citations.rds")

for(i in 2993:nrow(results_citations)) {
  #i <- 1 

  #idx <- sample(1:nrow(ccl_dat), 1)
  idx <- i
  tmpSearch <- ccl_dat[idx, "search"]
  tmpId <- ccl_dat[idx, "synonyms"]
  
  slug <- paste0(tmpId, "_", tmpSearch) %>% slugify
  
  tissue <- paste0('"', tmpSearch, '"')
  cell_line <- paste0('"', tmpId, '"')
  tmpQuery <- paste(search_prefix, tissue, cell_line)
  #cat(tmpQuery)
  query <- url_escape(tmpQuery)

  url <- paste0(base_url, query)
  
  tmpFile <- paste0("cache/", slug, ".rds")
  if(file.exists(tmpFile)) {
    session <- readRDS(tmpFile)
    #writeLines(content(session$response, "text"), paste0("cache/", slug, ".html"))
  } else {
    session <- html_session(url, user_agent(uastring))
    saveRDS(session, tmpFile)
  }

  tmp <- session %>% 
    html_nodes(xpath='//*[@class="gs_ab_mdw"]') %>%
    html_text() 
  
  tmpResults <- str_split(tmp[2], " ")[[1]]
  if(grepl("[A-Za-z]", tmpResults[1])) {
    citations <- str_split(tmp[2], " ")[[1]][2]    
  } else {
    citations <- str_split(tmp[2], " ")[[1]][1]
  }
  
  citations <- str_replace_all(citations, ",", "")
  citations <- as.numeric(citations)
  
  if(grepl("^[0-9]$", tmpId) || nchar(cell_line) <= ncharLimit) {
    citations <- NA_character_
    cat("HIT: ")
  }
  
  cat("I: ", i , " T: ", Sys.time(), " C: ", citations, " Q: ", tmpQuery, "\n")
  
  results_citations$citations[i] <- citations
  saveRDS(results_citations, "results_citations.rds")
    
  Sys.sleep(time)
}

# -dissertation -thesis "cancer" "cell" "squamous" "lung" "DLRP" 
# -dissertation -thesis "cancer" "cell" "PWR-1E" "prostate"


