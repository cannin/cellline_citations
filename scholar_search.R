library(rvest)
library(httr)
library(stringr)
library(readr)
library(digest)

source("slugify.R")

# EXAMPLES Google vs Pubmed 
# -dissertation -thesis -patent "cancer" "cell line" "squamous" "bb49 hnc"

# For: Error in the HTTP2 framing layer From: https://github.com/jeroen/curl/issues/156
httr::set_config(httr::config(http_version = 0))

# PARAMETERS ----
#ccl_dat <- readRDS("cellosaurus.rds")
ccl_dat <- readRDS("cclp_search_info.rds")

# Base URL, 1951 is the year that the Hela cell line was created. 1951 is the starting year of the search
base_url <- "https://scholar.google.com/scholar?hl=en&as_ylo=1950&as_yhi=&as_sdt=1%2C22&as_vis=1&&q="
uastring <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/81.0.4044.138 Safari/537.36"

# Additional search terms common to all searches 
search_prefix <- '-dissertation -thesis -patent "cancer" "cell line"'

# Cell lines with less than this many characters 
nchar_limit <- 4

# Sleep time in seconds 
time <- 60

# Without secondary terms, cell lines like Kuramochi (ovarian) will return cancer papers in 
# lung cancer; Kuramochi is a Japanese name
include_secondary_term <- TRUE

output_file <- "results_citations.rds"

cache_dir <- "cache"

if(file.exists(output_file)) {
  results_citations <- readRDS(output_file)
} else {
  results_citations <- ccl_dat
  results_citations$citations <- NA_integer_
}

# SEARCH ----
for(i in 1:nrow(results_citations)) {
  #i <- 1342

  #idx <- sample(1:nrow(ccl_dat), 1)
  idx <- i
  tmp_id <- ccl_dat[idx, "synonyms"]
  tmp_search <- ccl_dat[idx, "search"]
  
  #search <- paste0('"', tmp_search, '"')
  search <- tmp_search
  cell_line <- paste0('"', tmp_id, '"')
  
  if(include_secondary_term) {
    tmp_query <- paste(search_prefix, search, cell_line)
  } else {
    tmp_query <- paste(search_prefix, cell_line)
  }
  
  slug <- paste0(slugify(cell_line), "_", digest(tmp_query))
  
  #cat(tmp_query)
  query <- url_escape(tmp_query)

  url <- paste0(base_url, query)
  
  tmpFile <- paste0(cache_dir, "/", slug, ".rds")
  if(file.exists(tmpFile)) {
    session <- readRDS(tmpFile)
    #writeLines(content(session$response, "text"), paste0("cache/", slug, ".html"))
  } else {
    session <- html_session(url, user_agent(uastring))
    saveRDS(session, tmpFile)
    Sys.sleep(time)
  }
  
  #write_xml(content(session$response), paste0(tmp_id, ".html"))

  tmp <- session %>% 
    html_nodes(xpath='//*[@class="gs_ab_mdw"]') %>%
    html_text() 
  
  tmp_results <- str_split(tmp[2], " ")[[1]]
  if(grepl("[A-Za-z]", tmp_results[1])) {
    citations <- str_split(tmp[2], " ")[[1]][2]    
  } else {
    citations <- str_split(tmp[2], " ")[[1]][1]
  }
  
  citations <- str_replace_all(citations, ",", "")
  citations <- as.numeric(citations)
  
  if(grepl("^[0-9]$", tmp_id) || nchar(cell_line) <= nchar_limit) {
    citations <- NA_character_
    cat("HIT: ")
  }
  
  authors <- session %>% 
    html_nodes(xpath='//*[@class="gs_a"]') %>%
    html_text() 
  
  tmp_id <- str_replace_all(tmp_id, "[\\[\\]]", "")
  isAuthor <- any(grepl(tmp_id, authors, ignore.case=TRUE))
  
  cat("I: ", i , " T: ", Sys.time(), " A: ", isAuthor, " C: ", citations, " Q: ", tmp_query, " U: ", url, "\n")
  
  results_citations$citations[i] <- citations
  results_citations$isAuthor[i] <- isAuthor
  saveRDS(results_citations, output_file)
}

# -dissertation -thesis "cancer" "cell" "squamous" "lung" "DLRP" 
# -dissertation -thesis "cancer" "cell" "PWR-1E" "prostate"


