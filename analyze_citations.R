library(magrittr)
library(readr)

work_dir <- "./"
results_citations_rds <- paste0(work_dir, "results_citations.rds")
results_citations_txt <- paste0(work_dir, "results_citations.txt")
results_citations_binned_txt <- paste0(work_dir, "results_citations_binned.txt")
citations_subset_txt <- paste0(work_dir, "citations_subset.txt")

results_citations <- readRDS(results_citations_rds)
write.table(results_citations, results_citations_txt, sep="\t", quote=FALSE, row.names = FALSE)

results_citations$citations <- as.numeric(results_citations$citations)

write.table(results_citations, results_citations_txt, sep = "\t", quote = FALSE, row.names = FALSE, col.names = TRUE)
unique(results_citations$search)

t1 <- gsub("[^A-Za-z]", "", results_citations$synonyms)
t2 <- gsub(" ", "", t1)
letterCnt <- sapply(t2, nchar)

t1 <- gsub("[^0-9]", "", results_citations$synonyms)
t2 <- gsub(" ", "", t1)
numCnt <- sapply(t2, nchar)

results_citations$letterCnt <- letterCnt
results_citations$numCnt <- numCnt
results_citations$totalCnt <- letterCnt + numCnt

# Ignore based on rules 
ignore_entries <- which(results_citations$letterCnt == 0 | (results_citations$numCnt == 0 & results_citations$totalCnt < 4))
results_citations$filtered_citations <- results_citations$citations
results_citations$filtered_citations[ignore_entries] <- NA

# Ignore based on obvious issues 
drop_synonyms <- c("scott", "lcms", "kelly", "lc-ms", "ishida", 
                   "giant cell tumor", "goto", "becker", "human erythroleukemia", "sima",
                   "messa", "mes-sa", "dang")
results_citations$filtered_citations[which(results_citations$synonyms %in% drop_synonyms)] <- NA

results_citations$summed_citations <- NA
ids <- unique(results_citations$identifier)
for(id in ids) {
  #id <- "SiMa"
  #id <- "639V"
  idx <- which(results_citations$identifier == id)
  tmp <- results_citations$filtered_citations[idx] %>% unique %>% as.numeric
  
  if(all(is.na(tmp))) {
    tmp <- NA    
  } else {
    tmp <- sum(tmp, na.rm=TRUE)
  }

  results_citations$summed_citations[idx] <- tmp
}

write.table(results_citations, results_citations_txt, sep = "\t", quote = FALSE, row.names = FALSE, col.names = TRUE)

# Remove cell lines with 0 citations 
t1 <- results_citations[, c("identifier", "search", "summed_citations")]
citations_subset <- unique(t1)
citations_subset[which(citations_subset$summed_citations == 0), "summed_citations"] <- NA

citations_subset <- citations_subset[with(citations_subset, order(-summed_citations)), ]
head(citations_subset)

write.table(citations_subset, citations_subset_txt, sep="\t", quote=FALSE, row.names = FALSE)

# BIN CITATIONS ----
results_citations <- read_tsv(results_citations_txt, col_types = cols(
  identifier = col_character(),
  synonyms = col_character(),
  search = col_character(),
  citations = col_double(),
  letterCnt = col_double(),
  numCnt = col_double(),
  totalCnt = col_double(),
  filtered_citations = col_double(),
  summed_citations = col_double()
))

tmp_citations <- results_citations[!is.na(results_citations$summed_citations),]

breaks <- c(-Inf, 10, 100, 1000, 10000, Inf)
labels <- c("<10", "<100", "<1000", "<10000", ">10000")

binned_citations<- cut(results_citations$summed_citations, breaks = breaks, labels = labels)
tmp <- cbind(results_citations, binned_citations=binned_citations)

t1 <- tmp[, c("identifier", "binned_citations")] %>% unique
table(t1$binned_citations)
table(t1$binned_citations) %>% sum

t2 <- merge(results_citations, t1, by="identifier", all.x=TRUE)
t2 <- t2[with(t2, order(binned_citations)), ]
t3 <- unique(t2[, c("identifier", "summed_citations", "binned_citations")])
t3 <- t3[order(t3$summed_citations), ]
write_tsv(t3, results_citations_binned_txt)


