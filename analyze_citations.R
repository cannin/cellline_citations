results_citations <- readRDS("results_citations.rds")
results_citations$citations <- as.numeric(results_citations$citations)

write.table(results_citations, "results_citations.txt",sep = "\t", quote = FALSE, row.names = FALSE, col.names = TRUE)
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
  idx <- which(results_citations$identifier == id)
  tmp <- results_citations$filtered_citations[idx] %>% unique %>% as.numeric %>% sum(., na.rm=TRUE)
  results_citations$summed_citations[idx] <- tmp
}

t1 <- results_citations[, c("identifier", "search", "summed_citations")]
citations_subset <- unique(t1)
citations_subset[which(citations_subset$summed_citations == 0), "summed_citations"] <- NA

citations_subset <- citations_subset[with(citations_subset, order(-summed_citations)), ]
head(citations_subset)

write.table(citations_subset, "citations_subset.txt", sep="\t", quote=FALSE, row.names = FALSE)


