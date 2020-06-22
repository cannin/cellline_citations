library(xml2)
library(magrittr)

# NOTE: Given a current cellosaurus_long.rds the search column added can be 
# modified by code in the next section 

# READ CELLOSAURUS ----
# Read and extract out various items of info from cellosaurus XML into data.frame
xml <- read_xml("cellosaurus.xml_040820.zip")
#tx1 <- xml_find_all(xml, "//cell-line[@category='Cancer cell line'][./species-list/cv-term[@accession='9606']]")
tx1 <- xml_find_all(xml, "//cell-line[./str-list/source-list/source[contains(text(),'Cosmic-CLP')]]")

results <- data.frame(identifier=character(0),
                      synonyms=character(0),
                      disease=character(0),
                      ncit=character(0),
                      species=character(0),
                      stringsAsFactors=FALSE)

for(i in 1:length(tx1)) {
    cat("I: ", i, "\n")
    tmp_tx1 <- tx1[[i]]

    identifier <- xml_text(xml_find_all(tmp_tx1, "./name-list/name[@type='identifier']"))
    synonyms <- paste(xml_text(xml_find_all(tmp_tx1, "./name-list/name[@type='synonym']")), collapse = "|")
    if(length(synonyms) == 0) { synonyms <- NA_character_ }

    disease <- xml_text(xml_find_first(tmp_tx1, "./disease-list/cv-term[@terminology='NCIt']"))
    if(length(disease) == 0) { disease <- NA_character_ }
    stopifnot(length(disease) == 1)

    ncit <- xml_attr(xml_find_first(tmp_tx1, "./disease-list/cv-term[@terminology='NCIt']"), "accession")
    if(length(ncit) == 0) { ncit <- NA_character_ }
    stopifnot(length(ncit) == 1)

    species <- xml_text(xml_find_all(tmp_tx1, "./species-list/cv-term"))
    if(length(species) == 0) { species <- NA_character_ }
    stopifnot(length(species) == 1)

    if(species == "Homo sapiens") {
        tmp_df <- data.frame(identifier=identifier,
                            synonyms=synonyms,
                            disease=disease,
                            ncit=ncit,
                            species=species,
                            stringsAsFactors=FALSE)
        results <- rbind(results, tmp_df)

        saveRDS(results, "cellosaurus.rds")
    }
}

# SPLIT SYNONYMS ----
results <- readRDS("cellosaurus.rds")

# Rather than one row per cell line, separate the synonyms into their own rows
results_long <- data.frame(identifier=character(0),
                           synonyms=character(0),
                           disease=character(0),
                           ncit=character(0),
                           species=character(0),
                           stringsAsFactors=FALSE)

for(i in 1:nrow(results)) {
#for(i in 1:10) {
    cat("I: ", i, "\n")

    tmp_results <- data.frame(identifier=results$identifier[i],
                              synonyms=results$identifier[i],
                              disease=results$disease[i],
                              ncit=results$ncit[i],
                              species=results$species[i],
                              stringsAsFactors=FALSE)

    results_long <- rbind(results_long, tmp_results)

    if(nchar(results$synonyms[i]) > 0) {
        tmp <- str_split(results$synonyms[i], "\\|")[[1]]

        for(j in 1:length(tmp)) {
            tmp_results <- data.frame(identifier=results$identifier[i],
                                     synonyms=tmp[j],
                                     disease=results$disease[i],
                                     ncit=results$ncit[i],
                                     species=results$species[i],
                                     stringsAsFactors=FALSE)

            results_long <- rbind(results_long, tmp_results)
        }
    }

    saveRDS(results_long, "cellosaurus_long.rds")
}

# ADD SEARCH FIELD ----
results_long <- readRDS("cellosaurus_long.rds")
results_long <- results_long[, !(colnames(results_long) %in% "search")]

# tmp <- unique(results_long[, c("ncit", "disease")])
# tmp <- tmp[order(tmp$disease),]
# write.table(tmp, "ncit_disease_terms_oncotree.txt", sep="\t", quote=FALSE, row.names = FALSE)

ncit_search_mapping <- read.table("ncit_disease_search_terms.txt", sep="\t", header = TRUE, stringsAsFactors = FALSE)

# Do basic processing that will help simplify the disease terms to unique keywords
t1 <- sapply(results_long$synonyms, nchar)
t2 <- gsub('([[:punct:]])', ' ', results_long$identifier[1]) %>% trimws %>% tolower

## Rules to reduce the disease info to more unique search keywords
t3 <- ncit_search_mapping$search %>% tolower %>% trimws
results_long <- merge(results_long, ncit_search_mapping, by=c("ncit", "disease"), all.x = TRUE)

#writeLines(sort(unique(t4)), "del_cellosaurus_long_search_terms.txt")

# Save results
saveRDS(results_long, "cellosaurus_long.rds")

#unique(results_long$search)
