library(xml2)
library(magrittr)

# Read and extract out various items of info from cellosaurus XML into data.frame
xml <- read_xml("cellosaurus_xml_051418.zip")
tx1 <- xml_find_all(xml, "//cell-line[@category='Cancer cell line']")

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
    if(length(synonyms) == 0) { synonyms <- NA_character_}

    disease <- xml_text(xml_find_all(tmp_tx1, "./disease-list/cv-term"))
    if(length(disease) == 0) { disease <- NA_character_}

    ncit <- xml_attr(xml_find_all(tmp_tx1, "./disease-list/cv-term"), "accession")
    if(length(ncit) == 0) { ncit <- NA_character_}

    species <- xml_text(xml_find_all(tmp_tx1, "./species-list/cv-term"))
    if(length(species) == 0) { species <- NA_character_}

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

results_long <- readRDS("cellosaurus_long.rds")

# Do basic processing that will help simplify the disease terms to unique keywords
t1 <- sapply(results_long$synonyms, nchar)
t2 <- gsub('([[:punct:]])', ' ', results_long$identifier[1]) %>% trimws %>% tolower

## Rules to reduce the disease info to more unique search keywords
t3 <- results_long$disease %>%
    tolower %>%
    gsub('adenocarcinoma', ' ', .) %>%
    gsub('carcinoma', ' ', .) %>%
    gsub('tumor', ' ', .) %>%
    gsub('childhood', ' ', .) %>%
    gsub('adult', ' ', .) %>%
    gsub('infant', ' ', .) %>%
    gsub('\\scell\\s', ' ', .) %>%
    gsub('hereditary', ' ', .) %>%
    gsub('cancer', ' ', .) %>%
    gsub('with', ' ', .) %>%
    gsub('gland', ' ', .) %>%
    gsub('upper', ' ', .) %>%
    gsub('recurrent', ' ', .) %>%
    gsub('acute', ' ', .) %>%
    gsub('malignant', ' ', .) %>%
    gsub('undifferentiated', ' ', .) %>%
    gsub('diffuse', ' ', .) %>%
    gsub('small', ' ', .) %>%
    gsub('system', ' ', .) %>%
    gsub('clear', ' ', .) %>%
    gsub('combined', ' ', .) %>%
    #gsub('syndrome', ' ', .) %>%
    gsub('system', ' ', .) %>%
    gsub('large', ' ', .) %>%
    gsub('invasive', ' ', .) %>%
    gsub('disease', ' ', .) %>%
    gsub('variant', ' ', .) %>%
    gsub('tissue', ' ', .) %>%
    gsub('otherwise', ' ', .) %>%
    gsub('specified', ' ', .) %>%
    gsub('of the', ' ', .) %>%
    gsub('well', ' ', .) %>%
    gsub('differentiated', ' ', .) %>%
    gsub('borderline', ' ', .) %>%
    gsub('gene fusions', ' ', .) %>%
    gsub('mixed', ' ', .) %>%
    gsub('grade', ' ', .) %>%
    gsub('low', ' ', .) %>%
    gsub('high', ' ', .) %>%
    gsub('immature', ' ', .) %>%
    gsub('inflammatory', ' ', .) %>%
    gsub('benign', ' ', .) %>%
    gsub('squamous', ' ', .) %>%
    gsub('non-', ' ', .) %>%
    gsub('papillary', ' ', .) %>%
    gsub('poorly', ' ', .) %>%
    gsub('sarcomatoid', ' ', .) %>%
    gsub('serous', ' ', .) %>%
    gsub('soft', ' ', .) %>%
    gsub('giant', ' ', .) %>%
    gsub('sporadic', ' ', .) %>%
    gsub('congenital', ' ', .) %>%
    gsub('sporadic', ' ', .) %>%
    gsub('intrinsic', ' ', .) %>%
    gsub('juvenile', ' ', .) %>%
    gsub('sporadic', ' ', .) %>%
    gsub('mature', ' ', .) %>%
    gsub('embryonal', ' ', .) %>%
    gsub('mucinous', ' ', .) %>%
    gsub('primitive', ' ', .) %>%
    gsub('pleomorphic', ' ', .) %>%
    gsub('mucinous', ' ', .) %>%
    gsub('round', ' ', .) %>%
    gsub('stromal', ' ', .) %>%
    gsub('plasma', ' ', .) %>%
    gsub('type', ' ', .) %>%
    gsub('primary', ' ', .) %>%
    gsub('multilayered', ' ', .) %>%
    gsub('extragonadal', ' ', .) %>%
    gsub('essential', ' ', .) %>%
    gsub('capillary', ' ', .) %>%
  
    gsub('\\sand\\s', ' ', .) %>%
    gsub('\\sthe\\s', ' ', .) %>%
    gsub('\\sof\\s', ' ', .) %>%
    gsub(',', ' ', .) %>%

    gsub('uterine\\s.+', 'uterine', .) %>%
    gsub('thyroid\\s.+', 'thyroid', .) %>%
    gsub('testicular\\s.+', 'testicular', .) %>%
    gsub('salivary\\s.+', 'salivary', .) %>%
    gsub('renal\\s.+', 'renal', .) %>%
    gsub('rectal\\s.+', 'rectal', .) %>%
    gsub('pancreatic\\s.+', 'pancreatic', .) %>%
    gsub('ovarian\\s.+', 'ovarian', .) %>%
    gsub('oral\\s.+', 'oral', .) %>%
    gsub('ovarian\\s.+', 'ovarian', .) %>%
    gsub('lung\\s.+', 'lung', .) %>%
    gsub('gastric\\s.+', 'gastric', .) %>%
    gsub('cervical\\s.+', 'cervical', .) %>%
    gsub('vulvar\\s.+', 'vulvar', .) %>%
    gsub('vaginal\\s.+', 'vaginal', .) %>%
    gsub('skin\\s.+', 'skin', .) %>%
    gsub('nasal\\s.+', 'nasal', .) %>%
    gsub('liver\\s.+', 'liver', .) %>%
    gsub('hepatocellular\\s.+', 'hepatocellular', .) %>%
    gsub('intestinal\\s.+', 'intestinal', .) %>%
    gsub('hepatocellular\\s.+', 'hepatocellular', .) %>%
    gsub('hepatocellular\\s.+', 'hepatocellular', .) %>%

    gsub('\\sneoplasm$', '', .) %>%
    gsub('\\ssquamous$', '', .) %>%
    gsub('\\sadeno$', '', .) %>%

    gsub('.*leukemia.*', 'leukemia', .) %>%
    gsub('.*lymphoma.*', 'lymphoma', .) %>%
    gsub('.*mesothelioma.*', 'mesothelioma', .) %>%
    gsub('.*leukemia.*', 'leukemia', .) %>%
    gsub('.*breast.*', 'breast', .) %>%
    gsub('.*mesothelioma.*', 'mesothelioma', .) %>%
    gsub('.*endocrine.*', 'endocrine', .) %>%
    gsub('.*uveal.*', 'uveal', .) %>%
    gsub('.*melanoma.*', 'melanoma', .) %>%
    gsub('.*lung.*', 'lung', .) %>%
    gsub('.*skin.*', 'skin', .) %>% # askin?
    gsub('.*endocrine.*', 'endocrine', .) %>%
    gsub('.*colon.*', 'colon', .) %>%
    gsub('.*pancreatic.*', 'pancreatic', .) %>%
    gsub('.*colon.*', 'colon', .) %>%
    gsub('.*mouth.*', 'mouth', .) %>%
    gsub('.*colon.*', 'colon', .) %>%
    gsub('.*cervical.*', 'cervical', .) %>%
    gsub('.*liposarcoma.*', 'cervical', .) %>%
    gsub('.*cervical.*', 'cervical', .) %>%
    gsub('.*neuroectodermal.*', 'neuroectodermal', .) %>%
    gsub('.*neuroectodermal.*', 'neuroectodermal', .) %>%

    gsub('\\snot$', ' ', .) %>%  
  
    gsub('  ', ' ', .) %>%
    gsub('  ', ' ', .) %>%
    gsub('  ', ' ', .) %>%
    trimws

t3 %>% sort %>% unique

results_long$search <- t3

# Save results
saveRDS(results_long, "cellosaurus_long.rds")

#unique(results_long$search)
