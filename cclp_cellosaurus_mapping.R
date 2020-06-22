# Read in cellosaurus data and CCLP cell line names 
cellosaurus <- readRDS("cellosaurus_long.rds")
tmp_cellosaurus <- cellosaurus[,c("identifier", "synonyms")]
tmp_cellosaurus <- unique(tmp_cellosaurus)
cclp_samples <- readLines("cclp_samples.txt")

idx_all <- integer(0) 

# Find the cell lines in cellosaurus 
for(i in 1:length(cclp_samples)) {
  #i <- 1 
  cat("I: ", i, "\n")
  
  idx <- which(tmp_cellosaurus$synonyms == cclp_samples[i])

  if(length(idx) == 1) {
    idx_all <- c(idx_all, idx)
  } else {
    idx_all <- c(idx_all, NA_integer_)
  }
}

## Debug 
length(idx_all)
cclp_samples[i]
tmp_cellosaurus[idx, ]
cclp_samples[which(is.na(idx_all))]

# Create a data.frame mapping CCLP cell line names and cellosaurus identifiers  
# NOTE: The loop beforew was over cclp_samples
mapping <- data.frame(cclp=cclp_samples[which(!is.na(idx_all))], 
                      cellosaurus=tmp_cellosaurus[idx_all[!is.na(idx_all)], "identifier"],
                      stringsAsFactors = FALSE)

tmp_cellosaurus[idx_all[!is.na(idx_all)], "identifier"] %>% length
cclp_samples[which(!is.na(idx_all))] %>% length

# Read in the information on cell lines missing from cellosaurus 
missing <- read.table("cclp_cellosaurus_mapping_missing.txt", sep="\t", stringsAsFactors = FALSE, header=TRUE, fill = TRUE)
tmp_missing <- missing[, c("cclp", "cellosaurus")]

# Merge the cell lines in cellosaurus and those that were missing 
tmp <- rbind(mapping, tmp_missing)
nrow(tmp)

# DEBUG Duplicated
tmp$cclp[which(duplicated(tmp$cclp))]

# Lines that are missing still
t2 <- cclp_samples[which(!(cclp_samples %in% tmp$cclp))]
t2

# Get the indicies for the cell lines in and not in cellosaurus to search with in the next part 
i0 <- which(tmp$cellosaurus == "") # not in cellosaurus
i1 <- which(!(tmp$cellosaurus == "")) # in cellosaurus
i2 <- tmp$cellosaurus[i1]
i3 <- which(tmp_cellosaurus$identifier %in% i2)

length(i0)+length(i1)
stopifnot(length(i0)+length(i1) == length(cclp_samples))

# Merge information for cell lines in cellosaurus and those missing from cellosaurus 
selected_identifiers <- tmp$cellosaurus[i1]
stopifnot(length(i1) == length(selected_identifiers))
length(which(!(unique(cellosaurus$identifier) %in% selected_identifiers)))
y0 <- cellosaurus[(cellosaurus$identifier %in% selected_identifiers), c("identifier", "synonyms", "search")]
y0$synonyms <- tolower(y0$synonyms)
y0 <- unique(y0)
stopifnot(length(i1) == length(unique(y0$identifier)))
#y0 <- y0[, c(2,3)]

#y0 <- cellosaurus[(cellosaurus$identifier %in% c("180D", "1783")), c(2,6)]
y1 <- missing[missing$cclp %in% tmp$cclp[i0], ]
colnames(y1) <- colnames(y0)
y1$synonyms <- tolower(y1$identifier)

## Search info will contain the information that will be searched in Google Scholar
search_info <- rbind(y0, y1)

t3 <- c(unique(search_info$synonyms), tmp_missing$cclp)
stopifnot(length(which(!(tolower(cclp_samples) %in% tolower(t3)))) == 0)
stopifnot(length(cclp_samples) == length(unique(search_info$identifier)))

saveRDS(search_info, "cclp_search_info.rds")
write_tsv(search_info, "cclp_search_info.txt")

