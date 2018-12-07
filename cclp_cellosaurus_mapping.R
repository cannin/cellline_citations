# Read in cellosaurus data and CCLP cell line names 
cellosaurus <- readRDS("cellosaurus_long.rds")
tmp_cellosaurus <- cellosaurus[,c(1:2)]
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
cclp_samples[i]
tmp_cellosaurus[idx, ]
cclp_samples[which(is.na(idx_all))]

# Create a data.frame mapping CCLP cell line names and cellosaurus identifiers  
mapping <- data.frame(cclp=cclp_samples[which(!is.na(idx_all))], 
                      cellosaurus=tmp_cellosaurus[idx_all[!is.na(idx_all)], "identifier"],
                      stringsAsFactors = FALSE)

tmp_cellosaurus[idx_all, ]
cclp_samples[which(is.na(idx_all))]

# Read in the information on cell lines missing from cellosaurus 
missing <- read.table("cclp_cellosaurus_mapping_missing.txt", sep="\t", stringsAsFactors = FALSE, header=TRUE, fill = TRUE)
tmp_missing <- missing[,1:2]

# Merge the cell lines in cellosaurus and those that were missing 
tmp <- rbind(mapping, tmp_missing)

t2 <- which(!(cclp_samples %in% tmp$cclp))
cclp_samples[t2]

# Get the indicies for the cell lines in and not in cellosaurus to search with in the next part 
i0 <- which(tmp$cellosaurus == "")
i1 <- which(!(tmp$cellosaurus == ""))
i2 <- tmp$cellosaurus[i1]
i3 <- which(tmp_cellosaurus$identifier %in% i2)

length(i0)+length(i1)

# Merge information for cell lines in cellosaurus and those missing from cellosaurus 
y0 <- cellosaurus[(cellosaurus$identifier %in% tmp$cellosaurus[i1]), c(1:2,6)]
y0$synonyms <- tolower(y0$synonyms)
y0 <- unique(y0)
#y0 <- y0[, c(2,3)]

#y0 <- cellosaurus[(cellosaurus$identifier %in% c("180D", "1783")), c(2,6)]
y1 <- missing[missing$cclp %in% tmp$cclp[i0], ]
colnames(y1) <- colnames(y0)
y1$synonyms <- y1$identifier

## Search info will contain the information that will be searched in Google Scholar
search_info <- rbind(y0, y1)

saveRDS(search_info, "cclp_search_info.rds")

