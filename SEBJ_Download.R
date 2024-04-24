key <- name_suggest(q = "Brassica", rank = "genus")$data$key[1]

occ_count(taxonKey = key, 
          hasCoordinate = TRUE, 
          basisOfRecord = "PRESERVED_SPECIMEN")

#occ_count suggests that we have 31962 variables

data <- occ_search(scientificName = "Brassica",
                   limit = 32000,
                   hasCoordinate = TRUE,
                   basisOfRecord = "PRESERVED_SPECIMEN")

write.xlsx(data$data,"Brassica.xlsx")

