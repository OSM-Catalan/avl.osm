municipis_avl <- names(avl_ctv)
municipis_osm <- monitorOSM::municipis[monitorOSM::municipis$regio == "PV", ]
municipis_osm <- municipis_osm[, c("name:ca", "comarca", "osm_type", "osm_id")]
names(municipis_osm) <- gsub("^name:ca$", "osm_name", names(municipis_osm))

grep(", ", municipis_avl, value = TRUE)

tesaurus_municipis <- matrix(NA_character_, nrow = length(municipis_avl), ncol = 1 + ncol(municipis_osm),
                             dimnames = list(NULL, c("avl_nom", names(municipis_osm))))
tesaurus_municipis[, 1] <- municipis_avl
# tesaurus_municipis$avl[1:length(municipis_avl)] <- municipis_avl
noms <- sapply(strsplit(municipis_avl, ", "), function(x) {
  if (length(x) == 1) return (x)
  out <- if (grepl("'", x[2])){
    paste0(x[2], x[1])
  } else {
    paste(x[2], x[1])
  }
})

for (i in seq_len(nrow(tesaurus_municipis))) {
  candidats <- grep(paste0("^", noms[i], "$"), municipis_osm$osm_name, ignore.case = TRUE)
  if (length(candidats) == 0) {
    candidats <- grep(noms[i], municipis_osm$osm_name, ignore.case = TRUE)
  }

  if (length(candidats) == 1) {
    tesaurus_municipis[i, names(municipis_osm)] <- as.character(municipis_osm[candidats, ])
  } else if (length(candidats) > 1){
    warning(municipis_avl[i], " - candidats: ", municipis_osm[candidats, ])
  }

  # municipis_osm[candidats, ]
}
tesaurus_municipis <- as.data.frame(tesaurus_municipis)

tesaurus_municipis$avl[is.na(tesaurus_municipis[, "osm_id"])]
sort(municipis_osm$osm_name[!municipis_osm$osm_id %in% tesaurus_municipis$osm_id])

# TODO: Afegir Torís a la relació de predomini lingüístic Valencià ----


## Enllaç manual ----

tesaurus_municipis[tesaurus_municipis$avl == "ALCOSSER DE PLANES", names(municipis_osm)] <-
  as.character(municipis_osm[municipis_osm$osm_name == "Alcosser", ])

tesaurus_municipis[tesaurus_municipis$avl == "ALFARA D'ALGÍMIA", names(municipis_osm)] <-
  as.character(municipis_osm[municipis_osm$osm_name == "Alfara de la Baronia", ])

tesaurus_municipis[tesaurus_municipis$avl == "BELL-LLOC", names(municipis_osm)] <-
  as.character(municipis_osm[municipis_osm$osm_name == "Benlloc", ])

tesaurus_municipis[tesaurus_municipis$avl == "ÈNOVA, L'", names(municipis_osm)] <-
  as.character(municipis_osm[municipis_osm$osm_name == "l'Énova", ])

tesaurus_municipis[tesaurus_municipis$avl == "DE BENITATXELL, EL", names(municipis_osm)] <-
  as.character(municipis_osm[municipis_osm$osm_name == "el Poble Nou de Benitatxell", ])

tesaurus_municipis[tesaurus_municipis$avl == "REAL DE MONTROI", names(municipis_osm)] <-
  as.character(municipis_osm[municipis_osm$osm_name == "Real", ])

tesaurus_municipis[tesaurus_municipis$avl == "SANT JOAN DE L'ÈNOVA", names(municipis_osm)] <-
  as.character(municipis_osm[municipis_osm$osm_name == "Sant Joanet", ])

tesaurus_municipis[tesaurus_municipis$avl == "TORÍS", names(municipis_osm)] <-
  c("Torís", "la Ribera Alta", "relation", "345966")
#   as.character(municipis_osm[municipis_osm$osm_name == "", ])

tesaurus_municipis[tesaurus_municipis$avl == "VILANOVA DE CASTELLÓ", names(municipis_osm)] <-
  as.character(municipis_osm[municipis_osm$osm_name == "Castelló", ])


apply(tesaurus_municipis, 2, function(x) any(duplicated(x)))

usethis::use_data(tesaurus_municipis, overwrite = TRUE)
