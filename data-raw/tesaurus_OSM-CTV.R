library(avl.osm)
library(osmdata)
library(sf)
library(pbapply)


## Combinació de topònims AVL - OSM

summary(sapply(avl_ctv, nrow))


## Cerca per noms coincidents dins d'un municipi ----
osm_CTVL <- list()
sel_mun <- seq_along(avl_ctv)

pb <- timerProgressBar(max = length(sel_mun))
for (i in sel_mun) {
  davl <- avl_ctv[[i]]
  davl$`ref:avl` <- rownames(davl)
  municipi <- tesaurus_municipis[tesaurus_municipis$avl_nom == names(avl_ctv)[i], ]
  names(davl) <- gsub("^municipi$", "avl_municipi", names(davl))
  names(davl) <- gsub("^tipus$", "avl_tipus", names(davl))

  message("\n", i, " / ", length(sel_mun), "\t", names(avl_ctv)[i])

  consulta <- try(opq(
    bbox = paste0(municipi$osm_type, "(id: ", municipi$osm_id, ")"),
    out = "tags center", osm_types = "nwr", timeout = 100
  ) |>
    add_osm_feature(key = "name", value = davl$toponim, match_case = FALSE))

  consulta_ca <- try(opq(
    bbox = paste0(municipi$osm_type, "(id: ", municipi$osm_id, ")"),
    out = "tags center", osm_types = "nwr", timeout = 100
  ) |>
    add_osm_feature(key = "name:ca", value = davl$toponim, match_case = FALSE))

  if (inherits(consulta, "overpass_query")) {
    dosm_name <- osmdata_data_frame(consulta)
  } else {
    warning("Consulta malformada per ", names(avl_ctv)[i])
    dosm_name <- data.frame()
  }

  if (inherits(consulta_ca, "overpass_query")) {
    dosm_ca <- osmdata_data_frame(consulta_ca)
  } else {
    warning("Consulta malformada per ", names(avl_ctv)[i])
    dosm_ca <- data.frame()
  }

  dosm <- unique(dbTools::rbind_addColumns(dosm_name, dosm_ca))

  if (nrow(dosm) == 0) {
    osm_CTVL[[names(avl_ctv)[i]]] <- cbind(dosm, davl[integer(),])
    next
  }

  dosm$`name:minuscula` <- tolower(dosm$name)
  dosm$`name:ca_minuscula` <- tolower(dosm$`name:ca`)
  davl$`name:minuscula` <- tolower(davl$toponim)

  dosm_name_avl <- merge(dosm, davl, by = "name:minuscula")

  # selecciona casos que es poden aprofitar amb name:ca com a clau
  dosm_ca <- dosm[which(dosm$`name:ca_minuscula` != dosm$`name:minuscula`), ]
  dosm_ca_avl <- merge(dosm_ca, davl, by.x = "name:ca_minuscula", by.y = "name:minuscula")

  dosm_avl <- unique(dbTools::rbind_addColumns(dosm_name_avl, dosm_ca_avl))
  dosm_avl$`name:minuscula` <- dosm_avl$`name:ca_minuscula` <- davl$`name:minuscula` <- NULL

  message(
    "casos: ", nrow(dosm), "\tcoincidents: ", nrow(dosm_avl), " (name: ",
    nrow(dosm_name_avl), ", name:ca: ", nrow(dosm_ca_avl), ")", appendLF = TRUE
  )

  # dosmGeo <- st_as_sf(
  #   dosm_avl,
  #   coords = c("osm_center_lon", "osm_center_lat"),
  #   crs = "+proj=longlat +datum=WGS84 +no_defs"
  # )

  sel_osm_names <- intersect(c("name", "name:ca", "alt_name", "alt_name:ca"), names(dosm_avl))
  sel_cols <- c(
    "toponim", sel_osm_names, "avl_tipus", "wikidata", "ref:avl", "osm_type", "osm_id",
    "osm_center_lon", "osm_center_lat", "avl_municipi", names(davl)
  )
  extra_cols <- sort(setdiff(names(dosm_avl), sel_cols))
  cols <- intersect(c(sel_cols, extra_cols), names(dosm_avl))
  rm_cols <- c("name:minuscula")
  cols <- setdiff(cols, rm_cols)

  osm_CTVL[[names(avl_ctv)[i]]] <- dosm_avl[, cols]
  setTimerProgressBar(pb, i)
}
close(pb)

sense_resultats <- names(osm_CTVL[sapply(osm_CTVL, nrow) == 0])
sense_resultats
sel_mun <- match(sense_resultats, names(avl_ctv))

sum(sapply(osm_CTVL, nrow))

# usethis::use_data(osm_CTVL, overwrite = TRUE)
load("data/osm_CTVL.rda", verbose = TRUE) # osm_CTVL

### Uneix en una taula i afegeix dades de municipi ----
osm_CTV_munL <- mapply(function(x, municipi) {
  mun <- tesaurus_municipis[tesaurus_municipis$avl_nom == municipi, ]
  names(mun) <- c("avl_municipi", "osm_municipi", "osm_comarca", "osm_typeMun", "osm_idMun")
  if (nrow(x) > 0) {
    out <- cbind(x, mun[, c("osm_municipi", "osm_comarca", "osm_typeMun", "osm_idMun")])
  } else {
    out <- cbind(x, mun[integer(), c("osm_municipi", "osm_comarca", "osm_typeMun", "osm_idMun")])
  }
}, x = osm_CTVL, municipi = names(osm_CTVL), SIMPLIFY = FALSE)
osm_CTV <- do.call(dbTools::rbind_addColumns, osm_CTV_munL)
row.names(osm_CTV) <- NULL


# Selecciona columnes
sel_osm_names <- intersect(c("name", "name:ca", "alt_name", "alt_name:ca"), names(osm_CTV))
sel_cols <- c(
  "toponim", sel_osm_names, "avl_tipus", "wikidata", "ref:avl", "osm_id", "osm_type",
  "osm_municipi", "osm_comarca", "osm_typeMun", "osm_idMun", "osm_center_lon", "osm_center_lat", "avl_municipi"
)
extra_cols <- setdiff(intersect(names(osm_CTV), names(avl_ctv)), sel_cols)
tags <- sort(setdiff(names(osm_CTV), c(sel_cols, extra_cols)))

n_tags <- sapply(osm_CTV[, tags], function(x) sum(!is.na(x)))
plot(sort(n_tags))
plot(sort(n_tags[n_tags > 100]))
tags_comuns <- names(which(n_tags > 100))
sel_tags <- setdiff(tags_comuns, c(grep("(^addr:|date$)", tags_comuns, value = TRUE)))

cols <- intersect(c(sel_cols, extra_cols, sel_tags), names(osm_CTV))
osm_CTV <- osm_CTV[, cols]

# usethis::use_data(osm_CTV, overwrite = TRUE)
load("data/osm_CTV.rda", verbose = TRUE) # osm_CTV

osm <- st_as_sf(osm_CTV,
                coords = c("osm_center_lon", "osm_center_lat"), remove = FALSE,
                crs = "+proj=longlat +datum=WGS84 +no_defs"
)
# mapview::mapview(osm)

head(osm_CTV, 15)


### Coincidències múltiples ----
sum(duplicated(osm_CTV$`ref:avl`))
sum(duplicated(osm_CTV[, c("osm_type", "osm_id")]))

dup_avl <- dbTools::duplicatedPK(osm_CTV, pk = "ref:avl")
dup_osm <- dbTools::duplicatedPK(osm_CTV, pk = c("osm_type", "osm_id"))

no_unics_pk_avl <- dbTools::nonUniqueValuesByPK(dup_avl, pk = "ref:avl")
no_unics_pk_osm <- dbTools::nonUniqueValuesByPK(dup_osm, pk = c("osm_type", "osm_id"))

sort(table(unlist(sapply(no_unics_pk_avl, names))))
sort(table(unlist(sapply(no_unics_pk_osm, names))))

## CONCLUSIONS: les coincidències múltiples amb els objectes d'OSM venen de topònims amb diferents coordenades
# provinents de diferents escales. Les coincidències múltiples amb els objectes del CTV venen de coincidències amb
# diferents elements d'OSM amb el mateix nom.
# Es poden resoldre les coincidències amb els valors de les etiquetes d'OSM i el camp tipus de l'AVL?
lapply(c("natural", "building", "place"), function(x) {
  out <- table(osm_CTV[, c("avl_tipus", x)])
  out <- out[, apply(out, 2, sum) > 0]
  out[apply(out, 1, sum) > 0, ]
})


noDup <- osm_CTV[!osm_CTV$`ref:avl` %in% dup_avl$`ref:avl`, ]


## No trobats: Cerca per noms aproximadament coincidents dins d'un municipi (value_exact = FALSE) ----
avl_pendents <- lapply(avl_ctv, function(x) x[!rownames(x) %in% osm_CTV$`ref:avl`, ])

osm_CTV_aproxL <- list()
sel_mun <- seq_along(avl_pendents)

pb <- timerProgressBar(max = length(sel_mun))
for (i in sel_mun) {
  davl <- avl_pendents[[i]]
  davl$`ref:avl` <- rownames(davl)
  municipi <- tesaurus_municipis[tesaurus_municipis$avl_nom == names(avl_pendents)[i], ]
  names(davl) <- gsub("^municipi$", "avl_municipi", names(davl))
  names(davl) <- gsub("^tipus$", "avl_tipus", names(davl))

  message("\n", i, " / ", length(sel_mun), "\t", names(avl_pendents)[i])

  consulta <- try(opq(
    bbox = paste0(municipi$osm_type, "(id: ", municipi$osm_id, ")"),
    out = "tags center", osm_types = "nwr", timeout = 100
  ) |>
    add_osm_feature(key = "name", value = davl$toponim, value_exact = FALSE, match_case = FALSE))

  consulta_ca <- try(opq(
    bbox = paste0(municipi$osm_type, "(id: ", municipi$osm_id, ")"),
    out = "tags center", osm_types = "nwr", timeout = 100
  ) |>
    add_osm_feature(key = "name:ca", value = davl$toponim, value_exact = FALSE, match_case = FALSE))

  if (inherits(consulta, "overpass_query")) {
    dosm_name <- osmdata_data_frame(consulta)
  } else {
    warning("Consulta malformada per ", names(avl_pendents)[i])
    dosm_name <- data.frame()
  }

  if (inherits(consulta_ca, "overpass_query")) {
    dosm_ca <- osmdata_data_frame(consulta_ca)
  } else {
    warning("Consulta malformada per ", names(avl_pendents)[i])
    dosm_ca <- data.frame()
  }

  dosm <- unique(dbTools::rbind_addColumns(dosm_name, dosm_ca))

  if (nrow(dosm) == 0) {
    osm_CTV_aproxL[[names(avl_ctv)[i]]] <- cbind(dosm, davl[integer(),])
    next
  }

  if ("name" %in% names(dosm)) {
    dosm$`name:minuscula` <- tolower(dosm$name)
  }
  if ("name:ca" %in% names(dosm)) {
    dosm$`name:ca_minuscula` <- tolower(dosm$`name:ca`)
  }

  davl$`name:minuscula` <- tolower(davl$toponim)
  # merge retorna 0 files pq no és exacte
  dosm_avlL <- lapply(unique(davl$`name:minuscula`), function(x) {
    coincident <- unique(c(grep(x, dosm$`name:minuscula`), grep(x, dosm$`name:ca_minuscula`)))
    if (length(coincident) == 0) {
      return(data.frame())
    } else {
      return(list(
        dosm = dosm[coincident, setdiff(names(dosm), c("name:minuscula", "name:ca_minuscula"))],
        davl = davl[davl$`name:minuscula` == x, setdiff(names(davl), "name:minuscula")]
      ))
    }
  })
  dosm_avlL <- dosm_avlL[sapply(dosm_avlL, length) > 0]
  names(dosm_avlL) <- sapply(dosm_avlL, function(x) unique(x$davl$toponim))

  # dosm_name_avl <- merge(dosm, davl, by = "name:minuscula")
  # dosm_avl <- unique(dbTools::rbind_addColumns(dosm_name_avl, dosm_ca_avl))

  message(
    "casos: ", length(dosm_avlL), "\tcoincidents: ", sum(sapply(dosm_avlL, function(x) nrow(x$dosm))), " (avl dupli: ",
    sum(sapply(dosm_avlL, function(x) nrow(x$davl) > 1)), ")", appendLF = TRUE
  )

  # dosmGeo <- st_as_sf(
  #   dosm_avl,
  #   coords = c("osm_center_lon", "osm_center_lat"),
  #   crs = "+proj=longlat +datum=WGS84 +no_defs"
  # )

  # sel_osm_names <- intersect(c("name", "name:ca", "alt_name", "alt_name:ca"), names(dosm_avl))
  # sel_cols <- c(
  #   "toponim", sel_osm_names, "wikidata", "ref:avl", "osm_type", "osm_id",
  #   "osm_center_lon", "osm_center_lat"
  # )
  # extra_cols <- sort(setdiff(names(dosm_avl), sel_cols))
  # cols <- intersect(c(sel_cols, extra_cols), names(dosm_avl))
  # rm_cols <- c("name:minuscula")
  # cols <- setdiff(cols, rm_cols)

  osm_CTV_aproxL[[names(avl_pendents)[i]]] <- dosm_avlL
  setTimerProgressBar(pb, i)
}
close(pb)

sense_resultats <- names(osm_CTV_aproxL[sapply(osm_CTVL, nrow) == 0])
sense_resultats
sel_mun <- match(sense_resultats, names(avl_pendents))

sum(sapply(osm_CTV_aproxL, length))

# usethis::use_data(osm_CTV_aproxL, overwrite = TRUE)
load("data/osm_CTV_aproxL.rda", verbose = TRUE) # osm_CTVL


# ### Uneix en una taula i afegeix dades de municipi ---
# osm_CTV_munL <- mapply(function(x, municipi) {
#   mun <- tesaurus_municipis[tesaurus_municipis$avl_nom == municipi, ]
#   names(mun) <- c("avl_municipi", "osm_municipi", "osm_comarca", "osm_typeMun", "osm_idMun")
#   if (nrow(x) > 0) {
#     out <- cbind(x, mun[, c("osm_municipi", "osm_comarca", "osm_typeMun", "osm_idMun")])
#   } else {
#     out <- cbind(x, mun[integer(), c("osm_municipi", "osm_comarca", "osm_typeMun", "osm_idMun")])
#   }
# }, x = osm_CTV_aproxL, municipi = names(osm_CTV_aproxL), SIMPLIFY = FALSE)
# osm_CTV_aprox <- do.call(dbTools::rbind_addColumns, osm_CTV_munL)
# row.names(osm_CTV_aprox) <- NULL
#
#
# # Selecciona columnes
# sel_osm_names <- intersect(c("name", "name:ca", "alt_name", "alt_name:ca"), names(osm_CTV_aprox))
# sel_cols <- c(
#   "toponim", sel_osm_names, "wikidata", "ref:avl", "osm_id", "osm_type", "avl_tipus",
#   "osm_municipi", "osm_comarca", "osm_typeMun", "osm_idMun", "osm_center_lon", "osm_center_lat"
# )
# extra_cols <- setdiff(intersect(names(osm_CTV_aprox), names(avl_ctv)), sel_cols)
# tags <- sort(setdiff(names(osm_CTV_aprox), c(sel_cols, extra_cols)))
#
# n_tags <- sapply(osm_CTV_aprox[, tags], function(x) sum(!is.na(x)))
# plot(sort(n_tags))
# plot(sort(n_tags[n_tags > 100]))
# tags_comuns <- names(which(n_tags > 100))
# sel_tags <- setdiff(tags_comuns, c(grep("(^addr:|date$)", tags_comuns, value = TRUE)))
#
# cols <- intersect(c(sel_cols, extra_cols, sel_tags), names(osm_CTV_aprox))
# osm_CTV_aprox <- osm_CTV_aprox[, cols]
#
# # usethis::use_data(osm_CTV_aprox, overwrite = TRUE)
# load("data/osm_CTV_aprox.rda", verbose = TRUE) # osm_CTV_aprox
#
# osm <- st_as_sf(osm_CTV_aprox,
#                 coords = c("osm_center_lon", "osm_center_lat"), remove = FALSE,
#                 crs = "+proj=longlat +datum=WGS84 +no_defs"
# )
# # mapview::mapview(osm)
#
# head(osm_CTV_aprox, 15)


# ### Coincidències múltiples ----
# sum(duplicated(osm_CTV_aprox$`ref:avl`))
# sum(duplicated(osm_CTV_aprox[, c("osm_type", "osm_id")]))
#
# dup_avl <- dbTools::duplicatedPK(osm_CTV_aprox, pk = "ref:avl")
# dup_osm <- dbTools::duplicatedPK(osm_CTV_aprox, pk = c("osm_type", "osm_id"))
#
# no_unics_pk_avl <- dbTools::nonUniqueValuesByPK(dup_avl, pk = "ref:avl")
# no_unics_pk_osm <- dbTools::nonUniqueValuesByPK(dup_osm, pk = c("osm_type", "osm_id"))
#
# sort(table(unlist(sapply(no_unics_pk_avl, names))))
# sort(table(unlist(sapply(no_unics_pk_osm, names))))
#
# ## CONCLUSIONS: les coincidències múltiples amb els objectes d'OSM venen de topònims amb diferents coordenades
# # provinents de diferents escales. Les coincidències múltiples amb els objectes del CTV venen de coincidències amb
# # diferents elements d'OSM amb el mateix nom.
# # Es poden resoldre les coincidències amb els valors de les etiquetes d'OSM i el camp Concepte de l'AVL?
# lapply(c("natural", "building", "place"), function(x) {
#   out <- table(osm_CTV_aprox[, c("avl_tipus", x)])
#   out <- out[, apply(out, 2, sum) > 0]
#   out[apply(out, 1, sum) > 0, ]
# })
#
# noDup <- osm_CTV_aprox[!osm_CTV_aprox$`ref:avl` %in% dup_avl$`ref:avl`, ]
#
# intersect(osm_CTV$`ref:avl`, osm_CTV_aprox$`ref:avl`)
# intersect(do.call(paste, osm_CTV[, c("osm_type", "osm_id")]), do.call(paste, osm_CTV_aprox[, c("osm_type", "osm_id")]))
# setdiff(do.call(paste, osm_CTV[, c("osm_type", "osm_id")]), do.call(paste, osm_CTV_aprox[, c("osm_type", "osm_id")]))
# setdiff(do.call(paste, osm_CTV_aprox[, c("osm_type", "osm_id")]), do.call(paste, osm_CTV[, c("osm_type", "osm_id")]))

## No trobats 2: Cerca per noms aproximadament coincidents independentment del municipi (value_exact = FALSE) ----
avl_pendents2 <- lapply(avl_ctv, function(x) x[!rownames(x) %in% osm_CTV_aprox$`ref:avl`, ])


# TODO: seleccionar topònims en valencià ----
osm_CTVL$`ALCÚDIA DE VEO`
osm_CTV_aproxL$`ALCÚDIA DE VEO`

## Conclusions ----
nrow(osm_CTV) # Elements d'OSM coincidents
length(unique(osm_CTV$`ref:avl`)) # Elements del CTV coincidents
sum(sapply(avl_pendents, nrow)) # Elements del CTV pendents
nrow(osm_CTV[is.na(osm_CTV$`name:ca`), ]) # Trobats que no tenen name:ca
