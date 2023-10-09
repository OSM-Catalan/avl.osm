## https://www.avl.gva.es/?col_leccio=corpus_toponimic_valencia
fitxer <- "https://www.avl.gva.es/documents/31987/99450/CTV.pdf"
# fitxer <- "data-raw/CTV.pdf"


# pdftools ----
ctv <- pdftools::pdf_text(fitxer) # equivalent a `pdftotext -layout data-raw/CTV.pdf`

# Rang de pàgines del CORPUS TOPONÍMIC VALENCIÀ ORDENAT PER MUNICIPIS VOLUM II
grep("ZONA DE PREDOMINI\\nVALENCIANOPARLANT", ctv, value=TRUE)
inici <- grep("ZONA DE PREDOMINI\\nVALENCIANOPARLANT", ctv)[2] + 1
grep("ZONA DE PREDOMINI\\nCASTELLANOPARLANT", ctv, value=TRUE)
fi <- grep("ZONA DE PREDOMINI\\nCASTELLANOPARLANT", ctv)[2] - 1

contingut <- ctv[inici:fi]

files <- lapply(contingut, function(x) {
  # x <- gsub("(\\n)+[0-9]+", "", x) # treu números de pàgina (no afecta ncol i es pot usar de nom d'element)
  # x <- gsub("^\\s+[A-Z]\\n", "", x) # treu posició de l'abecedari
  # x <- gsub("[ ]{2,}", "\t", x) # afegeix \t com a separadors de columnes

  if (length(x) == 0 || x %in% c(NA, "", "\\n")) return(data.frame())

  read.table(text = x, header = FALSE, sep = "\t", row.names = NULL, fill = TRUE, encoding = "UTF-8")
})

ncols <- sapply(files, ncol)
table(ncols)
contingut[ncols == 0]
files <- files[ncols != 0]
contingut <- contingut[ncols != 0]
ncols <- sapply(files, ncol)
table(ncols)


## Números de pàgina ----

## Extreu
sapply(files, function(x) apply(x, 2, function(y) length(unique(y))))
pagines <- sapply(files, function(x) {
  # pag <- apply(x, 2, function(y) grep("^[0-9]+$", y, value = TRUE))
  # unlist(pag)
  pag <- gsub("\\s+", "", x[nrow(x), 1])
})
names(files) <- names(contingut) <- paste0("pag", pagines)

## Elimina
sapply(files, function(x) x[nrow(x), ])
## CONCLUSIÓ: elimina totes les útlimes files que (només) contenen números (ull amb "Barri de les 613 Vivendes")
files <- lapply(files, function(x) {
  if (any(grepl("[0-9]+", x[nrow(x), ]))) {
    x<- x[-nrow(x), , drop = FALSE]
  }
  return(x)
})


## Capçalera de pàgines i lletres ordre alfabètic ----

sapply(files, function(x) gsub("^\\s+", "", x[1,]))
sapply(files, function(x) x[1,])
sapply(files, function(x) x[2,])
sapply(files, function(x) grep("^\\s+[A-Z]$", x[2,], value = TRUE))
## CONCLUSIÖ: 1a fila conté o lletres de posició a l'alfabet o la capçalera amb el nom de municipi. Elimina
# 2a fila conté o dades vàlides o lletres de posició a l'alfabet. Elimina només les lletres de posició a l'alfabet

files <- lapply(files, function(x) {
  x <- x[-1, , drop = FALSE]
  if (grepl("^\\s+[A-Z]$", x[1, ])) { # 2a línia
    x <- x[-1, , drop = FALSE]
  }
  if (grepl("\\s+[A-Z]$", x[1, ])) {
    x[1, ] <- gsub("\\s+[A-Z]$", "", x[1, ])
  }
  x
})


## Altres lletres de posició a l'alfabet:
sapply(files, function(x) grep("\\s+[A-Z]$", x[, 1], value = TRUE))

files <- lapply(files, function(x) {
  x[, 1] <- gsub("\\s+[A-Z]$", "", x[, 1])
  x <- x[x[, 1] != "", , drop = FALSE]
  x
})


## Separa columnes per nchar. Diferent per cada pàgina ----
print(files, right = FALSE)

taula <- lapply(files, function(x) {
  inici_columnes <- gregexec("\\s*([A-Za-z]).+\\s{2,}([A-Za-z]).+\\s{2,}([A-Za-z])", x[1, ])
  if (inici_columnes[[1]][[1]] == -1) { # menys de 3 columnes
    inici_columnes <- gregexec("\\s*([A-Za-z]).+\\s{2,}([A-Za-z])", x[1, ])
    if (inici_columnes[[1]][[1]] == -1) { # menys de 2 columnes
      inici_columnes <- gregexec("\\s*([A-Za-z])", x[1, ])
      out <- data.frame(c1. = substr(x[, 1], inici_columnes[[1]][2, 1], nchar(x[, 1])))
    } else {
      out <- data.frame(list(
        c1. = substr(x[, 1], inici_columnes[[1]][2, 1], inici_columnes[[1]][3, 1] - 1),
        c2. = substr(x[, 1], inici_columnes[[1]][3, 1], nchar(x[, 1]))
      ))
    }
  } else {
    out <- data.frame(list(
      c1. = substr(x[, 1], inici_columnes[[1]][2, 1], inici_columnes[[1]][3, 1] - 1),
      c2. = substr(x[, 1], inici_columnes[[1]][3, 1], inici_columnes[[1]][4, 1] - 1),
      c3. = substr(x[, 1], inici_columnes[[1]][4, 1], nchar(x[, 1]))
    ))
  }

  return(out)
})

print(taula, right = FALSE)
print(taula[c("pag72", "pag308")], right=FALSE)


## Arregla topònims multilínia (comencen amb > 2 espais en blanc) i espais ----
taula <- lapply(taula, function(x) {
  out <- data.frame(lapply(x, function(y) {
    multi <- grep("^\\s+[A-Za-z]", y)

    ## Municipis multilínia no comencen amb espais però són 2 files seguides començant amb [A-Z]{2,}
    mun <- grep("^[A-ZÀÈÉÍÓÒÚ]{2,}", y)
    multi_mun <- mun[c(FALSE, diff(mun) == 1)]

    multi <- c(multi, multi_mun)
    if (length(multi)) {
      y[multi - 1] <- paste(gsub("\\s+$", "", y[multi - 1]), gsub("^\\s+|\\s+$", "", y[multi]))
      y[multi] <- ""
    }
    gsub("^\\s+|\\s+$", "", y)
  }))
})


## Uneix pàgines ----
toponims0 <- do.call(c, lapply(taula, function(x) do.call(c, x)))

## Zona de predomini castellanoparlant amb part part de domini valencianoparlant
alc_veo <- read.delim("data-raw/Alcúdia de Veo.txt", header = FALSE, encoding = "UTF-8")
rownames(alc_veo) <- do.call(c, sapply(split(alc_veo[, 1], alc_veo[, 1]), function(x) paste0(x, 1:length(x))))
toponims0 <- c(toponims0, structure(alc_veo[, 2], names = rownames(alc_veo)))

toponims0 <- toponims0[toponims0 != ""]
toponims0 <- gsub("’", "'", toponims0)
toponims0 <- gsub("ﬁ", "fi", toponims0)



## Municipis multilínia no corregits pq tenien línia en blanc entremig
mun <- grep("^[A-ZÀÈÉÍÓÒÚ]{2,}", toponims0)
multi_mun <- mun[c(FALSE, diff(mun) == 1)]
toponims0[sort(c(multi_mun, multi_mun - 1))]

toponims0[multi_mun - 1] <- paste(toponims0[multi_mun - 1], toponims0[multi_mun])
toponims0 <- toponims0[-multi_mun]


### Extreu municipis i tipus ----
grep("[A-ZÀÈÉÍÓÒÚ]{2,}", toponims0, value=TRUE)
municipis <- grep("^[A-ZÀÈÉÍÓÒÚ]{2,}", toponims0, value=TRUE)
n_municipis <- table(municipis)
u_municipis <- unique(municipis)

sort(table(toponims0), decreasing = TRUE)
tipus <- c(names(which(table(toponims0) > 200)), "Relleu litoral i marí")

avl_ctv <- list()
for (i in seq_along(u_municipis)) {
  m <- u_municipis[i]
  m_1 <- if (i + 1 > length(u_municipis)) NA else u_municipis[i + 1]
  if (n_municipis[m] == 1) {
    fi <- if (is.na(m_1)) length(toponims0) else match(m_1, toponims0) - 1
    topo_m <- toponims0[(match(m, toponims0) + 1):fi]
  } else {
    fi <- if (is.na(m_1)) length(toponims0) else match(m_1, toponims0) - 1
    inici <-  which(toponims0 %in% m)[2] + 1 # el primer és a la capçalera de la pàgina, fora de la llista
    topo_m <- toponims0[inici:fi]

  }

  tipus_i <- match(tipus, topo_m)
  names(tipus_i) <- tipus
  tipus_i <- sort(na.omit(tipus_i))
  tipus_v <- character(length(topo_m))
  for (j in seq_along(tipus_i)) {
    fi <- if (j + 1 > length(tipus_i)) length(topo_m) else tipus_i[j+1] - 1
    tipus_v[tipus_i[j]:fi] <- names(tipus_i)[j]
  }

  d <- data.frame(municipi = m, tipus = tipus_v, toponim=topo_m)
  d <- d[d$tipus != d$toponim, ]
  avl_ctv[[m]] <- d
}

avl_ctv <- avl_ctv[sort(names(avl_ctv))]

avl_ctva <- do.call(rbind, avl_ctv)
err <- which(apply(avl_ctva, 1, function(x) any(x == "")))

# openxlsx::write.xlsx(avl_ctv, file = "data-raw/CTV.xlsx")
# write.table(
#   do.call(rbind, avl_ctv), file = "data-raw/CTV.tsv",
#   quote = TRUE, sep = "\t", na = "", row.names = FALSE, col.names = TRUE, qmethod = "double"
# )

usethis::use_data(avl_ctv, overwrite = TRUE)

