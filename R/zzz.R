.onAttach <- function(libname, pkgname) {
  msg <- paste0(
    "Dades sota d'OSM llic\u00E8ncia ODbL 1.0. (c) Col\u00B7laboradors d'OpenStreetMap ",
    "https://www.openstreetmap.org/copyright i AVL ",
    "https://www.avl.gva.es/aviso-legal/ amb cessi\u00F3 d'\u00FAs a OSM ",
    "(https://wiki.openstreetmap.org/wiki/File:AVL_autoritza.jpg)"
  )
  packageStartupMessage(msg)
}
