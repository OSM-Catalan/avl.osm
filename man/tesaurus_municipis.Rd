\name{tesaurus_municipis}
\alias{tesaurus_municipis}
\docType{data}
\title{
Tesaurus de municipis AVL - OSM
}
\description{
Dades amb les relacions entre els municipis de l'AVL i els d'OSM.
}
\usage{tesaurus_municipis}
\format{
  Un `data.frame` amb 395 files per les següents 4 columnes.
  \describe{
    \item{\code{avl_nom}}{Nom dels municipis segons el Corpus Toponímic Valencià.}
    \item{\code{comarca}}{Comarca del municipi.}
    \item{\code{osm_name}}{Nom de dels municipis a OSM.}
    \item{\code{osm_type}}{Tipus de l'element corresponent a OSM.}
    \item{\code{osm_id}}{Identificador de l'element corresponent a OSM.}
  }
}
\details{
Alguns municipis han canviat de nom des de l'edició del Corpus Toponímic Valencià.
Municipis no presents al Corpus Toponímic Valencià: `Llocnou d'en Fenollet`.
}
\references{
Dades generades a \code{data-raw/tesaurus_municipis.R}.
}
\examples{
str(tesaurus_municipis)
}
\seealso{
Altres dades:
\code{\link{avl_ctv}}
}
\concept{dades}
\keyword{datasets}
