\name{osm_CTVL}
\alias{osm_CTVL}
\docType{data}
\title{
Tesaurus dels topònims del CTV amb elements d'OSM per municipis
}
\description{
Inclou els elements d'OSM que es troben al mateix municipi que el topònim del CTV i que tenen el mateix nom (\code{name}
o \code{name:ca}) sense tenir en compte les majúscules i minúscules.
}
\usage{data("osm_CTVL")}
\format{
 Llista de \code{data.frame}s pels 395 municipis. Cada \code{data.frame} conté diverses columnes columnes:
  \describe{
    \item{\code{toponim}}{Topònim de l'AVL.}
    \item{\code{name}}{Etiqueta \code{name} de l'objecte a OSM.}
    \item{\samp{name:ca}}{Etiqueta \code{name:ca} de l'objecte a OSM.}
    \item{\code{alt_name}}{Etiqueta \code{alt_name} de l'objecte a OSM.}
    \item{\samp{alt_name:ca}}{Etiqueta \code{alt_name:ca} de l'objecte a OSM.}
    \item{\code{avl_tipus}}{Tipus d'element segons el CTV (\verb{Altres llocs d'interés}, \verb{Hidrografia},
      \verb{Orografia}, \verb{Partides i paratges}, \verb{Poblament}, \verb{Relleu litoral i marí},
      \verb{Vies de comunicació}).}
    \item{\code{wikidata}}{Etiqueta \code{wikidata} de l'objecte a OSM.}
    \item{\samp{ref:avl}}{Número de pàgina, columna i fila al pdf original.}
    \item{\code{osm_id}}{Identificador numèric de l'objecte a OSM.}
    \item{\code{osm_type}}{Tipus de l'objecte a OSM.}
    \item{\code{osm_municipi}}{Nom del municipi on es troba l'element a OSM (\code{name:ca}).}
    \item{\code{osm_comarca}}{Nom de la comarca on es troba l'element a OSM (\code{name:ca}).}
    \item{\code{osm_typeMun}}{Identificador numèric del municipi on es troba l'element a OSM.}
    \item{\code{osm_idMun}}{Tipus del municipi on es troba l'element a OSM.}
    \item{\code{osm_center_lon}}{Longitud del centre de la caixa de l'objecte a OSM.}
    \item{\code{osm_center_lat}}{Latitud del centre de la caixa de l'objecte a OSM.}
    \item{\code{avl_municipi}}{Nom dels municipis segons el Corpus Toponímic Valencià.}
    \item{\code{...}}{Etiquetes dels elements de OSM.}
  }
}
\note{
Tots els topònims són en valencià excepte els de l'\code{ALCÚDIA DE VEO}, que n'hi ha alguns en castellà.
}
\references{
Dades generades a \code{data-raw/tesaurus_OSM-CTV.R}.
}
\examples{
str(osm_CTVL)
}
\seealso{
Altres dades:
\code{\link{osm_CTV}}, \code{\link{avl_ctv}}, \code{\link{tesaurus_municipis}}
}
\concept{dades}
\keyword{datasets}
