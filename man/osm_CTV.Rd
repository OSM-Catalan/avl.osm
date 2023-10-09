\name{osm_CTV}
\alias{osm_CTV}
\docType{data}
\title{
Tesaurus dels topònims del CTV amb elements d'OSM
}
\description{
Inclou els elements d'OSM que es troben al mateix municipi que el topònim del CTV i que tenen el mateix nom (\code{name}
o \code{name:ca}) sense tenir en compte les majúscules i minúscules.
}
\usage{data("osm_CTV")}
\format{
  Un \code{data.frame} amb 14314 files amb les següents 55 variables.
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
    \item{\code{...}}{Etiquetes dels elements de OSM amb més de 100 casos entre tots els elements.}

%     \item{\code{amenity}}{a character vector}
%     \item{\code{boundary}}{a character vector}
%     \item{\code{bridge}}{a character vector}
%     \item{\code{building}}{a character vector}
%     \item{\samp{cycleway:both}}{a character vector}
%     \item{\code{ele}}{a character vector}
%     \item{\code{highway}}{a character vector}
%     \item{\code{historic}}{a character vector}
%     \item{\code{intermittent}}{a character vector}
%     \item{\code{landuse}}{a character vector}
%     \item{\code{lane_markings}}{a character vector}
%     \item{\code{lanes}}{a character vector}
%     \item{\code{layer}}{a character vector}
%     \item{\code{lit}}{a character vector}
%     \item{\code{man_made}}{a character vector}
%     \item{\code{maxspeed}}{a character vector}
%     \item{\samp{name:es}}{a character vector}
%     \item{\code{natural}}{a character vector}
%     \item{\code{oneway}}{a character vector}
%     \item{\code{place}}{a character vector}
%     \item{\code{population}}{a character vector}
%     \item{\code{public_transport}}{a character vector}
%     \item{\code{ref}}{a character vector}
%     \item{\samp{ref:ine}}{a character vector}
%     \item{\code{religion}}{a character vector}
%     \item{\code{sidewalk}}{a character vector}
%     \item{\code{source}}{a character vector}
%     \item{\samp{source:ele}}{a character vector}
%     \item{\samp{source:name}}{a character vector}
%     \item{\code{surface}}{a character vector}
%     \item{\code{tracktype}}{a character vector}
%     \item{\code{tunnel}}{a character vector}
%     \item{\code{type}}{a character vector}
%     \item{\code{waterway}}{a character vector}
%     \item{\samp{waterway_category:NDS}}{a character vector}
%     \item{\samp{waterway:type}}{a character vector}
%     \item{\code{width}}{a character vector}
%     \item{\code{wikipedia}}{a character vector}
  }
}
\note{
Tots els topònims són en valencià excepte els de l'\code{ALCÚDIA DE VEO}, que n'hi ha alguns en castellà.
}
\references{
Dades generades a \code{data-raw/tesaurus_OSM-CTV.R}.
}
\examples{
\dontrun{
mapa <- sf::st_as_sf(
  osm_CTV,
  coords = c("osm_center_lon", "osm_center_lat"), remove = FALSE,
  crs = "+proj=longlat +datum=WGS84 +no_defs"
)
mapa
}
}
\seealso{
Altres dades:
\code{\link{osm_CTVL}}, \code{\link{avl_ctv}}, \code{\link{tesaurus_municipis}}
}
\concept{dades}
\keyword{datasets}
