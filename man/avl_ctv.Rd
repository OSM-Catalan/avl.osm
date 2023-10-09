\name{avl_ctv}
\alias{avl_ctv}
\docType{data}
\title{
Corpus Toponímic Valencià
}
\description{
Topònims dels 395 municipis de l'àrea de predomini valencianoparlant recollists per l'Acadèmia Valenciana de la Llengua.
}
\usage{avl_ctv}
\format{
  Llista de \code{data.frame}s pels 395 municipis. Cada \code{data.frame} conté 3 columnes:
  \describe{
    \item{\code{municipi}}{Nom dels municipis segons el Corpus Toponímic Valencià.}
    \item{\code{tipus}}{Tipus d'element (\verb{Altres llocs d'interés}, \verb{Hidrografia}, \verb{Orografia},
      \verb{Partides i paratges}, \verb{Poblament}, \verb{Relleu litoral i marí}, \verb{Vies de comunicació}).}
    \item{\code{toponim}}{Topònim de l'AVL.}
  }
}
%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
\source{
\url{https://www.avl.gva.es/?col_leccio=corpus_toponimic_valencia}
}
\references{
Dades extretes a \code{data-raw/avl_ctv.R}.
}
\examples{
str(do.call(rbind, avl_ctv))
}
\seealso{
Altres dades:
\code{\link{tesaurus_municipis}}
}
\concept{dades}
\keyword{datasets}
