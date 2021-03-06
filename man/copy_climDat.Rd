% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/copy_climDat.R
\name{copy_climDat}
\alias{copy_climDat}
\title{The function is used to copy the climate files from the various, specified sources to PROJECT_PATH}
\usage{
copy_climDat(x, query.overwrite = TRUE)
}
\arguments{
\item{x}{The list where the project settings are stored. It has to contain elements \code{PROJECT_PATH},
\code{weather$file_name}, and \code{weather$paths_full}.
\itemize{
  \item{"PROJECT_PATH"}{character with path specifying the location of the simulation experiment}
  \item{"weather$file_name"}{Stuff}
  \item{"weather$paths_full"}{Stuff}
}}

\item{query.overwrite}{query if existing files in PROJECT_PATH should be overwritten. Default to \code{TRUE}.}
}
\value{
Vector of characters specifying the file.path names path names of the location of the climate files where copied, too.
The location of the file in the PROJECT_PATH
}
\description{
The function is used to copy the climate files from the various, specified sources to PROJECT_PATH
}
\details{
Used in the setup of the simulation experiment.
}
\examples{
\dontrun{
# No sensible example, system specific
}
}
\author{
Tobias KD Weber , \email{tobias.weber@uni-hohenheim.de}
}
