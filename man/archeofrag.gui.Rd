\name{archeofrag.gui}
\alias{archeofrag.gui}

\title{archeofrag.gui}
\description{Launch the archeofrag.gui application}

\usage{
archeofrag.gui(n.cores=NULL)
}

\arguments{
  \item{n.cores}{integer. The number of cores to use for parallel execution. If NULL, half of the available cores are registered.}
}

\details{

This function launches the `archeofrag.gui` application.

Note that the  optional determination of graph planarity in  the \code{\link[archeofrag]{archeofrag}} package requires the  \code{RBGL} package.
}
\value{
    Launch the `archeofrag.gui` R Shiny application.
}
\author{Sebastien Plutniak <sebastien.plutniak at posteo.net>}

\seealso{
  \code{\link[archeofrag]{archeofrag}}
	\code{\link[archeofrag]{frag.simul.process}}
	\code{\link[archeofrag]{frag.get.parameters}}
	\code{\link[doParallel]{registerDoParallel}}
}

\examples{
    # launching the app:
    if(interactive()){
     archeofrag.gui()
    }
}
