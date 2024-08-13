\name{receiver_separations}
\alias{receiver_separations}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculate Reciver Separations
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
receiver_separations(dist_set)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dist_set}{
%%     ~~Describe \code{dist_set} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (dist_set)
{
    library(geosphere)
    dist_matrix <- distm(dist_set[, c("Long", "Lat")], fun = distHaversine)
    row_names <- dist_set$Station
    col_names <- dist_set$Station
    dimnames(dist_matrix) <- list(row_names, col_names)
    rx_combs <- expand.grid(receiver_id = row_names, neighbor_id = col_names)
    rx_combs$distance <- as.vector(dist_matrix)
    rx_combs \%>\% dplyr::mutate(receiver_id = as.character(receiver_id),
        neighbor_id = as.character(neighbor_id))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
