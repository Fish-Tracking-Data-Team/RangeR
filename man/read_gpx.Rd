\name{read_gpx}
\alias{read_gpx}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Read GPX files to Dataframes
%%  ~~function to do ... ~~
}
\description{
Reads .gpx file format and creates a dataframe for each layer type found in source file
}
\usage{
read_gpx(x, extract_extension = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{source file of .gpx  format}
  \item{extract_extension}{logic identifies if TrackPointExtension data is present and needs to be extracted. TrackPointExtension data can include variables such as water temperature and depth from echo sounders}

}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns individual dataframes for each layer type located in the source file (eg. waypoint, trackpoint)
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
function (x)
{
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
