% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hype_par_hype.R
\name{R6_par_hype}
\alias{R6_par_hype}
\title{Parameter for hyperparameter optimization}
\description{
Parameter for hyperparameter optimization

Parameter for hyperparameter optimization
}
\examples{
p1 <- R6_par_hype$new()
class(p1)
print(p1)
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{partrans}}{The transformation type.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-par_hype-getseq}{\code{R6_par_hype$getseq()}}
\item \href{#method-par_hype-clone}{\code{R6_par_hype$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-par_hype-getseq"></a>}}
\if{latex}{\out{\hypertarget{method-par_hype-getseq}{}}}
\subsection{Method \code{getseq()}}{
Get a sequence, uniform on the transformed scale
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{R6_par_hype$getseq(n)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{n}}{Number of points. Ignored for discrete.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-par_hype-clone"></a>}}
\if{latex}{\out{\hypertarget{method-par_hype-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{R6_par_hype$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
