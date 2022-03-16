#' Parameter for hyperparameter optimization
#' @field partrans The transformation type.
#' @export
#' @examples
#' p1 <- par_hype$new()
#' class(p1)
#' print(p1)
# par_hype ----
par_hype <- R6::R6Class(
  classname="par_hype",
  public=list(
    partrans=""
  )
)
