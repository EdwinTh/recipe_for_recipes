#' <The function header>
#'
#' `check_<name>` creates a *specification* of a recipe
#'  check that will <your text here>
#'
#' @param recipe A recipe object. The check will be added to the
#'  sequence of operations for this recipe.
#' @param ... One or more selector functions to choose which
#'  variables are affected by the check. See [selections()]
#'  for more details. For the `tidy` method, these are not
#'  currently used.
#' @param role Not used by this check since no new variables are
#'  created.
#' @param skip A logical. Should the check be skipped when the
#'  recipe is baked by [bake.recipe()]? While all operations are baked
#'  when [prep.recipe()] is run, some operations may not be able to be
#'  conducted on new data (e.g. processing the outcome variable(s)).
#'  Care should be taken when using `skip = TRUE` as it may affect
#'  the computations for subsequent operations
#' @param <optional extra arguments to your check>
#' @return An updated version of `recipe` with the new check
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` (the
#'  selectors or variables selected) and `value` (the means).
#'
#' @keywords datagen
#' @concept preprocessing normalization_methods
#' @export
#' @details <explain what the new check does>
#'
#' @examples
#' <give at least one example of how your check works>
#' @seealso [recipe()] [prep.recipe()]
#'   [bake.recipe()]
check_<name> <-
  function(recipe,
           ...,
           role = NA,
           skip = FALSE,
           trained = FALSE,
           <extra arguments>) {
    add_check(
      recipe,
      check_<name>_new(
        terms   = ellipse_check(...),
        trained = trained,
        role    = role,
        <extra arguments>
      )
    )
  }

## Initializes a new object
check_<name>_new <-
  function(terms = NULL,
           role = NA,
           skip = FALSE,
           trained = FALSE,
           <extra arguments>) {
    check(
      subclass = "<name>",
      terms = terms,
      role = role,
      skip = skip,
      trained = trained,
      <extra_arguments>
    )
  }

prep.check_<name> <- function(x,
                             training,
                             info = NULL, # info is an argument to terms_select
                             ...) {
  col_names <- terms_select(x$terms, info = info)

  ## Here you derive the statistics from the train set that you would like
  ## to pass on to newdata. Such as the means you use for centering.

  check_<name>_new(
    terms = x$terms,
    role  = x$role,
    skip  = x$skip,
    trained = TRUE,
    <extra arguments>
  )
}

bake.check_<name> <- function(object,
                             newdata,
                             ...) {

  ## Here you do the preparing of the newdata.
  ## Possibly using the statistics you derived in the prep check.

  as_tibble(newdata)
}

print.check_<name> <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("<check message here>", sep = "")
    printer(<variables names from x>, x$terms, x$trained, width = width)
    invisible(x)
  }


#' @rdname check_center
#' @param x A `check_center` object.
tidy.check_<name> <- function(x, ...) {
  if (is_trained(x)) {
    ## tidying when your check is not yet trained
  } else {
    ##tidying when your check is trained
  }
  res
}
