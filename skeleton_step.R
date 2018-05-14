#' <The function header>
#'
#' `step_<name>` creates a *specification* of a recipe
#'  step that will <your text here>
#'
#' @param recipe A recipe object. The step will be added to the
#'  sequence of operations for this recipe.
#' @param ... One or more selector functions to choose which
#'  variables are affected by the step. See [selections()]
#'  for more details. For the `tidy` method, these are not
#'  currently used.
#' @param role Not used by this step since no new variables are
#'  created.
#' @param skip A logical. Should the step be skipped when the
#'  recipe is baked by [bake.recipe()]? While all operations are baked
#'  when [prep.recipe()] is run, some operations may not be able to be
#'  conducted on new data (e.g. processing the outcome variable(s)).
#'  Care should be taken when using `skip = TRUE` as it may affect
#'  the computations for subsequent operations
#' @param <optional extra arguments to your step>
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` (the
#'  selectors or variables selected) and `value` (the means).
#'
#' @keywords datagen
#' @concept preprocessing normalization_methods
#' @export
#' @details <explain what the new step does>
#'
#' @examples
#' <give at least one example of how your step works>
#' @seealso [recipe()] [prep.recipe()]
#'   [bake.recipe()]
step_<name> <-
  function(recipe,
           ...,
           role = NA,
           skip = FALSE,
           trained = FALSE,
           <extra arguments>) {
    add_step(
      recipe,
      step_<name>_new(
        terms   = ellipse_check(...),
        trained = trained,
        role    = role,
        <extra arguments>
      )
    )
  }

## Initializes a new object
step_<name>_new <-
  function(terms = NULL,
           role = NA,
           skip = FALSE,
           trained = FALSE,
           <extra arguments>) {
    step(
      subclass = "<name>",
      terms = terms,
      role = role,
      skip = skip,
      trained = trained,
      <extra_arguments>
    )
  }

prep.step_<name> <- function(x,
                             training,
                             info = NULL, # info is an argument to terms_select
                             ...) {
  col_names <- terms_select(x$terms, info = info)

  ## Here you derive the statistics from the train set that you would like
  ## to pass on to newdata. Such as the means you use for centering.

  step_<name>_new(
    terms = x$terms,
    role  = x$role,
    skip  = x$skip,
    trained = TRUE,
    <extra arguments>
  )
}

bake.step_<name> <- function(object,
                             newdata,
                             ...) {

  ## Here you do the preparing of the newdata.
  ## Possibly using the statistics you derived in the prep step.

  as_tibble(newdata)
}

print.step_<name> <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("<step message here>", sep = "")
    printer(<variables names from x>, x$terms, x$trained, width = width)
    invisible(x)
  }


#' @rdname step_center
#' @param x A `step_center` object.
tidy.step_<name> <- function(x, ...) {
  if (is_trained(x)) {
    ## tidying when your step is not yet trained
  } else {
    ##tidying when your step is trained
  }
  res
}
