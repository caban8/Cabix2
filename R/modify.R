# DODAĆ PÓŹNIEJ CUSTOM CONDITION


#' Modify selected columns of a date.frame using a function
#'
#' This function modifies selected columns of a data.frame by applying a function to them with a supplemented list of arguments.
#'
#' @param .x A data.frame to modify
#' @param .y A list specifying modifications for selected elements of .x
#' @param .fun A function to apply to selected elements of .x
#' @param ... Additional arguments to be passed to the function
#'
#' @return A modified data.frame
#'
#' @details This function iterates over names in .y, and for each name, it applies the function .fun to the corresponding element in .x and .y and stores the result in .x.
#'
#' @examples
#' # Define the list to modify
#' my_list <- list(a = 1:5, b = 6:10, c = 11:15)
#'
#' # Define modifications to apply
#' modifications <- list(a = 10, b = 20)
#'
#' # Modify the list applying the modifications using the `imodify2` function
#' imodify2(my_list, modifications, function(x, y) x + y)
#'
#' @export
imodify2 <- function(.x, .y, .fun, ...) {



  if (!is.list(.y)) {
    rlang::abort("The second argument must be a list")
  }


  for (name in names(.y)) {
    .x[[name]] <- .fun(.x[[name]], .y[[name]], ...)

  }

  return(.x)

}


