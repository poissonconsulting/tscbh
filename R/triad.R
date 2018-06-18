#' Add Triad
#'
#' @param child A string of the child station.
#' @param parent1 A string of the first parent station.
#' @param parent2 A string of the second parent station.
#' @inheritParams tsdbr::ts_disconnect_db
#' @return A data frame of the imported triad.
#' @export
ts_add_triad <- function(child, parent1, parent2, conn = getOption("tsdbr.conn", NULL)) {
  check_string(child)
  check_string(parent1)
  check_string(parent2)
  
  triads <- data.frame(Child = child,
                       Parent1 = parent1,
                       Parent2 = parent2,
                       stringsAsFactors = FALSE)
  ts_add_triads(triads, conn)
}

#' Add Triads
#' 
#' @param triads A data frame of triads with columns Child, Parent1, Parent2.
#' @inheritParams tsdbr::ts_disconnect_db
#' @return The imported triad data.
#' @export
ts_add_triads <- function(triads, conn = getOption("tsdbr.conn", NULL)) {
  check_data(triads,
             values = list(Child  = "",
                           Parent1 = "",
                           Parent2 = ""
             ),
             nrow = TRUE)
  
  
  triads <- triads[c("Child", "Parent1", "Parent2")]
  
  add(triads, "Triad", conn)
}
