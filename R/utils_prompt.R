prompt_next <- function() {
  x <- readline(prompt="Go next? ")
  grepl("y", x)
}
