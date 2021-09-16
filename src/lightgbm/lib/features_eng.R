# ------------------------------------------------------------------------------
# Import dependencies
# ------------------------------------------------------------------------------
library(pacman)
p_load(this.path)
# ------------------------------------------------------------------------------
#
#
#
#
#
# ------------------------------------------------------------------------------
# Functions
# ------------------------------------------------------------------------------
plot_rules <- function(rules, interactive = FALSE) {
  plot(
    rules, 
    measure = c("support", "lift"), 
    shading = "confidence", 
    jitter  = 0,
    engine  = if(interactive) 'interactive' else 'default'
  )
}

mostrar<-function(datos){
  print(5+datos)
  return(5+datos)
}

