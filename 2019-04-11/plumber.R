if (!require("pacman")) { install.packages("pacman") }
pacman::p_load_gh("pachamaltese/datos")
pacman::p_load(plumber)

#* @apiTitle Modelo lineal

#* modelo
#* @param x variable independiente
#* @param y variable dependiente
#* @get /modelo
function(x, y) {
  form <- as.formula(paste(y, x, sep = "~"))
  broom::tidy(lm(form, data = diamantes))
}

#* modelo2
#* @param x variable independiente
#* @param y variable dependiente
#* @param m modelo
#* @get /modelo2
function(x, y, m) {
  form <- switch(m,
                 "lin" = as.formula(paste(y, x, sep = "~")),
                 "log" = as.formula(sprintf("I(log(%s)) ~ I(log(%s))", y ,x))
  )
  
  broom::tidy(lm(form, data = diamantes))
}
