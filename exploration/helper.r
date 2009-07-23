
# Helper functions ----------------------------------------------------------


savePlot <- function(..., plot= FALSE, big = TRUE)
{
nameOfPlot <- substitute(...)
nameOfPlot <- gsub("_", "-", nameOfPlot)

  dir.create("exports/", showWarnings = FALSE)

  cat("\nPrinting plot ", nameOfPlot,".pdf in folder 'exports'", sep ="")
  if(plot)
    if(big)
      ggsave(..., file=paste("exports/", nameOfPlot,"_BIG.pdf",sep = "", collapse = ""), width=20, height=15)    
    else
      ggsave(..., file=paste("exports/", nameOfPlot,".pdf",sep = "", collapse = ""), width=8, height=6)
  else
    cat("\nJust Kidding!!!\n")
  cat("\n")
}

index_with_time <- function(column, time) column / abs(column[time == min(time)]) * 100


deseas <- function(var, month) {
  # var - ave(var, month, mean, na.rm = TRUE) + mean(var, na.rm = TRUE)
  resid(rlm(var ~ factor(month))) + mean(var, na.rm = TRUE)
}
qdeseas <-  failwith(NA, deseas, quiet =T)

smoothhelp <- function(var, date)
  predict(gam(var ~ s(date)))

log_d <- function(var)
{
  var[var < 1 ] <- 1
#  var[var==0] <- 1
  log(var)
}

log_smooth <- function(var, date)
  smooth(log_d(var), date)

log_deseas <- function(var, date)
  deseas(log_d(var), date)
log_qdeseas <- failwith(NA, deseas, quiet = T)
qsmooth <- failwith(NA, smoothhelp, quiet = T)