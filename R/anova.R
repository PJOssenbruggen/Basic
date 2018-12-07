#' \code{anova} returns \code{ANOVA} results, boxplot and \code{Q-Q} plot.
#'
#' @return The \code{anova} runs \code{brktrials4wrapper} \code{size} times.
#' @param zip output from \code{zipper.simulate}, a data frame
#' @param sbs output from \code{sbs.simulate}, a data.frame
#' @param colnum column number of interest, a number
#' @param figtitle, plot title, character string
#' @usage anova(zip, sbs, colnum, figtitle)
#' @export
anova <- function(zip, sbs, colnum, figtitle) {
  zip         <- as.matrix(zip)
  zip         <- zip[,colnum]
  zip         <- zip[!is.na(zip)]
  sbs         <- as.matrix(sbs)
  sbs         <- sbs[,colnum]
  sbs         <- sbs[!is.na(sbs)]
  sbs.df      <- data.frame(treatment = rep("side-by-side",length(sbs)), obs = sbs)
  zip.df      <- data.frame(treatment = rep("zipper",length(zip)), obs = zip)
  df          <- rbind(sbs.df, zip.df)
  boxplot(obs ~ treatment, data = df, col = gray(0.8))
  title(main = figtitle)
  fm          <- aov(obs ~ treatment, data = df)
  print(summary(fm))
  print(summary.lm(fm))
  print(alias(fm))
  resids      <- resid(fm)
  qqnorm(resids, ylab = "Residuals")
  qqline(resids)
  return(list(fm,summary.lm(fm),print(alias(fm))))
}
