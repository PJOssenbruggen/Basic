#' \code{factorialdesign} .
#'
#' @return The \code{factorialdesign} runs \code{brktrials4wrapper} \code{size} times.
#' @param zip1 output from \code{zipper.simulate}, a data frame
#' @param sbs1 output from \code{sbs.simulate}, a data.frame
#' @param zip2 output from \code{zipper.simulate}, a data frame
#' @param sbs2 output from \code{sbs.simulate}, a data.frame
#' @param colnum column number of interest, a number
#' @param figtitle, plot title, character string
#' @usage factorialdesign(zip1, sbs1, zip2, sbs2, colnum, figtitle)
#' @export
factorialdesign <- function(zip1, sbs1, zip2, sbs2, colnum, figtitle) {
  zip1         <- as.matrix(zip1)
  zip1         <- zip1[,colnum]
  zip1         <- zip1[!is.na(zip1)]
  effect1      <- rep("1",length(zip1))
  df1          <- data.frame(effect = effect1, obs = zip1)
  zip2         <- as.matrix(zip2)
  zip2         <- zip2[,colnum]
  zip2         <- zip2[!is.na(zip2)]
  effect2      <- rep("2",length(zip2))
  df2          <- data.frame(effect = effect2, obs = zip2)
  df12         <- rbind(df1,df2)
  treat1       <- rep("zip", dim(df12)[1])
  df12         <- cbind(treat = treat1, df12)
  sbs1         <- as.matrix(sbs1)
  sbs1         <- sbs1[,colnum]
  sbs1         <- sbs1[!is.na(sbs1)]
  effect1      <- rep("1",length(sbs1))
  df3          <- data.frame(effect = effect1, obs = sbs1)
  sbs2         <- as.matrix(sbs2)
  sbs2         <- sbs2[,colnum]
  sbs2         <- sbs2[!is.na(sbs2)]
  effect2      <- rep("2",length(sbs2))
  df4          <- data.frame(effect = effect2, obs = sbs2)
  df34         <- rbind(df3,df4)
  treat2       <- rep("sbs", dim(df34)[1])
  df34         <- cbind(treat = treat2, df34)
  df           <- rbind(df12, df34)
  df
  boxplot(obs ~ treat + effect, data = df, col = gray(0.8))
  title(main = figtitle)
  browser()
  fm           <- aov(obs ~ treat + effect + treat:effect, data = df)
  print(summary(fm))
  print(summary.lm(fm))
  print(alias(fm))
  browser()
  fm           <- aov(obs ~ effect, data = df)
  print(summary(fm))
  print(summary.lm(fm))
  print(alias(fm))
  browser()
  print(xtable::xtable(summary(fm)))
  print(xtable::xtable(summary.lm(fm)))
  print(alias(fm))
  print(model.tables(fm, type = "means", se = TRUE))
  resid         <- resid(fm)
#  qqnorm(resid, ylab = "Residuals")
#  qqline(resid)
}
