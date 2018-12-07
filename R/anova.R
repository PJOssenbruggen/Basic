#' \code{anova} uses the law of large numbers to establish confidence intervals traffic performance measures.
#'
#' @return The \code{anova} runs \code{brktrials4wrapper} \code{size} times.
#' @param zipper.output output from \code{zipper.simulate}, a data frame
#' @param sbs.output output from \code{sbs.simulate}, a data.frame
#' @usage anova(zipper.output, sbs.output)
#' @export
anova <- function(zipper.output, sbs.output) {
  zipper.w    <- zipper.output$w
  zipper.w    <- zipper.w[!is.na(zipper.w)]
  zipper.q    <- zipper.output$q.d
  zipper.q    <- zipper.q[!is.na(zipper.q)]
  sbs.w       <- sbs.output$w
  sbs.w       <- sbs.w[!is.na(sbs.w)]
  sbs.q       <- sbs.output$q.d
  sbs.q       <- sbs.q[!is.na(sbs.q)]
  sbs.w.df    <- data.frame(treatment = rep("side-by-side",length(sbs.w)), obs = sbs.w)
  zipper.w.df <- data.frame(treatment = rep("zipper",length(zipper.w)), obs = zipper.w)
  w.df        <- rbind(sbs.w.df, zipper.w.df)
  sbs.q.df    <- data.frame(treatment = rep("side-by-side",length(sbs.q)), obs = sbs.q)
  zipper.q.df <- data.frame(treatment = rep("zipper",length(zipper.q)), obs = zipper.q)
  q.df        <- rbind(sbs.q.df, zipper.q.df)
  par(mfrow = c(2,2), pty = "s")
  boxplot(obs ~ treatment, data = w.df, col = gray(0.8))
  title(main = "Delay")
  boxplot(obs ~ treatment, data = q.df, col = gray(0.8))
  title(main = "Flow")
  fmw        <- aov(obs ~ treatment, data = w.df)
#  options(contrasts = c("contr.sum", "contr.poly"))
#  fmw1       <- aov(obs ~ treatment, data = w.df)
  print(summary(fmw))
#  print(summary(fmw1))
  rw         <- resid(fmw)
  qqnorm(rw, ylab = "w residuals")
  qqline(rw)
  fmq <- aov(obs ~ treatment, data = q.df)
#  options(contrasts = c("contr.sum", "contr.poly"))
#  fmq1       <- aov(obs ~ treatment, data = q.df)
  print(summary(fmq))
#  print(summary(fmq1))
  rq         <- resid(fmq)
  qqnorm(rq, ylab = "q residuals")
  qqline(rq)
}
