#' \code{factorialdesign2} uses the law of large numbers to establish confidence intervals traffic performance measures.
#'
#' @return The \code{factorialdesign2} runs \code{brktrials4wrapper} \code{size} times.
#' @param zipper.output2.5 output from \code{zipper.simulate}, a data frame
#' @param sbs.output2.5 output from \code{sbs.simulate}, a data.frame
#' @param zipper.output5 output from \code{zipper.simulate}, a data frame
#' @param sbs.output5 output from \code{sbs.simulate}, a data.frame
#' @usage factorialdesign2(zipper.output2.5, sbs.output2.5, zipper.output5, sbs.output5)
#' @export
factorialdesign2 <- function(zipper.output2.5, sbs.output2.5,zipper.output5, sbs.output5) {
  zipper.w2.5  <- zipper.output2.5$w
  zipper.w2.5  <- zipper.w2.5[!is.na(zipper.w2.5)]
  zipper.q2.5  <- zipper.output2.5$q.d
  zipper.q2.5  <- zipper.q2.5[!is.na(zipper.q2.5)]
  sbs.w2.5     <- sbs.output2.5$w
  sbs.w2.5     <- sbs.w2.5[!is.na(sbs.w2.5)]
  sbs.q2.5     <- sbs.output2.5$q.d
  sbs.q2.5     <- sbs.q2.5[!is.na(sbs.q2.5)]
  sbs.w.df2.5  <- data.frame(treatment = rep("S",length(sbs.w2.5)),
                             usd = rep("2.5",length(sbs.q2.5)), obs = sbs.w2.5)
  zipper.w.df2.5 <- data.frame(treatment = rep("Z",length(zipper.w2.5)),
                               usd = rep("2.5",length(zipper.q2.5)),   obs = zipper.w2.5)
  w.df2.5        <- rbind(sbs.w.df2.5, zipper.w.df2.5)
  sbs.q.df2.5    <- data.frame(treatment = rep("S",length(sbs.q2.5)),
                               usd = rep("2.5",length(sbs.q2.5)), obs = sbs.q2.5)
  zipper.q.df2.5 <- data.frame(treatment = rep("Z",length(zipper.q2.5)),
                               usd = rep("2.5",length(zipper.q2.5)), obs = zipper.q2.5)
  q.df2.5        <- rbind(sbs.q.df2.5, zipper.q.df2.5)
  zipper.w5  <- zipper.output5$w
  zipper.w5  <- zipper.w5[!is.na(zipper.w5)]
  zipper.q5  <- zipper.output5$q.d
  zipper.q5  <- zipper.q5[!is.na(zipper.q5)]
  sbs.w5     <- sbs.output5$w
  sbs.w5     <- sbs.w5[!is.na(sbs.w5)]
  sbs.q5     <- sbs.output5$q.d
  sbs.q5     <- sbs.q5[!is.na(sbs.q5)]
  sbs.w.df5  <- data.frame(treatment = rep("S",length(sbs.w5)),
                           usd = rep("5",length(sbs.w5)), obs = sbs.w5)
  zipper.w.df5 <- data.frame(treatment = rep("Z",length(zipper.w5)),
                             usd = rep("5",length(zipper.w5)), obs = zipper.w5)
  w.df5        <- rbind(sbs.w.df5, zipper.w.df5)
  sbs.q.df5    <- data.frame(treatment = rep("S",length(sbs.q5)),
                             usd = rep("5",length(sbs.q5)), obs = sbs.q5)
  zipper.q.df5 <- data.frame(treatment = rep("Z",length(zipper.q5)),
                             usd = rep("5",length(zipper.q5)), obs = zipper.q5)
  q.df5        <- rbind(sbs.q.df5, zipper.q.df5)
  q.df <- rbind(q.df2.5,q.df5)
  w.df <- rbind(w.df2.5,w.df5)
  par(mfrow = c(2,2), pty = "s")
  boxplot(obs ~ treatment + usd, data = w.df, col = gray(0.8))
  title(main = "Delay")
  boxplot(obs ~ treatment + usd, data = q.df, col = gray(0.8))
  title(main = "Flow")
  print(data.frame("DELAY"))
  fmw        <- aov(obs ~ treatment + usd, data = w.df)
  print(summary(fmw))
  print(summary.lm(fmw))
  print(alias(fmw))
  browser()

  fmw1       <- aov(obs ~ usd, data = w.df)
  print(summary(fmw1))
  print(summary.lm(fmw1))
  print(alias(fmw1))
  browser()


  rw         <- resid(fmw1)
  qqnorm(rw, ylab = "w residuals")
  qqline(rw)

  print(data.frame("FLOW"))
  fmq        <- aov(obs ~ treatment + usd + treatment:usd, data = q.df)
  print(summary(fmq))
  print(summary.lm(fmq))
  print(alias(fmq))
  browser()

  fmq1       <- aov(obs ~ treatment + usd, data = q.df)
  print(summary(fmq1))
  print(summary.lm(fmq1))
  print(alias(fmq1))
  browser()

  fmq2       <- aov(obs ~ usd, data = q.df)
  print(summary(fmq2))
  print(summary.lm(fmq2))
  print(alias(fmq2))
  browser()

  rq         <- resid(fmq1)
  qqnorm(rq, ylab = "q residuals")
  qqline(rq)
}
