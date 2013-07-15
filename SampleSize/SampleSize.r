sample.size = function(c.lev, margin=.5, c.interval=.05, population) {
  z.val = qnorm(.5+c.lev/200)
  ss = (z.val^2 * margin * (1-margin))/c.interval^2
  p.ss = round((ss/(1 + ((ss-1)/population))), digits=0)
  METHOD = paste("Recommended sample size for a population of ", population,
  " at a ", c.lev, "% confidence level", sep = "")
  structure(list(Population = population, "Confidence level" = c.lev,
  "Margin of error" = c.interval, "Response distribution" = margin,
  "Recommended sample size" = p.ss, method = METHOD),
  class = "power.htest")
}

sample.size.table = function(margin=.5, c.interval=.05, population) {
  z.val=c(1.281551565545, 1.644853626951, 1.959963984540, 2.326347874041,
  2.575829303549, 2.807033768344, 3.090232306168, 3.290526731492,
  3.890591886413)
  ss = (z.val^2 * margin * (1-margin))/(c.interval^2)
  p.ss = ss/(1 + ((ss-1)/population))
  c.level = c("80%","90%","95%","98%","99%","99.5%","99.8%","99.9%","99.99%")
  results = data.frame(c.level, round(p.ss, digits = 0))
  names(results) = c("Confidence Level", "Sample Size")
  METHOD = c("Suggested sample sizes at different confidence levels")
  moe = paste((c.interval*100), "%", sep="")
  resp.dist = paste((margin*100),"%", sep="")
  pre = structure(list(Population=population, "Margin of error" = moe,
  "Response distribution" = resp.dist, method = METHOD),
  class = "power.htest")
  print(pre)
  print(results)
}