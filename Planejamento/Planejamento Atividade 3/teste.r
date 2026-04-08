data(morley)
morley_wide <- reshape(morley,
                       timevar = "Run",
                       idvar = "Expt",
                       direction = "wide")

library(dplyr)
library(nortest)
library(nortsTest)

library(moments)

install.packages("nortest")

install.packages("nortsTest")

morley %>%
  group_by(Expt) %>%
  summarise(
    W = shapiro.test(Speed)$statistic,
    p_valor_sw = shapiro.test(Speed)$p.value,
    p_valor_ad = ad.test(Speed)$p.value,
    p_valor_ks = ks.test(Speed, "pnorm", mean(Speed), sd(Speed))$p.value,
    p_valor_cvm = chisq.test(Speed)$p.value
  )
