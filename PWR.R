library(pwr)
pwr.p.test(h = ES.h(p1 = 0.60, p2 = 0.50),
           sig.level = 0.05,
           power = 0.80,
           alternative = "greater")

p.out <- pwr.p.test(h = ES.h(p1 = 0.6, p2 = 0.50),
                    sig.level = 0.05,
                    power = 0.90,
                    alternative = "greater")
plot(p.out)

pwr.p.test(h = ES.h(p1 = 0.75, p2 = 0.50),
           sig.level = 0.01,
           n = 40,
           alternative = "greater")