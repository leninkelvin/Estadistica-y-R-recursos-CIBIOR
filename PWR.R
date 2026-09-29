library(pwr)

# En este ejemplo se usara la hipótesis nula es que la moneda es "normal"
# Y cae 50% en cara y 50% cruz
# La hipótesis alternativa es que la columna prefiere una cara sobre la otra
# 25% más.

pwr.p.test(h = ES.h(p1 = 0.75, p2 = 0.50),  # hipótesis
           sig.level = 0.05,                # alfa
           power = 0.80,                    # poder de la muestra
           alternative = "greater")         # dirección del incremento

p.out <- pwr.p.test(h = ES.h(p1 = 0.75, p2 = 0.50),
                    sig.level = 0.05,
                    power = 0.90,
                    alternative = "greater")
plot(p.out)

pwr.p.test(h = ES.h(p1 = 0.75, p2 = 0.50),
           sig.level = 0.01,
           n = 40,
           alternative = "greater")
