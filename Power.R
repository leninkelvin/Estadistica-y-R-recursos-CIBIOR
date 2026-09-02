# Load pwr package and set seed for reproducibility

library(pwr)

# Two-sample t-test: how many participants per group do we need?

# Effect size d = 0.5 (medium), alpha = 0.05, power = 0.80
result_two_sample <- pwr.t.test(
  d = 0.8,
  sig.level = 0.05,
  power = 0.90,
  type = "two.sample"
)
print(result_two_sample)
#> 
#>      Two-sample t test power calculation 
#> 
#>               n = 63.77
#>               d = 0.5
#>       sig.level = 0.05
#>           power = 0.8
#>     alternative = two.sided
#> 
#> NOTE: n is number in *each* group
n_two_sample <- ceiling(result_two_sample$n)
cat("You need", n_two_sample, "participants per group,", n_two_sample * 2, "total.\n")
#> You need 64 participants per group, 128 total.


#> # One-sample t-test (comparing to a known value)
n_one <- ceiling(pwr.t.test(d = 0.5, sig.level = 0.05, power = 0.80,
                            type = "one.sample")$n)

# Paired t-test (before/after in same participants)
n_paired <- ceiling(pwr.t.test(d = 0.5, sig.level = 0.05, power = 0.80,
                               type = "paired")$n)

cat("One-sample:", n_one, "| Paired:", n_paired, "| Two-sample:", n_two_sample, "per group\n")
#> One-sample: 34 | Paired: 34 | Two-sample: 64 per group


#> # Power curve: power vs. sample size for d = 0.5
ns <- seq(10, 150, by = 5)
powers <- sapply(ns, function(n) {
  pwr.t.test(n = n, d = 0.5, sig.level = 0.05, type = "two.sample")$power
})

plot(ns, powers, type = "l", lwd = 2, col = "#4B6FA5",
     xlab = "Sample size per group", ylab = "Power",
     main = "Power curve: two-sample t-test (d = 0.5)")
abline(h = 0.80, lty = 2, col = "red")
abline(v = 64, lty = 2, col = "darkgreen")
legend("bottomright", legend = c("Power", "80% threshold", "n = 64"),
       lty = c(1, 2, 2), col = c("#4B6FA5", "red", "darkgreen"))