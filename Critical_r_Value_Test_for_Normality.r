number_tests <- 10000 #Number of trials to average over
number_samples <- seq(5,75,5) #Variety of sample sizes stored in a vector
critical_r <- vector("numeric", length(number_samples)) #Vector to store the critical r value for each sample size
for(i in 1:length(number_samples))
{
  r_vals <- vector("numeric", number_tests)
  for(j in 1:number_tests)
  {
    qqp <- qqnorm(rnorm(number_samples[i]), plot.it = FALSE) #Generate a random sample of size number_samples[i] with rnorm; qqnorm does not plot QQ plot here, but returns a list containing two vectors (sample values and theoretical values)
    r_vals[j] <- cor(qqp$x,qqp$y) #Find the correlation coefficient between sample and theoretical values
  }
  sorted_r_vals <- sort(r_vals) #Sort the correlation coefficients (r) obtained from each trial in ascending order
  critical_r[i] <- sorted_r_vals[(number_tests*0.01) + 1] #Pick the correlation coefficient (r) just higher than the worst (lowest) 1% of values
}
critical_r #Vector of critical r values for each sample size
plot(number_samples, critical_r) 