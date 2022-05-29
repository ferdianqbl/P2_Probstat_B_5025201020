#--------1----------------
x_data <- c(78, 75, 67, 77, 70, 72, 78, 74, 77)
y_data <- c(100, 95, 70, 90, 90, 90, 89, 90, 100)
dif<- c()
for(i in 1:9)
  dif[i] <- y_data[i] - x_data[i]

#--A--
sd(dif)

# --B--
mean_dif = mean(dif)
mean_sample_data = mean(dif[1:6])
total_n = 6
total_sd = sd(dif[1:6])
t_val = (mean_sample_data - mean_dif) / (total_sd / sqrt(total_n))
p_val = pt(t_val, total_n-1, lower.tail = TRUE)
p_val

#--C--
install.packages("BSDA")
library(BSDA)
t.test(dif, alternative = 'two.sided', mu = mean_dif)


#--------------2-------------
#--A--
z_scores <- (100-23500)/sd(100)
z_scores