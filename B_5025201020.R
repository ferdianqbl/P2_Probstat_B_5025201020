#--------1----------------
x_data <- c(78, 75, 67, 77, 70, 72, 78, 74, 77)
y_data <- c(100, 95, 70, 90, 90, 90, 89, 90, 100)
dif<- c()
for(i in 1:9)
  dif[i] <- y_data[i] - x_data[i]

#--A--
sd(x_data)
sd(y_data)
sd(dif)

# --B--
mean_dif = mean(dif)
mean_sample_data = mean(dif[1:6])
total_n = 6
total_sd = sd(dif[1:6])
t_val = (mean_sample_data - mean_dif) / (total_sd / sqrt(total_n))
p_val = pt(t_val, total_n-1, lower.tail = TRUE)
p_val

t.test(x_data, y_data, alternative = "greater", var.equal = FALSE)

#--C--
var.test(x_data, y_data)
t.test(dif, alternative = 'two.sided', mu = mean_dif)
t.test(x_data, y_data, var.equal = TRUE)

#--------------2-------------
install.packages("BSDA")
library(BSDA)
#--A & C--
# jawaban di README

#--B--
t_valB = (23500 - 20000) / (3900 / sqrt(100))
p_valB = pt(t_valB, 99, lower.tail = TRUE)
p_valB
tsum.test(mean.x=23500, s.x=3900, n.x=100, mu=20000)


#--------------3--------------
mean_x = 3.64
sd_x = 1.67
n_x = 19
mean_y = 2.79
sd_y = 1.32
n_y = 27
df = n_x + n_y - 2

#--A--
# jawaban di README

#--B--
varianceSample = (((n_x-1)*(sd_x**2)) + ((n_y-1)*(sd_y**2))) / ((n_x-1) + (n_y-1))
t_xy = (mean_x - mean_y) / (sqrt(varianceSample*((1/n_x) + (1/n_y))))

t_xy

tsum.test(mean.x=mean_x, 
          s.x = sd_x, 
          n.x = n_x, 
          mean.y =mean_y, 
          s.y = sd_y, 
          n.y = n_y, 
          alternative = "less", 
          var.equal = TRUE)
tsum.test(mean.x=mean_x, 
          s.x = sd_x, 
          n.x = n_x, 
          mean.y =mean_y, 
          s.y = sd_y, 
          n.y = n_y, 
          alternative = "greater", 
          var.equal = TRUE)
tsum.test(mean.x=mean_x, 
          s.x = sd_x, 
          n.x = n_x, 
          mean.y =mean_y, 
          s.y = sd_y, 
          n.y = n_y, 
          alternative = "two.sided", 
          var.equal = TRUE)

#--C--
install.packages("mosaic")
library(mosaic)

plotDist(dist='t', df=2, col="red")

#--D, E, & F--
# jawaban di README



