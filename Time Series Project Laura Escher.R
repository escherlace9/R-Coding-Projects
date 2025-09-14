#Time Series Project Laura Escher

#setupdirectories
fredr_set_key("insert API key here") #here we load in my api key to access the data (not listed for privacy purposes)

#LoadData
kenyantea_price <-fredr(series_id = "PTEAUSDM", #loading in the data set for tea prices
                             observation_start= as.Date("1990-01-01"),
                             observation_end = as.Date("2025-06-01")) %>% #start and end dates
  select(date,series_id,value) %>%
  mutate(index_first_diff =value-lag(value), #creating a first differences
         index_second_diff =difference(value,differences= 2)) %>% #creating a second differences
  tsibble(index=date)

length(kenyantea_price$value) #checking to make sure that values and years are same length for plotting

plot(kenyantea_price$date, kenyantea_price$value, type = 'l', #it is hard to tell if stationary just from this graph. use 10 year increment blocks to assess.
     main="Graph of Kenyan Tea Prices",
     xlab="Year",
     ylab="Tea Price",
     col = 'blue') #type l means I want a line and not dots

acf(kenyantea_price$value,
    main="Autocorrelation Function Graph of Kenyan Tea Price") #autocorrelation function graph for levels.

ur.df(kenyantea_price$value, type = 'none') #Dickey Fuller Test for stationarity.

sd(kenyantea_price$index_first_diff, na.rm = T)/sd(kenyantea_price$value) #rule of thumb, the last rule for determining stationarity.

#Question 2 - Monte Carlo Simulation

#-------------random walk ----------------
num_obs <- 50

sd_y <- 1
sd_x <- 1
mu_y <- 0
mu_x <- 0

iter <- 10000 #how many iterations we want, here 10,000.
store_t_stat <- numeric()
for (i in 1:iter) {
  y <- rnorm(num_obs, mean = mu_y, sd = sd_y) #independent random walk
  x <- rnorm(num_obs, mean = mu_x, sd = sd_x) #independent random walk
  y <- stats::filter(y, filter = 1, method = 'recursive') #this is looking back to yt-1 periods ago
  x <- stats::filter(x, filter = 1, method = 'recursive') #same here
  reg <- lm(y~x-1) #lm means linear model, minus 1 is not including the y-intercept
  sreg <- summary(reg)
  store_t_stat[i] <- sreg$coefficients[3]
}
sreg$coefficients #checking the t-stat on the coefficient. for this simulation, t-stat = -7.146682

#plot(y)

#summary(reg)

hist(store_t_stat,
     ylim=c(0,0.45),
     probability = TRUE,
     main = "Histogram of T-Statistics (Random Walk)",
     xlab = "T-Statistic",
     col='lightblue') #histogram of t-statistics by itself. no overlay here.
curve(dt(x, df = 49), 
      col = "salmon", 
      lwd = 2, 
      add = TRUE,
      from = min(store_t_stat),
      to = max(store_t_stat)) #adding overlay

mean(store_t_stat < -1.96 | store_t_stat > 1.96) #this stores the proportion of t-stats that fall outside of the 5% significance range.

#qt(c(0.025, 0.975), df = 49) #0.05 significance level, with 0.025 on each tail

#----------------stationary series-----------------

iter <- 10000 #how many iterations we want, here 10,000.
store_t_stat_stationary <- numeric()
for (i in 1:iter) {
  y_stationary <- rnorm(num_obs, mean = mu_y, sd = sd_y)
  x_stationary <- rnorm(num_obs, mean = mu_x, sd = sd_x)
  reg_stat <- lm(y_stationary~x_stationary-1) #stat meaning stationary
  sreg_stat <- summary(reg_stat) #stat meaning stationary
  store_t_stat_stationary[i] <- sreg_stat$coefficients[3]
}
sreg_stat$coefficients #checking the t-stat on the coefficient, here it is 1.974951.

hist(store_t_stat_stationary,
     ylim=c(0,0.45),
     probability = TRUE,
     main = "Histogram of T-Statistics (Stationary)",
     xlab = "T-Statistic",
     col='mistyrose') #histogram of t-statistics by itself. no overlay here.
curve(dt(x, df = 49), 
      col = "seagreen", 
      lwd = 2,
      from =min(store_t_stat_stationary),
      to = max(store_t_stat_stationary),
      xlab = 'hello',
      add = TRUE) #adding overlay

mean(store_t_stat_stationary < -1.96 | store_t_stat_stationary > 1.96) #here, 0.0523 is the proportion of t-stats that fall outside of the 5% significance range.
