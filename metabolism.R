library(dplyr)
library(tidyr)
library(ggplot2)

# simulate some fake data
d <- expand.grid(site=letters[1:4],
                 temp=c(-5, 1, 10),
                 ind=1:10)
d$mass <- rnorm(nrow(d), 100, 5)
d$co2 <- rnorm(nrow(d), 10, 1) + (d$temp + 5) ^ 2 +
      as.integer(factor(d$site)) * 10

# summarize by site*temp
d <- group_by(d, site, temp) %>%
      summarize(mass=mean(mass),
                co2=mean(co2))

# functions to calculate q10, and predict CO2
get_q10 <- function(t1, t2, r1, r2) (r2/r1) ^ (10/(t2-t1))
get_r2 <- function(t1, t2, r1, q10) q10 ^ ((t2-t1)/10) * r1

ggplot(d, aes(temp, co2/mass, color=site)) +
      geom_point() +
      geom_line()

# calculate q10 for each site
q <- filter(d, temp %in% c(1, 10)) %>%
      mutate(co2_mass = co2/mass,
             temp=paste0("r", temp)) %>%
      select(-co2, -mass) %>%
      spread(temp, co2_mass) %>%
      mutate(temp1=1,
             temp2=10,
             q10 = get_q10(temp1, temp2, r1, r10))

# simulate some temperature time series
climate <- expand.grid(site=letters[1:4],
                       time=1:1000) 
climate$temp <- rnorm(nrow(climate), 5)

# predict r2 for every timestep
climate <- left_join(climate, q) %>%
      mutate(r2 = get_r2(temp1, temp, r1, q10))

# sum r2 for each site
r2 <- group_by(climate, site) %>%
      summarize(r2=sum(r2))

