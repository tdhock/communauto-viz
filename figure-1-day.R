## http://www.communauto.com/fr/combien.html

## I want to choose the lower price between the Forfait C and the
## tarif Longue distance.
two.prices <- function(x, limit, price.under.limit, price.over.limit){
  ifelse(x > limit,
         price.under.limit*limit + price.over.limit*(x-limit),
         price.under.limit*x)
}

rate.list <-
  list(longue.distance=function(df){
    df$CAD.kilometers <- two.prices(df$kilometers, 300, 0.17, 0.13)
    df$CAD.days <- two.prices(df$days, 1, 32.95, 29.95)
    df$CAD <- with(df, CAD.kilometers + CAD.days)
    df
  }, forfait.c=function(df){
    only.days <- df$hours %/% 24
    only.hours <- df$hours %% 24
    df$CAD.kilometers <- two.prices(df$kilometers, 50, 0.40, 0.30)
    ## Add 40¢/h or $4/day from Thursday to Sunday.
    CAD.per.day <- 23 + 4
    CAD.per.hour <- 2.3 + 0.4 ## TODO: ifelse thurs-sun ...
    df$CAD.hours <- CAD.per.day * only.days +
      ifelse(only.hours < 10, only.hours * CAD.per.hour, CAD.per.day)
    df$CAD <- with(df, CAD.kilometers + CAD.hours)
    df
  })

## Data from my April 2014 bill:
April2014 <-
  data.frame(only.days=c(1, 2),
             only.hours=c(13, 3),
             prix.temps=c(54, 62.10),
             kilometers=c(583, 334),
             prix.km=c(179.90, 105.20),
             total.du=c(233.90, 167.30))
April2014$hours <- with(April2014, only.days * 24 + only.hours)
computed <- rate.list$forfait.c(April2014)
with(computed, {
  stopifnot(all.equal(prix.km, CAD.kilometers))
  stopifnot(all.equal(CAD.hours, prix.temps))
  stopifnot(all.equal(CAD, total.du))
})
