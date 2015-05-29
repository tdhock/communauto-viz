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

## Data from my April 2015 bill:
April2015 <-
  data.frame(only.days=c(1, 2),
             only.hours=c(13, 3),
             prix.temps=c(54, 62.10),
             kilometers=c(583, 334),
             prix.km=c(179.90, 105.20),
             total.du=c(233.90, 167.30))
April2015$hours <- with(April2015, only.days * 24 + only.hours)
computed <- rate.list$forfait.c(April2015)
with(computed, {
  stopifnot(all.equal(prix.km, CAD.kilometers))
  stopifnot(all.equal(CAD.hours, prix.temps))
  stopifnot(all.equal(CAD, total.du))
})

## Situation: 10-24 hour rental + variable number of kilometers.
equality <- (32.95-27)/(0.4-0.17)
dot.km <- c(300, 50, equality)
dots <-
  data.frame(rate=c("longue.distance", "forfait.c", "equality"),
             kilometers=dot.km,
             days=1, hours=24)
dot.row.list <- list()
for(dot.i in 1:nrow(dots)){
  dot.row <- dots[dot.i, ]
  rate <- if(dot.row$rate == "equality"){
    "forfait.c"
  }else{
    paste(dot.row$rate)
  }
  rate.fun <- rate.list[[rate]]
  result <- rate.fun(dot.row)
  dot.row.list[[dot.i]] <- result[, c("rate", "kilometers", "CAD")]
}
dot.rows <- do.call(rbind, dot.row.list)

one.day <- data.frame(kilometers=0:500, hours=24, days=1)
CAD.list <- list()
for(rate in names(rate.list)){
  rate.fun <- rate.list[[rate]]
  result <- rate.fun(one.day)
  print(head(result))
  CAD.list[[rate]] <-
    data.frame(rate, result[, c("kilometers", "CAD")])
}
CAD <- do.call(rbind, CAD.list)
label.txt <- subset(CAD, kilometers==0)
label.txt$price.text <-
  c("32.95$/first day\n29.95$/additional day\n149.75$/week",
    "27$/day")
label.txt$label <- with(label.txt, sprintf("%s\n%s", rate, price.text))
rownames(label.txt) <- label.txt$rate
CAD$label <- label.txt[paste(CAD$rate), "label"]
dot.rows$label <- label.txt[paste(dot.rows$rate), "label"]
dot.rows$label <- with(dot.rows, ifelse(is.na(label), paste(rate), label))
label.colors <-
  c("#1B9E77",
    "#D95F02",
    equality="black")
names(label.colors)[1:2] <- label.txt$label
vlines <- dot.rows[1:2,]

rate.labels <-
  data.frame(kilometers=c(50, 50, 300, 300),
             CAD=c(90, 90, 50, 50),
             hjust=c(1, 0, 1, 0),
             rate=c("forfait.c", "forfait.c",
               "longue.distance", "longue.distance"),
             text=c("0.40$/km ", " 0.30$/km",
               "0.17$/km ", " 0.13$/km"))
rate.labels$label <- label.txt[paste(rate.labels$rate), "label"]

library(directlabels)
with.legend <-
  ggplot()+
    ggtitle("Communauto Thurs-Sun rates, 10-24 hours")+
    scale_y_continuous("Canadian dollars",
                       minor_breaks=NULL,
                       breaks=c(0, 100,32.95, 27, dot.rows$CAD),
                       labels=scales::dollar)+
    theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))+
    scale_x_continuous(breaks=c(0, dot.rows$kilometers),
                       minor_breaks=NULL,
                       labels=function(x){
                         ifelse(round(x)==x,paste(as.integer(x)),
                                sprintf("%.1f", x))
                       })+
    coord_cartesian(xlim=c(-200, 400), ylim=c(0, 100))+
    geom_line(aes(kilometers, CAD, color=label),
              size=1,
              data=CAD)+
    geom_text(aes(kilometers, CAD, color=label,
                  label=text,
                  hjust=hjust),
              data=rate.labels)+
    geom_vline(aes(xintercept=kilometers, color=label),
               alpha=0.5,
               data=vlines)+
    geom_point(aes(kilometers, CAD, color=label),
               data=dot.rows,
               pch=1)+
    scale_color_manual(values=label.colors)
with.labels <-
  direct.label(with.legend, "first.polygons")+
    guides(color="none")

pdf("figure-1-day.pdf")
print(with.labels)
dev.off()
