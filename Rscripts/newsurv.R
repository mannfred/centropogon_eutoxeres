library(here)
library(tidyverse)
library(survival)

id <- c(rep(1, 3), rep(2, 3), rep(3, 2))
tstart <- c(0, 2, 6, 0, 2, 7, 0, 4)
tstop <- c(2, 6 , 9, 2, 7, 10, 4, 5)
event <- factor(c("A", "B", "cen", "A", "B", "cen", "B", "cen"))
istate <- factor(c(rep(c("A", "A", "B"), 2), "B", "B"))

mdata <- data.frame(id = id, tstart = tstart, tstop = tstop, event = event, istate = istate)
survcheck(Surv(tstart, tstop, event) ~1, mdata, id=id)

sfit2 <- survival::survfit(survival::Surv(tstart, tstop, event) ~ 1, data= mdata, id = id)
plot(sfit2, col=1:3, noplot=NULL)


# ---------------------------

id <- c(1,1,2,2)
tstart <- c(0, 8, 0, 7)
tstop <- c(8, 12, 7, 11)
event <- factor(c("A", "B", "A", "B"))
mdata <- data.frame(id = id, tstart = tstart, tstop = tstop, event = event)

survcheck(Surv(tstart, tstop, event) ~1, mdata, id=id)

sfit2 <- survfit(Surv(tstart, tstop, event) ~ 1, data= mdata, id = id)
