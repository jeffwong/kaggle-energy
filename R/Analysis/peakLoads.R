require(ggplot2)

energy = read.csv("Load_history_training.csv", header=T)

energy.subset.indices = which(energy$year == 2008 & energy$month >= 4 & energy$zone_id == 1)
energy.subset = energy[energy.subset.indices,]
energy.maxtimes = apply(energy.subset[,-(1:4)], 1, function(i) {
    which.max(i)
})

energy.maxtimes.df = data.frame(x=energy.maxtimes)
ggplot(energy.maxtimes.df, aes(x=x)) +
  geom_histogram(aes(y=..density..),binwidth=1,color="black",fill="white") +
  geom_density(alpha=.2,fill="#FF6666") +
  geom_vline(aes(xintercept=median(x, na.rm=T)),color="red", linetype="dashed", size=1) +
  ggtitle("Distribution of Peak Load Times April 08 - June 08") +
  scale_x_continuous("Hour of Day")
ggsave("PeakLoadTimes.png")
