subset_danceable<- subset(df, dance_f == "danceable")
subset_more_no<- subset(df, dance_f == "more no than yes")
subset_more_yes<- subset(df, dance_f == "more yes than no")


boxplot(subset_danceable$bpm)
boxplot(subset_more_no$bpm)
boxplot(subset_more_yes$bpm)
