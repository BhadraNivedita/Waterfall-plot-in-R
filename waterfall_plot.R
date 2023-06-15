##https://www.themillerlab.io/posts/waterfall_charts_in_r/

library(tidyverse)
library(dplyr)
library(knitr)

# We will first create a data set that specifies patients with locally advanced/unresectable MCC and metastatic disease.

# Waterfall plots are displayed in descending order from the worst tumor response from baseline value on the left to the best value on the right side of the plot
## Therefore, we will create data for 55 subjects and order them in decreasing order since a positive value here represents increase in tumor size from baseline and a negative value represents a decrease in size.

# We will randomly assign the two doses, 80 mg or 150 mg, to the 56 subjects
Merkel <- data.frame(
  id=c(1:56), 
  type = sample((rep(c("laMCC", "metMCC"), times =28))), 
  response = c(30, sort(runif(n=53,min=-10,max=19), decreasing=TRUE),-25,-31), 
  dose= sample(rep(c(80, 150), 28)))

# Let's assign Best Overall Response (BOR)
Merkel$BOR= (c("PD", rep(c("SD"), times =54),"PR"))

kable((Merkel))


MCC<- barplot(Merkel$response, 
              col="blue", 
              border="blue", 
              space=0.5, ylim=c(-50,50), 
              main = "Waterfall plot for Target Lesion Tumor Size", 
              ylab="Change from baseline (%)",
              cex.axis=1.5, cex.lab=1.5)



col <- ifelse(Merkel$dose == 80, 
              "steelblue", # if dose = 80 mg, then the color will be steel blue
              "cadetblue") # if dose != 80 mg (i.e. 150 mg here), then the color will be cadet blue

MCC<- barplot(Merkel$response, 
              col=col, 
              border=col, 
              space=0.5, 
              ylim=c(-50,50),
              main = "Waterfall plot for Target Lesion Tumor Size", 
              ylab="Change from baseline (%)",
              cex.axis=1.5, 
              legend.text= c( "80mg", "150mg"),
              args.legend=list(title="Treatment Dose", fill=c("steelblue", "cadetblue"), border=NA, cex=0.9))



col <- ifelse(Merkel$type == "laMCC", 
              "#BC5A42", # if type of disease = locally MCC, then the color will be #BC5A42 (deep red)
              "#009296") # if type of disease != locaally MCC (i.e. mMCC), then the color will be ##009296 (greenish-blue)

MCC<- barplot(Merkel$response, 
              col=col, 
              border=col, 
              space=0.5, 
              ylim=c(-50,50),
              main = "Waterfall plot for Target Lesion Tumor Size", 
              ylab="Change from baseline (%)",
              cex.axis=1.5, 
              legend.text= c( "locally advanced MCC", "Metastatic MCC"),
              args.legend=list(title="Disease", fill=c("#BC5A42", "#009296"), border=NA, cex=0.9))


col <- ifelse(Merkel$BOR == "CR", 
              "green", # if a subject had a CR the bar will be green, if they did not have a CR....
              ifelse(Merkel$BOR == "PR", 
                     "steelblue", # then, if a subject had a PR the bar will be steel blue, if they did not have a PR or CR....
                     ifelse(Merkel$BOR == "PD", 
                            "red", # then, if a subject had a PD the bar will be red, otherwise if they did not have a PR or CR or PD.... 
                            ifelse(Merkel$BOR == "SD", 
                                   "cadetblue", # then they must have ahd a SD, so the bar will be cadetblue, otherwise....
                                   "") # the color will be blank (which is not really an option, b/c they must have either a CR, PR, PD or SD)
                     )))


MCC<- barplot(Merkel$response, 
              col=col, 
              border=col, 
              space=0.5, 
              ylim=c(-50,50),
              main = "Waterfall plot for Target Lesion Tumor Size", ylab="Change from baseline (%)",
              cex.axis=1.5, 
              legend.text= c( "CR: Complete Response", "PR: Partial Response", "SD: Stable Disease", "PD: Progressive Disease"),
              args.legend=list(title="Best Overall Response", fill=c("green","steelblue",  "cadetblue", "red"), border=NA, cex=0.9))


col <- ifelse(Merkel$BOR == "CR", 
              "green",
              ifelse(Merkel$BOR == "PR", 
                     "steelblue",
                     ifelse(Merkel$BOR == "PD", 
                            "red", 
                            ifelse(Merkel$BOR == "SD", 
                                   "cadetblue", ""))))

MCC<- barplot(Merkel$response, 
              col=col, 
              border=col, 
              space=0.5, 
              ylim=c(-50,50),
              main = "Waterfall plot for Target Lesion Tumor Size",
              ylab="Change from baseline (%)",
              cex.axis=1.5, 
              legend.text= c( "CR: Complete Response", "PR: Partial Response", "SD: Stable Disease", "PD: Progressive Disease"),
              args.legend=list(title="Best Overall Response", fill=c("green","steelblue",  "cadetblue", "red"), border=NA, cex=1.0))

# Use the abline() function
## The abline() function is a simple way to add lines in R
### It takes the arguments: abline(a=NULL, b=NULL, h=NULL, v=NULL, ...)
#### a, b : single values denoting the intercept and the slope of the line
#### h : the y-value(s) for horizontal line(s)
#### v : the x-value(s) for vertical line(s)

abline(h=20, col = "black", lwd=0.5) # The "PD" line
abline(h=-30, col = "black", lwd=0.5) # This "PR" line
