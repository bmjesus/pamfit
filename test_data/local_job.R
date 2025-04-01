




light <- tryCatch({
  as.numeric(utils::read.csv(light_file$datapath, header = FALSE))
},error=function(e){NaN}
)



#pamfit_results
plot(pamfit_results$f_images$F_1)

plot(pamfit_results$model_outputs$alpha)

plot(light_pixel[11:14],topleft_etr[11:14])

abline(lm(topleft_etr[10:13]~light_pixel[10:13]))

summary(lm(topleft_etr[10:13]~light_pixel[10:13]))

(38/(topleft_etr[13]/topleft_etr[10]))
max(x)/()

y1<-topleft_etr[10:13]
x1<-   light_pixel[10:13]
summary(lm(y1~x1))

ps = max(ETR) + beta

