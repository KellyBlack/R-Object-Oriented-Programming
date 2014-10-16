

state.x77

states <- as.data.frame(state.x77[,c("Murder", "Population","Illiteracy", "Income", "Frost")])

cor(states)



# Cook's distance
fit <- lm(Murder ~ Population, data=states)
cutoff <- 4/(nrow(states)-length(fit$coefficients)-2)
plot(fit, which=4, cook.levels=cutoff)
abline(h=cutoff, lty=2, col="red")

fitted(fit)["Nevada"]
residuals(fit)["Nevada"]
rstudent(fit)["Nevada"]

hat.plot <- function(fit) {
p <- length(coefficients(fit))
n <- length(fitted(fit))
plot(hatvalues(fit), main="Index Plot of Hat Values")
abline(h=c(2,3)*p/n, col="red", lty=2)
identify(1:n, hatvalues(fit), names(hatvalues(fit)))
}
hat.plot(fit)

#> names(l)
# [1] "sevenDayEmployment" "businessOwned"      "caseID"            
# [4] "timeFirstChildWoke" "timeLastChildInBed" "theDate"           
# [7] "theDay"             "hourlyRate"         "profit"            
#[10] "stillWork"          "layedOff"           "bubba"             
#> 'bubba' %in% names(l)
#[1] TRUE
#> 'bubbaa' %in% names(l)
#[1] FALSE
#> 


#Downloaded: 25 June 2014
#http://www.bls.gov/tus/datafiles_2013.htm
#http://www.bls.gov/tus/special.requests/atusresp_2013.zip



