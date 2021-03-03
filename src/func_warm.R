# Peturb monthly temperature for TABLE 12
warm <- function(monthlyTemp, warmingType, T_change) {
  
  if (warmingType == "Mono") {
    warmingFactor = c(1, 1)
  } else {
    warmingFactor = c(.76, 1.76)
  }
  
  winterTemp <-
    monthlyTemp[which(monthlyTemp$MONTH %in% c(12, 1, 2)), c("YEAR", "MONTH", "TAVG")]
  winterTemp$TAVG <- winterTemp$TAVG + T_change * (warmingFactor[1])
  
  springTemp <-
    monthlyTemp[which(monthlyTemp$MONTH %in% c(3, 4, 5)), c("YEAR", "MONTH", "TAVG")]
  springTemp$TAVG <- springTemp$TAVG + T_change * (warmingFactor[2])
  
  monthlyTemp <- rbind(winterTemp, springTemp)
  
  return(monthlyTemp)
}