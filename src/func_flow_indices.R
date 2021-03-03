# 11 OBSERVED STATION FLOW AGGREGATES
flowAggregator <- function(stations) {
  Apr_Jul <- group_by(stations, Year) %>%
    filter(Month %in% 4:7) %>%
    summarise_all(funs(sum))

  Oct_Mar <- group_by(stations, Year) %>%
    filter(Month %in% c(10, 11, 12, 1, 2, 3)) %>%
    summarise_all(funs(sum))

  Oct_Sep <- group_by(stations, Year) %>%
    summarise_all(funs(sum))

  Jan_May <- group_by(stations, Year) %>%
    filter(Month %in% c(1, 2, 3, 4, 5)) %>%
    summarise_all(funs(sum))

  Apr_May <- group_by(stations, Year) %>%
    filter(Month %in% c(4, 5)) %>%
    summarise_all(funs(sum))

  Apr_Sep <- group_by(stations, Year) %>%
    filter(Month %in% 4:9) %>%
    summarise_all(funs(sum))

  #SacFlows
  Apr_Jul_Sac <- Apr_Jul$BND + Apr_Jul$FTO + Apr_Jul$YRS + Apr_Jul$AMF
  Oct_Mar_Sac <- Oct_Mar$BND + Oct_Mar$FTO + Oct_Mar$YRS + Oct_Mar$AMF
  Oct_Sep_Sac <- Oct_Sep$BND + Oct_Sep$FTO + Oct_Sep$YRS + Oct_Sep$AMF

  #SJFlows
  Apr_Jul_SJ <- Apr_Jul$SNS + Apr_Jul$TLG + Apr_Jul$MRC + Apr_Jul$SJF
  Oct_Mar_SJ <- Oct_Mar$SNS + Oct_Mar$TLG + Oct_Mar$MRC + Oct_Mar$SJF
  Oct_Sep_SJ <- Oct_Sep$SNS + Oct_Sep$TLG + Oct_Sep$MRC + Oct_Sep$SJF

  #Eight Station
  Jan_May_8Sta <- Jan_May$BND + Jan_May$FTO + Jan_May$YRS + Jan_May$AMF + Jan_May$SNS + Jan_May$TLG + Jan_May$MRC + Jan_May$SJF
  Apr_May_8Sta <- Apr_May$BND + Apr_May$FTO + Apr_May$YRS + Apr_May$AMF + Apr_May$SNS + Apr_May$TLG + Apr_May$MRC + Apr_May$SJF

  #Shasta
  Oct_Sep_Sha <- Oct_Sep$SIS

  #Feather River
  Apr_Jul_Fea <- Apr_Jul$FTO
  Oct_Sep_Fea <- Oct_Sep$FTO

  #American River
  Apr_Sep_Am <- Apr_Sep$AMF

  #Trinity River
  Oct_Sep_Tr <- Oct_Sep$TNL

  return(list("Apr_Jul" = Apr_Jul, "Oct_Mar" = Oct_Mar,
    "Oct_Sep" = Oct_Sep, "Jan_May" = Jan_May,
    "Apr_May" = Apr_May, "Apr_Sep" = Apr_Sep,
    "Apr_Jul_Sac" = Apr_Jul_Sac, "Oct_Mar_Sac" = Oct_Mar_Sac, "Oct_Sep_Sac" = Oct_Sep_Sac,
    "Apr_Jul_SJ" = Apr_Jul_SJ, "Oct_Mar_SJ" = Oct_Mar_SJ, "Oct_Sep_SJ" = Oct_Sep_SJ,
    "Jan_May_8Sta" = Jan_May_8Sta, "Apr_May_8Sta" = Apr_May_8Sta,
    "Oct_Sep_Sha" = Oct_Sep_Sha, "Apr_Jul_Fea" = Apr_Jul_Fea, "Oct_Sep_Fea" = Oct_Sep_Fea,
    "Apr_Sep_Am" = Apr_Sep_Am, "Oct_Sep_Tr" = Oct_Sep_Tr
    ))
}

# SACRAMENTO 4 RIVER 40-30-30 INDEX
sacIndex <- function(sacAprJul,sacOctMar,modelYears,
                     aji,omi,c,d,bn,an,w) {
  SacIndex <- array(NA, modelYears)
  SacIndex[1] <- aji * sacAprJul[1] + omi * sacOctMar[1] + 0.3 * 6.5
  SacWYT <- array(NA, modelYears)
  SacWYT[1] <- if (SacIndex[1] < c) { as.integer(5)
  } else { if (SacIndex[1] < d) as.integer(4)
    else { if (SacIndex[1] < bn) as.integer(3)
    else { if (SacIndex[1] < an) as.integer(2)
    else as.integer(1)
    }}}
  for (i in 2:modelYears) {
    SacIndex[i] <- if (SacIndex[i- 1] > 10) {
        aji * sacAprJul[i] + omi * sacOctMar[i] + 0.3 * 10
      } else {
        aji * sacAprJul[i] + omi * sacOctMar[i] + 0.3 * SacIndex[i - 1]
      }
    SacWYT[i] <- if (SacIndex[i] < c) { as.integer(5)
    } else { if (SacIndex[i] < d) as.integer(4)
      else { if (SacIndex[i] < bn) as.integer(3)
      else { if (SacIndex[i] < an) as.integer(2)
      else as.integer(1)
      }}}
  }
  return(list("SacIndex" = SacIndex, "SacWYT" = SacWYT))
}

#SAN JOAQUIN 4 RIVER 60-20-20 INDEX
sjIndex <- function (sjAprJul,sjOctMar, modelYears,
                     aji,omi,c,d,bn,an,w) {
  SJIndex <- array(NA, modelYears)
  SJIndex[1] <- aji * sjAprJul[1] + omi * sjOctMar[1] + 0.2 * 2.5
  SJWYT <- array(NA, modelYears)
  SJWYT[1] <- if (SJIndex[1] < c) { as.integer(5)
  } else { if (SJIndex[1] < d) as.integer(4)
    else { if (SJIndex[1] < bn) as.integer(3)
    else { if (SJIndex[1] < an) as.integer(2)
    else as.integer(1)
    }}}
  for (i in 2:modelYears) {
    SJIndex[i] <- if (SJIndex[i - 1] > 4.5) {
        aji * sjAprJul[i] + omi * sjOctMar[i] + 0.2 * 4.5
      } else {
        aji * sjAprJul[i] + omi * sjOctMar[i] + 0.2 * SJIndex[i- 1]
      }
    SJWYT[i] <- if (SJIndex[i] < c) { as.integer(5)
    } else { if (SJIndex[i] < d) as.integer(4)
      else { if (SJIndex[i] < bn) as.integer(3)
      else { if (SJIndex[i] < an) as.integer(2)
      else as.integer(1)
      }}}
  }
  return(list("SJIndex" = SJIndex, "SJWYT" = SJWYT))
}

# SHASTA RIVER INDEX
shastaIndex <- function(shastaOctSep, modelYears) {

  cond1IndSha <- array(NA, modelYears)
  cond1IndSha[1] <- if (shastaOctSep[1] < 3.2) 1 else 2

  cond2Sha <- array(NA, modelYears)
  cond2Sha[1] <- 0

  cond2IndSha <- array(NA, modelYears)
  cond2IndSha[1] <- if (cond2Sha[1] > 0.8) 1 else 2

  ShaIndex <- array(NA, modelYears)
  ShaIndex[1] <- if ((cond1IndSha[1] + cond2IndSha[1]) < 4) 1 else 2

  for (i in 2:modelYears) {
    cond1IndSha[i] <- if (shastaOctSep[i] < 3.2) 1 else 2
    cond2Sha[i] <- if (shastaOctSep[i] < 4 & shastaOctSep[i - 1] < 4) {
        8 - shastaOctSep[i] - shastaOctSep[i - 1]
      } else 0
    cond2IndSha[i] <- if (cond2Sha[i] > 0.8) 1 else 2
    ShaIndex[i] <- if ((cond1IndSha[i] + cond2IndSha[i]) < 4) 1 else 2
  }

  return(ShaIndex)
}


#Feather River Index
featherIndex <- function(feaAprJul,feaOctSep,modelYears) {
  cond1IndFea <- array(NA, modelYears)
  cond1IndFea[1] <- if (feaAprJul[1] < 0.6) 1 else 0

  cond2Fea <- array(NA, modelYears)
  cond2Fea[1] <- 0

  cond2IndFea <- array(NA, modelYears)
  cond2IndFea[1] <- if (cond2Fea[1] > 0.4) 1 else 0

  FeaIndex <- array(NA, modelYears)
  FeaIndex[1] <- min((cond1IndFea[1] + cond2IndFea[1]), 1)

  for (i in 2:modelYears) {
    cond1IndFea[i] <- if (feaAprJul[i] < 0.6) 1 else 0
    cond2Fea[i] <- if (feaOctSep[i] < 2.5 & feaOctSep[i - 1] < 2.5) {
        5 - feaOctSep[i] - feaOctSep[i - 1]
      } else 0
    cond2IndFea[i] <- if (cond2Fea[i] > 0.4) 1 else 0
    FeaIndex[[i]] <- as.integer(min((cond1IndFea[i] + cond2IndFea[i]), 1))
  }

  return(FeaIndex)
}

#Trinity River Index
trinWYT <- function(trinOctSep, modelYears) {
  TrinWYT <- array(NA, modelYears)
  for (i in 1:modelYears) {
    TrinWYT[i] <- if (trinOctSep[i] < 0.65) { as.integer(5)
      } else { if (trinOctSep[[i]] < 1.025) as.integer(4)
        else { if (trinOctSep[[i]] < 1.35) as.integer(3)
        else { if (trinOctSep[[i]] < 2) as.integer(2)
        else as.integer(1)
        }}}
    }
  return(TrinWYT)
}

