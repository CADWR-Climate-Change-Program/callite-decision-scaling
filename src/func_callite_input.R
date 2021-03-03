stationsToDatabase <- function(station_perturb_dir, callite_db, t_change_list,
  p_change_list, add_9_db_tbl_name, obs_11_db_tbl_name, rim_12_db_tbl_name,
  add_9_txt_file_name, obs_11_txt_file_name, rim_12_txt_file_name) {
  
  dbExecute(con,paste('DROP TABLE IF EXISTS',paste0(add_9_db_tbl_name),'CASCADE'))
  dbExecute(con,paste('DROP TABLE IF EXISTS',paste0(obs_11_db_tbl_name),'CASCADE'))
  dbExecute(con,paste('DROP TABLE IF EXISTS',paste0(rim_12_db_tbl_name),'CASCADE'))

  for (perturb in station_perturb_dir[2:64]) {
    # parse t and p from folder name of format "0_7DP1_0DT"
    t = substr(basename(perturb),6,8) # 0_7DP>>1_0<<DT
    p = substr(basename(perturb),1,3) # >>0_7<<DP1_0DT

    #### 9 Addt'l Stations ####
    tbl = read.table(file.path(perturb, add_9_txt_file_name),
                     header = T, sep = ',')
    colnames(tbl) <- c("Year", "Month", "BearRiver", "CacheCreek", "CalaverasRiver",
                       "ChowchillaRiver", "CosumnesRiver", "FresnoRiver",
                       "MokelumneRiver", "PutahCreek", "StonyCreek")

    tbl$dt <- round(as.numeric(t_change_list[[t]]) *1.0,1)
    tbl$dp <- round(as.numeric(p_change_list[[p]]) *1.0,1)
    Year <- tbl$Year
    Month <- tbl$Month
    dbWriteTable(callite_db, add_9_db_tbl_name, tbl, append = T, row.names=FALSE)

    tbl = read.table(file.path(perturb, obs_11_txt_file_name),
                     header = T, sep = ',')
    colnames(tbl) <- c("Year", "Month", "AMF", "BLB", "BND", "FTO", "MRC",
                       "SIS", "SJF", "SNS", "TLG", "TNL", "YRS")

    tbl$dt <- round(as.numeric(t_change_list[[t]]) *1.0,1)
    tbl$dp <- round(as.numeric(p_change_list[[p]]) *1.0,1)
    dbWriteTable(callite_db, obs_11_db_tbl_name, tbl, append = T, row.names=FALSE)

    tbl = read.table(file.path(perturb, rim_12_txt_file_name),
                     skip=3, header = F, sep = ',')
    colnames(tbl) <- c("I_FOLSM", "I_MCLRE", "I_MELON", "I_MLRTN",
                       "I_MOKELUMNE", "I_NHGAN", "I_OROVL", "I_PEDRO",
                       "I_SHSTA", "I_TRNTY", "I_YUBA", "I_WKYTN")
    tbl <- cbind(Year,Month,tbl)

    tbl$dt <- round(as.numeric(t_change_list[[t]]) *1.0,1)
    tbl$dp <- round(as.numeric(p_change_list[[p]]) *1.0,1)
    dbWriteTable(callite_db, rim_12_db_tbl_name, tbl, append = T, row.names=FALSE)
  }

  # Create views in the sqlite database for the volumetric streamflows
  # of the 11 observed and 9 additional stations.
  # The 12 rim flows are output from SAC-SMA-DS as volume
  # so there is no need to convert them.
  # 11 observed (mm to m to ft to ac-ft)
  view_SQL = paste('CREATE VIEW', paste0(obs_11_db_tbl_name, '_maf'),
                  'AS SELECT dt, dp, Year, Month,',
                  'AMF * 0.001 * 3.28084 * 1192084.247 / 1e6 AS AMF,',
                  'BLB * 0.001 * 3.28084 * 476088.972 / 1e6 AS BLB,',
                  'BND * 0.001 * 3.28084 * 6386051.927 / 1e6 AS BND,',
                  'FTO * 0.001 * 3.28084 * 2330562.077 / 1e6 AS FTO,',
                  'MRC * 0.001 * 3.28084 * 673997.868 / 1e6 AS MRC,',
                  'SIS * 0.001 * 3.28084 * 4780655.140 / 1e6 AS SIS,',
                  'SJF * 0.001 * 3.28084 * 1072680.817 / 1e6 AS SJF,',
                  'SNS * 0.001 * 3.28084 * 627490.889 / 1e6 AS SNS,',
                  'TLG * 0.001 * 3.28084 * 983437.363 / 1e6 AS TLG,',
                  'TNL * 0.001 * 3.28084 * 459906.679 / 1e6 AS TNL,',
                  'YRS * 0.001 * 3.28084 * 710451.826 / 1e6 AS YRS',
                  'FROM',paste0(obs_11_db_tbl_name))
  dbExecute(callite_db,view_SQL)
  # 9 additional (mm to m to m3 to af)
  view_SQL = paste('CREATE VIEW', paste0(add_9_db_tbl_name, '_taf'),
                  'AS SELECT dt, dp, Year, Month,',
                  'BearRiver * 0.001 * 730771760 * 0.000810714 / 1e3 AS BearRiver,',
                  'CacheCreek * 0.001 * 2455821437 * 0.000810714/ 1e3 AS CacheCreek,',
                  'CalaverasRiver * 0.001 * 927537363 * 0.000810714 / 1e3 AS CalaverasRiver,',
                  'ChowchillaRiver * 0.001 * 685393248 * 0.000810714 / 1e3 AS ChowchillaRiver,',
                  'CosumnesRiver * 0.001 * 1384751552 * 0.000810714 / 1e3 AS CosumnesRiver,',
                  'FresnoRiver * 0.001 * 717246675 * 0.000810714 / 1e3 AS FresnoRiver,',
                  'MokelumneRiver * 0.001 * 1408736800 * 0.000810714 / 1e3 AS MokelumneRiver,',
                  'PutahCreek * 0.001 * 1477609964 * 0.000810714 / 1e3 AS PutahCreek,',
                  'StonyCreek * 0.001 * 1926664012 * 0.000810714 / 1e3 AS StonyCreek',
                  'FROM', paste0(add_9_db_tbl_name))
  dbExecute(callite_db,view_SQL)
}

stationRead <- function(p_change_val,t_change_val,callite_db,calLite_run_count,model_years,
  sim_hydro_name,fields,generic_yr_check) {
  stations <- vector("list", length = calLite_run_count)
  for (run in 1:calLite_run_count) {
    stations[[run]] <- dbGetQuery(callite_db,paste('SELECT',fields,'FROM',sim_hydro_name,
                                                  'WHERE dt =',t_change_val,'AND dp =',p_change_val,
                                                  'ORDER BY Year',
                                                  'LIMIT',model_years * 12,
                                                  'OFFSET',(run - 1) * model_years * 12))

    if (generic_yr_check == TRUE) {
      stations[[run]][[1]] <-  rep(1:model_years, each = 12)
    }
  }
  return(stations)
}

wyTyping <- function(p_change_val,t_change_val,
                     sac_aji,sac_omi,sac_c,sac_d,sac_bn,sac_an,sac_w,
                     sj_aji,sj_omi,sj_c,sj_d,sj_bn,sj_an,sj_w,
                     callite_db,calLite_run_count,run_name,
                     model_years, obs_11_stations, dbWrite) {

  wyTypingResults <- vector("list", length = calLite_run_count)

  flowAgg <- lapply(obs_11_stations, function(x) flowAggregator(x))
  Apr_Jul_Sac = lapply(flowAgg, function(x) x$Apr_Jul_Sac)
  Oct_Mar_Sac = lapply(flowAgg, function(x) x$Oct_Mar_Sac)
  Oct_Sep_Sac = lapply(flowAgg, function(x) x$Oct_Sep_Sac)
  SacIndex <- lapply(flowAgg, function(x) sacIndex(x$Apr_Jul_Sac, x$Oct_Mar_Sac,model_years,
    sac_aji,sac_omi,sac_c,sac_d,sac_bn,sac_an,sac_w)$SacIndex)
  SacWYT = lapply(flowAgg, function(x) sacIndex(x$Apr_Jul_Sac, x$Oct_Mar_Sac,model_years,
    sac_aji,sac_omi,sac_c,sac_d,sac_bn,sac_an,sac_w)$SacWYT)
  Apr_Jul_SJ = lapply(flowAgg, function(x) x$Apr_Jul_SJ)
  Oct_Mar_SJ = lapply(flowAgg, function(x) x$Oct_Mar_SJ)
  Oct_Sep_SJ = lapply(flowAgg, function(x) x$Oct_Sep_SJ)
  SJIndex = lapply(flowAgg, function(x) sjIndex(x$Apr_Jul_SJ, x$Oct_Mar_SJ,model_years,
    sj_aji,sj_omi,sj_c,sj_d,sj_bn,sj_an,sj_w)$SJIndex)
  SJWYT = lapply(flowAgg, function(x) sjIndex(x$Apr_Jul_SJ, x$Oct_Mar_SJ,model_years,
    sj_aji,sj_omi,sj_c,sj_d,sj_bn,sj_an,sj_)$SJWYT)
  Jan_May_8Sta = lapply(flowAgg, function(x) x$Jan_May_8Sta)
  Apr_May_8Sta = lapply(flowAgg, function(x) x$Apr_May_8Sta)
  ShaIndex = lapply(flowAgg, function(x) shastaIndex(x$Oct_Sep_Sha,model_years))
  Apr_Sep_Am = lapply(flowAgg, function(x) x$Apr_Sep_Am)
  FeaIndex = lapply(flowAgg, function(x) featherIndex(x$Apr_Jul_Fea,x$Oct_Sep_Fea,model_years))
  Oct_Sep_Tr = lapply(flowAgg, function(x) x$Oct_Sep_Tr)
  TrinWYT = lapply(flowAgg, function(x) trinWYT(x$Oct_Sep_Tr, model_years))

  for (run in 1:calLite_run_count) {
    wyTypingResults[[run]] <-
        data.frame(
          Year = 1:model_years,
          Apr_Jul_Sac = Apr_Jul_Sac[[run]],
          Oct_Mar_Sac = Oct_Mar_Sac[[run]],
          Oct_Sep_Sac = Oct_Sep_Sac[[run]],
          SacIndex = as.numeric(SacIndex[[run]]),
          SacWYT = as.numeric(SacWYT[[run]]),
          Apr_Jul_SJ = Apr_Jul_SJ[[run]],
          Oct_Mar_SJ = Oct_Mar_SJ[[run]],
          Oct_Sep_SJ = Oct_Sep_SJ[[run]],
          SJIndex = as.numeric(SJIndex[[run]]),
          SJWYT = as.numeric(SJWYT[[run]]),
          Jan_May_8Sta = Jan_May_8Sta[[run]],
          Apr_May_8Sta = Apr_May_8Sta[[run]],
          ShaIndex = as.numeric(ShaIndex[[run]]),
          Apr_Sep_Am = Apr_Sep_Am[[run]],
          FeaIndex = as.numeric(FeaIndex[[run]]),
          Oct_Sep_Tr = Oct_Sep_Tr[[run]],
          TrinWYT = as.numeric(TrinWYT[[run]])
        )
    wyTypingForDB = wyTypingResults[[run]]
    wyTypingForDB$Year = seq((run*50) + 850,(run*50) + 849 + model_years)
    wyTypingForDB$dp = round(as.numeric(p_change_val) *1.0,1)
    wyTypingForDB$dt = round(as.numeric(t_change_val) *1.0,1)
    if (dbWrite == TRUE) {
      dbWriteTable(callite_db,paste0('sim_wyTyping_',run_name),
                   wyTypingForDB,append = TRUE,row.names=FALSE)
    }
  }
  return(wyTypingResults)
}

qMapping <- function(calLite_run_count, correlations, dss_in,
  add_9_stations, rim_12_stations, add_9_rim_12_historical) {

  # list of correlated simulated flows
  active_mainnodes <- as.character(unique(correlations[, 3]))

  # Merge simulated 9 add and 12 rim flow callite run lists
  flow_sacsma_node <- vector("list", length = calLite_run_count)
  flow_sacsma_node <- mapply(function(x,y) cbind(x[, 1:2],
                        x[, which(colnames(x) %in% active_mainnodes)],
                        y[, which(colnames(y) %in% active_mainnodes)]),
                        add_9_stations, rim_12_stations, SIMPLIFY = FALSE)

  # limit main_nodes_hist to those not correlated to any particular callite input (sub_nodes),
  # in this case only MokelumneRiver
  flow_hist_mainnode <- add_9_rim_12_historical[, c(1:2, which(colnames(add_9_rim_12_historical)
                                                       %in% active_mainnodes))]
  # callite default input observations for sub nodes
  flow_callite_subnode <- cbind(dss_in[, 1:2],
                                dss_in[, c(correlations[, 1])])

  # get list of 8 (no Mokelumne) and 12 simulated flow names
  main_nodes <- colnames(flow_hist_mainnode)[-(1:2)]

  # create mapped name list of callit default inputs (sub_node)
  # to its best correlated simulated flow (main_node)
  sub_nodes <- lapply(main_nodes, function(x) correlations[which(correlations[, 3] == x), 1])

  # run the quantile mapping
  dss_in_qmapped <- qMapCalLite(flow_sacsma_node,
                               flow_hist_mainnode,
                               flow_callite_subnode,
                               main_nodes,sub_nodes)

  # ANY NAN'S IN DSSIN_QMAPPED DATA?
  dss_in_qmapped <- lapply(dss_in_qmapped, function(x) {
    index <- unique(which(is.na(x), arr.ind = TRUE)[, 2])
    if (length(index) > 0) {
      print(which(is.na(x), arr.ind = TRUE))
      print(colnames(x)[index])
      for (i in 1:length(index)) {
        x[which(is.na(x[, index[i]])), index[i]] <-
          min(x[, index[i]], na.rm = TRUE)
      }
    }
    x
  })

  return(dss_in_qmapped)
}

wytMapping <- function(inputs_wyt_sac,inputs_wyt_sj,p_change_val,t_change_val,
  callite_db,calLite_run_count,model_years,run_name) {
  sql = paste("SELECT Year, MONTH,",inputs_wyt_sac,
            "FROM ref_SAC_WYTM_calc a",
            "INNER JOIN", paste0("sim_wyTyping_", run_name),
            "b ON a.SAC_WYT = b.SacWYT",
            "WHERE b.dt =",t_change_val,"AND b.dp =",p_change_val,
            "ORDER BY Year, wy_order, MONTH")
  sacWYtype <- dbGetQuery(callite_db,sql)

  sql = paste("SELECT Year, MONTH,",inputs_wyt_sj,
            "FROM ref_SJR_WYTM_calc  a",
            "INNER JOIN", paste0("sim_wyTyping_", run_name),
            "b ON a.SJR_WYT = b.SJWYT",
            "WHERE b.dt =",t_change_val,"AND b.dp =",p_change_val,
            "ORDER BY Year, wy_order, MONTH;")
  sjWYtype <- dbGetQuery(callite_db,sql)

  dss_in_wytyped <- vector("list",length=calLite_run_count)

  for (run in 1:calLite_run_count) {
    start <- ((run - 1) * model_years * 12) + 1
    end <- ((run) * model_years * 12)
    dss_in_wytyped[[run]] <- cbind(sacWYtype[(start:end),],
                                  sjWYtype[(start:end),-(1:2)])
  }
  return(dss_in_wytyped)
}

dssNew <- function(calLite_run_count,dss_in,dss_WYT,dss_Qmap) {

  dss_in_qMapWYT_cbind <- vector("list",length=calLite_run_count)
  dss_in_qMapWYT_cbind <- mapply(function(x,y) cbind(x,y[,-(1:2)]),
                                dss_Qmap, dss_WYT, SIMPLIFY = F)

  dss_in_new <- vector("list",length=calLite_run_count)
  dss_in_new <- mapply(function(x,y) {
    x <- dss_in[1:dim(y)[1],]
    for (i in 3:dim(dss_in)[2]) {
      x[, i] <- y[, which(colnames(y) == colnames(dss_in)[i])] # reorder to match dss_in
    }
    x[,-(1:2)] # remove year and month and return data frame
    },
    dss_in_new, dss_in_qMapWYT_cbind, SIMPLIFY = F)

  #Step 6d: Adjustments to AD and I terms
  #AD_WILKNS
  dss_in_new <- lapply(dss_in_new, function(x) {
      x <- apply(x, 1, function(z) {
        #AD_WILKNS
        z["AD_WILKNS.FLOW.ACCRDEPL"] <-
          if (z["I_SHSTA.FLOW.INFLOW"] <= 13300) {
            -1262 + 0.43 * z["I_SHSTA.FLOW.INFLOW"]
          } else {
            28262 - 1.7 * z["I_SHSTA.FLOW.INFLOW"]
          }
        #DEMAND_DAGUER - can't be greater than I_YUBA; if it is, replace it with I_YUBA
        dauger <- z["DEMAND_D_DAGUER_NP.DEMAND"] 
        yuba <- z["I_YUBA.FLOW.INFLOW"]
        z["DEMAND_D_DAGUER_NP.DEMAND"] <- if (dauger > yuba) {yuba} else {dauger}
        
        #AD_Calavaras and I512 and TS_adj_RB and TS_adj_WLK and INT_CWEIR and I501
        #must not fall below 0.00.
        z["AD_CALAVERAS.FLOW.ACCRDEPL"] <-
          if (z["AD_CALAVERAS.FLOW.ACCRDEPL"] < 0) 0 else z["AD_CALAVERAS.FLOW.ACCRDEPL"]
        z["I512.FLOW.INFLOW"] <-
          if (z["I512.FLOW.INFLOW"] < 0) 0 else z["I512.FLOW.INFLOW"]
        z["TS_ADJ_RB.FLOW"] <-
          if (z["TS_ADJ_RB.FLOW"] < 0) 0 else z["TS_ADJ_RB.FLOW"]
        z["TS_ADJ_WLK.FLOW"] <-
          if (z["TS_ADJ_WLK.FLOW"] < 0) 0 else z["TS_ADJ_WLK.FLOW"]
        z["INT_CWEIR.INTEGER"] <-
          if (z["INT_CWEIR.INTEGER"] < 0) 0 else z["INT_CWEIR.INTEGER"]
        z["I501.FLOW.INFLOW"] <-
          if (z["I501.FLOW.INFLOW"] < 0) 0 else z["I501.FLOW.INFLOW"]
        z
      })
     as.data.frame(t(x))
    }
  )
  return(dss_in_new)
}

writeDSS <- function(path_names,out_dir,dss_in_new,calLite_run_count) {
  for (j in 1:calLite_run_count) {
    cols_txtfiles <- c(100, 100, 100, 100, 100, 100, 100, 96)
    counter <- 0

    for (itxtfile in 1:length(cols_txtfiles)) {
      line1 <- "31OCT1921 2400"

      for (i in 2:cols_txtfiles[itxtfile]) {
        line1 <- paste0(line1, ",31OCT1921 2400")
      }

      line2 <- paste0(
          "/",
          path_names[(counter + 1), 'A'],
          "/",
          path_names[(counter + 1), 'B'],
          "/",
          path_names[(counter + 1), 'C'],
          "//",
          path_names[(counter + 1), 'D'],
          "/",
          path_names[(counter + 1), 'E'],
          "/"
        )

      for (i in 2:cols_txtfiles[itxtfile]) {
        line2 <- paste0(
            line2,
            ",/",
            path_names[(counter + i), 'A'],
            "/",
            path_names[(counter + i), 'B'],
            "/",
            path_names[(counter + i), 'C'],
            "//",
            path_names[(counter + i), 'D'],
            "/",
            path_names[(counter + i), 'E'],
            "/"
          )
      }

      line3 <- path_names[(counter + 1), 'F']

      for (i in 2:cols_txtfiles[itxtfile]) {
        line3 <- paste0(line3, ",", path_names[(counter + i), 'F'])
      }

      header <- rbind(as.character(line1),
              as.character(line2),
              as.character(line3))

      name <- file.path(out_dir, paste0("data_",j,"_",(counter + 1),"_",
          (counter + cols_txtfiles[itxtfile]),".txt")
        )

      write.table( header,
        name,
        row.names = FALSE,
        col.names = FALSE,
        quote = FALSE
      )

      write.table(
        round(dss_in_new[[j]][, (counter + 1):(counter + cols_txtfiles[itxtfile])], 6),
        name,
        quote = FALSE,
        row.names = FALSE,
        col.names = FALSE,
        append = TRUE,
        sep = ","
      )

      write.table(
        t(replicate(12, c(
          rep(0.00, cols_txtfiles[itxtfile])
        ))),
        name,
        quote = FALSE,
        row.names = FALSE,
        col.names = FALSE,
        append = TRUE,
        sep = ","
      )

      counter <- counter + cols_txtfiles[itxtfile]
    }
  }
}

table1create <- function(callite_db,calLite_run_count,model_years,
  tbl1,WYT_results,obs_11_stations) {
  table_1_new <- vector("list",length=calLite_run_count)
  table_1_new <- lapply(table_1_new, function(x) x <- tbl1)
  x2days <- dbGetQuery(callite_db,paste("SELECT * FROM ref_x2days_lookup"))
  
  # Get cnf days for August according to Jan-May 8 Station index
  cnf_days_month8 <- lapply(WYT_results, function(x) {
     apply(x, 1, function (z) {
      z <- if (z["Jan_May_8Sta"] < 7.35 & z["Apr_May_8Sta"] < 5) 0 else 31
    })})
  # Get cnf days for September according to Jan-May 8 Station index
  cnf_days_month9 <- lapply(WYT_results, function(x) {
     apply(x, 1, function (z) {
      z <- if (z["Jan_May_8Sta"] < 7.35 & z["Apr_May_8Sta"] < 5) 0 else 30
    })})
  # Fill in new cnf days to table 1 according to matching water year and month
  table_1_new <- mapply(function(x,y,z) {
    for (j in 1:length(y)) {
        x$cnf_days[which(x$wateryear == (j + 1921) & x$month == 8)] <- y[j]
        x$cnf_days[which(x$wateryear == (j + 1921) & x$month == 9)] <- z[j]
        }
    x
    }, table_1_new, cnf_days_month8, cnf_days_month9, SIMPLIFY = F)
  # Fill in new roe and chipps days according to 8 station index and x2 lookup
  table_1_new <- mapply(function(x,y) {
    
    sta8 <- cbind(y$Year+1921, y$Month,
                      y$BND + y$FTO + y$YRS + y$AMF + y$SNS + y$TLG + y$MRC + y$SJF)

    for (j in 1:(model_years*5)) {
      
      x$roe_days[j] <- if (x[j,3] == 0) {
          0
        } else {
          x2days[max(which(x2days[,7] < sta8[which(sta8[,1] == x[j,1] &
                    sta8[,2] == x[j,2]),3] * 1000)), (x[j,2] + 3)]
        }
      
      x$chs_days[j] <- if (x[j,3] == 0) {
        0
        } else if (
          x2days[max(which(x2days[,1] < sta8[which(sta8[,1] == x[j,1] &
                    sta8[,2] == x[j,2]),3] * 1000)), (x[j,2] - 3)] < x$roe_days[j]) {
          x$roe_days[j]
        } else {
          x2days[max(which(x2days[,1] < sta8[which(sta8[,1] == x[j,1] &
                    sta8[,2] == x[j,2]),3] * 1000)), (x[j,2] - 3)]
        }
      
    }
    x
    }, table_1_new, obs_11_stations,SIMPLIFY = F)
  return(table_1_new)
}

table2create <- function(calLite_run_count,model_years,tbl2,obs_11_stations) {
  table_2_new <- vector("list",length=calLite_run_count)
  table_2_new <- lapply(table_2_new, function(x) x <- tbl2)
  table_2_new <- mapply(function(x,y) {
    sta8 <- cbind(y$Year+1921, y$Month,
                      y$BND + y$FTO + y$YRS + y$AMF + y$SNS + y$TLG + y$MRC + y$SJF)
    for (i in 2:(2 + model_years - 1)) {
      x[i, 2] <- sta8[which(sta8[, 1] == x[i, 1] & sta8[, 2] == 12), 3] * 1000
    }
    x
    },table_2_new, obs_11_stations, SIMPLIFY = F)
}

table3create <- function(calLite_run_count,model_years,tbl3,WYT_results) {
  table_3_new <- vector("list",length=calLite_run_count)
  table_3_new <- lapply(table_3_new, function(x) x <- tbl3)
  table_3_new <- mapply(function(x,y) {
    for (j in 17:(17 + model_years - 1)) {
      x[j, 2] <- y$Oct_Mar_Sac[j - 16]
      x[j, 3] <- y$Apr_Jul_Sac[j - 16]
      x[j, 4] <- y$Oct_Sep_Sac[j - 16]
      x[j, 5] <- y$SacIndex[j - 16]
    }
    x
    }, table_3_new, WYT_results, SIMPLIFY = F)
}

table4create <- function(calLite_run_count,model_years,tbl4,WYT_results) {
  table_4_new <- vector("list",length=calLite_run_count)
  table_4_new <- lapply(table_4_new, function(x) x <- tbl4)
  table_4_new <- mapply(function(x,y) {
    for (j in 4:(4 + model_years - 1)) {
      x[j, 2] <- y$SJWYT[j - 3]
    }
    x
    }, table_4_new, WYT_results, SIMPLIFY = F)
}

table5create <- function(calLite_run_count,model_years,tbl4New,tbl5) {
  table_5_new <- vector("list",length=calLite_run_count)
  table_5_new <- lapply(table_5_new, function(x) x <- tbl5)
  table_5_new <- mapply(function(x,y) {
    SJR5ave <- rollmean(y[2], 5, na.pad = TRUE, align = "right")
    x[2:82, 2] <- SJR5ave[5:85]
    x
    }, table_5_new, tbl4New, SIMPLIFY = F)
}

table6create <- function(callite_db,calLite_run_count,model_years,tbl6,obs_11_stations) {
  table_6_new <- vector("list",length=calLite_run_count)
  table_6_new <- lapply(table_6_new, function(x) x <- tbl6)
  febei_lookup <- dbGetQuery(callite_db,paste("SELECT * FROM ref_FebEI_lookup"))
  table_6_new <- mapply(function(x,y) {
    y <- subset(y, y$Month==1)
    sta8 <- cbind(y$Year+1921, y$BND + y$FTO + y$YRS + y$AMF + y$SNS + y$TLG + y$MRC + y$SJF)
    for (j in 1:model_years) {
      # ratio is max of feb ei lookup values for given january 8 station flow amount
        x[j,2] <- febei_lookup[
          max(which(febei_lookup[, 1] < sta8[which(sta8[, 1] == x[j, 1]), 2])), 2]
      }
    x
    }, table_6_new, obs_11_stations, SIMPLIFY = F)
}

table7create <- function(calLite_run_count,model_years,tbl7,WYT_results) {
  table_7_new <- vector("list",length=calLite_run_count)
  table_7_new <- lapply(table_7_new, function(x) x <- tbl7)
  table_7_new <- mapply(function(x,y) {
    for (j in 2:(2 + model_years - 1)) {
        x[j, 2] <- y$Jan_May_8Sta[j - 1] * 1000
      }
    x
    }, table_7_new, WYT_results, SIMPLIFY = F)
}

table8create <- function(calLite_run_count,model_years,tbl8,WYT_results) {
  table_8_new <- vector("list",length = calLite_run_count)
  table_8_new <- lapply(table_8_new, function(x) x <- tbl8)
  table_8_new <- mapply(function(x,y) {
    for (j in 3:(3 + model_years - 1)) {
        x[j, 2] <- y$SacWYT[j - 2]
        x[j, 3] <- y$SJWYT[j - 2]
        x[j, 4] <- if (y$ShaIndex[j - 2] == 1) 4 else 1
        x[j, 5] <- if (y$Apr_Sep_Am[j - 2] < 0.6) 2 else 1
        x[j, 6] <- y$FeaIndex[j - 2]
        x[j, 7] <- y$TrinWYT[j - 2]
      }
    x
    }, table_8_new, WYT_results, SIMPLIFY = F)
}

table9create <- function(calLite_run_count,model_years,tbl9,WYT_results) {
  table_9_new <- vector("list",length = calLite_run_count)
  table_9_new <- lapply(table_9_new, function(x) x <- tbl9)
  table_9_new <- mapply(function(x,y) {
    for (j in 2:(2 + model_years - 1)) {
        x[j, 2] <- y$TrinWYT[j - 1]
      }
    x
    }, table_9_new, WYT_results, SIMPLIFY = F)
}

table10create <- function(calLite_run_count,model_years,tbl10,sac_air_temp,sac_temp_trigger,iTemp) {
  table_10_new <- vector("list",length = calLite_run_count)
  table_10_new <- lapply(table_10_new, function(x) x <- tbl10)
  calLiteRun <- 1
  sac_air_tempPerturbSet <- sac_air_temp[[iTemp]]
  table_10_new <- lapply(table_10_new, function(x) {
    month <- array(NA, model_years)
    day <- array(NA, model_years)
    for (j in 1:model_years) {
      sample <- sac_air_tempPerturbSet[which(sac_air_tempPerturbSet[, 1] == 899 + j +
                                           (calLiteRun - 1) * model_years), ]$TAVG
      index1 <- which(sample < sac_temp_trigger)
      if ((length(index1) > 0) & (index1[1] != length(sample))) {
        index <- index1[min(which(sample[index1 + 1] >= sac_temp_trigger))]
        lower <- sample[index]
        upper <- sample[index + 1]
        days <- as.numeric((sac_temp_trigger - lower) / (upper - lower) * 30) #every month as 30 days
        month[j] <- if (days < 15) { index - 1 + 3 } else { index - 1 + 4 }
        day[j] <- if (days < 15) { 15 + round(days, 0) } else { round(days, 0) - 15 }
        day[j] <- if (day[j] == 0) 1 else day[j]
      } else {
        month[j] <- 3
        day[j] <- 15
      }
    }
    x[1:model_years, 2] <- month
    x[1:model_years, 3] <- day
    calLiteRun <<- calLiteRun + 1
    x
    })
}

table11create <- function(callite_db,calLite_run_count,model_years,
  tbl11,obs_11_stations,obs_11_stations_hist,random_forecast,forecast_case) {
  
  table_11_new <- vector("list",length = calLite_run_count)
  table_11_new <- lapply(table_11_new, function(x) x <- tbl11)
  # Get forecast error stats for AMF
  forecast_stat <- dbGetQuery(callite_db,paste("SELECT * FROM ref_forecast_error",
                                             "WHERE station='AMF' AND forecastCase=",
                                           paste0("'",forecast_case,"'"),
                                             "ORDER BY month"))
  
  table_11_new <- mapply(function(x,y,z) {
    #sum month-set flows and convert to TAF
    feb_sep <- group_by(y[, c(1:2, 3)], Year) %>%
      filter(Month %in% c(2:9)) %>%
      summarise(foreflow_feb = sum(AMF) * 1000)
    mar_sep <- group_by(y[, c(1:2, 3)], Year) %>%
      filter(Month %in% c(3:9)) %>%
      summarise(foreflow_mar = sum(AMF) * 1000)
    apr_sep <- group_by(y[, c(1:2, 3)], Year) %>%
      filter(Month %in% c(4:9)) %>%
      summarise(foreflow_apr = sum(AMF) * 1000)
    may_sep <- group_by(y[, c(1:2, 3)], Year) %>%
      filter(Month %in% c(5:9)) %>%
      summarise(foreflow_may = sum(AMF) * 1000)

    #get minimum historical month-set flow in MAF
    feb_sep_min <- as.data.frame(obs_11_stations_hist) %>%
      filter(MONTH %in% c(2:9)) %>% group_by(YEAR) %>%
      summarise(feb_sep_sum = sum(AMF_hist)) %>%
      min(.$feb_sep_sum, na.rm = T)
    mar_sep_min <- as.data.frame(obs_11_stations_hist) %>%
      filter(MONTH %in% c(3:9)) %>% group_by(YEAR) %>%
      summarise(mar_sep_sum = sum(AMF_hist)) %>%
      min(.$mar_sep_sum, na.rm = T)
    apr_sep_min <- as.data.frame(obs_11_stations_hist) %>%
      filter(MONTH %in% c(4:9)) %>% group_by(YEAR) %>%
      summarise(apr_sep_sum = sum(AMF_hist)) %>%
      min(.$apr_sep_sum, na.rm = T)
    may_sep_min <- as.data.frame(obs_11_stations_hist) %>%
      filter(MONTH %in% c(5:9)) %>% group_by(YEAR) %>%
      summarise(may_sep_sum = sum(AMF_hist)) %>%
      min(.$may_sep_sum, na.rm = T)

    for (k in 1:model_years) {
        febrand <- qnorm(z[(k - 1) * 5 + 2, 3], mean = forecast_stat$mean[1], forecast_stat$sd[1])
        marrand <- qnorm(z[(k - 1) * 5 + 3, 3], mean = forecast_stat$mean[2], forecast_stat$sd[2])
        aprrand <- qnorm(z[(k - 1) * 5 + 4, 3], mean = forecast_stat$mean[3], forecast_stat$sd[3])
        mayrand <- qnorm(z[(k - 1) * 5 + 5, 3], mean = forecast_stat$mean[4], forecast_stat$sd[4])
        FEB_0 <- max(feb_sep_min * 1000,
            round(feb_sep$foreflow_feb[k] - feb_sep$foreflow_feb[k] * febrand,0))
        MAR_0 <- max(mar_sep_min * 1000,
            round(mar_sep$foreflow_mar[k] - mar_sep$foreflow_mar[k] * marrand,0))
        APR_0 <- max(apr_sep_min * 1000,
            round(apr_sep$foreflow_apr[k] - apr_sep$foreflow_apr[k] * aprrand,0))
        MAY_0 <- max(may_sep_min * 1000,
            round(may_sep$foreflow_may[k] - may_sep$foreflow_may[k] * mayrand,0))
        x[k, 2] <- FEB_0
        x[k, 3] <- MAR_0
        x[k, 4] <- APR_0
        x[k, 5] <- MAY_0
    }
    x
    }, table_11_new, obs_11_stations, random_forecast, SIMPLIFY = F)
}

table12create <- function(callite_db,calLite_run_count,model_years,
  tbl12,obs_11_stations,obs_11_stations_hist,random_forecast,forecast_case) {
  table_12_new <- vector("list",length = calLite_run_count)
  table_12_new <- lapply(table_12_new, function(x) x <- tbl12)
  # Get forecast error stats for AMF
  forecast_stat <- dbGetQuery(callite_db,paste("SELECT * FROM ref_forecast_error",
                                             "WHERE station='FTO' AND forecastCase=",
                                           paste0("'",forecast_case,"'"),
                                             "ORDER BY month"))
  
  table_12_new <- mapply(function(x,y,z) {
    #sum month-set flows and convert to TAF
    jan_sep <- group_by(y[, c(1:2,6)], Year) %>%
      filter(Month %in% c(1:9)) %>%
      summarise(foreflow_jan = sum(FTO) * 1000)
    feb_sep <- group_by(y[, c(1:2,6)], Year) %>%
      filter(Month %in% c(2:9)) %>%
      summarise(foreflow_feb = sum(FTO) * 1000)
    mar_sep <- group_by(y[, c(1:2,6)], Year) %>%
      filter(Month %in% c(3:9)) %>%
      summarise(foreflow_mar = sum(FTO) * 1000)
    apr_sep <- group_by(y[, c(1:2,6)], Year) %>%
      filter(Month %in% c(4:9)) %>%
      summarise(foreflow_apr = sum(FTO) * 1000)
    may_sep <- group_by(y[, c(1:2,6)], Year) %>%
      filter(Month %in% c(5:9)) %>%
      summarise(foreflow_may = sum(FTO) * 1000)

    #get minimum historical month-set flow in MAF
    jan_sep_min <- as.data.frame(obs_11_stations_hist) %>%
      filter(MONTH %in% c(1:9)) %>% group_by(YEAR) %>%
      summarise(jan_sep_sum = sum(FTO_hist)) %>%
      min(.$jan_sep_sum, na.rm = T)
    feb_sep_min <- as.data.frame(obs_11_stations_hist) %>%
      filter(MONTH %in% c(2:9)) %>% group_by(YEAR) %>%
      summarise(feb_sep_sum = sum(FTO_hist)) %>%
      min(.$feb_sep_sum, na.rm = T)
    mar_sep_min <- as.data.frame(obs_11_stations_hist) %>%
      filter(MONTH %in% c(3:9)) %>% group_by(YEAR) %>%
      summarise(mar_sep_sum = sum(FTO_hist)) %>%
      min(.$mar_sep_sum, na.rm = T)
    apr_sep_min <- as.data.frame(obs_11_stations_hist) %>%
      filter(MONTH %in% c(4:9)) %>% group_by(YEAR) %>%
      summarise(apr_sep_sum = sum(FTO_hist)) %>%
      min(.$apr_sep_sum, na.rm = T)
    may_sep_min <- as.data.frame(obs_11_stations_hist) %>%
      filter(MONTH %in% c(5:9)) %>% group_by(YEAR) %>%
      summarise(may_sep_sum = sum(FTO_hist)) %>%
      min(.$may_sep_sum, na.rm = T)

    for (k in 1:model_years) {
        janrand <- qnorm(z[(k - 1) * 5 + 2, 3], mean = forecast_stat$mean[1], forecast_stat$sd[1])
        febrand <- qnorm(z[(k - 1) * 5 + 2, 3], mean = forecast_stat$mean[2], forecast_stat$sd[2])
        marrand <- qnorm(z[(k - 1) * 5 + 3, 3], mean = forecast_stat$mean[3], forecast_stat$sd[3])
        aprrand <- qnorm(z[(k - 1) * 5 + 4, 3], mean = forecast_stat$mean[4], forecast_stat$sd[4])
        mayrand <- qnorm(z[(k - 1) * 5 + 5, 3], mean = forecast_stat$mean[5], forecast_stat$sd[5])
        JAN_0 <- max(jan_sep_min * 1000,
            round(jan_sep$foreflow_jan[k] - jan_sep$foreflow_jan[k] * janrand,0))
        FEB_0 <- max(feb_sep_min * 1000,
            round(feb_sep$foreflow_feb[k] - feb_sep$foreflow_feb[k] * febrand,0))
        MAR_0 <- max(mar_sep_min * 1000,
            round(mar_sep$foreflow_mar[k] - mar_sep$foreflow_mar[k] * marrand,0))
        APR_0 <- max(apr_sep_min * 1000,
            round(apr_sep$foreflow_apr[k] - apr_sep$foreflow_apr[k] * aprrand,0))
        MAY_0 <- max(may_sep_min * 1000,
            round(may_sep$foreflow_may[k] - may_sep$foreflow_may[k] * mayrand,0))
        x[(k - 1) * 5 + 1, 3] <- JAN_0
        x[(k - 1) * 5 + 2, 3] <- FEB_0
        x[(k - 1) * 5 + 3, 3] <- MAR_0
        x[(k - 1) * 5 + 4, 3] <- APR_0
        x[(k - 1) * 5 + 5, 3] <- MAY_0
      }
    x
    }, table_12_new, obs_11_stations, random_forecast, SIMPLIFY = F)
}

table13create <- function(callite_db,calLite_run_count,model_years,
  tbl13,obs_11_stations,obs_11_stations_hist,random_forecast,forecast_case) {
  table_13_new <- vector("list",length = calLite_run_count)
  table_13_new <- lapply(table_13_new, function(x) x <- tbl13)
  # Get forecast error stats for AMF
  forecast_stat <- dbGetQuery(callite_db,paste("SELECT * FROM ref_forecast_error",
                                             "WHERE station='SIS' AND forecastCase=",
                                           paste0("'",forecast_case,"'"),
                                             "ORDER BY month"))
  
  table_13_new <- mapply(function(x,y,z) {
    #sum month-set flows and convert to TAF
    feb_sep <- group_by(y[, c(1:2, 8)], Year) %>%
      filter(Month %in% c(2:9)) %>%
      summarise(foreflow_feb = sum(SIS) * 1000)
    mar_sep <- group_by(y[, c(1:2, 8)], Year) %>%
      filter(Month %in% c(3:9)) %>%
      summarise(foreflow_mar = sum(SIS) * 1000)
    apr_sep <- group_by(y[, c(1:2, 8)], Year) %>%
      filter(Month %in% c(4:9)) %>%
      summarise(foreflow_apr = sum(SIS) * 1000)
    may_sep <- group_by(y[, c(1:2, 8)], Year) %>%
      filter(Month %in% c(5:9)) %>%
      summarise(foreflow_may = sum(SIS) * 1000)

    #get minimum historical month-set flow in MAF
    feb_sep_min <- as.data.frame(obs_11_stations_hist) %>%
      filter(MONTH %in% c(2:9)) %>% group_by(YEAR) %>%
      summarise(feb_sep_sum = sum(SIS_hist)) %>%
      min(.$feb_sep_sum, na.rm = T)
    mar_sep_min <- as.data.frame(obs_11_stations_hist) %>%
      filter(MONTH %in% c(3:9)) %>% group_by(YEAR) %>%
      summarise(mar_sep_sum = sum(SIS_hist)) %>%
      min(.$mar_sep_sum, na.rm = T)
    apr_sep_min <- as.data.frame(obs_11_stations_hist) %>%
      filter(MONTH %in% c(4:9)) %>% group_by(YEAR) %>%
      summarise(apr_sep_sum = sum(SIS_hist)) %>%
      min(.$apr_sep_sum, na.rm = T)
    may_sep_min <- as.data.frame(obs_11_stations_hist) %>%
      filter(MONTH %in% c(5:9)) %>% group_by(YEAR) %>%
      summarise(may_sep_sum = sum(SIS_hist)) %>%
      min(.$may_sep_sum, na.rm = T)

    for (k in 1:model_years) {
        febrand <- qnorm(z[(k - 1) * 5 + 2, 3], mean = forecast_stat$mean[1], forecast_stat$sd[1])
        marrand <- qnorm(z[(k - 1) * 5 + 3, 3], mean = forecast_stat$mean[2], forecast_stat$sd[2])
        aprrand <- qnorm(z[(k - 1) * 5 + 4, 3], mean = forecast_stat$mean[3], forecast_stat$sd[3])
        mayrand <- qnorm(z[(k - 1) * 5 + 5, 3], mean = forecast_stat$mean[4], forecast_stat$sd[4])
        FEB_0 <- max(feb_sep_min * 1000,
            round(feb_sep$foreflow_feb[k] - feb_sep$foreflow_feb[k] * febrand,0))
        MAR_0 <- max(mar_sep_min * 1000,
            round(mar_sep$foreflow_mar[k] - mar_sep$foreflow_mar[k] * marrand,0))
        APR_0 <- max(apr_sep_min * 1000,
            round(apr_sep$foreflow_apr[k] - apr_sep$foreflow_apr[k] * aprrand,0))
        MAY_0 <- max(may_sep_min * 1000,
            round(may_sep$foreflow_may[k] - may_sep$foreflow_may[k] * mayrand,0))
        x[k, 2] <- FEB_0
        x[k, 3] <- MAR_0
        x[k, 4] <- APR_0
        x[k, 5] <- MAY_0
      }
    x
    }, table_13_new, obs_11_stations, random_forecast, SIMPLIFY = F)
}

writeDB <- function(p_change_val,t_change_val,callite_db,calLite_run_count,model_years,
                    table_data,run_name,month_count,columns,table_name,cast_table_check) {
  
  for (run in 1:calLite_run_count) {
    
    if (cast_table_check == TRUE) {
      
      tableToDB = table_data[[run]][1:(model_years*month_count),columns]
      tableToDB = dcast(tableToDB, wateryear~month)
      colnames(tableToDB) = c('Year','JAN_0','FEB_0','MAR_0','APR_0','MAY_0')
      tableToDB$Year = seq((run*50) + 850,(run*50) + 849 + model_years)
      tableToDB$dp = round(as.numeric(p_change_val) *1.0,1)
      tableToDB$dt = round(as.numeric(t_change_val) *1.0,1)
      
    } else {
      
      tableToDB = table_data[[run]][1:model_years,columns]
      tableToDB$Year = seq((run*50) + 850,(run*50) + 849 + model_years)
      tableToDB$dp = round(as.numeric(p_change_val) *1.0,1)
      tableToDB$dt = round(as.numeric(t_change_val) *1.0,1)
      
    }
    
    dbWriteTable(callite_db,paste0('sim_',table_name,'_',run_name),tableToDB,append = TRUE)
    
  }
}

writeTable <- function(calLite_run_count,out_dir,table_name,table,def_table_path,n_rows_header) {
  for (j in 1:calLite_run_count) {

    header <- read.table(def_table_path,
      header = FALSE,
      nrows = n_rows_header,
      fill = TRUE,
      colClasses = "character",
      sep = "\t")

    colHeader <- read.table(def_table_path,
      header = FALSE,
      skip = n_rows_header,
      nrows = 1,
      fill = TRUE,
      colClasses = "character")

    name <- file.path(out_dir,paste0(table_name,"_",j,".table"))

    write.table(
      header,
      name,
      row.names = FALSE,
      col.names = FALSE,
      quote = FALSE
    )

    write.table(
      colHeader,
      name,
      row.names = FALSE,
      col.names = FALSE,
      quote = FALSE,
      append = TRUE,
      sep = "\t"
    )

    write.table(
      table[[j]],
      name,
      quote = FALSE,
      row.names = FALSE,
      col.names = FALSE,
      append = TRUE,
      sep = "\t"
    )
  }
}

##### Project Setup #####

##### THIS GRABS THE DSS INPUTS TO CALLITE, 100 AT A TIME ######
dssCreate <- function(def_DSS_dir) {
  years <- rep(seq(1922, 2003), each = 12)
  months <- rep(c(10, 11, 12, 1:9), 82)
  dss_in_1_100 <-
    read.table(
      file.path(def_DSS_dir,"dssin_1-100.txt"),
      header = TRUE,
      sep = ","
    )
  dss_in_1_100 <- dss_in_1_100[, -1]
  dss_in_1_100 <- cbind(years, months, dss_in_1_100)
  dss_in_101_200 <-
    read.table(
      file.path(def_DSS_dir,"dssin_101-200.txt"),
      header = TRUE,
      sep = ","
    )
  dss_in_101_200 <- dss_in_101_200[, -1]
  dss_in_101_200 <- cbind(years, months, dss_in_101_200)
  dss_in_201_300 <-
    read.table(
      file.path(def_DSS_dir,"dssin_201-300.txt"),
      header = TRUE,
      sep = ","
    )
  dss_in_201_300 <- dss_in_201_300[, -1]
  dss_in_201_300 <- cbind(years, months, dss_in_201_300)
  dss_in_301_400 <-
    read.table(
      file.path(def_DSS_dir,"dssin_301-400.txt"),
      header = TRUE,
      sep = ","
    )
  dss_in_301_400 <- dss_in_301_400[, -1]
  dss_in_301_400 <- cbind(years, months, dss_in_301_400)
  dss_in_401_500 <-
    read.table(
      file.path(def_DSS_dir,"dssin_401-500.txt"),
      header = TRUE,
      sep = ","
    )
  dss_in_401_500 <- dss_in_401_500[, -1]
  dss_in_401_500 <- cbind(years, months, dss_in_401_500)
  dss_in_501_600 <-
    read.table(
      file.path(def_DSS_dir,"dssin_501-600.txt"),
      header = TRUE,
      sep = ","
    )
  dss_in_501_600 <- dss_in_501_600[, -1]
  dss_in_501_600 <- cbind(years, months, dss_in_501_600)
  dss_in_601_700 <-
    read.table(
      file.path(def_DSS_dir,"dssin_601-700.txt"),
      header = TRUE,
      sep = ","
    )
  dss_in_601_700 <- dss_in_601_700[, -1]
  dss_in_601_700 <- cbind(years, months, dss_in_601_700)
  dss_in_701_796 <-
    read.table(
      file.path(def_DSS_dir,"dssin_701-796.txt"),
      header = TRUE,
      sep = ","
    )
  dss_in_701_796 <- dss_in_701_796[, -1]
  dss_in_701_796 <- cbind(years, months, dss_in_701_796)

  dss_input <-
    cbind(
      dss_in_1_100,
      dss_in_101_200[, -(1:2)],
      dss_in_201_300[, -(1:2)],
      dss_in_301_400[, -(1:2)],
      dss_in_401_500[, -(1:2)],
      dss_in_501_600[, -(1:2)],
      dss_in_601_700[, -(1:2)],
      dss_in_701_796[, -(1:2)]
    )
  dss_in_colnames <- colnames(dss_input)
  dss_in_colnames_short <-
    sub(".*?CALLITE.(.*?)..1MON.*", "\\1", dss_in_colnames)
  colnames(dss_input) <- dss_in_colnames_short

  return(dss_input)
}

##### CalLite Input Directory Creation #####
calliteInputDirCreate <- function(pt_run_folder,input_dir) {
  dir.create(file.path(input_dir,pt_run_folder,
                       "DSS"),recursive = TRUE)
  dir.create(file.path(input_dir,pt_run_folder,
                       "New_ForecastTables"), recursive = TRUE)
  dir.create(file.path(input_dir,pt_run_folder,
                       "Lookup_tables"), recursive = TRUE)
}

# Function to rename data frame columns in a list of data frames
# The first argument is the list object
# Second argument is the names vector
columnRename <- function(data_list, new_names)  {
  lapply(seq(data_list), function(x) {
    y <- data.frame(data_list[[x]])
    names(y) <- new_names
    return(y)
  })
}

