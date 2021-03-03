
collectDSSvar <- function(type, vars, callite_DB, sim_name, calLite_input_dir,
                          pt_run_folder, folder_sub_path, callite_run_count,
                          pChangeVal,tChangeVal,sdChangeVal) {

    # create empty dss and var lists for iteration
    dssList = list()
    varList = list()

    for (i in 1:callite_run_count) {

      dss <-  file.path(calLite_input_dir,pt_run_folder,folder_sub_path,paste0('Output_',type,'_',i,'_BaseCase.dss'))
      mydss = opendss(dss, warnIfNew=TRUE, stopIfNew=FALSE)

      for (var in 1:nrow(vars)) {

        timeSeries = getFullTSC(mydss, vars$path[var])
        timeSeries = timeSeries["1921-11/1971-10"]
        colnames(timeSeries) <- vars$name[var]
        varList[[var]] <- as.data.frame(timeSeries)

      }

      varsOut <- dplyr::bind_cols(varList)
      months <- as.integer(rep(c(10,11,12,1,2,3,4,5,6,7,8,9),50))
      years <- as.integer(rep((900+((i-1)*50)):(949+((i-1)*50)),each=12))
      varsOut$Year <- years
      varsOut$Month <- months
      dssList[[dss]] <- varsOut

    }

    dssAll <- dplyr::bind_rows(dssList)
    dssAll$dt <- round(as.numeric(tChangeVal *1.0),1)
    dssAll$dp <- round(as.numeric(pChangeVal *1.0),1)
    dssAll$dsd <- round(as.numeric(sdChangeVal *1.0),1)
    dbWriteTable(callite_DB, sim_name,dssAll,append=T)

}

createAnnualTable <- function (dbCon,runname,perturbY,perturbX) {

  perturbX <- if(perturbX=='P_lev') {'dp'} else if (perturbX=='SD_lev') {'dsd'}

  sql = paste0('
        DROP TABLE IF EXISTS sim_calliteDV_',runname,'_annual;')
  dbExecute(dbCon,sql)

  sql = paste0('
        CREATE TABLE sim_calliteDV_',runname,'_annual AS 
        SELECT A.Water_Year, A.dt, A.',perturbX,', 
          A.SWP_DELIVERIES, A.CVPTOTALDEL, A.DELTA_EXPORTS, A.D_BANKS, A.D_JONES,
          A.SHORTAGES, A.EXP_ANN_RELAX, A.MRDO_ANN_RELAX,
          B.NOD_STORAGE_apr, B.S_TRNTY_apr, B.S_SHSTA_apr, B.S_FOLSM_apr, B.S_OROVL_apr, B.S_SLCVP_apr, B.S_SLSWP_apr, B.S_WKYTN_apr,
          C.NOD_STORAGE_oct, C.S_TRNTY_oct, C.S_SHSTA_oct, C.S_FOLSM_oct, C.S_OROVL_oct, C.S_SLCVP_oct, C.S_SLSWP_oct, C.S_WKYTN_oct,
          D.C_SACSJR_winter, E.C_SACSJR_spring, F.C_SACSJR_summer, G.C_SACSJR_fall
        from (SELECT Year as Water_Year, dt,  ',perturbX,',
          sum(SWP_CO_TOTAL__SWP_DELIVERY * 59.4) + 
          sum(SWP_TA_TOTAL__SWP_DELIVERY * 59.4) +
          sum(SWP_IN_TOTAL__SWP_DELIVERY * 59.4) AS SWP_DELIVERIES,
          sum(`EXP_ANN_RELAX__SOFT-CONSTRAINT` * 59.4) AS EXP_ANN_RELAX,
          sum(`MRDO_ANN_RELAX__SOFT-CONSTRAINT` * 59.4) AS MRDO_ANN_RELAX,
          sum(`EXP_ANN_RELAX__SOFT-CONSTRAINT` * 59.4) + 
          sum(`MRDO_ANN_RELAX__SOFT-CONSTRAINT` * 59.4) +
          sum(`SHORT_AD_HST__SHORTAGE-FLOW` * 59.4) +
          sum(`SHORT_AD_KSWCK__SHORTAGE-FLOW` * 59.4) +
          sum(`SHORT_AD_NIMBUS__SHORTAGE-FLOW` * 59.4) +
          sum(`SHORT_AD_REDBLF__SHORTAGE-FLOW` * 59.4) +
          sum(`SHORT_AD_SACAME__SHORTAGE-FLOW` * 59.4) +
          sum(`SHORT_AD_SACFEA__SHORTAGE-FLOW` * 59.4) +
          sum(`SHORT_AD_THERM__SHORTAGE-FLOW` * 59.4) +
          sum(`SHORT_AD_WILKNS__SHORTAGE-FLOW` * 59.4) +
          sum(`SHORT_AD_YOLOBP__SHORTAGE-FLOW` * 59.4) +
          sum(`SHORT_AD_YUBFEA__SHORTAGE-FLOW` * 59.4) AS SHORTAGES, -- SHORT_C_MERCED1TOTAL and SHORT_D_HST
          sum(`D_BANKS__FLOW-DELIVERY` * 59.4) AS D_BANKS,
            sum(`D_JONES__FLOW-DELIVERY` * 59.4) AS D_JONES,
            sum(`D_BANKS__FLOW-DELIVERY` * 59.4) + sum(`D_JONES__FLOW-DELIVERY` * 59.4) AS DELTA_EXPORTS,
            sum(`CVPTOTALDEL__FLOW-DELIVERY` * 59.4) AS CVPTOTALDEL
        --     sum(`DEL_CVP_PAG_S__FLOW-DELIVERY` * 59.4) AS DEL_CVP_PAG_S
          FROM sim_calliteDV_',runname,' 
          GROUP BY Year, dt, ',perturbX,') A
        join (SELECT Year as Water_Year, dt,  ',perturbX,',
            S_TRNTY__STORAGE as S_TRNTY_apr,
                S_SHSTA__STORAGE as S_SHSTA_apr,
                S_FOLSM__STORAGE as S_FOLSM_apr,
                S_OROVL__STORAGE as S_OROVL_apr,
                S_SLCVP__STORAGE as S_SLCVP_apr,
                S_SLSWP__STORAGE as S_SLSWP_apr,
                S_WKYTN__STORAGE as S_WKYTN_apr,
                S_SHSTA__STORAGE + S_OROVL__STORAGE + S_FOLSM__STORAGE + S_TRNTY__STORAGE as NOD_STORAGE_apr
          FROM sim_calliteDV_',runname,' 
          WHERE Month = 4
          GROUP BY Year, dt, ',perturbX,') B on A.dt = B.dt and A.',perturbX,' = B.',perturbX,' and A.Water_Year = B.Water_Year
        join (SELECT Year as Water_Year, dt,  ',perturbX,',
            S_TRNTY__STORAGE as S_TRNTY_oct,
                S_SHSTA__STORAGE as S_SHSTA_oct,
                S_FOLSM__STORAGE as S_FOLSM_oct,
                S_OROVL__STORAGE as S_OROVL_oct,
                S_SLCVP__STORAGE as S_SLCVP_oct,
                S_SLSWP__STORAGE as S_SLSWP_oct,
                S_WKYTN__STORAGE as S_WKYTN_oct,
                S_SHSTA__STORAGE + S_OROVL__STORAGE + S_FOLSM__STORAGE + S_TRNTY__STORAGE as NOD_STORAGE_oct
          FROM sim_calliteDV_',runname,'
          WHERE Month = 9
          GROUP BY Year, dt, ',perturbX,') C on A.dt = C.dt and A.',perturbX,' = C.',perturbX,' and A.Water_Year = C.Water_Year
        join (SELECT Year as Water_Year, dt,  ',perturbX,',
            avg(`C_SACSJR__FLOW-CHANNEL`) AS C_SACSJR_winter
          FROM sim_calliteDV_',runname,'
          WHERE Month IN (12,1,2)
          GROUP BY Year, dt, ',perturbX,') D on A.dt = D.dt and A.',perturbX,' = D.',perturbX,' and A.Water_Year = D.Water_Year
        join (SELECT Year as Water_Year, dt,  ',perturbX,',
            avg(`C_SACSJR__FLOW-CHANNEL`) AS C_SACSJR_spring
          FROM sim_calliteDV_',runname,'
          WHERE Month IN (3,4,5)
          GROUP BY Year, dt, ',perturbX,') E on A.dt = E.dt and A.',perturbX,' = E.',perturbX,' and A.Water_Year = E.Water_Year
        join (SELECT Year as Water_Year, dt,  ',perturbX,',
            avg(`C_SACSJR__FLOW-CHANNEL`) AS C_SACSJR_summer
          FROM sim_calliteDV_',runname,'
          WHERE Month IN (6,7,8)
          GROUP BY Year, dt, ',perturbX,') F on A.dt = F.dt and A.',perturbX,' = F.',perturbX,' and A.Water_Year = F.Water_Year
        join (SELECT Year as Water_Year, dt,  ',perturbX,',
            avg(`C_SACSJR__FLOW-CHANNEL`) AS C_SACSJR_fall
          FROM sim_calliteDV_',runname,'
          WHERE Month IN (9,10,11)
          GROUP BY Year, dt, ',perturbX,') G on A.dt = G.dt and A.',perturbX,' = G.',perturbX,' and A.Water_Year = G.Water_Year')
  dbExecute(dbCon,sql)

}
