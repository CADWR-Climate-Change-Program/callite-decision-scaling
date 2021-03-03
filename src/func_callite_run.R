slrGUI <- function(SLR_Type, ANN_set, pt_run_folder, dir_newtbls, dir_callite_tbls) {
  
  SLRtable <- readLines(file.path(dir_callite_tbls, "GUI_HydroClimate.table"))

  if (ANN_set=='x64') {
  
    SLRlimits <- data.frame(
      "Type" = c("0SLR", "15SLR", "30SLR", "45SLR", "60SLR", rep("BaseCase", 9)),
      "T_lev" = c(NA, NA, NA, NA, NA, "0_0", "0_5", "1_0", "1_5", "2_0","2_5", "3_0", "3_5", "4_0"),
      "SLRamt" = c(0, 1, 2, 3, 4, 0, 1, 2, 3, 4, 4, 4, 4, 4))
  
    SLR <- subset(SLRlimits, Type == SLR_Type & (T_lev == substring(pt_run_folder, 6, 8) | is.na(T_lev)))
  
    SLRtable[11] <- paste0("3\t", SLR$SLRamt,
                           paste("\t!Simulated SLR level",
                                 "0=Base (+0 C),",
                                 "1=15cm (+0.5 C),",
                                 "2=30cm (+1.0 C),",
                                 "3=45cm (+1.5 C),",
                                 "4=60cm (+2.0 C) "))
    
  } else if (ANN_set=='x32'){
    
    SLRlimits <- data.frame(
      "Type" = c("0SLR", "15SLR", "45SLR", rep("BaseCase", 9)),
      "T_lev" = c(NA, NA, NA, "0_0", "0_5", "1_0", "1_5", "2_0","2_5", "3_0", "3_5", "4_0"), 
      "SLRamt" = c(0, 1, 2, 0, 1, 1, 2, 2, 2, 2, 2, 2))
    
    SLR <- subset(SLRlimits, Type == SLR_Type & (T_lev == substring(pt_run_folder, 6, 8) | is.na(T_lev)))
    
    SLRtable[11] <- paste0("3\t", SLR$SLRamt,
                           paste("\t!Simulated SLR level",
                                 "0= Base (0 degree Temp increase),",
                                 "1= 15cm (0.5-1.0 degree Temp increase),",
                                 "2= 45cm (1.5 and above Temp increase) "))
  }

  writeLines(SLRtable, con = file.path(dir_newtbls, paste0("GUI_HydroClimate_", SLR_Type, ".table")))

  file.copy(file.path(dir_newtbls, paste0("GUI_HydroClimate_", SLR_Type, ".table")),
            file.path(dir_callite_tbls, "GUI_HydroClimate.table"),
            overwrite = TRUE)
}

calliteSVwriteDSS <- function(dir_newdss, run, java_py_dir, default_input_dir, txt_files) {

    # grab empty (default) state variable DSS file
    if(run==1) {
    file.copy(file.path(default_input_dir, "Blank_SV.dss"),
              file.path(dir_newdss, "Output_SV.dss"),
              overwrite = TRUE )
    }

    for (j in 1:8) {
      # copy text DSS vars into temp data text file
      file.copy(file.path(dir_newdss, paste0("data_", run , txt_files[j])),
                file.path(dir_newdss, "data.txt"),
                overwrite = TRUE)

      # insert data.tct to DSS file with python script call
      system(paste(file.path(java_py_dir,"vscript"),
                   file.path(java_py_dir, "dss_from_text_SV.py"),
                   file.path(dir_newdss, "data.txt"),file.path(dir_newdss, "Output_SV.dss")),
             invisible = FALSE, wait = TRUE)
    }

    # copy new dss to run files
    file.copy(file.path(dir_newdss, "Output_SV.dss"),
              file.path(dir_newdss, paste0("Output_SV_", run, ".dss")),
              overwrite = TRUE)

}

calliteLookupTblsWrite <- function(lookup_tbls, dir_newtbls, run, dir_callite_tbls) {
  for (j in 1:length(lookup_tbls)) {
    file.copy(file.path(dir_newtbls, paste0(lookup_tbls[j], "_", run, ".table")),
              file.path(dir_callite_tbls, paste0(lookup_tbls[j], ".table")),
              overwrite = TRUE)
  }
}

calliteForecastTblsWrite <- function(forecast_tbls, dir_newForetbls, run, dir_callite_tbls) {
  for (j in 1:length(forecast_tbls)) {
    file.copy(file.path(dir_newForetbls, paste0(forecast_tbls[j], "_", run, ".table")),
              file.path(dir_callite_tbls, paste0(forecast_tbls[j], ".table")),
              overwrite = TRUE)
  }
}

calliteINITwriteDSS <- function(run, default_input_dir, dir_callite_SV, java_py_dir,
  calLite_input_dir, dir_newdss, last_cent_only) {
  if (run == 1 | (run == 21 & last_cent_only == TRUE)) {

    file.copy(file.path(default_input_dir, "CL_INIT.dss"),
              file.path(dir_newdss, paste0("CL_INIT_", run, ".dss")),
              overwrite = TRUE)

    file.copy(file.path(default_input_dir, "CL_INIT.dss"),
              file.path(dir_callite_SV, "CL_INIT.dss"),
              overwrite = TRUE)
    print('Prime Initial Conditions DSS Created')

  } else{

    system(paste(file.path(java_py_dir,"vscript"),
                 file.path(java_py_dir,"dss_to_text_DV_to_INIT.py"),
                 file.path(calLite_input_dir, "data.txt"),
                 file.path(dir_newdss,paste0("Output_DV_",run - 1,".dss"))),
           invisible = TRUE, wait = TRUE)

    checkinit <- read.csv(file.path(calLite_input_dir, "data.txt"))

    if (checkinit[1, 46] != 0) {

      temptxt <- readLines(con = file.path(calLite_input_dir, "data.txt"))
      temptxt[4] <- sub("value,", x = temptxt[2], "")
      temptxt[2] <- sub("pathname,", x = temptxt[1], "")
      temptxt[2] <- gsub("2020D09E", x = temptxt[2], "INITIAL")
      temptxt[1] <- paste((rep("30SEP1921 2400,", 56)), collapse = " ")

      writeLines(temptxt, con = file.path(calLite_input_dir, "data.txt"))

      file.copy(file.path(default_input_dir, "CL_INIT.dss"),
                file.path(dir_newdss, paste0("CL_INIT_", run, ".dss")),
                overwrite = TRUE)

      system(paste(file.path(java_py_dir, "vscript"),
                   file.path(java_py_dir, "dss_from_text_INIT.py"),
                   file.path(calLite_input_dir, "data.txt"),
                   file.path(dir_newdss,paste0("CL_INIT_",run,".dss"))),
             invisible = TRUE, wait = TRUE)

      file.copy(file.path(dir_newdss,paste0("CL_INIT_",run,".dss")),
                file.path(dir_callite_SV, "CL_INIT.dss"),
                overwrite = T)

      print(paste('Initial Conditions DSS Created for Run',run))

    } else {

      file.copy(file.path(default_input_dir, "CL_INIT.dss"),
                file.path(dir_callite_SV, "CL_INIT.dss"),
                overwrite = T)

      file.copy(file.path(default_input_dir, "CL_INIT.dss"),
                file.path(dir_newdss, paste0("CL_INIT_", run, ".dss")),
                overwrite = TRUE)

      print(paste('Prime Initial Conditions DSS Created for Run',run))

    }
  }
}