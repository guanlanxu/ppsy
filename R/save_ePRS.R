#' Save score tables to local drive.
#'
#' @param admin String. Administration.
#' @param mock T or F.
#' @param RSnum String. RS1-3.
#' @param testcode_list A vector of testcodes.
#' @param mode_list A vector of mode.
#' @param battery_list A vector of 1s or 0s.
#' @param pre_post String.pre or post.

#' @return Files will be written to the local folder. No objects will be returned.


save_ePRS <- function(admin, mock,RSnum,testcode_list, mode_list, battery_list, pre_post){

  mode_list = tolower(mode_list)
  testcode_list = tolower(testcode_list)

  E_battery_idx = which(battery_list == '1' & mode_list == "e")
  E_nonbattery_idx = which(battery_list == '0' & mode_list == "e")
  P_nonbattery_idx = which(battery_list == '0' & mode_list == "p")

  mypath = get_path(admin,mock=mock, RSnum = RSnum,testcode = testcode_list[[1]],mode = mode_list[[1]], pre_post = pre_post)

  ## EPRS tables
  library(openxlsx)

  # create tables
  if (length(E_battery_idx) >0){
    wb_pl4b_E_battery = createWorkbook()
    wb_semb_E_battery = createWorkbook()
    wb_ssb_E_battery = createWorkbook()

    addWorksheet(wb_pl4b_E_battery, "pl4b")
    addWorksheet(wb_semb_E_battery, "semb")
    addWorksheet(wb_ssb_E_battery, "ssb")

    # add headings
    heading_pl4b_E_battery = get_eprs_heading(admin = admin, what = "pl4b", 1)
    heading_semb_E_battery = get_eprs_heading(admin = admin, what = "semb", 1)
    heading_ssb_E_battery = get_eprs_heading(admin = admin, what = "ssb", 1)

    writeData(wb_pl4b_E_battery,"pl4b",x = heading_pl4b_E_battery,colNames = F,sep = ", ")
    writeData(wb_semb_E_battery,"ssb",x = heading_semb_E_battery,colNames = F,sep = ", ")
    writeData(wb_ssb_E_battery,"semb",x = heading_ssb_E_battery,colNames = F,sep = ", ")

    startRow_pl4b_E_battery = nrow(heading_pl4b_E_battery)+1
    startRow_ssb_E_battery = nrow(heading_ssb_E_battery)+1
    startRow_semb_E_battery = nrow(heading_semb_E_battery)+1


    for (t in 1: length(E_battery_idx)){
      pl4b = get_eprs(admin =admin,
                      testcode =testcode_list[E_battery_idx[t]],
                      mode = mode_list[E_battery_idx[t]],
                      battery = battery_list[E_battery_idx[t]],
                      RSnum = RSnum,
                      mock = mock,
                      pre_post = pre_post,
                      what = "pl4b")$pl4b


      ssb =  get_eprs(admin =admin,
                      testcode =testcode_list[E_battery_idx[t]],
                      mode = mode_list[E_battery_idx[t]],
                      battery = battery_list[E_battery_idx[t]],
                      RSnum = RSnum,
                      mock = mock,
                      pre_post = pre_post,
                      what = "ssb")$ssb


      semb = get_eprs(admin =admin,
                      testcode =testcode_list[E_battery_idx[t]],
                      mode = mode_list[E_battery_idx[t]],
                      battery = battery_list[E_battery_idx[t]],
                      RSnum = RSnum,
                      mock = mock,
                      pre_post = pre_post,
                      what = "semb")$semb

      writeData(wb_pl4b_E_battery,"pl4b",x = pl4b,colNames = F,sep = ", ",startRow = startRow_pl4b_E_battery, startCol = 2)
      writeData(wb_ssb_E_battery,"ssb",x = ssb,colNames = F,sep = ", ",startRow = startRow_ssb_E_battery, startCol = 2)
      writeData(wb_semb_E_battery,"semb",x = semb,colNames = F,sep = ", ",startRow = startRow_semb_E_battery, startCol = 2)

      startRow_pl4b_E_battery = startRow_pl4b_E_battery + nrow(pl4b)
      startRow_ssb_E_battery = startRow_ssb_E_battery + nrow(ssb)
      startRow_semb_E_battery = startRow_semb_E_battery + nrow(semb)
    }

    saveWorkbook(wb_ssb_E_battery, mypath$ssb_e_battery_path, overwrite = TRUE)
    saveWorkbook(wb_semb_E_battery, mypath$semb_e_battery_path, overwrite = TRUE)
    saveWorkbook(wb_pl4b_E_battery, mypath$pl4b_e_battery_path, overwrite = TRUE)
  }

  if (length(E_nonbattery_idx) >0){
    wb_pl4b_E_nonbattery = createWorkbook()
    wb_semb_E_nonbattery = createWorkbook()
    wb_ssb_E_nonbattery = createWorkbook()

    addWorksheet(wb_pl4b_E_nonbattery, "pl4b")
    addWorksheet(wb_semb_E_nonbattery, "semb")
    addWorksheet(wb_ssb_E_nonbattery, "ssb")

    # add headings
    heading_pl4b_E_nonbattery = get_eprs_heading(admin = admin, what = "pl4b", 0)
    heading_semb_E_nonbattery = get_eprs_heading(admin = admin, what = "semb", 0)
    heading_ssb_E_nonbattery = get_eprs_heading(admin = admin, what = "ssb", 0)

    writeData(wb_pl4b_E_nonbattery,"pl4b",x = heading_pl4b_E_nonbattery,colNames = F,sep = ", ")
    writeData(wb_semb_E_nonbattery,"ssb",x = heading_semb_E_nonbattery,colNames = F,sep = ", ")
    writeData(wb_ssb_E_nonbattery,"semb",x = heading_ssb_E_nonbattery,colNames = F,sep = ", ")

    startRow_pl4b_E_nonbattery = nrow(heading_pl4b_E_nonbattery)+1
    startRow_ssb_E_nonbattery = nrow(heading_ssb_E_nonbattery)+1
    startRow_semb_E_nonbattery = nrow(heading_semb_E_nonbattery)+1

    for (t in 1: length(E_nonbattery_idx)){
      pl4b = get_eprs(admin =admin,
                      testcode =testcode_list[E_nonbattery_idx[t]],
                      mode = mode_list[E_nonbattery_idx[t]],
                      battery = battery_list[E_nonbattery_idx[t]],
                      RSnum = RSnum,
                      mock = mock,
                      pre_post = pre_post,
                      what = "pl4b")$pl4b


      ssb =  get_eprs(admin =admin,
                      testcode =testcode_list[E_nonbattery_idx[t]],
                      mode = mode_list[E_nonbattery_idx[t]],
                      battery = battery_list[E_nonbattery_idx[t]],
                      RSnum = RSnum,
                      mock = mock,
                      pre_post = pre_post,
                      what = "ssb")$ssb


      semb = get_eprs(admin =admin,
                      testcode =testcode_list[E_nonbattery_idx[t]],
                      mode = mode_list[E_nonbattery_idx[t]],
                      battery = battery_list[E_nonbattery_idx[t]],
                      RSnum = RSnum,
                      mock = mock,
                      pre_post = pre_post,
                      what = "semb")$semb

      writeData(wb_pl4b_E_nonbattery,"pl4b",x = pl4b,colNames = F,sep = ", ",startRow = startRow_pl4b_E_nonbattery, startCol = 2)
      writeData(wb_ssb_E_nonbattery,"ssb",x = ssb,colNames = F,sep = ", ",startRow = startRow_ssb_E_nonbattery, startCol = 2)
      writeData(wb_semb_E_nonbattery,"semb",x = semb,colNames = F,sep = ", ",startRow = startRow_semb_E_nonbattery, startCol = 2)

      startRow_pl4b_E_nonbattery = startRow_pl4b_E_nonbattery + nrow(pl4b)
      startRow_ssb_E_nonbattery = startRow_ssb_E_nonbattery + nrow(ssb)
      startRow_semb_E_nonbattery = startRow_semb_E_nonbattery + nrow(semb)
    }

    saveWorkbook(wb_ssb_E_nonbattery, mypath$ssb_e_Nbattery_path, overwrite = TRUE)
    saveWorkbook(wb_semb_E_nonbattery, mypath$semb_e_Nbattery_path, overwrite = TRUE)
    saveWorkbook(wb_pl4b_E_nonbattery, mypath$pl4b_e_Nbattery_path, overwrite = TRUE)


  }

  if (length(P_nonbattery_idx) >0){
    wb_pl4b_P_nonbattery = createWorkbook()
    wb_semb_P_nonbattery = createWorkbook()
    wb_ssb_P_nonbattery = createWorkbook()

    addWorksheet(wb_pl4b_P_nonbattery, "pl4b")
    addWorksheet(wb_semb_P_nonbattery, "semb")
    addWorksheet(wb_ssb_P_nonbattery, "ssb")

    # add headings
    heading_pl4b_P_nonbattery = get_eprs_heading(admin = admin, what = "pl4b", 0)
    heading_semb_P_nonbattery = get_eprs_heading(admin = admin, what = "semb", 0)
    heading_ssb_P_nonbattery = get_eprs_heading(admin = admin, what = "ssb", 0)

    writeData(wb_pl4b_P_nonbattery,"pl4b",x = heading_pl4b_P_nonbattery,colNames = F,sep = ", ")
    writeData(wb_semb_P_nonbattery,"ssb",x = heading_semb_P_nonbattery,colNames = F,sep = ", ")
    writeData(wb_ssb_P_nonbattery,"semb",x = heading_ssb_P_nonbattery,colNames = F,sep = ", ")

    startRow_pl4b_P_nonbattery = nrow(heading_pl4b_P_nonbattery)+1
    startRow_ssb_P_nonbattery = nrow(heading_ssb_P_nonbattery)+1
    startRow_semb_P_nonbattery = nrow(heading_semb_P_nonbattery)+1


    for (t in 1: length(P_nonbattery_idx)){
      pl4b = get_eprs(admin =admin,
                      testcode =testcode_list[P_nonbattery_idx[t]],
                      mode = mode_list[P_nonbattery_idx[t]],
                      battery = battery_list[P_nonbattery_idx[t]],
                      RSnum = RSnum,
                      mock = mock,
                      pre_post = pre_post,
                      what = "pl4b")$pl4b


      ssb =  get_eprs(admin =admin,
                      testcode =testcode_list[P_nonbattery_idx[t]],
                      mode = mode_list[P_nonbattery_idx[t]],
                      battery = battery_list[P_nonbattery_idx[t]],
                      RSnum = RSnum,
                      mock = mock,
                      pre_post = pre_post,
                      what = "ssb")$ssb


      semb = get_eprs(admin =admin,
                      testcode =testcode_list[P_nonbattery_idx[t]],
                      mode = mode_list[P_nonbattery_idx[t]],
                      battery = battery_list[P_nonbattery_idx[t]],
                      RSnum = RSnum,
                      mock = mock,
                      pre_post = pre_post,
                      what = "semb")$semb

      writeData(wb_pl4b_P_nonbattery,"pl4b",x = pl4b,colNames = F,sep = ", ",startRow = startRow_pl4b_P_nonbattery, startCol = 2)
      writeData(wb_ssb_P_nonbattery,"ssb",x = ssb,colNames = F,sep = ", ",startRow = startRow_ssb_P_nonbattery, startCol = 2)
      writeData(wb_semb_P_nonbattery,"semb",x = semb,colNames = F,sep = ", ",startRow = startRow_semb_P_nonbattery, startCol = 2)

      startRow_pl4b_P_nonbattery = startRow_pl4b_P_nonbattery + nrow(pl4b)
      startRow_ssb_P_nonbattery = startRow_ssb_P_nonbattery + nrow(ssb)
      startRow_semb_P_nonbattery = startRow_semb_P_nonbattery + nrow(semb)
    }

    saveWorkbook(wb_ssb_P_nonbattery, mypath$ssb_p_path, overwrite = TRUE)
    saveWorkbook(wb_semb_P_nonbattery, mypath$semb_p_path, overwrite = TRUE)
    saveWorkbook(wb_pl4b_P_nonbattery, mypath$pl4b_p_path, overwrite = TRUE)

  }


  if (mypath$test %in% c("EOC","ACH")){

    if (length(E_battery_idx) >0){
      wb_pl3b_E_battery = createWorkbook()
      wb_irtb_E_battery = createWorkbook()

      addWorksheet(wb_pl3b_E_battery, "pl3b")
      addWorksheet(wb_irtb_E_battery, "irtb")

      # add headings
      heading_pl3b_E_battery = get_eprs_heading(admin = admin, what = "pl3b", 1)
      heading_irtb_E_battery = get_eprs_heading(admin = admin, what = "irtb", 1)

      writeData(wb_pl3b_E_battery,"pl3b",x = heading_pl3b_E_battery,colNames = F,sep = ", ")
      writeData(wb_irtb_E_battery,"irtb",x = heading_irtb_E_battery,colNames = F,sep = ", ")

      startRow_pl3b_E_battery = nrow(heading_pl3b_E_battery)+1
      startRow_irtb_E_battery = nrow(heading_irtb_E_battery)+1


      for (t in 1: length(E_battery_idx)){
        pl3b = get_eprs(admin =admin,
                        testcode =testcode_list[E_battery_idx[t]],
                        mode = mode_list[E_battery_idx[t]],
                        battery = battery_list[E_battery_idx[t]],
                        RSnum = RSnum,
                        mock = mock,
                        pre_post = pre_post,
                        what = "pl3b")$pl3b


        irtb =  get_eprs(admin =admin,
                        testcode =testcode_list[E_battery_idx[t]],
                        mode = mode_list[E_battery_idx[t]],
                        battery = battery_list[E_battery_idx[t]],
                        RSnum = RSnum,
                        mock = mock,
                        pre_post = pre_post,
                        what = "irtb")$irtb


        writeData(wb_pl3b_E_battery,"pl3b",x = pl3b,colNames = F,sep = ", ",startRow = startRow_pl3b_E_battery, startCol = 2)
        writeData(wb_irtb_E_battery,"irtb",x = irtb,colNames = F,sep = ", ",startRow = startRow_irtb_E_battery, startCol = 2)

        startRow_pl3b_E_battery = startRow_pl3b_E_battery + nrow(pl3b)
        startRow_irtb_E_battery = startRow_irtb_E_battery + nrow(irtb)
      }

      saveWorkbook(wb_irtb_E_battery, mypath$irtb_e_battery_path, overwrite = TRUE)
      saveWorkbook(wb_pl3b_E_battery, mypath$pl3b_e_battery_path, overwrite = TRUE)
    }

    if (length(E_nonbattery_idx) >0){
      wb_pl3b_E_nonbattery = createWorkbook()
      wb_irtb_E_nonbattery = createWorkbook()

      addWorksheet(wb_pl3b_E_nonbattery, "pl3b")
      addWorksheet(wb_irtb_E_nonbattery, "irtb")

      # add headings
      heading_pl3b_E_nonbattery = get_eprs_heading(admin = admin, what = "pl3b", 0)
      heading_irtb_E_nonbattery = get_eprs_heading(admin = admin, what = "irtb", 0)

      writeData(wb_pl3b_E_nonbattery,"pl3b",x = heading_pl3b_E_nonbattery,colNames = F,sep = ", ")
      writeData(wb_irtb_E_nonbattery,"irtb",x = heading_irtb_E_nonbattery,colNames = F,sep = ", ")

      startRow_pl3b_E_nonbattery = nrow(heading_pl3b_E_nonbattery)+1
      startRow_irtb_E_nonbattery = nrow(heading_irtb_E_nonbattery)+1


      for (t in 1: length(E_nonbattery_idx)){
        pl3b = get_eprs(admin =admin,
                        testcode =testcode_list[E_nonbattery_idx[t]],
                        mode = mode_list[E_nonbattery_idx[t]],
                        battery = battery_list[E_nonbattery_idx[t]],
                        RSnum = RSnum,
                        mock = mock,
                        pre_post = pre_post,
                        what = "pl3b")$pl3b


        irtb =  get_eprs(admin =admin,
                         testcode =testcode_list[E_nonbattery_idx[t]],
                         mode = mode_list[E_nonbattery_idx[t]],
                         battery = battery_list[E_nonbattery_idx[t]],
                         RSnum = RSnum,
                         mock = mock,
                         pre_post = pre_post,
                         what = "irtb")$irtb


        writeData(wb_pl3b_E_nonbattery,"pl3b",x = pl3b,colNames = F,sep = ", ",startRow = startRow_pl3b_E_nonbattery, startCol = 2)
        writeData(wb_irtb_E_nonbattery,"irtb",x = irtb,colNames = F,sep = ", ",startRow = startRow_irtb_E_nonbattery, startCol = 2)

        startRow_pl3b_E_nonbattery = startRow_pl3b_E_nonbattery + nrow(pl3b)
        startRow_irtb_E_nonbattery = startRow_irtb_E_nonbattery + nrow(irtb)
      }

      saveWorkbook(wb_irtb_E_nonbattery, mypath$irtb_e_Nbattery_path, overwrite = TRUE)
      saveWorkbook(wb_pl3b_E_nonbattery, mypath$pl3b_e_Nbattery_path, overwrite = TRUE)


    }

    if (length(P_nonbattery_idx) >0){
      wb_pl3b_P_nonbattery = createWorkbook()
      wb_irtb_P_nonbattery = createWorkbook()

      addWorksheet(wb_pl3b_P_nonbattery, "pl3b")
      addWorksheet(wb_irtb_P_nonbattery, "irtb")

      # add headings
      heading_pl3b_P_nonbattery = get_eprs_heading(admin = admin, what = "pl3b", 0)
      heading_irtb_P_nonbattery = get_eprs_heading(admin = admin, what = "irtb", 0)

      writeData(wb_pl3b_P_nonbattery,"pl3b",x = heading_pl3b_P_nonbattery,colNames = F,sep = ", ")
      writeData(wb_irtb_P_nonbattery,"irtb",x = heading_irtb_P_nonbattery,colNames = F,sep = ", ")

      startRow_pl3b_P_nonbattery = nrow(heading_pl3b_P_nonbattery)+1
      startRow_irtb_P_nonbattery = nrow(heading_irtb_P_nonbattery)+1


      for (t in 1: length(E_nonbattery_idx)){
        pl3b = get_eprs(admin =admin,
                        testcode =testcode_list[P_nonbattery_idx[t]],
                        mode = mode_list[P_nonbattery_idx[t]],
                        battery = battery_list[P_nonbattery_idx[t]],
                        RSnum = RSnum,
                        mock = mock,
                        pre_post = pre_post,
                        what = "pl3b")$pl3b


        irtb =  get_eprs(admin =admin,
                         testcode =testcode_list[P_nonbattery_idx[t]],
                         mode = mode_list[P_nonbattery_idx[t]],
                         battery = battery_list[P_nonbattery_idx[t]],
                         RSnum = RSnum,
                         mock = mock,
                         pre_post = pre_post,
                         what = "irtb")$irtb


        writeData(wb_pl3b_P_nonbattery,"pl3b",x = pl3b,colNames = F,sep = ", ",startRow = startRow_pl3b_P_nonbattery, startCol = 2)
        writeData(wb_irtb_P_nonbattery,"irtb",x = irtb,colNames = F,sep = ", ",startRow = startRow_irtb_P_nonbattery, startCol = 2)

        startRow_pl3b_P_nonbattery = startRow_pl3b_P_nonbattery + nrow(pl3b)
        startRow_irtb_P_nonbattery = startRow_irtb_P_nonbattery + nrow(irtb)
      }

      saveWorkbook(wb_irtb_P_nonbattery, mypath$irtb_p_path, overwrite = TRUE)
      saveWorkbook(wb_pl3b_P_nonbattery, mypath$pl3b_p_path, overwrite = TRUE)

    }

    heading_ab = ppsy::get_eprs_heading(admin = admin, what = "aberrant", 0)
    write.table(heading_ab, mypath$aberrant_path,append = F, sep = ",",col.names =  F,row.names = F,quote = F,na = "")

    for (i in 1:length(testcode_list)){
      aberrant = get_eprs(admin = "tneocspr23", testcode = testcode_list[i],mode = mode_list[i],
                          status = "OP",
                          battery = battery_list[i] ,
                          RSnum = "RS2",mock = mock,
                          pre_post = pre_post,
                          what = "aberrant")$aberrant

      write.table(aberrant, mypath$aberrant_path,append = T, sep = ",",col.names =  F,row.names = F,quote = F,na = "")
    }



  }



}
