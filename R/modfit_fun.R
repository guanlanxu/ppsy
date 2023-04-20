#' Compute Q1 and G2 for model fit.
#'
#' @param admin String.
#' @param testcode String.
#' @param mode P or E.
#' @param status OP or FT.
#' @param RSnum RS1-3.
#' @param mock T or F.
#' @return fit_tab: A table with fit statistics
#' @return fit_plot: A list of fit plots.
#' @import haven  data.table    dplyr   foreign
#' @export


# modfit_fun(admin, testcode, mode,status,RSnum, mock)

modfit_fun <- function(admin,testcode,  mode,status,RSnum, mock) {
  library(haven)
  library(dplyr)
  library(data.table)
  library(ppsy)
  library(foreign)


  mypath = get_path(admin = admin, testcode = testcode, mode = mode, status = status, RSnum = RSnum, mock = mock)
  test = mypath$test
  IRTpro_theta = read.delim(paste0(mypath$out_path,".--sco.txt"),sep = "",header = F)
  IRTpro_theta = as.data.frame(IRTpro_theta)
  colnames(IRTpro_theta) = c("check","id","ncat","theta","theta_se")

  freeirt_full = read.csv(mypath$freeirt_path_csv)
  freeirt_full$maxslots = ifelse(freeirt_full$IRTLOC > 0, freeirt_full$IRTLOC, 0)
  freeirt = freeirt_full[which(freeirt_full$EXCLUDED_FROM_IRT_ANALYSIS != "Y" | is.na(freeirt_full$EXCLUDED_FROM_IRT_ANALYSIS)),]
  nuitems = nrow(freeirt[which(freeirt$maxslots !=0 ),])


  IDM = read.csv(mypath$idm_original_path)
  IDM[is.na(IDM)] = -1
  #IDM_items = IDM_theta[,paste0("score_",freeirt$IRTLOC)]
  set.seed(123)
  IDM$rnd = runif(nrow(IDM),0,1)
  IDM$id = as.numeric(rownames(IDM))
  # merge theta into IDM by ID
  IDM_theta = left_join(IDM,IRTpro_theta, by = "id")
  IDM_theta = IDM_theta[order(IDM_theta$theta,IDM_theta$rnd),]
  DEC = round_sas(seq(1:9)*nrow(IDM)/10,0)
  IDM_theta$rowname = seq(1:nrow(IDM_theta))
  IDM_theta[which(IDM_theta$rowname <= DEC[1]),"DECILE"] = 1
  IDM_theta[which(IDM_theta$rowname> DEC[1] & IDM_theta$rowname<= DEC[2]),"DECILE"] = 2
  IDM_theta[which(IDM_theta$rowname > DEC[2] & IDM_theta$rowname <= DEC[3]),"DECILE"] = 3
  IDM_theta[which(IDM_theta$rowname > DEC[3] & IDM_theta$rowname <= DEC[4]),"DECILE"] = 4
  IDM_theta[which(IDM_theta$rowname > DEC[4] & IDM_theta$rowname <= DEC[5]),"DECILE"] = 5
  IDM_theta[which(IDM_theta$rowname > DEC[5] & IDM_theta$rowname <= DEC[6]),"DECILE"] = 6
  IDM_theta[which(IDM_theta$rowname > DEC[6] & IDM_theta$rowname <= DEC[7]),"DECILE"] = 7
  IDM_theta[which(IDM_theta$rowname > DEC[7] & IDM_theta$rowname <= DEC[8]),"DECILE"] = 8
  IDM_theta[which(IDM_theta$rowname > DEC[8] & IDM_theta$rowname <= DEC[9]),"DECILE"] = 9
  IDM_theta[which(IDM_theta$rowname > DEC[9]),"DECILE"] = 10

  IDM_items = IDM_theta[,paste0("score_",freeirt$IRTLOC)]
  N_ij = table(IDM_theta$DECILE)

  # compute OBSected probability

  theta.tab = data.frame(matrix(rep(IDM_theta$theta, nrow(freeirt)), nrow = nrow(freeirt), byrow = T))
  IDM_itemsT = t(IDM_items)
  theta.tab[IDM_itemsT == -1] <- NA

  freeirt_theta = data.frame(cbind(freeirt[,c("MODEL","UN_IRT_A","UN_IRT_B","UN_IRT_C",paste0("UN_IRT_STEP",1:5))], theta.tab))


  ppp = apply(freeirt_theta,1,function (x) irt_fun(model = x[1],theta.vec = as.numeric(x[10:(length(IDM_theta$theta)+9)]),
                                                   D = 1.702, a = x[2], b = x[3], c = x[4],
                                                   steps = x[5:9]))


  pi.exp = sapply(ppp, function(x) x$p)
  pi0.exp = sapply(ppp, function(x) x$pi$p0)
  pi1.exp = sapply(ppp, function(x) x$pi$p1)
  pi2.exp = sapply(ppp, function(x) x$pi$p2)
  pi3.exp = sapply(ppp, function(x) x$pi$p3)
  pi4.exp = sapply(ppp, function(x) x$pi$p4)
  pi5.exp = sapply(ppp, function(x) x$pi$p5)

  remove(ppp)

  pi.exp = as.data.frame(pi.exp)
  pi0.exp = as.data.frame(pi0.exp)
  pi1.exp = as.data.frame(pi1.exp)
  pi2.exp = as.data.frame(pi2.exp)
  pi3.exp = as.data.frame(pi3.exp)
  pi4.exp = as.data.frame(pi4.exp)
  pi5.exp = as.data.frame(pi5.exp)



  # pi0.OBS = apply(freeirt_theta,1,function (x) irt_fun(model = x[1],theta.vec = as.numeric(x[10:28408]),
  #                                                      D = 1.702, a = x[2], b = x[3], c = x[4],
  #                                                      steps = x[5:9])$pi$p0)
  # pi1.OBS = apply(freeirt_theta,1,function (x) irt_fun(model = x[1],theta.vec = as.numeric(x[10:28408]),
  #                                                      D = 1.702, a = x[2], b = x[3], c = x[4],
  #                                                      steps = x[5:9])$pi$p1)



  if (max(freeirt$MAX_POINTS)==2 & ncol(pi3.exp) ==1 & ncol(pi4.exp) ==1 & ncol(pi5.exp) ==1 ){
    pi3.exp = as.data.frame(t(pi3.exp)[rep(seq_len(nrow(t(pi3.exp))), each = nrow(IDM_items)), ])
    pi4.exp = as.data.frame(t(pi4.exp)[rep(seq_len(nrow(t(pi4.exp))), each = nrow(IDM_items)), ])
    pi5.exp = as.data.frame(t(pi5.exp)[rep(seq_len(nrow(t(pi5.exp))), each = nrow(IDM_items)), ])

  } else if (max(freeirt$MAX_POINTS)==3 & ncol(pi4.exp) ==1 & ncol(pi5.exp) ==1 ){
    pi4.exp = as.data.frame(t(pi4.exp)[rep(seq_len(nrow(t(pi4.exp))), each = nrow(IDM_items)), ])
    pi5.exp = as.data.frame(t(pi5.exp)[rep(seq_len(nrow(t(pi5.exp))), each = nrow(IDM_items)), ])
  } else if (max(freeirt$MAX_POINTS)==4 & ncol(pi5.exp) ==1){
    pi5.exp = as.data.frame(t(pi5.exp)[rep(seq_len(nrow(t(pi5.exp))), each = nrow(IDM_items)), ])
  } else  if (max(freeirt$MAX_POINTS)==1 & ncol(pi2.exp) ==1 & ncol(pi3.exp) ==1 & ncol(pi4.exp) ==1 & ncol(pi5.exp) ==1){

    pi2.exp = as.data.frame(t(pi2.exp)[rep(seq_len(nrow(t(pi2.exp))), each = nrow(IDM_items)), ])
    pi3.exp = as.data.frame(t(pi3.exp)[rep(seq_len(nrow(t(pi3.exp))), each = nrow(IDM_items)), ])
    pi4.exp = as.data.frame(t(pi4.exp)[rep(seq_len(nrow(t(pi4.exp))), each = nrow(IDM_items)), ])
    pi5.exp = as.data.frame(t(pi5.exp)[rep(seq_len(nrow(t(pi5.exp))), each = nrow(IDM_items)), ])
  }


  pi0.exp[is.na(pi0.exp)] = 0
  pi1.exp[is.na(pi1.exp)] = 0
  pi2.exp[is.na(pi2.exp)] = 0
  pi3.exp[is.na(pi3.exp)] = 0
  pi4.exp[is.na(pi4.exp)] = 0
  pi5.exp[is.na(pi5.exp)] = 0

  pi0_sum = apply(pi0.exp,2, function (x) aggregate(x, by=list(DECILE=IDM_theta$DECILE), FUN=sum)$x)
  pi1_sum = apply(pi1.exp,2, function (x) aggregate(x, by=list(DECILE=IDM_theta$DECILE), FUN=sum)$x)
  pi2_sum = apply(pi2.exp,2, function (x) aggregate(x, by=list(DECILE=IDM_theta$DECILE), FUN=sum)$x)
  pi3_sum = apply(pi3.exp,2, function (x) aggregate(x, by=list(DECILE=IDM_theta$DECILE), FUN=sum)$x)
  pi4_sum = apply(pi4.exp,2, function (x) aggregate(x, by=list(DECILE=IDM_theta$DECILE), FUN=sum)$x)
  pi5_sum = apply(pi5.exp,2, function (x) aggregate(x, by=list(DECILE=IDM_theta$DECILE), FUN=sum)$x)
  pi0_sum = as.data.frame(pi0_sum)
  pi1_sum = as.data.frame(pi1_sum)
  pi2_sum = as.data.frame(pi2_sum)
  pi3_sum = as.data.frame(pi3_sum)
  pi4_sum = as.data.frame(pi4_sum)
  pi5_sum = as.data.frame(pi5_sum)


  #
  N_ij_FT = as.data.frame(apply(pi.exp, 2, function (x) aggregate(x, by=list(DECILE=IDM_theta$DECILE), FUN=function (y) length(y[which(!is.na(y))]))$x))
#


  exp.pts = colSums(pi.exp,na.rm = T)/freeirt$N_IRT
  IDM_items = IDM_theta[,paste0("score_",freeirt$IRTLOC)]
  IDM_items[IDM_items == -1] <- NA
  OBS.pts = colSums(IDM_items,na.rm = T)/freeirt$N_IRT


  Oi0_sum = as.data.frame(apply(IDM_items,2, function (x) aggregate(x, by=list(DECILE=IDM_theta$DECILE), FUN=function(x) length(x[which(x == 0)]))$x))
  Oi1_sum = as.data.frame(apply(IDM_items,2, function (x) aggregate(x, by=list(DECILE=IDM_theta$DECILE), FUN=function(x) length(x[which(x == 1)]))$x))
  Oi2_sum = as.data.frame(apply(IDM_items,2, function (x) aggregate(x, by=list(DECILE=IDM_theta$DECILE), FUN=function(x) length(x[which(x == 2)]))$x))
  Oi3_sum = as.data.frame(apply(IDM_items,2, function (x) aggregate(x, by=list(DECILE=IDM_theta$DECILE), FUN=function(x) length(x[which(x == 3)]))$x))
  Oi4_sum = as.data.frame(apply(IDM_items,2, function (x) aggregate(x, by=list(DECILE=IDM_theta$DECILE), FUN=function(x) length(x[which(x == 4)]))$x))
  Oi5_sum = as.data.frame(apply(IDM_items,2, function (x) aggregate(x, by=list(DECILE=IDM_theta$DECILE), FUN=function(x) length(x[which(x == 5)]))$x))



  EXP_i0j = pi0_sum/N_ij_FT
  EXP_i1j = pi1_sum/N_ij_FT
  EXP_i2j = pi2_sum/N_ij_FT
  EXP_i3j = pi3_sum/N_ij_FT
  EXP_i4j = pi4_sum/N_ij_FT
  EXP_i5j = pi5_sum/N_ij_FT

  EXP_i0j[EXP_i0j == 1] = 1-1E-10
  EXP_i0j[EXP_i0j == 0] = 1E-10
  EXP_i1j[EXP_i1j == 1] = 1-1E-10
  EXP_i1j[EXP_i1j == 0] = 1E-10
  EXP_i2j[EXP_i2j == 1] = 1-1E-10
  EXP_i2j[EXP_i2j == 0] = 1E-10
  EXP_i3j[EXP_i3j == 1] = 1-1E-10
  EXP_i3j[EXP_i3j == 0] = 1E-10
  EXP_i4j[EXP_i4j == 1] = 1-1E-10
  EXP_i4j[EXP_i4j == 0] = 1E-10
  EXP_i5j[EXP_i5j == 1] = 1-1E-10
  EXP_i5j[EXP_i5j == 0] = 1E-10



  OBS_i0j = Oi0_sum/N_ij_FT
  OBS_i1j = Oi1_sum/N_ij_FT
  OBS_i2j = Oi2_sum/N_ij_FT
  OBS_i3j = Oi3_sum/N_ij_FT
  OBS_i4j = Oi4_sum/N_ij_FT
  OBS_i5j = Oi5_sum/N_ij_FT


  Q1 = colSums((N_ij_FT * (OBS_i0j- EXP_i0j)^2)/EXP_i0j,na.rm = T)+
    colSums((N_ij_FT * (OBS_i1j- EXP_i1j)^2)/EXP_i1j,na.rm = T)+
    colSums((N_ij_FT * (OBS_i2j- EXP_i2j)^2)/EXP_i2j,na.rm = T)+
    colSums((N_ij_FT * (OBS_i3j- EXP_i3j)^2)/EXP_i3j,na.rm = T)+
    colSums((N_ij_FT * (OBS_i4j- EXP_i4j)^2)/EXP_i4j,na.rm = T)+
    colSums((N_ij_FT * (OBS_i5j- EXP_i5j)^2)/EXP_i5j,na.rm = T)


  df_ZQ1=rep(NA,nrow(freeirt))
  df_ZQ1[which(freeirt$MODEL=="GPC")] = 10* freeirt[which(freeirt$MODEL == "GPC"),]$MAX_POINTS - (freeirt[which(freeirt$MODEL == "GPC"),]$MAX_POINTS +1)

  df_ZQ1[which(freeirt$MODEL=="3PL")]=7
  df_ZQ1[which(freeirt$MODEL=="2PL")]=8

  ZQ1 = (Q1-df_ZQ1)/sqrt(2*df_ZQ1)

  ZQ1_cut = ifelse(freeirt$MAX_POINTS ==4, nrow(IDM)*4/1500,freeirt$N_IRT*4/1500 )
  fitrate = ifelse(ZQ1 <=ZQ1_cut,"OK","FL" )
  ZQ1_flag = ifelse(ZQ1 > ZQ1_cut, "Y","N")




  ############################################# G2####################################################################
  ## G2 uses same OBS and OBS but need to merge groups.
  ##############################################################

  ## Logic: compute N_P_ adj for individual items, untill all N_P are >=5
  ##        recompute N_P, i.e., recompute theta.mean vector and feed into the irt functions to get N_P.
  ##        thus, N_ij need to update as well
  ## Variables need to iterate:
  ##        1. N_P_adj;
  ##        2. N_ij;
  ##        3. theta_mean_vec
  ## output: (get the new group assignment and recompute all of following:)
  ##        1. N_P_adj for each item;
  ##        2. N_ij_adj for each item;
  ##        3. OBS_adj for each item;

  G2 = rep(NA, nrow(freeirt))

  #i=12

  # inital N_P_item and assign it to adjust

  for (i in 1: nrow(freeirt)){
    #  for (i in 1: nrow(free.par)){
    # assign theta_avg to theta.vec for irt functions
    theta.vec = tapply(IDM_theta$theta, IDM_theta$DECILE, FUN=mean)
    # theta_avg = theta.vec
    N_ij = N_ij_FT # dim = (10,nuitems)
    IDM_theta$mrg_cat = IDM_theta$DECILE

    N_P_item = G2_int_adj(theta.vec.short = theta.vec, Ncount = N_ij, item = i, freeirt = freeirt)

    #   cond.list = lapply(N_P_item, function (x) (x<5))
    #    cond.short = lapply(cond.list, function (x) any(x))
    #    cond.vec = unlist(cond.short)


    while ( (nrow(N_ij)>1) & any(unlist(lapply(lapply(N_P_item, function (x) (x<5)), function (x) any(x)) ),na.rm = T) ){

      # staring from the first min segment to merge,
      seg.to.merge = min(unique(unlist(lapply(lapply(N_P_item, function (x) (x<5)), function (x) which(x == "TRUE")))))

      # merge min segment to the adjascent segement
      # check which category < 5
      which.cat = (unlist(lapply(lapply(N_P_item, function (x) (x<5)), function(x) which(x[seg.to.merge] == "TRUE"))))[1] # start from the 1st elemtn
      cat.to.merge = as.numeric(substr(names(which.cat),5,5)) + 1

      # if seg.to.merge == 1, merge to 2nd seg
      if (seg.to.merge == 1 ) {
        seg.target =(as.numeric(names(table(IDM_theta$mrg_cat))))[seg.to.merge]
        seg.after.merge = (as.numeric(names(table(IDM_theta$mrg_cat))))[seg.to.merge+1]
        #    IDM_theta$mrg_cat = IDM_theta$DECILE
        IDM_theta[which(IDM_theta$mrg_cat==seg.target),"mrg_cat"] = seg.after.merge
        # update theta_avg_adj and N_ij_adj
        theta_avg = as.numeric(tapply(IDM_theta$theta, IDM_theta$mrg_cat, FUN=mean))
        n_decile = names(table(IDM_theta$mrg_cat))
        N_ij = matrix(NA, nrow = length(n_decile),ncol = nuitems)
        for (n in 1:length(n_decile)) {
          N_ij[n,] = apply(IDM_items[which(IDM_theta$mrg_cat==n_decile[n]),], 2, function (x) {length(x[which(!is.na(x))])})
        }
        theta.vec = theta_avg
        N_P_item = G2_int_adj(theta.vec.short = theta_avg, Ncount = N_ij, item = i, freeirt = freeirt)

      }

      # if seg.to.merge == lastone, merge to the one above
      if (seg.to.merge != 1 & seg.to.merge == length(N_P_item[[cat.to.merge]]) ) {
        seg.target = (as.numeric(names(table(IDM_theta$mrg_cat))))[seg.to.merge]
        seg.after.merge = (as.numeric(names(table(IDM_theta$mrg_cat))))[seg.to.merge-1]
        #    IDM_theta$mrg_cat = IDM_theta$DECILE
        IDM_theta[which(IDM_theta$mrg_cat==seg.target),"mrg_cat"] = seg.after.merge
        # update theta_avg_adj and N_ij_adj
        theta_avg = as.numeric(tapply(IDM_theta$theta, IDM_theta$mrg_cat, FUN=mean))
        n_decile = names(table(IDM_theta$mrg_cat))
        N_ij = matrix(NA, nrow = length(n_decile),ncol = nuitems)
        for (n in 1:length(n_decile)) {
          N_ij[n,] = apply(IDM_items[which(IDM_theta$mrg_cat==n_decile[n]),], 2, function (x) {length(x[which(!is.na(x))])})
        }
        theta.vec = theta_avg
        N_P_item = G2_int_adj(theta.vec.short = theta_avg, Ncount = N_ij, item = i,freeirt = freeirt)
      }

      # if seg.to.merge is in the middle, merge to above or below based on N_P_item value
      if (seg.to.merge > 1 & seg.to.merge < length(N_P_item[[cat.to.merge]])){
        # check N_P_item, which is larger
        N_P_above = N_P_item[[cat.to.merge]][seg.to.merge+1]
        N_P_below = N_P_item[[cat.to.merge]][seg.to.merge-1]
        if (N_P_above >= N_P_below) {
          # merge to above
          seg.target = (as.numeric(names(table(IDM_theta$mrg_cat))))[seg.to.merge]
          seg.after.merge = (as.numeric(names(table(IDM_theta$mrg_cat))))[seg.to.merge+1]
          #    IDM_theta$mrg_cat = IDM_theta$DECILE
          IDM_theta[which(IDM_theta$mrg_cat==seg.target),"mrg_cat"] = seg.after.merge
          # update theta_avg_adj and N_ij_adj
          theta_avg = as.numeric(tapply(IDM_theta$theta, IDM_theta$mrg_cat, FUN=mean))
          n_decile = names(table(IDM_theta$mrg_cat))
          N_ij = matrix(NA, nrow = length(n_decile),ncol = nuitems)
          for (n in 1:length(n_decile)) {
            N_ij[n,] = apply(IDM_items[which(IDM_theta$mrg_cat==n_decile[n]),], 2, function (x) {length(x[which(!is.na(x))])})
          }
          theta.vec = theta_avg
          N_P_item = G2_int_adj(theta.vec.short = theta_avg, Ncount = N_ij, item = i, freeirt = freeirt)


        } else if (N_P_above < N_P_below) {
          # merge to below
          seg.target = (as.numeric(names(table(IDM_theta$mrg_cat))))[seg.to.merge]
          seg.after.merge = (as.numeric(names(table(IDM_theta$mrg_cat))))[seg.to.merge-1]
          #    IDM_theta$mrg_cat = IDM_theta$DECILE
          IDM_theta[which(IDM_theta$mrg_cat==seg.target),"mrg_cat"] = seg.after.merge
          # update theta_avg_adj and N_ij_adj
          theta_avg = as.numeric(tapply(IDM_theta$theta, IDM_theta$mrg_cat, FUN=mean))
          n_decile = names(table(IDM_theta$mrg_cat))
          N_ij = matrix(NA, nrow = length(n_decile),ncol = nuitems)
          for (n in 1:length(n_decile)) {
            N_ij[n,] = apply(IDM_items[which(IDM_theta$mrg_cat==n_decile[n]),], 2, function (x) {length(x[which(!is.na(x))])})
          }
          theta.vec = theta_avg
          N_P_item = G2_int_adj(theta.vec.short = theta_avg, Ncount = N_ij, item = i, freeirt = freeirt)
        }

      }


    }
    # end of the while loop

    # compute the OBS result


    n.seg = length(table(IDM_theta$mrg_cat))
    name.seg = names(table(IDM_theta$mrg_cat))

    OBS_item = list(
      N_P_0 = rep(NA,nrow(N_ij)),
      N_P_1 = rep(NA,nrow(N_ij)),
      N_P_2 = rep(NA,nrow(N_ij)),
      N_P_3 = rep(NA,nrow(N_ij)),
      N_P_4 = rep(NA,nrow(N_ij)),
      N_P_5 = rep(NA,nrow(N_ij)))


    for (j in 1:n.seg){

      OBS_item[[1]][j] = nrow(IDM_items[which(IDM_items[,i] ==0 &IDM_theta$mrg_cat==name.seg[j]),])
      OBS_item[[2]][j] = nrow(IDM_items[which(IDM_items[,i] ==1 &IDM_theta$mrg_cat==name.seg[j]),])
      OBS_item[[3]][j] = nrow(IDM_items[which(IDM_items[,i] ==2 &IDM_theta$mrg_cat==name.seg[j]),])
      OBS_item[[4]][j] = nrow(IDM_items[which(IDM_items[,i] ==3 &IDM_theta$mrg_cat==name.seg[j]),])
      OBS_item[[5]][j] = nrow(IDM_items[which(IDM_items[,i] ==4 &IDM_theta$mrg_cat==name.seg[j]),])
      OBS_item[[6]][j] = nrow(IDM_items[which(IDM_items[,i] ==5 &IDM_theta$mrg_cat==name.seg[j]),])

    }

    # G2
    N_P_rbind = rbind(N_P_item[[1]][1:n.seg],N_P_item[[2]][1:n.seg],N_P_item[[3]][1:n.seg],N_P_item[[4]][1:n.seg],N_P_item[[5]][1:n.seg],N_P_item[[6]][1:n.seg])
    OBS_rbind = rbind(OBS_item[[1]][1:n.seg],OBS_item[[2]][1:n.seg],OBS_item[[3]][1:n.seg],OBS_item[[4]][1:n.seg],OBS_item[[5]][1:n.seg],OBS_item[[6]][1:n.seg])

    N_P_rbind[is.na(N_P_rbind)] = 0

    G2[i] = 2*sum(OBS_rbind * log(OBS_rbind/N_P_rbind), na.rm = T)


  }

  G2[which(G2 == "Inf")] = NA
  g2_c = ifelse(freeirt$ITEM_STATUS == "OP",sqrt(G2/(G2+nrow(IDM))) ,sqrt(G2/(G2+freeirt$N_IRT)))



  g2_fit = ifelse(g2_c < .1, "OK",
                  ifelse(g2_c<0.287, "SMALL",
                         ifelse(g2_c < 0.447, "MEDIUM","LARGE")))
  g2_flag = ifelse(g2_c >=0.35,"Y",'N')

  fitrate = ifelse(ZQ1 <=ZQ1_cut,"OK","FL" )



  modfit = data.frame(UIN = freeirt$UIN,Q1 = Q1,df= df_ZQ1,ZQ1 = ZQ1,ZQ1_cut = ZQ1_cut,fitrate=fitrate,ZQ1_flag = ZQ1_flag,
                      G2=G2,g2_c=g2_c, g2_fit=g2_fit,g2_flag=g2_flag)
  modfit = left_join(freeirt_full,modfit,by = "UIN")

  write.csv(modfit,mypath$irtwq1_path_csv,row.names = F,na='')

  write.foreign(modfit, mypath$irtwq1_path_txt,
                mypath$irtwq1_path_sas, package="SAS")




  #################################################################################################################
  ##  FIT PLOT
  ##############################################################
  p=list(NA)
  for (i in 1:nuitems){
    toplot = data.frame(theta = rep(theta.vec, 12),
                        prob = c(OBS_i0j[,i], OBS_i1j[,i],OBS_i2j[,i],OBS_i3j[,i],OBS_i4j[,i],OBS_i5j[,i],
                                 EXP_i0j[,i], EXP_i1j[,i],EXP_i2j[,i],EXP_i3j[,i],EXP_i4j[,i],EXP_i5j[,i]),
                        score = rep(c(rep(0,length(theta.vec)),rep(1,length(theta.vec)),rep(2,length(theta.vec)),
                                      rep(3,length(theta.vec)),rep(4,length(theta.vec)),rep(5,length(theta.vec))),2),
                        group = c(rep("observed",length(theta.vec) * 6),rep("expected",length(theta.vec) * 6)))


    toplot$score = as.factor(toplot$score)
    toplot$prob[toplot$prob<0.000000001] = NA
    p[[i]] = ggplot(toplot,aes(x = theta, y = prob,colour=group, shape = score))+
      geom_line()+
      geom_text(label = toplot$score,show.legend = F)+
      scale_x_continuous(breaks = round(seq(-2, 2, by = 0.5),2),limits = c(-2,2)) +
      scale_y_continuous(breaks = round(seq(0, 1, by = 0.2),2),limits = c(0,1.03)) +
      labs(title = "Observed vs. Expected Points",
           subtitle = paste0("IRTLOC = ",modfit$IRTLOC[i],
                             "; UIN = ",modfit$UIN[i],  #"; IDMseq = ", mydrift_small$IRTLOC[i],
                             "; A = ", round_sas(modfit$UN_IRT_A[i],5),
                             "; B = ", round_sas(modfit$UN_IRT_B[i],5),
                             "; C = ", round_sas(modfit$UN_IRT_C[i],5)
           ))
    if (ZQ1_flag[i] == "Y"){
      p[[i]] = p[[i]]+geom_hline(aes(yintercept = 1.01, linetype = "ZQ1_FLAG"),color = "blue", linewidth = 2)
    }
    if (g2_flag[i] == "Y"){
      p[[i]] = p[[i]]+geom_hline(aes(yintercept = 1.02, linetype = "G2_FLAG"),color = "yellow", linewidth = 2)
    }
    modfit$SX2_FLAG[is.na(modfit$SX2_FLAG)] = "N"
    if (modfit$SX2_FLAG[i] == "Y"){
      p[[i]] = p[[i]]+geom_hline(aes(yintercept = 1.03, linetype = "SX2_FLAG"),color = "orange", linewidth = 2)
    }

    if (ZQ1_flag[i] == "Y" & g2_flag[i] == "Y" & modfit$SX2_FLAG[i] != "Y"){
      p[[i]] = p[[i]] + scale_linetype_manual(name = "FLAG", values = c(1, 1),
                                              guide = guide_legend(override.aes = list(color = c("yellow", "blue"))))
    }else if (ZQ1_flag[i] == "Y" & g2_flag[i] != "Y" & modfit$SX2_FLAG[i] == "Y"){
      p[[i]] = p[[i]] + scale_linetype_manual(name = "FLAG", values = c(1, 1),
                                              guide = guide_legend(override.aes = list(color = c("orange", "blue"))))
    }else if (ZQ1_flag[i] != "Y" & g2_flag[i] == "Y" & modfit$SX2_FLAG[i] == "Y"){
      p[[i]] = p[[i]] + scale_linetype_manual(name = "FLAG", values = c(1, 1),
                                              guide = guide_legend(override.aes = list(color = c("yellow", "orange"))))
    }else if (ZQ1_flag[i] == "Y" & g2_flag[i] == "Y" & modfit$SX2_FLAG[i] == "Y"){
      p[[i]] = p[[i]] + scale_linetype_manual(name = "FLAG", values = c(1, 1,1),
                                              guide = guide_legend(override.aes = list(color = c("yellow","orange", "blue"))))
    }
  }

  pdf(mypath$fit_plot_path)
#  grid.table(anch_prms_print,rows=NULL,theme =ttheme_default(base_size = 7))
  invisible(lapply(p, print))
  dev.off()

  return(list(fit_tab = modfit,fit_plot = p))
}

