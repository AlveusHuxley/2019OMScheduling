library(sqldf)

loop_num <- 0
avg_happiness_old <- 0
avg_happiness_new <- 0


while(TRUE){
  start_time <- Sys.time()
  
  PrefMatrix <- read.csv(file = "C:\\Users\\nick\\OneDrive - Signature Travel Network\\Desktop\\PreferenceMatrix.csv")
  AtndAvail <- read.csv(file = "C:\\Users\\nick\\OneDrive - Signature Travel Network\\Desktop\\AttendeeAvail.csv")
  AtndApptCounts <- read.csv(file = "C:\\Users\\nick\\OneDrive - Signature Travel Network\\Desktop\\AttendeeApptCounts.csv")
  AtndPrefCounts <- read.csv(file = "C:\\Users\\nick\\OneDrive - Signature Travel Network\\Desktop\\AttendeePrefCounts.csv")
  RepAvail <- read.csv(file = "C:\\Users\\nick\\OneDrive - Signature Travel Network\\Desktop\\RepAvail.csv")
  RepApptCounts <- read.csv(file = "C:\\Users\\nick\\OneDrive - Signature Travel Network\\Desktop\\RepApptCounts.csv")
  
  df_SharesGroup1 <- read.csv(file = "C:\\Users\\nick\\OneDrive - Signature Travel Network\\Desktop\\SharesGroup1.csv")
  df_SharesGroup2 <- read.csv(file = "C:\\Users\\nick\\OneDrive - Signature Travel Network\\Desktop\\SharesGroup2.csv")
  df_SharesGroup3 <- read.csv(file = "C:\\Users\\nick\\OneDrive - Signature Travel Network\\Desktop\\SharesGroup3.csv")
  SharesGroup1 <- as.character(df_SharesGroup1[,1])
  SharesGroup2 <- as.character(df_SharesGroup2[,1])
  SharesGroup3 <- as.character(df_SharesGroup3[,1])
  
  num_atnd <- nrow(AtndAvail)
  
  high_pref_weight = 3
  medium__pref_weight = 2
  #HARD CODED IN df_happiness_index SQL QUERY ALSO
  
  survey_cutoff <- 40
  atnds_above_survey_cutoff <- as.character(AtndPrefCounts$Invitee.ID[AtndPrefCounts$Total >= survey_cutoff])
  atnds_below_survey_cutoff <- as.character(AtndPrefCounts$Invitee.ID[AtndPrefCounts$Total < survey_cutoff])
  
  ################## RANDOMLY SET APPOINTMENT BLOCKS, QUARTER OF ATTENDEES FOR EACH THREE APPOINTMENT TIME FRAME ######################################
  #Assuming 227 total attendees
  
  while(TRUE){
    atnd_block_list <- as.character(AtndAvail$Invitee.ID)
    block <- sample(atnd_block_list, 90)
    #Force people in Shares Group 1 and 2 to have the same block day
    block_sharesgroup1_same_company <- as.character(df_SharesGroup1[df_SharesGroup1$Company %in% df_SharesGroup1[SharesGroup1 %in% block,2], 1])
    block_sharesgroup2_same_company <- as.character(df_SharesGroup2[df_SharesGroup2$Company %in% df_SharesGroup2[SharesGroup2 %in% block,2], 1])  
    block <- append(block, as.character(block_sharesgroup1_same_company[!(block_sharesgroup1_same_company %in% block)]))
    block <- append(block, as.character(block_sharesgroup2_same_company[!(block_sharesgroup2_same_company %in% block)]))                   
    #print(length(block))
    
    if(length(block) == 114){
      break
    }
  }
  
  AtndAvail[,2][(AtndAvail$Invitee.ID %in% block)] <- rep(0, length(block))
  AtndAvail[,3][(AtndAvail$Invitee.ID %in% block)] <- rep(0, length(block))
  AtndAvail[,4][(AtndAvail$Invitee.ID %in% block)] <- rep(0, length(block))
  AtndAvail[,5][(AtndAvail$Invitee.ID %in% block)] <- rep(0, length(block))
  AtndAvail[,6][(AtndAvail$Invitee.ID %in% block)] <- rep(0, length(block))
  AtndAvail[,7][(AtndAvail$Invitee.ID %in% block)] <- rep(0, length(block))
  AtndAvail[,8][(AtndAvail$Invitee.ID %in% block)] <- rep(0, length(block))
  
  block <- atnd_block_list[!(atnd_block_list %in% block)]
  
  AtndAvail[,9][(AtndAvail$Invitee.ID %in% block)] <- rep(0, 113)
  AtndAvail[,10][(AtndAvail$Invitee.ID %in% block)] <- rep(0, 113)
  AtndAvail[,11][(AtndAvail$Invitee.ID %in% block)] <- rep(0, 113)
  AtndAvail[,12][(AtndAvail$Invitee.ID %in% block)] <- rep(0, 113)
  AtndAvail[,13][(AtndAvail$Invitee.ID %in% block)] <- rep(0, 113)
  AtndAvail[,14][(AtndAvail$Invitee.ID %in% block)] <- rep(0, 113)
  AtndAvail[,15][(AtndAvail$Invitee.ID %in% block)] <- rep(0, 113)
  
  #for (i in 1:nrow(AtndAvail)){
  #  print(sum(as.numeric(AtndAvail[i, 2:ncol(AtndAvail)])))
  #}
  
  ######################################################################################################################################################
  
  
  for (i in 1:nrow(RepAvail)){
    
    rep <- as.character(RepAvail$Rep.ID[i])
    supplier_name <- as.character(RepAvail$Company.Name)[i]
    
    for (j in 1:14){
      #print(supplier_name)#############################print
      #print(i)
      #print(j)
      zero_appts_atnds <- as.character(AtndApptCounts$Invitee.ID[AtndApptCounts$Total == 0])
      one_to_three_appts_atnds <- as.character(AtndApptCounts$Invitee.ID[AtndApptCounts$Total %in% c(1,2,3)])
      high_interest_atnds <- as.character(PrefMatrix$Invitee.ID[PrefMatrix[supplier_name] == "High"])
      medium_interest_atnds <- as.character(PrefMatrix$Invitee.ID[PrefMatrix[supplier_name] == "Medium"])
      high_or_medium_interest_atnds <- append(high_interest_atnds, medium_interest_atnds)
      
      df_happiness_index <- sqldf("SELECT TBL1.*, TBL1.HappinessNumerator/TBL1.HappinessDenominator as HappinessIndex FROM (SELECT AtndApptCounts.[Invitee.ID], AtndApptCounts.High * 3 + AtndApptCounts.Medium * 3 as HappinessNumerator, AtndPrefCounts.High * 3 + AtndPrefCounts.Med * 2 as HappinessDenominator FROM AtndApptCounts INNER JOIN AtndPrefCounts ON AtndApptCounts.[Invitee.ID] = AtndPrefCounts.[Invitee.ID]) AS TBL1")
      time_name <- names(RepAvail)[j+3]
      
      #print(time_name) ######################################print
      
      
      avail_atnds <- as.character(AtndAvail$Invitee.ID[AtndAvail[time_name] == 1])
      pick_from <- c()
      winner <- ""
      happiness_numerator_gain <- 0
      
      
      
      
      
      #############################PRIORITY SCHEMA FOR WHICH ATTENDEES TO PICK FROM###############################################################
      
      zero_appts_above_survey_cutoff_high_interest_avail <- zero_appts_atnds[zero_appts_atnds %in% atnds_above_survey_cutoff[atnds_above_survey_cutoff %in% high_interest_atnds[high_interest_atnds %in% avail_atnds]]]
      zero_appts_below_survey_cutoff_high_interest_avail <- zero_appts_atnds[zero_appts_atnds %in% atnds_below_survey_cutoff[atnds_below_survey_cutoff %in% high_interest_atnds[high_interest_atnds %in% avail_atnds]]]
      one_to_three_appts_above_survey_cutoff_high_interest_avail <- one_to_three_appts_atnds[one_to_three_appts_atnds %in% atnds_above_survey_cutoff[atnds_above_survey_cutoff %in% high_interest_atnds[high_interest_atnds %in% avail_atnds]]]
      one_to_three_appts_below_survey_cutoff_high_interest_avail <- one_to_three_appts_atnds[one_to_three_appts_atnds %in% atnds_below_survey_cutoff[atnds_below_survey_cutoff %in% high_interest_atnds[high_interest_atnds %in% avail_atnds]]]
      above_survey_cutoff_high_interest_avail <- atnds_above_survey_cutoff[atnds_above_survey_cutoff %in% high_interest_atnds[high_interest_atnds %in% avail_atnds]]
      below_survey_cutoff_high_interest_avail <- atnds_below_survey_cutoff[atnds_below_survey_cutoff %in% high_interest_atnds[high_interest_atnds %in% avail_atnds]]
      zero_appts_above_survey_cutoff_medium_interest_avail <- zero_appts_atnds[zero_appts_atnds %in% atnds_above_survey_cutoff[atnds_above_survey_cutoff %in% medium_interest_atnds[medium_interest_atnds %in% avail_atnds]]]
      zero_appts_below_survey_cutoff_medium_interest_avail <- zero_appts_atnds[zero_appts_atnds %in% atnds_below_survey_cutoff[atnds_below_survey_cutoff %in% medium_interest_atnds[medium_interest_atnds %in% avail_atnds]]]
      one_to_three_appts_above_survey_cutoff_medium_interest_avail <- one_to_three_appts_atnds[one_to_three_appts_atnds %in% atnds_above_survey_cutoff[atnds_above_survey_cutoff %in% medium_interest_atnds[medium_interest_atnds %in% avail_atnds]]]
      one_to_three_appts_below_survey_cutoff_medium_interest_avail <- one_to_three_appts_atnds[one_to_three_appts_atnds %in% atnds_below_survey_cutoff[atnds_below_survey_cutoff %in% medium_interest_atnds[medium_interest_atnds %in% avail_atnds]]]
      above_survey_cutoff_medium_interest_avail <- atnds_above_survey_cutoff[atnds_above_survey_cutoff %in% medium_interest_atnds[medium_interest_atnds %in% avail_atnds]]
      below_survey_cutoff_medium_interest_avail <- atnds_below_survey_cutoff[atnds_below_survey_cutoff %in% medium_interest_atnds[medium_interest_atnds %in% avail_atnds]]
      
      
      if (length(zero_appts_above_survey_cutoff_high_interest_avail) != 0){
        pick_from <- zero_appts_above_survey_cutoff_high_interest_avail
        happiness_numerator_gain <- high_pref_weight
      } else if (length(zero_appts_below_survey_cutoff_high_interest_avail) != 0){
        pick_from <- zero_appts_below_survey_cutoff_high_interest_avail
        happiness_numerator_gain <- high_pref_weight
      } else if (length(one_to_three_appts_above_survey_cutoff_high_interest_avail) != 0){
        pick_from <- one_to_three_appts_above_survey_cutoff_high_interest_avail
        happiness_numerator_gain <- high_pref_weight
      } else if (length(one_to_three_appts_below_survey_cutoff_high_interest_avail) != 0){
        pick_from <- one_to_three_appts_below_survey_cutoff_high_interest_avail
        happiness_numerator_gain <- high_pref_weight
      } else if (length(above_survey_cutoff_high_interest_avail) != 0){
        pick_from <- above_survey_cutoff_high_interest_avail
        happiness_numerator_gain <- high_pref_weight
      } else if (length(below_survey_cutoff_high_interest_avail) != 0){
        pick_from <- below_survey_cutoff_high_interest_avail
        happiness_numerator_gain <- high_pref_weight
      } else if (length(zero_appts_above_survey_cutoff_medium_interest_avail) != 0){
        pick_from <- zero_appts_above_survey_cutoff_medium_interest_avail
        happiness_numerator_gain <- medium__pref_weight
      } else if (length(one_to_three_appts_above_survey_cutoff_medium_interest_avail) != 0){
        pick_from <- one_to_three_appts_above_survey_cutoff_medium_interest_avail
        happiness_numerator_gain <- medium__pref_weight
      } else if (length(one_to_three_appts_below_survey_cutoff_medium_interest_avail) != 0){
        pick_from <- one_to_three_appts_below_survey_cutoff_medium_interest_avail
        happiness_numerator_gain <- medium__pref_weight
      } else if (length(zero_appts_below_survey_cutoff_medium_interest_avail) != 0){
        pick_from <- zero_appts_below_survey_cutoff_medium_interest_avail
        happiness_numerator_gain <- medium__pref_weight
      } else if (length(above_survey_cutoff_medium_interest_avail) != 0) {
        pick_from <- above_survey_cutoff_medium_interest_avail
        happiness_numerator_gain <- medium__pref_weight
      } else if (length(below_survey_cutoff_medium_interest_avail) != 0){
        pick_from <- below_survey_cutoff_medium_interest_avail
        happiness_numerator_gain <- medium__pref_weight
      } else {
        print("No Winner")#######################################print
        next
      }
      
      
      ########################################################################################################################################
      
      
      ################################################pick winner function to calculate happiness gain############################################
      
      if (length(pick_from) == 1){
        winner = pick_from[1]
      } else {
        
        df_pick_from <- df_happiness_index[df_happiness_index$Invitee.ID %in% pick_from,]
        df_pick_from$HappinessNumerator <- df_pick_from$HappinessNumerator + happiness_numerator_gain
        df_pick_from$HappinessIndex <- df_pick_from$HappinessNumerator / df_pick_from$HappinessDenominator
        max_happiness_gain <- max(df_pick_from$HappinessIndex)
        df_pick_from <- df_pick_from[(df_pick_from$HappinessIndex == max_happiness_gain),]
        
        if (nrow(df_pick_from) == 1){
          winner <- as.character(df_pick_from$Invitee.ID[1])
        } else {
          winner <- sample(as.character(df_pick_from$Invitee.ID), 1)
        }
      }
      
      #set RepAvail to Invitee.ID    
      RepAvail[,time_name][RepAvail$Rep.ID == rep] <- winner
      
      #set AtndAvail to Rep.ID    
      AtndAvail[,time_name][AtndAvail$Invitee.ID == winner] <- rep
      
      #set AtndApptCounts += 1
      if (happiness_numerator_gain == 3){
        AtndApptCounts$High[AtndApptCounts$Invitee.ID == winner] <- AtndApptCounts$High[AtndApptCounts$Invitee.ID == winner] + 1
      } else {
        AtndApptCounts$Medium[AtndApptCounts$Invitee.ID == winner] <- AtndApptCounts$Medium[AtndApptCounts$Invitee.ID == winner] + 1
      }
      AtndApptCounts$Total <- AtndApptCounts$High + AtndApptCounts$Medium
      
      #set RepApptCounts += 1    
      if (happiness_numerator_gain == 3){
        RepApptCounts$High[RepApptCounts$Rep.ID == rep] <- RepApptCounts$High[RepApptCounts$Rep.ID == rep] + 1
      } else {
        RepApptCounts$Medium[RepApptCounts$Rep.ID == rep] <- RepApptCounts$Medium[RepApptCounts$Rep.ID == rep] + 1
      }
      RepApptCounts$Total <- RepApptCounts$High + RepApptCounts$Medium
      
      #set PrefMatrix to null for that supplier and winning atnd, keep AtndPrefCounts the same    
      PrefMatrix[,supplier_name][PrefMatrix$Invitee.ID == winner] <- ""
      
      
      
      #Check share group
      if (winner %in% SharesGroup1){
        winner_company <- as.character(df_SharesGroup1[df_SharesGroup1$Invitee.ID == winner,2])
        other_winners <- as.character(df_SharesGroup1$Invitee.ID[df_SharesGroup1$Company == winner_company])
        other_winners <- other_winners[other_winners != winner]
        
        for (k in 1:length(other_winners)){
          #set AtndAvail to RepID
          AtndAvail[,time_name][AtndAvail$Invitee.ID == other_winners[k]] <- rep
          
          #set AtndApptCounts += 1
          if (happiness_numerator_gain == 3){
            AtndApptCounts$High[AtndApptCounts$Invitee.ID == other_winners[k]] <- AtndApptCounts$High[AtndApptCounts$Invitee.ID == other_winners[k]] + 1
          } else {
            AtndApptCounts$Medium[AtndApptCounts$Invitee.ID == other_winners[k]] <- AtndApptCounts$Medium[AtndApptCounts$Invitee.ID == other_winners[k]] + 1
          }
          AtndApptCounts$Total <- AtndApptCounts$High + AtndApptCounts$Medium
          
          #set PrefMatrix to null for that supplier and shared atnd, keep atndprefcounts the same
          PrefMatrix[,supplier_name][PrefMatrix$Invitee.ID == other_winners[k]] <- ""
        }
      } else if (winner %in% SharesGroup2){
        
        winner_company <- as.character(df_SharesGroup2[df_SharesGroup2$Invitee.ID == winner,2])
        other_winners <- as.character(df_SharesGroup2$Invitee.ID[df_SharesGroup2$Company == winner_company])
        other_winners <- other_winners[other_winners != winner]
        other_winners <- other_winners[other_winners %in% avail_atnds]
        other_winners <- other_winners[other_winners %in% high_or_medium_interest_atnds]
        
        if (length(other_winners != 0)){
          for (k in 1:length(other_winners)){
            #set AtndAvail to RepID
            AtndAvail[,time_name][AtndAvail$Invitee.ID == other_winners[k]] <- rep
            
            #set AtndApptCounts += 1
            if (happiness_numerator_gain == 3){
              AtndApptCounts$High[AtndApptCounts$Invitee.ID == other_winners[k]] <- AtndApptCounts$High[AtndApptCounts$Invitee.ID == other_winners[k]] + 1
            } else {
              AtndApptCounts$Medium[AtndApptCounts$Invitee.ID == other_winners[k]] <- AtndApptCounts$Medium[AtndApptCounts$Invitee.ID == other_winners[k]] + 1
            }
            AtndApptCounts$Total <- AtndApptCounts$High + AtndApptCounts$Medium
            
            #set PrefMatrix to null for that supplier and shared atnd, keep atndprefcounts the same
            PrefMatrix[,supplier_name][PrefMatrix$Invitee.ID == other_winners[k]] <- ""
          }
        }
      }else if (winner %in% SharesGroup3){
        winner_company <- as.character(df_SharesGroup3[df_SharesGroup3$Invitee.ID == winner,2])
        other_winners <- as.character(df_SharesGroup3$Invitee.ID[df_SharesGroup3$Company == winner_company])
        other_winners <- other_winners[other_winners != winner]
        other_winners <- other_winners[other_winners %in% avail_atnds]
        other_winners <- other_winners[other_winners %in% high_or_medium_interest_atnds]
        
        if (length(other_winners) != 0){
          for (k in 1:length(other_winners)){
            #set AtndAvail to RepID
            AtndAvail[,time_name][AtndAvail$Invitee.ID == other_winners[k]] <- rep
            
            #set AtndApptCounts += 1
            if (happiness_numerator_gain == 3){
              AtndApptCounts$High[AtndApptCounts$Invitee.ID == other_winners[k]] <- AtndApptCounts$High[AtndApptCounts$Invitee.ID == other_winners[k]] + 1
            } else {
              AtndApptCounts$Medium[AtndApptCounts$Invitee.ID == other_winners[k]] <- AtndApptCounts$Medium[AtndApptCounts$Invitee.ID == other_winners[k]] + 1
            }
            AtndApptCounts$Total <- AtndApptCounts$High + AtndApptCounts$Medium
            
            #set PrefMatrix to null for that supplier and shared atnd, keep atndprefcounts the same
            PrefMatrix[,supplier_name][PrefMatrix$Invitee.ID == other_winners[k]] <- ""
          }
        }
      }

        #if share group 2 or 3, only assign same appt to others with high or medium interest in same company who are available
          #set atndavail to repid
          #set atndapptcounts += 1 
          #set prefmatrix to null for that supplier for all in that company (not just all who were available)
        #if share group 3, assign same appt to others with high or medium interest in s ame company who are available
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      #print(winner)
    }
    
    #  if (i == 200){
    #    break
    #  }
  }
  
  
  
  avg_happiness_old <- mean(df_happiness_index$HappinessIndex[!is.na(df_happiness_index$HappinessIndex)])
  
  if (avg_happiness_old > avg_happiness_new){
    out_AtndApptCounts <- AtndApptCounts
    out_AtndAvail <- AtndAvail
    out_PrefMatrix <- PrefMatrix
    out_RepApptCounts <- RepApptCounts
    out_RepAvail <- RepAvail
    out_df_happiness_index <- sqldf("SELECT TBL1.*, TBL1.HappinessNumerator/TBL1.HappinessDenominator as HappinessIndex FROM (SELECT AtndApptCounts.[Invitee.ID], AtndApptCounts.High * 3 + AtndApptCounts.Medium * 3 as HappinessNumerator, AtndPrefCounts.High * 3 + AtndPrefCounts.Med * 2 as HappinessDenominator FROM AtndApptCounts INNER JOIN AtndPrefCounts ON AtndApptCounts.[Invitee.ID] = AtndPrefCounts.[Invitee.ID]) AS TBL1")
    
    df_compare_output <- sqldf("SELECT * FROM AtndApptCounts INNER JOIN AtndPrefCounts ON AtndApptcounts.[Invitee.ID] = AtndPrefCounts.[Invitee.ID]")
    avg_happiness_new <- avg_happiness_old
  }
  
  print(avg_happiness_new)
  
  
  
  end_time <- Sys.time()
  elapsed <- end_time - start_time
  print(elapsed)
  
  loop_num <- loop_num + 1
  if (loop_num == 6){
    break
  }
  write.csv(df_compare_output, file = "C:\\Users\\nick\\OneDrive - Signature Travel Network\\Desktop\\df_compare_ouput.csv", row.names = FALSE)
  write.csv(out_AtndAvail, file = "C:\\Users\\nick\\OneDrive - Signature Travel Network\\Desktop\\out_AtndAvail.csv", row.names = FALSE)
  write.csv(out_RepApptCounts, file = "C:\\Users\\nick\\OneDrive - Signature Travel Network\\Desktop\\out_RepApptCounts.csv", row.names = FALSE)
  write.csv(out_RepAvail, file = "C:\\Users\\nick\\OneDrive - Signature Travel Network\\Desktop\\out_RepAvail.csv", row.names = FALSE)
  write.csv(out_PrefMatrix, file = "C:\\Users\\nick\\OneDrive - Signature Travel Network\\Desktop\\out_PrefMatrix.csv", row.names = FALSE)
}

