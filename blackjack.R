# Functions for the blackjack simulation tutorial

#### PART 1 ####################################################################

newdeck = function() {
####################################
#  returns a vector of cards representing 
#  a standard 52 card deck without any jokers
####################################

	deck <- c("AS", "2S", "3S", "4S", "5S", "6S", "7S", "8S", "9S", "10S", "JS", "QS", "KS",
          "AH", "2H", "3H", "4H", "5H", "6H", "7H", "8H", "9H", "10H", "JH", "QH", "KH",
          "AC", "2C", "3C", "4C", "5C", "6C", "7C", "8C", "9C", "10C", "JC", "QC", "KC",
          "AD", "2D", "3D", "4D", "5D", "6D", "7D", "8D", "9D", "10D", "JD", "QD", "KD")

	  return(deck)
}

convertCardsToNumeric <- function(cards) {
############################ 
#  converts a vector of strings representing 
#  cards into a numeric vector of card values
#  aces are converted to 11 for ease of summation
############################
	deck_ref <- c(11, 2, 3, 4, 5, 6, 7, 8, 9, 10, 10, 10, 10,
		      11, 2, 3, 4, 5, 6, 7, 8, 9, 10, 10, 10, 10,
    		      11, 2, 3, 4, 5, 6, 7, 8, 9, 10, 10, 10, 10,
    		      11, 2, 3, 4, 5, 6, 7, 8, 9, 10, 10, 10, 10)		      

        deck <- newdeck();
	ret_vector = numeric(length(cards))
	ref_idx <- match(cards, deck)
	for (c in 1:length(cards)) {
	  ret_vector[c] <- deck_ref[ref_idx[c]]
	}
	return(ret_vector)
}	

countCardsByDenomination <- function(cards) {
############################ 
#  converts a vector of strings representing 
#  cards into a numeric vector of number of 
#  cards of specific denominations aces=1
############################
	deck_ref <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13,
		      1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13,
    		      1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13,
    		      1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)		      

        deck <- newdeck();
	ret_vector = numeric(13)
	ref_idx <- match(cards, deck)
	for (c in 1:length(cards)) {
	    idx <- deck_ref[ref_idx[c]]
	    ret_vector[idx] <- ret_vector[idx] + 1
	}
	return(ret_vector)
}	



shuffle = function(cards) {
#############################
#  shuffles a vector of cards passed as a parameter
#  that can be multiple decks as is typical in 
#  a shoe for playing blackjack; note use
#  of native R function: sample
############################

	return(sample(cards, length(cards)))
}

makeHand <- function(up_cards, dn_card = "") {
##########################
#  A "hand" data frame represents cards dealt on the table
#  for a player or the dealer we'll include information like the sum of cards
#  sum of face up cards for the dealer and
#  whether the hand is "soft" meaning that it includes
#  aces valued at 11 vs 1
##########################
    # concatenate the down and up cards into one array called cards
    # putting the down card at the front
    if (dn_card != "") { 
       cards <- c(dn_card, up_cards)
    } else {
       cards <- up_cards
    }

    # create the hand data frame "is_soft" means there are aces valued
    # as 11 that can be re-valued as 1 if a hit pushes the hand over 21
    hand <- data.frame("UP_CARDS" = I(list(up_cards)), 
    	    	       "DN_CARD" = dn_card, 
    	               "SUM" = 0, 
		       "UP_SUM" = 0,
		       "HAS_ACES" = FALSE,
       		       "IS_SOFT" = FALSE,
		       "IS_NATURAL" = FALSE,
		       "OUTCOME" = "")

    # convert the cards to their numeric values
    numeric_hand = convertCardsToNumeric(cards)

    # check for aces: note the use of the %in% syntax
    # this syntax generates a vector of TRUE/FALSE depending on
    # whether elements in vector 1 match elements in vector 2 
    aces <- match(numeric_hand, c(11), nomatch=0)
    if (sum(aces) > 0) {
       hand$HAS_ACES = TRUE
       hand$IS_SOFT = TRUE  # assume true for now this may turn out to be false later
    }

    # check for naturals: naturals are ace+face_card on 1st deal
    if(length(cards) == 2 && sum(aces) == 1 && sum(numeric_hand) == 21) {
      # this is a natural
      hand$IS_NATURAL = TRUE;
    }

   # sum the hand and check if we're over 21
   # if over 21, check if we have aces 
   hand_sum <- sum(numeric_hand)

   if (hand_sum > 21) {
      # do we have aces to reduce the overage?    
      ace_credit <- 10*sum(aces)   # ace credit = zero when there are no aces

      # see if ace_credit can reduce hand_sum to below 21
      if ((hand_sum - ace_credit) < 21) {

      	 # see how many aces we need to use
	 num_aces_to_use = floor((hand_sum - 21)/10)+1

	 if (num_aces_to_use < sum(aces)) {
	    # we have more aces to use for credit in the future - hand remains soft
	    hand$IS_SOFT = TRUE	    
	    hand_sum <- hand_sum - 10*num_aces_to_use
	 } else {
	    hand$IS_SOFT = FALSE	    
	    hand_sum <- hand_sum - 10*num_aces_to_use
	 }

      } else {
         # this hand is bust
	 hand_sum <- (hand_sum - 10*sum(aces))
      	 hand$IS_SOFT <- FALSE
	 hand$OUTCOME <- "BUST"
      } 
   }
   
   hand$SUM <- hand_sum
   if (dn_card != "") {
      hand$UP_SUM = hand_sum - numeric_hand[1]	    
   } else {
      hand$UP_SUM = hand_sum
   }

    return(hand)
}

makeShoe = function(num_decks, use_cut_card = TRUE) {
######################################
#  Returns a dataframe containing a shoe  
#  A shoe dataframe contains an ordered set of shuffled
#  cards and a record of the cards dealt to each player
#  and the dealer
#
######################################

     # create a collection of cards from the requested number of decks
     s <- newdeck()
     if (num_decks > 1) {
       for (i in 2:num_decks) {
          s <- c(s, newdeck())
       }
    }

    # shuffle the cards
    s <- shuffle(s) 

    # add a cut card 
    if (use_cut_card) {
       # if using a cut card target cutting last 25% of
       # deck w/ +/- 10% variability of placement based on dealer     
       loc <- num_decks*52 - floor(num_decks*52*(.25 + runif(1,-.1,.1))); 
       s <- c(s[1:loc], "CUT", s[(loc+1):length(s)])  
     } else {
       # if not using a cut card, we will simulate reshuffling of the discards by
       # setting a cut card at the end of the deck and adding a reshuffled first
       # 2/3 of the shoe to simulate the dealer taking some discards and
       # reshuffling them for the last deal
       loc <- (length(s)+1)
       reshuf <- shuffle(s[1:floor(2*length(s)/3)])
       s <- c(s, "CUT", reshuf)
     }

    # create the data frame
    shoe <- data.frame("SHOE" = s, HAND_NUM = "",
    	    	       "DLR_UP_CARD" = "", "DLR_DN_CARD" = "", 
    	               "PLR1_CARD" = "", 
		       "PLR2_CARD" = "", 
       		       "PLR3_CARD" = "", 
       		       "PLR4_CARD" = "")

    return(shoe)
}


dealCard <- function(shoe, player, hand_number, is_dlr_down = FALSE) {
#################################################
#  Deals one card to the specified player and updates
#  the shoe 
#  player 0 is the dealer who goes last
#################################################

    cut_idx <- getCutCardIdx(shoe);
    card_idx <- getCurrentShoeIdx(shoe)

    # check if this is the cut card and skip if so
    if (cut_idx == card_idx) {
     shoe[card_idx,"HAND_NUM"] = hand_number
     card_idx = card_idx +1;
    }

    # set the hand number 
    shoe[card_idx,"HAND_NUM"] = hand_number;	

    # deal the next card and update the hand dataframe for the appropriate player
    card_dealt <- shoe[card_idx,1]

    if (is_dlr_down) {
      shoe[card_idx,"DLR_DN_CARD"] <- card_dealt
    } else if (player == 0) {
      shoe[card_idx,"DLR_UP_CARD"] <- card_dealt
      } else {    
      colname = paste("PLR", player, "_CARD", sep = "");
      shoe[card_idx,colname] <- card_dealt;
    }

   return(shoe)
}

getCurrentShoeIdx <- function(shoe) {
########################################
# returns the index of the next undealt card
#
########################################
        for (i in 1:length(shoe[,1])) {
	   if (shoe[i,2] == "") {
	       return(i)
	   }
	}
}

getCutCardIdx <- function(shoe) {
###################################
#  returns the index of the cut card if used
#  otherwise returns (-1)
#####################################
	for (i in 1:length(shoe[,1])) {
	   if (shoe[i,1] == 'CUT') {
	       return(i)
	   }
	}
	
	return(length(shoe[,1]))
}

getCardsInRound <- function (shoe, hand = -1) {
######################################
#  returns all the cards played in the round
#  (HAND in the data frame) 
#####################################

	# get all 4 players cards
	cards <- getCardsInPlayersHand(shoe, 0, hand)
	for (p in 1:4) {
	   pcards <- getCardsInPlayersHand(shoe, p, hand)
	   cards <- c(cards, pcards)
	} 
	return(cards)
}



getCardsInPlayersHand <- function(shoe, player, hand = -1) {
######################################
#  returns the players current hand as a set of cards
#  and can be used to make a hand dataframe
#####################################

       if (hand == -1) {
          # return the last hand in the shoe
          hands <- getHandsInShoe(shoe);
       	  hand <- max(hands)
       }

       cards = "";
       if(player == 0) {
          # this is the dealers hand if the down card is dealt,
	  # it is the first card in the list
	  cdn = shoe[shoe$HAND_NUM == hand, "DLR_DN_CARD"]
	  cup = shoe[shoe$HAND_NUM == hand, "DLR_UP_CARD"]	  
	  cards <- c(cdn, cup)
       } else {
       	  colname = paste("PLR", player, "_CARD", sep = "");
       	  cards <- shoe[shoe$HAND_NUM == hand, colname];
	  }
       cards <- cards[cards != ""]
       return(cards)
}

getHandsInShoe <- function(shoe) {
##############################
#  returns the hands in the shoe as an ordered array of integers
######################################

     hands <- unique(shoe[,2]);
     if (length(hands) == 1) {
        return(0)
     } else {
        hands <- hands[hands != ""]
     	hands <- strtoi(hands)        
     }
     return(hands)     
}

dealHand <- function(shoe, numplayers, cur_hand) {
##############################
#  curhand is the index of the hand being played in this
#  shoe and starts with 1
#  numplayers is 1-4
#  returns the hands in a shoe as an ordered array of integers
#
#  note that there are some other supporting functions
#  defined as well; see the file blackjack.R or type
#  print(fcn_name) in the console
######################################

     # deal each player card 1
     for (p in 1:numplayers) {
         shoe <- dealCard(shoe, p, cur_hand, FALSE);
     }
     
     # deal the dealer face up card
     shoe <- dealCard(shoe, 0, cur_hand, FALSE);	

     # deal the players 2nd cards
     for (p in 1:numplayers) {
         shoe <- dealCard(shoe, p, cur_hand, FALSE);
     }

     # deal the dealer face down card
     shoe <- dealCard(shoe, 0, cur_hand, TRUE);	


}

playRound <- function(shoe, numplayers, print_summary = TRUE) {
#######################################
#  Plays one round (game hand) to completion with 
#  stated number of players
#  returns the updated shoe
#
#######################################

     hands = getHandsInShoe(shoe);
     # get the next hand
     cur_hand <- 1
     if (length(hands) > 0) {
       cur_hand <- max(hands)+1
     }

     if (print_summary) { print(paste("Playing hand: ", cur_hand)) }

     shoe <- dealHand(shoe, numplayers, cur_hand)

     # check for dealer natural 21
     dlr_cards <- getCardsInPlayersHand(shoe, 0) 
     dlr_hand <- makeHand(dlr_cards[2], dlr_cards[1])


     if (dlr_hand$IS_NATURAL) {
       # end the round
       #print("dealer natural")       

     } else {
         # start gameplay for the hand starting with player 1
     	 for (p in 1:numplayers) {
       	     hit_me <- testForHitBasic(shoe, p)
             while(hit_me) {
	      shoe <- dealCard(shoe, p, cur_hand, FALSE)
       	      hit_me <- testForHitBasic(shoe, p)	
       	     }
	}
     
	# dealer plays
     	hit_me <- testForHitBasic(shoe, 0)
	while(hit_me) {
	  shoe <- dealCard(shoe, 0, cur_hand, FALSE)
       	  hit_me <- testForHitBasic(shoe, 0)	
     	}

	if (print_summary) {
	   hands <- scoreRound(shoe, cur_hand, numplayers)
	   print(hands)
	}

     }

     return(shoe)
}


scoreRoundsInShoe <- function(shoe, shoe_num = 1, num_players = 4) {
################################
#  Scores all the rounds in a given shoe and returns a 
#  dataframe of all player/dealer hands
#
###############################
     allhands <- getHandsInShoe(shoe)
     for (h in allhands) {
	 if (!exists("hands_in_shoe")) {
	     hand <- scoreRound(shoe, h, shoe_num)
	     hands_in_shoe <- hand
	 } else {
 	     hands_in_shoe <- rbind(hands_in_shoe, scoreRound(shoe, h, shoe_num))
	 }
     }

     return(hands_in_shoe)

}


scoreRound <- function(shoe, hand_num, shoe_num = 1, num_players = 4) {
#######################################
#  Evaluates the wins and losses for the specified
#  round and returns a hands dataframe
#  with a row for each players hand as well as the dealer
######################################

     	# determine busts, wins, losses, ties
	dlr_cards <- getCardsInPlayersHand(shoe, 0, hand_num)
	dlr_hand <- makeHand(dlr_cards[2:length(dlr_cards)], dlr_cards[1])
	hands <- dlr_hand
	dealer_sum = dlr_hand[1,"SUM"]

	cards_played_idx <- which(shoe[, "HAND_NUM"] == hand_num)
	first_card_idx <- cards_played_idx[1];
	last_card_idx <- cards_played_idx[length(cards_played_idx)]
	hands["FIRST_CARD_IDX"] <- first_card_idx
	hands["LAST_CARD_IDX"] <- last_card_idx
	hands["PLAYER"] <- 0
	hands["SHOE_NUM"] <- shoe_num
	hands["HAND_NUM"] <- hand_num

	if (dlr_hand$IS_NATURAL) {
	       hands[1, "OUTCOME"] <- "DLR_NATURAL"
	} 

	if(dealer_sum > 21) {
	       hands[1, "OUTCOME"] <- "DLR_BUST"
	} else {
	       hands[1, "OUTCOME"] <- "DLR_STAND"
	}

	for (p in 1:num_players) {
     	    p_hand <- makeHand(getCardsInPlayersHand(shoe, p, hand_num))
	    player_sum = p_hand[1,"SUM"]
	    if (player_sum > 21) {
	       p_hand[1, "OUTCOME"] <- "BUST"
	    } else if(dealer_sum > 21) {
	       p_hand[1, "OUTCOME"] <- "WIN_DLR_BUST"	       
	    } else if(player_sum > dealer_sum) {
	       p_hand[1, "OUTCOME"] <- "WIN"
	    } else if(player_sum < dealer_sum) {
	       p_hand[1, "OUTCOME"] <- "LOSS"
            } else if (dealer_sum == player_sum) {
	       p_hand[1, "OUTCOME"] <- "TIE"
	    }
	    p_hand["PLAYER"] <- p
	    p_hand["FIRST_CARD_IDX"] <- first_card_idx
	    p_hand["LAST_CARD_IDX"] <- last_card_idx	    
   	    p_hand["SHOE_NUM"] <- shoe_num
	    p_hand["HAND_NUM"] <- hand_num
            hands <- rbind(hands, p_hand)
	}

	return(hands)

}


testForHitBasic <- function(shoe, player) {
#######################################
#  this function implements a simple and 
#  universal hit test that stands at 17
#  and hits a soft 17 (typical dealer rules)
#  returns T/F 
######################################

   card_idx <- getCurrentShoeIdx(shoe)	       
   hand <- makeHand(getCardsInPlayersHand(shoe, player))

   take_hit = FALSE

   # simple strategy == stand at hard 17, hit soft 17
   if (hand$SUM < 17) {
     take_hit = TRUE
   } else if (hand$SUM == 17 && hand$IS_SOFT) {
     take_hit = TRUE
   }

   return(take_hit)
}


playShoe <- function(shoe, numplayers = 4) {
###################################
#  plays hands repeatededly until cut
#  card is reached and hand is completed
#  returns the updated Shoe
###################################
   keep_playing = TRUE
   while(keep_playing) {
     cut_idx <- getCutCardIdx(shoe);
     card_idx <- getCurrentShoeIdx(shoe)
     if (card_idx < cut_idx) {
       	 shoe <- playRound(shoe, numplayers, FALSE)
     } else {
       	 keep_playing = FALSE
     }
    }
   return(shoe)

}


playShoes <- function(number_of_shoes, decks_per_shoe = 4, savefile = TRUE, filename = "shoes.RData") {
###################################
#  plays the number of shoes requested
#  and saves the list "shoes" in
#  file shoes.RData
###################################
   shoes <- list()
   for (i in 1:number_of_shoes) {
        s <- makeShoe(4)    
        s <- playShoe(s)
        shoes <- c(shoes, s)
	cat(".")
   }
   save(shoes, file = "shoes.RData")
   return(shoes)
}

getShoeFromShoes <- function(shoes, shoenum) {
##############################
#  convenience method to return a single 
#  shoe from the shoes list
##############################

    listcolnames <- names(shoes)
    # extract the specified shoe
    s <- (shoenum-1)*8 + 1
    shoe <- as.data.frame(shoes[s])
    for (c in 1:7) {
       shoe <- cbind(shoe, as.data.frame(shoes[s+c]))
    }

    return(shoe)
    
}

getCardsPlayed <- function(shoe, hand_num) {
###################################
#  returns a vector of integers reflecting card
#  values of all cards played in the
#  specified hand
###################################
	all_cards_played <- shoe[shoe$HAND_NUM == hand_num, "SHOE"]
	all_cards_played <- all_cards_played[all_cards_played != "CUT"] # remove the cut card if nec
	all_cards_played_numeric <- convertCardsToNumeric(all_cards_played) 
 	#print(all_cards_played)
	#print(all_cards_played_numeric)
	return(all_cards_played_numeric)
}

getMeanCardsRemainingInShoe <- function(shoe, hand_num) {
##########################
# return the mean card value of the remaining shoe at the 
# start of the hand; this is the basis of card counting
#########################
	idx_first_card_in_hand <- match(hand_num, shoe$HAND_NUM)
	shoe_length <- length(shoe[,"SHOE"])
	remaining_cards <- shoe[idx_first_card_in_hand:shoe_length, "SHOE"]

	# remove cut card
	remaining_cards <- remaining_cards[remaining_cards != "CUT"]
	remaining_cards_numeric <- convertCardsToNumeric(remaining_cards) 
	return(mean(remaining_cards_numeric))
}


getHandsInShoes <- function(shoes)  {
##########################
# score all the hands in our collection of shoes
# and make one data structure that we will reuse
# called hands_in_shoes
###########################
         numshoes = length(shoes)/8
         for (s in 1:numshoes) {
            shoe <- getShoeFromShoes(shoes, s)
	    hands_in_this_shoe <- scoreRoundsInShoe(shoe, s)
	    if (!exists("hands_in_shoes")) {
               hands_in_shoes <- hands_in_this_shoe
            } else {
               hands_in_shoes <- rbind(hands_in_shoes, hands_in_this_shoe)
            }
         }
	 
	 return(hands_in_shoes)

}



getWinsAndLosses <- function(hands_in_shoes) {
########################
#  Tallys the wins and losses for each player in the
#  given collection of hands
# 
########################
	 # tally the wins and losses for players 1-4
         wins_losses <- data.frame("Player" = numeric(), "Wins" = numeric(),
                                   "WinDlrBust" = numeric(),
                                   "TotHands" = numeric(), "FractionWon" = numeric())

	 for (p in 1:4) {
             playerhands <- hands_in_shoes[hands_in_shoes$PLAYER == p,]
             wins <- playerhands[playerhands$OUTCOME == "WIN", ]
             wins_dlr_bust <- playerhands[playerhands$OUTCOME == "WIN_DLR_BUST", ]
             wins_losses[p, "Player"] <- p
             wins_losses[p, "Wins"] <- length(wins[, 1])
             wins_losses[p, "WinDlrBust"] <- length(wins_dlr_bust[, 1])
             wins_losses[p, "TotHands"] <- length(playerhands[, 1])
             wins_losses[p, "FractionWon"] <- ((wins_losses[p, "Wins"] + wins_losses[p, "WinDlrBust"])
                                                / wins_losses[p, "TotHands"])
         }
	 return(wins_losses)

} 

min_max_norm <- function(x) {
##################################
# this is just just a simple Min-Max normalization function
# for the array passed in
##################################
  ret <- (x - min(x)) / (max(x) - min(x))
  return(ret)
}

plot_histogram <- function(df, feature, bin_num = 30, density_adjust = 1) {
##############################
# plot a histogram with a bar chart overlaid with a transparent density function
##############################
    plt <- ggplot(df, aes(x=eval(parse(text=feature)))) +
    geom_histogram(aes(y = ..density..), alpha=0.7, fill="#33AADE", color="black", bins = bin_num) +
    geom_density(alpha=0.3, fill="red", adjust = density_adjust) +
    geom_vline(aes(xintercept=mean(eval(parse(text=feature)))), color="black", linetype="dashed", size=1) +
    labs(x=feature, y = "Density")
    print(plt)
}

plot_multi_histogram <- function(df, feature, label_column, bin_num = 30, density_adjust = 1) {
##############################
# plot multiple histograms with overlaid transparent density function, separated by color
##############################
    plt <- ggplot(df, aes(x=eval(parse(text=feature)), fill=eval(parse(text=label_column)))) +
    geom_histogram(alpha=0.25, position="identity", aes(y = ..density..), color="black", bins = bin_num) +
    geom_density(alpha=0.25, adjust = density_adjust) +
    geom_vline(aes(xintercept=mean(eval(parse(text=feature)))), color="black", linetype="dashed", size=1) +
    labs(x=feature, y = "Density")
    plt + guides(fill=guide_legend(title=label_column))
    return(plt)
}

##################################################################
######################## FUNCTIONS TO CREATE FIGURES #############
##################################################################


# When possible, I prefer to generate figures within functions that return
# the figure object for further processing - this helps to keep the code
# modular as well as foster re-use

# see the corresponding code-chunk label in the Rmd file

figShuffleBubble <- function(shoes) {

  ordered_deck <- newdeck()
  ordered_deck<- c(ordered_deck, "CUT") # add the cut card to the end of the deck
  position_count_df <- NULL

  for (card in ordered_deck) {
    idxarr_logical <- shoes[[1]] %in% card  # returns an array of booleans
    idxarr_int <- as.integer(idxarr_logical)  # converts booleans to 0 1

    # NB that we can't directly name a column in a dataframe using a
    # variable containing a string with the column name
    # so we create the df with a placeholder column then rename it
    df <- data.frame("tmp" = idxarr_int)
    colnames(df)<- c(card)

    if (is.null(position_count_df)) {
       position_count_df <- df
    } else {
       position_count_df <- cbind(position_count_df, df)
    }

    for (s in seq(from = 9, to = length(shoes), by = 8)) {
       # now that we've set the column type, the R system will convert T/F to 1/0 under addition
       idxarr_logical <- shoes[[s]] %in% card  # returns an array of booleans
       idxarr_int <- as.integer(idxarr_logical)  # converts booleans to 0 1
       position_count_df[,card] <- position_count_df[,card] + idxarr_int
    }
  }

  ## generate the bubble plot using the data frame
  library(ggplot2)
  library(dplyr)
  library(reshape2)

  # add a card_loc column to the df and reshape the data into a format that ggplot can handle
  card_names <- colnames(position_count_df) # get the card column names
  df = data.frame("card_loc" = 1:length(idxarr_int)) # create a number sequence column
  pos_count_to_melt <- cbind(position_count_df, df) # add it to the position count
                                                    # dataframe and save as new dataframe
  # reshape into an x,y,z format
  pos_count_melted <- melt(pos_count_to_melt, id.vars = ("card_loc"),
                                             measure.vars = card_names)
  str(pos_count_melted)

  fig <- ggplot(pos_count_melted, aes(x=card_loc, y=variable, size=value)) +
                           geom_point(alpha=0.3) +
                           scale_size(range = c(.1, 2), name="Occurrances") +
                           xlab("Location in shoe") +
                           ylab("Card") +
                           theme(text = element_text(size = 9))
   return(fig)
}


figShuffleBinom <- function (shoes) {

  ordered_deck <- newdeck()
  ordered_deck<- c(ordered_deck, "CUT") # add the cut card to the end of the deck
  position_count_df <- NULL

  for (card in ordered_deck) {
    idxarr_logical <- shoes[[1]] %in% card  # returns an array of booleans
    idxarr_int <- as.integer(idxarr_logical)  # converts booleans to 0 1

    # NB that we can't directly name a column in a dataframe using a
    # variable containing a string with the column name
    # so we create the df with a placeholder column then rename it
    df <- data.frame("tmp" = idxarr_int)
    colnames(df)<- c(card)

    if (is.null(position_count_df)) {
       position_count_df <- df
    } else {
       position_count_df <- cbind(position_count_df, df)
    }

    for (s in seq(from = 9, to = length(shoes), by = 8)) {
       # now that we've set the column type, the R system will convert T/F to 1/0 under addition
       idxarr_logical <- shoes[[s]] %in% card  # returns an array of booleans
       idxarr_int <- as.integer(idxarr_logical)  # converts booleans to 0 1
       position_count_df[,card] <- position_count_df[,card] + idxarr_int
    }
  }


  # add a card_loc column to the df and reshape the data into a format that ggplot can handle
  pval_to_melt <- position_count_df # create a copy of the position count dataframe
  for (c in colnames(pval_to_melt)) {
      freq <- 4/209;
      if (c == "CUT") { freq <- 1/409 }
      counts <- position_count_df[,c]
      pvals <- (1-pbinom(counts, size = 209, prob = freq))  # use the binomial dist
      pval_to_melt[,c] <- pvals
  }
 
  card_names <- colnames(pval_to_melt) # get the card column names
  df = data.frame("card_loc" = 1:length(idxarr_int)) # create a number sequence column
  pval_to_melt <- cbind(pval_to_melt, df) # add it to the position count
                                                    # dataframe 
  # reshape
  pval_melted <- melt(pval_to_melt, id.vars = ("card_loc"),
                                             measure.vars = card_names)
  
   # add a card_loc column to the df and reshape the data into a format that ggplot can handle
  card_names <- colnames(position_count_df) # get the card column names
  df = data.frame("card_loc" = 1:length(idxarr_int)) # create a number sequence column
  pos_count_to_melt <- cbind(position_count_df, df) # add it to the position count
                                                    # dataframe and save as new dataframe

  fig <- ggplot(pval_melted, aes(x=card_loc, y=variable, fill=value)) +
                      geom_tile() +
                      scale_fill_gradientn(limits = c(0, .1), colors = c("red", "white"), na.value = "white", name = "P Value") +
                      theme(text = element_text(size = 10))  



   return(fig)
}


figShufflePoisson <- function (shoes) {

  ordered_deck <- newdeck()
  ordered_deck<- c(ordered_deck, "CUT") # add the cut card to the end of the deck
  position_count_df <- NULL

  for (card in ordered_deck) {
    idxarr_logical <- shoes[[1]] %in% card  # returns an array of booleans
    idxarr_int <- as.integer(idxarr_logical)  # converts booleans to 0 1

    # NB that we can't directly name a column in a dataframe using a
    # variable containing a string with the column name
    # so we create the df with a placeholder column then rename it
    df <- data.frame("tmp" = idxarr_int)
    colnames(df)<- c(card)

    if (is.null(position_count_df)) {
       position_count_df <- df
    } else {
       position_count_df <- cbind(position_count_df, df)
    }

    for (s in seq(from = 9, to = length(shoes), by = 8)) {
       # now that we've set the column type, the R system will convert T/F to 1/0 under addition
       idxarr_logical <- shoes[[s]] %in% card  # returns an array of booleans
       idxarr_int <- as.integer(idxarr_logical)  # converts booleans to 0 1
       position_count_df[,card] <- position_count_df[,card] + idxarr_int
    }
  }


  # add a card_loc column to the df and reshape the data into a format that ggplot can handle
  pval_to_melt <- position_count_df # create a copy of the position count dataframe
  for (c in colnames(pval_to_melt)) {
      lambda <- 4;
      if (c == "CUT") { lambda <- 1 }
      counts <- position_count_df[,c]
      pvals <- (1-ppois(counts, lambda))  # use the Poisson dist
      pval_to_melt[,c] <- pvals
  }
 
  card_names <- colnames(pval_to_melt) # get the card column names
  df = data.frame("card_loc" = 1:length(idxarr_int)) # create a number sequence column
  pval_to_melt <- cbind(pval_to_melt, df) # add it to the position count
                                                    # dataframe 
  # reshape
  pval_melted <- melt(pval_to_melt, id.vars = ("card_loc"),
                                             measure.vars = card_names)
  
   # add a card_loc column to the df and reshape the data into a format that ggplot can handle
  card_names <- colnames(position_count_df) # get the card column names
  df = data.frame("card_loc" = 1:length(idxarr_int)) # create a number sequence column
  pos_count_to_melt <- cbind(position_count_df, df) # add it to the position count
                                                    # dataframe and save as new dataframe

  fig <- ggplot(pval_melted, aes(x=card_loc, y=variable, fill=value)) +
                      geom_tile() +
                      scale_fill_gradientn(limits = c(0, .1), colors = c("red", "white"), na.value = "white", name = "P Value")


   return(fig)

}

tableCardFrequency <- function(shoes) {

    if (!exists("hands_in_shoes")) { hands_in_shoes <- getHandsInShoes(shoes) }

    # Here we want to assay the frequency with which certain cards
    # appear in hands that have gone bust vs hands that have not

    # create a data frame with the number of each card type in each hand and the outcome of the hand
    busted_card_count <- data.frame("BUST" = numeric(), "aces" = numeric(), "twos" = numeric(),
    		                    "threes" = numeric(), "fours" = numeric(), "fives" = numeric(),
    		                    "sixes" = numeric(), "sevens" = numeric(), "eights" = numeric(),
				    "nines" = numeric(), "tens" = numeric(), "jacks" = numeric(),
				     "queens" = numeric(), "kings" = numeric());

    # create a new dataframe with the number of cards in
    # hands that went bust or standed
    for (i in 1: length(hands_in_shoes[,1])) {
        h <- hands_in_shoes[i,]
        if (h$OUTCOME == "BUST" | h$OUTCOME == "DLR_BUST") {
	   busted_card_count[i, "BUST"] <- TRUE
	} else {
	   busted_card_count[i, "BUST"] <- FALSE
	 }

	# get the cards in the player/dealer hand
	cards <- h$UP_CARDS[[1]]
	if (h$DN_CARD != "") {
	  cards <- c(h$DN_CARD, cards)
	}

	# convert to count of aces, twos, etc
	num_each <- countCardsByDenomination(cards)

	# add this row to the bust_card_count dataframe
	busted_card_count[i, 2:14] <- num_each
    }

    busted_card_count_freq <- busted_card_count[1:2,]
    busted_card_count_freq[1,] <- colSums(busted_card_count[busted_card_count$BUST == 1,])
    busted_card_count_freq[2,] <- colSums(busted_card_count[busted_card_count$BUST == 0,])

    library(reshape2)
    df1 <- data.frame("CARD" = 1:13, "COUNT" = melt(busted_card_count_freq[1,2:14])[,2], "OUTCOME" = rep("BUST",13))
    df2 <- data.frame("CARD" = 1:13, "COUNT" = melt(busted_card_count_freq[2,2:14])[,2], "OUTCOME" = rep("STAND",13))
    df <- rbind(df1, df2)

    return (df)

}


figCardFrequency <- function(shoes) {

    if (!exists("hands_in_shoes")) { hands_in_shoes <- getHandsInShoes(shoes) }
    
    # Here we want to assay the frequency with which certain cards
    # appear in hands that have gone bust vs hands that have not

    # create a data frame with the number of each card type in each hand and the outcome of the hand
    busted_card_count <- data.frame("BUST" = numeric(), "aces" = numeric(), "twos" = numeric(),
    		                    "threes" = numeric(), "fours" = numeric(), "fives" = numeric(),
    		                    "sixes" = numeric(), "sevens" = numeric(), "eights" = numeric(),
				    "nines" = numeric(), "tens" = numeric(), "jacks" = numeric(),
				     "queens" = numeric(), "kings" = numeric());

    # create a new dataframe with the number of cards in
    # hands that went bust or standed
    for (i in 1: length(hands_in_shoes[,1])) {
        h <- hands_in_shoes[i,]
        if (h$OUTCOME == "BUST" | h$OUTCOME == "DLR_BUST") {
	   busted_card_count[i, "BUST"] <- TRUE
	} else {
	   busted_card_count[i, "BUST"] <- FALSE
	 }

	# get the cards in the player/dealer hand
	cards <- h$UP_CARDS[[1]]
	if (h$DN_CARD != "") {
	  cards <- c(h$DN_CARD, cards)
	}

	# convert to count of acess, twos, etc
	num_each <- countCardsByDenomination(cards)

	# add this row to the bust_card_count dataframe
	busted_card_count[i, 2:14] <- num_each
    }

    busted_card_count_freq <- busted_card_count[1:2,]
    busted_card_count_freq[1,] <- colSums(busted_card_count[busted_card_count$BUST == 1,])
    busted_card_count_freq[2,] <- colSums(busted_card_count[busted_card_count$BUST == 0,])
    num_bust_cards <- sum(busted_card_count_freq[1,2:14])
    num_stand_cards <- sum(busted_card_count_freq[2,2:14])

    busted_card_count_freq[1,] <- busted_card_count_freq[1,] / num_bust_cards;
    busted_card_count_freq[2,] <- busted_card_count_freq[2,] / num_stand_cards;	

    library(reshape2)
    df1 <- data.frame("CARD" = 1:13, "FREQ" = melt(busted_card_count_freq[1,2:14])[,2], "OUTCOME" = rep("BUST",13))
    df2 <- data.frame("CARD" = 1:13, "FREQ" = melt(busted_card_count_freq[2,2:14])[,2], "OUTCOME" = rep("STAND",13))
    df <- rbind(df1, df2)


    fig <- ggplot(df, aes(x=factor(CARD), y=FREQ, fill=OUTCOME)) +
                geom_bar(stat='identity', position='dodge') +
		geom_segment(aes(x = 0, xend = 14, y = 0.077, yend = 0.077),
                    		   color="darkgrey", linetype="dashed") +
                ylab("Frequency") +
		scale_y_continuous(label = scales::percent) +
                scale_x_discrete("Card Denomination",
                                 breaks = 1:13,
                                 labels=c("aces", "2's", "3's", "4's", "5's", "6's", "7's", "8's",
                                          "9's", "10's", "jacks", "queens", "kings")) +
	        theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))

     return(fig)
}


figCardFrequencyScatter <- function(shoes) {

    if (!exists("hands_in_shoes")) { hands_in_shoes <- getHandsInShoes(shoes) }

    # Here we want to assay the frequency with which certain cards
    # appear in hands that have gone bust vs hands that have not

    # create a data frame with the number of each card type in each hand and the outcome of the hand
    busted_card_count <- data.frame("BUST" = numeric(), "aces" = numeric(), "twos" = numeric(),
    		                    "threes" = numeric(), "fours" = numeric(), "fives" = numeric(),
    		                    "sixes" = numeric(), "sevens" = numeric(), "eights" = numeric(),
				    "nines" = numeric(), "tens" = numeric(), "jacks" = numeric(),
				     "queens" = numeric(), "kings" = numeric());

    # create a new dataframe with the number of cards in
    # hands that went bust or standed
    for (i in 1: length(hands_in_shoes[,1])) {
        h <- hands_in_shoes[i,]
        if (h$OUTCOME == "BUST" | h$OUTCOME == "DLR_BUST") {
	   busted_card_count[i, "BUST"] <- TRUE
	} else {
	   busted_card_count[i, "BUST"] <- FALSE
	 }

	# get the cards in the player/dealer hand
	cards <- h$UP_CARDS[[1]]
	if (h$DN_CARD != "") {
	  cards <- c(h$DN_CARD, cards)
	}

	# convert to count of aces, twos, etc
	num_each <- countCardsByDenomination(cards)

	# add this row to the bust_card_count dataframe
	busted_card_count[i, 2:14] <- num_each
    }

    busted_card_count_freq <- busted_card_count[1:2,]
    busted_card_count_freq[1,] <- colSums(busted_card_count[busted_card_count$BUST == 1,])
    busted_card_count_freq[2,] <- colSums(busted_card_count[busted_card_count$BUST == 0,])
    num_bust_cards <- sum(busted_card_count_freq[1,2:14])
    num_stand_cards <- sum(busted_card_count_freq[2,2:14])

    busted_card_count_freq[1,] <- busted_card_count_freq[1,] / num_bust_cards;
    busted_card_count_freq[2,] <- busted_card_count_freq[2,] / num_stand_cards;	

    cards = c("aces", "2's", "3's", "4's", "5's", "6's", "7's", "8's",
              "9's", "10's", "jacks", "queens", "kings")
    df <- data.frame("CARD"=character(), "BUST_FREQ" = numeric(), "STAND_FREQ" = numeric())
    for (i in 1:13) {
    	df[i, "CARD"] <- cards[i]
	df[i,"BUST_FREQ"] <- busted_card_count_freq[1,1+i]
	df[i,"STAND_FREQ"] <- busted_card_count_freq[2,1+i]
     }

     print(df)

     fig <- ggplot(df, aes(x=BUST_FREQ, y=STAND_FREQ, color=CARD)) +
                geom_point() +
		scale_y_continuous(label = scales::percent, limits = c(0.06, .095)) +
		scale_x_continuous(label = scales::percent, limits = c(0.045, .095)) +
		geom_segment(aes(x = 0.045, xend = .095, y = 0.077, yend = 0.077),
                    		   color="darkgrey", linetype="dashed") +
		geom_segment(aes(x = 0.077, xend = 0.077), y = 0.045, yend = .095,
                    		   color="darkgrey", linetype="dashed") +				   
		ylab("Freq in Stand Hands") +
		xlab("Freq in Bust Hands")

     return(fig)

}



figCardFreqCluster <- function(shoes) {
    if (!exists("hands_in_shoes")) { hands_in_shoes <- getHandsInShoes(shoes) }

    # Here we want to assay the frequency with which certain cards
    # appear in hands that have gone bust vs hands that have not

    # create a data frame with the number of each card type in each hand and the outcome of the hand
    busted_card_count <- data.frame("BUST" = numeric(), "aces" = numeric(), "twos" = numeric(),
    		                    "threes" = numeric(), "fours" = numeric(), "fives" = numeric(),
    		                    "sixes" = numeric(), "sevens" = numeric(), "eights" = numeric(),
				    "nines" = numeric(), "tens" = numeric(), "jacks" = numeric(),
				     "queens" = numeric(), "kings" = numeric());

    # create a new dataframe with the number of cards in
    # hands that went bust or standed
    for (i in 1: length(hands_in_shoes[,1])) {
        h <- hands_in_shoes[i,]
        if (h$OUTCOME == "BUST" | h$OUTCOME == "DLR_BUST") {
	   busted_card_count[i, "BUST"] <- TRUE
	} else {
	   busted_card_count[i, "BUST"] <- FALSE
	 }

	# get the cards in the player/dealer hand
	cards <- h$UP_CARDS[[1]]
	if (h$DN_CARD != "") {
	  cards <- c(h$DN_CARD, cards)
	}

	# convert to count of aces, twos, etc
	num_each <- countCardsByDenomination(cards)

	# add this row to the bust_card_count dataframe
	busted_card_count[i, 2:14] <- num_each
    }

    busted_card_count_freq <- busted_card_count[1:2,]
    busted_card_count_freq[1,] <- colSums(busted_card_count[busted_card_count$BUST == 1,])
    busted_card_count_freq[2,] <- colSums(busted_card_count[busted_card_count$BUST == 0,])
    num_bust_cards <- sum(busted_card_count_freq[1,2:14])
    num_stand_cards <- sum(busted_card_count_freq[2,2:14])

    busted_card_count_freq[1,] <- busted_card_count_freq[1,] / num_bust_cards;
    busted_card_count_freq[2,] <- busted_card_count_freq[2,] / num_stand_cards;	

    cards = c("aces", "2's", "3's", "4's", "5's", "6's", "7's", "8's",
              "9's", "10's", "jacks", "queens", "kings")
    df <- data.frame("CARD"=character(), "BUST_FREQ" = numeric(), "STAND_FREQ" = numeric())
    for (i in 1:13) {
    	df[i, "CARD"] <- cards[i]
	df[i,"BUST_FREQ"] <- busted_card_count_freq[1,1+i]
	df[i,"STAND_FREQ"] <- busted_card_count_freq[2,1+i]
     }

     ##############
     # Perform the k-means clustering and visualize the
     # resulting clusters, note the required packages
     #############

     library(tidyverse)
     library(cluster)
     library(factoextra)

     # first we need to scale the data in the BUST_FREQ and STAND_FREQ columns
     df_scaled <- df;
     df_scaled[,2] <- scale(df[,2])
     df_scaled[,3] <- scale(df[,3])

     # run the clustering command
     k3 <- kmeans(df[,2:3], centers = 4, nstart = 25);

     # visualize the results     
     fig <- fviz_cluster(k3, data = df[,2:3]) 
     return(fig)    
}



figIndexBoxPlot <- function(shoes) {
    if (!exists("hands_in_shoes")) { hands_in_shoes <- getHandsInShoes(shoes) }
    rounds <- data.frame("IS_BUST" = numeric(), "NUM_ACES" = numeric(), "NUM_TENCARDS" = numeric(),
       	  	     "NUM_4578" = numeric(), "NUM_2369" = numeric(),
		     "SHOE_NUM" = numeric (), "HAND_NUM" = numeric(), "OUTCOME"=character())
    last_shoe <- -1
    last_hand <- -1
    for (h in 1:length(hands_in_shoes[,1])) {
    	if (hands_in_shoes[h,"SHOE_NUM"] != last_shoe ||
	    hands_in_shoes[h,"HAND_NUM"] != last_hand)
	{
	    last_shoe <- hands_in_shoes[h, "SHOE_NUM"]
	    last_hand <- hands_in_shoes[h, "HAND_NUM"]		
	    cards <- getCardsInRound(getShoeFromShoes(shoes, last_shoe), last_hand)
	    card_count <- countCardsByDenomination(cards)
	    n_aces <- card_count[1]
	    n_tens <- sum(card_count[10:13])
	    n_other <- sum(card_count[2:9])
	    n_2369 <- card_count[2] + card_count[3] + card_count[6] + card_count[9]
	    n_4578 <- card_count[4] + card_count[5] + card_count[7] + card_count[8] 
	}
	rounds[h, "IS_BUST"] <- 0	    
	rounds[h, "NUM_ACES"] <- n_aces
	rounds[h, "NUM_TENCARDS"] <- n_tens
	rounds[h, "NUM_4578"] <- n_4578
	rounds[h, "NUM_2369"] <- n_2369
	rounds[h, "SHOE_NUM"] <- last_shoe
	rounds[h, "HAND_NUM"] <- last_hand
	rounds[h, "OUTCOME"] <- "STAND"	    
	outcome <- hands_in_shoes[h, "OUTCOME"] 
	if (outcome == "BUST" || outcome == "DLR_BUST") {
	     rounds[h, "IS_BUST"] <- 1
	     rounds[h, "OUTCOME"] <- "BUST"	    
	}
	
    }

    #my.model <- glm(IS_BUST ~ NUM_ACES + NUM_TENCARDS + NUM_4578 + NUM_2369,
    #	     	    data = rounds, family = "binomial", maxit = 1000)

    print(summary(rounds))
    
    rounds.melted <- melt(rounds, id=c("IS_BUST", "OUTCOME", "SHOE_NUM", "HAND_NUM"))

    fig <- ggplot(rounds.melted, aes(x=variable, y=value, fill=OUTCOME)) +
           geom_boxplot() +
	   labs(y="Sum of all cards in round") +
           theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), axis.title.x = element_blank())
	   
    return(fig)
}






########################### TESTING FXNS ########################

loadvars <- function() {
	 load("shoes.RData")
	 shoes <<- shoes
	 hands_in_shoes <<- getHandsInShoes(shoes)
}


testfxn2 <- function(hands_in_shoes) {

    # Here we want to assay the frequency with which certain cards
    # appear in hands that have gone bust vs hands that have not

    # create a data frame with the number of each card type in each hand and the outcome of the hand
    busted_card_count <- data.frame("BUST" = numeric(), "aces" = numeric(), "twos" = numeric(),
    		                    "threes" = numeric(), "fours" = numeric(), "fives" = numeric(),
    		                    "sixes" = numeric(), "sevens" = numeric(), "eights" = numeric(),
				    "nines" = numeric(), "tens" = numeric(), "jacks" = numeric(),
				     "queens" = numeric(), "kings" = numeric());

    # create a new dataframe with the number of cards in
    # hands that went bust or standed
    for (i in 1: length(hands_in_shoes[,1])) {
        h <- hands_in_shoes[i,]
        if (h$OUTCOME == "BUST" | h$OUTCOME == "DLR_BUST") {
	   busted_card_count[i, "BUST"] <- TRUE
	} else {
	   busted_card_count[i, "BUST"] <- FALSE
	 }

	# get the cards in the player/dealer hand
	cards <- h$UP_CARDS[[1]]
	if (h$DN_CARD != "") {
	  cards <- c(h$DN_CARD, cards)
	}

	# convert to count of aces, twos, etc
	num_each <- countCardsByDenomination(cards)

	# add this row to the bust_card_count dataframe
	busted_card_count[i, 2:14] <- num_each
    }

    busted_card_count_freq <- busted_card_count[1:2,]
    busted_card_count_freq[1,] <- colSums(busted_card_count[busted_card_count$BUST == 1,])
    busted_card_count_freq[2,] <- colSums(busted_card_count[busted_card_count$BUST == 0,])
    num_busts <- busted_card_count_freq[1,1]
    num_stands <- length(busted_card_count[,1]) - num_busts
    busted_card_count_freq[1,] <- busted_card_count_freq[1,] / num_busts;
    busted_card_count_freq[2,] <- busted_card_count_freq[2,] / num_stands;	

    library(reshape2)
    df1 <- data.frame("CARD" = 1:13, "FREQ" = melt(busted_card_count_freq[1,2:14])[,2], "OUTCOME" = rep("BUST",13))
    df2 <- data.frame("CARD" = 1:13, "FREQ" = melt(busted_card_count_freq[2,2:14])[,2], "OUTCOME" = rep("STAND",13))
    df <- rbind(df1, df2)


    fig <- ggplot(df, aes(x=factor(CARD), y=FREQ, fill=OUTCOME)) +
                geom_bar(stat='identity', position='dodge') +
                ylab("Frequency") +
                scale_x_discrete("Card Denomination",
                                 breaks = 1:13,
                                 labels=c("aces", "2's", "3's", "4's", "5's", "6's", "7's", "8's",
                                          "9's", "10's", "jacks", "queens", "kings"))


    return(busted_card_count)

}


testfxn4 <- function(hands_in_shoes) {

    library(ggplot2)
    library(cowplot)

    # what is the frequency of card denominations in the hand when
    # the delaer busts or stands?
    bust_card_freq <- vector(mode = "numeric", length = 13)
    stand_card_freq <- vector(mode = "numeric", length = 13)

    dlr_busts <- hands_in_shoes[hands_in_shoes$OUTCOME == "DLR_STAND",]
    t <- length(dlr_busts[,1]) + length(dlr_stands[,1])

    num_per_hand <- data.frame("BUST" = numeric(t), "ACES" = numeric(t), "TWOS" = numeric(t),
    		    	       "THREES" = numeric(t), "FOURS" = numeric(t), "FIVES" = numeric(t),
			       "SIXES" = numeric(t), "SEVENS" = numeric(t), "EIGHTS" = numeric(t),
			       "NINES" = numeric(t), "TENS" = numeric(t), "JACKS" = numeric(t),
			       "QUEENS" = numeric(t), "KINGS" = numeric(t));
    deck <- newdeck();
    for (i in 1:length(dlr_busts[,1])) {
    	bust_cards <- c(dlr_busts[i, "DN_CARD"], dlr_busts[[i, "UP_CARDS"]])
	bust_deck_idx <- match(bust_cards, deck)    
	num_per_hand[i, "BUST"] <- 1
	for(c in bust_deck_idx) {
	   idx <- ((c-1) %% 13) + 1
	   bust_card_freq[idx] <- bust_card_freq[idx] + 1	    
	   num_per_hand[i, idx+1] <- num_per_hand[i, idx+1] + 1
	 }
    }
    print(bust_card_freq)
    bust_card_freq <- bust_card_freq / sum(bust_card_freq);

    row_oset <- length(dlr_busts[,1])
    for (i in 1:length(dlr_stands[,1])) {
    	stand_cards <- c(dlr_stands[i, "DN_CARD"], dlr_stands[[i, "UP_CARDS"]])
	stand_deck_idx <- match(stand_cards, deck)    
	num_per_hand[i+row_oset, "BUST"] <- 0
	for(c in stand_deck_idx) {
	   idx <- ((c-1) %% 13) + 1
	   stand_card_freq[idx] <- stand_card_freq[idx] + 1	    
	   num_per_hand[i+row_oset, idx+1] <- num_per_hand[i+row_oset, idx+1] + 1
	}	
    }
    print(stand_card_freq)
    stand_card_freq <- stand_card_freq / sum(stand_card_freq);    

    print(bust_card_freq)
    print(stand_card_freq)

    # perform a logistic regression to see which cards are most significantly
    # associated with the risk of going bust
    my.model <- glm(BUST ~ ACES + TWOS + THREES + FOURS + FIVES + SIXES + SEVENS + EIGHTS + NINES + TENS + JACKS + QUEENS + KINGS,
    	     	    data = num_per_hand, family = "binomial", maxit = 1000)
    #my.model <- glm(BUST ~ ACES + SIXES + SEVENS + TENS + JACKS + QUEENS + KINGS,
    #	     	    data = num_per_hand, family = "binomial", maxit = 1000)		    

    summary(my.model)

    df1 <- data.frame("CARD" = 1:13, "FREQ" = bust_card_freq, "OUTCOME" = rep("BUST",13))
    df2 <- data.frame("CARD" = 1:13, "FREQ" = stand_card_freq, "OUTCOME" = rep("STAND",13))    
    df3 <- rbind(df1, df2)
    
    fig <- ggplot(df3, aes(x=factor(CARD), y=FREQ, fill=OUTCOME)) +
    		geom_bar(stat='identity', position='dodge') +
                ylab("Frequency") +
		scale_x_discrete("Card Denomination",
				 breaks = 1:13,
		                 labels=c("aces", "2's", "3's", "4's", "5's", "6's", "7's", "8's",
				          "9's", "10's", "jacks", "queens", "kings"))

    return(fig)
}








########################### END TESTING FXNS #################################



#### FXNS IN DEVELOPMENT  ####################################################################


getCardsAndOutcomes <- function(shoes, hands_in_shoes) {
	 ## calc some statistics on the cards played in the hand
	 cards_and_outcomes <- data.frame("SHOE_NUM" = numeric(), "HAND_NUM" = numeric(),
	 		                  "PLAYER" = numeric(), "OUTCOME" = character(),
					  "CARDS_PLAYED" = I(list()),
					  "CARDS_MEAN" = numeric(),
					  "CARDS_SUM" = numeric(),
					  "CARDS_NUM_TENCARDS" = numeric(),
					  "CARDS_NUM_LOWCARDS" = numeric(),
					  "CARDS_NUM_TEN_AND_LOWCARDS" = numeric(),
					  "CARDS_HIGH_LOW_RATIO" = numeric(),
					  "CARDS_HIGH_LOW_DIFFERENCE" = numeric(),
					  "CARDS_DEALT_UPCARD_SUM" = numeric(),
					  "CARDS_MEAN_VAL_REMAINING_SHOE" = numeric())


         last_shoe <- -1
	 last_hand <- -1
	 for (i in 1:length(hands_in_shoes[,"SHOE_NUM"])) {

	     shoe_num = hands_in_shoes[i, "SHOE_NUM"];
	     hand_num = hands_in_shoes[i, "HAND_NUM"];
	     if (shoe_num != last_shoe || hand_num != last_hand) {
	     	shoe <- getShoeFromShoes(shoes, shoe_num)
	     	all_cards_played <- getCardsPlayed(shoe, hand_num)
		all_cards_played[all_cards_played == 11] <- 1  # convert aces from 11 to 1
		cards_mean <- mean(all_cards_played)
		cards_sum <- sum(all_cards_played)
		cards_num_facecards <- sum(match(all_cards_played, 10, nomatch = 0))
		cards_num_lowcards <- length(all_cards_played[all_cards_played < 5])
		mean_cards_remaining_in_shoe <- getMeanCardsRemainingInShoe(shoe, hand_num)
		last_shoe <- shoe_num
		last_hand <- hand_num
	     }
	     cards_and_outcomes[i, "SHOE_NUM"] <- last_shoe
	     cards_and_outcomes[i, "HAND_NUM"] <- last_hand
	     cards_and_outcomes[i, "PLAYER"] <- hands_in_shoes[i, "PLAYER"]
	     cards_and_outcomes[i, "OUTCOME"] <- hands_in_shoes[i, "OUTCOME"]
	     cards_and_outcomes[i,]$CARDS_PLAYED <- list(all_cards_played)
	     cards_and_outcomes[i, "CARDS_MEAN"] <- cards_mean
	     cards_and_outcomes[i, "CARDS_SUM"] <- cards_sum
	     cards_and_outcomes[i, "CARDS_NUM_TENCARDS"] <- cards_num_facecards
	     cards_and_outcomes[i, "CARDS_NUM_LOWCARDS"] <- cards_num_lowcards
	     cards_and_outcomes[i, "CARDS_NUM_TEN_AND_LOWCARDS"] <- cards_num_lowcards + cards_num_facecards
	     cards_and_outcomes[i, "CARDS_HIGH_LOW_RATIO"] <- cards_num_facecards / cards_num_lowcards
	     cards_and_outcomes[i, "CARDS_HIGH_LOW_DIFFERENCE"] <- cards_num_facecards - cards_num_lowcards
	     cards_and_outcomes[i, "CARDS_MEAN_VAL_REMAINING_SHOE"] <- mean_cards_remaining_in_shoe
	     if (hands_in_shoes[i, "PLAYER"] == 0) {
	        cards = hands_in_shoes[i, "UP_CARDS"][[1]][1]
	     } else {
	        cards = hands_in_shoes[i, "UP_CARDS"][[1]][1:2]
	     } 
	     cards_and_outcomes[i, "CARDS_DEALT_UPCARD_SUM"] <- sum(convertCardsToNumeric(cards))
	 }

	 return (cards_and_outcomes)
}



enroll_step = function(excel_df, cohorts_df) {
    library(ggplot2)
    library(lubridate)
    times <- excel_df[,6]
    cum_events <- 1:length(times)
    DF <- data.frame(cum_events, times)
    p <- ggplot(DF, aes(times, cum_events)) + geom_step() 
    max_y = sum(cohorts_df[,2])    
    print(max_y)
    p + scale_y_continuous(name= "Patients Enrolled", limits = c(0, max_y))
    d <- par("usr")
    p + scale_x_continuous(name = "Date", limits = c(d[1], d[2]+70))
    p + scale_x_date(date_labels = "%B", date_breaks = "1 month") +
      theme(axis.text.x = element_text(angle=45, hjust = 1))
    
    #ggplot(times, cum_events)
    #lines(times, cum_events, type = "s")
    #scale_x_date(date_labels="%B")
}


loglog = function(X, Y) {
  library(ggplot2)   
  ggplot(Y, X, ylim = c(0,40), ylab="dose", xlab="concentration", log="x", axes=F, type = "b") 
  axis(1, at=log10Tck('x','major'), tcl= 0.4) # bottom
  axis(1, at=log10Tck('x','minor'), tcl= 0.2, labels=NA) # bottom
  axis(2) # normal y axis
  box()
}

log10Tck <- function(side, type){
  lim <- switch(side, 
                x = par('usr')[1:2],
                y = par('usr')[3:4],
                stop("side argument must be 'x' or 'y'"))
  at <- floor(lim[1]) : ceiling(lim[2])
  return(switch(type, 
                minor = outer(1:9, 10^(min(at):max(at))),
                major = 10^at,
                stop("type argument must be 'major' or 'minor'")
  ))
}


