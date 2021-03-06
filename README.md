# Inside-Edge-PAA
A script to clean up Inside Edge fielding data and calculating Plays Above Average (PAA)
First conceived by Andrew Grant (@realandrewgrant on Twitter), this script uses Inside Edge fielding Data from Fangraphs.

Start by exporting the data off the fangraphs leaderboard; you'll need to select split seasons (even if you only want one season), otherwise you'll be missing a 'Season' column in the csv and this will throw off all column calls on the script by 1 (hint: if you split teams, players who've been traded midseason will have two entries for each team).  Create a variable 'filename' and set it to the name of the csv file you got from fangraphs.  Then run the script.  It will generate two data frames:
1) defense, which gives the inside edge info for each player.  For each players, the expected number of plays made by an average player at the same position with an identical number of plays of the same difficulties will be calculated.  The player's PAA is simply his plays made minus his expected plays made.  I've also added a few other columns for convenience; hard plays are the combined number of plays in the lowest three bins (Impossible, Remote, and Unlikely); Easy Plays are the total plays in the upper three bins (Even, Likely, and Routine).  Easy Play Percent is what it sounds like.
2) d.of this calculates PAA for outfields by looking at the average outfielder across all positions, instead of treating each outfield position separately. 

I've provided a csv file with 2015 data; PAA is given to one digit of precision but your guess is as good as mine on accuracy.
