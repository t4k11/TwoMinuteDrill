# TwoMinuteDrill

### How did your team perform in the two minute drill this season?

---

There were **172** attempted late game comeback drives in the 2018 season.

"Late game comeback drive"  is defined here as:

1. Team with possession trailing by between 0 and 8 points
1. Drive starting with less than 5 minutes to go in regulation
1. Drive starting with more than 40 yards to go

In the following graphs, **each point represents the first play of a drive**, with time remaining at the start of the drive on the x-axis and score differential on the y-axis. The axes are reversed to put drives that are more likely to result in a win in the lower left corner and ones that are less likely in the top right.

The points are color coded by the outcome of the game in regulation, with "Tie" meaning that the game went into overtime. I used the outcome of the game rather than the outcome of the drive itself because 1.) that data is wasn't readily available, and 2.) Clock management is important, and not leaving enough time on the clock for your opponent to mount a comeback is part of executing a late game offense.

---

# [Every Attempted Late Game Comeback Drive from the 2018 Season](https://i.imgur.com/HX9A8Yx.jpg)

[**Late Game Comeback Drives Broken Down by Team**](https://imgur.com/a/gQCUI8r)

---

##### Comeback Drives by Point Differential

Point Diff. | Loss| Tie| Win| Win Pct.
---|---|---|---|---|
**-8**|	6|	4|	|	0.20|
**-7**|	24|	6|	3|	0.18|
**-6 to -4**|	27|	2|	9|	0.26|
**-3**|	23|	3|	9|	0.30|
**-1 to -2**|	12|	|	10|	0.45|
**0**|	4|	19|	11|	0.60|
**Total:**| **96**|	**34**| **42**| **0.34**

##### Comeback Drives By Team

Team| Loss| Tie| Win| Win Pct.
---|---|---|---|---|
ARI|	2|	|	1|	0.33|
ATL|	3|	1|	|	0.13|
BAL|	4|	3|	|	0.21|
BUF|	4|	|	1|	0.2|
CAR|	8|	|	1|	0.11|
CHI|	2|	3|	|	0.3|
CIN|	6|	|	2|	0.25|
CLE|	3|	4|	|	0.29|
DAL|	2|	4|	5|	0.64|
DEN|	5|	|	2|	0.29|
DET|	2|	|	|	0|
GB|	1|	2|	3|	0.67|
HOU| 3|	2|	2|	0.43|
IND|	4|	1|	2|	0.36|
JAX|	3|	|	|	0|
KC|	3|	1|	1|	0.3|
LAR|	1|	|	1|	0.5|
LAC|	|	|	2|	1|
MIA|	1|	2|	1|	0.5|
MIN|	1|	2|	|	0.33|
NE|	1|	|	1|	0.5|
NO|	1|	|	2|	0.67|
NYG|	4|	|	1|	0.2|
NYJ|	7|	1|	1|	0.17|
OAK|	1|	2|	4|	0.71|
PHI|	4|	2|	2|	0.38|
PIT|	3|	1|	2|	0.42|
SEA|	3|	2|	2|	0.43|
SF|	6|	1|	|	0.07|
TB|	4|	|	1|	0.2|
TEN|	1|	|	2|	0.67|
WAS| 3|	|	|	0|
**Total:**| **96**|	**34**| **42**| **0.34**

---

Note:

The "Win Probability" shading from the main plot was determined using a logistic regression model. This model creates a smooth gradient of probabilities and is not able to reflect the changes in win probability that occur when you cross important score thresholds like -3 and -7. However, I intended for this to be mainly an exercise in visualization not predictive modelling, so logistic regression was chosen because it looks nicest :) 
