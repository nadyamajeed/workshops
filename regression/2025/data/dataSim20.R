##### start of code #####

data.frame(
  id = 1:1000
) |>
  dplyr::mutate(
    # sex - equal male & female
    sex = sample(
      c("M", "F"), 
      size = 1000, 
      replace = TRUE),
    # major
    major = sample(
      c("PSYC", "PSYC", "POSC", "SOCG"),
      size = 1000,
      replace = TRUE),
    # have a cat? - 67% no, 33% yes
    haveCat = sample(
      c("No", "No", "Yes"),
      size = 1000,
      replace = TRUE),
    # have a dog? - 75% no, 25% yes
    haveDog = sample(
      c("No", "No", "No", "Yes"),
      size = 1000,
      replace = TRUE),
    # height in cm - normal dist ish between 144 and 194 incl
    heightCM = 
      rnorm(1000) + 2*(sex=="M"),
    heightCM = (heightCM-min(heightCM)) / (max(heightCM)-min(heightCM)),
    heightCM = round(heightCM*(194-144)) + 144,
    # years in uni
    yearsInUni = sample(
      c(1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 5),
      size = 1000,
      replace = TRUE
    ),
    # extraversion - normal dist ish between 1 to 7 incl
    extraversion = 
      rnorm(1000) + 2*(sex=="M") + 0.1*(heightCM) + 1*(yearsInUni) + 0.1*(major=="POSC") +
      1*(haveDog=="Yes") - 1*(haveCat=="Yes") + 2*(haveDog=="Yes")*(haveCat=="Yes"),
    extraversion = (extraversion-min(extraversion)) / (max(extraversion)-min(extraversion)),
    extraversion = round(extraversion*6) + 1,
    # optimism - normal dist ish between 1 to 7 incl
    optimism = 
      rnorm(1000) + 2*(sex=="M") - 1*(yearsInUni),
    optimism = (optimism-min(optimism)) / (max(optimism)-min(optimism)),
    optimism = round(optimism*6) + 1,
    # gratitude - normal dist ish between 1 to 7 incl
    gratitude = 
      rnorm(1000) + 2*(sex=="F") + 0.1*(major=="SOCG"),
    gratitude = (gratitude-min(gratitude)) / (max(gratitude)-min(gratitude)),
    gratitude = round(gratitude*6) + 1,
    # preferred food place - 33% Connexion, 33% Koufu, 33% Funan
    foodPreference = sample(
      c("Connexion", "Koufu", "Funan"),
      size = 1000,
      replace = TRUE),
    # spicy tolerance - normal dist ish between 1 to 7 incl
    spicyTolerance =
      rnorm(1000) + 0.2*(extraversion) + 2*(foodPreference=="Funan") + 0.1*(major=="SOCG"),
    spicyTolerance = (spicyTolerance-min(spicyTolerance)) / (max(spicyTolerance)-min(spicyTolerance)),
    spicyTolerance = round(spicyTolerance*6) + 1,
    # ladder rank - normal dist ish between 1 to 10 incl
    ladderRank = 
      rnorm(1000) + 1*(sex=="M") + 1*(optimism) + 0.1*(foodPreference=="Funan"),
    ladderRank = (ladderRank-min(ladderRank)) / (max(ladderRank)-min(ladderRank)),
    ladderRank = round(ladderRank*9) + 1,
    # fomo - normal dist ish between 1 to 7 incl
    fomo = 
      rnorm(1000) + 2*(haveCat=="N") + 4*(haveDog=="N") - 1*(gratitude),
    fomo = (fomo-min(fomo)) / (max(fomo)-min(fomo)),
    fomo = round(fomo*6) + 1,
    # conflict avoidance - normal dist ish between 1 to 7 incl
    conflictAvoidance = 
      rnorm(1000) - 1*(extraversion) + 1*(sex=="F") + -2*(spicyTolerance) + -1*(major=="SOCG"),
    conflictAvoidance = (conflictAvoidance-min(conflictAvoidance)) / (max(conflictAvoidance)-min(conflictAvoidance)),
    conflictAvoidance = round(conflictAvoidance*6) + 1,
    # happiness - dv, likert 1 to 7
    happiness = 
      rnorm(1000) +
      -0.04*(foodPreference=="Funan") +
      0.05*(heightCM) +
      0.10*(sex=="F") +
      0.20*(haveCat=="Yes") +
      0.30*(haveDog=="Yes") +
      -0.40*(fomo) +
      0.50*(optimism) +
      0.50*(gratitude) +
      0.60*(ladderRank),
    happiness = (happiness-min(happiness)) / (max(happiness)-min(happiness)),
    happiness = round(happiness*6) + 1,
    # stress - dv, likert 1 to 7
    stress = 
      rnorm(1000) +
      0.07*(foodPreference=="Funan") +
      0.10*(sex=="F") +
      0.20*(haveCat=="Yes") +
      0.30*(haveDog=="Yes") +
      0.40*(fomo) +
      -0.50*(optimism) +
      -0.10*(gratitude) +
      0.60*(yearsInUni),
    stress = (stress-min(stress)) / (max(stress)-min(stress)),
    stress = round(stress*6) + 1,
    # attitude towards tiktok - dv, likert 1 to 7
    tiktokLiking = 
      rnorm(1000) +
      0.10*(sex=="F") +
      -0.20*(haveCat=="Yes") +
      -0.30*(haveDog=="Yes") +
      0.40*(fomo) +
      0.10*(major=="SOCG"),
    tiktokLiking = (tiktokLiking-min(tiktokLiking)) / (max(tiktokLiking)-min(tiktokLiking)),
    tiktokLiking = round(tiktokLiking*6) + 1,
    # attitude towards chatgpt - dv, likert 1 to 7
    chatgptLiking = 
      rnorm(1000) +
      0.05*(sex=="F") +
      -0.40*(haveCat=="Yes") +
      -0.30*(haveDog=="Yes") +
      0.90*(fomo) +
      0.10*(major=="PSYC"),
    chatgptLiking = (chatgptLiking-min(chatgptLiking)) / (max(chatgptLiking)-min(chatgptLiking)),
    chatgptLiking = round(chatgptLiking*6) + 1,
    # meme knowledge - dv, likert 1 to 7
    memeKnowledge = 
      rnorm(1000) +
      0.75*(sex=="M") +
      0.50*(haveCat=="Yes") +
      -0.10*(haveDog=="Yes") +
      0.75*(fomo) +
      -0.10*(yearsInUni) +
      0.50*(stress) +
      5.00*(tiktokLiking) +
      1.00*(major=="POSC"),
    memeKnowledge = (memeKnowledge-min(memeKnowledge)) / (max(memeKnowledge)-min(memeKnowledge)),
    memeKnowledge = round(memeKnowledge*6) + 1,
    # cognitive ability - dv, likert 1 to 7
    cognitiveAbility = 
      rnorm(1000) +
      5*(yearsInUni) +
      -5*(stress) +
      1*(ladderRank) +
      -10*(chatgptLiking) +
      10*(memeKnowledge),
    cognitiveAbility = (cognitiveAbility-min(cognitiveAbility)) / (max(cognitiveAbility)-min(cognitiveAbility)),
    cognitiveAbility = round(cognitiveAbility*6) + 1
  ) |>
  write.csv("sim20.csv", row.names = FALSE)

##### end of code #####
