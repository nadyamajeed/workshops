# sem.csv

sem.csv is taken from real data collected from 176 undergraduates in Singapore (Majeed et al., unpublished manuscript). In a correlational study, undergraduates provided their demographic data, answered various questions related to their level of musical experience, and completed nine cognitive tasks assessing their executive functioning.

Variables:
  * female (sex, dummy-coded with 0 = male, 1 = female)
  * age (age in years)
  * income (household income; continuous from 1 to 9 where higher values indicate higher household income)
  * ladder (social ladder ranking; continuous from 1 to 10 where higher values indicate higher standing in society)
  * ef_XX_XX (executive functioning)
    * ef_ic_XX (inhibitory control; continuous binned scores where 1 = best possible score, 20 = worst possible score)
      * ef_ic_af (arrow flanker task)
      * ef_ic_cf (colour flanker task)
      * ef_ic_ef (eriksen flanker task)
    * ef_ts_XX (task-switching ability; continuous binned scores where 1 = best possible score, 20 = worst possible score)
      * ef_ts_al (animacy locomotion task)
      * ef_ts_cs (colour shape task)
      * ef_ts_mp (magnitude parity task)
    * ef_wm_XX (working memory capacity; continuous PCU scores where 0 = worst possible score, 1 = best possible score)
      * ef_wm_os (operation span task)
      * ef_wm_rs (rotation span task)
      * ef_wm_ss (symmetry span task)
  * musicLessons (years of formal music lessons) 
  * musicPractice (years of music practice)
  * OMSI_COMPUTED (musical experience as measured by the OMSI)
