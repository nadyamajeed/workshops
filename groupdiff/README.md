This folder contains four datasets, and four example R scripts within two files.

# ttest-between.csv

ttest-between.csv is taken from real data collected from 385 women in the US (Majeed et al., unpublished manuscript). In a between-subjects experiment, women were randomly assigned to either a gender-math stereotype threat condition or to a control condition. Self-rated task anxiety was measured after the manipulation.

Variables:
  * id (anonymous participant identifier)
  * condition (threat or control)
  * taskAnxiety (task-related anxiety; continuous from 1.00 to 7.00 where higher values indicate more anxiety)

# ttest-within.csv

ttest-within.csv is taken from real data collected from 82 undergraduates in Singapore (Study 1 from [Quek et al., 2021](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7911752/)). In a within-subject experiment, undergraduates underwent both a brief mindfulness breathing experimental condition and a mind-wandering control condition in a counterbalanced order across two sessions. Self-rated stress was measured after each manipulation.

Variables:
  * id (anonymous participant identifier)
  * stress_ctrl (stress in control condition; continuous from 1.00 to 5.00 where higher values indicate more stress)
  * stress_exp (stress in experimental condition; continuous from 1.00 to 5.00 where higher values indicate more stress)

# anova-between.csv

anova-between.csv is hypothethical data "collected" from 300 participants. In a 2 (sex: male vs. female) x 3 (drink: water vs. tea vs. coffee) between-subjects quasi-experiment, participants were randomly assigned to consume a drink (water, tea, or coffee) and reported their sex (male or female). Self-rated alertness was measured after consuming the drink.

Variables:
  * id (anonymous participant identifier)
  * sex (male or female)
  * condition (drink; water or tea or coffee)
  * alertness (self-reported alertness; continuous from 1.00 to 7.00 where higher values indicate more alertness)

The dataset was specifically created for teaching purposes for DawnLab @ SMU. R Code was adapted from exercises adapted by Huey Woon Lee (hwlee@smu.edu.sg), originally created by Kevin P. McIntyre (kmcintyr@trinity.edu; https://sites.google.com/view/openstatslab/home).

# anova-within.csv

anova-within.csv is taken from real data collected from 62 undergraduates in Singapore (from Majeed et al., 2021; [published version](https://doi.org/10.1037/pmu0000283), [open-access preprint version](https://psyarxiv.com/cfk3d)). In a within-subject experiment, undergraduates underwent each of three conditions (listening to happy music vs. listening to sad music vs. listening to pink noise) in a randomised order across three weeks (five nights per condition). Self-rated stress was measured each morning after listening to the audio. For the purposes of the current ANOVA demonstration, the multilevel data (maximum of 5 observations per condition per participant) have been summarised into one data point per condition per participant (i.e., total 62 × 3 = 186 observations).

Variables:
  * id (anonymous participant identifier)
  * condition (audio; happy music or sad music or pink noise)
  * stress (self-reported stress; continuous from 1.00 to 10.00 where higher values indicate more stress)
