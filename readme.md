## MRP using the 2021 England and Wales Census and the British Election Study Internet Panel

This is a quick implementation of Multilevel Regression and Post-stratification for England and Wales that I created as a weekend project.
It uses breakdowns of consituency population by age, sex, and highest qualification from the 2021 England and Wales Censsu along with past election results to generate predictions for how a constituency would vote as of December 2022.

## How to use
In order to run the scripts properly, you need to download three files

- A csv breakdown of population by age, sex, and highest qualification in the 2021 Census using the [ONS's custom dataset builder](https://www.ons.gov.uk/datasets/create)
- The wave 1-24 BES internet panel [stata file](https://www.britishelectionstudy.com/wp-content/uploads/2023/07/BES2019_W24_Panel_v24.0.dta.zip) (requires free account)
- The 2010-2019 BES Constituency Results with Census and Candidate Data [stata file](https://www.britishelectionstudy.com/wp-content/uploads/2022/01/BES-2019-General-Election-results-file-v1.1.dta) (requires free account)

Place these all in the [data directory](/data) and then you can run the scripts in the [src directory](/src).

[val_2019_mrp.R](/src/val_2019_mrp.R) validates the method against the actual 2019 general election results from the 2010-2019 Constituency results file and [W24_mrp.R](/src/W24_mrp.R) does MRP for wave 24 of the BES internet panel (fielded in mid-December 2022).
Plots comparing the predictions with the actual 2019 election results are saved to the [output directory](/output). You can also place shapefiles for the constituency boundaries for creating chorpleths in the [boundaries folder](/boundaries).