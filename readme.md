## Data Sources

* Georgia Election Data
  * https://results.enr.clarityelections.com/GA/105369/web.264614/#/summary
    * Retrieved with retrieve_ga.py
      * Please note the Clarify package must come from https://github.com/openelections/clarify.
  * https://sos.ga.gov/admin/uploads/Active_Voters_by_Race_and_Gender_By_Congressional_as_of_November_1_2020.xlsx
    * The county xml files do not always have accurate precinct total registered voter data. For example, every precinct in Screven County reported 0 total voters. Paulding County numbers their precincts in the state's xlsx data and names them in the xml data. If you're modeling or mapping turnout, you must fix this data!
* Precinct-level Shapefile
  * Voting and Election Science Team, 2020, "2020 Precinct Shapefiles", https://doi.org/10.7910/DVN/XPW7T7, Harvard Dataverse, V5; ga_2020_general.zip [fileName]
    * At first glance, it looks like there are large precincts missing in Bryan County and Muscogee/Cherokee Counties. These areas are Fort Stewart and Fort Benning, respectively. They are certainly not missing.
  * Fulton County Shapefile
    * https://gisdata.fultoncountyga.gov/datasets/5f6cad4026b2483bbb3c5fe9c9266671_0?geometry=-86.209%2C33.444%2C-82.740%2C34.243
  

## How to Run (or Update) This

1. Download the 2020 shape files from Harvard's Voting and Election Science team. Unzip this directory into `ga_2020_general`.
2. Download Fulton county's shapefile and unzip this directory into `fulton_county`.
3. Open a terminal and `cd` into the `data` directory. Run `python retrieve_ga.py`. Go eat lunch. This will probably take a while.
4. Download https://sos.ga.gov/admin/uploads/Active_Voters_by_Race_and_Gender_By_Congressional_as_of_November_1_2020.xlsx from the Georgia Secretary of State. Save this into the `data` directory.
5. Fire up your R editor of choice. (I'm wishy-washy. I can't make up my mind whether I prefer Atom with the ide-r extension, emacs, or RStudio. They're all great.)
6. The following R files must be run in the following order before knitting the rmarkdown file.
   1. `fonts.R`
   2. `ga_clean.R`
   3. `ga_map.R`
   4. `load_data.R`
   5. `create_voter_points.R` - Generating one point per vote cast will take an hour minimum. Even if you divide the votes by 10, which lets you create a smaller image without changing the visualization's interpretation, you're still talking about over 400,000 points. Get comfortable or run this before going to sleep!
   6. `sen1_data.R` is a work in progress. It's far from finished, but it is used in ga_map.Rmd. This is for a future comparison, plus runoff analysis. It is not suitable for use at this time. Run this, but do not draw any conclusions from it!
7. Now, run `ga_map.Rmd` at your leisure.
8. `network_bar_charts.R`can only be run after `ga_map.Rmd`.
   1. Third-party candidates are not included in these visualizations because of how the data was presented by the major networks. This does not mean these candidates did not exist! Due to this, the `fake_real_news` function's `est_voters = network's projected turnout - third party votes`. Here estimated votes means how many votes does the network think are still uncounted.
   2. `calc_est_voters` requires the percent of Republican and Democratic votes. For example, 40% is always entered here as 0.40.

