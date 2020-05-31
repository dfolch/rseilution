# rseilution
R tools for deriving air toxicity for geographic coordinates

---
This code will append annual air toxicity data to any point location within the United States. The toxicity data come from the US Environmental Protection Agency's (EPA) [Risk-Screening Environmental Indicators (RSEI)](https://www.epa.gov/rsei) model, which is based on data collected for the [Toxic Release Inventory (TRI)](https://www.epa.gov/toxics-release-inventory-tri-program).

The code offers many options. The following example is for points with a unique ID (in column `geo_id`) in a single state (Delaware `de`) for a single year (`2015`) stored with latitude/longitude coordinates (in columns `lat` and `lon`) in a CSV file (`my_points.csv`). It will return the results to a CSV file (`results.csv`).
```
source('RSEIAssignedToPoints.R')
results <- appendRSEI('my_points.csv', state='de', year=2015,
                      unique_id='geo_id', x_name='lon', y_name='lat',
                      out_file='results.csv')
```

If the points are associated with multiple states and/or multiple years, you will need to pass the column names containing that information for each point to the `state` and `year` parameters respectively. States need to be coded using their [two character code](https://about.usps.com/who-we-are/postal-history/state-abbreviations.pdf) (upper or lower case) and years as integers or strings.
```
source('RSEIAssignedToPoints.R')
results <- appendRSEI('my_points.csv', state='state_code', year='visit_year',
                      unique_id='geo_id', x_name='lon', y_name='lat',
                      out_file='results.csv')
```
---

We designed this to be accessible for both regular R users and non-R users alike. If you don't use R, you just need to install R and run the script as shown above by passing in your CSV (or SHP file). It will return a CSV (or SHP) that can be opened in other software. For R users, the script can take data frames, spatial points data frames and simple features objects, and it returns spatial points data frames that can be manipulated and plotted.

RSEI toxicity data is provided by EPA as a 0.5 mile raster covering the entire US. The code associates each point with the raster cell it falls in, and returns the associated toxicity data aggregated over the selected year. The code also returns the annualized data allowing for toxicity maps to be produced.

Log of total toxicity for 2002 Rhode Island, with three points of interest added.
![total toxicity](/test_data/ri_2002_toxicity.png)

Log of lead toxicity for 2002 Rhode Island, with three points of interest added.
![lead toxicity](/test_data/ri_2002_lead.png)

The [Jupyter Notebook](/RSEIlution_tutoiral.ipynb) and [RMarkdown](/RSEIlution_tutoiral.Rmd) files in the repository contain more examples on the functionality and data output.

---
We encourage collaborators who would like to extend the functionality.

This work was supported by the Environmental Influences on Child Health Outcomes (ECHO) program of the National Institutes of Health (NIH) under award number 4UH3OD023332-03.
