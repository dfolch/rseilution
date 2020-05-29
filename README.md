# rseilution
R tools for deriving air toxicity for geographic coordinates
---
This code will append annual air toxicity data to any point location within the United States. The toxicity data come from the US Environmental Protection Agency's (EPA) [Risk-Screening Environmental Indicators (RSEI)](https://www.epa.gov/rsei) model, which is based on data collected for the [Toxic Release Inventory (TRI)](https://www.epa.gov/toxics-release-inventory-tri-program). 

The code offers many options. The following example is for points with a unique ID (in column `geo_id`) in a single state (Delaware (`de`)) for a single year (`2015`) stored with latitude/longitude coordinates (in columns `lat` and `lon`) in a CSV file (`my_points.csv`). It will return the results to CSV file (`results.csv`). 
```
source('RSEIAssignedToPoints.R')
results <- appendRSEI('my_points.csv', state='de', year=2015, unique_id='geo_id', x_name='lon', y_name='lat', out_file='results.csv')
```

If the points are associated with multiple states and/or multiple years, you will need to pass the column names containing that information for each point to the `state` and `year` parameters respectively. States need to be coded using their [two character code](https://about.usps.com/who-we-are/postal-history/state-abbreviations.pdf) and years as integers. 
```
source('RSEIAssignedToPoints.R')
results <- appendRSEI('my_points.csv', state='state_code', year='visit_year', unique_id='geo_id', x_name='lon', y_name='lat', out_file='results.csv')
```
---

The Jupyter Notebook and RMarkdown files contain more examples on the functionality and data output.

RSEI toxicity data is provided by EPA as a 0.5 mile raster covering the entire US. The code associates each point with the raster cell it falls in, and returns the associated toxicity data aggregated over the selected year. The code also returns the annualized data allowing for toxicity maps to be produced.

Log of total toxicity for 2002 Rhode Island, with three points of interest added.
![total toxicity](/test_data/ri_2002_toxicity.png)

Log of lead toxicity for 2002 Rhode Island, with three points of interest added (same scale as total toxicity).
![lead toxicity](/test_data/ri_2002_lead.png)

---
We encourage collaborators who would like to extend the functionality. 

The work was was supported by the Environmental Influences on Child Health Outcomes (ECHO) program of the National Institutes of Health (NIH) under award number 4UH3OD023332-03.
