# Sensitive Communities Project

## Summary
The following document summarizes UDP research into developing a new SB 50 sensitive communities definition, to account for change-over-time and to incorporate more refined measures of community vulnerability.

## Methods WIP Doc

[Sensitive Communities Methods](https://docs.google.com/document/d/1JvqZLnh_sZIgfrxaXPtBSJRbTEPcbx-jl_tvf5JlqiU/edit)

### Review of Relevant Methodologies

1. Vulnerability is often measured in terms of a tract's **median income** relative to regional or city medians. Tracts with lower than median income or above average % of low income households are classified as low income tracts. Over 50% of reviewed studies used income as a measure of vulnerability. 
2. Studies sometimes employ **demographic factors** such as % rental households, % people of color, and % low-education to nuance measures of vulnerability. 
3. Low income and demographic profiles are further refined by metrics associated with **risk of speculation or future investment** including distance to CBD, adjacency to high cost or changing neighborhoods, and low housing values/rents. 
4. Displacement is said to occur more often when a vulnerable tract experiences high **increases** in **median household income**, **housing value**, or **rent** relative to regional trends.
5. Demographic shifts used to further nuance definitions of gentrification/displacement include an **increase** in the share of **college educated** and/or **white residents**, and **decreases in low income households**. 

### Review of the Literature
Literature reviews on new construction and zoning reform on development-induced displacement offers mixed results. Important takeaways include: 
* Regional-scale studies tent to suggest that **new construction moderates prices and/or reduces displacement pressures** (Zuk and Chapple; Ellen et al. (2019); Mast (2019)).
* Neighborhood-scale results aren't as uniform: Bunten (2019) **links new development to higher rates of out migration in low income/gentrifiable tracts**, Zuk and Chapple (2016) find **no significant effect**. 
* Zuk and Chapple (2019) and Ellen et al (2019) both report a **strong link between subsidized housing, displacement mitigation, and/or price moderations at regional scales**. 
* Freemark (2019) shows how **upzoning in Chicago increased speculation and the prices of existing housing stock**, but finds **no impacts on the number of newly permitted dwellings over a five year period**. 
* Lens and Monkkonen (2015) find that **density restrictions are associated with segregation of the wealthy and middle income**, and that **local pressure to regulate land is linked to higher income segregation**. 

### Feedback from Stakeholder Calls

Stakeholders across categories agreed that capturing recent shifts in neighborhood affordability and demographics would improve the process of identifying sensitive communities in SB 50. Specific suggestions differed only slightly between stakeholder groups: 

* Academics advised UDP to pay attention to **low income**, **rent burdened**, or **linguistically isolated communities** that are experiencing recent, above regional shifts in **median income**, **housing value**, and **rent**; increased investment in the form of **new development**; **loss of affordable housing stock** (including Housing Choice Vouchers in use); and **areas adjacent to high cost or gentrifying neighborhoods**. 
* Advocates offered similar suggestions, but placed a stronger emphasis on **African American and Latinx displacement**, as well as vulnearbility of specific groups, such as **seniors**, people who are **LGBT**, and **low income** families. Stakeholders in the Central Valley indicated that rent burden is a good indicator of resident vulnerability. 
* Bill authors and sponsors voiced support for the addition of a **change over time methodology**, as well as **revisions to the definition of sensitive communities as a whole**.

#### Recommendations for a change-over-time methodology for sensitive communities

Based on this review of relevant methodologies, literature, and stakeholder feedback, the following methodology is proposed: 

**Vulnerability**: A tract will be considered vulnerable if:
* At least X% LI HHs (2017) AND
* % renters > regional median AND
* Share of HHs rent-burdened above regional median

**Change over time**: A tract will be considered to have displacement pressure  in 2017 if: 
* Vulnerable (see above - renters, rent burden, hold-out LI HHs)
* Development investment increasing (# of housing units per census, or USPS NOSTAT increasing above 75th percentile for region)
* Subsidized housing decreasing (decrease in Housing Choice Voucher above 75th percentile for region, or other subsidized from CHPC?)
* Change in median rent > 75th percentile for region (btw T1 - 2017) 

**A note on risk factors**: In addition to demographic measurements of vulnerability, we should consider how to incorporate “risk factors” into our definitions. Using our methodological and literature reviews, I’ve identified five risk factors worth considering: 
* Proximity to tract with rent > rm 
* Proximity to tract gentrified in the last 10 years 
* Proximity to jobs-rich area 
* Proximity to fixed-rail or frequent bus service
* Median rent > rent of long term tenants (rent gap)


**A note on geographies**: While tracts will be used in most areas throughout the state, I propose we use block groups in rural areas to account for geographically dispersed populations. We should also consider how to handle patchwork designation patterns, potentially defining non-vulnerable tracts surrounded by sensitive communities on three or more sides as sensitive communities themselves. 


## Tasks
- [ ] Explore areas that have seen various changes 
    * Race
    * Rent
    * 50% AMI housing stock value. (where are the 50% ami rentals at?)
    * Adjacency
- [ ] Get HCV data
- [ ] Include gentrification measures 
    * Consider disadvantage measures
- [ ] Calculate Spatial Cross Regressive Models
- [ ] Produce rmarkdown with tmap examples
    * Include documentation on each variables 
    * Produce different maps with different variable sets

