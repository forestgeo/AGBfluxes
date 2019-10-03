
<img src="https://i.imgur.com/vTLlhbp.png" align="right" height=88 /> Compute biomass fluxes at ForestGEO sites
===============================================================================================================

<!-- [![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental) -->
<!-- [![Travis build status](https://travis-ci.org/forestgeo/AGBfluxes.svg?branch=master)](https://travis-ci.org/forestgeo/AGBfluxes) -->
<!-- [![CRAN status](https://www.r-pkg.org/badges/version/AGBfluxes)](https://cran.r-project.org/package=AGBfluxes) -->
<!--  [![Coverage status](https://coveralls.io/repos/github/forestgeo/AGBfluxes/badge.svg)](https://coveralls.io/r/forestgeo/AGBfluxes?branch=master) -->

Project and contributors
---------------------------
This package was initially developped to test for a directional change in forest dynamics across ForestGEO sites. The main author is Ervan Rutishauser (er.rutishauser@gmail.com), Smithsonian Tropical Research Institute Post-doc (2016-2019), with contribution from Helene Muller-Landau (current administrator; MullerH@si.edu) and Mauro Lepore.
To cite this code, please cite the following publication:

**Rutishauser, E., Wright, S. J., Condit, R., Hubbell, S. P., Davies, S. J., & Muller‐Landau, H. C. (2019). Testing for changes in biomass dynamics in large-scale forest datasets. Global Change Biology, 0(ja). https://doi.org/10.1111/gcb.14833**

<img src="https://i.imgur.com/Z1BJo8D.png" align="right" height=88 />

Main purpose of the package
---------------------------

This package has a main function **data\_preparation()** that works either at stem (stem=T) or tree (stem=F) levels, and works in 2 steps:

### A. Complement the data

-   compiles multiple censuses into a single file to,
-   checks for consistency in stem/tree status (alive/dead) over time,
-   (optional) fills gaps (i.e. missing DBHs or POM values) by simple linear interpolation (fill\_missing=T),
-   (optional) corrects POM changes through application of a taper correction (taper\_correction=T),
-   allocate wood density from CTFS wood density database and DRYAD
-   estimates stem/tree above-ground dry biomass (AGB)

### B. Format the data

-   merges information from stems to single tree for each census interval,
-   codes if a tree is recruited, alive, dead or broken/resprouted
-   computes annual **AGB productivity** (if alive), **ingrowth** (if recruited or resprouted) or **loss** (if dead) at tree-level
-   flags obvious measurement errors (annual AGB growth &gt; X % (X = maxrel) of mean annual AGB growth across all census intervals) The function returns a data.frame where each row correspond to the initial and final measurments (i.e. DBH, POM, status) per **tree** for a given census intervals. Variables related to the initial and final census are denoted with **1** and **2**, respectively.

Variable are defined as follow:

<table style="width:35%;">
<colgroup>
<col width="15%" />
<col width="19%" />
</colgroup>
<thead>
<tr class="header">
<th>Variable</th>
<th>Definition</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>treeID</td>
<td>Unique tree ID</td>
</tr>
<tr class="even">
<td>dbh1</td>
<td>Measured dbh at intial census</td>
</tr>
<tr class="odd">
<td>dbhc1</td>
<td>Corrected dbh at intial census</td>
</tr>
<tr class="even">
<td>status1</td>
<td>Status (alive/dead) at intial census</td>
</tr>
<tr class="odd">
<td>code1</td>
<td>Original CTFS code at intial census</td>
</tr>
<tr class="even">
<td>hom1</td>
<td>Original height of measurement at intial census</td>
</tr>
<tr class="odd">
<td>sp</td>
<td>CTFS species name acronym</td>
</tr>
<tr class="even">
<td>wsg</td>
<td>Wood-density allocated at lowest taxonomic level</td>
</tr>
<tr class="odd">
<td>agb1</td>
<td>Above-ground tree biomass estimate at initial census</td>
</tr>
<tr class="even">
<td>date1</td>
<td>Date of census</td>
</tr>
<tr class="odd">
<td>...</td>
<td></td>
</tr>
<tr class="even">
<td>broken</td>
<td>Has the main stem DBH &gt; 10cm broken (i.e. AGB reduction &gt; 20%)?</td>
</tr>
<tr class="odd">
<td>agbl</td>
<td>AGB loss due to main stem breakage</td>
</tr>
<tr class="even">
<td>agb1.surv</td>
<td>AGB of surving stems (if any) after main stem breakage</td>
</tr>
<tr class="odd">
<td>interval</td>
<td>first, second, third... census interval</td>
</tr>
<tr class="even">
<td>year</td>
<td>Calendar year of census</td>
</tr>
<tr class="odd">
<td>gx</td>
<td>X coordinate</td>
</tr>
<tr class="even">
<td>gy</td>
<td>Y coordinate</td>
</tr>
<tr class="odd">
<td>quadrat</td>
<td>20x20m quadrat</td>
</tr>
<tr class="even">
<td>name</td>
<td>Genus and species</td>
</tr>
<tr class="odd">
<td>ID</td>
<td>Concatenation of treeID and stem tag</td>
</tr>
<tr class="even">
<td>int</td>
<td>Census interval length in days</td>
</tr>
<tr class="odd">
<td>code</td>
<td>Corrected tree status, can be: &quot;A&quot; = alive, &quot;AC&quot; = alive, with POM changed, &quot;B&quot; = broken, &quot;Rsp&quot; = resprouted, &quot;R&quot; = recruited or &quot;D&quot; = dead</td>
</tr>
<tr class="even">
<td>dHOM</td>
<td>hom2-hom1</td>
</tr>
<tr class="odd">
<td>prod.g</td>
<td>annual AGB productivity for trees coded as &quot;A&quot; or &quot;AC&quot;</td>
</tr>
<tr class="even">
<td>prod.r</td>
<td>annual AGB productivity for trees coded as &quot;Rsp&quot; or &quot;R&quot;</td>
</tr>
<tr class="odd">
<td>loss</td>
<td>annual AGB loss for trees coded as &quot;B&quot; or &quot;D&quot;</td>
</tr>
<tr class="even">
<td>ficus</td>
<td>Is that tree a large (DBH &gt; 50cm) strangler fig?</td>
</tr>
<tr class="odd">
<td>prod.rel</td>
<td>relative producitivity (prod.g/average-productivity-per-hectare)</td>
</tr>
<tr class="even">
<td>error</td>
<td>Is prod.rel &gt; maxrel (1), or prod.rel &lt; -maxrel (-1)</td>
</tr>
<tr class="odd">
<td>error.loss</td>
<td>Binary. Was that tree flagged as &quot;error&quot; prior to death?</td>
</tr>
</tbody>
</table>

Resulting data set can further be used to compute AGB fluxes at a site (as described below).

Installation
------------

You can install the released version of AGBfluxes from [CRAN](https://CRAN.R-project.org) with: `r # install.packages("devtools") devtools::install_github("AGBfluxes")`

Example
-------

`data_preparation()` outputs a data.table, which has a special `print()` method.
