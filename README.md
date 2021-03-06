
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)

# Wetland\_SkeenaESI\_Monitoring

This repository presents 4 groups of analysis/data processing scripts:

1.  Set of scripts that read in the Skeena East Environmental
    Stewardship Initiative’s (ESI) Wetland Ecosystem Services Protocol
    (WESP) 2019 and 2020 field cards, cleanup and summarize;
2.  Generate a list of potential 2020 and 2021 wetlands to sample,
    stratified based on Nation Territory and groupings of Biogeoclimatic
    Ecosystem Classification (BEC) zones, and selected to capture range
    of wetland flow characteristics, adjacent land type and disturbance;
3.  Read in 2021 WESP field cards from the file generated from the WESP
    Survey 123 form supplied by BC Wildlife Federation (BCWF); and
4.  compiles Tier 1.5 wetland date, specifically indicators downloaded
    from Climate BC and landform position.

### Data

Field data was collected in 2019, 2020 and 2021 by Skeena East ESI field
crews. Paper forms were transferred onto excel spreadsheets in 2019 and
2020. In 2021 a Survey 123 form was used on mobile devices uploaded to a
BCWF server and then provided in an excel file for processing.

Data on wetlands was collated by Jesse Fraser using provincial and
regional inventory. Skeena-Stikine FLNRORD District office supplied an
updated roads layer - available upon request.

Raster Land type, forest age and human footprint ESI specific layers
used in this analysis are also available upon request

ClimateBC variables - <http://climatebc.ca>

Landform data from Adaptwest’s landfacets
<https://adaptwest.databasin.org/pages/adaptwest-landfacets>

### Usage

There are four sets of scripts that are contained in the repo, they need
to be run in order, note there is some duplication between sets\*:

Plot Data processing:

-   plot\_01\_load\_2019.R
-   plot\_02\_clean\_data.R
-   plot\_01\_load\_2020.R
-   plot\_02\_clean\_data\_2020.R

Clean field data is then passed to Wet\_03\_analysis\_6\_samplePrep.R

-   plot\_01\_load\_2021.R - 2021 field data
-   plot\_02\_cleanFn\_2021.R - functional assessment surveys
-   plot\_02\_cleanStressor\_2021.R - wetland stressor surveys

Sample selection:

Load and clean base data:

-   Wet\_01\_load\_spatial.R\*
-   Wet\_02\_clean\_1\_spatial.R\*

Organize data used for site selection:

-   Wet\_02\_clean\_2\_spatial\_AddWshds.R
-   Wet\_02\_clean\_3\_spatial\_AddFREP.R
-   Wet\_02\_clean\_4\_spatial\_AddCGL.R
-   Wet\_03\_analysis\_1\_AdminUnits.R
-   Wet\_03\_analysis\_2\_BECStrata.R
-   Wet\_03\_analysis\_3\_FlowStrata.R
-   Wet\_03\_analysis\_4\_LandTypeStrataN.R
-   Wet\_03\_analysis\_5\_DisturbanceStrataN.R

Conduct site selection:

-   Wet\_03\_analysis\_6\_samplePrep.R
-   Wet\_03\_analysis\_7\_2019\_ReportCard.R
-   Wet\_03\_analysis\_8\_sampleRequirements.R
-   Wet\_03\_analysis\_9\_sampleAddStrata.R
-   Wet\_03\_analysis\_10\_2020\_ReportCard.R
-   Wet\_03\_analysis\_11\_sample2020PickNewSite.R
-   Wet\_03\_analysis\_12\_2020SiteAdjust.R
-   Wet\_03\_analysis\_13\_sample2020RevisedSummary.R

Output - variety of formats for participants:

-   Wet\_04\_output\_1.R\*
-   Wet\_04\_output\_2.R

Tier 1.5 wetland data:

-   Wet\_01\_load\_spatial.R\*
-   Wet\_02\_clean\_1\_spatial.R\*
-   Tier1.5\_03\_analysis.R
-   Wet\_04\_output\_1.R\*

### Project Status

This project is part of Skeena East ESI a collaboration of Skeena First
Nations and the Provincial Government.

### Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an
[issue](https://github.com/bcgov/Wetland_SkeenaESI_Monitoring/issues/).

### How to Contribute

If you would like to contribute, please see our
[CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of
Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree
to abide by its terms.

### License

    Copyright 2020 Province of British Columbia

    Licensed under the Apache License, Version 2.0 (the &quot;License&quot;);
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an &quot;AS IS&quot; BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

------------------------------------------------------------------------

This repository is maintained by
[ENVEcosystems](https://github.com/orgs/bcgov/teams/envecosystems/members).
