# README for outputs/QA_reports/suggested_fixes/species_to_recode/

This directory contains the output of the `species_to_recode` data generated  by the quarto QA template
at `inst/extdata/_extensions/template_QA_veg_data.qmd`.

The filename syntax is as follows #take the syntax from function write_QA_report in the QA template

```
<type>_<base_input_file>_<date>_<short_SHA>.csv
```

where:

* `<type>` is the type of data being reported on (e.g. `species_to_recode`)
* `<base_input_file>` is the base input file used to generate the data
* `<date>` is the date the data was generated
* `<short_SHA>` is the short SHA of the git commit used to generate the data

For example, a file generated from the `species_to_recode` data in the `veg_data` file on `2021-01-01` with a short SHA of `1234567` would look like this:

```
species_to_recode_veg_data_2021-01-01_1234567.csv
```


This data identifies species entries that are fuzzy matched to species recorded 
in the master species file at "data/raw_data/veg_data/VEFMAP_species_master.csv".

The output of species_to_recode is a table with the following columns: 

* `species_master`: The species name in the master species file
* `genus`: The genus of the species in the master species file
* `family`: The family of the species in the master species file
* `origin`: The origin of the species in the master species file
* `lifeform`: The lifeform of the species in the master species file
* `classification`: The classification of the species in the master species file
* `instreamveg`: The instreamveg value of the species in the master species file
* `wpfg`: The wpfg value of the species in the master species file
* `wpfgsource`: The wpfgsource value of the species in the master species file
* `group`: The group of the species in the master species file
* `rec_grp`: The rec_grp of the species in the master species file
* `species_veg_data`: The fuzzy matched species name in the veg data file
