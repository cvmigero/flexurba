# flexurba 0.2.2

Process CRAN feedback:

* Arguments `filenames` in `flexurba::download_GHSLdata()`, and `output_filenames` and `global_filenames` in `flexurba::crop_GHSLdata()` are now required as per CRAN policy. 

* Minor changes to unit test such that files created in the temporary directory are cleaned up.

* Changes to examples of `download_GHSLdata`. 

# flexurba 0.2.1

* Update spatial boundaries in `flexurba::units_belgium` based on data from the [Algemene Directie Statistiek - Statistics Belgium](https://statbel.fgov.be/nl/open-data/statistische-sectoren-2024), as these are available under Creative Commons CC BY 4.0 licence. Note that analysis that rely on `flexurba::units_belgium` might have different results. 

* Update parameter list in the function `flexurba::DoU_get_grid_parameters()`. The values `UC_smooth_pop` and `UC_smooth_pop_window` were missing from Level 1. 

* Several minor modifications on the developers side of the package to prepare for a CRAN submission.


# flexurba 0.2.0

* New `apply_threshold()` identifies urban areas by applying a threshold on grid cells.

* New `convert_regions_to_grid()` convert regions from a vector layer to gridded format.

* New R package [`flexurbaData`](https://flexurbadata-ac82f4.pages.gitlab.kuleuven.be/) with global datasets accompanying the Flexurba R package. The package includes processed population and built-up area derived from the [Global Human Settlement Layer](https://human-settlement.emergency.copernicus.eu/download.php), and night-time light grid derived from the [Earth Observation Group](https://eogdata.mines.edu/products/vnl/#annual_v2). 

* New `load_proxies_belgium()` loads a sample of the proxy datasets in `flexurbaData` for the country of Belgium.

* Two new vignettes: (1) `vignette("vig8-apply-thresholds")` discussing different thresholding approaches and (2) `vignette("vig9-different-proxies")` on different proxy datasets used to construct urban boundaries.

* The functions that were specifically design to reconstruct the *Degree of Urbanisation* delineation have been renamed with the prefix `DoU_` to make a more consistent API and allowing for a better tab-completion. The old names remain available for backward compatibility.
  
  | New name                            | Old name                        |
  | ----------------------------------- | ------------------------------- |
  | `DoU_preprocess_grid()`             | `preprocess_grid()`             |
  | `DoU_classify_grid()`               | `classify_grid()`               |
  | `DoU_classify_grid_urban_centres()` | `classify_grid_urban_centres()` |
  | `DoU_classify_gid_urban_clusters()` | `classify_gid_urban_clusters()` |
  | `DoU_classify_grid_rural()`         | `classify_grid_rural()`         |
  | `DoU_classify_grid_water()`         | `classify_grid_water()`         |
  | `DoU_get_grid_parameters()`         | `get_grid_parameters()`         |
  | `DoU_get_optimal_builtup()`         | `get_optimal_builtup()`         |
  | `DoU_preprocess_units()`            | `preprocess_units()`            |
  | `DoU_classify_units()`              | `classify_units()`              |
  | `DoU_preprocess_units()`            | `preprocess_units()`            |
  | `DoU_load_grid_data_belgium()`      | `load_grid_data_belgium()`      |
  | `DoU_plot_grid()`                   | `plot_grid()`                   |
  | `DoU_plot_units()`                  | `plot_units()`                  |
  
* The grid classification rules for semi-dense urban clusters in Level 2 of DEGURBA have been modified in July 2024. To reflect the most up-to-date rules, the default `flexurba` grid parameters for semi-dense urban clusters in `DoU_get_grid_parameters()` have also been updated. Specifically:
  * the minimum density threshold (`SDUC_density_threshold`) increased from 300 to 900 inhabitants per km²
  * the minimum size threshold (`SDUC_size_threshold`) decreased from 5000 to 2500 inhabitants 
  * the contiguity rule (`SDUC_contiguity_rule`) is modified from the queen (=`8`) to rook (=`4`) contiguity
  * the buffer size for semi-dense urban clusters `SDUC_buffer_size` is adjusted to 2 km


  <br/>Note that these changes will affect the results of `DoU_classify_grid()` and `DoU_classify_grid_urban_clusters()` if `level1`is set to `FALSE`. The previous classification rules for semi-dense urban clusters can still be applied by specifying them explicit in the function argument `parameters`.
<br/><br/>
* The unit classification rules for Level 2 of DEGURBA have been modified in July 2024. To reflect the most up-to-date rules, the `flexurba` function `DoU_classify_units()` is also updated (see the documentation page for more information). The previous classification rules can still be applied by setting the function argument `rules_from_2021` to `TRUE`. 

* The function reference is restructured and the README, function documentation pages and vignettes are modified to reflect the updates. 
