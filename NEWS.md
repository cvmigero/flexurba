# flexurba 0.2.0

* New `apply_threshold()` identifies urban areas by applying a threshold on grid cells.

* New `convert_regions_to_grid()` convert regions from a vector layer to gridded format.

- New R package [`flexurbaData`](https://flexurbadata-ac82f4.pages.gitlab.kuleuven.be/) with global datasets accompanying the Flexurba R package. The package includes processed population and built-up area derived from the [Global Human Settlement Layer](https://human-settlement.emergency.copernicus.eu/download.php), and night-time light grid derived from the [Earth Observation Group](https://eogdata.mines.edu/products/vnl/#annual_v2). 

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
  
* The function reference is restructured and the README, function documentation pages and vignettes are modified to reflect the updates. 
