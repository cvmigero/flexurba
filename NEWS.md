# Flexurba 1.0.0.0

- New function to identify urban areas by applying a threshold on grid cells. For more information see `apply_threshold()`.

- New function `convert_regions_to_grid()`

- Restructure the function reference.

- The functions that are specificly design to reconstruct the *Degree of Urbanisation* delineation have been renamed with the prefix `DoU_` to make a more consistent API and allowing for a better tab-completion. The old names remain available for backward compatibility.
  
  | New name                 | Old name          |
  | ------------------------ | ----------------- |
  | `DoU_plot_grid()`        | `plot_grid()`     |
