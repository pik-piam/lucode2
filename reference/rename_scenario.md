# Renames the scenariofolder and the scenario contained in it

Use this function to change the name of a run after it has finished.
This function renames the run folder, change the run title in the cfg
and in the reporting. This can be useful if the initial name of a run
was not meaningful. However, inconsistencies will remain, since the
function will NOT rename the scenario in the list file, the gdx, and the
results database.

## Usage

``` r
rename_scenario(map, keep_time_stamp = FALSE)
```

## Arguments

- map:

  Named vector, containing the new scenario names as elements and the
  corresponding old folder names as the emelents' names.

- keep_time_stamp:

  Logical indicating whether timestamp of old folder name should be
  transferred to new folder name or not (default = FALSE).

## Author

David Klein
