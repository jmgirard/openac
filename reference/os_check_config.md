# Resolve an openSMILE config name to an absolute path

Check that a named openSMILE configuration exists in the installed
openSMILE `config/` directory and return its absolute path. Errors if
the config is not found.

## Usage

``` r
os_check_config(config)
```

## Arguments

- config:

  A required string naming an openSMILE config, with or without the
  `.conf` extension (e.g. `"egemaps/v02/eGeMAPSv02"`).

## Value

A string giving the absolute path to the matching `.conf` file.

## Examples

``` r
if (FALSE) { # \dontrun{
os_check_config("egemaps/v02/eGeMAPSv02")
} # }
```
