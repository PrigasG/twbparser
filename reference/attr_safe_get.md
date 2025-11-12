# Safely extract an attribute from a named list

Safely extract an attribute from a named list

## Usage

``` r
attr_safe_get(attrs, name, default = NA_character_)
```

## Arguments

- attrs:

  Named list (e.g.,
  [`xml2::xml_attrs()`](http://xml2.r-lib.org/reference/xml_attr.md)
  result)

- name:

  Attribute to retrieve

- default:

  Fallback value

## Value

Scalar character
