
# Sensory Board

## Installation

You can install the development version of `{sensoryboard}` from
[GitHub](https://github.com/) with:

``` r
if (!require("remotes")) {
  install.packages("remotes")
}
remotes::install_github(
  "jcrodriguez1989/sensory_board", subdir = "sensoryboard", dependencies = TRUE
)
```

## Example

### Setting the Attributes to Evaluate

To set which attributes -and their type- to evaluate by the panelists,
this should be performed in a `csv` file, as exemplified in
[atributos.csv](atributos.csv).

    #>                Nombre Valores
    #> 1 Intensidad de color Numeric
    #> 2              Aromas    Text
    #> 3             Sabores    Text
    #> 4              Frutal Numeric
    #> 5              Floral Numeric
    #> 6            Herbáceo Numeric

This `csv` file should contain two columns:

-   Nombre: The names of the attribute to be evaluated.
-   Valores: The types of the attribute to be evaluated, must be one of
    `{"Numeric", "Text"}`

### Setting the Products to Evaluate

To set which products to evaluate by the panelists, this should be
performed in a `csv` file, as exemplified in
[productos.csv](productos.csv).

    #>       Copa
    #> 1 Herencia
    #> 2      Uno
    #> 3  Eredita
    #> 4    Nieto

This `csv` file should contain one column, the name of this column will
be the name of the product to be evaluated, for example `"Copa"`. The
values of this first columns, will be the product to be evaluated.

### Running the App

#### Running the Panel

Let’s assume we are using the two files presented above, to run the
panel app, from an R console type:

``` r
sensoryboard::run_panel(
  products_file = "productos.csv",
  attributes_file = "atributos.csv",
  answers_dir = "Answers/"
)
```

#### Running the Board

Let’s assume we are using the two files presented above, to run the
board app, from an R console type:

``` r
sensoryboard::run_board(answers_dir = "Answers/")
```
