
<!-- README.md is generated from README.Rmd. Please edit that file -->

# shootnloot

EXPERIMENTAL

A package to easily share objects between remote sessions. You’ll need a
google account.

With {shootnloot} it takes a single call to upload an object, a script,
or your full workspace, and one more call on the recipient side to fetch
it.

## Installation

Install with:

``` r
remotes::install_github("moodymudskipper/shootnloot")
```

## Example

To upload publicly conveniently I first define a new id, this basically
defines an online paste bin we’ll be sending to and fetching from.

``` r
new_id(type = "anyone")
```

    #> [1] "1XzarXBNkLDYoeA5XsBa6NUUlYFUGcx4j"

Then I set it as my default id in my *.RProfile*.

``` r
options(shootnloot.ids = c(default = "1XzarXBNkLDYoeA5XsBa6NUUlYFUGcx4j"))
```

And I communicate it to you the recipient so you can put in your own
*.RProfile* :

``` r
options(shootnloot.ids = c(antoine = "1XzarXBNkLDYoeA5XsBa6NUUlYFUGcx4j"))
```

From there whenever I use `shoot_object()` :

``` r
shoot_object(head(iris))
```

You’ll be able to loot the object with :

``` r
loot_object("antoine")
```

    #> trying URL 'https://drive.google.com/uc?id=1XzarXBNkLDYoeA5XsBa6NUUlYFUGcx4j'
    #> content type 'application/x-gzip' length 296 bytes
    #> ==================================================
    #> downloaded 296 bytes
    #>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
    #> 1          5.1         3.5          1.4         0.2  setosa
    #> 2          4.9         3.0          1.4         0.2  setosa
    #> 3          4.7         3.2          1.3         0.2  setosa
    #> 4          4.6         3.1          1.5         0.2  setosa
    #> 5          5.0         3.6          1.4         0.2  setosa
    #> 6          5.4         3.9          1.7         0.4  setosa

Try to loot it yourself, that’s all you need:

``` r
remotes::install_github("moodymudskipper/shootnloot")
library(shootnloot)
options(shootnloot.ids = c(antoine = "1XzarXBNkLDYoeA5XsBa6NUUlYFUGcx4j"))
loot_object("antoine")
```

Different permissions can be set using arguments in `new_id()`, I might
want to set a location accessible only to my team for instance :

    new_id(type = "domain", domain = "mycompany.com")
    # in my .RProfile
    options(shootnloot.ids = c(default = <default_id>, <mycompany> = <my_new_id>))
    # in my team mate's .RProfile
    options(shootnloot.ids = c(<mycompany> = <my_new_id>))
    # And then I can shoot safely :
    shoot_object(mydata, "<mycompany>")
    # And they can loot as above
    loot_object("<mycompany>")

We can shoot and loot the entire global environment with
`shoot_workspace()` and `loot_workspace()`.

If we’re using RStudio we can shoot and loot a file `shoot_file()` and
`loot_file()`. By default `shoot_file()` uploads the current script
including unsaved changes. `loot_file()` fetches the file and opens it
in a new editor tab.

# Aknowledgements

This wraps the {googledrive} package. Thanks to Lucy D’Agostino McGowan,
Jennifer Bryan and other contributors to this great package.
