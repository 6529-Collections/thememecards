
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance        contract        
     Length:80          Length:80          Min.   :1.000   Length:80         
     Class :character   Class :character   1st Qu.:1.000   Class :character  
     Mode  :character   Mode  :character   Median :1.000   Mode  :character  
                                           Mean   :1.038                     
                                           3rd Qu.:1.000                     
                                           Max.   :3.000                     
         name          
     Length:80         
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 21504269 # https://etherscan.io/block/21504269
block_hash <- "0x3621b64735bdc8c6a2f750a1c39731cfe2a0bbd82bd5243ed91ae526f96ece30"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4732 

## Code

``` r
subtract_multi <- function(x, y) {
  for (i in y) {
    where <- match(i, x)
    if (!is.na(where)) {
      x <- x[-where]
    }
  }
  return(x)
}

pick <- function(df,
                 contracts=c(),
                 address_remove=NULL,
                 address_subtract=NULL,
                 address_max=Inf,
                 address_pick=NA,
                 address_replace=FALSE) {

  df <- df %>%
    dplyr::filter(name %in% contracts) %>%
    dplyr::filter(!(address %in% address_remove))
  
  df_by_address <- df %>%
    dplyr::group_by(address) %>%
    dplyr::summarise(
      balance = ifelse(sum(balance) <= address_max, sum(balance), address_max)
    )
  
  pool <- df_by_address %>%
    dplyr::arrange(address) %>%
    dplyr::select(address, balance) %>%
    purrr::pmap(function(address, balance) {
      base::rep(address, balance)
    }) %>%
    unlist() %>%
    subtract_multi(address_subtract)
  
  if (is.na(address_pick)) {
    return(pool)
  } else {
    return(base::sample(pool, size=address_pick, replace=address_replace))
  }
}

tally <- function(x) {
  if (length(x) > 0) {
    unlist(x) %>%
    data.frame(address = .) %>%
    dplyr::group_by(address) %>%
    dplyr::summarise(
      amount = n()
    ) %>%
    dplyr::arrange(desc(amount), address)
  }
}
```

``` r
base::set.seed(seed)

address_remove <- c(
  "0x3a3548e060be10c2614d0a4cb0c03cc9093fd799",
  "0x4b76837f8d8ad0a28590d06e53dcd44b6b7d4554",
  "0x0887773b5f43c58f0da7bd0402fc2d49482eb845",
  "0xcda72070e455bb31c7690a170224ce43623d0b6f",
  "0x41a322b28d0ff354040e2cbc676f0320d8c8850d",
  "0x000000000000000000000000000000000000dead",
  "0xA19193B6Bd97798695097e71EAb6d310F99f1955",
  "0x0000000000000000000000000000000000000000"
)

hodlers_remove <- c(
  ""
)

allow_artist1    <- pick(snapshot, contracts=c("SuperRare","Reverse","ExpressiveSpace","RUNNINGINROUTINE","PEPEHAUS","GeometricContradiction","GeometricComics","GeometricPath","Overthink","PREDATORFORGIVENESS","FREESTYLESESSIONZ"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("MovsumEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("SuperRare","Reverse","ExpressiveSpace","RUNNINGINROUTINE","PEPEHAUS","GeometricContradiction","GeometricComics","GeometricPath","Overthink","PREDATORFORGIVENESS","FREESTYLESESSIONZ","MovsumEditions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 37 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x052564eb0fd8b340803df55def89c25c432f43f4      1
     2 0x05c86371a36f93ded4c237aba96ad18121676d98      1
     3 0x06e13bd0a3cba08e61028f2326b9ea2ca2539900      1
     4 0x2119096307e05be78fed4b7c8cffc83a43334106      1
     5 0x290fcbad45285084c9d99c8673070e2095531ef6      1
     6 0x2aaba4b94a805f0fea696dc11de324c2320ff4e1      1
     7 0x3b2528e2d4d5324eaa32b6db253ccac2b2f26d85      1
     8 0x465a646e92746a7191ec6ee7223d3b78d20367e5      1
     9 0x50f27cdb650879a41fb07038bf2b818845c20e17      1
    10 0x51787a2c56d710c68140bdadefd3a98bff96feb4      1
    11 0x560962063aa6b854dc04b83fe2e23ca02d885264      1
    12 0x5c70af3ddc7e815a40f2a09d22483e6e24eddf8e      1
    13 0x696dccfc16eede10129afa2acd8946d4cdf709ef      1
    14 0x698f3eaf3defee3c5a00b64bd65feee9015d6970      1
    15 0x6a8cb248d4e85fc0f5cdf0b7e47799c940faea7f      1
    16 0x6c299fd0fbf1ec228f7e8d7fb6fab4ff8e8752f2      1
    17 0x75aadbb7c36db057e5ac64183899f118de76df35      1
    18 0x8efd9addd8de6a4e64664d1893dec51f8c3339e9      1
    19 0x8f61040ac8f42f67e33592e711238a3f71788bba      1
    20 0x91aca13b42ac160c9e79a3cdcbb42cf68c1f15b4      1
    21 0x949b269f72d4f2721001de5036a5721b13050e39      1
    22 0x96a0627f560f68d5cf5e5429a713789cc098709c      1
    23 0x9a7dc5307940fa170f9093ca548bda0edb602767      1
    24 0x9dbef4969a9b01994a05867b2c25c169c60c7048      1
    25 0xb244f4b768208ca90f7cd92c347ee017b4ceb44f      1
    26 0xb4ed04164fb9a2b530dc3cde21c726dd7db94636      1
    27 0xc449f005667bef849261b35accf931a4bace48fb      1
    28 0xc4d4125c83d1108df969dda998227fca80fd82dc      1
    29 0xc68c7771ec6a6e5d67d62aa9c6f22df69865e401      1
    30 0xd3c69ec93a9841796a2f41b342aa1d1f2a4f508c      1
    31 0xec7100abdbcf922f975148c6516bc95696ca0ef6      1
    32 0xef34feb024f0a6f4d5cf3a7cd30bb0f041a6af58      1
    33 0xf0b606b9d4d8809eb41a744eb19be23b0dea74df      1
    34 0xf26f2f6f86cf3e8832a07da6f053a66a7a45697d      1
    35 0xfebaeca1976afa7a12298ec2016a0497253f2e1a      1
    36 0xff18fd1347fc75b558043d3d882a337a33d05a49      1
    37 0xfff423212eb0b97788d43a0c38b7d5762ba3c6e6      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 13 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0e8d35bb16acc48144253d703eb19358f739aaae      1
     2 0x10469a0096279403b9f87680f4a75578fc14616f      1
     3 0x186031dedc522f75c430b21e1f9c4b28c1114bb4      1
     4 0x3168a9334c9106bcc947b4ac0818eb1613b91e64      1
     5 0x4c2f526cdaaeea289fb02fce54ac5cf9ffb5229b      1
     6 0x66ff8f288c413e61c8ee807088ce301c03076e6e      1
     7 0x6a290fd02a31c78a225b98f3d587d61f4aeba437      1
     8 0x9b848370417c9f525d9168f2aa91a2cd819240e8      1
     9 0xa12a9cb850ddf6abe695949a50465dc1d6b3a34e      1
    10 0xacbb4ab18c40b4c0a2cdaef9e3568735d2f1bc31      1
    11 0xb890f264b1a7591cdf9e21b3b640a741cf677ae9      1
    12 0xf566c12516d171b451dacb93c114753c07f980e0      1
    13 0xff559fc20d9b6d78e6e570d234b69c18142bb65e      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 50 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x052564eb0fd8b340803df55def89c25c432f43f4      1
     2 0x05c86371a36f93ded4c237aba96ad18121676d98      1
     3 0x06e13bd0a3cba08e61028f2326b9ea2ca2539900      1
     4 0x0e8d35bb16acc48144253d703eb19358f739aaae      1
     5 0x10469a0096279403b9f87680f4a75578fc14616f      1
     6 0x186031dedc522f75c430b21e1f9c4b28c1114bb4      1
     7 0x2119096307e05be78fed4b7c8cffc83a43334106      1
     8 0x290fcbad45285084c9d99c8673070e2095531ef6      1
     9 0x2aaba4b94a805f0fea696dc11de324c2320ff4e1      1
    10 0x3168a9334c9106bcc947b4ac0818eb1613b91e64      1
    11 0x3b2528e2d4d5324eaa32b6db253ccac2b2f26d85      1
    12 0x465a646e92746a7191ec6ee7223d3b78d20367e5      1
    13 0x4c2f526cdaaeea289fb02fce54ac5cf9ffb5229b      1
    14 0x50f27cdb650879a41fb07038bf2b818845c20e17      1
    15 0x51787a2c56d710c68140bdadefd3a98bff96feb4      1
    16 0x560962063aa6b854dc04b83fe2e23ca02d885264      1
    17 0x5c70af3ddc7e815a40f2a09d22483e6e24eddf8e      1
    18 0x66ff8f288c413e61c8ee807088ce301c03076e6e      1
    19 0x696dccfc16eede10129afa2acd8946d4cdf709ef      1
    20 0x698f3eaf3defee3c5a00b64bd65feee9015d6970      1
    21 0x6a290fd02a31c78a225b98f3d587d61f4aeba437      1
    22 0x6a8cb248d4e85fc0f5cdf0b7e47799c940faea7f      1
    23 0x6c299fd0fbf1ec228f7e8d7fb6fab4ff8e8752f2      1
    24 0x75aadbb7c36db057e5ac64183899f118de76df35      1
    25 0x8efd9addd8de6a4e64664d1893dec51f8c3339e9      1
    26 0x8f61040ac8f42f67e33592e711238a3f71788bba      1
    27 0x91aca13b42ac160c9e79a3cdcbb42cf68c1f15b4      1
    28 0x949b269f72d4f2721001de5036a5721b13050e39      1
    29 0x96a0627f560f68d5cf5e5429a713789cc098709c      1
    30 0x9a7dc5307940fa170f9093ca548bda0edb602767      1
    31 0x9b848370417c9f525d9168f2aa91a2cd819240e8      1
    32 0x9dbef4969a9b01994a05867b2c25c169c60c7048      1
    33 0xa12a9cb850ddf6abe695949a50465dc1d6b3a34e      1
    34 0xacbb4ab18c40b4c0a2cdaef9e3568735d2f1bc31      1
    35 0xb244f4b768208ca90f7cd92c347ee017b4ceb44f      1
    36 0xb4ed04164fb9a2b530dc3cde21c726dd7db94636      1
    37 0xb890f264b1a7591cdf9e21b3b640a741cf677ae9      1
    38 0xc449f005667bef849261b35accf931a4bace48fb      1
    39 0xc4d4125c83d1108df969dda998227fca80fd82dc      1
    40 0xc68c7771ec6a6e5d67d62aa9c6f22df69865e401      1
    41 0xd3c69ec93a9841796a2f41b342aa1d1f2a4f508c      1
    42 0xec7100abdbcf922f975148c6516bc95696ca0ef6      1
    43 0xef34feb024f0a6f4d5cf3a7cd30bb0f041a6af58      1
    44 0xf0b606b9d4d8809eb41a744eb19be23b0dea74df      1
    45 0xf26f2f6f86cf3e8832a07da6f053a66a7a45697d      1
    46 0xf566c12516d171b451dacb93c114753c07f980e0      1
    47 0xfebaeca1976afa7a12298ec2016a0497253f2e1a      1
    48 0xff18fd1347fc75b558043d3d882a337a33d05a49      1
    49 0xff559fc20d9b6d78e6e570d234b69c18142bb65e      1
    50 0xfff423212eb0b97788d43a0c38b7d5762ba3c6e6      1

## Versioning

``` r
R.version$version.string
```

    [1] "R version 4.2.2 (2022-10-31)"

``` r
packageVersion("tidyverse")
```

    [1] '1.3.2'

``` r
packageVersion("magrittr")
```

    [1] '2.0.3'
