
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance        contract        
     Length:103         Length:103         Min.   :1.000   Length:103        
     Class :character   Class :character   1st Qu.:1.000   Class :character  
     Mode  :character   Mode  :character   Median :1.000   Mode  :character  
                                           Mean   :1.078                     
                                           3rd Qu.:1.000                     
                                           Max.   :3.000                     
         name          
     Length:103        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 18996969 # https://etherscan.io/block/18996969
block_hash <- "0xfa59d1ed7c30dd85ab7943f5568fde650540b6098cbd1f16f0e5c4d846d6ed1b"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4811 

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
  "0x000000000000000000000000000000000000dead"
)

hodlers_remove <- c(
  ""
)

allow_artist1    <- pick(snapshot, contracts=c("PixelCore","FinallyHome","PartOne","PartOneII","PartTwo","birthofapixel"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("ThinkForYourselfEditions","WynEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("PixelCore","FinallyHome","PartOne","PartOneII","PartTwo","birthofapixel","ThinkForYourselfEditions","WynEditions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 22 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x04b5a01e011352624245a5150d7f36e754ef8b62      1
     2 0x052564eb0fd8b340803df55def89c25c432f43f4      1
     3 0x182e0862fe4b624c9b7199df8fa52b07f4283632      1
     4 0x193bb5dcb851ef8f75dbbd6950c962c3a5f4b0e3      1
     5 0x1c2ea5a58d54914e06bee9932f5acd0a622930e8      1
     6 0x27b455c481d62f91a7b55d09cf2e3cd70858ef85      1
     7 0x33e8d10459643faab56f1974291cb603b88a09f8      1
     8 0x37009f9ae9dcc0a6d59204e1aeaf9da4968caec8      1
     9 0x3dc94530601d9ba00a014ae7e0210dcf9960eb67      1
    10 0x465a646e92746a7191ec6ee7223d3b78d20367e5      1
    11 0x46b249da04697c801cd592030f6ef81f9433398f      1
    12 0x46b2bd5c888e6d57a839c559fd6076f2c15a8cb1      1
    13 0x4b4d75b8b0fc7528ea2614b2ca83555824c07867      1
    14 0x53314ed80d217744eaa3cfa13fea6692d08627ac      1
    15 0x63be81f380d82134b71ca8663142e8dc63f7c8a8      1
    16 0x8641cfc99087659f0b84174a137d2c7f6fe861c2      1
    17 0x9b65bcad994551a7ee35be0750f45e444e7f5cc2      1
    18 0xa1168968174d3a4f2b6666051800115d86fdc235      1
    19 0xae060f1ac3ed0b80cdd7e6f271c139fedc5a3d2f      1
    20 0xcd94d3aa70986658d876ed6497636167014b1d1e      1
    21 0xd0ed643099041b805adec2e2af3329c06a20ffe6      1
    22 0xe75764a435f7831e8dfb960dbd85f01f78b84f47      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 48 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x07218f914320e3a4b9c80f9867bf0a47cb6ebb79      1
     2 0x092cd1a6d222a167f5d0767e6444c8b45c92cc72      1
     3 0x0940d80b446f303a98fc596cb401e9a966b55022      1
     4 0x0d71ec30c323037bdac7bd174d4a95cec5a4ee0d      1
     5 0x0f0eae91990140c560d4156db4f00c854dc8f09e      1
     6 0x119fcd292b95551e8af472fc4721ac34cc5f48e5      1
     7 0x3e0914fc2b3b67a476142020b7d6990d01ad234e      1
     8 0x3e7374ee60f296ef2350c47a5110cb5233321756      1
     9 0x42a41d6874dcb96596bd92e2e5f881c341df38c5      1
    10 0x478bb542f7658d635abba67edb987806dff5b83d      1
    11 0x507c20ef269726b2d0b8efe9f6b75a4ddd9ef741      1
    12 0x5aa71418ac6238c78ea9a032a768dbbbf86bfeca      1
    13 0x5f9d41289ad44d17e6c51f889276999112e4fffc      1
    14 0x66ff8f288c413e61c8ee807088ce301c03076e6e      1
    15 0x6e63a4caeccb4f341ee9c9175c9cc554bdb6d10b      1
    16 0x74401b66f746630ccee98689b1a20e0149fbccab      1
    17 0x76f29c135b3062d53d5ed70f2e21d5978f50bdc1      1
    18 0x78d65f91babbcf9f08061ce21ae7443a69f6f07f      1
    19 0x79f238d3d0dba4bcba894be2e972b0818aeeaf3c      1
    20 0x7af2e406bcc85580f2994f6cee959b280c6e0c32      1
    21 0x7e5124202b1a177cbc254b2c2262541682f20601      1
    22 0x7fd1d8e388b29ef0b8a00ca07e2266686a5559ee      1
    23 0x8abe2749b67ad7debb7442c46ea4a0e003d4852a      1
    24 0x90a8734076d7a7db4303fed64cf46467069f11d8      1
    25 0x938a0af4b86057489bc651dd02c080890d8ed5e5      1
    26 0x973ef7e4e91cc9098709d540a8ddfb708b331c87      1
    27 0x9b848370417c9f525d9168f2aa91a2cd819240e8      1
    28 0x9dbef4969a9b01994a05867b2c25c169c60c7048      1
    29 0xa549487362b9bc4a66804fbf4774b2cc26ffd4bb      1
    30 0xa7ecfc1e491baeec24b33ed24bc84916af54f240      1
    31 0xab557245f9de18a7a2b59e8bc4f361dd83d16807      1
    32 0xadcf829d51724329e6fe859a933a9ece93c22610      1
    33 0xbefe5d435616619253be2e448310f70136d0fddc      1
    34 0xc79556a222367ecb3c2291d5c6d2b3e58e25fbf1      1
    35 0xc83bcd5fc46f61fe331bd63398f776adcb711f12      1
    36 0xca89443bc26ce9a6ba429042f678353ef3e54841      1
    37 0xcd8b72710595c69019c109aef5b1b92eea7f995f      1
    38 0xd35fc346e15ba5b446917c9fd23a9471d6144701      1
    39 0xdcd50efb4ccdc9c409b4a4d8eb919ed301c86abe      1
    40 0xdcfa171d1d3e09aab3a3b85b72c0ef458d574a54      1
    41 0xe198be4bb62bb3109cd6e51c50991906c9937381      1
    42 0xe681e5232da0c9ba5d28e6775bb3b6945eaf50f4      1
    43 0xec23992d5da7feefdf6e21dfc688d0b56082d1b5      1
    44 0xec63e88ba3d5482cbfa59872d2d889ba57796e17      1
    45 0xf0b606b9d4d8809eb41a744eb19be23b0dea74df      1
    46 0xf2439241881964006369c0e2377d45f3740f48a0      1
    47 0xf7830c1e0df63386a20058ac3a811beffdb88abf      1
    48 0xff53ddc9e7cc87a21f19e10858edcba53b009832      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 70 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x04b5a01e011352624245a5150d7f36e754ef8b62      1
     2 0x052564eb0fd8b340803df55def89c25c432f43f4      1
     3 0x07218f914320e3a4b9c80f9867bf0a47cb6ebb79      1
     4 0x092cd1a6d222a167f5d0767e6444c8b45c92cc72      1
     5 0x0940d80b446f303a98fc596cb401e9a966b55022      1
     6 0x0d71ec30c323037bdac7bd174d4a95cec5a4ee0d      1
     7 0x0f0eae91990140c560d4156db4f00c854dc8f09e      1
     8 0x119fcd292b95551e8af472fc4721ac34cc5f48e5      1
     9 0x182e0862fe4b624c9b7199df8fa52b07f4283632      1
    10 0x193bb5dcb851ef8f75dbbd6950c962c3a5f4b0e3      1
    11 0x1c2ea5a58d54914e06bee9932f5acd0a622930e8      1
    12 0x27b455c481d62f91a7b55d09cf2e3cd70858ef85      1
    13 0x33e8d10459643faab56f1974291cb603b88a09f8      1
    14 0x37009f9ae9dcc0a6d59204e1aeaf9da4968caec8      1
    15 0x3dc94530601d9ba00a014ae7e0210dcf9960eb67      1
    16 0x3e0914fc2b3b67a476142020b7d6990d01ad234e      1
    17 0x3e7374ee60f296ef2350c47a5110cb5233321756      1
    18 0x42a41d6874dcb96596bd92e2e5f881c341df38c5      1
    19 0x465a646e92746a7191ec6ee7223d3b78d20367e5      1
    20 0x46b249da04697c801cd592030f6ef81f9433398f      1
    21 0x46b2bd5c888e6d57a839c559fd6076f2c15a8cb1      1
    22 0x478bb542f7658d635abba67edb987806dff5b83d      1
    23 0x4b4d75b8b0fc7528ea2614b2ca83555824c07867      1
    24 0x507c20ef269726b2d0b8efe9f6b75a4ddd9ef741      1
    25 0x53314ed80d217744eaa3cfa13fea6692d08627ac      1
    26 0x5aa71418ac6238c78ea9a032a768dbbbf86bfeca      1
    27 0x5f9d41289ad44d17e6c51f889276999112e4fffc      1
    28 0x63be81f380d82134b71ca8663142e8dc63f7c8a8      1
    29 0x66ff8f288c413e61c8ee807088ce301c03076e6e      1
    30 0x6e63a4caeccb4f341ee9c9175c9cc554bdb6d10b      1
    31 0x74401b66f746630ccee98689b1a20e0149fbccab      1
    32 0x76f29c135b3062d53d5ed70f2e21d5978f50bdc1      1
    33 0x78d65f91babbcf9f08061ce21ae7443a69f6f07f      1
    34 0x79f238d3d0dba4bcba894be2e972b0818aeeaf3c      1
    35 0x7af2e406bcc85580f2994f6cee959b280c6e0c32      1
    36 0x7e5124202b1a177cbc254b2c2262541682f20601      1
    37 0x7fd1d8e388b29ef0b8a00ca07e2266686a5559ee      1
    38 0x8641cfc99087659f0b84174a137d2c7f6fe861c2      1
    39 0x8abe2749b67ad7debb7442c46ea4a0e003d4852a      1
    40 0x90a8734076d7a7db4303fed64cf46467069f11d8      1
    41 0x938a0af4b86057489bc651dd02c080890d8ed5e5      1
    42 0x973ef7e4e91cc9098709d540a8ddfb708b331c87      1
    43 0x9b65bcad994551a7ee35be0750f45e444e7f5cc2      1
    44 0x9b848370417c9f525d9168f2aa91a2cd819240e8      1
    45 0x9dbef4969a9b01994a05867b2c25c169c60c7048      1
    46 0xa1168968174d3a4f2b6666051800115d86fdc235      1
    47 0xa549487362b9bc4a66804fbf4774b2cc26ffd4bb      1
    48 0xa7ecfc1e491baeec24b33ed24bc84916af54f240      1
    49 0xab557245f9de18a7a2b59e8bc4f361dd83d16807      1
    50 0xadcf829d51724329e6fe859a933a9ece93c22610      1
    51 0xae060f1ac3ed0b80cdd7e6f271c139fedc5a3d2f      1
    52 0xbefe5d435616619253be2e448310f70136d0fddc      1
    53 0xc79556a222367ecb3c2291d5c6d2b3e58e25fbf1      1
    54 0xc83bcd5fc46f61fe331bd63398f776adcb711f12      1
    55 0xca89443bc26ce9a6ba429042f678353ef3e54841      1
    56 0xcd8b72710595c69019c109aef5b1b92eea7f995f      1
    57 0xcd94d3aa70986658d876ed6497636167014b1d1e      1
    58 0xd0ed643099041b805adec2e2af3329c06a20ffe6      1
    59 0xd35fc346e15ba5b446917c9fd23a9471d6144701      1
    60 0xdcd50efb4ccdc9c409b4a4d8eb919ed301c86abe      1
    61 0xdcfa171d1d3e09aab3a3b85b72c0ef458d574a54      1
    62 0xe198be4bb62bb3109cd6e51c50991906c9937381      1
    63 0xe681e5232da0c9ba5d28e6775bb3b6945eaf50f4      1
    64 0xe75764a435f7831e8dfb960dbd85f01f78b84f47      1
    65 0xec23992d5da7feefdf6e21dfc688d0b56082d1b5      1
    66 0xec63e88ba3d5482cbfa59872d2d889ba57796e17      1
    67 0xf0b606b9d4d8809eb41a744eb19be23b0dea74df      1
    68 0xf2439241881964006369c0e2377d45f3740f48a0      1
    69 0xf7830c1e0df63386a20058ac3a811beffdb88abf      1
    70 0xff53ddc9e7cc87a21f19e10858edcba53b009832      1

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
