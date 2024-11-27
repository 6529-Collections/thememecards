
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:82          Length:82          Min.   :1   Length:82         
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:82         
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 21246969 # https://etherscan.io/block/21246969
block_hash <- "0xabdd89d2b8753b380a23b2661d8f44e893d16559d12a3be8af53dcf80b2c46d0"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4699 

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

allow_artist1    <- pick(snapshot, contracts=c("Foundation","Blossom","MAB369","A369","GLB","11","Pepeforever","Fashion","MakersPlace"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("MABEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("Foundation","Blossom","MAB369","A369","GLB","11","Pepeforever","Fashion","MakersPlace","MABEditions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 30 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x035da52e7aa085f7e71764c0c8a1ce6690e3dfef      1
     2 0x04173063c0c491470b735954790f94ed307aae9d      1
     3 0x07579c800c1687cbb0d0d37df893d2689209453f      1
     4 0x1dd6b5397a802730422c8388d51013d005d82d1c      1
     5 0x20e7bfeef1d38899ec88124829dd34e711c1e26c      1
     6 0x2aaba4b94a805f0fea696dc11de324c2320ff4e1      1
     7 0x30dec07af807a47c4c5a2ba69a22738831f002b8      1
     8 0x4756e5a5d0668195feb1ec0e7f96832ffc51c4a8      1
     9 0x4f94fa697f4eaa8c25a097fa0c9d254cdb114465      1
    10 0x4f9604e7f15d4757f54a39a906d6425ab47dc80b      1
    11 0x5568945b2978f255e653f364acff5c1aa85d83a2      1
    12 0x5c5b6cade3f45fca78aac5a9877f1b73c51300e3      1
    13 0x6fbe3ee1f62cd0d7e6942dffe4ccf8c8c3993520      1
    14 0x745af2056a6fe8f5ec1e0f6bf532061f786cb9f4      1
    15 0x75aadbb7c36db057e5ac64183899f118de76df35      1
    16 0x8888888888e9997e64793849389a8faf5e8e547c      1
    17 0x88888888eef9d7648d15ad26c68d1653464856b8      1
    18 0x8e3ee7d42540e97ba650a12ea35fe267339ddd04      1
    19 0x93c838add3ac72b42f65417408f900a7155e1bb8      1
    20 0x951038bb372d16180c0afb0f46ab283059154004      1
    21 0x9e53563dbfea80b39bf683b7d52df449a2b247e7      1
    22 0xa22414e4af5767448624017c7e0151d22490412b      1
    23 0xc255faae7ac9422e7e8627f4699b1c3d2469d7e8      1
    24 0xc979a6e2f98d31bec9669d088dcf787a2a0cce57      1
    25 0xe80a275d513ef28e2f0f8fd4d55fc32a294a480c      1
    26 0xf58dbd74f26e4bdbcf2a26e2f848e8348530f0e4      1
    27 0xf86780ede26501cfc4649b4c1d7629e68a10f314      1
    28 0xfa53981b7e4f55a0590e88a72232ba843fe8d08d      1
    29 0xfae08711a394c25a8564436b6a13473d5fa9482b      1
    30 0xfbc76261fd55cf91b81a97dbcc2b3f6118f2b935      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 14 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x04eeee5127bb2015e62bb56d0de983f3e0242779      1
     2 0x13b745fc35b0bac9bab9fd20b7c9f46668232607      1
     3 0x3441cab3d341b355b7a7420bf1ad2bc5a379baed      1
     4 0x9984f76f69a556c1e65bdc07c188245d74584c7e      1
     5 0xa071dc9b8367b59dbf5fffcbf5565c30425045ce      1
     6 0xa46e876c3bab36cc0443be11ff1f1ed81e3a54a0      1
     7 0xb26145160a80616dc909885aeda90af97eea349e      1
     8 0xc51b914201aa84f608f63aca674eb263bafa1961      1
     9 0xc73908441bfd21b6039e776b52d1f584b229c8d0      1
    10 0xd7613b407a9eb751f7e391aa2588063ee4e883d1      1
    11 0xe50afee4482e3805c65265748bd99293203f2728      1
    12 0xe5cc81377f1041773b4d4045fcdf12ab62e4d24d      1
    13 0xea87d49cc404d3f2f453cb2dbdc445c7de43946e      1
    14 0xf4b8b10999afe08966aa4a1d9a6596fb1697f2be      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 44 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x035da52e7aa085f7e71764c0c8a1ce6690e3dfef      1
     2 0x04173063c0c491470b735954790f94ed307aae9d      1
     3 0x04eeee5127bb2015e62bb56d0de983f3e0242779      1
     4 0x07579c800c1687cbb0d0d37df893d2689209453f      1
     5 0x13b745fc35b0bac9bab9fd20b7c9f46668232607      1
     6 0x1dd6b5397a802730422c8388d51013d005d82d1c      1
     7 0x20e7bfeef1d38899ec88124829dd34e711c1e26c      1
     8 0x2aaba4b94a805f0fea696dc11de324c2320ff4e1      1
     9 0x30dec07af807a47c4c5a2ba69a22738831f002b8      1
    10 0x3441cab3d341b355b7a7420bf1ad2bc5a379baed      1
    11 0x4756e5a5d0668195feb1ec0e7f96832ffc51c4a8      1
    12 0x4f94fa697f4eaa8c25a097fa0c9d254cdb114465      1
    13 0x4f9604e7f15d4757f54a39a906d6425ab47dc80b      1
    14 0x5568945b2978f255e653f364acff5c1aa85d83a2      1
    15 0x5c5b6cade3f45fca78aac5a9877f1b73c51300e3      1
    16 0x6fbe3ee1f62cd0d7e6942dffe4ccf8c8c3993520      1
    17 0x745af2056a6fe8f5ec1e0f6bf532061f786cb9f4      1
    18 0x75aadbb7c36db057e5ac64183899f118de76df35      1
    19 0x8888888888e9997e64793849389a8faf5e8e547c      1
    20 0x88888888eef9d7648d15ad26c68d1653464856b8      1
    21 0x8e3ee7d42540e97ba650a12ea35fe267339ddd04      1
    22 0x93c838add3ac72b42f65417408f900a7155e1bb8      1
    23 0x951038bb372d16180c0afb0f46ab283059154004      1
    24 0x9984f76f69a556c1e65bdc07c188245d74584c7e      1
    25 0x9e53563dbfea80b39bf683b7d52df449a2b247e7      1
    26 0xa071dc9b8367b59dbf5fffcbf5565c30425045ce      1
    27 0xa22414e4af5767448624017c7e0151d22490412b      1
    28 0xa46e876c3bab36cc0443be11ff1f1ed81e3a54a0      1
    29 0xb26145160a80616dc909885aeda90af97eea349e      1
    30 0xc255faae7ac9422e7e8627f4699b1c3d2469d7e8      1
    31 0xc51b914201aa84f608f63aca674eb263bafa1961      1
    32 0xc73908441bfd21b6039e776b52d1f584b229c8d0      1
    33 0xc979a6e2f98d31bec9669d088dcf787a2a0cce57      1
    34 0xd7613b407a9eb751f7e391aa2588063ee4e883d1      1
    35 0xe50afee4482e3805c65265748bd99293203f2728      1
    36 0xe5cc81377f1041773b4d4045fcdf12ab62e4d24d      1
    37 0xe80a275d513ef28e2f0f8fd4d55fc32a294a480c      1
    38 0xea87d49cc404d3f2f453cb2dbdc445c7de43946e      1
    39 0xf4b8b10999afe08966aa4a1d9a6596fb1697f2be      1
    40 0xf58dbd74f26e4bdbcf2a26e2f848e8348530f0e4      1
    41 0xf86780ede26501cfc4649b4c1d7629e68a10f314      1
    42 0xfa53981b7e4f55a0590e88a72232ba843fe8d08d      1
    43 0xfae08711a394c25a8564436b6a13473d5fa9482b      1
    44 0xfbc76261fd55cf91b81a97dbcc2b3f6118f2b935      1

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
