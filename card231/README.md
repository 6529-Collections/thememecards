
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:37          Length:37          Min.   :1   Length:37         
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:37         
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 19846969 # https://etherscan.io/block/19846969
block_hash <- "0xb4c64a3b41c50d1f5b51508dd56b403d6e74fcc6045f280c934ba9e8fb0654cf"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4688 

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

allow_artist1    <- pick(snapshot, contracts=c("Foundation","PEPE","PIXELFANTASY","SpaceGamesRobots","BombadilVideoGames","PEGZFRIENDS","MechaMovers","SPECTRUMGLITCH","SuperRare"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 24 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0c76717074a973f3f6e3d422166c999483f6c7fd      1
     2 0x16722731f337609fc2554bb606170427b6456a51      1
     3 0x2debdf4427ccbcfdbc7f29d63964499a0ec184f6      1
     4 0x326ce9984f582d2021d67be7e5e32281832dfc59      1
     5 0x33d7a3ad5b4168f30c051ee66afd3c2a865ed919      1
     6 0x406bbd4b112e77d7091e36c23b92cca918b5419a      1
     7 0x43b31a62ada9bd2f1c617a021dc9dd0f81cb8681      1
     8 0x51ce28855ea50b4eac9656230adf738a5813ca04      1
     9 0x5849734d2e3ada23b64e12311adfa6bcd6fe687c      1
    10 0x6112083d4bb12dbf64e4850842685aaefbeacb10      1
    11 0x6410edfbdacce91f466d2958d41109a880f607d5      1
    12 0x6519f68ca26c7f59deeabce7194da4d14e1f2847      1
    13 0x6db056b0edbecb60d227f460ad47d8cf2df2dc14      1
    14 0x8888888888e9997e64793849389a8faf5e8e547c      1
    15 0x88888888eef9d7648d15ad26c68d1653464856b8      1
    16 0x97064190190d4e2fb284a2d55dd53504e291c4cd      1
    17 0x973eba6ec3ca78ff3af5a3a9b85f44f8628da870      1
    18 0xa0a112f44b44d49c8401cc7f223566187cacd2e4      1
    19 0xbdb0dd845e95d2e24b77d9bef54d4df82baf8335      1
    20 0xbfc9f6c46715755e2ee8b01f20ab36d22cb4ad4f      1
    21 0xc97958ff5370c56b7e0e6fb58437f00702714d49      1
    22 0xd2dbe21025dede10517a8be905deb0af0bf2cfbc      1
    23 0xeb43b5597e3bde0b0c03ee6731ba7c0247e1581e      1
    24 0xfc7d068f069e712dd35b439bf6ed9df8f58d07de      1

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
