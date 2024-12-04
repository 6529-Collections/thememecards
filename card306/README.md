
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:122         Length:122         Min.   :1   Length:122        
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:122        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 21296969 # https://etherscan.io/block/21296969
block_hash <- "0xd585e5d015d0adb3c3d63e58733c0216620d23aa7211bcac8757e6dc7a8f3444"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4583 

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


allow_artist_all <- pick(snapshot, contracts=c("SuperRare","baiweiFND","seal","ZATO","baiwei","CHRoNCLASH"), address_remove=address_remove,address_max=1)
```

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
     1 0x039ec159a2bf718516185adefae02c476bf79813      1
     2 0x047ac0ba72c6e5d9bd97f02037df075cb7cf286e      1
     3 0x055580e7b88225dae30dc1305ad03aba43686449      1
     4 0x09bc641b6fa2308b030c312653f9675dfaa58e2b      1
     5 0x0b83d0c0976b71724054e9f74a471f7d1c7abdc4      1
     6 0x0ea9f5d0c97b430d5426465e2832a504bd6dd9f9      1
     7 0x13c8d03c54e2d3642d0412d2e1969e3b783368b4      1
     8 0x2df03775b120a966a9dc78d6c0262e20ed9697f0      1
     9 0x2f0d1767a1de48f7328985c9915b85341418e901      1
    10 0x335e19b3f5dcfa2e398ee7d5ff430a1f8ccc88b2      1
    11 0x36af22ee7d4ff3f1289e08804edc4df82d248a85      1
    12 0x402f51a590f54f4c5ce9c14bec1181156ecd2da1      1
    13 0x40cf19158b7104298527db2090ce88c783c7565d      1
    14 0x423b4ede81784027f38dd9fda7e324efa896f230      1
    15 0x48c290b4e0273c57a30d9ce41cf4ff27fcf79317      1
    16 0x4bc739c61818c15765272fb90435530687ab9caf      1
    17 0x4def29da0614a4fb3155dea7984f37c2b99b1d8d      1
    18 0x55c06d471a6f04b3933947ce0af9211b85f3cf82      1
    19 0x5b354cbfec21373c1a019a6d6f8a6c45d425573f      1
    20 0x5b91d8975cd743a4e345ca99776a33c5432ea163      1
    21 0x5fbf93d92e237e93ab00f3289cd289ea806261a8      1
    22 0x6540d468bd75ae655ac1939a0398ef2c58828fff      1
    23 0x6f737253039a0c0540bf689ada3b516baffe175b      1
    24 0x7298b0fb3fbb1a2900364f45f9f0f92b65ace069      1
    25 0x72b54fc5fb73928ecb9a0681ec9bb7217a977e19      1
    26 0x8185f88f37be8073863822fa2fb91acc023681a1      1
    27 0x832d0917b83cc41d5c049493ab6fc34ff6074af0      1
    28 0x8cdd754b3059d7c82f69246e6b1c5e94becda370      1
    29 0x9428e55418755b2f902d3b1f898a871ab5634182      1
    30 0x9ba87eafb8f9c4c19c21a40b346fe718917970f9      1
    31 0xa4a8f770643750f0e05189fea7013524a9817ab7      1
    32 0xa7ef08585b7188bcc140053b039933620bf7e04f      1
    33 0xabc9b3af7ba302c60049dc826fb2532b37504196      1
    34 0xb1008e36d294c236be3c7c95e10ce4c6242ccd5d      1
    35 0xb893e57d4059635089c905b4eafb15d1b23f99c4      1
    36 0xcf10097022fe92fa601eaa8beeff6658c3dc1b12      1
    37 0xdac86e3ecdf4dc8535dc036e166a89f34a62f278      1
    38 0xdf85b9146d73a852d7b900d1abd974f2a8119f57      1
    39 0xe4b4bab0fad75de00e620deec5af7c137040fff1      1
    40 0xe504759749dfb8edfcbf528e8fad4b94c9e87e6e      1
    41 0xe619d906188cff594d3396c1f82f66ef00dea82c      1
    42 0xe653cb5b106580c6e434c78f3882bbc587e9bc97      1
    43 0xe803aad78e6eabcde6f820d2c64cf83402eddbe2      1
    44 0xe99322e71900ef0d4e93aa1fae17666c1294640a      1
    45 0xf4186e22b1508f3bfee9ccfe4546dfc136bf2aa6      1
    46 0xf4e79c9d2fe8b68eab649893dd51608f1e8e9985      1
    47 0xf777af5cfa7a0f242b3e0cf7fff4823c15dde70f      1
    48 0xfbc76261fd55cf91b81a97dbcc2b3f6118f2b935      1
    49 0xfd2d9a0346623f17f6acd704158f3817d7feec50      1
    50 0xfe5e90ba7cdaadba443c0c27010eb1d51327eafb      1

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
