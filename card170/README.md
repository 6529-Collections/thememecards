
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:75          Length:75          Min.   :1   Length:75         
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:75         
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 18596969 # https://etherscan.io/block/18596969
block_hash <- "0x5f8b821141cf306d58df4da16bea0e79a029d651a87e5772c4f3bcff705f6cfe"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4805 

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

allow_artist1    <- pick(snapshot, contracts=c("SuperRare","Foundation","BluuguMoments","bluuguartefact","BluuguMoments"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 51 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x083cf6d6fcf2e42b47976c236f3d0d6f15423019      1
     2 0x157ae65f17c8eac3d76b9a5253108537025afdf1      1
     3 0x15e33839a35482698c83847081a189e4c59dafc2      1
     4 0x170c3fd3ad13ab3aa85735fe1623f4d4df1bf5f4      1
     5 0x17fa6f6b1c44336969b6c02ce7063f7106395fac      1
     6 0x1b7f00ca9d8fea5a9853f18ad247f806a8dce57a      1
     7 0x1cc2566f458e29b2b648b751a59efbf3ac21d678      1
     8 0x2debdf4427ccbcfdbc7f29d63964499a0ec184f6      1
     9 0x31c72d0d03adc76456d3c14a4fe19f8aa8307c16      1
    10 0x31c89730817b4a2ad30eb64ebf2bdcf3043d76ba      1
    11 0x321191d4c161a0eca8e4d9de044452b7c38376e1      1
    12 0x324fadf3789e92b0f19d4b896974a612840062d5      1
    13 0x3852471d266d9e2222ca9fdd922bafc904dc49e5      1
    14 0x389db2812b5571d7ce4f03466c91b26581c9ea44      1
    15 0x3a64ff88d44c432204afa4ce5028dc9a9091bc90      1
    16 0x3f7d1580862aa750c26980c944a03ecf57019e9f      1
    17 0x43911ca91ab74c8d90381b408e08c7c36792e062      1
    18 0x4b46f1e241838a910945a3fac242ddf6f5d7c041      1
    19 0x5a17fb43794212f5fac2298747c8757b8dd94a17      1
    20 0x5a62ea69c58a14bf16367b14b73a014d90e40118      1
    21 0x6496039c9f0b4f3f8abff54b2a2e67788a9641b0      1
    22 0x6f1186be2ae811b9e9e8ebb8bbe021d2c38a87d7      1
    23 0x7b640407513bc16167ef3450fd6339803982e976      1
    24 0x862eb245e19172dc3da2e16248ade9deba757004      1
    25 0x86fef6ec5320f6cf9231f524ae89e198419cdc0f      1
    26 0x88bd0324fc0b474b3c5860669492a1b75ecb565b      1
    27 0x89b664cbf1ffa0fda453830a56eb2f8abb42ac8c      1
    28 0x8af3e5d0b93332a972f02c50767befa71408ce86      1
    29 0x946237dd48b0751d59df97487ce483a0b27cd2d6      1
    30 0x9742fab7be7c7bd7a3e0af4c8f3b308fc7d64fc6      1
    31 0x9769334fc882775f4951865aa473481880669d47      1
    32 0xa3a8585c8481742a0f2c96b1b5aa84d596a51a2c      1
    33 0xafad28b439834c174fd251756a5a7b025e4f558d      1
    34 0xb188ee1daca51ce6b58ccf8a81ce4025a714bc73      1
    35 0xb3f0771103120f80ee3f2fdedf9c856a3af683b1      1
    36 0xb4188367021173e8e352e90152566ca3bd939fba      1
    37 0xb63a5c5710f06e111fa14ac82da2183c5102b504      1
    38 0xc5da215997d4439b5d120e8926dce0b3de73e624      1
    39 0xc63dc0f9324fcd308a72e5a8e1d983f7e232b705      1
    40 0xd19286bdd1a2b06df3c1105407c38511ffb87959      1
    41 0xd4091d661a44648d61bd3bb51e129d0d60892056      1
    42 0xdab36a4ce12aa220e18c60ef0de489c232322a1a      1
    43 0xdb28c2f1438a5158bebdabeccf98c0cc164282ab      1
    44 0xe2523a55e885318ce20621133ad31005d193df56      1
    45 0xe359ab04cec41ac8c62bc5016c10c749c7de5480      1
    46 0xe74166afb1a6c0ca3c355ba33965a8c7fc3e2616      1
    47 0xe94f5002581b2e841f35ee810d6f2b8bbc986210      1
    48 0xebc453e098ea5ca81e216441891c84bc4fb6e8e6      1
    49 0xf19ab0e65c902ffedcb95a76a93ca7bf789239ed      1
    50 0xf65ffed29a2e7ca9554cfea52ea500b5adc5fc13      1
    51 0xfdf7a7a9ff5cbac5d1a5db56af059f593b329380      1

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
