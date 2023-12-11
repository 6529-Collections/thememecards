
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance        contract        
     Length:87          Length:87          Min.   :1.000   Length:87         
     Class :character   Class :character   1st Qu.:1.000   Class :character  
     Mode  :character   Mode  :character   Median :1.000   Mode  :character  
                                           Mean   :1.023                     
                                           3rd Qu.:1.000                     
                                           Max.   :2.000                     
         name          
     Length:87         
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 18747269 # https://etherscan.io/block/18747269
block_hash <- "0x3640ce0c1687e9b8cd22a01cee40fb7e1099a00137edb8e39851f6ebd36f44d1"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4692 

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

allow_artist1    <- pick(snapshot, contracts=c("SuperRare","SuperRare2","FranRodrguez11","Foundation","Wisdom","SecretSociety"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("AlpenglowEditions","UnnamedIIEditions","TheDreamerEditions","VanityEditions","NGEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("SuperRare","SuperRare2","FranRodrguez11","Foundation","Wisdom","SecretSociety","AlpenglowEditions","UnnamedIIEditions","TheDreamerEditions","VanityEditions","NGEditions"), address_remove=address_remove,address_max=1)
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
     1 0x052564eb0fd8b340803df55def89c25c432f43f4      1
     2 0x057cc058adc5564df77c6aee28ac04ac62d6e1a4      1
     3 0x0f79fc26408ef7fc1d193187685eac9ea921858e      1
     4 0x1cead4aa504383c0aba0598781236127fc3af316      1
     5 0x2bc4a480f8ad345b398abb23854e3062d5c57c51      1
     6 0x2e47f5d3961d35d12e4feb5e79e02384bd0d56b6      1
     7 0x31c72d0d03adc76456d3c14a4fe19f8aa8307c16      1
     8 0x3432b45b9ee95bd5c31a726b936cf2ec719a2153      1
     9 0x3b22621a2b0e1f8dd3056975efc2150d5fbee980      1
    10 0x604c3667786bcaf593018236a9e314334a99589d      1
    11 0x665654f2d2a151be2d0f8e3697e6ce780f732af2      1
    12 0x6b6ae848f555f70944bc99d736fe29fcaecf8b23      1
    13 0x6dc43be93a8b5fd37dc16f24872babc6da5e5e3e      1
    14 0x7d544a853dbcd39a53315e7002f4951a6d2f080d      1
    15 0x89d349cac2fcda7c9e87e9cd261b1828304df3e0      1
    16 0x8ac236f975c5b44948c45c662834e28f017c7097      1
    17 0xc65127557c1eef1469de35d2ff00b8aeab8eb154      1
    18 0xca536f1a439eadb69643b850a198939bae77ee4e      1
    19 0xd179ed69b7962f48af502a1fac9abeda70e83a6b      1
    20 0xd1e8a92f44bdc83bc620c56a7913fd97de5abe10      1
    21 0xd4d05fe29a112ebd5cc8f463f716c204ba5f70ce      1
    22 0xe01faf4bcfc290ed8f8335825db710a5c4437336      1
    23 0xe14397613d3c700d3abb63d15ff481d09841ad8f      1
    24 0xfd9ce79fd7f62ca88ace958cd2716f4cce25e2df      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 43 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0d53aba24d961c60a12bd7bbd0c5bc28df00a600      1
     2 0x129f7af67d4d62ffeafadccd9be6566f0e33be48      1
     3 0x12e7df22c809ac3fb4a4fcbda3b492ee9e5bf93d      1
     4 0x13a7d2fb3f73b57ef539c35c4ec59ed49b1833ca      1
     5 0x1c8ba193f5d7fde5ee2072b799390bb1283157f1      1
     6 0x1e47fc142bc94a0b7f6870d1d37055855930536d      1
     7 0x216506985f6de2f2ecda7498f8de8e46d4b832ec      1
     8 0x24673b7ae3cd2f69a2e49b6d0a313f6a1007ecd5      1
     9 0x2a17c76955d6a7130a31091139e46a9491c99e12      1
    10 0x2b91f332616084654098d4c13ceb9e82c8b73848      1
    11 0x2e6af0db64ceb81f393265242c018130acb7d306      1
    12 0x330bce303d27df0eb1b856da3464278db1db1ac5      1
    13 0x333fccc5046da80e10078363f7e0faefe08aeab3      1
    14 0x33d00dce2942a33e2b553b0b206b24f160799bb9      1
    15 0x368aedb06bb81adc81bdbf9368e5c72134394789      1
    16 0x374fe022810a90d6a64b4789e18eea6b62517457      1
    17 0x3b23b754a8f59c9adaf721302a40862e387aaa4c      1
    18 0x3ccaa6158ac53054fccb904dfc1407a18befe3c9      1
    19 0x47c553e889f7bfa75cf3775473180a04e9effcfa      1
    20 0x52f9b3aa0cb7cad168e4756f9d3306d0b59c526b      1
    21 0x543774d6a9e0c8fec5248f1d7d1512848f4a2b35      1
    22 0x571b8db726c2a29d237f88c9efeb896290bf395b      1
    23 0x5f4ef1da0a7b2181d321f67de9efd7cbc4b32868      1
    24 0x62d03153358ff577ee0dcf1db555a687f59a8875      1
    25 0x6d2ad2c79a8a8c4a6c77140dfc56bda96e0ddac3      1
    26 0x767a58ba5b0e404f5befbde4d7f00926df568fe4      1
    27 0x772e3295e83fa106f327c21de8cd032dcd563327      1
    28 0x863051567c4b24efff0f7584e265e86cdb0c5d3e      1
    29 0x8ed55d7ae8a86032bfb78eedfd318086f9bc43bc      1
    30 0x9f1c6db9f8603bb40126f9f17b339fdb5b7bd13f      1
    31 0xa4e12ce3955fc4289dec95c5e2a696ba24a845cf      1
    32 0xa812ae608d18b528da5683cdc77f6334c706ef74      1
    33 0xb39734396b95660707ef7f3d3a76e8566ee34377      1
    34 0xbb9beeaa6202c7c29cbf40fe2a28133211b3a63b      1
    35 0xbfacde4341f32b5f77ad81831e128ede4b5e6073      1
    36 0xc8f2f8978798d4e8a8f89df6e68866cbf454d926      1
    37 0xcc48857242141cc7f4f8555f8a3d5bed03556c19      1
    38 0xcc72d9e9525b55297c03c2d09b9677617a02d156      1
    39 0xccafc39c3a5be579f191334feaf3bce3316a46a2      1
    40 0xd6ee002752c995a118e42611baf52aba7cc1fef1      1
    41 0xd7d1c6484e4b8b3c09c7b9ca8722aa52b0046c12      1
    42 0xf2439241881964006369c0e2377d45f3740f48a0      1
    43 0xff559fc20d9b6d78e6e570d234b69c18142bb65e      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 67 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x052564eb0fd8b340803df55def89c25c432f43f4      1
     2 0x057cc058adc5564df77c6aee28ac04ac62d6e1a4      1
     3 0x0d53aba24d961c60a12bd7bbd0c5bc28df00a600      1
     4 0x0f79fc26408ef7fc1d193187685eac9ea921858e      1
     5 0x129f7af67d4d62ffeafadccd9be6566f0e33be48      1
     6 0x12e7df22c809ac3fb4a4fcbda3b492ee9e5bf93d      1
     7 0x13a7d2fb3f73b57ef539c35c4ec59ed49b1833ca      1
     8 0x1c8ba193f5d7fde5ee2072b799390bb1283157f1      1
     9 0x1cead4aa504383c0aba0598781236127fc3af316      1
    10 0x1e47fc142bc94a0b7f6870d1d37055855930536d      1
    11 0x216506985f6de2f2ecda7498f8de8e46d4b832ec      1
    12 0x24673b7ae3cd2f69a2e49b6d0a313f6a1007ecd5      1
    13 0x2a17c76955d6a7130a31091139e46a9491c99e12      1
    14 0x2b91f332616084654098d4c13ceb9e82c8b73848      1
    15 0x2bc4a480f8ad345b398abb23854e3062d5c57c51      1
    16 0x2e47f5d3961d35d12e4feb5e79e02384bd0d56b6      1
    17 0x2e6af0db64ceb81f393265242c018130acb7d306      1
    18 0x31c72d0d03adc76456d3c14a4fe19f8aa8307c16      1
    19 0x330bce303d27df0eb1b856da3464278db1db1ac5      1
    20 0x333fccc5046da80e10078363f7e0faefe08aeab3      1
    21 0x33d00dce2942a33e2b553b0b206b24f160799bb9      1
    22 0x3432b45b9ee95bd5c31a726b936cf2ec719a2153      1
    23 0x368aedb06bb81adc81bdbf9368e5c72134394789      1
    24 0x374fe022810a90d6a64b4789e18eea6b62517457      1
    25 0x3b22621a2b0e1f8dd3056975efc2150d5fbee980      1
    26 0x3b23b754a8f59c9adaf721302a40862e387aaa4c      1
    27 0x3ccaa6158ac53054fccb904dfc1407a18befe3c9      1
    28 0x47c553e889f7bfa75cf3775473180a04e9effcfa      1
    29 0x52f9b3aa0cb7cad168e4756f9d3306d0b59c526b      1
    30 0x543774d6a9e0c8fec5248f1d7d1512848f4a2b35      1
    31 0x571b8db726c2a29d237f88c9efeb896290bf395b      1
    32 0x5f4ef1da0a7b2181d321f67de9efd7cbc4b32868      1
    33 0x604c3667786bcaf593018236a9e314334a99589d      1
    34 0x62d03153358ff577ee0dcf1db555a687f59a8875      1
    35 0x665654f2d2a151be2d0f8e3697e6ce780f732af2      1
    36 0x6b6ae848f555f70944bc99d736fe29fcaecf8b23      1
    37 0x6d2ad2c79a8a8c4a6c77140dfc56bda96e0ddac3      1
    38 0x6dc43be93a8b5fd37dc16f24872babc6da5e5e3e      1
    39 0x767a58ba5b0e404f5befbde4d7f00926df568fe4      1
    40 0x772e3295e83fa106f327c21de8cd032dcd563327      1
    41 0x7d544a853dbcd39a53315e7002f4951a6d2f080d      1
    42 0x863051567c4b24efff0f7584e265e86cdb0c5d3e      1
    43 0x89d349cac2fcda7c9e87e9cd261b1828304df3e0      1
    44 0x8ac236f975c5b44948c45c662834e28f017c7097      1
    45 0x8ed55d7ae8a86032bfb78eedfd318086f9bc43bc      1
    46 0x9f1c6db9f8603bb40126f9f17b339fdb5b7bd13f      1
    47 0xa4e12ce3955fc4289dec95c5e2a696ba24a845cf      1
    48 0xa812ae608d18b528da5683cdc77f6334c706ef74      1
    49 0xb39734396b95660707ef7f3d3a76e8566ee34377      1
    50 0xbb9beeaa6202c7c29cbf40fe2a28133211b3a63b      1
    51 0xbfacde4341f32b5f77ad81831e128ede4b5e6073      1
    52 0xc65127557c1eef1469de35d2ff00b8aeab8eb154      1
    53 0xc8f2f8978798d4e8a8f89df6e68866cbf454d926      1
    54 0xca536f1a439eadb69643b850a198939bae77ee4e      1
    55 0xcc48857242141cc7f4f8555f8a3d5bed03556c19      1
    56 0xcc72d9e9525b55297c03c2d09b9677617a02d156      1
    57 0xccafc39c3a5be579f191334feaf3bce3316a46a2      1
    58 0xd179ed69b7962f48af502a1fac9abeda70e83a6b      1
    59 0xd1e8a92f44bdc83bc620c56a7913fd97de5abe10      1
    60 0xd4d05fe29a112ebd5cc8f463f716c204ba5f70ce      1
    61 0xd6ee002752c995a118e42611baf52aba7cc1fef1      1
    62 0xd7d1c6484e4b8b3c09c7b9ca8722aa52b0046c12      1
    63 0xe01faf4bcfc290ed8f8335825db710a5c4437336      1
    64 0xe14397613d3c700d3abb63d15ff481d09841ad8f      1
    65 0xf2439241881964006369c0e2377d45f3740f48a0      1
    66 0xfd9ce79fd7f62ca88ace958cd2716f4cce25e2df      1
    67 0xff559fc20d9b6d78e6e570d234b69c18142bb65e      1

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
