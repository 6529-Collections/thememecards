
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot2.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:20263       Length:20263       Min.   :1   Length:20263      
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:20263      
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 17832869 # https://etherscan.io/block/17832869
block_hash <- "0x2e45a9c808911764aaef378cc07c7396d1921d51112cd9c8dcbb782a1eb3b966"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4610 

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

allow_memesRandom2_phase1 <- pick(snapshot, contracts=c("memes"), address_remove=address_remove,address_pick=100,address_max=1)
```

## Allow Random Memes 2 Phase 1

``` r
c(allow_memesRandom2_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_random100Memes_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 100 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x00dbe3800f1988e6a027b56b3c0bd3a30b6e7b69      1
      2 0x0112b93de32e5353402bec6d177af4f8c8cd40fd      1
      3 0x02eff1f92249f482a4cb6e8f25f6b01cb7381f5c      1
      4 0x0400ad7432b46d22aaa70a1cae09d22ac600c8f4      1
      5 0x0fbf0be174da2a813c1538dfd6ee3c01f6eddfd2      1
      6 0x128e151b59181e923b0fd9e82ee7a7175fcc6c2f      1
      7 0x15020cc98cc0752b85e6b87e0e6d1aec12a5d12f      1
      8 0x15237b9fc8c244246abee701f07e42185c5111c3      1
      9 0x18a7ae469892d8272fc16464c935ac977d19b04f      1
     10 0x1adef8b7253c43caaf50500913d422ae688367bd      1
     11 0x1b1405f6dc51121a63c71e9d7282c221d84dccfe      1
     12 0x1cb89e486db5774ba084f683796286848df489d0      1
     13 0x1f2f6361023b414ae8325fbf14b6a502269c346c      1
     14 0x2556b5c5df5af26562f4f10e97db6f9869d6d16a      1
     15 0x278321e66d4d7c400eefcaee96deaaea75f42434      1
     16 0x279a84d4d6b1d4b1895086c775db1e91d5849cdf      1
     17 0x27d4fee2fbe69bc02be96d1e146abe0a29797424      1
     18 0x27e037e0461e3d5587de786ea23441d83772353d      1
     19 0x28eca43a0f5f8063ef3d51ddb9b88fbade8692fa      1
     20 0x2c1725bff8496ab8098406639ee34e5b015f74a2      1
     21 0x2dc23418844eacd31a7d91a5571e000960553283      1
     22 0x3173303e94b4f2ec4e53406909513171afa5c647      1
     23 0x35112cce413383ffe3e846a4d784180128f58761      1
     24 0x3950d3f8813301bef45b29c014ee5a74c47d4249      1
     25 0x3b748a60dfa1d58eac080a5ef24b11a082edb6d2      1
     26 0x3d8b359962463b022459d43f9bb8d7fc77f611c9      1
     27 0x3dc3be0a9278028fde0e3ff65c6154e2e36dee9d      1
     28 0x3fda15273e4e7211377352e0bb84c02703303723      1
     29 0x3feec5cc114811ba1110ce2319a6a9a0d3e8326f      1
     30 0x46b249da04697c801cd592030f6ef81f9433398f      1
     31 0x48116616e768cb2ccf933bce172dbce9f6d9a4ed      1
     32 0x4d123a7a38b1baab51fe31318b9269f41f2580f2      1
     33 0x4d544c95f86ab807e25c7c5ecd0ccfccdea9e51f      1
     34 0x4dd5a4d1fcb6f8938aa8ad070aefe1b19e8f9c0c      1
     35 0x504552190ee73a58ce0c9ad2690e82f67ef14479      1
     36 0x50d12e66195a67c80531f251ceac41e7e5e67340      1
     37 0x536d83e66759cceb0d3b5d6b70d61696404ead7d      1
     38 0x55834cd1f57c4a73d643caa176ec8db4da0669a4      1
     39 0x56ba72d74446569f2ef8d430bc70d8b3cffd0399      1
     40 0x56befbe80008cec71897675b412c2ec746a5f63e      1
     41 0x57d579a203d8b5d1cfa6b751307c2d68ab2b15e4      1
     42 0x5b9c5070841a1cd3d58a95cbb183a4541b0ac79c      1
     43 0x5bc928bf0dab1e4a2ddd9e347b0f22e88026d76c      1
     44 0x5d18c78f374286d1fa6b1880545bfad714c29273      1
     45 0x646aedb1e877fa189b5aaa6c36009a8970be93ce      1
     46 0x673804777cf233284c068d3f2099c92d7c6a5199      1
     47 0x688976844782ca545968d368060744900c5f49b7      1
     48 0x690225f6f19775c5899fa71defcbd1b2db5711a4      1
     49 0x6b1d9489208f6693ac85fed38cddd1fcf278b094      1
     50 0x6d9aba400a2a487a5fb76c6d56518835553cd284      1
     51 0x70e7a6621f4cb3c3e073d0539899f49fc88424c0      1
     52 0x7546c60ae8d65dc6dd7a0f61c169818059ef49db      1
     53 0x757b8003c330418875f07e368c40b1b5ab4c6509      1
     54 0x7619ee03a91ea6b931d9e4e8408451b93115364e      1
     55 0x76b4b991d3ae570a2d0d04ed511e9779080c5340      1
     56 0x80bddfc2bd0b7c7fbc9691859948060c5bf86d59      1
     57 0x85e4971f80f1644f78b1c41e5d880cb56d4de80a      1
     58 0x89e084cc06b180ec5beb1d8992e62b0a61abb2c4      1
     59 0x8b3b6309f228177c2ae5f61a8cf6e1af84aefa66      1
     60 0x8bc80e913fb7f1e315d787f5c3cf445e44ab40a4      1
     61 0x8c847c9b48be308ab7002011acfcc728483d8b21      1
     62 0x9969db4034a136650cdb07955cdf5635499a4012      1
     63 0x9bed6e8a49d415adfeae4ba4d0ef2994049454f1      1
     64 0x9d2fd63c49509e82c7abe56d864deabf41ff4a1e      1
     65 0x9da6640321b9c7a538b5b126e2cfbdc6a999144c      1
     66 0x9de39be30b176c327dd8824f4d7ab5b6d2930db7      1
     67 0x9e1fcd7cc0fa12e74212488fc5cad56850e20542      1
     68 0x9ee9a7e08e78d6a9cd9f47ef72592791c0a5d174      1
     69 0xa0664d876a0708b015acdfa8df46d1f47e068fcd      1
     70 0xa14213bb6f4e324d5cea5606f1b5a4af12d9009d      1
     71 0xa1486dae9f01e7da3bd8efbd12af0e1be1c73b60      1
     72 0xa976fb8ed77bc1016a06074ff5e3e4a5fa6161e0      1
     73 0xad4002c4d4a34675a1887bd36e11a8372577a2f6      1
     74 0xb09316b71c0ae83f1306c5a8dc67f32450e0adf9      1
     75 0xb5bd1ca7ef13954faf50286742aeb54d072ce21d      1
     76 0xb6848f941dc6c5baae2d1ef18b22b5c02f5d83ad      1
     77 0xb7bd84b71fbe6f2ade449508b2b78cae45a18dc0      1
     78 0xb88194f9bcc5ec80cf80c163ca2b123917468793      1
     79 0xbf7a23a47cb36f4bbb84912227a0db2470b815ed      1
     80 0xc044de7f0f2001ac5ba0d9cb2d10201c05f88402      1
     81 0xc1bc1b32b437b81345b84f77d300d95bd24cbb95      1
     82 0xc805afc358852017ec0487b27d743091d632a4f3      1
     83 0xcab81f14a3fc98034a05bab30f8d0e53e978c833      1
     84 0xcf54b70bf3cec789dea3b7b2a01d3147cd6cd0ef      1
     85 0xd0f6805f43f002229faf7c6676480bab015cd6f6      1
     86 0xd2ea8c9440b63fa16e0e82ae96b80ced580bd8ab      1
     87 0xda6ab7d0a045a8b7dc25f8a50b383af7ff49977f      1
     88 0xdf90682f3c884651e13d48acb7e4431764c92c48      1
     89 0xe036a80f628e531982189fb6c4f277deca521e36      1
     90 0xe10f736985a42ddf342f75c83e0fa457077b2284      1
     91 0xe25e6dec6cc0375947479520968cb52c24704cec      1
     92 0xe35c3d08163da9bd4efa00879a78504d69820b5c      1
     93 0xe496a4f9472a4935ebf4ff78a18ae21e2728ffaf      1
     94 0xe79c263403b0071093bd7968ad1b6ea94d3bcf1c      1
     95 0xe9e88f56f5431d692446ec723c2f9f9cb4eeca42      1
     96 0xeb697a943f4edc306de51ac5fce1366a30496e2f      1
     97 0xf450a5d6c4205ca8151fb1c6faf49a02c8a527fc      1
     98 0xf474e475fc675a37075ce0d9d5b4367d54c4d8fe      1
     99 0xfed93f0ad93ff25993274d8245a2bb2133de38c0      1
    100 0xff6ccb4c64e197f053272cf85f64cda42bf550b3      1

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
