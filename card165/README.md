
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance        contract        
     Length:113         Length:113         Min.   :1.000   Length:113        
     Class :character   Class :character   1st Qu.:1.000   Class :character  
     Mode  :character   Mode  :character   Median :1.000   Mode  :character  
                                           Mean   :1.018                     
                                           3rd Qu.:1.000                     
                                           Max.   :2.000                     
         name          
     Length:113        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 18546969 # https://etherscan.io/block/18546969
block_hash <- "0xc52e73558909a731bc9f3250790f68fd79ecaa39671018c37abde2737296fe91"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4492 

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

allow_artist1    <- pick(snapshot, contracts=c("DunesPainting","FortheCulture","GiulioAprin1of1s","SuperRare"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("GiulioAprinEditions","FortheCultureEditions","NiftyGateway"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("DunesPainting","FortheCulture","GiulioAprin1of1s","SuperRare","GiulioAprinEditions","FortheCultureEditions","NiftyGateway"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 12 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x040df97224a2d65df277ab4adb58596a42a9698c      1
     2 0x0d4eaedcc47baa81d29dd35eab84a10c58fa6c44      1
     3 0x193959c6cd6a6b36f150b173383260cfeefc59f6      1
     4 0x319982b8c4dd935eea2bf9c73b07fa1e89e34e88      1
     5 0x3c8cd5597ac98cb0871db580d3c9a86a384b9106      1
     6 0x51522a59dae2a6585a8a4d6ee901821041ae209d      1
     7 0x84278a458775791d91014faf6d3d2d00ac662f98      1
     8 0xa490a0346808dda91aea6698cb19e4697d9fc5cc      1
     9 0xab4273e7137f9531fcb47d558d9bab0e726e6937      1
    10 0xac179270dd51b055107350f800960ad1d7bf6495      1
    11 0xbad0238191c58dfc11c8c1bef23bfd1b2662ebd4      1
    12 0xc97705a8e073c9c0ba05d71aede57903a4a359f6      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 86 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x02f26a1ff2d800406cbf4e3d959ef3c047c7ed15      1
     2 0x0320de3378dcde180758ad2d41c0e1c6dcbb441d      1
     3 0x088bb97ec41e98254bf209e19616a003140322a6      1
     4 0x0bb75bef057da63a0ae4b25fe9adafd35cd92b87      1
     5 0x0deeaab5d6b065c71f6d3cef275f3b9b68ea0496      1
     6 0x103465fe2c9a42fad3a5be6d173a9122f239cd8f      1
     7 0x10f5489bf995c2510c4dbf54bd7d24e9a2397ee9      1
     8 0x14a281535f2f5828d695f1fde223e58c6a12b8d7      1
     9 0x16fdd091c5184a5a5ffb501b09d2b6bea17b6721      1
    10 0x1859e88b0fd6a622d2a1b11b8d828ad8a7966bef      1
    11 0x18e30984d2be32293299f79c17c76ef4f767fe09      1
    12 0x1dcfdcb24456a7dba5342479cff0f9ff30145fb8      1
    13 0x21b1327d68134363dbcaca9730fd4cd67ec84ed5      1
    14 0x2289c8fed5be4b1c4fcfd7a74ad2afcdd277312f      1
    15 0x2604bccf91c4b2c5d1aa4a5f389e32bbf3d77c6e      1
    16 0x2812195be21d9f86b7dc15c9242d18d4ba66988c      1
    17 0x29d7e1da8daca7a9d1edaec80255bbaf0eef594a      1
    18 0x29ffebd73dd219b601ba99888bb5d16d58eef222      1
    19 0x2fb5bf565ca66786bf2c2b3f7f47b5e0c650768c      1
    20 0x3098c04b75c7fecba9c7a80fc24c097995e95681      1
    21 0x30dec07af807a47c4c5a2ba69a22738831f002b8      1
    22 0x333fccc5046da80e10078363f7e0faefe08aeab3      1
    23 0x38e3d4a5bf7dea9280e389864d4bf9834cc2f266      1
    24 0x3c76ae5318ee343152f2c1a83514fd583492071b      1
    25 0x43fc9ec3282a04d47d71dd6a676fe80f5a9357d5      1
    26 0x465d46e45ab8868035829ac53cc2c25a578aad13      1
    27 0x4660e9cbe586a7ec727e076b5593eab9c51b5e4c      1
    28 0x4b5bf7fcccbd98450b215b619b7dbdb036a3dd46      1
    29 0x4cc2b3075c3cc735958a50ce1b3dd00173e97b04      1
    30 0x4cd4b014f0442a87564fcc7845202ce2c624ac3a      1
    31 0x4eb8a34d631cbf724a48d5701b57bad24c1e4779      1
    32 0x52ea81876b11596634d4cf3ca1ea83971fb00137      1
    33 0x578b076f33c021ca8ec8873be00c734559a99057      1
    34 0x589ad95db08979d176ff66e2880b185680967b55      1
    35 0x58f606e6a973e57165bb07057a75f047f42455a7      1
    36 0x5e1127be6dab460330e97e0dbb4912accf6ea178      1
    37 0x5e4b41b770a8e2bec9d6dc294f09ce10c444a2b0      1
    38 0x63b39fec34e761f30a025989409344efdd7dd4f1      1
    39 0x64d18532f07855920ab624de8d2a8a365f365e5b      1
    40 0x6a71efbc3d99cb376730fbd27fa0a00efee4810f      1
    41 0x6c413852c216a680df93d03ca8e3b61c757a942b      1
    42 0x70ff264d01c0b39fa2f75a023bc90eced4b2d40e      1
    43 0x716754c08c77d935a93b1853a19e4ccbeef4f351      1
    44 0x72cc766074a0394744b7da382cde96a5ca9dfebf      1
    45 0x7304689aac83c3b236332b0c233878f1819ca89d      1
    46 0x77f299d1e625fa2fa5467049478bba9ea3b7ae17      1
    47 0x79d8708b071adc332ce260ad263d26b89d2d34a3      1
    48 0x7b23d4f8aabc412da087aa32249d3ac923683e8d      1
    49 0x7c0a8020eb335dbccb6e2a66726143708a3f74d0      1
    50 0x82f23de5a474fc904041c357576afe53c30dd250      1
    51 0x848ba349b050fd8b703cc5fef2c85ae7d8350556      1
    52 0x84c74863c2ff8c529d1651924cb41d13ae3921c3      1
    53 0x8fc8cc057c0d42c4958e7ac293257a38ba566332      1
    54 0x90238a0a150c47e326a19ad38f05700021fcfa62      1
    55 0x9724ed7c5371f8791479e5f9b454c72622a19821      1
    56 0x99c7be4c85c4e9d26b7cab28a4e6dbfc8bc19178      1
    57 0x9ba3fbc8dcb0fbecd8acf0273a11bf357ab98b1c      1
    58 0x9c163eb3589b0dcf366fc23c581307618b68fb06      1
    59 0x9f35af4727fb91114563d8a8f779a792cb557f3f      1
    60 0xa65a864ca19535e4c06000768c6cfa9f6ebaeb12      1
    61 0xa837c3d5fdfe4e878a3f8370df89cfd972b116e1      1
    62 0xab598be7413ae9a89bb319e36b8aa8b31cbe6314      1
    63 0xac104d56e4f8e0462f8c001ec7dde4c68deb596f      1
    64 0xb154b590d9fbb17c7b6166f16f389f0121186d3a      1
    65 0xb1decffa6f7f48fae36eef34cac37d939cd0ba96      1
    66 0xb4532c764c26dab6d69b5b62a201fd624ca9a6ba      1
    67 0xb5a4a9677de36140b4330a3c90500a091723da4b      1
    68 0xb917c85e98a7d3c0d55dfdbf820bac80c18c97d9      1
    69 0xbdece221567514d57f17d3524da55ebb62197cb6      1
    70 0xc2776ea495d89bc282cde6ffff94784a3fac74ef      1
    71 0xc3cdbc995698254ae6739db0d8e248aafa8d6116      1
    72 0xc6b947791f61316ac434cde35b43b9dad306a39d      1
    73 0xc6eaa69c6f5541388122154297834b94c68dbdd1      1
    74 0xc7502c148b7cf1f9fe9e7a255a77f9c1fd7dccaf      1
    75 0xc9d25b9a3496c776688833d6ccfe507ef4f41645      1
    76 0xcd90c980b25eaabace2a760b364ea5cccb95e2b4      1
    77 0xdcfd6d6e63f15a391d96d1b76575ae39ad6965d9      1
    78 0xe1c35e6cbbb0175af3ff0c823a1c6a8ea54b4fda      1
    79 0xe4f44f453fbafdf39e7af6720b505fb64b041666      1
    80 0xe73739c857524d09708152489370dfbbebd6585a      1
    81 0xe96ead0ad625122677684448155ba4ae2700814d      1
    82 0xebfe1ab93d1122e065adcafd8c3174261e8e726f      1
    83 0xf12165c1be8faf064a32b12d86e1a491aeb39a3b      1
    84 0xf1637adedeb89559c70481c3cb4f74ebac80d829      1
    85 0xf6263fa2fc82bfef43a87e0127153d07a3adf0cf      1
    86 0xf69a71bf7db7d20bda6c011166340f5222585043      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 98 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x02f26a1ff2d800406cbf4e3d959ef3c047c7ed15      1
     2 0x0320de3378dcde180758ad2d41c0e1c6dcbb441d      1
     3 0x040df97224a2d65df277ab4adb58596a42a9698c      1
     4 0x088bb97ec41e98254bf209e19616a003140322a6      1
     5 0x0bb75bef057da63a0ae4b25fe9adafd35cd92b87      1
     6 0x0d4eaedcc47baa81d29dd35eab84a10c58fa6c44      1
     7 0x0deeaab5d6b065c71f6d3cef275f3b9b68ea0496      1
     8 0x103465fe2c9a42fad3a5be6d173a9122f239cd8f      1
     9 0x10f5489bf995c2510c4dbf54bd7d24e9a2397ee9      1
    10 0x14a281535f2f5828d695f1fde223e58c6a12b8d7      1
    11 0x16fdd091c5184a5a5ffb501b09d2b6bea17b6721      1
    12 0x1859e88b0fd6a622d2a1b11b8d828ad8a7966bef      1
    13 0x18e30984d2be32293299f79c17c76ef4f767fe09      1
    14 0x193959c6cd6a6b36f150b173383260cfeefc59f6      1
    15 0x1dcfdcb24456a7dba5342479cff0f9ff30145fb8      1
    16 0x21b1327d68134363dbcaca9730fd4cd67ec84ed5      1
    17 0x2289c8fed5be4b1c4fcfd7a74ad2afcdd277312f      1
    18 0x2604bccf91c4b2c5d1aa4a5f389e32bbf3d77c6e      1
    19 0x2812195be21d9f86b7dc15c9242d18d4ba66988c      1
    20 0x29d7e1da8daca7a9d1edaec80255bbaf0eef594a      1
    21 0x29ffebd73dd219b601ba99888bb5d16d58eef222      1
    22 0x2fb5bf565ca66786bf2c2b3f7f47b5e0c650768c      1
    23 0x3098c04b75c7fecba9c7a80fc24c097995e95681      1
    24 0x30dec07af807a47c4c5a2ba69a22738831f002b8      1
    25 0x319982b8c4dd935eea2bf9c73b07fa1e89e34e88      1
    26 0x333fccc5046da80e10078363f7e0faefe08aeab3      1
    27 0x38e3d4a5bf7dea9280e389864d4bf9834cc2f266      1
    28 0x3c76ae5318ee343152f2c1a83514fd583492071b      1
    29 0x3c8cd5597ac98cb0871db580d3c9a86a384b9106      1
    30 0x43fc9ec3282a04d47d71dd6a676fe80f5a9357d5      1
    31 0x465d46e45ab8868035829ac53cc2c25a578aad13      1
    32 0x4660e9cbe586a7ec727e076b5593eab9c51b5e4c      1
    33 0x4b5bf7fcccbd98450b215b619b7dbdb036a3dd46      1
    34 0x4cc2b3075c3cc735958a50ce1b3dd00173e97b04      1
    35 0x4cd4b014f0442a87564fcc7845202ce2c624ac3a      1
    36 0x4eb8a34d631cbf724a48d5701b57bad24c1e4779      1
    37 0x51522a59dae2a6585a8a4d6ee901821041ae209d      1
    38 0x52ea81876b11596634d4cf3ca1ea83971fb00137      1
    39 0x578b076f33c021ca8ec8873be00c734559a99057      1
    40 0x589ad95db08979d176ff66e2880b185680967b55      1
    41 0x58f606e6a973e57165bb07057a75f047f42455a7      1
    42 0x5e1127be6dab460330e97e0dbb4912accf6ea178      1
    43 0x5e4b41b770a8e2bec9d6dc294f09ce10c444a2b0      1
    44 0x63b39fec34e761f30a025989409344efdd7dd4f1      1
    45 0x64d18532f07855920ab624de8d2a8a365f365e5b      1
    46 0x6a71efbc3d99cb376730fbd27fa0a00efee4810f      1
    47 0x6c413852c216a680df93d03ca8e3b61c757a942b      1
    48 0x70ff264d01c0b39fa2f75a023bc90eced4b2d40e      1
    49 0x716754c08c77d935a93b1853a19e4ccbeef4f351      1
    50 0x72cc766074a0394744b7da382cde96a5ca9dfebf      1
    51 0x7304689aac83c3b236332b0c233878f1819ca89d      1
    52 0x77f299d1e625fa2fa5467049478bba9ea3b7ae17      1
    53 0x79d8708b071adc332ce260ad263d26b89d2d34a3      1
    54 0x7b23d4f8aabc412da087aa32249d3ac923683e8d      1
    55 0x7c0a8020eb335dbccb6e2a66726143708a3f74d0      1
    56 0x82f23de5a474fc904041c357576afe53c30dd250      1
    57 0x84278a458775791d91014faf6d3d2d00ac662f98      1
    58 0x848ba349b050fd8b703cc5fef2c85ae7d8350556      1
    59 0x84c74863c2ff8c529d1651924cb41d13ae3921c3      1
    60 0x8fc8cc057c0d42c4958e7ac293257a38ba566332      1
    61 0x90238a0a150c47e326a19ad38f05700021fcfa62      1
    62 0x9724ed7c5371f8791479e5f9b454c72622a19821      1
    63 0x99c7be4c85c4e9d26b7cab28a4e6dbfc8bc19178      1
    64 0x9ba3fbc8dcb0fbecd8acf0273a11bf357ab98b1c      1
    65 0x9c163eb3589b0dcf366fc23c581307618b68fb06      1
    66 0x9f35af4727fb91114563d8a8f779a792cb557f3f      1
    67 0xa490a0346808dda91aea6698cb19e4697d9fc5cc      1
    68 0xa65a864ca19535e4c06000768c6cfa9f6ebaeb12      1
    69 0xa837c3d5fdfe4e878a3f8370df89cfd972b116e1      1
    70 0xab4273e7137f9531fcb47d558d9bab0e726e6937      1
    71 0xab598be7413ae9a89bb319e36b8aa8b31cbe6314      1
    72 0xac104d56e4f8e0462f8c001ec7dde4c68deb596f      1
    73 0xac179270dd51b055107350f800960ad1d7bf6495      1
    74 0xb154b590d9fbb17c7b6166f16f389f0121186d3a      1
    75 0xb1decffa6f7f48fae36eef34cac37d939cd0ba96      1
    76 0xb4532c764c26dab6d69b5b62a201fd624ca9a6ba      1
    77 0xb5a4a9677de36140b4330a3c90500a091723da4b      1
    78 0xb917c85e98a7d3c0d55dfdbf820bac80c18c97d9      1
    79 0xbad0238191c58dfc11c8c1bef23bfd1b2662ebd4      1
    80 0xbdece221567514d57f17d3524da55ebb62197cb6      1
    81 0xc2776ea495d89bc282cde6ffff94784a3fac74ef      1
    82 0xc3cdbc995698254ae6739db0d8e248aafa8d6116      1
    83 0xc6b947791f61316ac434cde35b43b9dad306a39d      1
    84 0xc6eaa69c6f5541388122154297834b94c68dbdd1      1
    85 0xc7502c148b7cf1f9fe9e7a255a77f9c1fd7dccaf      1
    86 0xc97705a8e073c9c0ba05d71aede57903a4a359f6      1
    87 0xc9d25b9a3496c776688833d6ccfe507ef4f41645      1
    88 0xcd90c980b25eaabace2a760b364ea5cccb95e2b4      1
    89 0xdcfd6d6e63f15a391d96d1b76575ae39ad6965d9      1
    90 0xe1c35e6cbbb0175af3ff0c823a1c6a8ea54b4fda      1
    91 0xe4f44f453fbafdf39e7af6720b505fb64b041666      1
    92 0xe73739c857524d09708152489370dfbbebd6585a      1
    93 0xe96ead0ad625122677684448155ba4ae2700814d      1
    94 0xebfe1ab93d1122e065adcafd8c3174261e8e726f      1
    95 0xf12165c1be8faf064a32b12d86e1a491aeb39a3b      1
    96 0xf1637adedeb89559c70481c3cb4f74ebac80d829      1
    97 0xf6263fa2fc82bfef43a87e0127153d07a3adf0cf      1
    98 0xf69a71bf7db7d20bda6c011166340f5222585043      1

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
