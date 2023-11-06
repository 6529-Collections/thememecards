
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance        contract        
     Length:121         Length:121         Min.   :1.000   Length:121        
     Class :character   Class :character   1st Qu.:1.000   Class :character  
     Mode  :character   Mode  :character   Median :1.000   Mode  :character  
                                           Mean   :1.025                     
                                           3rd Qu.:1.000                     
                                           Max.   :3.000                     
         name          
     Length:121        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 18497169 # https://etherscan.io/block/18497169
block_hash <- "0x81565b4f5be16aeedf02c13589c35f7e1eff80aa835017091fdfe68726bfa238"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4711 

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

allow_artist1    <- pick(snapshot, contracts=c("8r122","8r122RARE","DeepDowninaDream","Illusions","MissingDevice","OnceUponATime","SOMETHINGSPECIAL"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("8r122Editions","WhatFeelsLike","EszterLakatosEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("8r122","8r122RARE","DeepDowninaDream","Illusions","MissingDevice","OnceUponATime","SOMETHINGSPECIAL","8r122Editions","WhatFeelsLike","EszterLakatosEditions"), address_remove=address_remove,address_max=1)
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
     1 0x060fb9f0d0529a80f5fb94ecad98df7df99ecb0a      1
     2 0x145358fac8cf55a4d14548169c8424f7c0f03c95      1
     3 0x14a281535f2f5828d695f1fde223e58c6a12b8d7      1
     4 0x23fed2634fdc629c1a82eb6ed3391efad2a58670      1
     5 0x5f73b065c8412b43e3db33cad0efc5038040a597      1
     6 0x993a69efe73e3f87df4276e40e81e426385fd2d8      1
     7 0xada53dd3c03a5b20dccf8ee6615cf31a94c8fa51      1
     8 0xaf9e18cd797548220317fb459095c6afac5c33a7      1
     9 0xdfd3b58cfc180168faebb6366b98aff25fe1f8f1      1
    10 0xeb43b5597e3bde0b0c03ee6731ba7c0247e1581e      1
    11 0xef1ab733bf094ae53226b4deb8713a5e1093d577      1
    12 0xf326fc9373450d52d6ec77a71b8c79eab2cca819      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 88 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x03d0954791e896b50d0a90e13c9802d30ed95141      1
     2 0x04ecb5a1565aab40632698244a92119ca56367a4      1
     3 0x05d222a877028d6966469bbc3b60f1af8590ffea      1
     4 0x05d2e466b841fe014e1ce2d1f826707b15f8914d      1
     5 0x08bc9bbbea71d7a2ff435ab288c1ff60af9589a6      1
     6 0x0bbcb94f7036b3606c1156c5034e68a0d120fec2      1
     7 0x0d3b24e36b382bdaa6b301ba1c755524bd220175      1
     8 0x0d53aba24d961c60a12bd7bbd0c5bc28df00a600      1
     9 0x113bdfac41a7bdb80578149b7e710230e637132f      1
    10 0x1a9d933f150616a48f2f03f4561dc68f9edd7db0      1
    11 0x1b551cb673dd5757501b85ef2c7008064fc655ff      1
    12 0x220b671f9688a4ee960910f83508a4c27a2e12a2      1
    13 0x221320d34800760e06b206acd01e626e463eb03e      1
    14 0x22bd04906e808196b19f7d10463fe463ed5f88cb      1
    15 0x25e0fbc652cfc890d308961b7d34c08d109a491b      1
    16 0x29a8fcdbe61302b42b05da77c7db2ef21ff630ca      1
    17 0x29b4e928cfd2221d7840d4644978651be8751a47      1
    18 0x2cd22313443210fd7924953c84a9ad23b1e23414      1
    19 0x2e53cecbf0a006752a0bfba76d3416812b14a2de      1
    20 0x34a8fb5ee57a895b52a7c4a44cb73fc28745031c      1
    21 0x365f7b46973f27740c08a077b66ec810cab39306      1
    22 0x37981c7a3df074b18b110f5e36ce94467ef0ca86      1
    23 0x3e5cce9272c63b2a452b865e1203ac3b197f0ca4      1
    24 0x4187ddd9340689c8e4a124b7532fca0a13eb9511      1
    25 0x43d3026ab05f1fddfb5621b4f5b35908f9b43890      1
    26 0x44022a7eea59beff273e0d033d76884fa6da0822      1
    27 0x44f829ccbdc9c15ca84d4fd3f00c4682c35f228e      1
    28 0x482293e4c21130f069169e5d7e9c99f239c5ee8e      1
    29 0x498d18b5b3c0b7f56635f3f52d4c466618ed21ad      1
    30 0x4cc84a4112922a69d15b27c07c000ceed99ef89f      1
    31 0x4d9beb5a48378acc8619e1cc8ee6a1af53379ae4      1
    32 0x4f8cdb39327588a2b11edcebc50cc1302aa87cfa      1
    33 0x4fd9328511776b0a182b402d244a395e1e1841fb      1
    34 0x511531656cb4bea59d62f3b1f3bb8b1d40eebef9      1
    35 0x53e9c9405762538bde0b5422429c8e87757ab8af      1
    36 0x5f6835cb7be1d77d9d90d6529321937833c63757      1
    37 0x608d68c29f5b1aadad592c6af035891fa09b73d7      1
    38 0x62ac7073454f5b8cd65558711161fc9f3436f76f      1
    39 0x62c5ee7bcc0e336bc58fc2946bb9671ab2878f7c      1
    40 0x63bd415fc10ce71af0fdd807d575fb7a42052117      1
    41 0x6802a74fc991c9b1ffd5aa5120fed4f54f3d346b      1
    42 0x6b4b60cd239b5b7947c98bb2b9bbd01db5bdf44c      1
    43 0x6f04833195de49ab7021c76f6c756ffa41cad262      1
    44 0x72a49224b51ecd4689e963fac92f01f56eef381d      1
    45 0x7304689aac83c3b236332b0c233878f1819ca89d      1
    46 0x749e0084f6c6b6966fd08ba0b543e9b2c36dba7c      1
    47 0x75a473c33bffb61e945d86b37113c0859965a789      1
    48 0x769ad932fbe334ce792e7abca81b77a602f8898b      1
    49 0x78a39de1075dc959997374fb1090a4b9ef569603      1
    50 0x7c0ca862b604c61a7b7e78647ad95563f60956c2      1
    51 0x7ed2c8dee9a3cf927c106a6faf495d2563059e07      1
    52 0x819afaab9701beb3e2a7f13b580dbefdf48f76f7      1
    53 0x87afc8adaff6ab99af285b3e4790b1aaac2e3461      1
    54 0x880c8fb6a59fb13ee7894b4fc18b7cbb529ed24e      1
    55 0x8b7d031fb0d8aab5a985583c6c56c4fffab27ee7      1
    56 0x9318f724edfcf60066a924ddb9133a312becf0f7      1
    57 0x973f5a0fe2f82f06f96bb663d6e3aa00f055f0b1      1
    58 0x9f04277994be82e9c595e49d5ae1539ecf4a5528      1
    59 0xa199f8ffecafa4f3d500d3ab1d1c8e0b49c9dfd0      1
    60 0xa222d6b50db57a3b0052d8d26cd69e73313376e8      1
    61 0xa3572715f6145f10bffe92626b0ee6b6d80320a7      1
    62 0xa4e9a636d9ec9472c3ebb1430266b634c4ac114f      1
    63 0xa7093e8a8bb5ba4300ed29822198d0367e7e6d66      1
    64 0xa837c3d5fdfe4e878a3f8370df89cfd972b116e1      1
    65 0xab94f597b94f45680b07997c394535d7ebc4a297      1
    66 0xb0da586fcd46e03af33b7c349df06b3e4f62d54f      1
    67 0xb179ec3179f20452710dc544fcd1486b8e7be4c0      1
    68 0xbec5d88ef9491b661ef33a596eed6a4e5cdb00cb      1
    69 0xc0f4f18aa3a60e0032e928b907b642e2f6ba63d0      1
    70 0xc22641d085eb4fbbf88fa657509429b85fd8d600      1
    71 0xc3969e98dc0fd5652f18aca65eec124d9b8e437c      1
    72 0xc578958dd1880cf00bffbb7feb9c28cbbbcad3bf      1
    73 0xc841fabb79c0b39bd0af850dcd5281022445eb51      1
    74 0xcc48857242141cc7f4f8555f8a3d5bed03556c19      1
    75 0xcc71dac21cf694648f73a191d675aa5bd3cf5ac7      1
    76 0xcf42743d1cad547d14beba9b65e70faeefaa12f3      1
    77 0xcfa85be45b38f69c0dc7967da750d9c996682d9c      1
    78 0xd7f85684204a9f8787ba8775d25480f4d7033767      1
    79 0xd92df6b7fe238fe2c14fdb5816eebcc131b53360      1
    80 0xdf61c50705beb255becef635f119c7f8cacb47ad      1
    81 0xe01a97fc4607a388a41626d5e039fdecbfc6acd9      1
    82 0xe0b43c8426972f1b3113c0ee0ea835539ec8d3dc      1
    83 0xe450637a5b515d5bb09d2b75cb4ca89700cc410c      1
    84 0xe5cc5e83667281776d22e403760ba55642e07dae      1
    85 0xe5dfedcaadf4df48f2fcc56663b89ed0ad73da3e      1
    86 0xeebff2d3f395aaf95ac43c813290ac75711ee28f      1
    87 0xfbc76261fd55cf91b81a97dbcc2b3f6118f2b935      1
    88 0xfc5446efe679f109f2772e45ea623caa63791d5e      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 100 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x03d0954791e896b50d0a90e13c9802d30ed95141      1
      2 0x04ecb5a1565aab40632698244a92119ca56367a4      1
      3 0x05d222a877028d6966469bbc3b60f1af8590ffea      1
      4 0x05d2e466b841fe014e1ce2d1f826707b15f8914d      1
      5 0x060fb9f0d0529a80f5fb94ecad98df7df99ecb0a      1
      6 0x08bc9bbbea71d7a2ff435ab288c1ff60af9589a6      1
      7 0x0bbcb94f7036b3606c1156c5034e68a0d120fec2      1
      8 0x0d3b24e36b382bdaa6b301ba1c755524bd220175      1
      9 0x0d53aba24d961c60a12bd7bbd0c5bc28df00a600      1
     10 0x113bdfac41a7bdb80578149b7e710230e637132f      1
     11 0x145358fac8cf55a4d14548169c8424f7c0f03c95      1
     12 0x14a281535f2f5828d695f1fde223e58c6a12b8d7      1
     13 0x1a9d933f150616a48f2f03f4561dc68f9edd7db0      1
     14 0x1b551cb673dd5757501b85ef2c7008064fc655ff      1
     15 0x220b671f9688a4ee960910f83508a4c27a2e12a2      1
     16 0x221320d34800760e06b206acd01e626e463eb03e      1
     17 0x22bd04906e808196b19f7d10463fe463ed5f88cb      1
     18 0x23fed2634fdc629c1a82eb6ed3391efad2a58670      1
     19 0x25e0fbc652cfc890d308961b7d34c08d109a491b      1
     20 0x29a8fcdbe61302b42b05da77c7db2ef21ff630ca      1
     21 0x29b4e928cfd2221d7840d4644978651be8751a47      1
     22 0x2cd22313443210fd7924953c84a9ad23b1e23414      1
     23 0x2e53cecbf0a006752a0bfba76d3416812b14a2de      1
     24 0x34a8fb5ee57a895b52a7c4a44cb73fc28745031c      1
     25 0x365f7b46973f27740c08a077b66ec810cab39306      1
     26 0x37981c7a3df074b18b110f5e36ce94467ef0ca86      1
     27 0x3e5cce9272c63b2a452b865e1203ac3b197f0ca4      1
     28 0x4187ddd9340689c8e4a124b7532fca0a13eb9511      1
     29 0x43d3026ab05f1fddfb5621b4f5b35908f9b43890      1
     30 0x44022a7eea59beff273e0d033d76884fa6da0822      1
     31 0x44f829ccbdc9c15ca84d4fd3f00c4682c35f228e      1
     32 0x482293e4c21130f069169e5d7e9c99f239c5ee8e      1
     33 0x498d18b5b3c0b7f56635f3f52d4c466618ed21ad      1
     34 0x4cc84a4112922a69d15b27c07c000ceed99ef89f      1
     35 0x4d9beb5a48378acc8619e1cc8ee6a1af53379ae4      1
     36 0x4f8cdb39327588a2b11edcebc50cc1302aa87cfa      1
     37 0x4fd9328511776b0a182b402d244a395e1e1841fb      1
     38 0x511531656cb4bea59d62f3b1f3bb8b1d40eebef9      1
     39 0x53e9c9405762538bde0b5422429c8e87757ab8af      1
     40 0x5f6835cb7be1d77d9d90d6529321937833c63757      1
     41 0x5f73b065c8412b43e3db33cad0efc5038040a597      1
     42 0x608d68c29f5b1aadad592c6af035891fa09b73d7      1
     43 0x62ac7073454f5b8cd65558711161fc9f3436f76f      1
     44 0x62c5ee7bcc0e336bc58fc2946bb9671ab2878f7c      1
     45 0x63bd415fc10ce71af0fdd807d575fb7a42052117      1
     46 0x6802a74fc991c9b1ffd5aa5120fed4f54f3d346b      1
     47 0x6b4b60cd239b5b7947c98bb2b9bbd01db5bdf44c      1
     48 0x6f04833195de49ab7021c76f6c756ffa41cad262      1
     49 0x72a49224b51ecd4689e963fac92f01f56eef381d      1
     50 0x7304689aac83c3b236332b0c233878f1819ca89d      1
     51 0x749e0084f6c6b6966fd08ba0b543e9b2c36dba7c      1
     52 0x75a473c33bffb61e945d86b37113c0859965a789      1
     53 0x769ad932fbe334ce792e7abca81b77a602f8898b      1
     54 0x78a39de1075dc959997374fb1090a4b9ef569603      1
     55 0x7c0ca862b604c61a7b7e78647ad95563f60956c2      1
     56 0x7ed2c8dee9a3cf927c106a6faf495d2563059e07      1
     57 0x819afaab9701beb3e2a7f13b580dbefdf48f76f7      1
     58 0x87afc8adaff6ab99af285b3e4790b1aaac2e3461      1
     59 0x880c8fb6a59fb13ee7894b4fc18b7cbb529ed24e      1
     60 0x8b7d031fb0d8aab5a985583c6c56c4fffab27ee7      1
     61 0x9318f724edfcf60066a924ddb9133a312becf0f7      1
     62 0x973f5a0fe2f82f06f96bb663d6e3aa00f055f0b1      1
     63 0x993a69efe73e3f87df4276e40e81e426385fd2d8      1
     64 0x9f04277994be82e9c595e49d5ae1539ecf4a5528      1
     65 0xa199f8ffecafa4f3d500d3ab1d1c8e0b49c9dfd0      1
     66 0xa222d6b50db57a3b0052d8d26cd69e73313376e8      1
     67 0xa3572715f6145f10bffe92626b0ee6b6d80320a7      1
     68 0xa4e9a636d9ec9472c3ebb1430266b634c4ac114f      1
     69 0xa7093e8a8bb5ba4300ed29822198d0367e7e6d66      1
     70 0xa837c3d5fdfe4e878a3f8370df89cfd972b116e1      1
     71 0xab94f597b94f45680b07997c394535d7ebc4a297      1
     72 0xada53dd3c03a5b20dccf8ee6615cf31a94c8fa51      1
     73 0xaf9e18cd797548220317fb459095c6afac5c33a7      1
     74 0xb0da586fcd46e03af33b7c349df06b3e4f62d54f      1
     75 0xb179ec3179f20452710dc544fcd1486b8e7be4c0      1
     76 0xbec5d88ef9491b661ef33a596eed6a4e5cdb00cb      1
     77 0xc0f4f18aa3a60e0032e928b907b642e2f6ba63d0      1
     78 0xc22641d085eb4fbbf88fa657509429b85fd8d600      1
     79 0xc3969e98dc0fd5652f18aca65eec124d9b8e437c      1
     80 0xc578958dd1880cf00bffbb7feb9c28cbbbcad3bf      1
     81 0xc841fabb79c0b39bd0af850dcd5281022445eb51      1
     82 0xcc48857242141cc7f4f8555f8a3d5bed03556c19      1
     83 0xcc71dac21cf694648f73a191d675aa5bd3cf5ac7      1
     84 0xcf42743d1cad547d14beba9b65e70faeefaa12f3      1
     85 0xcfa85be45b38f69c0dc7967da750d9c996682d9c      1
     86 0xd7f85684204a9f8787ba8775d25480f4d7033767      1
     87 0xd92df6b7fe238fe2c14fdb5816eebcc131b53360      1
     88 0xdf61c50705beb255becef635f119c7f8cacb47ad      1
     89 0xdfd3b58cfc180168faebb6366b98aff25fe1f8f1      1
     90 0xe01a97fc4607a388a41626d5e039fdecbfc6acd9      1
     91 0xe0b43c8426972f1b3113c0ee0ea835539ec8d3dc      1
     92 0xe450637a5b515d5bb09d2b75cb4ca89700cc410c      1
     93 0xe5cc5e83667281776d22e403760ba55642e07dae      1
     94 0xe5dfedcaadf4df48f2fcc56663b89ed0ad73da3e      1
     95 0xeb43b5597e3bde0b0c03ee6731ba7c0247e1581e      1
     96 0xeebff2d3f395aaf95ac43c813290ac75711ee28f      1
     97 0xef1ab733bf094ae53226b4deb8713a5e1093d577      1
     98 0xf326fc9373450d52d6ec77a71b8c79eab2cca819      1
     99 0xfbc76261fd55cf91b81a97dbcc2b3f6118f2b935      1
    100 0xfc5446efe679f109f2772e45ea623caa63791d5e      1

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
