
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:74          Length:74          Min.   :1   Length:74         
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:74         
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 19146069 # https://etherscan.io/block/19146069
block_hash <- "0x08fad7af02aca7f771b7738eb4f1dc9db5cad0ee39b8c3ea05729b60c13fa8df"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 5031 

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

allow_artist1    <- pick(snapshot, contracts=c("SuperRare","Foundation","MakersPlace"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("PixelBlossomGenesisEditions","NGLegendsEditions","NGLegendsEditions2"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("SuperRare","Foundation","MakersPlace","PixelBlossomGenesisEditions","NGLegendsEditions","NGLegendsEditions2"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 6 × 2
      address                                    amount
      <chr>                                       <int>
    1 0x0fb586f584dca4f2ea2d8bfb19ab72bae35d6900      1
    2 0x2debdf4427ccbcfdbc7f29d63964499a0ec184f6      1
    3 0x5294057157f9c4c4c63850644c9494d3e63f0b06      1
    4 0x6a682375545982da6d5c926dc65e3a8d360720e2      1
    5 0x7583534d2f2c3699b1acd11b9f2fac9c35acc45c      1
    6 0xba02a5a08328fc0818ea35bef6604446cbac4c29      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 62 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0040122ed305e0b16dfc9fd8e0e9a74a3cdd5509      1
     2 0x010a4d11266eafd07c115ec72990cb1c7727cbe7      1
     3 0x092cd1a6d222a167f5d0767e6444c8b45c92cc72      1
     4 0x11674c2ebbb09e3c007492502027db7d92b3e1c7      1
     5 0x1388ae7b87bdf98369d28c747642049d0078055d      1
     6 0x183abe67478eb7e87c96ca28e2f63dec53f22e3a      1
     7 0x190b3eb98f74118cba62243bc9ef953ab53bd07a      1
     8 0x2d43afff705b2358cf18f41ed80dc3047c7c332c      1
     9 0x31c72d0d03adc76456d3c14a4fe19f8aa8307c16      1
    10 0x36ed861a319278e5b6c39028884b0ca93df105d0      1
    11 0x3ba5ec4582722a525e325731546fe9317a79e8e1      1
    12 0x3c575350c6f0e46e908ea5d8ecd8692afe8ee1bd      1
    13 0x420ecd4ec65c0fea87d8dc5d16c2476c42466b65      1
    14 0x45753b8b1f4a93c72a91d23667f846ea31783f42      1
    15 0x45966b11c1691dc933babd260496b02ba67a403d      1
    16 0x46b2bd5c888e6d57a839c559fd6076f2c15a8cb1      1
    17 0x4e52fea10fee5fdbee9af37a54591977418e2948      1
    18 0x50b88f3f1ea8948731c6af4c0ebfdef1beccd6fa      1
    19 0x5779ed044776ae2d0fb4c3ec9be5029b203df7ce      1
    20 0x5810c4be53bc8a052c2ae7ee82f52d5cc8b0d5f0      1
    21 0x59334fcdc46ccb51357b61689255eabb51b170f6      1
    22 0x5a8116f07937394e6ac2a51995ab6a054c08bf9e      1
    23 0x5cad0ca5517840d9d76ba363c32cd04fb23db5d6      1
    24 0x5f2175a85aa9e7300c83be7958bf395dc7ec7795      1
    25 0x62e5f581db2d03928e5f318ec332368bf1bc7567      1
    26 0x6af7812fc932da2d34738a33f295de54011fd16b      1
    27 0x6e63a4caeccb4f341ee9c9175c9cc554bdb6d10b      1
    28 0x762da606029d3120735aa1eec15464e265db7a3c      1
    29 0x78a9c6ed2174584a109957580164e12c04ec3ec5      1
    30 0x7d39fa48a433aa853f7d14130d3dcd6a81b96a77      1
    31 0x7eca4f3d197da34c5761fc707326ef29935bc4d3      1
    32 0x85cd60df7dbdb162a253d0855aaa366c65d278a7      1
    33 0x871f304fe640c2df1545930573e2f303ab7a1235      1
    34 0x95298343ae03528a6b3c5d211005937f4987b51d      1
    35 0x95d89b7069d3e401efe987a94e4cc4c64af746fb      1
    36 0xa168de0efc8e87c0fc52d01826a900b140c66cb0      1
    37 0xa3bcdd84a2a3a3ae09a8ebd1a7a2f142618397ce      1
    38 0xa7e6b17f94831d4b60c1d7ff4ee4acc2404dfa05      1
    39 0xaa78bed36217e822f180d82b03d78fcb52e108b9      1
    40 0xafc9d2e4f7e7be7d1c5090f02a196ca9a144f02e      1
    41 0xb0e984d4bf1830dcb6c8c4e0f81d0869d2f037b1      1
    42 0xc098b15a6d715eb6a328d0763347fe49e642a1e7      1
    43 0xc185ffb12406b8bd994c7805ed0339ce9f2529ec      1
    44 0xc18d4c2fee42accfe7bfdce98bfacbabf76242e9      1
    45 0xc643c9411a6b489e9833b16631140f42bbfcb6d1      1
    46 0xc91444fa4c5809e07d9541a9671ff9a9dfdf3b84      1
    47 0xc98f7f574b5d8e3ce1565cdb38eda867ea227325      1
    48 0xc9ae113b8451d0afc8a021d796322d545a7e7dbe      1
    49 0xcd8b72710595c69019c109aef5b1b92eea7f995f      1
    50 0xd55ca002c8ac7d73b8423696ed55f8f40652914a      1
    51 0xd6602dc0640fa9654320229e81323c29c73cffb3      1
    52 0xd81ce8e89dd987c8ab630858c8f1e9df14788c35      1
    53 0xd901c103960a9203fa463c3d48f3c5f3ccb0e637      1
    54 0xdf25c3ce7d37858922b1894abe131e6d65c0b4a0      1
    55 0xe052113bd7d7700d623414a0a4585bcae754e9d5      1
    56 0xec7139879921b3682a1685e28b6da5a0b54a4336      1
    57 0xedba5d56d0147aee8a227d284bcaac03b4a87ed4      1
    58 0xf31781b4811b737e66662997679fb07dacf63355      1
    59 0xf489eca17a07140c37d96bbb497a2d7d7adc971f      1
    60 0xf53af966dab1afaaac1171dd941148384d38417d      1
    61 0xf83a76a56123e2c44d41e7b9716c712a8eb34eeb      1
    62 0xfba206e73709f614e5a85aff27a98692d4f3c579      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 68 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0040122ed305e0b16dfc9fd8e0e9a74a3cdd5509      1
     2 0x010a4d11266eafd07c115ec72990cb1c7727cbe7      1
     3 0x092cd1a6d222a167f5d0767e6444c8b45c92cc72      1
     4 0x0fb586f584dca4f2ea2d8bfb19ab72bae35d6900      1
     5 0x11674c2ebbb09e3c007492502027db7d92b3e1c7      1
     6 0x1388ae7b87bdf98369d28c747642049d0078055d      1
     7 0x183abe67478eb7e87c96ca28e2f63dec53f22e3a      1
     8 0x190b3eb98f74118cba62243bc9ef953ab53bd07a      1
     9 0x2d43afff705b2358cf18f41ed80dc3047c7c332c      1
    10 0x2debdf4427ccbcfdbc7f29d63964499a0ec184f6      1
    11 0x31c72d0d03adc76456d3c14a4fe19f8aa8307c16      1
    12 0x36ed861a319278e5b6c39028884b0ca93df105d0      1
    13 0x3ba5ec4582722a525e325731546fe9317a79e8e1      1
    14 0x3c575350c6f0e46e908ea5d8ecd8692afe8ee1bd      1
    15 0x420ecd4ec65c0fea87d8dc5d16c2476c42466b65      1
    16 0x45753b8b1f4a93c72a91d23667f846ea31783f42      1
    17 0x45966b11c1691dc933babd260496b02ba67a403d      1
    18 0x46b2bd5c888e6d57a839c559fd6076f2c15a8cb1      1
    19 0x4e52fea10fee5fdbee9af37a54591977418e2948      1
    20 0x50b88f3f1ea8948731c6af4c0ebfdef1beccd6fa      1
    21 0x5294057157f9c4c4c63850644c9494d3e63f0b06      1
    22 0x5779ed044776ae2d0fb4c3ec9be5029b203df7ce      1
    23 0x5810c4be53bc8a052c2ae7ee82f52d5cc8b0d5f0      1
    24 0x59334fcdc46ccb51357b61689255eabb51b170f6      1
    25 0x5a8116f07937394e6ac2a51995ab6a054c08bf9e      1
    26 0x5cad0ca5517840d9d76ba363c32cd04fb23db5d6      1
    27 0x5f2175a85aa9e7300c83be7958bf395dc7ec7795      1
    28 0x62e5f581db2d03928e5f318ec332368bf1bc7567      1
    29 0x6a682375545982da6d5c926dc65e3a8d360720e2      1
    30 0x6af7812fc932da2d34738a33f295de54011fd16b      1
    31 0x6e63a4caeccb4f341ee9c9175c9cc554bdb6d10b      1
    32 0x7583534d2f2c3699b1acd11b9f2fac9c35acc45c      1
    33 0x762da606029d3120735aa1eec15464e265db7a3c      1
    34 0x78a9c6ed2174584a109957580164e12c04ec3ec5      1
    35 0x7d39fa48a433aa853f7d14130d3dcd6a81b96a77      1
    36 0x7eca4f3d197da34c5761fc707326ef29935bc4d3      1
    37 0x85cd60df7dbdb162a253d0855aaa366c65d278a7      1
    38 0x871f304fe640c2df1545930573e2f303ab7a1235      1
    39 0x95298343ae03528a6b3c5d211005937f4987b51d      1
    40 0x95d89b7069d3e401efe987a94e4cc4c64af746fb      1
    41 0xa168de0efc8e87c0fc52d01826a900b140c66cb0      1
    42 0xa3bcdd84a2a3a3ae09a8ebd1a7a2f142618397ce      1
    43 0xa7e6b17f94831d4b60c1d7ff4ee4acc2404dfa05      1
    44 0xaa78bed36217e822f180d82b03d78fcb52e108b9      1
    45 0xafc9d2e4f7e7be7d1c5090f02a196ca9a144f02e      1
    46 0xb0e984d4bf1830dcb6c8c4e0f81d0869d2f037b1      1
    47 0xba02a5a08328fc0818ea35bef6604446cbac4c29      1
    48 0xc098b15a6d715eb6a328d0763347fe49e642a1e7      1
    49 0xc185ffb12406b8bd994c7805ed0339ce9f2529ec      1
    50 0xc18d4c2fee42accfe7bfdce98bfacbabf76242e9      1
    51 0xc643c9411a6b489e9833b16631140f42bbfcb6d1      1
    52 0xc91444fa4c5809e07d9541a9671ff9a9dfdf3b84      1
    53 0xc98f7f574b5d8e3ce1565cdb38eda867ea227325      1
    54 0xc9ae113b8451d0afc8a021d796322d545a7e7dbe      1
    55 0xcd8b72710595c69019c109aef5b1b92eea7f995f      1
    56 0xd55ca002c8ac7d73b8423696ed55f8f40652914a      1
    57 0xd6602dc0640fa9654320229e81323c29c73cffb3      1
    58 0xd81ce8e89dd987c8ab630858c8f1e9df14788c35      1
    59 0xd901c103960a9203fa463c3d48f3c5f3ccb0e637      1
    60 0xdf25c3ce7d37858922b1894abe131e6d65c0b4a0      1
    61 0xe052113bd7d7700d623414a0a4585bcae754e9d5      1
    62 0xec7139879921b3682a1685e28b6da5a0b54a4336      1
    63 0xedba5d56d0147aee8a227d284bcaac03b4a87ed4      1
    64 0xf31781b4811b737e66662997679fb07dacf63355      1
    65 0xf489eca17a07140c37d96bbb497a2d7d7adc971f      1
    66 0xf53af966dab1afaaac1171dd941148384d38417d      1
    67 0xf83a76a56123e2c44d41e7b9716c712a8eb34eeb      1
    68 0xfba206e73709f614e5a85aff27a98692d4f3c579      1

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
