
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:124         Length:124         Min.   :1   Length:124        
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:124        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 22195369 # https://etherscan.io/block/22195369
block_hash <- "0x59784c80b624a51589f7ace55306261a679a37c45ede60a38494e731427b53a2"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4330 

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



allow_artist_all <- pick(snapshot, contracts=c("Foundation","AlmostNothing","LostinWhite","TenGhostsandaChurch","Waterworld","XoseCasalCollection"), address_remove=address_remove,address_max=1)
```

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 89 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x030571c35b84e01613383d631c10cc8afaf83977      1
     2 0x056f154c822cb374508cd318038c3d1e1230c377      1
     3 0x0734c876c0923eb8356baaf51e5f71229b7d2bb5      1
     4 0x0f0eae91990140c560d4156db4f00c854dc8f09e      1
     5 0x1619f7076866ddc852a5a0a69cbb4d6a338d6d67      1
     6 0x180c7f25a4dba3310cc3746619256d1eda5a4f5a      1
     7 0x193d6bc3375f4821cc2c3d69f79261576cc3432b      1
     8 0x1b591e98dc0ed3f967359242064c76161fb4c0dc      1
     9 0x1b8532be318e881a6d073b6b24aa584d76d017fe      1
    10 0x1b9fbead09468a48eb10856ba0eeb1649c6ec4c9      1
    11 0x1c3f8d46b9db827548078a2e7dd46cb487eed0e1      1
    12 0x1d3dd4cc14c9bc93bc8744559be4ace760f7a1d3      1
    13 0x2153a766c5c25a5a7d9d06b1fa165f8c80517d23      1
    14 0x245535c8e074f27cfb0fb1c23cf47a51d5fd9225      1
    15 0x2e70f8a381efd5142474e8ce61fa3d05d3ce0576      1
    16 0x2fb0e475f6d495dfdfd9176af9113f48f7687565      1
    17 0x30dec07af807a47c4c5a2ba69a22738831f002b8      1
    18 0x30f0f915a4752445cfeadfac0beb539fa1192b38      1
    19 0x317fdbf289a41435df30efcb00015f95b1eaddfe      1
    20 0x330bce303d27df0eb1b856da3464278db1db1ac5      1
    21 0x3437c527d6c5cdb8cf0ca5029eef864085576574      1
    22 0x36799ff7bf99602870b91572e2a897b06c91c87b      1
    23 0x40d2f4399d23f9afb82d0a6b73055f13208614f9      1
    24 0x48e7d74810b0950ef03232468ec294a1a14e2e23      1
    25 0x4d58baa3c3e25af70f7080cbf26dbff0fe316be3      1
    26 0x51eea86e142bf26847f160ae4392b3de8a3ca03a      1
    27 0x52abe6a75eaaed6545b615ec4e0d08e689e84cc5      1
    28 0x551d12d97ff1d56fa8009c88b65d986d60c5667b      1
    29 0x57e91198ac30c230010f51024df943e1f4147124      1
    30 0x5bc926e531431b5a1a0f26e2dd4a7aa8f322b1ce      1
    31 0x6088a918b024d2f4a2dc9f734f3656c9c27e978d      1
    32 0x6147bb122124d41cd6aef242f6c016cf633ff08c      1
    33 0x61e1e9a8bde81106b881512c502c38fe17bcbbb5      1
    34 0x697e9c6783e348ac67d2c129b6b00ad616bf74a5      1
    35 0x6b1d9489208f6693ac85fed38cddd1fcf278b094      1
    36 0x6e81af1bcd504a51da5bd6a0d7df70d7674ce90e      1
    37 0x720d2aafacf8c934984efb7d4fde200be05385f5      1
    38 0x727f25672f4f2815831ed496c87b33faeb639238      1
    39 0x75c5b5498181da0ff9d3618372c4fde670b1d809      1
    40 0x7779f8144951811c8d7418a6cf4aa58f65e221b3      1
    41 0x78a42a84bfe3e173c3a9246b3f5f1c5aa8bbae72      1
    42 0x7fd1d8e388b29ef0b8a00ca07e2266686a5559ee      1
    43 0x8497277c9339170a0420e86da8352e0c084624cd      1
    44 0x8a3b8dfe66bc420c1da15af38bf1ad3640594538      1
    45 0x8ee376de530fb9a734df676e7e4342b48355f483      1
    46 0x8f70b3ac45a6896532fb90b992d5b7827ba88d3c      1
    47 0x8f9be4310f9abb0e5843cc6363908c9b01dfeb3f      1
    48 0x90c0e8c6e4791b9e4f99272fd70016a682056847      1
    49 0x914f7592f9c7969ff0a098c2c1d64f10c3407255      1
    50 0x93699c54757c807cf39e77892a4f2e572e98c604      1
    51 0x94777385a21559d7b9d161803d12ef7d70905b9f      1
    52 0x996e1d4ce3e1e558889832832004b2466153adbe      1
    53 0x9be353a221c4f3762092f44896054bd071b1481d      1
    54 0xa5fc9436df21125e88a2f73089bd17dbafe46e74      1
    55 0xa7efbb4a06a680f3b65c5861ec387408ceafbec8      1
    56 0xaf4a749f3e218a67e40fbc295ba58f93c890a27d      1
    57 0xb1fcc14c34e77105d6b320d79bc720fb529ff5eb      1
    58 0xb509189877c46f92cb784d59e1fb6dfc03cd7ede      1
    59 0xb6cf777e3696a502107417265c92d1b075636a10      1
    60 0xb6fef67b00d644fbb5e4368aed9f85ec2d134fd5      1
    61 0xb98d10d9f6d07ba283bfd21b2dfec050f9ae282a      1
    62 0xbaa738690c4e617f64c00fcbdb2ee69281d5da80      1
    63 0xbd46f6fa9045e203dfc0cd31d19294dc2011cce2      1
    64 0xbdc4a5c0ff7275736cad102c7408555fb5d6c495      1
    65 0xc02a3f4424c8834fcb1f31832501f9e3e090262a      1
    66 0xc02e6b0d0c1a5d8cd26beeba0fe8d76c5d2f19b9      1
    67 0xc2978441f46a76c60e0cd59e986498b75a40572d      1
    68 0xc3e2fa491f1f2a36407f5ef3561b4bbc7851ef80      1
    69 0xc6400a5584db71e41b0e5dfbdc769b54b91256cd      1
    70 0xc7d9753e4a1b95351af5fed8e0997e02f1611d31      1
    71 0xc8780bf4dc27a310ec708794b9f40553cc545da0      1
    72 0xce5de409ae461a16d17837a55ca245b7805b19cb      1
    73 0xcf8cf5df28db4f4e8376c90d8cebd5f7a4f73620      1
    74 0xcfcfdcc985aa078e8b4e086c9355a3ed888197df      1
    75 0xd2350c37a02b40f8a3bcf94da9466fc418a5f409      1
    76 0xd385605623db1be4974838829703b8e29124bf37      1
    77 0xd7342ea20a5afbf24352b5ca61e09844167914cb      1
    78 0xd78f0e92c56c45ff017b7116189eb5712518a7e9      1
    79 0xd9a64ab214cc849ae97c67a40caac2a71e38acf3      1
    80 0xd9e04cadccc5ef61851653591b384e597eca17ec      1
    81 0xdeb9fdd1711a826577347f46a2edee59dc143589      1
    82 0xe530507083ecb8d2b474b96544216e0dc92b1883      1
    83 0xe9c2d3bf3a898f700cade5f5f4a89a5e5756f4e4      1
    84 0xeb9ae650f8e7eae4f6dca2a2af35596add420347      1
    85 0xebfd394de050a10b0a9d8408ef395aaa8d269de8      1
    86 0xf3290df945e484a4ff846fb5baeb6d1147a7aa69      1
    87 0xf5b0d29df7b5f55f06ca78ab9e1c0c53eb552608      1
    88 0xf78e40101da8a9ced4948cc600f7f6ef6d1f9107      1
    89 0xf7ce48a33cf8c9e6874f8f68ebbfca40bca748bc      1

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
