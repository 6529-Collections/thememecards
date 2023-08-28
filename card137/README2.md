
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot2.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:14516       Length:14516       Min.   :1   Length:14516      
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:14516      
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 18004469 # https://etherscan.io/block/18004469
block_hash <- "0xe8f36c14f955a122bdd145b6a377b043570370bcb6975ce80a963fdfdb7775b8"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4615 

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
      1 0x00eb4f521acf3b6245c751cb89bef91cf420a1f7      1
      2 0x01d1cc95f80849a4564c9dae114255dba06a8d35      1
      3 0x01d6f0b38e2814dd8ef0163bfc05492eb7ea9b23      1
      4 0x034cea68e9305839d2e9cf5ec38a064017e68ba8      1
      5 0x0471e9569afb0c3a46924100e0110c1cc625bd0a      1
      6 0x05eec49e27704acf5c5ce31ecfcde4e382d78ba0      1
      7 0x08a4aabec361e622598ec8ffe30181ac0e004a1a      1
      8 0x0a5b7c226ee4c3510a6f567a5e081971a0f79a90      1
      9 0x0a6f270ff37facd07aa85e48add3e73bb8101686      1
     10 0x0aaf7ea77d6df1761da21f269f65dc5c0db22fea      1
     11 0x0bb75bef057da63a0ae4b25fe9adafd35cd92b87      1
     12 0x0f25c5ac5cbed4ff15aab4ca3639f87a453b9ab8      1
     13 0x118fe7cec5f26a7c50f6c5bf732f0e6951222ef9      1
     14 0x11f4d765a2c3cb55a9b3aa22f80a3cf4534d4d71      1
     15 0x131adc38217160e51c33e3aa61c4cd856179bb10      1
     16 0x1370006e1416c3651f1bce41370804a1868c5b3a      1
     17 0x139a0975ea36cec4c59447002a875749ac9c460f      1
     18 0x15694ff1ff07e724d8e0282c448c4731205a0f1a      1
     19 0x16962bb36c9b28b93b432aa05ae57aee4dbc03fa      1
     20 0x17526dd2955c6d7b4450bf066d196d7001e70804      1
     21 0x19142e3df9e34966c876024dfb9d805f4ea4a131      1
     22 0x1df4b6d97eb84c007f52cbfd942c987a72e94ce6      1
     23 0x1e2eb50a5ff17b4d2e143474206df72350375b22      1
     24 0x2275520b9d06a72eaacc444d9fd4fd72c58336ea      1
     25 0x288235a308282d99d474e16f55cd24616b6e3a48      1
     26 0x2eb507b90cfdfde48a8e439ef441e2bf353cc7a2      1
     27 0x3a98f478825d9114eb118779a75fc9b0998cb9ec      1
     28 0x40ebf8a762801d1706d9d16a8abfec4c452d15e5      1
     29 0x42a75dde1981f918ba5a84d340811f72ea18dbf0      1
     30 0x4494829e14477a43a14f903e0629c226b752c442      1
     31 0x44b51387773ce3581156d9accb27849a204f31dc      1
     32 0x47a6452c0adb81c0851a92609e577458807212d3      1
     33 0x47e255bf1ae9040151774d307c9e01d12da71d57      1
     34 0x484a12a27e8c95718e0b60d14369bfeaa426aff3      1
     35 0x4c42c0b130e8c13655d85f93d00f77cde1aa623c      1
     36 0x4fd217f4d9c70e7ef41dfd549c7ffa9c14358a52      1
     37 0x504ce3e7a71a4c87506b34fa4848f32d81ba3d8a      1
     38 0x540e16d0e898e45850ffa5bbb4d117d5aaef8f0b      1
     39 0x5712ec69b1dedf934303ce530ae2b4f1d3ca4c61      1
     40 0x5e451b2d167c2a8faf0a8b23cff27996a28f28bc      1
     41 0x5f1db8d30744eed120fa29d7045a5d32e34c9724      1
     42 0x60d8d52a265cc4c1e685128b69c4a9335ef9ecc8      1
     43 0x6250ad95533b1f6d2ff1ffcb3df7d31cec0b1271      1
     44 0x679bd1d460b29feed7bf989d1d1f2db0449eb605      1
     45 0x6d0c7c6c4ed61a3c599e9b8a37aa38aeb78cb259      1
     46 0x6f2b6ac9c798abcbe5b5e313f41d401009f2bd95      1
     47 0x7193a5e5dc30b70dc5e434ba2144ae76e33c3eab      1
     48 0x75f331fda7b8fa1fcba2bdd4197dfdfcb8632c7a      1
     49 0x782de3f99f9c73c125a5e6b494373a3c68a2a914      1
     50 0x7841c102d8d13c1f78c0d942e73117cdc6119697      1
     51 0x793aba8ff4b3caadba0aa143326ae8fad5b12412      1
     52 0x7971e007a4e4d4dc1f8380f5d91d3f52b5e53461      1
     53 0x7fd7e17bb15ceaf850376453dea842386f07aa12      1
     54 0x834c771e04770b1f54fe13d258e5a640d78fba29      1
     55 0x8397e2d5fadaa6e0817e376df5ba1d016a2f804f      1
     56 0x885869cb03bfa16e2e21e0e0d1b64b8ae9374f4d      1
     57 0x8a27755c3902c294bd559d31284c95e28b772955      1
     58 0x8af30b3ff1c29119ed336d45f77a6d59c3273b92      1
     59 0x93316235ca4d1227ca3cc51de928aba21fc809b8      1
     60 0x94db5f225a1f6968cd33c84580c0adae52a04edf      1
     61 0x999626c3a4c83f2480317052fe2856fa78984b4f      1
     62 0x9c335e4d8cacda86647ff4497789019c7a762a62      1
     63 0x9d9103b2951450fad4884c0f19a18e5f9cd4171c      1
     64 0x9efd5ab8ec90ae878f5ec5bff755d211d672c76a      1
     65 0xa2a9f1f06e9609cae834d7fb249ecd5598989d90      1
     66 0xa59c86a6336c3e0932ae89206e2501d482c4f520      1
     67 0xaa47a4ffc979363232c99b99fada0f2734b0aeee      1
     68 0xb209e4bd577cecb120fcd1797ee399ddd6533ac5      1
     69 0xb3729c6d8924863327c35aa396073120eb76686f      1
     70 0xb51910166a76de8afc66572f16f222f5d867bf5f      1
     71 0xb53349160e38739b37e4bbfcf950ed26e26fcb41      1
     72 0xb749a586080436e616f097f193ba9cb6a25e7ea6      1
     73 0xb9c4f9e24e02b715c2bfd3d2ca9470303e23ef77      1
     74 0xb9fa078a43da2664c8499c5d2073f2526f96f106      1
     75 0xbe5fe39a448c7be806a86f202fa1f5f146dbeb31      1
     76 0xbf98fb88344f74a9a94629d6ae9988055d1ceb76      1
     77 0xc29b6d6a91f5cb8efecb4722d4150ecca925434f      1
     78 0xc385caee082bb0e900bccbbec8bb2fe650369ecb      1
     79 0xc3a538864600c21fd57eb45cdaca0f1665dccd8f      1
     80 0xc57dd3415e5bd83eb3b48b51d87f94e91d203372      1
     81 0xc68092e6a9fcf32b1e1e3bba0e9132b6005599a1      1
     82 0xce1eb9636c412245c92352d6fdfeca9f97a89a4a      1
     83 0xd170ffecc42ed18ce1a163c5ce6087a4c2a68bee      1
     84 0xd1f11145770b6d504dba8497b641369fb2e4dce3      1
     85 0xd20ce27f650598c2d790714b4f6a7222b8ddce22      1
     86 0xd9270fd770b3e06f7b4a7452f531f035826a2eac      1
     87 0xd92df32ec6cfa9e5d58d68cfbefb3fb699771487      1
     88 0xdde8df9a7dc9f68bdac815f493d1d731de911b5a      1
     89 0xe496a4f9472a4935ebf4ff78a18ae21e2728ffaf      1
     90 0xe6a9d02ed5ba17a74440b8d4bc82d1bcd698dca3      1
     91 0xe957b0829c6ae6ac461d7365b7927e1ff604114b      1
     92 0xeb393d85bc24651ff5163e254150825a306b0b8a      1
     93 0xeb4e17f4b6a9490ee3579c0f1edf144868f2753c      1
     94 0xeee899b6521db73e94f4b9224cdf3db0010fa334      1
     95 0xf0d6999725115e3ead3d927eb3329d63afaec09b      1
     96 0xf51fd1e7bb2064fd4dc313397283f98a860bcdfb      1
     97 0xf5321850a731d785f500901ec5ca494f1876c262      1
     98 0xf71c063492a833c6124236a746da25f6f240e44d      1
     99 0xf7496410e44878d05236dcb3de2238bc057c1582      1
    100 0xff93e7aa23280d83115aa513b16de7493edd0196      1

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
