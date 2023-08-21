
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot2.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:14976       Length:14976       Min.   :1   Length:14976      
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:14976      
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 17954469 # https://etherscan.io/block/17954469
block_hash <- "0xfa011997417cb82dd91a9b515d555c1529e51349f8e0e36be79edbf8baaa9d03"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4710 

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
      1 0x00a7f53d5a574a71ad23fb85f18f3b4c5a21ef54      1
      2 0x0279273242ea0a8be97f682221c0c7f9187416db      1
      3 0x0a9f85014478279c83eb0fc981cd8d14687adbf6      1
      4 0x13e8810a25171327c55a2903ff00704f27c0c8a4      1
      5 0x1635e11ad250443fc3bc7beef75e47a601b25d93      1
      6 0x172a10897fefab6445372250cb2769cd34b1abab      1
      7 0x17c5cb83b05e0f8f678b5817a66cdde7328f3aa6      1
      8 0x1b5dc7f6a12471bd4f6f584e648edd8fcde50512      1
      9 0x1e6d8d1a6076916d38b120740308d07c01cdb2c7      1
     10 0x1eb44b180681e39c3a362090e43cd6e8ce16588f      1
     11 0x1febadf986428f0888469b0a5e8f6d791496e3c0      1
     12 0x20e89aa0ef531121443142734d64bad6f485f3d9      1
     13 0x221c631649bd7f9d26a0693bc791589056b47899      1
     14 0x23be5fba1ba9c07ee676250040db99474da72997      1
     15 0x25cab69f98105d5bfacebd4ab306fe6828407c1c      1
     16 0x2eea5c25d45b7861b24c5a36b3535222fb6a0aff      1
     17 0x2fbd96729bbc8b02c3084583ada26fffc15278f1      1
     18 0x2ff074271386511555f0682139943cda5fa5937d      1
     19 0x31eae19c7737925fad2ff2f007ccef3c9a7e2239      1
     20 0x33a34e27a81436ba9d79276406a285e89a8bd8a8      1
     21 0x3ba41a9e97f80fec64ed68819faf12dcc6dff576      1
     22 0x3cc74d03bfd87af3bd48d9ef886a052aae0fde7c      1
     23 0x3db28e19b63cb8b3c257d37f56bf0455faf11e0b      1
     24 0x406371fde9671f71a891442d21acddb4d1a976af      1
     25 0x42723163636a5ba05925f543bcbfb5dccacc81d8      1
     26 0x438eb5b02a3b9dd3c093e6b50f9f031841e6a29f      1
     27 0x4536034e4412e42a0470a6143df01206e30c5995      1
     28 0x4d207177677fb1128281a54fad993d3736c7f02a      1
     29 0x4f494d0f18153cd75429588dc67e0975b67dcf46      1
     30 0x51664b043cecd86527a93c95d163888bec348e63      1
     31 0x518b870520476e08ee523a285e9255b443883885      1
     32 0x528e7db54e33c52277ebd5daac7fcde897ca5085      1
     33 0x5d2c9691e81afe76af2aa39cfaac400e541bfdd2      1
     34 0x61b2889889591267229b8d05a7893ebe3822a8a0      1
     35 0x6a9b72846ab3cfaa3394460ba314c9d3da02110d      1
     36 0x6b4b60cd239b5b7947c98bb2b9bbd01db5bdf44c      1
     37 0x6dbf4c620c8fd522c5f3de5cf8ef12394dae7170      1
     38 0x6e7d6b505deb2c67fb64ece9995a26b8ad70678f      1
     39 0x720325f6e2a7d1ded0ba8de8c77a60fca4d292cd      1
     40 0x7426b39865d11207b8f795b10d70843fc3289051      1
     41 0x74acc173f943abe0c54cc6acdbb1307715a796ab      1
     42 0x767d003a3a6983bc3e5e64e8c8c4852f7d3163ec      1
     43 0x77e8001a83e5175e36b0d80c11fe8b5a65ab5ca3      1
     44 0x79d8708b071adc332ce260ad263d26b89d2d34a3      1
     45 0x7ac4c950e7d8975e18e6626931c9f6e1b38f2683      1
     46 0x7f873ec58d567e78a2c1dca322cbdb6d490ccb7d      1
     47 0x7fe04fbcbff289cab8b0f2bb305c945f2d36cc0b      1
     48 0x81d4f31946c4ea0cbfde9664170a100a866c316f      1
     49 0x85de9bedc3686f8c97890980b57ce57ca2d20274      1
     50 0x895bef95023f591ebd1a9e8f53bed2b73702e4d1      1
     51 0x8a01a85f1962938bbe6d19266581eae9ed33004f      1
     52 0x8a58f89cd7a2cdfd167ed99d2ff7139fa723aac8      1
     53 0x8ea6a8de40043b5e8085704d450849f99b816976      1
     54 0x90f5dc8364db3a6083f493d9d4814b9ba9b47237      1
     55 0x9168fa36b033006090a9159e9ec47609d84b1e45      1
     56 0x9392012d81fac3d178177a5517f06055ea1d35e5      1
     57 0x967be09d0652a0be582f217630963df129157d55      1
     58 0x973231cf471f768f0939042e0591da1b15f4799f      1
     59 0x9a33124dd1a12903736f225ba97cb5fe4615bb7d      1
     60 0x9bb524bccce50c6a3606cecd80cfaa38d7d4fc26      1
     61 0xa4766aa40e3f8c0e5f4d95be05a222a574f98f10      1
     62 0xa8c045e857c1c4550119b612f22c3b27ece10340      1
     63 0xaa15a96fba6c84737e632a93140d9f549f55338f      1
     64 0xaa300c7fc927345162a00b83fca3ebbc5828b5eb      1
     65 0xab5f32d395b515d132d5a1f1e3337f4f679d13be      1
     66 0xab7473da0af4658ae2879cb641d82e7652721486      1
     67 0xb1636b8336e98cd896e9bc6f91ca6ebb3c81bb00      1
     68 0xb5ddb5ed294826f7f211754d7c05cf983b1a3065      1
     69 0xbb6cf6429c8491942df257a96e8723b066475781      1
     70 0xbd46f6fa9045e203dfc0cd31d19294dc2011cce2      1
     71 0xbe2127ebbd7c2a3e6473e213749f212b4f97c69b      1
     72 0xc0e40065d60328f33269d3b4846dbfbcb2b12045      1
     73 0xc1d95b52611c5171b3a41df55c5966dd8abb6c8a      1
     74 0xc213c7e9f8e499e0d50afb58478d6079254c0209      1
     75 0xc4bafdc0a6b7c1339055cea9dfd433b28815ae78      1
     76 0xc68092e6a9fcf32b1e1e3bba0e9132b6005599a1      1
     77 0xc6bd194286802c5975cf681201e41f21a4e256aa      1
     78 0xc70b41c06cd32245effd5fc34881e33022347cf1      1
     79 0xc9b6321dc216d91e626e9baa61b06b0e4d55bdb1      1
     80 0xce0f69eebfa13caef1f580eee21631963a90b332      1
     81 0xcfaedb4d9a764fefca8126d2bcb2a7f233aadf9d      1
     82 0xcfdc5645b426d3ba7acd6511a5c19e94fb7dba3b      1
     83 0xd58518b059895d0628f065f42d2d90d37f347f1a      1
     84 0xd78b3277352d7c7752831de72510f485f3e8a58c      1
     85 0xda766f4e22456e5f932070eb91b7e6b14c6b08e9      1
     86 0xdb945002b39af8c78dac5ba02123ad32cde12a10      1
     87 0xdf849eafee13891d2552789452bd8db31685dfbf      1
     88 0xe375b00384ecbed3da6d2f8dec7b6784cf3693d9      1
     89 0xe48db8b3474b09b32140f2b2780ca8b2049c1bba      1
     90 0xea6a4c5514d847c1a474a7b61d305b7ec11f1373      1
     91 0xedd1f30d69898e4cb710cfb47c6114d31e6fed06      1
     92 0xf012b5afd31d8aa22b3b97740e41731e3e30f7d8      1
     93 0xf1b96faf44d0a04f7a39ed90b4b3a2942403b109      1
     94 0xf272da84f95b482ad3b317f95c623bd8da4408a8      1
     95 0xf4c0bd9a1625b99090328521cb73adec5dfacbe8      1
     96 0xf98c3c402f9db363d92e9bb15afabfb9c0290114      1
     97 0xfa43b254584c0edae92e57db903e0eac695908e9      1
     98 0xfc0c364d64a9664f284a8d74ee27f88d897251e9      1
     99 0xfea710fb904464feeb6a12bdb668e11646aea9b9      1
    100 0xfefc2850b25fe1aa3e9aa154df61c6b9bc7aa374      1

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
