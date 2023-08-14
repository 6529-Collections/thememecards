
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot2.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:15449       Length:15449       Min.   :1   Length:15449      
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:15449      
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 17904369 # https://etherscan.io/block/17904369
block_hash <- "0xa6f1af90bf78d949a48f55e29c142f77e5b186df6775812f732d4814b1e50492"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4494 

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
      1 0x0a605f26a5b6c37b27fddb155ccb495fd4f128c0      1
      2 0x0c544f463600eb7d94b3694f3f3eb171c2f1a93c      1
      3 0x0c9d7de299b6f803279b79e8ae06c6a27e6f3f1d      1
      4 0x131adc38217160e51c33e3aa61c4cd856179bb10      1
      5 0x179504149c675376952500c65bd2f83052952541      1
      6 0x19899e40bb768dd09f24db2d8538c98d0cbda62b      1
      7 0x1a051968e421023d61a0afac659b6916784f251f      1
      8 0x1bf1baac1e98ade5df093e015e0f783cc4b81b36      1
      9 0x21aa9286d8c7091c57659dfd7106cf5ff6c51f78      1
     10 0x27e121bfb21f631496ad5b9783d049facbdd411e      1
     11 0x28b8906d06784d25557b6b6d9705455b265809b3      1
     12 0x28f3903fc90ebf184d2763af2d7db321f2fecab3      1
     13 0x29273917eb777b8d227e64ea423d07e6246088fd      1
     14 0x2d00e20e9712618b68b3ece1297502d4c9bd70d8      1
     15 0x2ed364599c727a3712679341a4863c6151411ffb      1
     16 0x2effc23a6f165f63ba9b15cec0c30b03486ad10e      1
     17 0x302522150ef22faafa6bf6530b330b5a4bc38369      1
     18 0x3330777cc5270db65e7df1d738e989dc16dd49f2      1
     19 0x338b3948e0d985fd89aae348757f7fb95f9f8060      1
     20 0x373d419979862c6845e74cee5ae1e075801b6abc      1
     21 0x3bf4d4c0b1890028c158ae3495af5dc75340e22e      1
     22 0x3bfcdda69bfef9ab5da3c3e75c0ff0e0a3074a5d      1
     23 0x3c098770c32634140f77401c7086cc2e48ca9ee5      1
     24 0x3dcff2782f07b19baed2f992e901d2d8990400d3      1
     25 0x4042def3d9fab6e6deeb72ae3fb3175b55e50a5b      1
     26 0x406371fde9671f71a891442d21acddb4d1a976af      1
     27 0x41cdf42b4d22b3edfea997a2f03f32b156b135a0      1
     28 0x4acff5f9ba072eedfca73dd706a44fb0b8c6c983      1
     29 0x4bf7db76757302876d319ab727e26ab66753128c      1
     30 0x4d51a39b4b74502cc5016e15e9106327936e3c5c      1
     31 0x4e948c19640c00fa1d2538ad3e9da14ab7320a14      1
     32 0x52016e2c8d60b8258f5152c4237dbde45660ff6e      1
     33 0x555a5083c82ca88cd4b4c2d0941495c9198ce6b8      1
     34 0x58058169d69f9c24a53ca88947cd2f9fc4e5561e      1
     35 0x5bd6db4f6394f71ff7b678d43cccd953e1a516c5      1
     36 0x5d7b71cda3ce450b5db872776c3524e574ee0b7a      1
     37 0x5e055693d6fee356abe3ff5960584b839184101e      1
     38 0x5e3161efeceab0d3f7f3fe46f0e3b045d0c9ec74      1
     39 0x64355910c6cf1bb7b1a70260d4947a3805fdca4d      1
     40 0x65806cfaa5fd763d379c92992d4eda2260c39d84      1
     41 0x6ad0c5e2611f3716533065b5b23d78638fd97a58      1
     42 0x6b9eb48af8d9d4ef58e3e37390f5cfdf6525f5dd      1
     43 0x72cccabd18786edb51877ee47e4098e992e95976      1
     44 0x75749be2cb9f4294b29c35e968b32c3ccd09088b      1
     45 0x75a56cd64e10c4f07a6d2cb9027580b7b9a728db      1
     46 0x780ba6c53069f5fed88a59ae3e7a78710e889594      1
     47 0x78450ed7653901900d34273a768b23f77cbc2556      1
     48 0x7855227676cce7592e4024056ba2073998f1ead2      1
     49 0x786f0558ba618f56478b01d163248246ca94b29c      1
     50 0x7ba876b2019fcddcbf5087c5c0c2f623f79e6b1f      1
     51 0x7d8b981b45879b054114d12912438dd17fca417b      1
     52 0x7f7c864dac139b5a36550481180b834ef283e056      1
     53 0x7f9b48021c6e6aef86c6563ab2fa031bdaeec3a9      1
     54 0x8808528b90944d93fd2b5c79fac546259eaf7d1b      1
     55 0x8d12daa8d907ad9c86c1c317e83cc430e9685771      1
     56 0x91215d71a00b0907ca67b25401172ab18d4b18f9      1
     57 0x93bbcf3310771090e88f19057c940524bb43bbba      1
     58 0x96de627be6262ad2e19553824aad1af6ba1ebe9b      1
     59 0x98bd03c7202cae140810febce681563b17bf91fe      1
     60 0x99768daa30519851f224d0e4e47546bd1733c219      1
     61 0x9aa824a302597e3477d0435cfc5b1e1b9eb23449      1
     62 0x9b16cc7f4ae8c718d630fe6fe474b85d2a0cbac3      1
     63 0x9baf2d175fc6d5c7f1490069c2db6cb2d9525363      1
     64 0x9bf10a9a1254edadef6a2a4a34e51b34a5928cf9      1
     65 0x9fae8562dc4fbcdd9d26d6d10a7fb88ce60a028e      1
     66 0xa2e08c33fb2ba273d2d8916b390da6a98c800ade      1
     67 0xa40c49ade6c72324821dc073a968bdc114bcb0d9      1
     68 0xa46e0226d3c988ebafadd1838daf2a3318f21783      1
     69 0xa56131d1c79a35efeedd812bb521b81c6712e407      1
     70 0xa7abf57fd7a4313080f018e2914912b1de3b9085      1
     71 0xaa20816a724c8bcd2b8bebb60b1a7a1f90e3ec0b      1
     72 0xb1904abbff9c65e538b7b4cf9e876c801a2bef23      1
     73 0xb1b7c542196d532dcde45fbabf1aaaedab620357      1
     74 0xb34048e2c59bcbd9c128ee6eecd61164c56a58fa      1
     75 0xb406ebb1a43cdc9c6eb5b392c712329c0b84d546      1
     76 0xb63ff8a10c4e456fb40bf88ca513ea397485c49e      1
     77 0xb98f24c3e0092e90dc9d5d80cd6cbb1034a966c4      1
     78 0xbc94e04a219ebd4d12b881fb0d6ff019d15a8885      1
     79 0xbe7a5ccea9f7279f714d4c9b4c5436dd38fb4fe1      1
     80 0xbf057af2c08cd61c9449de0b3efe826a438f2393      1
     81 0xc73b8bc73a63c94fc7c5a54d026faf7b540cf113      1
     82 0xc78c838e66e93ab9bccaa72404e01adfcca423a7      1
     83 0xcae62b9dbb0bbcc3f64cc95aa084c8f015e894ee      1
     84 0xcba794653c56b28800ae4fe5c2ea2c89712c964c      1
     85 0xce0f69eebfa13caef1f580eee21631963a90b332      1
     86 0xd4012143f24c2a02a89110241942b9628c1fe636      1
     87 0xd8261516087b76e1027019da8756d85d8ace7b1e      1
     88 0xda4ff7b6a1eb3176c347a3ffc56a26ee82fb6893      1
     89 0xdb5008e73c7cefe15a147450352be3cbd258da97      1
     90 0xdcbf46cbb479ad18861b488acb13af096ab88368      1
     91 0xdd61eecd6cd27f00b745570c61226f3d68913a0b      1
     92 0xe1e1c69d71dd81dfac922ee18f9a2fcffad337a9      1
     93 0xe2b34bbf669096a794397376fb0587e98eb81016      1
     94 0xe681e5232da0c9ba5d28e6775bb3b6945eaf50f4      1
     95 0xea1126e70185400a2cf0de2a6b35428fb8affd29      1
     96 0xeda4aeabae2d559e7c5aed03cc288bbf44a03134      1
     97 0xef3b70a6f22181efa9d93b2b6d77a269de816550      1
     98 0xf050225c84486b20811eb5a7527bd39a8cd3c085      1
     99 0xfe18ebf2d3e54af293bb27b0bce19e2857831708      1
    100 0xfe504884abd05cb7fe828afb689c06e2a5b4fe64      1

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
