
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance        contract        
     Length:166         Length:166         Min.   :1.000   Length:166        
     Class :character   Class :character   1st Qu.:1.000   Class :character  
     Mode  :character   Mode  :character   Median :1.000   Mode  :character  
                                           Mean   :1.078                     
                                           3rd Qu.:1.000                     
                                           Max.   :5.000                     
         name          
     Length:166        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 21751369 # https://etherscan.io/block/21751369
block_hash <- "0xa3c28556f93aab71a87c7a6ac4e5b6a6cddbf1efbca7719d4a1cec9994cab15a"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 5079 

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

allow_artist1    <- pick(snapshot, contracts=c("SuperRare","Foundation","Touchables","TheGreatDepression","NirosNews","Nirosmusic","MakersPlace"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("LysergicChroniclesEditions","NirosNewsEditions","NirosBooksEditions","BiddersEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("SuperRare","Foundation","Touchables","TheGreatDepression","NirosNews","Nirosmusic","MakersPlace","LysergicChroniclesEditions","NirosNewsEditions","NirosBooksEditions","BiddersEditions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 32 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x03018c0dad483a344841675321d9676054dc9c9c      1
     2 0x03232119141e4eed18229bf1be2a3967c5fd435d      1
     3 0x0f0eae91990140c560d4156db4f00c854dc8f09e      1
     4 0x180669310d089fafcbee40479ee719752d680d4e      1
     5 0x194cbec1b7f8ef5582c8b7f5f9b05fd2334c4c48      1
     6 0x257d9128e4b8abe05ca3b045a216ed86bcf08af4      1
     7 0x335e19b3f5dcfa2e398ee7d5ff430a1f8ccc88b2      1
     8 0x3775669465352353f599f73586494747463f851b      1
     9 0x3c30f1593c14357f2c2f7cb43a33903bbb7250a8      1
    10 0x3da1d16c93cb5dd30457bd7e2670663026b22e2c      1
    11 0x3fdbeedcbfd67cbc00fc169fcf557f77ea4ad4ed      1
    12 0x41d25f9650fc4b6737d44ac4804fbd5797e8f694      1
    13 0x461123fb3a23b5503c01afdb83b0e4387c98ed4b      1
    14 0x515755d6c208804c759af3f5a17a7cfdd3c15791      1
    15 0x5e130cb7f8cdcfb5a15018ee5846769703ec4478      1
    16 0x5f2bdf26f6528ce05aac77d7fa52bac7a836ef66      1
    17 0x74401b66f746630ccee98689b1a20e0149fbccab      1
    18 0x7b2ad6af37be6a77c95556d9453e4877b6a8437d      1
    19 0x886c4d1d4bc99399a42ef9cdcbf0f0dfa700a860      1
    20 0x8986de8e798ab056c7ec7054b3ac20f5565ede82      1
    21 0x9bc7b76e958e9322102c08059aaeff37f85704ac      1
    22 0xaf63db4e76a7cf1920232a748b65d5c9c8689fbb      1
    23 0xbe8fe12b9eb1ca2a593e6c070c71c294b6fe9f00      1
    24 0xbfa4b0328c09fea066044dac351a56ecc360d507      1
    25 0xcd8b72710595c69019c109aef5b1b92eea7f995f      1
    26 0xd394cf7e0c9f7c89f81a2870c576c1910974047b      1
    27 0xd81ce8e89dd987c8ab630858c8f1e9df14788c35      1
    28 0xd96ab2e36ef9b3ec905ddc25ae36f31ea150825a      1
    29 0xe89e3a3ab1aec780eb5019e1edc54cb4c7642c20      1
    30 0xebfd394de050a10b0a9d8408ef395aaa8d269de8      1
    31 0xf6d45c42445391b350be3cad097d85a0fe438174      1
    32 0xf74323badd0b59421fc3ca1b374a38622a0fcca3      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 39 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0000005bdbda0d9687bafa0e7a5831b8c4375cf8      1
     2 0x1149e32247435e624e9aaf631a74f40c0044994c      1
     3 0x1351d2cf237dc34eeec11f0e3740f0245f61a54e      1
     4 0x18561020ee51f3cff0d788d16a821b96c1d3b600      1
     5 0x1bb7b4f7d98a9d204186b36d3852e28f3286f577      1
     6 0x1e8e749b2b578e181ca01962e9448006772b24a2      1
     7 0x2882898129bfb577f756350d8443265038fce7cc      1
     8 0x2b91f332616084654098d4c13ceb9e82c8b73848      1
     9 0x301cc218009608e59f9647f4b7dd8a49be8493af      1
    10 0x330bce303d27df0eb1b856da3464278db1db1ac5      1
    11 0x389458f93e387fc568ca4568c231a64ffd0456d2      1
    12 0x3b23b754a8f59c9adaf721302a40862e387aaa4c      1
    13 0x3ec99927b3673e718bbbbde1df19eefcd0e03272      1
    14 0x4257564c6a110a896168a42efee4c5ae3dfd26f0      1
    15 0x443c0e1803d9059a7281d6a1c041e8f6a7b27663      1
    16 0x46b2bd5c888e6d57a839c559fd6076f2c15a8cb1      1
    17 0x478bb542f7658d635abba67edb987806dff5b83d      1
    18 0x5420082c71ac0bcab123d947d29b0370f2c548bd      1
    19 0x5f312df04b19979ef4bd5876737cfa481a928c0d      1
    20 0x5f98736a54e68104d5e4f272484840094712d66e      1
    21 0x637834627c4ee31c28d5494de3801a6dc21046b2      1
    22 0x64acc74b69faaaa90a5bcd5c46b4de3baf0c51b8      1
    23 0x667bedb9211dbe0dd20d4f00b51682be1e3f41ed      1
    24 0x66ff8f288c413e61c8ee807088ce301c03076e6e      1
    25 0x767a58ba5b0e404f5befbde4d7f00926df568fe4      1
    26 0x7ce30498e564f081ca65a226f44b1751f93a0f82      1
    27 0x8f489a0f2bfcea788163c7032388717306af387b      1
    28 0x93a119cef5f8bcc53a88cdb25b646aba59f3c613      1
    29 0x9631d389a1fa14996d93d3bd1948a4e844eecad7      1
    30 0xaa6f81a48768f1eb8c6c63dd349aa115e08b117b      1
    31 0xab0a13710c5efd1f149212782593bce505aea44c      1
    32 0xc63b52f868d719e36eeaa8726173b9ffb8090587      1
    33 0xcc72d9e9525b55297c03c2d09b9677617a02d156      1
    34 0xdf1e3abb229d42a182ad61ce8a63355a8a3eb0f8      1
    35 0xe37c3eb4f67d318503faa5f84e20c49ce59b9850      1
    36 0xf19ab0e65c902ffedcb95a76a93ca7bf789239ed      1
    37 0xf2439241881964006369c0e2377d45f3740f48a0      1
    38 0xf43b2be4aa887f426f05f78604b364af667c608d      1
    39 0xff7efba11a8888d6974997d4a8940bf83cd7a580      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 71 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0000005bdbda0d9687bafa0e7a5831b8c4375cf8      1
     2 0x03018c0dad483a344841675321d9676054dc9c9c      1
     3 0x03232119141e4eed18229bf1be2a3967c5fd435d      1
     4 0x0f0eae91990140c560d4156db4f00c854dc8f09e      1
     5 0x1149e32247435e624e9aaf631a74f40c0044994c      1
     6 0x1351d2cf237dc34eeec11f0e3740f0245f61a54e      1
     7 0x180669310d089fafcbee40479ee719752d680d4e      1
     8 0x18561020ee51f3cff0d788d16a821b96c1d3b600      1
     9 0x194cbec1b7f8ef5582c8b7f5f9b05fd2334c4c48      1
    10 0x1bb7b4f7d98a9d204186b36d3852e28f3286f577      1
    11 0x1e8e749b2b578e181ca01962e9448006772b24a2      1
    12 0x257d9128e4b8abe05ca3b045a216ed86bcf08af4      1
    13 0x2882898129bfb577f756350d8443265038fce7cc      1
    14 0x2b91f332616084654098d4c13ceb9e82c8b73848      1
    15 0x301cc218009608e59f9647f4b7dd8a49be8493af      1
    16 0x330bce303d27df0eb1b856da3464278db1db1ac5      1
    17 0x335e19b3f5dcfa2e398ee7d5ff430a1f8ccc88b2      1
    18 0x3775669465352353f599f73586494747463f851b      1
    19 0x389458f93e387fc568ca4568c231a64ffd0456d2      1
    20 0x3b23b754a8f59c9adaf721302a40862e387aaa4c      1
    21 0x3c30f1593c14357f2c2f7cb43a33903bbb7250a8      1
    22 0x3da1d16c93cb5dd30457bd7e2670663026b22e2c      1
    23 0x3ec99927b3673e718bbbbde1df19eefcd0e03272      1
    24 0x3fdbeedcbfd67cbc00fc169fcf557f77ea4ad4ed      1
    25 0x41d25f9650fc4b6737d44ac4804fbd5797e8f694      1
    26 0x4257564c6a110a896168a42efee4c5ae3dfd26f0      1
    27 0x443c0e1803d9059a7281d6a1c041e8f6a7b27663      1
    28 0x461123fb3a23b5503c01afdb83b0e4387c98ed4b      1
    29 0x46b2bd5c888e6d57a839c559fd6076f2c15a8cb1      1
    30 0x478bb542f7658d635abba67edb987806dff5b83d      1
    31 0x515755d6c208804c759af3f5a17a7cfdd3c15791      1
    32 0x5420082c71ac0bcab123d947d29b0370f2c548bd      1
    33 0x5e130cb7f8cdcfb5a15018ee5846769703ec4478      1
    34 0x5f2bdf26f6528ce05aac77d7fa52bac7a836ef66      1
    35 0x5f312df04b19979ef4bd5876737cfa481a928c0d      1
    36 0x5f98736a54e68104d5e4f272484840094712d66e      1
    37 0x637834627c4ee31c28d5494de3801a6dc21046b2      1
    38 0x64acc74b69faaaa90a5bcd5c46b4de3baf0c51b8      1
    39 0x667bedb9211dbe0dd20d4f00b51682be1e3f41ed      1
    40 0x66ff8f288c413e61c8ee807088ce301c03076e6e      1
    41 0x74401b66f746630ccee98689b1a20e0149fbccab      1
    42 0x767a58ba5b0e404f5befbde4d7f00926df568fe4      1
    43 0x7b2ad6af37be6a77c95556d9453e4877b6a8437d      1
    44 0x7ce30498e564f081ca65a226f44b1751f93a0f82      1
    45 0x886c4d1d4bc99399a42ef9cdcbf0f0dfa700a860      1
    46 0x8986de8e798ab056c7ec7054b3ac20f5565ede82      1
    47 0x8f489a0f2bfcea788163c7032388717306af387b      1
    48 0x93a119cef5f8bcc53a88cdb25b646aba59f3c613      1
    49 0x9631d389a1fa14996d93d3bd1948a4e844eecad7      1
    50 0x9bc7b76e958e9322102c08059aaeff37f85704ac      1
    51 0xaa6f81a48768f1eb8c6c63dd349aa115e08b117b      1
    52 0xab0a13710c5efd1f149212782593bce505aea44c      1
    53 0xaf63db4e76a7cf1920232a748b65d5c9c8689fbb      1
    54 0xbe8fe12b9eb1ca2a593e6c070c71c294b6fe9f00      1
    55 0xbfa4b0328c09fea066044dac351a56ecc360d507      1
    56 0xc63b52f868d719e36eeaa8726173b9ffb8090587      1
    57 0xcc72d9e9525b55297c03c2d09b9677617a02d156      1
    58 0xcd8b72710595c69019c109aef5b1b92eea7f995f      1
    59 0xd394cf7e0c9f7c89f81a2870c576c1910974047b      1
    60 0xd81ce8e89dd987c8ab630858c8f1e9df14788c35      1
    61 0xd96ab2e36ef9b3ec905ddc25ae36f31ea150825a      1
    62 0xdf1e3abb229d42a182ad61ce8a63355a8a3eb0f8      1
    63 0xe37c3eb4f67d318503faa5f84e20c49ce59b9850      1
    64 0xe89e3a3ab1aec780eb5019e1edc54cb4c7642c20      1
    65 0xebfd394de050a10b0a9d8408ef395aaa8d269de8      1
    66 0xf19ab0e65c902ffedcb95a76a93ca7bf789239ed      1
    67 0xf2439241881964006369c0e2377d45f3740f48a0      1
    68 0xf43b2be4aa887f426f05f78604b364af667c608d      1
    69 0xf6d45c42445391b350be3cad097d85a0fe438174      1
    70 0xf74323badd0b59421fc3ca1b374a38622a0fcca3      1
    71 0xff7efba11a8888d6974997d4a8940bf83cd7a580      1

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
