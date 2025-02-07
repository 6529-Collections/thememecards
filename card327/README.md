
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:228         Length:228         Min.   :1   Length:228        
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:228        
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

allow_artist1    <- pick(snapshot, contracts=c("SuperRare","SuperRare2","SpiritAnimals","NatureStudies","MakersPlace","Foundation","danielladoodleCollection"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("PathwayEditions","KnownOriginEditions","KnownOrigin2Editions","RussetGoldEditions","SundropEditions","DreamlandEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("SuperRare","SuperRare2","SpiritAnimals","NatureStudies","MakersPlace","Foundation","danielladoodleCollection","PathwayEditions","KnownOriginEditions","KnownOrigin2Editions","RussetGoldEditions","SundropEditions","DreamlandEditions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 77 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0183d9be08db4024c2dc9aec10b9526fc6adeef7      1
     2 0x01b41056fa643f2391d34fb5a1d4876cc3e692e4      1
     3 0x035da52e7aa085f7e71764c0c8a1ce6690e3dfef      1
     4 0x036a489268633441be512580066c175b72610572      1
     5 0x03e946eb92a2e2d32aa12c92ade2a8e6fb7943bf      1
     6 0x0504aced77f859f8b0be73f864dd2f0c1019d41a      1
     7 0x0674a9ffa6b3749247d4e4366a924c69d386ddf6      1
     8 0x092cd1a6d222a167f5d0767e6444c8b45c92cc72      1
     9 0x0b07bcf6e39f8c746cbb3b56dd8573cc0b60d683      1
    10 0x0d9dd37db38a7a780de6049faead08ec81a001b1      1
    11 0x0ed0d0f3a5f831d1567ad6f8e58280977a1b205c      1
    12 0x1006bb216bd18a13b3df7993e366449b0148ece9      1
    13 0x10e0f432af201961c8afad72fbc68653757b0460      1
    14 0x14441ac732c0d8cf15f10043d355da11c831d828      1
    15 0x14baaa573906c24ec9dbdbd3d092fdb869b2d0f4      1
    16 0x280676491188f56fa386d9833d84702ac1e24c71      1
    17 0x2d36fcb71196beff330a07197a78d87bd1447b58      1
    18 0x2e218a9dbc931a9de514f52ea9c941d1170555ff      1
    19 0x2f34dfb91116c5f56aeb444fd18e7ef0d8158f7b      1
    20 0x300db63d96b5b5061560e4f1ba7a04277ba14917      1
    21 0x306105b952e4143944754b630325144185c9b595      1
    22 0x3852471d266d9e2222ca9fdd922bafc904dc49e5      1
    23 0x3b9ffe03ece0938d4f8ddea48fd5a610f9df32ff      1
    24 0x3d5d1da38a94cc555ae32bc0039cae9bb3fd8092      1
    25 0x45faddea1c17a97cf61dd605226d511fd09dff7f      1
    26 0x463d5e23b9d49462d56994d411e5a9c2b7c4f1ad      1
    27 0x4daf9138d98dc4d2823145378fd36a29b4fbac32      1
    28 0x4fb3bcc6bfb95dcf8189c55e73ffd94e0c21f0ed      1
    29 0x50b88f3f1ea8948731c6af4c0ebfdef1beccd6fa      1
    30 0x55adbdf8a6396356de42aad937ea79caa2cab88f      1
    31 0x56b1f55c30ccee15d761ca6cfd46e751cb52a3fe      1
    32 0x5cbb31e39ca93ca13d8e9f686bf749b14076bc65      1
    33 0x5dbd2ba1e9443651378e4dd36acce9c87c5c9b82      1
    34 0x63b7d0006102e672e6936fcfe6197e4c782f51d4      1
    35 0x651350a0ddab993dd69eb18fe63914a90bdca006      1
    36 0x69021ae8769586d56791d29615959997c2012b99      1
    37 0x69e11a201756fb2e9326ebd3e57a9fd8ad4ba4a5      1
    38 0x6afbba8699e53d47bb986dda1dd39c73635cf73f      1
    39 0x6bfd30747ba72ac7bb2f20d6138e1312020fafe8      1
    40 0x6d4fdc55136dee5fb4f27a278c9cc35ad01d2d3c      1
    41 0x7405fe24003a50e4f4117d35e9b5a9f5e512fede      1
    42 0x762da606029d3120735aa1eec15464e265db7a3c      1
    43 0x7895e668bd43c9623af538f274c06cb22cbc74f2      1
    44 0x7fcfeb41316c67ca985c652e639367762d5e98da      1
    45 0x83a2fc8c7958422faa8a003e955751d696f546e1      1
    46 0x86286d93d79a9e5b78de97cbbc8dcaba0f2489fc      1
    47 0x8c2e67bd3d0f432ef75d9ae6623cd0dfbce66050      1
    48 0x8c42313ff71b7c816365ea1b546e450838000578      1
    49 0x8d7c3921ecf289a2c272529d0bd57f9c2e3f997a      1
    50 0x8e63e4376dde62cb82f5a91c4bce4156457dedb1      1
    51 0x92baffdd6cfb11a4e57a58ffec4833b4d1abd25d      1
    52 0x9769334fc882775f4951865aa473481880669d47      1
    53 0x9df947cd8a9810964473339c38a95201ecbe112d      1
    54 0xabd9643f92e771831a6dcd588844a75b03b0ac12      1
    55 0xac91fe820d836704770e1c3d374c42cd418c29c5      1
    56 0xb2d60143097b4f992bfbe955a22dbb2acd9a8eab      1
    57 0xb707c62e32ae30f774655bb564beff22c4ff103a      1
    58 0xb70b0cf596222839c3564e5e3af925bde49a3679      1
    59 0xbad470e65c8986c9bb3e5d75d8e3685b1679c393      1
    60 0xbaea3cf94abd0d6e0f029ef5b0e54e9424a72985      1
    61 0xc69b462d6e06e9df218243c7f487aea68926705f      1
    62 0xc87ccb68b985cbfe03e332c2732b3507a93fffe5      1
    63 0xc9a9d8943680e5986dedff4e07082c83a39585c6      1
    64 0xcc6c1d21e8474b3578e69eb036c712ab08ffdfbb      1
    65 0xd27fc8aee201ec6a35f3ae32c457d04c44965158      1
    66 0xe4957d0691f3aaeb414c693c1e8acde0bf4a22c3      1
    67 0xe6315e5da4fbaf222e2fff3def288e296af03d79      1
    68 0xec8c1050b45789f9ee4d09dcc7d64aaf9e233338      1
    69 0xed8b0f296a201d46066d895998bc5c191e47086d      1
    70 0xedba5d56d0147aee8a227d284bcaac03b4a87ed4      1
    71 0xef09d13bb0c85244169109e311944a30376d5e39      1
    72 0xf3fda50f3963e704e8ae85a982631c96afb999b2      1
    73 0xf52393e120f918ffba50410b90a29b1f8250c879      1
    74 0xf598aadf12a2fc4709b5db0c4c169715efaf2038      1
    75 0xf6b1a88d33ef451cfb23f2172ffb8860f7844fb6      1
    76 0xfab9a3d37999e12252b47468d2ffd4be15936012      1
    77 0xfb38cc51133ef30286229be866a261c36c1e8f8c      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 27 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x00b6f8f1ee0241075799aa7474c70304d8fa0814      1
     2 0x0bfbbd04e9265b28f047353c0678f895ff64ac17      1
     3 0x0eb47beabd9ce2cab7cbed57aa6b040975bdc1b7      1
     4 0x12581b265d8c3dd2ede993f57be85c3554016341      1
     5 0x13e856de31dccf0280802cd291b2c7c882d73d0c      1
     6 0x1ba12981833ec62721208f5e66f4201521f47339      1
     7 0x3a63c149cfc97591181f49601322864cd7d1aad4      1
     8 0x44ed32b27aebf445c209dab3ea7e3a9d50f37b2a      1
     9 0x586edd5c6eef0a4895b027e0f77d8433e322d9eb      1
    10 0x5b47b64b852354de153ff36c372ec8ec6550f8d7      1
    11 0x5e2889eb037aeac4f64fbb9856514f6dfebada63      1
    12 0x6186290b28d511bff971631c916244a9fc539cfe      1
    13 0x661804b590cd9158d6922457639b9f334e8f88ef      1
    14 0x684d0ec564f9ba389f237cae3807f7359a70502c      1
    15 0x68c1bd5d5f2dd748afdfa7b4b4ea043c60595095      1
    16 0x6d7cb92a4a8a92dfd8e86d3036891063b4efa68b      1
    17 0x6f846445a41d2461a1704f4de9351bbc2ef24692      1
    18 0x7d55580ed3479db36b1d680517ffe408ef177e05      1
    19 0x8c94072567b278b9fa12192cca899b3ce0ed5fdc      1
    20 0x8dc287825d71f758bab052608ba8a4f156f84176      1
    21 0xa52c1aa50fe9f138982696da79f2287c7c62a836      1
    22 0xa6379c8ef983f2edd73f6ca7e49ed5bc0a1bf161      1
    23 0xb38bd68a6f0ba599808d65c7a9cf2a428105b680      1
    24 0xc8690ca8c73329be3124b38944e19d80b509a733      1
    25 0xe309c4b70acb2c1ae526655abd8c26602a68a120      1
    26 0xee4243eea50d8840f459da4fada53679aec1c702      1
    27 0xfc278dd0a2757f24f80d4b5fdadee36314df76cb      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 104 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x00b6f8f1ee0241075799aa7474c70304d8fa0814      1
      2 0x0183d9be08db4024c2dc9aec10b9526fc6adeef7      1
      3 0x01b41056fa643f2391d34fb5a1d4876cc3e692e4      1
      4 0x035da52e7aa085f7e71764c0c8a1ce6690e3dfef      1
      5 0x036a489268633441be512580066c175b72610572      1
      6 0x03e946eb92a2e2d32aa12c92ade2a8e6fb7943bf      1
      7 0x0504aced77f859f8b0be73f864dd2f0c1019d41a      1
      8 0x0674a9ffa6b3749247d4e4366a924c69d386ddf6      1
      9 0x092cd1a6d222a167f5d0767e6444c8b45c92cc72      1
     10 0x0b07bcf6e39f8c746cbb3b56dd8573cc0b60d683      1
     11 0x0bfbbd04e9265b28f047353c0678f895ff64ac17      1
     12 0x0d9dd37db38a7a780de6049faead08ec81a001b1      1
     13 0x0eb47beabd9ce2cab7cbed57aa6b040975bdc1b7      1
     14 0x0ed0d0f3a5f831d1567ad6f8e58280977a1b205c      1
     15 0x1006bb216bd18a13b3df7993e366449b0148ece9      1
     16 0x10e0f432af201961c8afad72fbc68653757b0460      1
     17 0x12581b265d8c3dd2ede993f57be85c3554016341      1
     18 0x13e856de31dccf0280802cd291b2c7c882d73d0c      1
     19 0x14441ac732c0d8cf15f10043d355da11c831d828      1
     20 0x14baaa573906c24ec9dbdbd3d092fdb869b2d0f4      1
     21 0x1ba12981833ec62721208f5e66f4201521f47339      1
     22 0x280676491188f56fa386d9833d84702ac1e24c71      1
     23 0x2d36fcb71196beff330a07197a78d87bd1447b58      1
     24 0x2e218a9dbc931a9de514f52ea9c941d1170555ff      1
     25 0x2f34dfb91116c5f56aeb444fd18e7ef0d8158f7b      1
     26 0x300db63d96b5b5061560e4f1ba7a04277ba14917      1
     27 0x306105b952e4143944754b630325144185c9b595      1
     28 0x3852471d266d9e2222ca9fdd922bafc904dc49e5      1
     29 0x3a63c149cfc97591181f49601322864cd7d1aad4      1
     30 0x3b9ffe03ece0938d4f8ddea48fd5a610f9df32ff      1
     31 0x3d5d1da38a94cc555ae32bc0039cae9bb3fd8092      1
     32 0x44ed32b27aebf445c209dab3ea7e3a9d50f37b2a      1
     33 0x45faddea1c17a97cf61dd605226d511fd09dff7f      1
     34 0x463d5e23b9d49462d56994d411e5a9c2b7c4f1ad      1
     35 0x4daf9138d98dc4d2823145378fd36a29b4fbac32      1
     36 0x4fb3bcc6bfb95dcf8189c55e73ffd94e0c21f0ed      1
     37 0x50b88f3f1ea8948731c6af4c0ebfdef1beccd6fa      1
     38 0x55adbdf8a6396356de42aad937ea79caa2cab88f      1
     39 0x56b1f55c30ccee15d761ca6cfd46e751cb52a3fe      1
     40 0x586edd5c6eef0a4895b027e0f77d8433e322d9eb      1
     41 0x5b47b64b852354de153ff36c372ec8ec6550f8d7      1
     42 0x5cbb31e39ca93ca13d8e9f686bf749b14076bc65      1
     43 0x5dbd2ba1e9443651378e4dd36acce9c87c5c9b82      1
     44 0x5e2889eb037aeac4f64fbb9856514f6dfebada63      1
     45 0x6186290b28d511bff971631c916244a9fc539cfe      1
     46 0x63b7d0006102e672e6936fcfe6197e4c782f51d4      1
     47 0x651350a0ddab993dd69eb18fe63914a90bdca006      1
     48 0x661804b590cd9158d6922457639b9f334e8f88ef      1
     49 0x684d0ec564f9ba389f237cae3807f7359a70502c      1
     50 0x68c1bd5d5f2dd748afdfa7b4b4ea043c60595095      1
     51 0x69021ae8769586d56791d29615959997c2012b99      1
     52 0x69e11a201756fb2e9326ebd3e57a9fd8ad4ba4a5      1
     53 0x6afbba8699e53d47bb986dda1dd39c73635cf73f      1
     54 0x6bfd30747ba72ac7bb2f20d6138e1312020fafe8      1
     55 0x6d4fdc55136dee5fb4f27a278c9cc35ad01d2d3c      1
     56 0x6d7cb92a4a8a92dfd8e86d3036891063b4efa68b      1
     57 0x6f846445a41d2461a1704f4de9351bbc2ef24692      1
     58 0x7405fe24003a50e4f4117d35e9b5a9f5e512fede      1
     59 0x762da606029d3120735aa1eec15464e265db7a3c      1
     60 0x7895e668bd43c9623af538f274c06cb22cbc74f2      1
     61 0x7d55580ed3479db36b1d680517ffe408ef177e05      1
     62 0x7fcfeb41316c67ca985c652e639367762d5e98da      1
     63 0x83a2fc8c7958422faa8a003e955751d696f546e1      1
     64 0x86286d93d79a9e5b78de97cbbc8dcaba0f2489fc      1
     65 0x8c2e67bd3d0f432ef75d9ae6623cd0dfbce66050      1
     66 0x8c42313ff71b7c816365ea1b546e450838000578      1
     67 0x8c94072567b278b9fa12192cca899b3ce0ed5fdc      1
     68 0x8d7c3921ecf289a2c272529d0bd57f9c2e3f997a      1
     69 0x8dc287825d71f758bab052608ba8a4f156f84176      1
     70 0x8e63e4376dde62cb82f5a91c4bce4156457dedb1      1
     71 0x92baffdd6cfb11a4e57a58ffec4833b4d1abd25d      1
     72 0x9769334fc882775f4951865aa473481880669d47      1
     73 0x9df947cd8a9810964473339c38a95201ecbe112d      1
     74 0xa52c1aa50fe9f138982696da79f2287c7c62a836      1
     75 0xa6379c8ef983f2edd73f6ca7e49ed5bc0a1bf161      1
     76 0xabd9643f92e771831a6dcd588844a75b03b0ac12      1
     77 0xac91fe820d836704770e1c3d374c42cd418c29c5      1
     78 0xb2d60143097b4f992bfbe955a22dbb2acd9a8eab      1
     79 0xb38bd68a6f0ba599808d65c7a9cf2a428105b680      1
     80 0xb707c62e32ae30f774655bb564beff22c4ff103a      1
     81 0xb70b0cf596222839c3564e5e3af925bde49a3679      1
     82 0xbad470e65c8986c9bb3e5d75d8e3685b1679c393      1
     83 0xbaea3cf94abd0d6e0f029ef5b0e54e9424a72985      1
     84 0xc69b462d6e06e9df218243c7f487aea68926705f      1
     85 0xc8690ca8c73329be3124b38944e19d80b509a733      1
     86 0xc87ccb68b985cbfe03e332c2732b3507a93fffe5      1
     87 0xc9a9d8943680e5986dedff4e07082c83a39585c6      1
     88 0xcc6c1d21e8474b3578e69eb036c712ab08ffdfbb      1
     89 0xd27fc8aee201ec6a35f3ae32c457d04c44965158      1
     90 0xe309c4b70acb2c1ae526655abd8c26602a68a120      1
     91 0xe4957d0691f3aaeb414c693c1e8acde0bf4a22c3      1
     92 0xe6315e5da4fbaf222e2fff3def288e296af03d79      1
     93 0xec8c1050b45789f9ee4d09dcc7d64aaf9e233338      1
     94 0xed8b0f296a201d46066d895998bc5c191e47086d      1
     95 0xedba5d56d0147aee8a227d284bcaac03b4a87ed4      1
     96 0xee4243eea50d8840f459da4fada53679aec1c702      1
     97 0xef09d13bb0c85244169109e311944a30376d5e39      1
     98 0xf3fda50f3963e704e8ae85a982631c96afb999b2      1
     99 0xf52393e120f918ffba50410b90a29b1f8250c879      1
    100 0xf598aadf12a2fc4709b5db0c4c169715efaf2038      1
    101 0xf6b1a88d33ef451cfb23f2172ffb8860f7844fb6      1
    102 0xfab9a3d37999e12252b47468d2ffd4be15936012      1
    103 0xfb38cc51133ef30286229be866a261c36c1e8f8c      1
    104 0xfc278dd0a2757f24f80d4b5fdadee36314df76cb      1

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
