
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance        contract        
     Length:200         Length:200         Min.   : 1.00   Length:200        
     Class :character   Class :character   1st Qu.: 1.00   Class :character  
     Mode  :character   Mode  :character   Median : 1.00   Mode  :character  
                                           Mean   : 1.08                     
                                           3rd Qu.: 1.00                     
                                           Max.   :10.00                     
         name          
     Length:200        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 19994869 # https://etherscan.io/block/19994869
block_hash <- "0x2d2d2ea9736d19b224505261b1bb28e6310e2d49e63c77742d104b8264b28e3e"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4396 

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

allow_artist1    <- pick(snapshot, contracts=c("SuperRare","BYOB","SPILLINGOUT","BHAREORIGINALS","BuildingaHappierHome","bodyofmine","meets1stdibs","colourandprose"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("HappierHomeDeluxeEditions","concerto3","concerto2","concerto1","bhareeditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("SuperRare","BYOB","SPILLINGOUT","BHAREORIGINALS","BuildingaHappierHome","bodyofmine","meets1stdibs","colourandprose","HappierHomeDeluxeEditions","concerto3","concerto2","concerto1","bhareeditions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 63 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x01309f3916a147bfabd4c62744b70ea7ac7024f7      1
     2 0x024fcdad70fc6ec129abaf66085aaf5fa0c002b8      1
     3 0x0aa87daf0dd4ecaa131d68d888a2a636c405393c      1
     4 0x0c54a765c77a88d9cda0bd7ad9b10422848d92ef      1
     5 0x11f46fff2624737aa0d38507e761b6ef770eb94d      1
     6 0x1257ea6f17f3bd82b323789cf08b79191cc82b6d      1
     7 0x15ab7dd2261026db55623d0da3946a3e022be19f      1
     8 0x1a839858ddd3e73869f74cdc07c8c6ad8c8ebde6      1
     9 0x1e3b6cd74d67421f01c588ca0a22809dc34a4c13      1
    10 0x1e8e749b2b578e181ca01962e9448006772b24a2      1
    11 0x22cd049c57f070fd6bde748e493aa2627621ebf7      1
    12 0x26939a29fcb1ca8d3bcc6c50093828abbc687a37      1
    13 0x33192b5575be811d84435bc0b2fb5fcd86850e93      1
    14 0x3a6801f383c6327e7bfff23aac660627b52dccb3      1
    15 0x3db0b224099e56488d93f4e887a6bad88c39cfa4      1
    16 0x403afdf9ea925d3b48e719a44610da1679a57651      1
    17 0x461123fb3a23b5503c01afdb83b0e4387c98ed4b      1
    18 0x47d8404fcd1c52e6642c7ed374fab052b381ccc1      1
    19 0x4de31ffa366271f90a6da19130d7ae8ba2541437      1
    20 0x4f764a08c66251e13bdd85b4bb0652b739736328      1
    21 0x54aa7e866378e4ff3140aaf167571111cc9cb5d7      1
    22 0x55e9bf66235fb1461e5d8d7216f1a4bdbe85824f      1
    23 0x55eea5ec5e162003b0a51418f7dce3dfab7f7007      1
    24 0x5a015b81b58abbd18d0832ff7361c3b4b72b07e9      1
    25 0x605e2886d8e4632b8acf819af733aa2cbdc94583      1
    26 0x6559b24c8c573771aa2a210add88bdd85790bb93      1
    27 0x660c56a3f42d9527324af8a51cbe78e8f2db17ae      1
    28 0x70c12a940ad80fea9f13962b0902549bba41d661      1
    29 0x753f8dab2ce1e6ec33e2967bcdd4974f60025319      1
    30 0x782adafbf47a604f146af4a059908e946eae539f      1
    31 0x7fd1d8e388b29ef0b8a00ca07e2266686a5559ee      1
    32 0x80ce7206da1ed2804d645a91705212ed742f285d      1
    33 0x87bddc08871da3e3cb8665e4ff3be6a1906d0a9e      1
    34 0x93cd108f0c9098d0bca0af4d8d0ea498957ce310      1
    35 0x949b269f72d4f2721001de5036a5721b13050e39      1
    36 0x99f88b2ddacb2e64b3e276fa7bf9149fe9eb76ba      1
    37 0x9e279e147afde24e2bc1630953d16cdeae46fb0a      1
    38 0x9f870b4f93abd9d45a2bceb6f0dfe25c74195fe5      1
    39 0xa123b88714b76762ec9e1e35db6b2637bc9aa2a3      1
    40 0xa2b50bfd6c82d8e8aed5704f3b931d7c78d02397      1
    41 0xa30d095a87068b90063d8b2c114bb553579d39e7      1
    42 0xb12afb3bc6b5ce1aa201eee5db0b1beacaf9fd88      1
    43 0xb2f8e4163142c03505b17ace865fad5208d6d6a4      1
    44 0xb3007ff3c3f40bdf0126fec7c8e43c3fc50ea813      1
    45 0xb39ff833f6b42d474bc649e3f435856c8f0cb426      1
    46 0xc481aeec5a219ca98643e71f0b967f40bf211b38      1
    47 0xc5fe7016bdc0b777fbcbfa9b3ad99bf3c6789191      1
    48 0xc79556a222367ecb3c2291d5c6d2b3e58e25fbf1      1
    49 0xc8b55f3d63420e4a5c741b96fa0869a134f400c1      1
    50 0xca536f1a439eadb69643b850a198939bae77ee4e      1
    51 0xcd94d3aa70986658d876ed6497636167014b1d1e      1
    52 0xcfac7a1765f45404be1b384739802f2ddbb80954      1
    53 0xd0d521cf3c2125b35c9a74fdb3b839214b1491a7      1
    54 0xd13e37b2bea300edda889157740fc2a0505b54a7      1
    55 0xd425191823f55456c9a17c8792c3d9c03586bbc9      1
    56 0xd45fc1cbc8237ed160f6a8dc2a1730484502d3e8      1
    57 0xd60f499d1a45e9aadf9633b460b2c96030eb827b      1
    58 0xe16b8dcd205afbba3bdea91a00990f08d8a439fd      1
    59 0xe67af016e88a886e86c70697f128b54f1ac11539      1
    60 0xec8c1050b45789f9ee4d09dcc7d64aaf9e233338      1
    61 0xefe3f8fa7fa99d77f8e50c8484184906cfe02d12      1
    62 0xfe2b8dc957459ac2a08fef57576bcb8a89845d8b      1
    63 0xfe5e101381727ba5a0e2cee531016192cd7d63cc      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 59 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0040122ed305e0b16dfc9fd8e0e9a74a3cdd5509      1
     2 0x022b266d1aa39725a4e64aee661f0277461e4e39      1
     3 0x027cae2ed1a23350a751452e907b4120330f9762      1
     4 0x04c95c6f8ea7ff7c3f7a2f148fae75650c5a875e      1
     5 0x061600d3515b387d562504eed5a1a485f9ae0ee4      1
     6 0x08493a92a95d4e93bdbc49f66e7647f1445a9210      1
     7 0x0aaaac358f29ca7290f78651ae9b08285b5e16d4      1
     8 0x14368bfb69824aa8645a36e7ff38584ccfe0fbb6      1
     9 0x1896d34befc66b19e38a4f38de1a72882fbfb788      1
    10 0x1b609fda3c97e90003be00c8223026e0d99f49b4      1
    11 0x2168b2295e912a27c0015bad9f09f090ebce1a99      1
    12 0x333fccc5046da80e10078363f7e0faefe08aeab3      1
    13 0x37416906c8011358dab16f0d73beebf580d4afa8      1
    14 0x4d76c6fa02315881482dabf1e38ea20cc4da3057      1
    15 0x55295e3cd36b56ad153965e1fa1997b9018ef5b1      1
    16 0x552f01d67b352aaa38bc675e30ced97f2451df63      1
    17 0x560aea8fe38dd38a9a7ca6fee7679a4cc201ac45      1
    18 0x59c962fc701a30a038b3e42785601e30510a29fb      1
    19 0x5aa71418ac6238c78ea9a032a768dbbbf86bfeca      1
    20 0x5d2e0b9b58267e83f55caeee80e901da348053c0      1
    21 0x647eb74a5135a0f24beee3c2d8432adcbb32c2a8      1
    22 0x66ff8f288c413e61c8ee807088ce301c03076e6e      1
    23 0x69cb3b1de24e08f1cfc2994171b6c6930498f750      1
    24 0x69dc258aa8a13bf898135b5474777d8ce9f15e17      1
    25 0x6a290fd02a31c78a225b98f3d587d61f4aeba437      1
    26 0x6ce9f49239e8adaf5bda1154262a3a054fad9db3      1
    27 0x77039399801f462b4ed13444a266b16355c471bf      1
    28 0x7e8cea8f8a65ec35f6ae7464aa2457f37a0fedc1      1
    29 0x8084b734329d203e65e08526b7ebe3b7895a1aa2      1
    30 0x83eed79eb18ba44e73a93b0b030e5044cc924f44      1
    31 0x84061843db2d0787a8aa7f5d623ffd709e4c4d74      1
    32 0x84f190d26ce9b2dfdc4b851cdeca634f0ef59e63      1
    33 0x9df495176076301dd318633fe3f67b6ef783a165      1
    34 0xa34223a79b330865a0141dbc08b190a9439d418d      1
    35 0xa4ca6d7afe69d4018c643e1ab3c1bdb7dc3462b4      1
    36 0xa837c3d5fdfe4e878a3f8370df89cfd972b116e1      1
    37 0xac06d32f100d50ca67f290307ec590443d639c8e      1
    38 0xb072ca86404a0ca5780b01a9f3ac60205b3d8018      1
    39 0xb3711303da2f2af927c4ab32ed4c0693572f35c7      1
    40 0xb89e89b79513d251341924698d50962c69dba33a      1
    41 0xc18d4c2fee42accfe7bfdce98bfacbabf76242e9      1
    42 0xc8bec4c9054784d1dfdab614cf8d9cb547a82107      1
    43 0xc9d25b9a3496c776688833d6ccfe507ef4f41645      1
    44 0xcaafdfe9f4cd69ebba2ca0d3f1ee8da6075756f9      1
    45 0xd1f3a51bde35942789eb3b3ba205fcdc340c8552      1
    46 0xdc7d0d897e1b7998c780d16b3c08482a74e71f33      1
    47 0xdf38a8b16f9bb88083c1f1edad41c5323d60f8a8      1
    48 0xe052113bd7d7700d623414a0a4585bcae754e9d5      1
    49 0xe47ac96b9a7b46ce16274d70153c30d98b251bdd      1
    50 0xe7bf954096508f883aaa8c4c9bdd616d8ce881e4      1
    51 0xe93939f3494900a812abe0d488674cf409dd16f4      1
    52 0xebe4c00073f1061ad27859478adcb4f5bc8ca67c      1
    53 0xee49f82e58a1c2b306720d0c68047cbf70c11fb5      1
    54 0xef755f4d7dd6241475030a3b268dbddaf997579a      1
    55 0xf297ffe8bd936dbbef991c913ea4a3c7bb2f3bb2      1
    56 0xf38f67d8b16752b18f7358ef216a35423e45f806      1
    57 0xfc62f4f007e53e7ac5bdab00ec28431c36634da0      1
    58 0xfd6e50c8e434a1f49af4f20cc0640ba3da68f84f      1
    59 0xff41da21ff3c36ed08b2640d499b6943b881ca35      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 122 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x0040122ed305e0b16dfc9fd8e0e9a74a3cdd5509      1
      2 0x01309f3916a147bfabd4c62744b70ea7ac7024f7      1
      3 0x022b266d1aa39725a4e64aee661f0277461e4e39      1
      4 0x024fcdad70fc6ec129abaf66085aaf5fa0c002b8      1
      5 0x027cae2ed1a23350a751452e907b4120330f9762      1
      6 0x04c95c6f8ea7ff7c3f7a2f148fae75650c5a875e      1
      7 0x061600d3515b387d562504eed5a1a485f9ae0ee4      1
      8 0x08493a92a95d4e93bdbc49f66e7647f1445a9210      1
      9 0x0aa87daf0dd4ecaa131d68d888a2a636c405393c      1
     10 0x0aaaac358f29ca7290f78651ae9b08285b5e16d4      1
     11 0x0c54a765c77a88d9cda0bd7ad9b10422848d92ef      1
     12 0x11f46fff2624737aa0d38507e761b6ef770eb94d      1
     13 0x1257ea6f17f3bd82b323789cf08b79191cc82b6d      1
     14 0x14368bfb69824aa8645a36e7ff38584ccfe0fbb6      1
     15 0x15ab7dd2261026db55623d0da3946a3e022be19f      1
     16 0x1896d34befc66b19e38a4f38de1a72882fbfb788      1
     17 0x1a839858ddd3e73869f74cdc07c8c6ad8c8ebde6      1
     18 0x1b609fda3c97e90003be00c8223026e0d99f49b4      1
     19 0x1e3b6cd74d67421f01c588ca0a22809dc34a4c13      1
     20 0x1e8e749b2b578e181ca01962e9448006772b24a2      1
     21 0x2168b2295e912a27c0015bad9f09f090ebce1a99      1
     22 0x22cd049c57f070fd6bde748e493aa2627621ebf7      1
     23 0x26939a29fcb1ca8d3bcc6c50093828abbc687a37      1
     24 0x33192b5575be811d84435bc0b2fb5fcd86850e93      1
     25 0x333fccc5046da80e10078363f7e0faefe08aeab3      1
     26 0x37416906c8011358dab16f0d73beebf580d4afa8      1
     27 0x3a6801f383c6327e7bfff23aac660627b52dccb3      1
     28 0x3db0b224099e56488d93f4e887a6bad88c39cfa4      1
     29 0x403afdf9ea925d3b48e719a44610da1679a57651      1
     30 0x461123fb3a23b5503c01afdb83b0e4387c98ed4b      1
     31 0x47d8404fcd1c52e6642c7ed374fab052b381ccc1      1
     32 0x4d76c6fa02315881482dabf1e38ea20cc4da3057      1
     33 0x4de31ffa366271f90a6da19130d7ae8ba2541437      1
     34 0x4f764a08c66251e13bdd85b4bb0652b739736328      1
     35 0x54aa7e866378e4ff3140aaf167571111cc9cb5d7      1
     36 0x55295e3cd36b56ad153965e1fa1997b9018ef5b1      1
     37 0x552f01d67b352aaa38bc675e30ced97f2451df63      1
     38 0x55e9bf66235fb1461e5d8d7216f1a4bdbe85824f      1
     39 0x55eea5ec5e162003b0a51418f7dce3dfab7f7007      1
     40 0x560aea8fe38dd38a9a7ca6fee7679a4cc201ac45      1
     41 0x59c962fc701a30a038b3e42785601e30510a29fb      1
     42 0x5a015b81b58abbd18d0832ff7361c3b4b72b07e9      1
     43 0x5aa71418ac6238c78ea9a032a768dbbbf86bfeca      1
     44 0x5d2e0b9b58267e83f55caeee80e901da348053c0      1
     45 0x605e2886d8e4632b8acf819af733aa2cbdc94583      1
     46 0x647eb74a5135a0f24beee3c2d8432adcbb32c2a8      1
     47 0x6559b24c8c573771aa2a210add88bdd85790bb93      1
     48 0x660c56a3f42d9527324af8a51cbe78e8f2db17ae      1
     49 0x66ff8f288c413e61c8ee807088ce301c03076e6e      1
     50 0x69cb3b1de24e08f1cfc2994171b6c6930498f750      1
     51 0x69dc258aa8a13bf898135b5474777d8ce9f15e17      1
     52 0x6a290fd02a31c78a225b98f3d587d61f4aeba437      1
     53 0x6ce9f49239e8adaf5bda1154262a3a054fad9db3      1
     54 0x70c12a940ad80fea9f13962b0902549bba41d661      1
     55 0x753f8dab2ce1e6ec33e2967bcdd4974f60025319      1
     56 0x77039399801f462b4ed13444a266b16355c471bf      1
     57 0x782adafbf47a604f146af4a059908e946eae539f      1
     58 0x7e8cea8f8a65ec35f6ae7464aa2457f37a0fedc1      1
     59 0x7fd1d8e388b29ef0b8a00ca07e2266686a5559ee      1
     60 0x8084b734329d203e65e08526b7ebe3b7895a1aa2      1
     61 0x80ce7206da1ed2804d645a91705212ed742f285d      1
     62 0x83eed79eb18ba44e73a93b0b030e5044cc924f44      1
     63 0x84061843db2d0787a8aa7f5d623ffd709e4c4d74      1
     64 0x84f190d26ce9b2dfdc4b851cdeca634f0ef59e63      1
     65 0x87bddc08871da3e3cb8665e4ff3be6a1906d0a9e      1
     66 0x93cd108f0c9098d0bca0af4d8d0ea498957ce310      1
     67 0x949b269f72d4f2721001de5036a5721b13050e39      1
     68 0x99f88b2ddacb2e64b3e276fa7bf9149fe9eb76ba      1
     69 0x9df495176076301dd318633fe3f67b6ef783a165      1
     70 0x9e279e147afde24e2bc1630953d16cdeae46fb0a      1
     71 0x9f870b4f93abd9d45a2bceb6f0dfe25c74195fe5      1
     72 0xa123b88714b76762ec9e1e35db6b2637bc9aa2a3      1
     73 0xa2b50bfd6c82d8e8aed5704f3b931d7c78d02397      1
     74 0xa30d095a87068b90063d8b2c114bb553579d39e7      1
     75 0xa34223a79b330865a0141dbc08b190a9439d418d      1
     76 0xa4ca6d7afe69d4018c643e1ab3c1bdb7dc3462b4      1
     77 0xa837c3d5fdfe4e878a3f8370df89cfd972b116e1      1
     78 0xac06d32f100d50ca67f290307ec590443d639c8e      1
     79 0xb072ca86404a0ca5780b01a9f3ac60205b3d8018      1
     80 0xb12afb3bc6b5ce1aa201eee5db0b1beacaf9fd88      1
     81 0xb2f8e4163142c03505b17ace865fad5208d6d6a4      1
     82 0xb3007ff3c3f40bdf0126fec7c8e43c3fc50ea813      1
     83 0xb3711303da2f2af927c4ab32ed4c0693572f35c7      1
     84 0xb39ff833f6b42d474bc649e3f435856c8f0cb426      1
     85 0xb89e89b79513d251341924698d50962c69dba33a      1
     86 0xc18d4c2fee42accfe7bfdce98bfacbabf76242e9      1
     87 0xc481aeec5a219ca98643e71f0b967f40bf211b38      1
     88 0xc5fe7016bdc0b777fbcbfa9b3ad99bf3c6789191      1
     89 0xc79556a222367ecb3c2291d5c6d2b3e58e25fbf1      1
     90 0xc8b55f3d63420e4a5c741b96fa0869a134f400c1      1
     91 0xc8bec4c9054784d1dfdab614cf8d9cb547a82107      1
     92 0xc9d25b9a3496c776688833d6ccfe507ef4f41645      1
     93 0xca536f1a439eadb69643b850a198939bae77ee4e      1
     94 0xcaafdfe9f4cd69ebba2ca0d3f1ee8da6075756f9      1
     95 0xcd94d3aa70986658d876ed6497636167014b1d1e      1
     96 0xcfac7a1765f45404be1b384739802f2ddbb80954      1
     97 0xd0d521cf3c2125b35c9a74fdb3b839214b1491a7      1
     98 0xd13e37b2bea300edda889157740fc2a0505b54a7      1
     99 0xd1f3a51bde35942789eb3b3ba205fcdc340c8552      1
    100 0xd425191823f55456c9a17c8792c3d9c03586bbc9      1
    101 0xd45fc1cbc8237ed160f6a8dc2a1730484502d3e8      1
    102 0xd60f499d1a45e9aadf9633b460b2c96030eb827b      1
    103 0xdc7d0d897e1b7998c780d16b3c08482a74e71f33      1
    104 0xdf38a8b16f9bb88083c1f1edad41c5323d60f8a8      1
    105 0xe052113bd7d7700d623414a0a4585bcae754e9d5      1
    106 0xe16b8dcd205afbba3bdea91a00990f08d8a439fd      1
    107 0xe47ac96b9a7b46ce16274d70153c30d98b251bdd      1
    108 0xe67af016e88a886e86c70697f128b54f1ac11539      1
    109 0xe7bf954096508f883aaa8c4c9bdd616d8ce881e4      1
    110 0xe93939f3494900a812abe0d488674cf409dd16f4      1
    111 0xebe4c00073f1061ad27859478adcb4f5bc8ca67c      1
    112 0xec8c1050b45789f9ee4d09dcc7d64aaf9e233338      1
    113 0xee49f82e58a1c2b306720d0c68047cbf70c11fb5      1
    114 0xef755f4d7dd6241475030a3b268dbddaf997579a      1
    115 0xefe3f8fa7fa99d77f8e50c8484184906cfe02d12      1
    116 0xf297ffe8bd936dbbef991c913ea4a3c7bb2f3bb2      1
    117 0xf38f67d8b16752b18f7358ef216a35423e45f806      1
    118 0xfc62f4f007e53e7ac5bdab00ec28431c36634da0      1
    119 0xfd6e50c8e434a1f49af4f20cc0640ba3da68f84f      1
    120 0xfe2b8dc957459ac2a08fef57576bcb8a89845d8b      1
    121 0xfe5e101381727ba5a0e2cee531016192cd7d63cc      1
    122 0xff41da21ff3c36ed08b2640d499b6943b881ca35      1

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
