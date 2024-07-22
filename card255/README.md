
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance        contract        
     Length:185         Length:185         Min.   :1.000   Length:185        
     Class :character   Class :character   1st Qu.:1.000   Class :character  
     Mode  :character   Mode  :character   Median :1.000   Mode  :character  
                                           Mean   :1.081                     
                                           3rd Qu.:1.000                     
                                           Max.   :4.000                     
         name          
     Length:185        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 20344269 # https://etherscan.io/block/20344269
block_hash <- "0x747cfff5be1b3bf609114b70c383f0b0368e5b092c58f4b4b7d4b7abad81ea2b"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4829 

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

allow_artist1    <- pick(snapshot, contracts=c("SuperRare","EndlessCity","Blackandwhite","TravelingArtist","Lost","Blackandwhitegraphic"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("OvergrownhouseEditions","HOUSESEditions","GrishaKIMEditions","TeukumafasaEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("SuperRare","EndlessCity","Blackandwhite","TravelingArtist","Lost","Blackandwhitegraphic","OvergrownhouseEditions","HOUSESEditions","GrishaKIMEditions","TeukumafasaEditions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 16 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x00ff192363430a35abbf968c535b64147e88abdb      1
     2 0x052564eb0fd8b340803df55def89c25c432f43f4      1
     3 0x1e299dc52eb9fdab6a6849f9731a948d8d72e474      1
     4 0x20e7bfeef1d38899ec88124829dd34e711c1e26c      1
     5 0x2a0a412e0a0d436cca7ddba177c4dd9f29801062      1
     6 0x3f7cf1e0d51c141884d7ef41378747a505f30441      1
     7 0x46b2bd5c888e6d57a839c559fd6076f2c15a8cb1      1
     8 0x7c0a8020eb335dbccb6e2a66726143708a3f74d0      1
     9 0x7cdbb8e309ce7c701ed200be123f16c6f7c9e525      1
    10 0x8501cb78b0892f50999b01434b41fb2da657f635      1
    11 0x8754e1ed406a72f8deca5b4c9654ad2db698ba83      1
    12 0x991c0030ebbdaf7f4dd0cc579ac9c37f2fd505c9      1
    13 0xb11efa4a9dcbdd5f6125494d90f9a8f998bd4a86      1
    14 0xc542492296d3537eb3fd16fd775bdf7ab8721c7c      1
    15 0xc97705a8e073c9c0ba05d71aede57903a4a359f6      1
    16 0xe1c9b7038a03dc898cd161827709a3c241af991e      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 115 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x002a3be63b3b312da5d58eb02d2b48869b46ec82      1
      2 0x011384e15a858a29f2b954f4a67ad3052ddc6d95      1
      3 0x017347cb75ac8725608e593ea35d83f9b2b3cfb8      1
      4 0x0504aced77f859f8b0be73f864dd2f0c1019d41a      1
      5 0x057931155d0c6355b1bd108b0fbfe9bc77d59ef0      1
      6 0x05f1ac205448fa6a9211c0aee140a85038dc119a      1
      7 0x08db792f0dc2492c67ba901adbe136e76648341c      1
      8 0x09d36e24cab699ad357b63bf59da51ef9aee2c3e      1
      9 0x133fab28caec665ade2a67d1b360ea1eb18b812c      1
     10 0x133fc918b3a27fa314c45a08ec7eb794ef0283fc      1
     11 0x14ae8100ea85a11bbb36578f83ab1b5c1cfdd61c      1
     12 0x15174623b8e114eb2f31851205075fc6f81be6d0      1
     13 0x19a26acfe87d16234c9deee5159f9dcde5a2a6b8      1
     14 0x1a1427a73b7cb0f4ea3f71c6c8090c4366c8ebe1      1
     15 0x1e2fa044a25bca1576ed94562db50f51dcc24ca8      1
     16 0x20c1ee597a8633188757ec6ee2f7743445c95829      1
     17 0x21ca8b1b771fb330349f7bb386481b9c3369085b      1
     18 0x29bbb2014ac9c1e73ce88f6be44be8da900f9758      1
     19 0x336bd505b648d378a251f06ec3f6693620913f92      1
     20 0x33bfd3ba3bd175148d1bb1ee87e865253fd265e5      1
     21 0x3622e93e796fd33547ecd5a09f22a63c271b9797      1
     22 0x399d887ced702fa13bd5002f77e34d08d803e6c1      1
     23 0x3a8f60314e65276f4f95cb4725c2050d086ac6e4      1
     24 0x3ed03dde47b949d35113a7480b68b462439a5975      1
     25 0x3f526ece92247050be2c66cae5ab6c2cb8bc2a4b      1
     26 0x4c2f526cdaaeea289fb02fce54ac5cf9ffb5229b      1
     27 0x4e58101511abcecb8b10c75fb94871f0d2e0e9fc      1
     28 0x4f21318139fc7e9c3b3f2d4929c9fdeec9508640      1
     29 0x53b9fb382a61b81814ea765f2ae4263c79b815d4      1
     30 0x53e37d76b6a9fdf6718c1e7267088e052731777d      1
     31 0x54372fd91473e0f6f8507a955cd45de9b9d740d5      1
     32 0x54e2738a4ccc7c3855a2ea0f4fb2512b754cfea7      1
     33 0x55e69122687e631fd2ab4eb7da3b06e7a3fb181a      1
     34 0x59068075a799594db03c0255eed68e8e121155c8      1
     35 0x5f23bf4d66ff4ea4e4c888ea9ef13404fbeb7b49      1
     36 0x5f2d8fb9e5153bd38f2c2e9b01ed6f7270e3b79e      1
     37 0x5f73b065c8412b43e3db33cad0efc5038040a597      1
     38 0x61b04d2b5cb1f1f8505e7b4e1cb3d9e8a0b15bae      1
     39 0x634ffd24513c0def2127e2d086a81968f948c7d7      1
     40 0x63923511f7b628483f9e160cc173e68a5ded11b6      1
     41 0x66660e9f5e1439845b97430cc9322673f5e6183a      1
     42 0x66f5ddc253852f407d334f8e90e3bc46fdaaecaa      1
     43 0x68bca5a8bdebe05fb8a6648c7316b4eb7e19a064      1
     44 0x69083e9cf68da3a2fedec4e987c452c2ab790635      1
     45 0x6c6e93874216112ef12a0d04e2679ecc6c3625cc      1
     46 0x6e977c8155d914e7aa99331169939978a4d1b102      1
     47 0x6eff21c2bfdf83a33aa690fe3db17fc672244907      1
     48 0x711b3011baf52c3a34588faa54f520184f7e0ff1      1
     49 0x75a56cd64e10c4f07a6d2cb9027580b7b9a728db      1
     50 0x75aadbb7c36db057e5ac64183899f118de76df35      1
     51 0x75ef2eff7b12f5f900ef5c787036deab18423d1a      1
     52 0x762da606029d3120735aa1eec15464e265db7a3c      1
     53 0x76b69cfe1707f66e919c3aa194c10b908abd9f0f      1
     54 0x7737ca9d2407f14fba41547c01dcff2793605892      1
     55 0x78fc8aa7bb4ee047257a245ca590fac4ae4aa97b      1
     56 0x79cbef6a2dd83a3e01d228f9d321b969c5c03462      1
     57 0x7b640407513bc16167ef3450fd6339803982e976      1
     58 0x7c34ebf14789fa954a5ea7eceedb39dd7a093528      1
     59 0x7dccc79361b0227a85c2ff788ca240d31f2a78d0      1
     60 0x8305c262597a6727ad1356e488ac4523ed8ec102      1
     61 0x8625d262e2e2f5529499c79f5e26ff20d644f3a9      1
     62 0x89d53e8612e8416bf960dc9444ce0e2a0878a582      1
     63 0x8bc97b57e59df9e683c108c722cc03bd80a72484      1
     64 0x9127c9221b22ea3789c90383284c72dcd7d9b9fb      1
     65 0x938a0af4b86057489bc651dd02c080890d8ed5e5      1
     66 0x9572001a64ad0a42fb908429dfa0214a3c18079d      1
     67 0x9631d389a1fa14996d93d3bd1948a4e844eecad7      1
     68 0x96ff54c8407f4a775f5b4b035a52a9a6180555b2      1
     69 0x970bb85b997af2369b4be78e703e552d4d0d7158      1
     70 0x9ec8e1ec82e6a35bc014d4175dab7a8ae2eb6a2c      1
     71 0xa15b854360feb78d69aa3e68f66e9c9ec2355689      1
     72 0xa3599cf1048930c9e897fe9245dd95511cd16f1c      1
     73 0xa3785d2f231808d54aa207a84aee81c85dcbfb21      1
     74 0xaa060b1cdde1e12b090c1d73cb2cde3fe5354aff      1
     75 0xab94f597b94f45680b07997c394535d7ebc4a297      1
     76 0xadfeb395469613b3ab99d788811e2ccd3058e51c      1
     77 0xb237535c03d2cf80f22405f542e436793ddb1e7f      1
     78 0xb8d4651ee9d97d7da426f82648a20d0e0fccd1d0      1
     79 0xb94480dad90c3b41a9c24d88f78533973e18fa75      1
     80 0xc154962f0d6ce9f0271ec377111a7d58eb0e2b24      1
     81 0xc18d4c2fee42accfe7bfdce98bfacbabf76242e9      1
     82 0xc2978441f46a76c60e0cd59e986498b75a40572d      1
     83 0xc3edf7cf94c76c14e73a6888c228d37ffeefa1c0      1
     84 0xc4dff49b768256d545049318b220a1bedf9ca743      1
     85 0xc55bc21c8df108e4f34c41b412da4b3039940e8d      1
     86 0xc7b85a9ccd23894fe34bb5462c4e902742faf22a      1
     87 0xcaf18bef4a4eb041017a83bea61f94a9c46f675f      1
     88 0xcd8b72710595c69019c109aef5b1b92eea7f995f      1
     89 0xd0b643edaa2be4aba13db18cb3e3d608cf567f61      1
     90 0xd35fc346e15ba5b446917c9fd23a9471d6144701      1
     91 0xd6d9977522c6f4126ec407de2af999c29e672268      1
     92 0xd6ee002752c995a118e42611baf52aba7cc1fef1      1
     93 0xd7efae6e8e0556ebb5e77a499a34fce6a4d8c722      1
     94 0xd81ce8e89dd987c8ab630858c8f1e9df14788c35      1
     95 0xd9a64ab214cc849ae97c67a40caac2a71e38acf3      1
     96 0xdaed1d81a52b2af44e30afc332ccdea86399cd09      1
     97 0xdca334874727e8f67070e16b20e8f02ffc4595c2      1
     98 0xdd0c252067e1197ef2c4ec48ea024d705159ed0a      1
     99 0xe108f21ac66745b0d2f825b7651fbd610422b3da      1
    100 0xe2b2793d51c652106807caf9b56db81f71e7e280      1
    101 0xe390b1ade1abbd83a2b103f3e2c02a3f45a1774d      1
    102 0xeaddbae2651e21b4ae261ffbfcc81970854c4ef6      1
    103 0xeb1c22baacafac7836f20f684c946228401ff01c      1
    104 0xec0790f972270ea0df143731f4b938d7eaf7fbf8      1
    105 0xef0621533b2c435c53964bd30eae64e24f10ffbe      1
    106 0xef34feb024f0a6f4d5cf3a7cd30bb0f041a6af58      1
    107 0xf1dafb1ca027e69b67952e8a385e7f43791f2e70      1
    108 0xf2b4a725e943b54cdabada2c9ae2727874a49eaa      1
    109 0xf3290df945e484a4ff846fb5baeb6d1147a7aa69      1
    110 0xf3fda50f3963e704e8ae85a982631c96afb999b2      1
    111 0xf76a07f67e1f6f9db8ebba3ee9acb6b8933f89f0      1
    112 0xf85d8406acbaca3c3e69b2fefaeeb050c427337b      1
    113 0xfb52fa34d865c0398a123ff4cfa2d54c1a453ce3      1
    114 0xfbff6be0bc8ca216aa62075b2798b70a6d099ae6      1
    115 0xfd861c7e3bd41a83b50f9dcb054600d18a7dfb93      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 131 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x002a3be63b3b312da5d58eb02d2b48869b46ec82      1
      2 0x00ff192363430a35abbf968c535b64147e88abdb      1
      3 0x011384e15a858a29f2b954f4a67ad3052ddc6d95      1
      4 0x017347cb75ac8725608e593ea35d83f9b2b3cfb8      1
      5 0x0504aced77f859f8b0be73f864dd2f0c1019d41a      1
      6 0x052564eb0fd8b340803df55def89c25c432f43f4      1
      7 0x057931155d0c6355b1bd108b0fbfe9bc77d59ef0      1
      8 0x05f1ac205448fa6a9211c0aee140a85038dc119a      1
      9 0x08db792f0dc2492c67ba901adbe136e76648341c      1
     10 0x09d36e24cab699ad357b63bf59da51ef9aee2c3e      1
     11 0x133fab28caec665ade2a67d1b360ea1eb18b812c      1
     12 0x133fc918b3a27fa314c45a08ec7eb794ef0283fc      1
     13 0x14ae8100ea85a11bbb36578f83ab1b5c1cfdd61c      1
     14 0x15174623b8e114eb2f31851205075fc6f81be6d0      1
     15 0x19a26acfe87d16234c9deee5159f9dcde5a2a6b8      1
     16 0x1a1427a73b7cb0f4ea3f71c6c8090c4366c8ebe1      1
     17 0x1e299dc52eb9fdab6a6849f9731a948d8d72e474      1
     18 0x1e2fa044a25bca1576ed94562db50f51dcc24ca8      1
     19 0x20c1ee597a8633188757ec6ee2f7743445c95829      1
     20 0x20e7bfeef1d38899ec88124829dd34e711c1e26c      1
     21 0x21ca8b1b771fb330349f7bb386481b9c3369085b      1
     22 0x29bbb2014ac9c1e73ce88f6be44be8da900f9758      1
     23 0x2a0a412e0a0d436cca7ddba177c4dd9f29801062      1
     24 0x336bd505b648d378a251f06ec3f6693620913f92      1
     25 0x33bfd3ba3bd175148d1bb1ee87e865253fd265e5      1
     26 0x3622e93e796fd33547ecd5a09f22a63c271b9797      1
     27 0x399d887ced702fa13bd5002f77e34d08d803e6c1      1
     28 0x3a8f60314e65276f4f95cb4725c2050d086ac6e4      1
     29 0x3ed03dde47b949d35113a7480b68b462439a5975      1
     30 0x3f526ece92247050be2c66cae5ab6c2cb8bc2a4b      1
     31 0x3f7cf1e0d51c141884d7ef41378747a505f30441      1
     32 0x46b2bd5c888e6d57a839c559fd6076f2c15a8cb1      1
     33 0x4c2f526cdaaeea289fb02fce54ac5cf9ffb5229b      1
     34 0x4e58101511abcecb8b10c75fb94871f0d2e0e9fc      1
     35 0x4f21318139fc7e9c3b3f2d4929c9fdeec9508640      1
     36 0x53b9fb382a61b81814ea765f2ae4263c79b815d4      1
     37 0x53e37d76b6a9fdf6718c1e7267088e052731777d      1
     38 0x54372fd91473e0f6f8507a955cd45de9b9d740d5      1
     39 0x54e2738a4ccc7c3855a2ea0f4fb2512b754cfea7      1
     40 0x55e69122687e631fd2ab4eb7da3b06e7a3fb181a      1
     41 0x59068075a799594db03c0255eed68e8e121155c8      1
     42 0x5f23bf4d66ff4ea4e4c888ea9ef13404fbeb7b49      1
     43 0x5f2d8fb9e5153bd38f2c2e9b01ed6f7270e3b79e      1
     44 0x5f73b065c8412b43e3db33cad0efc5038040a597      1
     45 0x61b04d2b5cb1f1f8505e7b4e1cb3d9e8a0b15bae      1
     46 0x634ffd24513c0def2127e2d086a81968f948c7d7      1
     47 0x63923511f7b628483f9e160cc173e68a5ded11b6      1
     48 0x66660e9f5e1439845b97430cc9322673f5e6183a      1
     49 0x66f5ddc253852f407d334f8e90e3bc46fdaaecaa      1
     50 0x68bca5a8bdebe05fb8a6648c7316b4eb7e19a064      1
     51 0x69083e9cf68da3a2fedec4e987c452c2ab790635      1
     52 0x6c6e93874216112ef12a0d04e2679ecc6c3625cc      1
     53 0x6e977c8155d914e7aa99331169939978a4d1b102      1
     54 0x6eff21c2bfdf83a33aa690fe3db17fc672244907      1
     55 0x711b3011baf52c3a34588faa54f520184f7e0ff1      1
     56 0x75a56cd64e10c4f07a6d2cb9027580b7b9a728db      1
     57 0x75aadbb7c36db057e5ac64183899f118de76df35      1
     58 0x75ef2eff7b12f5f900ef5c787036deab18423d1a      1
     59 0x762da606029d3120735aa1eec15464e265db7a3c      1
     60 0x76b69cfe1707f66e919c3aa194c10b908abd9f0f      1
     61 0x7737ca9d2407f14fba41547c01dcff2793605892      1
     62 0x78fc8aa7bb4ee047257a245ca590fac4ae4aa97b      1
     63 0x79cbef6a2dd83a3e01d228f9d321b969c5c03462      1
     64 0x7b640407513bc16167ef3450fd6339803982e976      1
     65 0x7c0a8020eb335dbccb6e2a66726143708a3f74d0      1
     66 0x7c34ebf14789fa954a5ea7eceedb39dd7a093528      1
     67 0x7cdbb8e309ce7c701ed200be123f16c6f7c9e525      1
     68 0x7dccc79361b0227a85c2ff788ca240d31f2a78d0      1
     69 0x8305c262597a6727ad1356e488ac4523ed8ec102      1
     70 0x8501cb78b0892f50999b01434b41fb2da657f635      1
     71 0x8625d262e2e2f5529499c79f5e26ff20d644f3a9      1
     72 0x8754e1ed406a72f8deca5b4c9654ad2db698ba83      1
     73 0x89d53e8612e8416bf960dc9444ce0e2a0878a582      1
     74 0x8bc97b57e59df9e683c108c722cc03bd80a72484      1
     75 0x9127c9221b22ea3789c90383284c72dcd7d9b9fb      1
     76 0x938a0af4b86057489bc651dd02c080890d8ed5e5      1
     77 0x9572001a64ad0a42fb908429dfa0214a3c18079d      1
     78 0x9631d389a1fa14996d93d3bd1948a4e844eecad7      1
     79 0x96ff54c8407f4a775f5b4b035a52a9a6180555b2      1
     80 0x970bb85b997af2369b4be78e703e552d4d0d7158      1
     81 0x991c0030ebbdaf7f4dd0cc579ac9c37f2fd505c9      1
     82 0x9ec8e1ec82e6a35bc014d4175dab7a8ae2eb6a2c      1
     83 0xa15b854360feb78d69aa3e68f66e9c9ec2355689      1
     84 0xa3599cf1048930c9e897fe9245dd95511cd16f1c      1
     85 0xa3785d2f231808d54aa207a84aee81c85dcbfb21      1
     86 0xaa060b1cdde1e12b090c1d73cb2cde3fe5354aff      1
     87 0xab94f597b94f45680b07997c394535d7ebc4a297      1
     88 0xadfeb395469613b3ab99d788811e2ccd3058e51c      1
     89 0xb11efa4a9dcbdd5f6125494d90f9a8f998bd4a86      1
     90 0xb237535c03d2cf80f22405f542e436793ddb1e7f      1
     91 0xb8d4651ee9d97d7da426f82648a20d0e0fccd1d0      1
     92 0xb94480dad90c3b41a9c24d88f78533973e18fa75      1
     93 0xc154962f0d6ce9f0271ec377111a7d58eb0e2b24      1
     94 0xc18d4c2fee42accfe7bfdce98bfacbabf76242e9      1
     95 0xc2978441f46a76c60e0cd59e986498b75a40572d      1
     96 0xc3edf7cf94c76c14e73a6888c228d37ffeefa1c0      1
     97 0xc4dff49b768256d545049318b220a1bedf9ca743      1
     98 0xc542492296d3537eb3fd16fd775bdf7ab8721c7c      1
     99 0xc55bc21c8df108e4f34c41b412da4b3039940e8d      1
    100 0xc7b85a9ccd23894fe34bb5462c4e902742faf22a      1
    101 0xc97705a8e073c9c0ba05d71aede57903a4a359f6      1
    102 0xcaf18bef4a4eb041017a83bea61f94a9c46f675f      1
    103 0xcd8b72710595c69019c109aef5b1b92eea7f995f      1
    104 0xd0b643edaa2be4aba13db18cb3e3d608cf567f61      1
    105 0xd35fc346e15ba5b446917c9fd23a9471d6144701      1
    106 0xd6d9977522c6f4126ec407de2af999c29e672268      1
    107 0xd6ee002752c995a118e42611baf52aba7cc1fef1      1
    108 0xd7efae6e8e0556ebb5e77a499a34fce6a4d8c722      1
    109 0xd81ce8e89dd987c8ab630858c8f1e9df14788c35      1
    110 0xd9a64ab214cc849ae97c67a40caac2a71e38acf3      1
    111 0xdaed1d81a52b2af44e30afc332ccdea86399cd09      1
    112 0xdca334874727e8f67070e16b20e8f02ffc4595c2      1
    113 0xdd0c252067e1197ef2c4ec48ea024d705159ed0a      1
    114 0xe108f21ac66745b0d2f825b7651fbd610422b3da      1
    115 0xe1c9b7038a03dc898cd161827709a3c241af991e      1
    116 0xe2b2793d51c652106807caf9b56db81f71e7e280      1
    117 0xe390b1ade1abbd83a2b103f3e2c02a3f45a1774d      1
    118 0xeaddbae2651e21b4ae261ffbfcc81970854c4ef6      1
    119 0xeb1c22baacafac7836f20f684c946228401ff01c      1
    120 0xec0790f972270ea0df143731f4b938d7eaf7fbf8      1
    121 0xef0621533b2c435c53964bd30eae64e24f10ffbe      1
    122 0xef34feb024f0a6f4d5cf3a7cd30bb0f041a6af58      1
    123 0xf1dafb1ca027e69b67952e8a385e7f43791f2e70      1
    124 0xf2b4a725e943b54cdabada2c9ae2727874a49eaa      1
    125 0xf3290df945e484a4ff846fb5baeb6d1147a7aa69      1
    126 0xf3fda50f3963e704e8ae85a982631c96afb999b2      1
    127 0xf76a07f67e1f6f9db8ebba3ee9acb6b8933f89f0      1
    128 0xf85d8406acbaca3c3e69b2fefaeeb050c427337b      1
    129 0xfb52fa34d865c0398a123ff4cfa2d54c1a453ce3      1
    130 0xfbff6be0bc8ca216aa62075b2798b70a6d099ae6      1
    131 0xfd861c7e3bd41a83b50f9dcb054600d18a7dfb93      1

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
