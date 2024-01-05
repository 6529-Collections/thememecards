
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance        contract        
     Length:327         Length:327         Min.   :1.000   Length:327        
     Class :character   Class :character   1st Qu.:1.000   Class :character  
     Mode  :character   Mode  :character   Median :1.000   Mode  :character  
                                           Mean   :1.064                     
                                           3rd Qu.:1.000                     
                                           Max.   :3.000                     
         name          
     Length:327        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 18896969 # https://etherscan.io/block/18896969
block_hash <- "0x774d7975d6bbeab3e65113f4600c7818fe65c098e8409437ee80eb4e81b0e4fa"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4626 

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

allow_artist1    <- pick(snapshot, contracts=c("SuperRare","1dontknows11","Foundation","KnownOrigin","1dontknows","RoyalHouseofMedici","1dkBD"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("ThelastbreathEditions","1dontknowsEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("SuperRare","1dontknows11","Foundation","KnownOrigin","1dontknows","RoyalHouseofMedici","1dkBD","ThelastbreathEditions","1dontknowsEditions"), address_remove=address_remove,address_max=1)
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
     1 0x052564eb0fd8b340803df55def89c25c432f43f4      1
     2 0x066ad9510d992444a856731d6af0888c2b2e5d76      1
     3 0x0dfd32320af10f58cd4a5c2b567b7739ea2d691c      1
     4 0x194cbec1b7f8ef5582c8b7f5f9b05fd2334c4c48      1
     5 0x1d4b9b250b1bd41daa35d94bf9204ec1b0494ee3      1
     6 0x1e299dc52eb9fdab6a6849f9731a948d8d72e474      1
     7 0x31c72d0d03adc76456d3c14a4fe19f8aa8307c16      1
     8 0x397fbd1e27352175cc5b6baf171a30911268823c      1
     9 0x40d6cc4ad15707844b320b1d3815e0f0cf09ff30      1
    10 0x40d775827365ae4d54cbc08a1a1c4f586b2c1d0a      1
    11 0x44f5b575d385ebc481a8e2d89bc5b86fcb2812a6      1
    12 0x44f829ccbdc9c15ca84d4fd3f00c4682c35f228e      1
    13 0x4592bdcf86f2bf3c82c68baa08d028a45ead125d      1
    14 0x55e9bf66235fb1461e5d8d7216f1a4bdbe85824f      1
    15 0x56e9796b0640904b0d8a691c85d8d4c58a9d75d8      1
    16 0x5aa71418ac6238c78ea9a032a768dbbbf86bfeca      1
    17 0x61c888e1aeb86420d7ccb15705fa192d19fbf363      1
    18 0x6b6ae848f555f70944bc99d736fe29fcaecf8b23      1
    19 0x6c47945ebbd4f4dd653e145825e50fcb069c23b1      1
    20 0x7b2ad6af37be6a77c95556d9453e4877b6a8437d      1
    21 0x8092f951225b060dac717c3e0b2aec98c9efd7ee      1
    22 0x8754e1ed406a72f8deca5b4c9654ad2db698ba83      1
    23 0x8c04a5777aa418f6b40a33e1984e605067bd2043      1
    24 0xa27c8e38a5b577d4c1aadd0426363ca83255c6d7      1
    25 0xa30d095a87068b90063d8b2c114bb553579d39e7      1
    26 0xab4273e7137f9531fcb47d558d9bab0e726e6937      1
    27 0xaff42573bc515b878513e945e246d0f1b8ff01cc      1
    28 0xbaea3cf94abd0d6e0f029ef5b0e54e9424a72985      1
    29 0xbc8cc8b1fee0ed52dea5acdfd0002c2a679ff13a      1
    30 0xeaddbae2651e21b4ae261ffbfcc81970854c4ef6      1
    31 0xf013e5f697e84c6831220a21a96556242ee9ad86      1
    32 0xfa2238eeb4e2e5612853bf29f925a36f971f9010      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 144 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x000a3e111b04a7b3ebcd59b8554b224336057a28      1
      2 0x022b266d1aa39725a4e64aee661f0277461e4e39      1
      3 0x02332483114d5761a740dfea90e8705f6b042eff      1
      4 0x027cae2ed1a23350a751452e907b4120330f9762      1
      5 0x02d61cc47cc9a19b2a023c33c094269d9af07862      1
      6 0x065094af6f8e06c0f0530bcf7d589e4d17608dfd      1
      7 0x0830337ed8d752b520d5547aadeae4b8d4154199      1
      8 0x09eaa08f8e288d34d416b92c53cadafb5cf1209b      1
      9 0x0a58e5e2e06a0f27c5b7db9d638f353af706bc3e      1
     10 0x0a8f272add5801eef838fe89b404a7e9d45f1893      1
     11 0x0bb75bef057da63a0ae4b25fe9adafd35cd92b87      1
     12 0x0d03dc7c38d838db62b0b6386622a5e2d9db66c8      1
     13 0x0d71ec30c323037bdac7bd174d4a95cec5a4ee0d      1
     14 0x0ea9f4f7c371e7044d187b3da7850ccaa6370a0c      1
     15 0x11669497499a3afb9dd5a7459c6dd9baa2457161      1
     16 0x1388ae7b87bdf98369d28c747642049d0078055d      1
     17 0x1418eeb275fd08e1bd1292c4befe8bb1c933aaf2      1
     18 0x174fb6da52a957b0620d0902ace4fea33ba84d35      1
     19 0x18e30984d2be32293299f79c17c76ef4f767fe09      1
     20 0x1c0c531442d0a7c0ced2dbda9ea02f43a0c0ccc2      1
     21 0x1cb9998ab32e6d172f301a10ed8e0272d96509b4      1
     22 0x1f3de51f4295be1ad3874706dc85f8effd989a22      1
     23 0x1fec1df6b61e7b20a30acc6e3974036479ed7d44      1
     24 0x1ff976ecff54149a9b929d708f29b233704e28b8      1
     25 0x2063c384e0cc27d32375fad27111d026e86e111e      1
     26 0x250302de8c3a0443dffc8e25f5d5d276ff5cde7f      1
     27 0x263b5c9df8af5b4a0b96a82d4d649da7a26f5c7c      1
     28 0x27b455c481d62f91a7b55d09cf2e3cd70858ef85      1
     29 0x2bd0b9e782648b66744cc22373eb600f6b987869      1
     30 0x2c3b3fea418bf2d82ab2036ee3a35cd9648fab7a      1
     31 0x2f71f0b0e5b7fc1b4f0ebd478d6b006a41a599b2      1
     32 0x301cc218009608e59f9647f4b7dd8a49be8493af      1
     33 0x3038b6ddf96e18c9d7f7b4fecaf7ea0d69c27de1      1
     34 0x3098c04b75c7fecba9c7a80fc24c097995e95681      1
     35 0x320456411d621674581108d1362ae39f86b869cf      1
     36 0x35882b95ad075746fcc0dd2e7d706a224cd790a6      1
     37 0x35a958858d46bcd9984360911bbaae44f4ef8f50      1
     38 0x37674e3f3e59579072eacb61cbcd445959be97a3      1
     39 0x3d889b6391506032f9e9380a8f5cd3199661fb72      1
     40 0x3de2a570f4a75001f7aebc0ea7a1082a0c042813      1
     41 0x3deed956b999b83361b85bff31d388c35125411d      1
     42 0x3edca1c467a44d10f6e63748c52552a04dcedc67      1
     43 0x3fb4a6b25b1cc3f58a2a669b2c25fda24c4dda35      1
     44 0x41254fe73d3b49f0fe3cc7743b4d40ef26bb338e      1
     45 0x420fe6199a0612006809dc0830d3f8cc80349088      1
     46 0x4660e9cbe586a7ec727e076b5593eab9c51b5e4c      1
     47 0x46f7909e605fec492b05265c7c6ecb46e94675b4      1
     48 0x4762b5ac03fc097a500b48cfefffc027e8b37227      1
     49 0x47ded080a4ef933e96e357d468363a8a0cac03f0      1
     50 0x47f5dcbb04e00951c228dbfa9934df1febd77680      1
     51 0x48a3c3187975058eabe2d1bd7535d4a26e3250ba      1
     52 0x490238b29f6595bce84fb1af434c2edaaa2ceca9      1
     53 0x497056b9e7832eeadb26c80724eb79d7ad0ca77d      1
     54 0x4a95836eae95b286e20b3296aa49ae59712a0d87      1
     55 0x4d255d9b31aa437a0df8d02565c99d0faa6ed49c      1
     56 0x4f764a08c66251e13bdd85b4bb0652b739736328      1
     57 0x4f8cdb39327588a2b11edcebc50cc1302aa87cfa      1
     58 0x5068893a404e022a4cbcfa26d17e37efdc4b8c56      1
     59 0x570ff649bb5ae8e032a4f9456aa1ad74cc3f3e8f      1
     60 0x5779ed044776ae2d0fb4c3ec9be5029b203df7ce      1
     61 0x5849734d2e3ada23b64e12311adfa6bcd6fe687c      1
     62 0x58f606e6a973e57165bb07057a75f047f42455a7      1
     63 0x5e3c1c9694346a27a7a1e33b904798d5393fad82      1
     64 0x5f2abc70bb1567c7807da87e944ddf965a0846bd      1
     65 0x5fc13c5a541041f8fa12fab492d064435eb8d2f5      1
     66 0x6605c7c3d96235beeb96650c025a96729dfc1f00      1
     67 0x66ff8f288c413e61c8ee807088ce301c03076e6e      1
     68 0x6c802912a21f3c0382d5286ef1e799fc4da3e7d7      1
     69 0x6cd6604bef8786ba11d5b04777916d3ddfa313fd      1
     70 0x6e977c8155d914e7aa99331169939978a4d1b102      1
     71 0x7091d6cb3e3497664e29b0022d516eaeb6d9ffe6      1
     72 0x70ff537a22312615a16e1ffef022c6b49b86d0ea      1
     73 0x757a21fbd39cccaff681e1930e273b9add0008db      1
     74 0x78086ad810f8f99a0b6c92a9a6c8857d3c665622      1
     75 0x79e4b132776e182c7d2d4322833609cba1ee7956      1
     76 0x7a0208a0b55fcbc31915e8ba7495e5b1f71609e7      1
     77 0x7c4a24c5b1c0eba323ac796b841708ca4f79feb1      1
     78 0x7cf663ebccb166f7b6f49388afaa250cb073fc00      1
     79 0x7d076d594dfdfbb04892be82e221328294ca044e      1
     80 0x7dff7fbd4e32ef7e77afb70d9d1e74709acd9007      1
     81 0x7e5124202b1a177cbc254b2c2262541682f20601      1
     82 0x7fc200b56b7d0120193f034aa20a51eebe15eea2      1
     83 0x7fdab8c244e0b775edeeb9ebb8356b59068c6873      1
     84 0x805f7c2e0a8b723e0fd3e1a1839a447641a999b1      1
     85 0x832408210f74c5cd8ac6fd8ebc92bb6b8a7834a0      1
     86 0x8d50d2eeeed7ea1de60c51ba3f767e48dfbd2320      1
     87 0x8fc4e14c1c2ab50be594208b0e5f1a22056316d3      1
     88 0x90198c48129ac7564933f8bab85669e303ff6b9f      1
     89 0x911c53395562d2947b891423886bff7218410931      1
     90 0x927705d26bd2aacfd0ed7492e8e21a86fecb4d1b      1
     91 0x9478c28d953f0a698bbb28441b7adb378bed125c      1
     92 0x94b08f4daf6b0b1501fe4297280ec99412f66030      1
     93 0x951360e606587ab33ebcad952593b2fc99d68c3a      1
     94 0x95bd9e6272a6967dd867d37ae1395f4810ca6398      1
     95 0x970bb85b997af2369b4be78e703e552d4d0d7158      1
     96 0x9744bd33f16340577b77a717872961f74e24d2dc      1
     97 0x994b2b32530c95cd4bdac0ec0e0b41b187385b94      1
     98 0x999eaef521b7425aca4c764cfb0ad0dc6e336e20      1
     99 0x99d81318e6dc3933ff30325aa2283474981c4e7d      1
    100 0x9d8fef7b595c9f1ea30d6fb4d85b655011c63e47      1
    101 0x9f35af4727fb91114563d8a8f779a792cb557f3f      1
    102 0xa261634e88daf2630946ee07c643bbd4de640eca      1
    103 0xa2b619163985c363a25e210a91023dda4e08d667      1
    104 0xa7695409c5fef39a8367759a279386302a683b9a      1
    105 0xab94f597b94f45680b07997c394535d7ebc4a297      1
    106 0xadb3b6543d2cc69d2d62b617555eb89d4db4af0d      1
    107 0xb11b38a7c3fdb1a151050dd13e62ec63c611ab47      1
    108 0xb18fd1c3505f1305df7881ae353429796e6c5c7f      1
    109 0xb4d7264fd36ea63210d4647a174feb04956eca4e      1
    110 0xb4e14ef794585b51afb4cf4c77d8dbae66f096c9      1
    111 0xb98d10d9f6d07ba283bfd21b2dfec050f9ae282a      1
    112 0xbb3be72a5f745a5a055dafa72b13a6e04fc5859e      1
    113 0xbbbc24fe3538ed5e6bb9b9c4fa0442c4c446720f      1
    114 0xbda74636999a025cbd9f7b16edd65323e95b1bb3      1
    115 0xbdbd64b55aaf5bdab528b0725f33a19a728b11d7      1
    116 0xbe2f803bfbcffbcd77aae4e9104406abfeda497a      1
    117 0xc18d4c2fee42accfe7bfdce98bfacbabf76242e9      1
    118 0xc33da4e7e5216130d90af5a4292db6c46ada41cc      1
    119 0xc5c9cfe3e5771b3e5a57a62c2141c58f2a3749c8      1
    120 0xc9ecb2ae80e7ddc67f3fb71b5a8c1b54b6836127      1
    121 0xca536f1a439eadb69643b850a198939bae77ee4e      1
    122 0xcd33e2aa93223ecc6cc816d836dce0f3e635732f      1
    123 0xce013eb11d874c8fa60ec9007626e154f33ef2c1      1
    124 0xcfceafde40ce4ae35cc62657eb742103b8e925fb      1
    125 0xd2498b4dc8402789736f7c94caf969ea65badfa2      1
    126 0xd258ab439a3e8d921149d1ac882e181ba82f41e7      1
    127 0xd841d89ff8775966750eae25547757751460e1eb      1
    128 0xda9e817a9d8a41b18252ebd0d77b38739ef8df23      1
    129 0xdab68971008b8565e363cee4dfd217420910a899      1
    130 0xdf44e1b315ad475bb10919f1312d4324852ad66f      1
    131 0xdff3c57d16c68d04d3961a66710cbb37624f3fd9      1
    132 0xe3a6f6025d361b75ac0f95b5e3820f12b357deee      1
    133 0xe8fb02309e4b2d7a7c82ebb2d9fc9472b90fd942      1
    134 0xe9b9bd9192c73ebda2f6c6f2b6b26d1425d0170d      1
    135 0xec7100abdbcf922f975148c6516bc95696ca0ef6      1
    136 0xed155c1d726d24a9193d3c3781a53610fb87caf9      1
    137 0xedda20e0cc11fae46747868fcea07e60c9b25379      1
    138 0xf17d31a3c87d4dcbc4eeecf9e5fc1822e4a99cd4      1
    139 0xf31781b4811b737e66662997679fb07dacf63355      1
    140 0xf76a3c1965b15fa7b109bc7ce18d64246dae6d72      1
    141 0xf8290ab401fd6bc9d7ad93b896c59b04e89ab354      1
    142 0xfa4e98a3973ac6d51918e69d49f749219518c5a5      1
    143 0xfb1bb9d4a4c50457d339a686212a0716ea0adc80      1
    144 0xfb2ec6543d97808d8a00fca199c39022b0b3368f      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 176 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x000a3e111b04a7b3ebcd59b8554b224336057a28      1
      2 0x022b266d1aa39725a4e64aee661f0277461e4e39      1
      3 0x02332483114d5761a740dfea90e8705f6b042eff      1
      4 0x027cae2ed1a23350a751452e907b4120330f9762      1
      5 0x02d61cc47cc9a19b2a023c33c094269d9af07862      1
      6 0x052564eb0fd8b340803df55def89c25c432f43f4      1
      7 0x065094af6f8e06c0f0530bcf7d589e4d17608dfd      1
      8 0x066ad9510d992444a856731d6af0888c2b2e5d76      1
      9 0x0830337ed8d752b520d5547aadeae4b8d4154199      1
     10 0x09eaa08f8e288d34d416b92c53cadafb5cf1209b      1
     11 0x0a58e5e2e06a0f27c5b7db9d638f353af706bc3e      1
     12 0x0a8f272add5801eef838fe89b404a7e9d45f1893      1
     13 0x0bb75bef057da63a0ae4b25fe9adafd35cd92b87      1
     14 0x0d03dc7c38d838db62b0b6386622a5e2d9db66c8      1
     15 0x0d71ec30c323037bdac7bd174d4a95cec5a4ee0d      1
     16 0x0dfd32320af10f58cd4a5c2b567b7739ea2d691c      1
     17 0x0ea9f4f7c371e7044d187b3da7850ccaa6370a0c      1
     18 0x11669497499a3afb9dd5a7459c6dd9baa2457161      1
     19 0x1388ae7b87bdf98369d28c747642049d0078055d      1
     20 0x1418eeb275fd08e1bd1292c4befe8bb1c933aaf2      1
     21 0x174fb6da52a957b0620d0902ace4fea33ba84d35      1
     22 0x18e30984d2be32293299f79c17c76ef4f767fe09      1
     23 0x194cbec1b7f8ef5582c8b7f5f9b05fd2334c4c48      1
     24 0x1c0c531442d0a7c0ced2dbda9ea02f43a0c0ccc2      1
     25 0x1cb9998ab32e6d172f301a10ed8e0272d96509b4      1
     26 0x1d4b9b250b1bd41daa35d94bf9204ec1b0494ee3      1
     27 0x1e299dc52eb9fdab6a6849f9731a948d8d72e474      1
     28 0x1f3de51f4295be1ad3874706dc85f8effd989a22      1
     29 0x1fec1df6b61e7b20a30acc6e3974036479ed7d44      1
     30 0x1ff976ecff54149a9b929d708f29b233704e28b8      1
     31 0x2063c384e0cc27d32375fad27111d026e86e111e      1
     32 0x250302de8c3a0443dffc8e25f5d5d276ff5cde7f      1
     33 0x263b5c9df8af5b4a0b96a82d4d649da7a26f5c7c      1
     34 0x27b455c481d62f91a7b55d09cf2e3cd70858ef85      1
     35 0x2bd0b9e782648b66744cc22373eb600f6b987869      1
     36 0x2c3b3fea418bf2d82ab2036ee3a35cd9648fab7a      1
     37 0x2f71f0b0e5b7fc1b4f0ebd478d6b006a41a599b2      1
     38 0x301cc218009608e59f9647f4b7dd8a49be8493af      1
     39 0x3038b6ddf96e18c9d7f7b4fecaf7ea0d69c27de1      1
     40 0x3098c04b75c7fecba9c7a80fc24c097995e95681      1
     41 0x31c72d0d03adc76456d3c14a4fe19f8aa8307c16      1
     42 0x320456411d621674581108d1362ae39f86b869cf      1
     43 0x35882b95ad075746fcc0dd2e7d706a224cd790a6      1
     44 0x35a958858d46bcd9984360911bbaae44f4ef8f50      1
     45 0x37674e3f3e59579072eacb61cbcd445959be97a3      1
     46 0x397fbd1e27352175cc5b6baf171a30911268823c      1
     47 0x3d889b6391506032f9e9380a8f5cd3199661fb72      1
     48 0x3de2a570f4a75001f7aebc0ea7a1082a0c042813      1
     49 0x3deed956b999b83361b85bff31d388c35125411d      1
     50 0x3edca1c467a44d10f6e63748c52552a04dcedc67      1
     51 0x3fb4a6b25b1cc3f58a2a669b2c25fda24c4dda35      1
     52 0x40d6cc4ad15707844b320b1d3815e0f0cf09ff30      1
     53 0x40d775827365ae4d54cbc08a1a1c4f586b2c1d0a      1
     54 0x41254fe73d3b49f0fe3cc7743b4d40ef26bb338e      1
     55 0x420fe6199a0612006809dc0830d3f8cc80349088      1
     56 0x44f5b575d385ebc481a8e2d89bc5b86fcb2812a6      1
     57 0x44f829ccbdc9c15ca84d4fd3f00c4682c35f228e      1
     58 0x4592bdcf86f2bf3c82c68baa08d028a45ead125d      1
     59 0x4660e9cbe586a7ec727e076b5593eab9c51b5e4c      1
     60 0x46f7909e605fec492b05265c7c6ecb46e94675b4      1
     61 0x4762b5ac03fc097a500b48cfefffc027e8b37227      1
     62 0x47ded080a4ef933e96e357d468363a8a0cac03f0      1
     63 0x47f5dcbb04e00951c228dbfa9934df1febd77680      1
     64 0x48a3c3187975058eabe2d1bd7535d4a26e3250ba      1
     65 0x490238b29f6595bce84fb1af434c2edaaa2ceca9      1
     66 0x497056b9e7832eeadb26c80724eb79d7ad0ca77d      1
     67 0x4a95836eae95b286e20b3296aa49ae59712a0d87      1
     68 0x4d255d9b31aa437a0df8d02565c99d0faa6ed49c      1
     69 0x4f764a08c66251e13bdd85b4bb0652b739736328      1
     70 0x4f8cdb39327588a2b11edcebc50cc1302aa87cfa      1
     71 0x5068893a404e022a4cbcfa26d17e37efdc4b8c56      1
     72 0x55e9bf66235fb1461e5d8d7216f1a4bdbe85824f      1
     73 0x56e9796b0640904b0d8a691c85d8d4c58a9d75d8      1
     74 0x570ff649bb5ae8e032a4f9456aa1ad74cc3f3e8f      1
     75 0x5779ed044776ae2d0fb4c3ec9be5029b203df7ce      1
     76 0x5849734d2e3ada23b64e12311adfa6bcd6fe687c      1
     77 0x58f606e6a973e57165bb07057a75f047f42455a7      1
     78 0x5aa71418ac6238c78ea9a032a768dbbbf86bfeca      1
     79 0x5e3c1c9694346a27a7a1e33b904798d5393fad82      1
     80 0x5f2abc70bb1567c7807da87e944ddf965a0846bd      1
     81 0x5fc13c5a541041f8fa12fab492d064435eb8d2f5      1
     82 0x61c888e1aeb86420d7ccb15705fa192d19fbf363      1
     83 0x6605c7c3d96235beeb96650c025a96729dfc1f00      1
     84 0x66ff8f288c413e61c8ee807088ce301c03076e6e      1
     85 0x6b6ae848f555f70944bc99d736fe29fcaecf8b23      1
     86 0x6c47945ebbd4f4dd653e145825e50fcb069c23b1      1
     87 0x6c802912a21f3c0382d5286ef1e799fc4da3e7d7      1
     88 0x6cd6604bef8786ba11d5b04777916d3ddfa313fd      1
     89 0x6e977c8155d914e7aa99331169939978a4d1b102      1
     90 0x7091d6cb3e3497664e29b0022d516eaeb6d9ffe6      1
     91 0x70ff537a22312615a16e1ffef022c6b49b86d0ea      1
     92 0x757a21fbd39cccaff681e1930e273b9add0008db      1
     93 0x78086ad810f8f99a0b6c92a9a6c8857d3c665622      1
     94 0x79e4b132776e182c7d2d4322833609cba1ee7956      1
     95 0x7a0208a0b55fcbc31915e8ba7495e5b1f71609e7      1
     96 0x7b2ad6af37be6a77c95556d9453e4877b6a8437d      1
     97 0x7c4a24c5b1c0eba323ac796b841708ca4f79feb1      1
     98 0x7cf663ebccb166f7b6f49388afaa250cb073fc00      1
     99 0x7d076d594dfdfbb04892be82e221328294ca044e      1
    100 0x7dff7fbd4e32ef7e77afb70d9d1e74709acd9007      1
    101 0x7e5124202b1a177cbc254b2c2262541682f20601      1
    102 0x7fc200b56b7d0120193f034aa20a51eebe15eea2      1
    103 0x7fdab8c244e0b775edeeb9ebb8356b59068c6873      1
    104 0x805f7c2e0a8b723e0fd3e1a1839a447641a999b1      1
    105 0x8092f951225b060dac717c3e0b2aec98c9efd7ee      1
    106 0x832408210f74c5cd8ac6fd8ebc92bb6b8a7834a0      1
    107 0x8754e1ed406a72f8deca5b4c9654ad2db698ba83      1
    108 0x8c04a5777aa418f6b40a33e1984e605067bd2043      1
    109 0x8d50d2eeeed7ea1de60c51ba3f767e48dfbd2320      1
    110 0x8fc4e14c1c2ab50be594208b0e5f1a22056316d3      1
    111 0x90198c48129ac7564933f8bab85669e303ff6b9f      1
    112 0x911c53395562d2947b891423886bff7218410931      1
    113 0x927705d26bd2aacfd0ed7492e8e21a86fecb4d1b      1
    114 0x9478c28d953f0a698bbb28441b7adb378bed125c      1
    115 0x94b08f4daf6b0b1501fe4297280ec99412f66030      1
    116 0x951360e606587ab33ebcad952593b2fc99d68c3a      1
    117 0x95bd9e6272a6967dd867d37ae1395f4810ca6398      1
    118 0x970bb85b997af2369b4be78e703e552d4d0d7158      1
    119 0x9744bd33f16340577b77a717872961f74e24d2dc      1
    120 0x994b2b32530c95cd4bdac0ec0e0b41b187385b94      1
    121 0x999eaef521b7425aca4c764cfb0ad0dc6e336e20      1
    122 0x99d81318e6dc3933ff30325aa2283474981c4e7d      1
    123 0x9d8fef7b595c9f1ea30d6fb4d85b655011c63e47      1
    124 0x9f35af4727fb91114563d8a8f779a792cb557f3f      1
    125 0xa261634e88daf2630946ee07c643bbd4de640eca      1
    126 0xa27c8e38a5b577d4c1aadd0426363ca83255c6d7      1
    127 0xa2b619163985c363a25e210a91023dda4e08d667      1
    128 0xa30d095a87068b90063d8b2c114bb553579d39e7      1
    129 0xa7695409c5fef39a8367759a279386302a683b9a      1
    130 0xab4273e7137f9531fcb47d558d9bab0e726e6937      1
    131 0xab94f597b94f45680b07997c394535d7ebc4a297      1
    132 0xadb3b6543d2cc69d2d62b617555eb89d4db4af0d      1
    133 0xaff42573bc515b878513e945e246d0f1b8ff01cc      1
    134 0xb11b38a7c3fdb1a151050dd13e62ec63c611ab47      1
    135 0xb18fd1c3505f1305df7881ae353429796e6c5c7f      1
    136 0xb4d7264fd36ea63210d4647a174feb04956eca4e      1
    137 0xb4e14ef794585b51afb4cf4c77d8dbae66f096c9      1
    138 0xb98d10d9f6d07ba283bfd21b2dfec050f9ae282a      1
    139 0xbaea3cf94abd0d6e0f029ef5b0e54e9424a72985      1
    140 0xbb3be72a5f745a5a055dafa72b13a6e04fc5859e      1
    141 0xbbbc24fe3538ed5e6bb9b9c4fa0442c4c446720f      1
    142 0xbc8cc8b1fee0ed52dea5acdfd0002c2a679ff13a      1
    143 0xbda74636999a025cbd9f7b16edd65323e95b1bb3      1
    144 0xbdbd64b55aaf5bdab528b0725f33a19a728b11d7      1
    145 0xbe2f803bfbcffbcd77aae4e9104406abfeda497a      1
    146 0xc18d4c2fee42accfe7bfdce98bfacbabf76242e9      1
    147 0xc33da4e7e5216130d90af5a4292db6c46ada41cc      1
    148 0xc5c9cfe3e5771b3e5a57a62c2141c58f2a3749c8      1
    149 0xc9ecb2ae80e7ddc67f3fb71b5a8c1b54b6836127      1
    150 0xca536f1a439eadb69643b850a198939bae77ee4e      1
    151 0xcd33e2aa93223ecc6cc816d836dce0f3e635732f      1
    152 0xce013eb11d874c8fa60ec9007626e154f33ef2c1      1
    153 0xcfceafde40ce4ae35cc62657eb742103b8e925fb      1
    154 0xd2498b4dc8402789736f7c94caf969ea65badfa2      1
    155 0xd258ab439a3e8d921149d1ac882e181ba82f41e7      1
    156 0xd841d89ff8775966750eae25547757751460e1eb      1
    157 0xda9e817a9d8a41b18252ebd0d77b38739ef8df23      1
    158 0xdab68971008b8565e363cee4dfd217420910a899      1
    159 0xdf44e1b315ad475bb10919f1312d4324852ad66f      1
    160 0xdff3c57d16c68d04d3961a66710cbb37624f3fd9      1
    161 0xe3a6f6025d361b75ac0f95b5e3820f12b357deee      1
    162 0xe8fb02309e4b2d7a7c82ebb2d9fc9472b90fd942      1
    163 0xe9b9bd9192c73ebda2f6c6f2b6b26d1425d0170d      1
    164 0xeaddbae2651e21b4ae261ffbfcc81970854c4ef6      1
    165 0xec7100abdbcf922f975148c6516bc95696ca0ef6      1
    166 0xed155c1d726d24a9193d3c3781a53610fb87caf9      1
    167 0xedda20e0cc11fae46747868fcea07e60c9b25379      1
    168 0xf013e5f697e84c6831220a21a96556242ee9ad86      1
    169 0xf17d31a3c87d4dcbc4eeecf9e5fc1822e4a99cd4      1
    170 0xf31781b4811b737e66662997679fb07dacf63355      1
    171 0xf76a3c1965b15fa7b109bc7ce18d64246dae6d72      1
    172 0xf8290ab401fd6bc9d7ad93b896c59b04e89ab354      1
    173 0xfa2238eeb4e2e5612853bf29f925a36f971f9010      1
    174 0xfa4e98a3973ac6d51918e69d49f749219518c5a5      1
    175 0xfb1bb9d4a4c50457d339a686212a0716ea0adc80      1
    176 0xfb2ec6543d97808d8a00fca199c39022b0b3368f      1

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
