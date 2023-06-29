
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:8103        Length:8103        Min.   :1   Length:8103       
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:8103       
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 17583269 # https://etherscan.io/block/17583269
block_hash <- "0x71b7fe8fffbc27b3e48276aeb58e10224487736f72e13829e391d0ac0f0d9e16"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4619 

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

allow_artist_phase1    <- pick(snapshot, contracts=c("WildMustangs","Plates","SuperRare","GMSunshine","EditionsbyCardelucci","33CuratedDrop"), address_remove=address_remove,address_max=1)

allow_memesRandom1_phase1 <- pick(snapshot, contracts=c("memes1"), address_remove=address_remove,address_pick=75,address_max=1)
allow_memesRandom2_phase1 <- pick(snapshot, contracts=c("memes2"), address_remove=address_remove,address_subtract=allow_memesRandom1_phase1,address_pick=75,address_max=1)
allow_memesRandom3_phase1 <- pick(snapshot, contracts=c("memes3"), address_remove=address_remove,address_subtract = c(allow_memesRandom1_phase1, allow_memesRandom2_phase1),address_pick=75,address_max=1)
allow_memesRandom4_phase1 <- pick(snapshot, contracts=c("memes4"), address_remove=address_remove,address_subtract = c(allow_memesRandom1_phase1, allow_memesRandom2_phase1, allow_memesRandom3_phase1),address_pick=75,address_max=1)
allow_gradient_phase1     <- pick(snapshot, contracts=c("gradient"), address_max=1)


allow_raw                 <- pick(snapshot, contracts=c("raw"), address_remove=address_remove,address_max=1)
allow_singles             <- pick(snapshot, contracts=c("65291/1s"), address_remove=address_remove,address_max=1)
```

## Allow Artist Phase 1

``` r
c(allow_artist_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_artist_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 100 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x0040122ed305e0b16dfc9fd8e0e9a74a3cdd5509      1
      2 0x03ee832367e29a5cd001f65093283eabb5382b62      1
      3 0x0be3825a0a92b206d1ca1e9e60e793f31fa11eb3      1
      4 0x0e0e34094e42f47fe25da3cc1b441faf15f8551a      1
      5 0x127db3b629eca11a2167fae194438143b57afac3      1
      6 0x1430997022e8ccf845af34596ef37f4754983f85      1
      7 0x15ab7dd2261026db55623d0da3946a3e022be19f      1
      8 0x1a962d6a95cea8060b1e9f55c5bc6622315c0108      1
      9 0x1b672e08c3cc6486b1c01d1114d5ced3c5682882      1
     10 0x2177da04f9479496c2292d6344d306aa49beb34a      1
     11 0x2692cf16c43fa816ccd9a3f9f315f50cb717357d      1
     12 0x29cc37bb515745a75eb9ec13d2db1b7bc69447fb      1
     13 0x2dba88cb3b435f99a3e58b6e0fe450e8f1a3f20f      1
     14 0x2f44fb58135ae5d3793803c73d471c7cde4bb774      1
     15 0x3327f53140683d19e2baaeb65054133b9b33c53d      1
     16 0x3564ce014967c81e25f6349124673cf60b7ac572      1
     17 0x36ed861a319278e5b6c39028884b0ca93df105d0      1
     18 0x385dc7d2ba88318c3a22de8cbe7b7b0ff54df842      1
     19 0x38baebb524bd1ce081e5c9b75739bce6e091e95f      1
     20 0x3c7112aae3ef1e7bb238f462491d7f7b702395fd      1
     21 0x3c8cd5597ac98cb0871db580d3c9a86a384b9106      1
     22 0x3cd378c9b1cb5f147ebf1b2c2564118946ae4ba1      1
     23 0x400e70ad39c77aeadc1dccb23db7a3b6ea7b7716      1
     24 0x4f678ee0d564802099eb263df505c15735da863d      1
     25 0x51fac3a2cecffe84f5d04c0cce96744e610b793d      1
     26 0x54f798ca0bd83f1d9827a9f07a4c4e1b8d5b1bd7      1
     27 0x5bc0aebdbab698e12fd33a2e133e6858fe6cdd76      1
     28 0x62c5ee7bcc0e336bc58fc2946bb9671ab2878f7c      1
     29 0x64a8dda827deb895a36667d01063f2ab840e46b2      1
     30 0x6901a4c2f80f216eea22b284f5a977fd9f66288a      1
     31 0x6902fc06bad55e3a9412a35e0e57ca12d78c5a9c      1
     32 0x6903f144a9e6da18313c51df9e4b83635debbc15      1
     33 0x69420f0cde588d9d35df060085ca200afdd80a5c      1
     34 0x696969917d14af0eccd470b0797a447087aba094      1
     35 0x6aaebe892dd0500687ef92706bb6eff88f9c52ae      1
     36 0x6b2aa5fc4508ec023a8e355959eaccef5e3a250b      1
     37 0x753f8dab2ce1e6ec33e2967bcdd4974f60025319      1
     38 0x757a21fbd39cccaff681e1930e273b9add0008db      1
     39 0x7771320992d8acf81092052520f15c8f66f85096      1
     40 0x792b37c2244ecc0bc18ad193498f17b27efae7d9      1
     41 0x7bcfb903b06e74751ad9ba657f7d5389c40726fb      1
     42 0x7c5504688eb4f81ed558550066dc218ffee1f6c8      1
     43 0x7d70b5b638a2af9816202880535c8fe8f1fb9772      1
     44 0x7ef865963d3a005670b8f8df6aed23e456fa75e0      1
     45 0x8030e495ea55d8ec8ae1e10bf7498bf7f0eac1aa      1
     46 0x822e60d79ecfd2d4525cb6192b6e9732d8b94bb6      1
     47 0x860feef3a90b30d4c96b265a425ebe4f629f2de6      1
     48 0x86ff14cf53924f77501726f82a3057ac22f7de15      1
     49 0x89073adc0d25a3d6f0eb799bc4d6952917ea0d48      1
     50 0x8a0a569b2937ecc360a686386e34df83cd2348a3      1
     51 0x8a7942799c2a605fb22d06cbce24fe70f18aa55b      1
     52 0x8c9decac37d6ad7026cbd1e704d5bb1096edb8c3      1
     53 0x946b37b64d6d1108f2a3b2a9009475472322fb3d      1
     54 0x952c6b14405566fa50434d23af7075b9e0833676      1
     55 0x9b7c0814eb341e6a4ca673148e8b577b13e70787      1
     56 0x9c802062173b02a486086bf693186353334aedc6      1
     57 0xa355065597f1c213160e664b65beda6cabf07bb0      1
     58 0xa7704af087f4a94062a6ddbfba29769c96fb5bff      1
     59 0xa7bce13c268c132eafa61633827b872a248cb352      1
     60 0xad7c157db676b8f3c5f4efddabf91becc2f43891      1
     61 0xb2b1e76265ce0a9eca7fef50d621f267108118e8      1
     62 0xb2dd242312ba03807d06f51298b2830855f3837d      1
     63 0xb7ace9d178e5ff2321eff5f9b73899fede036569      1
     64 0xbaea3cf94abd0d6e0f029ef5b0e54e9424a72985      1
     65 0xbbc973e2afc9d7f912d745423007cd3d98c94460      1
     66 0xbc72fd79d7b4f486a00504fd450e072c13cc5b4d      1
     67 0xc01c99b2543e28eff240e90384a1fd757a484927      1
     68 0xc6893eeb690596e44f6c8668990a5cd7b8b1cedb      1
     69 0xc6c5ee2c54c79695ebef26f3171e5b96ed74578d      1
     70 0xc72f40a397453051349f73cf2e2a04fac06e37a3      1
     71 0xcbc37d1120d7fa7013bcf35582cb12ea17b2bd4e      1
     72 0xcd712edf000b0edc8ba3ee86a8ddee35e20919da      1
     73 0xce1b5af3e3cab6efe401e146644612784302fc4b      1
     74 0xd0ec43248df125fbb02da77934ff8b3e675f198f      1
     75 0xd64b4f50c8d118f6dd33f16846345616ab80e16a      1
     76 0xd669155d39e22ee851f4558a3230ef7de057426c      1
     77 0xd7342ea20a5afbf24352b5ca61e09844167914cb      1
     78 0xdce8b8c7260afd32c258b9feff832234ac589bcc      1
     79 0xdd1e6fee5428f89391963adc501d6a5fb9d9997e      1
     80 0xdead7c4a317c2c5b90ad3d3f4fbde992df5222b4      1
     81 0xdefaced6a03f09d084b4ce120e3f0f12a9ec7a0d      1
     82 0xdf067d1c62b6a94e45ced3f01ebc94ae304cb045      1
     83 0xe38064adbd04738cc570aeec49ded154df00a153      1
     84 0xe4c6c46645988bbafd8ef6b4d1b60d969cc857c3      1
     85 0xe4dda14480e066223a9c1e01e2fe086ba4fc71d2      1
     86 0xe6a1da162fa649480839ee96352908fcc3d6e9b7      1
     87 0xe7a8a504a82f0a829656261a54d0443f85e19604      1
     88 0xe96ba1a10f72b58934b9ac0e3fb471d2ba65b757      1
     89 0xed83f0bceef992fa58c8bc8a27269eedb3574922      1
     90 0xee0d982d26d7bf4f52a62a06d22eb7c00576cbb6      1
     91 0xeeec0dc13dc88d302d3f381e078d37e1880555ce      1
     92 0xef8e27bad0f2eee4e691e5b1eaab3c019e369557      1
     93 0xf1a1ef114e9416b2f49a8a84fda34af165a97686      1
     94 0xf2dca9d0652c43784522397c11b19694c73074a6      1
     95 0xf3bf7cf9e6b96e754f8d0d927f2162683b278322      1
     96 0xf813e0de2293b487f75caca607290f3161944f3c      1
     97 0xf9d681c3b81aa1d0ecb3fdb4c69ca57714eb63f4      1
     98 0xfad7819967a6ef122a123f92416616a9a56eb5f0      1
     99 0xfaded1038e430c866ae93d6b61da8edd4b28f067      1
    100 0xfb3df26810b5f41172731ee406c2ad577d5c8a5a      1

## Allow Random1 Memes Phase 1

``` r
c(allow_memesRandom1_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_random1memes_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 75 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x04df8d02f912d34fef12a1b0488ee56fd6f7416c      1
     2 0x0a98f97e89743406a8611e1b4219a073b60ffed3      1
     3 0x0acabdaceab63c08bc5aea63d7e07da7c2e42959      1
     4 0x0d6b4f304b91a3b67b1db34776ef7e95effbc729      1
     5 0x111818a51c4177e8980566beea68fe334be7b76a      1
     6 0x11a22b262e505d355f975e1e48a365b5d4811ae0      1
     7 0x128ad42b82c752c5b4c7f679231d8956c98038eb      1
     8 0x14abeea8db07c46b37a79bfa856d4c2da6d3e6df      1
     9 0x167a4e6066d6c96d5c012006f5cffc9f606131ec      1
    10 0x16f3d833bb91aebb5066884501242d8b3c3b5e61      1
    11 0x2177da04f9479496c2292d6344d306aa49beb34a      1
    12 0x2a0a412e0a0d436cca7ddba177c4dd9f29801062      1
    13 0x2be96c05fb7e81b020c42086b69d21bbf427b53a      1
    14 0x2ca75a892b59bc7cf863ba261e49ab40a4844ee3      1
    15 0x343e85ac3009ac01538d4251900ce1a2b7d7ffec      1
    16 0x35d7dd9230a1f079c907b53e7c5822b34e197a1d      1
    17 0x37ea6c993c5f42e1fc3455d5781e4a580760970a      1
    18 0x431181dae361813567f35ee2abac73291820fcc9      1
    19 0x45133db3eadd8718f5c3a868ec2f7148e460dcfd      1
    20 0x4b6c1d196e06446eb0534326bbfb02cc3d073a2b      1
    21 0x4d7e6948fb5ed2d92897de0605cd642eff69fbf4      1
    22 0x4f492b2a3ed9bf0c65d508dab4f7c5f5c04ca6c3      1
    23 0x506452ab0dacf04db0ab1544b6be4019651ada90      1
    24 0x51a2d3572577b6c47186c82165ab8a61e046dc83      1
    25 0x62ba9de7d81b96d697fc2f26b5eb647f184f9b2e      1
    26 0x692d6cf19e0c082185e20ff5ea6bb267e7aeb278      1
    27 0x69c8c2923005d26eaeea9500d7602eff8c81c848      1
    28 0x6e5632d334b861dfe4312c7ba34e175b60be0b5a      1
    29 0x6fdd8c800cbb81790cd48a20f90ce7e725f89c27      1
    30 0x774a34da2ee2e2d242180819f1ee88783215f7b9      1
    31 0x7ae3b0627dac32d3ee16c204ef791e37067b8460      1
    32 0x7c741ed01ee259feba4b7f9cac114f48bcafacf3      1
    33 0x843708d85621273f3bbc643b348da3a60d5b0334      1
    34 0x8b4567fa8c4715c27a682215a031033a8b544206      1
    35 0x8f160eb907a80517a3fa6d22f7cf20f552696a44      1
    36 0x8ffa8d00db5df56a20c30b737c5efdaabe140df4      1
    37 0x9233389b9afa0a6e3bb403f9d211c23a3b290d69      1
    38 0x95999c47c3e32a337ef108d657675c2757a606ed      1
    39 0x98004498c85c4b1997b3f669475a038bbcec2160      1
    40 0x9e1f7b007511036b5cbe5df6d39550bdd2e8bc99      1
    41 0x9e640badecb7c628c6188b74488823e879f42a1a      1
    42 0x9f35af4727fb91114563d8a8f779a792cb557f3f      1
    43 0xa04f4a4b7306cb72f30828834db01699362a4989      1
    44 0xa0f1de4882a5cd1851989d9a1e9bbe9b4604e9a9      1
    45 0xa5214719feb1d07b66292a5d4d0a2712bd048435      1
    46 0xa5ce27ca5e31b1de92db2d2bc109b3e23cf1d4c4      1
    47 0xab3ba2d668215acd23f7da923683a5ed88bad625      1
    48 0xafb4fccf58bfc06ebc8ccd387b68140cca05f4c5      1
    49 0xb8937891f08af9854a5ae7a5ec0cbaf4e68acd4c      1
    50 0xbd3a5d12af9fd82fb927fd26216e4dc2722d2337      1
    51 0xc18d4c2fee42accfe7bfdce98bfacbabf76242e9      1
    52 0xc40d07925d1ac5500e9dfc4e0a05eb24a75d195e      1
    53 0xc5a2c3a524be0792519655d4093b44b988db4f68      1
    54 0xc8cb180415b83f3f9a61691d246fe39a74c6a41e      1
    55 0xcd30b6de9dadb0a1598eb12a4ccf34b98c98c1df      1
    56 0xd193a7d7b231c204b76b9d638768ea602de515f6      1
    57 0xd1afbe4da2a5adc6faf30fcbea9e4eea0ba8c70a      1
    58 0xd3364ea1c742cc2d396ac65e1723b8d608a939bb      1
    59 0xd36590461162795ee33099b2076a0d4e017ae17c      1
    60 0xd3b37c6567e2702aa727cff4ce5939b4a07a2fde      1
    61 0xd5ec003289265705727b622f1700fe814e54ca67      1
    62 0xd6b2735e290ba3726d25c9762098cd26a16f023f      1
    63 0xd7e32b8a326ffd9e784a1ee1eea37684a7512171      1
    64 0xdc78107155918e230246439e4159fea4c477eae9      1
    65 0xdd5cecf2835d465d490f79ad9773a848805a3219      1
    66 0xdffd6968900b3f4f667b69c5f9df65d9d7796a1c      1
    67 0xe8a05c7f4e9c1aa060cf941dbb50381f342d7d43      1
    68 0xe96ba1a10f72b58934b9ac0e3fb471d2ba65b757      1
    69 0xee3aa3fa5b780bc73da73a82d47fa2d8cdc1a647      1
    70 0xeecc9f94c674161e84ab0c60c020432ea8f88bc0      1
    71 0xf054274dd74987395d28136e53f39ef4f7b19994      1
    72 0xf1e1c701b49b1dc2405a9e8ef40f9f88802b80fa      1
    73 0xf8be957f65e67fb0342992a51c30290d5758f880      1
    74 0xfa977a0125c16a658a59ca1e59c7c48d58142226      1
    75 0xffe6832850476eb6d5ff184d747ed84f1b686aa9      1

## Allow Random2 Memes Phase 1

``` r
c(allow_memesRandom2_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_random2memes_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 75 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x03c5e3ff9514031f910c9bc9ebbc7839e02b23d8      1
     2 0x0a7669015fef82f5136c9a2e44e1ecbd2a5aec19      1
     3 0x0e2f031a1142ab3919685cf82aa764d9c5c0ea86      1
     4 0x0f0eae91990140c560d4156db4f00c854dc8f09e      1
     5 0x18595f049a9c5c2019efda113009b0ec1c89ceaa      1
     6 0x1af369bc1069dd286ff59cd69553550c07e0dd05      1
     7 0x1dcfdcb24456a7dba5342479cff0f9ff30145fb8      1
     8 0x2917e947fb1925abe184d4062520a11bcf2a5b36      1
     9 0x2fb24b1021e51fa23db91a9b995000181fda0963      1
    10 0x3927e502c865a2c873a735775e224930eadfd2e3      1
    11 0x3a10fd1387555cd75db8142393fbaba98efe28d4      1
    12 0x3bc161e3a5604165f3165ed8aaaf0d490e559324      1
    13 0x3d20c3f372207074df1a0ab5272c7168ce23d178      1
    14 0x4705bc2775cee0fe266dc2a376010c0eb1bcb474      1
    15 0x487869b7557f0fdecbc5141da1d7c48c15c5b8eb      1
    16 0x4ab5189a020ccd88f05c36de69e4d868d29c75a9      1
    17 0x52349a2c4ea4e4b94ada3d75c9d3a318c024706f      1
    18 0x54669ca58bf3a59caea8ae5135db491e4738f65a      1
    19 0x571493b6bb0b99dda41e2da1774b8c9b5bc302af      1
    20 0x57bd982d577660ab22d0a65d2c0a32e482112348      1
    21 0x5bd832dc1a5ac61f687b87c4199b844819a4d8ed      1
    22 0x5d25087405105bab12624c73488ec186066a6376      1
    23 0x614a62584658d555cc28b3aabdb691e85e002c8b      1
    24 0x67c6083d540696c6aa1cf31101d78780e0d6bb47      1
    25 0x6e1510ed41ba743344e9ba80646194351cca0c7f      1
    26 0x733983a4c7e778ca9f57cac6ee361c4a9b24e4b1      1
    27 0x753c892626c4fa09d1aedc0e9b52bda97a4afa00      1
    28 0x765718a8e4a5562be34b122af259ce5a71372e12      1
    29 0x78fc8aa7bb4ee047257a245ca590fac4ae4aa97b      1
    30 0x7b86a910a6c797316dda30b78098e331e08b9f34      1
    31 0x7dbba7f0551aef5894fd6ee331b95dbb4349f5d4      1
    32 0x8586c32dbe1c986bffb8f1ed6c3e8c6489f4fa3c      1
    33 0x90af376346ca97d4e9d1e510e82543ef99b56a28      1
    34 0x94f052ca65a34d7f581bba95a749b1cf52689dd5      1
    35 0x953b0f8afc70e0fca9e2e43b0a4914be002c4e94      1
    36 0x982d3c5223f6b5794fccb3208eb164d042cf2526      1
    37 0x9ad53944b52e271f1114028b918202cfaaec0a3d      1
    38 0x9b7c0814eb341e6a4ca673148e8b577b13e70787      1
    39 0x9e5198418081fe00026ca0ddedbfd3915ab98298      1
    40 0x9f4fe19fed4a008a270840fe95660f25df36c734      1
    41 0xa225158ea124f311cd0d50cfeaf3407b9412b077      1
    42 0xa33e5e1ccf57c0caf78ae399061f270dd24ffcdf      1
    43 0xa6d4758ef49d2ae8e65e1a02216cb9744aee6b23      1
    44 0xa94ba6f81ede0d81a5252e71916b5c5567ad6a01      1
    45 0xaa1d3f5d45333e40467e989d472effac4da00da9      1
    46 0xac01ad9a3e3c3448a5a0e6fc798def034a01a67c      1
    47 0xb335326c7f2cd2e4eb09ce3d1745c92f66497b7e      1
    48 0xb6c189179b204d14fcc0b608bc97243b389b0030      1
    49 0xbb59498b19783e5d81f72ad07acdac619b6808e2      1
    50 0xbecf64aab1c6813a526ce63483cb8cadb2988c07      1
    51 0xc0ced4439c1f0871f16a46e13fbe41fbf61ba265      1
    52 0xc58326c7020f26345f4568cc09daddf019a8e6d4      1
    53 0xca339bd4739227f71b646f8b23ca098c86f6c3a5      1
    54 0xcb85b581caa71949784d2e826cf9df391c244b33      1
    55 0xcea266acb92d5603dc53b5d6bc6e568dcde0d311      1
    56 0xceae4ca7d082856d140f3672780029d6e90c3dcd      1
    57 0xd7342ea20a5afbf24352b5ca61e09844167914cb      1
    58 0xd7d16a716f6dfcef1e9553bf12136463f6414f1a      1
    59 0xd7d941ff2890bda98f40a5dda0593d239a603f09      1
    60 0xdcefc9ff1e47e458e56959154c1cdcf56003d30b      1
    61 0xdebd22d7f63648dfb69bb6e2a4c88264d578c0a4      1
    62 0xe4c8335467ce3b1854dd828f14bbb33aa4771818      1
    63 0xe7079eec020ddfc3f1c0abe1d946c55e6ed30eb3      1
    64 0xea9f3a983d965e582c34eb852c18babac52050d8      1
    65 0xed1e25178d7a6438c356d3459fc3efa9458ad52b      1
    66 0xee05f658e18eb04d250f829b1920c2fbf6907e27      1
    67 0xef04cb0003ca57dc7bb563d5ea3248241b145bdb      1
    68 0xefb3da5189a6169a61176d1357579e135a1d1187      1
    69 0xf161ff39e19f605b2115afaeccbb3a112bbe4004      1
    70 0xf54913955c6035a12465e24ba2f162df2a026d9a      1
    71 0xf74b25d3f6616e51b2d8be6c8cec54b045229655      1
    72 0xfc0ac71b8ca9b219b3c74625755f62202d19ad39      1
    73 0xfcdf3c2b2b6ba00e6db59c70c424b27970ff3974      1
    74 0xfe8312a959e031c7d4cbe3f9cdd3ec8726d0d80e      1
    75 0xff4f0dbc9ee7583c0c61c7333a5daf8d0ab4aeb3      1

## Allow Random3 Memes Phase 1

``` r
c(allow_memesRandom3_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_random3memes_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 75 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x017ffb7c9e5aff47d9569265d7c78b2494099782      1
     2 0x019d370ada720786cda2c4f78f0f4657af2ff92e      1
     3 0x05cda24eeefd1f24f18dac398f5a612674d3ca5e      1
     4 0x0c664c03eebcecb6c21e3b3bc77c9dffed5bd694      1
     5 0x0ce390f18af702cca546297845a4a51d102123cf      1
     6 0x0d5668339c3c17aa7f06ba7fe2667536cf15d748      1
     7 0x0ea9f4f7c371e7044d187b3da7850ccaa6370a0c      1
     8 0x0f3c76ade30adf61b64d01154911308491784dfe      1
     9 0x17726f58ba7350a33d08d1bbad623e155fa2daa0      1
    10 0x183abe67478eb7e87c96ca28e2f63dec53f22e3a      1
    11 0x195f2cd867c1c02ae6e61a14ec53f5f01b440edc      1
    12 0x1cb89e486db5774ba084f683796286848df489d0      1
    13 0x1e31c76b78ff00e3cb64d41d5844626d991ab9e8      1
    14 0x1e7ba17cc3f031a0c5795739317f6b2022ca39f5      1
    15 0x2380ca49ed8e933c97905977763693e5cf8770f4      1
    16 0x2595aaffb353e812bb5cbb63f7f62e36fe528f02      1
    17 0x27e037e0461e3d5587de786ea23441d83772353d      1
    18 0x2924196a2ec71ea4ae1b1357381eccdcee6c18f2      1
    19 0x3025430ae8a81cd13e3d0969b1093f8d82bbbd7d      1
    20 0x34978faf3a9f469da7248d1365ddf69ac099588c      1
    21 0x36eebc48da111b14b010a97abecf8bb68d10095c      1
    22 0x3877b4c39476e3af04cb1ff8003db09f39f068f9      1
    23 0x3de4b60cb19faebf58efea6d4cd99fb5295cf95c      1
    24 0x4470e0f5a0a3969cb0d5aba71ad4e6c759dfc824      1
    25 0x46e6aa05e0867d5f0feb749e81e005f5567ab317      1
    26 0x47d278101ee9335c3a3baa14fe184f757229a7b8      1
    27 0x48238faa9bd6d56fb44a32dac6185f04eabf2117      1
    28 0x4ac69caa7bc279fec93db0d10793eb8516c7a9d1      1
    29 0x4c3d85e7fc0c91215eb6a4186592a41d975d2a4f      1
    30 0x4e61548d94c8649ebfc2f5f54d2272fcbc687bf2      1
    31 0x541db1ed2628f2f4897417d06181af6a179e90d0      1
    32 0x55d4a9e5cde6fbe40991e7570e01899891f6b0d2      1
    33 0x575f6540c16a72696c14a17fa64f049992d661ab      1
    34 0x593cadf136c68c720d446461c1bfee600647c6b8      1
    35 0x5a3a1461310884df894b7e973305f690fc5779d0      1
    36 0x5c076659e81dd48a5d73c81168fe739c2ab89087      1
    37 0x5dafb2d1f8b20501da62ba00c0c283293aa0b70c      1
    38 0x6215e708a7b61f5ed49642f692f28ba09b807725      1
    39 0x632734882ed0127fbdf2666478df42aa916bdc84      1
    40 0x6576a26763cefa8f37005c067f7902960dec6b8e      1
    41 0x6b4b2cbb69b9ca07328a40e7ffff3870f627c8fc      1
    42 0x78143238df750e9d9f1082e12ed56f2bfa332d65      1
    43 0x7a71dc357646a8149a920dbd26f66f07769f01d9      1
    44 0x7f94e30381aa6657c45833ec7fce2e493c1888ef      1
    45 0x879a8a1868d620584afa0c9c8807628d553c6bbe      1
    46 0x8b17e8b3f8315e73986f0c8370cea4f3a974a532      1
    47 0x95d41776812c6ca222dd5fdea56c065b55ff7655      1
    48 0xa1cecd5e4f6683189c3d4f832a16600c20e5b42a      1
    49 0xa2917120c698fb5f2a03e3fd3524bda85a3eaef6      1
    50 0xa44b68fc590a2fd1cf48f2689a8aa2c8b62d2261      1
    51 0xa8450d71b98ca4955c63519ef593ba886c5b2b4f      1
    52 0xa96d4549729c2a826237d91b3a3700cad7dfec4a      1
    53 0xaae8c50d76c76fb8947c9a203103d28b55862977      1
    54 0xafccebed934565586a749c66ff352b0fc871042b      1
    55 0xb8efa945cf7f35854afb80e9ac05f5c1dc172fb3      1
    56 0xbaea3cf94abd0d6e0f029ef5b0e54e9424a72985      1
    57 0xbb8fafa8a629c4dce022d95e098ccccee1acd942      1
    58 0xbc1eb4359ab755af079f6ef77e3faac465e53eda      1
    59 0xbeddf3e43015f5221f5d1ac0fd0d2ab3352d2f7b      1
    60 0xc06a00899ebd1481f243ac4ec92a276c1ff26e8a      1
    61 0xc0bcbc0a91cd42e4808faf190e0ad6e9a6c027cc      1
    62 0xc31ba2d3275a397c43b1a4ab707cd79adf310233      1
    63 0xc6c3752bdab737b8cfa174e6ecfa620ec0cc162e      1
    64 0xc97cfd2c3a3e61316e931b784bde21e61ce15b82      1
    65 0xce56a746edaf84e65aa6a2bbb45b2328f6a99935      1
    66 0xcf7c7758512aaf43979b64812cb79bd3f60f4413      1
    67 0xd61bee56435dc7eeca895bae90fc8b9c7fe709eb      1
    68 0xd70150a7a5a42d4cd25974dae077a79a1547fcf2      1
    69 0xe061ea94f07de987a55a49067b0d7ca3feaffbc7      1
    70 0xe39bd9e3af2054dd193b0d0217cfbd460f104902      1
    71 0xe560646ef7a69400974d85e818bc0e054bde65c1      1
    72 0xf6ec5b0d097178a0acf493afdcda2a54545ab0f3      1
    73 0xf81c1d67b9c1f80eb30c610e2a78c2a1b1fb013c      1
    74 0xfa3dabf38f872f50c62c3d39920d173d215806ec      1
    75 0xfc0c476530d9742cb116027c04559d0dc26bbd12      1

## Allow Random4 Memes Phase 1

``` r
c(allow_memesRandom4_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_random4memes_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 75 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x01a319d74bdb2ebd4381d9baa3dceead665fa613      1
     2 0x04434c2cf7d7d90255348079e840b4d50a62588e      1
     3 0x0763e484268042be6e5743c731a7adf47a48ec19      1
     4 0x09276485fb976693e79ed9cf8defa435deb3be1b      1
     5 0x096dd05d5e0abc910d41e080562e7f22b30b1864      1
     6 0x11dcf0498e9ebca2f8f3ca01a9cbbd9b8fb23b91      1
     7 0x168bbc7185fdff0e54fee49720ddc6e7527a3099      1
     8 0x175366fd3e2cd57bc82eff05c9295a29688e6ceb      1
     9 0x17e7202da7a6411c27db0b8ec0d0839bec645202      1
    10 0x1aa760cf97f1389e03a2c8b4ae7aa1d748d1b666      1
    11 0x1cc848568daabc2541f58c6d2f3344972ac1f2cd      1
    12 0x1e1500c0905fdaef033dc4c1fcaaba2a46491f4e      1
    13 0x22fc06a488b236753a89caeed39b58046b153069      1
    14 0x24141a358980b41084a487bb39c5e0a95b6e6559      1
    15 0x2ca212af7e65631d46a1cf406427f257d2976a71      1
    16 0x2db0ac6cb953573153c2740502f60e248e2aefb9      1
    17 0x3245d30c08bb3629d5848b1a440ab8b040eab291      1
    18 0x3775669465352353f599f73586494747463f851b      1
    19 0x4069d163d9ff94f4e7025f28ac3f3c5df3f7a149      1
    20 0x4313ca73355b0b098a78d406b42a0d86296bf0f4      1
    21 0x4c14c82fb34796ffd8a24b0ce6827ee8bc640f1e      1
    22 0x548418b6265658d5f1f07d37ceed4cfedb4e3202      1
    23 0x5573f84b3e56ae210add18d898036038470695cf      1
    24 0x585b7059e89da5eedb4d533c31961bf4708afc9e      1
    25 0x5f5b5a6b4661c2a4280984b3921ffd3d58bd42f1      1
    26 0x68a7ac13477aad590982293feeeb786a00276cf2      1
    27 0x6e5bb7c3264941b23f7dd6caef868cecf4ea48dd      1
    28 0x702782ec4d986dd1d821d71452cd6d10ea5e0ea0      1
    29 0x73ffb5a957ee669930c70466ffec4be0898fbcfa      1
    30 0x78a9e315c6d6fe8ea0ee1cc40ead04477f02bed1      1
    31 0x78fb3d569650ea743fb7876312cb5ff7505dd602      1
    32 0x797a47774c061c45ee94232217bfceae6ac937c8      1
    33 0x7af061a6ec9616135231cb8de2ed2a0a4140e90b      1
    34 0x7c344832ec4142108f6b6bc49ce884281e6a19a3      1
    35 0x862eb245e19172dc3da2e16248ade9deba757004      1
    36 0x90198c48129ac7564933f8bab85669e303ff6b9f      1
    37 0x91aca13b42ac160c9e79a3cdcbb42cf68c1f15b4      1
    38 0x93757e12bfb1c0d4d2e319b621e27cb499bd35be      1
    39 0x971995d5961a3fda21d5cce39a9c6615e138cb02      1
    40 0x98025f7776884d38093508e31f4e3119e2979cad      1
    41 0x99b5a0c718e1497564c94c7869d7e5b39866cd68      1
    42 0x9bd4b05b6f3cd3778012f72c16c42fd0490cfb3e      1
    43 0xa3fec25e8af3ce136ee58e0a78e0528c56fd840e      1
    44 0xa5ff2bbc712fc114c29f3900ebb81b7713fe131f      1
    45 0xad9f4655afbb676cf93b324c6d0852498a4f48f0      1
    46 0xade6cb680030f97cf612ff7694b1e4c30ba27735      1
    47 0xb3276a67634206590d3e0f7192b4df34ec184eb9      1
    48 0xb51906278fc209710dd755212fb7ea8d4d423df0      1
    49 0xbd5224f9509c0f4cfcc99296dd72196661fa8d40      1
    50 0xbda24920e435105f9c65637eb79a6408dfb4583b      1
    51 0xbe1ad586f9461ff4e1757f939b8cc635a43fa63c      1
    52 0xc18120b4b22c78ea55f8a7acbd7d7082d3c73a2b      1
    53 0xc611376086b5152a78070d89290d0a21e8d17c17      1
    54 0xc79f0bc9f9af9ec4b83bdf94858a1d8812321870      1
    55 0xcab81f14a3fc98034a05bab30f8d0e53e978c833      1
    56 0xcb9f87919b6818ccf3e8350e0ee2e13200dc8fe8      1
    57 0xd0b4045c2bbf09f41e460b6ec938db540fb23160      1
    58 0xd2f132930ea3c58a389c6ad3a91ef104f95fe697      1
    59 0xd86aed968fb30957a2a55151bb9e226bdd90bf64      1
    60 0xd8c40a8f23c062837410b348e257d48ed1cf8482      1
    61 0xd9ba239a881f718072e57d9810846c8d705f93a4      1
    62 0xdb1576b1939edfb84c167d6fbc70bdf104634749      1
    63 0xdc7bfc3058654da90692457f68ee3a34e2dd3908      1
    64 0xdd97c0870ecfd9a59fb06a5c89c4a33860eb99e4      1
    65 0xdf6ef836af8a66b7fc4f3f241e3a4363a304c7b8      1
    66 0xdfdf1845befa1dce09cd171b7b79b460e49cf80b      1
    67 0xe1d8a566721622f88b38e4b81e6424dd19822f90      1
    68 0xe496a4f9472a4935ebf4ff78a18ae21e2728ffaf      1
    69 0xe55de13319ed4de98f740ba608ca49d27a420982      1
    70 0xe8f19812dea9a836125bec88ed5ad9c3436dd3fb      1
    71 0xe9d5a03e6b2d7644ae73a07c866966e4fd92a4b3      1
    72 0xea72fbceda4e851c9171a6b57a287ef986344efc      1
    73 0xed7733744888db5a79c82a43096f657b0f28368b      1
    74 0xee0d982d26d7bf4f52a62a06d22eb7c00576cbb6      1
    75 0xf6eb2f6d790b7571751e5803d86de376b248e11d      1

## Allow Gradient Phase 1

``` r
c(allow_gradient_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_gradient_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 79 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0a98f97e89743406a8611e1b4219a073b60ffed3      1
     2 0x0b3e3a27d85996e20b2cdd0e7f1e1208816d05b8      1
     3 0x0ca4dc095608c6b153019f734106581c85f2dcc3      1
     4 0x129c6695dfe7a906bd8fda202d26dfff601f83a4      1
     5 0x1566ae673ae80725bcce901b486c336e6acef465      1
     6 0x16b92abc4e0e0d074e927c4bb46a5c3ee54ffff0      1
     7 0x17eebf2b0c31f6af94abdb551d1dd509ba6e4f0a      1
     8 0x1a9aee029a1b8c69c4404890c85715cc726746c7      1
     9 0x1aa1c4fdb3b3de2b005a363281d0419e10d1b173      1
    10 0x22fbaa2dd20b848084545515fb04b9c846942779      1
    11 0x248458947c120ca057ec028b3fe7e4b3f26fdb3d      1
    12 0x27b1abafb2cb065cfaf41b4b7ee95d27192151b2      1
    13 0x2924196a2ec71ea4ae1b1357381eccdcee6c18f2      1
    14 0x2947cd7346b3a372b357256b8955ebd4b5735987      1
    15 0x2f2242b447a54cb110b8a7991cf8a27054ee6921      1
    16 0x3511ae23ee25e2b97dce883c7d496ff5d18f1dfa      1
    17 0x3cb63b82d778105e43f064ed739b0655f1f0fb87      1
    18 0x3d0a1d20ac580ca8fa35888c92dd0859887f3398      1
    19 0x40d2f4399d23f9afb82d0a6b73055f13208614f9      1
    20 0x43b0bd27d5016e969193412933387b0dd4cf3e0a      1
    21 0x477ea7a022e51b6eb0dcb6d802fb5f0cfc3b4a81      1
    22 0x48464efe55fbcae8ae0c992b306afcf21d4910cf      1
    23 0x520e8d6c5b2dbbe62bf8a40653a314d879b19b86      1
    24 0x52cc2c8db59411310e130b47d478472d9f7e4597      1
    25 0x53006f95def268f88dc1b8216654ab56f3afd052      1
    26 0x575f6540c16a72696c14a17fa64f049992d661ab      1
    27 0x59068075a799594db03c0255eed68e8e121155c8      1
    28 0x60d2bc84a71af61b82d1b4168c9103ff6c4898eb      1
    29 0x615502edd5cf2d59244dd0dd53d3c8ec035e172b      1
    30 0x61d9d9cc8c3203dab7100ea79ced77587201c990      1
    31 0x64f7de90dc79d775703bbec66a1591c7a26a22f0      1
    32 0x665654f2d2a151be2d0f8e3697e6ce780f732af2      1
    33 0x670a840848eba22eb2a090fd65e1ff60c3ba9e5b      1
    34 0x699990a8e7ada9e92c932d6e8fb365024fc74b43      1
    35 0x69e68074f1aada957edd39c5eae0069973343f30      1
    36 0x6f0735bf1e6c69030d6990cdd580345b370eb50a      1
    37 0x6f113b0c8c7266d7514e4986e3d6aaf013b6754f      1
    38 0x729fcbb6e1289c88ce5113ec1c83a48a8e3c9f2f      1
    39 0x73bcb8c5e30bf85806aade7fc36f16c6b80fd3b9      1
    40 0x75005a05bc163b85991b9c9facbcf3155372422e      1
    41 0x78a576d3751e8cc8f30c54c367fe8e5a2cd71228      1
    42 0x7e5ab36876a267560e7191cedbe99ee7bc04bc30      1
    43 0x82139687faae8a29851902783e02e699de0e0846      1
    44 0x85603a042455d2f36f3ec52c1c756344aa70c5ef      1
    45 0x88ccdfe9dd047b4cec4c1102d2d803e2d8bf683e      1
    46 0x896b94f4f27f12369698c302e2049cae86936bbb      1
    47 0x89ef05c30c821c49b67d06e630686c5c3232baab      1
    48 0x8ba68cfe71550efc8988d81d040473709b7f9218      1
    49 0x982d3c5223f6b5794fccb3208eb164d042cf2526      1
    50 0xa56c04347abee42f663eff9bc2d0147b97c8f782      1
    51 0xa743c8c57c425b84cb2ed18c6b9ae3ad21629cb5      1
    52 0xab2056903a7b62bac46f45a3d7a70ac799ca88cb      1
    53 0xae0d16586e5d60d334624c115216a52b9b1a0335      1
    54 0xae72c6a6fad9fa9d82d089e1ebf73b3043855425      1
    55 0xb6cf25f5cf8a1e1727d988facdd47f1dfc492caf      1
    56 0xb735af7ae1a77d1ec764c862c2c09bdbf2b34b27      1
    57 0xba4575ea27041d99e6614ec02318f1e23a623fe2      1
    58 0xbbdd72fcce73c2626719be00259ddffef0d5673d      1
    59 0xbf270918afe2ad16093ddce904fc358ad337cefa      1
    60 0xbf814810b44978de273191fd612aa47f7b69d564      1
    61 0xc02e6b0d0c1a5d8cd26beeba0fe8d76c5d2f19b9      1
    62 0xc2419841dcb9a0f8906d06463ae24e00e1470846      1
    63 0xc6400a5584db71e41b0e5dfbdc769b54b91256cd      1
    64 0xcaa1c396e70384db58dd33be74b26fb119e98c3a      1
    65 0xcabd5f77ca9d48f4ef9793f20de42ad39ce93979      1
    66 0xd007058e9b58e74c33c6bf6fbcd38baab813cbb6      1
    67 0xd1f6e5592361257aef68b96df42aef064080c5cc      1
    68 0xdde27b3ba448e15357183060ae006bb54ebb3d86      1
    69 0xde112b77a1a72c726d8c914c23dceaa53273c351      1
    70 0xe359ab04cec41ac8c62bc5016c10c749c7de5480      1
    71 0xe3b41ae8785e4107cc69f988042ff4a66a367fac      1
    72 0xea39c551834d07ee2ee87f1ceff843c308e089af      1
    73 0xef5ab90a44b68d4f5e3f6be6af4bedb12cd2c66e      1
    74 0xf15a6b54e68884d27e1bebb1624d70c227b7d04b      1
    75 0xf2c5f1fd977dbd6de9d04bc4e62dff722d4bb1a1      1
    76 0xf8aace471723dba6514f6c6d43651611a0c34529      1
    77 0xfcbca52b23db28470eb97680d18e988d8b60b608      1
    78 0xfd22004806a6846ea67ad883356be810f0428793      1
    79 0xfd849d8cf365fe98bc60025ff7fab45cef0f557b      1

## Allow 6529

``` r
c(allow_raw, allow_singles) %>%
tally() %T>%
readr::write_csv(file="allow_6529_phase2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 4 × 2
      address                                    amount
      <chr>                                       <int>
    1 0x2c8875f34ceb219f61b7453b2c5f100ec2f6ed33      1
    2 0x89ef05c30c821c49b67d06e630686c5c3232baab      1
    3 0x9274f2f89fbe5dc00c21c628e46a18c7187c14d7      1
    4 0x9dc5b2cee788e3e9c67489b5d7ce634dbf48a32e      1

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
