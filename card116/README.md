
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance         contract        
     Length:42890       Length:42890       Min.   : 1.000   Length:42890      
     Class :character   Class :character   1st Qu.: 1.000   Class :character  
     Mode  :character   Mode  :character   Median : 1.000   Mode  :character  
                                           Mean   : 1.003                     
                                           3rd Qu.: 1.000                     
                                           Max.   :30.000                     
         name          
     Length:42890      
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 17569269 # https://etherscan.io/block/17569269
block_hash <- "0x83784aa134f797e63a73234fce9a948232df65f7bcdfba31660a8c6ef14a8a70"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4666 

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

allow_artist_phase1       <- pick(snapshot, contracts=c("TheRaven","Abyss","Sadboi11s","SadboiEditions","MedievalPop","Dark","preybysadboi"), address_remove=address_remove,address_max=1)

allow_memesRandom1_phase1 <- pick(snapshot, contracts=c("memes1"), address_remove=address_remove,address_pick=75,address_max=1)
allow_memesRandom2_phase1 <- pick(snapshot, contracts=c("memes2"), address_remove=address_remove,address_subtract=allow_memesRandom1_phase1,address_pick=75,address_max=1)
allow_memesRandom3_phase1 <- pick(snapshot, contracts=c("memes3"), address_remove=address_remove,address_subtract = c(allow_memesRandom1_phase1, allow_memesRandom2_phase1),address_pick=75,address_max=1)
allow_memesRandom4_phase1 <- pick(snapshot, contracts=c("memes4"), address_remove=address_remove,address_subtract = c(allow_memesRandom1_phase1, allow_memesRandom2_phase1, allow_memesRandom3_phase1),address_pick=75,address_max=1)
allow_gradient_phase1     <- pick(snapshot, contracts=c("gradient"), address_max=1)


allow_artist_phase2       <- pick(snapshot, contracts=c("SadAPlenty"), address_remove=address_remove,address_subtract=allow_artist_phase1,address_max=1)
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

    # A tibble: 95 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0527e39c0ec1612dc0be61299ed6860cc0796040      1
     2 0x05576913eea5d79b83f28f0cb0d12be54fdae8dc      1
     3 0x05dabb2ed7f3f2bd666f67c5d0ad30a5ff2b7a52      1
     4 0x089195a905c8d11a22c977aee408d8a694bcdf18      1
     5 0x0a346913f236cd80825cdb14d4167b6ab12e6afc      1
     6 0x0dfe9ec2f57c3b95653f47da96b49861e4010b36      1
     7 0x11c03b0e147af0e527a7f8bdfec94987fc5c579d      1
     8 0x13eef4ef8fca471f242ab0f8f49a3db6017ada33      1
     9 0x14ec3c75b6b68f59af5bc328a4aab494b5b7b735      1
    10 0x151e86b0f89ed7940231b6f198e1656664b8cfe9      1
    11 0x175fc6de1db2ff5368726f7bbc9934b9f2333917      1
    12 0x17e566d94b9e9471eaaa1fd48fed92666fe0e6c0      1
    13 0x1ee9b03ef647a795dc9923e8d22be37727bc3077      1
    14 0x22bd04906e808196b19f7d10463fe463ed5f88cb      1
    15 0x2a0a412e0a0d436cca7ddba177c4dd9f29801062      1
    16 0x2b9c44b4681823c826e51eb380e539ed35a3e4e0      1
    17 0x303d6227822c5d45c5d1ca7d51249e908ded2fc3      1
    18 0x320456411d621674581108d1362ae39f86b869cf      1
    19 0x373d419979862c6845e74cee5ae1e075801b6abc      1
    20 0x3840f067df8a1900a04051fb4bc639f7f83d519d      1
    21 0x3876a65d530da2208616b30e61c7c9a150ed4fe8      1
    22 0x3fb2ae6dfca30228a3c226ac7b69bf14722e05ce      1
    23 0x4277191c63831c7445e7f434b3968c18c85b3516      1
    24 0x429a254235038a0ba6edfb60177ce012e82330e2      1
    25 0x44f829ccbdc9c15ca84d4fd3f00c4682c35f228e      1
    26 0x45a9abd5720af408015e9051a641250fc4dca5fa      1
    27 0x46b2bd5c888e6d57a839c559fd6076f2c15a8cb1      1
    28 0x4b44f85d6bb71482995105962fd073742c94107e      1
    29 0x4f6afd125968accbcdfbab27ce1cef36ba741c08      1
    30 0x5e61395ad75b1b016888081d153c0c5811666e1e      1
    31 0x5efdb6d8c798c2c2bea5b1961982a5944f92a5c1      1
    32 0x621db9ec394a5047581f80ac95d7716cbc83529a      1
    33 0x62cf3d7a2bf7672eb087c439f5d1fa4e2093a732      1
    34 0x651a6f6c0b331a2f9d036adcf0ef5e268055d7d7      1
    35 0x65b27ba7362ce3f241dafdfc03ef24d080e41413      1
    36 0x6811eaa638b04aedafeaddfca36c50ea303b1866      1
    37 0x6ca1e305b3fdf0e8207492628cbb2856c102fcde      1
    38 0x70abf84fee2f1e5373cf9e7ed9ca21545251d38c      1
    39 0x71d59439660a89dae173749ad3fecf5a878ea747      1
    40 0x72a1033cfafbb17ce3c64e97af1eaf53cf7718f1      1
    41 0x753f8dab2ce1e6ec33e2967bcdd4974f60025319      1
    42 0x7b343c896f85657976dfc88f282a69fb2e2a99ba      1
    43 0x7d70b5b638a2af9816202880535c8fe8f1fb9772      1
    44 0x7e5ab36876a267560e7191cedbe99ee7bc04bc30      1
    45 0x7e7216319b016c758ed6f792eff389291aa8040f      1
    46 0x7ec3054f96f18709eb7c834a0457a1d56164f7a5      1
    47 0x7f6d34a4fdb21729ac15aa0ae01b5b47ff2f5f31      1
    48 0x7fd1d8e388b29ef0b8a00ca07e2266686a5559ee      1
    49 0x83b97b9454d90791480cfda42c01d461d4d4eaa3      1
    50 0x847f33f5c02b0ce987c337fcb190650a91995859      1
    51 0x8684e8c7d4ae0256372e5fd727ca2bd3f4ac95b2      1
    52 0x8ac6a89a3484b372acf4f0de03646c8b2a962911      1
    53 0x8fc54e3d572012099f3f48369657d58f653e92e4      1
    54 0x9499054d02a725316d61fa896c29d58550ee4a5b      1
    55 0x97064190190d4e2fb284a2d55dd53504e291c4cd      1
    56 0x9a667dcebfab972af744ac02e6d477945fc24da2      1
    57 0x9a7dc5307940fa170f9093ca548bda0edb602767      1
    58 0x9decd2a74340a4f22d2dbb73583c85c898447589      1
    59 0xa631378f8f9864a49b3a8b08fa8cff080ce64384      1
    60 0xa72ac4dc61f794d76c408d68d3995d7f75982a3b      1
    61 0xa8bd82a0bd23206f707407276d08faf44879ba57      1
    62 0xa92e641eabfc0cfa1736dc8afb3874ae1d9077ac      1
    63 0xac5558b2f3c84203fa6efb6eb8ba130ebb8caa40      1
    64 0xada53dd3c03a5b20dccf8ee6615cf31a94c8fa51      1
    65 0xb0bdd53b627d7e61cfc5c13ef110e47e210fac6f      1
    66 0xb54a56f5a61f9e63c71bba6e3eca4e1a75835e7b      1
    67 0xb893ae8a1824604f6df4dfde52e2754921ba1a73      1
    68 0xc0efdd2b7616b1a469d5fc05dcc90f29647f03b2      1
    69 0xcd8b72710595c69019c109aef5b1b92eea7f995f      1
    70 0xcdb467660ba28d90b58f428c3c0ea75dc902a249      1
    71 0xce8ad80ce1a979381d209ac230d02adafb9fa897      1
    72 0xd2b12f1a75b2313980d7c410002e6a48cf389b8d      1
    73 0xd6aba7daea7e570dcd400fabeaef75933c7c9967      1
    74 0xd831a9d032d19d4b4d4ad76bd93bf91fb8fb07fd      1
    75 0xd91a9d604db19eac6679cd37151eaf18fb4b3e2a      1
    76 0xda37896e56f12d640230a9e5115756a5cda9a581      1
    77 0xdafcf1931a891fbb29a29c47ddee5ee76e24dc2f      1
    78 0xdcae87821fa6caea05dbc2811126f4bc7ff73bd1      1
    79 0xe0eb3554d76b8a66ca66cf3343b3f92a8d240b69      1
    80 0xe1c9b7038a03dc898cd161827709a3c241af991e      1
    81 0xe2d914329e9a80e85b70ff123891368095b1da4a      1
    82 0xe412186a1865c4fe54a79e0adcb08b81233df674      1
    83 0xe734a912068e1b127488290f71f64029e6274a09      1
    84 0xea1126e70185400a2cf0de2a6b35428fb8affd29      1
    85 0xebe326d8de3413f8132518dcfd45e6cbff7e5c27      1
    86 0xec23992d5da7feefdf6e21dfc688d0b56082d1b5      1
    87 0xec7100abdbcf922f975148c6516bc95696ca0ef6      1
    88 0xeda801310228d4d8eb818a2a90beaa6f10c08db0      1
    89 0xf207e69a7cc3b4a00fec3039891a77b3df8c3577      1
    90 0xf5b407a5bd88f549f6e8fbf4aa13364a7e93581e      1
    91 0xf6ee907fd2e4dace32f0a5e388bee471dc49a323      1
    92 0xf81c1d67b9c1f80eb30c610e2a78c2a1b1fb013c      1
    93 0xf8a0cb622304daea70f51079c8645e8bfbc147cf      1
    94 0xfb2ec6543d97808d8a00fca199c39022b0b3368f      1
    95 0xffb5592fb5a51f85668ea6634c88ac1949cc063c      1

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
     1 0x03024d05b1aea10b64ff3af1fed6a3df9adeebb7      1
     2 0x04df8d02f912d34fef12a1b0488ee56fd6f7416c      1
     3 0x06d643b9ee73de07ade8b5c9fd520fa2aa28262a      1
     4 0x0acabdaceab63c08bc5aea63d7e07da7c2e42959      1
     5 0x0f9ab846449ca068f9714818a6ed0da5d819e9e4      1
     6 0x1033caf4e55579e8aa1cc59c3c302d7d924f9f89      1
     7 0x111818a51c4177e8980566beea68fe334be7b76a      1
     8 0x17e31bf839acb700e0f584797574a2c1fde46d0b      1
     9 0x18595f049a9c5c2019efda113009b0ec1c89ceaa      1
    10 0x1a7f174dcd4211d41c7c4d7dbb3a0e4db5780c67      1
    11 0x20163ec2afd9698433a031b0a7dcd70ed77ba8c7      1
    12 0x28e6c1352950238be088ef2a741f3c8b91b9ffad      1
    13 0x2917e947fb1925abe184d4062520a11bcf2a5b36      1
    14 0x2be96c05fb7e81b020c42086b69d21bbf427b53a      1
    15 0x2ca75a892b59bc7cf863ba261e49ab40a4844ee3      1
    16 0x31bedb3962ab3a1d2a2e070853aa5c4acdb734f4      1
    17 0x37ea6c993c5f42e1fc3455d5781e4a580760970a      1
    18 0x3cb63b82d778105e43f064ed739b0655f1f0fb87      1
    19 0x3fa5a25f48ba1b736761706801be4f639ca4853e      1
    20 0x45360f55024132b3110166e1b327170daa2cc299      1
    21 0x4a0e071be98b9514aad70f5d2595a8ab23a7f053      1
    22 0x4c941a23ec94580d3b5442aa8d5851583fd8bcce      1
    23 0x52690f90740621f89f58521433e9b0921d626708      1
    24 0x542c5648d46462207fba8a4bd7d890b4154b722f      1
    25 0x54669ca58bf3a59caea8ae5135db491e4738f65a      1
    26 0x57bd982d577660ab22d0a65d2c0a32e482112348      1
    27 0x5d0d23ae81ebe1b6ac96411b2c7bc3bf7463fa25      1
    28 0x62ba9de7d81b96d697fc2f26b5eb647f184f9b2e      1
    29 0x6e5632d334b861dfe4312c7ba34e175b60be0b5a      1
    30 0x70c8db61d09271f4c90950ba8c6cbaef918f12f2      1
    31 0x714b194ba18daf0288848054bec788de08c00cea      1
    32 0x78e37b881d078d4b2f90de0422dadfd4da50ed4f      1
    33 0x7a231b6d33f1da74e9fb593bc785b4914a852867      1
    34 0x7c741ed01ee259feba4b7f9cac114f48bcafacf3      1
    35 0x7e737324cd6702e1b93c9240072ec9cc036dce21      1
    36 0x8043812aea5a07dd6523b5d83abd5e606422944f      1
    37 0x8874174a2366668d54fea6343f71709389563c8a      1
    38 0x957143815f0e1d1b1a31b2bfcded5b416ee675ed      1
    39 0x9e5198418081fe00026ca0ddedbfd3915ab98298      1
    40 0x9f4fe19fed4a008a270840fe95660f25df36c734      1
    41 0xa1fbc0a9ff967ce51b8ea952b35ff7005cd4cc63      1
    42 0xa490a0346808dda91aea6698cb19e4697d9fc5cc      1
    43 0xa4b61e227361a9cd9e62ca10946c27748a382cab      1
    44 0xa5ce27ca5e31b1de92db2d2bc109b3e23cf1d4c4      1
    45 0xa817b9bd1ece360e4a1692894a5ad1d40b889f20      1
    46 0xadebdeab678647a457743ea3af98f8b804e45c24      1
    47 0xb4627672ee52660a9e453ec541834e04583f3602      1
    48 0xb8700157128ff544c46cf35dec5a19120baa8eb2      1
    49 0xbd3a5d12af9fd82fb927fd26216e4dc2722d2337      1
    50 0xbfe384d79969bcb583b8a0e5fedc314aee480e7e      1
    51 0xc04208f289d3842ac168f2c373b3164e1d872650      1
    52 0xc18d4c2fee42accfe7bfdce98bfacbabf76242e9      1
    53 0xc1b0307ff325527511310c7d6fd3248188742f86      1
    54 0xc40d07925d1ac5500e9dfc4e0a05eb24a75d195e      1
    55 0xc4bfc1dc25827f39ad4a66bfc968456c7852a9a2      1
    56 0xc6cf5a020bcfc65c6f8181c04cbb5ef5050fe28e      1
    57 0xcb85b581caa71949784d2e826cf9df391c244b33      1
    58 0xcd30b6de9dadb0a1598eb12a4ccf34b98c98c1df      1
    59 0xce990032a3ed14e197421270a9bec9e276bf2f31      1
    60 0xcea266acb92d5603dc53b5d6bc6e568dcde0d311      1
    61 0xd3b37c6567e2702aa727cff4ce5939b4a07a2fde      1
    62 0xd5ec003289265705727b622f1700fe814e54ca67      1
    63 0xd7d941ff2890bda98f40a5dda0593d239a603f09      1
    64 0xda34ee56031bacb68cc3ce57339c2a11c28d8eb3      1
    65 0xdc78107155918e230246439e4159fea4c477eae9      1
    66 0xe418a9a5e49dde0d13e1ef51d4bfb7fcc51c28df      1
    67 0xee3aa3fa5b780bc73da73a82d47fa2d8cdc1a647      1
    68 0xeecc9f94c674161e84ab0c60c020432ea8f88bc0      1
    69 0xeeeb5dd03eaf4ab6b72949191d8ffcfe27f7d973      1
    70 0xf36796fc4f8fee589ff959264c9e99ca37a1b659      1
    71 0xf4141aef39803327497f6b81a21bb3f2acfa2436      1
    72 0xf624e9324f9b330cc0289775d7b91e945e881134      1
    73 0xf6fb4e6cdd7ed50dabd90d10b1ddb60a85720737      1
    74 0xfed52d251e31178ff8bf4a0d611721c544f74fc0      1
    75 0xff4f0dbc9ee7583c0c61c7333a5daf8d0ab4aeb3      1

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
     1 0x03f4cb9e297ea659f30e09341ee7155a7d136398      1
     2 0x04294157cfba9ff0892f48f8345ea3539995f449      1
     3 0x0c481cd7d09bdd222351d52622d272367cb3d159      1
     4 0x0e5c3e22b7239cef46f56741ff66369f3cfecb77      1
     5 0x0fb4e7dff29c22029975373d1bbf5873ffc3c61a      1
     6 0x11924c505ecab6a2434bdfe94bc0ff1ca081facb      1
     7 0x156db724902f03f3986f6adfdc38f7696e4d5111      1
     8 0x16b5ec9e34bd0a916822d6785b1c600ac911d6dd      1
     9 0x1e0486ee85dd758078d75c674f3d28efc4c899fc      1
    10 0x1ecded4519a9688ef4bfb5f15811c52f048ba1a6      1
    11 0x1ee5106b2233169b84dad2acdbf498b29c3c7d15      1
    12 0x23ae72f8336aca747ef02d596403de56cca489fb      1
    13 0x29cd8d94a4e564a6d6576b467ea0bb9d51c9f05e      1
    14 0x2cfdaf06e39e5d9d3c9afe20362b725b41f68725      1
    15 0x2d924c0bc8033e7866b90ca2a76cbf4b5714b11b      1
    16 0x2fb24b1021e51fa23db91a9b995000181fda0963      1
    17 0x3a10fd1387555cd75db8142393fbaba98efe28d4      1
    18 0x3d20c3f372207074df1a0ab5272c7168ce23d178      1
    19 0x40e6d6e798dc878e12987ed978f23c2391f1f570      1
    20 0x44b4d430e7c56e4ca04c5799e561063ccc1f1df2      1
    21 0x44f2afb689aea37ab17feb2420f17f9a3e851307      1
    22 0x45133db3eadd8718f5c3a868ec2f7148e460dcfd      1
    23 0x49d6531aea6dced6595d50da685274766e4cfc8b      1
    24 0x50f8c08b0124092e1001b355f4b8ae2df85f715c      1
    25 0x5bd832dc1a5ac61f687b87c4199b844819a4d8ed      1
    26 0x5d25087405105bab12624c73488ec186066a6376      1
    27 0x5d5fe1910a289b24455af49ee3e41405f94b921b      1
    28 0x614a62584658d555cc28b3aabdb691e85e002c8b      1
    29 0x615502edd5cf2d59244dd0dd53d3c8ec035e172b      1
    30 0x62af590649b7abe80f8c6dffbb92e0875b65e37c      1
    31 0x67a90218cc5fc1adde45ad27a257f268023b2ecb      1
    32 0x699990a8e7ada9e92c932d6e8fb365024fc74b43      1
    33 0x6b2aa5fc4508ec023a8e355959eaccef5e3a250b      1
    34 0x6b4fedc18a1e8cc0523e56cc9b45080e1ea00005      1
    35 0x6e1ac9b3f4499a73385dd8d2daed6449406d49f4      1
    36 0x70e680b9493685f72e76243c09993fca768eedf1      1
    37 0x75d4fcaffb56055bf9df3574d345473e31a16076      1
    38 0x765718a8e4a5562be34b122af259ce5a71372e12      1
    39 0x76ea151c88fd91902f7bf3f829db65dc9ba5d45b      1
    40 0x8586c32dbe1c986bffb8f1ed6c3e8c6489f4fa3c      1
    41 0x8854ad314fdbcc275eb0d5f4fd6dd86ab7981cce      1
    42 0x88ccdfe9dd047b4cec4c1102d2d803e2d8bf683e      1
    43 0x8a063961e57a68a8a1f68930d758e1bde05fc6b3      1
    44 0x8b4567fa8c4715c27a682215a031033a8b544206      1
    45 0x90af376346ca97d4e9d1e510e82543ef99b56a28      1
    46 0x94f052ca65a34d7f581bba95a749b1cf52689dd5      1
    47 0x9997e55f76e34f14d7bd6b268a2e16f896389ee8      1
    48 0xa10bb823fb55a9199e4f521d0a88993c4cba7150      1
    49 0xa7cafd18dd8bc1e23203058d66f89f0f0ee539d9      1
    50 0xa9237c4eeebc879208e59c867cb6d7cbffc1df30      1
    51 0xac1c7cc22ff431b5fc07c177b629d926a45eafa8      1
    52 0xac6e4334a82b8434b0122bc3c9b42d0a5f190d4a      1
    53 0xae72c6a6fad9fa9d82d089e1ebf73b3043855425      1
    54 0xafc093b1c8419f05d4de6ff54d38121c0d733752      1
    55 0xb099a3c3043b262518d9b6cd4d5c9f589b15e2ad      1
    56 0xb335326c7f2cd2e4eb09ce3d1745c92f66497b7e      1
    57 0xbc1eb4359ab755af079f6ef77e3faac465e53eda      1
    58 0xbe2f803bfbcffbcd77aae4e9104406abfeda497a      1
    59 0xbfb7969e75ee4e58620f74135d167f5bdd60800a      1
    60 0xc0ced4439c1f0871f16a46e13fbe41fbf61ba265      1
    61 0xd361363a6401870ff8a23b7c77c004aecb8c8b84      1
    62 0xd36590461162795ee33099b2076a0d4e017ae17c      1
    63 0xd7342ea20a5afbf24352b5ca61e09844167914cb      1
    64 0xdc819a832bfa55c933238a474954f3646da275ed      1
    65 0xe26027e219998c0acfbd00b74795dc850aee244a      1
    66 0xe60458f765bc61e78940c5a275e9523d1f049690      1
    67 0xe776de32c9dc8889e2dcf1cf0606010a51d1abb2      1
    68 0xea9f3a983d965e582c34eb852c18babac52050d8      1
    69 0xeb0bd6bfa6d109205f857caab59f651fe7631094      1
    70 0xebe4c00073f1061ad27859478adcb4f5bc8ca67c      1
    71 0xef04cb0003ca57dc7bb563d5ea3248241b145bdb      1
    72 0xf620422e1e2cbf123386ea664f536d1016e5a5d0      1
    73 0xf681efdcba35c69dc0fe56b7fdb56324f5abd385      1
    74 0xf8be957f65e67fb0342992a51c30290d5758f880      1
    75 0xff7e887f2b7c7594e601ed1c2e44b55bb80f774b      1

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
     1 0x01282b55c6be0b0d724d2a9324feb3c08229f2ca      1
     2 0x085d07d65b41158a1545eecf05316edb5d163b54      1
     3 0x09e6267d0cb691fe647da5029d28ab0fd62d9325      1
     4 0x0aadeef83545196ccb2ce70fabf8be1afa3c9b87      1
     5 0x0c664c03eebcecb6c21e3b3bc77c9dffed5bd694      1
     6 0x12c4908dc21e88a8f268c3f363098eed251c1aa3      1
     7 0x17fdd5ab047e502ce1faa065f49170875b083a47      1
     8 0x18412b9f84df461bdc0f2e0709df507d5bdb6363      1
     9 0x1d25029d92a7051f46a13be9a512443c966d3542      1
    10 0x20aa168e6c793646f60737399c8466dd643d4281      1
    11 0x21804e35a54aa4820e6cd409d70926a63dba3e45      1
    12 0x231595e3673a10e846803194f4982e1cf3389161      1
    13 0x269c2d0badff4e365ac2a0da8c8e3f186b6adaba      1
    14 0x271c6f480aeb26672b915a7b33159ce8f1e97fd3      1
    15 0x273396317b20d90dff0b69a8852ea912240828fe      1
    16 0x27b1abafb2cb065cfaf41b4b7ee95d27192151b2      1
    17 0x32606c4aa95ea78d7c5b1e0e347e3e9ae451434e      1
    18 0x3368b4733a4552b38b7d81c215d88cac4c78d8e5      1
    19 0x342522ae61de25d48c66807a2cecac4681be3d33      1
    20 0x36eebc48da111b14b010a97abecf8bb68d10095c      1
    21 0x3839a6c85a38de97abd0ecc092ef3311a7f1311e      1
    22 0x3cd76a7a90e0f6a7dfe6791a54ec459acaebc5ca      1
    23 0x50a9f0f42ecde1df805b1ba6e885881accef049e      1
    24 0x524b7c9b4ca33ba72445dfd2d6404c81d8d1f2e3      1
    25 0x53edcefe31da6a0051df17ad80e19ff93c490b17      1
    26 0x541db1ed2628f2f4897417d06181af6a179e90d0      1
    27 0x59a5493513ba2378ed57ae5ecfb8a027e9d80365      1
    28 0x59e5e7b519ba5314f095bcd988ec214d31e8162d      1
    29 0x5a3a1461310884df894b7e973305f690fc5779d0      1
    30 0x5df6dd99718f32ae362b41e78b7bbf66ebccc179      1
    31 0x60bc11840966ae3946ad1904a8efb6622226be25      1
    32 0x632734882ed0127fbdf2666478df42aa916bdc84      1
    33 0x638fba3bbbe8828394d2b13f92fba75bf32c8efb      1
    34 0x6576a26763cefa8f37005c067f7902960dec6b8e      1
    35 0x66fa06bd24294ab3bf123153e49b9065cb7672e7      1
    36 0x6d9a6835d7562d0458c821eb7b1438a313b9ae13      1
    37 0x722e2e4c15be1fdded3c86f4100bc32b181827f5      1
    38 0x753c892626c4fa09d1aedc0e9b52bda97a4afa00      1
    39 0x7921dcfa9f14dc980d7e8f946b7bd223624c3db9      1
    40 0x7a71dc357646a8149a920dbd26f66f07769f01d9      1
    41 0x7faa22f45695a77ad4042c4cfb2c0330caa6a329      1
    42 0x80cf63b40741e041515e6ba6a4d327088540c6a6      1
    43 0x877d45306e8c7506e9a20c9b7a79bdda97c4e7fe      1
    44 0x879a8a1868d620584afa0c9c8807628d553c6bbe      1
    45 0x892eaf46a9c1cb75c3f230420dcbee6320c26932      1
    46 0x99f70f9aa62bd71a7e0719afc2d6c67c6aaaadbc      1
    47 0x9d52fd3f58557f3b5fcd2b10314bf566cabca60a      1
    48 0xa03d04ff89308adf54badf18b46fee9980093524      1
    49 0xa32560268dfff7e62cd0cda1dcc699406321aed6      1
    50 0xa39d385628bd00438f8a9ba4050a25a0210f84eb      1
    51 0xa40117a1394fc5cfb3576fe12632381b0b79c0c0      1
    52 0xaad4210b800f14660ef4068029d428936ebd21fd      1
    53 0xae0d16586e5d60d334624c115216a52b9b1a0335      1
    54 0xaf12a9af19331fa9b89915537ce9d41bce5e4c5d      1
    55 0xb18e3c41faf4139b89b4ebf1f5ef645a3ad0ec7f      1
    56 0xb53ee69b92ad6d12e6f3b1f849ad3e706e31a263      1
    57 0xb7187511a0a0f5a63a538b7bc0abdd1a01979848      1
    58 0xca288359ba6319c7e7c7377a670183acb3781cda      1
    59 0xcd8b72710595c69019c109aef5b1b92eea7f995f      1
    60 0xceb775d4b09c2fba2fe51efcab6e39a7da1528c3      1
    61 0xe061ea94f07de987a55a49067b0d7ca3feaffbc7      1
    62 0xe28470253f0c9c7afbae7f61795d6b1ca4644b2f      1
    63 0xe39bd9e3af2054dd193b0d0217cfbd460f104902      1
    64 0xe3b41ae8785e4107cc69f988042ff4a66a367fac      1
    65 0xe44946a036d9c1f8438d4d2a33acd969d8c48706      1
    66 0xe5ecabd057db846332957105e7d7bbc8369e088c      1
    67 0xe5fccbbc11e4a36a77e9d8e47a9b28ec44e2e2ff      1
    68 0xe83105d4e5b144e388d7d35c65c44e0da8c8016f      1
    69 0xe9e88f56f5431d692446ec723c2f9f9cb4eeca42      1
    70 0xea7d3818a3b736fba40852cd90471df33c6d6821      1
    71 0xeb775bf133c142e873f2ba6925d53107550e8703      1
    72 0xebaedb897fc974f35bf36f075d60cd75e46d2d4c      1
    73 0xfc61d7884b047f9f4bf56e984f773f1bd5d51480      1
    74 0xfcdf3c2b2b6ba00e6db59c70c424b27970ff3974      1
    75 0xfe8312a959e031c7d4cbe3f9cdd3ec8726d0d80e      1

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
     1 0x0340da03212bf3448e99b85f886f4d2017b0ca78      1
     2 0x087e14b18e87367a7cc58f04989d749b8a0e71a1      1
     3 0x0a123325a2eb92dda47587d697e9a333aad9dd97      1
     4 0x0ae6190349b79dedb16fadc3bb58e428938f3644      1
     5 0x0d4178564cbf36e7eb7166bd9292080d2f7484c2      1
     6 0x0dac0d611db5955bbe881cf8d75d4c80271cae83      1
     7 0x13b75d28f53a4335a544e05a54ed4b52d19a16ec      1
     8 0x159968fad6a3df074cfe7e587cde0c5f375c1b70      1
     9 0x1635e11ad250443fc3bc7beef75e47a601b25d93      1
    10 0x172cff9b843d0ae167d33a74709b0a3affb6a5d1      1
    11 0x185d5f60fb7c59d344ba939361908916cbffe1dd      1
    12 0x1a51bcbc8d3642680d8267550f54e6e4cc5c4984      1
    13 0x1f77d34deff2b72b0a0258603effe72704742ebb      1
    14 0x20251a0505ead51fb2c6ce5c1f399924ea068322      1
    15 0x208833804d09cf965023f2bdb03d78e3056b4767      1
    16 0x21a51b7c3ba93e70884e34482a9790e6b72ff9f0      1
    17 0x25bfb4440eabe6316568c1037ca15de16a3b55d0      1
    18 0x2866237e831ec3ebfb44cfbf6cf7641eeba433a9      1
    19 0x2b5faeffc4b8770144c29805de1f87fefa7e3156      1
    20 0x2de9c8ad139793ad2f72628a960f5265ed9ef28f      1
    21 0x3275d4465a4248d5e2bc1d702557467c24d34bf4      1
    22 0x3d0a1d20ac580ca8fa35888c92dd0859887f3398      1
    23 0x3ed17fda58b4ca8b4d425fa5a3b92bb496bc0a55      1
    24 0x423ce4833b42b48611c662cfdc70929e3139b009      1
    25 0x47a710fa6bed241a8ef0080f10cad9a90bd670ca      1
    26 0x4bada215008b595f9d35379c06bdba0283fffcff      1
    27 0x4c420110bdc4dbf16e8d5e0b559168dace5f58b5      1
    28 0x4cf3f7c5b4fc7a14cfafa3ed80d82f643c1de8a4      1
    29 0x629fd2b73c8e01888597e3d2344ec6098575af13      1
    30 0x63648f2c3e96b0f548575fc1b1efca1a61f5de50      1
    31 0x68230fa9310ae6673ae78b6f8e9eb2b9f3bcf372      1
    32 0x6ab0bfded6af31cb0a5987f08564ecfbee691757      1
    33 0x6adba05ab489f2bfdd050616ddeb04598555c09d      1
    34 0x6bdead6b984d74baee2da16bef7f58319e659398      1
    35 0x709c13074ed89a5d8efe996b4f1e2315d833f431      1
    36 0x71a4e46e5d78de036f7e6c916be092566e1565b9      1
    37 0x720ff4cc224b2d3f0cbb06834d6b595e6c3be84c      1
    38 0x72cb2d89bf120f1a4677f94befec51b614f1f4cf      1
    39 0x7a79944ac7e770cfd13ec024a4b31b8c5efee60d      1
    40 0x7b45836d84d9283cbabe63864e953089ff5ea182      1
    41 0x8469b7b08d30c63fea3a248a198de9d634b63d70      1
    42 0x87193fe62b63b811bbab15842c413b3bae94a6a2      1
    43 0x88c5677336e252c7a53c29dd20375edc802a5919      1
    44 0x89eb11795fb79ab0f565342a906ed8491022cabe      1
    45 0x8af60da4e29007eb55b366e89b6c8ec7a64ecc8d      1
    46 0x8b2ce5097f4a1619ba02f443ebab5f306342b867      1
    47 0x8dcdd79c07672a786c376fc29880ce366bc7c563      1
    48 0x8e02c045c6690749393e9c3b8fd6befa651cb718      1
    49 0x8f3ddf567bff663d61a12a5d5f42f2e6f220f89c      1
    50 0x8fc3f4559da4c68d460b78e9e1721a557632214a      1
    51 0x94042981fbe827237acf0d49bbb51fa8c1c56aa1      1
    52 0x96d910ec8b20599be19bd6301aa5de78b7c7e66a      1
    53 0x980e22587fc29d25cf2c82baa6933dc0f73c09c4      1
    54 0xa1486dae9f01e7da3bd8efbd12af0e1be1c73b60      1
    55 0xa84b6da113c0279363dd1dcc7465677a4b0455a9      1
    56 0xae37a30979b78921c418085e121b3f6bccfb35ae      1
    57 0xb49efc9ea89b256758abbb8d89d4206e6df75c34      1
    58 0xb74ea5880c2a22d508eef4a753eee9b85d512951      1
    59 0xbd79b7e0cae4500eb02e6f212ddf2681ac8fb6d4      1
    60 0xbf44c636a4a4bbe257a87067b25c4c875170e04d      1
    61 0xc611376086b5152a78070d89290d0a21e8d17c17      1
    62 0xc7382780c13d05fa6ed857fda3fef3d1b9e0e00d      1
    63 0xca0a8c0eaad3e8c0939c718cf439e30011b86294      1
    64 0xd1e8a92f44bdc83bc620c56a7913fd97de5abe10      1
    65 0xd95ca3bab3b2f9b3a0fb9c065546f7bc4bd5de0b      1
    66 0xda913cc4d779082876ac1f6468e7c0b27781d4d3      1
    67 0xe0e31366067277dde4a391cd5d77f43cdb9ffa6d      1
    68 0xedc1040e47f75ff85867ff4358c6229801af31f8      1
    69 0xef80395b4943e1741c92a6f2119eba58cfed889a      1
    70 0xf26524b357be116f8d59a12fd38c84a01c94c81b      1
    71 0xf3ab1c5047cbc4cb0a61c9cada9b2929e18d11d1      1
    72 0xf4a2f16ea9ee6b69e90df8d8cf5a5b7622fc89ca      1
    73 0xf5d3988fa95817b747e88f097175d1cd4a51e099      1
    74 0xf8db01f59d0caa15067156ff7ed786eaf207753e      1
    75 0xf9d983b7be9d41f8f94180de199a2ba6828ef458      1

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
    19 0x3df7131fbd4f6b27be8317c69e41817498e1bc0c      1
    20 0x40d2f4399d23f9afb82d0a6b73055f13208614f9      1
    21 0x43b0bd27d5016e969193412933387b0dd4cf3e0a      1
    22 0x477ea7a022e51b6eb0dcb6d802fb5f0cfc3b4a81      1
    23 0x48464efe55fbcae8ae0c992b306afcf21d4910cf      1
    24 0x520e8d6c5b2dbbe62bf8a40653a314d879b19b86      1
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

## Allow Artist Phase 2

``` r
c(allow_artist_phase2) %>%
tally() %T>%
readr::write_csv(file="allow_artist_phase2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 985 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x00007ec94a52f5842385aa3130edd336686b1cc7      1
      2 0x0042cb6be82fd326ffa5fc2edba67fe270adfa98      1
      3 0x005c1071bae3aa43b15bc51eb76c6f863695d320      1
      4 0x0061ea01803b55e0d3d16359e89d050a852b577f      1
      5 0x007bead08df370a5ceb341d608128e4250e06d81      1
      6 0x00bb8932dfda68e68f41b55e03898a4d2e96bdc8      1
      7 0x014eac8a5a723d00f150baec88a7e7ff5894c1cb      1
      8 0x0155e31d9c01c28395277bd4210bf5c51965e8d6      1
      9 0x017ffb7c9e5aff47d9569265d7c78b2494099782      1
     10 0x01a4d3d7a9d2bfc3bbb91d93b582df28a0d938f8      1
     11 0x01abece92ea50d95546f97a60a0f136be504e71a      1
     12 0x0265310ff0d23bb25350bb8b7d2f461629f983e3      1
     13 0x0270201d3397dab6ab7fc0068d07f454503ce52f      1
     14 0x027a513b9df74e49433d1e436670c588d1a8b420      1
     15 0x02891d5bf76bddfe678d6449838c3ceb2ba40160      1
     16 0x03593a431c7c3b01cea6836c2199c43614e17e71      1
     17 0x038971dff8438edc76a761e53507510da527a758      1
     18 0x03b367733f46743534c9bb13bd4fb5ae2780babc      1
     19 0x03e51cc7381cf0c50f838bb8edf74ea74a4cbe55      1
     20 0x041a7e2bb9b2c7f38d9d39102a509bee9b3aa1b1      1
     21 0x0476858c6bc264ef8701c9cf2a9dde1c5a6fba7e      1
     22 0x04c4b7c8491204de95f9186a5847b20604bd59b4      1
     23 0x053b8ebe5c8aee5ca41fdb86de4849d5be6e3c77      1
     24 0x05a487a677d9f72f3d1c3e420a19248a98265a55      1
     25 0x05afd13ce408a732e43d203336fd2ec4698d777b      1
     26 0x064ec9737b7000c26d62ed1226be10327daa678c      1
     27 0x0653e299ee741808f12427277fc8a062d9c718e1      1
     28 0x067ba93f94ee3787b3c3f1b790657a88177cba70      1
     29 0x06807d7e07290be6adbacd6f209c4ced6f183d74      1
     30 0x06d6c45ae033e37790eed506a61d597d6708495b      1
     31 0x06f769b254c0a9a9f420c0367dce57d003c75d61      1
     32 0x07081c03375900e92a77311c48165a7d70c901d5      1
     33 0x07278577398cc956af76ac19367a1c87c8933965      1
     34 0x0766888adaf83aeea250865b2273d619de133cdd      1
     35 0x076adfc3d52d4f67a4e9c7a5dd314232ef08ece9      1
     36 0x0799b8948e0f4f0b4cf467194dd122f1bdca8a8a      1
     37 0x07f6f97ac29fb6882746e7c95797d30c1b50448f      1
     38 0x0829d05c1a0a482a98c8e6bd5d5901b4d72e23f8      1
     39 0x0843cc62071b612125e08a4ac569a61ed47f478a      1
     40 0x08493a92a95d4e93bdbc49f66e7647f1445a9210      1
     41 0x088e901c8c94111612ae7bdec41872ec328b77e0      1
     42 0x08a5be993528e1921dd2b831d84ba7f426ec9cec      1
     43 0x08c3a0ea4ea8611a0e2f6fe85532d8728f9ce85d      1
     44 0x08e23da12a37a94e1d753eeef3f949a03a64fd7e      1
     45 0x0958f34e31d7f9748cbdd3914acd19b92c3451a5      1
     46 0x09b2f0988b6d24336b348c84c2764d2b4f85142f      1
     47 0x09c52c99e701304332b5998227f07d2648e8a72c      1
     48 0x0a0849c3ba531ad66c7d103bb4d055b0db32d45c      1
     49 0x0aaaac358f29ca7290f78651ae9b08285b5e16d4      1
     50 0x0b5c326d1a1846256613f57c9742f8b8d37578d5      1
     51 0x0ba175b34c80dba65bbc6cda2828eacb25338ee4      1
     52 0x0bc0500bdfbbe9c6bb2f9fd373a0e7438b0f2b22      1
     53 0x0be81b078cd9a6a3e667249b2c4ca530cf394a86      1
     54 0x0c60d8b3d638d9a9bbda663fcf9ad9a8d010323d      1
     55 0x0c7ec0c05d6f9a4455851bf55972529c12fb7bc6      1
     56 0x0cc1722f6b583e5ce86b5a90c74fce98e7160611      1
     57 0x0d323d390c75e717e5807fc8b97ecf9c26741ab9      1
     58 0x0d5cbff2d942722a14729bf01cc9d5421ed15df7      1
     59 0x0d6ce4f0e1fa53af3f624e17da5f47791dcaf70c      1
     60 0x0d8ee9db27e44c6bbce8a6620c13f2c1404ff119      1
     61 0x0e03ff63b9990291d2ea87d08bac3099214b99f3      1
     62 0x0f0ef9408ca5b0dc364ff5921bf544c811e9c94e      1
     63 0x0f2005e5b1edf748a045d721e472886fee1815e4      1
     64 0x0f8572c54032fff121a1a85a3b58f164bcaefd41      1
     65 0x1059c7e69c743ed1cdd10380fd3127536d5eafc3      1
     66 0x10a96b4879a1b4c27b0bf5baec123f19467b74de      1
     67 0x10e0d3ac359524b67a96ba6d06be86b2a1b0f45b      1
     68 0x10f227713a06aab880010a9e4acddeacd783f148      1
     69 0x113aa7aaa8aa85322beef06dcfd1dbff732400cf      1
     70 0x115894b2859e58419d1ef4787a79a431ce2615c3      1
     71 0x1182b52af754b950090f22e03d619fdc9978091e      1
     72 0x11a22b262e505d355f975e1e48a365b5d4811ae0      1
     73 0x12077364bce9d3ac2467f83bfdd8108edb049a1b      1
     74 0x121f79e6304db1f9f30c3c0f5e3f22ee3a73b638      1
     75 0x1223e13ff2316d55efc0a67fb53b80f1cf9dee70      1
     76 0x1288eb773f9ab5fd2368c870f02aaf97a7bfce9c      1
     77 0x12909c49a9a7d8e563608a81501e98a46a65b0b7      1
     78 0x129803f7812b8038bb0c242e44ba9c4abefc89a2      1
     79 0x12cba59f5a74db81a12ff63c349bd82cbf6007c2      1
     80 0x133eae03e7fa46aede6c0dc9639e164489f8c53f      1
     81 0x134b4ccb9d178f893687948337aac079cc5f55ec      1
     82 0x1351d2cf237dc34eeec11f0e3740f0245f61a54e      1
     83 0x13bd8ac1dde1255419663f77d927ba19f09cf6b1      1
     84 0x14669df8bfcf56ca60dbc3397b8e8f0c0ad23062      1
     85 0x14aadcc3d1009717be4d1bab961392afec2c6861      1
     86 0x14d8f85750808f609614d7727596bafac4471547      1
     87 0x15296589df4156703d25475ea9b1039030acfaeb      1
     88 0x154f2f5f885fea897365786fc6aef57b4e35384e      1
     89 0x16649728deb4510a4a5e8eefa01660f922d34cc8      1
     90 0x168c50a42c482a1c9ee142c9c9b6dca6bef0cda4      1
     91 0x16b5d88807ab665c6b58855ebf526d331ed763fc      1
     92 0x16b696d16af4ea24d727568bed97f968b4d54e48      1
     93 0x171b59df58268c7bbd16f676190bdf14de0f0b4d      1
     94 0x175c4ebe408adffefaaea326d9c40e0cc51bb5e9      1
     95 0x17a77d63765963d0cb9dec12fa8f62e68fee8fd4      1
     96 0x17b7108490dd5c006259ef9b32366eb68085e7b8      1
     97 0x17e337c80486fb056b11f83e32190d83f5a48ecd      1
     98 0x18285e915b1dfe8ba9fd735e9dbff9947bf960e2      1
     99 0x1868ce22e19d052c724e63a912056b1f3d19b49a      1
    100 0x18977a54f785c7b98c4a9b0c1c3b72fe97ee171e      1
    101 0x19016222bd086e8c277c54e51db88dbb96f85d62      1
    102 0x192be71658bc00ef6c6296cde604f23c1541a9d7      1
    103 0x193b553c127f6771d1d61ab370804e22723e7e0d      1
    104 0x19aac1bfdb0860aa4a2801f06c948052fa7c4172      1
    105 0x19c54e86f5829f96121db97b3ab3eed0d6e76fd4      1
    106 0x1a41ec571ec8fc815febf81fcbaf9d7f7ec98849      1
    107 0x1a52e67b158508989bc98903027f3a3939a60c56      1
    108 0x1abeaf05b60bfbd434b4e64b616ddb97d0962a80      1
    109 0x1b2dfbf142e6ca328c7a2d6735bfae3dfddefc4b      1
    110 0x1b906aaecf9d1465b26e7dccf84e6b999d8cad79      1
    111 0x1be9021726e5eac98bd3079b49a8de6e60744e04      1
    112 0x1bfc00c48261312f79e6fe39cea0545fc09c414b      1
    113 0x1c22e514203bd6157c7db84c616f3746bd03b431      1
    114 0x1d03e3b729213b4694008dd231b96c93e3dced13      1
    115 0x1d10b0166a761f269adf864ebdaca9be445ca637      1
    116 0x1df3381e0d11fd2684a7ea20dae4cca1bebcfe1d      1
    117 0x1e28ab2a1f6bff716121a6983a8ffad186d1c182      1
    118 0x1e36a3a437d6dcaf052a52848eec5e012934695b      1
    119 0x1e3b6cd74d67421f01c588ca0a22809dc34a4c13      1
    120 0x1e9dd2c5e16e1575abe8c8dafb663ac5f2a34ea4      1
    121 0x1edbadabe2a0835143b2df98e10e490028c7eedd      1
    122 0x1f01d99a90ad0c752e7765de29c386a169bd9e37      1
    123 0x1f056b16deaec42660f7c19ee4e976f5d14ed45a      1
    124 0x1f1a8244fa3157117737426c609c70c6884d048d      1
    125 0x1f76abd6d383f38ca40e79411f5840365b50703c      1
    126 0x1fcaa4d3912461b10442da1724080427c8e1a0e0      1
    127 0x201ce2dfea8cb17ae16f1731b4e202a55da2545e      1
    128 0x208ec529e1311e825a881849b2802764843cd154      1
    129 0x20c0e84995211cfbe87d4370772c9f102e22cec2      1
    130 0x20c742eea314dff601ea72f6bcc93beccf4cf41c      1
    131 0x20f88f6c9e3c4a313bacace8187e48e96e726de7      1
    132 0x2219718b0ff30bf9729b75c697a1752d02881816      1
    133 0x2282dbc88123851f31c20e3417571261f7fc33b1      1
    134 0x2283f11e7ea0f3f266e819f68f816d6cd469d2ff      1
    135 0x22fb48e316f78030c06560e48d6b2b7e9dd10a40      1
    136 0x237039d3e730dcd638464074c8ccbb95a8308925      1
    137 0x23f70c505334867b24e52edbaee69fb13bc910e7      1
    138 0x2412f6ef9048d6be243188f3a6f4c4ff76b4c311      1
    139 0x2488463e4c0269db94328f825f5e138d87f41d95      1
    140 0x24cf9a64049ddbe900a61bc37608b45d6771443a      1
    141 0x24fbadccd6684106e24065694ac87b0e98819235      1
    142 0x2522971eac605dac332cb72a0cb2681a3a718aad      1
    143 0x25665e8b95792c23d6167d2d0903fdc6bfef87e9      1
    144 0x25b50eed13bf9847f1688d1c3e450ff956bab478      1
    145 0x264928da5e53a86ae08f3e74765399eb6a707567      1
    146 0x26dd4199269bec9457f84adb029db917a910c711      1
    147 0x27303ffb77f7dfc70e4f41b2e1cfa85ca1893ee1      1
    148 0x276686a606e2f16dbbc72c6b412568c43e5b7c9d      1
    149 0x277664f432e9b606242fb18d9bf36623a9006135      1
    150 0x28418f6a75b3346ab3314e96c895f6096d6cf9ce      1
    151 0x289089cee85200e9e07d09bc266aa58a3266bf1e      1
    152 0x29059f184faa0a32c83971c8951829c8be27d3f3      1
    153 0x2913a9b54cd7b776f3a0fa6c515b7b967a8b28cb      1
    154 0x292d634189452e37b73ab087fc945309885ff5f4      1
    155 0x295aab6060a6f4a63414bac65155f1d3b73f3d0a      1
    156 0x29b00c1fa9ce129cf504e06223d45b249343d77c      1
    157 0x29f85e55429de927531fd525d2d4cb4f7088b76c      1
    158 0x2a27e5174b9a6ef1f8cd32504ba72ca94ed9135f      1
    159 0x2a9d14200b2801044e8275e62131a2fb4e213377      1
    160 0x2ac0b77652cfb7ebde8190d7c3e1a41e18dcc66f      1
    161 0x2ac4cd6cd6083ee0f64ba8737e2abaf020f942e8      1
    162 0x2ad6ddf08ee9344dab9c3cd0f37becb765b808b0      1
    163 0x2af80f0b83d496c173ef3fbc0e221890ed1fffa9      1
    164 0x2b13b15cac53d97003a42d9ab37768278aaf45cb      1
    165 0x2b62271a83743e52cd16f586fa67d2abaa63a1a6      1
    166 0x2b67eea410e0ee03e5c9e9283602e1ff3c42d2f1      1
    167 0x2b9a677752998afbc99f1eed6e4b684ad5c6765a      1
    168 0x2bdbd38b2a6e15a7e8cf407486a606d958a4496b      1
    169 0x2c2274bea55a4c93dec18128834674e364deb7ef      1
    170 0x2c3b3fea418bf2d82ab2036ee3a35cd9648fab7a      1
    171 0x2c66e16b36bd476e97c1c0341d48053adb6ce910      1
    172 0x2ca598ff2f679678360da6f46a6c3b791ed4cf30      1
    173 0x2d0bf022e326eecaf231eead1a360f6472b715d6      1
    174 0x2d11aae00487160cbc5b41442a0b14b5e1ac2d1d      1
    175 0x2d13271c668d31e0ddd4563727e871f82dceb41c      1
    176 0x2d3ddff14559a4ff350b766c211e9d6f92253a03      1
    177 0x2d6d6ccc36a2b018e06b6f3e049997885d59e0e3      1
    178 0x2e20fa4cd028a09e596cb9daa017c2f1972d9f5a      1
    179 0x2e78ad7472dbb0e938b008de443dcaa9f8abfa4a      1
    180 0x2e823e43872def2fb951337f81313ac95c3cfb1f      1
    181 0x2eb788ab43cc4d2029c122c6d768fa0076e064cf      1
    182 0x2ece4423c7843a1187f6a438c3884edb98f337ba      1
    183 0x2ee27de2451f089f63434cdf6ddc27c77690f685      1
    184 0x2f059371297852181bb3565f0d07378359f76690      1
    185 0x2f08948b482801894bf5fbcd0d28a1dd2e15182a      1
    186 0x2f1308d54056c398a5ce402e4f0792537c987262      1
    187 0x2f5c0e558e7f5b4fbc83436784d8d3a213a4f5bd      1
    188 0x2fb5bf565ca66786bf2c2b3f7f47b5e0c650768c      1
    189 0x307227ffb8a90c7962249ae43c942d4d14be6332      1
    190 0x3136b78de4fa7d2f70bf263fddfa5a428233c662      1
    191 0x316a35ebc7bfb945ab84e8bf6167585602306192      1
    192 0x31d8a64bff959fe12b13c070cd7d318eb0de9cce      1
    193 0x32417dc69162e493cae4648e6919e468b28a2f56      1
    194 0x324a26fc926c6a0511f47f329fcb5887440da303      1
    195 0x32781d9834745e0ee4c3f275c4637f895bb72e70      1
    196 0x32cc3a7ae3d1a1f7f0f41f564175dd4d4951c877      1
    197 0x32d9bdffe9ca0487c1154cabf7c97c73b3313396      1
    198 0x32e4cee1148b8f20cfd660d3f999dc8384b5a708      1
    199 0x3318bc0827558d805afa21bbac01d7f2bf6ea7e6      1
    200 0x332bee6129955e2b97cf2e4affccb123178a9aee      1
    201 0x334f95d8ffdb85a0297c6f7216e793d08ab45b48      1
    202 0x33631426077c380edb2fee05effae4e8f481b648      1
    203 0x33a34e27a81436ba9d79276406a285e89a8bd8a8      1
    204 0x3447d5ba1657c6f304c57b4edca2b02834211aa8      1
    205 0x34582e98f3f7f7864a03c388f14c311d445f5c86      1
    206 0x34850d672aefdea4908ce545dfe5b5e9379bf81d      1
    207 0x353d1af0fa732f6230ff4ab0eb2a6a3dcf4c54f2      1
    208 0x3557108b89d45bab83fab9cedd03cdcd4cd67b26      1
    209 0x355de1773d5c46f0e7b4ea7cae9a553bd3f9c150      1
    210 0x362afcd82d28a12b401a3688c49d8c5a6a52c2db      1
    211 0x364a021d0f8ba073d69b62682e09daf838153581      1
    212 0x36551ab96e3d386029165fff58a2a61179515249      1
    213 0x36600f4b7eb07cd40e431c66b5659619fd85d842      1
    214 0x36dc0ca6d2b5529ff2df460a1aec01b1f1e31768      1
    215 0x36f9d77959bf4e02bf589b59e77ce71ea9118edc      1
    216 0x3745caebb933de2d89a650a6ef83cd18e25d3df1      1
    217 0x37ba996dd7eb738af38af7839a17ad19cad5f155      1
    218 0x37bf130e77c0fd3f8f5a8d8725f69b3a90cd22c4      1
    219 0x37bf54a1baa1c555b1f89c12a66ea25f898cc8ac      1
    220 0x37fc0edb4991dfba1e461b037cb4fba615423057      1
    221 0x38429a952b673474a0fb75ce32029654cf62e405      1
    222 0x3882efe29eb18bc9e1a440c36f7a2244fd7fc67e      1
    223 0x3884675a87d175a2f68ea98a700f99a84ebe503b      1
    224 0x388947fa3aa839238cfbe6dd71ba2f892881aee5      1
    225 0x38e4276ea9c182d51c7f17d50ea99757278c4ff9      1
    226 0x3913d13611d2f128e3e1d3ee7daba53e4d37a2c8      1
    227 0x39256c222e2a16db63f21da9d8266fc6f95f45b9      1
    228 0x396da62ca102dc76e66cb9d8ad04cb505ef40605      1
    229 0x39845b526f52f7dac462046b8928d26a4ee216b9      1
    230 0x39c937dc6ec447caf3c148e848fddc55a62ab54b      1
    231 0x3a1b4d6cd46f89238814ae032fe37c9668cea2eb      1
    232 0x3a6cc5c1dc1d7adc56596049efad42fe1f6d2f28      1
    233 0x3aa6b39b0e050ea91faafe075b31e541f7a02f81      1
    234 0x3aaf754935c16ec0634b7b1016110fd037f991da      1
    235 0x3afbe1834d019440e3d57d175aab67f22dff5990      1
    236 0x3b0aa499cc6acde1d4a7433da6968d7bb8bd8509      1
    237 0x3b3790f972c7c71d571f9838319f031559523baa      1
    238 0x3b6622178d7231903af58a8f5ebae7a084de7096      1
    239 0x3b6996b7aa8371b0301167eefca33efca13307d6      1
    240 0x3ba524ddfd9ebc194b43e5b285c95c267cba8d14      1
    241 0x3c154956e13a035c30b58a07f2fae91822730dc9      1
    242 0x3c256a8e91f939304b83af92e5dca9a3e1824bf7      1
    243 0x3c4fbefc1f215d7caf8c393efa1f9d54fdc62f19      1
    244 0x3c954b89c32bf04c21080e719b7a96a5caa01ab5      1
    245 0x3cd446e111453e0c8613ae3306d09121c47d39f3      1
    246 0x3cf1e5ed41daa0c85753776b807a8b5b7cf6cde9      1
    247 0x3d3c3eeac517b72670db36cb7380cd18b929430b      1
    248 0x3d9928527dff1e6123f0d054bfcd328f96f704a9      1
    249 0x3db0b224099e56488d93f4e887a6bad88c39cfa4      1
    250 0x3dbb25810d96b6160319f2f2f64baaef549ef370      1
    251 0x3debdf063754d68c1047f0aa7e85c2b561ac6727      1
    252 0x3eae4371ee19a0b5782dcc13c2b8cc5a6f2575c8      1
    253 0x3ed0df6b6842e928172ab077e9cfd917e5b364fb      1
    254 0x3ed575835ce23355ebfb97dada3ea033166b8966      1
    255 0x3efa19acf8c33c96659f95be7660b62d6cdaca73      1
    256 0x3efbd8606489bb5ac5662cdbcccb261e3ba0dde8      1
    257 0x3f4373afdde3d7de3ac433acc7de685338c3980e      1
    258 0x3f50437069eba29b13bcd2cb0d4fad679ee03c86      1
    259 0x401f8b8348b8ccfdf04bbfb4e3e6df105c9e71e1      1
    260 0x40225eb6a9a2f6801b13aa9fbe30e5cf4638b2ac      1
    261 0x405ce2d22faff3a0420568c84cc660548070ce52      1
    262 0x40684ed766be1fbfcf987aa2e417abb4396e824f      1
    263 0x40b35cf63c0d2ca561affb06e91d0db818dde14a      1
    264 0x40cdc0e59044a114df62cf7e5800527ca39273b2      1
    265 0x40f7f7b62993383b1accfce003eb822be158091d      1
    266 0x411c4bbde302555b2a19413c4b1fe2520deda180      1
    267 0x4145a736eeb23174f3874fb9d3418f4acfed6de1      1
    268 0x416bf3ea73ca228605e0a9dea40f3ae7cf6860a6      1
    269 0x41bbe162ab4cbe8086c52d58708958c2e1ec3fba      1
    270 0x41bd1e75ec5b56cedfa18a223de19a0a0768f1f5      1
    271 0x41e467415de60e1fd222648e2bd66d6665b6dc62      1
    272 0x422c7e684f30e3c9a6bbc94225f2a3192831d822      1
    273 0x423463787ebf140eaedafd2f490db93c22a419db      1
    274 0x42662b8b6c05ccd61660fdf509ae1ba2eaa12e83      1
    275 0x42c04fbb7f4fd32e7b86ed045dd7fde016baaf77      1
    276 0x42dac2ec3e4231a0d046e7ba6053519f9db20b7d      1
    277 0x4331b8b8ddfa11ccaa63346f3046858b5722e664      1
    278 0x435084e31582277387f9b5615c9c34dbf3bad80e      1
    279 0x4396c4086a5473e1e686f6f51c764174ae22ce57      1
    280 0x43be4c4ddb9b7224862548c5256e2178098b9958      1
    281 0x43f4d6a5482ce5b05b48b16a1c2cb2ece35f8087      1
    282 0x44d7b16ad7355f1663237787de4f7797e8d12ffa      1
    283 0x44fe4ab0002a38089b51d4155b01e981b0640ae8      1
    284 0x45c7e8d02f9f4c519c33df73705e966b56ac0e54      1
    285 0x460589fa352ba741d22e6dcb4b38f1fefe7b85d8      1
    286 0x4656aaf577615ae1ddc33d4dbc023d6a0d902347      1
    287 0x4661e47f96883c3e3bddd8fdd8dd56a14648738f      1
    288 0x467bb605c50f8f815bdeef5c03ad82e40335d185      1
    289 0x46841779e1fbfdf7208b211f5b7f3cf68b59b9c0      1
    290 0x46f08d1ae78e2e7e08e5d6b24b17e7a97cbcce5a      1
    291 0x4739fda4950e4ef5ebc97f48eaf06b1bb80044de      1
    292 0x473e1279290009775d34f0dd181963508c02411e      1
    293 0x478e59a221ae43571b706789603cce0a87e769a1      1
    294 0x47ded080a4ef933e96e357d468363a8a0cac03f0      1
    295 0x482293e4c21130f069169e5d7e9c99f239c5ee8e      1
    296 0x483f8a805d9b56ce6b95ce5435c798580a2a2749      1
    297 0x4851aa7241d05ac0cc4ff69c0378f1b865ccbc7e      1
    298 0x48c4a95447332cd5b96bf3e069bd3f3d74ac8119      1
    299 0x491befac53aac201e97cd6f4c750e4a958d22d46      1
    300 0x49859ba307414a2197c0d301ff5014f9e0f4ebc8      1
    301 0x49ac0304f647fbefaeeed1ac79f9e43ea2323515      1
    302 0x49e3b59f49d55142f800182565812277c16abeb3      1
    303 0x49f5a86fac6761f7ed534dbce069c9ac6f9f9574      1
    304 0x4a5e5c333bfeb68bebe9a8516ebcc8c22febccb3      1
    305 0x4a72e14491582cbb89e354454e229a60cea01af0      1
    306 0x4a77d5aae67ac24302b3e14c1ced945950def38c      1
    307 0x4acca705f748b18b5005e1bc7ff69fdeae521f31      1
    308 0x4b2c7483f892af02d76d8f715e15eee31bb6e3be      1
    309 0x4b70222d33ec85c4cf182e42258a8f5ec7a9a6c2      1
    310 0x4b7cde268f0365ff7dbb2fbdf141c432a2f09e07      1
    311 0x4b840ebb364180d85479bc4ee1bc7c2fbb50838e      1
    312 0x4bb23a688e57b588c8b5bfcf832825207cdd4da1      1
    313 0x4bf8bf33d4a5c55f713c7cbdb7c4d4886a555274      1
    314 0x4c69e8c2a18662e208d3399261c74c14cb68effe      1
    315 0x4c7512f396536340f598735be36522ec7c61c82a      1
    316 0x4cab4c8f162ae7a6d1a91520506c649468ada62a      1
    317 0x4d4c374dd7d600461750438c74f18c2401d0bb92      1
    318 0x4d5373f04fad358fa26ea2cb2a08472493cfb089      1
    319 0x4dcfe2d2db47d1ec46a1945e6830f0ced5b45ee4      1
    320 0x4e0eb926340e40f4c496e249b477c802c0af345f      1
    321 0x4e51b3580dc76ba3bd529ce66970c28ca0debbf3      1
    322 0x4e775129042e52f9b0419a9faaf33a745fbdf2fc      1
    323 0x4ee48f917afc87dc0ffaefdfb9f3757a091c632e      1
    324 0x4f547d3de88cdba1a25ca7af6324d2e6e9025e1d      1
    325 0x50311306a4a0ef9ce1f0ee6fcd689282455e0aed      1
    326 0x50c42f8183ba4335d05eab73c3b9862ba251c8bc      1
    327 0x511531656cb4bea59d62f3b1f3bb8b1d40eebef9      1
    328 0x5172a03c43cb585456ea5b7e200bd18b11d354b6      1
    329 0x51c37532c8f230850b810b23fc54d93ed92a69fe      1
    330 0x525dbd6005aff707c92bccad1e593f99f6f14e96      1
    331 0x5267ff5cc69bb34ecf04554b6846be9d97c98936      1
    332 0x526b5f9b11ff885bb34681c900025541ebb54241      1
    333 0x527f99bbe073481242776e2cd2394d6a221e3284      1
    334 0x52c0e2b66ee82ea1c4dcf66f37fb69926ce864ca      1
    335 0x530f6ff39500fb9acc711e1af5a239adcbac2d91      1
    336 0x532e4ee74af0b765283353f4c6f37492ed14c98e      1
    337 0x5332e6ba67ecba01d7dc63e6d164f8caffb8a9ba      1
    338 0x5382d71dc469aa93d839c70dac22fe0713fa3986      1
    339 0x54021e58af1756dc70ce7034d7636de2d2f1fa74      1
    340 0x545789dcce672319656e858b4ab8d1cac49ce1c2      1
    341 0x5458b89ec91752e7b2e48f141104016dd5b0b3bf      1
    342 0x546eacc85b3f7a2855fe8f02bd4797aedf152a33      1
    343 0x5495a900e9d9bcb94175530b166d3af1e8b74e39      1
    344 0x549c262d21164b57376704813875d5bc558e5dab      1
    345 0x54bcc30f9f0d579d694c5842a52f4ffa2b858621      1
    346 0x54dbefeea01a18884dbcf86289a7e7ab58ad34fd      1
    347 0x54fa7dbdff6e5c34ccaa6975c38d1da9c46363f1      1
    348 0x5515290c6bab1e1f18183371d959e26617e09b0d      1
    349 0x559bb335d833e18cc23e0cea795b9241c76d68e9      1
    350 0x5652ff69744dde68c0fa195ee6250538089acbee      1
    351 0x56baae74802828d6ecec660d7b59fd1faf876f62      1
    352 0x56d8e8cf227daf1b331d096c336c04711918b498      1
    353 0x57045a139aa21497bf2df3bec82009769646a514      1
    354 0x570ff649bb5ae8e032a4f9456aa1ad74cc3f3e8f      1
    355 0x573ba15a705d7e1510f1b8aa52167a86ad9bc444      1
    356 0x57619527af4fa4b2a3fb2fefe6b219da1197443b      1
    357 0x578b076f33c021ca8ec8873be00c734559a99057      1
    358 0x57bacf5d4646c1a2b27fdbdb5d8cd4c8dc5d6803      1
    359 0x581177a4c82024c7fed37b6439c76a5970b1e3b0      1
    360 0x58454e733bf7d5999607c0e777a9a4df00d60d82      1
    361 0x58954c42e77393662721830d0fc9086e7e6b6673      1
    362 0x58aedbcbd0c07db3290d57d71777e24ad798b596      1
    363 0x590ef336b0617900bd61201cc34c3cceecd20381      1
    364 0x59cadefbcaa0493f0161f810ceb8b9757ad968a4      1
    365 0x59e713e24adedd084d351c654aaa0fb41335b0d5      1
    366 0x5a7e827c1cb0b6f9724a750eaf87e713e2d988db      1
    367 0x5aaa3bb7610e797e8900490ffeb64a96507e95c3      1
    368 0x5abbaae86c37e7f6c40107775ea0535687ce24f1      1
    369 0x5ac44973620104732ba6172d7fe4fef654662ebe      1
    370 0x5ad2f7ecd426196b3433c10f4850cfd947d078e5      1
    371 0x5ae425a51cd03f1a838768d33c501aaed9e9b529      1
    372 0x5b8b68ae1b1e3fd7039e6775b755e58e14c654f6      1
    373 0x5bc0aebdbab698e12fd33a2e133e6858fe6cdd76      1
    374 0x5c43555db810eca0f8704cd39d3630d6ecedcb7d      1
    375 0x5cf9d500bf608925c747a72b6df9043df9084332      1
    376 0x5d12b24e8b7c1a42bb48515afffcbf4ba2abd7ff      1
    377 0x5d316ea130c35bd2865df97efdcabfb2600f3eb3      1
    378 0x5d489d5b0fe652f29e900e105f263cf444fd1afc      1
    379 0x5d635b58ba3394c6dc6d89975003bb6be84eeeee      1
    380 0x5dfa3092be17f441f85a4b3218a7675f0efcf9a3      1
    381 0x5e5f8b2d9898017716fdd3252297a3694ab04456      1
    382 0x5e624a7ad13b5c01d547b1a95a386d1f6147bf56      1
    383 0x5eb32ab06e708e2af41ea23e784539a02d2a5282      1
    384 0x5ebf1aa6a2d7d08ca1028a704dd32346bc290a9b      1
    385 0x5eda3035025fcb8c8b01407b6f2e731561609b08      1
    386 0x5f052be5d7e4b85618b27772f3b100403e5ecbfe      1
    387 0x5f5c2d34ca56f35a1545c879d14cf0b4a590c3d3      1
    388 0x5f7fcb87bff096adfde013551e22a10537f0d175      1
    389 0x5fc1ebe4a23d68c887f51ea1333d4cc9c602f511      1
    390 0x5ff1ba50a18c36cd215e7ba9398484029e233a46      1
    391 0x6002ebe4144d7f3b0cd10e2ac0c0f357eb1c1c51      1
    392 0x6005ec783e48753fae6110730ea2ca62387883b3      1
    393 0x60612025fdaef4ed17db1f7b2d09a6949271fee2      1
    394 0x6075960e5676dc5fcdc1aee9c334343f08a960c6      1
    395 0x60a80a4f9c92232cea79b6cd93be391efc746c94      1
    396 0x60cc7c50b00c9fb573a216597acd875060cbaac5      1
    397 0x612337ff252d2050f5273414756f843f510b1a98      1
    398 0x617ed24cd137a30dbbe94b53de9ecf4b5ed8e90f      1
    399 0x61ec03f65955fb65eb33a1f90401b686d324aeb7      1
    400 0x621313798eb5e14429e6a115a5d348aa9875a519      1
    401 0x62378418109526beb9798082af510b7e08335b24      1
    402 0x623b97b0e64581e7f8f9e542c4664e3af3dfa811      1
    403 0x62b134b608a42e5e7b6f54640c6ee26a8f3c96cd      1
    404 0x62b74e95447915dd5b1d6c0c9b70621677c67ac8      1
    405 0x62f6ca6594049a5510c1eed5efe5bd667ee61013      1
    406 0x631d6440a916f97a714dd75bc99ff2cce508a1ca      1
    407 0x63349cf92db06a54abfcfe10e9ad41af2aa28dc7      1
    408 0x63c33be3be7ff1a956bad578daab8e00669acc39      1
    409 0x64ac3c97a72cfd6e1e3184caa26bb910caa0e121      1
    410 0x65634374ca9d6db13b5bdcffe029049a0a257a58      1
    411 0x65840a9471a8df058e2afeed473cfd82f16ccccc      1
    412 0x65a1dca1802e5728a692899b4706b58940e86d61      1
    413 0x6608bcdd242e412c4184841471366302acdeee1c      1
    414 0x664ac1a4bcbe71c0a0ccfb4f82dbdf3d4f395a58      1
    415 0x66c192a727377d5da4de47cca34211df3efbe9ca      1
    416 0x66c4cc16a62de4c33094ac55010523e56925909d      1
    417 0x66ec5ebf30c230976cfdf808a17fbd0add302537      1
    418 0x66ff8f288c413e61c8ee807088ce301c03076e6e      1
    419 0x676c22e13aa0df85695fb60a73ba651383ab2995      1
    420 0x67c6ffefbbade852adbcaa705d9ed35f0ae23be8      1
    421 0x6800304b221660f23f382cf5ec42ab2dfc66890f      1
    422 0x681efc30f97494f7f491bdc6acaf9c7782a26816      1
    423 0x6832486707c27a933e46d1a0502f294397a71965      1
    424 0x68a1422f9d4afacf6e8e4455fb3a4d86b6761492      1
    425 0x68c3a31fe734d7db7d046f461021790f8e5a95d8      1
    426 0x68c3e263574d185fcf0b4ce3f0459931ab7d3659      1
    427 0x69351b9ccae3957e64a212d1654000a88c6d0909      1
    428 0x6977b01b082909578ca61528029425ca07410e8f      1
    429 0x69830b5550e7260591096cab22c356b336b09f31      1
    430 0x69c38c760634c23f3a3d9be441eccbd2e50e5f73      1
    431 0x69e4767d4c63dbde5ed038eb52da33cf1ab9e4fe      1
    432 0x69f21b919283a735568a22c0b34316920f54b7ee      1
    433 0x6a474c30c9e503a5671132e4d7b8b20524571880      1
    434 0x6a6cf39911a3f77a563cc6fc5ad6e1eb05303141      1
    435 0x6aa759fbac7770956df0e27be35b7291f91a40c5      1
    436 0x6aaf3fae56674d3e1fc4ed4d9c2e55d312be4181      1
    437 0x6abdded3814046c7f0be895176596196d2fd6347      1
    438 0x6b1303ae7ff1890c087c13d1855b96ed8399a546      1
    439 0x6b254f8a91e255a4fb916501f896e15522799d82      1
    440 0x6c0deab3b4e2484b87363d697034ca390aabd4a9      1
    441 0x6c1952bd44c30e966c01990e45d0c56641d839ac      1
    442 0x6ccc566d56645b2867baa22db0eacc7532548e4a      1
    443 0x6d76bc513e91577bfa738d4c8561b6f4eeaf0db4      1
    444 0x6d7e2075dcc5b58509a759483481eeab99a89466      1
    445 0x6d89f7e733c6c5e0c24538056bed032ab1b522c1      1
    446 0x6dcb2a373398b17ef9b052d547a3785a9ac6985e      1
    447 0x6e0dfe55fbf0c7a9902aaca535635bf86a03f1be      1
    448 0x6e16d42f951c3500b7f21209eb04be3f205762f3      1
    449 0x6e76d547ad9ffb5ec6152e6a08ea5c939911cc06      1
    450 0x6e9b6b70f16901e855c6b69a32639ab307e9b433      1
    451 0x6ea742ba5f2f2ccf69f120fcd57071121cddd912      1
    452 0x6eb4edc064265d121d6d0253cae9fd76d32ee691      1
    453 0x6efe3f760e0c324818227e4c95c9dc482f32f942      1
    454 0x6f37713f852b816302addd3c2eabcbbe4c8095e0      1
    455 0x6fc9c61a79f995275c962a7429cfa4177686807b      1
    456 0x6ff75d3bf69d586325327691bd98c5e572b6bace      1
    457 0x7006507dabd3222a7c828ad9af0f87707b19748e      1
    458 0x708bd0f7441998b0641b734fbe9fd536cce60399      1
    459 0x70a5d1e52821f7a96d6b722c2ef6a7f943549491      1
    460 0x70f867254df37dbe4f516a4e6d279a92618eeb0d      1
    461 0x70f9f931d4644aa303a5427088233e9033384f83      1
    462 0x7151e5282bc1c952700ff070a933f17c644a66f6      1
    463 0x719f17ed85c9208f5fa7dbf4067bcffa6d63f508      1
    464 0x71aa6c4e87225fcae2df49f977c7fc0d1e8d2112      1
    465 0x71d7bf0e4cdb2da337d1c08d699e3242381b46c6      1
    466 0x7266ad236fbb8c98776a0c9b7cc7fef7628a668d      1
    467 0x729e8f9dd341417a846d545dba51de1f10795431      1
    468 0x72ecf5fb686b2acd3a57cf8f9961ed22c219c9c4      1
    469 0x7305c7bdb7e7ac29a4e8f01c3fd8121bda20bd32      1
    470 0x731470ff567eb0d6288fda4a50b7059e70a7b4f9      1
    471 0x731b3fd211c96d9649a365b8ecf3a7e03f9ae65c      1
    472 0x741aaea52e2d9c815c6c0b9512e956d7094a2933      1
    473 0x747b5e0f9e803006b3e9eb5da8de41bdda757df8      1
    474 0x7591807846aa2e640434c9573daeee8dd1d8213f      1
    475 0x762b605faff94e2deb42d50f4567450f284dd362      1
    476 0x7691a308ef369041ddee37144426381e8c758047      1
    477 0x771bf20e2067582c363461f6180bc1c938668880      1
    478 0x7729de882e81fe6db9751f51d3d70eb273d13639      1
    479 0x7730ee605313f9900860132baed51ebc4a61ddea      1
    480 0x773955091b539d722908ad84f5d55e5f5bf5281a      1
    481 0x774363d02e2c8fbbeb5b6b37daa590317d5c4152      1
    482 0x77a395a6f7c6e91192697abb207ea3c171f4b338      1
    483 0x77db37410eb0dbdf7f9dec1b196565344389b462      1
    484 0x78421dbc279879b09b5c97904a3dddf61538b206      1
    485 0x784cf6f74bb9121630f028bed83419a550008b2d      1
    486 0x7858f91c6752400f2234584a25006335be6d09bb      1
    487 0x786d413c5d08d580535843384f4e608e4fa4dc95      1
    488 0x7945561d43d9b7f582f9c250ab5f02c44c34d9fb      1
    489 0x79beb796fae8dd94604e47212fad950718bf4933      1
    490 0x7a337857b6494007fdcf65516c35e864b623e2bd      1
    491 0x7a40497782f2ad3ab9e9c08e67d5a95641110d52      1
    492 0x7a986b3e98a9cfce38e2070acc6b0e2aced31c91      1
    493 0x7ab54a55d63616634a8cd39ef617807c12a17526      1
    494 0x7b45836d84d9283cbabe63864e953089ff5ea182      1
    495 0x7b49f722ff9b35f920ef482474de0c856a3a5737      1
    496 0x7b553f7117a66eacc15ae02e1956cfd8d81c6b56      1
    497 0x7ba3d1c4f46516fb975fb012f7db04381188e907      1
    498 0x7bf0bd441f7a791fa7f785afd3229efffae009eb      1
    499 0x7c5156091271668b047a97842c79ed44dc0e60e6      1
    500 0x7d1d98e1e5e2fd74d97c87eaa0ef62011d3d2d77      1
    501 0x7dc1edfbb5ee7d7c8f1a53b59dcb6e6901928401      1
    502 0x7de58784488bec22798e1b192347d0eb67108d7e      1
    503 0x7dfa7a0ce4249c8083d91b01a871fac4a1492337      1
    504 0x7e1d52cf27224efaa06cca6fc58d556b189f2fba      1
    505 0x7e604316bf70f92987b0602d3ea6371c01fc5d0b      1
    506 0x7e97e648b6576187f1a4a03b194cbfd4ee76f543      1
    507 0x7f138cdf66bd4f2446be6dc3ea4fd65a4d0064a9      1
    508 0x7f7da962a340d1bd03dc547bc1b0faeae177c3b8      1
    509 0x7f96d44e48e0ac8d8635c6b97aaa8de9436eb8e7      1
    510 0x7f9879d66926105cb5ca3ac5eea257354d18f238      1
    511 0x7fdba87da6ed499f19fccd1b8072b4dcf6991ffe      1
    512 0x7ff9a10ee542689f17da368a67f9092f5a7263b0      1
    513 0x802ac6dba5100397f2af64936a3fa0aa426c1b32      1
    514 0x8084b734329d203e65e08526b7ebe3b7895a1aa2      1
    515 0x80a15cdb60035f96e447392509fac8f9bc63c8bb      1
    516 0x80ad90c846f96ad067f88ad53c2b3fa26dc648a2      1
    517 0x813fe2a70832697d21a836abfffa57f18dd4c6f3      1
    518 0x819969cc76204bcf4f72d3eb886188f5937e45f2      1
    519 0x8204f97d4a654ffc4bf5d3efedbb443ec35db58d      1
    520 0x8205d352e13db52a124db78329c35773ef3a26e0      1
    521 0x825c6943f9c16eec210d65f061ed109ee06c8639      1
    522 0x8273afafaed9d1126fbf634c7f22aed716f28e17      1
    523 0x827af0562c9dfcc3976d091d57f6cd3baf05800e      1
    524 0x82acbee0e21ca256f5cbb32b7b141a774457569a      1
    525 0x82e3f60e5280becafe7c0e115886b3f2c614e328      1
    526 0x83057aa552a731dd04ccfc3c4d1642120f70310e      1
    527 0x834a9484537c2760ef6d6e6736d1ea800d67966c      1
    528 0x835ba67f633b57e567605dbda7912118982a32ca      1
    529 0x836ca162d001ca23f40eca50dcdd21c66e350f64      1
    530 0x83ca8758ea3ed6ef6beaf7055f0b23cc74624492      1
    531 0x841d1cfcd74f2022aa5d7a831ecb0673afe212fe      1
    532 0x842baa3c26d0502ce4269b999a40d09f7cb4cfad      1
    533 0x843d2c4abb459a2a1ab3ceb2f877b6e8c503571e      1
    534 0x84746f954094c53809b18fac2ea5ca4a426abd62      1
    535 0x84a6d7972e984b03beb7fa0a53f952d3b7542c00      1
    536 0x84dbd0cab0b53283c8cbbc1e2f08302c685f0c19      1
    537 0x8529bf06095b39ebd67ab1a94c4802c098e76ca4      1
    538 0x85518daeb3ab2b95dbb0d76a3b07a669d8191e2c      1
    539 0x85869daad348bee8a5c8403e72f4b1037b6286d1      1
    540 0x85b1fee378bab503fbeda9516c405d3f97dd9002      1
    541 0x863b22773daf094656e7339bbb844514a7e20555      1
    542 0x8640ae66adc5f24a957ee08b96dd681a4a668c0a      1
    543 0x86ac4ca5b5f5774cdc5e8f16c4be71ff45fb20e8      1
    544 0x86cfe5b9d71a61eb489fb323d8b839d89983fb37      1
    545 0x875aa6e369f3724213f8c351668f2b00b938eeb8      1
    546 0x87619b5520de0e36355a6473767201fe658bdeb9      1
    547 0x8798a04a2fff6f417c1bee147acebde8ea759526      1
    548 0x87acf8966c0d161e993274fe77ee05d3eee01a6f      1
    549 0x880c8fb6a59fb13ee7894b4fc18b7cbb529ed24e      1
    550 0x88776a976dcfe5527ecb598bbbff344d86d75e26      1
    551 0x8884988d6d493a9071dfd0ac365a939d4d7eabca      1
    552 0x88918dc9ee100e8d4409f85e560115c1cf92c04d      1
    553 0x88b60f0f3e8b4c7400eb5205a9d2b0676fdedb13      1
    554 0x89085bfad917cd50d19fbb58b8c0b93b78f4023d      1
    555 0x89117915810d3fa4aa272485e39a20850b89bcdc      1
    556 0x893ef19b7c39acbf74b4ea429a7abd6294eb12cf      1
    557 0x89b4c4af005af1fdca2f037fdb0e5cba96e0bb27      1
    558 0x89cba006791a54408a2b5c0361a2cde8883f4215      1
    559 0x8acd9cc99d622fde692f0f6ebb6c840c41d7df08      1
    560 0x8b1fb7a40e19ae73c305020d4ddc1a010aa857e6      1
    561 0x8b432d6a5484338172fcd4d5c40c9a22e20fa991      1
    562 0x8b90eb28dd723fd07f31d1a6c867dcca00f10f1f      1
    563 0x8c0f252da419f19e14a1dafc3b15fc8d811a1f52      1
    564 0x8cb1597136d2cb67e217dfb83959ac4c0de29867      1
    565 0x8ce69f3f36465366de7dce5fc2137ebe37a24122      1
    566 0x8d6b39422b220d40d1980430b601d5b74a7382ab      1
    567 0x8d9468b4a3aaaac721160458450e409a662c6603      1
    568 0x8daa60e0ff964bb4d1f30fae8791626229c3f63b      1
    569 0x8db71dbbc4685540ace14d9596f60c85d9e9bcd2      1
    570 0x8ea1b83d63851346dacd88d72b30219af00ccade      1
    571 0x8f0787223e8e759ed9370bc35cbd8d7f475b2c03      1
    572 0x8f2ed3172e9c7f352a647f542541134755564e9d      1
    573 0x8f3590efabc2ec4d3d9cfe5b0e8d992af3801f94      1
    574 0x8f4931cf0e59d4d5fc5e5211d15096b16e319127      1
    575 0x8f5917b6ac2198563c848f0a6b486dfc8f0bef86      1
    576 0x8f8c5a62d0cbac51e6e6b72703be47c01a0102fb      1
    577 0x8ff6dd33b71215c952bcb24f8ecc4071f6316cca      1
    578 0x8ffa8d00db5df56a20c30b737c5efdaabe140df4      1
    579 0x9045003fecbc67280240c07b0df300d64d09f7da      1
    580 0x90477d50794f34d4e3e4749d641cc173a7c8dcfd      1
    581 0x90d8fa933ae8ceb753e25c57ddfa75d610823de3      1
    582 0x90e96bb8955a2e19d5659c68cbe6fa43c8c4ce2d      1
    583 0x912064700cc53ca921992a739e4510f9e060edcc      1
    584 0x9130b857a48e541a8cac603440cd7a3e3a6a249a      1
    585 0x9197f339cca98b2bc14e98235ec1a59cb2090d77      1
    586 0x91ad7e7cb734af5eca9baa6f4803411fb6279237      1
    587 0x91bf76c99d49337a797240595758403446cc0e4c      1
    588 0x92314e2ed6853b436e92d54b22a55d450889769c      1
    589 0x92ac9be044aecc6534e56024a0d97e91d4c35136      1
    590 0x92b1b677728e36734d59067a4ce53e5bf1f3a2ab      1
    591 0x930922b1114ed66734bd773184667b227520f43f      1
    592 0x93654b2e823195f17e559146b8b3e1990dd0e68a      1
    593 0x93e184a947402238c1773ced37ed62360ec2b678      1
    594 0x93ec3017b0ce86de3390d431ac1c6bf4a939ddd7      1
    595 0x9414ab4a67eebc7b9c7c3759152fceffb1244e5a      1
    596 0x942248e8cfe1f3636d77eef3f874b460e97708ea      1
    597 0x94395b9f171b6cb4530e582df14df6fb58e8ef5a      1
    598 0x944ccc0cd1d02fae13e2781c192a057b18f15c2e      1
    599 0x94b764bd9f21bacba12487c4f67b6baa7be3a538      1
    600 0x94fb300b3796bd07bdda0f3bf94cf17952a0a11a      1
    601 0x954e8561d16c7e380e789e618a227378de9b7571      1
    602 0x9571738fb9888c4d0f5ea84beb8477ebb026f739      1
    603 0x958f3ff49e0bb6591894d0e25dad67bb84c13cba      1
    604 0x95ab395ce362516f9cf70739eb418a5e7c5fb8a9      1
    605 0x95cff92d71f55b69bdcb41fd33f3c6a323debccc      1
    606 0x9604b57ef6601a529fd46c25fe5b4c2b40345922      1
    607 0x96a7e4d9796ec600e8b42cd1b0adb71fcf91390b      1
    608 0x96f123700044e61445dcae6df2de78dc754504f3      1
    609 0x970c5cdaadc92dfd4a2b50ab1afac7e558628f4f      1
    610 0x970dec26ba135ac56e498d44a3ee1431a01a5a46      1
    611 0x976828b23af10f75e94133b15f31ffb054572c7e      1
    612 0x97b523151db5d61623d01df0fa5625393ddf6fef      1
    613 0x97c99aa33df79cdcd8799fd9e9c4692d90358f8b      1
    614 0x983d345eaf205ef51f8f3e25458010dc70259d26      1
    615 0x986a0f1c33b06716a1acea82a761d36ac0866dd2      1
    616 0x990450d56c41ef5e7d818e0453c2f813feb9448a      1
    617 0x990f124747d946a083ad3cdea043c34c0687c43c      1
    618 0x996b1da8c7488b7ebe936dfb6234a362e0576b6f      1
    619 0x99d741d8675c5eb5c19d1a2d28da523fed6c014e      1
    620 0x99dfc1d9524989a97c1c707577ea6f28edd2a89b      1
    621 0x9a16b7f76153683a1f8d034732b00e56ef468adc      1
    622 0x9a1957b0c29d647874e34ad4718bdbeed64c5b47      1
    623 0x9a5f58c841ae3342f65c5cc616a886923015c8fc      1
    624 0x9a65f025427f84737f068b0beb8eb8b0597cee91      1
    625 0x9aefd2dba613d52bd3bb6c0071879ab88bac46eb      1
    626 0x9b5929f1a5469877c50487fe7f64aeeb1b4713f2      1
    627 0x9b7ad9cb6207707cc70838c459d3185ddb4f6d00      1
    628 0x9b8e074fcea7c7b27aa362c503df6423691c31c8      1
    629 0x9b9cc63626692b73b65b315cb586a7b543d3391f      1
    630 0x9bc38bf1d3f1161f831e45ae8295090eaae8bfd9      1
    631 0x9bf67e402053471b186428a5f085613f5e16056f      1
    632 0x9c213bd7e10a3b88172b300fd5b6953cef579c66      1
    633 0x9d5315a9d07aecb453b1cdfbbb053c3fabb9bcd3      1
    634 0x9dee7c1e05e031b429b187868c034bb11c55c046      1
    635 0x9e6e058c06c7ceeb6090e8438fc722f7d34188a3      1
    636 0x9eaa362844dbe545b63b8c0bda90fed9ea1f5625      1
    637 0x9eda11e56201e6dcac487876d925e4af8c43ae69      1
    638 0x9f0db4aaf3970874cbbc4e96fa1f00fb41af3395      1
    639 0x9f12842ad1f0b5d39c31515ec9baa96cd3857236      1
    640 0x9f128b52128027dd6ea9892f741519979d36fa34      1
    641 0x9f24c1dea4e0c9a23728b86447326cf2cc037d22      1
    642 0x9f6821b0b9b377c8743852a4fe222cd2cd63c75c      1
    643 0xa03f864ac19ae8ff7c6dfb50205e5ad37a7f27da      1
    644 0xa0a3324d40e08b4c5512b37ea9044f95fd1fc01b      1
    645 0xa14487fe83e390a0d238e0454cddf9a429172c79      1
    646 0xa159ded56319551e264d24782d290958b5808f27      1
    647 0xa1a1ea2f4ef2c85d24de68057939c1d2daf4a131      1
    648 0xa1a9fea3c41cf1dc96926f3874d83452dadead8d      1
    649 0xa1d98e07875c1ddb018ac42e2c54f47cf4dc5e0a      1
    650 0xa1fcce708c79b74d0652f359c87b7eb6df0e6ab1      1
    651 0xa2188a2d67fb39e386def087f43b9407aabe5c0e      1
    652 0xa29d054918fea8c4c703ebe1ce734ca065d732f7      1
    653 0xa34e4524019ce00dcc2cc0dee4c8f5cc2f715d85      1
    654 0xa38aafa588bb69e51bf5797a466007713e461abb      1
    655 0xa3aa5a2f2c7fd180f8b429a284d5467c8ccda11d      1
    656 0xa3e01870bc0bdb6a457a07dd7a38f1d0a8acbeec      1
    657 0xa4321358bfe64f50a5b9686bdc9ed20098476c23      1
    658 0xa46664ba7222221475146c6710c812741a6c8bf5      1
    659 0xa5343d151fc822ff8e7ff5db0930e994356b0372      1
    660 0xa5bb7e1b8fddf97ad98b4982593573dc4b22248c      1
    661 0xa5c01e0235363082e3bae5f5f39848ae5bbe6796      1
    662 0xa5d981bc0bc57500ffedb2674c597f14a3cb68c1      1
    663 0xa65b73c58956cdb130e60cb6ebc54032a6773aa9      1
    664 0xa72dd288b659c8f9c8279751b25af28a2b4e631a      1
    665 0xa72f51f5d40a9b218e72695e852c8ed888015839      1
    666 0xa733d56ad3a24161b48034b058e500f77bba69ce      1
    667 0xa738ac1b5f9d7a0bdfbeabc12f528fe10b7a4a6d      1
    668 0xa7ad4296d9f41434d1982a4c4cd98df88121858f      1
    669 0xa7b7f3550f9b9611d12d6cf9e58fdbe63b7c4f86      1
    670 0xa837c1e2b87aab4d28fee399c6593b66422fd5d2      1
    671 0xa873c76320f02d0e702ce4fa07fc8850fd8af328      1
    672 0xa8ce0d3c2d53bef414b634107906cc9c6db5d25f      1
    673 0xa8d4520210f70332b1f183cfaa33a4c49c2647dc      1
    674 0xa8f0a8bb575a67dffa46d0a1a9fcd02311b9f65f      1
    675 0xa93ba46e62b36d3cc3e406a69b1f9fdb17429f97      1
    676 0xa9b409f82e31f212f3886afad0bd3ee3e39cd5f0      1
    677 0xa9c28eb83763d33ebeb279109f1b3913774fb01c      1
    678 0xaa2449dd4407dea129615bc6ad12e16e5e46cc35      1
    679 0xaa570bff9aa30aa540b4ea28365194ddaa9a72f9      1
    680 0xab12bad50fa40b37371331c00fb5a035dc5624a1      1
    681 0xab273800f710a9f148e844a20ca6f1bdbde2cc59      1
    682 0xab35ac8661e0011de2b4c20798410c015eaaf598      1
    683 0xabe9acce140a2d338a5df5f08b6dc87366e34d31      1
    684 0xac6caaffd6473d6d253bbed8d7abda6faa827be4      1
    685 0xac81baab1585a1f3d3a4255dba63f0e80118b080      1
    686 0xaceddf103cb1fb2cd043f5c3c090740066e673d3      1
    687 0xad4d39612f84636ccfeb4c056d6bce7e8be703d6      1
    688 0xadd403e94985eee3108e16987eea30c10e2df2ef      1
    689 0xae573ca4bf977ef34aa1db19995d674efc065b90      1
    690 0xaef4baa66318f4821402998272678274b6123fcc      1
    691 0xaefc4c562002de306714a40cc0a31b86f7e79077      1
    692 0xaf37c6b05098645f13044aebaef4048f6568cfb4      1
    693 0xaf41f4d3923d0e7029237a86341453cb819cb9f0      1
    694 0xaf7d3f754776e42452387fd0f6575b70cf2f4990      1
    695 0xaf94ab95b2a57b8ea48ebe1f9030b95a9b30cd54      1
    696 0xafe7c8c46b5154217d65e98c97501d551170f5ae      1
    697 0xafef17e03503b8cbb14d189044e2eb7e82ffc651      1
    698 0xb00b9f1de05d36e511515970aff6f06a7124b00b      1
    699 0xb00ce961a60f7c3af00f78219e8cb4ee2d66b00c      1
    700 0xb03f5cc470484d1586d0a113d652bdc07c23598c      1
    701 0xb099a3c3043b262518d9b6cd4d5c9f589b15e2ad      1
    702 0xb0aa25d9e1d77c1df609c09bcc1fd43c40800469      1
    703 0xb1028c83aca7ae2d3c433204529e4f9e7619ec0e      1
    704 0xb12721aaa8a74f652e3c8856d28bd6f2220a1bea      1
    705 0xb13dfc88a37e32da6326d03ae528c203da941e37      1
    706 0xb17fd4ed9f04b7a9f40b1fef4a2308cbb9c8f0c3      1
    707 0xb25a2d4605ac6be241c70a7f4829026c19ad557e      1
    708 0xb27092983cd5a2c44e28c934eb3eaa4b7216503c      1
    709 0xb2f9a575aae59a1fbc58d62edcef41e25a2e08d6      1
    710 0xb37c689d218824c6ce6779518d93d55b8ad01c30      1
    711 0xb39a6b4cb933aab3c30da765acf26c4c00a4bf11      1
    712 0xb3b93e879585a49390a149cd3106eb076c2832bc      1
    713 0xb3c3b3038bd2231b7b61a510102b459e694690a4      1
    714 0xb42e100f77afd154a0895fb30f830f7f8081735c      1
    715 0xb5363508582a631413b8b9c99f836f4b673988a1      1
    716 0xb53f172a2cb274975ac804d0282efcfb1ebcafa3      1
    717 0xb55078da3438ca8e1babc92f0b5e849c798474e9      1
    718 0xb57736df73b746d8a522fdeb366b3c08026c4256      1
    719 0xb5c3814ac0bc5177a22593e3b3d196dfe8954855      1
    720 0xb5c42ea3d0c9d300171f3da51f4303205cb3d8fa      1
    721 0xb64fed2aff534d5320bf401d0d5b93ed7abcf13e      1
    722 0xb651ce75b85335960e7f64727c2517856fd4f7df      1
    723 0xb78a49b86a595c121be84ba0973e59d4bd9d9c60      1
    724 0xb7fc789d617bfbc1dc64011e978ebc767f89f98f      1
    725 0xb7ff8fa6ed3b1da7ffde127144481a45580ab1f5      1
    726 0xb835367ae1cafcea58a10a51b17fea25d16c3dab      1
    727 0xb89d68928ecae680c0a5b0444e361a5b62f69554      1
    728 0xb8a2f8bbaa19923a8aeecca91f88e94d7a4057a2      1
    729 0xb8b3de89e7660163c51fc13e372f77cf0956bf39      1
    730 0xb8dff5f476bc035653c4463c7374ddae6ffa9947      1
    731 0xb910e4caae9ebb64680e4f16980bf7c0d595bce6      1
    732 0xb948986f15986165d9694a60a00d9e163e9763bd      1
    733 0xb9cf9da8c080ea2491cc815dc1a81d2dca336a01      1
    734 0xba2babda560eb00b9ca2c8252546d819b1cd4855      1
    735 0xba4ab1412354ebd38d74247e63955fd7d2785088      1
    736 0xbad858a0cf09f210fcf35cbf83569178879b47f2      1
    737 0xbada55eaf935f820cda957d981cba3a2c94d1cfa      1
    738 0xbb214c93187e55cb74d24da8e44717aed3045cb8      1
    739 0xbb44530e21b3a5aaf0c86ba10d605e1396badd88      1
    740 0xbc762401f2834333970f3591fa2d56d89aedeca2      1
    741 0xbcd5a68725612976bec27c1c194728206974afb5      1
    742 0xbcfd321433f3523d9396cd2c8c0fb2828266487b      1
    743 0xbd156211059c7ca569328016f608be92764f5525      1
    744 0xbd6d0b8c22adb7d035beaf9b7d82c4001646417f      1
    745 0xbdbd64b55aaf5bdab528b0725f33a19a728b11d7      1
    746 0xbdf407529a6ed5f14736286699d58ec72b337315      1
    747 0xbed2cff507ac477d93386b3ceef0669f1e090841      1
    748 0xbee1f7e369b3271088ed58bf225df13cd96d32d5      1
    749 0xbf0b2965cd0e1cb12f62b7e331ace761a82aed76      1
    750 0xbf3b26680cbde3d1d99ba3af53208d4ab12907df      1
    751 0xbf61bdda744038f20945baa544caac35ce9dcbed      1
    752 0xbfb1ad3ecb4c87d53cb6a51c8618a39d15fcfe04      1
    753 0xbfcf0663ec8eabd2090fdcb36534fc8352bdc042      1
    754 0xc038353e954ed66070ab780775053bf6734eba02      1
    755 0xc0402d5f435248d0b64db1bd24aea06379ec1ee7      1
    756 0xc0d381905154ec333d06654687658d41b0a04008      1
    757 0xc0e3035e868cd6214005ca6c50f2936bcff38751      1
    758 0xc117c92319376a0ab225b936f2eeeec8d387c951      1
    759 0xc1d0235c61e9fc6c0d651925dc1a536f2be21623      1
    760 0xc2065e8c845448b6fd7a3ad168918f2ba6e9d45f      1
    761 0xc2688e73000b200fc8bb51fbb28a6ece57e1e051      1
    762 0xc283e22fc5858bc799094c4c62e1f9cf02bee306      1
    763 0xc295d8f3b0b766a424fde52fe32ef9db99daaaaa      1
    764 0xc360f94a2c44dd3ea62c50b856c4f1b8de89d6a4      1
    765 0xc389d69da36d21ec2c3e47638585b4146875c88f      1
    766 0xc425149af54cf0a493f8b015f5e416e41d9f4dc5      1
    767 0xc45fe02789bed6565bc0ef8cc39dbecc8fe3ace1      1
    768 0xc47462dd85d9d2441a132fd348dd0c7a1fb92c2c      1
    769 0xc4ae6614bbfa0817a9edaf68b26f8b3eca0cf0fa      1
    770 0xc4c15f8e015d17d97606a22148d36a439dceefb6      1
    771 0xc4df46bb8fbb85ef865a98731e82e3569ec5e32b      1
    772 0xc567658c9d31dc32ebf0d4205513532122df4a79      1
    773 0xc60aaf878595cc0a2b6cd38c240c1dcca6cd8eed      1
    774 0xc6411ca5a80deb7abc0827da82e2ddb9c906614a      1
    775 0xc68eff7333b3b4b8658c8b897ec58f451e8b1f17      1
    776 0xc6a885eb1fe22a8b63dbc3435ddeef38fb552f68      1
    777 0xc6e54b8871670b82bce9f097f4c1ce0184402c07      1
    778 0xc6e90a981ba4c7df0a9008b770cd34f41a5aad0e      1
    779 0xc7cd7d2fc933fd6346b58675adc6386db9f6ee84      1
    780 0xc7f5db6c0e270f57d5114fd0b9acd6f0a28c09d3      1
    781 0xc81f004554857798f6f836230cfce8f18aef5e4a      1
    782 0xc8581ba5bdbca1efc59026b434b00c542702d963      1
    783 0xc89a23bd417a2a119c6e1ead10a18888a324b28d      1
    784 0xc8abc105a6278a51b6c41f1c167f5ab20f577453      1
    785 0xc8b28b6a310904c83d128be83b3797073b5c5302      1
    786 0xc8ba4aba8d4fb51df85533cbfd9809f0fca8ccc9      1
    787 0xc90931ad448e2939af1fa6f3ab75227cb993a707      1
    788 0xc91444fa4c5809e07d9541a9671ff9a9dfdf3b84      1
    789 0xc93c7f71581dfeaab59bed908888dac5689f312a      1
    790 0xc94e90a34f2d9aa2285babc2dcb6ef56229bd652      1
    791 0xca434cf098aeaecdbca8a813177a3f9d2a901b7c      1
    792 0xca50cc37abaa58d19e3a23ccb086f17f8384cb3c      1
    793 0xca8782ca0abae0471fadd23d75e4375326f34b53      1
    794 0xcb0937777d9e5dd21f7439ec0bd74e9c96f01ce6      1
    795 0xcb1edc00d120c55578890a118d4f3e2490cddddd      1
    796 0xcb69c5838afd73317abe43ea23574ddf7a6e51b7      1
    797 0xcbacb882c8e84b472a7dbcaf44fd7e0e8ea5e5cb      1
    798 0xcbfc85f401409b9a61456036a9dc745566dbadbb      1
    799 0xcc48857242141cc7f4f8555f8a3d5bed03556c19      1
    800 0xcc4c30600cc1a3e8fdc866ee3c6f73dc9248428f      1
    801 0xccf08c3959440736a4ebbe384ee8a0925137dda0      1
    802 0xcd2b14986db9638fb7b9bebd8678dd1c011bf4b5      1
    803 0xcd71a3972f7a21dd4ef9febac48bf96ce727e066      1
    804 0xcdb1d02bc1da07e8d166241d339e645d1466579c      1
    805 0xce013eb11d874c8fa60ec9007626e154f33ef2c1      1
    806 0xce8c3cba82836bd0f6ef07ef5d7890f6ce52f43f      1
    807 0xcf16ecfa0d363447b5644744fd819e04648fdbb7      1
    808 0xcf8d75809e9cf0dc2d4d0edee65e87eb8005c3f4      1
    809 0xcfa98b9b7d61c26720eed4b00bf27530b5515eb1      1
    810 0xcffeb17f67a226f900d29f441485fcdecf0f350d      1
    811 0xd0e11b6081bba5d8e141ba1eea7c4b36a3d749fe      1
    812 0xd0ed643099041b805adec2e2af3329c06a20ffe6      1
    813 0xd1789248d74123238891201180ba5486e10c8170      1
    814 0xd187b2b267522fa6ba950db8090741558106ee4a      1
    815 0xd1e3e1eacb93eec2c1b114a07f7c19b6220a8fb7      1
    816 0xd1efc9fbe559607d17b6313bf2ad3dc02b612fee      1
    817 0xd20432412ee610f60046fbf00f099fe8d2aa6ee0      1
    818 0xd2329f1b3e281ba2603fd5188b44606446ac46ce      1
    819 0xd2c61e9f43ac659cd4044b776b4109a0d35f15ce      1
    820 0xd322f18366e7d0f3495e38efdf6009933491e252      1
    821 0xd34f5fa271c2c7c80682833988b5bf0fb234204c      1
    822 0xd35dcbef197c49f1f1ff84e3bf1791d987fc6e2f      1
    823 0xd3723fce4acaf50b9db8e61391c083703e912358      1
    824 0xd3a74341adac943de6600468393bb6ca4431a7fd      1
    825 0xd3c01a4275a54c767b078f207f113b424ce02f09      1
    826 0xd3f9aaf681b5a7ec3a513fc5a813c136f581c365      1
    827 0xd40d3099ff48ce9d40544b264d7a0591d4982d6e      1
    828 0xd422e83efaac0211b3837b95e256a986ac035808      1
    829 0xd45e9d8774b30b20f23dad76bde25515d667b1ee      1
    830 0xd497297d11b9db122c28eff60b489b72f7b71849      1
    831 0xd49f82b538863fc28f310877403f2d98bf63e310      1
    832 0xd526ebc929877963ee14e984fdb1d63b0fc2a096      1
    833 0xd565e9de39844c30c8e976f54431456d97324e02      1
    834 0xd5a498bbc6d21e4e1cdbb8fec58e3ecd7124fb43      1
    835 0xd5e77df1262e6329f8307c896f15391682ae950b      1
    836 0xd65b77d0b66f9f56209fdf49f30f2cd280d6749c      1
    837 0xd67d2edd07dc770b6e6c737190e09fde5d252d9a      1
    838 0xd68543e607bb645f4809e8d129d5876dd73b2032      1
    839 0xd79ec61eae2e65dc0dcb7124a5d5b557f28e074e      1
    840 0xd7b9cc24af9a18f947a98b571f2ca89668e635c1      1
    841 0xd7c516529a83dfa7d01421eb5162ef6486970dbb      1
    842 0xd81ce8e89dd987c8ab630858c8f1e9df14788c35      1
    843 0xd8625f9178a259b14f37edd79d11100dd5bb1c2f      1
    844 0xd92dc000228667c12888de666f20c04ac97c940e      1
    845 0xd9c5935288faf9c3c3c0c95c012bc1ea20cdd8c5      1
    846 0xd9d3ec2afb7e1d3fe2f128cad3de34b6f94524d7      1
    847 0xda00a06ab3bbd3544b79c1350c463cab9f196880      1
    848 0xda240316402698ddbaa9594a81246f2c8569d2dd      1
    849 0xda479b10f0eb048018d62986ab70d02b3909e20e      1
    850 0xdaf4eadcc23fcd123f48f37850099df0e0ef9545      1
    851 0xdb18e29e4adf828f35adb2161eca0bfc3e77a9b1      1
    852 0xdb1e949301aa6fb96ec5a89d9a2851c19f3a3077      1
    853 0xdb2935c53ac1e3215f66097e8eadfb468fd54227      1
    854 0xdb4acc75f4e71da313d2c85ef5cca28dbaf3c43f      1
    855 0xdb6878051dc03268e3555becb48de78a137b7641      1
    856 0xdbba45c16f14b33c5faa9e142065c7255a4db0e0      1
    857 0xdc39de24fc3023daf915345726714636989c1f75      1
    858 0xdc4841edb3337a0f10257dae50c7b6fa87846382      1
    859 0xdc655c00d3197412af3d4ec85b6af3e8c561ebbd      1
    860 0xdca649392b3a6de6a0d733fe5a5071ae12560f39      1
    861 0xdcd9eb477943518ff5911d405373cfb3b2711ff5      1
    862 0xdd417b717fc0e3357fc220f77a047430afe9b1da      1
    863 0xdd61ea700c479e760bf2bec43ef7f79a02874c1a      1
    864 0xddf10384d6d32002137a778cf8959dbd0c1094f4      1
    865 0xddfae4d2eb8c05756da72a4e0af0ea6097384db3      1
    866 0xde3f927b523741583e392e0205a610a5d107f756      1
    867 0xde4f118110c2e20e5a8df899861ef5aac3aad287      1
    868 0xdea5ab6e60bd856c0b07a8531cb81e882abca593      1
    869 0xdf08d187f37d377a9a511b64e8d865ab2aa28539      1
    870 0xdf65b4a9db027f83b7be731d36ea3bdab2f6ff0b      1
    871 0xdfc58352dd417d3bcd899f4f925b64d9b59f7889      1
    872 0xdfd628d37c04819cdb9358836a01fb1d6494fb7b      1
    873 0xdff973cde861da3cc462a8e244297ec9cd746ff5      1
    874 0xe08f5c4c9347015907b01a8dc108942ba5827e6d      1
    875 0xe181348f432925161eabe888b459bb4b95fee63f      1
    876 0xe1935aade104acc1a342702d3e53c60bff365a6a      1
    877 0xe2c945b5093f6314e889e0c5b260d86457c47301      1
    878 0xe30da40796a21db992f8c18474e1b51facbad24c      1
    879 0xe37ea33b3fea6bbfdc9501a88a4d18a23f9f641b      1
    880 0xe3845e7cfb050331640daf67e555f1cf7a497818      1
    881 0xe38827de1da26e1f930adaad79f3b108c93f2063      1
    882 0xe398bba9f748415f27ab4715147c295dc2cbfb9a      1
    883 0xe39e244e7a89e9c6a1d84f5b8e0037bebee4b951      1
    884 0xe3bed8b68a15019d92fb780c5742f6661c54dee0      1
    885 0xe4330b03f40d51b00e484e0889d403b74d3476e8      1
    886 0xe54624aaf3f3dc2f9c8c9af4d5d9fbba41415e65      1
    887 0xe57e6e5c31282d666e3a51bcb825c02385953e2f      1
    888 0xe5e9ffe707ee071998340972af6fb178f5c64ba6      1
    889 0xe616e04c1237eff59f168626e11e6b8ca082f73d      1
    890 0xe63586f0ebe319d338b60487243021be9bc596d0      1
    891 0xe72dcf20abaeb8f7a834811bebb14cb3f7232558      1
    892 0xe75764a435f7831e8dfb960dbd85f01f78b84f47      1
    893 0xe77d2106f2277961fd4f0c8033a55f48d5dfaecb      1
    894 0xe7d254965c04468b559cbd796ee9d13b14011aa3      1
    895 0xe84d9d990a6f783a3b8e08b527161c8ac0b96513      1
    896 0xe8ad1457481c00a92fba2488e34effe5742d2ec7      1
    897 0xe8c9e155b48f4ed8f79d080fc5d3e2326e671b40      1
    898 0xe8e15525fcee15410ed827008c457124080f7753      1
    899 0xe8f1ea7b69aa49f00e98001fea4c024ac8852d25      1
    900 0xe91a7012ec1c170021b9038dccbda484924c1354      1
    901 0xe94ba66bb607c0b95ae916d3d1e7e9ac4ff554eb      1
    902 0xe9a5347f0342cba2cd6a233888eac657ddc42230      1
    903 0xe9a567610a8f4efc33cca1f319d62b76b804c5f1      1
    904 0xe9b9bd9192c73ebda2f6c6f2b6b26d1425d0170d      1
    905 0xea0e95671074c0b8fb9a699c2562932651021c32      1
    906 0xea1728d85ed9922d51651c54cd48ffa12b537ba4      1
    907 0xea2ddc5fce9fa081c929b52079d4342caa5224a9      1
    908 0xea70c723f179eaaf138e5d72fd592447fc585e89      1
    909 0xea7d51fbe858a043ba7f9d3d562631dce03eb464      1
    910 0xeb2f259baf137b169c0daecef0a1163fee316dab      1
    911 0xebef26cea024e09f020f97e6b0cc75919346c0ac      1
    912 0xec9bf38c72d871acfffded3342fa7e08a61bbbbb      1
    913 0xecaebf8f5a28a6ef322bb5428c11270ed057c497      1
    914 0xed03716e2cdc32ae1beefef00a1b1643641e4d18      1
    915 0xed04d3792c439b8af827b3e46ee626827de53db5      1
    916 0xed2b65b57f0fa21eadf3dad1a7d94092651c0bc9      1
    917 0xed31f32fd47e97bac4e7b4cf1201452e8463517e      1
    918 0xed5008ec9473d978c2fc61699c1549ea91365df3      1
    919 0xee06d007d469094468a63071d33c7daea8ded22d      1
    920 0xee69e8d61ab10190151556fc95af46b4d5bf3a86      1
    921 0xeef591f85f5a828fc24e71fc304e7132e80eb1c5      1
    922 0xeefb17a15c15d53eeb1bd742ff120f57f1ac2885      1
    923 0xef55bbcc3f60d203c45a560ddc2c80e2775d8d34      1
    924 0xefbfc695b171ec729ac92e7e2b0dbdde3fe201c6      1
    925 0xf0140d6d45a72ac4495115c3e5b68e505556c527      1
    926 0xf051b3107529c85c97fde99d7feb14bca8caed91      1
    927 0xf06e892380655e3109c90b319990cc6bc4c70995      1
    928 0xf0824ca3eba3f0318ab98d7196249ef9dc66d26a      1
    929 0xf128de46d785c308ad52b7f984af809b4a37b973      1
    930 0xf18af590e3e0bdb9fffbc449f6ded1b27ca07739      1
    931 0xf1cc92218180b5b5bbc15a158c3fe764e48a5033      1
    932 0xf253c885f7e5f2818c03547b16f5e9068d4735aa      1
    933 0xf266bd362edec2656ff30d797206d1ca608178d1      1
    934 0xf373645298c36928b58484c830959b3fe434d65b      1
    935 0xf38f67d8b16752b18f7358ef216a35423e45f806      1
    936 0xf3e8d5b704ccf09c02af5df9bfad1cf6e823703b      1
    937 0xf419728a27c259f3c871d3d98785b3ce72d3882f      1
    938 0xf4872463e2b785c8ed3e683d0f9d54474cf91fab      1
    939 0xf4b07f6d9fd5b9ff0c86169bef013014dbb4eacf      1
    940 0xf58ee3228270be721d850de0e130ad3c35bb848c      1
    941 0xf59baccbacfa9c87970a6ccd52af8dfdf87dff1c      1
    942 0xf59ec163682c8cdf015783272cfbaa23aac95d18      1
    943 0xf5d3dca1d4cc5f21f8df1a96d7b852f7c61ab435      1
    944 0xf6502ea66eebce6534fcc8a9736e745c5af63523      1
    945 0xf6843599d50f804d1d6bab9b84137b92ca53f327      1
    946 0xf6a8fe66caae1b516603434430a4b41fd188d821      1
    947 0xf6ee35d2506ed59879efcfd4c0678e71401e8cfe      1
    948 0xf70ebfd828b993e2863670530c2ec62c049f37ad      1
    949 0xf7427ec1bb1913eba8b18d3779a32704c652e6d0      1
    950 0xf77bb93d483b991a3bcb72e8d17f619774582944      1
    951 0xf79ff30828a23af27e8533a6091f46ab9e951539      1
    952 0xf7c66bcd6ea3607174da0bc8458bd1508027e539      1
    953 0xf81f2376a4f634573d6309ccbdc2cd65ba43c901      1
    954 0xf869ba1d3dd90d1f82a18f9706bbe03bbe9de13c      1
    955 0xf905e552d0851241b0693e32c7a1b958c5289d4e      1
    956 0xf960a94f0b11b9c2742bc24421fc9ec43470ba76      1
    957 0xf9a78796a49f8a20b56df08b1e293ee68d7867b6      1
    958 0xf9b441d8582852db81cc548c7e7b718d417d6ac2      1
    959 0xfa0f8714927504b521dea0b5f3dad9d6fe903148      1
    960 0xfa266b47c299d429565f2ed6de788ddf378f9d7e      1
    961 0xfa756a92973b1bbbee9df4b2ea3dae2e5891d424      1
    962 0xfa90bc0c1094c47d39f5c76e0f9deeeb48cb1abd      1
    963 0xfade41403a1eee72d805c42f1eaf5f53a54d0e0d      1
    964 0xfaee4108800e13ac5de7faadf83031a262f39945      1
    965 0xfaf4cb7c8fec1b00851c287f54036b770c9e5f44      1
    966 0xfb2771052f63c3913b8bdee4a8bf22cba34a4910      1
    967 0xfb362a82a7af0f9506b052be5632fbcf367e15dd      1
    968 0xfb74eee69be0c2bd8fb42ee67b6b6c4e05cd9ddd      1
    969 0xfba88e1b33fa0f14deb62124ab08ab39b6baa1fb      1
    970 0xfbc25aa1ac2a0fa8feb5ece002057ca33f412e95      1
    971 0xfc56a9c09b2d467df87cbc1c3a87753d2ba6afd7      1
    972 0xfc925cba2b706ec69474857aa901307a37d81ca4      1
    973 0xfcc2b212ec41a436c0831cdf2718df60dff934d6      1
    974 0xfcc2e71c063667ba5ae9efa7d98924b8e7687f25      1
    975 0xfd3b543cde56958584b2fa9ac69a8a998a90f15f      1
    976 0xfd64e8e4e7ddec10fd7b1667f3409307dcb5d1c0      1
    977 0xfd6866214582e58ae146f0c2603ffe7cd5fda611      1
    978 0xfdee4f4437e27293aa0786b74fcdd7818ac719f8      1
    979 0xfe0174255e410defaaf58b06e009e0ebcd74db59      1
    980 0xfea2e0a6c90c9e85483d9e69195ccd8a1049f4b3      1
    981 0xfecea928a996918dfe3242e7580ba3288d4ccd2c      1
    982 0xff0fafb54823806ecca746a9df5be8b14bb9af72      1
    983 0xff93e7aa23280d83115aa513b16de7493edd0196      1
    984 0xffa4410d9d8e63f255146475f0e3326667996482      1
    985 0xffce06ddc814537ff78076df32bf4bce108ec66f      1

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
