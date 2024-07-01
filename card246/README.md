
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:203         Length:203         Min.   :1   Length:203        
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:203        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 20195569 # https://etherscan.io/block/20195569
block_hash <- "0xe76ebd33a25c8ec484de50fd70b4eee2792a8c77f7aee0b8674ed9f01c217275"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4815 

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

allow_artist1    <- pick(snapshot, contracts=c("SuperRare","IdeesNoires","HereWeArt","ElementalShapes","RaphaelErba","Foundation","OrigamiStory","MindTrap","Indefinitely","Seasons","ORIGAMINJAgm"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("DaybyDayEditions","SonateauClairdeLuneEditions","MydreamswithmeEditions","FacethenightEditions","TimewaitsfornoOneEditions","CarpeDiemEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("SuperRare","IdeesNoires","HereWeArt","ElementalShapes","RaphaelErba","Foundation","OrigamiStory","MindTrap","Indefinitely","Seasons","ORIGAMINJAgm","DaybyDayEditions","SonateauClairdeLuneEditions","MydreamswithmeEditions","FacethenightEditions","TimewaitsfornoOneEditions","CarpeDiemEditions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 83 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0279273242ea0a8be97f682221c0c7f9187416db      1
     2 0x092f8828636a2b8061ff963f397c66de13f5928b      1
     3 0x0daac372398974373b29cb61c31deb11afa7ce23      1
     4 0x1132f3d7f1001dabe7b0a7e6ecefe162c9b97cea      1
     5 0x13ee79c28343b76a63f8862efbfd24f2a293a2d2      1
     6 0x1619f7076866ddc852a5a0a69cbb4d6a338d6d67      1
     7 0x17124e15738b30d0abd345900f0a98049c6d55df      1
     8 0x17301dbe98bef7001d68d4e8823347efae377543      1
     9 0x17bb4076ab59e0ef20ad5a873ab4b5341bf01b78      1
    10 0x1de4d49959eafa01ab3706aa20707a3f1dbe2424      1
    11 0x205a46012bc3c1a2583fdd426679dff276fb257e      1
    12 0x24422361687270c1ac2dd3f336e1bc130849617b      1
    13 0x2924196a2ec71ea4ae1b1357381eccdcee6c18f2      1
    14 0x2debdf4427ccbcfdbc7f29d63964499a0ec184f6      1
    15 0x31d8a64bff959fe12b13c070cd7d318eb0de9cce      1
    16 0x35b0dca4f59a7b89776fb33f4d09bab3855e4360      1
    17 0x37009f9ae9dcc0a6d59204e1aeaf9da4968caec8      1
    18 0x3a2ea5595098565e7362e04da30c5ec29435731f      1
    19 0x3b8248f02994ed419addf103a029345bc444ea60      1
    20 0x46b2bd5c888e6d57a839c559fd6076f2c15a8cb1      1
    21 0x484a12a27e8c95718e0b60d14369bfeaa426aff3      1
    22 0x511ef2d9d1b08b0bda6770448aca66df803998d4      1
    23 0x577ebc5de943e35cdf9ecb5bbe1f7d7cb6c7c647      1
    24 0x58b9f539c84bd534e00b3e1636446841eb0be60f      1
    25 0x58e0dfc4848dcea5cf67100b2d616604a9f88681      1
    26 0x596558fd287175afa7ff695f565f65e38547b0e6      1
    27 0x59d14847c0d1584a867fe98acabb1dc95ab1496f      1
    28 0x5b00f3d1644ff3106248b2c50505a67abf31efff      1
    29 0x6112083d4bb12dbf64e4850842685aaefbeacb10      1
    30 0x616ba726629c1e58d991785c83db45fb99406610      1
    31 0x61b0d91df6a1e2a35cf25f8cd0285fb5cc7376de      1
    32 0x6b4b60cd239b5b7947c98bb2b9bbd01db5bdf44c      1
    33 0x6ca1e305b3fdf0e8207492628cbb2856c102fcde      1
    34 0x6ed738ce5dc8946443e5f2c11545d014c46f2bfc      1
    35 0x7233a7b1d6d2a2a19ca0f6321c5ef0b859b0822e      1
    36 0x73b77ec753862706b986995ca69bb2e023116823      1
    37 0x7635daa2c1f2f9d4be9b8d91ebd25001f74b78a4      1
    38 0x7a04e11f6708d9dc4398b04a15cdb3329c449ef8      1
    39 0x7d70b5b638a2af9816202880535c8fe8f1fb9772      1
    40 0x869ce3c7415f06db4ff6d599d629c1a9c7c8a820      1
    41 0x8754e1ed406a72f8deca5b4c9654ad2db698ba83      1
    42 0x886c4d1d4bc99399a42ef9cdcbf0f0dfa700a860      1
    43 0x8a6adb9e6e8dba6bddae8bdfb17fb4657720c600      1
    44 0x900e7ad1ab18cebb4c21f71795364e9b636831ca      1
    45 0x907f8902bdd9173426ca24b5d80cde10b4ac89bb      1
    46 0x9499054d02a725316d61fa896c29d58550ee4a5b      1
    47 0x96815f3766b7796555e755fef8ba40770e211700      1
    48 0x9f282005722daeb7cbbbf9f5055b818b0f22af47      1
    49 0xa1a3524e1bd29f71d11e787d36baebb91bfc20da      1
    50 0xa4f0670468dfb825e6c2b5571e2c97d0844190a3      1
    51 0xa967d8b2d5b623a4b09eaf985a4463fea19b7668      1
    52 0xab6ca2017548a170699890214bfd66583a0c1754      1
    53 0xab94f597b94f45680b07997c394535d7ebc4a297      1
    54 0xabce38e203ebe8599e03eb59117c157f4e520bb4      1
    55 0xac12b102ef0fcd2f36b9633f614d45f145fa42a4      1
    56 0xb0bdd53b627d7e61cfc5c13ef110e47e210fac6f      1
    57 0xb64a8e573635d71e84a7c2ba03fbd431d2f3e17c      1
    58 0xb84f6da1b9accee009bdaba61573179b666b0d1a      1
    59 0xbee5646b4544eec699c365b7fae7e846ce19773d      1
    60 0xbf56b5178f175fefca63dd5ba4506a7648d6a2d4      1
    61 0xc136f93985b60455ec6d0ed8bc05f29861cea04a      1
    62 0xc3a8b0ee40098e32c1d749ebcdc6c144ada911cd      1
    63 0xc917fd2d5a9a4b91c4e8a08c07ced3bd36c55981      1
    64 0xcd8b72710595c69019c109aef5b1b92eea7f995f      1
    65 0xce90a7949bb78892f159f428d0dc23a8e3584d75      1
    66 0xd393d5dfac85c439f90e0ecdd3cd98da3a742dd2      1
    67 0xd46eb557762ebcdce876ca57040e3caa54836054      1
    68 0xd6ee002752c995a118e42611baf52aba7cc1fef1      1
    69 0xd81ce8e89dd987c8ab630858c8f1e9df14788c35      1
    70 0xd8fef4bb781ad488b124b86b0bc7701454060160      1
    71 0xd9550ece0d90a8e08b323283e656eb11be69560d      1
    72 0xe0719af1123a4f6beff552d5fb963cb278ca1ae2      1
    73 0xe4c8335467ce3b1854dd828f14bbb33aa4771818      1
    74 0xe5798a530bb7105e148d38ac884f05c28ed8e804      1
    75 0xe6b16e51e78d46bc57fbd34fd0746f399d6f39ed      1
    76 0xec9c3f8bba1d2380429bab40917345fb6685ad27      1
    77 0xed6692f58f389f7379bbcefdce043789b359bd95      1
    78 0xee45149b4cb501dbeb289c4768c373f891374775      1
    79 0xefffdc05e7c5b305fbd504366b01f2d6424cb8c4      1
    80 0xf53af966dab1afaaac1171dd941148384d38417d      1
    81 0xf96e01123d6ecb492b5151c80f8ee3ab15c8cae5      1
    82 0xfc7030fd3e3ca7541fd4f4a71b9d7b7243e83a37      1
    83 0xfc7d299fc04ecceef29324f531cbec2a7bc7dbd4      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 68 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0a690b298f84d12414f5c8db7de1ece5a4605877      1
     2 0x0b2a8484a6c256af7ed7ac0c4be73dee0877d7ef      1
     3 0x1e292eabab30b13d27d685a0a020a044a161e0ea      1
     4 0x21ed9bd5e12c3625f864c485c9a40a156f37f968      1
     5 0x2289c8fed5be4b1c4fcfd7a74ad2afcdd277312f      1
     6 0x285e62f4e30d3b8d55ac62601c325996ab5872c5      1
     7 0x2ce48f39c5440bf2216ecd04663c563cae49ccac      1
     8 0x2da5f7f40d72f6889abe9fa48220e34b5a369b3e      1
     9 0x3a661a18cb0258119636dfdde098648c6ad5ba62      1
    10 0x3a6c7ddbac1525e447f6a74b1f63fbc7a458907e      1
    11 0x3fe465902811769e3e8738cd162632e85dcbaa86      1
    12 0x42cd7103b8c7b195516a72be6a0d65df71747fad      1
    13 0x43a4e886f371ee955e65cd9d88485c8edffdd3ff      1
    14 0x44e84bb96968d341455796ef1f72b6348d8f5411      1
    15 0x4783281a06e91e5d0a136be35b86bd93e8d43904      1
    16 0x49159a8257ddf96b50ed05522db3b3827b9753e9      1
    17 0x4c82a8c78d95ffb5ecadada60b950c577f2ae565      1
    18 0x55d647550ac4dee50e00edf13debee5404f4c452      1
    19 0x5f23bf4d66ff4ea4e4c888ea9ef13404fbeb7b49      1
    20 0x660a41f9eb85b5607378aa8b05d29eff5334d64f      1
    21 0x6ac8c59004b5b9f07a3789db09ac2f7075ca0319      1
    22 0x6af7812fc932da2d34738a33f295de54011fd16b      1
    23 0x6e977c8155d914e7aa99331169939978a4d1b102      1
    24 0x6eb357501f7285c1b01b0548e9f5c4607b8607f6      1
    25 0x70353a9a3a2fe66ab44adecd61bb22b5928a6599      1
    26 0x75aadbb7c36db057e5ac64183899f118de76df35      1
    27 0x788550d00579f66c06ce209d14056c8f2c0a8188      1
    28 0x7c4a24c5b1c0eba323ac796b841708ca4f79feb1      1
    29 0x8544a547366ebfa8711ccf60667cbf7c8b0943f1      1
    30 0x88be96dc047cde01286e991f7333a2e9a4865856      1
    31 0x89d53e8612e8416bf960dc9444ce0e2a0878a582      1
    32 0x8eb514633f8fc1aee4b405f8165f3eeb42826f6d      1
    33 0x8f54249ae4c8a73e92d44459e026c9197670f3fd      1
    34 0x951038bb372d16180c0afb0f46ab283059154004      1
    35 0xa58c937e002030677bf6066b4efeeb9e76163e31      1
    36 0xaaf773699e7e05ee899f2687674ae1b35b9ffc65      1
    37 0xab4081aa5d3f58a47432f119acb4e468bbbe0e53      1
    38 0xae3f5f96ff418888b4e3fe41a0fabbba9eed9670      1
    39 0xaed3d37b761bf61767208da1dbb3a971b9f13fb5      1
    40 0xaf9e18cd797548220317fb459095c6afac5c33a7      1
    41 0xafa3a87e6b4179552615faaf3c600d00016d10a1      1
    42 0xb41e0a6bc73f238e52b359b62de2ae69b3411b0e      1
    43 0xb44c873fedf6606757a3b76f56604806ed620d49      1
    44 0xb53bde837cca74c3c9f507c3f1cd8d3f4bcccd84      1
    45 0xbbccf66c4617ecaec7ef59cac89b3d1511a262eb      1
    46 0xbdca355c8a7de13fd93178ed9bd8f883100c0f5f      1
    47 0xbe8fe12b9eb1ca2a593e6c070c71c294b6fe9f00      1
    48 0xc1bc1b32b437b81345b84f77d300d95bd24cbb95      1
    49 0xc52b17b3f2282f87922d7d394c817e81e11169d6      1
    50 0xc68c7771ec6a6e5d67d62aa9c6f22df69865e401      1
    51 0xcb37f1fe07988e6ff6c21b28b986fed13ebfa549      1
    52 0xcb7c599cac4e944348145f5dad93b682c1b87d24      1
    53 0xcd82ce4b6715dee331aa5fefd6226cbbcc21e4ec      1
    54 0xcf98c9010cb28492d75dfaf838efc64ba2b4f531      1
    55 0xd172680150dacf83d136d7d6f6083fbda4b306d0      1
    56 0xd35fc346e15ba5b446917c9fd23a9471d6144701      1
    57 0xd425191823f55456c9a17c8792c3d9c03586bbc9      1
    58 0xd88c9989f70ab4f1396fed77014f13d961f3a894      1
    59 0xd8ad39fb56624e0b54e29de732d20f90fd9a0278      1
    60 0xdcd87f8c295c5cdd1dda66f11ed1a01c42a5f1a3      1
    61 0xe01a97fc4607a388a41626d5e039fdecbfc6acd9      1
    62 0xe1994b2fd8310218a06875b4b3d7cbca8ec83e4c      1
    63 0xe2d128323cf7560a6e7a82726d7b425aedc7a556      1
    64 0xe687e816e58ceb199e7dae6c0c3a440cb7afad23      1
    65 0xf17b07497c09f11211975b0ffa2d9ec5cf96252c      1
    66 0xf822ac8fbd32c6fa8c832d166219cec4dfcbb924      1
    67 0xf9139e87107923477387f98288bca9bd6fec14c3      1
    68 0xfa43b254584c0edae92e57db903e0eac695908e9      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 151 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x0279273242ea0a8be97f682221c0c7f9187416db      1
      2 0x092f8828636a2b8061ff963f397c66de13f5928b      1
      3 0x0a690b298f84d12414f5c8db7de1ece5a4605877      1
      4 0x0b2a8484a6c256af7ed7ac0c4be73dee0877d7ef      1
      5 0x0daac372398974373b29cb61c31deb11afa7ce23      1
      6 0x1132f3d7f1001dabe7b0a7e6ecefe162c9b97cea      1
      7 0x13ee79c28343b76a63f8862efbfd24f2a293a2d2      1
      8 0x1619f7076866ddc852a5a0a69cbb4d6a338d6d67      1
      9 0x17124e15738b30d0abd345900f0a98049c6d55df      1
     10 0x17301dbe98bef7001d68d4e8823347efae377543      1
     11 0x17bb4076ab59e0ef20ad5a873ab4b5341bf01b78      1
     12 0x1de4d49959eafa01ab3706aa20707a3f1dbe2424      1
     13 0x1e292eabab30b13d27d685a0a020a044a161e0ea      1
     14 0x205a46012bc3c1a2583fdd426679dff276fb257e      1
     15 0x21ed9bd5e12c3625f864c485c9a40a156f37f968      1
     16 0x2289c8fed5be4b1c4fcfd7a74ad2afcdd277312f      1
     17 0x24422361687270c1ac2dd3f336e1bc130849617b      1
     18 0x285e62f4e30d3b8d55ac62601c325996ab5872c5      1
     19 0x2924196a2ec71ea4ae1b1357381eccdcee6c18f2      1
     20 0x2ce48f39c5440bf2216ecd04663c563cae49ccac      1
     21 0x2da5f7f40d72f6889abe9fa48220e34b5a369b3e      1
     22 0x2debdf4427ccbcfdbc7f29d63964499a0ec184f6      1
     23 0x31d8a64bff959fe12b13c070cd7d318eb0de9cce      1
     24 0x35b0dca4f59a7b89776fb33f4d09bab3855e4360      1
     25 0x37009f9ae9dcc0a6d59204e1aeaf9da4968caec8      1
     26 0x3a2ea5595098565e7362e04da30c5ec29435731f      1
     27 0x3a661a18cb0258119636dfdde098648c6ad5ba62      1
     28 0x3a6c7ddbac1525e447f6a74b1f63fbc7a458907e      1
     29 0x3b8248f02994ed419addf103a029345bc444ea60      1
     30 0x3fe465902811769e3e8738cd162632e85dcbaa86      1
     31 0x42cd7103b8c7b195516a72be6a0d65df71747fad      1
     32 0x43a4e886f371ee955e65cd9d88485c8edffdd3ff      1
     33 0x44e84bb96968d341455796ef1f72b6348d8f5411      1
     34 0x46b2bd5c888e6d57a839c559fd6076f2c15a8cb1      1
     35 0x4783281a06e91e5d0a136be35b86bd93e8d43904      1
     36 0x484a12a27e8c95718e0b60d14369bfeaa426aff3      1
     37 0x49159a8257ddf96b50ed05522db3b3827b9753e9      1
     38 0x4c82a8c78d95ffb5ecadada60b950c577f2ae565      1
     39 0x511ef2d9d1b08b0bda6770448aca66df803998d4      1
     40 0x55d647550ac4dee50e00edf13debee5404f4c452      1
     41 0x577ebc5de943e35cdf9ecb5bbe1f7d7cb6c7c647      1
     42 0x58b9f539c84bd534e00b3e1636446841eb0be60f      1
     43 0x58e0dfc4848dcea5cf67100b2d616604a9f88681      1
     44 0x596558fd287175afa7ff695f565f65e38547b0e6      1
     45 0x59d14847c0d1584a867fe98acabb1dc95ab1496f      1
     46 0x5b00f3d1644ff3106248b2c50505a67abf31efff      1
     47 0x5f23bf4d66ff4ea4e4c888ea9ef13404fbeb7b49      1
     48 0x6112083d4bb12dbf64e4850842685aaefbeacb10      1
     49 0x616ba726629c1e58d991785c83db45fb99406610      1
     50 0x61b0d91df6a1e2a35cf25f8cd0285fb5cc7376de      1
     51 0x660a41f9eb85b5607378aa8b05d29eff5334d64f      1
     52 0x6ac8c59004b5b9f07a3789db09ac2f7075ca0319      1
     53 0x6af7812fc932da2d34738a33f295de54011fd16b      1
     54 0x6b4b60cd239b5b7947c98bb2b9bbd01db5bdf44c      1
     55 0x6ca1e305b3fdf0e8207492628cbb2856c102fcde      1
     56 0x6e977c8155d914e7aa99331169939978a4d1b102      1
     57 0x6eb357501f7285c1b01b0548e9f5c4607b8607f6      1
     58 0x6ed738ce5dc8946443e5f2c11545d014c46f2bfc      1
     59 0x70353a9a3a2fe66ab44adecd61bb22b5928a6599      1
     60 0x7233a7b1d6d2a2a19ca0f6321c5ef0b859b0822e      1
     61 0x73b77ec753862706b986995ca69bb2e023116823      1
     62 0x75aadbb7c36db057e5ac64183899f118de76df35      1
     63 0x7635daa2c1f2f9d4be9b8d91ebd25001f74b78a4      1
     64 0x788550d00579f66c06ce209d14056c8f2c0a8188      1
     65 0x7a04e11f6708d9dc4398b04a15cdb3329c449ef8      1
     66 0x7c4a24c5b1c0eba323ac796b841708ca4f79feb1      1
     67 0x7d70b5b638a2af9816202880535c8fe8f1fb9772      1
     68 0x8544a547366ebfa8711ccf60667cbf7c8b0943f1      1
     69 0x869ce3c7415f06db4ff6d599d629c1a9c7c8a820      1
     70 0x8754e1ed406a72f8deca5b4c9654ad2db698ba83      1
     71 0x886c4d1d4bc99399a42ef9cdcbf0f0dfa700a860      1
     72 0x88be96dc047cde01286e991f7333a2e9a4865856      1
     73 0x89d53e8612e8416bf960dc9444ce0e2a0878a582      1
     74 0x8a6adb9e6e8dba6bddae8bdfb17fb4657720c600      1
     75 0x8eb514633f8fc1aee4b405f8165f3eeb42826f6d      1
     76 0x8f54249ae4c8a73e92d44459e026c9197670f3fd      1
     77 0x900e7ad1ab18cebb4c21f71795364e9b636831ca      1
     78 0x907f8902bdd9173426ca24b5d80cde10b4ac89bb      1
     79 0x9499054d02a725316d61fa896c29d58550ee4a5b      1
     80 0x951038bb372d16180c0afb0f46ab283059154004      1
     81 0x96815f3766b7796555e755fef8ba40770e211700      1
     82 0x9f282005722daeb7cbbbf9f5055b818b0f22af47      1
     83 0xa1a3524e1bd29f71d11e787d36baebb91bfc20da      1
     84 0xa4f0670468dfb825e6c2b5571e2c97d0844190a3      1
     85 0xa58c937e002030677bf6066b4efeeb9e76163e31      1
     86 0xa967d8b2d5b623a4b09eaf985a4463fea19b7668      1
     87 0xaaf773699e7e05ee899f2687674ae1b35b9ffc65      1
     88 0xab4081aa5d3f58a47432f119acb4e468bbbe0e53      1
     89 0xab6ca2017548a170699890214bfd66583a0c1754      1
     90 0xab94f597b94f45680b07997c394535d7ebc4a297      1
     91 0xabce38e203ebe8599e03eb59117c157f4e520bb4      1
     92 0xac12b102ef0fcd2f36b9633f614d45f145fa42a4      1
     93 0xae3f5f96ff418888b4e3fe41a0fabbba9eed9670      1
     94 0xaed3d37b761bf61767208da1dbb3a971b9f13fb5      1
     95 0xaf9e18cd797548220317fb459095c6afac5c33a7      1
     96 0xafa3a87e6b4179552615faaf3c600d00016d10a1      1
     97 0xb0bdd53b627d7e61cfc5c13ef110e47e210fac6f      1
     98 0xb41e0a6bc73f238e52b359b62de2ae69b3411b0e      1
     99 0xb44c873fedf6606757a3b76f56604806ed620d49      1
    100 0xb53bde837cca74c3c9f507c3f1cd8d3f4bcccd84      1
    101 0xb64a8e573635d71e84a7c2ba03fbd431d2f3e17c      1
    102 0xb84f6da1b9accee009bdaba61573179b666b0d1a      1
    103 0xbbccf66c4617ecaec7ef59cac89b3d1511a262eb      1
    104 0xbdca355c8a7de13fd93178ed9bd8f883100c0f5f      1
    105 0xbe8fe12b9eb1ca2a593e6c070c71c294b6fe9f00      1
    106 0xbee5646b4544eec699c365b7fae7e846ce19773d      1
    107 0xbf56b5178f175fefca63dd5ba4506a7648d6a2d4      1
    108 0xc136f93985b60455ec6d0ed8bc05f29861cea04a      1
    109 0xc1bc1b32b437b81345b84f77d300d95bd24cbb95      1
    110 0xc3a8b0ee40098e32c1d749ebcdc6c144ada911cd      1
    111 0xc52b17b3f2282f87922d7d394c817e81e11169d6      1
    112 0xc68c7771ec6a6e5d67d62aa9c6f22df69865e401      1
    113 0xc917fd2d5a9a4b91c4e8a08c07ced3bd36c55981      1
    114 0xcb37f1fe07988e6ff6c21b28b986fed13ebfa549      1
    115 0xcb7c599cac4e944348145f5dad93b682c1b87d24      1
    116 0xcd82ce4b6715dee331aa5fefd6226cbbcc21e4ec      1
    117 0xcd8b72710595c69019c109aef5b1b92eea7f995f      1
    118 0xce90a7949bb78892f159f428d0dc23a8e3584d75      1
    119 0xcf98c9010cb28492d75dfaf838efc64ba2b4f531      1
    120 0xd172680150dacf83d136d7d6f6083fbda4b306d0      1
    121 0xd35fc346e15ba5b446917c9fd23a9471d6144701      1
    122 0xd393d5dfac85c439f90e0ecdd3cd98da3a742dd2      1
    123 0xd425191823f55456c9a17c8792c3d9c03586bbc9      1
    124 0xd46eb557762ebcdce876ca57040e3caa54836054      1
    125 0xd6ee002752c995a118e42611baf52aba7cc1fef1      1
    126 0xd81ce8e89dd987c8ab630858c8f1e9df14788c35      1
    127 0xd88c9989f70ab4f1396fed77014f13d961f3a894      1
    128 0xd8ad39fb56624e0b54e29de732d20f90fd9a0278      1
    129 0xd8fef4bb781ad488b124b86b0bc7701454060160      1
    130 0xd9550ece0d90a8e08b323283e656eb11be69560d      1
    131 0xdcd87f8c295c5cdd1dda66f11ed1a01c42a5f1a3      1
    132 0xe01a97fc4607a388a41626d5e039fdecbfc6acd9      1
    133 0xe0719af1123a4f6beff552d5fb963cb278ca1ae2      1
    134 0xe1994b2fd8310218a06875b4b3d7cbca8ec83e4c      1
    135 0xe2d128323cf7560a6e7a82726d7b425aedc7a556      1
    136 0xe4c8335467ce3b1854dd828f14bbb33aa4771818      1
    137 0xe5798a530bb7105e148d38ac884f05c28ed8e804      1
    138 0xe687e816e58ceb199e7dae6c0c3a440cb7afad23      1
    139 0xe6b16e51e78d46bc57fbd34fd0746f399d6f39ed      1
    140 0xec9c3f8bba1d2380429bab40917345fb6685ad27      1
    141 0xed6692f58f389f7379bbcefdce043789b359bd95      1
    142 0xee45149b4cb501dbeb289c4768c373f891374775      1
    143 0xefffdc05e7c5b305fbd504366b01f2d6424cb8c4      1
    144 0xf17b07497c09f11211975b0ffa2d9ec5cf96252c      1
    145 0xf53af966dab1afaaac1171dd941148384d38417d      1
    146 0xf822ac8fbd32c6fa8c832d166219cec4dfcbb924      1
    147 0xf9139e87107923477387f98288bca9bd6fec14c3      1
    148 0xf96e01123d6ecb492b5151c80f8ee3ab15c8cae5      1
    149 0xfa43b254584c0edae92e57db903e0eac695908e9      1
    150 0xfc7030fd3e3ca7541fd4f4a71b9d7b7243e83a37      1
    151 0xfc7d299fc04ecceef29324f531cbec2a7bc7dbd4      1

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
