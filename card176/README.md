
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:428         Length:428         Min.   :1   Length:428        
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:428        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 18696969 # https://etherscan.io/block/18696969
block_hash <- "0x5ff34e45b287db4b27cac139870fcca96f89a9be9670f14914327f57ac26a5f3"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4677 

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

allow_artist1    <- pick(snapshot, contracts=c("SuperrareRocket","THENINE","ARCANEDIVERGENCE","OBSIDIAN","InfiniteGames","Foundation","KnownOrigin","Semisomna","ROCKETGIRL","LiminalJungle","MadWorld","EATME","LAKESIDE","Umbra","ARCANUMAQUAEOBSCURAE","FUN","BRAVENEWWORLD"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("KnownOriginEditions","LiminalJungleEditions","AbstractOpenEditions","MadWorldEditions","CARNALEditions","OpticalDelusionsEditions","UmbraEditions","FUGITIVUSEditions","PLUSQUAMHUMANUMEditions","ELECTRA"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("SuperrareRocket","THENINE","ARCANEDIVERGENCE","OBSIDIAN","InfiniteGames","Foundation","KnownOrigin","Semisomna","ROCKETGIRL","LiminalJungle","MadWorld","EATME","LAKESIDE","Umbra","ARCANUMAQUAEOBSCURAE","FUN","BRAVENEWWORLD","KnownOriginEditions","LiminalJungleEditions","AbstractOpenEditions","MadWorldEditions","CARNALEditions","OpticalDelusionsEditions","UmbraEditions","FUGITIVUSEditions","PLUSQUAMHUMANUMEditions","ELECTRA"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 134 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x00de0dc707689f31236a376734ef508449e1aa1e      1
      2 0x02b165144c45f30452ccbbda356de42316b89626      1
      3 0x06186e1bfb2309c3cbc9dfed039a45d7187bd6d1      1
      4 0x08db792f0dc2492c67ba901adbe136e76648341c      1
      5 0x0bc8233afce8695e8164f8ea89177377d0fff13e      1
      6 0x11ae370040f234d35fd205360a56e9af516178ce      1
      7 0x1257400e85b69c4efc4de33170ca11bec259bc7e      1
      8 0x13189e189f7a25dd85b32f327d65a3511bb8e8e1      1
      9 0x1340fc92d9f817d41f76a5a90c188d8633c307d3      1
     10 0x14739b2b9ca2dbfe90dd86085eb958940ad0221c      1
     11 0x1c14134c410610db5857c7428163a78592734a99      1
     12 0x2168b2295e912a27c0015bad9f09f090ebce1a99      1
     13 0x22bd04906e808196b19f7d10463fe463ed5f88cb      1
     14 0x22f4e93c90a42cf7ae3be1336865292a2346e168      1
     15 0x230e1833aeff798558a60db545ec42f0e446133f      1
     16 0x263ab80489c5f2c1e777f0366b501f2675bd9526      1
     17 0x2cb3338f518b2d61470b4d1d503bdc84c1a04ecd      1
     18 0x2ce780d7c743a57791b835a9d6f998b15bbba5a4      1
     19 0x2fd688827eb1f1c1475d7a15831cb8cc663573b5      1
     20 0x30386046cb5727fccc84f4e02606d39bafcc0bab      1
     21 0x30dec07af807a47c4c5a2ba69a22738831f002b8      1
     22 0x31c72d0d03adc76456d3c14a4fe19f8aa8307c16      1
     23 0x34bfcef78cfbbba72e92d30efeea33e36f7e6a22      1
     24 0x363380e1b06fcfdb05006a57a26b143eca920342      1
     25 0x36a5bc205df1ed65c86301022cfc343a6ce546ff      1
     26 0x3852471d266d9e2222ca9fdd922bafc904dc49e5      1
     27 0x38e1a00cead40b4086527f5c536079ada06c1ea9      1
     28 0x3c6df4b329b9faf209b763b6cf3e2a6017cacafa      1
     29 0x3ccb86a565daa86d1613a634dd16663779bbd63f      1
     30 0x3dbc93aecd50e485ecd2dd6dbe73d1ec19a4d839      1
     31 0x3e5f46ad03a924d0c1ddc9aa311fdd265be312df      1
     32 0x3f526ece92247050be2c66cae5ab6c2cb8bc2a4b      1
     33 0x3fc6c595a3445d56bf5d8322a392f3e027197c6a      1
     34 0x4718d398dd4b0ec472700bd1f6091ed82c49c256      1
     35 0x478bb542f7658d635abba67edb987806dff5b83d      1
     36 0x49ca963ef75bceba8e4a5f4ceab5fd326bef6123      1
     37 0x4b6b892b6878e61f421066a01fc03d0648228f82      1
     38 0x4c5287834576f7f1f3732fc2dddef2d2db3fea53      1
     39 0x54cd09bb9e196e29481a72909f66145de9c90c35      1
     40 0x5794f7de490ea3f193ca8b60373db91a86fda811      1
     41 0x58847353d941840f261366561dab73bcce277b6c      1
     42 0x596558fd287175afa7ff695f565f65e38547b0e6      1
     43 0x5a8116f07937394e6ac2a51995ab6a054c08bf9e      1
     44 0x5c4fe960950ba0e09a72869c3d51fe70f07580e0      1
     45 0x5e7b14fa1bea3c0b4b485beb3a3fb837b0adbd6b      1
     46 0x5fc13c5a541041f8fa12fab492d064435eb8d2f5      1
     47 0x6002ebe4144d7f3b0cd10e2ac0c0f357eb1c1c51      1
     48 0x62c5653b71c26dab6624d9cc81610fb1d6920710      1
     49 0x63923511f7b628483f9e160cc173e68a5ded11b6      1
     50 0x66ff8f288c413e61c8ee807088ce301c03076e6e      1
     51 0x69e630c314bb7c62307e6bef8d53f96c2b5cb6be      1
     52 0x6b6ae848f555f70944bc99d736fe29fcaecf8b23      1
     53 0x6c802912a21f3c0382d5286ef1e799fc4da3e7d7      1
     54 0x6e63a4caeccb4f341ee9c9175c9cc554bdb6d10b      1
     55 0x702323dfcc1061e8af7816c483f34a184e5befd9      1
     56 0x710b4f7437102c23338448361ddc7f0ef0abc6bd      1
     57 0x72eb30d3ca53f5e839325e2eacf535e70a9e6987      1
     58 0x732bb12525961f5853154deb9d0a4aacd2eb240f      1
     59 0x73a181109fde969bb8da3fe7604ca4be982c3454      1
     60 0x75aadbb7c36db057e5ac64183899f118de76df35      1
     61 0x77f80700f974ca52cf52bdc87efcb3b0e0c4f78b      1
     62 0x780701eda98d4437967881553efe18da3be04249      1
     63 0x79204328fe037bc69d515040b0e471aa170b75cc      1
     64 0x7af061a6ec9616135231cb8de2ed2a0a4140e90b      1
     65 0x7b63b80115c38bf0f422f7bcfa1e92c468bc8333      1
     66 0x7b7ec26a15eb19995be1323251f4494b50f1c6e5      1
     67 0x7cfe27a10f32ec0c7e609ffecf53058444705b55      1
     68 0x80aa186f646fa560495c4e965aa983b958a47369      1
     69 0x827e1537171289e51b26b982d530d2210d3676fa      1
     70 0x84300dcc7ca9cf447e886fa17c11fa22557d1af0      1
     71 0x85a30ede5e46029457e20d55427ee8673fad7762      1
     72 0x8a9d3812020d70bd487c79f2ce471ccf87e17ad1      1
     73 0x8cfed4f4bfd8fe8b98efe52f4f708ef512b4d2d7      1
     74 0x8e2f963c1f0ea3ff1fad4c989f860104ad33174c      1
     75 0x8e7a0b4c6550e22880081451c4f75e497e037e3e      1
     76 0x8ed4d3c67ee1a73f5df8ca3e10698f1cd4c5b14c      1
     77 0x93a6d60ee0a7059d9f508f0af995581278d90e41      1
     78 0x93c838add3ac72b42f65417408f900a7155e1bb8      1
     79 0x9481d8bc76fb1b7d2f95733a6ec0b40d0cb1fffc      1
     80 0x96951beae8a3c191800877cdde67dc12a5881584      1
     81 0x973231cf471f768f0939042e0591da1b15f4799f      1
     82 0x98fe967044e428cc279160435956200b14896071      1
     83 0x9aaddc5040d2093dc0dafc0803cbfca822341bca      1
     84 0x9af7edc48bc8db5e4afd3af95df143ea29e2e2d6      1
     85 0x9bc27a47b4413c51f884ae4e4b9170f3a0d7f742      1
     86 0x9cc04d9f5715891d28f0556316b9544e5522e1c2      1
     87 0x9f35af4727fb91114563d8a8f779a792cb557f3f      1
     88 0xa04611ac0438bdada004680628704151c8db044f      1
     89 0xa28fd178c686beebd6818d6772e56f6e7a6ff5cc      1
     90 0xa6c88571d0028f47adba983a7240bf12af94633e      1
     91 0xa8357ee17cb3ff5a6b9694ddc8fde0ed2ce9d788      1
     92 0xa88fe6fa01fcc112bb2164c6e37d63395b923e5f      1
     93 0xab7473da0af4658ae2879cb641d82e7652721486      1
     94 0xacb818130ac9cd4484848848a0ca3e7dda4bb265      1
     95 0xb3ac051668a6eddaa387c166af19e73283d9b0ba      1
     96 0xb7d3a787a39f25457ca511dc3f0591b546f5e02f      1
     97 0xbb22b864bf23b8e46afa71a6599854243e6c353b      1
     98 0xbc7b2461bfaa2fb47bd8f632d0c797c3bfd93b93      1
     99 0xbd7a1c16d7b3af4af6d1a0fd7437466d765656ce      1
    100 0xbebd1e62ba2412d9378847cfd7cc3f759de1504e      1
    101 0xbece662e6b7c1e898c747a21281bcd602afc1367      1
    102 0xbf4a907ac732179f0a3ab15c2e9715da19a27801      1
    103 0xc02a3f4424c8834fcb1f31832501f9e3e090262a      1
    104 0xc225c612926ef5f9e9578b865275a02bec6999ee      1
    105 0xc3c50d8ae310a06800696ff7218458350cfcca1d      1
    106 0xc517297ab0a8d95c1188a703c252688733740cd8      1
    107 0xc5338f905c1a1c061ad097083c78f9c7991a537f      1
    108 0xc5a6d80bc7a2c3093a68e2ad05e8205dc0c0726b      1
    109 0xc5f5f52479f19c945c97fac8d06570fb14091df3      1
    110 0xc7b57729663ddd90a05af66b42e9d4f71448f099      1
    111 0xcbc7e8a39a0ec84d6b0e8e0dd98655f348ecd44f      1
    112 0xd2737fa3f336c1106a65b69379ce4a53f02128e7      1
    113 0xd2d15a46a6a864e6070cbe05fdb7e32636ba0048      1
    114 0xd458f62925b3e135680626a0f520098972f93fe9      1
    115 0xd46bc961065f56d92921a5fea66125822a7eeaa0      1
    116 0xd7d184fec20a50aa7e522aa2f92971b5b1bb2a88      1
    117 0xd7e683c992855b5e8edca21807fb03d4321d4f12      1
    118 0xdb3f28bc3bc82a2c577f71fc5a30ff774135f9a2      1
    119 0xdc613e6ecba50027235ac03ab1a844b441e57b43      1
    120 0xdcb6a79adcda176acd549fb95c5d34a395fe622b      1
    121 0xdd82bc8cbe61efb3953abe592a497639b2f77243      1
    122 0xde3697dda384ce178d04d8879f7a66423f72a326      1
    123 0xe0c16b87b9af4a281bd53e6b6d67f77400dc9625      1
    124 0xe16dce8e44b310953255ffeaa9086f1e3600e999      1
    125 0xe288a00df4b697606078876788e4d64633cd2e01      1
    126 0xe6fe14a30631b22d3f78d9ec7869eb62d6e73daa      1
    127 0xe7513e5595a751af9f298ecea7741cdd839dc7d4      1
    128 0xeb0e194d60f307391bbeffdc6d4bec8f56aac045      1
    129 0xeced6a11834fdf236ac5bdda8c755ea8a8b385b7      1
    130 0xf1e1c701b49b1dc2405a9e8ef40f9f88802b80fa      1
    131 0xf5a562cb859b5e037a88f96c0a1fd3f2d0c5fdbe      1
    132 0xf6f5c3fa8c9ef5a243af69a2e110b695ab96af3d      1
    133 0xfbe22e4bc2b9f4ddc44d8b5c1d94e2680f7ab129      1
    134 0xfd2021d69164ddcd27d2cd1391d5ddc5be1ca9d2      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 131 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x00b755deba4118b0d9e99803256cb7784515ce8f      1
      2 0x02380eb61a3560a34d76c0ad6f95f647ae607922      1
      3 0x03cec965f26a18d5e23f8bc542709baf10e19c96      1
      4 0x04830005c7dbd3671f2c4072c61d4e82b3c27674      1
      5 0x07f6f97ac29fb6882746e7c95797d30c1b50448f      1
      6 0x105d8f9b6b8586b89e73a595a1470aa55814d951      1
      7 0x119269c2c2f8cd2604eff729f61b328a48c472b6      1
      8 0x12fb6a37d42e178c53c9958ddbce741cc758f5f7      1
      9 0x17124e15738b30d0abd345900f0a98049c6d55df      1
     10 0x19fedef56411bb49afaf65f7e4be50629be17632      1
     11 0x1bb7b4f7d98a9d204186b36d3852e28f3286f577      1
     12 0x1de924a164fdb51e06fa58e73b00d83893b6304e      1
     13 0x205798065be2bf10ba02a2303396b38befdc3c06      1
     14 0x25505edc43b783873e96c5a6a844aef494bdbfa2      1
     15 0x29d7e1da8daca7a9d1edaec80255bbaf0eef594a      1
     16 0x2b5b85025abde58e0f38f3b9b07fd2b30d3361bc      1
     17 0x2b909662427fbf80b5253c0ab9f810fd0a024e79      1
     18 0x2ce48f39c5440bf2216ecd04663c563cae49ccac      1
     19 0x2d92214855f88f1a3f24b1a3f8f6d533cd0e2e37      1
     20 0x2e6803e20fc55714e7140f516823d2ae27a12c79      1
     21 0x30dcdf808bb9313e54c577859e7f41197677831c      1
     22 0x31c091b3d8d20fb3631e22c012e797ec70c20ab4      1
     23 0x365f7b46973f27740c08a077b66ec810cab39306      1
     24 0x37c196fbd9a4355af684ff988ff4a17ccb181ebe      1
     25 0x3af840cd5d04f29c7ba476a5067049a1eb5c2662      1
     26 0x4095301a454311b507c0ef52924a7753f0e1406e      1
     27 0x4478ad8276c7966b2e2594cef6a9e474ec4ad61f      1
     28 0x456fb001c4b6aa2d4c99c587fce826d9ae733529      1
     29 0x466cf4f9bdf6ace5bc7c016eb9aaac5f951a3b2f      1
     30 0x46b249da04697c801cd592030f6ef81f9433398f      1
     31 0x49bd3fdd5fdf30124dd77c137b178de74fe8649b      1
     32 0x4b4fc57ba00dc7b572af496140effc540d1d4474      1
     33 0x51ce28855ea50b4eac9656230adf738a5813ca04      1
     34 0x52fd624e3c42c33022ccca829b8d58f98b8dd422      1
     35 0x54eef9bcd9e939e1ff4d946e3eabebac1a482e5b      1
     36 0x571244ea8cf2a68928e54ccc8479e6bfab32aab1      1
     37 0x573bc03f18f41b200b949a973d67b453a7ef11a4      1
     38 0x58392a0c14b542ad1433c2a8b481a78347aaf1e5      1
     39 0x5a683aa37c6e976fa71ce18a28787ab645ba31bc      1
     40 0x5b1022d859a46f42fcf24bcd8f5ab616e52b4800      1
     41 0x5d7b71cda3ce450b5db872776c3524e574ee0b7a      1
     42 0x60eaaa669c70166dc7504a860353161cf525da0b      1
     43 0x63569c554e62aa224bec21d43d9a184e8333fbff      1
     44 0x64d18532f07855920ab624de8d2a8a365f365e5b      1
     45 0x660d8705cc155c05ce811e9c5dff827b5a6512fe      1
     46 0x6948108b463d5ee9c1cf6626502accea8fd47b7d      1
     47 0x694efab23e7a9e9c0317ee50dbc8fa895f8b81ba      1
     48 0x6dfc6d154b7437bee766ba3edb820974538de232      1
     49 0x703daf8e0b424420004190c3082441e52b7ecd4c      1
     50 0x7461c99e39e825344ddb86a16e428d3fad499d86      1
     51 0x75775181080b3684cc3be770ba070d1ecc1ec50d      1
     52 0x77037050bb27ae8f09118a0c480224d897589c65      1
     53 0x791d85b7184a76d03d599f0cf940c11033b7e377      1
     54 0x7a41821e9ad5ccefe77611e58eed762a5a4d7572      1
     55 0x7e3cdbbc82edd873f1cb9aa0ed45062fc6a5dbf6      1
     56 0x80c791b450b4d1e82e6fea4991bd2cab5c49d27a      1
     57 0x830ed8630c973d55f24615fd5493a51a1ffcf913      1
     58 0x856a4ed4c11a610cc2a4dd706f5642208bf6521d      1
     59 0x8699793e22403b355185d8ff76e7392f98aafa46      1
     60 0x88b20748727541eddab55df878a7bc6e8335431b      1
     61 0x8a8035f056af830b7205c58c1dc037f826fc2b92      1
     62 0x8e5150e73d4ff74706df51bf4329199e0cad7350      1
     63 0x920269799de0a4397d65543e5850772c9c10cd2d      1
     64 0x956b4de943668aa6339df8830e66cf99a2dffdab      1
     65 0x970bb85b997af2369b4be78e703e552d4d0d7158      1
     66 0x97284ad97375173d88f760f9758c7d1e8658cdee      1
     67 0x99581599e8823ee9130380281478e729e926353f      1
     68 0x995e0505603a19ee5c469d2359427bea68c6e953      1
     69 0x9980db65004727f5ca68e57c616a3cd0db39b5b4      1
     70 0x99fba4de1aa39ad8a1472276a0c334fbe0b53505      1
     71 0x9b25f796dcf939b73d30ab1006d62ddde1a0b1d8      1
     72 0x9d21f5f14605e228d41fb8d559dd1af39c630224      1
     73 0x9f7facdc938d4d0358b7e7d224ba2d1ff070e346      1
     74 0xa1e37bfc8638a92b9cb5b731d12d79575e7fe7db      1
     75 0xa3dc5bab4782df8916181f67d259575b864e97d1      1
     76 0xa52126809c92a62ba2f2966b5d8ce695a802da5e      1
     77 0xa5855f4f40a409ffde3a171eb616926aec5d7b9c      1
     78 0xa693fe70a4180c29d0ea522dbd4833dced5fb94a      1
     79 0xa7625e30eb5356a0b777e9617b55a92dddff2975      1
     80 0xa81725fae8bb3930a95debb0adeaa184b5e9cc10      1
     81 0xacc4b6f3ca8d59f631c1148dcaa52b9d7f5c819a      1
     82 0xad119a5520aab0664859a847b8117da7aea55948      1
     83 0xae78f07e15cfd1fb9fed5b7156708271188ad620      1
     84 0xae92ac902fb7937f652dcec459424b122747a86c      1
     85 0xb5a4e4cb6aaa9f6ea1e00444251682e0f20f86ea      1
     86 0xb998874bcb70f1075d2cac03b560245ae2917989      1
     87 0xbae3043c5a35f35ea414cbfe5857cd9eb4d7b742      1
     88 0xbdddfb2ff21efcc00a2ca5a7d1fdc5798c98f07b      1
     89 0xbf412be283fe1105c709f1f64d40e9f70057a305      1
     90 0xc0ad23a629c6de23c03203b396c5d1e1585111c3      1
     91 0xc142dc5ceb96f73635d66b62bfde2a0eea4e4ce1      1
     92 0xc283e9996f7c651eaab983692f0649a2e432ce26      1
     93 0xc37016f3023d0218718ca22a423d9dd00eb58275      1
     94 0xc5b6a215a753b6a98dd1fea01861d2f2b661fd0b      1
     95 0xc72f40a397453051349f73cf2e2a04fac06e37a3      1
     96 0xc99be1c7d78f0bd8bf024131a54efd252ebee832      1
     97 0xc9f62dbc54f560847c48ba70e7a3281fd6b6e086      1
     98 0xcb4de7db08b4dd0a9954fcfa0a09762f58436df0      1
     99 0xcb94d0cebdd0b78b127dbb1b2a1666b2062a32f5      1
    100 0xce6f91223a8e4d81252e0fc4fb8ec5899e3b104e      1
    101 0xced183f3efe63c054f36bc2d701cfe4631e61f1a      1
    102 0xcf79b7ba24c70341e3fd5409381e1f00aab275d9      1
    103 0xcffbda2841ef95df3e977982ecc55429b8d82eb5      1
    104 0xd3522562d0004f080550719f61b846ab0ce5f54d      1
    105 0xd6786fee06c76b8a1c5c4b0ac9025f29d347b69c      1
    106 0xd8e2b66013ffc6538017b1c6acab712a098ec1bf      1
    107 0xdbd8fefa7f919825c22ab1a1ebfc5ce51340f1bb      1
    108 0xdcd50efb4ccdc9c409b4a4d8eb919ed301c86abe      1
    109 0xdfd818378e60a1b7e26cf165cc57de8ff0a65478      1
    110 0xe1d23eddf8f0f597a8582d86586999ffc41bb030      1
    111 0xe40dcd3e8587c72cf741293c55d0ecb1fca5a852      1
    112 0xe681e5232da0c9ba5d28e6775bb3b6945eaf50f4      1
    113 0xe71183e421da96f5f11c642bf6224dfda97e513f      1
    114 0xe855957a9e3ce38ed80665a7de93e13d418b5e22      1
    115 0xe88807c721a2faacc42992399b406b310c7d84e3      1
    116 0xe889714f7b03a9125a09db9f1d0185f1ea4a4ce2      1
    117 0xe8c9333d6ba367ebd7ed1afd83088b5b5ea15d84      1
    118 0xe95e4c20b70aeeff43469933d46acc2aa3e6acf4      1
    119 0xe9976059d92d5bdf8464b55aa048c4343fa01886      1
    120 0xebc26a568603bc913d0806c5438e5ed2e05e87be      1
    121 0xedd983eddf5dee5d0af9bbc30866e7d4c843b975      1
    122 0xf1d83de0ead95747f4ac184b36659c0d538309c8      1
    123 0xf25ad24b791e37e83f4dadfe212e0e9bb45a1f8b      1
    124 0xf2aeb2e49ca3bd2ff618b9f19688b07d3c1928a0      1
    125 0xf2dca9d0652c43784522397c11b19694c73074a6      1
    126 0xf4593d840c00832dfe26d4bffa8423e1108323fb      1
    127 0xf631f197fd1d178d404bc10cf91d25c3d7451fb8      1
    128 0xf83d854970c6d7fe7a6f6d26872cc121b996887a      1
    129 0xf8d101b33a948fc07b2dd8cbf392c04b586ed56d      1
    130 0xfc540d8334ad4d64405182d9336bb3e1fd62f028      1
    131 0xff13f9b23a16a82c3ccdd29ece6af667eeb15834      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 265 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x00b755deba4118b0d9e99803256cb7784515ce8f      1
      2 0x00de0dc707689f31236a376734ef508449e1aa1e      1
      3 0x02380eb61a3560a34d76c0ad6f95f647ae607922      1
      4 0x02b165144c45f30452ccbbda356de42316b89626      1
      5 0x03cec965f26a18d5e23f8bc542709baf10e19c96      1
      6 0x04830005c7dbd3671f2c4072c61d4e82b3c27674      1
      7 0x06186e1bfb2309c3cbc9dfed039a45d7187bd6d1      1
      8 0x07f6f97ac29fb6882746e7c95797d30c1b50448f      1
      9 0x08db792f0dc2492c67ba901adbe136e76648341c      1
     10 0x0bc8233afce8695e8164f8ea89177377d0fff13e      1
     11 0x105d8f9b6b8586b89e73a595a1470aa55814d951      1
     12 0x119269c2c2f8cd2604eff729f61b328a48c472b6      1
     13 0x11ae370040f234d35fd205360a56e9af516178ce      1
     14 0x1257400e85b69c4efc4de33170ca11bec259bc7e      1
     15 0x12fb6a37d42e178c53c9958ddbce741cc758f5f7      1
     16 0x13189e189f7a25dd85b32f327d65a3511bb8e8e1      1
     17 0x1340fc92d9f817d41f76a5a90c188d8633c307d3      1
     18 0x14739b2b9ca2dbfe90dd86085eb958940ad0221c      1
     19 0x17124e15738b30d0abd345900f0a98049c6d55df      1
     20 0x19fedef56411bb49afaf65f7e4be50629be17632      1
     21 0x1bb7b4f7d98a9d204186b36d3852e28f3286f577      1
     22 0x1c14134c410610db5857c7428163a78592734a99      1
     23 0x1de924a164fdb51e06fa58e73b00d83893b6304e      1
     24 0x205798065be2bf10ba02a2303396b38befdc3c06      1
     25 0x2168b2295e912a27c0015bad9f09f090ebce1a99      1
     26 0x22bd04906e808196b19f7d10463fe463ed5f88cb      1
     27 0x22f4e93c90a42cf7ae3be1336865292a2346e168      1
     28 0x230e1833aeff798558a60db545ec42f0e446133f      1
     29 0x25505edc43b783873e96c5a6a844aef494bdbfa2      1
     30 0x263ab80489c5f2c1e777f0366b501f2675bd9526      1
     31 0x29d7e1da8daca7a9d1edaec80255bbaf0eef594a      1
     32 0x2b5b85025abde58e0f38f3b9b07fd2b30d3361bc      1
     33 0x2b909662427fbf80b5253c0ab9f810fd0a024e79      1
     34 0x2cb3338f518b2d61470b4d1d503bdc84c1a04ecd      1
     35 0x2ce48f39c5440bf2216ecd04663c563cae49ccac      1
     36 0x2ce780d7c743a57791b835a9d6f998b15bbba5a4      1
     37 0x2d92214855f88f1a3f24b1a3f8f6d533cd0e2e37      1
     38 0x2e6803e20fc55714e7140f516823d2ae27a12c79      1
     39 0x2fd688827eb1f1c1475d7a15831cb8cc663573b5      1
     40 0x30386046cb5727fccc84f4e02606d39bafcc0bab      1
     41 0x30dcdf808bb9313e54c577859e7f41197677831c      1
     42 0x30dec07af807a47c4c5a2ba69a22738831f002b8      1
     43 0x31c091b3d8d20fb3631e22c012e797ec70c20ab4      1
     44 0x31c72d0d03adc76456d3c14a4fe19f8aa8307c16      1
     45 0x34bfcef78cfbbba72e92d30efeea33e36f7e6a22      1
     46 0x363380e1b06fcfdb05006a57a26b143eca920342      1
     47 0x365f7b46973f27740c08a077b66ec810cab39306      1
     48 0x36a5bc205df1ed65c86301022cfc343a6ce546ff      1
     49 0x37c196fbd9a4355af684ff988ff4a17ccb181ebe      1
     50 0x3852471d266d9e2222ca9fdd922bafc904dc49e5      1
     51 0x38e1a00cead40b4086527f5c536079ada06c1ea9      1
     52 0x3af840cd5d04f29c7ba476a5067049a1eb5c2662      1
     53 0x3c6df4b329b9faf209b763b6cf3e2a6017cacafa      1
     54 0x3ccb86a565daa86d1613a634dd16663779bbd63f      1
     55 0x3dbc93aecd50e485ecd2dd6dbe73d1ec19a4d839      1
     56 0x3e5f46ad03a924d0c1ddc9aa311fdd265be312df      1
     57 0x3f526ece92247050be2c66cae5ab6c2cb8bc2a4b      1
     58 0x3fc6c595a3445d56bf5d8322a392f3e027197c6a      1
     59 0x4095301a454311b507c0ef52924a7753f0e1406e      1
     60 0x4478ad8276c7966b2e2594cef6a9e474ec4ad61f      1
     61 0x456fb001c4b6aa2d4c99c587fce826d9ae733529      1
     62 0x466cf4f9bdf6ace5bc7c016eb9aaac5f951a3b2f      1
     63 0x46b249da04697c801cd592030f6ef81f9433398f      1
     64 0x4718d398dd4b0ec472700bd1f6091ed82c49c256      1
     65 0x478bb542f7658d635abba67edb987806dff5b83d      1
     66 0x49bd3fdd5fdf30124dd77c137b178de74fe8649b      1
     67 0x49ca963ef75bceba8e4a5f4ceab5fd326bef6123      1
     68 0x4b4fc57ba00dc7b572af496140effc540d1d4474      1
     69 0x4b6b892b6878e61f421066a01fc03d0648228f82      1
     70 0x4c5287834576f7f1f3732fc2dddef2d2db3fea53      1
     71 0x51ce28855ea50b4eac9656230adf738a5813ca04      1
     72 0x52fd624e3c42c33022ccca829b8d58f98b8dd422      1
     73 0x54cd09bb9e196e29481a72909f66145de9c90c35      1
     74 0x54eef9bcd9e939e1ff4d946e3eabebac1a482e5b      1
     75 0x571244ea8cf2a68928e54ccc8479e6bfab32aab1      1
     76 0x573bc03f18f41b200b949a973d67b453a7ef11a4      1
     77 0x5794f7de490ea3f193ca8b60373db91a86fda811      1
     78 0x58392a0c14b542ad1433c2a8b481a78347aaf1e5      1
     79 0x58847353d941840f261366561dab73bcce277b6c      1
     80 0x596558fd287175afa7ff695f565f65e38547b0e6      1
     81 0x5a683aa37c6e976fa71ce18a28787ab645ba31bc      1
     82 0x5a8116f07937394e6ac2a51995ab6a054c08bf9e      1
     83 0x5b1022d859a46f42fcf24bcd8f5ab616e52b4800      1
     84 0x5c4fe960950ba0e09a72869c3d51fe70f07580e0      1
     85 0x5d7b71cda3ce450b5db872776c3524e574ee0b7a      1
     86 0x5e7b14fa1bea3c0b4b485beb3a3fb837b0adbd6b      1
     87 0x5fc13c5a541041f8fa12fab492d064435eb8d2f5      1
     88 0x6002ebe4144d7f3b0cd10e2ac0c0f357eb1c1c51      1
     89 0x60eaaa669c70166dc7504a860353161cf525da0b      1
     90 0x62c5653b71c26dab6624d9cc81610fb1d6920710      1
     91 0x63569c554e62aa224bec21d43d9a184e8333fbff      1
     92 0x63923511f7b628483f9e160cc173e68a5ded11b6      1
     93 0x64d18532f07855920ab624de8d2a8a365f365e5b      1
     94 0x660d8705cc155c05ce811e9c5dff827b5a6512fe      1
     95 0x66ff8f288c413e61c8ee807088ce301c03076e6e      1
     96 0x6948108b463d5ee9c1cf6626502accea8fd47b7d      1
     97 0x694efab23e7a9e9c0317ee50dbc8fa895f8b81ba      1
     98 0x69e630c314bb7c62307e6bef8d53f96c2b5cb6be      1
     99 0x6b6ae848f555f70944bc99d736fe29fcaecf8b23      1
    100 0x6c802912a21f3c0382d5286ef1e799fc4da3e7d7      1
    101 0x6dfc6d154b7437bee766ba3edb820974538de232      1
    102 0x6e63a4caeccb4f341ee9c9175c9cc554bdb6d10b      1
    103 0x702323dfcc1061e8af7816c483f34a184e5befd9      1
    104 0x703daf8e0b424420004190c3082441e52b7ecd4c      1
    105 0x710b4f7437102c23338448361ddc7f0ef0abc6bd      1
    106 0x72eb30d3ca53f5e839325e2eacf535e70a9e6987      1
    107 0x732bb12525961f5853154deb9d0a4aacd2eb240f      1
    108 0x73a181109fde969bb8da3fe7604ca4be982c3454      1
    109 0x7461c99e39e825344ddb86a16e428d3fad499d86      1
    110 0x75775181080b3684cc3be770ba070d1ecc1ec50d      1
    111 0x75aadbb7c36db057e5ac64183899f118de76df35      1
    112 0x77037050bb27ae8f09118a0c480224d897589c65      1
    113 0x77f80700f974ca52cf52bdc87efcb3b0e0c4f78b      1
    114 0x780701eda98d4437967881553efe18da3be04249      1
    115 0x791d85b7184a76d03d599f0cf940c11033b7e377      1
    116 0x79204328fe037bc69d515040b0e471aa170b75cc      1
    117 0x7a41821e9ad5ccefe77611e58eed762a5a4d7572      1
    118 0x7af061a6ec9616135231cb8de2ed2a0a4140e90b      1
    119 0x7b63b80115c38bf0f422f7bcfa1e92c468bc8333      1
    120 0x7b7ec26a15eb19995be1323251f4494b50f1c6e5      1
    121 0x7cfe27a10f32ec0c7e609ffecf53058444705b55      1
    122 0x7e3cdbbc82edd873f1cb9aa0ed45062fc6a5dbf6      1
    123 0x80aa186f646fa560495c4e965aa983b958a47369      1
    124 0x80c791b450b4d1e82e6fea4991bd2cab5c49d27a      1
    125 0x827e1537171289e51b26b982d530d2210d3676fa      1
    126 0x830ed8630c973d55f24615fd5493a51a1ffcf913      1
    127 0x84300dcc7ca9cf447e886fa17c11fa22557d1af0      1
    128 0x856a4ed4c11a610cc2a4dd706f5642208bf6521d      1
    129 0x85a30ede5e46029457e20d55427ee8673fad7762      1
    130 0x8699793e22403b355185d8ff76e7392f98aafa46      1
    131 0x88b20748727541eddab55df878a7bc6e8335431b      1
    132 0x8a8035f056af830b7205c58c1dc037f826fc2b92      1
    133 0x8a9d3812020d70bd487c79f2ce471ccf87e17ad1      1
    134 0x8cfed4f4bfd8fe8b98efe52f4f708ef512b4d2d7      1
    135 0x8e2f963c1f0ea3ff1fad4c989f860104ad33174c      1
    136 0x8e5150e73d4ff74706df51bf4329199e0cad7350      1
    137 0x8e7a0b4c6550e22880081451c4f75e497e037e3e      1
    138 0x8ed4d3c67ee1a73f5df8ca3e10698f1cd4c5b14c      1
    139 0x920269799de0a4397d65543e5850772c9c10cd2d      1
    140 0x93a6d60ee0a7059d9f508f0af995581278d90e41      1
    141 0x93c838add3ac72b42f65417408f900a7155e1bb8      1
    142 0x9481d8bc76fb1b7d2f95733a6ec0b40d0cb1fffc      1
    143 0x956b4de943668aa6339df8830e66cf99a2dffdab      1
    144 0x96951beae8a3c191800877cdde67dc12a5881584      1
    145 0x970bb85b997af2369b4be78e703e552d4d0d7158      1
    146 0x97284ad97375173d88f760f9758c7d1e8658cdee      1
    147 0x973231cf471f768f0939042e0591da1b15f4799f      1
    148 0x98fe967044e428cc279160435956200b14896071      1
    149 0x99581599e8823ee9130380281478e729e926353f      1
    150 0x995e0505603a19ee5c469d2359427bea68c6e953      1
    151 0x9980db65004727f5ca68e57c616a3cd0db39b5b4      1
    152 0x99fba4de1aa39ad8a1472276a0c334fbe0b53505      1
    153 0x9aaddc5040d2093dc0dafc0803cbfca822341bca      1
    154 0x9af7edc48bc8db5e4afd3af95df143ea29e2e2d6      1
    155 0x9b25f796dcf939b73d30ab1006d62ddde1a0b1d8      1
    156 0x9bc27a47b4413c51f884ae4e4b9170f3a0d7f742      1
    157 0x9cc04d9f5715891d28f0556316b9544e5522e1c2      1
    158 0x9d21f5f14605e228d41fb8d559dd1af39c630224      1
    159 0x9f35af4727fb91114563d8a8f779a792cb557f3f      1
    160 0x9f7facdc938d4d0358b7e7d224ba2d1ff070e346      1
    161 0xa04611ac0438bdada004680628704151c8db044f      1
    162 0xa1e37bfc8638a92b9cb5b731d12d79575e7fe7db      1
    163 0xa28fd178c686beebd6818d6772e56f6e7a6ff5cc      1
    164 0xa3dc5bab4782df8916181f67d259575b864e97d1      1
    165 0xa52126809c92a62ba2f2966b5d8ce695a802da5e      1
    166 0xa5855f4f40a409ffde3a171eb616926aec5d7b9c      1
    167 0xa693fe70a4180c29d0ea522dbd4833dced5fb94a      1
    168 0xa6c88571d0028f47adba983a7240bf12af94633e      1
    169 0xa7625e30eb5356a0b777e9617b55a92dddff2975      1
    170 0xa81725fae8bb3930a95debb0adeaa184b5e9cc10      1
    171 0xa8357ee17cb3ff5a6b9694ddc8fde0ed2ce9d788      1
    172 0xa88fe6fa01fcc112bb2164c6e37d63395b923e5f      1
    173 0xab7473da0af4658ae2879cb641d82e7652721486      1
    174 0xacb818130ac9cd4484848848a0ca3e7dda4bb265      1
    175 0xacc4b6f3ca8d59f631c1148dcaa52b9d7f5c819a      1
    176 0xad119a5520aab0664859a847b8117da7aea55948      1
    177 0xae78f07e15cfd1fb9fed5b7156708271188ad620      1
    178 0xae92ac902fb7937f652dcec459424b122747a86c      1
    179 0xb3ac051668a6eddaa387c166af19e73283d9b0ba      1
    180 0xb5a4e4cb6aaa9f6ea1e00444251682e0f20f86ea      1
    181 0xb7d3a787a39f25457ca511dc3f0591b546f5e02f      1
    182 0xb998874bcb70f1075d2cac03b560245ae2917989      1
    183 0xbae3043c5a35f35ea414cbfe5857cd9eb4d7b742      1
    184 0xbb22b864bf23b8e46afa71a6599854243e6c353b      1
    185 0xbc7b2461bfaa2fb47bd8f632d0c797c3bfd93b93      1
    186 0xbd7a1c16d7b3af4af6d1a0fd7437466d765656ce      1
    187 0xbdddfb2ff21efcc00a2ca5a7d1fdc5798c98f07b      1
    188 0xbebd1e62ba2412d9378847cfd7cc3f759de1504e      1
    189 0xbece662e6b7c1e898c747a21281bcd602afc1367      1
    190 0xbf412be283fe1105c709f1f64d40e9f70057a305      1
    191 0xbf4a907ac732179f0a3ab15c2e9715da19a27801      1
    192 0xc02a3f4424c8834fcb1f31832501f9e3e090262a      1
    193 0xc0ad23a629c6de23c03203b396c5d1e1585111c3      1
    194 0xc142dc5ceb96f73635d66b62bfde2a0eea4e4ce1      1
    195 0xc225c612926ef5f9e9578b865275a02bec6999ee      1
    196 0xc283e9996f7c651eaab983692f0649a2e432ce26      1
    197 0xc37016f3023d0218718ca22a423d9dd00eb58275      1
    198 0xc3c50d8ae310a06800696ff7218458350cfcca1d      1
    199 0xc517297ab0a8d95c1188a703c252688733740cd8      1
    200 0xc5338f905c1a1c061ad097083c78f9c7991a537f      1
    201 0xc5a6d80bc7a2c3093a68e2ad05e8205dc0c0726b      1
    202 0xc5b6a215a753b6a98dd1fea01861d2f2b661fd0b      1
    203 0xc5f5f52479f19c945c97fac8d06570fb14091df3      1
    204 0xc72f40a397453051349f73cf2e2a04fac06e37a3      1
    205 0xc7b57729663ddd90a05af66b42e9d4f71448f099      1
    206 0xc99be1c7d78f0bd8bf024131a54efd252ebee832      1
    207 0xc9f62dbc54f560847c48ba70e7a3281fd6b6e086      1
    208 0xcb4de7db08b4dd0a9954fcfa0a09762f58436df0      1
    209 0xcb94d0cebdd0b78b127dbb1b2a1666b2062a32f5      1
    210 0xcbc7e8a39a0ec84d6b0e8e0dd98655f348ecd44f      1
    211 0xce6f91223a8e4d81252e0fc4fb8ec5899e3b104e      1
    212 0xced183f3efe63c054f36bc2d701cfe4631e61f1a      1
    213 0xcf79b7ba24c70341e3fd5409381e1f00aab275d9      1
    214 0xcffbda2841ef95df3e977982ecc55429b8d82eb5      1
    215 0xd2737fa3f336c1106a65b69379ce4a53f02128e7      1
    216 0xd2d15a46a6a864e6070cbe05fdb7e32636ba0048      1
    217 0xd3522562d0004f080550719f61b846ab0ce5f54d      1
    218 0xd458f62925b3e135680626a0f520098972f93fe9      1
    219 0xd46bc961065f56d92921a5fea66125822a7eeaa0      1
    220 0xd6786fee06c76b8a1c5c4b0ac9025f29d347b69c      1
    221 0xd7d184fec20a50aa7e522aa2f92971b5b1bb2a88      1
    222 0xd7e683c992855b5e8edca21807fb03d4321d4f12      1
    223 0xd8e2b66013ffc6538017b1c6acab712a098ec1bf      1
    224 0xdb3f28bc3bc82a2c577f71fc5a30ff774135f9a2      1
    225 0xdbd8fefa7f919825c22ab1a1ebfc5ce51340f1bb      1
    226 0xdc613e6ecba50027235ac03ab1a844b441e57b43      1
    227 0xdcb6a79adcda176acd549fb95c5d34a395fe622b      1
    228 0xdcd50efb4ccdc9c409b4a4d8eb919ed301c86abe      1
    229 0xdd82bc8cbe61efb3953abe592a497639b2f77243      1
    230 0xde3697dda384ce178d04d8879f7a66423f72a326      1
    231 0xdfd818378e60a1b7e26cf165cc57de8ff0a65478      1
    232 0xe0c16b87b9af4a281bd53e6b6d67f77400dc9625      1
    233 0xe16dce8e44b310953255ffeaa9086f1e3600e999      1
    234 0xe1d23eddf8f0f597a8582d86586999ffc41bb030      1
    235 0xe288a00df4b697606078876788e4d64633cd2e01      1
    236 0xe40dcd3e8587c72cf741293c55d0ecb1fca5a852      1
    237 0xe681e5232da0c9ba5d28e6775bb3b6945eaf50f4      1
    238 0xe6fe14a30631b22d3f78d9ec7869eb62d6e73daa      1
    239 0xe71183e421da96f5f11c642bf6224dfda97e513f      1
    240 0xe7513e5595a751af9f298ecea7741cdd839dc7d4      1
    241 0xe855957a9e3ce38ed80665a7de93e13d418b5e22      1
    242 0xe88807c721a2faacc42992399b406b310c7d84e3      1
    243 0xe889714f7b03a9125a09db9f1d0185f1ea4a4ce2      1
    244 0xe8c9333d6ba367ebd7ed1afd83088b5b5ea15d84      1
    245 0xe95e4c20b70aeeff43469933d46acc2aa3e6acf4      1
    246 0xe9976059d92d5bdf8464b55aa048c4343fa01886      1
    247 0xeb0e194d60f307391bbeffdc6d4bec8f56aac045      1
    248 0xebc26a568603bc913d0806c5438e5ed2e05e87be      1
    249 0xeced6a11834fdf236ac5bdda8c755ea8a8b385b7      1
    250 0xedd983eddf5dee5d0af9bbc30866e7d4c843b975      1
    251 0xf1d83de0ead95747f4ac184b36659c0d538309c8      1
    252 0xf1e1c701b49b1dc2405a9e8ef40f9f88802b80fa      1
    253 0xf25ad24b791e37e83f4dadfe212e0e9bb45a1f8b      1
    254 0xf2aeb2e49ca3bd2ff618b9f19688b07d3c1928a0      1
    255 0xf2dca9d0652c43784522397c11b19694c73074a6      1
    256 0xf4593d840c00832dfe26d4bffa8423e1108323fb      1
    257 0xf5a562cb859b5e037a88f96c0a1fd3f2d0c5fdbe      1
    258 0xf631f197fd1d178d404bc10cf91d25c3d7451fb8      1
    259 0xf6f5c3fa8c9ef5a243af69a2e110b695ab96af3d      1
    260 0xf83d854970c6d7fe7a6f6d26872cc121b996887a      1
    261 0xf8d101b33a948fc07b2dd8cbf392c04b586ed56d      1
    262 0xfbe22e4bc2b9f4ddc44d8b5c1d94e2680f7ab129      1
    263 0xfc540d8334ad4d64405182d9336bb3e1fd62f028      1
    264 0xfd2021d69164ddcd27d2cd1391d5ddc5be1ca9d2      1
    265 0xff13f9b23a16a82c3ccdd29ece6af667eeb15834      1

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