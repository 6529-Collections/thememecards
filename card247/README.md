
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance        contract        
     Length:564         Length:564         Min.   : 1.00   Length:564        
     Class :character   Class :character   1st Qu.: 1.00   Class :character  
     Mode  :character   Mode  :character   Median : 1.00   Mode  :character  
                                           Mean   : 1.08                     
                                           3rd Qu.: 1.00                     
                                           Max.   :10.00                     
         name          
     Length:564        
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

allow_artist1    <- pick(snapshot, contracts=c("Veneer","GIFedOut","GIFedOutII","YouDontBringMeFlowers","Foundation","KnownOrigin","KnownOrigin2","LifeWithArtDerivatives"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("PiecesofMeEditions","BetweentheLinesEditions","ErinMcGeanEditions","HotLipsEditions","LifeWithArtCollageEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("Veneer","GIFedOut","GIFedOutII","YouDontBringMeFlowers","Foundation","KnownOrigin","KnownOrigin2","LifeWithArtDerivatives","PiecesofMeEditions","BetweentheLinesEditions","ErinMcGeanEditions","HotLipsEditions","LifeWithArtCollageEditions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 50 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0a9931ab317d8398e7316f7889f6ca39de328699      1
     2 0x109632e956ca6baf528fe5a9724c43b0c4a63fbe      1
     3 0x133aaeba663680fa7332d5603120330cad7962b0      1
     4 0x168cba97403dc4c37090216dbb4cc77295b1e802      1
     5 0x1b2c142ae4b9c72d2b8957079563d171b7f72892      1
     6 0x1d4b9b250b1bd41daa35d94bf9204ec1b0494ee3      1
     7 0x205a46012bc3c1a2583fdd426679dff276fb257e      1
     8 0x23fed2634fdc629c1a82eb6ed3391efad2a58670      1
     9 0x276ae69309b0cfde2cbdb9151b1ed8abef1501bb      1
    10 0x295dade584004e8094ea8d03e8c92de23a2153e0      1
    11 0x2debdf4427ccbcfdbc7f29d63964499a0ec184f6      1
    12 0x33192b5575be811d84435bc0b2fb5fcd86850e93      1
    13 0x337101def3eeb6f06e071efe02216274507937bb      1
    14 0x34978faf3a9f469da7248d1365ddf69ac099588c      1
    15 0x376f96833b85f5a212adddbce0d172340621d1c5      1
    16 0x3b0aa499cc6acde1d4a7433da6968d7bb8bd8509      1
    17 0x42f9134e9d3bf7eee1f8a5ac2a4328b059e7468c      1
    18 0x4f7106c4c161e138b1a39f47c91c4f1043437fb2      1
    19 0x51ec8bb228af8aed84d935e0cbed0fdb3fc39114      1
    20 0x596558fd287175afa7ff695f565f65e38547b0e6      1
    21 0x5b5d9a8a140606e680176a8aeb240752bcbcb57c      1
    22 0x5cbb31e39ca93ca13d8e9f686bf749b14076bc65      1
    23 0x6974606889a3b1ff76a2347441324b13a4bbb7e6      1
    24 0x72eb30d3ca53f5e839325e2eacf535e70a9e6987      1
    25 0x76b045846db3c0bc9a63b552a32e7542704bca90      1
    26 0x782adafbf47a604f146af4a059908e946eae539f      1
    27 0x7fdab8c244e0b775edeeb9ebb8356b59068c6873      1
    28 0x86db3904cbec100c6e3e6bef91d5d353440b8f2f      1
    29 0x9b7c0814eb341e6a4ca673148e8b577b13e70787      1
    30 0xa43d0698b2afbe80c4080b12f1dc4648dfd0fea7      1
    31 0xa4d8907bf757d268c9d054d8cc274e2f54e7c760      1
    32 0xb0bdd53b627d7e61cfc5c13ef110e47e210fac6f      1
    33 0xb2afbea61b32693eaaef380da99c6d5bdbfe3e7f      1
    34 0xb3c3b3038bd2231b7b61a510102b459e694690a4      1
    35 0xbfacde4341f32b5f77ad81831e128ede4b5e6073      1
    36 0xc01c99b2543e28eff240e90384a1fd757a484927      1
    37 0xc68c7771ec6a6e5d67d62aa9c6f22df69865e401      1
    38 0xd5fab5d372081b4f20fd4636bde96e9061aaa8a4      1
    39 0xd787001237818c10614bc6cef5d39ccb0348a9da      1
    40 0xe7a2d729fbe71f285998e34dd1fe63de840088c9      1
    41 0xeb43b5597e3bde0b0c03ee6731ba7c0247e1581e      1
    42 0xec9e512fe7e90134d8ca7295329ccb0a57c91ecb      1
    43 0xf0b606b9d4d8809eb41a744eb19be23b0dea74df      1
    44 0xf1e1c701b49b1dc2405a9e8ef40f9f88802b80fa      1
    45 0xf207e69a7cc3b4a00fec3039891a77b3df8c3577      1
    46 0xf3290df945e484a4ff846fb5baeb6d1147a7aa69      1
    47 0xf74aa398050f15c7c58750ce71cf0be5440ba47a      1
    48 0xfa0a72e159bbb6694beac1a4641a8b54b03198d2      1
    49 0xfde25d8e5e1e3b03f4d0b1d5c793e3916fd85bc4      1
    50 0xff1a6681aee53d032e8f5fb3d27ccf29d493e922      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 228 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x017ffb7c9e5aff47d9569265d7c78b2494099782      1
      2 0x01909d7ded4d972c0171f2d5a85424cd0250f9bd      1
      3 0x01d49769490d7e0c602b8b6287ee5853feebd36d      1
      4 0x021f3dd7e060570b5ccd81e2b5de9097f4b028b7      1
      5 0x03183fcd2d6a40b466375cdcafe2cd61d0cda659      1
      6 0x039d7c84bc43f8bc311ec21aeef57dcb45a8091d      1
      7 0x04c95c6f8ea7ff7c3f7a2f148fae75650c5a875e      1
      8 0x05427066977dae4b0379839b31000c70014a5a2c      1
      9 0x070472b67db0a109e8ceac51f75b7ec8a52b3725      1
     10 0x077bfc14dd6725f260e1abfd5c942ee13a27091b      1
     11 0x0830337ed8d752b520d5547aadeae4b8d4154199      1
     12 0x09ee3a243463f3ad0a276db78176de6ce28dab0a      1
     13 0x0a1c6f1638adbe3873c7e8dad6c0e3b08285ba07      1
     14 0x0a2542a170aa02b96b588aa3af8b09ab22a9d7ac      1
     15 0x0a2ef7c6098e564361137407bf6e9300e3ded7c2      1
     16 0x0aa64168a95269bd41f0e9c0430df998f919cf01      1
     17 0x0aab24c3531f19139ba3a04d07d33930bc243e7b      1
     18 0x0c544f463600eb7d94b3694f3f3eb171c2f1a93c      1
     19 0x0d779d67a428457cabec145a0f94703d14cd496b      1
     20 0x0ea9f4f7c371e7044d187b3da7850ccaa6370a0c      1
     21 0x0eb7c928edfcf3502ecf4bbebc6cab6c0582ea85      1
     22 0x11f46fff2624737aa0d38507e761b6ef770eb94d      1
     23 0x1321d53babdd597edeb22e606d51ab4515045c0e      1
     24 0x14368bfb69824aa8645a36e7ff38584ccfe0fbb6      1
     25 0x14977b0dbe7e155f9907effecbb70c9b7a05e737      1
     26 0x1744e40b79850c8399558fa6aad09223eaced5cc      1
     27 0x181cb650fb05f91986c89fd8c2c25a8b6f6db864      1
     28 0x1ad60130a2528c6f73a8c6e50758532949627dfd      1
     29 0x1ae50668be1f32179bce00eb203121f00907d808      1
     30 0x1b609fda3c97e90003be00c8223026e0d99f49b4      1
     31 0x1c1cfc6bb84736ca1f789138c58ae586b169b6a4      1
     32 0x1c29fed7470938f31d21eaccb89ecea1d779684f      1
     33 0x1de0df3c7d569fbc9278f1d50016f91f5504365d      1
     34 0x1de4d49959eafa01ab3706aa20707a3f1dbe2424      1
     35 0x1e227911af7eb6ba32ddff2615af2bf5cebabca5      1
     36 0x1e2fa044a25bca1576ed94562db50f51dcc24ca8      1
     37 0x1e8e95cef28e094c461c26545fab6e03580664d1      1
     38 0x1ef7bcc3976056d3f0238b6f8653287d4d2ea24e      1
     39 0x253078874946b4d65e6bba6bfa4e156d3900b16e      1
     40 0x25665e8b95792c23d6167d2d0903fdc6bfef87e9      1
     41 0x2705f89c9d83e35971265cdbc2f7482ce671a476      1
     42 0x2a00f63af45627ff351549106ea735bd973aa86e      1
     43 0x2a1c77a684426fe6ced7a3fca5878fed76fdd1a3      1
     44 0x2c9d055a512b8a47e1bcc78492f88435bb600fe3      1
     45 0x2e0d63ffcb08ea20ff3acdbb72dfec97343885d2      1
     46 0x2fab0249c6a60d715135f12e51de7c130c26289d      1
     47 0x301cc218009608e59f9647f4b7dd8a49be8493af      1
     48 0x3098c04b75c7fecba9c7a80fc24c097995e95681      1
     49 0x330bce303d27df0eb1b856da3464278db1db1ac5      1
     50 0x3327f53140683d19e2baaeb65054133b9b33c53d      1
     51 0x33849c8a4a9539699ed988b3ca6753bb3a597338      1
     52 0x339ea14d82373cdbd329ad32a1f192d3c5c1efc4      1
     53 0x340a61c9f46b585f4430a236be98a7021aa71703      1
     54 0x341c7de61071dfbd0c7275aa2464e2256c0a3172      1
     55 0x365733fc86e2a7d4ab70dfe106497ede37b0d729      1
     56 0x379c8800dd777ef5fbabab6689f42d3b31d09bb9      1
     57 0x37aa0d5ecb03264ec5cc97bea24f75e202790744      1
     58 0x37e17dca626f48c4a10bcd4345a994a63c48c226      1
     59 0x385dc7d2ba88318c3a22de8cbe7b7b0ff54df842      1
     60 0x38a161d47f01b375f505fcb13e73a315819c7eb3      1
     61 0x38fd26289ee3ff78ac060dd01f7f58f8cdd1ea1f      1
     62 0x3cd378c9b1cb5f147ebf1b2c2564118946ae4ba1      1
     63 0x3ef81dc5f1e838e8eccfa3a176d2518eac3fef00      1
     64 0x3f5e9a8578aa0ebf9c897147154efa9fe18f419c      1
     65 0x403655c60503645e122f7db4082ebccfb153c24c      1
     66 0x418cd73af8131dc641c7a79147ff41741317f9a3      1
     67 0x42b46951ae492fde820016ad6432c1360a227dd1      1
     68 0x486f4b109891212c5041a27c5f3f8bf0e6bccd03      1
     69 0x486f5eba364ef3e8835679d9f933de6fd9d7b50e      1
     70 0x497b5af00112f42f57b2496bde21123b1d4f85d6      1
     71 0x49a7dc7b2e0343d7f87d70cfc71b6213c0ea7a71      1
     72 0x4cc2b3075c3cc735958a50ce1b3dd00173e97b04      1
     73 0x4da48ac48b782a7c01e70065e4a51faf2c3f7b09      1
     74 0x4de5ae339b406a8cd5037e9138a62e86aa5352ef      1
     75 0x4e05bc165652140654e0f07b7cb429e5e1b0ed92      1
     76 0x4e323ee346748d7a11c020a38e4feec377779533      1
     77 0x4ec8e85b5cef9df1ac16a9d953ceef453ffa28e1      1
     78 0x507cb3192622ae463215e16f2eb7b53135ae3eb4      1
     79 0x50e2a2e9ac6aa66b7d54517ee02b89bcb58df934      1
     80 0x53c760cb4eacbea81682f6e09b674b249b09f273      1
     81 0x54196238400305778bff5fa200ee1896f6a9d5c2      1
     82 0x5481032ecc006ebddc39e478b9544ba04ed8c2af      1
     83 0x565a9ce52c5a62c42a00dde19c52395ce9044a4e      1
     84 0x56b367a6303b3d626bfbabc625b8e0b5bd9452f8      1
     85 0x56bdc5fe7f9752e7f5a381e394acfe72a724462b      1
     86 0x56e38341b37358d2d89b56bac0db50cf7bcf32a7      1
     87 0x56e507da02d59190a59824308d1f63d40e0278af      1
     88 0x5733899af30ad5f6c644fdb518a0eec023d253ac      1
     89 0x578b076f33c021ca8ec8873be00c734559a99057      1
     90 0x5864bd03561da804ad0dd5a9f901a59217ebe7e4      1
     91 0x5936eeab739c6db9a0bb5de8b79a4bbee65200a0      1
     92 0x59ca0534fce8434f2a38ddd0e50522efd8f22c4c      1
     93 0x5a39e2d5a81fe9e630ee574df446b78f742f5d9e      1
     94 0x5aa71418ac6238c78ea9a032a768dbbbf86bfeca      1
     95 0x5ad44bfff1737931b97f4a7caaefb3b0c915ab72      1
     96 0x5c57abd3548b87ef9babea37ed3abd51fad523a3      1
     97 0x5f804c9a49045dc7f50a580231a2e71fba49badd      1
     98 0x5f8b9b4541ecef965424f1db923806aad626add2      1
     99 0x647eb74a5135a0f24beee3c2d8432adcbb32c2a8      1
    100 0x6486d5dbf01f1edc446c87220003f49edb43972f      1
    101 0x662bd629c0fa1130673cb9bc7ad443dcdb7e39e9      1
    102 0x665654f2d2a151be2d0f8e3697e6ce780f732af2      1
    103 0x689a19f57077f682a1d7cc19d9066f1a834721a2      1
    104 0x6997c9f5fc0b7e5917f0eb46b85d3f6552d11bc3      1
    105 0x6a290fd02a31c78a225b98f3d587d61f4aeba437      1
    106 0x6b293241a55b01ac774dd41d5573a95b68a42333      1
    107 0x6b6ae848f555f70944bc99d736fe29fcaecf8b23      1
    108 0x6d90bffb3da5b53b0fdd2388b7dff4f7341f8ae1      1
    109 0x7056d3ff77c3ac9811bba12783ba3c51a40e6664      1
    110 0x70850885537eac231440d8391aabe8dddf094561      1
    111 0x71217bfb053c861986fc42db8afa026f002fc37d      1
    112 0x74b2c6e55d847fc133c7f0c7908f6ecf4c3ffcbb      1
    113 0x757a21fbd39cccaff681e1930e273b9add0008db      1
    114 0x76a357d6ffd0c6990ed459d1e88c75de57b0b97b      1
    115 0x77039399801f462b4ed13444a266b16355c471bf      1
    116 0x771d9799cb1651eb7b32bd3643c45bb658460c27      1
    117 0x7be7d9f3a7c986fc529a3671b75e6471164ab176      1
    118 0x7c5504688eb4f81ed558550066dc218ffee1f6c8      1
    119 0x7d5300e22db1b2850da1beed10a894893731c1a7      1
    120 0x7fd1d8e388b29ef0b8a00ca07e2266686a5559ee      1
    121 0x8058d889c48b7a79cfdaa54dd6d623b09c9146a3      1
    122 0x810657d67c68cf7299f7ecbc321b293ee064c5df      1
    123 0x814d60499bd401eca426c86ee9b4167967dff8dd      1
    124 0x822ec855cc9f41b592622108e6f3565e976430c6      1
    125 0x82f23de5a474fc904041c357576afe53c30dd250      1
    126 0x83a271446056bc0fc5ff9e62674b41bbcff391c1      1
    127 0x86a35e57968f0d6591b1886594c6b7721fc96943      1
    128 0x879b57e61574c4742222807e397617d800d35be6      1
    129 0x89681dd2759eb8ce7f5e7a878eb1d92aec175765      1
    130 0x897996600a1177dac84899e3d8d3f04d1b7db82e      1
    131 0x8c94072567b278b9fa12192cca899b3ce0ed5fdc      1
    132 0x8cf7071f9418662b4a3a3396d2f08bec7c563e79      1
    133 0x90573f41899660d9f4290c0b4c7d616f617d5ef3      1
    134 0x917d5cfac648ec9cc7dfb2df4b86c99e65a25441      1
    135 0x91b79d7e40da6a5bfb81312d3813f39e91ae5052      1
    136 0x9850bb06c783cc4e7d14a229a0d156b79a066689      1
    137 0x9a521c898cf8f9924c407ab8817d2330ed71c908      1
    138 0x9b49c839bc165294cdb3b1702b6ee04fd7ae18c6      1
    139 0x9ba3fbc8dcb0fbecd8acf0273a11bf357ab98b1c      1
    140 0x9ca09fb674b92965543da65e0273385349d599f0      1
    141 0xa05cd277ba50ec72fc41917437a6c09e39b99bbb      1
    142 0xa23f27fe27b76c8df9b6d2e1c599e1ac80b44bef      1
    143 0xa355065597f1c213160e664b65beda6cabf07bb0      1
    144 0xa3861a789d05597bc046e8e6d5e10ecde947569f      1
    145 0xa3dc5bab4782df8916181f67d259575b864e97d1      1
    146 0xa41031b409ceacfb8b1ed83c7f3afc2cd2db09ec      1
    147 0xa586cbf75fb4b8987bab7d24be4545fcaa0e757c      1
    148 0xa70efb85078129902669bf30dfaaa1ea024bd907      1
    149 0xa728da5f229b59fc8df19af2f1bcece8ef7a5bf9      1
    150 0xa7ecfc1e491baeec24b33ed24bc84916af54f240      1
    151 0xa90dc6d356b622b9783f5020722e6f92b663438f      1
    152 0xaa63ed7b52bc0c03e4c53af307069c98530b3b94      1
    153 0xab6ca2017548a170699890214bfd66583a0c1754      1
    154 0xabf8018c6206623fabc2d5fcfd32917bb72ba11b      1
    155 0xafad902e93f06d177c1e5ebcd3cfe0c80df127ff      1
    156 0xb0800e49781b75b7b3fe72b97a61bb4ea762cdb2      1
    157 0xb0a9494cfa48074cac477233d035c6cb0f8c55ae      1
    158 0xb2d856d91fbe15675985ee4b62a0e725ab7dbe6d      1
    159 0xb2dd242312ba03807d06f51298b2830855f3837d      1
    160 0xb312d5e5ff06cf216a454f37ca58f4ddb7f305cb      1
    161 0xb412f82383d0f9a97e3be04598ea79cd4af96d51      1
    162 0xb46adfe7e10675b1e43f9132da7f00a9ad3642f3      1
    163 0xb64c53fe28949054dfa164fd6dc302b94f699755      1
    164 0xb6515204e28b3be64183c9d50dfb65b49cbf2684      1
    165 0xb7187511a0a0f5a63a538b7bc0abdd1a01979848      1
    166 0xb89e9c6a4ede53d6988747d1a1706342070cd568      1
    167 0xb9252dc5f53aab5cd98af7f382598328dc7c1014      1
    168 0xb944d8c673142aa64548c8660e9b24c2948ccb89      1
    169 0xb98d10d9f6d07ba283bfd21b2dfec050f9ae282a      1
    170 0xbb37a80631a88d46bdcb538fb4dddb1a59a08faf      1
    171 0xbc2f3873cd6650474d7153f3ecc0a4ac23968fff      1
    172 0xbc556e3bde9825748b8232c96b7d93ad4ce911bb      1
    173 0xbc8cc8b1fee0ed52dea5acdfd0002c2a679ff13a      1
    174 0xbf89b34ede7a7dea10bcb8be91e345344ac3039e      1
    175 0xc18d4c2fee42accfe7bfdce98bfacbabf76242e9      1
    176 0xc30986907f64c3de38f266ce9f8295a1869c7692      1
    177 0xc444abef2558712680e3d4abaf201e34f716289f      1
    178 0xc46db2d89327d4c41eb81c43ed5e3dff111f9a8f      1
    179 0xc4e60829d772ca5f566e45211becbf5f4cc763b5      1
    180 0xc4f94270364674f3bcce1487d98a959e72427dba      1
    181 0xc6c5ee2c54c79695ebef26f3171e5b96ed74578d      1
    182 0xc83bcd5fc46f61fe331bd63398f776adcb711f12      1
    183 0xc85b3cbf81e63e0524717efc4e53dc12658b1fc5      1
    184 0xc8b0d32bc09fb11c12c82582825c1e6b624822b8      1
    185 0xc8d748b7caf72aee8e12a02092ed67cf6d8e1371      1
    186 0xcb06bede7cb0a4b333581b6bdcd05f7cc737b9cc      1
    187 0xcb37f1fe07988e6ff6c21b28b986fed13ebfa549      1
    188 0xceae4ca7d082856d140f3672780029d6e90c3dcd      1
    189 0xcfdae6d4701d10d41ba4d08d9342b02a9e18b90c      1
    190 0xcfe28f88c49e38c21b8acbf6a85f783b8c923679      1
    191 0xd2f86cbed86db85c859903ff5f110b15d95b1350      1
    192 0xd38919a385cf9bbade3249dad91b8d880b42bee0      1
    193 0xd4b6595ff5f3c21e0b00edb8947a31110a9c4b8f      1
    194 0xd591ba8be98a12000eb854279bf923e40c20dc6e      1
    195 0xd59c88ab107d1e13b773aa63865382e5bbfb1fd4      1
    196 0xd7342ea20a5afbf24352b5ca61e09844167914cb      1
    197 0xd8c73bcef080f33e37ea5a415bb0778ecd72ce3b      1
    198 0xd9c0e1af68d08c8c00d418431e8c036662a82e37      1
    199 0xda4745f0254044e4b702c9301737c2a0b8c69a6a      1
    200 0xdc22aae1b7de53bdf33ff777edc115e853f6b4cf      1
    201 0xdd0b7b86ff6988de3d9e6f780605a50c48d050c6      1
    202 0xdd3968e776f3cafb758757aae262d392a5fda723      1
    203 0xdfbdb9b9174862ecb1010c39ca72409c1d63b18f      1
    204 0xe2ba803f7271bc51d2beaa5e5c41c7cedeced9f2      1
    205 0xe4a5b683257c0d7b269b88a313fda5f980adf04a      1
    206 0xe4b5ee34deef84754d4d437b2a4f832e24bdd177      1
    207 0xe4c6c46645988bbafd8ef6b4d1b60d969cc857c3      1
    208 0xe66d15146b16cee22f0231dfdc830e7e56c2d4c6      1
    209 0xe8bee2f6bf252ed9efbbd483ca4093ace7350c92      1
    210 0xeb6e8b9a69c976343793446e8ca685bbbf5a7251      1
    211 0xebfd394de050a10b0a9d8408ef395aaa8d269de8      1
    212 0xed2dd5830a5b152546259db69480903db4d3d07f      1
    213 0xed83f0bceef992fa58c8bc8a27269eedb3574922      1
    214 0xedd983eddf5dee5d0af9bbc30866e7d4c843b975      1
    215 0xf1d83de0ead95747f4ac184b36659c0d538309c8      1
    216 0xf24910880c3b65dd40a30969443557b62478e535      1
    217 0xf25ad24b791e37e83f4dadfe212e0e9bb45a1f8b      1
    218 0xf2adc5447d48a456079ec2cdc13e25b0b43d5e04      1
    219 0xf3696a84edcc19cc9db92af3977b027969070912      1
    220 0xf44e62363c6089355c8c80a5f4a5e42800e64dbb      1
    221 0xf54fe7f5f0f8d13ce9b685b8ad167b466b637f0d      1
    222 0xf740a4d6b0d35e3289af8f3a6767836744c3377a      1
    223 0xf76a3c1965b15fa7b109bc7ce18d64246dae6d72      1
    224 0xf9b45750c92b3dab01f26c46160bf981b82f8028      1
    225 0xf9e040b9ffa696966326ba930a0d3eda458478f8      1
    226 0xfb3df26810b5f41172731ee406c2ad577d5c8a5a      1
    227 0xfc3d6045934890e2f59283f8f373998121725291      1
    228 0xfed473cd045563542414e31af846a43bf58fc59a      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 278 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x017ffb7c9e5aff47d9569265d7c78b2494099782      1
      2 0x01909d7ded4d972c0171f2d5a85424cd0250f9bd      1
      3 0x01d49769490d7e0c602b8b6287ee5853feebd36d      1
      4 0x021f3dd7e060570b5ccd81e2b5de9097f4b028b7      1
      5 0x03183fcd2d6a40b466375cdcafe2cd61d0cda659      1
      6 0x039d7c84bc43f8bc311ec21aeef57dcb45a8091d      1
      7 0x04c95c6f8ea7ff7c3f7a2f148fae75650c5a875e      1
      8 0x05427066977dae4b0379839b31000c70014a5a2c      1
      9 0x070472b67db0a109e8ceac51f75b7ec8a52b3725      1
     10 0x077bfc14dd6725f260e1abfd5c942ee13a27091b      1
     11 0x0830337ed8d752b520d5547aadeae4b8d4154199      1
     12 0x09ee3a243463f3ad0a276db78176de6ce28dab0a      1
     13 0x0a1c6f1638adbe3873c7e8dad6c0e3b08285ba07      1
     14 0x0a2542a170aa02b96b588aa3af8b09ab22a9d7ac      1
     15 0x0a2ef7c6098e564361137407bf6e9300e3ded7c2      1
     16 0x0a9931ab317d8398e7316f7889f6ca39de328699      1
     17 0x0aa64168a95269bd41f0e9c0430df998f919cf01      1
     18 0x0aab24c3531f19139ba3a04d07d33930bc243e7b      1
     19 0x0c544f463600eb7d94b3694f3f3eb171c2f1a93c      1
     20 0x0d779d67a428457cabec145a0f94703d14cd496b      1
     21 0x0ea9f4f7c371e7044d187b3da7850ccaa6370a0c      1
     22 0x0eb7c928edfcf3502ecf4bbebc6cab6c0582ea85      1
     23 0x109632e956ca6baf528fe5a9724c43b0c4a63fbe      1
     24 0x11f46fff2624737aa0d38507e761b6ef770eb94d      1
     25 0x1321d53babdd597edeb22e606d51ab4515045c0e      1
     26 0x133aaeba663680fa7332d5603120330cad7962b0      1
     27 0x14368bfb69824aa8645a36e7ff38584ccfe0fbb6      1
     28 0x14977b0dbe7e155f9907effecbb70c9b7a05e737      1
     29 0x168cba97403dc4c37090216dbb4cc77295b1e802      1
     30 0x1744e40b79850c8399558fa6aad09223eaced5cc      1
     31 0x181cb650fb05f91986c89fd8c2c25a8b6f6db864      1
     32 0x1ad60130a2528c6f73a8c6e50758532949627dfd      1
     33 0x1ae50668be1f32179bce00eb203121f00907d808      1
     34 0x1b2c142ae4b9c72d2b8957079563d171b7f72892      1
     35 0x1b609fda3c97e90003be00c8223026e0d99f49b4      1
     36 0x1c1cfc6bb84736ca1f789138c58ae586b169b6a4      1
     37 0x1c29fed7470938f31d21eaccb89ecea1d779684f      1
     38 0x1d4b9b250b1bd41daa35d94bf9204ec1b0494ee3      1
     39 0x1de0df3c7d569fbc9278f1d50016f91f5504365d      1
     40 0x1de4d49959eafa01ab3706aa20707a3f1dbe2424      1
     41 0x1e227911af7eb6ba32ddff2615af2bf5cebabca5      1
     42 0x1e2fa044a25bca1576ed94562db50f51dcc24ca8      1
     43 0x1e8e95cef28e094c461c26545fab6e03580664d1      1
     44 0x1ef7bcc3976056d3f0238b6f8653287d4d2ea24e      1
     45 0x205a46012bc3c1a2583fdd426679dff276fb257e      1
     46 0x23fed2634fdc629c1a82eb6ed3391efad2a58670      1
     47 0x253078874946b4d65e6bba6bfa4e156d3900b16e      1
     48 0x25665e8b95792c23d6167d2d0903fdc6bfef87e9      1
     49 0x2705f89c9d83e35971265cdbc2f7482ce671a476      1
     50 0x276ae69309b0cfde2cbdb9151b1ed8abef1501bb      1
     51 0x295dade584004e8094ea8d03e8c92de23a2153e0      1
     52 0x2a00f63af45627ff351549106ea735bd973aa86e      1
     53 0x2a1c77a684426fe6ced7a3fca5878fed76fdd1a3      1
     54 0x2c9d055a512b8a47e1bcc78492f88435bb600fe3      1
     55 0x2debdf4427ccbcfdbc7f29d63964499a0ec184f6      1
     56 0x2e0d63ffcb08ea20ff3acdbb72dfec97343885d2      1
     57 0x2fab0249c6a60d715135f12e51de7c130c26289d      1
     58 0x301cc218009608e59f9647f4b7dd8a49be8493af      1
     59 0x3098c04b75c7fecba9c7a80fc24c097995e95681      1
     60 0x330bce303d27df0eb1b856da3464278db1db1ac5      1
     61 0x33192b5575be811d84435bc0b2fb5fcd86850e93      1
     62 0x3327f53140683d19e2baaeb65054133b9b33c53d      1
     63 0x337101def3eeb6f06e071efe02216274507937bb      1
     64 0x33849c8a4a9539699ed988b3ca6753bb3a597338      1
     65 0x339ea14d82373cdbd329ad32a1f192d3c5c1efc4      1
     66 0x340a61c9f46b585f4430a236be98a7021aa71703      1
     67 0x341c7de61071dfbd0c7275aa2464e2256c0a3172      1
     68 0x34978faf3a9f469da7248d1365ddf69ac099588c      1
     69 0x365733fc86e2a7d4ab70dfe106497ede37b0d729      1
     70 0x376f96833b85f5a212adddbce0d172340621d1c5      1
     71 0x379c8800dd777ef5fbabab6689f42d3b31d09bb9      1
     72 0x37aa0d5ecb03264ec5cc97bea24f75e202790744      1
     73 0x37e17dca626f48c4a10bcd4345a994a63c48c226      1
     74 0x385dc7d2ba88318c3a22de8cbe7b7b0ff54df842      1
     75 0x38a161d47f01b375f505fcb13e73a315819c7eb3      1
     76 0x38fd26289ee3ff78ac060dd01f7f58f8cdd1ea1f      1
     77 0x3b0aa499cc6acde1d4a7433da6968d7bb8bd8509      1
     78 0x3cd378c9b1cb5f147ebf1b2c2564118946ae4ba1      1
     79 0x3ef81dc5f1e838e8eccfa3a176d2518eac3fef00      1
     80 0x3f5e9a8578aa0ebf9c897147154efa9fe18f419c      1
     81 0x403655c60503645e122f7db4082ebccfb153c24c      1
     82 0x418cd73af8131dc641c7a79147ff41741317f9a3      1
     83 0x42b46951ae492fde820016ad6432c1360a227dd1      1
     84 0x42f9134e9d3bf7eee1f8a5ac2a4328b059e7468c      1
     85 0x486f4b109891212c5041a27c5f3f8bf0e6bccd03      1
     86 0x486f5eba364ef3e8835679d9f933de6fd9d7b50e      1
     87 0x497b5af00112f42f57b2496bde21123b1d4f85d6      1
     88 0x49a7dc7b2e0343d7f87d70cfc71b6213c0ea7a71      1
     89 0x4cc2b3075c3cc735958a50ce1b3dd00173e97b04      1
     90 0x4da48ac48b782a7c01e70065e4a51faf2c3f7b09      1
     91 0x4de5ae339b406a8cd5037e9138a62e86aa5352ef      1
     92 0x4e05bc165652140654e0f07b7cb429e5e1b0ed92      1
     93 0x4e323ee346748d7a11c020a38e4feec377779533      1
     94 0x4ec8e85b5cef9df1ac16a9d953ceef453ffa28e1      1
     95 0x4f7106c4c161e138b1a39f47c91c4f1043437fb2      1
     96 0x507cb3192622ae463215e16f2eb7b53135ae3eb4      1
     97 0x50e2a2e9ac6aa66b7d54517ee02b89bcb58df934      1
     98 0x51ec8bb228af8aed84d935e0cbed0fdb3fc39114      1
     99 0x53c760cb4eacbea81682f6e09b674b249b09f273      1
    100 0x54196238400305778bff5fa200ee1896f6a9d5c2      1
    101 0x5481032ecc006ebddc39e478b9544ba04ed8c2af      1
    102 0x565a9ce52c5a62c42a00dde19c52395ce9044a4e      1
    103 0x56b367a6303b3d626bfbabc625b8e0b5bd9452f8      1
    104 0x56bdc5fe7f9752e7f5a381e394acfe72a724462b      1
    105 0x56e38341b37358d2d89b56bac0db50cf7bcf32a7      1
    106 0x56e507da02d59190a59824308d1f63d40e0278af      1
    107 0x5733899af30ad5f6c644fdb518a0eec023d253ac      1
    108 0x578b076f33c021ca8ec8873be00c734559a99057      1
    109 0x5864bd03561da804ad0dd5a9f901a59217ebe7e4      1
    110 0x5936eeab739c6db9a0bb5de8b79a4bbee65200a0      1
    111 0x596558fd287175afa7ff695f565f65e38547b0e6      1
    112 0x59ca0534fce8434f2a38ddd0e50522efd8f22c4c      1
    113 0x5a39e2d5a81fe9e630ee574df446b78f742f5d9e      1
    114 0x5aa71418ac6238c78ea9a032a768dbbbf86bfeca      1
    115 0x5ad44bfff1737931b97f4a7caaefb3b0c915ab72      1
    116 0x5b5d9a8a140606e680176a8aeb240752bcbcb57c      1
    117 0x5c57abd3548b87ef9babea37ed3abd51fad523a3      1
    118 0x5cbb31e39ca93ca13d8e9f686bf749b14076bc65      1
    119 0x5f804c9a49045dc7f50a580231a2e71fba49badd      1
    120 0x5f8b9b4541ecef965424f1db923806aad626add2      1
    121 0x647eb74a5135a0f24beee3c2d8432adcbb32c2a8      1
    122 0x6486d5dbf01f1edc446c87220003f49edb43972f      1
    123 0x662bd629c0fa1130673cb9bc7ad443dcdb7e39e9      1
    124 0x665654f2d2a151be2d0f8e3697e6ce780f732af2      1
    125 0x689a19f57077f682a1d7cc19d9066f1a834721a2      1
    126 0x6974606889a3b1ff76a2347441324b13a4bbb7e6      1
    127 0x6997c9f5fc0b7e5917f0eb46b85d3f6552d11bc3      1
    128 0x6a290fd02a31c78a225b98f3d587d61f4aeba437      1
    129 0x6b293241a55b01ac774dd41d5573a95b68a42333      1
    130 0x6b6ae848f555f70944bc99d736fe29fcaecf8b23      1
    131 0x6d90bffb3da5b53b0fdd2388b7dff4f7341f8ae1      1
    132 0x7056d3ff77c3ac9811bba12783ba3c51a40e6664      1
    133 0x70850885537eac231440d8391aabe8dddf094561      1
    134 0x71217bfb053c861986fc42db8afa026f002fc37d      1
    135 0x72eb30d3ca53f5e839325e2eacf535e70a9e6987      1
    136 0x74b2c6e55d847fc133c7f0c7908f6ecf4c3ffcbb      1
    137 0x757a21fbd39cccaff681e1930e273b9add0008db      1
    138 0x76a357d6ffd0c6990ed459d1e88c75de57b0b97b      1
    139 0x76b045846db3c0bc9a63b552a32e7542704bca90      1
    140 0x77039399801f462b4ed13444a266b16355c471bf      1
    141 0x771d9799cb1651eb7b32bd3643c45bb658460c27      1
    142 0x782adafbf47a604f146af4a059908e946eae539f      1
    143 0x7be7d9f3a7c986fc529a3671b75e6471164ab176      1
    144 0x7c5504688eb4f81ed558550066dc218ffee1f6c8      1
    145 0x7d5300e22db1b2850da1beed10a894893731c1a7      1
    146 0x7fd1d8e388b29ef0b8a00ca07e2266686a5559ee      1
    147 0x7fdab8c244e0b775edeeb9ebb8356b59068c6873      1
    148 0x8058d889c48b7a79cfdaa54dd6d623b09c9146a3      1
    149 0x810657d67c68cf7299f7ecbc321b293ee064c5df      1
    150 0x814d60499bd401eca426c86ee9b4167967dff8dd      1
    151 0x822ec855cc9f41b592622108e6f3565e976430c6      1
    152 0x82f23de5a474fc904041c357576afe53c30dd250      1
    153 0x83a271446056bc0fc5ff9e62674b41bbcff391c1      1
    154 0x86a35e57968f0d6591b1886594c6b7721fc96943      1
    155 0x86db3904cbec100c6e3e6bef91d5d353440b8f2f      1
    156 0x879b57e61574c4742222807e397617d800d35be6      1
    157 0x89681dd2759eb8ce7f5e7a878eb1d92aec175765      1
    158 0x897996600a1177dac84899e3d8d3f04d1b7db82e      1
    159 0x8c94072567b278b9fa12192cca899b3ce0ed5fdc      1
    160 0x8cf7071f9418662b4a3a3396d2f08bec7c563e79      1
    161 0x90573f41899660d9f4290c0b4c7d616f617d5ef3      1
    162 0x917d5cfac648ec9cc7dfb2df4b86c99e65a25441      1
    163 0x91b79d7e40da6a5bfb81312d3813f39e91ae5052      1
    164 0x9850bb06c783cc4e7d14a229a0d156b79a066689      1
    165 0x9a521c898cf8f9924c407ab8817d2330ed71c908      1
    166 0x9b49c839bc165294cdb3b1702b6ee04fd7ae18c6      1
    167 0x9b7c0814eb341e6a4ca673148e8b577b13e70787      1
    168 0x9ba3fbc8dcb0fbecd8acf0273a11bf357ab98b1c      1
    169 0x9ca09fb674b92965543da65e0273385349d599f0      1
    170 0xa05cd277ba50ec72fc41917437a6c09e39b99bbb      1
    171 0xa23f27fe27b76c8df9b6d2e1c599e1ac80b44bef      1
    172 0xa355065597f1c213160e664b65beda6cabf07bb0      1
    173 0xa3861a789d05597bc046e8e6d5e10ecde947569f      1
    174 0xa3dc5bab4782df8916181f67d259575b864e97d1      1
    175 0xa41031b409ceacfb8b1ed83c7f3afc2cd2db09ec      1
    176 0xa43d0698b2afbe80c4080b12f1dc4648dfd0fea7      1
    177 0xa4d8907bf757d268c9d054d8cc274e2f54e7c760      1
    178 0xa586cbf75fb4b8987bab7d24be4545fcaa0e757c      1
    179 0xa70efb85078129902669bf30dfaaa1ea024bd907      1
    180 0xa728da5f229b59fc8df19af2f1bcece8ef7a5bf9      1
    181 0xa7ecfc1e491baeec24b33ed24bc84916af54f240      1
    182 0xa90dc6d356b622b9783f5020722e6f92b663438f      1
    183 0xaa63ed7b52bc0c03e4c53af307069c98530b3b94      1
    184 0xab6ca2017548a170699890214bfd66583a0c1754      1
    185 0xabf8018c6206623fabc2d5fcfd32917bb72ba11b      1
    186 0xafad902e93f06d177c1e5ebcd3cfe0c80df127ff      1
    187 0xb0800e49781b75b7b3fe72b97a61bb4ea762cdb2      1
    188 0xb0a9494cfa48074cac477233d035c6cb0f8c55ae      1
    189 0xb0bdd53b627d7e61cfc5c13ef110e47e210fac6f      1
    190 0xb2afbea61b32693eaaef380da99c6d5bdbfe3e7f      1
    191 0xb2d856d91fbe15675985ee4b62a0e725ab7dbe6d      1
    192 0xb2dd242312ba03807d06f51298b2830855f3837d      1
    193 0xb312d5e5ff06cf216a454f37ca58f4ddb7f305cb      1
    194 0xb3c3b3038bd2231b7b61a510102b459e694690a4      1
    195 0xb412f82383d0f9a97e3be04598ea79cd4af96d51      1
    196 0xb46adfe7e10675b1e43f9132da7f00a9ad3642f3      1
    197 0xb64c53fe28949054dfa164fd6dc302b94f699755      1
    198 0xb6515204e28b3be64183c9d50dfb65b49cbf2684      1
    199 0xb7187511a0a0f5a63a538b7bc0abdd1a01979848      1
    200 0xb89e9c6a4ede53d6988747d1a1706342070cd568      1
    201 0xb9252dc5f53aab5cd98af7f382598328dc7c1014      1
    202 0xb944d8c673142aa64548c8660e9b24c2948ccb89      1
    203 0xb98d10d9f6d07ba283bfd21b2dfec050f9ae282a      1
    204 0xbb37a80631a88d46bdcb538fb4dddb1a59a08faf      1
    205 0xbc2f3873cd6650474d7153f3ecc0a4ac23968fff      1
    206 0xbc556e3bde9825748b8232c96b7d93ad4ce911bb      1
    207 0xbc8cc8b1fee0ed52dea5acdfd0002c2a679ff13a      1
    208 0xbf89b34ede7a7dea10bcb8be91e345344ac3039e      1
    209 0xbfacde4341f32b5f77ad81831e128ede4b5e6073      1
    210 0xc01c99b2543e28eff240e90384a1fd757a484927      1
    211 0xc18d4c2fee42accfe7bfdce98bfacbabf76242e9      1
    212 0xc30986907f64c3de38f266ce9f8295a1869c7692      1
    213 0xc444abef2558712680e3d4abaf201e34f716289f      1
    214 0xc46db2d89327d4c41eb81c43ed5e3dff111f9a8f      1
    215 0xc4e60829d772ca5f566e45211becbf5f4cc763b5      1
    216 0xc4f94270364674f3bcce1487d98a959e72427dba      1
    217 0xc68c7771ec6a6e5d67d62aa9c6f22df69865e401      1
    218 0xc6c5ee2c54c79695ebef26f3171e5b96ed74578d      1
    219 0xc83bcd5fc46f61fe331bd63398f776adcb711f12      1
    220 0xc85b3cbf81e63e0524717efc4e53dc12658b1fc5      1
    221 0xc8b0d32bc09fb11c12c82582825c1e6b624822b8      1
    222 0xc8d748b7caf72aee8e12a02092ed67cf6d8e1371      1
    223 0xcb06bede7cb0a4b333581b6bdcd05f7cc737b9cc      1
    224 0xcb37f1fe07988e6ff6c21b28b986fed13ebfa549      1
    225 0xceae4ca7d082856d140f3672780029d6e90c3dcd      1
    226 0xcfdae6d4701d10d41ba4d08d9342b02a9e18b90c      1
    227 0xcfe28f88c49e38c21b8acbf6a85f783b8c923679      1
    228 0xd2f86cbed86db85c859903ff5f110b15d95b1350      1
    229 0xd38919a385cf9bbade3249dad91b8d880b42bee0      1
    230 0xd4b6595ff5f3c21e0b00edb8947a31110a9c4b8f      1
    231 0xd591ba8be98a12000eb854279bf923e40c20dc6e      1
    232 0xd59c88ab107d1e13b773aa63865382e5bbfb1fd4      1
    233 0xd5fab5d372081b4f20fd4636bde96e9061aaa8a4      1
    234 0xd7342ea20a5afbf24352b5ca61e09844167914cb      1
    235 0xd787001237818c10614bc6cef5d39ccb0348a9da      1
    236 0xd8c73bcef080f33e37ea5a415bb0778ecd72ce3b      1
    237 0xd9c0e1af68d08c8c00d418431e8c036662a82e37      1
    238 0xda4745f0254044e4b702c9301737c2a0b8c69a6a      1
    239 0xdc22aae1b7de53bdf33ff777edc115e853f6b4cf      1
    240 0xdd0b7b86ff6988de3d9e6f780605a50c48d050c6      1
    241 0xdd3968e776f3cafb758757aae262d392a5fda723      1
    242 0xdfbdb9b9174862ecb1010c39ca72409c1d63b18f      1
    243 0xe2ba803f7271bc51d2beaa5e5c41c7cedeced9f2      1
    244 0xe4a5b683257c0d7b269b88a313fda5f980adf04a      1
    245 0xe4b5ee34deef84754d4d437b2a4f832e24bdd177      1
    246 0xe4c6c46645988bbafd8ef6b4d1b60d969cc857c3      1
    247 0xe66d15146b16cee22f0231dfdc830e7e56c2d4c6      1
    248 0xe7a2d729fbe71f285998e34dd1fe63de840088c9      1
    249 0xe8bee2f6bf252ed9efbbd483ca4093ace7350c92      1
    250 0xeb43b5597e3bde0b0c03ee6731ba7c0247e1581e      1
    251 0xeb6e8b9a69c976343793446e8ca685bbbf5a7251      1
    252 0xebfd394de050a10b0a9d8408ef395aaa8d269de8      1
    253 0xec9e512fe7e90134d8ca7295329ccb0a57c91ecb      1
    254 0xed2dd5830a5b152546259db69480903db4d3d07f      1
    255 0xed83f0bceef992fa58c8bc8a27269eedb3574922      1
    256 0xedd983eddf5dee5d0af9bbc30866e7d4c843b975      1
    257 0xf0b606b9d4d8809eb41a744eb19be23b0dea74df      1
    258 0xf1d83de0ead95747f4ac184b36659c0d538309c8      1
    259 0xf1e1c701b49b1dc2405a9e8ef40f9f88802b80fa      1
    260 0xf207e69a7cc3b4a00fec3039891a77b3df8c3577      1
    261 0xf24910880c3b65dd40a30969443557b62478e535      1
    262 0xf25ad24b791e37e83f4dadfe212e0e9bb45a1f8b      1
    263 0xf2adc5447d48a456079ec2cdc13e25b0b43d5e04      1
    264 0xf3290df945e484a4ff846fb5baeb6d1147a7aa69      1
    265 0xf3696a84edcc19cc9db92af3977b027969070912      1
    266 0xf44e62363c6089355c8c80a5f4a5e42800e64dbb      1
    267 0xf54fe7f5f0f8d13ce9b685b8ad167b466b637f0d      1
    268 0xf740a4d6b0d35e3289af8f3a6767836744c3377a      1
    269 0xf74aa398050f15c7c58750ce71cf0be5440ba47a      1
    270 0xf76a3c1965b15fa7b109bc7ce18d64246dae6d72      1
    271 0xf9b45750c92b3dab01f26c46160bf981b82f8028      1
    272 0xf9e040b9ffa696966326ba930a0d3eda458478f8      1
    273 0xfa0a72e159bbb6694beac1a4641a8b54b03198d2      1
    274 0xfb3df26810b5f41172731ee406c2ad577d5c8a5a      1
    275 0xfc3d6045934890e2f59283f8f373998121725291      1
    276 0xfde25d8e5e1e3b03f4d0b1d5c793e3916fd85bc4      1
    277 0xfed473cd045563542414e31af846a43bf58fc59a      1
    278 0xff1a6681aee53d032e8f5fb3d27ccf29d493e922      1

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
