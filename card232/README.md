
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance       contract        
     Length:390         Length:390         Min.   :1.00   Length:390        
     Class :character   Class :character   1st Qu.:1.00   Class :character  
     Mode  :character   Mode  :character   Median :1.00   Mode  :character  
                                           Mean   :1.01                     
                                           3rd Qu.:1.00                     
                                           Max.   :2.00                     
         name          
     Length:390        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 19846969 # https://etherscan.io/block/19846969
block_hash <- "0xb4c64a3b41c50d1f5b51508dd56b403d6e74fcc6045f280c934ba9e8fb0654cf"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4688 

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

allow_artist1    <- pick(snapshot, contracts=c("Anxiety","IntrinsicEssence","JourneyThroughLens","ZhannetPodobed","CarpeDiem","MakersPlace","Foundation","KnownOrigin","Paintings","DigitalAbstraction"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("JoynGaslessEditions","AirdropsAndEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("Anxiety","IntrinsicEssence","JourneyThroughLens","ZhannetPodobed","CarpeDiem","MakersPlace","Foundation","KnownOrigin","Paintings","DigitalAbstraction","JoynGaslessEditions","AirdropsAndEditions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 107 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x0075a6e987bb3f7a6c41b095fb5cdc6767bd3ee8      1
      2 0x00b87c1b8c5f615c514f6d8174ab59633f1c9ed7      1
      3 0x00ff192363430a35abbf968c535b64147e88abdb      1
      4 0x052564eb0fd8b340803df55def89c25c432f43f4      1
      5 0x0a8825c03100e6cd1a82266729bf90f7fe71dc6d      1
      6 0x0cf81cc563266b405e3f66f525e83d9e7262059b      1
      7 0x0d2f9fbc29b788a79fd53d93f9b8d495235cec3e      1
      8 0x11918a9a1cd1abe8e302a8ab8e6f2068f85fc5ea      1
      9 0x12475379baa30ef38ffeae8afc1067d1cbf94a8b      1
     10 0x1257400e85b69c4efc4de33170ca11bec259bc7e      1
     11 0x131c72678f8826612467b5ebf0ec485dcfb5065c      1
     12 0x13f4691f0084e6d1e595e43a66ce50dff5559d44      1
     13 0x14977b0dbe7e155f9907effecbb70c9b7a05e737      1
     14 0x19461698453e26b98cee5b984e1a86e13c0f68be      1
     15 0x1a49096ba1a258b51f7e60c703dcf799f0959cf8      1
     16 0x1a60dfb071b039c6e33dcb3220891c83da72c1be      1
     17 0x213f80d641de4945f547b4a2c7200736b0b48bf8      1
     18 0x243b34d9f7dc0808c7d4117bfbd0ee6de670c285      1
     19 0x2670f8569345d20ee7fd191fd9937823b1909d03      1
     20 0x2b85b841b85fe6bf05e1a41330fe13882ed1f286      1
     21 0x2baf9d01cf1f51b270aaacc80ea08b1323fa1be6      1
     22 0x2c9f8cecae8242654d0bd744e3b35f5c19ab4066      1
     23 0x2cd07965af7339ebbbc19a83072d75df92a6956d      1
     24 0x2d6836b5d89e319f38ac3c058f4c835f0c69b398      1
     25 0x2d80d89c6e28bffe6adb9d51be3d8cd339e0bc44      1
     26 0x3912a27a0429ad3bef065a76fd9ca148531a05af      1
     27 0x3bbcd028b9f2d5dc63613d1c03bef0545b2a66f2      1
     28 0x3f304c6721f35ff9af00fd32650c8e0a982180ab      1
     29 0x3f5535777b429fbdafb21d6f6f47849e479b38d0      1
     30 0x424a1d4a95947d969ef09f8d3aac97f943e14488      1
     31 0x445db19a4e04ff5624744d5aba50c2590612093a      1
     32 0x4aefbc09674c2aa7e3d0ed2500ee56323d0a35ff      1
     33 0x4b434c35b7b43e5ccd8700074a817cae119a04f3      1
     34 0x5070781ec368ddd83f9e28193e82668f9edc4932      1
     35 0x532e31f49828dda5009da88609e4527c2c7d0db8      1
     36 0x54966b7b70eba0a6bd5f5ab5e292f266e2f89c89      1
     37 0x55c0c8186541868c03f434e9606e112ee0153965      1
     38 0x576b9d138f67c7b1e70074b265b608a0b1ddbd7d      1
     39 0x5773271490cd30e904397779551380f104d134dc      1
     40 0x57798ea2de5e864959a3f00a12f9224147edf7c8      1
     41 0x60c4e51a1c797f562e628cddb4aeb84ac9f27d41      1
     42 0x614c3ce52760343fccb7d908eda39f114620a1e7      1
     43 0x63b7d0006102e672e6936fcfe6197e4c782f51d4      1
     44 0x6483313c87d8cf60ab4a21407a67e4ac7fab6031      1
     45 0x64d18532f07855920ab624de8d2a8a365f365e5b      1
     46 0x66143651495a1ccbfd2f708af8336c580935349a      1
     47 0x68ddc4e9c6daefd31e349cbf7217226b000aecdd      1
     48 0x6960770b484a5a455e39f098549eb8dbba62d434      1
     49 0x6a7415b36133c6c3957e4772b8009068e170c648      1
     50 0x6c5f56ec2cbbe1c9a8539407b121834d2c8f8dfe      1
     51 0x6db5e720a947c7cc2f3fcad4cf5058402fc456c6      1
     52 0x6e388502b891ca05eb52525338172f261c31b7d3      1
     53 0x6e5b0de0add71e1052b11abbfe6c433dd0466fb4      1
     54 0x71a548c09c6d09df38811c6df025d281e878258b      1
     55 0x72ab6547ee820299d843f38147d017ca817b4a6f      1
     56 0x742875d6c2533f1cf131adb1cb26526740cb6c91      1
     57 0x75a56cd64e10c4f07a6d2cb9027580b7b9a728db      1
     58 0x75aadbb7c36db057e5ac64183899f118de76df35      1
     59 0x7a1a0bf0223d0b956f98766eb2427a92ee8266a2      1
     60 0x819395f9310c601f1cec217eaa711e0e200c8a78      1
     61 0x867476e2a84de15852311bd3299b41626c778be4      1
     62 0x86a35e57968f0d6591b1886594c6b7721fc96943      1
     63 0x8f77d28c23479f2abeb72a31c577ac0ae5de1ebf      1
     64 0x93670564b92634dea224e7f6932343fa93c22554      1
     65 0x93a6d60ee0a7059d9f508f0af995581278d90e41      1
     66 0x93c838add3ac72b42f65417408f900a7155e1bb8      1
     67 0x94c147ef72a679e2dc3a825e4b881b42beef258f      1
     68 0x97d9225418a7e8fafbcaa017573b0476b939bafe      1
     69 0x9a7eed6209dce4744cabe2945bfe7d31217b1547      1
     70 0xa1a1ac9a2773ef585ac461c0850b3268829462eb      1
     71 0xa2c62a66f6660166838b95db60f234dfb59e765e      1
     72 0xa3603f2623e3280c78865251a1b5b67c5a300d51      1
     73 0xa5430730f12f1128bf10dfba38c8e00bc4d90eea      1
     74 0xa82c20f746fbc7a8e22329ac38e1a1cc43db8e94      1
     75 0xab4273e7137f9531fcb47d558d9bab0e726e6937      1
     76 0xb7325c055854ef3ee451aae865cd68f05113d05c      1
     77 0xb7b2baa66b65e454fb5a2fc7ced0f07e46c871be      1
     78 0xba3a85ebd1d08df696afa8d278785e032b45a083      1
     79 0xbcd78799974f8a5b9eda6b5b0ae6d90d29d3f9e0      1
     80 0xc18ad8c7f967e736bf1bf6c85b35b5ca4743236f      1
     81 0xc35902fffe09dabc98ef31da313255d35eb4711f      1
     82 0xc3d3f90e2210bbe23690e32b80a81745eb4db807      1
     83 0xc3fadff037048a820391fc6ca70d42de8078cef6      1
     84 0xc6e89a98aec81debbdc2105ebc6d070f6b0f7f22      1
     85 0xcd2fb99047946b44058af51317d99dfcdb199999      1
     86 0xcdc626617b2f975f0d33dcb76f5607436e8efd1a      1
     87 0xcebb735535fe629d12947cfa5e0df7fdc0955055      1
     88 0xd48173c381685c40c554e33164b9370507d1b236      1
     89 0xd9d4e0f4c81d13edf3ee8cec6ff026a06d418301      1
     90 0xdd1f61e95ca9ec479de81f869921ed191dfeeba8      1
     91 0xe25f5ffa52933e17b40b252d9a51c4fe14cc4750      1
     92 0xe4774f2df243e526330a9cfb9998a32ab1f17378      1
     93 0xe6d6758d2fc0a7a0c476794855d035f3bcba245f      1
     94 0xe8fd1914571b437d8b2a7b4df52f296ba9ac5d40      1
     95 0xe9c963a60b0da4304b65df8e519705e1195c6163      1
     96 0xea53a482c24d19982f7c2a3e48d543da3f60e711      1
     97 0xeba9d7523c1842907587c92dedccbeff49c9ad77      1
     98 0xecb1ea77cddd83a0696c2d44e8bf4a6e40c57a8f      1
     99 0xf013e5f697e84c6831220a21a96556242ee9ad86      1
    100 0xf11e0fd4e6cb70bced7ceb27fdb6d6a6bce23f94      1
    101 0xf25ad24b791e37e83f4dadfe212e0e9bb45a1f8b      1
    102 0xf3478bf2d1120590fbf4cf4544c70d20237037d8      1
    103 0xf4a12bc4596e1c3e19d512f76325b52d72d375cf      1
    104 0xf6620eba0895d90edd97ede6db19f09f9970a47b      1
    105 0xfb3df26810b5f41172731ee406c2ad577d5c8a5a      1
    106 0xfbc76261fd55cf91b81a97dbcc2b3f6118f2b935      1
    107 0xff13f9b23a16a82c3ccdd29ece6af667eeb15834      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 64 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0264a803979bd0b485a6701f1ab17aab3bcd8ff7      1
     2 0x1182ed989ebb1cac8d6312bf08162b5c4e8d621f      1
     3 0x123c6d03cc6810e35b9e396402c55bc501602b8e      1
     4 0x126e7568537e9062c58083d43a275358cbe9fe52      1
     5 0x158f25e42485952f063d4f5724339d4ced376432      1
     6 0x165c95786ce1d6e3bdd6c51aff29438e65d2ce86      1
     7 0x169f66fe4ec8c14c8feca1ce4d1b34561f5413fe      1
     8 0x18194a7c958ac751c3fdd11afe4aa0a4bbde49a2      1
     9 0x185481022e7082b5d2d2aa2332552df3d65cdcce      1
    10 0x19fedef56411bb49afaf65f7e4be50629be17632      1
    11 0x2334d91317950c19a6c21c69d69f5ab8a8f275fa      1
    12 0x2844a1f5c5164b9adae211df8931838a2d33b5ed      1
    13 0x2a839d803d6b2db089408f2e20a23c07ea9b56fe      1
    14 0x3098c04b75c7fecba9c7a80fc24c097995e95681      1
    15 0x3279e7d7e140055858b8c9ba4beb615281710ac9      1
    16 0x33a34e27a81436ba9d79276406a285e89a8bd8a8      1
    17 0x3543538abe754a5edbb3c97736cae4b30a77e0ba      1
    18 0x36e40e5c762730b4f6d7e8fff1464b9949537fc9      1
    19 0x37009f9ae9dcc0a6d59204e1aeaf9da4968caec8      1
    20 0x372b974a8819faaeb1f0ce3912dc6c81688cd6fe      1
    21 0x382c6f4dd388a71458aaefa837b385ac6c33ddf0      1
    22 0x39e96a0c67056fe4a8d294f396b927347747dc3f      1
    23 0x3d12ac8da240f812191704c852834fb9a6591672      1
    24 0x3d4b9f9967cff180921bf507725712b6a4f90ce7      1
    25 0x41b52cf255d0708b399ee6a48253e45aff929b60      1
    26 0x46091f24f499344fb0cc79e78948adb48cb7ca6e      1
    27 0x47c553e889f7bfa75cf3775473180a04e9effcfa      1
    28 0x4b2ef7127640964bf5cef3d1bf8d8f72d8d386f5      1
    29 0x4f99d481981f464d824592604f58bcee9cb8abf6      1
    30 0x540a684e5ccc3dd20639590cc1750d8b5f3a3b66      1
    31 0x583c3d9e5fd01bd3471d1ceb5f27737f36933080      1
    32 0x5a04ad067b3a022bbc03dd49f105d462b6bbc1ff      1
    33 0x5dc23549cdc0ea41a9b6ee688e0bc24070649cd7      1
    34 0x68bca5a8bdebe05fb8a6648c7316b4eb7e19a064      1
    35 0x6904dfa88d5345dc7e2de0e1eba32a6c1e672b93      1
    36 0x6ebf583ead402a32f310dc16536067e45e20c9a6      1
    37 0x77d04628a093b243d9750f32893766c3d0b0449d      1
    38 0x791fad8329a0a56e2ce98380d6d6fda3302337da      1
    39 0x7fc53dfff734d7cb9f533154344e4d569a34f8e8      1
    40 0x8083def24331022579d60a2f6b3654457cefbf80      1
    41 0x81cc15402a47f18e210aa57fb9370a1b5525047e      1
    42 0x924449ea24fc318e4867e1773625ea62ef53513e      1
    43 0x92717279d5ea53d1a025dd287c25179cf65e9e53      1
    44 0x9af7edc48bc8db5e4afd3af95df143ea29e2e2d6      1
    45 0x9cad606ef8153b3b06d58a6ecf9e20050c92d8a1      1
    46 0x9cf3c4860b7b5e93defb70121fbe599f90160d45      1
    47 0xa56b838ff6d5fa6d53440e76087ec1cd49192551      1
    48 0xb099a3c3043b262518d9b6cd4d5c9f589b15e2ad      1
    49 0xb5292bb264b19eb7fafd054236ee2f2ee4509d8e      1
    50 0xb6bccedea3bfea7074e41053a0cef5f07589fbfc      1
    51 0xb721640f923f9eac62032097cbd916501a3d3e0f      1
    52 0xc6aef9394574777c237fc10bb122589d36d13dc7      1
    53 0xc7f91d9f1a2609c89d600bacf38a21f09e77ddd8      1
    54 0xd35edc0941e5880128dfc0c17bdd74ae82ba38b2      1
    55 0xd88c17aad286da44d7216f487b9a65ce1f7d4581      1
    56 0xe1aa5ce07798306293e72ca2e187ca75a568188c      1
    57 0xe4afeef97d66447b319e13b10b60d0470c406df6      1
    58 0xe6dfbf931cbdfd299a7f8fad8627b33c536bb28e      1
    59 0xe84d4f3463632587d03a26bb68979ef317bda4d7      1
    60 0xe9f1c3cf268e67d2bad6c410156afc2d063ad4e7      1
    61 0xec2b07e1e0a129ab38b38e70b96e05e742635e55      1
    62 0xee2a4c655ea792d92f12a3caea21188be6dea808      1
    63 0xee667a3c89c7ee6e5a9595f998d32642dfd0931f      1
    64 0xf91ba1faf962b672a7ae12815afd2f432bc74186      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 171 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x0075a6e987bb3f7a6c41b095fb5cdc6767bd3ee8      1
      2 0x00b87c1b8c5f615c514f6d8174ab59633f1c9ed7      1
      3 0x00ff192363430a35abbf968c535b64147e88abdb      1
      4 0x0264a803979bd0b485a6701f1ab17aab3bcd8ff7      1
      5 0x052564eb0fd8b340803df55def89c25c432f43f4      1
      6 0x0a8825c03100e6cd1a82266729bf90f7fe71dc6d      1
      7 0x0cf81cc563266b405e3f66f525e83d9e7262059b      1
      8 0x0d2f9fbc29b788a79fd53d93f9b8d495235cec3e      1
      9 0x1182ed989ebb1cac8d6312bf08162b5c4e8d621f      1
     10 0x11918a9a1cd1abe8e302a8ab8e6f2068f85fc5ea      1
     11 0x123c6d03cc6810e35b9e396402c55bc501602b8e      1
     12 0x12475379baa30ef38ffeae8afc1067d1cbf94a8b      1
     13 0x1257400e85b69c4efc4de33170ca11bec259bc7e      1
     14 0x126e7568537e9062c58083d43a275358cbe9fe52      1
     15 0x131c72678f8826612467b5ebf0ec485dcfb5065c      1
     16 0x13f4691f0084e6d1e595e43a66ce50dff5559d44      1
     17 0x14977b0dbe7e155f9907effecbb70c9b7a05e737      1
     18 0x158f25e42485952f063d4f5724339d4ced376432      1
     19 0x165c95786ce1d6e3bdd6c51aff29438e65d2ce86      1
     20 0x169f66fe4ec8c14c8feca1ce4d1b34561f5413fe      1
     21 0x18194a7c958ac751c3fdd11afe4aa0a4bbde49a2      1
     22 0x185481022e7082b5d2d2aa2332552df3d65cdcce      1
     23 0x19461698453e26b98cee5b984e1a86e13c0f68be      1
     24 0x19fedef56411bb49afaf65f7e4be50629be17632      1
     25 0x1a49096ba1a258b51f7e60c703dcf799f0959cf8      1
     26 0x1a60dfb071b039c6e33dcb3220891c83da72c1be      1
     27 0x213f80d641de4945f547b4a2c7200736b0b48bf8      1
     28 0x2334d91317950c19a6c21c69d69f5ab8a8f275fa      1
     29 0x243b34d9f7dc0808c7d4117bfbd0ee6de670c285      1
     30 0x2670f8569345d20ee7fd191fd9937823b1909d03      1
     31 0x2844a1f5c5164b9adae211df8931838a2d33b5ed      1
     32 0x2a839d803d6b2db089408f2e20a23c07ea9b56fe      1
     33 0x2b85b841b85fe6bf05e1a41330fe13882ed1f286      1
     34 0x2baf9d01cf1f51b270aaacc80ea08b1323fa1be6      1
     35 0x2c9f8cecae8242654d0bd744e3b35f5c19ab4066      1
     36 0x2cd07965af7339ebbbc19a83072d75df92a6956d      1
     37 0x2d6836b5d89e319f38ac3c058f4c835f0c69b398      1
     38 0x2d80d89c6e28bffe6adb9d51be3d8cd339e0bc44      1
     39 0x3098c04b75c7fecba9c7a80fc24c097995e95681      1
     40 0x3279e7d7e140055858b8c9ba4beb615281710ac9      1
     41 0x33a34e27a81436ba9d79276406a285e89a8bd8a8      1
     42 0x3543538abe754a5edbb3c97736cae4b30a77e0ba      1
     43 0x36e40e5c762730b4f6d7e8fff1464b9949537fc9      1
     44 0x37009f9ae9dcc0a6d59204e1aeaf9da4968caec8      1
     45 0x372b974a8819faaeb1f0ce3912dc6c81688cd6fe      1
     46 0x382c6f4dd388a71458aaefa837b385ac6c33ddf0      1
     47 0x3912a27a0429ad3bef065a76fd9ca148531a05af      1
     48 0x39e96a0c67056fe4a8d294f396b927347747dc3f      1
     49 0x3bbcd028b9f2d5dc63613d1c03bef0545b2a66f2      1
     50 0x3d12ac8da240f812191704c852834fb9a6591672      1
     51 0x3d4b9f9967cff180921bf507725712b6a4f90ce7      1
     52 0x3f304c6721f35ff9af00fd32650c8e0a982180ab      1
     53 0x3f5535777b429fbdafb21d6f6f47849e479b38d0      1
     54 0x41b52cf255d0708b399ee6a48253e45aff929b60      1
     55 0x424a1d4a95947d969ef09f8d3aac97f943e14488      1
     56 0x445db19a4e04ff5624744d5aba50c2590612093a      1
     57 0x46091f24f499344fb0cc79e78948adb48cb7ca6e      1
     58 0x47c553e889f7bfa75cf3775473180a04e9effcfa      1
     59 0x4aefbc09674c2aa7e3d0ed2500ee56323d0a35ff      1
     60 0x4b2ef7127640964bf5cef3d1bf8d8f72d8d386f5      1
     61 0x4b434c35b7b43e5ccd8700074a817cae119a04f3      1
     62 0x4f99d481981f464d824592604f58bcee9cb8abf6      1
     63 0x5070781ec368ddd83f9e28193e82668f9edc4932      1
     64 0x532e31f49828dda5009da88609e4527c2c7d0db8      1
     65 0x540a684e5ccc3dd20639590cc1750d8b5f3a3b66      1
     66 0x54966b7b70eba0a6bd5f5ab5e292f266e2f89c89      1
     67 0x55c0c8186541868c03f434e9606e112ee0153965      1
     68 0x576b9d138f67c7b1e70074b265b608a0b1ddbd7d      1
     69 0x5773271490cd30e904397779551380f104d134dc      1
     70 0x57798ea2de5e864959a3f00a12f9224147edf7c8      1
     71 0x583c3d9e5fd01bd3471d1ceb5f27737f36933080      1
     72 0x5a04ad067b3a022bbc03dd49f105d462b6bbc1ff      1
     73 0x5dc23549cdc0ea41a9b6ee688e0bc24070649cd7      1
     74 0x60c4e51a1c797f562e628cddb4aeb84ac9f27d41      1
     75 0x614c3ce52760343fccb7d908eda39f114620a1e7      1
     76 0x63b7d0006102e672e6936fcfe6197e4c782f51d4      1
     77 0x6483313c87d8cf60ab4a21407a67e4ac7fab6031      1
     78 0x64d18532f07855920ab624de8d2a8a365f365e5b      1
     79 0x66143651495a1ccbfd2f708af8336c580935349a      1
     80 0x68bca5a8bdebe05fb8a6648c7316b4eb7e19a064      1
     81 0x68ddc4e9c6daefd31e349cbf7217226b000aecdd      1
     82 0x6904dfa88d5345dc7e2de0e1eba32a6c1e672b93      1
     83 0x6960770b484a5a455e39f098549eb8dbba62d434      1
     84 0x6a7415b36133c6c3957e4772b8009068e170c648      1
     85 0x6c5f56ec2cbbe1c9a8539407b121834d2c8f8dfe      1
     86 0x6db5e720a947c7cc2f3fcad4cf5058402fc456c6      1
     87 0x6e388502b891ca05eb52525338172f261c31b7d3      1
     88 0x6e5b0de0add71e1052b11abbfe6c433dd0466fb4      1
     89 0x6ebf583ead402a32f310dc16536067e45e20c9a6      1
     90 0x71a548c09c6d09df38811c6df025d281e878258b      1
     91 0x72ab6547ee820299d843f38147d017ca817b4a6f      1
     92 0x742875d6c2533f1cf131adb1cb26526740cb6c91      1
     93 0x75a56cd64e10c4f07a6d2cb9027580b7b9a728db      1
     94 0x75aadbb7c36db057e5ac64183899f118de76df35      1
     95 0x77d04628a093b243d9750f32893766c3d0b0449d      1
     96 0x791fad8329a0a56e2ce98380d6d6fda3302337da      1
     97 0x7a1a0bf0223d0b956f98766eb2427a92ee8266a2      1
     98 0x7fc53dfff734d7cb9f533154344e4d569a34f8e8      1
     99 0x8083def24331022579d60a2f6b3654457cefbf80      1
    100 0x819395f9310c601f1cec217eaa711e0e200c8a78      1
    101 0x81cc15402a47f18e210aa57fb9370a1b5525047e      1
    102 0x867476e2a84de15852311bd3299b41626c778be4      1
    103 0x86a35e57968f0d6591b1886594c6b7721fc96943      1
    104 0x8f77d28c23479f2abeb72a31c577ac0ae5de1ebf      1
    105 0x924449ea24fc318e4867e1773625ea62ef53513e      1
    106 0x92717279d5ea53d1a025dd287c25179cf65e9e53      1
    107 0x93670564b92634dea224e7f6932343fa93c22554      1
    108 0x93a6d60ee0a7059d9f508f0af995581278d90e41      1
    109 0x93c838add3ac72b42f65417408f900a7155e1bb8      1
    110 0x94c147ef72a679e2dc3a825e4b881b42beef258f      1
    111 0x97d9225418a7e8fafbcaa017573b0476b939bafe      1
    112 0x9a7eed6209dce4744cabe2945bfe7d31217b1547      1
    113 0x9af7edc48bc8db5e4afd3af95df143ea29e2e2d6      1
    114 0x9cad606ef8153b3b06d58a6ecf9e20050c92d8a1      1
    115 0x9cf3c4860b7b5e93defb70121fbe599f90160d45      1
    116 0xa1a1ac9a2773ef585ac461c0850b3268829462eb      1
    117 0xa2c62a66f6660166838b95db60f234dfb59e765e      1
    118 0xa3603f2623e3280c78865251a1b5b67c5a300d51      1
    119 0xa5430730f12f1128bf10dfba38c8e00bc4d90eea      1
    120 0xa56b838ff6d5fa6d53440e76087ec1cd49192551      1
    121 0xa82c20f746fbc7a8e22329ac38e1a1cc43db8e94      1
    122 0xab4273e7137f9531fcb47d558d9bab0e726e6937      1
    123 0xb099a3c3043b262518d9b6cd4d5c9f589b15e2ad      1
    124 0xb5292bb264b19eb7fafd054236ee2f2ee4509d8e      1
    125 0xb6bccedea3bfea7074e41053a0cef5f07589fbfc      1
    126 0xb721640f923f9eac62032097cbd916501a3d3e0f      1
    127 0xb7325c055854ef3ee451aae865cd68f05113d05c      1
    128 0xb7b2baa66b65e454fb5a2fc7ced0f07e46c871be      1
    129 0xba3a85ebd1d08df696afa8d278785e032b45a083      1
    130 0xbcd78799974f8a5b9eda6b5b0ae6d90d29d3f9e0      1
    131 0xc18ad8c7f967e736bf1bf6c85b35b5ca4743236f      1
    132 0xc35902fffe09dabc98ef31da313255d35eb4711f      1
    133 0xc3d3f90e2210bbe23690e32b80a81745eb4db807      1
    134 0xc3fadff037048a820391fc6ca70d42de8078cef6      1
    135 0xc6aef9394574777c237fc10bb122589d36d13dc7      1
    136 0xc6e89a98aec81debbdc2105ebc6d070f6b0f7f22      1
    137 0xc7f91d9f1a2609c89d600bacf38a21f09e77ddd8      1
    138 0xcd2fb99047946b44058af51317d99dfcdb199999      1
    139 0xcdc626617b2f975f0d33dcb76f5607436e8efd1a      1
    140 0xcebb735535fe629d12947cfa5e0df7fdc0955055      1
    141 0xd35edc0941e5880128dfc0c17bdd74ae82ba38b2      1
    142 0xd48173c381685c40c554e33164b9370507d1b236      1
    143 0xd88c17aad286da44d7216f487b9a65ce1f7d4581      1
    144 0xd9d4e0f4c81d13edf3ee8cec6ff026a06d418301      1
    145 0xdd1f61e95ca9ec479de81f869921ed191dfeeba8      1
    146 0xe1aa5ce07798306293e72ca2e187ca75a568188c      1
    147 0xe25f5ffa52933e17b40b252d9a51c4fe14cc4750      1
    148 0xe4774f2df243e526330a9cfb9998a32ab1f17378      1
    149 0xe4afeef97d66447b319e13b10b60d0470c406df6      1
    150 0xe6d6758d2fc0a7a0c476794855d035f3bcba245f      1
    151 0xe6dfbf931cbdfd299a7f8fad8627b33c536bb28e      1
    152 0xe84d4f3463632587d03a26bb68979ef317bda4d7      1
    153 0xe8fd1914571b437d8b2a7b4df52f296ba9ac5d40      1
    154 0xe9c963a60b0da4304b65df8e519705e1195c6163      1
    155 0xe9f1c3cf268e67d2bad6c410156afc2d063ad4e7      1
    156 0xea53a482c24d19982f7c2a3e48d543da3f60e711      1
    157 0xeba9d7523c1842907587c92dedccbeff49c9ad77      1
    158 0xec2b07e1e0a129ab38b38e70b96e05e742635e55      1
    159 0xecb1ea77cddd83a0696c2d44e8bf4a6e40c57a8f      1
    160 0xee2a4c655ea792d92f12a3caea21188be6dea808      1
    161 0xee667a3c89c7ee6e5a9595f998d32642dfd0931f      1
    162 0xf013e5f697e84c6831220a21a96556242ee9ad86      1
    163 0xf11e0fd4e6cb70bced7ceb27fdb6d6a6bce23f94      1
    164 0xf25ad24b791e37e83f4dadfe212e0e9bb45a1f8b      1
    165 0xf3478bf2d1120590fbf4cf4544c70d20237037d8      1
    166 0xf4a12bc4596e1c3e19d512f76325b52d72d375cf      1
    167 0xf6620eba0895d90edd97ede6db19f09f9970a47b      1
    168 0xf91ba1faf962b672a7ae12815afd2f432bc74186      1
    169 0xfb3df26810b5f41172731ee406c2ad577d5c8a5a      1
    170 0xfbc76261fd55cf91b81a97dbcc2b3f6118f2b935      1
    171 0xff13f9b23a16a82c3ccdd29ece6af667eeb15834      1

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
