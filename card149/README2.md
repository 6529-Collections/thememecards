
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot2.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:11471       Length:11471       Min.   :1   Length:11471      
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:11471      
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 18204169 # https://etherscan.io/block/18204169
block_hash <- "0x647b2ae133316fcf619a6f7c35e2e78159e9cc358f30059d08a35cfd9ebec9e4"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4723 

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

allow_memesRandom2_phase1 <- pick(snapshot, contracts=c("memes"), address_remove=address_remove,address_pick=300,address_max=1)
```

## Allow Random Memes 2 Phase 1

``` r
c(allow_memesRandom2_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_random300Memes_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 300 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x00935c528d520b0091b4cad9da987fe021e7ac51      1
      2 0x00dec4b9193c7519cdc420d660b7a12559f04c94      1
      3 0x0155e31d9c01c28395277bd4210bf5c51965e8d6      1
      4 0x032ab14d5f69020afc87f43cacc0ef182a750d1c      1
      5 0x0427a43fa3c5490ce7e9a7914b366f84f5c4f588      1
      6 0x04547314afabd1060fe6513518584b1ca602b51c      1
      7 0x0594ad5995af0c6107ae1c031eefebe3eae42c1e      1
      8 0x05c250120ce07ba6fe361b39ac344148435c25ca      1
      9 0x061ae2b316ba7808763be9032b23c0a35a64a2be      1
     10 0x062164d0c571002b9b34a9a71ddaeeb0b6f93132      1
     11 0x0623a3b40d4a7a95e410d203fae7d8f9c92ed480      1
     12 0x063132e1d536793a8d1e8f0d850af809fd7858c6      1
     13 0x0743882fa09a0c257b18d53dc1b101d3f32a04e5      1
     14 0x076d40d4453a72a3dec44970b1529baee379dd0b      1
     15 0x07d86ecc077579796a5f159abbca9fa5b4f49ea5      1
     16 0x07dd8451d27ebb6442395a512a081dafc6791850      1
     17 0x07e92b07f74a3d021534e2f0382a9054411b8b2e      1
     18 0x09276485fb976693e79ed9cf8defa435deb3be1b      1
     19 0x0fe61de274aed1803e66ba1b863d966079cb9116      1
     20 0x1052dd3882746ed4388b00b0353a69b38f612471      1
     21 0x11142e97271d26febef714a117fea7d279f77378      1
     22 0x1536b993645ed5c0b731011f8534554646a9b588      1
     23 0x159ebf9863e44f9d910c0e8d8948cc91b803c168      1
     24 0x15bcb3c18ffb1c957a30ac53fb85733fff606446      1
     25 0x160a588038a7284760ad05cea4466d2774d06c7a      1
     26 0x161a2562960f75d92fbbacc6abdc23fce9e571b8      1
     27 0x16ba0452b203b02d711eb772fa01206c4f82bb48      1
     28 0x17cbfe407a0a2285594fe4d8a88038c9b00b876e      1
     29 0x18528b590e730f70ba39b7b72f54de1d12e3519a      1
     30 0x1b21a4287cbfa8f81753de2a17028bf2fa231034      1
     31 0x1d28a703ab06bcef8666e4692fe63188b6fcbca2      1
     32 0x1eb322c016815ee5b29c071586c1b75be5934576      1
     33 0x1f82cfcd6a494c4da544c5aaaf79bcc054d6d52b      1
     34 0x1f9f7531a3d5102f9cbafe7425fc945fbf58678c      1
     35 0x20111f285d896e87ff96c9cac66c053ccd79eafc      1
     36 0x205b07af095793ccef2b1b24fe5d54f4aaacb6d4      1
     37 0x212d461a5171a8eace10ab40b4f8de0399051b57      1
     38 0x21abbfa7a1ac6300cd9df848784f0012c61cccf8      1
     39 0x223e42994eb3c7b4953c86973892ccf0bab4374a      1
     40 0x22cbf92936f171e073350eb771d6f0c8b38330e0      1
     41 0x23145f55648b1399356b519e96fc369f85a1f82d      1
     42 0x23711364e014ab455dee32f02e6d2ae7d070f9b9      1
     43 0x23b8cf0bf2e6232b11998ab87913ede1ce7bd902      1
     44 0x2522a1a91830e7934d9da6d10eeeddef88ace7ac      1
     45 0x2702d7b843a994a388b830f64f9ca17118f965c1      1
     46 0x28ee3a4ae140743a78684f8dd98c128b54c1365b      1
     47 0x2a193336b79d9462bb36215210d44e9d60878c65      1
     48 0x2a91af61145656fc7f909912ed4db131d82d70be      1
     49 0x2aaba4b94a805f0fea696dc11de324c2320ff4e1      1
     50 0x2ab173ada010a29de3c312edc0d6d39608c69697      1
     51 0x2c9d56f321fd120009743fef09e2a6110178e200      1
     52 0x2cd00bdf1605046cef0f50420831bf3b7d675882      1
     53 0x2cf84928261f655a47d04ec714d3bedf9375de46      1
     54 0x2dc820ae955aa5a34cd073f72b76e127cd7bed1d      1
     55 0x2f054c2f2c3497b96c66436fc006ebe096439be6      1
     56 0x2fa5ac62ff5c3a2800e3eff0d1bdd14bdeb0e1a4      1
     57 0x2fca9a598678d0d09309b06eeff493dbf6c7d1a9      1
     58 0x30eaf1e4db6e830e112988b87a300c03c75cc566      1
     59 0x31d447ccf5ed1905287ca964b72f5fef01682944      1
     60 0x332ab3f0dd19b601e988024bce17c9795a8c88f7      1
     61 0x33914463964f994f093bfbaef4db18110dad60d7      1
     62 0x346b54cdf75cd6dd2f41f5557695f210253a6e94      1
     63 0x354ac42d5e29ef57d956d6967c54b084524d7b84      1
     64 0x35fb72d1b0f2c6945d019bedee3bf0b7b51f3073      1
     65 0x365f7b46973f27740c08a077b66ec810cab39306      1
     66 0x36799ff7bf99602870b91572e2a897b06c91c87b      1
     67 0x386db8a4a2659cd4e1f76d3162c45e212b63a3de      1
     68 0x39067aeb2a223685bc606bc94545e53015468eef      1
     69 0x39a167e13c3fa53025069ef7e62c7cc8e8b46fe5      1
     70 0x3a2c6b9d1548d752b85a17bb8cde226ee70980e2      1
     71 0x3a2ea5595098565e7362e04da30c5ec29435731f      1
     72 0x3a4c7ac6873316e2c62a168150c49362ea4a77f5      1
     73 0x3a662f787168e8d856e801c75449876f4da9208a      1
     74 0x3ac2cc26fbb230e704393dc79e97cca47e709247      1
     75 0x3b2dd499b5c4fdf97f794525a9586f62e334622d      1
     76 0x3c2ea9fc12e137e55165a53d3f7c9fb7a95b6b0b      1
     77 0x3d0400db7e6b017c05078cc07ed31ec4963fa994      1
     78 0x3d6301815a5b5188e50a90e268179447d1c58b70      1
     79 0x3ee9c72e4fe1e6fceafcda4c89fab3cf98e7c5d1      1
     80 0x3f1b2862d959961c8e0021991320bef9a7d506ac      1
     81 0x3f3e791194fb13723b2bfcd663057d4ee157c30f      1
     82 0x40ba9dc0ffc712c5c8859ab9f7ba9fe2edee2cc5      1
     83 0x4155ab641b6c36be6822704e82b2b3b5d7e55778      1
     84 0x41e399b9243bd6f772e30cf79dcbaba791143d6c      1
     85 0x42143e982e44084e0f52c97dbee4b06b7e5c4370      1
     86 0x42cef12222672e260d275b80a0ed0bc40896af67      1
     87 0x43176751abfbd7a39549748aab57641c26df7129      1
     88 0x462fcc4b3fc64e0dfd751424bc3b070d79efb300      1
     89 0x4874211da929dd87e28a4267c4b96a5d0ccf48a7      1
     90 0x4909c45bce6c9f11081ee53404b866aee9e0ddbb      1
     91 0x4cb086a121e0bff03ba0d0a2c57b15b6df399ab1      1
     92 0x4cef61837e4c2186468145e6b9668af92694aa43      1
     93 0x4d207177677fb1128281a54fad993d3736c7f02a      1
     94 0x4dad859d49a20aee625c60e78c19f103bb879f9c      1
     95 0x4df00689952974edbe1cc05d2710ea1b2b6e185f      1
     96 0x4dfd1688ec88704e0d43976c58c34dcdb12ab55c      1
     97 0x4e12b79845ecd48ba5165e8fc9e95436811c2b58      1
     98 0x4ec44ce5506aa428eda482dd8f1e11971e54fe86      1
     99 0x4f494d0f18153cd75429588dc67e0975b67dcf46      1
    100 0x4f65f97c0e40e8635253cf27ce5c412c450b4710      1
    101 0x5062643314417a88ae40bbdc8b15f21c4abb288b      1
    102 0x51db75f734f02f392452d94e06b7b703bf6ce197      1
    103 0x5200fdc6d083895d7722794d8d002a3f04943a90      1
    104 0x5391ff3a579bbcb8372638f62defc7773a8f55e5      1
    105 0x546c553b189d5032992e3095df53efeeb027c9de      1
    106 0x54cb5138bfe9e89445ace9b6cdff739f9e3dc2d0      1
    107 0x5643a3518350ae920b442ab70aac56ae6d45b8be      1
    108 0x5674ea0b10559d94d1141eba00ff14211ea50151      1
    109 0x5688eedc08e9e7301c1d7102c95112253b393e1e      1
    110 0x56f1760d582a25c1807a3d1c9f83ee5553f0f290      1
    111 0x575dfa1096ac4cb85f5f1b15bb7cfd9734d8a064      1
    112 0x58aa118039cd7937b86dfbdaba48e79fa54861be      1
    113 0x591f177a6bdb1b6adba108a3f9ae19603c38bb84      1
    114 0x59a6febeafd5faf543033377b879d0f9f513f52d      1
    115 0x5a98a362d3aa4ddb8df4a7554c73be6a4ccb3fcd      1
    116 0x5b067f845fd99cf1e80e2b5833bbe6e6910f6843      1
    117 0x5b60608b5a9930114a3587a97bc93b5091927380      1
    118 0x5b77982fc8dcf080d14c6a2b5d39460d157790fd      1
    119 0x5c9032bd91b230214f14a94110589fadfc6b657c      1
    120 0x61166f389a70d84b0d95c606de3b29344ab00654      1
    121 0x620051b8553a724b742ae6ae9cc3585d29f49848      1
    122 0x64acc74b69faaaa90a5bcd5c46b4de3baf0c51b8      1
    123 0x64d23aedf99d0884bad89e1b78d4b54a9385dea1      1
    124 0x6523ae151fb606439bb75855c6ae966b9251981e      1
    125 0x6718c0b9061159823440abf4635707b2ed8e5ca8      1
    126 0x67d693dee156cf89ee524b961cf19e4c023936e3      1
    127 0x683a8c3353b67d863a34ef52270d436c1126cf66      1
    128 0x6885c0a837c320f47a66df243c49a4e605ceb1ab      1
    129 0x69a949d2330bfac2fdbee90ccfa9923343326b19      1
    130 0x6a045f224e69a9f2ec95ba6f8a83afe03af8754e      1
    131 0x6c109123bf8b42645213162c405adb132cf08074      1
    132 0x6cf6972be629d30428331a68d4bbbea8e0dac615      1
    133 0x6d384f83c01340f0a3487177b162674f3164cb1d      1
    134 0x6ebe7f9cda8619ab2c508ba6c8f6675836185e0c      1
    135 0x6f7a49da0f184509814039956137dadb9ccda4f8      1
    136 0x7006fac41dbd5c8f026927a76252b37ee1de1225      1
    137 0x7051beebdc1fe5a678a8504370a15f9491b30688      1
    138 0x70af781f6851b057adcd963da042aaca1915050b      1
    139 0x710852027bc25db82ba562d5b1499b6d9b02fa9c      1
    140 0x734ed31ad4853ecb36e8554ec35408652642f3a0      1
    141 0x75d7d7972a62b00ff7ef071741c929f59d185ee6      1
    142 0x777c87cacd482f5f5667859bc0a83acc7c931151      1
    143 0x777faa4f4f23f6fe4cda72fcce53fcf299f09726      1
    144 0x77d25628e427bc4596c1a9e0de6f562bf57a1768      1
    145 0x78450ed7653901900d34273a768b23f77cbc2556      1
    146 0x78d9f7747bfb3b06e10e2fa7a99c0209955eda1b      1
    147 0x79fdbef5ce6d2302db090cb75de318d2c3edccee      1
    148 0x7a20b6dd971673cc75f507c1408c13140c11b472      1
    149 0x7b220358c3ec9ec03b08120dccec56f16fbf33a4      1
    150 0x7b7fe6b8cd0c122780c94194d4112ce6f0263f29      1
    151 0x7b858de2d9f5f5f7ceb2b59a2a6c0638a6507c00      1
    152 0x7c3018a930e2d0d060069ac1ec38dbf9619e20be      1
    153 0x7c4f9133852935cbc555d907cf981408cd2d4834      1
    154 0x7d0ddc336fc0cfb5a13efe490de40decfe0900df      1
    155 0x7d639b54904816df6ef24f10ad421a874621b512      1
    156 0x7dc35b473b6ff652c1c437996244a8d14d026252      1
    157 0x7fda1f3307b6e1fdf607747e7ab047ff16e55159      1
    158 0x8272262b6f5c8b5fe307c99d4524b29f0eeb543e      1
    159 0x8386262bc928b5c7a0d19496c4921adffada05f2      1
    160 0x8397e2d5fadaa6e0817e376df5ba1d016a2f804f      1
    161 0x8400b081f5dc9c5bffe1b57276afdfab2dfbf29c      1
    162 0x848ba349b050fd8b703cc5fef2c85ae7d8350556      1
    163 0x8555e51f5af836096b165327302f27304ec7aaa9      1
    164 0x855c49e2af12e43c83526c5f5f639eaf004b81d4      1
    165 0x8605cf36085697fcda20be5488df08f407f73430      1
    166 0x87accabbdedce63379af9d246bb979a3a481534c      1
    167 0x88188aa8406201f427f171c28e489c897d75ac7a      1
    168 0x8880b69c95c0183bd3d4f30bc4272e457552c3d2      1
    169 0x89678c9dee8ea64b42eab9d235d1f2b28e23c40b      1
    170 0x89b88287e2905003551f06ed77c3491be360de8d      1
    171 0x89bb71d271e19b3309f817b48be7a4c46435079e      1
    172 0x89e8493a292aa54734f7c2ff4765719138f18189      1
    173 0x8e82a5b8f8f0b86ead15a4e56dfdcf2418ffe647      1
    174 0x8e9be9e56975feb2af4ab5c609343472ebb80bc8      1
    175 0x903b1b960ecfb37d994fa0cc537777d8ed766656      1
    176 0x90468a45f4d34fd76876b845b4c6291ef7bc6e64      1
    177 0x9052e1b507c2b00ad3acdab4e47eb2dcb98dd59f      1
    178 0x920af9763ac18429f36c92727056d3606e4921d5      1
    179 0x92d6d3dc53e657e22f2ed3ae34b1128d3b1ec9a8      1
    180 0x9388f86ca5c16a420e65f6580e997e21bd2665ad      1
    181 0x9429b7d2a312b140e18981757b2b44b33881e76b      1
    182 0x94a5705a0f0927099638fb068c6e3ad77501d837      1
    183 0x94bd59027a36469827442108631d4a07d0e08846      1
    184 0x952dd5887bd9a061768cec90e2ac8f3c1ed21480      1
    185 0x9553803357f9441ee426e701a1bd8031f814fcf0      1
    186 0x96236adb640ec620a85898378375cedf03ca21ff      1
    187 0x979ded3b9685b12eb3f17ee6824928979f849c26      1
    188 0x984b8f799f8a2efb188f7be8b386130fc4651bbb      1
    189 0x99f230a1a61044cf092d0c9cdea53b36f6a18904      1
    190 0x9b921fad875b06183d3992a125026c89915d71f6      1
    191 0x9bc2879384ec9543718c93bdc769050286f91281      1
    192 0x9c6b08515b6414ea390f9c3c3d72b14146618e09      1
    193 0x9cfaeb046da5cf9f7325977bd0678f953f0dfc28      1
    194 0x9d8945e2c08751ce18bab505a57073c8de16cd11      1
    195 0x9e5f73d2bae44a6d6815eee51c292ba235f04f6e      1
    196 0x9ee9a7e08e78d6a9cd9f47ef72592791c0a5d174      1
    197 0x9f455b1fb95d998ab1f25a0e957352fe11f508c0      1
    198 0xa0202fbe4ee4ebcc96fbef5c3fdd46ff29cb8351      1
    199 0xa08f910c21ebff52622490c79dea4158fac5fed2      1
    200 0xa2a9f1f06e9609cae834d7fb249ecd5598989d90      1
    201 0xa3c523365e5604ad806c1c599c90aa9ebf7a3277      1
    202 0xa40d281de9c15d501ef21ac32f777a5a551fddd6      1
    203 0xa46e0226d3c988ebafadd1838daf2a3318f21783      1
    204 0xa626d27bbe486fa00f58df20a881c7ad224c411d      1
    205 0xa783ed50f76ea884120e6216fb268ec0f02f65c4      1
    206 0xa810ee0b25765421f1772428e6a5c5357cf55183      1
    207 0xa931a25217ea04269e9fb9b90e2ffc72e9e92839      1
    208 0xa975ebaf140d206080dee102552dbf88d057af18      1
    209 0xaa15a96fba6c84737e632a93140d9f549f55338f      1
    210 0xab7473da0af4658ae2879cb641d82e7652721486      1
    211 0xacc0e80e3cccede7d2948ab542700bb9499d2623      1
    212 0xadd93b1acdfa3e865bd1e6495182e9f1ac5cc35b      1
    213 0xaecc64a55d46551e410d3875201e9b8cd63827eb      1
    214 0xaf664a54b35ebddd483ad6a7c8030229b4ed745c      1
    215 0xb0bf7fd6f02258b47f9f0d2dcc469a43f6f62c37      1
    216 0xb29479ef88dfff8dac466cf6c716e7985c4241c5      1
    217 0xb4b8d2f1f52d9d7dcc629941735816172e774fd4      1
    218 0xb5363508582a631413b8b9c99f836f4b673988a1      1
    219 0xb61ac751e031802665b52240ebf55210499e97ca      1
    220 0xb66be9ff7f3be2a847406e312b86bf7a32e94e3a      1
    221 0xb6ea381cac9da2de47c6a760445bbeab2b1db480      1
    222 0xb91a99f020a4e485e4b162f799c27d60962c6180      1
    223 0xbadcfb8e4e3c6a766c63121a3e32caf2f4f0938d      1
    224 0xbb102b973d602bf0678a722baaa596d664781f5f      1
    225 0xbb34d08c67373e54f066e6e7e6c77846ff7d2714      1
    226 0xbbb78ad0a8ab71da46b5fd8d2f5bdb3d591f7809      1
    227 0xbcf31a444c1c83f7421bb7c0a57f54b1c27fea8a      1
    228 0xbd88526a40bd28acb6761a238d7751d3f1bb5fb8      1
    229 0xbe58a1b85d11e2083bb7c5766284ed7848448d1d      1
    230 0xbf06d42287bafa1d13121e3116a6a9f5f1266181      1
    231 0xc1553aca9feb623ecf04ab7c0eed91a73db40389      1
    232 0xc1b23d8514d2332a91c565915a7f4e889cfd46b8      1
    233 0xc25d23d642b05b9546d85574b53f8b62b6b11a7b      1
    234 0xc4e19acf82441eab7e45a5e874cece70cd811434      1
    235 0xc575343fc91828440620c0c9a2faab09d26f4f83      1
    236 0xc81bdadc8f4541a03296ac019fe830331cbca51f      1
    237 0xc87e9dff30a7472329237c4ad222b35b529f0fb7      1
    238 0xc887fadc6baaa8cd234d9104a65b37265522b024      1
    239 0xc88d3f26925697931b4efa72a3a8752a551568c3      1
    240 0xc91915d01bb96ec08b9b5fd767034de9cd390f17      1
    241 0xc947a80884c307bad745c82c078099725bc005a0      1
    242 0xcba2e4e7e929579101c428bc075ef864a8085b36      1
    243 0xccd3edb8cb78074b274f0bd86e013694cae3fc33      1
    244 0xcd0cdbbea19a7932487feee640279a23db9d7de0      1
    245 0xd0a8b4fee6427b2aefbd64acf7564767dc61c99d      1
    246 0xd10a92535af8782e06f22ce3f71e63c44c2b4e50      1
    247 0xd12f477ad6cfac95a12e08f96f06cdacf23c6643      1
    248 0xd1642fcc093202f30e64d28eb8de8d0714958f8f      1
    249 0xd16a7ef2382dea4c6d9b31e87cf351ab3266b7ca      1
    250 0xd1b643c63d4dfcf4fae32b58d87843553a64b58e      1
    251 0xd4b2b0215255ada525da8484b78049b19696d739      1
    252 0xd5d290d2638f4fefbda64895bbfeaef52aa5fac6      1
    253 0xd630b9fe9683b2e03735cab7b5df1a0a3c258636      1
    254 0xd63dc86e7fdbe5923481b96beb2a5a79119e51e7      1
    255 0xd6871d2ce0977460ccabb6483bf2b2e23e68ceb5      1
    256 0xd6a984153acb6c9e2d788f08c2465a1358bb89a7      1
    257 0xd73bd59e7c0d075c9a83cad272cf17c46f7f2784      1
    258 0xd78c7aa0455e78e5305ffc4a672b419b448ec816      1
    259 0xd7efae6e8e0556ebb5e77a499a34fce6a4d8c722      1
    260 0xd7f47f4932f65c2bc1650e6f10218a0528543437      1
    261 0xd8e7ce8904819d5a93a6bfd48b794b0a1da41a58      1
    262 0xd9270fd770b3e06f7b4a7452f531f035826a2eac      1
    263 0xd9ea1d79c21722ed93e5f2fe6c1ae5f013d03d00      1
    264 0xda6ab7d0a045a8b7dc25f8a50b383af7ff49977f      1
    265 0xdb6aab3f6e320926ade666f03a34af31ed08372a      1
    266 0xdc594f0eda0e5efac7097c5f1f5c450f856c58e1      1
    267 0xde178a961723eb49654d20cb8482e54d46c58901      1
    268 0xe0b4bfaea4ce41d50449e7e3f8d3a53ffe8005c2      1
    269 0xe12a3415ea8f8c2096914ff490371aae86b9ed87      1
    270 0xe14b40c23f0974550e810841ab9cf5b1b378748d      1
    271 0xe1976fd4def8ef7a89ed3149239fd982daf6f892      1
    272 0xe2880a450f9c565e01a91e83cbdce2b695b7bc18      1
    273 0xe29c6ee34871cbc6fe503a06bd7776a63f375a93      1
    274 0xe3ba95950e0e679bd4868e6c60232a742e9fc9b9      1
    275 0xe50a5145483c22242be79bb231963b21cfd94041      1
    276 0xe831ee92a14819cea1639f6bc14e632bdde0e233      1
    277 0xe859ef79c40250a4554a35c84794ca6c80516200      1
    278 0xeb4c5c24468c3e666d01257b76a73326411b7928      1
    279 0xeca345b5c749b8a9299b154fb036020e13394eb7      1
    280 0xeca588e75c8eb0d2322f52c2c90bd525c5a5d93d      1
    281 0xedfaed62db466ceb580a7804affcc65184a4bc46      1
    282 0xef1fc81fea415a39631a33d92a8f2cc117761e27      1
    283 0xf035839daaf652f03156b297a27eda25a35e6316      1
    284 0xf049ed4da9e12c6e2a0928fa6c975ebb60c872f3      1
    285 0xf07f65cd74becd71339fa95bc10c3c7c0a070d0a      1
    286 0xf0c55da3a8274f6843434df0da437ed4ac0dd310      1
    287 0xf0ca81409a3ec1660c3f4f275d21fdabbc63a9e6      1
    288 0xf1e1cdb82e2d7ebeb0ba3d054642beda42322091      1
    289 0xf2e21450c87701d95d289aca6eef297fa74e231c      1
    290 0xf5a74a49825fddc6d331d4141c9a35f723709e08      1
    291 0xf6d45c42445391b350be3cad097d85a0fe438174      1
    292 0xf6f0667a6b09a119e065925a4795d2d4e69705ed      1
    293 0xf7d4df98f4be31bf93a0f3175f45e7d69668d2b2      1
    294 0xf886367b3ef11510b3201d937073782e5f3eae23      1
    295 0xf91a1142d9e4910fb5e6ea3b965440a1dcef12fe      1
    296 0xfaf1345b95a606f44e392536d864a5960cd4df23      1
    297 0xfb0570809265afc607c349bae0ec8df2299fb6f5      1
    298 0xfb361a36fca7e92fdb47c40df43713f68044ffb2      1
    299 0xfb455150962853ec9130bfdc5707fcf9dab6b7a9      1
    300 0xfecf9b009af216056d27759c1489f00fc62428e2      1

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
