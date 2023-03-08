
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance        contract        
     Length:27466       Length:27466       Min.   :1.000   Length:27466      
     Class :character   Class :character   1st Qu.:1.000   Class :character  
     Mode  :character   Mode  :character   Median :1.000   Mode  :character  
                                           Mean   :1.001                     
                                           3rd Qu.:1.000                     
                                           Max.   :5.000                     
         name          
     Length:27466      
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 16775269 # https://etherscan.io/block/16775269
block_hash <- "0xd713cd8872b387e9b98c57376741b73cfd39ad05c28ba0e0acfb01e572c62bf3"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4705 

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
  "0xc6400a5584db71e41b0e5dfbdc769b54b91256cd",
  "0x4b76837f8d8ad0a28590d06e53dcd44b6b7d4554",
  "0x0887773b5f43c58f0da7bd0402fc2d49482eb845",
  "0xcda72070e455bb31c7690a170224ce43623d0b6f",
  "0x41a322b28d0ff354040e2cbc676f0320d8c8850d",
  "0x000000000000000000000000000000000000dead"
)

hodlers_remove <- c(
  ""
)

airdrop_memes_all   <- pick(snapshot, contracts=c("memes_full_bothSZNs"),address_remove=address_remove, address_pick=15,address_max=1)
airdrop_memes_szn2  <- pick(snapshot, contracts=c("memes_full_SZN2"),address_remove=address_remove,address_subtract=airdrop_memes_all,address_pick=10,address_max=1)
airdrop_gradient    <- pick(snapshot, contracts=c("gradient"),address_pick=5, address_max=1)
airdrop_eric        <- pick(snapshot, contracts=c("SuperRare","OceanIntersection"), address_remove=address_remove,address_pick=30,address_max=1)
airdrop_eric2       <- pick(snapshot, contracts=c("SuperRare","OceanIntersection"), address_remove=address_remove,address_subtract=airdrop_eric,address_max=1)


allow_eric_editions     <- pick(snapshot, contracts=c("BeachBoulevard","EricRubensEditions","WorldofColorEditions","StrangersGiveaway"), address_remove=address_remove, address_subtract=c(airdrop_eric, airdrop_eric2),address_max=1)
allow_gradient_phase1   <- pick(snapshot, contracts=c("gradient"), address_subtract=airdrop_gradient,address_max=1)
allow_memes_1_phase1    <- pick(snapshot, contracts=c("memes_full_bothSZNs"), address_remove=address_remove, address_subtract = c(airdrop_memes_all, airdrop_memes_szn2),address_max=1)
allow_memes_2_phase1    <- pick(snapshot, contracts=c("memes_full_SZN2"), address_remove=address_remove, address_subtract = c(airdrop_memes_all, airdrop_memes_szn2,allow_memes_1_phase1),address_pick=102,address_max=1)
allow_memes_3_phase1    <- pick(snapshot, contracts=c("memes_top_200"), address_remove=address_remove, address_subtract = c(airdrop_memes_all, airdrop_memes_szn2,allow_memes_1_phase1,allow_memes_2_phase1),address_max=1)
allow_memes_4_phase1    <- pick(snapshot, contracts=c("memes_random_69"), address_remove=address_remove, address_subtract = c(airdrop_memes_all, airdrop_memes_szn2,allow_memes_1_phase1,allow_memes_2_phase1,allow_memes_3_phase1),address_pick=69,address_max=1)

allow_raw           <- pick(snapshot, contracts=c("raw"), address_remove=address_remove,address_max=1)
allow_singles       <- pick(snapshot, contracts=c("65291/1s"), address_remove=address_remove,address_max=1)
allow_memes_phase2  <- pick(snapshot, contracts=c("memes_top_500","memes_full_SZN2"), address_remove=address_remove, address_subtract = c(airdrop_memes_all, airdrop_memes_szn2,allow_memes_1_phase1,allow_memes_2_phase1,allow_memes_3_phase1,allow_memes_4_phase1),address_max=1)
```

## Airdrop Memes Both SZNs

``` r
c(airdrop_memes_all) %>%
tally() %T>%
readr::write_csv(file="airdrop_memes_15_both_szns.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 15 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x1ffc29a768e26ab393ea93e4284773410a84b660      1
     2 0x3876be5be4998adecbfbbad26604a762467e7f42      1
     3 0x51a2d3572577b6c47186c82165ab8a61e046dc83      1
     4 0x729fcbb6e1289c88ce5113ec1c83a48a8e3c9f2f      1
     5 0x7ed69ff055d746d451f675635e9bf773accc6a97      1
     6 0x7fdab8c244e0b775edeeb9ebb8356b59068c6873      1
     7 0x80a1c9fdc26199a69d190ebc8ad287ef48758977      1
     8 0xc97958ff5370c56b7e0e6fb58437f00702714d49      1
     9 0xd3364ea1c742cc2d396ac65e1723b8d608a939bb      1
    10 0xe25b24cebed3055236e369570a437a99e1d32602      1
    11 0xeadd2d51e75a8b42302adc2aff0157a4bf626adb      1
    12 0xee2c055f7706b9dfcd98cd5a23d5629d6316c0bd      1
    13 0xef15bd3f923245b24d64d6fbf57f036fe650c23a      1
    14 0xf1f476f144df01480297dca47efca565e8b0c9f1      1
    15 0xf84408070b4de16d6bb0ab2fd8b19d37e3fd1422      1

## Airdrop Memes SZN2

``` r
c(airdrop_memes_szn2) %>%
tally() %T>%
readr::write_csv(file="airdrop_memes_10_szn2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 10 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x20163ec2afd9698433a031b0a7dcd70ed77ba8c7      1
     2 0x23602ca06e977c86339ffddad74966e824ab691e      1
     3 0x28e6c1352950238be088ef2a741f3c8b91b9ffad      1
     4 0x34e03545e5527f54243ce5a4f88ebb202efb0ae3      1
     5 0x3bc161e3a5604165f3165ed8aaaf0d490e559324      1
     6 0x42d38ed60a64d0b8c36f190e185d17db3617a091      1
     7 0x69cb3b1de24e08f1cfc2994171b6c6930498f750      1
     8 0x6e1510ed41ba743344e9ba80646194351cca0c7f      1
     9 0x791967551708fe99eb2cd8dd580ef98c61e67ac3      1
    10 0x9e5198418081fe00026ca0ddedbfd3915ab98298      1

## Airdrop Gradients

``` r
c(airdrop_gradient) %>%
tally() %T>%
readr::write_csv(file="airdrop_gradient_5.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 5 × 2
      address                                    amount
      <chr>                                       <int>
    1 0x129c6695dfe7a906bd8fda202d26dfff601f83a4      1
    2 0x3cb63b82d778105e43f064ed739b0655f1f0fb87      1
    3 0x64f7de90dc79d775703bbec66a1591c7a26a22f0      1
    4 0x896b94f4f27f12369698c302e2049cae86936bbb      1
    5 0xfd849d8cf365fe98bc60025ff7fab45cef0f557b      1

## Airdrop Artist

``` r
c(airdrop_eric) %>%
tally() %T>%
readr::write_csv(file="airdrop_artist.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 30 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x04c95c6f8ea7ff7c3f7a2f148fae75650c5a875e      1
     2 0x20ac3cbf53483d9ee591c033942d841f78a70ac2      1
     3 0x20d64f7a9b31a08871a091414e1f757bf3eb47c6      1
     4 0x2dba88cb3b435f99a3e58b6e0fe450e8f1a3f20f      1
     5 0x32606c4aa95ea78d7c5b1e0e347e3e9ae451434e      1
     6 0x3852471d266d9e2222ca9fdd922bafc904dc49e5      1
     7 0x3ba41a9e97f80fec64ed68819faf12dcc6dff576      1
     8 0x3cfcd52b643ba0650f5976805d1324e50a36840b      1
     9 0x47450b6044689a96cb603308d8da10f0ae501aa7      1
    10 0x5f75fb6104ac8ce72879347db1041adf2f7745d6      1
    11 0x7619ee03a91ea6b931d9e4e8408451b93115364e      1
    12 0x7b220358c3ec9ec03b08120dccec56f16fbf33a4      1
    13 0x7efe3ac6ec0ff5b6136766ac79a97c1e9d8fd585      1
    14 0x82139687faae8a29851902783e02e699de0e0846      1
    15 0x8379ac05ef71f982d497f392dc1a14ee859efd95      1
    16 0x8497277c9339170a0420e86da8352e0c084624cd      1
    17 0x88668b63218bcfd31692e14f635d195f9f45c030      1
    18 0x8a5a244b08678a7de0605494fe4e76c552935d38      1
    19 0x9c163eb3589b0dcf366fc23c581307618b68fb06      1
    20 0xa2272469c824d2b34407c08c1b377f3027406fea      1
    21 0xaf9e18cd797548220317fb459095c6afac5c33a7      1
    22 0xb52343dbb87db123289ebbd6d8277ce9f1b71ca1      1
    23 0xbaea3cf94abd0d6e0f029ef5b0e54e9424a72985      1
    24 0xbc77ce78c157fb7a3bf846e6ef9a6887cc2f95ab      1
    25 0xc3f4728f673181e20862d21d028cfeaacfb6d409      1
    26 0xca7e077ab56ba580f7dbe6e50cb2468827bcc40e      1
    27 0xdc594f0eda0e5efac7097c5f1f5c450f856c58e1      1
    28 0xdcaa90d9f3b75cda80764326f6594b58d0585d21      1
    29 0xead90127d60f78f3e97781938418004be1794eae      1
    30 0xf1e1c701b49b1dc2405a9e8ef40f9f88802b80fa      1

## Airdrop Artist 2

``` r
c(airdrop_eric2) %>%
tally() %T>%
readr::write_csv(file="airdrop_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 15 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x052564eb0fd8b340803df55def89c25c432f43f4      1
     2 0x23fed2634fdc629c1a82eb6ed3391efad2a58670      1
     3 0x2506cd8c7bee35ebe54670bd28eb00692eacd426      1
     4 0x2508b81fe684e5840f3f427d44492714d399dfaf      1
     5 0x4322aa46ffed67809862da613725728e2fb8eae3      1
     6 0x484a12a27e8c95718e0b60d14369bfeaa426aff3      1
     7 0x88da36152d4691e8d4efc998ddb7b22fe1b16898      1
     8 0x900e7ad1ab18cebb4c21f71795364e9b636831ca      1
     9 0x94de7e2c73529ebf3206aa3459e699fbcdfcd49b      1
    10 0xac707ee01a605bfc712536d703e5e790357c48e5      1
    11 0xb2fe2b854395e955340a38ee90cbbd2e496fc5e1      1
    12 0xb474bf171329edc02237ebf31003f9465e1ceda1      1
    13 0xbe2aaec782c989aa11f87a66c9fabb913efb24e0      1
    14 0xe01a97fc4607a388a41626d5e039fdecbfc6acd9      1
    15 0xe25449220e56387f005027f1c76d5a2fd807bad6      1

## Allow Artist Editions Phase 1

``` r
c(allow_eric_editions) %>%
tally() %T>%
readr::write_csv(file="allow_artist_editions_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 322 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x0047a1e80b148c5d35ccd2592b98c109df88d6d5      1
      2 0x01a47d02a50f3e633232483c8af8ee0da6b260dd      1
      3 0x02479bbb9413ac734ad94460e597f98e60d78536      1
      4 0x025fdd585f03ce846740fe5542469f1de425e439      1
      5 0x027cae2ed1a23350a751452e907b4120330f9762      1
      6 0x02d4bac7bf74c983f475a96e658d280d270a4d57      1
      7 0x0352de60830c6852c3d8ed0fe9cce7ff2cc865b2      1
      8 0x040580e951625d17514faa1aae86a893ba68cc42      1
      9 0x05cda24eeefd1f24f18dac398f5a612674d3ca5e      1
     10 0x074ae057b158f2656348f9cf19033dd72a597f3d      1
     11 0x077bfc14dd6725f260e1abfd5c942ee13a27091b      1
     12 0x0944f42caa5e846943c0641d1a47ed7b81fc5325      1
     13 0x0970b838d4d209b21de42ca52f7e0fe79811de26      1
     14 0x0a1c3ab1393a77c7b35ec20cd3293b7966493869      1
     15 0x0aed1878803e2f86b6ddb83f5523f0a793e973fc      1
     16 0x0b175a06961b0e11c73a3ac9a0947bdb6769cf00      1
     17 0x0b36792f715b99a773a938ae9c733b59522d2bbe      1
     18 0x0b4f222cb69554ee96bae54abbd5583d40539266      1
     19 0x0b8edc5d3b491aa703f446e28d20f407f093c13b      1
     20 0x0c2af46a28397a4301b503d9d3a96a0ad5a9d55c      1
     21 0x0c52eeeaf5b01797cdadcc273b2c42ffb278a102      1
     22 0x0daac372398974373b29cb61c31deb11afa7ce23      1
     23 0x0e42d8fcf5166d332ce8df3b65c5e20468fb7359      1
     24 0x0e552d0b3562806d1546b2f6b25cd973ec65e4b9      1
     25 0x0f5eed026a842bccbf206cadad6e2245b96ff0e9      1
     26 0x10b16f5e6e6c78f9b31b7ff618b7ee8d74b470b6      1
     27 0x1179c5ea42299e75e3b5cb1f5df35e0a6a34cb8a      1
     28 0x122118af844eafeaea3e2c43c8346dae87e6c9c0      1
     29 0x15a33852846b4762d71a62928aa7c64d8aac8544      1
     30 0x15bd2aa5d34727f72d17636b055b62299486fe1a      1
     31 0x16380b4892e790f9547e92d2dab841189e469b68      1
     32 0x1822438e03542b3a31cd39f434fbd7f0b9eaf7bd      1
     33 0x18c4c93fc44aa2da78b8b3a0f438873ad2b3e2e1      1
     34 0x18e804e61c44fd194753ce89b32a87acd8435a8d      1
     35 0x19e080e1cd70240c17002ff11dd02ad0508bcb12      1
     36 0x1a73306100f9f27e153ef1044be99bfba644e7d2      1
     37 0x1a807eb2dd167f83ddc254134dd80a71b6d931d2      1
     38 0x1b5b34d20b994631fce946281bdee1d302b0b924      1
     39 0x1c5762f9899b46c19d4c9b8fbdd38d35856227f3      1
     40 0x1cedf6308a24e1a809db6d11042d837e5d52e576      1
     41 0x1d4335f58decba83d56897057a49fe3af72cf919      1
     42 0x1e6ec0adb8d5b769c7b94ee74b1011cf60a54008      1
     43 0x1e927ca8376d51c163fff2038080484ece23212b      1
     44 0x1f5c84e19b7d45aae5ff5b058726f25fbbdbff23      1
     45 0x1ff2875cd548eeae6d22f26bae2d72a50fd143c7      1
     46 0x20413792ca7ec69958b535af3b00d4ac422d8771      1
     47 0x20e3e8207f052ec4f59e3e134327ec1e14bc0acb      1
     48 0x233af98ca9fa11cac6ee6d05552dc95130578936      1
     49 0x23fcd221270323b177852b307d8bee389652027c      1
     50 0x24783027f070019a562e2e2bebc1478c78e6d45e      1
     51 0x24bb550601cea437f7ae2c74c1d586af0b3fee0b      1
     52 0x258c9b02cb450574fe9b85684ca94bad6347490b      1
     53 0x2624e6bdce94156695330a43617de537518052f0      1
     54 0x271271e6c59d4e47d67cd7ac13f8d0232fa4c919      1
     55 0x2718a8b3e39f799970d07d72faf178d0f6abce3a      1
     56 0x27ded67d88c0da8c6b0fb0f7c78d87926d71119a      1
     57 0x2882898129bfb577f756350d8443265038fce7cc      1
     58 0x28f8056b6d87f3a92373a9403fbfea1a2da51904      1
     59 0x294af89e9c338f5f0b54bdfa7db81af97f6cd24b      1
     60 0x29e3eb982c907a22af11815f6846cb7b1b434c99      1
     61 0x2c4f28d7f333650773c4b2a337ebbd5404b41c56      1
     62 0x2d13271c668d31e0ddd4563727e871f82dceb41c      1
     63 0x2de55ccfbf37e76e536934e1c5b7164b12792c4d      1
     64 0x2e5ad3626e358bfd67bd7d2a462c927faf116055      1
     65 0x2ebe0b56db4e962a9a61c2e5301afeba89b84d87      1
     66 0x3089d5dccf383570ca6d2f11059753cf7618131a      1
     67 0x30943b6e8799908dbabcb68308472cc8d4017cbf      1
     68 0x31109265a787ef9fbf997242e36e9d78e8a61999      1
     69 0x321ed5739440bdbd731d54a19aa20b18398d374f      1
     70 0x330bce303d27df0eb1b856da3464278db1db1ac5      1
     71 0x331e231d6230c32681559925c20fb67614be1ffe      1
     72 0x3430f112dcd0a5a2e9df7f0c8732729b764af1b3      1
     73 0x348ee4ed9363299832c33dbb1b52c7fbd5571754      1
     74 0x34e3f9567aee97397ac7a002df2ef4f30193f1a6      1
     75 0x36250e1943dd50c0eebf5c6423a19be1c361c3d1      1
     76 0x36baef820b43bed7011c6bedc609c0a6a0d499a2      1
     77 0x36e10f7bb46a7e8a9000e20ac6bdc20721d15f1a      1
     78 0x3a6372b2013f9876a84761187d933dee0653e377      1
     79 0x3a6dfb855d7b0e08e23fb1515d7d33b816ea85c0      1
     80 0x3b22621a2b0e1f8dd3056975efc2150d5fbee980      1
     81 0x3c470a618ca5b1298dcbcc90bab5ad2b26719625      1
     82 0x3c8a67ff9d751c3cce50c9acf617959396daacd3      1
     83 0x3cb8b92b9d595f9c1aa6da6fa1d99ad965106c6c      1
     84 0x3cd378c9b1cb5f147ebf1b2c2564118946ae4ba1      1
     85 0x3cdee27bc42f4efa1cc2803942392b1ad777e5dd      1
     86 0x3d9fe7337f067dd52d1dc8cb45490b1ad6c2f65b      1
     87 0x3fa7cdb19e8637d22d1fe37190942ae4b21868e5      1
     88 0x403afdf9ea925d3b48e719a44610da1679a57651      1
     89 0x40d2f4399d23f9afb82d0a6b73055f13208614f9      1
     90 0x41dd1c6338e5b3cdf9dc69e20dad9834ae36a6d3      1
     91 0x42b46951ae492fde820016ad6432c1360a227dd1      1
     92 0x433f6a853d8e1b545da99bdcd35fafc0585dced3      1
     93 0x45f2e08b35a959a87833c57c4bacbf58f95eefb7      1
     94 0x48755ebcaf7fafbd4ac2e73aa5d09bfe93977963      1
     95 0x488bca29538691dd89cf32f9ff4d49974a389f13      1
     96 0x48ae825591a926da5f49aca43608f28fdf37210b      1
     97 0x4943cc7813da1697265fded90a2bfb749eba0d89      1
     98 0x496f6204f61c19aa4a14ffe97cad02ea73e9658f      1
     99 0x49cacab31682089ea11913614d2ba3065264294d      1
    100 0x4a2d5ae059ba3363d5cc6e414fe55462c1217dd8      1
    101 0x4b70222d33ec85c4cf182e42258a8f5ec7a9a6c2      1
    102 0x4b92e36ced303eeb59bd6c5dac555b2be13fe2b6      1
    103 0x4ccdd0c30dbdaa825d308612d93704e8391bce9d      1
    104 0x4ec741b83ec1f0b491152904b1b8383c2975031a      1
    105 0x519e815db12bff4e28c41753d8f72485d55a9b40      1
    106 0x5231b639babbd14c518176f12a6bb8cd85fd6e70      1
    107 0x54021e58af1756dc70ce7034d7636de2d2f1fa74      1
    108 0x5420e4eff68fb834ab1fe8afb6d1755308c20cb6      1
    109 0x55d647550ac4dee50e00edf13debee5404f4c452      1
    110 0x563153823d702516f92fc24edd9358d6973f60f9      1
    111 0x564a8e13d7dd23d5525160d204165bdbcb69b4db      1
    112 0x5665a1ab30ad80b76730b4b816e2cf557c64f963      1
    113 0x56b367a6303b3d626bfbabc625b8e0b5bd9452f8      1
    114 0x586f4a17c9b3765811de88a9d5cfb77187ae631d      1
    115 0x5a6c70e14e35ebc8bd3636ed272a48c58fcc514c      1
    116 0x5aa71418ac6238c78ea9a032a768dbbbf86bfeca      1
    117 0x5b2576a02fe7ed6794a0ece070093735063c87db      1
    118 0x5b8d70429660815aaa13566426d0ed38dc07694a      1
    119 0x5e22e968eef4bd8bd194a82694fbce9a997dd270      1
    120 0x5f3bfc737ba026670a78966687ce095084a1e895      1
    121 0x5fd4252a8aa0a7498b5284f6e443938515773827      1
    122 0x60129ecfccf296ae1972fc6ef500eb1c2829da31      1
    123 0x60df487b9e3ddf8e45552c7217d28e2722a93afd      1
    124 0x610656a1a666a3a630da432bc750bc0be585aeb4      1
    125 0x617bbff4eebe3389842d952943cca435873f9c8a      1
    126 0x61e935bb49ee40dae181d1e795d0deb84fbaadf6      1
    127 0x623b4fb6ff29ea1f8a6d45cc5fe547012ea63a48      1
    128 0x626e0c1c51c9e61026457b3d3ef487d01b8d6998      1
    129 0x6273c7a95a5a205f06780886943be548c2a882f6      1
    130 0x62eaa51dcac065abb55eed7047785a0080e7dfb4      1
    131 0x631cd42eb1063d1fe2b49a78744f3094702edbab      1
    132 0x631ebefd35fa59ac01e9c0514cdbcf9cb70b18bf      1
    133 0x6363de32650afe0db2ebcb597ee4e328672afdf6      1
    134 0x657a0bfb3b275afaed2507a0575de37522758421      1
    135 0x65b5ea1e9df2f92f4fe3bdb6f2cc8550c608a534      1
    136 0x65c3ab2108e754fa5079f982e15ab0b7b5976f37      1
    137 0x66567f4ec7743e58713f44f083da3de78a52556a      1
    138 0x668248df4595e09aa253b31478312748078f7a20      1
    139 0x674ad04d48d7f1a1e7477296b3b1b1cb81d175fc      1
    140 0x69711432cf1bea86242fc41fc829fc804f3e410d      1
    141 0x69cb3b1de24e08f1cfc2994171b6c6930498f750      1
    142 0x6a1f9a200afcbf94cbe168d48e24b602e559584f      1
    143 0x6b67623ff56c10d9dcfc2152425f90285fc74ddd      1
    144 0x6c2a066b6ce2872bd5398347e97223c6f6f84104      1
    145 0x6caea79a0d013c489a4e1b42f06d6ef6274dcc59      1
    146 0x6cc4774cf4d4c738e3310f1b210c6ffe23d93999      1
    147 0x6d7196598358188573b75d4d41bb86c20da2e56d      1
    148 0x6dd08e3c12a32071d643b49f585f954f59c6d57e      1
    149 0x6dfc99cdefd0f2da327b0f31fd0046d8096e955b      1
    150 0x6eff21c2bfdf83a33aa690fe3db17fc672244907      1
    151 0x6f54c650190b44ac9efe531380417ccf12d2d06a      1
    152 0x6fd74f15d3404ae913e67a037645bd0b3ee29ad5      1
    153 0x70b71b98c8b95643462c68e0a396d0a6d15337bf      1
    154 0x70e2dbbeeb13ec39192ee681268cf49bf2773493      1
    155 0x72703b554a7089f93ff1fc6cc6c0e623900a7b80      1
    156 0x72e00700f3f3994001603ee494c0a2d6bfcf8d78      1
    157 0x73ef0e2dba5807c3130e50c950cb78c4e7f364f5      1
    158 0x74ccf779d780acb91e9f2f30db2ffc0b73f28341      1
    159 0x7586834e655ee2de6357b2c8423b76efc5fbcc6b      1
    160 0x7738e4dd4fc3b6c95b8925f078363924c0f0b428      1
    161 0x7844c9a57afaebc030ce68eee4ad9ee2deffa790      1
    162 0x7cc696eeb48dc6acee89447168302a90c116bc34      1
    163 0x7cdc15eba13609a244e96b6b9e4c4a95f81ec953      1
    164 0x7d5300e22db1b2850da1beed10a894893731c1a7      1
    165 0x7d989ce97cff246e23b2c8e9d431e922c3e85dea      1
    166 0x7da76d3fd32a926fd55f8773bdca1422e29cb521      1
    167 0x7dcb39fe010a205f16ee3249f04b24d74c4f44f1      1
    168 0x801ac1049467293c3d8564a039c63eb133757e82      1
    169 0x803e96717d25ed306160d35ab0368dd595d0efb6      1
    170 0x80897383d295d79079dfec48f4343f121dcb7c03      1
    171 0x833be569e4694cb6101ba9fa85f11de6b2ccfd3a      1
    172 0x8350be096c616e555d89c6a91d1df14798e8ff17      1
    173 0x84749201695a2b18dff62242a3368e9f6fbda3c8      1
    174 0x85c3689d2f998e0516dfee8360930121752de70e      1
    175 0x86ba22e3e4a2ec1ae984e9a53e64001c7631c660      1
    176 0x87da297a832706b5115ee1feb47b7fdb7c476261      1
    177 0x8924ac1196e2ae89603925b6e001dd14e3f7fdff      1
    178 0x89d42f152a826e28c69413ec5c98e6bccb7c7abf      1
    179 0x8a202d605a74316efe3ab9200b4683c4295f01dc      1
    180 0x8ab83d869f2bc250b781d26f6584fd5c562fdd9d      1
    181 0x8ed55d7ae8a86032bfb78eedfd318086f9bc43bc      1
    182 0x90857ed7527973799f4d02ad74c9ff66f09ee075      1
    183 0x92b3cd6a5e8dfe8df7ea7bd40ed75e670107b782      1
    184 0x92c29c4f366ef2b8224a66f6db058922d27003d0      1
    185 0x94b08f4daf6b0b1501fe4297280ec99412f66030      1
    186 0x951b169afc2b7ac70799dbad1cfc8f318506f34c      1
    187 0x952c6b14405566fa50434d23af7075b9e0833676      1
    188 0x96b6d6b1630b1c6b9ed0bf0bb88bc6dd3434d04b      1
    189 0x97ef1dcc57b782408aef46cc7db3b133bea5df36      1
    190 0x9810a37d7ae10d65d58f636dc4e264197968886b      1
    191 0x9825fea6d8f03447e58844643024281846dfb2e6      1
    192 0x9947f0e1dab15c77c3e19d8315bf7b637d596b7d      1
    193 0x998280c00d90fa742ca24becd6d897d26cd1539f      1
    194 0x9b25142253717e120cb77868b8a805d56ab62369      1
    195 0x9bb82fbf10cf4959909bab9be07805bd1d28d04a      1
    196 0x9bbd708653f4f105d5eadd19607b7f360fa787af      1
    197 0x9bc5f0190b7fe0cddae91f069c26a6f7a2970fe8      1
    198 0x9c57318d2153070aba5f0302f4d7a76f8050fc88      1
    199 0x9ca6ad1ee310e546d3c37251a5f85cf9d539ff59      1
    200 0x9d11dbc80b3119549fef6a9379f8a5924809c717      1
    201 0x9d36ee50503d0928d213926c1ecbab626a36e4af      1
    202 0x9d8d5fcd9ace767e09548badb85c0bb7d5889f59      1
    203 0x9e121698d68cf4c2b34202ccee11ea62769a8bc7      1
    204 0x9f1c6db9f8603bb40126f9f17b339fdb5b7bd13f      1
    205 0x9fe6eb793adc54267868f310a5f1c029ad85526b      1
    206 0xa0f313bc002c8c6c75f4d772c5dd22dce5692b66      1
    207 0xa140bf519cf50f0e985e9bcb663a731c3c2d4439      1
    208 0xa21e8556a8d69a8168818f16d9495f0c0efdd987      1
    209 0xa27726e56a9b03c50fa298d91c6c8e3c8c200b9b      1
    210 0xa3861a789d05597bc046e8e6d5e10ecde947569f      1
    211 0xa456e09a747488bb1c33b194cf4cb180d2dfe720      1
    212 0xa46425571f389ce2ae50e24cc7efaa074adb972f      1
    213 0xa6a5cd02e5c9cc822ec2e6b11e5f5682e94c2d7e      1
    214 0xa7aa8e48871b3a77196fafbf52144616135e2544      1
    215 0xa7b32f0c55bfe631a9470a6b3312a6685ffd9f45      1
    216 0xa7bce13c268c132eafa61633827b872a248cb352      1
    217 0xa82bd7c6678b1f35049ec64c6e8d413baf83686a      1
    218 0xa8539fe0f1115c987ed98930f51756f99f4a4643      1
    219 0xa88e1c1322f1a35d187857dd3808a06482d9b79b      1
    220 0xab5f32d395b515d132d5a1f1e3337f4f679d13be      1
    221 0xab6b0eb346947452c7feb7ad4f5b7aeb5b4eeca8      1
    222 0xac0f76a8c30e23bc197d26465e7be319713c5224      1
    223 0xac2318d4fbfb276b279f03d46942254b941bb345      1
    224 0xac3a9a3bce192106a8305289688a56fbd5ca98de      1
    225 0xace3898b6e111f8154234e22d248cc3165cee849      1
    226 0xadac20a8285f29551e2bba86b23099d94ea0b4eb      1
    227 0xadd93b1acdfa3e865bd1e6495182e9f1ac5cc35b      1
    228 0xaea57cbf427757d9d51f0fb024cd7dfeb15c0275      1
    229 0xaf37c6b05098645f13044aebaef4048f6568cfb4      1
    230 0xafc093b1c8419f05d4de6ff54d38121c0d733752      1
    231 0xb00b18b3bf6ecc43e6235ee69424d4a220437a4d      1
    232 0xb0cf026678fc237478d19b768b6204c4d13c8f89      1
    233 0xb1bfd5873a33bb2d052e724808a87534fd9d4153      1
    234 0xb4374bd294601a2b69dc6c0de1243f047ebe6e29      1
    235 0xb4b732fd4d12149d5ac06c70032c657b976ccd66      1
    236 0xb4f2ce3bd9afb8e4d08901840f025d679240f86b      1
    237 0xb519095fb7f2438d778f44afb9cdc2f9611d85ed      1
    238 0xb65bf181d1ce58f5ad7ae1d4ddc9a8445157bf6c      1
    239 0xb6cf777e3696a502107417265c92d1b075636a10      1
    240 0xb7187511a0a0f5a63a538b7bc0abdd1a01979848      1
    241 0xb89e9c6a4ede53d6988747d1a1706342070cd568      1
    242 0xb944d8c673142aa64548c8660e9b24c2948ccb89      1
    243 0xbcadea86e8fe97aca4b75bbd65b1829e249530cd      1
    244 0xbd4230b9243e223a469b36fe2a6556fc1280926b      1
    245 0xbe2063c4149d894583946b8b0f9e13b131bc5dda      1
    246 0xbf38423c1412a58f8ec0ec00abc25a1de0b5ef82      1
    247 0xbf46b318a65bc213eb8e5ef4ea02590f82fd92b2      1
    248 0xbf533400fc4ee1d378442a2decbefd3c1bb7afcf      1
    249 0xbf84189fabef7cb88e1e804fc0954910dd758c2b      1
    250 0xbfc9ca1c434ab19e5f75acd2d603dc0621ef64e2      1
    251 0xbfdb88d943b68a7498b1c1eda1b627177ae1db92      1
    252 0xc076945068c012539f4cee94e959b186c41f9ce2      1
    253 0xc10fc4f8566f2e89cbfb682e50552c193aa1244d      1
    254 0xc1a80d351232fd07ee5733b5f581e01c269068a9      1
    255 0xc3086007a83a86b81b42958539d17881c1642195      1
    256 0xc4e7423f38b0d7408b77751df07dc3a49d1b880b      1
    257 0xc542492296d3537eb3fd16fd775bdf7ab8721c7c      1
    258 0xc65bf75996cc648fda3172de46d1f4a3f8931936      1
    259 0xc68c7771ec6a6e5d67d62aa9c6f22df69865e401      1
    260 0xc75ca7ca4d0e4a513bf3d073d0b5a31ec8511230      1
    261 0xc81491a9a8ec21804ec44b6e200d3f67e0a9b938      1
    262 0xcb249211091af4fc730a4a315213563e8c698f9f      1
    263 0xcb9586245ad6e07585515d7ad43d19d163b72bf2      1
    264 0xcbb0403736b26c1180808ade89d9ac4e3589ac7d      1
    265 0xcdd99ee657a33b1da6802f4117d7e5cb2ffa5d79      1
    266 0xceae4ca7d082856d140f3672780029d6e90c3dcd      1
    267 0xcf04c33bfea64cf5dd6ea97e2d5620f825b03b49      1
    268 0xcf4e74c2672fc85807a442a4e45bc903eaada4f9      1
    269 0xcf88fa6ee6d111b04be9b06ef6fad6bd6691b88c      1
    270 0xd022b6911c116ce7ee5cd61bc1b07c34d6cc2e0e      1
    271 0xd255cb7583c3150784973248c9435ad1c7f49842      1
    272 0xd29edd1f18e51ad5bcb236919a6d9a4bfc796de0      1
    273 0xd4357b0a6f66bf471ad579299ff11c841a41fbe9      1
    274 0xd48b0bb8d09e66f23bdfaef2f8c276d98862b96f      1
    275 0xd59120f2a160c9862301c1f3bb5f171279f95512      1
    276 0xd5bbc087949bdddf852df59f011cd8196037565f      1
    277 0xd6208d782261c3df0015d15e4156ce80a32b6c22      1
    278 0xd78bb8dc8c5ff23cc3c1b39f8577ae8a4ebd46d6      1
    279 0xd7d1c6484e4b8b3c09c7b9ca8722aa52b0046c12      1
    280 0xda4ff7b6a1eb3176c347a3ffc56a26ee82fb6893      1
    281 0xdae386aeb8848cdafd00f2c045516e30222571dd      1
    282 0xdb1691c6a1e76bcc214e6cb06f238c342eab55b7      1
    283 0xdb8a6dfec19cdb3d2359c143f7e40715119e0298      1
    284 0xdc220de7655c7c49f5b486884ca11076bf82b10f      1
    285 0xdc3046b66b248f5461929e39cd0fd1e09fe3726c      1
    286 0xdcae87821fa6caea05dbc2811126f4bc7ff73bd1      1
    287 0xdea9b42b9ff7ff7875462283e3ee0c2cfa2b1ce3      1
    288 0xdf803bbe664ff0f46f709e65c7a0acd62117a164      1
    289 0xdf804b144b9a1fd34d80a46d938b7ac9e80c4bfc      1
    290 0xe07295c04d02f2efc8d3701c5582a7651c7a9c2e      1
    291 0xe0cdfbeaa6964cd87055832cb4ee928450189f31      1
    292 0xe179b53d7d44a60f47b0e79ede821440eb605bca      1
    293 0xe1994b2fd8310218a06875b4b3d7cbca8ec83e4c      1
    294 0xe487ed5ec80cca6e8a44f6d6f3f712088df8ad4f      1
    295 0xe58d908d08663223dba913159ac155eb2e8fbfe2      1
    296 0xe597d8a65604b109510a6bdf3730e23d22e61482      1
    297 0xe644b8deb627d630496ccb2675ba195049c98a27      1
    298 0xe6b0425091b3997f9ca13459f18b4896fdfdcb99      1
    299 0xe7d51a7701ac85ba7cbcbd879c69802b8b02ec34      1
    300 0xe8b399cafd95a31340adbab296ba56de59512b62      1
    301 0xe930ec12bd6d8bb293a2ecd97786d0b61bcfe88e      1
    302 0xe96ba1a10f72b58934b9ac0e3fb471d2ba65b757      1
    303 0xea9f83606ea6b80f15ddcfc8bacd61df1a12225a      1
    304 0xeaad4725ab831bb5d2b37e5ffac14ad413de5d1d      1
    305 0xec59e8f5bf4eebdfba456e4c557c72e19ee84963      1
    306 0xec9581e8943b32035fd90ad2607ec49aedc3b64b      1
    307 0xeeaf0e02035c82b6b053c61b7bd38d051964c778      1
    308 0xefe556dfda44224325ed20d800de6908a2503603      1
    309 0xf041c4f026547f7fbea6904f77bea16997024751      1
    310 0xf054083c63253067f600910a0091d921ce9dd9fc      1
    311 0xf0edd65b6191f644c70a68cb173f18d56d50029d      1
    312 0xf2991f38f4e822cf3ca3f26a55307c959fae3984      1
    313 0xf338afe0b7d435a59e57e450d408eb4be3b62e99      1
    314 0xf47813a4595c86595311f3dc5bda053ff5b2e6da      1
    315 0xf54611a627afa7c00a58569932554372dd3f4b3b      1
    316 0xf59a5a36b969de497c4b4bb644122e7888b75c34      1
    317 0xf76982fe0c2ac32c9126002b8988f5946421ce4c      1
    318 0xf86a588f3604e3b12899f710e3d572f76ffb94b8      1
    319 0xfa082f6a03f5b97b0e39bab1e739c69c8bfbd87d      1
    320 0xfa595a41018dd2dbfff8147fcc5478bb424e3984      1
    321 0xfc39882bd2bfebdad7ddaa7f08af717a54d5900b      1
    322 0xfd2cad7a1172e81ed7a88b1546275c8b889f4fa7      1

## Allow Gradient Phase 1

``` r
c(allow_gradient_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_gradient_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 74 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x04df8d02f912d34fef12a1b0488ee56fd6f7416c      1
     2 0x0a98f97e89743406a8611e1b4219a073b60ffed3      1
     3 0x0b3e3a27d85996e20b2cdd0e7f1e1208816d05b8      1
     4 0x0ca4dc095608c6b153019f734106581c85f2dcc3      1
     5 0x134cb213549c9f114176adb9e4a78d6a272a3672      1
     6 0x1566ae673ae80725bcce901b486c336e6acef465      1
     7 0x1a9aee029a1b8c69c4404890c85715cc726746c7      1
     8 0x22fbaa2dd20b848084545515fb04b9c846942779      1
     9 0x248458947c120ca057ec028b3fe7e4b3f26fdb3d      1
    10 0x27b1abafb2cb065cfaf41b4b7ee95d27192151b2      1
    11 0x2924196a2ec71ea4ae1b1357381eccdcee6c18f2      1
    12 0x2947cd7346b3a372b357256b8955ebd4b5735987      1
    13 0x2f2242b447a54cb110b8a7991cf8a27054ee6921      1
    14 0x3511ae23ee25e2b97dce883c7d496ff5d18f1dfa      1
    15 0x3d0a1d20ac580ca8fa35888c92dd0859887f3398      1
    16 0x40d2f4399d23f9afb82d0a6b73055f13208614f9      1
    17 0x43b0bd27d5016e969193412933387b0dd4cf3e0a      1
    18 0x477ea7a022e51b6eb0dcb6d802fb5f0cfc3b4a81      1
    19 0x48464efe55fbcae8ae0c992b306afcf21d4910cf      1
    20 0x520e8d6c5b2dbbe62bf8a40653a314d879b19b86      1
    21 0x53006f95def268f88dc1b8216654ab56f3afd052      1
    22 0x53562e8ecee54356dc89ad263a444528b34d6c80      1
    23 0x575f6540c16a72696c14a17fa64f049992d661ab      1
    24 0x59068075a799594db03c0255eed68e8e121155c8      1
    25 0x5fdb5fdb61bc2975f3be446c5ae9c5df490f55d2      1
    26 0x60d2bc84a71af61b82d1b4168c9103ff6c4898eb      1
    27 0x615502edd5cf2d59244dd0dd53d3c8ec035e172b      1
    28 0x61d9d9cc8c3203dab7100ea79ced77587201c990      1
    29 0x665654f2d2a151be2d0f8e3697e6ce780f732af2      1
    30 0x670a840848eba22eb2a090fd65e1ff60c3ba9e5b      1
    31 0x699990a8e7ada9e92c932d6e8fb365024fc74b43      1
    32 0x69e68074f1aada957edd39c5eae0069973343f30      1
    33 0x6f0735bf1e6c69030d6990cdd580345b370eb50a      1
    34 0x6f113b0c8c7266d7514e4986e3d6aaf013b6754f      1
    35 0x729fcbb6e1289c88ce5113ec1c83a48a8e3c9f2f      1
    36 0x73bcb8c5e30bf85806aade7fc36f16c6b80fd3b9      1
    37 0x75005a05bc163b85991b9c9facbcf3155372422e      1
    38 0x78a576d3751e8cc8f30c54c367fe8e5a2cd71228      1
    39 0x7e5ab36876a267560e7191cedbe99ee7bc04bc30      1
    40 0x82139687faae8a29851902783e02e699de0e0846      1
    41 0x85603a042455d2f36f3ec52c1c756344aa70c5ef      1
    42 0x88ccdfe9dd047b4cec4c1102d2d803e2d8bf683e      1
    43 0x8ba68cfe71550efc8988d81d040473709b7f9218      1
    44 0x8bd9c4b5ea8f6f14c9b14d830ddb67f3720d77f6      1
    45 0x8fc2326375174bfe3baccc98cb9906921dc9599c      1
    46 0x982d3c5223f6b5794fccb3208eb164d042cf2526      1
    47 0xa56c04347abee42f663eff9bc2d0147b97c8f782      1
    48 0xa743c8c57c425b84cb2ed18c6b9ae3ad21629cb5      1
    49 0xab2056903a7b62bac46f45a3d7a70ac799ca88cb      1
    50 0xae0d16586e5d60d334624c115216a52b9b1a0335      1
    51 0xae72c6a6fad9fa9d82d089e1ebf73b3043855425      1
    52 0xb6cf25f5cf8a1e1727d988facdd47f1dfc492caf      1
    53 0xb735af7ae1a77d1ec764c862c2c09bdbf2b34b27      1
    54 0xba4575ea27041d99e6614ec02318f1e23a623fe2      1
    55 0xbbdd72fcce73c2626719be00259ddffef0d5673d      1
    56 0xbf270918afe2ad16093ddce904fc358ad337cefa      1
    57 0xbf814810b44978de273191fd612aa47f7b69d564      1
    58 0xc02e6b0d0c1a5d8cd26beeba0fe8d76c5d2f19b9      1
    59 0xc2419841dcb9a0f8906d06463ae24e00e1470846      1
    60 0xc6400a5584db71e41b0e5dfbdc769b54b91256cd      1
    61 0xcaa1c396e70384db58dd33be74b26fb119e98c3a      1
    62 0xcabd5f77ca9d48f4ef9793f20de42ad39ce93979      1
    63 0xd007058e9b58e74c33c6bf6fbcd38baab813cbb6      1
    64 0xd3e401814d1faa8ca0419ecccbfee93ac7b15b31      1
    65 0xde112b77a1a72c726d8c914c23dceaa53273c351      1
    66 0xe359ab04cec41ac8c62bc5016c10c749c7de5480      1
    67 0xe3b41ae8785e4107cc69f988042ff4a66a367fac      1
    68 0xea39c551834d07ee2ee87f1ceff843c308e089af      1
    69 0xef5ab90a44b68d4f5e3f6be6af4bedb12cd2c66e      1
    70 0xf15a6b54e68884d27e1bebb1624d70c227b7d04b      1
    71 0xf2c5f1fd977dbd6de9d04bc4e62dff722d4bb1a1      1
    72 0xf8aace471723dba6514f6c6d43651611a0c34529      1
    73 0xfcbca52b23db28470eb97680d18e988d8b60b608      1
    74 0xfd22004806a6846ea67ad883356be810f0428793      1

## Allow Memes 1 Phase 1 (Full Set Both SZNs)

``` r
c(allow_memes_1_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_memes_full_both_szns_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 45 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0064f02799ea7748a9b51b5e78bcd274d9e7d0a1      1
     2 0x06e13bd0a3cba08e61028f2326b9ea2ca2539900      1
     3 0x0936d312ba234451fa7a90f68c7ede0f4fa962c6      1
     4 0x12cd871105b29760a61f56753d54a59381f5bcc6      1
     5 0x13eee41d67b8d99e11174161d72cf8ccd194458c      1
     6 0x144bf0aaf16debadcd2638f572ebe42a92f5bcc0      1
     7 0x185952b3bb31da0ae18354bbb90ae40adc840c33      1
     8 0x19e7a132bd4b42f580f546197f42e19c42cdfe6c      1
     9 0x248458947c120ca057ec028b3fe7e4b3f26fdb3d      1
    10 0x2a0a412e0a0d436cca7ddba177c4dd9f29801062      1
    11 0x2c8875f34ceb219f61b7453b2c5f100ec2f6ed33      1
    12 0x2ec4a2bcd4f33c7c9aafab7cfa865ec15508bf62      1
    13 0x2f2242b447a54cb110b8a7991cf8a27054ee6921      1
    14 0x388160a99390392278afdba240046b8b5e73f77b      1
    15 0x3df7131fbd4f6b27be8317c69e41817498e1bc0c      1
    16 0x43b0bd27d5016e969193412933387b0dd4cf3e0a      1
    17 0x53006f95def268f88dc1b8216654ab56f3afd052      1
    18 0x54913cc8ea17731d62589039dd0152f306473843      1
    19 0x5de26d392ea46ffc17131042e2584fe6ba46206f      1
    20 0x5df5342342701b8ae5bce28f74ebb73b5fc13a54      1
    21 0x69e68074f1aada957edd39c5eae0069973343f30      1
    22 0x7525e71f51bda1fbc326000714d2fc68ed5aed6b      1
    23 0x81974c1a09e53c5129c6e4f74e547fda0adf4d2d      1
    24 0x82139687faae8a29851902783e02e699de0e0846      1
    25 0x8774be790cb9e12d5edaf2eb8a3f6c89410a497d      1
    26 0x90fa40f24362c24de09b1cb86df9f0b1f1b15a90      1
    27 0x96e861634b4c0e0a9e7c6a65dec549cc2a8a0e56      1
    28 0xa71000e72d38e6a84d7190f59fd3dfc73931c0e8      1
    29 0xa978eadb605761725d11d3b3a4045cf5859e2d3c      1
    30 0xac772daaa5079e005eec3e53314d6d1e9149dc87      1
    31 0xad102dacb54816ee8b128c63b83c7b5fc587f4d7      1
    32 0xadebdeab678647a457743ea3af98f8b804e45c24      1
    33 0xb05b7ea3714a95857e45629def2b9c3577690208      1
    34 0xb31a4974499daad3255206daee7d1f3595fa259e      1
    35 0xb692704d590e224c67fa96830c4b2526fccaf3a3      1
    36 0xb6cf25f5cf8a1e1727d988facdd47f1dfc492caf      1
    37 0xbbc37f68e9876d64b2c55016081528ae0a85d8b2      1
    38 0xbe9998830c38910ef83e85eb33c90dd301d5516e      1
    39 0xc02e6b0d0c1a5d8cd26beeba0fe8d76c5d2f19b9      1
    40 0xc78cd2e1e8ad4a288eddafb139c9d0891ad01ae7      1
    41 0xcaa1c396e70384db58dd33be74b26fb119e98c3a      1
    42 0xcaf3365d474690a1ac6643d3d6ef44cb0c6deec4      1
    43 0xce8ad80ce1a979381d209ac230d02adafb9fa897      1
    44 0xde112b77a1a72c726d8c914c23dceaa53273c351      1
    45 0xef5ab90a44b68d4f5e3f6be6af4bedb12cd2c66e      1

## Allow Memes 2 Phase 1 (Full Set SZN2)

``` r
c(allow_memes_2_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_memes_full_szn2_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 102 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x0216ac50fdb6b46f6b74254b84ecdd1d431d670c      1
      2 0x05a13dcf55ea6d532f15284f39e02811fc183a8a      1
      3 0x076097cf460e3b5fc3a94c43d9a598164bb29168      1
      4 0x0acabdaceab63c08bc5aea63d7e07da7c2e42959      1
      5 0x0cb5b9eeed8d51ae57e70e46b2a3596b94ece6c7      1
      6 0x0ea8767de750442ec4a34d81bffcfd4417396f47      1
      7 0x111b863c2f7d1833d8f53830647c260169e99626      1
      8 0x128ad42b82c752c5b4c7f679231d8956c98038eb      1
      9 0x144c704bf25f1865e4b24fd6596ffed7d92470b0      1
     10 0x14abeea8db07c46b37a79bfa856d4c2da6d3e6df      1
     11 0x1acb5e5c169aed4b9f963bdf786a8377bff678a1      1
     12 0x1b6265a40839a331ace2d81bc82b6104703c0426      1
     13 0x1be1eac561b34c9b5e45eba9c3fb51fc5fc2796e      1
     14 0x1d10b0166a761f269adf864ebdaca9be445ca637      1
     15 0x1dcfdcb24456a7dba5342479cff0f9ff30145fb8      1
     16 0x20542dffc2f7a51cbc087f54fcafd9ffd8b5c899      1
     17 0x215cbb1b60e2cc1f1e8626cadca386b278aa0dee      1
     18 0x220da3fc3b905c968dfb20c81c170bc4dce56603      1
     19 0x23ae72f8336aca747ef02d596403de56cca489fb      1
     20 0x27bb5366ef655819337d6ffd29a55905608c853b      1
     21 0x2917e947fb1925abe184d4062520a11bcf2a5b36      1
     22 0x2c45f65d254e85584b47ec82450c91a30cfc6be1      1
     23 0x2cfdaf06e39e5d9d3c9afe20362b725b41f68725      1
     24 0x32a0d6b013cf8ecad1e37e99532570411d398d05      1
     25 0x35d7dd9230a1f079c907b53e7c5822b34e197a1d      1
     26 0x378dcff8e6be1778b04f5712d686517bb6a01927      1
     27 0x37c14f24c73a9e151575a0af3dd2dd5cbbca8a29      1
     28 0x3f4373afdde3d7de3ac433acc7de685338c3980e      1
     29 0x41254fe73d3b49f0fe3cc7743b4d40ef26bb338e      1
     30 0x429683282e5a28b81aab1f9126b8d5e4847dfbeb      1
     31 0x47e64ae528b2d1320bae6282044d240ff67e703e      1
     32 0x487869b7557f0fdecbc5141da1d7c48c15c5b8eb      1
     33 0x4b6c1d196e06446eb0534326bbfb02cc3d073a2b      1
     34 0x4c48d00997c6d23d6b47a9ce8caa6027d0068e42      1
     35 0x4dd8f136fe8a8202cd6395ad34ef5ae49c26e78b      1
     36 0x53f1b3d8990472d1adcd7b76348e5508db17cea0      1
     37 0x557bd9e829f35f1d853c2c85341458892753fa1f      1
     38 0x58059014c5952c04370bcb88a2e0503e9eafb209      1
     39 0x66b280b5778c35c719209614428caddf00aaa3ce      1
     40 0x6b29380bb9955d5b2df1624c5c6a72a5e69a6b80      1
     41 0x6b539c723716628725aee9e7e26796cfe1f38fcc      1
     42 0x6b6d5c57533d22d0de14db59fcd28b9ea5296090      1
     43 0x6bae54650c0d17f4aee76839a7d319dc5060d6b0      1
     44 0x6d1db4a7e83dae0eee7e95d421722d46d2a7e94b      1
     45 0x70c8db61d09271f4c90950ba8c6cbaef918f12f2      1
     46 0x7ae3b0627dac32d3ee16c204ef791e37067b8460      1
     47 0x7e0051a0b48f3c5848e66fd02bea3400f979a89a      1
     48 0x8043812aea5a07dd6523b5d83abd5e606422944f      1
     49 0x808421753a181e96812796b7ab43d3f356cc5a77      1
     50 0x8120cd4fd4d867dae6dc1c2e9c504224ed045f34      1
     51 0x83ca0f19abd240c3b04bd55a2046c308d042e196      1
     52 0x843708d85621273f3bbc643b348da3a60d5b0334      1
     53 0x84df1fb4801e72cf6958a994dc0b690f9e7cb23b      1
     54 0x8586c32dbe1c986bffb8f1ed6c3e8c6489f4fa3c      1
     55 0x86c71b7d27d00d7814a763f9e6f16fdb09f7e4df      1
     56 0x8d5e59e11838cff72af5fb0681d96a9136ad0604      1
     57 0x8ffa8d00db5df56a20c30b737c5efdaabe140df4      1
     58 0x957143815f0e1d1b1a31b2bfcded5b416ee675ed      1
     59 0x97d2b1b4a249ec77d56fd1576546996d14f7db1a      1
     60 0x97ece7185467c78293f3b796bde3704421d4fa69      1
     61 0x9e1e3857fb2484379858b9daf230379015a7a100      1
     62 0x9f3c44a7331ca9da2d3432bcb0df91186a27e3d7      1
     63 0xa7c342af335ea7e0747452ae2e3c1901498a9a76      1
     64 0xac9169946d4f6fea622900fa6aa1812d5500bade      1
     65 0xb08f95dbc639621dbaf48a472ae8fce0f6f56a6e      1
     66 0xb335326c7f2cd2e4eb09ce3d1745c92f66497b7e      1
     67 0xb7546d1b04151e1e96d38275af66eaf984fdad2a      1
     68 0xb8b8ce5e3d0ad5c0169e130c64048b6d3cfcd343      1
     69 0xc03e57b6ace9dd62c84a095e11e494e3c8fd4d42      1
     70 0xc18d4c2fee42accfe7bfdce98bfacbabf76242e9      1
     71 0xc29ded4c6ede7482068197c3e3205d850ffedb0f      1
     72 0xc58326c7020f26345f4568cc09daddf019a8e6d4      1
     73 0xc5a2c3a524be0792519655d4093b44b988db4f68      1
     74 0xce990032a3ed14e197421270a9bec9e276bf2f31      1
     75 0xd0d9597369fc67b9eb2703d6e9ce5fac0a77f33b      1
     76 0xd1dd467f1630be10d67205d6c816429f8ee49124      1
     77 0xd2f2a56affb67a9ae2499ab9df1689ee78fad89a      1
     78 0xd4e791d799b8873d86ac06a5fedf2f1a75f639be      1
     79 0xd5ec003289265705727b622f1700fe814e54ca67      1
     80 0xd6b2735e290ba3726d25c9762098cd26a16f023f      1
     81 0xd72c8dd33954e8aa53d5c108906b751ce4b2382b      1
     82 0xd905de91f928b20ea0ccf455e5ba07eb6373955c      1
     83 0xd939fe2e4776ccaddb823a26eacb45c66d8ad5e9      1
     84 0xdf36b093cbc06522b68105c3d9141db188e980b6      1
     85 0xe418a9a5e49dde0d13e1ef51d4bfb7fcc51c28df      1
     86 0xe60458f765bc61e78940c5a275e9523d1f049690      1
     87 0xe7c5ef2cee9c15b04fc7dea61a1ec443dd8e7fd1      1
     88 0xe7f31a4516dbebc0f443a8d4ac8b22d1a8f096e0      1
     89 0xeac5f9b3cd48123a69fe69ce93a7f58100a56552      1
     90 0xec47cbbd9e05f8c4d587eb604f41740b0b2f33e4      1
     91 0xecea65d22233e7beed752a06c46cefc6c90a1d7f      1
     92 0xee91d62eb5aaea933efbfd0790613af5db305006      1
     93 0xeeeb5dd03eaf4ab6b72949191d8ffcfe27f7d973      1
     94 0xf28fc8d0eb98a34f2b03d384f7db6f1e7b877a33      1
     95 0xf2c5f1fd977dbd6de9d04bc4e62dff722d4bb1a1      1
     96 0xf4dde6ea673fb810b6f503bf441908bb30b929f8      1
     97 0xf624e9324f9b330cc0289775d7b91e945e881134      1
     98 0xf681efdcba35c69dc0fe56b7fdb56324f5abd385      1
     99 0xf6fb4e6cdd7ed50dabd90d10b1ddb60a85720737      1
    100 0xfd849d8cf365fe98bc60025ff7fab45cef0f557b      1
    101 0xfeea8258077cc06444679958185f4198dd4cd324      1
    102 0xffe6832850476eb6d5ff184d747ed84f1b686aa9      1

## Allow Memes 3 Phase 1 (Next Top 200)

``` r
c(allow_memes_3_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_memes_200_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 200 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x017347cb75ac8725608e593ea35d83f9b2b3cfb8      1
      2 0x02b294ed96e1175b8c71071a303e8a7d2396b017      1
      3 0x03024d05b1aea10b64ff3af1fed6a3df9adeebb7      1
      4 0x03f4cb9e297ea659f30e09341ee7155a7d136398      1
      5 0x04294157cfba9ff0892f48f8345ea3539995f449      1
      6 0x047c4d7af709ae19d3bbbd5f9e10461d2554da00      1
      7 0x04df8d02f912d34fef12a1b0488ee56fd6f7416c      1
      8 0x0843e80240f9304f3fe7059f25f3e1a66886a0eb      1
      9 0x0a87cbf5eec20d8d9880ad21f0260522ff5a675a      1
     10 0x0c481cd7d09bdd222351d52622d272367cb3d159      1
     11 0x0d6b4f304b91a3b67b1db34776ef7e95effbc729      1
     12 0x0da4438a9cc8d41fe9c0c6efd302a173ed967da7      1
     13 0x0e5c3e22b7239cef46f56741ff66369f3cfecb77      1
     14 0x111818a51c4177e8980566beea68fe334be7b76a      1
     15 0x11a22b262e505d355f975e1e48a365b5d4811ae0      1
     16 0x13928eb9a86c8278a45b6ff2935c7730b58ac675      1
     17 0x147f466fef6bd04617fc0c8037ea01c73bc25a3f      1
     18 0x16f3d833bb91aebb5066884501242d8b3c3b5e61      1
     19 0x17e31bf839acb700e0f584797574a2c1fde46d0b      1
     20 0x18595f049a9c5c2019efda113009b0ec1c89ceaa      1
     21 0x1a4370fdd7173d0a41ff7c63a8e0249479ba0225      1
     22 0x1a6f0dddb884854355023a2dfe26e9174f8e0290      1
     23 0x1a7d7e6abea492e7b9a3cae667f468c72e884671      1
     24 0x1c172d05b75178fc669d74407243cc932030f139      1
     25 0x1ce68e5912ef8cb4e83620a50b70edc900ad4759      1
     26 0x1ecded4519a9688ef4bfb5f15811c52f048ba1a6      1
     27 0x21b9c7556376efdbf9ee316a4ede0c8257fb7533      1
     28 0x22418183c90741bab6606c0e0d240ae6c3a148f0      1
     29 0x22e8efe40ddb7f13b17b4c10f768967fc7a9f875      1
     30 0x22fbaa2dd20b848084545515fb04b9c846942779      1
     31 0x2801dc73a6dcefb364b959606e0c61234105fd5a      1
     32 0x29c2188c6c318ab5cae4ae4b11a49edf2ec9ab0e      1
     33 0x29cd8d94a4e564a6d6576b467ea0bb9d51c9f05e      1
     34 0x2a462fb59a5b0885b60ad31c5fe473d5ba0d79fd      1
     35 0x2be96c05fb7e81b020c42086b69d21bbf427b53a      1
     36 0x2d1543711bd262108fd8a1afb43d8871f76c134c      1
     37 0x2d8b81cde837334beb89a9c0a88179ad234770be      1
     38 0x2d924c0bc8033e7866b90ca2a76cbf4b5714b11b      1
     39 0x301e2d2a98c5873ac27fd9eae85f0153959100fa      1
     40 0x313c8673ff69dfd7be2a46bc540305d3e2d233b0      1
     41 0x32e263cb9003f7fef329080bdcbc82f5cfd6c02f      1
     42 0x32ffe815277ff53dd2a73557664e229899e6501e      1
     43 0x343e85ac3009ac01538d4251900ce1a2b7d7ffec      1
     44 0x367dc97068ab54ba1dfbfc0fad12fbcb7b3a0d09      1
     45 0x37ea6c993c5f42e1fc3455d5781e4a580760970a      1
     46 0x3cb63b82d778105e43f064ed739b0655f1f0fb87      1
     47 0x3d91ad906c3969f86c588c988f721d5a735c6f81      1
     48 0x3fa5a25f48ba1b736761706801be4f639ca4853e      1
     49 0x405020c797a64f155c9966c88e5c677b2dbca5ab      1
     50 0x410a9f44948576c60f94b165f63eb79ce769a04c      1
     51 0x417c269b82387ab605bdfbd312d91baf03dc8516      1
     52 0x420ecd4ec65c0fea87d8dc5d16c2476c42466b65      1
     53 0x4220132c9df1ab7bd2913f0fd03297c90e7cc6fe      1
     54 0x431181dae361813567f35ee2abac73291820fcc9      1
     55 0x47d539d4dd9b6f21ccabc5c96bbbf7071290938e      1
     56 0x47dc3a7aec5c0e1f4b2c71303a4b1eaa0bee3e4c      1
     57 0x48464efe55fbcae8ae0c992b306afcf21d4910cf      1
     58 0x4c941a23ec94580d3b5442aa8d5851583fd8bcce      1
     59 0x4d7e6948fb5ed2d92897de0605cd642eff69fbf4      1
     60 0x4f492b2a3ed9bf0c65d508dab4f7c5f5c04ca6c3      1
     61 0x506452ab0dacf04db0ab1544b6be4019651ada90      1
     62 0x511c85042524a41bbf30e7a8533a84b8c5e55f43      1
     63 0x541db1ed2628f2f4897417d06181af6a179e90d0      1
     64 0x54669ca58bf3a59caea8ae5135db491e4738f65a      1
     65 0x557c60995797fa7b47be105227a2e46148d85750      1
     66 0x5663d7d0751db1a80f01d6974e02036b6d44d56a      1
     67 0x56b367a6303b3d626bfbabc625b8e0b5bd9452f8      1
     68 0x5c3097e7fd3b97d9fdeec6d378884c892ff0545f      1
     69 0x5d0d23ae81ebe1b6ac96411b2c7bc3bf7463fa25      1
     70 0x5d25087405105bab12624c73488ec186066a6376      1
     71 0x5e3118e91b0a02a5e2b92e49700c63434e94f0b2      1
     72 0x5eefc3ae27be1483ad6b864b45ed72fc39e6bb6c      1
     73 0x5f656037e30a003862cf878db24ab5f537177fd9      1
     74 0x614b89f072ea263a9387460963142e73548fbaf1      1
     75 0x62ba9de7d81b96d697fc2f26b5eb647f184f9b2e      1
     76 0x64ca4373a93e25822dcfdca0306e899a29e92b3c      1
     77 0x670a840848eba22eb2a090fd65e1ff60c3ba9e5b      1
     78 0x692d6cf19e0c082185e20ff5ea6bb267e7aeb278      1
     79 0x69c8c2923005d26eaeea9500d7602eff8c81c848      1
     80 0x6e5632d334b861dfe4312c7ba34e175b60be0b5a      1
     81 0x6e88c16794cb212bdcfd6b6f8901af2f83237d3b      1
     82 0x6f566a6672f615c2975f6c48224c46153e12ffcf      1
     83 0x6fdd8c800cbb81790cd48a20f90ce7e725f89c27      1
     84 0x72703b554a7089f93ff1fc6cc6c0e623900a7b80      1
     85 0x74d50ea9f8cf5652e75102c3fa049c01534dd6c4      1
     86 0x774a34da2ee2e2d242180819f1ee88783215f7b9      1
     87 0x78e37b881d078d4b2f90de0422dadfd4da50ed4f      1
     88 0x7a231b6d33f1da74e9fb593bc785b4914a852867      1
     89 0x7af061a6ec9616135231cb8de2ed2a0a4140e90b      1
     90 0x7c741ed01ee259feba4b7f9cac114f48bcafacf3      1
     91 0x7c84aab9f3eb0897191d95afb2aeb56a63973532      1
     92 0x7ca00a09e3b431d48c30e3e6cceaaeaf6385cc81      1
     93 0x7dbba7f0551aef5894fd6ee331b95dbb4349f5d4      1
     94 0x7dd14501c25c221ffe213d7d56d225f4fe411038      1
     95 0x7e737324cd6702e1b93c9240072ec9cc036dce21      1
     96 0x7f1796071eb91aebda10a03115fa3cd95efdb25f      1
     97 0x8179e48d959a4c41d5e0432ac05b30707149d649      1
     98 0x84aeccde4c9f217e83d3fa28c31d34378b903f91      1
     99 0x8623a32af48544b055fb6ae64f33eb43edf091ff      1
    100 0x862464bbd83d5743f9cf873c6ea3d187d0f909bf      1
    101 0x87e91205adab9e2b0be9f7562af3f5b42fad0838      1
    102 0x8854ad314fdbcc275eb0d5f4fd6dd86ab7981cce      1
    103 0x8854b06ba346a703a3c043e2e7b8822db3ca6b3a      1
    104 0x8874174a2366668d54fea6343f71709389563c8a      1
    105 0x88ccdfe9dd047b4cec4c1102d2d803e2d8bf683e      1
    106 0x8a063961e57a68a8a1f68930d758e1bde05fc6b3      1
    107 0x8af0b9a9b751e086122bc340188bd9d99b8c7ec1      1
    108 0x8b4567fa8c4715c27a682215a031033a8b544206      1
    109 0x8b4d0402f7b2f063f255214b7095b5911a257a30      1
    110 0x8bc3757a675be4c2e459813769992ec2c60baaaf      1
    111 0x8f160eb907a80517a3fa6d22f7cf20f552696a44      1
    112 0x8f6504b48c66eea4e9fbf64fe220e02f1cc4934e      1
    113 0x92b1b677728e36734d59067a4ce53e5bf1f3a2ab      1
    114 0x954d65e5310607e12f93f6156de92cd37ffcae8e      1
    115 0x98004498c85c4b1997b3f669475a038bbcec2160      1
    116 0x9b742d1e98e5bb2f4d50f9fbbc047daf288ffc8b      1
    117 0x9b7c0814eb341e6a4ca673148e8b577b13e70787      1
    118 0x9caed8a01c87721570de895a9725334a0a0cdb70      1
    119 0x9e1f7b007511036b5cbe5df6d39550bdd2e8bc99      1
    120 0x9e640badecb7c628c6188b74488823e879f42a1a      1
    121 0x9f6ae0370d74f0e591c64cec4a8ae0d627817014      1
    122 0xa04f4a4b7306cb72f30828834db01699362a4989      1
    123 0xa0f1de4882a5cd1851989d9a1e9bbe9b4604e9a9      1
    124 0xa32c38646299818ccedc6401818c2e1639c39c08      1
    125 0xa41341bab6588948da8503d1dfb0a0ab0ea269cb      1
    126 0xa490a0346808dda91aea6698cb19e4697d9fc5cc      1
    127 0xa4b61e227361a9cd9e62ca10946c27748a382cab      1
    128 0xa5214719feb1d07b66292a5d4d0a2712bd048435      1
    129 0xa59422ec0634a0263bcdfce5e29dd2df3803447e      1
    130 0xa5ce27ca5e31b1de92db2d2bc109b3e23cf1d4c4      1
    131 0xa6d4758ef49d2ae8e65e1a02216cb9744aee6b23      1
    132 0xa711818b11bdd5797042ade80e3a59687558a4e1      1
    133 0xa817b9bd1ece360e4a1692894a5ad1d40b889f20      1
    134 0xa90aa01ec98f390cb86cd1846f7c0fd342a07955      1
    135 0xab2056903a7b62bac46f45a3d7a70ac799ca88cb      1
    136 0xac6e4334a82b8434b0122bc3c9b42d0a5f190d4a      1
    137 0xaec3f7e735505c2b787b05d67f4adaefe3dfd678      1
    138 0xafb4fccf58bfc06ebc8ccd387b68140cca05f4c5      1
    139 0xb196931ec22517b0510705eb56d5652fe73877f0      1
    140 0xb26c452e8ac20ae7445fcf46b6478d3984f5d8e9      1
    141 0xb2a18003067dd5f96ce5a51f276c01873c136e5c      1
    142 0xb37c5603fb0c6c2e2b62d0525affed9e1e5a6a19      1
    143 0xb42ab92f5af0f162fffefa2b1e12702ce3fc9e17      1
    144 0xb4627672ee52660a9e453ec541834e04583f3602      1
    145 0xb6c7e03ede87692deb1de7e40723b952e0fd3773      1
    146 0xb8700157128ff544c46cf35dec5a19120baa8eb2      1
    147 0xb8937891f08af9854a5ae7a5ec0cbaf4e68acd4c      1
    148 0xba12fda058a14eb03c14613601c3a30d6f955196      1
    149 0xbd3a5d12af9fd82fb927fd26216e4dc2722d2337      1
    150 0xbd751cac1fb27d930a6a0394d97c77908d94ad5d      1
    151 0xbe8fe12b9eb1ca2a593e6c070c71c294b6fe9f00      1
    152 0xbfb7969e75ee4e58620f74135d167f5bdd60800a      1
    153 0xc1966d218b7492dd9cd672a5a237cef4d82004e5      1
    154 0xc2e8ed8cc0be70f91fc9aa903d5f4538719d7dec      1
    155 0xc45920062985116eaac6589058ed337066d6f2e6      1
    156 0xc4bfc1dc25827f39ad4a66bfc968456c7852a9a2      1
    157 0xc6411ff69f1ec6bfb4b082f47db61dbedcab250d      1
    158 0xc6cf5a020bcfc65c6f8181c04cbb5ef5050fe28e      1
    159 0xc7a295b1b2df1efb82aa204958b98ac30171cb85      1
    160 0xc7bb15c11595c877302ddfb330a4082d92f5bcd7      1
    161 0xc7d2d152dda8cf7a7860288c3db9a1a4ce29162b      1
    162 0xc94a284808e31b9ef43f9c9a48933e5a268cff9e      1
    163 0xc952ec84dd5d8ec32937a8e4105922c664564450      1
    164 0xc9ae113b8451d0afc8a021d796322d545a7e7dbe      1
    165 0xcb85b581caa71949784d2e826cf9df391c244b33      1
    166 0xcd30b6de9dadb0a1598eb12a4ccf34b98c98c1df      1
    167 0xd0b2d2d86a45bf2f13a3af83df6e7b5fe98050f9      1
    168 0xd1afbe4da2a5adc6faf30fcbea9e4eea0ba8c70a      1
    169 0xd2b9f9d1e6734f241b2a6eb64ef7f6e63af0a97d      1
    170 0xd3069c9e486a8b77add55ae3ceb3a4b138fb76c7      1
    171 0xd36590461162795ee33099b2076a0d4e017ae17c      1
    172 0xd3b37c6567e2702aa727cff4ce5939b4a07a2fde      1
    173 0xd7d941ff2890bda98f40a5dda0593d239a603f09      1
    174 0xd7e32b8a326ffd9e784a1ee1eea37684a7512171      1
    175 0xdc78107155918e230246439e4159fea4c477eae9      1
    176 0xdcefc9ff1e47e458e56959154c1cdcf56003d30b      1
    177 0xdd5cecf2835d465d490f79ad9773a848805a3219      1
    178 0xdded73b0bbdb3782a453c9202f54c746fd391068      1
    179 0xddfd836f7c9e42055b1f6ceb005fee4c7882f161      1
    180 0xdebd22d7f63648dfb69bb6e2a4c88264d578c0a4      1
    181 0xdffd6968900b3f4f667b69c5f9df65d9d7796a1c      1
    182 0xe05e90015f6577ea1b1eb682ff8e1db1b7b6bcce      1
    183 0xe1c22662f4f1320c6e5319d002b3c3d52f0d0135      1
    184 0xe2d22dc1c2f7c58f86606e405325c69f5210a6a7      1
    185 0xe74ca58ef724b94fac7f2e09e2e48536a5c1ad03      1
    186 0xe8a05c7f4e9c1aa060cf941dbb50381f342d7d43      1
    187 0xe96eb4507a1d162bbb99301fe592d310e9489e40      1
    188 0xea0ed16746257eb2bc11de2fefd63cdeece23a98      1
    189 0xea39c551834d07ee2ee87f1ceff843c308e089af      1
    190 0xed1e25178d7a6438c356d3459fc3efa9458ad52b      1
    191 0xee3aa3fa5b780bc73da73a82d47fa2d8cdc1a647      1
    192 0xeeb9148a32b45672e16591ee18ceabc773b099b5      1
    193 0xeecc9f94c674161e84ab0c60c020432ea8f88bc0      1
    194 0xef04cb0003ca57dc7bb563d5ea3248241b145bdb      1
    195 0xf054274dd74987395d28136e53f39ef4f7b19994      1
    196 0xf161ff39e19f605b2115afaeccbb3a112bbe4004      1
    197 0xf4141aef39803327497f6b81a21bb3f2acfa2436      1
    198 0xf8be957f65e67fb0342992a51c30290d5758f880      1
    199 0xfed52d251e31178ff8bf4a0d611721c544f74fc0      1
    200 0xfff39900273ffb1045c7cfde62df1720b63fd6bd      1

## Allow Memes 4 Phase 1 (Random 69 from remaining)

``` r
c(allow_memes_4_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_memes_random69_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 69 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x04d553053e186e4d58861a5bf3b7075fd904c6d0      1
     2 0x06cc21aeea76cebb12da254b6e982516662bbca4      1
     3 0x07e92b07f74a3d021534e2f0382a9054411b8b2e      1
     4 0x084c2c02a1448dc79692d8908648cca6d5e0b1d1      1
     5 0x0c5c2e6a4ce269106ba9ae1ea397b02b9643111d      1
     6 0x11e964bdb0504ff35dee22862da8f72603ab4289      1
     7 0x1354eeed19ff645a63d6bf46fcf85c1c62d8dffe      1
     8 0x142cc94ba3b7ea0d77d06cfc3fa40aedc186f601      1
     9 0x14b27e03a394cadec25b2b20573e1ce88f482e91      1
    10 0x164a45abedabb674c031f7bbb0226cdd06fe41a7      1
    11 0x19f605f48cb963115f5d8f8d9e8ec1890877745b      1
    12 0x266b2d881291c5bb36cf386d16e5f8ebb8e19a71      1
    13 0x2db89e03296c0f69921ab5d24ad9cdba44a7ab49      1
    14 0x31231036ed51376a70b3824d7da1a4e036def5a4      1
    15 0x36d89738405a8325b7c560cab2dd1b88565effd3      1
    16 0x381e6beb053a494b7520eeb4f6e26d4ccb503a05      1
    17 0x388cbeec5a031e4aad12599b04a4aa4f088e0f73      1
    18 0x39f1165e037bcc35c164e744d4a2667dc8135114      1
    19 0x41d3ea0d6a64d603cb8c3ff710f05f546061cdde      1
    20 0x435a4ccc3712b0b40ce16a1c7bfa263ed6e9bb33      1
    21 0x48238faa9bd6d56fb44a32dac6185f04eabf2117      1
    22 0x4a6d345c287e3c315eafd0e6a4274a74c6cb9af3      1
    23 0x4b3b0a6c0bacb088b6b0b0ba31bab77282068117      1
    24 0x4e75ec3796da954c7f94d98f5c7808ee95281504      1
    25 0x54dcb363205d953d620809cd315640cc36b679e4      1
    26 0x59c1591ecd3689e911a49ebd406f9fb157171cef      1
    27 0x5ac9982211070c077213471cfbd496a7af34cb14      1
    28 0x5dc97f41742524ecb3885ea3cc33439c100acdb3      1
    29 0x61cb53ba546c5264cbe9324843a25882cb82e1db      1
    30 0x645fd16f3508391e8385fc0a181076994079aa1b      1
    31 0x64932f86d69f2717307f41b4c6b8198000583c63      1
    32 0x6d25c0fa070e625dac76b29bcf0917303cd52e7b      1
    33 0x6d8dec3fd68d94a7189a98346ea52b4d32e00012      1
    34 0x6e2fbc139e37b43f4f29cc10b7bede36587f7306      1
    35 0x710852027bc25db82ba562d5b1499b6d9b02fa9c      1
    36 0x7c3018a930e2d0d060069ac1ec38dbf9619e20be      1
    37 0x7cdbb8e309ce7c701ed200be123f16c6f7c9e525      1
    38 0x7cef7db4289005fd56dfa6c73bb2be6ec13bc250      1
    39 0x7e5b399e254665590266ac6a9e2a1e3336576cc0      1
    40 0x7eed3caf47728c4b22bf9b70e708b513b5350f55      1
    41 0x7fdc3797c6c4e0683e40964d526d2eec89686cc8      1
    42 0x7feea2dc4c9537f81f26602ccc8d38b0169ab2d8      1
    43 0x8215643e5757d44940586aa670c932bd703034f3      1
    44 0x82d32aaec5b77528d920843d84cfdf45e84ae9b4      1
    45 0x8c1321c782c169aa93c7c211ed92956e84d2f3e8      1
    46 0x8e68520b3ce0561da3542c624b77afff589216ce      1
    47 0x93243bf5f577c1e325987862267d0314f72e0050      1
    48 0x9959dd45f8d5168cc556e8103b346590185e9616      1
    49 0x9a5d5f6d17b9d9093f1107dd6da934c292a601cc      1
    50 0xa75dc095ed4ad69b088c3eb8ba2f93f1aa942b6f      1
    51 0xaa2cfa73b429d1189a8d72d0d9a7b1557590eef4      1
    52 0xac2318d4fbfb276b279f03d46942254b941bb345      1
    53 0xadb630d5795b384a5ceae977543714b249aee808      1
    54 0xbcaeb5ddadbb3ba2880b19d254e0cf5e23a68917      1
    55 0xc3f1d077fa590018d3924ce8da1a085b4eae506d      1
    56 0xc9aefca21844fdb3fe8f58d883f908991d757692      1
    57 0xca6983947efc848df45c9e84b279438e24727d2e      1
    58 0xce0f69eebfa13caef1f580eee21631963a90b332      1
    59 0xd64110f209e3f77580b72a96ba9be29517e41221      1
    60 0xdaac4e26f11223cc043dfd8e3efaf72333067339      1
    61 0xdd3adf8a817421dc7a7434c9cbb148601396ac44      1
    62 0xde8328c4f5ceeb2c2b3e836b3354413161c81285      1
    63 0xdf2e06c29cfcdb4f1231504d2116da7bad9395ee      1
    64 0xe74abe3bc87a4cf34acfd7c548208700c0e669b0      1
    65 0xe8091980f2190b5798c9758c1b7b6d84fd701b51      1
    66 0xe8d471f152de648d750d041da4c040cfb2b95ecf      1
    67 0xee2a4c655ea792d92f12a3caea21188be6dea808      1
    68 0xeed489a100880c2d18361aa4d37b210788863c4d      1
    69 0xf859068452f14ca012f3b2c9604bc8edf3283db9      1

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
    1 0x13eac939d3c7ff1a985d164d1f14411505b4c822      1
    2 0x2c8875f34ceb219f61b7453b2c5f100ec2f6ed33      1
    3 0x9274f2f89fbe5dc00c21c628e46a18c7187c14d7      1
    4 0x9dc5b2cee788e3e9c67489b5d7ce634dbf48a32e      1

## Allow Memes Phase 2 (Remaining from Top 500)

``` r
c(allow_memes_phase2) %>%
tally() %T>%
readr::write_csv(file="allow_memes_phase2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 168 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x039649f7c2f548692184da3fedf316f58e8356c0      1
      2 0x03c5e3ff9514031f910c9bc9ebbc7839e02b23d8      1
      3 0x09b2f0988b6d24336b348c84c2764d2b4f85142f      1
      4 0x0a7669015fef82f5136c9a2e44e1ecbd2a5aec19      1
      5 0x0c4a911ed6af1032beec7c19225c91a3160e39e3      1
      6 0x0c6306c1ff8f0ca8432b761085d5ce74160a499a      1
      7 0x0dd38657d7c8024e7d62bde3d0423ca34769be50      1
      8 0x0e3e001a00c07e2cb3219d1ec0c47cc532d87804      1
      9 0x0e81a8a496f9cb7e1d29382e91f06a6f7416cb9a      1
     10 0x0f9ab846449ca068f9714818a6ed0da5d819e9e4      1
     11 0x0fb7f9eb8e9bee4dd70dc334b94dee2621a32fb3      1
     12 0x100f28c9b8b1fda897eadc775caf74e0f3560f73      1
     13 0x11924c505ecab6a2434bdfe94bc0ff1ca081facb      1
     14 0x159e5998669ec6628a6b670c2ef1ddbd93084698      1
     15 0x167a4e6066d6c96d5c012006f5cffc9f606131ec      1
     16 0x16b5ec9e34bd0a916822d6785b1c600ac911d6dd      1
     17 0x177a8340bddc2041cc362aabf543dee4e394e3e3      1
     18 0x18412b9f84df461bdc0f2e0709df507d5bdb6363      1
     19 0x1a7f174dcd4211d41c7c4d7dbb3a0e4db5780c67      1
     20 0x20ebf9ae7c66da763fc1ce86062ce883389c8e23      1
     21 0x2177da04f9479496c2292d6344d306aa49beb34a      1
     22 0x21f023839e7b6fee67d33e4548791fa388564a02      1
     23 0x2afc032aaf6d7ead33fb05f9d42d874874dfb9d1      1
     24 0x2c9646bf2477a747897047b69ca1a1913ae5f611      1
     25 0x2ca75a892b59bc7cf863ba261e49ab40a4844ee3      1
     26 0x2cac9e6d1d630a35cb8019779daf4c5fd96135ca      1
     27 0x301be4f5965cc6d386a3aad84f1344ecf1042c3e      1
     28 0x30eedd4ea7e1959fc9d6879cd26f1d8baabbc5f6      1
     29 0x334cb32bc18ec632ab04ee55157aae1ff165e093      1
     30 0x346aaf2e75cfccb82cff0fcb8d2cdc21d5656358      1
     31 0x387f802c6a9ecffc8b8f6bb7b80ea0db4cef9810      1
     32 0x39e6d2b65fcfa0b3b2d74973b9eb67b6d68990bd      1
     33 0x3a10fd1387555cd75db8142393fbaba98efe28d4      1
     34 0x3ae8d9ba72848bf5356f8cc56450532c9bbaf9fc      1
     35 0x3d20c3f372207074df1a0ab5272c7168ce23d178      1
     36 0x3d3af253b037d3b22c4c810673c5d14de16d1af3      1
     37 0x40e6d6e798dc878e12987ed978f23c2391f1f570      1
     38 0x411dd34f17d5b6398f155f768ed27c32ad194862      1
     39 0x4269aadfd043b58cba893bfe6029c9895c25cb61      1
     40 0x42e5abe19ba467b0b32a4d042d3dfd4ba2e196af      1
     41 0x43b9778940c53abf2ca96ba900ff181ec09d7fcc      1
     42 0x44f301b1de6c3fec0f8a8aea53311f5cca499904      1
     43 0x45360f55024132b3110166e1b327170daa2cc299      1
     44 0x462fcc4b3fc64e0dfd751424bc3b070d79efb300      1
     45 0x46abfa031be839b1599513887a27a403e8d6598d      1
     46 0x4705bc2775cee0fe266dc2a376010c0eb1bcb474      1
     47 0x4a0e071be98b9514aad70f5d2595a8ab23a7f053      1
     48 0x4ad5bf7218b6695630208560bb62a38016ed64b3      1
     49 0x4c26c796abcf3513807efa54d54c34405b84a36a      1
     50 0x52690f90740621f89f58521433e9b0921d626708      1
     51 0x526ef837f56eb025908ba91811a6ed0a3b32c560      1
     52 0x527bb834cc5c8ff730c673880e51372282b06e14      1
     53 0x53b2da71db2e266882a465d9aeeaae3e8beeb9ee      1
     54 0x542c5648d46462207fba8a4bd7d890b4154b722f      1
     55 0x571493b6bb0b99dda41e2da1774b8c9b5bc302af      1
     56 0x5c95598fe454882fec1139c3ed1284255b49c8b3      1
     57 0x5e0737b90f1db90eb4f423dec067fd6c06a958d0      1
     58 0x5feadf8ef647da7f07584b3d33fcd565b79359a4      1
     59 0x614a62584658d555cc28b3aabdb691e85e002c8b      1
     60 0x6232d7a6085d0ab8f885292078eeb723064a376b      1
     61 0x62af590649b7abe80f8c6dffbb92e0875b65e37c      1
     62 0x62c868061cddbe38e01e7a8295fdcec72c10ca06      1
     63 0x64f02c24bb510e27b5eaec705eaa840ff7b4df15      1
     64 0x67c6083d540696c6aa1cf31101d78780e0d6bb47      1
     65 0x68d45e76a87e9f1706e0d36b7815c8dbdb7e6aea      1
     66 0x692997405c1cebbc56ff3416fa0b59ab97fac696      1
     67 0x699990a8e7ada9e92c932d6e8fb365024fc74b43      1
     68 0x69cd43dd4ecf3a076b1b9d2cfd41589987129bc0      1
     69 0x6b2aa5fc4508ec023a8e355959eaccef5e3a250b      1
     70 0x6b6ae848f555f70944bc99d736fe29fcaecf8b23      1
     71 0x6ce52498047b8279ccc7c25b41c95cd482525e54      1
     72 0x6d35392121a6687fe19d6159601b17517f73d0ea      1
     73 0x70814fb113d5b2b22902b568b67ebef657d9d66d      1
     74 0x72cea5aaaabd1cc5232bd2117e5d21e83cbd0e51      1
     75 0x753c892626c4fa09d1aedc0e9b52bda97a4afa00      1
     76 0x75cf6e0dc5294bef8cb5a6fcd23dc9badbfc443e      1
     77 0x7729cfd6841ec449c20fa7d3970d2e0d035c856c      1
     78 0x78fc8aa7bb4ee047257a245ca590fac4ae4aa97b      1
     79 0x7b8a6e7777793614f27799199e32e80f03d18dbe      1
     80 0x8668681d2017c2322d7ba37070a8ea223ef3729c      1
     81 0x8856dcf8d1f9be083a25251b0149ad74f971b806      1
     82 0x886478d3cf9581b624cb35b5446693fc8a58b787      1
     83 0x88a6f6ac171e24be54d95dfa7ceff80ff185b346      1
     84 0x8bd9c4b5ea8f6f14c9b14d830ddb67f3720d77f6      1
     85 0x8d12c02671848b17c18322027a2578ea7afbb702      1
     86 0x8dcd5fa47d7ef22db41b2b66504923ccca5065a3      1
     87 0x90af376346ca97d4e9d1e510e82543ef99b56a28      1
     88 0x9233389b9afa0a6e3bb403f9d211c23a3b290d69      1
     89 0x94f052ca65a34d7f581bba95a749b1cf52689dd5      1
     90 0x956f7db2fdb52de9453ae184d51d1883b8cd9d68      1
     91 0x96de627be6262ad2e19553824aad1af6ba1ebe9b      1
     92 0x97a45305513133c62b2e3a1f2181dfb5ffdbb450      1
     93 0x993220d8cc0e48e7b88c96ec7f37cc3b6e6009ce      1
     94 0x9ad53944b52e271f1114028b918202cfaaec0a3d      1
     95 0x9e23d2db65a1b8b6f31fb57c47148907545b3ff5      1
     96 0x9e32560cbc32e1ea4c737d65f043a0d8f8310422      1
     97 0x9f35af4727fb91114563d8a8f779a792cb557f3f      1
     98 0x9f4fe19fed4a008a270840fe95660f25df36c734      1
     99 0x9feba45425c51b03890604d6ad51f4ee7c9d4866      1
    100 0xa10bb823fb55a9199e4f521d0a88993c4cba7150      1
    101 0xa1fbc0a9ff967ce51b8ea952b35ff7005cd4cc63      1
    102 0xa21e1e6c69dfed02b6a69c4c89ea46a1e6e31b86      1
    103 0xa56c04347abee42f663eff9bc2d0147b97c8f782      1
    104 0xa62da2ea9f5bb03a58174060535ae32131973178      1
    105 0xa749f12e124790d45e62bcd3f7bf1d2218f2f21e      1
    106 0xa8806ccb79069c976e37ed1950ada0eacef6a289      1
    107 0xa8879c580a54f190ed53b43d30de269097ad7543      1
    108 0xa94ba6f81ede0d81a5252e71916b5c5567ad6a01      1
    109 0xa97204eb6b0f3e58e8acff48a2317511e7628949      1
    110 0xaa1d3f5d45333e40467e989d472effac4da00da9      1
    111 0xab3ba2d668215acd23f7da923683a5ed88bad625      1
    112 0xac02b3653a6fd4d507df142a0337951e39c5619b      1
    113 0xacb1568482f9af61568d5fd05a54bfe8b7e60ee3      1
    114 0xad43d2aea785e775bd38b5bbf4c5808572758373      1
    115 0xae20574304b6264d3129dbfe5df72eead8c5b208      1
    116 0xb1c11a879c47debc22e3816e7b727fc8bbe3c8ac      1
    117 0xb356ab415aafa4b688a6c457e61a4a8f7d5096f0      1
    118 0xb3b16685ca2d2a764882e2bb2a8d3d769cd74145      1
    119 0xb5976803fe7b4fa10834e98433f38ba208c53c0c      1
    120 0xb67a577a1855dc3a43df1d94b05a8f5d29570f89      1
    121 0xb735af7ae1a77d1ec764c862c2c09bdbf2b34b27      1
    122 0xb775df51776d6767f8c2f5a14b667e300f60447f      1
    123 0xb976163a6f251c208bcf3196b9c4ddf927b63e72      1
    124 0xb98af149b33dee33c97650e877a497bd6a934d54      1
    125 0xb9ad9d091c6841e640dba4cab02baefaf1134cfd      1
    126 0xbbc6e7b905d7794a2318e22ec011bf36b09e1d2b      1
    127 0xbbdd72fcce73c2626719be00259ddffef0d5673d      1
    128 0xbc2ab46887142d546a976d56eb9a3d9da147ee65      1
    129 0xbc9ca5bd0f07700929f8d538233b0a9e60f4ddc5      1
    130 0xbf270918afe2ad16093ddce904fc358ad337cefa      1
    131 0xbfe384d79969bcb583b8a0e5fedc314aee480e7e      1
    132 0xc04208f289d3842ac168f2c373b3164e1d872650      1
    133 0xc1b0307ff325527511310c7d6fd3248188742f86      1
    134 0xc40d07925d1ac5500e9dfc4e0a05eb24a75d195e      1
    135 0xc457fee0564bf3eb972114762f62ab45c0e3590b      1
    136 0xc522289168311a765cf17c067f0118578c99cf08      1
    137 0xc7ec028c706ec2fcfdcf66bba7e80ded634f6ca9      1
    138 0xc8cb180415b83f3f9a61691d246fe39a74c6a41e      1
    139 0xc8d3e10a72d2d846ebd134dfbd23e14cc26a4ed5      1
    140 0xca339bd4739227f71b646f8b23ca098c86f6c3a5      1
    141 0xca6b710cbef9ffe90d0ab865b76d6e6bba4db5f9      1
    142 0xcb85cf75d53fceb37af86b475903817db2b99d17      1
    143 0xcd69150ece65cf880eaa7b91ca6fbb7d38e97cc3      1
    144 0xcea266acb92d5603dc53b5d6bc6e568dcde0d311      1
    145 0xd02cf646520827d74d4ff0435f96220819772b4b      1
    146 0xd193a7d7b231c204b76b9d638768ea602de515f6      1
    147 0xd2ce17b0566df31f8020700fbda6521d28d98c22      1
    148 0xd74e767c77d2e9f9e467e7914f2379da81b63a44      1
    149 0xd981a38d80de1c1528349b841833d3a9a745cb37      1
    150 0xddc27a0a0c24d60b607dbc7b008f2016585e57bf      1
    151 0xdde27b3ba448e15357183060ae006bb54ebb3d86      1
    152 0xe2b76c9f15a1e5841a91b404ab4be1c3e5d19551      1
    153 0xe776de32c9dc8889e2dcf1cf0606010a51d1abb2      1
    154 0xe96ba1a10f72b58934b9ac0e3fb471d2ba65b757      1
    155 0xebe4c00073f1061ad27859478adcb4f5bc8ca67c      1
    156 0xed690606f908361c26bd2b1ad4fa31ed201dca4c      1
    157 0xf12e159643edeeba920518cc614820ab5726335e      1
    158 0xf1a17ba0d48798a3cc2bab1fb3cac942c4d6817b      1
    159 0xf3998eced96ae3cfe880ac9d2637b4118da48e22      1
    160 0xf4df143e90c2095e173f861a22f1e6a2503d06cb      1
    161 0xf6109779752d1fdb8b6063a158bed1b02000b700      1
    162 0xf620422e1e2cbf123386ea664f536d1016e5a5d0      1
    163 0xfa69519696e9d4abdf7d054c3ba44d32fe350ead      1
    164 0xfa977a0125c16a658a59ca1e59c7c48d58142226      1
    165 0xfd17019d6a7ddc7ad585afa68dbef71084162601      1
    166 0xfe7ace0f186a54c0be46f992dd3072e0053a1010      1
    167 0xff4f0dbc9ee7583c0c61c7333a5daf8d0ab4aeb3      1
    168 0xff7e887f2b7c7594e601ed1c2e44b55bb80f774b      1

## All Memes

``` r
c(airdrop_memes_all, airdrop_memes_szn2,allow_memes_1_phase1,allow_memes_2_phase1,allow_memes_3_phase1,allow_memes_4_phase1,allow_memes_phase2) %>%
tally() %T>%
readr::write_csv(file="all_memes.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 609 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x0064f02799ea7748a9b51b5e78bcd274d9e7d0a1      1
      2 0x017347cb75ac8725608e593ea35d83f9b2b3cfb8      1
      3 0x0216ac50fdb6b46f6b74254b84ecdd1d431d670c      1
      4 0x02b294ed96e1175b8c71071a303e8a7d2396b017      1
      5 0x03024d05b1aea10b64ff3af1fed6a3df9adeebb7      1
      6 0x039649f7c2f548692184da3fedf316f58e8356c0      1
      7 0x03c5e3ff9514031f910c9bc9ebbc7839e02b23d8      1
      8 0x03f4cb9e297ea659f30e09341ee7155a7d136398      1
      9 0x04294157cfba9ff0892f48f8345ea3539995f449      1
     10 0x047c4d7af709ae19d3bbbd5f9e10461d2554da00      1
     11 0x04d553053e186e4d58861a5bf3b7075fd904c6d0      1
     12 0x04df8d02f912d34fef12a1b0488ee56fd6f7416c      1
     13 0x05a13dcf55ea6d532f15284f39e02811fc183a8a      1
     14 0x06cc21aeea76cebb12da254b6e982516662bbca4      1
     15 0x06e13bd0a3cba08e61028f2326b9ea2ca2539900      1
     16 0x076097cf460e3b5fc3a94c43d9a598164bb29168      1
     17 0x07e92b07f74a3d021534e2f0382a9054411b8b2e      1
     18 0x0843e80240f9304f3fe7059f25f3e1a66886a0eb      1
     19 0x084c2c02a1448dc79692d8908648cca6d5e0b1d1      1
     20 0x0936d312ba234451fa7a90f68c7ede0f4fa962c6      1
     21 0x09b2f0988b6d24336b348c84c2764d2b4f85142f      1
     22 0x0a7669015fef82f5136c9a2e44e1ecbd2a5aec19      1
     23 0x0a87cbf5eec20d8d9880ad21f0260522ff5a675a      1
     24 0x0acabdaceab63c08bc5aea63d7e07da7c2e42959      1
     25 0x0c481cd7d09bdd222351d52622d272367cb3d159      1
     26 0x0c4a911ed6af1032beec7c19225c91a3160e39e3      1
     27 0x0c5c2e6a4ce269106ba9ae1ea397b02b9643111d      1
     28 0x0c6306c1ff8f0ca8432b761085d5ce74160a499a      1
     29 0x0cb5b9eeed8d51ae57e70e46b2a3596b94ece6c7      1
     30 0x0d6b4f304b91a3b67b1db34776ef7e95effbc729      1
     31 0x0da4438a9cc8d41fe9c0c6efd302a173ed967da7      1
     32 0x0dd38657d7c8024e7d62bde3d0423ca34769be50      1
     33 0x0e3e001a00c07e2cb3219d1ec0c47cc532d87804      1
     34 0x0e5c3e22b7239cef46f56741ff66369f3cfecb77      1
     35 0x0e81a8a496f9cb7e1d29382e91f06a6f7416cb9a      1
     36 0x0ea8767de750442ec4a34d81bffcfd4417396f47      1
     37 0x0f9ab846449ca068f9714818a6ed0da5d819e9e4      1
     38 0x0fb7f9eb8e9bee4dd70dc334b94dee2621a32fb3      1
     39 0x100f28c9b8b1fda897eadc775caf74e0f3560f73      1
     40 0x111818a51c4177e8980566beea68fe334be7b76a      1
     41 0x111b863c2f7d1833d8f53830647c260169e99626      1
     42 0x11924c505ecab6a2434bdfe94bc0ff1ca081facb      1
     43 0x11a22b262e505d355f975e1e48a365b5d4811ae0      1
     44 0x11e964bdb0504ff35dee22862da8f72603ab4289      1
     45 0x128ad42b82c752c5b4c7f679231d8956c98038eb      1
     46 0x12cd871105b29760a61f56753d54a59381f5bcc6      1
     47 0x1354eeed19ff645a63d6bf46fcf85c1c62d8dffe      1
     48 0x13928eb9a86c8278a45b6ff2935c7730b58ac675      1
     49 0x13eee41d67b8d99e11174161d72cf8ccd194458c      1
     50 0x142cc94ba3b7ea0d77d06cfc3fa40aedc186f601      1
     51 0x144bf0aaf16debadcd2638f572ebe42a92f5bcc0      1
     52 0x144c704bf25f1865e4b24fd6596ffed7d92470b0      1
     53 0x147f466fef6bd04617fc0c8037ea01c73bc25a3f      1
     54 0x14abeea8db07c46b37a79bfa856d4c2da6d3e6df      1
     55 0x14b27e03a394cadec25b2b20573e1ce88f482e91      1
     56 0x159e5998669ec6628a6b670c2ef1ddbd93084698      1
     57 0x164a45abedabb674c031f7bbb0226cdd06fe41a7      1
     58 0x167a4e6066d6c96d5c012006f5cffc9f606131ec      1
     59 0x16b5ec9e34bd0a916822d6785b1c600ac911d6dd      1
     60 0x16f3d833bb91aebb5066884501242d8b3c3b5e61      1
     61 0x177a8340bddc2041cc362aabf543dee4e394e3e3      1
     62 0x17e31bf839acb700e0f584797574a2c1fde46d0b      1
     63 0x18412b9f84df461bdc0f2e0709df507d5bdb6363      1
     64 0x185952b3bb31da0ae18354bbb90ae40adc840c33      1
     65 0x18595f049a9c5c2019efda113009b0ec1c89ceaa      1
     66 0x19e7a132bd4b42f580f546197f42e19c42cdfe6c      1
     67 0x19f605f48cb963115f5d8f8d9e8ec1890877745b      1
     68 0x1a4370fdd7173d0a41ff7c63a8e0249479ba0225      1
     69 0x1a6f0dddb884854355023a2dfe26e9174f8e0290      1
     70 0x1a7d7e6abea492e7b9a3cae667f468c72e884671      1
     71 0x1a7f174dcd4211d41c7c4d7dbb3a0e4db5780c67      1
     72 0x1acb5e5c169aed4b9f963bdf786a8377bff678a1      1
     73 0x1b6265a40839a331ace2d81bc82b6104703c0426      1
     74 0x1be1eac561b34c9b5e45eba9c3fb51fc5fc2796e      1
     75 0x1c172d05b75178fc669d74407243cc932030f139      1
     76 0x1ce68e5912ef8cb4e83620a50b70edc900ad4759      1
     77 0x1d10b0166a761f269adf864ebdaca9be445ca637      1
     78 0x1dcfdcb24456a7dba5342479cff0f9ff30145fb8      1
     79 0x1ecded4519a9688ef4bfb5f15811c52f048ba1a6      1
     80 0x1ffc29a768e26ab393ea93e4284773410a84b660      1
     81 0x20163ec2afd9698433a031b0a7dcd70ed77ba8c7      1
     82 0x20542dffc2f7a51cbc087f54fcafd9ffd8b5c899      1
     83 0x20ebf9ae7c66da763fc1ce86062ce883389c8e23      1
     84 0x215cbb1b60e2cc1f1e8626cadca386b278aa0dee      1
     85 0x2177da04f9479496c2292d6344d306aa49beb34a      1
     86 0x21b9c7556376efdbf9ee316a4ede0c8257fb7533      1
     87 0x21f023839e7b6fee67d33e4548791fa388564a02      1
     88 0x220da3fc3b905c968dfb20c81c170bc4dce56603      1
     89 0x22418183c90741bab6606c0e0d240ae6c3a148f0      1
     90 0x22e8efe40ddb7f13b17b4c10f768967fc7a9f875      1
     91 0x22fbaa2dd20b848084545515fb04b9c846942779      1
     92 0x23602ca06e977c86339ffddad74966e824ab691e      1
     93 0x23ae72f8336aca747ef02d596403de56cca489fb      1
     94 0x248458947c120ca057ec028b3fe7e4b3f26fdb3d      1
     95 0x266b2d881291c5bb36cf386d16e5f8ebb8e19a71      1
     96 0x27bb5366ef655819337d6ffd29a55905608c853b      1
     97 0x2801dc73a6dcefb364b959606e0c61234105fd5a      1
     98 0x28e6c1352950238be088ef2a741f3c8b91b9ffad      1
     99 0x2917e947fb1925abe184d4062520a11bcf2a5b36      1
    100 0x29c2188c6c318ab5cae4ae4b11a49edf2ec9ab0e      1
    101 0x29cd8d94a4e564a6d6576b467ea0bb9d51c9f05e      1
    102 0x2a0a412e0a0d436cca7ddba177c4dd9f29801062      1
    103 0x2a462fb59a5b0885b60ad31c5fe473d5ba0d79fd      1
    104 0x2afc032aaf6d7ead33fb05f9d42d874874dfb9d1      1
    105 0x2be96c05fb7e81b020c42086b69d21bbf427b53a      1
    106 0x2c45f65d254e85584b47ec82450c91a30cfc6be1      1
    107 0x2c8875f34ceb219f61b7453b2c5f100ec2f6ed33      1
    108 0x2c9646bf2477a747897047b69ca1a1913ae5f611      1
    109 0x2ca75a892b59bc7cf863ba261e49ab40a4844ee3      1
    110 0x2cac9e6d1d630a35cb8019779daf4c5fd96135ca      1
    111 0x2cfdaf06e39e5d9d3c9afe20362b725b41f68725      1
    112 0x2d1543711bd262108fd8a1afb43d8871f76c134c      1
    113 0x2d8b81cde837334beb89a9c0a88179ad234770be      1
    114 0x2d924c0bc8033e7866b90ca2a76cbf4b5714b11b      1
    115 0x2db89e03296c0f69921ab5d24ad9cdba44a7ab49      1
    116 0x2ec4a2bcd4f33c7c9aafab7cfa865ec15508bf62      1
    117 0x2f2242b447a54cb110b8a7991cf8a27054ee6921      1
    118 0x301be4f5965cc6d386a3aad84f1344ecf1042c3e      1
    119 0x301e2d2a98c5873ac27fd9eae85f0153959100fa      1
    120 0x30eedd4ea7e1959fc9d6879cd26f1d8baabbc5f6      1
    121 0x31231036ed51376a70b3824d7da1a4e036def5a4      1
    122 0x313c8673ff69dfd7be2a46bc540305d3e2d233b0      1
    123 0x32a0d6b013cf8ecad1e37e99532570411d398d05      1
    124 0x32e263cb9003f7fef329080bdcbc82f5cfd6c02f      1
    125 0x32ffe815277ff53dd2a73557664e229899e6501e      1
    126 0x334cb32bc18ec632ab04ee55157aae1ff165e093      1
    127 0x343e85ac3009ac01538d4251900ce1a2b7d7ffec      1
    128 0x346aaf2e75cfccb82cff0fcb8d2cdc21d5656358      1
    129 0x34e03545e5527f54243ce5a4f88ebb202efb0ae3      1
    130 0x35d7dd9230a1f079c907b53e7c5822b34e197a1d      1
    131 0x367dc97068ab54ba1dfbfc0fad12fbcb7b3a0d09      1
    132 0x36d89738405a8325b7c560cab2dd1b88565effd3      1
    133 0x378dcff8e6be1778b04f5712d686517bb6a01927      1
    134 0x37c14f24c73a9e151575a0af3dd2dd5cbbca8a29      1
    135 0x37ea6c993c5f42e1fc3455d5781e4a580760970a      1
    136 0x381e6beb053a494b7520eeb4f6e26d4ccb503a05      1
    137 0x3876be5be4998adecbfbbad26604a762467e7f42      1
    138 0x387f802c6a9ecffc8b8f6bb7b80ea0db4cef9810      1
    139 0x388160a99390392278afdba240046b8b5e73f77b      1
    140 0x388cbeec5a031e4aad12599b04a4aa4f088e0f73      1
    141 0x39e6d2b65fcfa0b3b2d74973b9eb67b6d68990bd      1
    142 0x39f1165e037bcc35c164e744d4a2667dc8135114      1
    143 0x3a10fd1387555cd75db8142393fbaba98efe28d4      1
    144 0x3ae8d9ba72848bf5356f8cc56450532c9bbaf9fc      1
    145 0x3bc161e3a5604165f3165ed8aaaf0d490e559324      1
    146 0x3cb63b82d778105e43f064ed739b0655f1f0fb87      1
    147 0x3d20c3f372207074df1a0ab5272c7168ce23d178      1
    148 0x3d3af253b037d3b22c4c810673c5d14de16d1af3      1
    149 0x3d91ad906c3969f86c588c988f721d5a735c6f81      1
    150 0x3df7131fbd4f6b27be8317c69e41817498e1bc0c      1
    151 0x3f4373afdde3d7de3ac433acc7de685338c3980e      1
    152 0x3fa5a25f48ba1b736761706801be4f639ca4853e      1
    153 0x405020c797a64f155c9966c88e5c677b2dbca5ab      1
    154 0x40e6d6e798dc878e12987ed978f23c2391f1f570      1
    155 0x410a9f44948576c60f94b165f63eb79ce769a04c      1
    156 0x411dd34f17d5b6398f155f768ed27c32ad194862      1
    157 0x41254fe73d3b49f0fe3cc7743b4d40ef26bb338e      1
    158 0x417c269b82387ab605bdfbd312d91baf03dc8516      1
    159 0x41d3ea0d6a64d603cb8c3ff710f05f546061cdde      1
    160 0x420ecd4ec65c0fea87d8dc5d16c2476c42466b65      1
    161 0x4220132c9df1ab7bd2913f0fd03297c90e7cc6fe      1
    162 0x4269aadfd043b58cba893bfe6029c9895c25cb61      1
    163 0x429683282e5a28b81aab1f9126b8d5e4847dfbeb      1
    164 0x42d38ed60a64d0b8c36f190e185d17db3617a091      1
    165 0x42e5abe19ba467b0b32a4d042d3dfd4ba2e196af      1
    166 0x431181dae361813567f35ee2abac73291820fcc9      1
    167 0x435a4ccc3712b0b40ce16a1c7bfa263ed6e9bb33      1
    168 0x43b0bd27d5016e969193412933387b0dd4cf3e0a      1
    169 0x43b9778940c53abf2ca96ba900ff181ec09d7fcc      1
    170 0x44f301b1de6c3fec0f8a8aea53311f5cca499904      1
    171 0x45360f55024132b3110166e1b327170daa2cc299      1
    172 0x462fcc4b3fc64e0dfd751424bc3b070d79efb300      1
    173 0x46abfa031be839b1599513887a27a403e8d6598d      1
    174 0x4705bc2775cee0fe266dc2a376010c0eb1bcb474      1
    175 0x47d539d4dd9b6f21ccabc5c96bbbf7071290938e      1
    176 0x47dc3a7aec5c0e1f4b2c71303a4b1eaa0bee3e4c      1
    177 0x47e64ae528b2d1320bae6282044d240ff67e703e      1
    178 0x48238faa9bd6d56fb44a32dac6185f04eabf2117      1
    179 0x48464efe55fbcae8ae0c992b306afcf21d4910cf      1
    180 0x487869b7557f0fdecbc5141da1d7c48c15c5b8eb      1
    181 0x4a0e071be98b9514aad70f5d2595a8ab23a7f053      1
    182 0x4a6d345c287e3c315eafd0e6a4274a74c6cb9af3      1
    183 0x4ad5bf7218b6695630208560bb62a38016ed64b3      1
    184 0x4b3b0a6c0bacb088b6b0b0ba31bab77282068117      1
    185 0x4b6c1d196e06446eb0534326bbfb02cc3d073a2b      1
    186 0x4c26c796abcf3513807efa54d54c34405b84a36a      1
    187 0x4c48d00997c6d23d6b47a9ce8caa6027d0068e42      1
    188 0x4c941a23ec94580d3b5442aa8d5851583fd8bcce      1
    189 0x4d7e6948fb5ed2d92897de0605cd642eff69fbf4      1
    190 0x4dd8f136fe8a8202cd6395ad34ef5ae49c26e78b      1
    191 0x4e75ec3796da954c7f94d98f5c7808ee95281504      1
    192 0x4f492b2a3ed9bf0c65d508dab4f7c5f5c04ca6c3      1
    193 0x506452ab0dacf04db0ab1544b6be4019651ada90      1
    194 0x511c85042524a41bbf30e7a8533a84b8c5e55f43      1
    195 0x51a2d3572577b6c47186c82165ab8a61e046dc83      1
    196 0x52690f90740621f89f58521433e9b0921d626708      1
    197 0x526ef837f56eb025908ba91811a6ed0a3b32c560      1
    198 0x527bb834cc5c8ff730c673880e51372282b06e14      1
    199 0x53006f95def268f88dc1b8216654ab56f3afd052      1
    200 0x53b2da71db2e266882a465d9aeeaae3e8beeb9ee      1
    201 0x53f1b3d8990472d1adcd7b76348e5508db17cea0      1
    202 0x541db1ed2628f2f4897417d06181af6a179e90d0      1
    203 0x542c5648d46462207fba8a4bd7d890b4154b722f      1
    204 0x54669ca58bf3a59caea8ae5135db491e4738f65a      1
    205 0x54913cc8ea17731d62589039dd0152f306473843      1
    206 0x54dcb363205d953d620809cd315640cc36b679e4      1
    207 0x557bd9e829f35f1d853c2c85341458892753fa1f      1
    208 0x557c60995797fa7b47be105227a2e46148d85750      1
    209 0x5663d7d0751db1a80f01d6974e02036b6d44d56a      1
    210 0x56b367a6303b3d626bfbabc625b8e0b5bd9452f8      1
    211 0x571493b6bb0b99dda41e2da1774b8c9b5bc302af      1
    212 0x58059014c5952c04370bcb88a2e0503e9eafb209      1
    213 0x59c1591ecd3689e911a49ebd406f9fb157171cef      1
    214 0x5ac9982211070c077213471cfbd496a7af34cb14      1
    215 0x5c3097e7fd3b97d9fdeec6d378884c892ff0545f      1
    216 0x5c95598fe454882fec1139c3ed1284255b49c8b3      1
    217 0x5d0d23ae81ebe1b6ac96411b2c7bc3bf7463fa25      1
    218 0x5d25087405105bab12624c73488ec186066a6376      1
    219 0x5dc97f41742524ecb3885ea3cc33439c100acdb3      1
    220 0x5de26d392ea46ffc17131042e2584fe6ba46206f      1
    221 0x5df5342342701b8ae5bce28f74ebb73b5fc13a54      1
    222 0x5e0737b90f1db90eb4f423dec067fd6c06a958d0      1
    223 0x5e3118e91b0a02a5e2b92e49700c63434e94f0b2      1
    224 0x5eefc3ae27be1483ad6b864b45ed72fc39e6bb6c      1
    225 0x5f656037e30a003862cf878db24ab5f537177fd9      1
    226 0x5feadf8ef647da7f07584b3d33fcd565b79359a4      1
    227 0x614a62584658d555cc28b3aabdb691e85e002c8b      1
    228 0x614b89f072ea263a9387460963142e73548fbaf1      1
    229 0x61cb53ba546c5264cbe9324843a25882cb82e1db      1
    230 0x6232d7a6085d0ab8f885292078eeb723064a376b      1
    231 0x62af590649b7abe80f8c6dffbb92e0875b65e37c      1
    232 0x62ba9de7d81b96d697fc2f26b5eb647f184f9b2e      1
    233 0x62c868061cddbe38e01e7a8295fdcec72c10ca06      1
    234 0x645fd16f3508391e8385fc0a181076994079aa1b      1
    235 0x64932f86d69f2717307f41b4c6b8198000583c63      1
    236 0x64ca4373a93e25822dcfdca0306e899a29e92b3c      1
    237 0x64f02c24bb510e27b5eaec705eaa840ff7b4df15      1
    238 0x66b280b5778c35c719209614428caddf00aaa3ce      1
    239 0x670a840848eba22eb2a090fd65e1ff60c3ba9e5b      1
    240 0x67c6083d540696c6aa1cf31101d78780e0d6bb47      1
    241 0x68d45e76a87e9f1706e0d36b7815c8dbdb7e6aea      1
    242 0x692997405c1cebbc56ff3416fa0b59ab97fac696      1
    243 0x692d6cf19e0c082185e20ff5ea6bb267e7aeb278      1
    244 0x699990a8e7ada9e92c932d6e8fb365024fc74b43      1
    245 0x69c8c2923005d26eaeea9500d7602eff8c81c848      1
    246 0x69cb3b1de24e08f1cfc2994171b6c6930498f750      1
    247 0x69cd43dd4ecf3a076b1b9d2cfd41589987129bc0      1
    248 0x69e68074f1aada957edd39c5eae0069973343f30      1
    249 0x6b29380bb9955d5b2df1624c5c6a72a5e69a6b80      1
    250 0x6b2aa5fc4508ec023a8e355959eaccef5e3a250b      1
    251 0x6b539c723716628725aee9e7e26796cfe1f38fcc      1
    252 0x6b6ae848f555f70944bc99d736fe29fcaecf8b23      1
    253 0x6b6d5c57533d22d0de14db59fcd28b9ea5296090      1
    254 0x6bae54650c0d17f4aee76839a7d319dc5060d6b0      1
    255 0x6ce52498047b8279ccc7c25b41c95cd482525e54      1
    256 0x6d1db4a7e83dae0eee7e95d421722d46d2a7e94b      1
    257 0x6d25c0fa070e625dac76b29bcf0917303cd52e7b      1
    258 0x6d35392121a6687fe19d6159601b17517f73d0ea      1
    259 0x6d8dec3fd68d94a7189a98346ea52b4d32e00012      1
    260 0x6e1510ed41ba743344e9ba80646194351cca0c7f      1
    261 0x6e2fbc139e37b43f4f29cc10b7bede36587f7306      1
    262 0x6e5632d334b861dfe4312c7ba34e175b60be0b5a      1
    263 0x6e88c16794cb212bdcfd6b6f8901af2f83237d3b      1
    264 0x6f566a6672f615c2975f6c48224c46153e12ffcf      1
    265 0x6fdd8c800cbb81790cd48a20f90ce7e725f89c27      1
    266 0x70814fb113d5b2b22902b568b67ebef657d9d66d      1
    267 0x70c8db61d09271f4c90950ba8c6cbaef918f12f2      1
    268 0x710852027bc25db82ba562d5b1499b6d9b02fa9c      1
    269 0x72703b554a7089f93ff1fc6cc6c0e623900a7b80      1
    270 0x729fcbb6e1289c88ce5113ec1c83a48a8e3c9f2f      1
    271 0x72cea5aaaabd1cc5232bd2117e5d21e83cbd0e51      1
    272 0x74d50ea9f8cf5652e75102c3fa049c01534dd6c4      1
    273 0x7525e71f51bda1fbc326000714d2fc68ed5aed6b      1
    274 0x753c892626c4fa09d1aedc0e9b52bda97a4afa00      1
    275 0x75cf6e0dc5294bef8cb5a6fcd23dc9badbfc443e      1
    276 0x7729cfd6841ec449c20fa7d3970d2e0d035c856c      1
    277 0x774a34da2ee2e2d242180819f1ee88783215f7b9      1
    278 0x78e37b881d078d4b2f90de0422dadfd4da50ed4f      1
    279 0x78fc8aa7bb4ee047257a245ca590fac4ae4aa97b      1
    280 0x791967551708fe99eb2cd8dd580ef98c61e67ac3      1
    281 0x7a231b6d33f1da74e9fb593bc785b4914a852867      1
    282 0x7ae3b0627dac32d3ee16c204ef791e37067b8460      1
    283 0x7af061a6ec9616135231cb8de2ed2a0a4140e90b      1
    284 0x7b8a6e7777793614f27799199e32e80f03d18dbe      1
    285 0x7c3018a930e2d0d060069ac1ec38dbf9619e20be      1
    286 0x7c741ed01ee259feba4b7f9cac114f48bcafacf3      1
    287 0x7c84aab9f3eb0897191d95afb2aeb56a63973532      1
    288 0x7ca00a09e3b431d48c30e3e6cceaaeaf6385cc81      1
    289 0x7cdbb8e309ce7c701ed200be123f16c6f7c9e525      1
    290 0x7cef7db4289005fd56dfa6c73bb2be6ec13bc250      1
    291 0x7dbba7f0551aef5894fd6ee331b95dbb4349f5d4      1
    292 0x7dd14501c25c221ffe213d7d56d225f4fe411038      1
    293 0x7e0051a0b48f3c5848e66fd02bea3400f979a89a      1
    294 0x7e5b399e254665590266ac6a9e2a1e3336576cc0      1
    295 0x7e737324cd6702e1b93c9240072ec9cc036dce21      1
    296 0x7ed69ff055d746d451f675635e9bf773accc6a97      1
    297 0x7eed3caf47728c4b22bf9b70e708b513b5350f55      1
    298 0x7f1796071eb91aebda10a03115fa3cd95efdb25f      1
    299 0x7fdab8c244e0b775edeeb9ebb8356b59068c6873      1
    300 0x7fdc3797c6c4e0683e40964d526d2eec89686cc8      1
    301 0x7feea2dc4c9537f81f26602ccc8d38b0169ab2d8      1
    302 0x8043812aea5a07dd6523b5d83abd5e606422944f      1
    303 0x808421753a181e96812796b7ab43d3f356cc5a77      1
    304 0x80a1c9fdc26199a69d190ebc8ad287ef48758977      1
    305 0x8120cd4fd4d867dae6dc1c2e9c504224ed045f34      1
    306 0x8179e48d959a4c41d5e0432ac05b30707149d649      1
    307 0x81974c1a09e53c5129c6e4f74e547fda0adf4d2d      1
    308 0x82139687faae8a29851902783e02e699de0e0846      1
    309 0x8215643e5757d44940586aa670c932bd703034f3      1
    310 0x82d32aaec5b77528d920843d84cfdf45e84ae9b4      1
    311 0x83ca0f19abd240c3b04bd55a2046c308d042e196      1
    312 0x843708d85621273f3bbc643b348da3a60d5b0334      1
    313 0x84aeccde4c9f217e83d3fa28c31d34378b903f91      1
    314 0x84df1fb4801e72cf6958a994dc0b690f9e7cb23b      1
    315 0x8586c32dbe1c986bffb8f1ed6c3e8c6489f4fa3c      1
    316 0x8623a32af48544b055fb6ae64f33eb43edf091ff      1
    317 0x862464bbd83d5743f9cf873c6ea3d187d0f909bf      1
    318 0x8668681d2017c2322d7ba37070a8ea223ef3729c      1
    319 0x86c71b7d27d00d7814a763f9e6f16fdb09f7e4df      1
    320 0x8774be790cb9e12d5edaf2eb8a3f6c89410a497d      1
    321 0x87e91205adab9e2b0be9f7562af3f5b42fad0838      1
    322 0x8854ad314fdbcc275eb0d5f4fd6dd86ab7981cce      1
    323 0x8854b06ba346a703a3c043e2e7b8822db3ca6b3a      1
    324 0x8856dcf8d1f9be083a25251b0149ad74f971b806      1
    325 0x886478d3cf9581b624cb35b5446693fc8a58b787      1
    326 0x8874174a2366668d54fea6343f71709389563c8a      1
    327 0x88a6f6ac171e24be54d95dfa7ceff80ff185b346      1
    328 0x88ccdfe9dd047b4cec4c1102d2d803e2d8bf683e      1
    329 0x8a063961e57a68a8a1f68930d758e1bde05fc6b3      1
    330 0x8af0b9a9b751e086122bc340188bd9d99b8c7ec1      1
    331 0x8b4567fa8c4715c27a682215a031033a8b544206      1
    332 0x8b4d0402f7b2f063f255214b7095b5911a257a30      1
    333 0x8bc3757a675be4c2e459813769992ec2c60baaaf      1
    334 0x8bd9c4b5ea8f6f14c9b14d830ddb67f3720d77f6      1
    335 0x8c1321c782c169aa93c7c211ed92956e84d2f3e8      1
    336 0x8d12c02671848b17c18322027a2578ea7afbb702      1
    337 0x8d5e59e11838cff72af5fb0681d96a9136ad0604      1
    338 0x8dcd5fa47d7ef22db41b2b66504923ccca5065a3      1
    339 0x8e68520b3ce0561da3542c624b77afff589216ce      1
    340 0x8f160eb907a80517a3fa6d22f7cf20f552696a44      1
    341 0x8f6504b48c66eea4e9fbf64fe220e02f1cc4934e      1
    342 0x8ffa8d00db5df56a20c30b737c5efdaabe140df4      1
    343 0x90af376346ca97d4e9d1e510e82543ef99b56a28      1
    344 0x90fa40f24362c24de09b1cb86df9f0b1f1b15a90      1
    345 0x9233389b9afa0a6e3bb403f9d211c23a3b290d69      1
    346 0x92b1b677728e36734d59067a4ce53e5bf1f3a2ab      1
    347 0x93243bf5f577c1e325987862267d0314f72e0050      1
    348 0x94f052ca65a34d7f581bba95a749b1cf52689dd5      1
    349 0x954d65e5310607e12f93f6156de92cd37ffcae8e      1
    350 0x956f7db2fdb52de9453ae184d51d1883b8cd9d68      1
    351 0x957143815f0e1d1b1a31b2bfcded5b416ee675ed      1
    352 0x96de627be6262ad2e19553824aad1af6ba1ebe9b      1
    353 0x96e861634b4c0e0a9e7c6a65dec549cc2a8a0e56      1
    354 0x97a45305513133c62b2e3a1f2181dfb5ffdbb450      1
    355 0x97d2b1b4a249ec77d56fd1576546996d14f7db1a      1
    356 0x97ece7185467c78293f3b796bde3704421d4fa69      1
    357 0x98004498c85c4b1997b3f669475a038bbcec2160      1
    358 0x993220d8cc0e48e7b88c96ec7f37cc3b6e6009ce      1
    359 0x9959dd45f8d5168cc556e8103b346590185e9616      1
    360 0x9a5d5f6d17b9d9093f1107dd6da934c292a601cc      1
    361 0x9ad53944b52e271f1114028b918202cfaaec0a3d      1
    362 0x9b742d1e98e5bb2f4d50f9fbbc047daf288ffc8b      1
    363 0x9b7c0814eb341e6a4ca673148e8b577b13e70787      1
    364 0x9caed8a01c87721570de895a9725334a0a0cdb70      1
    365 0x9e1e3857fb2484379858b9daf230379015a7a100      1
    366 0x9e1f7b007511036b5cbe5df6d39550bdd2e8bc99      1
    367 0x9e23d2db65a1b8b6f31fb57c47148907545b3ff5      1
    368 0x9e32560cbc32e1ea4c737d65f043a0d8f8310422      1
    369 0x9e5198418081fe00026ca0ddedbfd3915ab98298      1
    370 0x9e640badecb7c628c6188b74488823e879f42a1a      1
    371 0x9f35af4727fb91114563d8a8f779a792cb557f3f      1
    372 0x9f3c44a7331ca9da2d3432bcb0df91186a27e3d7      1
    373 0x9f4fe19fed4a008a270840fe95660f25df36c734      1
    374 0x9f6ae0370d74f0e591c64cec4a8ae0d627817014      1
    375 0x9feba45425c51b03890604d6ad51f4ee7c9d4866      1
    376 0xa04f4a4b7306cb72f30828834db01699362a4989      1
    377 0xa0f1de4882a5cd1851989d9a1e9bbe9b4604e9a9      1
    378 0xa10bb823fb55a9199e4f521d0a88993c4cba7150      1
    379 0xa1fbc0a9ff967ce51b8ea952b35ff7005cd4cc63      1
    380 0xa21e1e6c69dfed02b6a69c4c89ea46a1e6e31b86      1
    381 0xa32c38646299818ccedc6401818c2e1639c39c08      1
    382 0xa41341bab6588948da8503d1dfb0a0ab0ea269cb      1
    383 0xa490a0346808dda91aea6698cb19e4697d9fc5cc      1
    384 0xa4b61e227361a9cd9e62ca10946c27748a382cab      1
    385 0xa5214719feb1d07b66292a5d4d0a2712bd048435      1
    386 0xa56c04347abee42f663eff9bc2d0147b97c8f782      1
    387 0xa59422ec0634a0263bcdfce5e29dd2df3803447e      1
    388 0xa5ce27ca5e31b1de92db2d2bc109b3e23cf1d4c4      1
    389 0xa62da2ea9f5bb03a58174060535ae32131973178      1
    390 0xa6d4758ef49d2ae8e65e1a02216cb9744aee6b23      1
    391 0xa71000e72d38e6a84d7190f59fd3dfc73931c0e8      1
    392 0xa711818b11bdd5797042ade80e3a59687558a4e1      1
    393 0xa749f12e124790d45e62bcd3f7bf1d2218f2f21e      1
    394 0xa75dc095ed4ad69b088c3eb8ba2f93f1aa942b6f      1
    395 0xa7c342af335ea7e0747452ae2e3c1901498a9a76      1
    396 0xa817b9bd1ece360e4a1692894a5ad1d40b889f20      1
    397 0xa8806ccb79069c976e37ed1950ada0eacef6a289      1
    398 0xa8879c580a54f190ed53b43d30de269097ad7543      1
    399 0xa90aa01ec98f390cb86cd1846f7c0fd342a07955      1
    400 0xa94ba6f81ede0d81a5252e71916b5c5567ad6a01      1
    401 0xa97204eb6b0f3e58e8acff48a2317511e7628949      1
    402 0xa978eadb605761725d11d3b3a4045cf5859e2d3c      1
    403 0xaa1d3f5d45333e40467e989d472effac4da00da9      1
    404 0xaa2cfa73b429d1189a8d72d0d9a7b1557590eef4      1
    405 0xab2056903a7b62bac46f45a3d7a70ac799ca88cb      1
    406 0xab3ba2d668215acd23f7da923683a5ed88bad625      1
    407 0xac02b3653a6fd4d507df142a0337951e39c5619b      1
    408 0xac2318d4fbfb276b279f03d46942254b941bb345      1
    409 0xac6e4334a82b8434b0122bc3c9b42d0a5f190d4a      1
    410 0xac772daaa5079e005eec3e53314d6d1e9149dc87      1
    411 0xac9169946d4f6fea622900fa6aa1812d5500bade      1
    412 0xacb1568482f9af61568d5fd05a54bfe8b7e60ee3      1
    413 0xad102dacb54816ee8b128c63b83c7b5fc587f4d7      1
    414 0xad43d2aea785e775bd38b5bbf4c5808572758373      1
    415 0xadb630d5795b384a5ceae977543714b249aee808      1
    416 0xadebdeab678647a457743ea3af98f8b804e45c24      1
    417 0xae20574304b6264d3129dbfe5df72eead8c5b208      1
    418 0xaec3f7e735505c2b787b05d67f4adaefe3dfd678      1
    419 0xafb4fccf58bfc06ebc8ccd387b68140cca05f4c5      1
    420 0xb05b7ea3714a95857e45629def2b9c3577690208      1
    421 0xb08f95dbc639621dbaf48a472ae8fce0f6f56a6e      1
    422 0xb196931ec22517b0510705eb56d5652fe73877f0      1
    423 0xb1c11a879c47debc22e3816e7b727fc8bbe3c8ac      1
    424 0xb26c452e8ac20ae7445fcf46b6478d3984f5d8e9      1
    425 0xb2a18003067dd5f96ce5a51f276c01873c136e5c      1
    426 0xb31a4974499daad3255206daee7d1f3595fa259e      1
    427 0xb335326c7f2cd2e4eb09ce3d1745c92f66497b7e      1
    428 0xb356ab415aafa4b688a6c457e61a4a8f7d5096f0      1
    429 0xb37c5603fb0c6c2e2b62d0525affed9e1e5a6a19      1
    430 0xb3b16685ca2d2a764882e2bb2a8d3d769cd74145      1
    431 0xb42ab92f5af0f162fffefa2b1e12702ce3fc9e17      1
    432 0xb4627672ee52660a9e453ec541834e04583f3602      1
    433 0xb5976803fe7b4fa10834e98433f38ba208c53c0c      1
    434 0xb67a577a1855dc3a43df1d94b05a8f5d29570f89      1
    435 0xb692704d590e224c67fa96830c4b2526fccaf3a3      1
    436 0xb6c7e03ede87692deb1de7e40723b952e0fd3773      1
    437 0xb6cf25f5cf8a1e1727d988facdd47f1dfc492caf      1
    438 0xb735af7ae1a77d1ec764c862c2c09bdbf2b34b27      1
    439 0xb7546d1b04151e1e96d38275af66eaf984fdad2a      1
    440 0xb775df51776d6767f8c2f5a14b667e300f60447f      1
    441 0xb8700157128ff544c46cf35dec5a19120baa8eb2      1
    442 0xb8937891f08af9854a5ae7a5ec0cbaf4e68acd4c      1
    443 0xb8b8ce5e3d0ad5c0169e130c64048b6d3cfcd343      1
    444 0xb976163a6f251c208bcf3196b9c4ddf927b63e72      1
    445 0xb98af149b33dee33c97650e877a497bd6a934d54      1
    446 0xb9ad9d091c6841e640dba4cab02baefaf1134cfd      1
    447 0xba12fda058a14eb03c14613601c3a30d6f955196      1
    448 0xbbc37f68e9876d64b2c55016081528ae0a85d8b2      1
    449 0xbbc6e7b905d7794a2318e22ec011bf36b09e1d2b      1
    450 0xbbdd72fcce73c2626719be00259ddffef0d5673d      1
    451 0xbc2ab46887142d546a976d56eb9a3d9da147ee65      1
    452 0xbc9ca5bd0f07700929f8d538233b0a9e60f4ddc5      1
    453 0xbcaeb5ddadbb3ba2880b19d254e0cf5e23a68917      1
    454 0xbd3a5d12af9fd82fb927fd26216e4dc2722d2337      1
    455 0xbd751cac1fb27d930a6a0394d97c77908d94ad5d      1
    456 0xbe8fe12b9eb1ca2a593e6c070c71c294b6fe9f00      1
    457 0xbe9998830c38910ef83e85eb33c90dd301d5516e      1
    458 0xbf270918afe2ad16093ddce904fc358ad337cefa      1
    459 0xbfb7969e75ee4e58620f74135d167f5bdd60800a      1
    460 0xbfe384d79969bcb583b8a0e5fedc314aee480e7e      1
    461 0xc02e6b0d0c1a5d8cd26beeba0fe8d76c5d2f19b9      1
    462 0xc03e57b6ace9dd62c84a095e11e494e3c8fd4d42      1
    463 0xc04208f289d3842ac168f2c373b3164e1d872650      1
    464 0xc18d4c2fee42accfe7bfdce98bfacbabf76242e9      1
    465 0xc1966d218b7492dd9cd672a5a237cef4d82004e5      1
    466 0xc1b0307ff325527511310c7d6fd3248188742f86      1
    467 0xc29ded4c6ede7482068197c3e3205d850ffedb0f      1
    468 0xc2e8ed8cc0be70f91fc9aa903d5f4538719d7dec      1
    469 0xc3f1d077fa590018d3924ce8da1a085b4eae506d      1
    470 0xc40d07925d1ac5500e9dfc4e0a05eb24a75d195e      1
    471 0xc457fee0564bf3eb972114762f62ab45c0e3590b      1
    472 0xc45920062985116eaac6589058ed337066d6f2e6      1
    473 0xc4bfc1dc25827f39ad4a66bfc968456c7852a9a2      1
    474 0xc522289168311a765cf17c067f0118578c99cf08      1
    475 0xc58326c7020f26345f4568cc09daddf019a8e6d4      1
    476 0xc5a2c3a524be0792519655d4093b44b988db4f68      1
    477 0xc6411ff69f1ec6bfb4b082f47db61dbedcab250d      1
    478 0xc6cf5a020bcfc65c6f8181c04cbb5ef5050fe28e      1
    479 0xc78cd2e1e8ad4a288eddafb139c9d0891ad01ae7      1
    480 0xc7a295b1b2df1efb82aa204958b98ac30171cb85      1
    481 0xc7bb15c11595c877302ddfb330a4082d92f5bcd7      1
    482 0xc7d2d152dda8cf7a7860288c3db9a1a4ce29162b      1
    483 0xc7ec028c706ec2fcfdcf66bba7e80ded634f6ca9      1
    484 0xc8cb180415b83f3f9a61691d246fe39a74c6a41e      1
    485 0xc8d3e10a72d2d846ebd134dfbd23e14cc26a4ed5      1
    486 0xc94a284808e31b9ef43f9c9a48933e5a268cff9e      1
    487 0xc952ec84dd5d8ec32937a8e4105922c664564450      1
    488 0xc97958ff5370c56b7e0e6fb58437f00702714d49      1
    489 0xc9ae113b8451d0afc8a021d796322d545a7e7dbe      1
    490 0xc9aefca21844fdb3fe8f58d883f908991d757692      1
    491 0xca339bd4739227f71b646f8b23ca098c86f6c3a5      1
    492 0xca6983947efc848df45c9e84b279438e24727d2e      1
    493 0xca6b710cbef9ffe90d0ab865b76d6e6bba4db5f9      1
    494 0xcaa1c396e70384db58dd33be74b26fb119e98c3a      1
    495 0xcaf3365d474690a1ac6643d3d6ef44cb0c6deec4      1
    496 0xcb85b581caa71949784d2e826cf9df391c244b33      1
    497 0xcb85cf75d53fceb37af86b475903817db2b99d17      1
    498 0xcd30b6de9dadb0a1598eb12a4ccf34b98c98c1df      1
    499 0xcd69150ece65cf880eaa7b91ca6fbb7d38e97cc3      1
    500 0xce0f69eebfa13caef1f580eee21631963a90b332      1
    501 0xce8ad80ce1a979381d209ac230d02adafb9fa897      1
    502 0xce990032a3ed14e197421270a9bec9e276bf2f31      1
    503 0xcea266acb92d5603dc53b5d6bc6e568dcde0d311      1
    504 0xd02cf646520827d74d4ff0435f96220819772b4b      1
    505 0xd0b2d2d86a45bf2f13a3af83df6e7b5fe98050f9      1
    506 0xd0d9597369fc67b9eb2703d6e9ce5fac0a77f33b      1
    507 0xd193a7d7b231c204b76b9d638768ea602de515f6      1
    508 0xd1afbe4da2a5adc6faf30fcbea9e4eea0ba8c70a      1
    509 0xd1dd467f1630be10d67205d6c816429f8ee49124      1
    510 0xd2b9f9d1e6734f241b2a6eb64ef7f6e63af0a97d      1
    511 0xd2ce17b0566df31f8020700fbda6521d28d98c22      1
    512 0xd2f2a56affb67a9ae2499ab9df1689ee78fad89a      1
    513 0xd3069c9e486a8b77add55ae3ceb3a4b138fb76c7      1
    514 0xd3364ea1c742cc2d396ac65e1723b8d608a939bb      1
    515 0xd36590461162795ee33099b2076a0d4e017ae17c      1
    516 0xd3b37c6567e2702aa727cff4ce5939b4a07a2fde      1
    517 0xd4e791d799b8873d86ac06a5fedf2f1a75f639be      1
    518 0xd5ec003289265705727b622f1700fe814e54ca67      1
    519 0xd64110f209e3f77580b72a96ba9be29517e41221      1
    520 0xd6b2735e290ba3726d25c9762098cd26a16f023f      1
    521 0xd72c8dd33954e8aa53d5c108906b751ce4b2382b      1
    522 0xd74e767c77d2e9f9e467e7914f2379da81b63a44      1
    523 0xd7d941ff2890bda98f40a5dda0593d239a603f09      1
    524 0xd7e32b8a326ffd9e784a1ee1eea37684a7512171      1
    525 0xd905de91f928b20ea0ccf455e5ba07eb6373955c      1
    526 0xd939fe2e4776ccaddb823a26eacb45c66d8ad5e9      1
    527 0xd981a38d80de1c1528349b841833d3a9a745cb37      1
    528 0xdaac4e26f11223cc043dfd8e3efaf72333067339      1
    529 0xdc78107155918e230246439e4159fea4c477eae9      1
    530 0xdcefc9ff1e47e458e56959154c1cdcf56003d30b      1
    531 0xdd3adf8a817421dc7a7434c9cbb148601396ac44      1
    532 0xdd5cecf2835d465d490f79ad9773a848805a3219      1
    533 0xddc27a0a0c24d60b607dbc7b008f2016585e57bf      1
    534 0xdde27b3ba448e15357183060ae006bb54ebb3d86      1
    535 0xdded73b0bbdb3782a453c9202f54c746fd391068      1
    536 0xddfd836f7c9e42055b1f6ceb005fee4c7882f161      1
    537 0xde112b77a1a72c726d8c914c23dceaa53273c351      1
    538 0xde8328c4f5ceeb2c2b3e836b3354413161c81285      1
    539 0xdebd22d7f63648dfb69bb6e2a4c88264d578c0a4      1
    540 0xdf2e06c29cfcdb4f1231504d2116da7bad9395ee      1
    541 0xdf36b093cbc06522b68105c3d9141db188e980b6      1
    542 0xdffd6968900b3f4f667b69c5f9df65d9d7796a1c      1
    543 0xe05e90015f6577ea1b1eb682ff8e1db1b7b6bcce      1
    544 0xe1c22662f4f1320c6e5319d002b3c3d52f0d0135      1
    545 0xe25b24cebed3055236e369570a437a99e1d32602      1
    546 0xe2b76c9f15a1e5841a91b404ab4be1c3e5d19551      1
    547 0xe2d22dc1c2f7c58f86606e405325c69f5210a6a7      1
    548 0xe418a9a5e49dde0d13e1ef51d4bfb7fcc51c28df      1
    549 0xe60458f765bc61e78940c5a275e9523d1f049690      1
    550 0xe74abe3bc87a4cf34acfd7c548208700c0e669b0      1
    551 0xe74ca58ef724b94fac7f2e09e2e48536a5c1ad03      1
    552 0xe776de32c9dc8889e2dcf1cf0606010a51d1abb2      1
    553 0xe7c5ef2cee9c15b04fc7dea61a1ec443dd8e7fd1      1
    554 0xe7f31a4516dbebc0f443a8d4ac8b22d1a8f096e0      1
    555 0xe8091980f2190b5798c9758c1b7b6d84fd701b51      1
    556 0xe8a05c7f4e9c1aa060cf941dbb50381f342d7d43      1
    557 0xe8d471f152de648d750d041da4c040cfb2b95ecf      1
    558 0xe96ba1a10f72b58934b9ac0e3fb471d2ba65b757      1
    559 0xe96eb4507a1d162bbb99301fe592d310e9489e40      1
    560 0xea0ed16746257eb2bc11de2fefd63cdeece23a98      1
    561 0xea39c551834d07ee2ee87f1ceff843c308e089af      1
    562 0xeac5f9b3cd48123a69fe69ce93a7f58100a56552      1
    563 0xeadd2d51e75a8b42302adc2aff0157a4bf626adb      1
    564 0xebe4c00073f1061ad27859478adcb4f5bc8ca67c      1
    565 0xec47cbbd9e05f8c4d587eb604f41740b0b2f33e4      1
    566 0xecea65d22233e7beed752a06c46cefc6c90a1d7f      1
    567 0xed1e25178d7a6438c356d3459fc3efa9458ad52b      1
    568 0xed690606f908361c26bd2b1ad4fa31ed201dca4c      1
    569 0xee2a4c655ea792d92f12a3caea21188be6dea808      1
    570 0xee2c055f7706b9dfcd98cd5a23d5629d6316c0bd      1
    571 0xee3aa3fa5b780bc73da73a82d47fa2d8cdc1a647      1
    572 0xee91d62eb5aaea933efbfd0790613af5db305006      1
    573 0xeeb9148a32b45672e16591ee18ceabc773b099b5      1
    574 0xeecc9f94c674161e84ab0c60c020432ea8f88bc0      1
    575 0xeed489a100880c2d18361aa4d37b210788863c4d      1
    576 0xeeeb5dd03eaf4ab6b72949191d8ffcfe27f7d973      1
    577 0xef04cb0003ca57dc7bb563d5ea3248241b145bdb      1
    578 0xef15bd3f923245b24d64d6fbf57f036fe650c23a      1
    579 0xef5ab90a44b68d4f5e3f6be6af4bedb12cd2c66e      1
    580 0xf054274dd74987395d28136e53f39ef4f7b19994      1
    581 0xf12e159643edeeba920518cc614820ab5726335e      1
    582 0xf161ff39e19f605b2115afaeccbb3a112bbe4004      1
    583 0xf1a17ba0d48798a3cc2bab1fb3cac942c4d6817b      1
    584 0xf1f476f144df01480297dca47efca565e8b0c9f1      1
    585 0xf28fc8d0eb98a34f2b03d384f7db6f1e7b877a33      1
    586 0xf2c5f1fd977dbd6de9d04bc4e62dff722d4bb1a1      1
    587 0xf3998eced96ae3cfe880ac9d2637b4118da48e22      1
    588 0xf4141aef39803327497f6b81a21bb3f2acfa2436      1
    589 0xf4dde6ea673fb810b6f503bf441908bb30b929f8      1
    590 0xf4df143e90c2095e173f861a22f1e6a2503d06cb      1
    591 0xf6109779752d1fdb8b6063a158bed1b02000b700      1
    592 0xf620422e1e2cbf123386ea664f536d1016e5a5d0      1
    593 0xf624e9324f9b330cc0289775d7b91e945e881134      1
    594 0xf681efdcba35c69dc0fe56b7fdb56324f5abd385      1
    595 0xf6fb4e6cdd7ed50dabd90d10b1ddb60a85720737      1
    596 0xf84408070b4de16d6bb0ab2fd8b19d37e3fd1422      1
    597 0xf859068452f14ca012f3b2c9604bc8edf3283db9      1
    598 0xf8be957f65e67fb0342992a51c30290d5758f880      1
    599 0xfa69519696e9d4abdf7d054c3ba44d32fe350ead      1
    600 0xfa977a0125c16a658a59ca1e59c7c48d58142226      1
    601 0xfd17019d6a7ddc7ad585afa68dbef71084162601      1
    602 0xfd849d8cf365fe98bc60025ff7fab45cef0f557b      1
    603 0xfe7ace0f186a54c0be46f992dd3072e0053a1010      1
    604 0xfed52d251e31178ff8bf4a0d611721c544f74fc0      1
    605 0xfeea8258077cc06444679958185f4198dd4cd324      1
    606 0xff4f0dbc9ee7583c0c61c7333a5daf8d0ab4aeb3      1
    607 0xff7e887f2b7c7594e601ed1c2e44b55bb80f774b      1
    608 0xffe6832850476eb6d5ff184d747ed84f1b686aa9      1
    609 0xfff39900273ffb1045c7cfde62df1720b63fd6bd      1

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
