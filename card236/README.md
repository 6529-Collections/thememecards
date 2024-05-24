
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance         contract        
     Length:475         Length:475         Min.   : 1.000   Length:475        
     Class :character   Class :character   1st Qu.: 1.000   Class :character  
     Mode  :character   Mode  :character   Median : 1.000   Mode  :character  
                                           Mean   : 1.171                     
                                           3rd Qu.: 1.000                     
                                           Max.   :30.000                     
         name          
     Length:475        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 19894669 # https://etherscan.io/block/19894669
block_hash <- "0x780b514de68019150f7bfbf6eda04d03922625649544e4a20db8e2b53739f7be"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4510 

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

allow_artist1    <- pick(snapshot, contracts=c("SuperRare","WHATYOUREALLYWANT","HungryGhosts"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("SERVITORSEditions","InsomniaJittersEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("SuperRare","WHATYOUREALLYWANT","HungryGhosts","SERVITORSEditions","InsomniaJittersEditions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 17 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x01ad73aa5de7e7bce4cc80dc9bde0876efeb109e      1
     2 0x0ac30c6aaf2ffc01938d92aca03f71f913636134      1
     3 0x112531fc891a17c9a412c9f61d89b0a152a9b501      1
     4 0x162b2a0d3a1b006c89f2e5c7dde58d5de06b3854      1
     5 0x193bb5dcb851ef8f75dbbd6950c962c3a5f4b0e3      1
     6 0x1c0c531442d0a7c0ced2dbda9ea02f43a0c0ccc2      1
     7 0x1d1617acc8d03bfc2193c285d5d2a0a8a616895a      1
     8 0x1d4b9b250b1bd41daa35d94bf9204ec1b0494ee3      1
     9 0x461123fb3a23b5503c01afdb83b0e4387c98ed4b      1
    10 0x62b08bd6ff6b0028101d547ce40e8d9019e0c0fe      1
    11 0x7015853f4be641296f6c92690ff69fd36704dc2d      1
    12 0x7b2ad6af37be6a77c95556d9453e4877b6a8437d      1
    13 0xa353401738e7ec6a4db9c0ee4c66eb5252f681c4      1
    14 0xaf63db4e76a7cf1920232a748b65d5c9c8689fbb      1
    15 0xc47faf99100078461a63864f50f1315b6abbd5e8      1
    16 0xcb9cadd85c510ff5a4fb2d2bb6fb4b74ff3aee34      1
    17 0xd0a030f8f91eaec83a31afc793c736fe396cf0be      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 175 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x022b266d1aa39725a4e64aee661f0277461e4e39      1
      2 0x04c95c6f8ea7ff7c3f7a2f148fae75650c5a875e      1
      3 0x04d71347029a2a443eebc664ebee13d0e6764eb5      1
      4 0x0713c21be9feef553e7e172f50c6067c24dad249      1
      5 0x0804f1be8bbbf75ff17dcf9aef7c505c321b7303      1
      6 0x0830337ed8d752b520d5547aadeae4b8d4154199      1
      7 0x08bc9bbbea71d7a2ff435ab288c1ff60af9589a6      1
      8 0x0e8b82de86eb0d8dec29875d464a25a4fbdd0e8a      1
      9 0x0f5ad36cf3bb8e98cc185e7385ba063fe4cb2018      1
     10 0x116723a14c7ee8ac3647d37bb5e9f193c29b3489      1
     11 0x1201d9875f6114c74724159f4a35b9657f861e7e      1
     12 0x1288eb773f9ab5fd2368c870f02aaf97a7bfce9c      1
     13 0x12e7df22c809ac3fb4a4fcbda3b492ee9e5bf93d      1
     14 0x1324427e278bc9fc462aea1d7af289c3580be609      1
     15 0x1699f86a565f60fff567f23fc60bb1ef9385b16e      1
     16 0x189375ee21c2fbd04af8f1c71b05f11f157918a8      1
     17 0x19122007e1d9b120b167e714b53acdcd80c014f3      1
     18 0x193959c6cd6a6b36f150b173383260cfeefc59f6      1
     19 0x1ad6d0c6b76bb9feccd139c4920eb6a88e6d2486      1
     20 0x1b7690aebc8f986a780a19d74c5a9c293edb0d44      1
     21 0x1bb7b4f7d98a9d204186b36d3852e28f3286f577      1
     22 0x1be920ef263005d1ade89ee090782e80a2ec7a4f      1
     23 0x1c356f1e0eea78fae12c999c6a39d603573fdf81      1
     24 0x1d54cd3ab1b1a37eee51c2ae8680a3ec4f31f54b      1
     25 0x1df3381e0d11fd2684a7ea20dae4cca1bebcfe1d      1
     26 0x1e3b6cd74d67421f01c588ca0a22809dc34a4c13      1
     27 0x1f623ee324ca178941545f01bfccfea38cda2072      1
     28 0x200b72949bd3db3963f84dd4b8089f51d58d2541      1
     29 0x2063c384e0cc27d32375fad27111d026e86e111e      1
     30 0x2168b2295e912a27c0015bad9f09f090ebce1a99      1
     31 0x217aa3ca8d6755945c5702e1bb2bb3e73026c73b      1
     32 0x22024cb67ba812ac6ec84f8e1fe2241c9ece0375      1
     33 0x2206445d241ccb7dae93b2d2acfc67f75b90fd76      1
     34 0x22098012d15b4ff9a2b1da8d658dae8e5d0d3fbb      1
     35 0x250c3cbc55750a3850ff7846697d225b44ae824e      1
     36 0x2557ff3e05c43f41255a45aaa42b1ffbb35674dd      1
     37 0x264fb9ed5553017beaa709e6410364acd8f0ded0      1
     38 0x27b455c481d62f91a7b55d09cf2e3cd70858ef85      1
     39 0x2a74bd40df54a0065aeed3cd764ac37325938b8f      1
     40 0x2fb5bf565ca66786bf2c2b3f7f47b5e0c650768c      1
     41 0x30959097823e37e5aa5aa9839506edd57c45cfa3      1
     42 0x330bce303d27df0eb1b856da3464278db1db1ac5      1
     43 0x33e8d10459643faab56f1974291cb603b88a09f8      1
     44 0x3685ea73cd33d0d9815780b8fa8caada4aabfc51      1
     45 0x36c72892fcc72b52fa3b82ed3bb2a467d9079b9a      1
     46 0x36e10f7bb46a7e8a9000e20ac6bdc20721d15f1a      1
     47 0x389ccea690c98a9c2077442a90d796b9e5c067de      1
     48 0x393dbc2473cb3fb7426fdc14df243b2f59176408      1
     49 0x399ae621f85a6d4407904e94c0a3405be9bf4a1d      1
     50 0x3a661a18cb0258119636dfdde098648c6ad5ba62      1
     51 0x3b23b754a8f59c9adaf721302a40862e387aaa4c      1
     52 0x3c2eb6c5faffafa66a4869797ed385637a73abb6      1
     53 0x3d96b47d7743bbf9554d1a44353bac120920a03e      1
     54 0x3e0914fc2b3b67a476142020b7d6990d01ad234e      1
     55 0x41bd1e75ec5b56cedfa18a223de19a0a0768f1f5      1
     56 0x420ecd4ec65c0fea87d8dc5d16c2476c42466b65      1
     57 0x45576be0d36a5b9f5bccc60995746a71a27af625      1
     58 0x46b2bd5c888e6d57a839c559fd6076f2c15a8cb1      1
     59 0x46f7909e605fec492b05265c7c6ecb46e94675b4      1
     60 0x478bb542f7658d635abba67edb987806dff5b83d      1
     61 0x47c553e889f7bfa75cf3775473180a04e9effcfa      1
     62 0x48238faa9bd6d56fb44a32dac6185f04eabf2117      1
     63 0x48d26fbd89dda10c72cd8dc7b82b9eee0af6b48d      1
     64 0x4a95836eae95b286e20b3296aa49ae59712a0d87      1
     65 0x4c95915aa398c0e4aef6d7dd1ff5c0f9adbf9729      1
     66 0x4f8cdb39327588a2b11edcebc50cc1302aa87cfa      1
     67 0x55dc2a116bfe1b3eb345203460db08b6bb65d34f      1
     68 0x568a2e5713458a263da2b86cae2f6af0693799fd      1
     69 0x58f606e6a973e57165bb07057a75f047f42455a7      1
     70 0x5aa71418ac6238c78ea9a032a768dbbbf86bfeca      1
     71 0x6002ebe4144d7f3b0cd10e2ac0c0f357eb1c1c51      1
     72 0x618c7671366caaab6701f06ae9925ae682c6bf97      1
     73 0x650b4b1c2bb3573ce7c11b9baea0012b472b34f6      1
     74 0x6555f8fb6a02c9c73d55c72959a9e0cebff13489      1
     75 0x6596eca2c999f686423bf30e40ffdb55f6b6f4f0      1
     76 0x66035c088491c5ad0c021a423574a8271be8dc56      1
     77 0x66ff8f288c413e61c8ee807088ce301c03076e6e      1
     78 0x6811eaa638b04aedafeaddfca36c50ea303b1866      1
     79 0x692997405c1cebbc56ff3416fa0b59ab97fac696      1
     80 0x6954ee7ede83ca555663704fe7e2b1528ff2c619      1
     81 0x6ce9f49239e8adaf5bda1154262a3a054fad9db3      1
     82 0x6dce0c9dc00fb34b5e1e932e1d640884f5f1782a      1
     83 0x6e63a4caeccb4f341ee9c9175c9cc554bdb6d10b      1
     84 0x6e977c8155d914e7aa99331169939978a4d1b102      1
     85 0x70cc2cd17112b1107917131c2e6d939d0b6df194      1
     86 0x7304689aac83c3b236332b0c233878f1819ca89d      1
     87 0x746b46d87543eaf779144c8870888119d6ae0c2a      1
     88 0x75259686c1c967399eda0b4b16f16fb0990f9615      1
     89 0x753f8dab2ce1e6ec33e2967bcdd4974f60025319      1
     90 0x75aadbb7c36db057e5ac64183899f118de76df35      1
     91 0x762da606029d3120735aa1eec15464e265db7a3c      1
     92 0x76490db4ca34ec8b6d709026f679e5b1ecedb0e8      1
     93 0x788dd831546ea3b490bca5060ba3bd78a46ad9f4      1
     94 0x78db9fa7214765014732cd9b32947a96473e7133      1
     95 0x79e4b132776e182c7d2d4322833609cba1ee7956      1
     96 0x7b5b247c60796faffbbea317692486cd080cfb3d      1
     97 0x7c397135064965fe5c25e25b643163ec33bb8b06      1
     98 0x7dd5b92716542ab2442030df6d87cec437a51ded      1
     99 0x7fc53dfff734d7cb9f533154344e4d569a34f8e8      1
    100 0x7fd1d8e388b29ef0b8a00ca07e2266686a5559ee      1
    101 0x80870d798bafb17877d6e82f8a5681515e43c200      1
    102 0x819dfc1b757c0ed67fea97e33e70cc4cc640f99d      1
    103 0x832408210f74c5cd8ac6fd8ebc92bb6b8a7834a0      1
    104 0x872e5925d1fb3a63176129d9b576dd7c196a05ff      1
    105 0x87da297a832706b5115ee1feb47b7fdb7c476261      1
    106 0x8ba9de821d795dec74f736345a3047c3221aab2c      1
    107 0x8f54249ae4c8a73e92d44459e026c9197670f3fd      1
    108 0x8fc4e14c1c2ab50be594208b0e5f1a22056316d3      1
    109 0x90198c48129ac7564933f8bab85669e303ff6b9f      1
    110 0x9109c5b9a5030532b4f116e055d3b1f1e74654cd      1
    111 0x950752398e94462732a82c2e35d2ff789aff3288      1
    112 0x96e861634b4c0e0a9e7c6a65dec549cc2a8a0e56      1
    113 0x97064190190d4e2fb284a2d55dd53504e291c4cd      1
    114 0x973231cf471f768f0939042e0591da1b15f4799f      1
    115 0x973a2ace28745ce4715659c60ef70b9e4c044086      1
    116 0x98f1bda2d45ffaca73fcf2154e9e007871e14934      1
    117 0x9af7edc48bc8db5e4afd3af95df143ea29e2e2d6      1
    118 0x9b82d32d9a43944a40e8a961e967be6856a71627      1
    119 0xa3785d2f231808d54aa207a84aee81c85dcbfb21      1
    120 0xa8879c580a54f190ed53b43d30de269097ad7543      1
    121 0xa90dc6d356b622b9783f5020722e6f92b663438f      1
    122 0xacb0ca20d2031bf7b28fddf13709ed13db7361ac      1
    123 0xacb818130ac9cd4484848848a0ca3e7dda4bb265      1
    124 0xad8cb7dd25b6f814bc067149c43cde2ab8eded58      1
    125 0xb2c21980ddb10aeefa8ab10cf79a036aff89376e      1
    126 0xb4bb21f96a5f8ba3feb0dde8495a4e312dcc356d      1
    127 0xb893ae8a1824604f6df4dfde52e2754921ba1a73      1
    128 0xb99172161b5edfb4e01f7ee5feb4264d684541f1      1
    129 0xbaca88029d2b4c3e7e06af8e5d7df2e3ac8c46c9      1
    130 0xbaea3cf94abd0d6e0f029ef5b0e54e9424a72985      1
    131 0xbe65d45635a400d895f0a2682c4fd3122b10fddd      1
    132 0xbec5d88ef9491b661ef33a596eed6a4e5cdb00cb      1
    133 0xbfbe3d5dac1e55dff6b03602e87594579838f99f      1
    134 0xc2d742fdbb31442b843a098dcec3c20054c28986      1
    135 0xc79556a222367ecb3c2291d5c6d2b3e58e25fbf1      1
    136 0xcc47fcbfdd6e558fb39cf5ef4d92e59890476b86      1
    137 0xcd8b72710595c69019c109aef5b1b92eea7f995f      1
    138 0xce013eb11d874c8fa60ec9007626e154f33ef2c1      1
    139 0xce41e7bc61770344a3ced518b2c1627a9ed30f80      1
    140 0xd01fa4427a05fd7944557ae94b67d90109fad402      1
    141 0xd13e37b2bea300edda889157740fc2a0505b54a7      1
    142 0xd1afbe4da2a5adc6faf30fcbea9e4eea0ba8c70a      1
    143 0xd35fc346e15ba5b446917c9fd23a9471d6144701      1
    144 0xd42d52b709829926531c64a32f2713b4dc8ea6f6      1
    145 0xd76dfe29f0371fb0640906c699165b6bab33c522      1
    146 0xd809a687e957761872b440909348fa6547cafabf      1
    147 0xd81ce8e89dd987c8ab630858c8f1e9df14788c35      1
    148 0xd88c17aad286da44d7216f487b9a65ce1f7d4581      1
    149 0xdafcf1931a891fbb29a29c47ddee5ee76e24dc2f      1
    150 0xdb21ee947cba24dff405ed5ee33913af4c5f7c0b      1
    151 0xdba78d3d196029880fb35be407401311d6dfb3cb      1
    152 0xde0ea6059f96b4f455923c27671f066365b84a29      1
    153 0xde41df3d6734e920a420646a0151ede1be81a6ba      1
    154 0xdf38a8b16f9bb88083c1f1edad41c5323d60f8a8      1
    155 0xdf95a9ca4f6a7dfe2576ca99d96417a62deb2e0a      1
    156 0xe3b1dbf3e1824ec1a736e977bd5dbe53cda4548e      1
    157 0xe447dd94b12b61100565d28247765e7a5edfa895      1
    158 0xe6ca11a7b413866c6b9b284b5f3af3061aedf785      1
    159 0xe703f231ab056ecb99c92a1232cc1020acfc72f8      1
    160 0xea9fb10809015cfc7712590c71cf1d8f8cc45890      1
    161 0xeb43b5597e3bde0b0c03ee6731ba7c0247e1581e      1
    162 0xec6e5577ce4a3882049a4f93b0dec3bbb8b1cad9      1
    163 0xec9e512fe7e90134d8ca7295329ccb0a57c91ecb      1
    164 0xecbd845ea648f597775ed689673dcf81855dfc61      1
    165 0xf2439241881964006369c0e2377d45f3740f48a0      1
    166 0xf311b7b3751618d2762ac3ef62e8be61662c2ffa      1
    167 0xf31781b4811b737e66662997679fb07dacf63355      1
    168 0xf556b3d8304f6c484336641e64893be5e4c75f86      1
    169 0xf824ef230b0f7fc9038f9fdbc249717419219e77      1
    170 0xfa4e98a3973ac6d51918e69d49f749219518c5a5      1
    171 0xfaa8a175b8b0fd2ddfb31347282afcb53870ef1a      1
    172 0xfb1bb9d4a4c50457d339a686212a0716ea0adc80      1
    173 0xfcb89bf7d52241add4f474693f2210c5c832f188      1
    174 0xfea9b1760505fe0ac6ac48c30bc81c9d7431f554      1
    175 0xff559fc20d9b6d78e6e570d234b69c18142bb65e      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 192 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x01ad73aa5de7e7bce4cc80dc9bde0876efeb109e      1
      2 0x022b266d1aa39725a4e64aee661f0277461e4e39      1
      3 0x04c95c6f8ea7ff7c3f7a2f148fae75650c5a875e      1
      4 0x04d71347029a2a443eebc664ebee13d0e6764eb5      1
      5 0x0713c21be9feef553e7e172f50c6067c24dad249      1
      6 0x0804f1be8bbbf75ff17dcf9aef7c505c321b7303      1
      7 0x0830337ed8d752b520d5547aadeae4b8d4154199      1
      8 0x08bc9bbbea71d7a2ff435ab288c1ff60af9589a6      1
      9 0x0ac30c6aaf2ffc01938d92aca03f71f913636134      1
     10 0x0e8b82de86eb0d8dec29875d464a25a4fbdd0e8a      1
     11 0x0f5ad36cf3bb8e98cc185e7385ba063fe4cb2018      1
     12 0x112531fc891a17c9a412c9f61d89b0a152a9b501      1
     13 0x116723a14c7ee8ac3647d37bb5e9f193c29b3489      1
     14 0x1201d9875f6114c74724159f4a35b9657f861e7e      1
     15 0x1288eb773f9ab5fd2368c870f02aaf97a7bfce9c      1
     16 0x12e7df22c809ac3fb4a4fcbda3b492ee9e5bf93d      1
     17 0x1324427e278bc9fc462aea1d7af289c3580be609      1
     18 0x162b2a0d3a1b006c89f2e5c7dde58d5de06b3854      1
     19 0x1699f86a565f60fff567f23fc60bb1ef9385b16e      1
     20 0x189375ee21c2fbd04af8f1c71b05f11f157918a8      1
     21 0x19122007e1d9b120b167e714b53acdcd80c014f3      1
     22 0x193959c6cd6a6b36f150b173383260cfeefc59f6      1
     23 0x193bb5dcb851ef8f75dbbd6950c962c3a5f4b0e3      1
     24 0x1ad6d0c6b76bb9feccd139c4920eb6a88e6d2486      1
     25 0x1b7690aebc8f986a780a19d74c5a9c293edb0d44      1
     26 0x1bb7b4f7d98a9d204186b36d3852e28f3286f577      1
     27 0x1be920ef263005d1ade89ee090782e80a2ec7a4f      1
     28 0x1c0c531442d0a7c0ced2dbda9ea02f43a0c0ccc2      1
     29 0x1c356f1e0eea78fae12c999c6a39d603573fdf81      1
     30 0x1d1617acc8d03bfc2193c285d5d2a0a8a616895a      1
     31 0x1d4b9b250b1bd41daa35d94bf9204ec1b0494ee3      1
     32 0x1d54cd3ab1b1a37eee51c2ae8680a3ec4f31f54b      1
     33 0x1df3381e0d11fd2684a7ea20dae4cca1bebcfe1d      1
     34 0x1e3b6cd74d67421f01c588ca0a22809dc34a4c13      1
     35 0x1f623ee324ca178941545f01bfccfea38cda2072      1
     36 0x200b72949bd3db3963f84dd4b8089f51d58d2541      1
     37 0x2063c384e0cc27d32375fad27111d026e86e111e      1
     38 0x2168b2295e912a27c0015bad9f09f090ebce1a99      1
     39 0x217aa3ca8d6755945c5702e1bb2bb3e73026c73b      1
     40 0x22024cb67ba812ac6ec84f8e1fe2241c9ece0375      1
     41 0x2206445d241ccb7dae93b2d2acfc67f75b90fd76      1
     42 0x22098012d15b4ff9a2b1da8d658dae8e5d0d3fbb      1
     43 0x250c3cbc55750a3850ff7846697d225b44ae824e      1
     44 0x2557ff3e05c43f41255a45aaa42b1ffbb35674dd      1
     45 0x264fb9ed5553017beaa709e6410364acd8f0ded0      1
     46 0x27b455c481d62f91a7b55d09cf2e3cd70858ef85      1
     47 0x2a74bd40df54a0065aeed3cd764ac37325938b8f      1
     48 0x2fb5bf565ca66786bf2c2b3f7f47b5e0c650768c      1
     49 0x30959097823e37e5aa5aa9839506edd57c45cfa3      1
     50 0x330bce303d27df0eb1b856da3464278db1db1ac5      1
     51 0x33e8d10459643faab56f1974291cb603b88a09f8      1
     52 0x3685ea73cd33d0d9815780b8fa8caada4aabfc51      1
     53 0x36c72892fcc72b52fa3b82ed3bb2a467d9079b9a      1
     54 0x36e10f7bb46a7e8a9000e20ac6bdc20721d15f1a      1
     55 0x389ccea690c98a9c2077442a90d796b9e5c067de      1
     56 0x393dbc2473cb3fb7426fdc14df243b2f59176408      1
     57 0x399ae621f85a6d4407904e94c0a3405be9bf4a1d      1
     58 0x3a661a18cb0258119636dfdde098648c6ad5ba62      1
     59 0x3b23b754a8f59c9adaf721302a40862e387aaa4c      1
     60 0x3c2eb6c5faffafa66a4869797ed385637a73abb6      1
     61 0x3d96b47d7743bbf9554d1a44353bac120920a03e      1
     62 0x3e0914fc2b3b67a476142020b7d6990d01ad234e      1
     63 0x41bd1e75ec5b56cedfa18a223de19a0a0768f1f5      1
     64 0x420ecd4ec65c0fea87d8dc5d16c2476c42466b65      1
     65 0x45576be0d36a5b9f5bccc60995746a71a27af625      1
     66 0x461123fb3a23b5503c01afdb83b0e4387c98ed4b      1
     67 0x46b2bd5c888e6d57a839c559fd6076f2c15a8cb1      1
     68 0x46f7909e605fec492b05265c7c6ecb46e94675b4      1
     69 0x478bb542f7658d635abba67edb987806dff5b83d      1
     70 0x47c553e889f7bfa75cf3775473180a04e9effcfa      1
     71 0x48238faa9bd6d56fb44a32dac6185f04eabf2117      1
     72 0x48d26fbd89dda10c72cd8dc7b82b9eee0af6b48d      1
     73 0x4a95836eae95b286e20b3296aa49ae59712a0d87      1
     74 0x4c95915aa398c0e4aef6d7dd1ff5c0f9adbf9729      1
     75 0x4f8cdb39327588a2b11edcebc50cc1302aa87cfa      1
     76 0x55dc2a116bfe1b3eb345203460db08b6bb65d34f      1
     77 0x568a2e5713458a263da2b86cae2f6af0693799fd      1
     78 0x58f606e6a973e57165bb07057a75f047f42455a7      1
     79 0x5aa71418ac6238c78ea9a032a768dbbbf86bfeca      1
     80 0x6002ebe4144d7f3b0cd10e2ac0c0f357eb1c1c51      1
     81 0x618c7671366caaab6701f06ae9925ae682c6bf97      1
     82 0x62b08bd6ff6b0028101d547ce40e8d9019e0c0fe      1
     83 0x650b4b1c2bb3573ce7c11b9baea0012b472b34f6      1
     84 0x6555f8fb6a02c9c73d55c72959a9e0cebff13489      1
     85 0x6596eca2c999f686423bf30e40ffdb55f6b6f4f0      1
     86 0x66035c088491c5ad0c021a423574a8271be8dc56      1
     87 0x66ff8f288c413e61c8ee807088ce301c03076e6e      1
     88 0x6811eaa638b04aedafeaddfca36c50ea303b1866      1
     89 0x692997405c1cebbc56ff3416fa0b59ab97fac696      1
     90 0x6954ee7ede83ca555663704fe7e2b1528ff2c619      1
     91 0x6ce9f49239e8adaf5bda1154262a3a054fad9db3      1
     92 0x6dce0c9dc00fb34b5e1e932e1d640884f5f1782a      1
     93 0x6e63a4caeccb4f341ee9c9175c9cc554bdb6d10b      1
     94 0x6e977c8155d914e7aa99331169939978a4d1b102      1
     95 0x7015853f4be641296f6c92690ff69fd36704dc2d      1
     96 0x70cc2cd17112b1107917131c2e6d939d0b6df194      1
     97 0x7304689aac83c3b236332b0c233878f1819ca89d      1
     98 0x746b46d87543eaf779144c8870888119d6ae0c2a      1
     99 0x75259686c1c967399eda0b4b16f16fb0990f9615      1
    100 0x753f8dab2ce1e6ec33e2967bcdd4974f60025319      1
    101 0x75aadbb7c36db057e5ac64183899f118de76df35      1
    102 0x762da606029d3120735aa1eec15464e265db7a3c      1
    103 0x76490db4ca34ec8b6d709026f679e5b1ecedb0e8      1
    104 0x788dd831546ea3b490bca5060ba3bd78a46ad9f4      1
    105 0x78db9fa7214765014732cd9b32947a96473e7133      1
    106 0x79e4b132776e182c7d2d4322833609cba1ee7956      1
    107 0x7b2ad6af37be6a77c95556d9453e4877b6a8437d      1
    108 0x7b5b247c60796faffbbea317692486cd080cfb3d      1
    109 0x7c397135064965fe5c25e25b643163ec33bb8b06      1
    110 0x7dd5b92716542ab2442030df6d87cec437a51ded      1
    111 0x7fc53dfff734d7cb9f533154344e4d569a34f8e8      1
    112 0x7fd1d8e388b29ef0b8a00ca07e2266686a5559ee      1
    113 0x80870d798bafb17877d6e82f8a5681515e43c200      1
    114 0x819dfc1b757c0ed67fea97e33e70cc4cc640f99d      1
    115 0x832408210f74c5cd8ac6fd8ebc92bb6b8a7834a0      1
    116 0x872e5925d1fb3a63176129d9b576dd7c196a05ff      1
    117 0x87da297a832706b5115ee1feb47b7fdb7c476261      1
    118 0x8ba9de821d795dec74f736345a3047c3221aab2c      1
    119 0x8f54249ae4c8a73e92d44459e026c9197670f3fd      1
    120 0x8fc4e14c1c2ab50be594208b0e5f1a22056316d3      1
    121 0x90198c48129ac7564933f8bab85669e303ff6b9f      1
    122 0x9109c5b9a5030532b4f116e055d3b1f1e74654cd      1
    123 0x950752398e94462732a82c2e35d2ff789aff3288      1
    124 0x96e861634b4c0e0a9e7c6a65dec549cc2a8a0e56      1
    125 0x97064190190d4e2fb284a2d55dd53504e291c4cd      1
    126 0x973231cf471f768f0939042e0591da1b15f4799f      1
    127 0x973a2ace28745ce4715659c60ef70b9e4c044086      1
    128 0x98f1bda2d45ffaca73fcf2154e9e007871e14934      1
    129 0x9af7edc48bc8db5e4afd3af95df143ea29e2e2d6      1
    130 0x9b82d32d9a43944a40e8a961e967be6856a71627      1
    131 0xa353401738e7ec6a4db9c0ee4c66eb5252f681c4      1
    132 0xa3785d2f231808d54aa207a84aee81c85dcbfb21      1
    133 0xa8879c580a54f190ed53b43d30de269097ad7543      1
    134 0xa90dc6d356b622b9783f5020722e6f92b663438f      1
    135 0xacb0ca20d2031bf7b28fddf13709ed13db7361ac      1
    136 0xacb818130ac9cd4484848848a0ca3e7dda4bb265      1
    137 0xad8cb7dd25b6f814bc067149c43cde2ab8eded58      1
    138 0xaf63db4e76a7cf1920232a748b65d5c9c8689fbb      1
    139 0xb2c21980ddb10aeefa8ab10cf79a036aff89376e      1
    140 0xb4bb21f96a5f8ba3feb0dde8495a4e312dcc356d      1
    141 0xb893ae8a1824604f6df4dfde52e2754921ba1a73      1
    142 0xb99172161b5edfb4e01f7ee5feb4264d684541f1      1
    143 0xbaca88029d2b4c3e7e06af8e5d7df2e3ac8c46c9      1
    144 0xbaea3cf94abd0d6e0f029ef5b0e54e9424a72985      1
    145 0xbe65d45635a400d895f0a2682c4fd3122b10fddd      1
    146 0xbec5d88ef9491b661ef33a596eed6a4e5cdb00cb      1
    147 0xbfbe3d5dac1e55dff6b03602e87594579838f99f      1
    148 0xc2d742fdbb31442b843a098dcec3c20054c28986      1
    149 0xc47faf99100078461a63864f50f1315b6abbd5e8      1
    150 0xc79556a222367ecb3c2291d5c6d2b3e58e25fbf1      1
    151 0xcb9cadd85c510ff5a4fb2d2bb6fb4b74ff3aee34      1
    152 0xcc47fcbfdd6e558fb39cf5ef4d92e59890476b86      1
    153 0xcd8b72710595c69019c109aef5b1b92eea7f995f      1
    154 0xce013eb11d874c8fa60ec9007626e154f33ef2c1      1
    155 0xce41e7bc61770344a3ced518b2c1627a9ed30f80      1
    156 0xd01fa4427a05fd7944557ae94b67d90109fad402      1
    157 0xd0a030f8f91eaec83a31afc793c736fe396cf0be      1
    158 0xd13e37b2bea300edda889157740fc2a0505b54a7      1
    159 0xd1afbe4da2a5adc6faf30fcbea9e4eea0ba8c70a      1
    160 0xd35fc346e15ba5b446917c9fd23a9471d6144701      1
    161 0xd42d52b709829926531c64a32f2713b4dc8ea6f6      1
    162 0xd76dfe29f0371fb0640906c699165b6bab33c522      1
    163 0xd809a687e957761872b440909348fa6547cafabf      1
    164 0xd81ce8e89dd987c8ab630858c8f1e9df14788c35      1
    165 0xd88c17aad286da44d7216f487b9a65ce1f7d4581      1
    166 0xdafcf1931a891fbb29a29c47ddee5ee76e24dc2f      1
    167 0xdb21ee947cba24dff405ed5ee33913af4c5f7c0b      1
    168 0xdba78d3d196029880fb35be407401311d6dfb3cb      1
    169 0xde0ea6059f96b4f455923c27671f066365b84a29      1
    170 0xde41df3d6734e920a420646a0151ede1be81a6ba      1
    171 0xdf38a8b16f9bb88083c1f1edad41c5323d60f8a8      1
    172 0xdf95a9ca4f6a7dfe2576ca99d96417a62deb2e0a      1
    173 0xe3b1dbf3e1824ec1a736e977bd5dbe53cda4548e      1
    174 0xe447dd94b12b61100565d28247765e7a5edfa895      1
    175 0xe6ca11a7b413866c6b9b284b5f3af3061aedf785      1
    176 0xe703f231ab056ecb99c92a1232cc1020acfc72f8      1
    177 0xea9fb10809015cfc7712590c71cf1d8f8cc45890      1
    178 0xeb43b5597e3bde0b0c03ee6731ba7c0247e1581e      1
    179 0xec6e5577ce4a3882049a4f93b0dec3bbb8b1cad9      1
    180 0xec9e512fe7e90134d8ca7295329ccb0a57c91ecb      1
    181 0xecbd845ea648f597775ed689673dcf81855dfc61      1
    182 0xf2439241881964006369c0e2377d45f3740f48a0      1
    183 0xf311b7b3751618d2762ac3ef62e8be61662c2ffa      1
    184 0xf31781b4811b737e66662997679fb07dacf63355      1
    185 0xf556b3d8304f6c484336641e64893be5e4c75f86      1
    186 0xf824ef230b0f7fc9038f9fdbc249717419219e77      1
    187 0xfa4e98a3973ac6d51918e69d49f749219518c5a5      1
    188 0xfaa8a175b8b0fd2ddfb31347282afcb53870ef1a      1
    189 0xfb1bb9d4a4c50457d339a686212a0716ea0adc80      1
    190 0xfcb89bf7d52241add4f474693f2210c5c832f188      1
    191 0xfea9b1760505fe0ac6ac48c30bc81c9d7431f554      1
    192 0xff559fc20d9b6d78e6e570d234b69c18142bb65e      1

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
