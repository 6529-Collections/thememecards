
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance          contract        
     Length:12122       Length:12122       Min.   :  1.000   Length:12122      
     Class :character   Class :character   1st Qu.:  1.000   Class :character  
     Mode  :character   Mode  :character   Median :  1.000   Mode  :character  
                                           Mean   :  1.129                     
                                           3rd Qu.:  1.000                     
                                           Max.   :120.000                     
         name          
     Length:12122      
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 17868669 # https://etherscan.io/block/17868669
block_hash <- "0xd55111db3ccdb8ed8b7d3fdc54a4a3cf7c0b8a3b2c0c7eeaa3b3e863980b0d80"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 5009 

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

allow_artist_phase1    <- pick(snapshot, contracts=c("PEPERIPS","THREEPANELCRIMES","SHDWP3PL","HACC00NS","ARTHEIST","CLICC3OG","SuperRare","SuperRare2"), address_remove=address_remove,address_max=1)
allow_gradient_phase1    <- pick(snapshot, contracts=c("gradient"), address_max=1)
allow_memesRandom_phase1 <- pick(snapshot, contracts=c("memes"), address_remove=address_remove,address_pick=300,address_max=1)
allow_raw                <- pick(snapshot, contracts=c("raw"), address_remove=address_remove,address_max=1)
allow_singles            <- pick(snapshot, contracts=c("65291/1s"), address_remove=address_remove,address_max=1)


allow_artist_phase2    <- pick(snapshot, contracts=c("FAKEFAKEFAKEMEMES","ARTHEISTS","THREEPANELCC0OKIES"), address_remove=address_remove,address_subtract=allow_artist_phase1,address_max=1)
```

## Allow Artist Phase 1

``` r
c(allow_artist_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_artist_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 188 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x005c1071bae3aa43b15bc51eb76c6f863695d320      1
      2 0x00bb8932dfda68e68f41b55e03898a4d2e96bdc8      1
      3 0x039d7c84bc43f8bc311ec21aeef57dcb45a8091d      1
      4 0x04c95c6f8ea7ff7c3f7a2f148fae75650c5a875e      1
      5 0x0d14cf6252064c19558afeb253702988da9cd667      1
      6 0x0f0eae91990140c560d4156db4f00c854dc8f09e      1
      7 0x0f3d81bde896c4d7f9727b6cbd968c64bad77c76      1
      8 0x103c8b5b5b2468f2552f0a1aa30738bf08ada0ca      1
      9 0x1064aa646a7aedbd40816fc0c35e044d0244a764      1
     10 0x110b081d4e165197dc6b6f4a920073c16fc6e6d3      1
     11 0x113aa7aaa8aa85322beef06dcfd1dbff732400cf      1
     12 0x11f46fff2624737aa0d38507e761b6ef770eb94d      1
     13 0x1259706239dc5c98a0c99aaa1dd82d5397cc81f4      1
     14 0x1288eb773f9ab5fd2368c870f02aaf97a7bfce9c      1
     15 0x136bbfe37988f82f8585ed155615b75371489d45      1
     16 0x1439b4d5a72343b68f12398c649df99d76b2af53      1
     17 0x14669df8bfcf56ca60dbc3397b8e8f0c0ad23062      1
     18 0x147bce36252a6fa76a14d6640ac278245047dcf7      1
     19 0x15a907f665a070bbfba3fd0fcc05bf0078ff44b2      1
     20 0x1689b564814db5f1c7abd74129771d282e2d652b      1
     21 0x18daa902fda72b63474bd3385633973795b77609      1
     22 0x19bb567ac0116876fc74f2f5512eaeec07ccf968      1
     23 0x1a21275f6bf8a2ee88e2427598f94f8ba3d35fe2      1
     24 0x1a9aee029a1b8c69c4404890c85715cc726746c7      1
     25 0x1e8a2bcb241d3ad860c95fe200c2cc6aed1fa5c8      1
     26 0x208b82b04449cd51803fae4b1561450ba13d9510      1
     27 0x20ce68b0a875023d1ce516a928a082ac5627fa7f      1
     28 0x26fc701c55b1546155eeda9f8a173a506e916314      1
     29 0x2998c9cb132e7b76cb0e0db3531adf6bfdc8cfca      1
     30 0x2b13b15cac53d97003a42d9ab37768278aaf45cb      1
     31 0x2c4ff0ce082d8ba63f6d1b8b5cf1995a42de3296      1
     32 0x2ca043bf85fe1553bdef8c0dbf021ad6202efb41      1
     33 0x2e0d63ffcb08ea20ff3acdbb72dfec97343885d2      1
     34 0x2ec4a2bcd4f33c7c9aafab7cfa865ec15508bf62      1
     35 0x3546bd99767246c358ff1497f1580c8365b25ac8      1
     36 0x3595a1508cb1180e8e7f50008db1109f5293efc5      1
     37 0x35eee1f835a08e4b0df848197c4d377c23b79ae9      1
     38 0x363d81817324f4bd57d5a022137fbcfded12589b      1
     39 0x3656d4b4047170ad6eedff0eb0a80c1c148a408e      1
     40 0x3686a4b272c646ef6fbe34377337d95db7356e63      1
     41 0x377d056cb6022ca451692e41b5dc89c9c2e24bd2      1
     42 0x3997b25296c0d3d2706b4e59b298e943a0bb42f1      1
     43 0x3acc6ce8530227575c94459e7581ea665b2c6475      1
     44 0x3cd446e111453e0c8613ae3306d09121c47d39f3      1
     45 0x3dcff2782f07b19baed2f992e901d2d8990400d3      1
     46 0x3deed956b999b83361b85bff31d388c35125411d      1
     47 0x3e27ba397f671e3d705b9ce8349934056f5adb07      1
     48 0x3e5f46ad03a924d0c1ddc9aa311fdd265be312df      1
     49 0x3ee4a408d933953571cea6615a55f2873ec8201d      1
     50 0x3fbe2fd902d278e3f05575149505f998445ea4b0      1
     51 0x3ff35a585da47785cd70492921bfe3c8b97c7aa9      1
     52 0x406a3399c50adcd50d62b925fc16f84dd91439a0      1
     53 0x416bf3ea73ca228605e0a9dea40f3ae7cf6860a6      1
     54 0x4298e663517593284ad4fe199b21815bd48a9969      1
     55 0x42bee6cbd9567035a741581e878bb04207744506      1
     56 0x431973b9593a6f273512008670979d32ce4f756d      1
     57 0x448cea2b10850b48ef33bdd92f89007832379598      1
     58 0x46d62a1c349dff0950dd17f09dcc2ef3f698df45      1
     59 0x47e2d5de9273d277738d7b77779772e8e429d319      1
     60 0x4851aa7241d05ac0cc4ff69c0378f1b865ccbc7e      1
     61 0x48ae667e94363be3ab27a4a3fc0e98c657b2b107      1
     62 0x498939d80526fe3a9133c4fecd51e193c1549624      1
     63 0x4a39ae58b605102913ac19b7c071da75b55b2674      1
     64 0x4a7351c3b0fbb1c40848b88d9f947c598e5e49d6      1
     65 0x4aa357c22dd3aa64b5874a8dff77bb19812c54ff      1
     66 0x4decba7ff1da83cc1a8e6530f2603b18860f5a1f      1
     67 0x4f329135096acc3dae209a9b36b182e9c1d237ac      1
     68 0x5041bcfd24d3a285d1b141919fa7276be6e208d3      1
     69 0x5179e27381a79dcbd2c5d0279af7ef1a8c46f854      1
     70 0x52ad2b7e5a8e591cf9ad7124c6dae8f53f87d936      1
     71 0x571ce1f54a8024edcb9f38da96206beeef726537      1
     72 0x5aa71418ac6238c78ea9a032a768dbbbf86bfeca      1
     73 0x5bfaf9fd21efb6c4b3a113a555dcef96ccd5e3e5      1
     74 0x5c70af3ddc7e815a40f2a09d22483e6e24eddf8e      1
     75 0x5cbb31e39ca93ca13d8e9f686bf749b14076bc65      1
     76 0x5d37431b1356d8cc54f3dd77657e566830d73186      1
     77 0x5d95baebb8412ad827287240a5c281e3bb30d27e      1
     78 0x5ef3709676163e00a8010dcd73fba55ad54dea73      1
     79 0x5fae9d4b591f213b3ba75287f2cfac0883d17f7a      1
     80 0x612337ff252d2050f5273414756f843f510b1a98      1
     81 0x62378418109526beb9798082af510b7e08335b24      1
     82 0x63c33be3be7ff1a956bad578daab8e00669acc39      1
     83 0x649ff45a74d3ffb86a31fcd1e9c88f46e8bb38a0      1
     84 0x681efc30f97494f7f491bdc6acaf9c7782a26816      1
     85 0x694e64d4ad77e0c234b7b1c55ac40302ad86ce3f      1
     86 0x6c2d65145b14978a95d14c8ddc5415d9fc447910      1
     87 0x6fd6afe08202d7aefdf533ee44dc0e62941c4b22      1
     88 0x70f729e89eabacacf11293e40a700524112514d3      1
     89 0x722e9468d7f8c2710b6dbfdf70f01b045aca1851      1
     90 0x730aba725664974efb753ee72ca789541c733db4      1
     91 0x762da606029d3120735aa1eec15464e265db7a3c      1
     92 0x77a395a6f7c6e91192697abb207ea3c171f4b338      1
     93 0x796646c66913023737b87cad9f0fb5c920bfe1f0      1
     94 0x7d2550161e8a31d0b9585bb9c88e63e9644af740      1
     95 0x7dafb564184aaed0a7832b70f8a392e764fce024      1
     96 0x7dddb8a4eefcfaf1ed7deb851349f513b4d56432      1
     97 0x7dea78692cc42374cc605f1291025c2641bc7fdc      1
     98 0x7e8bc7be173ee3b7fc496099a1fce689bd3c9755      1
     99 0x7f138cdf66bd4f2446be6dc3ea4fd65a4d0064a9      1
    100 0x7f302243cd0f214fb869430d4509fcd561e875da      1
    101 0x7f9879d66926105cb5ca3ac5eea257354d18f238      1
    102 0x82e133653e5069a61062b28795ef19a99a3d2c75      1
    103 0x849a27048de274c084f42551f395823f8fa18f2e      1
    104 0x85cc3c8da85612013acabe9b5d954d578860b3c1      1
    105 0x85e5514fc67b6a2c228330cdcae543783a440947      1
    106 0x863b22773daf094656e7339bbb844514a7e20555      1
    107 0x872e5925d1fb3a63176129d9b576dd7c196a05ff      1
    108 0x880c8fb6a59fb13ee7894b4fc18b7cbb529ed24e      1
    109 0x8957340cd5b2d567574b8cf59c0193cd3e047ff5      1
    110 0x8a18a7ea34a476018da24c385f07dc2508a45c50      1
    111 0x8f4931cf0e59d4d5fc5e5211d15096b16e319127      1
    112 0x8fcdff1c8a5f19d56e9959ed72419aed8f7e2e44      1
    113 0x92a293f563872671750060ab5935e1117c3f7294      1
    114 0x92ef4baf847f2045c0a7d90ed6b3c25cc088da57      1
    115 0x94b764bd9f21bacba12487c4f67b6baa7be3a538      1
    116 0x95759d87638cd0880e059062af9f9eb7956a1c6a      1
    117 0x973a2ace28745ce4715659c60ef70b9e4c044086      1
    118 0x984700290a73d76578c56fa78e66e15591753c3e      1
    119 0x98f1bda2d45ffaca73fcf2154e9e007871e14934      1
    120 0x9a15235379cf1111ea102850d442b743bf586fc5      1
    121 0xa033ceac558745ff742818529d7d84f7d1ee55cd      1
    122 0xa06e3eca46df7e71238ebd04fa627bffc7d3ebde      1
    123 0xa1dec63cdc411a79dd7a868a1cb0732e2746f6f0      1
    124 0xa56a1511359e0e7f12b1f5b9a464d7065a293323      1
    125 0xa62da2ea9f5bb03a58174060535ae32131973178      1
    126 0xa7be876283a68eb88f52c04950b195266800caa3      1
    127 0xa7d7ac8fe7e8693b5599c69cc7d4f6226677845b      1
    128 0xa91b9d36acb333c4b0fc175bb353498857a94b9d      1
    129 0xac06d32f100d50ca67f290307ec590443d639c8e      1
    130 0xaf2ab6d0e8f69656e7c8c967351de32f0d60fe76      1
    131 0xafbd69a1e9c961e1bdfcb132d6045d671e08c800      1
    132 0xb4005db54adecf669babc3efb19b9b7e3978ebc2      1
    133 0xb5018bc174321ffe9e0a38d262e9a448fbd21cdb      1
    134 0xb52343dbb87db123289ebbd6d8277ce9f1b71ca1      1
    135 0xb6fd849e71db599c9ed30f346725c776e6999fa1      1
    136 0xb890f264b1a7591cdf9e21b3b640a741cf677ae9      1
    137 0xb99172161b5edfb4e01f7ee5feb4264d684541f1      1
    138 0xbaea3cf94abd0d6e0f029ef5b0e54e9424a72985      1
    139 0xbb14040d6244b6576d6c914f023c7f9774aa09f2      1
    140 0xbb44530e21b3a5aaf0c86ba10d605e1396badd88      1
    141 0xbc5e0726a49d7eff356bfbd2b91b24dd612b0533      1
    142 0xbe4f0cdf3834bd876813a1037137dcfad79acd99      1
    143 0xbf2364550baae6bd9fca72703f669e335cd98e2b      1
    144 0xbf5aa87878c968c9d062f51cabd04b66023c8e36      1
    145 0xbfc3c19d3be0b0f54dca49fb136d08eea86a0229      1
    146 0xc0efdd2b7616b1a469d5fc05dcc90f29647f03b2      1
    147 0xc0ff7fab43b3a7e72371d5dc840dc020e6ee1905      1
    148 0xc13e40ce127c727f611b96eeb31e55efb133a11c      1
    149 0xc1754e1edcdde07b89b7cd690ade3a3136704b28      1
    150 0xc295d8f3b0b766a424fde52fe32ef9db99daaaaa      1
    151 0xc2d742fdbb31442b843a098dcec3c20054c28986      1
    152 0xc2f82a1f287b5b5aebff7c19e83e0a16cf3bd041      1
    153 0xc8b28b6a310904c83d128be83b3797073b5c5302      1
    154 0xca812530a5a97f2cfb321fbd6f40da292e9f2045      1
    155 0xcb06bede7cb0a4b333581b6bdcd05f7cc737b9cc      1
    156 0xcbfb9a917c2b45fbd8e820e66602ef26dff9cd40      1
    157 0xcc18e76428fbf1d3da045b166cedf2e4548de601      1
    158 0xcf7b0637b0ceb93cf36298ab0dfb48b47edd74e2      1
    159 0xd37905283adf21d3e628b423cbe20c4583ba9979      1
    160 0xd3fac37e04a928f581459ea81d7b1e2dc64c2fbb      1
    161 0xd4bae9122912210fac2f47b5c72fd420574dde5b      1
    162 0xd50a805a547ed4f8da3602615e77ab9281a0560a      1
    163 0xd551a348558dbbdc8aca70ab136d8404cc9025e3      1
    164 0xd9204d84e1859f19ddab7961da221d2989fd068a      1
    165 0xd97631a21615ab030e1fff058c21118c51bc3829      1
    166 0xd9ff1fbd68e910392dc404f8df0fcd23a64921c3      1
    167 0xdaf5eba2f3d3dc422d62632fc9d5d457cb875b8c      1
    168 0xdc4a23d309b5030b57713d6c4a5ceb7cdc6b927d      1
    169 0xdcd5ab1f4bc3354ef6003ff79fa239ae216facd3      1
    170 0xe038f95ab0401979a4d4738ae666dc6d67dd917a      1
    171 0xe1050589c0a6f570754ddcbe8ac5a66c5da606e2      1
    172 0xe2917a00d54d686cec24b284afe34f50c258f3dc      1
    173 0xe50f359b5bc6dac1d5eb19e1b2fde2cf322d6c02      1
    174 0xe63fa6524fa2d252cc3b46fdb4839900bfbfbb49      1
    175 0xeb753e906cad6eef229d6fa0512ea6d02c69ff47      1
    176 0xec6e5577ce4a3882049a4f93b0dec3bbb8b1cad9      1
    177 0xee05f658e18eb04d250f829b1920c2fbf6907e27      1
    178 0xeeb3fae739097ad99695b3a9c0eec6dd604d79db      1
    179 0xf1e1c701b49b1dc2405a9e8ef40f9f88802b80fa      1
    180 0xf207e69a7cc3b4a00fec3039891a77b3df8c3577      1
    181 0xf443913c2bc40773c11315951b6a13b61434ea28      1
    182 0xf4d1a203b3a79385bcbf66960051522402ac917e      1
    183 0xf5c68206a746424fe96b580bf7e4274c47d66324      1
    184 0xf76a3c1965b15fa7b109bc7ce18d64246dae6d72      1
    185 0xfa24220e5fc440dc548b1dd08d079063adf93f28      1
    186 0xfb6f29bea510f901fbe3eda927f39e80fc972893      1
    187 0xfd4938b02074df1d3aa15a97ee561f40704b2195      1
    188 0xffb6d97bd1e7b7bd08595096d15037401a1f416b      1

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
    22 0x48464efe55fbcae8ae0c992b306afcf21d4910cf      1
    23 0x520e8d6c5b2dbbe62bf8a40653a314d879b19b86      1
    24 0x53006f95def268f88dc1b8216654ab56f3afd052      1
    25 0x575f6540c16a72696c14a17fa64f049992d661ab      1
    26 0x59068075a799594db03c0255eed68e8e121155c8      1
    27 0x60d2bc84a71af61b82d1b4168c9103ff6c4898eb      1
    28 0x615502edd5cf2d59244dd0dd53d3c8ec035e172b      1
    29 0x61d9d9cc8c3203dab7100ea79ced77587201c990      1
    30 0x64f7de90dc79d775703bbec66a1591c7a26a22f0      1
    31 0x665654f2d2a151be2d0f8e3697e6ce780f732af2      1
    32 0x670a840848eba22eb2a090fd65e1ff60c3ba9e5b      1
    33 0x699990a8e7ada9e92c932d6e8fb365024fc74b43      1
    34 0x69e68074f1aada957edd39c5eae0069973343f30      1
    35 0x6f0735bf1e6c69030d6990cdd580345b370eb50a      1
    36 0x6f113b0c8c7266d7514e4986e3d6aaf013b6754f      1
    37 0x729fcbb6e1289c88ce5113ec1c83a48a8e3c9f2f      1
    38 0x73bcb8c5e30bf85806aade7fc36f16c6b80fd3b9      1
    39 0x75005a05bc163b85991b9c9facbcf3155372422e      1
    40 0x78a576d3751e8cc8f30c54c367fe8e5a2cd71228      1
    41 0x82139687faae8a29851902783e02e699de0e0846      1
    42 0x85603a042455d2f36f3ec52c1c756344aa70c5ef      1
    43 0x88ccdfe9dd047b4cec4c1102d2d803e2d8bf683e      1
    44 0x896b94f4f27f12369698c302e2049cae86936bbb      1
    45 0x89ef05c30c821c49b67d06e630686c5c3232baab      1
    46 0x8ba68cfe71550efc8988d81d040473709b7f9218      1
    47 0x982d3c5223f6b5794fccb3208eb164d042cf2526      1
    48 0xa56c04347abee42f663eff9bc2d0147b97c8f782      1
    49 0xa743c8c57c425b84cb2ed18c6b9ae3ad21629cb5      1
    50 0xab2056903a7b62bac46f45a3d7a70ac799ca88cb      1
    51 0xae0d16586e5d60d334624c115216a52b9b1a0335      1
    52 0xae72c6a6fad9fa9d82d089e1ebf73b3043855425      1
    53 0xb6cf25f5cf8a1e1727d988facdd47f1dfc492caf      1
    54 0xb735af7ae1a77d1ec764c862c2c09bdbf2b34b27      1
    55 0xba4575ea27041d99e6614ec02318f1e23a623fe2      1
    56 0xbbdd72fcce73c2626719be00259ddffef0d5673d      1
    57 0xbf270918afe2ad16093ddce904fc358ad337cefa      1
    58 0xbf814810b44978de273191fd612aa47f7b69d564      1
    59 0xc02e6b0d0c1a5d8cd26beeba0fe8d76c5d2f19b9      1
    60 0xc2419841dcb9a0f8906d06463ae24e00e1470846      1
    61 0xc4e8b752c53df925013e03fe4d2273a8ccb6c545      1
    62 0xc6400a5584db71e41b0e5dfbdc769b54b91256cd      1
    63 0xcaa1c396e70384db58dd33be74b26fb119e98c3a      1
    64 0xcabd5f77ca9d48f4ef9793f20de42ad39ce93979      1
    65 0xcfbf3aa72bcc8af0ba064d9e7b83888495a280de      1
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

## Allow Random Memes Phase 1

``` r
c(allow_memesRandom_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_random300Memes_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 300 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x00e484da1156202e9dd341ad7ea9c908bb919e96      1
      2 0x01072faa900f4ca3b40944ca815f39837bce193b      1
      3 0x0150b0ec3c7b72879fbfdbdd79c18a8368b194e0      1
      4 0x018305f64f71290f16ef245dd063a7470fde44ba      1
      5 0x03183fcd2d6a40b466375cdcafe2cd61d0cda659      1
      6 0x0387876a09a45581109eecab2ff4ac049e76ba37      1
      7 0x03ed8e6d50bff0b8c49675f0bba94087d5e579ac      1
      8 0x04d0c64d8b303586af5cf6bb37db16bd7b78c43d      1
      9 0x0573383fcc26eb066010d28d96ab02f7c5cde67b      1
     10 0x057931155d0c6355b1bd108b0fbfe9bc77d59ef0      1
     11 0x05eec49e27704acf5c5ce31ecfcde4e382d78ba0      1
     12 0x077bfc14dd6725f260e1abfd5c942ee13a27091b      1
     13 0x085d07d65b41158a1545eecf05316edb5d163b54      1
     14 0x0a49fbe88cfc413181dbe93c9e3b6184b2264071      1
     15 0x0b9f898921b2bb8cd2d7d30faffec2c90200cc8c      1
     16 0x0c0d8f387030c631de001d99d40b2e519cf4d10f      1
     17 0x0c664c03eebcecb6c21e3b3bc77c9dffed5bd694      1
     18 0x0d08c74268c1260d9b50175c41738e1a45819700      1
     19 0x0ea9f4f7c371e7044d187b3da7850ccaa6370a0c      1
     20 0x0f5d5751d4f300c3d0adbcadc05f0af4f8baf788      1
     21 0x0f78afe5b1e689cc2b205a78531957a286c42511      1
     22 0x123be94bca3e5ae858039112d14c1c740a3989f9      1
     23 0x12c3d9510bba6fa40c55d4f30e1be25f5f953e22      1
     24 0x12c4908dc21e88a8f268c3f363098eed251c1aa3      1
     25 0x13bfd26cf7d981aae5f8d4d88af4b34b4b1d4da4      1
     26 0x13c39b3e6e6159bc233fad90825f1f9f84fcb102      1
     27 0x1566ae673ae80725bcce901b486c336e6acef465      1
     28 0x15a415c7589efaf71345006d33bfa07a9a42c8c8      1
     29 0x17d6df353c4062b6097ff8c872ff6ce31381988e      1
     30 0x18412b9f84df461bdc0f2e0709df507d5bdb6363      1
     31 0x185d5f60fb7c59d344ba939361908916cbffe1dd      1
     32 0x195f2cd867c1c02ae6e61a14ec53f5f01b440edc      1
     33 0x1af369bc1069dd286ff59cd69553550c07e0dd05      1
     34 0x1d25029d92a7051f46a13be9a512443c966d3542      1
     35 0x1d522bd33cdd1f60be71b0b7b2efe4e9f20a5263      1
     36 0x1da0f2a58e65d98a8d04ba3da56cff9f5ace5615      1
     37 0x1e0486ee85dd758078d75c674f3d28efc4c899fc      1
     38 0x1e31c76b78ff00e3cb64d41d5844626d991ab9e8      1
     39 0x1e6ef3e23dbf38428e752a7293b698b189c7317f      1
     40 0x1e7ba17cc3f031a0c5795739317f6b2022ca39f5      1
     41 0x1f916bbf39ab189a9e3d9e1823a7b1a8e9e5f204      1
     42 0x1fad9f65b39d235469bcb59bc664872b93eecac5      1
     43 0x206b0526f4866e7580780015cbacbb4a8dbb8c80      1
     44 0x20aa168e6c793646f60737399c8466dd643d4281      1
     45 0x215fc4d12967ddc2432a7b8936f2849740edb7bd      1
     46 0x21804e35a54aa4820e6cd409d70926a63dba3e45      1
     47 0x231595e3673a10e846803194f4982e1cf3389161      1
     48 0x2380ca49ed8e933c97905977763693e5cf8770f4      1
     49 0x275c83d76b1b0cc42bd066e327c4e578a327ebcc      1
     50 0x2924196a2ec71ea4ae1b1357381eccdcee6c18f2      1
     51 0x2a3c05f993d34bab6fa50c72f2465777d1c5d118      1
     52 0x2bb2ee28a5bba51db2c104b6c36a6907c21d4d4b      1
     53 0x2c52248bf9f5715570ad007ef4d9c660ed8ae2e7      1
     54 0x2d646486397bbdd894a9bb62d91fc5bdcb8d9f45      1
     55 0x2e5e62c8cd9ede2874b6a9c87f843389bfd7cb3b      1
     56 0x2f44fb58135ae5d3793803c73d471c7cde4bb774      1
     57 0x3025430ae8a81cd13e3d0969b1093f8d82bbbd7d      1
     58 0x317c32ca0309bb8a1f9d8a3e2836544f233c841e      1
     59 0x325d12fe8bce0f969f879679275adccfe8ba0605      1
     60 0x33a34e27a81436ba9d79276406a285e89a8bd8a8      1
     61 0x34978faf3a9f469da7248d1365ddf69ac099588c      1
     62 0x34b93462e65303f3857460971584fd0d908f2f45      1
     63 0x35bb964878d7b6ddfa69cf0b97ee63fa3c9d9b49      1
     64 0x36eebc48da111b14b010a97abecf8bb68d10095c      1
     65 0x3839a6c85a38de97abd0ecc092ef3311a7f1311e      1
     66 0x3927e502c865a2c873a735775e224930eadfd2e3      1
     67 0x3927f1b51afd205311bdddeb5646c4c6a309b2b2      1
     68 0x39cc9c86e67baf2129b80fe3414c397492ea8026      1
     69 0x3a0c596a40bbbb558d86b0f692884aadf1dbf20d      1
     70 0x3a30fa661820bf261b39f41a63916cad97b20e60      1
     71 0x3ac61447a75149ba67639100ecc92c76fb20941c      1
     72 0x3b0b262b187001522c34edcafc378500133ab230      1
     73 0x3b748a60dfa1d58eac080a5ef24b11a082edb6d2      1
     74 0x3bd5e344ac629c9f232f921bafdeeec312deac9b      1
     75 0x3cd378c9b1cb5f147ebf1b2c2564118946ae4ba1      1
     76 0x3cd76a7a90e0f6a7dfe6791a54ec459acaebc5ca      1
     77 0x3e5543a342446999ac11609af4beb82339ca41ae      1
     78 0x4322aa46ffed67809862da613725728e2fb8eae3      1
     79 0x4470e0f5a0a3969cb0d5aba71ad4e6c759dfc824      1
     80 0x4525b5402a6d9eefc656e828b9391aa9dff2446c      1
     81 0x455867c5b24bf1a29ee02e0195b9ff36bea17ca8      1
     82 0x455ce1afc1403b728789b4a8c5aa512600b668d8      1
     83 0x46abfa031be839b1599513887a27a403e8d6598d      1
     84 0x47e054a8fcf3e27a4bed979c33ab65a5883fe437      1
     85 0x48238faa9bd6d56fb44a32dac6185f04eabf2117      1
     86 0x48f1b764b8cd851025b88fa28f92cb855b9079c0      1
     87 0x4a15e3d354043fdefbb8fa77a55da211f4058190      1
     88 0x4ab5189a020ccd88f05c36de69e4d868d29c75a9      1
     89 0x4ac69caa7bc279fec93db0d10793eb8516c7a9d1      1
     90 0x4b2c1ce6a01981dc3ee79825bdc3f3cd7932bf11      1
     91 0x4c26c796abcf3513807efa54d54c34405b84a36a      1
     92 0x4c3d85e7fc0c91215eb6a4186592a41d975d2a4f      1
     93 0x4c7f1354ae3d30f7a88c9d972ad7f96b44ce0dee      1
     94 0x4e3cc03eb2be6122f2316b84d1c7edb900e90dba      1
     95 0x4e61548d94c8649ebfc2f5f54d2272fcbc687bf2      1
     96 0x4e8849962c43d9d7540f64b24bf76704926926ea      1
     97 0x4f13eabaf32ef4ad3837ae88fcfc6c69edba0377      1
     98 0x4fed7d0eb3bf1bf1ba69320962c81b132ad4474f      1
     99 0x50002a9b8e9938d509de84dc3eb3aabfbec1451e      1
    100 0x5022cf478c7124d508ab4a304946cb4fa3d9c39e      1
    101 0x50a9f0f42ecde1df805b1ba6e885881accef049e      1
    102 0x514afe6575b5d81cecaa86a6bddf28de2f89ba45      1
    103 0x53edcefe31da6a0051df17ad80e19ff93c490b17      1
    104 0x5458c15ff220bf75b92a76229fae88f93df9c6c6      1
    105 0x55bae3706b678ee2d5a6d6d2faec8a41854aaf9a      1
    106 0x55d4a9e5cde6fbe40991e7570e01899891f6b0d2      1
    107 0x56517e41ef464db3753ecfdd2dbcdd2f045b573c      1
    108 0x5659de064e163a7178b2d3b802ff13274d7d8e69      1
    109 0x574a8af005c82eaf05e92c9ccc01048e1eb1ae41      1
    110 0x575f6540c16a72696c14a17fa64f049992d661ab      1
    111 0x57dec6115e561f3de9fb7429cf14de429713b3d6      1
    112 0x59a5493513ba2378ed57ae5ecfb8a027e9d80365      1
    113 0x5a5936a4552382a900a3669665f11472f6a38a57      1
    114 0x5c3097e7fd3b97d9fdeec6d378884c892ff0545f      1
    115 0x5c9e2a6fec34b510996a8e2a3d1e2c47a382a8b9      1
    116 0x5d20e6c0db887b5c3281edfce5bb7677565e2b8f      1
    117 0x5d25087405105bab12624c73488ec186066a6376      1
    118 0x5dafb2d1f8b20501da62ba00c0c283293aa0b70c      1
    119 0x5df6dd99718f32ae362b41e78b7bbf66ebccc179      1
    120 0x6072b510aa765dd2f31975c9aa65dde39fd1a282      1
    121 0x60e5299f49cdbc13d152323105af462071b22c87      1
    122 0x61e814fe997f0b2816fb9ac3c7df3aaa38d8ebb6      1
    123 0x632734882ed0127fbdf2666478df42aa916bdc84      1
    124 0x638fba3bbbe8828394d2b13f92fba75bf32c8efb      1
    125 0x63f11d2060d147e75c2bfb83dbebe155b4d06b66      1
    126 0x646ef8e3c8e79a10daf8f32621a3e468501cc3e4      1
    127 0x6576a26763cefa8f37005c067f7902960dec6b8e      1
    128 0x65ba4f92d7dfa813ddbd849d9faf38a723dd9b12      1
    129 0x6767b1e546dcb7499652a1fc4bd6f1e36992623b      1
    130 0x67a90218cc5fc1adde45ad27a257f268023b2ecb      1
    131 0x68d45e76a87e9f1706e0d36b7815c8dbdb7e6aea      1
    132 0x69cc363055d8c3d5e211865e805e145ab9208d57      1
    133 0x6a278927f03422419901b77be4d47700b1f3599c      1
    134 0x6ae613a84a155fd80c8e6f14cb3a1d8958f51b2c      1
    135 0x6b0fb37210fe1cc00b164c8a89dffec3c75cea31      1
    136 0x6b4b2cbb69b9ca07328a40e7ffff3870f627c8fc      1
    137 0x6b4fedc18a1e8cc0523e56cc9b45080e1ea00005      1
    138 0x6bae54650c0d17f4aee76839a7d319dc5060d6b0      1
    139 0x6df000635d86613626c5208d7c9d71b84e091220      1
    140 0x6e1510ed41ba743344e9ba80646194351cca0c7f      1
    141 0x6f0735bf1e6c69030d6990cdd580345b370eb50a      1
    142 0x7102a591ded392de38cca9ac35c29e0f6ecef137      1
    143 0x7120a2b756ed48b6f433f70481b16b353df58b88      1
    144 0x71784687d4c74338bf284bea22956c74fbe6d631      1
    145 0x722e2e4c15be1fdded3c86f4100bc32b181827f5      1
    146 0x72bc237cf598505dc00d4fcc23b1663e206455a9      1
    147 0x72f52de160ece454a2d75a410f85f4978045882d      1
    148 0x74bfa6fc87e23d64451f5bb084b6fbf79f46e495      1
    149 0x74e225269effacee134aadb51aa135ad066f55b8      1
    150 0x75850227f61183759cbbfe2b3fb196e798ce0b95      1
    151 0x75cf6e0dc5294bef8cb5a6fcd23dc9badbfc443e      1
    152 0x764abe778aa96cd04972444a8e1db83df13f7e66      1
    153 0x76d01054ff91afc2d515f7fea9a3e3313e248615      1
    154 0x78130d139ee6c5c1b99a49ac70271fe696dc2b3b      1
    155 0x782adafbf47a604f146af4a059908e946eae539f      1
    156 0x782ff3f609427680db175365c24c238f89fdd276      1
    157 0x78f0269f5b1ca914c8c58993f41181f89e212b27      1
    158 0x7a71dc357646a8149a920dbd26f66f07769f01d9      1
    159 0x7ad225751ed67118e9f54f87e08f64e43ffbef0a      1
    160 0x7c5504688eb4f81ed558550066dc218ffee1f6c8      1
    161 0x7d5300e22db1b2850da1beed10a894893731c1a7      1
    162 0x7dbba7f0551aef5894fd6ee331b95dbb4349f5d4      1
    163 0x7decf7a31168778f311c57b9a948abaa7321001e      1
    164 0x7faa22f45695a77ad4042c4cfb2c0330caa6a329      1
    165 0x808a023b72260170c95d831f589a1ae0dca1e43e      1
    166 0x81590c16b9d1495d133cf881acf5cc0b56730b74      1
    167 0x832834fc5b0b35ffd4cf697c4ec08815ad8d6a52      1
    168 0x853c69418605529a68907aaf7789270e3cf69d97      1
    169 0x860f0aa48ec759df1034d72e0311482a8b01db83      1
    170 0x86125855ea134f1da31015663fd3c19a657a8d16      1
    171 0x86286d93d79a9e5b78de97cbbc8dcaba0f2489fc      1
    172 0x8699793e22403b355185d8ff76e7392f98aafa46      1
    173 0x873dfcbbc61ef7ebadb652adb98df9f69399b70b      1
    174 0x879a8a1868d620584afa0c9c8807628d553c6bbe      1
    175 0x88f038389cbe95a917042bdb0f3afc434c680edc      1
    176 0x8b17e8b3f8315e73986f0c8370cea4f3a974a532      1
    177 0x8c2dabfea8751dd7901e35a015dc6c943ffa0d7c      1
    178 0x8c4138dcead987bb50929ecfbeb39ef3e28b4c76      1
    179 0x8d6cfe2ff08d8b6766eaef33990b78f8990b4520      1
    180 0x915c6cb77d302b8c514561feee12b5cb6532a97e      1
    181 0x91e8be20d0dfb2293b97317e906223c1fa833a48      1
    182 0x93549cf8047b3e305fb10df7110da213e9429f9c      1
    183 0x93c838add3ac72b42f65417408f900a7155e1bb8      1
    184 0x94db5f225a1f6968cd33c84580c0adae52a04edf      1
    185 0x98172111480cc81621fd8b12bed2fb5095be11a5      1
    186 0x99980dd0184773ea492c23033e1309609a7e75fe      1
    187 0x99f70f9aa62bd71a7e0719afc2d6c67c6aaaadbc      1
    188 0x99f8f74b1808bd7f1e9e76b7d82151b35dfdd6ee      1
    189 0x9c7471c6d03cddab9ef6e1db6ed93fb8f9328072      1
    190 0x9d52fd3f58557f3b5fcd2b10314bf566cabca60a      1
    191 0x9ea50708e47a237182a56c481a4d11dd021b7ec2      1
    192 0x9f281c5b04c091096ac468a9388f0ee6b0b8b1f5      1
    193 0x9f64e6a575f50d7c8c5aea4be962f86ee2c5ca31      1
    194 0x9fc80955aee9e3eb8c511a50a72b7e589700ffd6      1
    195 0xa0a43e56c6df95cf01d089d1284987468f8ddcff      1
    196 0xa1cecd5e4f6683189c3d4f832a16600c20e5b42a      1
    197 0xa24111399b765325e151f5f4cd11197cf4afa9b2      1
    198 0xa2917120c698fb5f2a03e3fd3524bda85a3eaef6      1
    199 0xa32560268dfff7e62cd0cda1dcc699406321aed6      1
    200 0xa33e5e1ccf57c0caf78ae399061f270dd24ffcdf      1
    201 0xa39d385628bd00438f8a9ba4050a25a0210f84eb      1
    202 0xa75dc095ed4ad69b088c3eb8ba2f93f1aa942b6f      1
    203 0xa8174abc36b3298356d273e512cd87223a2b3ffa      1
    204 0xa8450d71b98ca4955c63519ef593ba886c5b2b4f      1
    205 0xa9237c4eeebc879208e59c867cb6d7cbffc1df30      1
    206 0xa96d4549729c2a826237d91b3a3700cad7dfec4a      1
    207 0xabfb3a0a0e4c1bc2ad036f4531217e2becd215ee      1
    208 0xac104d56e4f8e0462f8c001ec7dde4c68deb596f      1
    209 0xae20574304b6264d3129dbfe5df72eead8c5b208      1
    210 0xaef63b54eacbdceff0b63609623d038195008d88      1
    211 0xaf12a9af19331fa9b89915537ce9d41bce5e4c5d      1
    212 0xafef3bb7fc3cd8ae083672b65f79e6cefd4ac619      1
    213 0xb33fb83c645ac2a12b138636fe59604312092484      1
    214 0xb6b4a02dca517564eb98790ff67d42b5b37a3d4e      1
    215 0xb70c3049982c09c18dcbf8596ccef6f5b3c239a3      1
    216 0xb7187511a0a0f5a63a538b7bc0abdd1a01979848      1
    217 0xb7abe0f0a1c31a88fdcdef71033cf7ae7d12f2d3      1
    218 0xb8dfd425f4d6227036d5342cc4ac2b90826e1b05      1
    219 0xb8e82b32ea205b0546d0ece1a175ad00e404dfa1      1
    220 0xb8efa945cf7f35854afb80e9ac05f5c1dc172fb3      1
    221 0xb976163a6f251c208bcf3196b9c4ddf927b63e72      1
    222 0xbaea3cf94abd0d6e0f029ef5b0e54e9424a72985      1
    223 0xbb8fafa8a629c4dce022d95e098ccccee1acd942      1
    224 0xbccc968eeebffc2586a50eef0ee1cf12273c9604      1
    225 0xbdc29767d8bc6dd6b08d64d74c8ecf11b3f5ccf4      1
    226 0xbecf64aab1c6813a526ce63483cb8cadb2988c07      1
    227 0xbeddf3e43015f5221f5d1ac0fd0d2ab3352d2f7b      1
    228 0xbf949494127d3cd72cd3399d4ab38312757f4d12      1
    229 0xbf99363007b348b7a019e70cce02e8ba62e95129      1
    230 0xbfde1b001f5446756f7b99b301c26ea6f9a721a4      1
    231 0xc14e891f453b14df5449a6644bc88dab2a5e5622      1
    232 0xc30e9fda779efe2081dae6529c0d07fc16769c8f      1
    233 0xc4c65d75dd6579671db24990bad1f502d7c96c8c      1
    234 0xc4f5b7207d510e2f35d045b278a29f57dbd9d15f      1
    235 0xc53f977136e1aa9031343fad39dcb4c11a1eb3c6      1
    236 0xc6c3752bdab737b8cfa174e6ecfa620ec0cc162e      1
    237 0xc72dd02e34129c57f0e68fb9e0df1fe185d71857      1
    238 0xc7c6eec4978799e6a53da79b540432820a349d0b      1
    239 0xc8f8e2f59dd95ff67c3d39109eca2e2a017d4c8a      1
    240 0xc99360f97475dcd02cbaa1940e0e6a2cb67186c8      1
    241 0xca288359ba6319c7e7c7377a670183acb3781cda      1
    242 0xcbfb9a917c2b45fbd8e820e66602ef26dff9cd40      1
    243 0xcda2efb1de50aa4476b4a20c36bfffdf97b5ae80      1
    244 0xce56a746edaf84e65aa6a2bbb45b2328f6a99935      1
    245 0xce90a7949bb78892f159f428d0dc23a8e3584d75      1
    246 0xceeab2af38e6b086cdce120c49f93b65f0b92b76      1
    247 0xcf281a48bb4342e7fc5cd5316d9bacd0e413e935      1
    248 0xd1e508c60d923cc9e93785da0bffe01710ccbac1      1
    249 0xd530282d853169a23822762af0dcab045d6295d3      1
    250 0xd61bee56435dc7eeca895bae90fc8b9c7fe709eb      1
    251 0xd81ce8e89dd987c8ab630858c8f1e9df14788c35      1
    252 0xd86057c793bdfb77bb11dcabfe054ee4fd86ebdd      1
    253 0xd89c641cf9cca44a3435895d24494d1bb7d70ee9      1
    254 0xd9c09e1ddacce4ee6548b66773bf4e6fe9dd0882      1
    255 0xdafa9e3dae493f0b9d6872eff4fda0f40d1b7488      1
    256 0xdbc7a4afadbff03d1ac183c9f664a95775c6d52e      1
    257 0xdc36237208adb46959e77a0052843ce5446afab4      1
    258 0xdc819a832bfa55c933238a474954f3646da275ed      1
    259 0xde328c96ae88f37733a8d2302a0eea537d557b24      1
    260 0xe01a97fc4607a388a41626d5e039fdecbfc6acd9      1
    261 0xe061ea94f07de987a55a49067b0d7ca3feaffbc7      1
    262 0xe1d29d0a39962a9a8d2a297ebe82e166f8b8ec18      1
    263 0xe1e57c6d779adea0f34cd3b84fb6bae3a54d1cfd      1
    264 0xe25c73435702fed11e9c5584ce6efe7cbff71739      1
    265 0xe262b5a7fa6d23be9007eeb93833d2e51d89834c      1
    266 0xe29bfd5761a18b5c3ac4a4271eb7fadd4c7fb942      1
    267 0xe3013b1d58b257684ed9862dbeef81d863e2e0f3      1
    268 0xe4000d4f3f0e59ca00b803f54784fe0654a192f4      1
    269 0xe50ae5655d227767ec3974a11da7e4f67476b96f      1
    270 0xe560646ef7a69400974d85e818bc0e054bde65c1      1
    271 0xe59e088489a85a6de407768deb671c7e493fd799      1
    272 0xe5cd0fc813d020644c496dd964a32eb9ac17e50d      1
    273 0xe5fccbbc11e4a36a77e9d8e47a9b28ec44e2e2ff      1
    274 0xe64777dcdca11fd8bde96ed05e7560ae789504b6      1
    275 0xe8cc7c453eca25450ca16d551db93d5f5740e054      1
    276 0xeb0bd6bfa6d109205f857caab59f651fe7631094      1
    277 0xeb43b5597e3bde0b0c03ee6731ba7c0247e1581e      1
    278 0xebaedb897fc974f35bf36f075d60cd75e46d2d4c      1
    279 0xed6e663b8d2192c515ff70ee0d6291e44db83be9      1
    280 0xee958e45f3464d712b8830deb5875c8ac105f698      1
    281 0xeee4ebaa421ed7aa32b925bbe2fe7506e33e7cd4      1
    282 0xf15a6b54e68884d27e1bebb1624d70c227b7d04b      1
    283 0xf262c25f75e93ddf7c1fcb4bb0d65a707ad6d023      1
    284 0xf3290df945e484a4ff846fb5baeb6d1147a7aa69      1
    285 0xf3e6fbbd35e2ea84bdfdce9be96ffdb2b8bd1fc8      1
    286 0xf4aa005cdd64b2c80e6cbfc30c427f80b5d1f0b4      1
    287 0xf599a05b4ea846a5afa1db274b508bb1ba1ddd93      1
    288 0xf777af5cfa7a0f242b3e0cf7fff4823c15dde70f      1
    289 0xf81c1d67b9c1f80eb30c610e2a78c2a1b1fb013c      1
    290 0xf868a2de45bc38844b16c9e63fda5e1de1d54a22      1
    291 0xf8fd6a5fa90940432e5c7921e332f0c5d33f2d38      1
    292 0xfa6bc968cf39a88aa67725463698b6a84bca865c      1
    293 0xfa766f029e703d341cc5386b9a4cf4d52be3b6a3      1
    294 0xfcbca52b23db28470eb97680d18e988d8b60b608      1
    295 0xfdb325752f9793ae7fa4aecc641cf5386846f566      1
    296 0xfe4da73ff6543b17b2b18e0e5d94bc87bd79f527      1
    297 0xfec4465b6aeac9c90e552ae7766bdc0ee0d8dbc9      1
    298 0xff08bbef8f2b6986018fe3bb62850aa7db843b40      1
    299 0xff1a6681aee53d032e8f5fb3d27ccf29d493e922      1
    300 0xff8991b1fa6b84e141bde964d442ceae0348040e      1

## Allow 6529

``` r
c(allow_raw, allow_singles) %>%
tally() %T>%
readr::write_csv(file="allow_6529_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 4 × 2
      address                                    amount
      <chr>                                       <int>
    1 0x2c8875f34ceb219f61b7453b2c5f100ec2f6ed33      1
    2 0x89ef05c30c821c49b67d06e630686c5c3232baab      1
    3 0x9274f2f89fbe5dc00c21c628e46a18c7187c14d7      1
    4 0xd3e401814d1faa8ca0419ecccbfee93ac7b15b31      1

## Allow Artist Phase 2

``` r
c(allow_artist_phase2) %>%
tally() %T>%
readr::write_csv(file="allow_artist_phase2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 674 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x000ab07b26c48ef3caf9ec23520d86794c9fd74a      1
      2 0x0113ac373e4855a0bb7fc1d4dbab2c92f0e801cb      1
      3 0x0152cda31cc3a44ad9a508d00fcbd9d87cda43be      1
      4 0x02726dc9a78572166571c11d43b5d27bc164003d      1
      5 0x02c1ca960532947a45d40a752944cd9648ccf900      1
      6 0x03469e8ca8e2a7bb0b96e79045ac0f1d9194ca56      1
      7 0x03abc6b18065e3abaa0444bf0ae3dba058258507      1
      8 0x03d690e07d1de703a7b8d05880111bf625a2736e      1
      9 0x0503bf05c49f96faac2b0f5fd394672ed4603c52      1
     10 0x05576913eea5d79b83f28f0cb0d12be54fdae8dc      1
     11 0x0593ed80604926c26dc540be883a73885ca070c5      1
     12 0x05a2f0eef8c2b4c68e5c0b5801d8fcc1b42b6cb9      1
     13 0x06b7558064992954fa982771742a4940ed44c3c1      1
     14 0x06c5b7dc5fc34a49065204f7a7e9bb46927d74f9      1
     15 0x06d78316cd6a146f09f6973f9dcfce36e76fba53      1
     16 0x06e2d6f58f8cc04391191375ac098b772d18c7ac      1
     17 0x071642731357694b43567e961c6b99647962a8c7      1
     18 0x074edf78d4925ee3c0f9ad61d231f3afcb85de5c      1
     19 0x07f6f97ac29fb6882746e7c95797d30c1b50448f      1
     20 0x089195a905c8d11a22c977aee408d8a694bcdf18      1
     21 0x098fdc962fe3506ce101c7849ceef2af88b242cf      1
     22 0x09b2f0988b6d24336b348c84c2764d2b4f85142f      1
     23 0x0a01b206898b01fe8e69cc0dd5bcece5758ddd80      1
     24 0x0a346913f236cd80825cdb14d4167b6ab12e6afc      1
     25 0x0ba0e48640a3e91e9a8aa2e75c5f5bdce8f6dce8      1
     26 0x0bb75bef057da63a0ae4b25fe9adafd35cd92b87      1
     27 0x0c50d718a7bed724ba11a1157a45059122ff5b73      1
     28 0x0c5780a740f75d94c530a5a665076eb65e3dac80      1
     29 0x0ca983ca594f997667be71aadf01dcc5963b1068      1
     30 0x0d73aa5de89b309798f9fcbba8b86c4fd5f542fc      1
     31 0x0dfe9ec2f57c3b95653f47da96b49861e4010b36      1
     32 0x0e19a153bb8647e1a28c616c72bb0f9307dcc62e      1
     33 0x0ef38ff87d8ae48538dadb42068a24a09cbec442      1
     34 0x0f234d9eeeb406722ef59d2b12131213b6f29576      1
     35 0x0f79000ed842e1b0701cb9d6d8aa992c5f7d12d4      1
     36 0x101f0e7e3e49f2c426763bcbb153ec9acbc1caa9      1
     37 0x107752288b215467de25c16119787d715ec8e26e      1
     38 0x10fbec46f97087503b7c535ba645f33ef1eb692f      1
     39 0x1134528d239cf419f9081ceebc3430ac191c318f      1
     40 0x115894b2859e58419d1ef4787a79a431ce2615c3      1
     41 0x118d6f5d61a9d881d452510f29c703abe9d80cdc      1
     42 0x11ac7890d565b6a210752b6304a57d12ee5635eb      1
     43 0x11c03b0e147af0e527a7f8bdfec94987fc5c579d      1
     44 0x11e7dfd1ceb3571e00888e96c77ed92fa32c3692      1
     45 0x120e4809a78cfc042bcb090b8daf6b40963626a4      1
     46 0x12c8ba50d4e003f56d0d0be442ec5ceb1d592ab1      1
     47 0x13146dabe7aa1e56488616b63eedcc27474c8496      1
     48 0x13256a56e9ffbc86d03b2157f093b6f9f21c2295      1
     49 0x13eef4ef8fca471f242ab0f8f49a3db6017ada33      1
     50 0x14d8f85750808f609614d7727596bafac4471547      1
     51 0x151e86b0f89ed7940231b6f198e1656664b8cfe9      1
     52 0x1594287badaa2d2c98278ee19845ea80c9b6418e      1
     53 0x15a8b95379448698c42e517002091226377e9710      1
     54 0x166fc420366c83a385dced8be850e9320613f986      1
     55 0x16757a2cc02c2fd4e0ca58c0af0d0a4aadbbeca9      1
     56 0x1776cfdcffc21cd51b35d1efaf5b3db4848da1d7      1
     57 0x178f6dc99291aefaf00ca7065d1450cff33191c7      1
     58 0x17a297a5e20abf80b4ca407dd044320c6d9f5d32      1
     59 0x17d02fc3f620fb8c9ee14e43e1a743807408c7b2      1
     60 0x17e566d94b9e9471eaaa1fd48fed92666fe0e6c0      1
     61 0x17f197bf61aae29472e4f14d9e039ffc926e56ae      1
     62 0x18e0f9aadde970d74430cc8636a381ccfcd1f559      1
     63 0x19284d531625e690785c67af206c7159aeeaf58d      1
     64 0x192fcb7706e539a77dcf1766d64bb49a468d80d7      1
     65 0x19365479ee3c66b71a91cf187400528503e0e9df      1
     66 0x1986f4bcc6b78d40e499e928a910dd7bde857734      1
     67 0x19d74f3ab452b027c0ff27612f611477f0a3b1ad      1
     68 0x1a99f8a22b184fe3ae18fbe64d24db397637014b      1
     69 0x1be9021726e5eac98bd3079b49a8de6e60744e04      1
     70 0x1c57ddccb31fa0cf7b7ffdb12cbebf80721b91f9      1
     71 0x1c6e3c966488416aabb5a819597da7d07853270c      1
     72 0x1ccd2496090c53687572edc2b62c3b58b6799bbd      1
     73 0x1cebe2bdbba77d30435abff2d248550023a2facd      1
     74 0x1d086b69c25f2903f2035a9b1ee4497629f7e2a2      1
     75 0x1d4b9b250b1bd41daa35d94bf9204ec1b0494ee3      1
     76 0x1d5467beb67510bf5b1f6004de451f970a82fa21      1
     77 0x1d5976b4dd9819ac36a29fbe83c8f3e226b61134      1
     78 0x1dd2542ad6187024064b08ed3fcb076021bd22c0      1
     79 0x1e227911af7eb6ba32ddff2615af2bf5cebabca5      1
     80 0x1e364eb88cbbabec80bcd7688f7059274f54a39a      1
     81 0x1e7f2ef8bbde7fcaea03ba0c83d5d293d066f41c      1
     82 0x1e8e749b2b578e181ca01962e9448006772b24a2      1
     83 0x1efc0e664fae4d145be8599d980cb0a5d7bb3c7a      1
     84 0x1f3dd5e77046ad79ed836750dd65fb0ef2df3e7f      1
     85 0x1f575360c38a0947b92407642219ab9d5ab44d9f      1
     86 0x1f57b7cf79f16e1a4e54d2bb881851679370be44      1
     87 0x1fb575dd6e450b473059defa940ae8f71fa1cb7e      1
     88 0x1fbe7b6152843f072daaec35db10c28bec8a6dcc      1
     89 0x1fc4f1e75b7ec5935121e9e48c5aad8d3f9be86d      1
     90 0x204864a8e052256825a3ca1dcc1027a916264103      1
     91 0x208a372c041dbc9a014dde4cb54180786f0df44a      1
     92 0x20c0e84995211cfbe87d4370772c9f102e22cec2      1
     93 0x20d9afc4f5b7e916b0f5adbdaf12b15c455984e0      1
     94 0x213594d974427e8c1bf4f4f557b7b6f4987c50c5      1
     95 0x217b91ab7bcc91de94cee4b216d9a8cc0a9031ee      1
     96 0x21eb929e7c56c2da635763946c27a40ed3a76d36      1
     97 0x22bd04906e808196b19f7d10463fe463ed5f88cb      1
     98 0x22e7787ea9607a3df167d730ed30c001a2dcea94      1
     99 0x234890bb60245ee6ebaf801e0812e37767005f54      1
    100 0x23a285f020784bbb0159ef02dd150321a0042334      1
    101 0x24eb16a94ea52ebdff84ae6a9a118b3c204c9c9f      1
    102 0x269fd4e0c6fbad7c682d8924570dbddda3b092dc      1
    103 0x270e171dc5a7a0f19cc4c0bc5ad7a0eeb5b8147d      1
    104 0x27df6d9c9f6ab123385bf1af3fa251aa56e014b1      1
    105 0x28d2d86d39a889c9000363c050a7163b3d4a5d4b      1
    106 0x28e8f3aff2663be454435b62ac3a2cbaf5bbf655      1
    107 0x29d5268dd303cb16344d68a185efb2f5ba006ef3      1
    108 0x2a71c223ec78f4c246d23fb85d83223ee910ba4e      1
    109 0x2a76f7df64889a1f20f5b6aa87ebffa9a38ab925      1
    110 0x2a7d9921ddfa35d46576d1b4b7dc62138a8f8755      1
    111 0x2a99d24f92e1ea7ecaca117a0e22114e22c8df29      1
    112 0x2aa0fa39847979fa2c96abb58d37246f9125ead2      1
    113 0x2b2a9cd8bceaf092552bad89e19901caad9dd2cf      1
    114 0x2b3e41b47511b254084785c9a1c8dc233d195d74      1
    115 0x2b9c44b4681823c826e51eb380e539ed35a3e4e0      1
    116 0x2bc52900a118390ea8a97b29eb5e7e75f1cd912c      1
    117 0x2bf201ca17f53cb16e66526ac073595e206fe656      1
    118 0x2c0bd965de00a507f4625ceb43c36adce38a9739      1
    119 0x2cefce1f856cb9f6c1c0f8b0ec76f528adb3fe74      1
    120 0x2cfd8238167a3e302ce00d9b26699d406d30d2ac      1
    121 0x2dc529053ace4ac1d2d82c27dd026a352bb69ca1      1
    122 0x2e4c240b0e344616c356dbee01fa1b63a2d72f13      1
    123 0x2e6435daceaa1e7e14e0715a5b8648abdebae061      1
    124 0x3053acc1ca39f4b490951a36f25c50be03ebe9b3      1
    125 0x30993edbf825e315b952f9615bb4314a56e35590      1
    126 0x30b30b0b3e640f50ffa898a95e4fd790d262b496      1
    127 0x32211f8e57ed7d5bdd6d180d45305f119f6f1a35      1
    128 0x3245938d5c684301d60275426447a860392072ec      1
    129 0x33926984172ec1365587987f6b923b4330008154      1
    130 0x33a34e27a81436ba9d79276406a285e89a8bd8a8      1
    131 0x33bfd3ba3bd175148d1bb1ee87e865253fd265e5      1
    132 0x34cf7ac942a815ddeddf1319ce91dea69af46dcb      1
    133 0x3568a628394a940cc8f9b3948bc207b50e4b85c3      1
    134 0x35f2e1d3a99c3fd78cd26db53960833b994448ea      1
    135 0x36064e9ce7aece9abb5404bdd98e0dab5d37ba64      1
    136 0x36fa19ee5cd778abe2a4b62f59b7a721e2e16702      1
    137 0x373d419979862c6845e74cee5ae1e075801b6abc      1
    138 0x373fd22e5489b4fcc84851330ae12bec8a24bc59      1
    139 0x37dc5c15c1c4a6761a56335f6c72a3d002d3ff83      1
    140 0x37fa7a65ce14dd3f3b21749e6409b828df6b79fb      1
    141 0x3840f067df8a1900a04051fb4bc639f7f83d519d      1
    142 0x3881c3aa678f4c914d23319ee78c011b1a13331b      1
    143 0x38a6a0da8c66467a3be19e293fb6a7a10fa7b7d2      1
    144 0x39124c1ebb47c10fb5082fafdb5435b4902b3728      1
    145 0x3913d13611d2f128e3e1d3ee7daba53e4d37a2c8      1
    146 0x39c4cfb864e8936f9fc221fe5354543020d6fb7f      1
    147 0x39d8873825197b891694f7e973600855035dd391      1
    148 0x3a1987915156cf3429626e956b010127ebbe421e      1
    149 0x3a24653d4bea05b0d9e22c9a26ad3ae094cbdbe3      1
    150 0x3aaa457c751c7a1820708c616b831602ecce9237      1
    151 0x3bb6d0feff2f1b4a75e52683e91e2ab19aabcb91      1
    152 0x3cb63b82d778105e43f064ed739b0655f1f0fb87      1
    153 0x3d9d270a41dd9c38e370127998b98a6d4262b240      1
    154 0x3e5e41ca02fbbfe7bc64f1a62c4c1fa7df2bd62b      1
    155 0x3fa5b646b19271033f059ec83de38738f3e3163d      1
    156 0x3fe61420c33b0e41ddd763adaaeb0b638e78b768      1
    157 0x404a3d8eef9eb924f872c74db29518eacbe73c83      1
    158 0x4074031c4f0e33c88f9e072dc8d65091b1731672      1
    159 0x40f465f1ba4c2aba91c0c896cb92bbe4c7e545df      1
    160 0x412b52644c50aa6b4d704318db37537a33fc01b2      1
    161 0x42143e982e44084e0f52c97dbee4b06b7e5c4370      1
    162 0x4252c3d4fdbe3dc20e8874c3bab6903d5cb0a04d      1
    163 0x42cf62449008fb43d086e1f7e31c95cbbbfc2a8a      1
    164 0x4306dd0c18b58e34e587003841d736a449392d49      1
    165 0x43176751abfbd7a39549748aab57641c26df7129      1
    166 0x4463fc46e16f1771ce0bfcab37b5ed9a1afbbae9      1
    167 0x44c2a80f8b924858db469a5b35128908e55c531f      1
    168 0x44c831d87a421529f00e07c3a6615e5db9ea9563      1
    169 0x456c8f06e6dd3fce8c8896f30535033a3f1df35c      1
    170 0x45a9abd5720af408015e9051a641250fc4dca5fa      1
    171 0x45d6a189e6cfcd7c3c914093ebf5a31dfdcf33e6      1
    172 0x46745d20510f2c305d12b8f3b6625f0cf8e720c5      1
    173 0x46b2bd5c888e6d57a839c559fd6076f2c15a8cb1      1
    174 0x46b6ace0b80664af6f7b9341e4d9b73394de8da6      1
    175 0x46b82704baf2e08ec2ffe72f32b6f92772bea15d      1
    176 0x475544001a333ec979cc954c84f9b750a25ab46d      1
    177 0x4838bb775b38a1594eae9a3d252b2a2ca812c0bd      1
    178 0x48a356748a488a2ad1369d848032706ba9a3e579      1
    179 0x48a78a1a2d5e02302285cd3d41336d1e54e2f018      1
    180 0x491befac53aac201e97cd6f4c750e4a958d22d46      1
    181 0x4943cc7813da1697265fded90a2bfb749eba0d89      1
    182 0x497b5af00112f42f57b2496bde21123b1d4f85d6      1
    183 0x4987a85a09b57aefe8ee55285ae0ade70ec6821d      1
    184 0x49a1308b0af57e5b65b7b445e7d7109cffd900e7      1
    185 0x49e6357e3972638bcaeff4a92c25b711ec71a94b      1
    186 0x4ae7c8efa8ce2fd897caca84cdf48261bc9aa429      1
    187 0x4b43041e418b16bb7dcf94a09f721cc73574fc04      1
    188 0x4b75ee116acdec99c285410f08bdb02d11516384      1
    189 0x4b98ea1a70df4e84c556ba1f0f41860f3dd3d7bf      1
    190 0x4c165e1e556d91ea36dbf8d8e0ac81b8706d63fb      1
    191 0x4cd6435ce1c6e391779e234a12bcd2b3fbc47c39      1
    192 0x4d1f42c4c3e3ca984d98959d3b7bd881e0bc5278      1
    193 0x4e1a3895bd8cace0764c161f3b11feaf58ac4f00      1
    194 0x4ea3d8196d0076b6c9090f262cf5e1ab3bc5981b      1
    195 0x4ee48f917afc87dc0ffaefdfb9f3757a091c632e      1
    196 0x4ee8fd730386dbcde705181eb10efc1db544d37f      1
    197 0x4f6afd125968accbcdfbab27ce1cef36ba741c08      1
    198 0x4f764a08c66251e13bdd85b4bb0652b739736328      1
    199 0x500898b272d3454d7ce39c1e99b469deffd92b74      1
    200 0x5138cb94d86d6cde09c491fd72c81095c965bf01      1
    201 0x51e13ff041d86dcc4b8126ed58050b7c2ba2c5b0      1
    202 0x52439c4de8c10ef8904e96ad00171c0e86169dde      1
    203 0x52981e081ea651a41e08a44f90c7895a70c3e198      1
    204 0x534b8531e362da97d808ba2ab3959fa2597bef11      1
    205 0x5381315a97df70fc1f47fe8489efb12ad48fc187      1
    206 0x53a664cea00d315ae9e17918ecb91ec2b37b032b      1
    207 0x5414984579eeb4154a97d786eae3fbd293aef054      1
    208 0x57115f7d04c16f3983802b7c5a2d5089a188d76a      1
    209 0x571b8db726c2a29d237f88c9efeb896290bf395b      1
    210 0x5794d81e058d2b36bb85e40cfb78b052444f8ee7      1
    211 0x57b43960b709a3019b0ff0abf1110fd22f47b294      1
    212 0x57d39dae1f6830ca11ac56687423f176ab2207dd      1
    213 0x583e5136c7c9dc1b3871a853f94a8beedc031f33      1
    214 0x58858eda382541d8ec29df08d77f7aed73613b92      1
    215 0x588672a61fb89f2dcd9a70001f06e8b692567755      1
    216 0x59977fee7680632fd779b55f428361b7ee4bfebe      1
    217 0x5a22391df530da46450de6ace8f7c28c2b11f0c3      1
    218 0x5a6cb310edee1fdaf593a1bca6746d6c1a3a5f00      1
    219 0x5a8bd25e00a77478e4a5e9f96837a386b3aabbbd      1
    220 0x5b4a139263f790722b6f485cd9fd9b95ede6d178      1
    221 0x5c7e1ca7b9570ca37f7c3e40cb6aece4f2782004      1
    222 0x5d4e840aee5c438934a4dbf8687d9624bca637d8      1
    223 0x5d7dcb9f59d4e1cf96463a72e866966149df1552      1
    224 0x5da50971f7981c4dadcda5c61b7e5ad579c15f78      1
    225 0x5dabc158b6e2a94590cb07c866bb0640742a180b      1
    226 0x5dfa3092be17f441f85a4b3218a7675f0efcf9a3      1
    227 0x5f312df04b19979ef4bd5876737cfa481a928c0d      1
    228 0x5fd21eba060ba46ad458d6a2b0db5c050f07feb2      1
    229 0x5ff6f59c446c810f9ce32896df5f92f51f832d43      1
    230 0x6002ebe4144d7f3b0cd10e2ac0c0f357eb1c1c51      1
    231 0x6018e57d73bd0bfbe773ef5b0bc099c37b16c988      1
    232 0x614ef48f3db5544a33c148921352ccb32ff480df      1
    233 0x61800f3230a6954888032a5f0bc37a7611a719ae      1
    234 0x61b0c4edb1f9fd4b18341b115afbea2aa303467b      1
    235 0x62050220633afe524d98fe3fccadbeacc2b96110      1
    236 0x62cf3d7a2bf7672eb087c439f5d1fa4e2093a732      1
    237 0x646aedb1e877fa189b5aaa6c36009a8970be93ce      1
    238 0x647eb74a5135a0f24beee3c2d8432adcbb32c2a8      1
    239 0x651a6f6c0b331a2f9d036adcf0ef5e268055d7d7      1
    240 0x6546570e89768e4aa7fbf0ca787f682a27b2a7d4      1
    241 0x668c911f4cd76204ceaa963825d86815de52ff07      1
    242 0x66aa706ac6a4640c25a9e52e60d8486e59bb371d      1
    243 0x66b9095056ed206c0ff7e009679cc8185abde2d0      1
    244 0x66e678dc7cb7033f2e52d6f943892d3099ababf9      1
    245 0x66ff8f288c413e61c8ee807088ce301c03076e6e      1
    246 0x67452f406020a9a4a672b70e7e282b6d98d7b805      1
    247 0x67691771270f0199bfe54a00f8343d15afc5b872      1
    248 0x680e8a5a1dc9499342a7bc0bfb3b99dff60ec1f9      1
    249 0x68706c9eaae9f8e29c980bb02213a2ebe425316e      1
    250 0x6888a6e4be281f0af6413c4e52eea4564770504f      1
    251 0x69cb3b1de24e08f1cfc2994171b6c6930498f750      1
    252 0x69fa19bd440a3ad5240ea8b9d6615d2199ff898a      1
    253 0x6a7e96f81789278929f823cf0894f2fb546f292a      1
    254 0x6abc553bb948ef12be7ca2d1325ba3ece7d4b33f      1
    255 0x6ac029ae2e792a56354c544347f38d68db618492      1
    256 0x6b1303ae7ff1890c087c13d1855b96ed8399a546      1
    257 0x6b5fcb24d90aba0d7d940fa075de534b235ef2c0      1
    258 0x6c3a5ccaffa3c19a5eb23b52da184d5b8a74b005      1
    259 0x6cad462e9c55e54adda4c5249825635a57ac6635      1
    260 0x6dd231a8afa47751a4a02f5f1de1ac49203c5480      1
    261 0x6deaebcc104e8f2fb0248fa6c44978bad249348f      1
    262 0x6dee5436c908439a7e961c715d83efa640fa545b      1
    263 0x6e01346be21254a576d72ace36c69c756b011ee5      1
    264 0x6ea742ba5f2f2ccf69f120fcd57071121cddd912      1
    265 0x6ec2997cf35457daa9de4cdc989c697d921b5285      1
    266 0x6f15ddf0abd573986875ed740c1a94f5fef65205      1
    267 0x6f54d40ccc174f9c7bd23252e4db8f1ca4291173      1
    268 0x704cf202792341d79a9fd6dd97046aa7ef3f4319      1
    269 0x707502591380bcfa98175486a38c15ce90e82097      1
    270 0x708bd0f7441998b0641b734fbe9fd536cce60399      1
    271 0x70970b86b677ea1e9490e8e7e1a750944960351d      1
    272 0x70be81471d39f1a864d24d53b77bf67cc4548305      1
    273 0x71042c2a74bbc93ce9b8d4eb6b42fc6f07ef31d5      1
    274 0x7193165655884ec9ab3d93773123967b97b5f669      1
    275 0x719f17ed85c9208f5fa7dbf4067bcffa6d63f508      1
    276 0x71fea57d15287a97028697582b92d9442e0cf565      1
    277 0x725ccdb7b49444e61f0ef170304fda6fe142ff54      1
    278 0x727c3220c023dcde47421d01d69b280d36d68311      1
    279 0x72d345197a1052c2515432c42b888b279ad5f40d      1
    280 0x72fef1abced43b44e2c838f68b785fc7028fb2c0      1
    281 0x7393ec110e757f4dbccd016cbbd95fb6257f4d0d      1
    282 0x740b639f20135d6ae6b6ee7af185a623aa4f912a      1
    283 0x74781a0ec38e8d3840a221ad20afd4e8f0d4078e      1
    284 0x74be16d43ff1b010bedebc3c5b266603914e63f7      1
    285 0x753f8dab2ce1e6ec33e2967bcdd4974f60025319      1
    286 0x75a473c33bffb61e945d86b37113c0859965a789      1
    287 0x75b03fbfcd3d33cb27cf522abacd6476722a9428      1
    288 0x75b977bba86cfead07817de400ba6ed291c18aeb      1
    289 0x772725c580d2f533c11f0cba5169a58c793b50e6      1
    290 0x77bb41b3a80982e19daae2cfe94403afcc613489      1
    291 0x77f961a9e4c61eedcc328636e08cc1519901266a      1
    292 0x780b371a201fb0082a0b32dfc1563eb7372aff69      1
    293 0x7833db851b29fde73dbe739be5a0338f6a3c742d      1
    294 0x7858f91c6752400f2234584a25006335be6d09bb      1
    295 0x7897189df101eb980230782a9a9cc4fe4f35c6c9      1
    296 0x78af47e0e9bcb8c24e27b837b638a241b28d4619      1
    297 0x78ff20aac0d76c3e01580f9181ad924f2b0e85e5      1
    298 0x7915e43086cd78be341df73726c0947b6334b978      1
    299 0x7935799cee02945d53562a052203dcc1f06808dd      1
    300 0x7a003b7efbc105e18b071030772b548e0973e626      1
    301 0x7a08e0799c228fbe8ba00a4583157eb7014a20e5      1
    302 0x7acf5bbc7bda34b1ec0a2b5a5ef33c5fc8165748      1
    303 0x7ae40fab9a289e0a6a2bebab2392ce83051065fd      1
    304 0x7b71afb18c297aee9fe0f0af06cff28e3227b13e      1
    305 0x7d4157ea91695c66709b57c08083e141e7241160      1
    306 0x7d70b5b638a2af9816202880535c8fe8f1fb9772      1
    307 0x7e5baaa750e052187b9972b9f3ba8ef908b8dead      1
    308 0x7e7216319b016c758ed6f792eff389291aa8040f      1
    309 0x7e77e3786a7283053ad015038d5695b87607d11f      1
    310 0x7ec3054f96f18709eb7c834a0457a1d56164f7a5      1
    311 0x7f94e30381aa6657c45833ec7fce2e493c1888ef      1
    312 0x80a1c9fdc26199a69d190ebc8ad287ef48758977      1
    313 0x810ae546f9f7a63b1e5fbf2aa55584971c713d0e      1
    314 0x8130761ba3ea99d65189dffa71dcc5fd0d1ead43      1
    315 0x81686bf7add4e8f22bdf00afcb4ae93f123a6429      1
    316 0x8264e9e0f4cbcbbbb3f8ecaec0a625b590ae790e      1
    317 0x8269e14b9a3ed95db483861788f3b4f006894a34      1
    318 0x82f0b20eb0d6b308178b6f7cd6d719cc3de3c20b      1
    319 0x830ba1078a698c0e318bdff86d1c6a2ffa7c3b0e      1
    320 0x83871c24ed8baf83439ba97b53cae89bcc6f5d3b      1
    321 0x839a4547fb3aef7c68bfc28248f7fd1de952b524      1
    322 0x83dfa1fe212a9edc6b9414a757c0c7fd3369a6d4      1
    323 0x847f33f5c02b0ce987c337fcb190650a91995859      1
    324 0x848a9126ff57004aa8bfdc99384f36a4754d4367      1
    325 0x850a3d0d157d4907dda29e15f33092ec1afb27c4      1
    326 0x8513fafe1813b6ec3bbd9fc4baf5340bda8d670b      1
    327 0x85677e10cdf661bf8f1400d90d191803be0ffc2a      1
    328 0x85f6f0e61d0752fe183955558f52a3fdccba4b90      1
    329 0x8610df5a928e4bcec0eaf3df4aaa66ca2ceb06ae      1
    330 0x8684e8c7d4ae0256372e5fd727ca2bd3f4ac95b2      1
    331 0x86ba22e3e4a2ec1ae984e9a53e64001c7631c660      1
    332 0x87f71dcae99b98fc8a1c184dee1e4f219f2555f2      1
    333 0x8813df11cf5ddc4cfb234ed7bf78b2cfa9a63ce5      1
    334 0x883d7f3d2164cb484f097b034f32629fa251fe91      1
    335 0x88439bf8a247cd733f65fa3acc78b21ebad7396e      1
    336 0x888e44eccad0dbc2afa7a7f4b1a65e14fc9cd561      1
    337 0x88d95780c75db73bd6d527be913dd6b3cb1a201b      1
    338 0x896848834f20b72a63a7472ba52a6c8f898d452d      1
    339 0x896ad7c1a8f93a48258bea5b6bb43f8fcbca945b      1
    340 0x8a78c3d2acf15ec38db81978d2fe94ef308a3449      1
    341 0x8ac6a89a3484b372acf4f0de03646c8b2a962911      1
    342 0x8aef89129806b23c7930dfdf2b46e22ae1849c49      1
    343 0x8af69db0e66957ec67128806016ba7e881719e0e      1
    344 0x8b90eb28dd723fd07f31d1a6c867dcca00f10f1f      1
    345 0x8c188a91794b88d566d71afb8c448c83e9692dbc      1
    346 0x8c3996617922c3ca288958c233141d2672b6c4cf      1
    347 0x8c64591265f2c8360973f1a1488a4e3b718102a2      1
    348 0x8c9d626e93b9bb10e101c92c219768d6d706e43b      1
    349 0x8d16e6af9faa28277b4fef9f7225ab1642a7dc8e      1
    350 0x8f2ed3172e9c7f352a647f542541134755564e9d      1
    351 0x8f942a50b40e7df4e093c27322c09bef9219da61      1
    352 0x8fb2a5d8736776291388827e9787f221a1d3633a      1
    353 0x8fb3ed352df6e8ea152dab12f2d060e4e52d9e6e      1
    354 0x8fd8cc0f855ade5470c8af71ad2b0df98b94e596      1
    355 0x911133ed843df23f9b8e5ab51933aa6248f27427      1
    356 0x912758e337812fd21a1c4f6f44e185f3af2f7964      1
    357 0x918ecbab06caaa01b00f593bbd2fdefd60a90ae8      1
    358 0x9197f339cca98b2bc14e98235ec1a59cb2090d77      1
    359 0x91a0a3f2e88bb16493b83e45a3febf992b67ba22      1
    360 0x91b26fffffb325e13f1ef592b0933696098044af      1
    361 0x9237c2ec55dd857536a1e5bd57ed020afa71f814      1
    362 0x923fe59a80c33efda53ddf70e3ffcff407b782d2      1
    363 0x9269ed73ad700fed0e0032b8f8457424859ceeaf      1
    364 0x9279446fde428352ce3a3764950f9cf67eaa856f      1
    365 0x92cc17c86ebf30cb1d80c6c7ba497f002e623647      1
    366 0x9345cca5752d705e18a30e94efc82af0a7f7318d      1
    367 0x93805d7897d4843b1309f2fdb7c83d4fbf4c6110      1
    368 0x93e6832b6d190d1d7373408833231b1dc3bff79f      1
    369 0x944d0a6e0aa75639447a41a17d556e60cb62a71b      1
    370 0x948ade8e848ad86291f81c0c6ea284bec78de15f      1
    371 0x96d2b5bfa179ce0979fef08ca7ecec497b050b75      1
    372 0x96fd61202a698ee3eac21e247a6b209ea5ffeb91      1
    373 0x985d2c22cfe22fe52c9a0291fdff228af0c47c27      1
    374 0x987a8f3a0a7ad3b6a4b943c0411f4021b22a24fe      1
    375 0x995e0505603a19ee5c469d2359427bea68c6e953      1
    376 0x99637e6f797c98458a18c99050c5d710e894fa14      1
    377 0x9a27b56809358f6b88890ac1888e5603bf7cfcb0      1
    378 0x9a7dc5307940fa170f9093ca548bda0edb602767      1
    379 0x9b167ae7a58a1916c2a7bd011e4e5445d34ece79      1
    380 0x9b628095d191ae097127b5a442463596b9a62600      1
    381 0x9bae2c333d3a242645d7c7c0430f0b9cedf372d0      1
    382 0x9bd91abdf39a167712b9f6c24d1c9805d5fb9242      1
    383 0x9cc1f9cf3e2d319585ed8c6420e43855460cda76      1
    384 0x9e7a5b836da4d55d681eed4495370e96295c785f      1
    385 0x9e8fdfc3d41b26a2870c293dc7e1be8675fa8b7b      1
    386 0x9f558ff9237242fd67b7aa42c162705f6c00db93      1
    387 0x9fa2ad6eb6a55697fd90f3b908f7cacd5ab2f454      1
    388 0x9fac4bd5547967e9196d8124a229929b259f4c1d      1
    389 0x9fb3e82ac0717403231518c2f27c40d4dd228ad1      1
    390 0x9ffaab6c24d4fb58d61b26b08c8795e9e323f28b      1
    391 0xa077ae1422fa600b6ad4143dfb010c9a1f603d3b      1
    392 0xa07873b25689f1d7d4b91f1d5db9cd8c277c5e23      1
    393 0xa0901c7b8177fb3645b64d618cfd94a688270f9c      1
    394 0xa0d849068220b32b4211c5db442d119c55ed5c02      1
    395 0xa1701ff02251e5719b985b56c91751e075552b2f      1
    396 0xa1a211d7e06accfabe872707f53a323624150dc0      1
    397 0xa21fed132341f41c7f96cd9a5cdb3965bc0b74b0      1
    398 0xa2b4a66a9f62c4c85bd9cb1af3db3294ad95bbe4      1
    399 0xa2e371d33ed087f8f752d31cbd00834e735b3e1d      1
    400 0xa3021f7cb66d508164df80b7d08a8b0105c298a1      1
    401 0xa331bab8231cdf93b1d8d510a7a5f61869f2e829      1
    402 0xa4a1618b19f74dca5ca90d6f575a1c18d415e20c      1
    403 0xa4d59f67b2af795de4fa23a6c137ae33adeb93c2      1
    404 0xa53fd5fdcc1599164df96ec6d60c572c673d7fb0      1
    405 0xa5497708983314f80a532298d657322cbc9e9af4      1
    406 0xa6ee0f0c7bb5ab94a666bc78455060ef4daa44bb      1
    407 0xa72ac4dc61f794d76c408d68d3995d7f75982a3b      1
    408 0xa7faf09d160d777c98beef579087266f6da167c9      1
    409 0xa817b9bd1ece360e4a1692894a5ad1d40b889f20      1
    410 0xa89d3b90cd681a1a9a12e008a8da9ff4a3e49ba7      1
    411 0xa92e641eabfc0cfa1736dc8afb3874ae1d9077ac      1
    412 0xa9654c135b2ec72d59209a5397f1302a753c9f17      1
    413 0xa9ec7ab90ecde9e33fc846de370a0f2532d513be      1
    414 0xaa4766abd7eda1a1767e309cfada5ca998cbb0c5      1
    415 0xab09512f2136d2faa07c2a5f9e8fec3df8fa8b3c      1
    416 0xabd1c15b532a00d554a4d073e3caa7b2bef09655      1
    417 0xabf04838ffcf99e991b7e0df92d9dc7ebd40113b      1
    418 0xabf27a13039fef8a1a410866875fb8192951f81f      1
    419 0xacca4975186f0a5602fa250d39566088861bb135      1
    420 0xad03756c766b32e1acefaa28e77d158892b580a6      1
    421 0xae1a641d4c0270b7a85959d6f6495a721deb1be7      1
    422 0xaef475ce425feed7e622afe670d1c9a0fa87aa4b      1
    423 0xaf8738a35eb57a2c69eefd4ed48947ab45fcf765      1
    424 0xafa605a5513534c284859dda1bd263239343297f      1
    425 0xafc6ce6e86869a4f4655634627a57ab21cdbb9ad      1
    426 0xb019260796f3f0be964eb7cbdaf9957a55087e52      1
    427 0xb085030e936a9d564f4f159f390e1bc2b4473804      1
    428 0xb0d015b4041d9c67ba0189553c20251ab78823ad      1
    429 0xb0d7c9d9512f5df9d4f2f42accfd9f6776b7df90      1
    430 0xb0f9eadb1cf936836b742f34913bcc26fa26fb49      1
    431 0xb0fa409c24c131bcb1ec8400a5142150a670e746      1
    432 0xb102b8d5c04e9127602f13f3f084b2cdafa67273      1
    433 0xb1adb605787d77ab92be597e9b191a48240b5aa3      1
    434 0xb1e02a3219905adc9ce79d44cfd62a2eb48a0c8a      1
    435 0xb220486e7f274bfcd178dc2f1fe8ef3da10250d9      1
    436 0xb27afe18e5e1fb961b0d6568d7c3bab305f0b82c      1
    437 0xb363188d50bfc47fbd7fd4335cffbba82da63d5e      1
    438 0xb37623ca6233e082d7d955d8a65ebf8faf2e16cc      1
    439 0xb38bd68a6f0ba599808d65c7a9cf2a428105b680      1
    440 0xb4374bd294601a2b69dc6c0de1243f047ebe6e29      1
    441 0xb48aa6dca31d3818b2e346a1123aeee3a7d6477e      1
    442 0xb4d502361a3c6f823eeb9a99af09e110382206ee      1
    443 0xb5339157be76de7c5a796eb7eae58239bf7501b5      1
    444 0xb54a56f5a61f9e63c71bba6e3eca4e1a75835e7b      1
    445 0xb5f6d035b3edc32678e30db933c09541d1928723      1
    446 0xb63cd0501cbee5b10e6d5abe14e7036bde59023c      1
    447 0xb722e190319010d6e412509b582a77dfd75a7858      1
    448 0xb7812e4ab68713f2bb3eabd5db092ea0f48dd4f2      1
    449 0xb7e180be28961aa94e1d17334bbe791a971b639b      1
    450 0xb8410f47e152e6ec0e7578f8e0d79d10fb90e09b      1
    451 0xb8a9e5df1cc3518c047e2cec3d64e0fa1155addd      1
    452 0xb8ad88b9d89aca9261ddcbfee62177ed86d99454      1
    453 0xb9820a7d1a1c97c339a471457c94cb67354f5b9f      1
    454 0xb98c19ee72cee0614f70a1cb164faf9fdcb15c65      1
    455 0xb9cfa61762e029bdce4a63edf2cd6ddc4a05ed32      1
    456 0xb9f22a6920505dc067cba48d2979aeb0723f0d16      1
    457 0xbc15ac912c4af743eed5c0f033824de9de95cd4f      1
    458 0xbc1e0021f9d22582b34b584b8516dff417e7ab71      1
    459 0xbc4a47cda3310139756216b392df419a4ab73d22      1
    460 0xbd6933e93ae7304971d495f3dd8af5afea3b16e4      1
    461 0xbd7a1c16d7b3af4af6d1a0fd7437466d765656ce      1
    462 0xbee84b5c642fb4d3d73723d2d643aeba00ee5900      1
    463 0xbff30d8d9b76ff510997c3c0435de011b4dfab9f      1
    464 0xc006f81f7468985121775afa5e950b92a1d84486      1
    465 0xc00ae171acd39f84e66e19c7bad7bb676c1fe10c      1
    466 0xc07fd6754d8cb84f059ad764ba512f2f65ce6f1a      1
    467 0xc117c92319376a0ab225b936f2eeeec8d387c951      1
    468 0xc14d1d58f7d40eb287ea898595dc0f6177765db7      1
    469 0xc23d3085a6c132eb7bec00a471a2ba10658d7edc      1
    470 0xc2efad9a2ee3f965ee773dfba7124c9aff8b9f4e      1
    471 0xc3dcfb6e3f954ff76e3f5fbd83eda41978b8a3cc      1
    472 0xc46191cfc58ee6c314d5c8ecfa2c552c281dad54      1
    473 0xc4806252e4b9f2a4119cabb8e7c469ee106b4849      1
    474 0xc483287d7e6d2d5427444b7b5690794d5c886c27      1
    475 0xc4a843ddf533540474fddb7a9a03342348f6742b      1
    476 0xc50728cdb57a4834108003aabe65c64519ea6933      1
    477 0xc58adc6945966c04c74efc5a045fec55a03685bf      1
    478 0xc625f5c43fcbc45d1805d0c76e1363b78dcb42e2      1
    479 0xc646e02557877d559e55a3984ec12654ddd84af3      1
    480 0xc6940f9e3f99ba27475776f3fdae5d2eb171bc97      1
    481 0xc6d117f4932ea6dc482b36ab2f956dc9ff0e1ceb      1
    482 0xc6d4c47ad61607e3bc80b0e85cc5b3dd93ce8f5d      1
    483 0xc704389a3f999f325d568df1ab676d4b750f7c37      1
    484 0xc734b794e323d0256695247c305ebc6964e57f60      1
    485 0xc754d3a9833d8a82ecf35ae7f9cf930505dee2be      1
    486 0xc7daf473c103aa2b112fe2f773e3a508a6999bb6      1
    487 0xc7f2be6f8890d98277c848c8904ec2a4879dfc25      1
    488 0xc8cb180415b83f3f9a61691d246fe39a74c6a41e      1
    489 0xc9403c75f4543400cd611083c8b8e5f46653d01d      1
    490 0xc99744851fc3f2820d3d0268dfea0bb2844d70a0      1
    491 0xc9ed24f32fe31a5157a73baff4c619e5b87d8cd2      1
    492 0xca7e077ab56ba580f7dbe6e50cb2468827bcc40e      1
    493 0xcadae9dc25832a40318ce937a6493540a849a01f      1
    494 0xcae06f3a267994540880e3c3f8bd0f566ba29d54      1
    495 0xcb1167f9272d07755b75410b79a1938af652d347      1
    496 0xcb4d076603b6177978f348ac422bb0bfb6d3cec6      1
    497 0xcbbb31b76ed2c48d824bae72e7f1ab6104f684e8      1
    498 0xcc4548de1871eaed6f03a11e524ebef7b26d6d9a      1
    499 0xcc48857242141cc7f4f8555f8a3d5bed03556c19      1
    500 0xcc985b1bacc1e84439212c8bb10904c653969cd0      1
    501 0xccaa4e0c517fbef425b4ee71ecf62f56dc34b3c9      1
    502 0xccc6a22733342c3559048159134eefbf8abc2fa1      1
    503 0xcd20a09cd0a904ecb248a4ecfed5519ce1d98431      1
    504 0xcdaa26894d54fcd849b2ecae5d3c2956bac22c5c      1
    505 0xce4aefb4369c700de41cfe60b99c1b7991015812      1
    506 0xce73d94e23da51631ef31f69fe7dfd01c90b7167      1
    507 0xce8ad80ce1a979381d209ac230d02adafb9fa897      1
    508 0xcf6876ea88e2c1250806f24447de64083105e24d      1
    509 0xd0393bd8b342fa6bc658717c349568cb70f675a7      1
    510 0xd054b5608aec1402ceb3b79deab5b65e5f56726d      1
    511 0xd13da2666b19114b2bd070ba4fc37df53cf0a337      1
    512 0xd1f11145770b6d504dba8497b641369fb2e4dce3      1
    513 0xd2173631a8bb9de6b0e1ca53002c73768176b116      1
    514 0xd27d135937b4d310b10dfa56f2f204b51d20ad86      1
    515 0xd28dab6105ec063dbd05468b27dedfaddf03d8bc      1
    516 0xd2b12f1a75b2313980d7c410002e6a48cf389b8d      1
    517 0xd2dbe21025dede10517a8be905deb0af0bf2cfbc      1
    518 0xd2e39bfe299634b4269cba0b273c0ba207128b4f      1
    519 0xd323e4b040111b3feba91091290685cf0be3080a      1
    520 0xd3436529b2dabb3db5c212cbe26f67cb5936008c      1
    521 0xd3502a41edecc95cc07a339dffc92e764e45090c      1
    522 0xd36954df517cfd9d533d4494b0e62b61c02fc29a      1
    523 0xd38af56dac5097569d6e85da7d93540a88e5be1b      1
    524 0xd3ab9d7be8d31c1aa36b03e019c51f767ce9aa23      1
    525 0xd3caf1220c43e914aaf6958400c88ec76ce63c80      1
    526 0xd42d52b709829926531c64a32f2713b4dc8ea6f6      1
    527 0xd469974e39b8f1efc615fad5f37804a809bcbd75      1
    528 0xd4d3cc92ba95731618c5030b7e5ba177ad23286d      1
    529 0xd4ff60a025befba77ab42cd0c9ecfe6a9702ab8e      1
    530 0xd51ae18577368dc5bedc46c153713934838bfea5      1
    531 0xd544b080e7e506a0ab3f72fd1b501a570b2ccd97      1
    532 0xd573881d6126cc0dbcaab79b9f01235208fab675      1
    533 0xd60a73b98e3a24eba2ee16650365d72f8c427251      1
    534 0xd6a0c200c19a448a6e8cb32dd7142028ba2e160d      1
    535 0xd6aba7daea7e570dcd400fabeaef75933c7c9967      1
    536 0xd6ba516060e86b2cc7ba939876fd555c2bd50457      1
    537 0xd6d7ea4833f22edbed3dbd3d71adf3cdd8e36a01      1
    538 0xd6d9977522c6f4126ec407de2af999c29e672268      1
    539 0xd6eae1fd1e8280b7e266ee80db708609a32f99e5      1
    540 0xd778494b24e123ecf9873fb60249dc9d16a10c78      1
    541 0xd7d743254bf6bae1a509f96d0369eb7f45a6f190      1
    542 0xd803ba08bc87a7b765956d13d50bc5d1f37db936      1
    543 0xd831a9d032d19d4b4d4ad76bd93bf91fb8fb07fd      1
    544 0xd83e662b83880081741d0e810c3fe8630fa6cb4d      1
    545 0xd86433c180ee423fd6adbf819e530da05d8f4f56      1
    546 0xd86eebce475b0fd187b1ef41dcf43193c3f152f6      1
    547 0xd8d5524409d27d84875f4cab561ec1c1fad7cb63      1
    548 0xd91a9d604db19eac6679cd37151eaf18fb4b3e2a      1
    549 0xd930e58c82141764d5649b649ef8e53fbf49ca3a      1
    550 0xd9b22d474bcf8bf9c04a1cea893a9f6749c9cd3d      1
    551 0xda1226e62433437c7c31e8da051942c41395a3aa      1
    552 0xda2d8a5d58e0e25fa9e07138ce27fc76d32291b2      1
    553 0xdb21ee947cba24dff405ed5ee33913af4c5f7c0b      1
    554 0xdb25afdb6b1556a11c5e29aceeddf497a038a09b      1
    555 0xdb74b2559b46a76649f302390472858e7eac2ac3      1
    556 0xdbd9754141713d637daa6317534bdcff28320341      1
    557 0xdbfd836c989e1fe9586cb0d1bfb35e7849be23a5      1
    558 0xdc5ac414eed5d73ca97e82c6c290a72b128f3411      1
    559 0xdcae87821fa6caea05dbc2811126f4bc7ff73bd1      1
    560 0xdcbbdfe19a969175c381fa21fd095777a1b726b0      1
    561 0xdcf41fc1fd3c6e013d09914ef87fd36add09c727      1
    562 0xdd8041f77deca64b0eb8c9b31f371cdb125ae62d      1
    563 0xde31b36631c7bd5883722cc8b7f11dc8fb061dc3      1
    564 0xde8f3d9ed712aa6b40d8842ff7294b19f98fcbe6      1
    565 0xde8f5f0b94134d50ad7f85ef02b9771203f939e5      1
    566 0xdf31e86ed28294326016271a7e25d9d808d4b796      1
    567 0xdf38a8b16f9bb88083c1f1edad41c5323d60f8a8      1
    568 0xdf85b9146d73a852d7b900d1abd974f2a8119f57      1
    569 0xdf95a9ca4f6a7dfe2576ca99d96417a62deb2e0a      1
    570 0xdfa9e9e75fa67db514d464e6d7f7df72c8734e12      1
    571 0xe052113bd7d7700d623414a0a4585bcae754e9d5      1
    572 0xe09c9e0b7c3707745b18e764e0973fa3a1a3bb39      1
    573 0xe1d23eddf8f0f597a8582d86586999ffc41bb030      1
    574 0xe22eb5fea7d68e3653f8947a2cd471ac7b333ae5      1
    575 0xe2503077b6e1e7198e1fb723f152dd7d228c2e1a      1
    576 0xe251c6a8ea470a015d477846f43a19bf2585e2c7      1
    577 0xe2d914329e9a80e85b70ff123891368095b1da4a      1
    578 0xe2e9f3e5e6793c313f76076271bfe535cb27f114      1
    579 0xe39a6290eab33f99aed8deb1f7a977c763ed4f8d      1
    580 0xe3b0900713d8e71566d7f58bcaac10c4db4dc2f0      1
    581 0xe3d62b9313d5f6988eaa60a11aa52fd003c2531f      1
    582 0xe412186a1865c4fe54a79e0adcb08b81233df674      1
    583 0xe46c47abad108c56a1437702f5084e277b67a577      1
    584 0xe47231e1c690df1b3de04492bd06011e6f75a599      1
    585 0xe477a9d753ebe88faab8c1db234d55c2339ddc0b      1
    586 0xe49795418957250f8e64f89a0a2cf58f8c280b89      1
    587 0xe4f44bc4520d3161bef03f62967bc361082b3ae0      1
    588 0xe51ff9ef0cb02a8715729de3da80e0f438033b14      1
    589 0xe559e37893cb23414f9886693a595caac010503b      1
    590 0xe5c67e60f60605375827305c6d6d5167b4aa6a25      1
    591 0xe5cf73f96f57d56d6e44bd82d8af95727a2fab04      1
    592 0xe67ac23d6c555e9348c1086ada7e03e3f999c5d6      1
    593 0xe68d37fc77ac92c709643783426d45e96e4e437a      1
    594 0xe6eceeede0901e7da44ca4da65feebcb7a2f7257      1
    595 0xe703f231ab056ecb99c92a1232cc1020acfc72f8      1
    596 0xe74abe3bc87a4cf34acfd7c548208700c0e669b0      1
    597 0xe753dbeda0495428c4f33da365e03e229df59b8d      1
    598 0xe7896a1724da8f616de8a0ecb0eaec8e552f4bc3      1
    599 0xe7968cfa660fbf62b68a614aad3bfd92185c21f6      1
    600 0xe79eb6ce66353cd38e5a9dc67c97c836c86c6912      1
    601 0xe852963163d5c971192f0c1b984d6ef4712c8a49      1
    602 0xea1126e70185400a2cf0de2a6b35428fb8affd29      1
    603 0xeb10f919d15740e4d1a695ff2ff5a239850b351b      1
    604 0xeb9405f3f6b09a2890b7a9d22ec7516803a08bfe      1
    605 0xeb9b233fd68f7d3aaab71198981eeecf52ce8d33      1
    606 0xebe326d8de3413f8132518dcfd45e6cbff7e5c27      1
    607 0xec0b68626f7f4b4ac4c9cae6def857949be12a2d      1
    608 0xec3be799c9217fd27eef5b7120554d5430eaf65f      1
    609 0xec47cbbd9e05f8c4d587eb604f41740b0b2f33e4      1
    610 0xec73b6125d252d244be18affed9a0daadc99aa5e      1
    611 0xecb8f02ab638b07b12e9f12b965467f12a6d7e7f      1
    612 0xecba5f51925e6ccec26da38dcd7d5305f6bdfbcb      1
    613 0xecbf3548f636e30d162eca850edad2803593ae52      1
    614 0xecef908ab495fc9d2b1f2bf53910f38a05ed2b31      1
    615 0xed6ad6eb05fa699ef3ca51501cf30eee74cdd88d      1
    616 0xee8df38386aa85be55fdf1460a61d656c51a5e3b      1
    617 0xee96703614ea707b0b99ecb55da74c04ff70f2ed      1
    618 0xeeb473a23dae149be9c2537cc873df2a3f365509      1
    619 0xef025842a441937e67323dbad18e994b547d7c4a      1
    620 0xef51c8be528fa9ad489fe06ab9f87bcb927bb4d2      1
    621 0xef70cf19f15aedeee97852b026c8c62699145462      1
    622 0xef8dda4e93c418fad5acedd37d91343c448c7762      1
    623 0xefec3491eaf5416cea6bb54b9bf34141950141f0      1
    624 0xf02fe5b5f57b0d75e4588919299f96f10da07f14      1
    625 0xf15ca677751ecc1dfc67b9067dd90b8c9e26fe0f      1
    626 0xf1c5f2c5acc61ecb847967bd0af2ecf1081cad4e      1
    627 0xf212e72c39f1b8c7ee19da58ea31378340c93608      1
    628 0xf2439241881964006369c0e2377d45f3740f48a0      1
    629 0xf2a5d9c71ddc45ca98476d39f3dfc2f16dccab68      1
    630 0xf34196597df209a04fc92b97071dcaabbc63f0e8      1
    631 0xf35d1869ad2e389ecd8c7976cc1042e8b2842c62      1
    632 0xf3e1f9230ef6bb5646dc04879e67a3005cb69b37      1
    633 0xf408a40e46a3953684985fa46a625ba1b5573a9b      1
    634 0xf43b2be4aa887f426f05f78604b364af667c608d      1
    635 0xf49beb85fc09cc46146420a923c7905f5501a907      1
    636 0xf4caeed98e84f3ddb7bbd3e83fab46d0758b82e3      1
    637 0xf507e035b9243ae54b9bca48dc5448d2888f50bd      1
    638 0xf5819cc26f0481c9b86294b4c24027518a04bd5b      1
    639 0xf5b407a5bd88f549f6e8fbf4aa13364a7e93581e      1
    640 0xf5bfc78f72e0cd0bbb2ba3fc40a450578be3498d      1
    641 0xf5ca89d5d79bee75f40fe74bada1239ac44b0032      1
    642 0xf5d4a3b9fb77d1d5600a10e0e1c4efd12f9be751      1
    643 0xf613cfd07af6d011fd671f98064214ab5b2942cf      1
    644 0xf6a2be213d8033f83c00e2fa71508b8fc5931f44      1
    645 0xf6a32f757196ac753a354f145f408bf88beacf29      1
    646 0xf6db6bc928be2bd8afc732834819e2106fc85eec      1
    647 0xf6dfa4a1213f1d14f898f2eb840b1405984fe014      1
    648 0xf6ee907fd2e4dace32f0a5e388bee471dc49a323      1
    649 0xf76a07f67e1f6f9db8ebba3ee9acb6b8933f89f0      1
    650 0xf7c66bcd6ea3607174da0bc8458bd1508027e539      1
    651 0xf81c1d67b9c1f80eb30c610e2a78c2a1b1fb013c      1
    652 0xf83e1c0fbd0615913dc12be6bc2ad7b13cf0a055      1
    653 0xf8796603477b641ca33301f3c9bbea2dbfbfc3a6      1
    654 0xf88b7fe22abc3f110db2c956d8b54029b0bb1aee      1
    655 0xf8db01f59d0caa15067156ff7ed786eaf207753e      1
    656 0xf8de893f810c417b09314d15c70d59a8284d2fed      1
    657 0xf8f7c5cb07fcd41978abe694296210bc0444da0a      1
    658 0xf928302171d98583a03088c5103e9597f60b9fcc      1
    659 0xf930a731f1d22fcbea9d18f82a2154547d85a83b      1
    660 0xf952aa34177dd2f36b4d2e7e4884fced1c82a8c9      1
    661 0xf99b11016b4e9ab00226446af3715610359fa8cd      1
    662 0xfa7831f7b115471251d6b0f05e3c80ac4c75e4ab      1
    663 0xfae772e7498b0cdfc4ea929da72333dcc0dd2517      1
    664 0xfae96f816876ae1bbec61a810a4f8c033303f206      1
    665 0xfc61cc811c53c7a71b1806a1eab14fe13e0dbb32      1
    666 0xfcdb35c1105ca8ea01df3b81a4570ff621817cb8      1
    667 0xfd1c6e38f7eac4f2aaf8941064f9a42098f246de      1
    668 0xfd35356dcd225bbc7e8f1fde622bfbf5af105fe6      1
    669 0xfd6949e5007f7f84b845ed9badc6a4652e94879f      1
    670 0xfd96f75963fbaff02341e3cff43f48c1f3b13343      1
    671 0xfe0174255e410defaaf58b06e009e0ebcd74db59      1
    672 0xfea60461324cfa6d07cfe1551599f2bb139c3543      1
    673 0xffb5592fb5a51f85668ea6634c88ac1949cc063c      1
    674 0xffba913bb056544b75e57312ec3eae2528c285e1      1

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
