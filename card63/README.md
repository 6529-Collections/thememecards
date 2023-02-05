
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "memesphase1.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:1450        Length:1450        Min.   :1   Length:1450       
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:1450       
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 16562269 # https://etherscan.io/block/16562269
block_hash <- "0x330a8e8c4b1c5181ec712444e28d2cfa73ebaf1656eb3e45a0fcc895d1014985"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4642 

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

allow_memes1       <- pick(snapshot, contracts=c("memes1"), address_pick=250)
allow_memes2       <- pick(snapshot, contracts=c("memes2"), address_pick=150)
```

## Allow Memes1

``` r
c(allow_memes1) %>%
tally() %T>%
readr::write_csv(file="allow_memes_phase1_250.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 250 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x01ac9c8027c3c91c49b33d1dd084ed5b87c7dc92      1
      2 0x02b294ed96e1175b8c71071a303e8a7d2396b017      1
      3 0x03024d05b1aea10b64ff3af1fed6a3df9adeebb7      1
      4 0x03f4cb9e297ea659f30e09341ee7155a7d136398      1
      5 0x04294157cfba9ff0892f48f8345ea3539995f449      1
      6 0x05a13dcf55ea6d532f15284f39e02811fc183a8a      1
      7 0x06e13bd0a3cba08e61028f2326b9ea2ca2539900      1
      8 0x0843e80240f9304f3fe7059f25f3e1a66886a0eb      1
      9 0x09b2f0988b6d24336b348c84c2764d2b4f85142f      1
     10 0x0a87cbf5eec20d8d9880ad21f0260522ff5a675a      1
     11 0x0a98f97e89743406a8611e1b4219a073b60ffed3      1
     12 0x0acabdaceab63c08bc5aea63d7e07da7c2e42959      1
     13 0x0c481cd7d09bdd222351d52622d272367cb3d159      1
     14 0x0c6306c1ff8f0ca8432b761085d5ce74160a499a      1
     15 0x0da4438a9cc8d41fe9c0c6efd302a173ed967da7      1
     16 0x0e3e001a00c07e2cb3219d1ec0c47cc532d87804      1
     17 0x0ea8767de750442ec4a34d81bffcfd4417396f47      1
     18 0x0f9ab846449ca068f9714818a6ed0da5d819e9e4      1
     19 0x100f28c9b8b1fda897eadc775caf74e0f3560f73      1
     20 0x111818a51c4177e8980566beea68fe334be7b76a      1
     21 0x11a22b262e505d355f975e1e48a365b5d4811ae0      1
     22 0x144c704bf25f1865e4b24fd6596ffed7d92470b0      1
     23 0x159e5998669ec6628a6b670c2ef1ddbd93084698      1
     24 0x167a4e6066d6c96d5c012006f5cffc9f606131ec      1
     25 0x17e31bf839acb700e0f584797574a2c1fde46d0b      1
     26 0x185952b3bb31da0ae18354bbb90ae40adc840c33      1
     27 0x1a6f0dddb884854355023a2dfe26e9174f8e0290      1
     28 0x1a7f174dcd4211d41c7c4d7dbb3a0e4db5780c67      1
     29 0x1acb5e5c169aed4b9f963bdf786a8377bff678a1      1
     30 0x1b6265a40839a331ace2d81bc82b6104703c0426      1
     31 0x1c172d05b75178fc669d74407243cc932030f139      1
     32 0x1ce68e5912ef8cb4e83620a50b70edc900ad4759      1
     33 0x1ef43b94fcb00872ec73d60ff37f5c77df80ee66      1
     34 0x21b9c7556376efdbf9ee316a4ede0c8257fb7533      1
     35 0x22e8efe40ddb7f13b17b4c10f768967fc7a9f875      1
     36 0x22fbaa2dd20b848084545515fb04b9c846942779      1
     37 0x23602ca06e977c86339ffddad74966e824ab691e      1
     38 0x2801dc73a6dcefb364b959606e0c61234105fd5a      1
     39 0x28e6c1352950238be088ef2a741f3c8b91b9ffad      1
     40 0x2a0a412e0a0d436cca7ddba177c4dd9f29801062      1
     41 0x2afc032aaf6d7ead33fb05f9d42d874874dfb9d1      1
     42 0x2c45f65d254e85584b47ec82450c91a30cfc6be1      1
     43 0x2c9646bf2477a747897047b69ca1a1913ae5f611      1
     44 0x2d0ddb67b7d551afa7c8fa4d31f86da9cc947450      1
     45 0x2d1543711bd262108fd8a1afb43d8871f76c134c      1
     46 0x2d924c0bc8033e7866b90ca2a76cbf4b5714b11b      1
     47 0x2e299b7faa9b906117181a4550d828b1300b48d1      1
     48 0x2ec4a2bcd4f33c7c9aafab7cfa865ec15508bf62      1
     49 0x2ee5fc834903186949c75e0dc10ec245397bf1fa      1
     50 0x2fd7a107328dd6e5ac618667ff895bbadd7d7935      1
     51 0x301e2d2a98c5873ac27fd9eae85f0153959100fa      1
     52 0x30b33f902981b5e567d3bb1a3cc847b35f626609      1
     53 0x32e263cb9003f7fef329080bdcbc82f5cfd6c02f      1
     54 0x32ffe815277ff53dd2a73557664e229899e6501e      1
     55 0x334cb32bc18ec632ab04ee55157aae1ff165e093      1
     56 0x346aaf2e75cfccb82cff0fcb8d2cdc21d5656358      1
     57 0x35d7dd9230a1f079c907b53e7c5822b34e197a1d      1
     58 0x367dc97068ab54ba1dfbfc0fad12fbcb7b3a0d09      1
     59 0x37ea6c993c5f42e1fc3455d5781e4a580760970a      1
     60 0x387f802c6a9ecffc8b8f6bb7b80ea0db4cef9810      1
     61 0x389458f93e387fc568ca4568c231a64ffd0456d2      1
     62 0x39bba3c54a3412127bdfb0f218efea52b5d28300      1
     63 0x3bf55d8aad7ca0c5cfe1f697d7783437b9e034fb      1
     64 0x3d20c3f372207074df1a0ab5272c7168ce23d178      1
     65 0x3d3af253b037d3b22c4c810673c5d14de16d1af3      1
     66 0x3d91ad906c3969f86c588c988f721d5a735c6f81      1
     67 0x3fa5a25f48ba1b736761706801be4f639ca4853e      1
     68 0x405020c797a64f155c9966c88e5c677b2dbca5ab      1
     69 0x411dd34f17d5b6398f155f768ed27c32ad194862      1
     70 0x420ecd4ec65c0fea87d8dc5d16c2476c42466b65      1
     71 0x4220132c9df1ab7bd2913f0fd03297c90e7cc6fe      1
     72 0x42d38ed60a64d0b8c36f190e185d17db3617a091      1
     73 0x43b9778940c53abf2ca96ba900ff181ec09d7fcc      1
     74 0x44f301b1de6c3fec0f8a8aea53311f5cca499904      1
     75 0x462fcc4b3fc64e0dfd751424bc3b070d79efb300      1
     76 0x47a79ee280b4a6a8d68a3ba16e4082a765a2b20f      1
     77 0x47e64ae528b2d1320bae6282044d240ff67e703e      1
     78 0x487869b7557f0fdecbc5141da1d7c48c15c5b8eb      1
     79 0x4a0e071be98b9514aad70f5d2595a8ab23a7f053      1
     80 0x4c48d00997c6d23d6b47a9ce8caa6027d0068e42      1
     81 0x4d7e6948fb5ed2d92897de0605cd642eff69fbf4      1
     82 0x4dd8f136fe8a8202cd6395ad34ef5ae49c26e78b      1
     83 0x4f492b2a3ed9bf0c65d508dab4f7c5f5c04ca6c3      1
     84 0x5022cf478c7124d508ab4a304946cb4fa3d9c39e      1
     85 0x506452ab0dacf04db0ab1544b6be4019651ada90      1
     86 0x51b407c2ff79d445f73437690e2e5c376538f39d      1
     87 0x542c5648d46462207fba8a4bd7d890b4154b722f      1
     88 0x54913cc8ea17731d62589039dd0152f306473843      1
     89 0x557bd9e829f35f1d853c2c85341458892753fa1f      1
     90 0x56543717d994d09d5862ab9c6f99bce964ae664a      1
     91 0x571b8db726c2a29d237f88c9efeb896290bf395b      1
     92 0x5de26d392ea46ffc17131042e2584fe6ba46206f      1
     93 0x5e0737b90f1db90eb4f423dec067fd6c06a958d0      1
     94 0x5eefc3ae27be1483ad6b864b45ed72fc39e6bb6c      1
     95 0x5f656037e30a003862cf878db24ab5f537177fd9      1
     96 0x6232d7a6085d0ab8f885292078eeb723064a376b      1
     97 0x64ca4373a93e25822dcfdca0306e899a29e92b3c      1
     98 0x67c6083d540696c6aa1cf31101d78780e0d6bb47      1
     99 0x699990a8e7ada9e92c932d6e8fb365024fc74b43      1
    100 0x69c8c2923005d26eaeea9500d7602eff8c81c848      1
    101 0x69cb3b1de24e08f1cfc2994171b6c6930498f750      1
    102 0x6b29380bb9955d5b2df1624c5c6a72a5e69a6b80      1
    103 0x6b6ae848f555f70944bc99d736fe29fcaecf8b23      1
    104 0x6ce52498047b8279ccc7c25b41c95cd482525e54      1
    105 0x6e5632d334b861dfe4312c7ba34e175b60be0b5a      1
    106 0x6e88c16794cb212bdcfd6b6f8901af2f83237d3b      1
    107 0x6fdd8c800cbb81790cd48a20f90ce7e725f89c27      1
    108 0x70c8db61d09271f4c90950ba8c6cbaef918f12f2      1
    109 0x710852027bc25db82ba562d5b1499b6d9b02fa9c      1
    110 0x72703b554a7089f93ff1fc6cc6c0e623900a7b80      1
    111 0x729fcbb6e1289c88ce5113ec1c83a48a8e3c9f2f      1
    112 0x753c892626c4fa09d1aedc0e9b52bda97a4afa00      1
    113 0x7729cfd6841ec449c20fa7d3970d2e0d035c856c      1
    114 0x774a34da2ee2e2d242180819f1ee88783215f7b9      1
    115 0x78e37b881d078d4b2f90de0422dadfd4da50ed4f      1
    116 0x7c741ed01ee259feba4b7f9cac114f48bcafacf3      1
    117 0x7e737324cd6702e1b93c9240072ec9cc036dce21      1
    118 0x7ed69ff055d746d451f675635e9bf773accc6a97      1
    119 0x7f1796071eb91aebda10a03115fa3cd95efdb25f      1
    120 0x8179e48d959a4c41d5e0432ac05b30707149d649      1
    121 0x8623a32af48544b055fb6ae64f33eb43edf091ff      1
    122 0x8668681d2017c2322d7ba37070a8ea223ef3729c      1
    123 0x886478d3cf9581b624cb35b5446693fc8a58b787      1
    124 0x8874174a2366668d54fea6343f71709389563c8a      1
    125 0x88ccdfe9dd047b4cec4c1102d2d803e2d8bf683e      1
    126 0x8a063961e57a68a8a1f68930d758e1bde05fc6b3      1
    127 0x8af0b9a9b751e086122bc340188bd9d99b8c7ec1      1
    128 0x8b4567fa8c4715c27a682215a031033a8b544206      1
    129 0x8b4d0402f7b2f063f255214b7095b5911a257a30      1
    130 0x8b6a3e1f151fbd8a45539e0942918e63d35c6cf4      1
    131 0x8bd9c4b5ea8f6f14c9b14d830ddb67f3720d77f6      1
    132 0x8c2dabfea8751dd7901e35a015dc6c943ffa0d7c      1
    133 0x8d12c02671848b17c18322027a2578ea7afbb702      1
    134 0x8f160eb907a80517a3fa6d22f7cf20f552696a44      1
    135 0x8ffa8d00db5df56a20c30b737c5efdaabe140df4      1
    136 0x90af376346ca97d4e9d1e510e82543ef99b56a28      1
    137 0x90fa40f24362c24de09b1cb86df9f0b1f1b15a90      1
    138 0x92b1b677728e36734d59067a4ce53e5bf1f3a2ab      1
    139 0x94f052ca65a34d7f581bba95a749b1cf52689dd5      1
    140 0x97ece7185467c78293f3b796bde3704421d4fa69      1
    141 0x98004498c85c4b1997b3f669475a038bbcec2160      1
    142 0x993220d8cc0e48e7b88c96ec7f37cc3b6e6009ce      1
    143 0x9ad53944b52e271f1114028b918202cfaaec0a3d      1
    144 0x9e23d2db65a1b8b6f31fb57c47148907545b3ff5      1
    145 0x9e32560cbc32e1ea4c737d65f043a0d8f8310422      1
    146 0x9e640badecb7c628c6188b74488823e879f42a1a      1
    147 0x9f432a2964351a7830bd9ea10f65107677d73e04      1
    148 0xa04f4a4b7306cb72f30828834db01699362a4989      1
    149 0xa0f1de4882a5cd1851989d9a1e9bbe9b4604e9a9      1
    150 0xa199f8ffecafa4f3d500d3ab1d1c8e0b49c9dfd0      1
    151 0xa2917120c698fb5f2a03e3fd3524bda85a3eaef6      1
    152 0xa32c38646299818ccedc6401818c2e1639c39c08      1
    153 0xa490a0346808dda91aea6698cb19e4697d9fc5cc      1
    154 0xa4b61e227361a9cd9e62ca10946c27748a382cab      1
    155 0xa5214719feb1d07b66292a5d4d0a2712bd048435      1
    156 0xa6d4758ef49d2ae8e65e1a02216cb9744aee6b23      1
    157 0xa71000e72d38e6a84d7190f59fd3dfc73931c0e8      1
    158 0xa749f12e124790d45e62bcd3f7bf1d2218f2f21e      1
    159 0xa817b9bd1ece360e4a1692894a5ad1d40b889f20      1
    160 0xa90aa01ec98f390cb86cd1846f7c0fd342a07955      1
    161 0xaa1d3f5d45333e40467e989d472effac4da00da9      1
    162 0xab3ba2d668215acd23f7da923683a5ed88bad625      1
    163 0xac01ad9a3e3c3448a5a0e6fc798def034a01a67c      1
    164 0xac02b3653a6fd4d507df142a0337951e39c5619b      1
    165 0xac6e4334a82b8434b0122bc3c9b42d0a5f190d4a      1
    166 0xacb1568482f9af61568d5fd05a54bfe8b7e60ee3      1
    167 0xad102dacb54816ee8b128c63b83c7b5fc587f4d7      1
    168 0xaec3f7e735505c2b787b05d67f4adaefe3dfd678      1
    169 0xb05b7ea3714a95857e45629def2b9c3577690208      1
    170 0xb0767b217fb1530b064bb1f835c57c047c08ae72      1
    171 0xb196931ec22517b0510705eb56d5652fe73877f0      1
    172 0xb1c11a879c47debc22e3816e7b727fc8bbe3c8ac      1
    173 0xb2a18003067dd5f96ce5a51f276c01873c136e5c      1
    174 0xb31a4974499daad3255206daee7d1f3595fa259e      1
    175 0xb335326c7f2cd2e4eb09ce3d1745c92f66497b7e      1
    176 0xb37c5603fb0c6c2e2b62d0525affed9e1e5a6a19      1
    177 0xb40969f60bfa246a09593099dace2ddd543254a3      1
    178 0xb4627672ee52660a9e453ec541834e04583f3602      1
    179 0xb5374cac6cad6b025246f19d20b0d4151b640558      1
    180 0xb692704d590e224c67fa96830c4b2526fccaf3a3      1
    181 0xb7546d1b04151e1e96d38275af66eaf984fdad2a      1
    182 0xb976163a6f251c208bcf3196b9c4ddf927b63e72      1
    183 0xb9ad9d091c6841e640dba4cab02baefaf1134cfd      1
    184 0xba12fda058a14eb03c14613601c3a30d6f955196      1
    185 0xbc2ab46887142d546a976d56eb9a3d9da147ee65      1
    186 0xbd751cac1fb27d930a6a0394d97c77908d94ad5d      1
    187 0xbf270918afe2ad16093ddce904fc358ad337cefa      1
    188 0xbfb7969e75ee4e58620f74135d167f5bdd60800a      1
    189 0xc1966d218b7492dd9cd672a5a237cef4d82004e5      1
    190 0xc1b0307ff325527511310c7d6fd3248188742f86      1
    191 0xc4bfc1dc25827f39ad4a66bfc968456c7852a9a2      1
    192 0xc522289168311a765cf17c067f0118578c99cf08      1
    193 0xc5a2c3a524be0792519655d4093b44b988db4f68      1
    194 0xc6411ff69f1ec6bfb4b082f47db61dbedcab250d      1
    195 0xc6cf5a020bcfc65c6f8181c04cbb5ef5050fe28e      1
    196 0xc78cd2e1e8ad4a288eddafb139c9d0891ad01ae7      1
    197 0xc7a295b1b2df1efb82aa204958b98ac30171cb85      1
    198 0xc7d2d152dda8cf7a7860288c3db9a1a4ce29162b      1
    199 0xc8cb180415b83f3f9a61691d246fe39a74c6a41e      1
    200 0xc94a284808e31b9ef43f9c9a48933e5a268cff9e      1
    201 0xc952ec84dd5d8ec32937a8e4105922c664564450      1
    202 0xc97958ff5370c56b7e0e6fb58437f00702714d49      1
    203 0xc9ae113b8451d0afc8a021d796322d545a7e7dbe      1
    204 0xca339bd4739227f71b646f8b23ca098c86f6c3a5      1
    205 0xca3dff8c740dee29528916eb049cea48f500d387      1
    206 0xcb85cf75d53fceb37af86b475903817db2b99d17      1
    207 0xce8ad80ce1a979381d209ac230d02adafb9fa897      1
    208 0xce990032a3ed14e197421270a9bec9e276bf2f31      1
    209 0xd193a7d7b231c204b76b9d638768ea602de515f6      1
    210 0xd1afbe4da2a5adc6faf30fcbea9e4eea0ba8c70a      1
    211 0xd2f2a56affb67a9ae2499ab9df1689ee78fad89a      1
    212 0xd36590461162795ee33099b2076a0d4e017ae17c      1
    213 0xd4e791d799b8873d86ac06a5fedf2f1a75f639be      1
    214 0xd537a2b2b97d104f2d9c7a84377fc11573629085      1
    215 0xd5ec003289265705727b622f1700fe814e54ca67      1
    216 0xd6b2735e290ba3726d25c9762098cd26a16f023f      1
    217 0xd7d941ff2890bda98f40a5dda0593d239a603f09      1
    218 0xd939fe2e4776ccaddb823a26eacb45c66d8ad5e9      1
    219 0xdc7d0d897e1b7998c780d16b3c08482a74e71f33      1
    220 0xdcefc9ff1e47e458e56959154c1cdcf56003d30b      1
    221 0xdd5cecf2835d465d490f79ad9773a848805a3219      1
    222 0xddc27a0a0c24d60b607dbc7b008f2016585e57bf      1
    223 0xde112b77a1a72c726d8c914c23dceaa53273c351      1
    224 0xe05e90015f6577ea1b1eb682ff8e1db1b7b6bcce      1
    225 0xe74ca58ef724b94fac7f2e09e2e48536a5c1ad03      1
    226 0xe776de32c9dc8889e2dcf1cf0606010a51d1abb2      1
    227 0xe96ba1a10f72b58934b9ac0e3fb471d2ba65b757      1
    228 0xea0ed16746257eb2bc11de2fefd63cdeece23a98      1
    229 0xea39c551834d07ee2ee87f1ceff843c308e089af      1
    230 0xeadd2d51e75a8b42302adc2aff0157a4bf626adb      1
    231 0xed1e25178d7a6438c356d3459fc3efa9458ad52b      1
    232 0xee2c055f7706b9dfcd98cd5a23d5629d6316c0bd      1
    233 0xeecc9f94c674161e84ab0c60c020432ea8f88bc0      1
    234 0xeeeb5dd03eaf4ab6b72949191d8ffcfe27f7d973      1
    235 0xef04cb0003ca57dc7bb563d5ea3248241b145bdb      1
    236 0xef5ab90a44b68d4f5e3f6be6af4bedb12cd2c66e      1
    237 0xf12e159643edeeba920518cc614820ab5726335e      1
    238 0xf28fc8d0eb98a34f2b03d384f7db6f1e7b877a33      1
    239 0xf2c5f1fd977dbd6de9d04bc4e62dff722d4bb1a1      1
    240 0xf3998eced96ae3cfe880ac9d2637b4118da48e22      1
    241 0xf624e9324f9b330cc0289775d7b91e945e881134      1
    242 0xf74b25d3f6616e51b2d8be6c8cec54b045229655      1
    243 0xf9c8d277b7485d29a0e76bdf2f53f584f9050baf      1
    244 0xfa69519696e9d4abdf7d054c3ba44d32fe350ead      1
    245 0xfc5ef50b9d7a080cd620f404efdfa287af9a3ac3      1
    246 0xfec4465b6aeac9c90e552ae7766bdc0ee0d8dbc9      1
    247 0xfed52d251e31178ff8bf4a0d611721c544f74fc0      1
    248 0xff4f0dbc9ee7583c0c61c7333a5daf8d0ab4aeb3      1
    249 0xffe6832850476eb6d5ff184d747ed84f1b686aa9      1
    250 0xfff39900273ffb1045c7cfde62df1720b63fd6bd      1

## Allow Memes2

``` r
c(allow_memes2) %>%
tally() %T>%
readr::write_csv(file="allow_memes_phase1_150.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 150 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x0007fcf5590179336c826a60c80976623c7bbc29      1
      2 0x001442c1a4c7ca5ec68091fc246ff9377e234510      1
      3 0x01e8e9927d7c6b71671865f05783c8cbe04cc559      1
      4 0x01f4ab9ccf822e74cd514b1fc16068e749d37b1c      1
      5 0x039649f7c2f548692184da3fedf316f58e8356c0      1
      6 0x03ee832367e29a5cd001f65093283eabb5382b62      1
      7 0x04490fb053a8ff110bea35f22d955c0092aae5f8      1
      8 0x0573383fcc26eb066010d28d96ab02f7c5cde67b      1
      9 0x05cda24eeefd1f24f18dac398f5a612674d3ca5e      1
     10 0x08e146bb525bd8391a4e22e665325de0f499e9c0      1
     11 0x0ce390f18af702cca546297845a4a51d102123cf      1
     12 0x0e757c27de6feed6d9f43120943ef07d89335483      1
     13 0x0ee7d116e0c89aaefb3c3653cdb71e384766ad11      1
     14 0x0f615319d7ceed5801faf6b13c9034de9223a3ec      1
     15 0x0f9c4213c040a39db2ba6f833472f739e61710b4      1
     16 0x1187ec27eb94e65717af65f3ae8c326bd8bb47c1      1
     17 0x12c4908dc21e88a8f268c3f363098eed251c1aa3      1
     18 0x1379aaccf761490ceba36b6ec572e5dfca48273a      1
     19 0x16dee223fc168abff7b979813cdf15866eed7e8d      1
     20 0x17af704a99b0394f47fa8a3eddb2f4d8d03cbdc7      1
     21 0x17fdd5ab047e502ce1faa065f49170875b083a47      1
     22 0x195f2cd867c1c02ae6e61a14ec53f5f01b440edc      1
     23 0x1c9ea0bb36a9fa526f76b51dd55ce1f23d352c26      1
     24 0x1e0a980dc878e8af6ed79ad189a7334d2cd737ef      1
     25 0x1fad9f65b39d235469bcb59bc664872b93eecac5      1
     26 0x20542dffc2f7a51cbc087f54fcafd9ffd8b5c899      1
     27 0x215fc4d12967ddc2432a7b8936f2849740edb7bd      1
     28 0x235ef6f6bcc455bb284ebefca68754c030bdc1ad      1
     29 0x2595aaffb353e812bb5cbb63f7f62e36fe528f02      1
     30 0x28cf5d9d465dfaf5c616958ef8b23dbee567e2b7      1
     31 0x29c02a5e462e187ec7e1c7874d012633f12c89d0      1
     32 0x2adfc86a4e073169ac5f8c850a9e80c90383f3f8      1
     33 0x2c52248bf9f5715570ad007ef4d9c660ed8ae2e7      1
     34 0x2cfdaf06e39e5d9d3c9afe20362b725b41f68725      1
     35 0x2de9c8ad139793ad2f72628a960f5265ed9ef28f      1
     36 0x2e8f6f8662593134dc8311e312885921855332bc      1
     37 0x322f3f6c176027df2cd524549161c71ff66012fe      1
     38 0x340b69daa84271b53a4b31d801ba72d19e6c934a      1
     39 0x342522ae61de25d48c66807a2cecac4681be3d33      1
     40 0x348d43bd8daeb9f13611d5f0838225e6a9d89bd0      1
     41 0x386e0ad23a9f182d884ee2b8f61ec8c626b94385      1
     42 0x3c554c31c2f1938bdde970ac9add36f6264b55bd      1
     43 0x3d5c819e5ab0395a8ff2ed2e27d42b92b91f5b2c      1
     44 0x44f2afb689aea37ab17feb2420f17f9a3e851307      1
     45 0x4705bc2775cee0fe266dc2a376010c0eb1bcb474      1
     46 0x47e054a8fcf3e27a4bed979c33ab65a5883fe437      1
     47 0x50002a9b8e9938d509de84dc3eb3aabfbec1451e      1
     48 0x50a9f0f42ecde1df805b1ba6e885881accef049e      1
     49 0x50d086175cc14fa5ae008591d72a93cf22b48e32      1
     50 0x52349a2c4ea4e4b94ada3d75c9d3a318c024706f      1
     51 0x53edcefe31da6a0051df17ad80e19ff93c490b17      1
     52 0x541237a3e049f2ef1105694b491152f450aba4db      1
     53 0x563a7e710f0e6e81d68b4a2fb6c273f179783a2a      1
     54 0x5b23c893ab99fa6d9ff3532d10a9019cae06383e      1
     55 0x5d1566f78607473f386315847c57c0f972ca5ab1      1
     56 0x5d20e6c0db887b5c3281edfce5bb7677565e2b8f      1
     57 0x5df6dd99718f32ae362b41e78b7bbf66ebccc179      1
     58 0x614a62584658d555cc28b3aabdb691e85e002c8b      1
     59 0x634ae4b57e6246f0318257743e5255648f9473a6      1
     60 0x652d7e1b706b10de0bfee179c1cc41df73d3ed34      1
     61 0x66567f4ec7743e58713f44f083da3de78a52556a      1
     62 0x66fa06bd24294ab3bf123153e49b9065cb7672e7      1
     63 0x69f15ca90038b437a1027f9ea40dd81638713325      1
     64 0x6b4fedc18a1e8cc0523e56cc9b45080e1ea00005      1
     65 0x6b539c723716628725aee9e7e26796cfe1f38fcc      1
     66 0x6bb59e15545dc9ab0949410cb3d8402ced7fef98      1
     67 0x6cb575052aa5c146de209ab813c3f6ab82424bcb      1
     68 0x6d35392121a6687fe19d6159601b17517f73d0ea      1
     69 0x6df000635d86613626c5208d7c9d71b84e091220      1
     70 0x71e22168b702bcff528b8974cd4b723250b67609      1
     71 0x7564314c19620a83b2c87e6d2febf0c3f846b1db      1
     72 0x7738e4dd4fc3b6c95b8925f078363924c0f0b428      1
     73 0x7865ea60cae35485f2a0483a385052164c487e27      1
     74 0x7a3c82c6b4ad6ee11ecf7f395431ed1135d6e46f      1
     75 0x7ae4784a907460858231609f565bd9580f609b05      1
     76 0x7cef7db4289005fd56dfa6c73bb2be6ec13bc250      1
     77 0x7e840de1d4987a189eda6440b9282edd452403d1      1
     78 0x832834fc5b0b35ffd4cf697c4ec08815ad8d6a52      1
     79 0x83ee0325fe8dd7d2734c9191bd5f7c532951dee8      1
     80 0x8a7a9210bb4f3aac040a7c692e2ee2943bbb4efe      1
     81 0x8da03feb7174b4b2a6b323f3dc40d97f3421cd07      1
     82 0x8f4b933491e1319799229ed9d36b179bb859d705      1
     83 0x8f8a1e5113b11926950185d74567dbafb0aece0b      1
     84 0x8fe89aa22200d1306aed5dad7dbad89c8faf8e26      1
     85 0x912e844820eafe9a6c2df0615dcfea91ff32ce75      1
     86 0x91cbd90345578b5833e2bd53ecd22846234d87ff      1
     87 0x93549cf8047b3e305fb10df7110da213e9429f9c      1
     88 0x953b0f8afc70e0fca9e2e43b0a4914be002c4e94      1
     89 0x968e708d939feb55d914e233cf1097f4adbcd5e4      1
     90 0x97a45305513133c62b2e3a1f2181dfb5ffdbb450      1
     91 0x9acb83a514399c9f5201f55ddab61481d0430943      1
     92 0x9c44bf6f8a7291b7a04bebcac7c5cd7fa7771ab1      1
     93 0x9c7471c6d03cddab9ef6e1db6ed93fb8f9328072      1
     94 0x9cfe0d47672f0d9891dc312d242349d52d8aba8d      1
     95 0x9f64e6a575f50d7c8c5aea4be962f86ee2c5ca31      1
     96 0x9fca5cb296333aa780d3367951f766ab3d7d4098      1
     97 0xa0b35fe7c9819211896d51b670ffebdbdce67fe2      1
     98 0xa0ff0e41d6847b1fce33a721a766e9e040027e6e      1
     99 0xa1cecd5e4f6683189c3d4f832a16600c20e5b42a      1
    100 0xa21e1e6c69dfed02b6a69c4c89ea46a1e6e31b86      1
    101 0xa40117a1394fc5cfb3576fe12632381b0b79c0c0      1
    102 0xa8c4e3ce1743d0f2a6c227548c982a7c40569940      1
    103 0xa96d4549729c2a826237d91b3a3700cad7dfec4a      1
    104 0xab0e2f3c3c29deaa7c3ca0dacec97a33d5c26708      1
    105 0xaf12a9af19331fa9b89915537ce9d41bce5e4c5d      1
    106 0xafef3bb7fc3cd8ae083672b65f79e6cefd4ac619      1
    107 0xb14c35b69ca390c359b75a6cd51b30429d8024e0      1
    108 0xb6b4a02dca517564eb98790ff67d42b5b37a3d4e      1
    109 0xb8efa945cf7f35854afb80e9ac05f5c1dc172fb3      1
    110 0xbba46be446a0f32588083f4389e5e77786cf7652      1
    111 0xbeddf3e43015f5221f5d1ac0fd0d2ab3352d2f7b      1
    112 0xbf9d462c9c6880d44001bcb8ca4a38a351d583d3      1
    113 0xc06a00899ebd1481f243ac4ec92a276c1ff26e8a      1
    114 0xc0ced4439c1f0871f16a46e13fbe41fbf61ba265      1
    115 0xc11a862ba1125a1bc6b4ff11b2ead6b8e73ea52f      1
    116 0xc2978441f46a76c60e0cd59e986498b75a40572d      1
    117 0xc5825a27a03ff2fba429f49d7e337c4d4dd39400      1
    118 0xc58326c7020f26345f4568cc09daddf019a8e6d4      1
    119 0xc97cc6cc4b07ad3f5919165c99ce43437d6114a1      1
    120 0xceae4ca7d082856d140f3672780029d6e90c3dcd      1
    121 0xcece625176cead11c8d2dd6149fda2496b0c192d      1
    122 0xd413f436a036b9773d7adccaac10242e27b9da74      1
    123 0xd4f358d4a415b4abb6f6deb40b86d7db62562960      1
    124 0xd530282d853169a23822762af0dcab045d6295d3      1
    125 0xd7192081e5f481364c190022f0012a729fba37a5      1
    126 0xd74e767c77d2e9f9e467e7914f2379da81b63a44      1
    127 0xd86057c793bdfb77bb11dcabfe054ee4fd86ebdd      1
    128 0xda23ab0b2636c3f9806a868278d73aef2439ab60      1
    129 0xdb9986bd0596b8a4873b09b4a10b81b13f2c9ddd      1
    130 0xdbc7a4afadbff03d1ac183c9f664a95775c6d52e      1
    131 0xe01a97fc4607a388a41626d5e039fdecbfc6acd9      1
    132 0xe262b5a7fa6d23be9007eeb93833d2e51d89834c      1
    133 0xe5798a530bb7105e148d38ac884f05c28ed8e804      1
    134 0xe5fccbbc11e4a36a77e9d8e47a9b28ec44e2e2ff      1
    135 0xe781fe7a6f65ee6b3efe66ed8f7f5c1e9f01cc55      1
    136 0xe817ed328e561507dc84d022f42b46f92e003002      1
    137 0xe8cc7c453eca25450ca16d551db93d5f5740e054      1
    138 0xea9f3a983d965e582c34eb852c18babac52050d8      1
    139 0xee958e45f3464d712b8830deb5875c8ac105f698      1
    140 0xeee4ebaa421ed7aa32b925bbe2fe7506e33e7cd4      1
    141 0xf0753b5515c095cdfce5a0d58af15dc5aa46fa94      1
    142 0xf1e1c701b49b1dc2405a9e8ef40f9f88802b80fa      1
    143 0xf44b63f62c7b6c59a883fdc67bdcd692995b8bbd      1
    144 0xf4aa421960cce8cf25190af71acc6949e848fe46      1
    145 0xf5851672ab7fc8729de7472dd273066b5e3b4de5      1
    146 0xf5d4a3b9fb77d1d5600a10e0e1c4efd12f9be751      1
    147 0xf6ec5b0d097178a0acf493afdcda2a54545ab0f3      1
    148 0xfc61d7884b047f9f4bf56e984f773f1bd5d51480      1
    149 0xfcbca52b23db28470eb97680d18e988d8b60b608      1
    150 0xff1a6681aee53d032e8f5fb3d27ccf29d493e922      1

## Versioning

``` r
R.version$version.string
```

    [1] "R version 4.2.1 (2022-06-23)"

``` r
packageVersion("tidyverse")
```

    [1] '1.3.2'

``` r
packageVersion("magrittr")
```

    [1] '2.0.3'
