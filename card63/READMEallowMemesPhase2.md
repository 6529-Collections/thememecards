
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "memesphase2.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:5679        Length:5679        Min.   :1   Length:5679       
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:5679       
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
```

## Allow Memes1

``` r
c(allow_memes1) %>%
tally() %T>%
readr::write_csv(file="allow_memes_phase2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 250 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x0012dbf6abe83eff2aa882f22c34077886460199      1
      2 0x0042cb6be82fd326ffa5fc2edba67fe270adfa98      1
      3 0x004849f2a278863184389d97ba4f77bcb5ae2a63      1
      4 0x01d6f0b38e2814dd8ef0163bfc05492eb7ea9b23      1
      5 0x021d5abea6efbcd5dba2c8ae9237471448ea0856      1
      6 0x022b266d1aa39725a4e64aee661f0277461e4e39      1
      7 0x0296edf4eb266dce6e3cbf44565beeb4a500ba78      1
      8 0x02e02cf22366d88b917e338ccacb31dd7431b0af      1
      9 0x0340da03212bf3448e99b85f886f4d2017b0ca78      1
     10 0x0395ed8151455465b5b41d278f911d282225d62b      1
     11 0x0396e41347f6a8ca45b4a3efa82c436786dd44b5      1
     12 0x03e5c7cd68bd09d1b90d9d6b376fdae894baaed1      1
     13 0x04feac68a244f7ef661b53860e328d56120744b1      1
     14 0x051d6381c7fcc991df10b4c65946393150a7cc6e      1
     15 0x052564eb0fd8b340803df55def89c25c432f43f4      1
     16 0x07fbc53d2ffba8a9cfc0aee2ae69fd691fd6d9ce      1
     17 0x089660619d26b6ca62eacd74e2ddd6089024fccc      1
     18 0x098b1c2a2f36e3db6d61899378ecdaf6fbff6dc8      1
     19 0x0a4854c02d9c7f25b8e81c776c0a15ac922d1bb3      1
     20 0x0b358edd6e7eb4b38af3ddd7fcea775254602532      1
     21 0x0c973219372daabc365e3b97e453a500c482264e      1
     22 0x0e0886a3fdbfbc8fa8ec7fef815d3e3b4ed0888b      1
     23 0x107752288b215467de25c16119787d715ec8e26e      1
     24 0x1370006e1416c3651f1bce41370804a1868c5b3a      1
     25 0x13c942e3f8be4faf966ef03e6038b33d000db22f      1
     26 0x142cc94ba3b7ea0d77d06cfc3fa40aedc186f601      1
     27 0x14ad49660b0d69c22a5a6a89db4a2f767dfb2e3e      1
     28 0x16dac8fcb28cbda1db793a612847a0be1a04e554      1
     29 0x17e3c9410737ff41a59d5cbb00aa178956bac45a      1
     30 0x184d23f3e045f53502a0513119f3254f40a0af0d      1
     31 0x1884ef1ad5254c3fe32fa0c0395eb4fa7316c8a5      1
     32 0x1a7e6c5a59f070681640d59222757781fe090b89      1
     33 0x1aae1bc09e1785c8cb00650db0c6015bd73b0c7e      1
     34 0x1b967d5a3a14f661cc3bb0632cda3e152765ea32      1
     35 0x1cd85d3f4392f9f623afa07d96109056cf154a02      1
     36 0x1d14bfa2fedcd3f4da56d42e97582f614519d075      1
     37 0x1d8e7c8d5ec8d2003ff65c08339ade30e0f9a0a3      1
     38 0x1dbdf2281b5e817498e8ecffcb4e25b64586e1d7      1
     39 0x1f26de38170502e1898083096ff114336827749f      1
     40 0x1f2f6361023b414ae8325fbf14b6a502269c346c      1
     41 0x20357eb5dc98da197c332d9fdd2a38ef58c91fb3      1
     42 0x2227cb63299db1ac748347be2eecd8238efd593d      1
     43 0x2377aecfc8838e04af76d3e8dc9192d391f00769      1
     44 0x23830f2c4a3ca2363853964f2a449d13339da888      1
     45 0x25a6a662aa143ab46b71a50908dd8a74ea56cab6      1
     46 0x26613696bd07c7a6e43c94ea457584f1a5d9f979      1
     47 0x27d4fee2fbe69bc02be96d1e146abe0a29797424      1
     48 0x282173335ae1f328b34eb9acadc052e59e70a89f      1
     49 0x292402b58d40393db796380cbacdc1d9d9971beb      1
     50 0x2937769116151d3609bfcd7b5efd23dee4f4d913      1
     51 0x294190b511ad6c7ca48c6d2aae3dfd71e65bb8a5      1
     52 0x29bca487c390fa4773fe10839af969e13f2e5844      1
     53 0x2d5b1521ff82edc319bcbe62ef16105067975836      1
     54 0x2f8f6b4a198423d4a6af82bbf150bf523e70b43d      1
     55 0x3019ac23e58d3bc8c24e4bd86c1c09d804d998fd      1
     56 0x30a933add6fd601d152aa4aaf7620b5e27e88888      1
     57 0x311399b3d6f728973bfe8ee06b11ea3aa67b0859      1
     58 0x31658df07a63634fd15ea2ab8ca6a739cecc0a55      1
     59 0x31c72d0d03adc76456d3c14a4fe19f8aa8307c16      1
     60 0x32113ff7ea112ed8a7151528e7468e648d0a692c      1
     61 0x3432b45b9ee95bd5c31a726b936cf2ec719a2153      1
     62 0x34a77cc4ef2b1021d5e82df61b4441cce4218158      1
     63 0x36be14c0d731d70333e4f906a0d33ef65182fd0d      1
     64 0x388892c25255c89f73ce9e22515f4b996f1768b7      1
     65 0x39079407a3be76fa77603858d0d8c753c9bb3925      1
     66 0x3960554bdb91f39c9170e338318dd0209c8be7d1      1
     67 0x39b055005a4c91a01a45514a39f175252fafbcf1      1
     68 0x3b02a822021af8ea7718b79717fea58ec3da11fe      1
     69 0x3b7d2e857c5993eb6e28c99a08edd69002bffb57      1
     70 0x3be58fe702ea3239ef6979b48087aa0e3f9d7ecf      1
     71 0x3caeac7ff83b19f4d14c02cda879c4740b4f0378      1
     72 0x3d0889c3cd2f9f9b53b7132b5e9a0c90d371b9ba      1
     73 0x3e34060586e19a0bf0be74267888fbdb0a8f7e5a      1
     74 0x3ec816cff24f6be50f0e3334301ff51f3f9d239d      1
     75 0x3f5e9a8578aa0ebf9c897147154efa9fe18f419c      1
     76 0x3f659ffda1f14333696eaf795b14ad9c4cc71f61      1
     77 0x3f7dd0b106b10d06bb4813715b4e24d9e626dd64      1
     78 0x3fd668561cdba0b33630df1c167dc4934db775d5      1
     79 0x407e3b220c084973192cc76244ff53baf88363ff      1
     80 0x41cdf42b4d22b3edfea997a2f03f32b156b135a0      1
     81 0x42b080ac985e13e81a0257827a766011bcf2a482      1
     82 0x44b1b486ce8d9c82f605626347a7b1f74add11de      1
     83 0x4683d126f02e6940922d2a42938b73eb96b889fc      1
     84 0x47d4f20ae83bcd350105f199f900e6e6104dab6a      1
     85 0x48f1b764b8cd851025b88fa28f92cb855b9079c0      1
     86 0x491befac53aac201e97cd6f4c750e4a958d22d46      1
     87 0x4a661468264d6df2f8557a060e0aa0e041870404      1
     88 0x4b3e362b40fcf2a42429a422d9b5f10c70a01575      1
     89 0x4c12647eee5b2ab6c11cf9c531f4d62e50e67d4c      1
     90 0x4ed2b2b013c4e5b2e33c090818d49e7bbd6103b7      1
     91 0x5147c9b5f453445f8f47fa9018d7aa85e012eb5f      1
     92 0x51ec5e1b8b3c4c6bae49619e657f94c4ad577b45      1
     93 0x524b7c9b4ca33ba72445dfd2d6404c81d8d1f2e3      1
     94 0x5301661d5a55e3712537949c65b86ea72cf41519      1
     95 0x55b2b6c905c119b00a01ed864cb48b55af307db5      1
     96 0x55d3225ae380b3e3c8896bcaa85f35eac22e3bec      1
     97 0x59e891b2368acadde5bab30c8eb1728a1e49c4f3      1
     98 0x5a1848a228391145d5b43e0ed0b52fcc51e2ba75      1
     99 0x5a381564aeda17faf048d0eba3ad7e7015463f78      1
    100 0x5a3e28c2bf04989e6a7506a9ef845ae2dbc6d90a      1
    101 0x5ac9982211070c077213471cfbd496a7af34cb14      1
    102 0x5b96cc343f3f8521748da96584f4a0be07b52b0f      1
    103 0x5bf4ed7f13cd6441834aa0a1ad65d0b79d478951      1
    104 0x5d2035ebe68f4ce03d6822a68472d82c74af18cf      1
    105 0x5f20abebe0ba11a9dc0f08e654e416bbee8979f9      1
    106 0x6123c96df7f6c34bd0a8451a38e2555b277439d3      1
    107 0x6590ceaf9227887c43c6b3eeffab08f326a1aabe      1
    108 0x66fa6d3aa74a9c2b0bd1676747dfc2f6951c936f      1
    109 0x68b8eae09441d6b6616eee770f31281e59319803      1
    110 0x69fa8c51f45b1593fe2832dac2091c40c1e59e6b      1
    111 0x6ac029ae2e792a56354c544347f38d68db618492      1
    112 0x6bdead6b984d74baee2da16bef7f58319e659398      1
    113 0x6e9e5efc250965ed45d8534038ebf0ac355bb6c3      1
    114 0x6f3a349df0dc8e88263a1ebdeb59a31be094fb88      1
    115 0x6f8c8174274f5d77824f109194505977868e1108      1
    116 0x6fe74660d9ccad62f5ce15657e95453f1c54b6fb      1
    117 0x7320dfa95fadd9bbc20bf8eca42587c44176e9a7      1
    118 0x73ffb5a957ee669930c70466ffec4be0898fbcfa      1
    119 0x7581a5a10ab7ab75d4cec577e4669893818fbbb6      1
    120 0x75fe6f9af2e4b4fb9e86ebe26bb31162d292b315      1
    121 0x76d25375d054eed0721760f3f1693a323f5ebe4b      1
    122 0x780193b15b2db9b9f4846dcef7ebfb3e2b751a8a      1
    123 0x78be6536548c362d4092d3a5c3baf6aa27b80ee4      1
    124 0x78de8f6c33ec16d7bc5e681c885ac910404a14cf      1
    125 0x791967551708fe99eb2cd8dd580ef98c61e67ac3      1
    126 0x7968f135d89e0538f2d74f5fcf2cdf4aa1ab69da      1
    127 0x79715c9b89c43c798743735ed136dcaa3fd410bf      1
    128 0x79dd11ee6b3970dd46a333ff03c9fa775680ae7d      1
    129 0x7a000e0fb9f25c03096baa26577141b84ed01da4      1
    130 0x7a832d1984a7b1d53418bee78a50a876285b1b32      1
    131 0x7f270624480605ef2a2212eed24b886aae1e2e7f      1
    132 0x7f873ec58d567e78a2c1dca322cbdb6d490ccb7d      1
    133 0x81787ecf8d91c1c6827f28e68e0ac3979a1263fd      1
    134 0x8272262b6f5c8b5fe307c99d4524b29f0eeb543e      1
    135 0x839869f27ddf564995726a32c15a56aae85a354a      1
    136 0x869c9aef10d4b33bdd8d26d3be5a15a6456f371f      1
    137 0x8754e1ed406a72f8deca5b4c9654ad2db698ba83      1
    138 0x87ae3929744864734cd009ad3be47d97fccb54d5      1
    139 0x88ceca090d7d810f0d3bde0cdfb1cf1f2301bc17      1
    140 0x89e8493a292aa54734f7c2ff4765719138f18189      1
    141 0x8a3add4c72da23e647b76150127b78d79c5f0847      1
    142 0x8a4a7fd2f22096cf19853e71d69730d297a097ca      1
    143 0x902552dee5e3cf54dc7a1eb97017b85788120055      1
    144 0x90a38dd336e281462b7e01bb6b63d85be22f6a6b      1
    145 0x914b1b26f461a8797d94f06ed41e01f72b0484f7      1
    146 0x91aca13b42ac160c9e79a3cdcbb42cf68c1f15b4      1
    147 0x920af9763ac18429f36c92727056d3606e4921d5      1
    148 0x927705d26bd2aacfd0ed7492e8e21a86fecb4d1b      1
    149 0x94042981fbe827237acf0d49bbb51fa8c1c56aa1      1
    150 0x94a5705a0f0927099638fb068c6e3ad77501d837      1
    151 0x94e310f2201daacd50027f5e49a4640cd7532fbd      1
    152 0x95f84e30595b324f64ae68eb6ab925f5c4b02ce5      1
    153 0x95fbffcc144a410ac03e7233452309fde653c9da      1
    154 0x97c2bea6800fe544353d42af587d0b110794068a      1
    155 0x97dddcbb6a6209cb02432b8d7a6e3aabb0372b8a      1
    156 0x98cba78ba705efaa9c9d9015b5c95ba1e2c3b2cd      1
    157 0x9ac0a2c799713233e8329e7267b2f7b4ecfb2c5f      1
    158 0x9cdf6db894d781ddc0fe592d96ab88fa25a70254      1
    159 0xa15b668b95fe5ea740da21db69947ac8bb400c51      1
    160 0xa1631f34541e352cd70e1c6902ca586c57d2e040      1
    161 0xa1c384289a9cafb44a4f792acf2e7f9ac5e5f3ad      1
    162 0xa1effda93fcb0df8e40b98bf10219be8447e564f      1
    163 0xa28fd178c686beebd6818d6772e56f6e7a6ff5cc      1
    164 0xa55048cc0369e412cf7db3d9bb30afb51d091a12      1
    165 0xa647a51a1af1ccae559f3ea0addbf2a42285a3b1      1
    166 0xa6c579879252d8abb6e9150e4aa0196ba81c7b27      1
    167 0xa82035c47259afcdc8c399a25b51f3f54e8312a0      1
    168 0xa8e376248fb85dd9680fdbeecc3ec72e20c37cac      1
    169 0xaa3cb22341b2118afdd12f127913762ae66142c9      1
    170 0xaae33d661bb2604158748ea861280231600cfe61      1
    171 0xab8270c8ffc59d00c8cf7350518faee68312a59e      1
    172 0xabfb3a0a0e4c1bc2ad036f4531217e2becd215ee      1
    173 0xabfb849eedfe3a34002372e3e1a8ed945d28a02b      1
    174 0xac4dcebd7a69144a9593f3459bfcbc2db4ebf09d      1
    175 0xacab4e4e1d44b9017a28ed430b040340a335cb59      1
    176 0xad319a7290a40ce1358f031b692a0cea5826b557      1
    177 0xaf44539ba68c318a0de5e1d7899a669f374e180b      1
    178 0xaf803180a4e75eae08697b36fa0bbce292fd3949      1
    179 0xb12a69982634121461c9e4346dd9cf9391cbc80c      1
    180 0xb1bbb5c35e0c9dfead55624c002467e166ec8de6      1
    181 0xb1db41aa2484e3f5f5a510e07003c29fd1b0f115      1
    182 0xb1eb435fec01c94098a692a9930af6dc12f24903      1
    183 0xb34017d870c7c9365cd3f51243161b596ce417df      1
    184 0xb5daeb78c105f2b2ea81e327b3dba5d012e4f3e5      1
    185 0xb5ddb5ed294826f7f211754d7c05cf983b1a3065      1
    186 0xb6999ce618285691c038e123267e3ca621303bfe      1
    187 0xb6c469ad9f2d399c5baa1a8a8861eefb7ac91fbe      1
    188 0xb855567a17c266c1d82a52bb093ef5b6a66deb01      1
    189 0xbb1ec73938b9df4baab4f5c43af96385a862811e      1
    190 0xbec35e379d6d421293cf55b0898441d24f197024      1
    191 0xbffec59f5e85f37af43a872dbeb641cd18921f04      1
    192 0xc04038c0d0b6ee1034cbde6239f71ff10b81bed6      1
    193 0xc1b23d8514d2332a91c565915a7f4e889cfd46b8      1
    194 0xc2543fe877e3cef40ae489d247dc605ffac4399b      1
    195 0xc2baa32b57a91b81d35ef3052319873bbd16cd12      1
    196 0xc2d9eb979f4679adc0a4328ac5caa096a03f18c3      1
    197 0xc3082af35fe9747d996c8913212e7cd28982081a      1
    198 0xc31ba2d3275a397c43b1a4ab707cd79adf310233      1
    199 0xc5ac4bc1c78163348aaee15a62aaa6c5f5c4e724      1
    200 0xc5ff9bb746a3e54f72853db38e81522df7d40680      1
    201 0xc721b0b9e17b32dc9f981eedbe4f70be04b96415      1
    202 0xc98a513970f320b11ab408c12eef176292e227bf      1
    203 0xca33ea12e2a6bb8e5bdde2a102bc61d78760c96e      1
    204 0xcba35e68a8e99cd32a31e37aa2e57e2eaa727c1b      1
    205 0xcc47fcbfdd6e558fb39cf5ef4d92e59890476b86      1
    206 0xccd3edb8cb78074b274f0bd86e013694cae3fc33      1
    207 0xcd0cdbbea19a7932487feee640279a23db9d7de0      1
    208 0xce435038720f5db23cc8627f825382dfe85a7e06      1
    209 0xd038d146dfab6b92df8ce2c92369f09375fc5b32      1
    210 0xd2a6ba13a6653166b35e712caef55d34acdd939d      1
    211 0xd311b220976fd80718ff10a8b9c90505697f5cc0      1
    212 0xd3641bd03a67af07550b049065a19f84333b4b5b      1
    213 0xd3770a917176446d5c0a3bd12db8e9327333c2e3      1
    214 0xd4c9aabb94d637c57fa39d32f7f249bc91f39380      1
    215 0xd4d365f2c4c136d0ba998565fc8cf28c5bb3d700      1
    216 0xd5ff82fbd36007cdc40509543aa3a4f856ca2b8c      1
    217 0xd63dc86e7fdbe5923481b96beb2a5a79119e51e7      1
    218 0xd73e55a3f739fbd783f7a2a307831afc31c6510b      1
    219 0xd8c39a8e2779efa2f3e8c57d9bcd18ae0bbad76c      1
    220 0xd8c40a8f23c062837410b348e257d48ed1cf8482      1
    221 0xd9e41d489a1e88ad888b08ae47642075762f947f      1
    222 0xdd166431d09f9ef7942dbeb12de6c4f2555d304c      1
    223 0xe35c3d08163da9bd4efa00879a78504d69820b5c      1
    224 0xe51748456ea9759c19d232a32f96ba3c1c110776      1
    225 0xe5e3258aa8b2b47a4ab5b5d227b191baf23b7853      1
    226 0xe628e7eb5807f73a555a41867af235351d603e2f      1
    227 0xe77a08d1513974f9cc89fd2eb043eb68355eed20      1
    228 0xe78f56dbcb2f1187ec5091832a967fa0a872074e      1
    229 0xe79eb6ce66353cd38e5a9dc67c97c836c86c6912      1
    230 0xe8d8edbf4d3dda68ce89b2988e1c31b105e3150f      1
    231 0xe9f1287bec6d70d950add3752d735420398b810c      1
    232 0xeaaeeb152d21122a8782bbb046edf48d0d9a389d      1
    233 0xeb379827548ece3cbfa0fdda3290785d1d7c3eab      1
    234 0xeb48ec2f762518e31315b0e7ceda734389907f31      1
    235 0xec9dec944ec3259a98653426be854793eaaee45a      1
    236 0xedc650c2fef03ccf974e9f5be047b6756c38d477      1
    237 0xf09306e25661b1a0a026a607dbb4f63ebb9f3558      1
    238 0xf0c11838870ca444f5dc0ba0271a36bb9d9f81ec      1
    239 0xf1339ec6a48c55c2f80157778f85c1064d7c77c1      1
    240 0xf5559d32946212a8f718c8d6c79c19c6200c9d6e      1
    241 0xf5890e39d44d51ab09b2b02e9c7f6310e7795bfc      1
    242 0xf7733612846b22d575cf1073f92979592159d2cc      1
    243 0xf7eb3d63816679f3b0d6a2f685f7a43f953afa8a      1
    244 0xf859068452f14ca012f3b2c9604bc8edf3283db9      1
    245 0xf85a9f2d6a528ff9d5a583d8ee12b0aeebba971f      1
    246 0xf944030287430a84583e565e8feb3792e9db0708      1
    247 0xf9ba5a5ec068a347a021efd32977e49a2724feb0      1
    248 0xf9e3f1e067e36d8766b53a2a2fd0fa0ebd08e210      1
    249 0xfc6f3670e59ea8a120f8d19fb04797c4351ae51a      1
    250 0xff9ee049afa0389e6358a6a20548dcb0cf7a12a9      1

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