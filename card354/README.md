
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:192         Length:192         Min.   :1   Length:192        
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:192        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 22346969 # https://etherscan.io/block/22346969
block_hash <- "0x6a16fd9f401632a2474131644cb16c6a0f246edc8bd591c4c32cfba804964abb"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4628 

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

allow_artist1    <- pick(snapshot, contracts=c("InFlux","INFLUX3","Meeings2","Meeings3","InSights"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("DerivativesEditions2","Influx2"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("InFlux","INFLUX3","Meeings2","Meeings3","InSights","DerivativesEditions2","Influx2"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 60 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x076e7ec4724fe43262accf765dfc8002fb83dd2b      1
     2 0x0d78f3fc2b19aeda12d21713bba7e8b3f8a53ff5      1
     3 0x0f0eae91990140c560d4156db4f00c854dc8f09e      1
     4 0x107752288b215467de25c16119787d715ec8e26e      1
     5 0x108bd290152849b520b7c788394359b9867a7fe0      1
     6 0x10cf494cdbceb726d21703d21131e131ff03ae33      1
     7 0x11f46fff2624737aa0d38507e761b6ef770eb94d      1
     8 0x12d4e572cf6b44196c6327ff0eb879ca547f5c34      1
     9 0x134309c4cf57bfa43ef66bf20bd0eeccdeb2d80c      1
    10 0x1430997022e8ccf845af34596ef37f4754983f85      1
    11 0x1996da1e0c54e113370729d1c22f84376f5830e9      1
    12 0x1e5fe56aa8f66909fc7f2239749897e171380b65      1
    13 0x1f8a5feb81be9a44ff57d2a60c0420fb19fc2ddf      1
    14 0x205a46012bc3c1a2583fdd426679dff276fb257e      1
    15 0x22098012d15b4ff9a2b1da8d658dae8e5d0d3fbb      1
    16 0x301b275e3b20f9eb9dbb54e6f0df0f4e01101e14      1
    17 0x35b0dca4f59a7b89776fb33f4d09bab3855e4360      1
    18 0x3852471d266d9e2222ca9fdd922bafc904dc49e5      1
    19 0x40d500be11e713a66f89b4cebc6d8d11e3a0f2a7      1
    20 0x412d974ca20038402a4e50d586fee6ba6a9279ed      1
    21 0x4909c45bce6c9f11081ee53404b866aee9e0ddbb      1
    22 0x4c7f1354ae3d30f7a88c9d972ad7f96b44ce0dee      1
    23 0x54be3b98564f0a89237ff10d5c7a053edf2af10c      1
    24 0x57798ea2de5e864959a3f00a12f9224147edf7c8      1
    25 0x577ebc5de943e35cdf9ecb5bbe1f7d7cb6c7c647      1
    26 0x58e0fd70f29d0f408561fa933941f6e12c1d0ff0      1
    27 0x59068075a799594db03c0255eed68e8e121155c8      1
    28 0x5948428d0711442f6ae3c8d06e3a9895f33cecd2      1
    29 0x59ff9ef6e31ca54ba033aa75f39e9cd2f3f22305      1
    30 0x6215e708a7b61f5ed49642f692f28ba09b807725      1
    31 0x665654f2d2a151be2d0f8e3697e6ce780f732af2      1
    32 0x695589d2bc5a63db1e830b1da7a07de73e58c6a8      1
    33 0x6d9dd2b0fbf6e7d800f31c55f0f2f9089dfb7dc2      1
    34 0x6e5b0de0add71e1052b11abbfe6c433dd0466fb4      1
    35 0x73e9f114536c6807b6d9388bbf76f5404c621a77      1
    36 0x773d715200ab5c07f39db9772e3c83c48534a585      1
    37 0x79e561168835c783240a0637320d308897bd0922      1
    38 0x7d57df72cc14bce0057fb2f8f17c1f6233e60966      1
    39 0x7fdab8c244e0b775edeeb9ebb8356b59068c6873      1
    40 0x80ce7206da1ed2804d645a91705212ed742f285d      1
    41 0x8179e48d959a4c41d5e0432ac05b30707149d649      1
    42 0x83a2fc8c7958422faa8a003e955751d696f546e1      1
    43 0x8baf972eef3effd5959b9d8e27e2289dcabde3c3      1
    44 0x8ea6a8de40043b5e8085704d450849f99b816976      1
    45 0x8f54249ae4c8a73e92d44459e026c9197670f3fd      1
    46 0x8fb17d9e2f2735fa5e5095440f418e4a05ee22d8      1
    47 0x974c16605e95d7c742d0a4ba79bd8e13ce4732f6      1
    48 0x9dbdad4abcc48610b22d56a6bdd1aa7f97171b06      1
    49 0xa43d0698b2afbe80c4080b12f1dc4648dfd0fea7      1
    50 0xb6328f4b747b807696da1bb2f574edcbc47682ab      1
    51 0xb884ad7ab6af74693fec7f364eb8bfb31fc7ecea      1
    52 0xc68c7771ec6a6e5d67d62aa9c6f22df69865e401      1
    53 0xcc6c1d21e8474b3578e69eb036c712ab08ffdfbb      1
    54 0xd530282d853169a23822762af0dcab045d6295d3      1
    55 0xd9a6070ebc448fdc9a7dad53ce2f7026783fea3c      1
    56 0xdd5b3eac7f9b255df9919ff664e32380680f306a      1
    57 0xeb3648865b1471996e5f65ae845f9eb9226b9a02      1
    58 0xef34feb024f0a6f4d5cf3a7cd30bb0f041a6af58      1
    59 0xf2f4747c4074cc433e9d881529c2ee27584b3256      1
    60 0xf95752fd023fd8802abdd9cbe8e9965f623f8a84      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 59 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x036aeaa55ba6cf87984a5f0a467b21c63b9bf9d1      1
     2 0x0c55e7d45f53b6999732a75917df6eb711e58674      1
     3 0x0f2bfc17d828ae9065ee52b2c535811a04700175      1
     4 0x136bbfe37988f82f8585ed155615b75371489d45      1
     5 0x14ddb398ce7a516c399a95b5d5c9a41326ec93de      1
     6 0x1b609fda3c97e90003be00c8223026e0d99f49b4      1
     7 0x270b43c8e8f1082f9784e9046d4db50d55d64eeb      1
     8 0x2c7ba77e6f1d25ee8f4905d4cd6934e47ba19bca      1
     9 0x3734c5ff3db6378fd4fed019ec6ff347350a17c3      1
    10 0x419beee486a63971332cee7170c2f675d92ac5d3      1
    11 0x4298e663517593284ad4fe199b21815bd48a9969      1
    12 0x4583e00b4d17ce9e969f94baa2a6336f923d68de      1
    13 0x483a124f6717ad67440af11a77dec25f1a3d1259      1
    14 0x4d91089f48a55a6ab1b8266efaf6557d3981f3c2      1
    15 0x549418604d15df787eb24ca47f1f02fcf1600a52      1
    16 0x5f08beb4ba97b71a164630a05a536f8028924e99      1
    17 0x5fae9d4b591f213b3ba75287f2cfac0883d17f7a      1
    18 0x69be10eb6333f12a54425f05cb637a61e5b3e2ff      1
    19 0x6b347a82fcac4e6a38d1fc40e3631bd8f9495e9f      1
    20 0x6f2f8df840138a525984e2a2355f5d7c16f380ad      1
    21 0x7320dfa95fadd9bbc20bf8eca42587c44176e9a7      1
    22 0x774a34da2ee2e2d242180819f1ee88783215f7b9      1
    23 0x7e5124202b1a177cbc254b2c2262541682f20601      1
    24 0x849a27048de274c084f42551f395823f8fa18f2e      1
    25 0x95bd2248056f86606f7ec0076d145516a92afebc      1
    26 0x99836cd8b265c8f0fcd35434689f46ce97bb1a23      1
    27 0x9c45f3cf0958c5a8b5b1f4477441b77c0fa68151      1
    28 0x9cefd6dea5eae107dc3b1fcec33059da7a8d597e      1
    29 0x9f6ae0370d74f0e591c64cec4a8ae0d627817014      1
    30 0xa4ac3321fd639a7e2b53da62897955d920c97012      1
    31 0xa8357ee17cb3ff5a6b9694ddc8fde0ed2ce9d788      1
    32 0xaa4f4217fb74f6e7f846acbcb1ed4465f9660543      1
    33 0xba0c1a66d34fb6b50fa59866bac0eeb67bf11da1      1
    34 0xc054568c26d5e15eacc160cd397b8bd9da62c13b      1
    35 0xc3df7d3ea6bc94cf674e22052a5d374ff08901bd      1
    36 0xc6a9f985af8737124aac0cc407a2ace271602261      1
    37 0xcb8ecb94f6c25af4fab696b91cbb0a1449b98f64      1
    38 0xce90a7949bb78892f159f428d0dc23a8e3584d75      1
    39 0xcf04c33bfea64cf5dd6ea97e2d5620f825b03b49      1
    40 0xd40b63bf04a44e43fbfe5784bcf22acaab34a180      1
    41 0xd458f62925b3e135680626a0f520098972f93fe9      1
    42 0xd60b8d278a98e068eaf3508e95cf1b0089961288      1
    43 0xd90b20094f68ecd80ed93fa18181b0ee879d82ce      1
    44 0xde3d222f60f73bbcaf922d356414f7df7a3ffe95      1
    45 0xdf1fa21aad71c50e642fca3aa4332da17bbea409      1
    46 0xe253b915b7f20cb482ec8eece1a6d7c34cfe16cc      1
    47 0xe69ee97f0fdd1007c9f54370d4aa0e79b2956172      1
    48 0xe6c95cf4a1ae50bb83c103d0f49ad80d11b04912      1
    49 0xe82c20f6911ef8a0af094b2996a1c38f13d8a3a8      1
    50 0xe92e64f0753c4b7c7ffc2e624c34b85a4af763e2      1
    51 0xea94dace59ede50c55d53cc90c26d8ec4ff0b685      1
    52 0xec9e512fe7e90134d8ca7295329ccb0a57c91ecb      1
    53 0xee2c055f7706b9dfcd98cd5a23d5629d6316c0bd      1
    54 0xee7c714ff16776751e6be6473370fddbf0e07c89      1
    55 0xf5d4a3b9fb77d1d5600a10e0e1c4efd12f9be751      1
    56 0xf6dfa4a1213f1d14f898f2eb840b1405984fe014      1
    57 0xf8d0c9f300d0649861963ebae0bb568533619448      1
    58 0xf9b3a71211e5976abc518c7ddc7a4e63481bd693      1
    59 0xfcdb35c1105ca8ea01df3b81a4570ff621817cb8      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 119 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x036aeaa55ba6cf87984a5f0a467b21c63b9bf9d1      1
      2 0x076e7ec4724fe43262accf765dfc8002fb83dd2b      1
      3 0x0c55e7d45f53b6999732a75917df6eb711e58674      1
      4 0x0d78f3fc2b19aeda12d21713bba7e8b3f8a53ff5      1
      5 0x0f0eae91990140c560d4156db4f00c854dc8f09e      1
      6 0x0f2bfc17d828ae9065ee52b2c535811a04700175      1
      7 0x107752288b215467de25c16119787d715ec8e26e      1
      8 0x108bd290152849b520b7c788394359b9867a7fe0      1
      9 0x10cf494cdbceb726d21703d21131e131ff03ae33      1
     10 0x11f46fff2624737aa0d38507e761b6ef770eb94d      1
     11 0x12d4e572cf6b44196c6327ff0eb879ca547f5c34      1
     12 0x134309c4cf57bfa43ef66bf20bd0eeccdeb2d80c      1
     13 0x136bbfe37988f82f8585ed155615b75371489d45      1
     14 0x1430997022e8ccf845af34596ef37f4754983f85      1
     15 0x14ddb398ce7a516c399a95b5d5c9a41326ec93de      1
     16 0x1996da1e0c54e113370729d1c22f84376f5830e9      1
     17 0x1b609fda3c97e90003be00c8223026e0d99f49b4      1
     18 0x1e5fe56aa8f66909fc7f2239749897e171380b65      1
     19 0x1f8a5feb81be9a44ff57d2a60c0420fb19fc2ddf      1
     20 0x205a46012bc3c1a2583fdd426679dff276fb257e      1
     21 0x22098012d15b4ff9a2b1da8d658dae8e5d0d3fbb      1
     22 0x270b43c8e8f1082f9784e9046d4db50d55d64eeb      1
     23 0x2c7ba77e6f1d25ee8f4905d4cd6934e47ba19bca      1
     24 0x301b275e3b20f9eb9dbb54e6f0df0f4e01101e14      1
     25 0x35b0dca4f59a7b89776fb33f4d09bab3855e4360      1
     26 0x3734c5ff3db6378fd4fed019ec6ff347350a17c3      1
     27 0x3852471d266d9e2222ca9fdd922bafc904dc49e5      1
     28 0x40d500be11e713a66f89b4cebc6d8d11e3a0f2a7      1
     29 0x412d974ca20038402a4e50d586fee6ba6a9279ed      1
     30 0x419beee486a63971332cee7170c2f675d92ac5d3      1
     31 0x4298e663517593284ad4fe199b21815bd48a9969      1
     32 0x4583e00b4d17ce9e969f94baa2a6336f923d68de      1
     33 0x483a124f6717ad67440af11a77dec25f1a3d1259      1
     34 0x4909c45bce6c9f11081ee53404b866aee9e0ddbb      1
     35 0x4c7f1354ae3d30f7a88c9d972ad7f96b44ce0dee      1
     36 0x4d91089f48a55a6ab1b8266efaf6557d3981f3c2      1
     37 0x549418604d15df787eb24ca47f1f02fcf1600a52      1
     38 0x54be3b98564f0a89237ff10d5c7a053edf2af10c      1
     39 0x57798ea2de5e864959a3f00a12f9224147edf7c8      1
     40 0x577ebc5de943e35cdf9ecb5bbe1f7d7cb6c7c647      1
     41 0x58e0fd70f29d0f408561fa933941f6e12c1d0ff0      1
     42 0x59068075a799594db03c0255eed68e8e121155c8      1
     43 0x5948428d0711442f6ae3c8d06e3a9895f33cecd2      1
     44 0x59ff9ef6e31ca54ba033aa75f39e9cd2f3f22305      1
     45 0x5f08beb4ba97b71a164630a05a536f8028924e99      1
     46 0x5fae9d4b591f213b3ba75287f2cfac0883d17f7a      1
     47 0x6215e708a7b61f5ed49642f692f28ba09b807725      1
     48 0x665654f2d2a151be2d0f8e3697e6ce780f732af2      1
     49 0x695589d2bc5a63db1e830b1da7a07de73e58c6a8      1
     50 0x69be10eb6333f12a54425f05cb637a61e5b3e2ff      1
     51 0x6b347a82fcac4e6a38d1fc40e3631bd8f9495e9f      1
     52 0x6d9dd2b0fbf6e7d800f31c55f0f2f9089dfb7dc2      1
     53 0x6e5b0de0add71e1052b11abbfe6c433dd0466fb4      1
     54 0x6f2f8df840138a525984e2a2355f5d7c16f380ad      1
     55 0x7320dfa95fadd9bbc20bf8eca42587c44176e9a7      1
     56 0x73e9f114536c6807b6d9388bbf76f5404c621a77      1
     57 0x773d715200ab5c07f39db9772e3c83c48534a585      1
     58 0x774a34da2ee2e2d242180819f1ee88783215f7b9      1
     59 0x79e561168835c783240a0637320d308897bd0922      1
     60 0x7d57df72cc14bce0057fb2f8f17c1f6233e60966      1
     61 0x7e5124202b1a177cbc254b2c2262541682f20601      1
     62 0x7fdab8c244e0b775edeeb9ebb8356b59068c6873      1
     63 0x80ce7206da1ed2804d645a91705212ed742f285d      1
     64 0x8179e48d959a4c41d5e0432ac05b30707149d649      1
     65 0x83a2fc8c7958422faa8a003e955751d696f546e1      1
     66 0x849a27048de274c084f42551f395823f8fa18f2e      1
     67 0x8baf972eef3effd5959b9d8e27e2289dcabde3c3      1
     68 0x8ea6a8de40043b5e8085704d450849f99b816976      1
     69 0x8f54249ae4c8a73e92d44459e026c9197670f3fd      1
     70 0x8fb17d9e2f2735fa5e5095440f418e4a05ee22d8      1
     71 0x95bd2248056f86606f7ec0076d145516a92afebc      1
     72 0x974c16605e95d7c742d0a4ba79bd8e13ce4732f6      1
     73 0x99836cd8b265c8f0fcd35434689f46ce97bb1a23      1
     74 0x9c45f3cf0958c5a8b5b1f4477441b77c0fa68151      1
     75 0x9cefd6dea5eae107dc3b1fcec33059da7a8d597e      1
     76 0x9dbdad4abcc48610b22d56a6bdd1aa7f97171b06      1
     77 0x9f6ae0370d74f0e591c64cec4a8ae0d627817014      1
     78 0xa43d0698b2afbe80c4080b12f1dc4648dfd0fea7      1
     79 0xa4ac3321fd639a7e2b53da62897955d920c97012      1
     80 0xa8357ee17cb3ff5a6b9694ddc8fde0ed2ce9d788      1
     81 0xaa4f4217fb74f6e7f846acbcb1ed4465f9660543      1
     82 0xb6328f4b747b807696da1bb2f574edcbc47682ab      1
     83 0xb884ad7ab6af74693fec7f364eb8bfb31fc7ecea      1
     84 0xba0c1a66d34fb6b50fa59866bac0eeb67bf11da1      1
     85 0xc054568c26d5e15eacc160cd397b8bd9da62c13b      1
     86 0xc3df7d3ea6bc94cf674e22052a5d374ff08901bd      1
     87 0xc68c7771ec6a6e5d67d62aa9c6f22df69865e401      1
     88 0xc6a9f985af8737124aac0cc407a2ace271602261      1
     89 0xcb8ecb94f6c25af4fab696b91cbb0a1449b98f64      1
     90 0xcc6c1d21e8474b3578e69eb036c712ab08ffdfbb      1
     91 0xce90a7949bb78892f159f428d0dc23a8e3584d75      1
     92 0xcf04c33bfea64cf5dd6ea97e2d5620f825b03b49      1
     93 0xd40b63bf04a44e43fbfe5784bcf22acaab34a180      1
     94 0xd458f62925b3e135680626a0f520098972f93fe9      1
     95 0xd530282d853169a23822762af0dcab045d6295d3      1
     96 0xd60b8d278a98e068eaf3508e95cf1b0089961288      1
     97 0xd90b20094f68ecd80ed93fa18181b0ee879d82ce      1
     98 0xd9a6070ebc448fdc9a7dad53ce2f7026783fea3c      1
     99 0xdd5b3eac7f9b255df9919ff664e32380680f306a      1
    100 0xde3d222f60f73bbcaf922d356414f7df7a3ffe95      1
    101 0xdf1fa21aad71c50e642fca3aa4332da17bbea409      1
    102 0xe253b915b7f20cb482ec8eece1a6d7c34cfe16cc      1
    103 0xe69ee97f0fdd1007c9f54370d4aa0e79b2956172      1
    104 0xe6c95cf4a1ae50bb83c103d0f49ad80d11b04912      1
    105 0xe82c20f6911ef8a0af094b2996a1c38f13d8a3a8      1
    106 0xe92e64f0753c4b7c7ffc2e624c34b85a4af763e2      1
    107 0xea94dace59ede50c55d53cc90c26d8ec4ff0b685      1
    108 0xeb3648865b1471996e5f65ae845f9eb9226b9a02      1
    109 0xec9e512fe7e90134d8ca7295329ccb0a57c91ecb      1
    110 0xee2c055f7706b9dfcd98cd5a23d5629d6316c0bd      1
    111 0xee7c714ff16776751e6be6473370fddbf0e07c89      1
    112 0xef34feb024f0a6f4d5cf3a7cd30bb0f041a6af58      1
    113 0xf2f4747c4074cc433e9d881529c2ee27584b3256      1
    114 0xf5d4a3b9fb77d1d5600a10e0e1c4efd12f9be751      1
    115 0xf6dfa4a1213f1d14f898f2eb840b1405984fe014      1
    116 0xf8d0c9f300d0649861963ebae0bb568533619448      1
    117 0xf95752fd023fd8802abdd9cbe8e9965f623f8a84      1
    118 0xf9b3a71211e5976abc518c7ddc7a4e63481bd693      1
    119 0xfcdb35c1105ca8ea01df3b81a4570ff621817cb8      1

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
