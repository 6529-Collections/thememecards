
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance        contract        
     Length:154         Length:154         Min.   :1.000   Length:154        
     Class :character   Class :character   1st Qu.:1.000   Class :character  
     Mode  :character   Mode  :character   Median :1.000   Mode  :character  
                                           Mean   :1.026                     
                                           3rd Qu.:1.000                     
                                           Max.   :3.000                     
         name          
     Length:154        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 19196469 # https://etherscan.io/block/19196469
block_hash <- "0x0b1730b9d0b8e92459d39efc7456728f98992aa859035ed613930245402127c7"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4244 

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

allow_artist1    <- pick(snapshot, contracts=c("VICTORYAeromobiles","UTOPIAUniverse","TheyAreAboveUs","Foundation","IsayevArt","UTOPIAInfinity"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("ShineWheelsAllusion","SectorAlphaIndustrialDistrict","TwoWorlds"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("VICTORYAeromobiles","UTOPIAUniverse","TheyAreAboveUs","Foundation","IsayevArt","UTOPIAInfinity","ShineWheelsAllusion","SectorAlphaIndustrialDistrict","TwoWorlds"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 13 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x052564eb0fd8b340803df55def89c25c432f43f4      1
     2 0x065094af6f8e06c0f0530bcf7d589e4d17608dfd      1
     3 0x0ce390f18af702cca546297845a4a51d102123cf      1
     4 0x1cf4b9b0379bc01a9be12b8efcdb7570a65e3fa9      1
     5 0x685e1487852c208332d5cfa6292ff3e49de28c22      1
     6 0x6a290fd02a31c78a225b98f3d587d61f4aeba437      1
     7 0x8c6af6a64673c45bd838b932cb179cc1849f6490      1
     8 0xb7eea5b9557e4da252d4b84d3ae599851fb29f4f      1
     9 0xd2cc73f5e429ab858f9757d5a9d5258dd02077de      1
    10 0xe91b2d985d7fb21e9e75b08ac34e932a65f66cf5      1
    11 0xf17b07497c09f11211975b0ffa2d9ec5cf96252c      1
    12 0xf85a9f2d6a528ff9d5a583d8ee12b0aeebba971f      1
    13 0xfb10df58c750b140b09d833bcff487d32a07e31b      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 125 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x027cae2ed1a23350a751452e907b4120330f9762      1
      2 0x0504aced77f859f8b0be73f864dd2f0c1019d41a      1
      3 0x076d40d4453a72a3dec44970b1529baee379dd0b      1
      4 0x08679fb863e0b509310104657800bb05375d5fdc      1
      5 0x0daac372398974373b29cb61c31deb11afa7ce23      1
      6 0x11e7dfd1ceb3571e00888e96c77ed92fa32c3692      1
      7 0x151e86b0f89ed7940231b6f198e1656664b8cfe9      1
      8 0x1ae5f1c9939a5fc8928fc544c4cbb667fb505daf      1
      9 0x1b0efb37c3e12ced2d77189de3cec825c75ffcda      1
     10 0x1d3dd4cc14c9bc93bc8744559be4ace760f7a1d3      1
     11 0x1eea95f2d2ed24cd3451da93a69efdd08767cc5b      1
     12 0x1ffbd6ba439561ef4525cc58ac6d7585859426dc      1
     13 0x2177da04f9479496c2292d6344d306aa49beb34a      1
     14 0x22ed7a936d3dea108004e3229f3bd3d84c7225db      1
     15 0x271c9feed68ddbcba1d20af2f6db0ab322355c97      1
     16 0x277d83ea66e1902e1822b9526ea7a2d150a363b6      1
     17 0x28b8906d06784d25557b6b6d9705455b265809b3      1
     18 0x29dc7b7728cdd28372503b6543180edef8ea154e      1
     19 0x2aaba4b94a805f0fea696dc11de324c2320ff4e1      1
     20 0x2d6b14f0aafc341d282a594599798e5c3df4f9ec      1
     21 0x2f1390ec03a03e57a1406bdef57c9cf487f62b78      1
     22 0x3382f79f284d21f5fa01bd296cf37b6a9c2b574d      1
     23 0x35d002a51b36c31f83f4ed87b94ee7a2ae49eacf      1
     24 0x377fb14e0823020e9bda6c21ad9cb8d98fd40191      1
     25 0x39117cedc1769fd480ff696a61e8534893805865      1
     26 0x39202692d6d6da6b8a997c5c0332d9cd498c81b3      1
     27 0x3bb29c76a3cd4ea92dbc9a694f140904702da7f6      1
     28 0x3ccb86a565daa86d1613a634dd16663779bbd63f      1
     29 0x3e43fb485c0f46aae9935b27b8e626ec6be596a5      1
     30 0x4187ddd9340689c8e4a124b7532fca0a13eb9511      1
     31 0x41ede8f5a6a14b3041b710a31fe9939adafa9d38      1
     32 0x45f2e08b35a959a87833c57c4bacbf58f95eefb7      1
     33 0x471c04a7ce29e9e79dbda1753654160c04bfa1d4      1
     34 0x47f5dcbb04e00951c228dbfa9934df1febd77680      1
     35 0x4cbc3966088d27859423c42cf2c00ff40ac0261d      1
     36 0x4cc84a4112922a69d15b27c07c000ceed99ef89f      1
     37 0x4cff7990f598a956d17ab11e80664bc8098d5df2      1
     38 0x4fc1252351e19e9a2c0204d818b2371c23492c12      1
     39 0x53887f0dee06c6459bc928f9f39beccac3947325      1
     40 0x5953227e1b31c4064bd504b8def3cec6d7a790b8      1
     41 0x5aa71418ac6238c78ea9a032a768dbbbf86bfeca      1
     42 0x5b00f3d1644ff3106248b2c50505a67abf31efff      1
     43 0x5c3200fdb14f78a14216e164abefcbdff4870caa      1
     44 0x5cad0ca5517840d9d76ba363c32cd04fb23db5d6      1
     45 0x5dbe2b1f8e3946dc425e472e15b891348cb93a8b      1
     46 0x5e31c357d03e9528e9bf95bd16e5c1ab3f7d37d0      1
     47 0x5eba361d96b57926f3c47bec163e526c6fc07720      1
     48 0x5f8e5ec735dce787ed8a49bbb7a57b1451bb6951      1
     49 0x5fc13c5a541041f8fa12fab492d064435eb8d2f5      1
     50 0x617ed24cd137a30dbbe94b53de9ecf4b5ed8e90f      1
     51 0x62484af9ebb75b60bc2476994a745b62c5310331      1
     52 0x6972323e5aeae1af4c74185cb68d72b42fcff0e2      1
     53 0x69cb3b1de24e08f1cfc2994171b6c6930498f750      1
     54 0x6dff1d922a97fda351aad8c5f7e8f69be187e0a6      1
     55 0x780af86f7d2c332641fc2c59965c57dd5bdb05b1      1
     56 0x793e48857f3ccdfe5cf3c504b6dfc7e8dab5b0e1      1
     57 0x7affd20b34d9172ab0b15797ec063222ad3e24f9      1
     58 0x7ce30498e564f081ca65a226f44b1751f93a0f82      1
     59 0x7fdab8c244e0b775edeeb9ebb8356b59068c6873      1
     60 0x7fdba87da6ed499f19fccd1b8072b4dcf6991ffe      1
     61 0x82e133653e5069a61062b28795ef19a99a3d2c75      1
     62 0x839dc785637408165f65585fccb7613870a40388      1
     63 0x851fe70498e1792739e429b466e3a12cf6e50de6      1
     64 0x87afc8adaff6ab99af285b3e4790b1aaac2e3461      1
     65 0x8d32eac23c716357ec7a03fb3f27861e45dc6d9a      1
     66 0x8ed55d7ae8a86032bfb78eedfd318086f9bc43bc      1
     67 0x8f06975b0c3cc46087134339fb22ee2d46d2106d      1
     68 0x8fb17d9e2f2735fa5e5095440f418e4a05ee22d8      1
     69 0x9050c8c69e6f1fe5cef50b28b3ebcbbe0d7e45f8      1
     70 0x91b8338652d8db39630ecaec352155d8eecda6fb      1
     71 0x924baf1e2bbc58e9cfe333d4c7074886a6af1afe      1
     72 0x931f5edddee2f460f7f1c62f69f88c796cdaf834      1
     73 0x9720dbe3f2716ea6fbf56ab4469b902e4acaa182      1
     74 0x995e0505603a19ee5c469d2359427bea68c6e953      1
     75 0x99e73a69536f7365fd4f2f2e5391f5daa484b99b      1
     76 0x9ddb728ee49b49074280b15ba6925deac06b78d1      1
     77 0xa26bd819bf68fcd12fa39a5cef6e41c3c000969e      1
     78 0xafc9d2e4f7e7be7d1c5090f02a196ca9a144f02e      1
     79 0xb014c63c49e759411e21670450cb2008ce4c99c7      1
     80 0xb05e9e74a9a9bbc241218c491ad038b8d40f4345      1
     81 0xb0e297eb644eabb16eac74220ba406b11edb2cd7      1
     82 0xb21866e83ce017a631b1244e225b19c85adb3ae2      1
     83 0xb3c3b3038bd2231b7b61a510102b459e694690a4      1
     84 0xb552dbae15c36ee6fbed4ad24b05e57d7bb30b87      1
     85 0xb833170cadf3f28c1023e3d255ea1ea736561a88      1
     86 0xb8aa379b7d261f8235dd525a53393d1e4b340251      1
     87 0xbaf66e1f03d860a790b0de6cb9aec272a5b3abb2      1
     88 0xbd088edb1aeced8d069a52f3bf253beb899437f2      1
     89 0xbde061ece978789051a2aacd0b6d18f5faf8c20b      1
     90 0xbefe5d435616619253be2e448310f70136d0fddc      1
     91 0xc0e401ae543da893f16fdf79a7cfd843b50e9644      1
     92 0xc295a7a1dfc05826414985ca561a821fbda7ded3      1
     93 0xc2d742fdbb31442b843a098dcec3c20054c28986      1
     94 0xc2eaa5870c31ffc069238a3c73c79bd29826e299      1
     95 0xc4c8cf8e9b28a75c0efa77003ac3d6a8e27bcae5      1
     96 0xc5c9cfe3e5771b3e5a57a62c2141c58f2a3749c8      1
     97 0xca6983947efc848df45c9e84b279438e24727d2e      1
     98 0xcf96e143cccd7f169bba7a63f416e4aa8aec96ac      1
     99 0xd2a84518a6e63699b1ea888c13adf87377bc7c47      1
    100 0xd2dbe21025dede10517a8be905deb0af0bf2cfbc      1
    101 0xd37905283adf21d3e628b423cbe20c4583ba9979      1
    102 0xd4091d661a44648d61bd3bb51e129d0d60892056      1
    103 0xd530282d853169a23822762af0dcab045d6295d3      1
    104 0xd891383632def64d4d1934619f61c6754b5c73f1      1
    105 0xda5c62c9773bdee4f81587b454f57d7767a889a9      1
    106 0xdca523dd6e3180498c7d1b285d0f1d6be30e04a4      1
    107 0xdd0b7b86ff6988de3d9e6f780605a50c48d050c6      1
    108 0xddc87432c0dd742bc71fe82d79371900647b86ce      1
    109 0xddd0853694857334d0020747be3faec42eb2101b      1
    110 0xdf067d1c62b6a94e45ced3f01ebc94ae304cb045      1
    111 0xe450637a5b515d5bb09d2b75cb4ca89700cc410c      1
    112 0xe483f7c09b4707e423f621c0d7453fb2c7eabc03      1
    113 0xe540a8d94bd379c4d9c0478c8fe9f7fe7f0b06e1      1
    114 0xe8d8edbf4d3dda68ce89b2988e1c31b105e3150f      1
    115 0xe96ba1a10f72b58934b9ac0e3fb471d2ba65b757      1
    116 0xebda75c5e193bbb82377b77e3c62c0b323240307      1
    117 0xed690606f908361c26bd2b1ad4fa31ed201dca4c      1
    118 0xefe336dce12ee2053484b8f9ee69dec2f6109f93      1
    119 0xefffdc05e7c5b305fbd504366b01f2d6424cb8c4      1
    120 0xf041c4f026547f7fbea6904f77bea16997024751      1
    121 0xf1ce03f0f25304a1fb911010f6e3232390ab121d      1
    122 0xf326fc9373450d52d6ec77a71b8c79eab2cca819      1
    123 0xf38f67d8b16752b18f7358ef216a35423e45f806      1
    124 0xf60af1b50886d6ed7b4bb04101e5ba0cc053906b      1
    125 0xf613cfd07af6d011fd671f98064214ab5b2942cf      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 138 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x027cae2ed1a23350a751452e907b4120330f9762      1
      2 0x0504aced77f859f8b0be73f864dd2f0c1019d41a      1
      3 0x052564eb0fd8b340803df55def89c25c432f43f4      1
      4 0x065094af6f8e06c0f0530bcf7d589e4d17608dfd      1
      5 0x076d40d4453a72a3dec44970b1529baee379dd0b      1
      6 0x08679fb863e0b509310104657800bb05375d5fdc      1
      7 0x0ce390f18af702cca546297845a4a51d102123cf      1
      8 0x0daac372398974373b29cb61c31deb11afa7ce23      1
      9 0x11e7dfd1ceb3571e00888e96c77ed92fa32c3692      1
     10 0x151e86b0f89ed7940231b6f198e1656664b8cfe9      1
     11 0x1ae5f1c9939a5fc8928fc544c4cbb667fb505daf      1
     12 0x1b0efb37c3e12ced2d77189de3cec825c75ffcda      1
     13 0x1cf4b9b0379bc01a9be12b8efcdb7570a65e3fa9      1
     14 0x1d3dd4cc14c9bc93bc8744559be4ace760f7a1d3      1
     15 0x1eea95f2d2ed24cd3451da93a69efdd08767cc5b      1
     16 0x1ffbd6ba439561ef4525cc58ac6d7585859426dc      1
     17 0x2177da04f9479496c2292d6344d306aa49beb34a      1
     18 0x22ed7a936d3dea108004e3229f3bd3d84c7225db      1
     19 0x271c9feed68ddbcba1d20af2f6db0ab322355c97      1
     20 0x277d83ea66e1902e1822b9526ea7a2d150a363b6      1
     21 0x28b8906d06784d25557b6b6d9705455b265809b3      1
     22 0x29dc7b7728cdd28372503b6543180edef8ea154e      1
     23 0x2aaba4b94a805f0fea696dc11de324c2320ff4e1      1
     24 0x2d6b14f0aafc341d282a594599798e5c3df4f9ec      1
     25 0x2f1390ec03a03e57a1406bdef57c9cf487f62b78      1
     26 0x3382f79f284d21f5fa01bd296cf37b6a9c2b574d      1
     27 0x35d002a51b36c31f83f4ed87b94ee7a2ae49eacf      1
     28 0x377fb14e0823020e9bda6c21ad9cb8d98fd40191      1
     29 0x39117cedc1769fd480ff696a61e8534893805865      1
     30 0x39202692d6d6da6b8a997c5c0332d9cd498c81b3      1
     31 0x3bb29c76a3cd4ea92dbc9a694f140904702da7f6      1
     32 0x3ccb86a565daa86d1613a634dd16663779bbd63f      1
     33 0x3e43fb485c0f46aae9935b27b8e626ec6be596a5      1
     34 0x4187ddd9340689c8e4a124b7532fca0a13eb9511      1
     35 0x41ede8f5a6a14b3041b710a31fe9939adafa9d38      1
     36 0x45f2e08b35a959a87833c57c4bacbf58f95eefb7      1
     37 0x471c04a7ce29e9e79dbda1753654160c04bfa1d4      1
     38 0x47f5dcbb04e00951c228dbfa9934df1febd77680      1
     39 0x4cbc3966088d27859423c42cf2c00ff40ac0261d      1
     40 0x4cc84a4112922a69d15b27c07c000ceed99ef89f      1
     41 0x4cff7990f598a956d17ab11e80664bc8098d5df2      1
     42 0x4fc1252351e19e9a2c0204d818b2371c23492c12      1
     43 0x53887f0dee06c6459bc928f9f39beccac3947325      1
     44 0x5953227e1b31c4064bd504b8def3cec6d7a790b8      1
     45 0x5aa71418ac6238c78ea9a032a768dbbbf86bfeca      1
     46 0x5b00f3d1644ff3106248b2c50505a67abf31efff      1
     47 0x5c3200fdb14f78a14216e164abefcbdff4870caa      1
     48 0x5cad0ca5517840d9d76ba363c32cd04fb23db5d6      1
     49 0x5dbe2b1f8e3946dc425e472e15b891348cb93a8b      1
     50 0x5e31c357d03e9528e9bf95bd16e5c1ab3f7d37d0      1
     51 0x5eba361d96b57926f3c47bec163e526c6fc07720      1
     52 0x5f8e5ec735dce787ed8a49bbb7a57b1451bb6951      1
     53 0x5fc13c5a541041f8fa12fab492d064435eb8d2f5      1
     54 0x617ed24cd137a30dbbe94b53de9ecf4b5ed8e90f      1
     55 0x62484af9ebb75b60bc2476994a745b62c5310331      1
     56 0x685e1487852c208332d5cfa6292ff3e49de28c22      1
     57 0x6972323e5aeae1af4c74185cb68d72b42fcff0e2      1
     58 0x69cb3b1de24e08f1cfc2994171b6c6930498f750      1
     59 0x6a290fd02a31c78a225b98f3d587d61f4aeba437      1
     60 0x6dff1d922a97fda351aad8c5f7e8f69be187e0a6      1
     61 0x780af86f7d2c332641fc2c59965c57dd5bdb05b1      1
     62 0x793e48857f3ccdfe5cf3c504b6dfc7e8dab5b0e1      1
     63 0x7affd20b34d9172ab0b15797ec063222ad3e24f9      1
     64 0x7ce30498e564f081ca65a226f44b1751f93a0f82      1
     65 0x7fdab8c244e0b775edeeb9ebb8356b59068c6873      1
     66 0x7fdba87da6ed499f19fccd1b8072b4dcf6991ffe      1
     67 0x82e133653e5069a61062b28795ef19a99a3d2c75      1
     68 0x839dc785637408165f65585fccb7613870a40388      1
     69 0x851fe70498e1792739e429b466e3a12cf6e50de6      1
     70 0x87afc8adaff6ab99af285b3e4790b1aaac2e3461      1
     71 0x8c6af6a64673c45bd838b932cb179cc1849f6490      1
     72 0x8d32eac23c716357ec7a03fb3f27861e45dc6d9a      1
     73 0x8ed55d7ae8a86032bfb78eedfd318086f9bc43bc      1
     74 0x8f06975b0c3cc46087134339fb22ee2d46d2106d      1
     75 0x8fb17d9e2f2735fa5e5095440f418e4a05ee22d8      1
     76 0x9050c8c69e6f1fe5cef50b28b3ebcbbe0d7e45f8      1
     77 0x91b8338652d8db39630ecaec352155d8eecda6fb      1
     78 0x924baf1e2bbc58e9cfe333d4c7074886a6af1afe      1
     79 0x931f5edddee2f460f7f1c62f69f88c796cdaf834      1
     80 0x9720dbe3f2716ea6fbf56ab4469b902e4acaa182      1
     81 0x995e0505603a19ee5c469d2359427bea68c6e953      1
     82 0x99e73a69536f7365fd4f2f2e5391f5daa484b99b      1
     83 0x9ddb728ee49b49074280b15ba6925deac06b78d1      1
     84 0xa26bd819bf68fcd12fa39a5cef6e41c3c000969e      1
     85 0xafc9d2e4f7e7be7d1c5090f02a196ca9a144f02e      1
     86 0xb014c63c49e759411e21670450cb2008ce4c99c7      1
     87 0xb05e9e74a9a9bbc241218c491ad038b8d40f4345      1
     88 0xb0e297eb644eabb16eac74220ba406b11edb2cd7      1
     89 0xb21866e83ce017a631b1244e225b19c85adb3ae2      1
     90 0xb3c3b3038bd2231b7b61a510102b459e694690a4      1
     91 0xb552dbae15c36ee6fbed4ad24b05e57d7bb30b87      1
     92 0xb7eea5b9557e4da252d4b84d3ae599851fb29f4f      1
     93 0xb833170cadf3f28c1023e3d255ea1ea736561a88      1
     94 0xb8aa379b7d261f8235dd525a53393d1e4b340251      1
     95 0xbaf66e1f03d860a790b0de6cb9aec272a5b3abb2      1
     96 0xbd088edb1aeced8d069a52f3bf253beb899437f2      1
     97 0xbde061ece978789051a2aacd0b6d18f5faf8c20b      1
     98 0xbefe5d435616619253be2e448310f70136d0fddc      1
     99 0xc0e401ae543da893f16fdf79a7cfd843b50e9644      1
    100 0xc295a7a1dfc05826414985ca561a821fbda7ded3      1
    101 0xc2d742fdbb31442b843a098dcec3c20054c28986      1
    102 0xc2eaa5870c31ffc069238a3c73c79bd29826e299      1
    103 0xc4c8cf8e9b28a75c0efa77003ac3d6a8e27bcae5      1
    104 0xc5c9cfe3e5771b3e5a57a62c2141c58f2a3749c8      1
    105 0xca6983947efc848df45c9e84b279438e24727d2e      1
    106 0xcf96e143cccd7f169bba7a63f416e4aa8aec96ac      1
    107 0xd2a84518a6e63699b1ea888c13adf87377bc7c47      1
    108 0xd2cc73f5e429ab858f9757d5a9d5258dd02077de      1
    109 0xd2dbe21025dede10517a8be905deb0af0bf2cfbc      1
    110 0xd37905283adf21d3e628b423cbe20c4583ba9979      1
    111 0xd4091d661a44648d61bd3bb51e129d0d60892056      1
    112 0xd530282d853169a23822762af0dcab045d6295d3      1
    113 0xd891383632def64d4d1934619f61c6754b5c73f1      1
    114 0xda5c62c9773bdee4f81587b454f57d7767a889a9      1
    115 0xdca523dd6e3180498c7d1b285d0f1d6be30e04a4      1
    116 0xdd0b7b86ff6988de3d9e6f780605a50c48d050c6      1
    117 0xddc87432c0dd742bc71fe82d79371900647b86ce      1
    118 0xddd0853694857334d0020747be3faec42eb2101b      1
    119 0xdf067d1c62b6a94e45ced3f01ebc94ae304cb045      1
    120 0xe450637a5b515d5bb09d2b75cb4ca89700cc410c      1
    121 0xe483f7c09b4707e423f621c0d7453fb2c7eabc03      1
    122 0xe540a8d94bd379c4d9c0478c8fe9f7fe7f0b06e1      1
    123 0xe8d8edbf4d3dda68ce89b2988e1c31b105e3150f      1
    124 0xe91b2d985d7fb21e9e75b08ac34e932a65f66cf5      1
    125 0xe96ba1a10f72b58934b9ac0e3fb471d2ba65b757      1
    126 0xebda75c5e193bbb82377b77e3c62c0b323240307      1
    127 0xed690606f908361c26bd2b1ad4fa31ed201dca4c      1
    128 0xefe336dce12ee2053484b8f9ee69dec2f6109f93      1
    129 0xefffdc05e7c5b305fbd504366b01f2d6424cb8c4      1
    130 0xf041c4f026547f7fbea6904f77bea16997024751      1
    131 0xf17b07497c09f11211975b0ffa2d9ec5cf96252c      1
    132 0xf1ce03f0f25304a1fb911010f6e3232390ab121d      1
    133 0xf326fc9373450d52d6ec77a71b8c79eab2cca819      1
    134 0xf38f67d8b16752b18f7358ef216a35423e45f806      1
    135 0xf60af1b50886d6ed7b4bb04101e5ba0cc053906b      1
    136 0xf613cfd07af6d011fd671f98064214ab5b2942cf      1
    137 0xf85a9f2d6a528ff9d5a583d8ee12b0aeebba971f      1
    138 0xfb10df58c750b140b09d833bcff487d32a07e31b      1

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
