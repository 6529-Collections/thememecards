
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance        contract        
     Length:201         Length:201         Min.   :1.000   Length:201        
     Class :character   Class :character   1st Qu.:1.000   Class :character  
     Mode  :character   Mode  :character   Median :1.000   Mode  :character  
                                           Mean   :1.249                     
                                           3rd Qu.:1.000                     
                                           Max.   :7.000                     
         name          
     Length:201        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 20896969 # https://etherscan.io/block/20896969
block_hash <- "0x1187fa5bf48e6a58ee1ae174cdd5b6837a846f9df2d93f64e8b1d31d8f40302e"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4767 

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

allow_artist1    <- pick(snapshot, contracts=c("SuperRare","LostHalos","EvaEllerArt","ShadyIndividuals","Religioso"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("EvaEllerEditions","SamanthaCavetEditions","NobleEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("SuperRare","LostHalos","EvaEllerArt","ShadyIndividuals","Religioso","EvaEllerEditions","SamanthaCavetEditions","NobleEditions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 10 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x193bb5dcb851ef8f75dbbd6950c962c3a5f4b0e3      1
     2 0x4b4d75b8b0fc7528ea2614b2ca83555824c07867      1
     3 0x782adafbf47a604f146af4a059908e946eae539f      1
     4 0x7be90f10f9c6cd02f32401c4929e5a5dbaa0a51b      1
     5 0x80601bf6cf2af7cca21c4d85a0352f6c49aee512      1
     6 0xc449f005667bef849261b35accf931a4bace48fb      1
     7 0xd2e03ea030322dfab19fd3975e822f2a16898a03      1
     8 0xd4091d661a44648d61bd3bb51e129d0d60892056      1
     9 0xe7014919c80cf68b64050290ca6d4c685648d14b      1
    10 0xf987a65115b267bc9a16f0eb52b7f7e94e554cbb      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 148 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x04b5a01e011352624245a5150d7f36e754ef8b62      1
      2 0x04c95c6f8ea7ff7c3f7a2f148fae75650c5a875e      1
      3 0x06486e323d993af90d544bd28ecbb6ce24a70937      1
      4 0x07f6f97ac29fb6882746e7c95797d30c1b50448f      1
      5 0x08493a92a95d4e93bdbc49f66e7647f1445a9210      1
      6 0x0a1c6f1638adbe3873c7e8dad6c0e3b08285ba07      1
      7 0x0a35c0d624855cf97e8eeb1ab2b0b53c6280d317      1
      8 0x0b30c8465cb2254a7a530011c872deb3f48f9ebe      1
      9 0x0bb75bef057da63a0ae4b25fe9adafd35cd92b87      1
     10 0x0ce390f18af702cca546297845a4a51d102123cf      1
     11 0x0da8ec9829af1b58d247078e398ce00420ddd942      1
     12 0x0f2005e5b1edf748a045d721e472886fee1815e4      1
     13 0x119fcd292b95551e8af472fc4721ac34cc5f48e5      1
     14 0x12d4e572cf6b44196c6327ff0eb879ca547f5c34      1
     15 0x13c39b3e6e6159bc233fad90825f1f9f84fcb102      1
     16 0x191d4c47676daa5abd35424f84f094f7e6a4ebb2      1
     17 0x1b672e08c3cc6486b1c01d1114d5ced3c5682882      1
     18 0x1cb9998ab32e6d172f301a10ed8e0272d96509b4      1
     19 0x1d10b0166a761f269adf864ebdaca9be445ca637      1
     20 0x1d25029d92a7051f46a13be9a512443c966d3542      1
     21 0x1e4f4df03ef9dd1bceb2d3f9f2cb1ba18c816704      1
     22 0x20357eb5dc98da197c332d9fdd2a38ef58c91fb3      1
     23 0x2168b2295e912a27c0015bad9f09f090ebce1a99      1
     24 0x2177da04f9479496c2292d6344d306aa49beb34a      1
     25 0x235f32bb3920bf99e9fa813a37fcebadaf9cdf39      1
     26 0x23602ca06e977c86339ffddad74966e824ab691e      1
     27 0x248458947c120ca057ec028b3fe7e4b3f26fdb3d      1
     28 0x24fbadccd6684106e24065694ac87b0e98819235      1
     29 0x265a25670a194df9ff63c848827455da74042087      1
     30 0x27fb0a35ac158fe460b91b04e5087b0131352ed9      1
     31 0x287bdf8c332d44bb015f8b4deb6513010c951f39      1
     32 0x2886254556ff9b1572845865a0270a3a7f52b4ff      1
     33 0x28cf5d9d465dfaf5c616958ef8b23dbee567e2b7      1
     34 0x2bfca0be6ba3fa7c5f496d8967ae7c041faee0d2      1
     35 0x2dba88cb3b435f99a3e58b6e0fe450e8f1a3f20f      1
     36 0x32084ce62e8955da04ce2a2969bd6edbb4b346a5      1
     37 0x3330777cc5270db65e7df1d738e989dc16dd49f2      1
     38 0x3564ce014967c81e25f6349124673cf60b7ac572      1
     39 0x35a958858d46bcd9984360911bbaae44f4ef8f50      1
     40 0x3a051f1d0cf548f5c24893934bb69cbf61844c9f      1
     41 0x3a661a18cb0258119636dfdde098648c6ad5ba62      1
     42 0x3b846d4d8823fb12cc3694a2584c5c32057808c8      1
     43 0x41254fe73d3b49f0fe3cc7743b4d40ef26bb338e      1
     44 0x4220132c9df1ab7bd2913f0fd03297c90e7cc6fe      1
     45 0x48c3f2762c9e811bbc2e5dfbe0bb6bd36dec5247      1
     46 0x4ca1c22a3bff99415c1095dc22b3595d2c54c458      1
     47 0x4cc2b3075c3cc735958a50ce1b3dd00173e97b04      1
     48 0x4cd5e58fb994b668a201e2166f933d8f998b5220      1
     49 0x4fb98d1499a24e9c794401e9fb96e9483a43d934      1
     50 0x5147c1a9e594041ccdba5d044b6274231dc6ebf5      1
     51 0x534e920f195732d77266e9b903829e13b3993164      1
     52 0x541a51cccc9fc7991ed0b1c3236e156e533b11a4      1
     53 0x5aa71418ac6238c78ea9a032a768dbbbf86bfeca      1
     54 0x5b33b6f41e87111b7d22685b297f086b5535034d      1
     55 0x5b513d7bb62f7f28bc3aba2d52fe6167a9b3a827      1
     56 0x5c3b3f880597349e7a1ae7034ff11ec6d7e909be      1
     57 0x5ce9ad759e41bf1b3dfc1a41db940a90d7a43460      1
     58 0x5f2abc70bb1567c7807da87e944ddf965a0846bd      1
     59 0x6079696f3d9fc53ba8350d8db44e048e6a3814b1      1
     60 0x60ce2c648e81d819c53d03bc6d7fa39ea5f40c6b      1
     61 0x618c7671366caaab6701f06ae9925ae682c6bf97      1
     62 0x628a4ff5077f8b083d56509f761c67bc7c85b754      1
     63 0x6811eaa638b04aedafeaddfca36c50ea303b1866      1
     64 0x69cb3b1de24e08f1cfc2994171b6c6930498f750      1
     65 0x6af7812fc932da2d34738a33f295de54011fd16b      1
     66 0x6b6ae848f555f70944bc99d736fe29fcaecf8b23      1
     67 0x6b88ccacf59e2bd4df9481c0c90d89936c9ad021      1
     68 0x6b8c6e15818c74895c31a1c91390b3d42b336799      1
     69 0x6c299fd0fbf1ec228f7e8d7fb6fab4ff8e8752f2      1
     70 0x6cb6b5f592438358704be91208a3a7420caea792      1
     71 0x6e37d79b4744dd82261cfb7b4ccf77fe513be06f      1
     72 0x71a7702ff3d858a0853a918fb44c537740a8bde9      1
     73 0x749f93bceb1f2512481112d149c938e15fb56e1b      1
     74 0x7556cc553036e8e67ca77b72fca81657988de13e      1
     75 0x75fe6f9af2e4b4fb9e86ebe26bb31162d292b315      1
     76 0x799b27750e06d18fc9b2510e9d2201fc8a5d011b      1
     77 0x7c33a89a994e022434e930f70cb99fbbd3e2197c      1
     78 0x7cc696eeb48dc6acee89447168302a90c116bc34      1
     79 0x7ee8f8f465896f56edbdb5209015b64249c96ddc      1
     80 0x7fc200b56b7d0120193f034aa20a51eebe15eea2      1
     81 0x7fdba87da6ed499f19fccd1b8072b4dcf6991ffe      1
     82 0x80ce7206da1ed2804d645a91705212ed742f285d      1
     83 0x83eed79eb18ba44e73a93b0b030e5044cc924f44      1
     84 0x850a3d0d157d4907dda29e15f33092ec1afb27c4      1
     85 0x85a30ede5e46029457e20d55427ee8673fad7762      1
     86 0x8608813a87fbc6ae1f107cee99ac30f955da9a9e      1
     87 0x877966054785c393ef68b2c4bc45d5676915c7e2      1
     88 0x8abe2749b67ad7debb7442c46ea4a0e003d4852a      1
     89 0x8d05c412f6b66ad39227763298c06c6196104b31      1
     90 0x94b08f4daf6b0b1501fe4297280ec99412f66030      1
     91 0x953a8dc0e0e2e8fdf8607e5c3c55dd53b2f4fba6      1
     92 0x9564307ca7793a87b8632059261c7b42cfb452ca      1
     93 0x97064190190d4e2fb284a2d55dd53504e291c4cd      1
     94 0x9720dbe3f2716ea6fbf56ab4469b902e4acaa182      1
     95 0x994908940ee4a9d39ff52063ebabd9ac3879a687      1
     96 0x9cbb4e39da99fa060770ed5b8db14914e4bd66f5      1
     97 0x9e1ccb3c9c55ab371d427745bf213df80cf23141      1
     98 0x9f35af4727fb91114563d8a8f779a792cb557f3f      1
     99 0xa3bcdd84a2a3a3ae09a8ebd1a7a2f142618397ce      1
    100 0xa43d0698b2afbe80c4080b12f1dc4648dfd0fea7      1
    101 0xa78b54a5486ce07838c1511c5ad5f72df2ac833a      1
    102 0xa7fb11eae44fc2ac891c018c97c4f529be910c14      1
    103 0xad569083dee565d4ddb648f32fa392d6d42864c6      1
    104 0xae33247dcd75250c4823db8433c1ed40bd63a27c      1
    105 0xb0bdd53b627d7e61cfc5c13ef110e47e210fac6f      1
    106 0xb3c3b3038bd2231b7b61a510102b459e694690a4      1
    107 0xb67a577a1855dc3a43df1d94b05a8f5d29570f89      1
    108 0xbc0398a920ae62e01f486f9fbee90ffeb46ad7a6      1
    109 0xbddb878b393bf91f8d34b2edf7087ca529e3ccaf      1
    110 0xc142dc5ceb96f73635d66b62bfde2a0eea4e4ce1      1
    111 0xc18d4c2fee42accfe7bfdce98bfacbabf76242e9      1
    112 0xc1eca986c20103f3a180d797f5be64702bae7b22      1
    113 0xc2547446c439be477dce96702fb1f9dfb61bab3a      1
    114 0xc9a9d8943680e5986dedff4e07082c83a39585c6      1
    115 0xca3671151f0ff704dc9f6f3dc9c5137f8e7153f8      1
    116 0xcb06bede7cb0a4b333581b6bdcd05f7cc737b9cc      1
    117 0xcb4de7db08b4dd0a9954fcfa0a09762f58436df0      1
    118 0xcd241563f7288b5eb2b9b715f736232889b62d8b      1
    119 0xd0762311e80eb6e1b1beecf398875ed4ea368876      1
    120 0xd0c3218303591c7ea68560509be2ef7b9e6d2191      1
    121 0xd2d667b236cf6dc95db080f9a8e6b1ce13971dd5      1
    122 0xd353a1a0051175b4ddacc8e6e6ee5f3976b907e8      1
    123 0xd521034f074037fe06080cb3aecdee5692a01666      1
    124 0xd641f147d75a05e701b48be05634e9ad0d706571      1
    125 0xd6dee643da993cf6be961f571cfaaacc444eccac      1
    126 0xd809a687e957761872b440909348fa6547cafabf      1
    127 0xdcffdb9fde25ab47d3412e3de4acf84ed034615a      1
    128 0xdf067d1c62b6a94e45ced3f01ebc94ae304cb045      1
    129 0xdf8b134fb7743acd805ecdee11335dd0cca921fc      1
    130 0xe1268b49d3e4e559f466239df4bee1d5cae04356      1
    131 0xe699a2d41f7acb6d933b771db02a46d10599b257      1
    132 0xe71fb70acb0195aa5ef969588fb0109acd9ef15b      1
    133 0xe8bee2f6bf252ed9efbbd483ca4093ace7350c92      1
    134 0xe9d53246209e4cd87dbc5d1fee71f44bff89a611      1
    135 0xea15534dcd3a12c78345357d0d496f7fa85a5b71      1
    136 0xed690606f908361c26bd2b1ad4fa31ed201dca4c      1
    137 0xed8982578836f7d498856b3f543f128bb8621f2a      1
    138 0xee2c055f7706b9dfcd98cd5a23d5629d6316c0bd      1
    139 0xf0199747902c5854fddfab51fe25684f1ef4c10a      1
    140 0xf18d8f1cfa7a24ed602128a46dcd9927dae6c05b      1
    141 0xf70ebfd828b993e2863670530c2ec62c049f37ad      1
    142 0xf7764568b698e5954892c00ed7d9a390b105c5f7      1
    143 0xf8986ce1fa0a656ee8d7e306c9182db06c6d6449      1
    144 0xf8e90d2d2f6f67a13d0a04374e22624a80a1e918      1
    145 0xfb1bb9d4a4c50457d339a686212a0716ea0adc80      1
    146 0xfcc3a7c8e7029f38e0109088c2a90ffa1518bde9      1
    147 0xfcf080d18e260a01246fd5c6e8a893a7def4d369      1
    148 0xfe2b8dc957459ac2a08fef57576bcb8a89845d8b      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 158 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x04b5a01e011352624245a5150d7f36e754ef8b62      1
      2 0x04c95c6f8ea7ff7c3f7a2f148fae75650c5a875e      1
      3 0x06486e323d993af90d544bd28ecbb6ce24a70937      1
      4 0x07f6f97ac29fb6882746e7c95797d30c1b50448f      1
      5 0x08493a92a95d4e93bdbc49f66e7647f1445a9210      1
      6 0x0a1c6f1638adbe3873c7e8dad6c0e3b08285ba07      1
      7 0x0a35c0d624855cf97e8eeb1ab2b0b53c6280d317      1
      8 0x0b30c8465cb2254a7a530011c872deb3f48f9ebe      1
      9 0x0bb75bef057da63a0ae4b25fe9adafd35cd92b87      1
     10 0x0ce390f18af702cca546297845a4a51d102123cf      1
     11 0x0da8ec9829af1b58d247078e398ce00420ddd942      1
     12 0x0f2005e5b1edf748a045d721e472886fee1815e4      1
     13 0x119fcd292b95551e8af472fc4721ac34cc5f48e5      1
     14 0x12d4e572cf6b44196c6327ff0eb879ca547f5c34      1
     15 0x13c39b3e6e6159bc233fad90825f1f9f84fcb102      1
     16 0x191d4c47676daa5abd35424f84f094f7e6a4ebb2      1
     17 0x193bb5dcb851ef8f75dbbd6950c962c3a5f4b0e3      1
     18 0x1b672e08c3cc6486b1c01d1114d5ced3c5682882      1
     19 0x1cb9998ab32e6d172f301a10ed8e0272d96509b4      1
     20 0x1d10b0166a761f269adf864ebdaca9be445ca637      1
     21 0x1d25029d92a7051f46a13be9a512443c966d3542      1
     22 0x1e4f4df03ef9dd1bceb2d3f9f2cb1ba18c816704      1
     23 0x20357eb5dc98da197c332d9fdd2a38ef58c91fb3      1
     24 0x2168b2295e912a27c0015bad9f09f090ebce1a99      1
     25 0x2177da04f9479496c2292d6344d306aa49beb34a      1
     26 0x235f32bb3920bf99e9fa813a37fcebadaf9cdf39      1
     27 0x23602ca06e977c86339ffddad74966e824ab691e      1
     28 0x248458947c120ca057ec028b3fe7e4b3f26fdb3d      1
     29 0x24fbadccd6684106e24065694ac87b0e98819235      1
     30 0x265a25670a194df9ff63c848827455da74042087      1
     31 0x27fb0a35ac158fe460b91b04e5087b0131352ed9      1
     32 0x287bdf8c332d44bb015f8b4deb6513010c951f39      1
     33 0x2886254556ff9b1572845865a0270a3a7f52b4ff      1
     34 0x28cf5d9d465dfaf5c616958ef8b23dbee567e2b7      1
     35 0x2bfca0be6ba3fa7c5f496d8967ae7c041faee0d2      1
     36 0x2dba88cb3b435f99a3e58b6e0fe450e8f1a3f20f      1
     37 0x32084ce62e8955da04ce2a2969bd6edbb4b346a5      1
     38 0x3330777cc5270db65e7df1d738e989dc16dd49f2      1
     39 0x3564ce014967c81e25f6349124673cf60b7ac572      1
     40 0x35a958858d46bcd9984360911bbaae44f4ef8f50      1
     41 0x3a051f1d0cf548f5c24893934bb69cbf61844c9f      1
     42 0x3a661a18cb0258119636dfdde098648c6ad5ba62      1
     43 0x3b846d4d8823fb12cc3694a2584c5c32057808c8      1
     44 0x41254fe73d3b49f0fe3cc7743b4d40ef26bb338e      1
     45 0x4220132c9df1ab7bd2913f0fd03297c90e7cc6fe      1
     46 0x48c3f2762c9e811bbc2e5dfbe0bb6bd36dec5247      1
     47 0x4b4d75b8b0fc7528ea2614b2ca83555824c07867      1
     48 0x4ca1c22a3bff99415c1095dc22b3595d2c54c458      1
     49 0x4cc2b3075c3cc735958a50ce1b3dd00173e97b04      1
     50 0x4cd5e58fb994b668a201e2166f933d8f998b5220      1
     51 0x4fb98d1499a24e9c794401e9fb96e9483a43d934      1
     52 0x5147c1a9e594041ccdba5d044b6274231dc6ebf5      1
     53 0x534e920f195732d77266e9b903829e13b3993164      1
     54 0x541a51cccc9fc7991ed0b1c3236e156e533b11a4      1
     55 0x5aa71418ac6238c78ea9a032a768dbbbf86bfeca      1
     56 0x5b33b6f41e87111b7d22685b297f086b5535034d      1
     57 0x5b513d7bb62f7f28bc3aba2d52fe6167a9b3a827      1
     58 0x5c3b3f880597349e7a1ae7034ff11ec6d7e909be      1
     59 0x5ce9ad759e41bf1b3dfc1a41db940a90d7a43460      1
     60 0x5f2abc70bb1567c7807da87e944ddf965a0846bd      1
     61 0x6079696f3d9fc53ba8350d8db44e048e6a3814b1      1
     62 0x60ce2c648e81d819c53d03bc6d7fa39ea5f40c6b      1
     63 0x618c7671366caaab6701f06ae9925ae682c6bf97      1
     64 0x628a4ff5077f8b083d56509f761c67bc7c85b754      1
     65 0x6811eaa638b04aedafeaddfca36c50ea303b1866      1
     66 0x69cb3b1de24e08f1cfc2994171b6c6930498f750      1
     67 0x6af7812fc932da2d34738a33f295de54011fd16b      1
     68 0x6b6ae848f555f70944bc99d736fe29fcaecf8b23      1
     69 0x6b88ccacf59e2bd4df9481c0c90d89936c9ad021      1
     70 0x6b8c6e15818c74895c31a1c91390b3d42b336799      1
     71 0x6c299fd0fbf1ec228f7e8d7fb6fab4ff8e8752f2      1
     72 0x6cb6b5f592438358704be91208a3a7420caea792      1
     73 0x6e37d79b4744dd82261cfb7b4ccf77fe513be06f      1
     74 0x71a7702ff3d858a0853a918fb44c537740a8bde9      1
     75 0x749f93bceb1f2512481112d149c938e15fb56e1b      1
     76 0x7556cc553036e8e67ca77b72fca81657988de13e      1
     77 0x75fe6f9af2e4b4fb9e86ebe26bb31162d292b315      1
     78 0x782adafbf47a604f146af4a059908e946eae539f      1
     79 0x799b27750e06d18fc9b2510e9d2201fc8a5d011b      1
     80 0x7be90f10f9c6cd02f32401c4929e5a5dbaa0a51b      1
     81 0x7c33a89a994e022434e930f70cb99fbbd3e2197c      1
     82 0x7cc696eeb48dc6acee89447168302a90c116bc34      1
     83 0x7ee8f8f465896f56edbdb5209015b64249c96ddc      1
     84 0x7fc200b56b7d0120193f034aa20a51eebe15eea2      1
     85 0x7fdba87da6ed499f19fccd1b8072b4dcf6991ffe      1
     86 0x80601bf6cf2af7cca21c4d85a0352f6c49aee512      1
     87 0x80ce7206da1ed2804d645a91705212ed742f285d      1
     88 0x83eed79eb18ba44e73a93b0b030e5044cc924f44      1
     89 0x850a3d0d157d4907dda29e15f33092ec1afb27c4      1
     90 0x85a30ede5e46029457e20d55427ee8673fad7762      1
     91 0x8608813a87fbc6ae1f107cee99ac30f955da9a9e      1
     92 0x877966054785c393ef68b2c4bc45d5676915c7e2      1
     93 0x8abe2749b67ad7debb7442c46ea4a0e003d4852a      1
     94 0x8d05c412f6b66ad39227763298c06c6196104b31      1
     95 0x94b08f4daf6b0b1501fe4297280ec99412f66030      1
     96 0x953a8dc0e0e2e8fdf8607e5c3c55dd53b2f4fba6      1
     97 0x9564307ca7793a87b8632059261c7b42cfb452ca      1
     98 0x97064190190d4e2fb284a2d55dd53504e291c4cd      1
     99 0x9720dbe3f2716ea6fbf56ab4469b902e4acaa182      1
    100 0x994908940ee4a9d39ff52063ebabd9ac3879a687      1
    101 0x9cbb4e39da99fa060770ed5b8db14914e4bd66f5      1
    102 0x9e1ccb3c9c55ab371d427745bf213df80cf23141      1
    103 0x9f35af4727fb91114563d8a8f779a792cb557f3f      1
    104 0xa3bcdd84a2a3a3ae09a8ebd1a7a2f142618397ce      1
    105 0xa43d0698b2afbe80c4080b12f1dc4648dfd0fea7      1
    106 0xa78b54a5486ce07838c1511c5ad5f72df2ac833a      1
    107 0xa7fb11eae44fc2ac891c018c97c4f529be910c14      1
    108 0xad569083dee565d4ddb648f32fa392d6d42864c6      1
    109 0xae33247dcd75250c4823db8433c1ed40bd63a27c      1
    110 0xb0bdd53b627d7e61cfc5c13ef110e47e210fac6f      1
    111 0xb3c3b3038bd2231b7b61a510102b459e694690a4      1
    112 0xb67a577a1855dc3a43df1d94b05a8f5d29570f89      1
    113 0xbc0398a920ae62e01f486f9fbee90ffeb46ad7a6      1
    114 0xbddb878b393bf91f8d34b2edf7087ca529e3ccaf      1
    115 0xc142dc5ceb96f73635d66b62bfde2a0eea4e4ce1      1
    116 0xc18d4c2fee42accfe7bfdce98bfacbabf76242e9      1
    117 0xc1eca986c20103f3a180d797f5be64702bae7b22      1
    118 0xc2547446c439be477dce96702fb1f9dfb61bab3a      1
    119 0xc449f005667bef849261b35accf931a4bace48fb      1
    120 0xc9a9d8943680e5986dedff4e07082c83a39585c6      1
    121 0xca3671151f0ff704dc9f6f3dc9c5137f8e7153f8      1
    122 0xcb06bede7cb0a4b333581b6bdcd05f7cc737b9cc      1
    123 0xcb4de7db08b4dd0a9954fcfa0a09762f58436df0      1
    124 0xcd241563f7288b5eb2b9b715f736232889b62d8b      1
    125 0xd0762311e80eb6e1b1beecf398875ed4ea368876      1
    126 0xd0c3218303591c7ea68560509be2ef7b9e6d2191      1
    127 0xd2d667b236cf6dc95db080f9a8e6b1ce13971dd5      1
    128 0xd2e03ea030322dfab19fd3975e822f2a16898a03      1
    129 0xd353a1a0051175b4ddacc8e6e6ee5f3976b907e8      1
    130 0xd4091d661a44648d61bd3bb51e129d0d60892056      1
    131 0xd521034f074037fe06080cb3aecdee5692a01666      1
    132 0xd641f147d75a05e701b48be05634e9ad0d706571      1
    133 0xd6dee643da993cf6be961f571cfaaacc444eccac      1
    134 0xd809a687e957761872b440909348fa6547cafabf      1
    135 0xdcffdb9fde25ab47d3412e3de4acf84ed034615a      1
    136 0xdf067d1c62b6a94e45ced3f01ebc94ae304cb045      1
    137 0xdf8b134fb7743acd805ecdee11335dd0cca921fc      1
    138 0xe1268b49d3e4e559f466239df4bee1d5cae04356      1
    139 0xe699a2d41f7acb6d933b771db02a46d10599b257      1
    140 0xe7014919c80cf68b64050290ca6d4c685648d14b      1
    141 0xe71fb70acb0195aa5ef969588fb0109acd9ef15b      1
    142 0xe8bee2f6bf252ed9efbbd483ca4093ace7350c92      1
    143 0xe9d53246209e4cd87dbc5d1fee71f44bff89a611      1
    144 0xea15534dcd3a12c78345357d0d496f7fa85a5b71      1
    145 0xed690606f908361c26bd2b1ad4fa31ed201dca4c      1
    146 0xed8982578836f7d498856b3f543f128bb8621f2a      1
    147 0xee2c055f7706b9dfcd98cd5a23d5629d6316c0bd      1
    148 0xf0199747902c5854fddfab51fe25684f1ef4c10a      1
    149 0xf18d8f1cfa7a24ed602128a46dcd9927dae6c05b      1
    150 0xf70ebfd828b993e2863670530c2ec62c049f37ad      1
    151 0xf7764568b698e5954892c00ed7d9a390b105c5f7      1
    152 0xf8986ce1fa0a656ee8d7e306c9182db06c6d6449      1
    153 0xf8e90d2d2f6f67a13d0a04374e22624a80a1e918      1
    154 0xf987a65115b267bc9a16f0eb52b7f7e94e554cbb      1
    155 0xfb1bb9d4a4c50457d339a686212a0716ea0adc80      1
    156 0xfcc3a7c8e7029f38e0109088c2a90ffa1518bde9      1
    157 0xfcf080d18e260a01246fd5c6e8a893a7def4d369      1
    158 0xfe2b8dc957459ac2a08fef57576bcb8a89845d8b      1

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
