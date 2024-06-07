
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance        contract        
     Length:153         Length:153         Min.   :1.000   Length:153        
     Class :character   Class :character   1st Qu.:1.000   Class :character  
     Mode  :character   Mode  :character   Median :1.000   Mode  :character  
                                           Mean   :1.144                     
                                           3rd Qu.:1.000                     
                                           Max.   :4.000                     
         name          
     Length:153        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 19994869 # https://etherscan.io/block/19994869
block_hash <- "0x2d2d2ea9736d19b224505261b1bb28e6310e2d49e63c77742d104b8264b28e3e"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4396 

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

allow_artist1    <- pick(snapshot, contracts=c("SuperRare","WilleaZwey","SleepingSunTrilogy","Foundation","WilleaZwey2","MakersPlace","KnownOrigin"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("STMEditions","ThroughtheSpectrumEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("SuperRare","WilleaZwey","SleepingSunTrilogy","Foundation","WilleaZwey2","MakersPlace","KnownOrigin","STMEditions","ThroughtheSpectrumEditions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 12 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0256c28028cac3e504f1b6c20bf7b3f4631e0d97      1
     2 0x0c6150ccada83c564aca79ead230e3c766525123      1
     3 0x1d41071761c941e854838583d1be87a2449dd036      1
     4 0x2e0d63ffcb08ea20ff3acdbb72dfec97343885d2      1
     5 0x6112083d4bb12dbf64e4850842685aaefbeacb10      1
     6 0x7b2ad6af37be6a77c95556d9453e4877b6a8437d      1
     7 0x9734223269742c4c683c93a507cfb5a97252023a      1
     8 0xab0ca98528c1c7911cbfa8fb08abf5973f61e058      1
     9 0xe14397613d3c700d3abb63d15ff481d09841ad8f      1
    10 0xe35c3d08163da9bd4efa00879a78504d69820b5c      1
    11 0xed3c3bffcf147850406d0e58ce4f4ebd2b5cd96c      1
    12 0xf1e1c701b49b1dc2405a9e8ef40f9f88802b80fa      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 124 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x03aeda839a71edb348468d16fa948b08f2480329      1
      2 0x072c3ee4e4c5e4eabb7f484738797fbf260f056b      1
      3 0x07d683bc2cb3115d5bcc1809df33246d27a36155      1
      4 0x0851ced43aab7bc38b0fa4fbc4e3849634d2ca67      1
      5 0x086dbf36d98d310d53d19e97ea56ea973616a489      1
      6 0x0974ff7e79d93c462421f23520c0e6ba300dcb97      1
      7 0x0abe711f654bd6a79f07daa3f2f6a91bc0f6f804      1
      8 0x0bf714fe535527c30778aac815caa4283d09bab9      1
      9 0x10f827a55dc63fa709d0998b1ad30aa24baa61f8      1
     10 0x15c8d7d32fcb740eead55cc96afd2fa05cbe6e7d      1
     11 0x170925355c88970d05e03c039621bd88a4dd8fd7      1
     12 0x17e31bf839acb700e0f584797574a2c1fde46d0b      1
     13 0x185d5f60fb7c59d344ba939361908916cbffe1dd      1
     14 0x18892ed65ed8341fb06cfbc826c0c9ade3444b32      1
     15 0x1e8e749b2b578e181ca01962e9448006772b24a2      1
     16 0x1f77d34deff2b72b0a0258603effe72704742ebb      1
     17 0x23602ca06e977c86339ffddad74966e824ab691e      1
     18 0x287bdf8c332d44bb015f8b4deb6513010c951f39      1
     19 0x2a42cf4e1cfc33850fd8d8da24f86cd89b82ad95      1
     20 0x2d3fd9ee49189959368448a5cbc63213121d66b5      1
     21 0x32bc332d4b1a58661bc693fcd66d4f90915baf71      1
     22 0x340dc2e57d64e4f8a7010eb347fd1ca1b18bf935      1
     23 0x360873a5f94496cd51b9d806fa5d95f85110dd47      1
     24 0x3993996b09949bba655d98c02c87ea6abf553630      1
     25 0x3b68f514f081715b564e5d32650fe3b77d7bd083      1
     26 0x3bfc2f16535b4193f2fdfd9b22e9e23cda0f8cdc      1
     27 0x3c061e4f94f198c80a7a78b345c4a1d5450f9544      1
     28 0x3e1f1fc109b0130a8ba13a1546c713d030b8613f      1
     29 0x3e77fe1e72f93e891691831e684997e0de75b35f      1
     30 0x42d0be801d1c9dfd8c4fb3ed149b2a992399b17c      1
     31 0x4345243bf39fc858ad8ad92694c2e0a8852c2f31      1
     32 0x465899ad28ddf2c67e6813a2219c472f2ce81646      1
     33 0x478bb542f7658d635abba67edb987806dff5b83d      1
     34 0x4c89db12a2dbc077c41100df96f40dc858ed82ab      1
     35 0x4ccc0aa065a37a3589e2db6e165d2f8f522e9fa2      1
     36 0x506c23d8882b3768f7b410c3fb2d525b1d7cc466      1
     37 0x59ea25fdeef4233ffb4e2a5317a3bd7e2efa622c      1
     38 0x5e451b2d167c2a8faf0a8b23cff27996a28f28bc      1
     39 0x5f4a55a7662381e3305a455b159d7d0a71d12996      1
     40 0x5f6918bbeffa5889caed21eb5fe195fffadcfb58      1
     41 0x5f73b065c8412b43e3db33cad0efc5038040a597      1
     42 0x5fdb3913e9b0c2c868431c69d7f9f22c6d1af80b      1
     43 0x66b280b5778c35c719209614428caddf00aaa3ce      1
     44 0x684b3eca8b4a795e5a794b3dd1072a99a7dc9290      1
     45 0x688740f94475e78abf7c14095432b13c314838b8      1
     46 0x6a9e6d86475a3061674abea97b1cbd9f67815ead      1
     47 0x73ed02129dec10ceda24f06174d093a4342cbc2b      1
     48 0x77863da34ca9d4872b86fb9a4973792ca6a11e16      1
     49 0x7b768c81353cd5ff73b74e72dc7409cf789436a3      1
     50 0x7ce30498e564f081ca65a226f44b1751f93a0f82      1
     51 0x7decf7a31168778f311c57b9a948abaa7321001e      1
     52 0x7e852c524fc0f29cb4618186d19461755896021a      1
     53 0x7f02d0944832ae644faf30cd17f5e1340cabaf3a      1
     54 0x84b9cb209d28152103e7b03d54b286886ee05c67      1
     55 0x86d1bad3d001821a3c6d5c1e5016d21051734ed9      1
     56 0x87ac0553e62fc074bcbaf9d348cc12d41a4c041e      1
     57 0x885e4e895c3ea85ff99e6d9b0d2249f4708d9bc4      1
     58 0x89900edbf6b88e99ad849ea34d6e3df969631380      1
     59 0x8a4ae95f8c670f8bafcea5a05d9808c4af43f5f9      1
     60 0x8a4b89d76a1a745a4a1adebd3793253fba0adadc      1
     61 0x8c649fb4d82f579f3a6b89ed3ab442b47a366a4a      1
     62 0x8d743c2709ce032fbb2ac1b83cb33670e02e1acd      1
     63 0x8e9874b009afb8006ae6ebe1ba3926fd1a1b4104      1
     64 0x8f5f88f20392a5413dda4179fbe0716345499373      1
     65 0x9051b3c1716fb28044cfddb9f7296ca59d579302      1
     66 0x965e987057313c6c01c66c2364c266046a17c408      1
     67 0x97a2de7077568ae866c654ba09fd27ba7b58e51b      1
     68 0x995b7fabdae160217f378bbb05669aa4bdcdc81f      1
     69 0x99c7be4c85c4e9d26b7cab28a4e6dbfc8bc19178      1
     70 0x9e1a9aad3d0539c14cddb8d2dd67fc7bf4194522      1
     71 0x9f6ae0370d74f0e591c64cec4a8ae0d627817014      1
     72 0xa0aacab0adf5fa5b4be715e67a1c59dcd09befe8      1
     73 0xa278ac5baeb8c9d61009de01bf645b37a99f3cb0      1
     74 0xa28fd178c686beebd6818d6772e56f6e7a6ff5cc      1
     75 0xa42e1debeba42786a4d2b8226927291462271e96      1
     76 0xa7aa4b444a75f6df5fcae6e563755b14cf573728      1
     77 0xa7cafd18dd8bc1e23203058d66f89f0f0ee539d9      1
     78 0xa817b9bd1ece360e4a1692894a5ad1d40b889f20      1
     79 0xab3ba2d668215acd23f7da923683a5ed88bad625      1
     80 0xaf469c4a0914938e6149cf621c54fb4b1ec0c202      1
     81 0xb00b18b3bf6ecc43e6235ee69424d4a220437a4d      1
     82 0xb1ca39076de4929857d75d3cd586863331a86fef      1
     83 0xb3d099f77185631c809655087dbce232ed9e3574      1
     84 0xb3ffd0f321abde1740f6e13be273348baad24291      1
     85 0xb775df51776d6767f8c2f5a14b667e300f60447f      1
     86 0xb802162900a4e2d1b2472b14895d56a73ae647e8      1
     87 0xb860b5ccff8ecb2038a80c999474ffccad0d7f33      1
     88 0xbb68512e92e324e9759f62e437c77642c829797d      1
     89 0xbb99eff820de753e969a9c7e2863f17cd559b7e7      1
     90 0xc185ffb12406b8bd994c7805ed0339ce9f2529ec      1
     91 0xc23bcc018f438854b98a3d4aef677a989855e707      1
     92 0xc4c911aac7e8e1d7bf4056f95dc650ba1cefcef2      1
     93 0xc50cec6e366125b7877d11450745016e8c108c4d      1
     94 0xc542492296d3537eb3fd16fd775bdf7ab8721c7c      1
     95 0xc55764a21ce15b710200383c3ba8748456a1d5a5      1
     96 0xc68c7771ec6a6e5d67d62aa9c6f22df69865e401      1
     97 0xc721b0b9e17b32dc9f981eedbe4f70be04b96415      1
     98 0xcbcba035ad73896c62580bc8f292eb146ec6dd4a      1
     99 0xd6eb449e39a260dd2c17d08147482e43ed770e53      1
    100 0xd6f6adf60bbdd7b573cae0329ea85978ccb60448      1
    101 0xd8c73bcef080f33e37ea5a415bb0778ecd72ce3b      1
    102 0xdb8ff91c3c3327b0b7088b7f0976872588c4b907      1
    103 0xdbaefc954bd7a51b2f157d722e6150f30ee86714      1
    104 0xe27116b4e637e0b700ef213743fa1be45499899d      1
    105 0xe40a2786405b4ed534633c7c63420d8514ea097a      1
    106 0xe4a7ce553722e3879ff8d00a3a11a226414644e0      1
    107 0xe69e40a39a0cc4badac5e90e9a07ccf652ebea24      1
    108 0xe87aa360ae045d348fb645d0d8698682651f79d3      1
    109 0xe8c9333d6ba367ebd7ed1afd83088b5b5ea15d84      1
    110 0xeab3739d4e716bb07901d72dc94b4139b6968623      1
    111 0xeafff95282ba3053ff49bfb77fb37ef30754eb62      1
    112 0xebe326d8de3413f8132518dcfd45e6cbff7e5c27      1
    113 0xeda7e22434821de120c1ea688288fa8d4877f2e5      1
    114 0xf03ef3e139a037363a9a8e623999a69276449039      1
    115 0xf0605b4cf092635e7fcf48a338756817c75b5471      1
    116 0xf13db964fef5a68b497e2c56914f0d6d11511d71      1
    117 0xf3231261b5c5ed2beea362a02669eee8b9c45497      1
    118 0xf4cd60a92a7d20997d8dd3ed30eb7b340f05f135      1
    119 0xf880214e8647a7dce530fe48401bde8464cd68db      1
    120 0xf95753ea96c13245f64d868a8a00128be8f5e6f1      1
    121 0xf95add02599af4680ee9af9d4dca37c9945fc30b      1
    122 0xfa5e6434064a5f9569d287a06d3790bd8945299d      1
    123 0xfc0344564a36d3f572beca9cdaf67ca123bd4b45      1
    124 0xfd994b2ebce36f22ddcb810cd7da6ba92c21dd30      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 136 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x0256c28028cac3e504f1b6c20bf7b3f4631e0d97      1
      2 0x03aeda839a71edb348468d16fa948b08f2480329      1
      3 0x072c3ee4e4c5e4eabb7f484738797fbf260f056b      1
      4 0x07d683bc2cb3115d5bcc1809df33246d27a36155      1
      5 0x0851ced43aab7bc38b0fa4fbc4e3849634d2ca67      1
      6 0x086dbf36d98d310d53d19e97ea56ea973616a489      1
      7 0x0974ff7e79d93c462421f23520c0e6ba300dcb97      1
      8 0x0abe711f654bd6a79f07daa3f2f6a91bc0f6f804      1
      9 0x0bf714fe535527c30778aac815caa4283d09bab9      1
     10 0x0c6150ccada83c564aca79ead230e3c766525123      1
     11 0x10f827a55dc63fa709d0998b1ad30aa24baa61f8      1
     12 0x15c8d7d32fcb740eead55cc96afd2fa05cbe6e7d      1
     13 0x170925355c88970d05e03c039621bd88a4dd8fd7      1
     14 0x17e31bf839acb700e0f584797574a2c1fde46d0b      1
     15 0x185d5f60fb7c59d344ba939361908916cbffe1dd      1
     16 0x18892ed65ed8341fb06cfbc826c0c9ade3444b32      1
     17 0x1d41071761c941e854838583d1be87a2449dd036      1
     18 0x1e8e749b2b578e181ca01962e9448006772b24a2      1
     19 0x1f77d34deff2b72b0a0258603effe72704742ebb      1
     20 0x23602ca06e977c86339ffddad74966e824ab691e      1
     21 0x287bdf8c332d44bb015f8b4deb6513010c951f39      1
     22 0x2a42cf4e1cfc33850fd8d8da24f86cd89b82ad95      1
     23 0x2d3fd9ee49189959368448a5cbc63213121d66b5      1
     24 0x2e0d63ffcb08ea20ff3acdbb72dfec97343885d2      1
     25 0x32bc332d4b1a58661bc693fcd66d4f90915baf71      1
     26 0x340dc2e57d64e4f8a7010eb347fd1ca1b18bf935      1
     27 0x360873a5f94496cd51b9d806fa5d95f85110dd47      1
     28 0x3993996b09949bba655d98c02c87ea6abf553630      1
     29 0x3b68f514f081715b564e5d32650fe3b77d7bd083      1
     30 0x3bfc2f16535b4193f2fdfd9b22e9e23cda0f8cdc      1
     31 0x3c061e4f94f198c80a7a78b345c4a1d5450f9544      1
     32 0x3e1f1fc109b0130a8ba13a1546c713d030b8613f      1
     33 0x3e77fe1e72f93e891691831e684997e0de75b35f      1
     34 0x42d0be801d1c9dfd8c4fb3ed149b2a992399b17c      1
     35 0x4345243bf39fc858ad8ad92694c2e0a8852c2f31      1
     36 0x465899ad28ddf2c67e6813a2219c472f2ce81646      1
     37 0x478bb542f7658d635abba67edb987806dff5b83d      1
     38 0x4c89db12a2dbc077c41100df96f40dc858ed82ab      1
     39 0x4ccc0aa065a37a3589e2db6e165d2f8f522e9fa2      1
     40 0x506c23d8882b3768f7b410c3fb2d525b1d7cc466      1
     41 0x59ea25fdeef4233ffb4e2a5317a3bd7e2efa622c      1
     42 0x5e451b2d167c2a8faf0a8b23cff27996a28f28bc      1
     43 0x5f4a55a7662381e3305a455b159d7d0a71d12996      1
     44 0x5f6918bbeffa5889caed21eb5fe195fffadcfb58      1
     45 0x5f73b065c8412b43e3db33cad0efc5038040a597      1
     46 0x5fdb3913e9b0c2c868431c69d7f9f22c6d1af80b      1
     47 0x6112083d4bb12dbf64e4850842685aaefbeacb10      1
     48 0x66b280b5778c35c719209614428caddf00aaa3ce      1
     49 0x684b3eca8b4a795e5a794b3dd1072a99a7dc9290      1
     50 0x688740f94475e78abf7c14095432b13c314838b8      1
     51 0x6a9e6d86475a3061674abea97b1cbd9f67815ead      1
     52 0x73ed02129dec10ceda24f06174d093a4342cbc2b      1
     53 0x77863da34ca9d4872b86fb9a4973792ca6a11e16      1
     54 0x7b2ad6af37be6a77c95556d9453e4877b6a8437d      1
     55 0x7b768c81353cd5ff73b74e72dc7409cf789436a3      1
     56 0x7ce30498e564f081ca65a226f44b1751f93a0f82      1
     57 0x7decf7a31168778f311c57b9a948abaa7321001e      1
     58 0x7e852c524fc0f29cb4618186d19461755896021a      1
     59 0x7f02d0944832ae644faf30cd17f5e1340cabaf3a      1
     60 0x84b9cb209d28152103e7b03d54b286886ee05c67      1
     61 0x86d1bad3d001821a3c6d5c1e5016d21051734ed9      1
     62 0x87ac0553e62fc074bcbaf9d348cc12d41a4c041e      1
     63 0x885e4e895c3ea85ff99e6d9b0d2249f4708d9bc4      1
     64 0x89900edbf6b88e99ad849ea34d6e3df969631380      1
     65 0x8a4ae95f8c670f8bafcea5a05d9808c4af43f5f9      1
     66 0x8a4b89d76a1a745a4a1adebd3793253fba0adadc      1
     67 0x8c649fb4d82f579f3a6b89ed3ab442b47a366a4a      1
     68 0x8d743c2709ce032fbb2ac1b83cb33670e02e1acd      1
     69 0x8e9874b009afb8006ae6ebe1ba3926fd1a1b4104      1
     70 0x8f5f88f20392a5413dda4179fbe0716345499373      1
     71 0x9051b3c1716fb28044cfddb9f7296ca59d579302      1
     72 0x965e987057313c6c01c66c2364c266046a17c408      1
     73 0x9734223269742c4c683c93a507cfb5a97252023a      1
     74 0x97a2de7077568ae866c654ba09fd27ba7b58e51b      1
     75 0x995b7fabdae160217f378bbb05669aa4bdcdc81f      1
     76 0x99c7be4c85c4e9d26b7cab28a4e6dbfc8bc19178      1
     77 0x9e1a9aad3d0539c14cddb8d2dd67fc7bf4194522      1
     78 0x9f6ae0370d74f0e591c64cec4a8ae0d627817014      1
     79 0xa0aacab0adf5fa5b4be715e67a1c59dcd09befe8      1
     80 0xa278ac5baeb8c9d61009de01bf645b37a99f3cb0      1
     81 0xa28fd178c686beebd6818d6772e56f6e7a6ff5cc      1
     82 0xa42e1debeba42786a4d2b8226927291462271e96      1
     83 0xa7aa4b444a75f6df5fcae6e563755b14cf573728      1
     84 0xa7cafd18dd8bc1e23203058d66f89f0f0ee539d9      1
     85 0xa817b9bd1ece360e4a1692894a5ad1d40b889f20      1
     86 0xab0ca98528c1c7911cbfa8fb08abf5973f61e058      1
     87 0xab3ba2d668215acd23f7da923683a5ed88bad625      1
     88 0xaf469c4a0914938e6149cf621c54fb4b1ec0c202      1
     89 0xb00b18b3bf6ecc43e6235ee69424d4a220437a4d      1
     90 0xb1ca39076de4929857d75d3cd586863331a86fef      1
     91 0xb3d099f77185631c809655087dbce232ed9e3574      1
     92 0xb3ffd0f321abde1740f6e13be273348baad24291      1
     93 0xb775df51776d6767f8c2f5a14b667e300f60447f      1
     94 0xb802162900a4e2d1b2472b14895d56a73ae647e8      1
     95 0xb860b5ccff8ecb2038a80c999474ffccad0d7f33      1
     96 0xbb68512e92e324e9759f62e437c77642c829797d      1
     97 0xbb99eff820de753e969a9c7e2863f17cd559b7e7      1
     98 0xc185ffb12406b8bd994c7805ed0339ce9f2529ec      1
     99 0xc23bcc018f438854b98a3d4aef677a989855e707      1
    100 0xc4c911aac7e8e1d7bf4056f95dc650ba1cefcef2      1
    101 0xc50cec6e366125b7877d11450745016e8c108c4d      1
    102 0xc542492296d3537eb3fd16fd775bdf7ab8721c7c      1
    103 0xc55764a21ce15b710200383c3ba8748456a1d5a5      1
    104 0xc68c7771ec6a6e5d67d62aa9c6f22df69865e401      1
    105 0xc721b0b9e17b32dc9f981eedbe4f70be04b96415      1
    106 0xcbcba035ad73896c62580bc8f292eb146ec6dd4a      1
    107 0xd6eb449e39a260dd2c17d08147482e43ed770e53      1
    108 0xd6f6adf60bbdd7b573cae0329ea85978ccb60448      1
    109 0xd8c73bcef080f33e37ea5a415bb0778ecd72ce3b      1
    110 0xdb8ff91c3c3327b0b7088b7f0976872588c4b907      1
    111 0xdbaefc954bd7a51b2f157d722e6150f30ee86714      1
    112 0xe14397613d3c700d3abb63d15ff481d09841ad8f      1
    113 0xe27116b4e637e0b700ef213743fa1be45499899d      1
    114 0xe35c3d08163da9bd4efa00879a78504d69820b5c      1
    115 0xe40a2786405b4ed534633c7c63420d8514ea097a      1
    116 0xe4a7ce553722e3879ff8d00a3a11a226414644e0      1
    117 0xe69e40a39a0cc4badac5e90e9a07ccf652ebea24      1
    118 0xe87aa360ae045d348fb645d0d8698682651f79d3      1
    119 0xe8c9333d6ba367ebd7ed1afd83088b5b5ea15d84      1
    120 0xeab3739d4e716bb07901d72dc94b4139b6968623      1
    121 0xeafff95282ba3053ff49bfb77fb37ef30754eb62      1
    122 0xebe326d8de3413f8132518dcfd45e6cbff7e5c27      1
    123 0xed3c3bffcf147850406d0e58ce4f4ebd2b5cd96c      1
    124 0xeda7e22434821de120c1ea688288fa8d4877f2e5      1
    125 0xf03ef3e139a037363a9a8e623999a69276449039      1
    126 0xf0605b4cf092635e7fcf48a338756817c75b5471      1
    127 0xf13db964fef5a68b497e2c56914f0d6d11511d71      1
    128 0xf1e1c701b49b1dc2405a9e8ef40f9f88802b80fa      1
    129 0xf3231261b5c5ed2beea362a02669eee8b9c45497      1
    130 0xf4cd60a92a7d20997d8dd3ed30eb7b340f05f135      1
    131 0xf880214e8647a7dce530fe48401bde8464cd68db      1
    132 0xf95753ea96c13245f64d868a8a00128be8f5e6f1      1
    133 0xf95add02599af4680ee9af9d4dca37c9945fc30b      1
    134 0xfa5e6434064a5f9569d287a06d3790bd8945299d      1
    135 0xfc0344564a36d3f572beca9cdaf67ca123bd4b45      1
    136 0xfd994b2ebce36f22ddcb810cd7da6ba92c21dd30      1

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
