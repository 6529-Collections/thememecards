
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance        contract        
     Length:208         Length:208         Min.   :1.000   Length:208        
     Class :character   Class :character   1st Qu.:1.000   Class :character  
     Mode  :character   Mode  :character   Median :1.000   Mode  :character  
                                           Mean   :1.043                     
                                           3rd Qu.:1.000                     
                                           Max.   :6.000                     
         name          
     Length:208        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 18646969 # https://etherscan.io/block/18646969
block_hash <- "0x693fbf053a3284264631e37a1f3998cd8be8506fc2433f9fdf45c30f64a99330"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4481 

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

allow_artist1    <- pick(snapshot, contracts=c("SuperRare","SuperRare2","Foundation","ART","ANIMATION","AfterTheEnd","Places"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("DecayingEditions","UlamaEditions","ISOMATRIXEditions","insideoutsideEditions","insideoutsideBurnEditions","SCROLLEditions","CYCLESEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("SuperRare","SuperRare2","Foundation","ART","ANIMATION","AfterTheEnd","Places","DecayingEditions","UlamaEditions","ISOMATRIXEditions","insideoutsideEditions","insideoutsideBurnEditions","SCROLLEditions","CYCLESEditions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 29 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x052564eb0fd8b340803df55def89c25c432f43f4      1
     2 0x0f0eae91990140c560d4156db4f00c854dc8f09e      1
     3 0x17dda0d374bf4246fbe320be4688be0d945f6812      1
     4 0x257a06d4a8949fe9e7bc61f0a5e754a8c46d3f4a      1
     5 0x2cd03d3ce4c5ec075f259ccc85d19706a9090dcc      1
     6 0x3113ab5c691ecdac25c07bbf2bf80eb005e4088c      1
     7 0x35cbedc3412d3c93dec4afa60ddf38c2a9c38865      1
     8 0x44ed32b27aebf445c209dab3ea7e3a9d50f37b2a      1
     9 0x5a39e2d5a81fe9e630ee574df446b78f742f5d9e      1
    10 0x6f1f500ee2b486d7755fbdea0c048672b07dbc73      1
    11 0x734dfdf8b3ef5f068c0b8766a3fabaccdb9f9c28      1
    12 0x77232c680649ebe49a48ef73b42d9940c6020983      1
    13 0x7ea63fd4ca570412275d6386ae0d40a66629b1f5      1
    14 0x83e6e89877c57c26bc090c6019c4004f8b8b0b07      1
    15 0x846978259adc90d5be03c7f2921af8b58857a1d6      1
    16 0xa17d53d78a2bbe050b8ebb00ceddb1d3d7bf7b32      1
    17 0xbc3613715492a94f76aaab581dcdfe61d8858b4b      1
    18 0xc1ae5c7603f36c513053e9220862d80a433524b0      1
    19 0xc7dc59ff6197cd1085913ee6079d9a2b59ea04e4      1
    20 0xcf541ee32f57cacfdd48e4554ebcd6cdf15759ca      1
    21 0xd86057c793bdfb77bb11dcabfe054ee4fd86ebdd      1
    22 0xd95dc76aa85c1cc1ac6894bf786e47f0cc131fd4      1
    23 0xda37896e56f12d640230a9e5115756a5cda9a581      1
    24 0xdb57a237bbd016f6da776b892fe7e75d8b495e62      1
    25 0xe35c3d08163da9bd4efa00879a78504d69820b5c      1
    26 0xe76091f84ddf27f9e773ca8bd2090830943f615c      1
    27 0xed3c3bffcf147850406d0e58ce4f4ebd2b5cd96c      1
    28 0xf91cfdc088368b23b5d03eeca53e5bd28cb8ec6d      1
    29 0xfbc76261fd55cf91b81a97dbcc2b3f6118f2b935      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 145 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x00ff192363430a35abbf968c535b64147e88abdb      1
      2 0x03ea64a5ced21e2cccfaea0aec283d0a0afe9424      1
      3 0x046cd19a339761c8f62ffa5c42023bc5e07e7c5c      1
      4 0x0513e58916432fdef1cc424f0f2b4cdfe438bc9a      1
      5 0x077bfc14dd6725f260e1abfd5c942ee13a27091b      1
      6 0x081e7a89b50656111b50dc35dc7242cf12bc96f4      1
      7 0x0cccb5199b2b6a4140dafd345a03a3cc8af6aa88      1
      8 0x0d9043db45efb536b95fa4495e4afdd14005875c      1
      9 0x13a7d2fb3f73b57ef539c35c4ec59ed49b1833ca      1
     10 0x1682df2df8142948cc3f6c6cb1150a0d53a3c9f7      1
     11 0x1b7690aebc8f986a780a19d74c5a9c293edb0d44      1
     12 0x1cd70bcccd6afff312700a14926448d3358b2377      1
     13 0x1f9dde7b25c87c97583d5771de13d60375aa3907      1
     14 0x209794bef29bb98ff7723f949e6bf893fd3bfb48      1
     15 0x21f63fec716869f726d309cd7cb65a1370463f65      1
     16 0x234e28c066ff70799cf74590be9f40ab08a63fd9      1
     17 0x242c6a1de1fb0ffc9c1783760e7a445f9a6a96b8      1
     18 0x24673b7ae3cd2f69a2e49b6d0a313f6a1007ecd5      1
     19 0x25acc72796bdf4579755708fdbc8409622d224f7      1
     20 0x25c13c0f5c957a7c50966de8141b4cd86627dd39      1
     21 0x26c46e6f7b37e3eea85b6edf0e95583d0bb292ad      1
     22 0x27c1f5ebca3f721cc44c19b650cbd8d48ff6c205      1
     23 0x285304c4cc2a512407cda997a805e551a5d5ad89      1
     24 0x289256fa6de33947fd292a9e94a738c3d986f8e5      1
     25 0x2894f4fe2c4dfc83b20445cf1511d9892fc7ba73      1
     26 0x2900af4543ac46aafc409d1791ffdace09363178      1
     27 0x2aaba4b94a805f0fea696dc11de324c2320ff4e1      1
     28 0x2f5963b32f07418cffc805d4a77f1d0bb5e38a81      1
     29 0x311f2b05f8c9dacf4443da9951443664368e726b      1
     30 0x32d3aa5df3203ac5613cd5b22c642929e3dae423      1
     31 0x36090d058f86da76134bb0980ebeb5c31693de55      1
     32 0x362dc61a04cb099ab6b912db07e0d6270342f16d      1
     33 0x38d261561db340087be6ce4448ea0943671126a0      1
     34 0x3c82fca06ca45ee7d91e8cda4783f0828803a9e1      1
     35 0x3e2dbda5f310aade1503557b2e7f356eaf161641      1
     36 0x3eeda66fdbf68ba99162fb59abc4d4be8dcf9a05      1
     37 0x3fc6c595a3445d56bf5d8322a392f3e027197c6a      1
     38 0x408630f7f8dd26f5e64b279b199e10dbafd6236b      1
     39 0x417bd2fb851ec7ff6b15376e663f950dcdbb54d2      1
     40 0x441ee5e616eefb10116d25fd2c69499eca8e9126      1
     41 0x45288feed8687238b947ba115b6119ae1c6cfbde      1
     42 0x46d7d9b3862aeafd771fb9ff08f038df6a727a1e      1
     43 0x473ac44a488ffc3f8237f45e469f60b416690f3d      1
     44 0x498d18b5b3c0b7f56635f3f52d4c466618ed21ad      1
     45 0x49dd1856f13f703d68a72947e9d143366c15bb49      1
     46 0x4cff7990f598a956d17ab11e80664bc8098d5df2      1
     47 0x50b14457fc25d32f3c5f330af97ec8ea1f9ec573      1
     48 0x526f1a63166306e97ae1e4e0245fd7f545799eb3      1
     49 0x590ef336b0617900bd61201cc34c3cceecd20381      1
     50 0x59968c2c25faf934664448f5ce2c022fce0b9398      1
     51 0x5a8df5afccd32bcab9b1fcf37da28b63289cbe92      1
     52 0x5b9097dcfdfaa0c67efd7c7186d3b81254731f9a      1
     53 0x5ce9ad759e41bf1b3dfc1a41db940a90d7a43460      1
     54 0x5e6684c5e5a5b1ee161dc2ab4f3bde5a8a7cb2bc      1
     55 0x5f2175a85aa9e7300c83be7958bf395dc7ec7795      1
     56 0x60e74f22084e28071bd9bbc72208932ed80e4c9e      1
     57 0x611909b813359e3facc833812559e07df37e418f      1
     58 0x614a61a3b7f2fd8750acaad63b2a0cfe8b8524f1      1
     59 0x623959f967b770bdb63c5b8beacaf51070466b10      1
     60 0x62ac7073454f5b8cd65558711161fc9f3436f76f      1
     61 0x640a23d365e30cbb635ad4cec456c20fc74455c8      1
     62 0x6493182cc538c8b4d60174bde8aaa6861f862b10      1
     63 0x66f5ddc253852f407d334f8e90e3bc46fdaaecaa      1
     64 0x674b645c64b23e7ddc0fa1f4ad9f33572b506951      1
     65 0x69c3541fd1eaa0da10cb8a40c5d45f2c9cb591f4      1
     66 0x6e15276248be70a3d1bcfe2852e75b81a9d6eafe      1
     67 0x6e977c8155d914e7aa99331169939978a4d1b102      1
     68 0x6eb7817d9c48c8aadd68ad694f9b14f3b4e3c450      1
     69 0x71cbdd648dfed6a77fe927dfa0bf3bb09106e5ab      1
     70 0x722e9468d7f8c2710b6dbfdf70f01b045aca1851      1
     71 0x72f8fa2f276006f31dedab47838dd6c09c263a62      1
     72 0x79e9aae53f3eff2a81557b92cde9bdf48b18cbc1      1
     73 0x7a0063f318abff5dd8a762290e30f710485baa8d      1
     74 0x7cc0a7154346b02669f9381c8d35155cee94c2c0      1
     75 0x7cc696eeb48dc6acee89447168302a90c116bc34      1
     76 0x803f27deaa1282653745c3abbd71df11e758944b      1
     77 0x810657d67c68cf7299f7ecbc321b293ee064c5df      1
     78 0x819dfc1b757c0ed67fea97e33e70cc4cc640f99d      1
     79 0x83ff31faa1fbe034b1bf17ad33759ed85da65e2f      1
     80 0x847768fa647de3f5735d6fb7638979ead0948aea      1
     81 0x84d620b80b7e6fbef7444715cce8984d5f236a52      1
     82 0x86aa7c2bb6b1844c0a67348b8761c3e98233a572      1
     83 0x8754e1ed406a72f8deca5b4c9654ad2db698ba83      1
     84 0x883ce32e186c69559e94c8ba276bfc4322fc194a      1
     85 0x8a3db9109f2ef0d234b6ca0b7a08f9bc63efd9a2      1
     86 0x8e63e4376dde62cb82f5a91c4bce4156457dedb1      1
     87 0x91aca13b42ac160c9e79a3cdcbb42cf68c1f15b4      1
     88 0x96e32dd09ff875fac038d41181cfbb2224a4573a      1
     89 0x97c592b9916e7327fedd77da55bd11ebaee78485      1
     90 0xa24e0affab0c33393bf41ba5475e54eaca80b2e6      1
     91 0xa6f95ffa23ca53d33390f3ad6d1da06f8a456ccf      1
     92 0xa7093e8a8bb5ba4300ed29822198d0367e7e6d66      1
     93 0xa837c3d5fdfe4e878a3f8370df89cfd972b116e1      1
     94 0xa9d508815d1216914706c03c566dc20dd972b383      1
     95 0xa9da2e4b36d75d3aee8630c6acd09fd567091f10      1
     96 0xab26422abc1e1d76161843facd1b38b19995716e      1
     97 0xab94f597b94f45680b07997c394535d7ebc4a297      1
     98 0xaf6461736ccf7756429a8e4c8db43d70026f8575      1
     99 0xb21866e83ce017a631b1244e225b19c85adb3ae2      1
    100 0xb7bbf49a39a22419401b72b6ed6e207e4b0ef924      1
    101 0xb8fd8687e854453c0b2a96fd559950fa9f3327d7      1
    102 0xb9433fd97274c124fff197f78e1c8b0299b3dd9f      1
    103 0xb98d10d9f6d07ba283bfd21b2dfec050f9ae282a      1
    104 0xba41c0b6c449fae362928e700abe76548325d826      1
    105 0xbbbf521d27aa03677bdf5b9829624bae061e18e8      1
    106 0xbf4e0b4a36d740ba54fdef3c15e45924d51e852c      1
    107 0xbfdf3266847b0cc9cf9bdc626bef48ff9c46e9cd      1
    108 0xc07f2785adfdd4ffe9288a1ab77ed5e147ade0bf      1
    109 0xc0f3e0854565ec00ecf2769fbd42654d6798c532      1
    110 0xc28d30dd716a40b0c19215b732ce7be0e80a5098      1
    111 0xc3fbc3f485f0d9b0bd21b13a4aaa8340160156cb      1
    112 0xc47ee8b101ab4a4f45aa2ec2518bf00d3ce794dd      1
    113 0xc578958dd1880cf00bffbb7feb9c28cbbbcad3bf      1
    114 0xc5aca861be2856d25821f3ef0317950c369044fa      1
    115 0xc8d8bbd63fd6ad5bec1b48ac08b6373caa8eb307      1
    116 0xcaafdfe9f4cd69ebba2ca0d3f1ee8da6075756f9      1
    117 0xcbace925cea953d062094c6a98bccf41aa31ce68      1
    118 0xcc4a2f7c97fd99b459a022368ef2e3ee4388de6b      1
    119 0xcdec862959d58d31958c15c171c4437ad9e6c711      1
    120 0xd04d1efeef7846d5664d9e1f925db4e6ab855c44      1
    121 0xd19286bdd1a2b06df3c1105407c38511ffb87959      1
    122 0xd3689e018d6a182d2806fa75b7bc580103f1b071      1
    123 0xd36d0ace6b18f13760376751af10741336131ce8      1
    124 0xd3a74341adac943de6600468393bb6ca4431a7fd      1
    125 0xd525262f779f8652b80fa2c4eac99f8a05be3cc5      1
    126 0xd573881d6126cc0dbcaab79b9f01235208fab675      1
    127 0xd668ee736188bafeb82bc118a05c5ab4524fd051      1
    128 0xd8599000833ab0b71de197162d3b89584b33ab98      1
    129 0xd8d1509a6a86585ac90296ac7703ad6d6ce7dc40      1
    130 0xd92df6b7fe238fe2c14fdb5816eebcc131b53360      1
    131 0xe052113bd7d7700d623414a0a4585bcae754e9d5      1
    132 0xe0580e24364dd221045c68a674f12666bd3e4253      1
    133 0xe4c882a729091448bf3c1bf13f12419d7c85072f      1
    134 0xe6d59c009e22fe01fc32d37b65fff9395f2a067d      1
    135 0xe7014919c80cf68b64050290ca6d4c685648d14b      1
    136 0xe726d0598cfbb879459451b9df5a481add1f36c2      1
    137 0xea05587655adae8ebd042397cf5fcd70abd384ab      1
    138 0xeba5c4630c35e5c85f9b6d63c50227b5ed93be1f      1
    139 0xf207e69a7cc3b4a00fec3039891a77b3df8c3577      1
    140 0xf38f67d8b16752b18f7358ef216a35423e45f806      1
    141 0xf45f7d6cc1d79dd677c3e976711cf29fc306f929      1
    142 0xf474f6d168648d3b6507cfc4546450dd10eb3470      1
    143 0xf5731ba57f5648562d159e912cbc2e921c8cd5d5      1
    144 0xfb3601ce2483506b90bed311d58a4c4bb74b23e2      1
    145 0xff397057ae73eb39251f87038009b616573f1c0d      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 174 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x00ff192363430a35abbf968c535b64147e88abdb      1
      2 0x03ea64a5ced21e2cccfaea0aec283d0a0afe9424      1
      3 0x046cd19a339761c8f62ffa5c42023bc5e07e7c5c      1
      4 0x0513e58916432fdef1cc424f0f2b4cdfe438bc9a      1
      5 0x052564eb0fd8b340803df55def89c25c432f43f4      1
      6 0x077bfc14dd6725f260e1abfd5c942ee13a27091b      1
      7 0x081e7a89b50656111b50dc35dc7242cf12bc96f4      1
      8 0x0cccb5199b2b6a4140dafd345a03a3cc8af6aa88      1
      9 0x0d9043db45efb536b95fa4495e4afdd14005875c      1
     10 0x0f0eae91990140c560d4156db4f00c854dc8f09e      1
     11 0x13a7d2fb3f73b57ef539c35c4ec59ed49b1833ca      1
     12 0x1682df2df8142948cc3f6c6cb1150a0d53a3c9f7      1
     13 0x17dda0d374bf4246fbe320be4688be0d945f6812      1
     14 0x1b7690aebc8f986a780a19d74c5a9c293edb0d44      1
     15 0x1cd70bcccd6afff312700a14926448d3358b2377      1
     16 0x1f9dde7b25c87c97583d5771de13d60375aa3907      1
     17 0x209794bef29bb98ff7723f949e6bf893fd3bfb48      1
     18 0x21f63fec716869f726d309cd7cb65a1370463f65      1
     19 0x234e28c066ff70799cf74590be9f40ab08a63fd9      1
     20 0x242c6a1de1fb0ffc9c1783760e7a445f9a6a96b8      1
     21 0x24673b7ae3cd2f69a2e49b6d0a313f6a1007ecd5      1
     22 0x257a06d4a8949fe9e7bc61f0a5e754a8c46d3f4a      1
     23 0x25acc72796bdf4579755708fdbc8409622d224f7      1
     24 0x25c13c0f5c957a7c50966de8141b4cd86627dd39      1
     25 0x26c46e6f7b37e3eea85b6edf0e95583d0bb292ad      1
     26 0x27c1f5ebca3f721cc44c19b650cbd8d48ff6c205      1
     27 0x285304c4cc2a512407cda997a805e551a5d5ad89      1
     28 0x289256fa6de33947fd292a9e94a738c3d986f8e5      1
     29 0x2894f4fe2c4dfc83b20445cf1511d9892fc7ba73      1
     30 0x2900af4543ac46aafc409d1791ffdace09363178      1
     31 0x2aaba4b94a805f0fea696dc11de324c2320ff4e1      1
     32 0x2cd03d3ce4c5ec075f259ccc85d19706a9090dcc      1
     33 0x2f5963b32f07418cffc805d4a77f1d0bb5e38a81      1
     34 0x3113ab5c691ecdac25c07bbf2bf80eb005e4088c      1
     35 0x311f2b05f8c9dacf4443da9951443664368e726b      1
     36 0x32d3aa5df3203ac5613cd5b22c642929e3dae423      1
     37 0x35cbedc3412d3c93dec4afa60ddf38c2a9c38865      1
     38 0x36090d058f86da76134bb0980ebeb5c31693de55      1
     39 0x362dc61a04cb099ab6b912db07e0d6270342f16d      1
     40 0x38d261561db340087be6ce4448ea0943671126a0      1
     41 0x3c82fca06ca45ee7d91e8cda4783f0828803a9e1      1
     42 0x3e2dbda5f310aade1503557b2e7f356eaf161641      1
     43 0x3eeda66fdbf68ba99162fb59abc4d4be8dcf9a05      1
     44 0x3fc6c595a3445d56bf5d8322a392f3e027197c6a      1
     45 0x408630f7f8dd26f5e64b279b199e10dbafd6236b      1
     46 0x417bd2fb851ec7ff6b15376e663f950dcdbb54d2      1
     47 0x441ee5e616eefb10116d25fd2c69499eca8e9126      1
     48 0x44ed32b27aebf445c209dab3ea7e3a9d50f37b2a      1
     49 0x45288feed8687238b947ba115b6119ae1c6cfbde      1
     50 0x46d7d9b3862aeafd771fb9ff08f038df6a727a1e      1
     51 0x473ac44a488ffc3f8237f45e469f60b416690f3d      1
     52 0x498d18b5b3c0b7f56635f3f52d4c466618ed21ad      1
     53 0x49dd1856f13f703d68a72947e9d143366c15bb49      1
     54 0x4cff7990f598a956d17ab11e80664bc8098d5df2      1
     55 0x50b14457fc25d32f3c5f330af97ec8ea1f9ec573      1
     56 0x526f1a63166306e97ae1e4e0245fd7f545799eb3      1
     57 0x590ef336b0617900bd61201cc34c3cceecd20381      1
     58 0x59968c2c25faf934664448f5ce2c022fce0b9398      1
     59 0x5a39e2d5a81fe9e630ee574df446b78f742f5d9e      1
     60 0x5a8df5afccd32bcab9b1fcf37da28b63289cbe92      1
     61 0x5b9097dcfdfaa0c67efd7c7186d3b81254731f9a      1
     62 0x5ce9ad759e41bf1b3dfc1a41db940a90d7a43460      1
     63 0x5e6684c5e5a5b1ee161dc2ab4f3bde5a8a7cb2bc      1
     64 0x5f2175a85aa9e7300c83be7958bf395dc7ec7795      1
     65 0x60e74f22084e28071bd9bbc72208932ed80e4c9e      1
     66 0x611909b813359e3facc833812559e07df37e418f      1
     67 0x614a61a3b7f2fd8750acaad63b2a0cfe8b8524f1      1
     68 0x623959f967b770bdb63c5b8beacaf51070466b10      1
     69 0x62ac7073454f5b8cd65558711161fc9f3436f76f      1
     70 0x640a23d365e30cbb635ad4cec456c20fc74455c8      1
     71 0x6493182cc538c8b4d60174bde8aaa6861f862b10      1
     72 0x66f5ddc253852f407d334f8e90e3bc46fdaaecaa      1
     73 0x674b645c64b23e7ddc0fa1f4ad9f33572b506951      1
     74 0x69c3541fd1eaa0da10cb8a40c5d45f2c9cb591f4      1
     75 0x6e15276248be70a3d1bcfe2852e75b81a9d6eafe      1
     76 0x6e977c8155d914e7aa99331169939978a4d1b102      1
     77 0x6eb7817d9c48c8aadd68ad694f9b14f3b4e3c450      1
     78 0x6f1f500ee2b486d7755fbdea0c048672b07dbc73      1
     79 0x71cbdd648dfed6a77fe927dfa0bf3bb09106e5ab      1
     80 0x722e9468d7f8c2710b6dbfdf70f01b045aca1851      1
     81 0x72f8fa2f276006f31dedab47838dd6c09c263a62      1
     82 0x734dfdf8b3ef5f068c0b8766a3fabaccdb9f9c28      1
     83 0x77232c680649ebe49a48ef73b42d9940c6020983      1
     84 0x79e9aae53f3eff2a81557b92cde9bdf48b18cbc1      1
     85 0x7a0063f318abff5dd8a762290e30f710485baa8d      1
     86 0x7cc0a7154346b02669f9381c8d35155cee94c2c0      1
     87 0x7cc696eeb48dc6acee89447168302a90c116bc34      1
     88 0x7ea63fd4ca570412275d6386ae0d40a66629b1f5      1
     89 0x803f27deaa1282653745c3abbd71df11e758944b      1
     90 0x810657d67c68cf7299f7ecbc321b293ee064c5df      1
     91 0x819dfc1b757c0ed67fea97e33e70cc4cc640f99d      1
     92 0x83e6e89877c57c26bc090c6019c4004f8b8b0b07      1
     93 0x83ff31faa1fbe034b1bf17ad33759ed85da65e2f      1
     94 0x846978259adc90d5be03c7f2921af8b58857a1d6      1
     95 0x847768fa647de3f5735d6fb7638979ead0948aea      1
     96 0x84d620b80b7e6fbef7444715cce8984d5f236a52      1
     97 0x86aa7c2bb6b1844c0a67348b8761c3e98233a572      1
     98 0x8754e1ed406a72f8deca5b4c9654ad2db698ba83      1
     99 0x883ce32e186c69559e94c8ba276bfc4322fc194a      1
    100 0x8a3db9109f2ef0d234b6ca0b7a08f9bc63efd9a2      1
    101 0x8e63e4376dde62cb82f5a91c4bce4156457dedb1      1
    102 0x91aca13b42ac160c9e79a3cdcbb42cf68c1f15b4      1
    103 0x96e32dd09ff875fac038d41181cfbb2224a4573a      1
    104 0x97c592b9916e7327fedd77da55bd11ebaee78485      1
    105 0xa17d53d78a2bbe050b8ebb00ceddb1d3d7bf7b32      1
    106 0xa24e0affab0c33393bf41ba5475e54eaca80b2e6      1
    107 0xa6f95ffa23ca53d33390f3ad6d1da06f8a456ccf      1
    108 0xa7093e8a8bb5ba4300ed29822198d0367e7e6d66      1
    109 0xa837c3d5fdfe4e878a3f8370df89cfd972b116e1      1
    110 0xa9d508815d1216914706c03c566dc20dd972b383      1
    111 0xa9da2e4b36d75d3aee8630c6acd09fd567091f10      1
    112 0xab26422abc1e1d76161843facd1b38b19995716e      1
    113 0xab94f597b94f45680b07997c394535d7ebc4a297      1
    114 0xaf6461736ccf7756429a8e4c8db43d70026f8575      1
    115 0xb21866e83ce017a631b1244e225b19c85adb3ae2      1
    116 0xb7bbf49a39a22419401b72b6ed6e207e4b0ef924      1
    117 0xb8fd8687e854453c0b2a96fd559950fa9f3327d7      1
    118 0xb9433fd97274c124fff197f78e1c8b0299b3dd9f      1
    119 0xb98d10d9f6d07ba283bfd21b2dfec050f9ae282a      1
    120 0xba41c0b6c449fae362928e700abe76548325d826      1
    121 0xbbbf521d27aa03677bdf5b9829624bae061e18e8      1
    122 0xbc3613715492a94f76aaab581dcdfe61d8858b4b      1
    123 0xbf4e0b4a36d740ba54fdef3c15e45924d51e852c      1
    124 0xbfdf3266847b0cc9cf9bdc626bef48ff9c46e9cd      1
    125 0xc07f2785adfdd4ffe9288a1ab77ed5e147ade0bf      1
    126 0xc0f3e0854565ec00ecf2769fbd42654d6798c532      1
    127 0xc1ae5c7603f36c513053e9220862d80a433524b0      1
    128 0xc28d30dd716a40b0c19215b732ce7be0e80a5098      1
    129 0xc3fbc3f485f0d9b0bd21b13a4aaa8340160156cb      1
    130 0xc47ee8b101ab4a4f45aa2ec2518bf00d3ce794dd      1
    131 0xc578958dd1880cf00bffbb7feb9c28cbbbcad3bf      1
    132 0xc5aca861be2856d25821f3ef0317950c369044fa      1
    133 0xc7dc59ff6197cd1085913ee6079d9a2b59ea04e4      1
    134 0xc8d8bbd63fd6ad5bec1b48ac08b6373caa8eb307      1
    135 0xcaafdfe9f4cd69ebba2ca0d3f1ee8da6075756f9      1
    136 0xcbace925cea953d062094c6a98bccf41aa31ce68      1
    137 0xcc4a2f7c97fd99b459a022368ef2e3ee4388de6b      1
    138 0xcdec862959d58d31958c15c171c4437ad9e6c711      1
    139 0xcf541ee32f57cacfdd48e4554ebcd6cdf15759ca      1
    140 0xd04d1efeef7846d5664d9e1f925db4e6ab855c44      1
    141 0xd19286bdd1a2b06df3c1105407c38511ffb87959      1
    142 0xd3689e018d6a182d2806fa75b7bc580103f1b071      1
    143 0xd36d0ace6b18f13760376751af10741336131ce8      1
    144 0xd3a74341adac943de6600468393bb6ca4431a7fd      1
    145 0xd525262f779f8652b80fa2c4eac99f8a05be3cc5      1
    146 0xd573881d6126cc0dbcaab79b9f01235208fab675      1
    147 0xd668ee736188bafeb82bc118a05c5ab4524fd051      1
    148 0xd8599000833ab0b71de197162d3b89584b33ab98      1
    149 0xd86057c793bdfb77bb11dcabfe054ee4fd86ebdd      1
    150 0xd8d1509a6a86585ac90296ac7703ad6d6ce7dc40      1
    151 0xd92df6b7fe238fe2c14fdb5816eebcc131b53360      1
    152 0xd95dc76aa85c1cc1ac6894bf786e47f0cc131fd4      1
    153 0xda37896e56f12d640230a9e5115756a5cda9a581      1
    154 0xdb57a237bbd016f6da776b892fe7e75d8b495e62      1
    155 0xe052113bd7d7700d623414a0a4585bcae754e9d5      1
    156 0xe0580e24364dd221045c68a674f12666bd3e4253      1
    157 0xe35c3d08163da9bd4efa00879a78504d69820b5c      1
    158 0xe4c882a729091448bf3c1bf13f12419d7c85072f      1
    159 0xe6d59c009e22fe01fc32d37b65fff9395f2a067d      1
    160 0xe7014919c80cf68b64050290ca6d4c685648d14b      1
    161 0xe726d0598cfbb879459451b9df5a481add1f36c2      1
    162 0xe76091f84ddf27f9e773ca8bd2090830943f615c      1
    163 0xea05587655adae8ebd042397cf5fcd70abd384ab      1
    164 0xeba5c4630c35e5c85f9b6d63c50227b5ed93be1f      1
    165 0xed3c3bffcf147850406d0e58ce4f4ebd2b5cd96c      1
    166 0xf207e69a7cc3b4a00fec3039891a77b3df8c3577      1
    167 0xf38f67d8b16752b18f7358ef216a35423e45f806      1
    168 0xf45f7d6cc1d79dd677c3e976711cf29fc306f929      1
    169 0xf474f6d168648d3b6507cfc4546450dd10eb3470      1
    170 0xf5731ba57f5648562d159e912cbc2e921c8cd5d5      1
    171 0xf91cfdc088368b23b5d03eeca53e5bd28cb8ec6d      1
    172 0xfb3601ce2483506b90bed311d58a4c4bb74b23e2      1
    173 0xfbc76261fd55cf91b81a97dbcc2b3f6118f2b935      1
    174 0xff397057ae73eb39251f87038009b616573f1c0d      1

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
