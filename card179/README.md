
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance        contract        
     Length:503         Length:503         Min.   :1.000   Length:503        
     Class :character   Class :character   1st Qu.:1.000   Class :character  
     Mode  :character   Mode  :character   Median :1.000   Mode  :character  
                                           Mean   :1.044                     
                                           3rd Qu.:1.000                     
                                           Max.   :5.000                     
         name          
     Length:503        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 18747269 # https://etherscan.io/block/18747269
block_hash <- "0x3640ce0c1687e9b8cd22a01cee40fb7e1099a00137edb8e39851f6ebd36f44d1"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4692 

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

allow_artist1    <- pick(snapshot, contracts=c("SuperRare","Gentlevandalism","FemmeDigitale","KnownOrigin","KnownOrigin2","MakersPlace","Foundation","WatercolorTuesday","LisaFogarty","2022CalendarGirls","Dollparts","WatercolorSirens"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("KnownOriginEditions","EditionsxLisaFogarty","VintageGirlsEditions","Watercolor2sdayEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("SuperRare","Gentlevandalism","FemmeDigitale","KnownOrigin","KnownOrigin2","MakersPlace","Foundation","WatercolorTuesday","LisaFogarty","2022CalendarGirls","Dollparts","WatercolorSirens","KnownOriginEditions","EditionsxLisaFogarty","VintageGirlsEditions","Watercolor2sdayEditions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 66 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x007880443b595eb375ab6b6566ad9a52630659ff      1
     2 0x035da52e7aa085f7e71764c0c8a1ce6690e3dfef      1
     3 0x0be23d08f9e5ca9f56c3c891f2cc3ade3a00feb0      1
     4 0x0c48427ec3f8eb65430c6812d3eed00669965742      1
     5 0x1423d7ba74817a2617fa3e69b609a54e4daf6f79      1
     6 0x1a50086cccfc2ed40dfa98aaee238a5dfcfc9424      1
     7 0x21301d901db04724597d1b6012ac49878157580d      1
     8 0x2652f12d0f15077f47c2d49f2d512cf0a4869dd2      1
     9 0x2719150aea3b6becfc85229f8a89b7f94aecce1b      1
    10 0x27d6b42f27bf312603a38f07edf29bcc510ba5a1      1
    11 0x2a903ac2b09124a7a5ab7874050cb217c0f9cf3e      1
    12 0x2c81cee7050c9d03295560851ffde123bdf9696a      1
    13 0x30a86a5b68e29ef151046937fc9bb435d4bb1679      1
    14 0x34a32ad4ba1ea1eb02ccd3ed5b9af9a8d8ea07a8      1
    15 0x365f7b46973f27740c08a077b66ec810cab39306      1
    16 0x3e5543a342446999ac11609af4beb82339ca41ae      1
    17 0x3fc6c595a3445d56bf5d8322a392f3e027197c6a      1
    18 0x406f4533b2f6209812d7ef9c7d69b8c54217c208      1
    19 0x4a2088df692b9d4d35346edf6bced1ca25582a43      1
    20 0x54aa7e866378e4ff3140aaf167571111cc9cb5d7      1
    21 0x5670c685e1dde13e6a9f7293ea6db0ac9090a7b4      1
    22 0x5ac80cfa44a7a54affd3ed29c6202ca9bd89f15f      1
    23 0x5be798bf258ac5319e74428a66e635df491de2b2      1
    24 0x621351604e300f3f4990aafcc17d0bb23f98ff6e      1
    25 0x645fd16f3508391e8385fc0a181076994079aa1b      1
    26 0x647c5714047295a799efcdc081dafd326a18357b      1
    27 0x65c7432e6662a96f4e999603991d5e929e57f60a      1
    28 0x676e09fac0b651c767c20d142a5df7fac8dbd027      1
    29 0x6e315531a3deb793e12eb7ab197df496de994029      1
    30 0x751f655ff4fbd7dd77f12e0f9ed5d0075cba1b5f      1
    31 0x80d7a0a61bf3c8982b74daf4a51966576b2ed5c6      1
    32 0x840f5d0fa8ac1f000287dfe54c97d4e76b058ef1      1
    33 0x8660f13931a44704327a645b6b7af94c403d18a2      1
    34 0x869ce3c7415f06db4ff6d599d629c1a9c7c8a820      1
    35 0x8ed4d3c67ee1a73f5df8ca3e10698f1cd4c5b14c      1
    36 0x8fc11454322ccc99884ab071d5f1ba6532dc491a      1
    37 0x91b8d9ea1c7c0f9660c72aa373a2a0d2b5ff28ee      1
    38 0x94de7e2c73529ebf3206aa3459e699fbcdfcd49b      1
    39 0x96951beae8a3c191800877cdde67dc12a5881584      1
    40 0x96ae24d0d512cbba22bee1045d4b4b6a75a21ec2      1
    41 0x97afa1a3507759dfe38c7d8fa00d3d17b3fcac7e      1
    42 0x987df219564cd274d32b4b442ffad1a185fc0509      1
    43 0x9ea493786d8be968a9652976543999456d0bb5fc      1
    44 0xa175029bff19b26b4a2e6da68e8bb909d6005fec      1
    45 0xb3d23c2de27b61f260b2b67cef8dbfcf42b00a12      1
    46 0xb9e5b44437744fdffc13a6f220a61d4cc44af624      1
    47 0xbfdf3266847b0cc9cf9bdc626bef48ff9c46e9cd      1
    48 0xc1add24ccd36ccdb949fb0a00d02e67612fb02bc      1
    49 0xc77b9a1efda8f3d87022cc8d323225a5a67fa902      1
    50 0xc8417e51eae510eb7595dbf6016318c771ddf0e7      1
    51 0xd1a861a8e288421bbb36ba6ac07590f88ec0452e      1
    52 0xd2a3f9c6fbe4c13d979898e603d64561264a6b35      1
    53 0xd387a6e4e84a6c86bd90c158c6028a58cc8ac459      1
    54 0xd9f22fadb01987622dd6137c5f3beada5f480458      1
    55 0xdb2b36c24064a0115fa0474fde4d9ba1f1099122      1
    56 0xe0945340f78dd8981f1f44968cc431f3f52495bc      1
    57 0xe25acc4ee8311f06bfb688f7363ac6def7650d28      1
    58 0xec693e2f81eb02bc9a9a6b2c7edaaa3734b4ffdc      1
    59 0xf1637adedeb89559c70481c3cb4f74ebac80d829      1
    60 0xf207e69a7cc3b4a00fec3039891a77b3df8c3577      1
    61 0xf70ebfd828b993e2863670530c2ec62c049f37ad      1
    62 0xf770e42fd25d745e9f25ab57cca3e96f7fe62d14      1
    63 0xf9de0d8cd1356d65d8045167387505e4456612fc      1
    64 0xfab9a3d37999e12252b47468d2ffd4be15936012      1
    65 0xfb7c8a716d8c9577ea293a28ff592bb967ea84c2      1
    66 0xff2450085510b5eb86c7f9451d5fbc0ca5a793aa      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 177 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x01bdcdc67014d0b3a9300f42413dcbd9003b1a91      1
      2 0x04c1c15a1554891fb2dd7c4939e7d95d06ee253f      1
      3 0x06186e1bfb2309c3cbc9dfed039a45d7187bd6d1      1
      4 0x065db2e3826165014e223ecb0513463b6dcb0c99      1
      5 0x0adfdf019fb9c5487d4cc65439091f48ffc2cecd      1
      6 0x0af67a9b598907cf15102ec6b21215989224adc3      1
      7 0x0bf3dbf258246cc63b07b51be90b9d188127b674      1
      8 0x0ec7273a197b219af34df5a552794a1f0fddedbb      1
      9 0x0efb853c40e9429ccdfe9564626da3a74ca3f5ec      1
     10 0x101864c31fabacb4138400d121ac07253e482da8      1
     11 0x10ac96adabc52de2909da2b1b3cb6821830449ba      1
     12 0x11f5ce814fbf0cda9e4a370d0f46a78f3dff9ae7      1
     13 0x123beaea9cdcc32ea24496f1790df00f3885eb73      1
     14 0x13804c166ced28dacaa3f61c15c100ad04ac53f2      1
     15 0x174749344cf308b6469bd1ad0c9197d19c66185e      1
     16 0x18c5ea5b6441d99d621cec21e9c034d97124d267      1
     17 0x18faba48193595f040e11af39c6e3ec3f6ad7086      1
     18 0x197b423b106ddf7e7463bca0c492fa79a3bb10bd      1
     19 0x19a44c4918f53a3c2fc75a0fe64a3265d8392a52      1
     20 0x19a68e5a24ead474cdbd1e74864317cc7ac3afa1      1
     21 0x1ad5991fc4aa356d0b13e012ba0015eb2a290702      1
     22 0x1e05dcea408fa504707a17ddb4b28145e762b8e7      1
     23 0x20a09a72de4bc2dbcc3c79a9b3f61aeea58c061a      1
     24 0x20a1e6ac98f4406aedec1623e7446f357d78e7c3      1
     25 0x21339e2ce5ce1d7a19a28e774513619c0c6259da      1
     26 0x22bd04906e808196b19f7d10463fe463ed5f88cb      1
     27 0x230f4981634d3c96ee9066f17652dc8b9049015f      1
     28 0x239a859b8a0e4aac4355b11ba81f6e9caa3fcac2      1
     29 0x249247f9dbbdb29bd6f61a19b176b347b473fdb9      1
     30 0x2503e93bb1b032f874e9b6b7920382f7d7555a49      1
     31 0x27c1f5ebca3f721cc44c19b650cbd8d48ff6c205      1
     32 0x2e08dab3417c6825f90c1fe07b5fa1b8753a9ca5      1
     33 0x2f27bd0821fb4f18cd4938c01f61adbed913f05d      1
     34 0x2f49d3a3e39c70cece9a064ab84202452a45ed2e      1
     35 0x354c2ab3f7a23f74cddc745b26aea53ec1602203      1
     36 0x3812ebe41b8d12e525b7983f4ac690470b7c26d0      1
     37 0x385fcbd4fdaa1ae35263141ba39b90f305016af4      1
     38 0x3a57b6f47f68fc3721f17fdc895b60b1cc1a2e40      1
     39 0x3a7c2b99d8a3a8064650025a5eb5cfb44c19814e      1
     40 0x3dbc93aecd50e485ecd2dd6dbe73d1ec19a4d839      1
     41 0x3f08a8029bca8aff2e9c0b828f2a3485c620a644      1
     42 0x44bd2863ec96b2ac852cfa9ca425dcb0a771310b      1
     43 0x478bb542f7658d635abba67edb987806dff5b83d      1
     44 0x482a13a18634574bdf495c03855d95c543804de6      1
     45 0x483388c064fdcd036ad9df3807332023fb1cbbe7      1
     46 0x4abf7e46e2b22da1d040bd240c4e965742b6ea8b      1
     47 0x4b3dcc15a8ab43128210fe3327bc830c36a15541      1
     48 0x4d72dbd59d76be47f1e9085842b7eca875644858      1
     49 0x4e216f28242b8d9270775e398cd19b93de693072      1
     50 0x4e90ad93240b10057d50fd365d968f122a23fa26      1
     51 0x501c8e70f1af355681aca5572d7ded26c188341c      1
     52 0x512c579153ac6fd961a7d9e7b19281b855aafbe1      1
     53 0x5290dbe2d1f7060ba2988a4e04b3b4679bc5ceee      1
     54 0x548ee9cf8322181edd2756281518b34507412ebb      1
     55 0x565196152f8a4066a51b498a44a9981145d441ff      1
     56 0x569e3f5d5e82325a52d4042ac99f24df923a414c      1
     57 0x56e96de10c5ff43b2de9522f2af1e577c2de5f8c      1
     58 0x5a04ad067b3a022bbc03dd49f105d462b6bbc1ff      1
     59 0x5b38576cf213064ef9582e2b90ad30adc3de6add      1
     60 0x61ecbafeb82b778592a6f8c6ec6b6f5b39fbcbd7      1
     61 0x62cf3d7a2bf7672eb087c439f5d1fa4e2093a732      1
     62 0x63569c554e62aa224bec21d43d9a184e8333fbff      1
     63 0x649444a98edc0c5d351459b925ec08572c1a1757      1
     64 0x6634f9d0c6b7fb5764312bd0c91556f51e67e544      1
     65 0x6749edd685f68135ed629fa5cde1269ba18d6ae9      1
     66 0x67ade1994b5d1806d2489cd57dac4c6635145238      1
     67 0x6834523fd1cee0997eeda9152420c5dbbaf1b95f      1
     68 0x68aac226ac458fb0d1deea0cc647be0b52fcea77      1
     69 0x6b88ccacf59e2bd4df9481c0c90d89936c9ad021      1
     70 0x6e388502b891ca05eb52525338172f261c31b7d3      1
     71 0x6f33d2bd3a13e8520f35b2a8495c64c6083eb8cd      1
     72 0x7035587a2d404cd9e9ff4625aaf4be7eb6a40b1c      1
     73 0x711fb130ce68576bf819683598efe46bf5e149fb      1
     74 0x717c034228c9584f0115aae20a4b52794d4a26ca      1
     75 0x75cd5c5f37e05797a61cffefb3d3f334f2d7fc1b      1
     76 0x762da606029d3120735aa1eec15464e265db7a3c      1
     77 0x7670700748cc4dc1102272b700bfad4d0b575edd      1
     78 0x7895e668bd43c9623af538f274c06cb22cbc74f2      1
     79 0x78ade9904cbcac9058ea8f986a10c0c5b53e1530      1
     80 0x79745bf97a3062ee487d217e0fedbd0258495679      1
     81 0x7b2c87a10c5c3fd0d838c0516b06c5c2344c57fe      1
     82 0x7b97a02a3a2922742590eda6e8e927ce71a677fa      1
     83 0x7c5504688eb4f81ed558550066dc218ffee1f6c8      1
     84 0x7c789d13654fc6cb030b3cb23e8ea54e72b17b58      1
     85 0x7e7a58aebff9a27b0405d0b9e2849c6c730aeed8      1
     86 0x7ebe5dc1faef7d3c90761de04be429cd0f36b594      1
     87 0x7fc55376d5a29e0ee86c18c81bb2fc8f9f490e50      1
     88 0x808421753a181e96812796b7ab43d3f356cc5a77      1
     89 0x81425a9b654d033aee76ab174a35991325d0bda9      1
     90 0x81cc15402a47f18e210aa57fb9370a1b5525047e      1
     91 0x832f4f4f88ab27a00b7683c9b5fc191e797187f3      1
     92 0x84f4bf35863eb02c9fa55f375bdf1f8984d0694f      1
     93 0x84f8bff48b845e7cc2f5a9175860ef6b0016c1d6      1
     94 0x85acf1dd1827ecd3749e21205a5cdc2d53998b71      1
     95 0x8619237fca77bd1c572841252823e84d34e59aac      1
     96 0x86a35e57968f0d6591b1886594c6b7721fc96943      1
     97 0x88eb1efd5dd34926b4303dc8bf9818bb7b7fc385      1
     98 0x8979b46eb58325125662ee55adafd07271e87c3d      1
     99 0x89d53e8612e8416bf960dc9444ce0e2a0878a582      1
    100 0x8dd00c7b5e2eaaf895f865cd9ce87f11cc56af6e      1
    101 0x8e58e01f3aa73fcd9be68039361b5342131f0c21      1
    102 0x8e8890d8ea6446c599487173c233ecfa8e7a4ddb      1
    103 0x9318f724edfcf60066a924ddb9133a312becf0f7      1
    104 0x94308832672c4e616503dc2010b605ff9057b96a      1
    105 0x97b28492da285daff9153fe89f354e7547e4206e      1
    106 0x98fe967044e428cc279160435956200b14896071      1
    107 0x9aa74ec75fbec40075a048e254c43aed324c33b6      1
    108 0x9c5fc4242d7ff6bf9d687e7be04c712d21c288cb      1
    109 0x9d7c2a03144d739aa462b6342ad656cc230a6823      1
    110 0x9f35af4727fb91114563d8a8f779a792cb557f3f      1
    111 0xa1a7a50abc188d834699bda055b072d4d1eaf239      1
    112 0xa2c6aa19102333335f83fae188d4d988ca948e8d      1
    113 0xa52d0ca88c0692e00634f5e26a57201b60b3e8dd      1
    114 0xa62be8397d8526566aa48f03149a050069075973      1
    115 0xa67f71d39574b6cde8bd5fae38d9e4c7a55e29df      1
    116 0xa7625e30eb5356a0b777e9617b55a92dddff2975      1
    117 0xa84e7cc73ae095bed288a799aa6f870f52fce6b4      1
    118 0xa891fde42e982148b5f2f6b06f04be8e706a190e      1
    119 0xaa34dde115e593dd26d0e6b250321803371dfc95      1
    120 0xabfc4b16844d99c9e0c8658858b930659713eb23      1
    121 0xadf7bb6e352ca256dbc814250c8d037526435792      1
    122 0xae342bc1ddace51f674151633f9fb3f52440e961      1
    123 0xb0627032a9381850e3ba7b749898d061ae74b5eb      1
    124 0xb07cf1b6d7661de58a59a30845b4a39b3906884a      1
    125 0xb4f98a050c9ba884e6bf2cdf0fe24466486d199c      1
    126 0xb95a3bf65c10a7ee64af73b028c667240e4a9955      1
    127 0xb963fa652274e887e7ab9876f436e054dfb3c3cd      1
    128 0xb981602bae363a3f6d6e7f9a911e8df1c1c60efb      1
    129 0xb98d10d9f6d07ba283bfd21b2dfec050f9ae282a      1
    130 0xbc2c2d0d64a98a1d66a4adf70cc01fe5bb14aca1      1
    131 0xbd68d765fd1c6607ee24265027463adf8312771d      1
    132 0xbd9a54c0cc23e75029116558d5bfc8e234743b0b      1
    133 0xc046b22e64e59d19831035a6e3a51fbed49bfed8      1
    134 0xc17e7bc7e3dfd514271d3de935f0c82737642037      1
    135 0xc3b4b9d5e0f633bfbf890ef182a10b119f16b7a6      1
    136 0xc5a6d80bc7a2c3093a68e2ad05e8205dc0c0726b      1
    137 0xc79d31c1f25dbbe3b79287ecd273b505d9b1b3d1      1
    138 0xc84d319bc74036c09cfb9dd5179c499ec6416aae      1
    139 0xc87e5b034d29da3976de8bf7528830aba0d6d82a      1
    140 0xca9c927f65a16202e2288c26841ccf104ce353a7      1
    141 0xceb6f6e28b178ac1a4a1341c5a294e8eb6f6eff4      1
    142 0xcf3a375a6b3ead8423bfa813c4127d1e027fe045      1
    143 0xd077b1cb1e0216609adfd9c4439c6d6b5787c8c2      1
    144 0xd12e60a79f53e056bfd3d53cf621d7555cd92c92      1
    145 0xd23505ed1641f2f9e44039a8efca1dca9a5329d1      1
    146 0xd64110f209e3f77580b72a96ba9be29517e41221      1
    147 0xd81ce8e89dd987c8ab630858c8f1e9df14788c35      1
    148 0xd89a6b9dcccc55192dab28fa74b1d9ce3e331d5a      1
    149 0xd8d97db2a8caec33d58420aaf637cb41058cb58d      1
    150 0xda47a6b9ec624f9d8cfe1db6423a26cbebc1246d      1
    151 0xdcae87821fa6caea05dbc2811126f4bc7ff73bd1      1
    152 0xdce2a14e55a3805ccd5d7cc80ca897ce5c0fa3a6      1
    153 0xdd1e5fad1b867499bfb1987bb783a17ced9a1171      1
    154 0xdd33abc5cf2dbe88da6f6dad71237ef7f7c6385b      1
    155 0xdd4370b1813a26663a4231840d4995ee54946423      1
    156 0xdfe9def041c0a05d3de86d24adaa8f9a31cd0118      1
    157 0xe089451137197d9922d02c650bbb9a197cab06c0      1
    158 0xe0b93aaa75212ef2839738e6fc139c36bdfef33e      1
    159 0xe1dc03950c2a646400fdedef7b74e1bfbcb5cfc1      1
    160 0xe288a00df4b697606078876788e4d64633cd2e01      1
    161 0xe46105bbda606babb797b343b21f930668e83c1d      1
    162 0xe7d0ea2014336c5c0b4dc01a54b2f27616331de6      1
    163 0xe8c9e155b48f4ed8f79d080fc5d3e2326e671b40      1
    164 0xe92ee21cb27e0d439f0a1a375b4edc4a71f85efc      1
    165 0xeb0e194d60f307391bbeffdc6d4bec8f56aac045      1
    166 0xed4dc1d10830480d49459f030e396c031536d14a      1
    167 0xedba5d56d0147aee8a227d284bcaac03b4a87ed4      1
    168 0xedd176ab2506193899a974829a05f2b7572aa9fd      1
    169 0xedda20e0cc11fae46747868fcea07e60c9b25379      1
    170 0xef0159e704d06c888a140a50e06b3eab8375b538      1
    171 0xef12add353ffc63c5e9489a6943e28f89cb4b378      1
    172 0xf24910880c3b65dd40a30969443557b62478e535      1
    173 0xf604e0e9461772014f0a250efa4b7d83573cd6d3      1
    174 0xfb40034905b34753b7d74223e496518d43548336      1
    175 0xfe97f3ba5da378cd83f94b02de8358045491d48f      1
    176 0xff13f9b23a16a82c3ccdd29ece6af667eeb15834      1
    177 0xff839b277ac4a40e458caff575dc24e129298cd1      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 243 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x007880443b595eb375ab6b6566ad9a52630659ff      1
      2 0x01bdcdc67014d0b3a9300f42413dcbd9003b1a91      1
      3 0x035da52e7aa085f7e71764c0c8a1ce6690e3dfef      1
      4 0x04c1c15a1554891fb2dd7c4939e7d95d06ee253f      1
      5 0x06186e1bfb2309c3cbc9dfed039a45d7187bd6d1      1
      6 0x065db2e3826165014e223ecb0513463b6dcb0c99      1
      7 0x0adfdf019fb9c5487d4cc65439091f48ffc2cecd      1
      8 0x0af67a9b598907cf15102ec6b21215989224adc3      1
      9 0x0be23d08f9e5ca9f56c3c891f2cc3ade3a00feb0      1
     10 0x0bf3dbf258246cc63b07b51be90b9d188127b674      1
     11 0x0c48427ec3f8eb65430c6812d3eed00669965742      1
     12 0x0ec7273a197b219af34df5a552794a1f0fddedbb      1
     13 0x0efb853c40e9429ccdfe9564626da3a74ca3f5ec      1
     14 0x101864c31fabacb4138400d121ac07253e482da8      1
     15 0x10ac96adabc52de2909da2b1b3cb6821830449ba      1
     16 0x11f5ce814fbf0cda9e4a370d0f46a78f3dff9ae7      1
     17 0x123beaea9cdcc32ea24496f1790df00f3885eb73      1
     18 0x13804c166ced28dacaa3f61c15c100ad04ac53f2      1
     19 0x1423d7ba74817a2617fa3e69b609a54e4daf6f79      1
     20 0x174749344cf308b6469bd1ad0c9197d19c66185e      1
     21 0x18c5ea5b6441d99d621cec21e9c034d97124d267      1
     22 0x18faba48193595f040e11af39c6e3ec3f6ad7086      1
     23 0x197b423b106ddf7e7463bca0c492fa79a3bb10bd      1
     24 0x19a44c4918f53a3c2fc75a0fe64a3265d8392a52      1
     25 0x19a68e5a24ead474cdbd1e74864317cc7ac3afa1      1
     26 0x1a50086cccfc2ed40dfa98aaee238a5dfcfc9424      1
     27 0x1ad5991fc4aa356d0b13e012ba0015eb2a290702      1
     28 0x1e05dcea408fa504707a17ddb4b28145e762b8e7      1
     29 0x20a09a72de4bc2dbcc3c79a9b3f61aeea58c061a      1
     30 0x20a1e6ac98f4406aedec1623e7446f357d78e7c3      1
     31 0x21301d901db04724597d1b6012ac49878157580d      1
     32 0x21339e2ce5ce1d7a19a28e774513619c0c6259da      1
     33 0x22bd04906e808196b19f7d10463fe463ed5f88cb      1
     34 0x230f4981634d3c96ee9066f17652dc8b9049015f      1
     35 0x239a859b8a0e4aac4355b11ba81f6e9caa3fcac2      1
     36 0x249247f9dbbdb29bd6f61a19b176b347b473fdb9      1
     37 0x2503e93bb1b032f874e9b6b7920382f7d7555a49      1
     38 0x2652f12d0f15077f47c2d49f2d512cf0a4869dd2      1
     39 0x2719150aea3b6becfc85229f8a89b7f94aecce1b      1
     40 0x27c1f5ebca3f721cc44c19b650cbd8d48ff6c205      1
     41 0x27d6b42f27bf312603a38f07edf29bcc510ba5a1      1
     42 0x2a903ac2b09124a7a5ab7874050cb217c0f9cf3e      1
     43 0x2c81cee7050c9d03295560851ffde123bdf9696a      1
     44 0x2e08dab3417c6825f90c1fe07b5fa1b8753a9ca5      1
     45 0x2f27bd0821fb4f18cd4938c01f61adbed913f05d      1
     46 0x2f49d3a3e39c70cece9a064ab84202452a45ed2e      1
     47 0x30a86a5b68e29ef151046937fc9bb435d4bb1679      1
     48 0x34a32ad4ba1ea1eb02ccd3ed5b9af9a8d8ea07a8      1
     49 0x354c2ab3f7a23f74cddc745b26aea53ec1602203      1
     50 0x365f7b46973f27740c08a077b66ec810cab39306      1
     51 0x3812ebe41b8d12e525b7983f4ac690470b7c26d0      1
     52 0x385fcbd4fdaa1ae35263141ba39b90f305016af4      1
     53 0x3a57b6f47f68fc3721f17fdc895b60b1cc1a2e40      1
     54 0x3a7c2b99d8a3a8064650025a5eb5cfb44c19814e      1
     55 0x3dbc93aecd50e485ecd2dd6dbe73d1ec19a4d839      1
     56 0x3e5543a342446999ac11609af4beb82339ca41ae      1
     57 0x3f08a8029bca8aff2e9c0b828f2a3485c620a644      1
     58 0x3fc6c595a3445d56bf5d8322a392f3e027197c6a      1
     59 0x406f4533b2f6209812d7ef9c7d69b8c54217c208      1
     60 0x44bd2863ec96b2ac852cfa9ca425dcb0a771310b      1
     61 0x478bb542f7658d635abba67edb987806dff5b83d      1
     62 0x482a13a18634574bdf495c03855d95c543804de6      1
     63 0x483388c064fdcd036ad9df3807332023fb1cbbe7      1
     64 0x4a2088df692b9d4d35346edf6bced1ca25582a43      1
     65 0x4abf7e46e2b22da1d040bd240c4e965742b6ea8b      1
     66 0x4b3dcc15a8ab43128210fe3327bc830c36a15541      1
     67 0x4d72dbd59d76be47f1e9085842b7eca875644858      1
     68 0x4e216f28242b8d9270775e398cd19b93de693072      1
     69 0x4e90ad93240b10057d50fd365d968f122a23fa26      1
     70 0x501c8e70f1af355681aca5572d7ded26c188341c      1
     71 0x512c579153ac6fd961a7d9e7b19281b855aafbe1      1
     72 0x5290dbe2d1f7060ba2988a4e04b3b4679bc5ceee      1
     73 0x548ee9cf8322181edd2756281518b34507412ebb      1
     74 0x54aa7e866378e4ff3140aaf167571111cc9cb5d7      1
     75 0x565196152f8a4066a51b498a44a9981145d441ff      1
     76 0x5670c685e1dde13e6a9f7293ea6db0ac9090a7b4      1
     77 0x569e3f5d5e82325a52d4042ac99f24df923a414c      1
     78 0x56e96de10c5ff43b2de9522f2af1e577c2de5f8c      1
     79 0x5a04ad067b3a022bbc03dd49f105d462b6bbc1ff      1
     80 0x5ac80cfa44a7a54affd3ed29c6202ca9bd89f15f      1
     81 0x5b38576cf213064ef9582e2b90ad30adc3de6add      1
     82 0x5be798bf258ac5319e74428a66e635df491de2b2      1
     83 0x61ecbafeb82b778592a6f8c6ec6b6f5b39fbcbd7      1
     84 0x621351604e300f3f4990aafcc17d0bb23f98ff6e      1
     85 0x62cf3d7a2bf7672eb087c439f5d1fa4e2093a732      1
     86 0x63569c554e62aa224bec21d43d9a184e8333fbff      1
     87 0x645fd16f3508391e8385fc0a181076994079aa1b      1
     88 0x647c5714047295a799efcdc081dafd326a18357b      1
     89 0x649444a98edc0c5d351459b925ec08572c1a1757      1
     90 0x65c7432e6662a96f4e999603991d5e929e57f60a      1
     91 0x6634f9d0c6b7fb5764312bd0c91556f51e67e544      1
     92 0x6749edd685f68135ed629fa5cde1269ba18d6ae9      1
     93 0x676e09fac0b651c767c20d142a5df7fac8dbd027      1
     94 0x67ade1994b5d1806d2489cd57dac4c6635145238      1
     95 0x6834523fd1cee0997eeda9152420c5dbbaf1b95f      1
     96 0x68aac226ac458fb0d1deea0cc647be0b52fcea77      1
     97 0x6b88ccacf59e2bd4df9481c0c90d89936c9ad021      1
     98 0x6e315531a3deb793e12eb7ab197df496de994029      1
     99 0x6e388502b891ca05eb52525338172f261c31b7d3      1
    100 0x6f33d2bd3a13e8520f35b2a8495c64c6083eb8cd      1
    101 0x7035587a2d404cd9e9ff4625aaf4be7eb6a40b1c      1
    102 0x711fb130ce68576bf819683598efe46bf5e149fb      1
    103 0x717c034228c9584f0115aae20a4b52794d4a26ca      1
    104 0x751f655ff4fbd7dd77f12e0f9ed5d0075cba1b5f      1
    105 0x75cd5c5f37e05797a61cffefb3d3f334f2d7fc1b      1
    106 0x762da606029d3120735aa1eec15464e265db7a3c      1
    107 0x7670700748cc4dc1102272b700bfad4d0b575edd      1
    108 0x7895e668bd43c9623af538f274c06cb22cbc74f2      1
    109 0x78ade9904cbcac9058ea8f986a10c0c5b53e1530      1
    110 0x79745bf97a3062ee487d217e0fedbd0258495679      1
    111 0x7b2c87a10c5c3fd0d838c0516b06c5c2344c57fe      1
    112 0x7b97a02a3a2922742590eda6e8e927ce71a677fa      1
    113 0x7c5504688eb4f81ed558550066dc218ffee1f6c8      1
    114 0x7c789d13654fc6cb030b3cb23e8ea54e72b17b58      1
    115 0x7e7a58aebff9a27b0405d0b9e2849c6c730aeed8      1
    116 0x7ebe5dc1faef7d3c90761de04be429cd0f36b594      1
    117 0x7fc55376d5a29e0ee86c18c81bb2fc8f9f490e50      1
    118 0x808421753a181e96812796b7ab43d3f356cc5a77      1
    119 0x80d7a0a61bf3c8982b74daf4a51966576b2ed5c6      1
    120 0x81425a9b654d033aee76ab174a35991325d0bda9      1
    121 0x81cc15402a47f18e210aa57fb9370a1b5525047e      1
    122 0x832f4f4f88ab27a00b7683c9b5fc191e797187f3      1
    123 0x840f5d0fa8ac1f000287dfe54c97d4e76b058ef1      1
    124 0x84f4bf35863eb02c9fa55f375bdf1f8984d0694f      1
    125 0x84f8bff48b845e7cc2f5a9175860ef6b0016c1d6      1
    126 0x85acf1dd1827ecd3749e21205a5cdc2d53998b71      1
    127 0x8619237fca77bd1c572841252823e84d34e59aac      1
    128 0x8660f13931a44704327a645b6b7af94c403d18a2      1
    129 0x869ce3c7415f06db4ff6d599d629c1a9c7c8a820      1
    130 0x86a35e57968f0d6591b1886594c6b7721fc96943      1
    131 0x88eb1efd5dd34926b4303dc8bf9818bb7b7fc385      1
    132 0x8979b46eb58325125662ee55adafd07271e87c3d      1
    133 0x89d53e8612e8416bf960dc9444ce0e2a0878a582      1
    134 0x8dd00c7b5e2eaaf895f865cd9ce87f11cc56af6e      1
    135 0x8e58e01f3aa73fcd9be68039361b5342131f0c21      1
    136 0x8e8890d8ea6446c599487173c233ecfa8e7a4ddb      1
    137 0x8ed4d3c67ee1a73f5df8ca3e10698f1cd4c5b14c      1
    138 0x8fc11454322ccc99884ab071d5f1ba6532dc491a      1
    139 0x91b8d9ea1c7c0f9660c72aa373a2a0d2b5ff28ee      1
    140 0x9318f724edfcf60066a924ddb9133a312becf0f7      1
    141 0x94308832672c4e616503dc2010b605ff9057b96a      1
    142 0x94de7e2c73529ebf3206aa3459e699fbcdfcd49b      1
    143 0x96951beae8a3c191800877cdde67dc12a5881584      1
    144 0x96ae24d0d512cbba22bee1045d4b4b6a75a21ec2      1
    145 0x97afa1a3507759dfe38c7d8fa00d3d17b3fcac7e      1
    146 0x97b28492da285daff9153fe89f354e7547e4206e      1
    147 0x987df219564cd274d32b4b442ffad1a185fc0509      1
    148 0x98fe967044e428cc279160435956200b14896071      1
    149 0x9aa74ec75fbec40075a048e254c43aed324c33b6      1
    150 0x9c5fc4242d7ff6bf9d687e7be04c712d21c288cb      1
    151 0x9d7c2a03144d739aa462b6342ad656cc230a6823      1
    152 0x9ea493786d8be968a9652976543999456d0bb5fc      1
    153 0x9f35af4727fb91114563d8a8f779a792cb557f3f      1
    154 0xa175029bff19b26b4a2e6da68e8bb909d6005fec      1
    155 0xa1a7a50abc188d834699bda055b072d4d1eaf239      1
    156 0xa2c6aa19102333335f83fae188d4d988ca948e8d      1
    157 0xa52d0ca88c0692e00634f5e26a57201b60b3e8dd      1
    158 0xa62be8397d8526566aa48f03149a050069075973      1
    159 0xa67f71d39574b6cde8bd5fae38d9e4c7a55e29df      1
    160 0xa7625e30eb5356a0b777e9617b55a92dddff2975      1
    161 0xa84e7cc73ae095bed288a799aa6f870f52fce6b4      1
    162 0xa891fde42e982148b5f2f6b06f04be8e706a190e      1
    163 0xaa34dde115e593dd26d0e6b250321803371dfc95      1
    164 0xabfc4b16844d99c9e0c8658858b930659713eb23      1
    165 0xadf7bb6e352ca256dbc814250c8d037526435792      1
    166 0xae342bc1ddace51f674151633f9fb3f52440e961      1
    167 0xb0627032a9381850e3ba7b749898d061ae74b5eb      1
    168 0xb07cf1b6d7661de58a59a30845b4a39b3906884a      1
    169 0xb3d23c2de27b61f260b2b67cef8dbfcf42b00a12      1
    170 0xb4f98a050c9ba884e6bf2cdf0fe24466486d199c      1
    171 0xb95a3bf65c10a7ee64af73b028c667240e4a9955      1
    172 0xb963fa652274e887e7ab9876f436e054dfb3c3cd      1
    173 0xb981602bae363a3f6d6e7f9a911e8df1c1c60efb      1
    174 0xb98d10d9f6d07ba283bfd21b2dfec050f9ae282a      1
    175 0xb9e5b44437744fdffc13a6f220a61d4cc44af624      1
    176 0xbc2c2d0d64a98a1d66a4adf70cc01fe5bb14aca1      1
    177 0xbd68d765fd1c6607ee24265027463adf8312771d      1
    178 0xbd9a54c0cc23e75029116558d5bfc8e234743b0b      1
    179 0xbfdf3266847b0cc9cf9bdc626bef48ff9c46e9cd      1
    180 0xc046b22e64e59d19831035a6e3a51fbed49bfed8      1
    181 0xc17e7bc7e3dfd514271d3de935f0c82737642037      1
    182 0xc1add24ccd36ccdb949fb0a00d02e67612fb02bc      1
    183 0xc3b4b9d5e0f633bfbf890ef182a10b119f16b7a6      1
    184 0xc5a6d80bc7a2c3093a68e2ad05e8205dc0c0726b      1
    185 0xc77b9a1efda8f3d87022cc8d323225a5a67fa902      1
    186 0xc79d31c1f25dbbe3b79287ecd273b505d9b1b3d1      1
    187 0xc8417e51eae510eb7595dbf6016318c771ddf0e7      1
    188 0xc84d319bc74036c09cfb9dd5179c499ec6416aae      1
    189 0xc87e5b034d29da3976de8bf7528830aba0d6d82a      1
    190 0xca9c927f65a16202e2288c26841ccf104ce353a7      1
    191 0xceb6f6e28b178ac1a4a1341c5a294e8eb6f6eff4      1
    192 0xcf3a375a6b3ead8423bfa813c4127d1e027fe045      1
    193 0xd077b1cb1e0216609adfd9c4439c6d6b5787c8c2      1
    194 0xd12e60a79f53e056bfd3d53cf621d7555cd92c92      1
    195 0xd1a861a8e288421bbb36ba6ac07590f88ec0452e      1
    196 0xd23505ed1641f2f9e44039a8efca1dca9a5329d1      1
    197 0xd2a3f9c6fbe4c13d979898e603d64561264a6b35      1
    198 0xd387a6e4e84a6c86bd90c158c6028a58cc8ac459      1
    199 0xd64110f209e3f77580b72a96ba9be29517e41221      1
    200 0xd81ce8e89dd987c8ab630858c8f1e9df14788c35      1
    201 0xd89a6b9dcccc55192dab28fa74b1d9ce3e331d5a      1
    202 0xd8d97db2a8caec33d58420aaf637cb41058cb58d      1
    203 0xd9f22fadb01987622dd6137c5f3beada5f480458      1
    204 0xda47a6b9ec624f9d8cfe1db6423a26cbebc1246d      1
    205 0xdb2b36c24064a0115fa0474fde4d9ba1f1099122      1
    206 0xdcae87821fa6caea05dbc2811126f4bc7ff73bd1      1
    207 0xdce2a14e55a3805ccd5d7cc80ca897ce5c0fa3a6      1
    208 0xdd1e5fad1b867499bfb1987bb783a17ced9a1171      1
    209 0xdd33abc5cf2dbe88da6f6dad71237ef7f7c6385b      1
    210 0xdd4370b1813a26663a4231840d4995ee54946423      1
    211 0xdfe9def041c0a05d3de86d24adaa8f9a31cd0118      1
    212 0xe089451137197d9922d02c650bbb9a197cab06c0      1
    213 0xe0945340f78dd8981f1f44968cc431f3f52495bc      1
    214 0xe0b93aaa75212ef2839738e6fc139c36bdfef33e      1
    215 0xe1dc03950c2a646400fdedef7b74e1bfbcb5cfc1      1
    216 0xe25acc4ee8311f06bfb688f7363ac6def7650d28      1
    217 0xe288a00df4b697606078876788e4d64633cd2e01      1
    218 0xe46105bbda606babb797b343b21f930668e83c1d      1
    219 0xe7d0ea2014336c5c0b4dc01a54b2f27616331de6      1
    220 0xe8c9e155b48f4ed8f79d080fc5d3e2326e671b40      1
    221 0xe92ee21cb27e0d439f0a1a375b4edc4a71f85efc      1
    222 0xeb0e194d60f307391bbeffdc6d4bec8f56aac045      1
    223 0xec693e2f81eb02bc9a9a6b2c7edaaa3734b4ffdc      1
    224 0xed4dc1d10830480d49459f030e396c031536d14a      1
    225 0xedba5d56d0147aee8a227d284bcaac03b4a87ed4      1
    226 0xedd176ab2506193899a974829a05f2b7572aa9fd      1
    227 0xedda20e0cc11fae46747868fcea07e60c9b25379      1
    228 0xef0159e704d06c888a140a50e06b3eab8375b538      1
    229 0xef12add353ffc63c5e9489a6943e28f89cb4b378      1
    230 0xf1637adedeb89559c70481c3cb4f74ebac80d829      1
    231 0xf207e69a7cc3b4a00fec3039891a77b3df8c3577      1
    232 0xf24910880c3b65dd40a30969443557b62478e535      1
    233 0xf604e0e9461772014f0a250efa4b7d83573cd6d3      1
    234 0xf70ebfd828b993e2863670530c2ec62c049f37ad      1
    235 0xf770e42fd25d745e9f25ab57cca3e96f7fe62d14      1
    236 0xf9de0d8cd1356d65d8045167387505e4456612fc      1
    237 0xfab9a3d37999e12252b47468d2ffd4be15936012      1
    238 0xfb40034905b34753b7d74223e496518d43548336      1
    239 0xfb7c8a716d8c9577ea293a28ff592bb967ea84c2      1
    240 0xfe97f3ba5da378cd83f94b02de8358045491d48f      1
    241 0xff13f9b23a16a82c3ccdd29ece6af667eeb15834      1
    242 0xff2450085510b5eb86c7f9451d5fbc0ca5a793aa      1
    243 0xff839b277ac4a40e458caff575dc24e129298cd1      1

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
