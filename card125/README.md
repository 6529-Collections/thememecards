
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance        contract        
     Length:2172        Length:2172        Min.   :1.000   Length:2172       
     Class :character   Class :character   1st Qu.:1.000   Class :character  
     Mode  :character   Mode  :character   Median :1.000   Mode  :character  
                                           Mean   :1.023                     
                                           3rd Qu.:1.000                     
                                           Max.   :5.000                     
         name          
     Length:2172       
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 17804369 # https://etherscan.io/block/17804369
block_hash <- "0x6d8919a904bb77d5afe3be45016f84de7165d40d2aa35fe1dfb18bb93ed47780"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4758 

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

allow_artist_phase1      <- pick(snapshot, contracts=c("SuperRare","RedPlanet","PastandPresence","Foundation","Gooberverse","TheOldApartmentEditions","TheOldApartmentPt2Editions","RyanDAndersonEditions","RedPlanetEditions"),address_remove=address_remove,address_max=1)
allow_gradient_phase1    <- pick(snapshot, contracts=c("gradient"), address_max=1)
allow_memesRandom_phase1 <- pick(snapshot, contracts=c("memes"), address_remove=address_remove,address_pick=300,address_max=1)
allow_raw             <- pick(snapshot, contracts=c("raw"), address_remove=address_remove,address_max=1)
allow_singles         <- pick(snapshot, contracts=c("65291/1s"), address_remove=address_remove,address_max=1)


allow_artist_phase2    <- pick(snapshot, contracts=c("GooberEditions","HyperDrive","HydroPlane"), address_remove=address_remove,address_subtract=allow_artist_phase1,address_max=1)
```

## Allow Artist Phase 1

``` r
c(allow_artist_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_artist_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 98 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x011a6f601ac7307a8e091e7d62b14b00802af217      1
     2 0x011f919a4eb08408de83f4ba74de8e89391f2484      1
     3 0x04c95c6f8ea7ff7c3f7a2f148fae75650c5a875e      1
     4 0x061600d3515b387d562504eed5a1a485f9ae0ee4      1
     5 0x0e66ebd413be5b4f82ef045f917da50a1324d223      1
     6 0x15ab7dd2261026db55623d0da3946a3e022be19f      1
     7 0x162b2a0d3a1b006c89f2e5c7dde58d5de06b3854      1
     8 0x17bb4076ab59e0ef20ad5a873ab4b5341bf01b78      1
     9 0x1a9aee029a1b8c69c4404890c85715cc726746c7      1
    10 0x1d3dd4cc14c9bc93bc8744559be4ace760f7a1d3      1
    11 0x1f623ee324ca178941545f01bfccfea38cda2072      1
    12 0x205a46012bc3c1a2583fdd426679dff276fb257e      1
    13 0x252ba78469235c352f58f28f7c98db62dd385146      1
    14 0x262e31030a8d35b59c53081eafba8b1750e39925      1
    15 0x2a623eee72ac545b6e7f9159f86a86ce7110f587      1
    16 0x2e0d63ffcb08ea20ff3acdbb72dfec97343885d2      1
    17 0x2eeb236c606b51d259643ec547a7f41392a7f071      1
    18 0x30abd7a7e7213f7d6d7f6de320e90ae3d2098317      1
    19 0x31f5afe73e35de36ec0b99ab683883af4dfc8dce      1
    20 0x330bce303d27df0eb1b856da3464278db1db1ac5      1
    21 0x3480c08ffcb3751721355887f689629314f36f3b      1
    22 0x35cbedc3412d3c93dec4afa60ddf38c2a9c38865      1
    23 0x3612b2e93b49f6c797066ca8c38b7f522b32c7cb      1
    24 0x3ac2cc26fbb230e704393dc79e97cca47e709247      1
    25 0x3ace4279442c0aa7aa692e58d6e7d1f72f99f5d2      1
    26 0x40d775827365ae4d54cbc08a1a1c4f586b2c1d0a      1
    27 0x472c2cfa4d5e6e2f4c1ee62c6b4f1a977e3dc943      1
    28 0x4cbc3966088d27859423c42cf2c00ff40ac0261d      1
    29 0x4de5ae339b406a8cd5037e9138a62e86aa5352ef      1
    30 0x4fd6704b683410e89184a1bbca594a854629c3c5      1
    31 0x523a06ca89600691701f6afa4dc4e8db0d60ed41      1
    32 0x59f3907ea8599428632dbf89c825ab89217a3528      1
    33 0x5aa71418ac6238c78ea9a032a768dbbbf86bfeca      1
    34 0x5dfa3092be17f441f85a4b3218a7675f0efcf9a3      1
    35 0x632463e42b3bd9be625f6008934fc2770fcde2c3      1
    36 0x645fd16f3508391e8385fc0a181076994079aa1b      1
    37 0x668f1e8ad3d43c50d0d95bf621b36dad4412f202      1
    38 0x66ebda986caee331f1e75a834d793bd04d996df9      1
    39 0x69499eeef4599604df9b7014a319964b0cd15777      1
    40 0x69d52023178740a87b7fa05c9b8921791a4dc0ca      1
    41 0x6a4bf9949dc169e79762cc116e309d08a6ea4a5e      1
    42 0x6eff21c2bfdf83a33aa690fe3db17fc672244907      1
    43 0x704c978d50590b21e4c2f43eb4da24fe61fcd707      1
    44 0x71e1f6aaadbb10a4681558bdeef10a2478bb567e      1
    45 0x722e9468d7f8c2710b6dbfdf70f01b045aca1851      1
    46 0x762da606029d3120735aa1eec15464e265db7a3c      1
    47 0x76f29c135b3062d53d5ed70f2e21d5978f50bdc1      1
    48 0x788dd831546ea3b490bca5060ba3bd78a46ad9f4      1
    49 0x78d9f7747bfb3b06e10e2fa7a99c0209955eda1b      1
    50 0x79c087364b5277ae49c1e7c35d79c449a90cc8b6      1
    51 0x7bb84017b5bf09e2a821f59b3d5e1afc6879e1af      1
    52 0x7ed9cf5dbf2f68bd33ea2c95ef2b4634976ba7f8      1
    53 0x7fdab8c244e0b775edeeb9ebb8356b59068c6873      1
    54 0x80ec8c4e035a9a200155a3f8d3e5fd29b2d8ca42      1
    55 0x812010bbe6652497d33c7a1fd986f629769ea9cc      1
    56 0x819dfc1b757c0ed67fea97e33e70cc4cc640f99d      1
    57 0x83507812f07800e572b859788476a95be9e8c900      1
    58 0x8447e981341dd44f7a6caf26e6452d3fb12d8592      1
    59 0x84f190d26ce9b2dfdc4b851cdeca634f0ef59e63      1
    60 0x85a30ede5e46029457e20d55427ee8673fad7762      1
    61 0x928881096f57781e91c35c8c7090cb0aeed2213b      1
    62 0x99e73a69536f7365fd4f2f2e5391f5daa484b99b      1
    63 0x9fca5cb296333aa780d3367951f766ab3d7d4098      1
    64 0xa106083a314d193415dd1e8f16f1e7aa707e6837      1
    65 0xac4109a00af7d892be8512e82a9c82cae63de88b      1
    66 0xad2bcb8dfaa66d67bd883f8c870e374027b42aab      1
    67 0xae0e56f5bbca0700ab74f3649b63fa3a4dccfec1      1
    68 0xaeb3fea566c8bcb7ae6826a03f818d9d386169ba      1
    69 0xaf9e18cd797548220317fb459095c6afac5c33a7      1
    70 0xb09eaa65b2b32959c57bfe5b964fe0a18ea8fd82      1
    71 0xb0bdd53b627d7e61cfc5c13ef110e47e210fac6f      1
    72 0xb893ae8a1824604f6df4dfde52e2754921ba1a73      1
    73 0xba0671aa3176ec9016e4af102dffe20a35385dbc      1
    74 0xbad2e817af781b6f1573d65409ddef24d9656f8b      1
    75 0xbaea3cf94abd0d6e0f029ef5b0e54e9424a72985      1
    76 0xbf8090eab036022b7e0c77d9e34793f1b756d94b      1
    77 0xc61f14dd2fedbba6414ed0f2e3036d50f7919379      1
    78 0xc6f6c89f7f297b5230d2fd028ac06f0677c1857b      1
    79 0xc709c2e280f246123d0bb72394bc79f10fbdab4c      1
    80 0xcf04c33bfea64cf5dd6ea97e2d5620f825b03b49      1
    81 0xd383a3b02bf956887211a8d96904da35f47c6bbb      1
    82 0xd3c18fbcc63e5701fa80493f07d3c0079e4ab592      1
    83 0xd3c69ec93a9841796a2f41b342aa1d1f2a4f508c      1
    84 0xd9682f77ee6addf6739d483856142a6e59172e80      1
    85 0xdafcf1931a891fbb29a29c47ddee5ee76e24dc2f      1
    86 0xdcefc9ff1e47e458e56959154c1cdcf56003d30b      1
    87 0xdf067d1c62b6a94e45ced3f01ebc94ae304cb045      1
    88 0xdf36b093cbc06522b68105c3d9141db188e980b6      1
    89 0xdfd3b58cfc180168faebb6366b98aff25fe1f8f1      1
    90 0xe1d29d0a39962a9a8d2a297ebe82e166f8b8ec18      1
    91 0xe74419fda5425b88fe288923f5df60a5cda057be      1
    92 0xe85041b675b616f635798a63db467289e5aa1e4d      1
    93 0xe96ba1a10f72b58934b9ac0e3fb471d2ba65b757      1
    94 0xed8982578836f7d498856b3f543f128bb8621f2a      1
    95 0xf2439241881964006369c0e2377d45f3740f48a0      1
    96 0xf2fc60b88137a9f22bbb5ed0b9310401ade39479      1
    97 0xf3290df945e484a4ff846fb5baeb6d1147a7aa69      1
    98 0xf58592f8e00ccd9e83ed740d63a85718e0bd5f79      1

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
      4 0x019d370ada720786cda2c4f78f0f4657af2ff92e      1
      5 0x02009370ff755704e9acbd96042c1ab832d6067e      1
      6 0x036aeaa55ba6cf87984a5f0a467b21c63b9bf9d1      1
      7 0x03ed8e6d50bff0b8c49675f0bba94087d5e579ac      1
      8 0x03ee832367e29a5cd001f65093283eabb5382b62      1
      9 0x0573383fcc26eb066010d28d96ab02f7c5cde67b      1
     10 0x05b99f8f59d67b5e7b1dea82b1370cab65ed91b9      1
     11 0x077bfc14dd6725f260e1abfd5c942ee13a27091b      1
     12 0x078cc4caa131eb9d6d3e31847946d5f37fd3f58b      1
     13 0x08e0a3e752d1338c0f02b986b9504f1009a962ca      1
     14 0x0a49fbe88cfc413181dbe93c9e3b6184b2264071      1
     15 0x0b3e3a27d85996e20b2cdd0e7f1e1208816d05b8      1
     16 0x0c76717074a973f3f6e3d422166c999483f6c7fd      1
     17 0x0cbe1fba05102c34365a742af159efc5a93d1a68      1
     18 0x0ea9f4f7c371e7044d187b3da7850ccaa6370a0c      1
     19 0x0f0ef9408ca5b0dc364ff5921bf544c811e9c94e      1
     20 0x0f3c76ade30adf61b64d01154911308491784dfe      1
     21 0x0f5a2df7fb9da8c8c7371cfb523ffa527518200e      1
     22 0x0f5d5751d4f300c3d0adbcadc05f0af4f8baf788      1
     23 0x0f78afe5b1e689cc2b205a78531957a286c42511      1
     24 0x107d1198854fbbcb2e1014c3ffab166b2595586a      1
     25 0x12a0fc764430a24833fde0310fce8071e1b5da08      1
     26 0x12c4908dc21e88a8f268c3f363098eed251c1aa3      1
     27 0x13c39b3e6e6159bc233fad90825f1f9f84fcb102      1
     28 0x15a33852846b4762d71a62928aa7c64d8aac8544      1
     29 0x15d7972819b3906fc430b1e7bc26c39e4b9e023a      1
     30 0x18412b9f84df461bdc0f2e0709df507d5bdb6363      1
     31 0x185d5f60fb7c59d344ba939361908916cbffe1dd      1
     32 0x197fe3ee16556577180f6578050802106e8bc446      1
     33 0x1da0f2a58e65d98a8d04ba3da56cff9f5ace5615      1
     34 0x1e7ba17cc3f031a0c5795739317f6b2022ca39f5      1
     35 0x1fad9f65b39d235469bcb59bc664872b93eecac5      1
     36 0x206b0526f4866e7580780015cbacbb4a8dbb8c80      1
     37 0x20aa168e6c793646f60737399c8466dd643d4281      1
     38 0x215fc4d12967ddc2432a7b8936f2849740edb7bd      1
     39 0x2380ca49ed8e933c97905977763693e5cf8770f4      1
     40 0x240d478106bbfb613b923be58c9269c7d90a41b1      1
     41 0x24fbadccd6684106e24065694ac87b0e98819235      1
     42 0x27e037e0461e3d5587de786ea23441d83772353d      1
     43 0x289256fa6de33947fd292a9e94a738c3d986f8e5      1
     44 0x2afc032aaf6d7ead33fb05f9d42d874874dfb9d1      1
     45 0x2b919bffea16d55828091fdb8a63f0678e17b26e      1
     46 0x2bb2ee28a5bba51db2c104b6c36a6907c21d4d4b      1
     47 0x2be9f18c46df11f05b581131af4d339c20c7254e      1
     48 0x2c52248bf9f5715570ad007ef4d9c660ed8ae2e7      1
     49 0x2f1390ec03a03e57a1406bdef57c9cf487f62b78      1
     50 0x301be4f5965cc6d386a3aad84f1344ecf1042c3e      1
     51 0x307fa253ab864cbf57483415909b37c36df3b8c8      1
     52 0x317c32ca0309bb8a1f9d8a3e2836544f233c841e      1
     53 0x32606c4aa95ea78d7c5b1e0e347e3e9ae451434e      1
     54 0x32dd9f6885332ccf8633a18cb0c2e25e68fe03d1      1
     55 0x343193017fd217d19fd983e31db701385c8504f8      1
     56 0x35cbedc3412d3c93dec4afa60ddf38c2a9c38865      1
     57 0x3839a6c85a38de97abd0ecc092ef3311a7f1311e      1
     58 0x3877b4c39476e3af04cb1ff8003db09f39f068f9      1
     59 0x3927f1b51afd205311bdddeb5646c4c6a309b2b2      1
     60 0x39cc9c86e67baf2129b80fe3414c397492ea8026      1
     61 0x3a10fd1387555cd75db8142393fbaba98efe28d4      1
     62 0x3ac61447a75149ba67639100ecc92c76fb20941c      1
     63 0x3b748a60dfa1d58eac080a5ef24b11a082edb6d2      1
     64 0x3bd5e344ac629c9f232f921bafdeeec312deac9b      1
     65 0x3cd378c9b1cb5f147ebf1b2c2564118946ae4ba1      1
     66 0x3cd76a7a90e0f6a7dfe6791a54ec459acaebc5ca      1
     67 0x3e998c50b1eaf1577416e2bf31a76d09a48bc3fb      1
     68 0x419beee486a63971332cee7170c2f675d92ac5d3      1
     69 0x41e9e61a699a85daf12dadebbb7e6e517bee6b54      1
     70 0x4322aa46ffed67809862da613725728e2fb8eae3      1
     71 0x4525b5402a6d9eefc656e828b9391aa9dff2446c      1
     72 0x455867c5b24bf1a29ee02e0195b9ff36bea17ca8      1
     73 0x4581c619ae0556b774f71adab6831a86da1aef17      1
     74 0x462fcc4b3fc64e0dfd751424bc3b070d79efb300      1
     75 0x46abfa031be839b1599513887a27a403e8d6598d      1
     76 0x46e6aa05e0867d5f0feb749e81e005f5567ab317      1
     77 0x4705bc2775cee0fe266dc2a376010c0eb1bcb474      1
     78 0x487869b7557f0fdecbc5141da1d7c48c15c5b8eb      1
     79 0x48be4681972473b498e8b686d38e04826c26fc4f      1
     80 0x49d6531aea6dced6595d50da685274766e4cfc8b      1
     81 0x4a15e3d354043fdefbb8fa77a55da211f4058190      1
     82 0x4ab5189a020ccd88f05c36de69e4d868d29c75a9      1
     83 0x4b25d8ba0910f41ceb3fe2ba48d3082ed12cd8e3      1
     84 0x4b2c1ce6a01981dc3ee79825bdc3f3cd7932bf11      1
     85 0x4b4d75b8b0fc7528ea2614b2ca83555824c07867      1
     86 0x4c26c796abcf3513807efa54d54c34405b84a36a      1
     87 0x4e3cc03eb2be6122f2316b84d1c7edb900e90dba      1
     88 0x4f13eabaf32ef4ad3837ae88fcfc6c69edba0377      1
     89 0x50002a9b8e9938d509de84dc3eb3aabfbec1451e      1
     90 0x5022cf478c7124d508ab4a304946cb4fa3d9c39e      1
     91 0x50d086175cc14fa5ae008591d72a93cf22b48e32      1
     92 0x514afe6575b5d81cecaa86a6bddf28de2f89ba45      1
     93 0x541db1ed2628f2f4897417d06181af6a179e90d0      1
     94 0x5458c15ff220bf75b92a76229fae88f93df9c6c6      1
     95 0x56bdc5fe7f9752e7f5a381e394acfe72a724462b      1
     96 0x574a8af005c82eaf05e92c9ccc01048e1eb1ae41      1
     97 0x575f6540c16a72696c14a17fa64f049992d661ab      1
     98 0x57bd982d577660ab22d0a65d2c0a32e482112348      1
     99 0x57dec6115e561f3de9fb7429cf14de429713b3d6      1
    100 0x58f606e6a973e57165bb07057a75f047f42455a7      1
    101 0x593cadf136c68c720d446461c1bfee600647c6b8      1
    102 0x599bf597fda4fc5e02a697906fb2b0b7ed9e964b      1
    103 0x59a5493513ba2378ed57ae5ecfb8a027e9d80365      1
    104 0x5a3a1461310884df894b7e973305f690fc5779d0      1
    105 0x5a5936a4552382a900a3669665f11472f6a38a57      1
    106 0x5b1022d859a46f42fcf24bcd8f5ab616e52b4800      1
    107 0x5b8ed97b1e85ea4e5e96c3a6e00b0835b087fce5      1
    108 0x5c076659e81dd48a5d73c81168fe739c2ab89087      1
    109 0x5c4fe960950ba0e09a72869c3d51fe70f07580e0      1
    110 0x5c9e2a6fec34b510996a8e2a3d1e2c47a382a8b9      1
    111 0x5d20e6c0db887b5c3281edfce5bb7677565e2b8f      1
    112 0x5d25087405105bab12624c73488ec186066a6376      1
    113 0x5d5fe1910a289b24455af49ee3e41405f94b921b      1
    114 0x5dafb2d1f8b20501da62ba00c0c283293aa0b70c      1
    115 0x5df6dd99718f32ae362b41e78b7bbf66ebccc179      1
    116 0x60bc11840966ae3946ad1904a8efb6622226be25      1
    117 0x60e5299f49cdbc13d152323105af462071b22c87      1
    118 0x61e814fe997f0b2816fb9ac3c7df3aaa38d8ebb6      1
    119 0x631cd42eb1063d1fe2b49a78744f3094702edbab      1
    120 0x638fba3bbbe8828394d2b13f92fba75bf32c8efb      1
    121 0x66567f4ec7743e58713f44f083da3de78a52556a      1
    122 0x6767b1e546dcb7499652a1fc4bd6f1e36992623b      1
    123 0x67a90218cc5fc1adde45ad27a257f268023b2ecb      1
    124 0x68d45e76a87e9f1706e0d36b7815c8dbdb7e6aea      1
    125 0x6a278927f03422419901b77be4d47700b1f3599c      1
    126 0x6ae613a84a155fd80c8e6f14cb3a1d8958f51b2c      1
    127 0x6b4b2cbb69b9ca07328a40e7ffff3870f627c8fc      1
    128 0x6b4fedc18a1e8cc0523e56cc9b45080e1ea00005      1
    129 0x6b9bcb0b854d7c7430baa55167d367d28a8dbf1b      1
    130 0x6bae54650c0d17f4aee76839a7d319dc5060d6b0      1
    131 0x6dd97134b53f40d239842716b2cf0179a11876e9      1
    132 0x6df000635d86613626c5208d7c9d71b84e091220      1
    133 0x6e1510ed41ba743344e9ba80646194351cca0c7f      1
    134 0x6edd8fa5550e29d47641f969d2ead3decaa29ba2      1
    135 0x71e22168b702bcff528b8974cd4b723250b67609      1
    136 0x722e2e4c15be1fdded3c86f4100bc32b181827f5      1
    137 0x72f315c115408afd9240c5be0946c8ebf7261fb1      1
    138 0x730a2f3421b2967ce8e9ae1f839839944626812c      1
    139 0x73bcb8c5e30bf85806aade7fc36f16c6b80fd3b9      1
    140 0x74bfa6fc87e23d64451f5bb084b6fbf79f46e495      1
    141 0x753c892626c4fa09d1aedc0e9b52bda97a4afa00      1
    142 0x75d4fcaffb56055bf9df3574d345473e31a16076      1
    143 0x78089705ed807984cf3edf9149c9369e8fabb9be      1
    144 0x78143238df750e9d9f1082e12ed56f2bfa332d65      1
    145 0x782ff3f609427680db175365c24c238f89fdd276      1
    146 0x7921dcfa9f14dc980d7e8f946b7bd223624c3db9      1
    147 0x79e561168835c783240a0637320d308897bd0922      1
    148 0x7b86a910a6c797316dda30b78098e331e08b9f34      1
    149 0x7bc7481751d7fcb76d08f596b03ba1e42378ea27      1
    150 0x7caa9f43822e288782e3e8797c8a16774c689b3d      1
    151 0x7cdbb8e309ce7c701ed200be123f16c6f7c9e525      1
    152 0x7cef7db4289005fd56dfa6c73bb2be6ec13bc250      1
    153 0x7dbba7f0551aef5894fd6ee331b95dbb4349f5d4      1
    154 0x7dcb39fe010a205f16ee3249f04b24d74c4f44f1      1
    155 0x7decf7a31168778f311c57b9a948abaa7321001e      1
    156 0x7f94e30381aa6657c45833ec7fce2e493c1888ef      1
    157 0x7f9bea812b9b6c3c4b74ec8aae849a5745cc3ffa      1
    158 0x81590c16b9d1495d133cf881acf5cc0b56730b74      1
    159 0x832834fc5b0b35ffd4cf697c4ec08815ad8d6a52      1
    160 0x834a9484537c2760ef6d6e6736d1ea800d67966c      1
    161 0x853c69418605529a68907aaf7789270e3cf69d97      1
    162 0x85603a042455d2f36f3ec52c1c756344aa70c5ef      1
    163 0x85914d049aa004d037cae65bad275ed4147f172e      1
    164 0x860f0aa48ec759df1034d72e0311482a8b01db83      1
    165 0x873dfcbbc61ef7ebadb652adb98df9f69399b70b      1
    166 0x879a8a1868d620584afa0c9c8807628d553c6bbe      1
    167 0x88f038389cbe95a917042bdb0f3afc434c680edc      1
    168 0x89fad20f29de44b28c66abcd47113227e8308479      1
    169 0x8c0a11eb047c1097c821c159b8e0c2c5f37f81bf      1
    170 0x8c2dabfea8751dd7901e35a015dc6c943ffa0d7c      1
    171 0x8c4138dcead987bb50929ecfbeb39ef3e28b4c76      1
    172 0x8da03feb7174b4b2a6b323f3dc40d97f3421cd07      1
    173 0x8f3a658a9736530e89c242ef572d7198cfd540ea      1
    174 0x8f4b933491e1319799229ed9d36b179bb859d705      1
    175 0x9197f339cca98b2bc14e98235ec1a59cb2090d77      1
    176 0x924baf1e2bbc58e9cfe333d4c7074886a6af1afe      1
    177 0x92c75284b0d863165a8b33e212f02ffeecb2853e      1
    178 0x93549cf8047b3e305fb10df7110da213e9429f9c      1
    179 0x93c838add3ac72b42f65417408f900a7155e1bb8      1
    180 0x94345607093b249b5100e8d1e499928dc163dfdc      1
    181 0x95d41776812c6ca222dd5fdea56c065b55ff7655      1
    182 0x9617562c6f338ed18e2e8e88cbbffce6003bae0e      1
    183 0x982d3c5223f6b5794fccb3208eb164d042cf2526      1
    184 0x9ea50708e47a237182a56c481a4d11dd021b7ec2      1
    185 0x9f281c5b04c091096ac468a9388f0ee6b0b8b1f5      1
    186 0xa1ff0c6d0e5995ff859912421b5df89104a32f5b      1
    187 0xa21e8556a8d69a8168818f16d9495f0c0efdd987      1
    188 0xa2917120c698fb5f2a03e3fd3524bda85a3eaef6      1
    189 0xa75dc095ed4ad69b088c3eb8ba2f93f1aa942b6f      1
    190 0xa8174abc36b3298356d273e512cd87223a2b3ffa      1
    191 0xa8450d71b98ca4955c63519ef593ba886c5b2b4f      1
    192 0xaaae5c0a8e05ee5b3824b2e9fe939d5dc3ba3336      1
    193 0xaae8c50d76c76fb8947c9a203103d28b55862977      1
    194 0xab0e2f3c3c29deaa7c3ca0dacec97a33d5c26708      1
    195 0xac01ad9a3e3c3448a5a0e6fc798def034a01a67c      1
    196 0xac104d56e4f8e0462f8c001ec7dde4c68deb596f      1
    197 0xad5bd06f5fb4633888ac7506ec0c87eb99998653      1
    198 0xae20574304b6264d3129dbfe5df72eead8c5b208      1
    199 0xaf12a9af19331fa9b89915537ce9d41bce5e4c5d      1
    200 0xafef3bb7fc3cd8ae083672b65f79e6cefd4ac619      1
    201 0xb01b3a335db49192a31af43a88c458e5070ca5e1      1
    202 0xb258e243a526e79c4b5c8dd5df490e42eb7927b3      1
    203 0xb2bc498a214282efa54877ecd082165d4cf86df4      1
    204 0xb33fb83c645ac2a12b138636fe59604312092484      1
    205 0xb3eb9bc116fcf22c3d6d5f920855d4bf34a9b0ba      1
    206 0xb53ee69b92ad6d12e6f3b1f849ad3e706e31a263      1
    207 0xb6b4a02dca517564eb98790ff67d42b5b37a3d4e      1
    208 0xb6c4ce4eae85066f8fc45093d12444c8b89a6aa9      1
    209 0xb6fc3c8f4e5233f2ee2ea1ab84a4d10c61f6d215      1
    210 0xb70c3049982c09c18dcbf8596ccef6f5b3c239a3      1
    211 0xb774f65d9eab67c9a587eadb5b79d0a02bfa5c42      1
    212 0xb8efa945cf7f35854afb80e9ac05f5c1dc172fb3      1
    213 0xb90aa714bd30e6f135ec15a0cd2605af1590d184      1
    214 0xb976163a6f251c208bcf3196b9c4ddf927b63e72      1
    215 0xb98be1aba8ae2adef0daa18247273fa06021daae      1
    216 0xba264037d93f4e06d5b19e9a73f29fa64689e1ba      1
    217 0xbaa4aa9933b4548cff0be2693e7af7e14e9ee49b      1
    218 0xbb8fafa8a629c4dce022d95e098ccccee1acd942      1
    219 0xbc1eb4359ab755af079f6ef77e3faac465e53eda      1
    220 0xbccc968eeebffc2586a50eef0ee1cf12273c9604      1
    221 0xbdc765d81db7e0ca47fe6bf57e987966a2c204ac      1
    222 0xbecf64aab1c6813a526ce63483cb8cadb2988c07      1
    223 0xbeddf3e43015f5221f5d1ac0fd0d2ab3352d2f7b      1
    224 0xbf949494127d3cd72cd3399d4ab38312757f4d12      1
    225 0xc06a00899ebd1481f243ac4ec92a276c1ff26e8a      1
    226 0xc14e891f453b14df5449a6644bc88dab2a5e5622      1
    227 0xc30e9fda779efe2081dae6529c0d07fc16769c8f      1
    228 0xc322e8ec33e9b0a34c7cd185c616087d9842ad50      1
    229 0xc33164f7ecc76869fafd44363cd094a22e0c296f      1
    230 0xc3c1744bccfe4691e332c873f9cb99b53214e03c      1
    231 0xc4c65d75dd6579671db24990bad1f502d7c96c8c      1
    232 0xc53f977136e1aa9031343fad39dcb4c11a1eb3c6      1
    233 0xc72f40a397453051349f73cf2e2a04fac06e37a3      1
    234 0xc7c6eec4978799e6a53da79b540432820a349d0b      1
    235 0xc89503a4e57619a2ac87a821732ff4afdb2bced2      1
    236 0xca3dff8c740dee29528916eb049cea48f500d387      1
    237 0xcbc7e8a39a0ec84d6b0e8e0dd98655f348ecd44f      1
    238 0xcbfb9a917c2b45fbd8e820e66602ef26dff9cd40      1
    239 0xcd094518f2d30fdd8c7911636bd8b52bd5303066      1
    240 0xce4122fec66c21b0114a8ef6da8bcc44c396cb66      1
    241 0xce56a746edaf84e65aa6a2bbb45b2328f6a99935      1
    242 0xceae52c73db66e280f8d5be3db89dc69463c7d72      1
    243 0xcf7c7758512aaf43979b64812cb79bd3f60f4413      1
    244 0xcfd648eb66b736351f48dbd5a1752708371c10f4      1
    245 0xd1042637cdd71f99f3dc5877030e67f3789e1c19      1
    246 0xd38a87d7b690323ef6883e887614502abcf9b1eb      1
    247 0xd4100a2e1aacdeb18bcda92a280124a7111a12b6      1
    248 0xd4e55db228e3761980be2f705dcd4be9a8777c6d      1
    249 0xd70150a7a5a42d4cd25974dae077a79a1547fcf2      1
    250 0xd7d16a716f6dfcef1e9553bf12136463f6414f1a      1
    251 0xd89c641cf9cca44a3435895d24494d1bb7d70ee9      1
    252 0xd8d97db2a8caec33d58420aaf637cb41058cb58d      1
    253 0xd939fe2e4776ccaddb823a26eacb45c66d8ad5e9      1
    254 0xda512cd98dd54056003f11ac3adaecef9850f8e3      1
    255 0xdafa9e3dae493f0b9d6872eff4fda0f40d1b7488      1
    256 0xdbeca36c48306867d9285e0e0d5ed1f5aec28a35      1
    257 0xdbefee517025559e7898d3a48f18221c32d3fcf5      1
    258 0xdd0a979d9a277144e73e05aab18261f71ee020a7      1
    259 0xe1759f0cd1358c049e7f64c26e13de9492f9f888      1
    260 0xe1d29d0a39962a9a8d2a297ebe82e166f8b8ec18      1
    261 0xe1e57c6d779adea0f34cd3b84fb6bae3a54d1cfd      1
    262 0xe23e0bd6e320c87367b0e4b797b42dc9b4fe7ca0      1
    263 0xe25c73435702fed11e9c5584ce6efe7cbff71739      1
    264 0xe26027e219998c0acfbd00b74795dc850aee244a      1
    265 0xe29bfd5761a18b5c3ac4a4271eb7fadd4c7fb942      1
    266 0xe3013b1d58b257684ed9862dbeef81d863e2e0f3      1
    267 0xe4d3cbd20bd9ab79a70a1612853154cb80b02961      1
    268 0xe50ae5655d227767ec3974a11da7e4f67476b96f      1
    269 0xe560646ef7a69400974d85e818bc0e054bde65c1      1
    270 0xe69cae620953aef05e9276f35a7d3cb598bf4b20      1
    271 0xe6c26d2c2376e8997057fdc9ec0076249a6851ce      1
    272 0xe781fe7a6f65ee6b3efe66ed8f7f5c1e9f01cc55      1
    273 0xe7985c511d1bf0c7668757650b974513628dea7c      1
    274 0xe8cc7c453eca25450ca16d551db93d5f5740e054      1
    275 0xea7d3818a3b736fba40852cd90471df33c6d6821      1
    276 0xeb43b5597e3bde0b0c03ee6731ba7c0247e1581e      1
    277 0xeb8661e8548dc074b9ff411080fdd9a377e6ed1e      1
    278 0xeb9d9836b14bb9eef7cc0f40f87f83f6f959cf52      1
    279 0xebe4c00073f1061ad27859478adcb4f5bc8ca67c      1
    280 0xee58519040de3504932f4c79e1d056ef1b42a026      1
    281 0xee958e45f3464d712b8830deb5875c8ac105f698      1
    282 0xf15a6b54e68884d27e1bebb1624d70c227b7d04b      1
    283 0xf476cd75be8fdd197ae0b466a2ec2ae44da41897      1
    284 0xf4aa005cdd64b2c80e6cbfc30c427f80b5d1f0b4      1
    285 0xf52393e120f918ffba50410b90a29b1f8250c879      1
    286 0xf54913955c6035a12465e24ba2f162df2a026d9a      1
    287 0xf54d81b2955832474a87a8dada4a042a05b83180      1
    288 0xf74b25d3f6616e51b2d8be6c8cec54b045229655      1
    289 0xf81c1d67b9c1f80eb30c610e2a78c2a1b1fb013c      1
    290 0xf868a2de45bc38844b16c9e63fda5e1de1d54a22      1
    291 0xfa3dabf38f872f50c62c3d39920d173d215806ec      1
    292 0xfc0c476530d9742cb116027c04559d0dc26bbd12      1
    293 0xfc382669ecf6a4f19efe1e32c64d75a1daffb1e1      1
    294 0xfc61d7884b047f9f4bf56e984f773f1bd5d51480      1
    295 0xfc7d068f069e712dd35b439bf6ed9df8f58d07de      1
    296 0xfcbca52b23db28470eb97680d18e988d8b60b608      1
    297 0xfdb325752f9793ae7fa4aecc641cf5386846f566      1
    298 0xfe73f0b55ee4b411d7d1e4d5d5d4f8834064e2b5      1
    299 0xff08bbef8f2b6986018fe3bb62850aa7db843b40      1
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

    # A tibble: 782 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x00629538e0dcdc069c4829bac7c9fa4e6b32e330      1
      2 0x00ff192363430a35abbf968c535b64147e88abdb      1
      3 0x0162f0b0deaf7147d5603d83c2c149737a6e80df      1
      4 0x01a9fda1c1f1085b518fecaefe9ffc1622116ce1      1
      5 0x01b202df3aa237846c715f33d15b94a5340695ef      1
      6 0x0235fb40f10b5ea49a9fb7660a8af76787c0d394      1
      7 0x02ac3a098a3f8197a6d4a5e6fcd4114df6b3e490      1
      8 0x037ca848f2d7efe94a2134aa6648a5fae097e19c      1
      9 0x03e0fe58a86ed7b5966780441d8eceac1ffb7575      1
     10 0x040c068b537231f4c9af48f133e089afa53b42e0      1
     11 0x04490fb053a8ff110bea35f22d955c0092aae5f8      1
     12 0x04830005c7dbd3671f2c4072c61d4e82b3c27674      1
     13 0x04d3471badaa22957ed8b46d80c5b985b8928994      1
     14 0x050920eda4014e25ded17c346b425239a468d63d      1
     15 0x0548cd296178434872b29a3c784af7f6157325fe      1
     16 0x05897e0d012d3d4335b9a6bd2981927ef70c27a5      1
     17 0x06186e1bfb2309c3cbc9dfed039a45d7187bd6d1      1
     18 0x065094af6f8e06c0f0530bcf7d589e4d17608dfd      1
     19 0x068518345fcea5e037581d49ebbab6c4c75b7572      1
     20 0x0699978658affeb9add6ea1fb7bb95605b001ca1      1
     21 0x07b3103f8eceee53f233e58d6b4481edab2784a1      1
     22 0x07cc6e767a3c7bcafb2b297ff028ddf4efc58feb      1
     23 0x07f00bf1f5d9cf040472b28f1464bb08100ce3e5      1
     24 0x0851ced43aab7bc38b0fa4fbc4e3849634d2ca67      1
     25 0x089fce8be711cabc806d69b1d0d5aebd52a06455      1
     26 0x08f8070f1a17791dcfda507d2e1a47bd78b6cdc6      1
     27 0x098d3b9d61de50bd85059969d1b302047d0ac73a      1
     28 0x0993c167e70df18f4596405cb213c279a397be14      1
     29 0x0998fc77b101e4c538f57bc7616c5ecd77deab5a      1
     30 0x09b2f0988b6d24336b348c84c2764d2b4f85142f      1
     31 0x0a18b5cf886c737e361668bfb059f1a835dd4561      1
     32 0x0a53b6171cd161eff42a097f0476908c9f4fe0b5      1
     33 0x0a9bc3b0d19bca983cd9a6d251dc5920e87b223e      1
     34 0x0ba175b34c80dba65bbc6cda2828eacb25338ee4      1
     35 0x0bb6c091afc1b7c6405b8696e81c50f8730b6bc7      1
     36 0x0c43d79baeba1acb896b4e35f02615eb25c3e4aa      1
     37 0x0c61d3d099c6a2fc6ad9f0effee4befa8a0d29df      1
     38 0x0cc5428702c220cc4a68dfb6c83c6930f7270e4a      1
     39 0x0ce576c539be4b38b6cfb5c56d2cf115ca90f97d      1
     40 0x0cfab25f87b5b2f22008e8859e8461e267e00b47      1
     41 0x0d66a00446021eeb569b9312c8c1bb8c3fdd0a09      1
     42 0x0d71ec30c323037bdac7bd174d4a95cec5a4ee0d      1
     43 0x0d73aa5de89b309798f9fcbba8b86c4fd5f542fc      1
     44 0x0dd189d8a05d4c7308aa5e08dba80ed57b9a5fbd      1
     45 0x0e1e11d175f40eebf88df3ac5caf4d4143b27d67      1
     46 0x0e8d35bb16acc48144253d703eb19358f739aaae      1
     47 0x0ef0d201c5d2104e88de94a08c2f128b0f261a67      1
     48 0x0f8361ef329b43fa48ac66a7cd8f619c517274f1      1
     49 0x0fb39dfd1b192bbb04e0e95c844329041b48f11d      1
     50 0x1005fbf6ff95df199813d173c9325286052c26cf      1
     51 0x1064aa646a7aedbd40816fc0c35e044d0244a764      1
     52 0x107250d1545be75cf1e6e782fa04beb66efb06ea      1
     53 0x11918c06422551b383ea62dd39e747399f92ee12      1
     54 0x11f46fff2624737aa0d38507e761b6ef770eb94d      1
     55 0x1257b6871bfca1da593ff5586e73eb1731b494fc      1
     56 0x12c3a430099dcf05cc6efd0f219ed49d5b03d1e4      1
     57 0x12efdcbbcdd6695109ada38e33c56173c2ffe2c3      1
     58 0x13257e056112d578414e5d291926c9d684d5d9ef      1
     59 0x13a7d2fb3f73b57ef539c35c4ec59ed49b1833ca      1
     60 0x13c39b3e6e6159bc233fad90825f1f9f84fcb102      1
     61 0x13e381682a7e3540f012f957537cbade75d3617f      1
     62 0x13edf543ea6e777c51b55b09c2a1d63f118f6dee      1
     63 0x13eee41d67b8d99e11174161d72cf8ccd194458c      1
     64 0x14368bfb69824aa8645a36e7ff38584ccfe0fbb6      1
     65 0x14d97d475502a0789dd5673cd5f691763ab55a3d      1
     66 0x153509e78fd04144d482e27716ae14c52d7932b0      1
     67 0x155bf49e1a07928dd6603b2314737c5fb95157a2      1
     68 0x1682f9a500e93e9b85b673a020de2ddbd6bfaa01      1
     69 0x169017ec3dc471b4f6161b568cf614f965fccd35      1
     70 0x16b0e4ad0b7287341e8436f335b558d168dd0985      1
     71 0x171446b041c6683e5f138b8a3f263bbffb8ee74a      1
     72 0x1776cfdcffc21cd51b35d1efaf5b3db4848da1d7      1
     73 0x179916c07eb0f064640f4fbf2acb55e0543feccb      1
     74 0x17de8591163a8a9b6752efb9d05cb7290e887a6c      1
     75 0x183abe67478eb7e87c96ca28e2f63dec53f22e3a      1
     76 0x18431316706d2149089d0d7fcf9a119332c5ed70      1
     77 0x1849a094d39bf4ff2305cfd86ef2b38fa032f877      1
     78 0x1876a2f935a86e6371e6e56e3fb3abcbb50125da      1
     79 0x18ce6fdfb37b288659ae9d7e3da592ce280bf91f      1
     80 0x19317be29d9fb86af1b72594e6e09ec0208fbcf4      1
     81 0x1935136a3a3902d546972bc6650fd5e59a363f02      1
     82 0x1996da1e0c54e113370729d1c22f84376f5830e9      1
     83 0x1a962d6a95cea8060b1e9f55c5bc6622315c0108      1
     84 0x1aae1bc09e1785c8cb00650db0c6015bd73b0c7e      1
     85 0x1b551cb673dd5757501b85ef2c7008064fc655ff      1
     86 0x1bb7b4f7d98a9d204186b36d3852e28f3286f577      1
     87 0x1cb9998ab32e6d172f301a10ed8e0272d96509b4      1
     88 0x1d4b8462dd9f7faa60944ef40b59e1b1e2cf34a1      1
     89 0x1d4b9b250b1bd41daa35d94bf9204ec1b0494ee3      1
     90 0x1dd01b89666b63e246e75ea9dd2659daa9d1092c      1
     91 0x1df04aa034af5f96dcd447301d425fb0c1ff1d11      1
     92 0x1df6e2636e29f50e29f4fb1ff7a9b1899108b0c5      1
     93 0x1df8acdee8ec275abe1c25bece73b8b310a44c62      1
     94 0x1e364eb88cbbabec80bcd7688f7059274f54a39a      1
     95 0x1e75f20ddea8db8c26a936eea3dd7e9e24dce978      1
     96 0x1e88bded5d09d8e8129066061ccd2e77b1364249      1
     97 0x1f08411e1493ef7c4e0c39b0dcb38495dba0fd33      1
     98 0x1f92fc15bcbeb4dd24eefbead8d7aee409f891dc      1
     99 0x1fc4b87ce7c31507ec1d0ccae20e674b13840a6c      1
    100 0x1fc550e98ad3021e32c47a84019f77a0792c60b7      1
    101 0x200f0e4a6844032e37ffccac645a76543c6c9bd2      1
    102 0x2177da04f9479496c2292d6344d306aa49beb34a      1
    103 0x22b1f5fd9fd3ad4cb7e40a32897c25e484714bc9      1
    104 0x22f057b5189d796e9b56159774a701b563280b2c      1
    105 0x2334d91317950c19a6c21c69d69f5ab8a8f275fa      1
    106 0x24673b7ae3cd2f69a2e49b6d0a313f6a1007ecd5      1
    107 0x24af52e99df6446831be2ab94e6db7add6a3b2f5      1
    108 0x24ea141a8b400c5f7e8fa11c6e031464bd637233      1
    109 0x255a03390e9248844203643cfe8aa888c4865af4      1
    110 0x258d10438cae128f43802209040fa12e1a46eab8      1
    111 0x25d50ce5c3c2962b1eb22963070432c8ace816b0      1
    112 0x264ca3862bdd88505839033bf684e3a217432d4b      1
    113 0x26dca2efd74dd7153e24835f2a30845acd722899      1
    114 0x271fcc43580c89d879e9b5373f1854b8f87a974e      1
    115 0x27d4cfc6dc3a0ff6a931d19f6d8e743c03bdf88d      1
    116 0x285304c4cc2a512407cda997a805e551a5d5ad89      1
    117 0x28a49b3772776a7e144901c800c86ef4e630e91f      1
    118 0x29165e46a5ca80de8ae9f9f8fdc561a0eb13e46f      1
    119 0x298523faa843ca40e83b7934170ca874a4fa1b04      1
    120 0x299fee10cd3f03ebb2f9eb5d59bfeb6c502dfeeb      1
    121 0x29bd2d1bc9382ab20ee799b6a8beaf9de1a8e929      1
    122 0x29d3c95d4d8a42ad598112a1c6f909e010ad37dc      1
    123 0x2B65EE5b1d99EC9Cb2a8217bAC34F0bdd1a19C4c      1
    124 0x2abbdcae6dcb79539eec185ec0110b7f33b8c00c      1
    125 0x2b3a9eb771b3838553fc2f79a127bb7bc568693c      1
    126 0x2b538e17628e68703b9527d4a3951f093a11cc76      1
    127 0x2b5a3c0954395a98e8bc8da8b67eeb1acbf5f17b      1
    128 0x2b5ae547dd73e0d0e7fdc8e433a549df428d91fc      1
    129 0x2b65ee5b1d99ec9cb2a8217bac34f0bdd1a19c4c      1
    130 0x2ba0fcc894522df700060a6015f7e96537b18412      1
    131 0x2beae0f40aee2d68fb9844b2cc70ad545b0e1150      1
    132 0x2bf2f0a96455fcbc86accda16acf7b04d2dd5343      1
    133 0x2c3c011a0c546fb4c67cdcc954c37355c86b29c1      1
    134 0x2c419c8b207b10b39673482d83caa3e11f3604c5      1
    135 0x2debdf4427ccbcfdbc7f29d63964499a0ec184f6      1
    136 0x2df64009b4b23292cef57680c576939aab1dbd45      1
    137 0x2df727da5b2071d16b94abc56666591c65bc73fb      1
    138 0x2ebd1a38b4eb11981ff2fb7899dff1ef2b5a6c5c      1
    139 0x2ec970270130edba7d9b1f0f7ce7dfb3d1f6cf6a      1
    140 0x2ed5ab4d78053b9cae07ffd772c445265a280225      1
    141 0x2ee0485f71764bcd2062a84d9455688c581b90f8      1
    142 0x2f486300ebb6102b459764d67364d527701189b6      1
    143 0x2f5926896bc7f9432392099bbc66526cea300077      1
    144 0x2f6702f8c3256c04f81167820b5b321e2c693adf      1
    145 0x2fa2eb271bb8e87c6f783204a56fd07092611aaa      1
    146 0x3006281df61c5e2d33b749af7f13e46f50853a61      1
    147 0x30148408c85e9fdaed9a64950d09a0c0a62c6e2e      1
    148 0x301cc218009608e59f9647f4b7dd8a49be8493af      1
    149 0x3098bc2a03e2078c4ea8d9399f62739eaed32028      1
    150 0x309ed08dc119221d2389bef2fd784e79fe0be02c      1
    151 0x30f2a414945ba487f6a9ca909d0cc0919c6a1812      1
    152 0x310ffe8defda3b8fd626ef05ef0b34cf0363e90d      1
    153 0x317c32ca0309bb8a1f9d8a3e2836544f233c841e      1
    154 0x318b811b59ab9fa6b8adcdd5a5eae70fc5686efa      1
    155 0x32627326386cb803de44248fe4dc5e0edfdf238f      1
    156 0x334022d77bfc9e8aa5b34907873457c545d9faf2      1
    157 0x3347b7111aad3b2f9f46983f1a1c60b99ba6487a      1
    158 0x335fa434e65c21941079151a285bc02a60c44350      1
    159 0x339eaa8d23a8c19a05d25c8cb10383d44bbd953e      1
    160 0x34477ab85120779e938d63a5969bdceb0c08bb0d      1
    161 0x347af5ca1b0455ec31acd09ba9b2c22c231bb3df      1
    162 0x34868ed83815aaa53cd1f85032beaee678a9e8de      1
    163 0x349dd667feeb7e75545aabbce466405e787d5dea      1
    164 0x34ad85c9c383d4c4d0420081ff85180fc8774c25      1
    165 0x34c2b83f0fe192aff0f00588556e56b09d4b5f26      1
    166 0x356d6b47a025c22f4165277bc0d3c07423544eb3      1
    167 0x35f2e1d3a99c3fd78cd26db53960833b994448ea      1
    168 0x360d5b47285caf3f85ca68181031c4b28795c0af      1
    169 0x36ed861a319278e5b6c39028884b0ca93df105d0      1
    170 0x3932f608f69eeb3e58ee067be0b03ca4d82da6fb      1
    171 0x39464bd60753db2e20f5c9a0c1a348056df56a91      1
    172 0x3991b0bb5095f14b98962ddc19cecda5a0d591f5      1
    173 0x39d3158a7c9d4f8eff36524b9a2065465995932c      1
    174 0x3a051f1d0cf548f5c24893934bb69cbf61844c9f      1
    175 0x3a515e1f01837c9ad48b578e7e60c85d650dcfba      1
    176 0x3a7483be794e67e003814ef77901fc06b43fa22d      1
    177 0x3b3ad18ffb699eae4bf7997c104a9b206ada9098      1
    178 0x3cb3cfe0c56215220d40d93353e0261e14ce9bcf      1
    179 0x3cca02f925ee47a0333d3e40b98956479b580d89      1
    180 0x3cf297c769b57c753c5d4606383c9249bccf6857      1
    181 0x3d6c8d09f33873c73ad60b54f1486cac3c2647a2      1
    182 0x3db0b224099e56488d93f4e887a6bad88c39cfa4      1
    183 0x3e27c4988d7a6498a28a771b6ed2fce5660291ea      1
    184 0x3e2daba02b8b09879ed9b517bf4603a3dd9c410f      1
    185 0x3e5cbba78dd36f651cefe21951de8f153ab1f8d2      1
    186 0x3ed03dde47b949d35113a7480b68b462439a5975      1
    187 0x3f6249c4d782aab00b5e9ab99e090035b80e0c15      1
    188 0x4095576446e7619608fceb14d225dec9f096b3ec      1
    189 0x409ed6cc5411d95122b332a20d3e9e0a45529bc5      1
    190 0x40d6cc4ad15707844b320b1d3815e0f0cf09ff30      1
    191 0x4161cb28b929e90c284cd280daddfc5cfc76188e      1
    192 0x4163b00bc2ba78f2ee5b17f858c32838beb0e29a      1
    193 0x41b68202dc64904e09d2e4f0a94879c741f27db2      1
    194 0x422b5827ec05142c3a972fc34a22891e229654c9      1
    195 0x422b96fbd43583f22006d704cad460111553e114      1
    196 0x423af79ca18d95009bee04fe80ff19be10fa2ad6      1
    197 0x423fa6f71071926cf8044084d8b0086cd053061e      1
    198 0x4257564c6a110a896168a42efee4c5ae3dfd26f0      1
    199 0x429c8a65f4858ab2e6dc7112e88f0c0796a42692      1
    200 0x42c2cf604842e6b3195b395066ee4ee19045e741      1
    201 0x437a0eb15b8afb8fa8a45c641498bcd4dc7fb91b      1
    202 0x443540a94c9d53445725c9637d533e0a5faf40f7      1
    203 0x44c2a80f8b924858db469a5b35128908e55c531f      1
    204 0x453f707df29334878c5c012da747705c2c6e8235      1
    205 0x457a223a59de9f7311c5d9af84bf81ef55c87735      1
    206 0x45ab04c54264f485eef8db0c20e531e9d37cd53a      1
    207 0x45c36f237fc406a0da254078f6a2efe08b921e62      1
    208 0x4688b6f3c384c3cffe95c92f599213de4a9fa9ca      1
    209 0x471c04a7ce29e9e79dbda1753654160c04bfa1d4      1
    210 0x472aebae9f5bf00fe02e18b5b49cc18b568f2687      1
    211 0x477d7e84de6b16551481217b3d177a3bbdb5828c      1
    212 0x4799b18582cbb51cc207ed9716034da309edf47e      1
    213 0x47b799c0f4240a84b2301606dd90acfa55f35354      1
    214 0x47c553e889f7bfa75cf3775473180a04e9effcfa      1
    215 0x47dccc7f5bec3b7a5169f0f3ff5f86c3938191ce      1
    216 0x48c53f294dd853f7779d3b11ba905e77397198d6      1
    217 0x49d4edb86a46ce214ab01f88e7c27ac7d26a19cf      1
    218 0x49f3d8150438b575b763314d7da472db8ebd4a46      1
    219 0x4a07571aa5d50373eb2c4bde72ffc10a3e580e7b      1
    220 0x4a75cdb098c9b4deb3095c4a209e1525e1d9d6c8      1
    221 0x4abfbd3ce4180c2752fb683cc002c4c57fd11ff0      1
    222 0x4b2a0281dd577e5767442688c1e84ab901a28d46      1
    223 0x4b3b0a6c0bacb088b6b0b0ba31bab77282068117      1
    224 0x4b92e36ced303eeb59bd6c5dac555b2be13fe2b6      1
    225 0x4bc1a7813efe373c3a2e6c6336fd41650e4ea5d6      1
    226 0x4bec1160255459b8f2145659ebd1468adfc758b2      1
    227 0x4c9ab1064da9c0d384530131498e3c34617a1508      1
    228 0x4cc84a4112922a69d15b27c07c000ceed99ef89f      1
    229 0x4cf8be01027ad66c4939181a5b8c5b2b281771f0      1
    230 0x4d0838582cab6aa41cc2fb5e65f87f2958dae620      1
    231 0x4d6fb29c0a7028b26f84b694f8ae4cd2ba90f0f5      1
    232 0x4d8106e28243662b253fb2a775ff927563bd1e66      1
    233 0x4de25fd241c4dde1ef996efa3f1e58d0ead33d0a      1
    234 0x4de45767e1b8038b335f1bcb25977f84af905251      1
    235 0x4e01fa9823f5a3f15539ed8597b8955d5831df46      1
    236 0x4e848cb280ba065f44e64f923a78a52141033f09      1
    237 0x4ea1577b6c155a588a6c18767e6faaef51091ac2      1
    238 0x4ebee6ba2771c19adf9af348985bcf06d3270d42      1
    239 0x4ef53c62b5e080143317dcd7e0598a4a6b5db28a      1
    240 0x4effe8c926e295467b9fc7b23048b72174638fc3      1
    241 0x4f0a9445b9b2d5a2c27814cdd92cf6da0e42dff8      1
    242 0x4ffb72b66eba24521736924f87dfe10677d82286      1
    243 0x4fffe1f377999580d589617e160714215fd99650      1
    244 0x507cb3192622ae463215e16f2eb7b53135ae3eb4      1
    245 0x508d963265aaa6b4e0c7851665013b7eb8c87355      1
    246 0x50aede2dd43570ef93558f3132450f4c483e63c1      1
    247 0x511406cb0b446a4cde4eea637d8bff4ef78fc910      1
    248 0x511845859fed52007ceca0332e7f1244d0b7a99b      1
    249 0x512c579153ac6fd961a7d9e7b19281b855aafbe1      1
    250 0x51adaffeeef731855e496b4d2d558e4bb12d0e8b      1
    251 0x51ec8bb228af8aed84d935e0cbed0fdb3fc39114      1
    252 0x523dbad9cb57615542fdfe65b78d0c0fb4de8fb2      1
    253 0x5290dbe2d1f7060ba2988a4e04b3b4679bc5ceee      1
    254 0x52ca3ce771d89598ad477186e7d469111f623a5f      1
    255 0x530b4c59d04156f02e95a349b1de94d41a3ad19d      1
    256 0x534b8531e362da97d808ba2ab3959fa2597bef11      1
    257 0x535998e95159fc5a240361c3d399498852f0c8a2      1
    258 0x5373c9763dd832f9ca5fe8ea011415715a7dea3f      1
    259 0x548d9422e865cf782c6485d175f3b9ad4d34643c      1
    260 0x54f6a34678d5b78205b024dc5db6222c3bee9e6d      1
    261 0x5570e477f36800e34c770cf44d066a50cc727342      1
    262 0x557c60995797fa7b47be105227a2e46148d85750      1
    263 0x55907cf476998d2f58591c6d0a10ecbbe249a8eb      1
    264 0x569ff93aec513dac5b818714268c4b1e49abcc40      1
    265 0x56b4764644d24e15050ddcf290959b89eba2ee34      1
    266 0x57045a139aa21497bf2df3bec82009769646a514      1
    267 0x57d70b1e7097f38773dbc982f882fdd772ccfff1      1
    268 0x581bd489306df3fd5095b79d914d0db0f52eebd5      1
    269 0x59405cb53fb84706f3c4b522c57d71f125fe5182      1
    270 0x59b74027768caa41c3902f369dc17d5f940d1fe4      1
    271 0x5adbb3c856f676bdb50875366114f7644b82f781      1
    272 0x5b33b6f41e87111b7d22685b297f086b5535034d      1
    273 0x5b67a18baab6e97b4b48c5ba84ecfd5350e824e4      1
    274 0x5becc69e08b4f3e76bc5bebe5a9e1693fd65f1f5      1
    275 0x5bfeb4ca066c9458842ac89b6e5cd983bd1a1034      1
    276 0x5c02e2dbd2752260b3dcfa32a826af3006f523cf      1
    277 0x5c4fe960950ba0e09a72869c3d51fe70f07580e0      1
    278 0x5cbb31e39ca93ca13d8e9f686bf749b14076bc65      1
    279 0x5d2ca9c220f66a5f181e53bac510fe1e1a0ba268      1
    280 0x5dca7bad26550b04a2d6911ba64bb7e7bdd67787      1
    281 0x5e09439b85aab7cf4371d44757f67ceb9dd12156      1
    282 0x5e3c1c9694346a27a7a1e33b904798d5393fad82      1
    283 0x5f73b065c8412b43e3db33cad0efc5038040a597      1
    284 0x5f98736a54e68104d5e4f272484840094712d66e      1
    285 0x5ffd8de19910efff95df729c54699aebcee8f747      1
    286 0x600798c86e380940bf92d353f057e844a0456b12      1
    287 0x6054787d63808d5576384cdfbede13a5b52a8588      1
    288 0x615daba0dc8ab7437fdad8ced7c27e8be1bddba4      1
    289 0x61705ab3e4f1948216f1127cf5209f60006aab5d      1
    290 0x617ed24cd137a30dbbe94b53de9ecf4b5ed8e90f      1
    291 0x61e9ddc8af951249a7586be8b8150c724fecaa9e      1
    292 0x62050220633afe524d98fe3fccadbeacc2b96110      1
    293 0x62a9c050f88dbf3244d1576bf5022825c4a924c1      1
    294 0x63754677a2ee99a140e85ed93eb6bb5a0cfe665b      1
    295 0x63d45f767a98f17ddc2f55171022692550602b64      1
    296 0x63f4bb2e64bbdf93c4b007bd88cdf38f4d251a52      1
    297 0x640cd530f93a46ab2eb43e58ae68631e561300af      1
    298 0x642387183ffd57a826f0d6d63988d33322ec3ebe      1
    299 0x647b7881b8a63fd8c6aab5b0244b9067223d0e12      1
    300 0x6494c47f9bf8aee68ae323d0332fe1de7c8c19a4      1
    301 0x64a18349b11f3cedff42c80733013f7f409e2aee      1
    302 0x64ad18fd2cde41578d231955c98a714f8cbac239      1
    303 0x65039d4327e54dd8e3c5388ce8e8a741e3b019f2      1
    304 0x6596eca2c999f686423bf30e40ffdb55f6b6f4f0      1
    305 0x660f95e3d6fc0a67cd6226a76bf7cf31a1c2a6ce      1
    306 0x66208416cb1daf88716af68e405fcc8994c059d2      1
    307 0x664f031869a916761d5151855289ba40bcca4da5      1
    308 0x666e2a4cb71ec0e594d02b9b4c898031ae54516a      1
    309 0x66e7fd9081c2eb343511f001980b6b3575494518      1
    310 0x66f5ddc253852f407d334f8e90e3bc46fdaaecaa      1
    311 0x67d693dee156cf89ee524b961cf19e4c023936e3      1
    312 0x67e5ab5b573f01030a1076dda7a79ef0f31a1bfa      1
    313 0x685cd3126cda027eb8b4136db2b088324282d28a      1
    314 0x685f2885fca8158f1390f0629d1e32e7675f19b0      1
    315 0x68667f9b371742fc25735727a220d044f685326a      1
    316 0x69265c31158da3ae19c82073e77a090254cb2c2d      1
    317 0x698f3eaf3defee3c5a00b64bd65feee9015d6970      1
    318 0x69c8c2923005d26eaeea9500d7602eff8c81c848      1
    319 0x6a08986aad2967bd659457472fefd9bcbd417316      1
    320 0x6a171ff0b5ea7edfa2b4ee9385a4f888f8bb8e02      1
    321 0x6a89560c4acd7100a8bf099819d213261865a346      1
    322 0x6a9065271055e710116209c8032d2e18e42535a7      1
    323 0x6b0a6825238919561b725fbef2024358ae916247      1
    324 0x6b6cf175b3dc7a732a1cea4a3eeccafee8d2dc85      1
    325 0x6b9120489028e5e577f55a5e36b2dd3a359f24fb      1
    326 0x6bcc8a952fbc6dd37626250487808924db2e8b24      1
    327 0x6bdf991ecf7e83597cd1e2ab26028c1adbc888cc      1
    328 0x6c2f670359a46e2d21daa954223caf3797d38600      1
    329 0x6ce9f49239e8adaf5bda1154262a3a054fad9db3      1
    330 0x6cfa99b2352163d70bd52de24cdbf553374b9335      1
    331 0x6d3783578cef837cee78990ad97932b34184e8a1      1
    332 0x6d5d4fb55c61019c5eb9236c3da58c774b8232d1      1
    333 0x6db8e922741c8bc816b30e8c944c3002c3b6098c      1
    334 0x6e01ffb8bff05755c5f5d3a22c36e238716f4998      1
    335 0x6e5b0de0add71e1052b11abbfe6c433dd0466fb4      1
    336 0x6ea87e3278f7fd4bc0383a87bc5e6a890712aa57      1
    337 0x6ec2997cf35457daa9de4cdc989c697d921b5285      1
    338 0x6f3f44440680cad37c8ef621f1800664febc44e8      1
    339 0x70a4e7e0b399c6ab736b15d66410f7015a8c6663      1
    340 0x70d00fc2a472ea9149952186dc264dfef421b37f      1
    341 0x71a7702ff3d858a0853a918fb44c537740a8bde9      1
    342 0x71add64adbb965bafd01437295968502eb61145e      1
    343 0x71d5daa6a7dc1b94918c8768749b5f9982200660      1
    344 0x72d930415a0b04cf4122e593413ef3422e049c9a      1
    345 0x73064f6c1c5dad8d917782aec16ab564feb3c011      1
    346 0x73d05c2ea70dfc3b220444c94567dbc84bb0d24c      1
    347 0x74a2e6a985b33454d39a0b333f51c3315cafc125      1
    348 0x74acc173f943abe0c54cc6acdbb1307715a796ab      1
    349 0x75256a46e98eb0f9c4eefc197eb3dd88d559a771      1
    350 0x75b772f2bb4f47fbb31b14d6e034b81cb0a03730      1
    351 0x75e698d81af7475c3da201065d262024895974cd      1
    352 0x7616e594f1832d0dc7aad3923967d76fd98416e2      1
    353 0x76fb3c5abe51698f18eb8cf34b0e6f079aa273cf      1
    354 0x77039399801f462b4ed13444a266b16355c471bf      1
    355 0x771388495f34d21c5574fefc04cd1d5811e00ada      1
    356 0x772725c580d2f533c11f0cba5169a58c793b50e6      1
    357 0x79a6ed4326d596c50a9bd965c8d8af101993047c      1
    358 0x79c5146bd916944b4f4aee4c2447644be2b78e0f      1
    359 0x79f238d3d0dba4bcba894be2e972b0818aeeaf3c      1
    360 0x7ad28ffa81c91c758ba48f26270c8df5b76f6198      1
    361 0x7aef0f86efeafea60f8dac562332b54a53235f6f      1
    362 0x7b8a6e7777793614f27799199e32e80f03d18dbe      1
    363 0x7c0cc383b247275eeea044f8be09f20fdc98d8b4      1
    364 0x7c0d2f1eb3dc2cc21d6118789d26f2db09311b1d      1
    365 0x7c2f06ab61a94fe9760cc8d9ad35a33c0472cb8e      1
    366 0x7cabb73f5b840b245ec2528751445da1f6dd7eee      1
    367 0x7cbfc34a25ec7efe239f779e664875aad696ace8      1
    368 0x7d5300e22db1b2850da1beed10a894893731c1a7      1
    369 0x7d68e5482797b92613a2d835cea3a9afd10ca60a      1
    370 0x7d77f4350945113b7f07df0465f5e87f75839595      1
    371 0x7e5ce10826ee167de897d262fcc9976f609ecd2b      1
    372 0x7eb47df291ff8f31f6402f9196229b013e3bee37      1
    373 0x7ee8f8f465896f56edbdb5209015b64249c96ddc      1
    374 0x7efc64b7745e4707d970469602b08ceb54ef617b      1
    375 0x7f757dacf82239662a8a81c43bb1efe37d141989      1
    376 0x7fe0cdab5d3798f7b7f405eee643a09f39fa329f      1
    377 0x8065f37ed93bc12669344d5954d960a9f8085eef      1
    378 0x810e096dda9ae3ae2b55a9c45068f9fe8eeea6db      1
    379 0x81b43dbe173e6a82575d657f2bde17dc0596cddd      1
    380 0x81c3112d7c5003ffb8356b6d9f2e570654e499c9      1
    381 0x81e388d5139f109e859f38230101e4f8b036d8e8      1
    382 0x8262987ffb8ea94b3186db3de81acdff2d8914f3      1
    383 0x827e1537171289e51b26b982d530d2210d3676fa      1
    384 0x834cde5f00cc4f1369c747714fb3f1ea5e692a19      1
    385 0x835a8a2a2f5c364c110347faf4d3cd974998ecef      1
    386 0x837d29d2578c2263c3cfd0853b807e4385e1fc2f      1
    387 0x83f61d3c25f0596ba217426edffa6a446169148c      1
    388 0x8411a8ba27d7f582c2860758bf2f901b851c30d3      1
    389 0x848ba349b050fd8b703cc5fef2c85ae7d8350556      1
    390 0x849a27048de274c084f42551f395823f8fa18f2e      1
    391 0x84aa1655d1f3f3e437fdacf0f957176baffc9869      1
    392 0x85184cb128b27113d9a5f61db1a1590b9d7e286f      1
    393 0x85bb680ebaef5524acb04ac8d60154f5a2c26121      1
    394 0x85ec5daf4385387443363d5039b8cffc56ef2ce3      1
    395 0x869ce3c7415f06db4ff6d599d629c1a9c7c8a820      1
    396 0x86fc973fc5146132afd4421497d031a032e2de71      1
    397 0x87035db09597a1c60d26c68ef079fefd6775e100      1
    398 0x875888a38ab22e435a7249aaf54a2c8f567528fd      1
    399 0x87bd35f9d42eb742b875cff296fe9961434931b3      1
    400 0x87f928cf25b212c627936c790850a9f4a4eb07cb      1
    401 0x883d7f3d2164cb484f097b034f32629fa251fe91      1
    402 0x884b2d521067b69b5cad884b91f9432b242eeccf      1
    403 0x888f9b2d893356de0d2e1f5ecae7f1ecab8b5b3b      1
    404 0x8899d0dedbb6ddd065e0fc1e7b3aaed9bc5d0ebd      1
    405 0x88be96dc047cde01286e991f7333a2e9a4865856      1
    406 0x88c8c0ddd49cdf3e0c26add290c2e63ed1ed8a0e      1
    407 0x88da36152d4691e8d4efc998ddb7b22fe1b16898      1
    408 0x88dd694eb900a670734892ca2e89f20defe2ac8c      1
    409 0x8928b26de9ecc59cacdba095c6ed6237f48ddbd2      1
    410 0x898952b4d071ead9d3bdcc5e430b17efc4996737      1
    411 0x8a0454b05a952c572619935ed46b6d8d0aa97a76      1
    412 0x8a1a6616253cb617d92f5e539b3570a9eb483127      1
    413 0x8ab6a97731705ca23b492ddad64524b4dce33647      1
    414 0x8abe2749b67ad7debb7442c46ea4a0e003d4852a      1
    415 0x8b66b420c4f3137b2bb4aa6a291bd81790bfece9      1
    416 0x8b9017e5e1e58f363faada402b63a6effb068d8f      1
    417 0x8b95f763e9c3ef95ac00ad6fd942512ebaaadbad      1
    418 0x8b9a3a156a32a7cf1b29b370b005a458d870176e      1
    419 0x8bc8b4e4921101bcc56ec084b906069b73de7276      1
    420 0x8bf048b0a714ef31398097037510ba449b2b99d8      1
    421 0x8bf3cc3e9130e994131a35f394dbcb6ff0bddfe3      1
    422 0x8c218c305dcd6a69e2f5d006609d929e3c440934      1
    423 0x8ca22a53169ce83d64fdde6f661a199f22ec4d1e      1
    424 0x8d4228752c94cc70c72595f45f8b0d8d869f7fdd      1
    425 0x8d7f598347e1d526e02e51e663ba837393068e6e      1
    426 0x8d8c82dbc693262f9e7748832df8fab11d29db49      1
    427 0x8e22a4c2901da99777d76f80f29640fc81ae96bb      1
    428 0x8efdbc11a0cf4ca2b93a083c5204db32833b5b5c      1
    429 0x8f049f77f04b5ecdef56fe0de2ed1d1c9019e83b      1
    430 0x8f3fc038da33e825e763624384f123bfaf543818      1
    431 0x8f4f373e9c2cd1c7e1158234fb42e48ebe6b7485      1
    432 0x8fb9afdcc074599d86327b731f47ffa5375823a9      1
    433 0x90322efb6d3ac81f5083cf89422a2909c3eb31ae      1
    434 0x90632e6f10e091d60325a07f3078b565a84e6a87      1
    435 0x906ef92ba0f4be8f87b8529ec6d8546cd85b263c      1
    436 0x90a28a801dd8b138d0d4e6d007107818a5ec7926      1
    437 0x90d97772f4469df443273d2946aaebd5158f75af      1
    438 0x90e984118aa0e7c8ebff4e644c62cc8986ee6d17      1
    439 0x911133ed843df23f9b8e5ab51933aa6248f27427      1
    440 0x911e33dbd5fa4d72d0cd0e6676fcde8831e10f2a      1
    441 0x9212694c3647638a6be268ad96dbfbe39a03a40e      1
    442 0x92400fed781086a3943a923075c18177f588c954      1
    443 0x925c559c7a4015fd25b47db32c85a4902c021952      1
    444 0x9260ae742f44b7a2e9472f5c299aa0432b3502fa      1
    445 0x9380da47618d355a9eec7a2ec37e50c2df9bca6b      1
    446 0x939484d835d090f44929e63e45ec2e76cb402a08      1
    447 0x93ecd5b996c4a3991b5662e43072997bc46a235a      1
    448 0x94340be5e70a8135c73ddc23e1896190cac34abf      1
    449 0x943a76484fac25dfab5e0f817221011b3571b71f      1
    450 0x94d7fa78a5477f91c8bb1f321846a2b7902c7c64      1
    451 0x9509ad9a025155857f9d788f34d30f52aa2db8ed      1
    452 0x954d65e5310607e12f93f6156de92cd37ffcae8e      1
    453 0x9569f9aaf301fa85ad078ef8563a9b7261ceb771      1
    454 0x958914bc3fc61629dcc5c11ce9d2e1dc254f3e57      1
    455 0x9593a156345ff9496701e16e3c5262dfb8ba6a46      1
    456 0x95c1504657ab244cf8b728625fd5e57a14a05606      1
    457 0x95fdd3157edae1f77e5e65cd269018353938a585      1
    458 0x967b073201215cabbbc0f1c1f2b258683b495cb4      1
    459 0x96f986dc2b32849f0239caca962424fb19981bb1      1
    460 0x97041728d56a7cff259f04d50e1394663f40548e      1
    461 0x9712927bf5eeddde93746371c950dc16195ac063      1
    462 0x97c923cf8d4f6d373d6c9c7d90a7fc87e356b702      1
    463 0x97d63485fbf624e4baa64291a294d0b9b55b32fd      1
    464 0x97d852801d8c728bcc4de82640c8811406434eb0      1
    465 0x980c2c317bc65801b45f2265c188bc054fb5c1e7      1
    466 0x9891934771d43ba7ed894b0295d90b5b5d61bff6      1
    467 0x98b341d8e9f281fe988ea3e6ac7a27625fe60d11      1
    468 0x98dee2f43682a89f1c8405adeaadd47ba2de20cf      1
    469 0x9903b5b880384f3999352b770790befda7cc7fa2      1
    470 0x995e0505603a19ee5c469d2359427bea68c6e953      1
    471 0x9a816a7a6236075987183c3438dd4916a0cf3947      1
    472 0x9c26583abdad7a5551fc1e85097a161b76e16450      1
    473 0x9c34a8bf0014a9259ad07e79e9f09c7d3ef9ef4b      1
    474 0x9c733fb1e7ccb61ea4935e673ad8411dc3514496      1
    475 0x9c924ae221f00035c0c0660a149ea30f17a036f8      1
    476 0x9ca09a0349e9b14acfddb76abb9abaa8879d4f57      1
    477 0x9cfd6d74396b635b7282bfe7cf949897725ab69c      1
    478 0x9da52a485062df0f23310ef3af3cea34278c69b5      1
    479 0x9e575c86934b02fdd30a537e2d6ff0761b81711e      1
    480 0x9ea2fee69fca3f4d5d668d0e3f55a3260ce29642      1
    481 0x9ee5e3ff06425cf972e77c195f70ecb18ac23d7f      1
    482 0x9f42fdf29597241e83df7d9f9963bcba4436b1d0      1
    483 0xa02bfd4376c12983a9749e609b209a87ed3e287c      1
    484 0xa071dc9b8367b59dbf5fffcbf5565c30425045ce      1
    485 0xa0bdf16f3c91633838ad715a4bc7e8b406093340      1
    486 0xa1039ab0b6a8f9732bebe4cbe04a4222c256a648      1
    487 0xa10d998f221c2decd8209277edaad0daf91654b0      1
    488 0xa14fe103bc14c6a88f2784f2e018fd71a25e46b7      1
    489 0xa2917120c698fb5f2a03e3fd3524bda85a3eaef6      1
    490 0xa2cc08ff4bb69714327f3ab44d7994a0c4ec8302      1
    491 0xa2ed517eefc5365b05a970622af0f01586453510      1
    492 0xa3bb15582fe17cfce5c0410862823b0a11789d11      1
    493 0xa3c77d5aff9fbecb27c75530eaa59deaf12e3c3c      1
    494 0xa3e525f573668bed0fc2484084d1505189ee3aa3      1
    495 0xa3f73670340b7e45bfa163ce1db77bd9264688bb      1
    496 0xa43711f5dcc3559a1e07ec124fd167e26c9bad43      1
    497 0xa450add6e6455d28f1f248c455b92df94e6d076e      1
    498 0xa46664ba7222221475146c6710c812741a6c8bf5      1
    499 0xa57876de5e9a0c58751b51f704cf2db517788a6d      1
    500 0xa59c3abdb871d866aea688cfbb4c5bb52dde7482      1
    501 0xa5a0b7c3dd5dddbfbd51e56b9170bb6d1253788b      1
    502 0xa6134eb7381978804646088df4bb42c5232d82da      1
    503 0xa69f1ccf023007165dbf15b6ce96a6ce043e10a2      1
    504 0xa6c12d417553f4c9a12c6d4376bc2b56e43eb2dc      1
    505 0xa7269c74294e399d700a58be9f035b652dd67e48      1
    506 0xa72a5f58a8e7ddf238858a6f90547292b30e8c58      1
    507 0xa7695409c5fef39a8367759a279386302a683b9a      1
    508 0xa7ecfc1e491baeec24b33ed24bc84916af54f240      1
    509 0xa837c3d5fdfe4e878a3f8370df89cfd972b116e1      1
    510 0xa97c7af7532e661db802069571920a718338618d      1
    511 0xa98d2867de6b1dff44fe4b355dea098e81d06aeb      1
    512 0xaa1766e466e961de4bfdfc5900ea90445363d0e2      1
    513 0xaa6f81a48768f1eb8c6c63dd349aa115e08b117b      1
    514 0xaa950d1cadd39935ecf1a11eb9f44955f21d3c90      1
    515 0xaada895f7339eff0f01a11fec26082abeaf10b8c      1
    516 0xab6b0eb346947452c7feb7ad4f5b7aeb5b4eeca8      1
    517 0xac05a3d00bb4d7541e6e44e524509048b0bcc72c      1
    518 0xaca333db2c8f8ef2142cf01c112cf2444b01004e      1
    519 0xacef91efa4e7467186e133d0fabb47ae1731cd9f      1
    520 0xad4294bcfdea6ceb2a4158a38a1a84a6c1a04052      1
    521 0xad91770c1e6b8d2937317ce589a7d31d856b4ae9      1
    522 0xad9f4655afbb676cf93b324c6d0852498a4f48f0      1
    523 0xada82ead2d86bd0ae4f04deba68bfa5395d37ad6      1
    524 0xafc68fea4556ce538a27804ae82f7e601df38bfc      1
    525 0xafd3311696dff4ee47e15bb3e424d9adeaafa0cf      1
    526 0xb011ab6f339acf72996828e5ceffa4a9556fc78d      1
    527 0xb0ae827e7a8c4b4eb71d4119c32a250333769364      1
    528 0xb0fabe3bcac50f065dbf68c0b271118ddc005402      1
    529 0xb123b174a74390a32beee7a5a0138f89b5b5ef70      1
    530 0xb14fe5618a5cf6d2df8f3f8d6e893a00ffbc48d8      1
    531 0xb1e079854268985431935ce53aa54c8e1722fa0d      1
    532 0xb24145bc5c77391484b57e05e47727c6aa58a2b6      1
    533 0xb273f2efdab32bdf5639b58a7993a096f054a7d1      1
    534 0xb2afbea61b32693eaaef380da99c6d5bdbfe3e7f      1
    535 0xb3106d09d4a8733484f0047d80a1436da156ecb7      1
    536 0xb3a1581b786d724d34377d2a90c6f239887d1908      1
    537 0xb3c907670200ba46575cdadd8f6ab553d9c04734      1
    538 0xb423f2a7786d8fdd70aada0d13a11ed573a84c57      1
    539 0xb43f3ae4301bf4a0e93e41b34d1ae88eee3c3a3a      1
    540 0xb4bfc43881de14afe8616cf04ef6003ce57b3b06      1
    541 0xb51910166a76de8afc66572f16f222f5d867bf5f      1
    542 0xb63b34508bcbc617ecb57b7c338282e1180a510a      1
    543 0xb6408cbd8f6d2ab32055840dfe2bc52274f6b3b5      1
    544 0xb682ef20de74af004cc2c418dcdc5ceceaf06d1b      1
    545 0xb6af0e59e41f75552af00138a9f62acaef2b6254      1
    546 0xb6e650a26e1d0eb143767e14fbea63cfd8523311      1
    547 0xb7de05e1ba9209608984e8d53b103b7d0ba727a5      1
    548 0xb80dbcba7b2a38ca14862674db5f456f43b8307a      1
    549 0xb8d4651ee9d97d7da426f82648a20d0e0fccd1d0      1
    550 0xb98d10d9f6d07ba283bfd21b2dfec050f9ae282a      1
    551 0xb9a900a49e1a22518a823ec43f9542c52e422c20      1
    552 0xb9f1eca1d567fb0f94990bb831f473530ede36d4      1
    553 0xba07c823f84bb2aa1cfda61cfa957750f2cf0022      1
    554 0xba3693094dd9d67ce95114579116eb4bf459f103      1
    555 0xba7fd2b3913691b77c2a5bc413efa306b5a77fb9      1
    556 0xbaf66e1f03d860a790b0de6cb9aec272a5b3abb2      1
    557 0xbb1a5027643d93de9f11a7eae87d06cc7f6c5f56      1
    558 0xbb41dd49254e8b9d631b835062392a460081734d      1
    559 0xbbef2095440c94bc247a8b661224b2364e7bf0bf      1
    560 0xbc181f72df07abc55d76dad9bdcfded5b7e34607      1
    561 0xbc56f43098e07fa18dcc4d5064eda629f2c67a22      1
    562 0xbd5126f2abac3b8030fdf0125e6fe5f03740b700      1
    563 0xbd72d021d3cb334deb3151db905ee073b8eee518      1
    564 0xbd8bee1e3953ae9211782ebba33d5168ab9ec157      1
    565 0xbddf499207d29e920c0500642567b43238b30fd3      1
    566 0xbe1aec4ae3b0f51a340519ceba04649420e3f3c4      1
    567 0xbe30eec0a41a86081d88eff15d963bcbd1e40eb8      1
    568 0xbe836bf050418a7f3cbf54284d49daca491fe3a3      1
    569 0xbeeb1fa8da9730bbc9cb3b5aa4e6ba0793fe0d21      1
    570 0xbef3983add26908b9b571eacbd63a04b16d602ba      1
    571 0xbf0d78ebe5e2c5a8ed9dbe42d539f5e8ea115d61      1
    572 0xbf463a3a99940a2bb64804151be0ae4200701365      1
    573 0xbfd0d2e488a811a3f96958046619abbc5a55f6d4      1
    574 0xbfdfe0b764bff626ecb1f746242e0ef4f230f8e1      1
    575 0xbff2cff10adf6cd9fbc9c495bc81e28cd1003e23      1
    576 0xc03edbf9b3fc06fc48dc482bdabd4282f70fd2cf      1
    577 0xc0c78af0a268d682dc348a00d081c655c104c907      1
    578 0xc0cc7fe46abab55a86120d6993b35a1c1faedfa6      1
    579 0xc167513801a009972f655ce8f57c50b0b4e70489      1
    580 0xc1bc1b32b437b81345b84f77d300d95bd24cbb95      1
    581 0xc22d03e13b888dabb33a1575e4f6542f0e996622      1
    582 0xc2f98e1e5daf31df23fd3d8996bf1e3e69190902      1
    583 0xc34ef2e1f74403c1366f7ee8e02465378734994a      1
    584 0xc43327b13ec860d8303e02cdc891b064bb5b7c4e      1
    585 0xc44505d1111ec42279aede122a77cba17359438d      1
    586 0xc458e1a4ec03c5039fbf38221c54be4e63731e2a      1
    587 0xc48d912c6596a0138e058323fd9929209a66cfd8      1
    588 0xc4995d598a6ac11c100bc59bd57eea75dc2cd5d4      1
    589 0xc49d2210568549898a636412a9801559b828de83      1
    590 0xc4a72063c03e77893b2f0181ffd22b34cab170fd      1
    591 0xc4badb4f5c97295b0573bc9ba5f68fa707c1ae96      1
    592 0xc4c65d75dd6579671db24990bad1f502d7c96c8c      1
    593 0xc521d52fabc9ea40f72c7065b2af9f0eaed20150      1
    594 0xc535329222c1d35f31c2e7314b38e1c85eb2091a      1
    595 0xc63ea8a88f6aee27323fb70f869064d76198e5d5      1
    596 0xc643c9411a6b489e9833b16631140f42bbfcb6d1      1
    597 0xc64cff13b928f67fec4545d3a485af317a535f91      1
    598 0xc6571c2fb66825f13b7751b1c334810d397618eb      1
    599 0xc68c7771ec6a6e5d67d62aa9c6f22df69865e401      1
    600 0xc6aa79a22b08c794b25dc58cb36f2250779cdf4f      1
    601 0xc734b794e323d0256695247c305ebc6964e57f60      1
    602 0xc734fce96d3dd328fd8bed5922cdee54ba3beeb6      1
    603 0xc7a85cdcf6d90e49d6cf1497a4c2432de0f2f5de      1
    604 0xc7f354b172e53c0dffa8db17b028f634cd2efcbf      1
    605 0xc8404bdf586b617298c06b8acd398baf5a407377      1
    606 0xc8a7dd90d2fb862f1db3a1f987f12e31f9d6ba47      1
    607 0xc8c3fdd5992f7e89294715ceef9cc2c8b1171d7d      1
    608 0xc8ed3c2d1509fcf3a3c97c68de3dba66381d337c      1
    609 0xc91444fa4c5809e07d9541a9671ff9a9dfdf3b84      1
    610 0xc93b3390679481a31aee78b12f1a17b600f2248f      1
    611 0xc96018558f8fc9cf85deeb083d6137e21470e80f      1
    612 0xc9622305993fa2ad798e34ed55d980121a87ed67      1
    613 0xc9ae113b8451d0afc8a021d796322d545a7e7dbe      1
    614 0xca1747bab1141143ad591790cae13e56f1f2fe78      1
    615 0xcaa95ef5bc0f724a76bd4ba66ddeb6d50da4d116      1
    616 0xcab4af915606304fabd7adee53acf501b1fe6372      1
    617 0xcab864d7b4ee4265638c64755408fc27d8a883c5      1
    618 0xcb482005596f52839ae4505d73164027ad103376      1
    619 0xcb8ecb94f6c25af4fab696b91cbb0a1449b98f64      1
    620 0xcbfde5f46e13bf0b69538f4107954681fbaca0b2      1
    621 0xcc850a4f5ba7035b62dff2620e563021849a47e6      1
    622 0xcce79d5dcf26a64f5b35b2adfe71b66a261bc309      1
    623 0xcd4d5fd34c73e9ab939288f5e569c6d9cfe92d1e      1
    624 0xcd8b72710595c69019c109aef5b1b92eea7f995f      1
    625 0xcd90c980b25eaabace2a760b364ea5cccb95e2b4      1
    626 0xcdab759a1d97c166de67f2826b1ef276b04a31c2      1
    627 0xcdd99ee657a33b1da6802f4117d7e5cb2ffa5d79      1
    628 0xcead03574e4b930ee871bf8bb49922148a63a8e6      1
    629 0xceb8bfb81569437aaa899e7b3c77305754f85333      1
    630 0xcec771b3ab9204c4eb0b731111658e7c8ba539cf      1
    631 0xcef6e9bbce404d720a5f074ba536b75c2022f443      1
    632 0xcf49f35d142797ac68fb528e5a19f03d84f42f9d      1
    633 0xcf61f9a35cc8b5c33dad7224470ec7522f10cfd6      1
    634 0xcf9741bbce8ba8ec2b0dc8f23399a0bcf5c019d5      1
    635 0xcfb3fa26d88333a9ab3f7912209c0d5d7a9f55f2      1
    636 0xd061c30d9711dda98b4e1b174781d9cd587493f7      1
    637 0xd0daddf983fce88bef3f10fc12280d0f0cd1208c      1
    638 0xd113e94605276b5f0a1ca545fdb2a513937423e2      1
    639 0xd1598c76c78acc698a5241578ea0b21afb29ad44      1
    640 0xd22d53b95ac27ad97ac3cbdf704103bc9708a1c5      1
    641 0xd24a2f1feae9d9af52fd7e0f85f20190e85a1fc1      1
    642 0xd2b7e133d4e7044f7ef36f697f4ddfe07411b717      1
    643 0xd2dbe21025dede10517a8be905deb0af0bf2cfbc      1
    644 0xd2ec6fb85782e5c5982743f66d293617803a2e0c      1
    645 0xd31a84c20bc430ad75e6a1903e7ddbee52211072      1
    646 0xd35fc346e15ba5b446917c9fd23a9471d6144701      1
    647 0xd3de6d39b49110c9d5042410b072048ff2c170ed      1
    648 0xd42d52b709829926531c64a32f2713b4dc8ea6f6      1
    649 0xd4a08cf067c83d1b2cc1d26831569b7850804be7      1
    650 0xd54a6e9358f0e90810812f28b533c51eec61b73e      1
    651 0xd565e9de39844c30c8e976f54431456d97324e02      1
    652 0xd57721b29f2a17ab6a0635210ce05dbbecf5cbf4      1
    653 0xd5e77df1262e6329f8307c896f15391682ae950b      1
    654 0xd5fab5d372081b4f20fd4636bde96e9061aaa8a4      1
    655 0xd605b8c680c34f15d7aaf597494fd839f75eba18      1
    656 0xd60bb1dad4ed8df7e34ca61d3a9c37768da070ba      1
    657 0xd64d7c9672b6784da7036a46351821973061d77b      1
    658 0xd68936188779efb41bef5659b9183b34fb7963fe      1
    659 0xd6b01c20918a7ba5d05e81dc59ded322962b4d37      1
    660 0xd6d7c1c2ade9bbb59a39ff2697724af7b0ea65c0      1
    661 0xd722a299d14a19b56cddaeae0e1d5a5786424570      1
    662 0xd72c91b02b13c7061770d8d94631443383c3ee73      1
    663 0xd7473ef09ae9c072f2ece3fe7ce64e670eeff283      1
    664 0xd761295d455e472ba07ecadded370efca55e6cc0      1
    665 0xd793f8c26d5f84738671be17dcff6dfde5cf7aed      1
    666 0xd7b83c30609db3f1f4ad68d9c046703a7d06d722      1
    667 0xd7ce4706f31606981dc35255c8ce27934f9a7624      1
    668 0xd7dc2dccf31b671af9c2e4188cb94bcc2035fd54      1
    669 0xd81421ad38a183d973cdd6dbe2994be17d0d203c      1
    670 0xd81ce8e89dd987c8ab630858c8f1e9df14788c35      1
    671 0xd83d994b102a5e6b452469d26fe5acd1608954f9      1
    672 0xd83daee534fdcf461df67c454d325059b3b952df      1
    673 0xd84d7bdc4b79171917c81ddbb22a01139eb4cd84      1
    674 0xd9242f1b2abb9e13bfb52547683e775cc4b4a7f3      1
    675 0xd93faff9b7e14722d837ab11ec296968648c4cb6      1
    676 0xd9b42f222263c31e57d90a77217759ee6701b204      1
    677 0xd9c09e1ddacce4ee6548b66773bf4e6fe9dd0882      1
    678 0xd9f34d44ee1bbbca8f0615b63a92f8aa25bbeab3      1
    679 0xda3863583c9fcd8a5d3506d91aaf33157124d1d1      1
    680 0xda4745f0254044e4b702c9301737c2a0b8c69a6a      1
    681 0xda82f4790dcdacd11100720c322e438efb55ed94      1
    682 0xdad148546841885b78d52e36c5a26a71292b5a75      1
    683 0xdaed1d81a52b2af44e30afc332ccdea86399cd09      1
    684 0xdaf780714ce4931f98129060445fc6eb4ab73bb1      1
    685 0xdc0aed47dc736d8b9b9b602c843be6b8ef1cfbba      1
    686 0xdc3a18b34630d85e6e3075c8f37461e615e00f67      1
    687 0xdca649392b3a6de6a0d733fe5a5071ae12560f39      1
    688 0xddfe40c75392145314bf5652f1e6129e09914e65      1
    689 0xde1d5bd2c0520fbaa844be002a646c657a39f66a      1
    690 0xde8f5f0b94134d50ad7f85ef02b9771203f939e5      1
    691 0xdecbb4aa1d4a8d83e9f5914fa2bafaa804973467      1
    692 0xdf95a9ca4f6a7dfe2576ca99d96417a62deb2e0a      1
    693 0xdfa413375306e2169adcbbe8551f69739328e6dd      1
    694 0xdfc4fbf6dcdc7782476cef39a0ce618d7d48f83c      1
    695 0xe001891949372e1aa33c50c7ea19568be32ecde7      1
    696 0xe16dce8e44b310953255ffeaa9086f1e3600e999      1
    697 0xe1859a4728f188f1166cf28d4f033fa61260cbba      1
    698 0xe1e57c6d779adea0f34cd3b84fb6bae3a54d1cfd      1
    699 0xe1f1130571911c80dcb473ca99cbd8cc7ee21def      1
    700 0xe23a53b56b3c1d36fd2a558dfd24a216e7d06d79      1
    701 0xe25b6d080af358126a23a341d3aace7324caedb2      1
    702 0xe288a00df4b697606078876788e4d64633cd2e01      1
    703 0xe295e230c842bef131c9e1fba8bb83b155d7587e      1
    704 0xe32cbe7b5d7416bd432514c1bbe36a930b1d2dac      1
    705 0xe33173117612d12c368925afd4231fce32ade8fe      1
    706 0xe345ec0931d02b9edc9e962dc929c9fc14bcd31e      1
    707 0xe37567fe840baae0c7bea95b7d6512d3a7289fce      1
    708 0xe38bb256dc7010b3c6c8f6ec5d7bdb2ba7da6761      1
    709 0xe3efd57b602dc396f7bcdcca34b03890bd53bd6d      1
    710 0xe450637a5b515d5bb09d2b75cb4ca89700cc410c      1
    711 0xe48ac0deeb484f7df6d1ca554dc9bf517a0a597c      1
    712 0xe496c05e5e2a669cc60ab70572776ee22ca17f03      1
    713 0xe54140bdf0de3787d04f80e75a07b48e0c7da780      1
    714 0xe5c992ffcf556066e2bf90da8b1726d246374d8b      1
    715 0xe733b65ef1fffa734c7cea6e8463506fff83855a      1
    716 0xe7985c511d1bf0c7668757650b974513628dea7c      1
    717 0xe7f4fb77920dc6ce633bd90544cfc3c4288135b9      1
    718 0xe85f832d9f551cf445a6e8ceaaee73ef88a340d6      1
    719 0xe8ce8248ad8b87d56163c24d21772785f811544f      1
    720 0xe91c4ec4a439ec9d6b11ef8d1df11b35142df7c9      1
    721 0xe96ead0ad625122677684448155ba4ae2700814d      1
    722 0xea310c966d3ff5e09c65487f1763b21361eb71ef      1
    723 0xeaddbae2651e21b4ae261ffbfcc81970854c4ef6      1
    724 0xeb10f919d15740e4d1a695ff2ff5a239850b351b      1
    725 0xeb60ef8013a5d6c665037cfecd412dcd424a8d00      1
    726 0xebb1bf432de7629712a3706389ecfe110a3aa6f8      1
    727 0xebcea89d0e3beb500449f7216bb0f456d1fd93cb      1
    728 0xec3b8767c8a1a771478c3c94d3a56208c9c14954      1
    729 0xec5f1a1d7ea81e5f08b7ae00e6f56f161f6ab869      1
    730 0xecdf77c650d854e0c856579006edb79e2b98bb4d      1
    731 0xed94d0b6074f48045baa69ca3284774a6701070a      1
    732 0xee4018b8dc252c5e52e3c2c659c148bd56b4628a      1
    733 0xeec592c11a5f1e1f359c6679d1a6bebbc7e29e47      1
    734 0xef8c41b482b0b3114218434fcec8e97d3a67d91a      1
    735 0xf033618c0eeeb9342641a227c17d76c799866f52      1
    736 0xf09aab7003a3b82c421002d1f9ffddba151bdf32      1
    737 0xf0c15c42d12a66a64c18b7b3aaabd301850c2b67      1
    738 0xf128de46d785c308ad52b7f984af809b4a37b973      1
    739 0xf165a9793a270c1347badf883f48c36cb5e2310e      1
    740 0xf19ab0e65c902ffedcb95a76a93ca7bf789239ed      1
    741 0xf207e69a7cc3b4a00fec3039891a77b3df8c3577      1
    742 0xf2bd3daea43ed0e66bbdf5ac8b0bc853903b3187      1
    743 0xf2c18be6ff751ade812b1cb7f3d17469510afd5a      1
    744 0xf39ed15d848c131727ae2ed3901986b79e8549d3      1
    745 0xf3d7459c7f6f75252aadf594d2ea74f04b359f82      1
    746 0xf43b2be4aa887f426f05f78604b364af667c608d      1
    747 0xf4dbede0aa5d580b7191acff0a40a33c885f69c9      1
    748 0xf4dd5a36fd073f16110b3b13fadac5dedf49c29e      1
    749 0xf4ff9b80e17e5f5d3cd10fdf1bcd22526e78d477      1
    750 0xf5143eb146ae2cc89a35f453f6d4bd7aaceeea91      1
    751 0xf5234cf2c0d5a5e076b977ff03382c539a578bd0      1
    752 0xf54611a627afa7c00a58569932554372dd3f4b3b      1
    753 0xf5efa80c6bfc275348c6b46db6a26191a0469aec      1
    754 0xf5fda09990a3cc9809a2b87ec5b1b081c02b8e33      1
    755 0xf6926d15fc0ee0113ac4840e7d881d92cf193a7d      1
    756 0xf78dcc14cb66b3ba98e3fef21d79ba7525a3eede      1
    757 0xf80b68ba428ee8cabcc295a1b59615fcca41eaeb      1
    758 0xf82d46e02dd50c280050d45f22d7e22aceb0f0ae      1
    759 0xf8938559870a560cab98a03ebe850e5bae2eac14      1
    760 0xf8c73bde7c356a9730ea12b728ed236d6c9bbde2      1
    761 0xf95f8d46c678f138406b15a795e959365d94a1eb      1
    762 0xf9cf3f5bb025a260deeb2e20861248c4894a42a5      1
    763 0xfa18da7e2db0802312bf47ed927ccffeeba4036a      1
    764 0xfb1369548e5b8768ca2c3ada0556606bd7d8b615      1
    765 0xfb949be0be9b4e99e824b1f868e84891aee4ee6a      1
    766 0xfbe15de877326b7b6597412d0ebb64e2b08409a5      1
    767 0xfc108e96697809b7cf9a9fb9d32560ed8ac96424      1
    768 0xfc3c6a609518116be8aa89b2e9e9dedfd0555c62      1
    769 0xfc7030fd3e3ca7541fd4f4a71b9d7b7243e83a37      1
    770 0xfcbf7a53ddabfd7ba9f490d312e8e5ec6b277f2b      1
    771 0xfcc2d631c320035bf59e48a49c5f729ebb452ebc      1
    772 0xfcf532153ef0a395c923bf59f597971bd69536db      1
    773 0xfd3e73f1ff7ea94fe5c30daf5f4898b4852004f1      1
    774 0xfd5fd34397f681b4159aa47581cb5ac1b10821c4      1
    775 0xfd64e8e4e7ddec10fd7b1667f3409307dcb5d1c0      1
    776 0xfd7308bf6ca04a6f8c3df61af2a56ebab40a529b      1
    777 0xfd9f09357696d126deca5be3f3e999338eb63504      1
    778 0xfe01ffe2fbb5e2b0fe1d9ba64e33b592c7b1e6f8      1
    779 0xfe0fb970566e5ee272692ce8b831f46317de639c      1
    780 0xfeaeb5b339fce39f675c484b91e687ce84274388      1
    781 0xff1697cd18f03242ea521a80334a9060f0c25c7a      1
    782 0xffdef5e9eeaceac0800a2f784105105dbaf1d884      1

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
