
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:31567       Length:31567       Min.   :1   Length:31567      
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:31567      
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 16874969 # https://etherscan.io/block/16874969
block_hash <- "0xa639339f77f8cec390c7308ac6b537041f3e3256bcfeedc963267438a6e6e3b1"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4623 

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



allow_bertone           <- pick(snapshot, contracts=c("SuperRare","Foundation"), address_remove=address_remove,address_max=1)
allow_gradient_phase1   <- pick(snapshot, contracts=c("gradient"), address_max=1)
allow_memes_phase1      <- pick(snapshot, contracts=c("memes"), address_remove=address_remove,address_pick=150,address_max=1)


allow_raw               <- pick(snapshot, contracts=c("raw"), address_remove=address_remove,address_max=1)
allow_singles           <- pick(snapshot, contracts=c("65291/1s"), address_remove=address_remove,address_max=1)
```

## Allow Artist Singles Phase 1

``` r
c(allow_bertone) %>%
tally() %T>%
readr::write_csv(file="allow_artist_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 31 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x00cb0de5da85a9736b5890142a5187d6d8c3c0a9      1
     2 0x066ad9510d992444a856731d6af0888c2b2e5d76      1
     3 0x0f0eae91990140c560d4156db4f00c854dc8f09e      1
     4 0x19cd2a253b3e559b2b5ad57170b4d5b97b64bfb3      1
     5 0x1b7f00ca9d8fea5a9853f18ad247f806a8dce57a      1
     6 0x1d4b9b250b1bd41daa35d94bf9204ec1b0494ee3      1
     7 0x2debdf4427ccbcfdbc7f29d63964499a0ec184f6      1
     8 0x3113ab5c691ecdac25c07bbf2bf80eb005e4088c      1
     9 0x3b0aa499cc6acde1d4a7433da6968d7bb8bd8509      1
    10 0x3b22621a2b0e1f8dd3056975efc2150d5fbee980      1
    11 0x3d4714e10f17a17bc4d2de4c5ee0cbdf07de351d      1
    12 0x461123fb3a23b5503c01afdb83b0e4387c98ed4b      1
    13 0x4761d299899cbfd0ad2419c38922a148a6c64254      1
    14 0x645fd16f3508391e8385fc0a181076994079aa1b      1
    15 0x71cdb6d8d33755834e99efe8f460a50fe459aea3      1
    16 0x7cb2c57ef6716ab9ffd68811fb1ab258570a6704      1
    17 0x7d87e23d7777d4829845f03f31721e10775799fd      1
    18 0x7fd6c3844264cd50ad2183afd058e3983dcea1af      1
    19 0x886c4d1d4bc99399a42ef9cdcbf0f0dfa700a860      1
    20 0x9769334fc882775f4951865aa473481880669d47      1
    21 0x980c9b472500f3bb6c08d876f45df3a1e3298496      1
    22 0xb1dc7628ca7e7fa568ff2e8d26b5f4efd3f8ef75      1
    23 0xbb34d08c67373e54f066e6e7e6c77846ff7d2714      1
    24 0xbe8fe12b9eb1ca2a593e6c070c71c294b6fe9f00      1
    25 0xbeddf3e43015f5221f5d1ac0fd0d2ab3352d2f7b      1
    26 0xc6400a5584db71e41b0e5dfbdc769b54b91256cd      1
    27 0xd394cf7e0c9f7c89f81a2870c576c1910974047b      1
    28 0xdcefc9ff1e47e458e56959154c1cdcf56003d30b      1
    29 0xdfd3b58cfc180168faebb6366b98aff25fe1f8f1      1
    30 0xe1d29d0a39962a9a8d2a297ebe82e166f8b8ec18      1
    31 0xfb10df58c750b140b09d833bcff487d32a07e31b      1

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
     1 0x04df8d02f912d34fef12a1b0488ee56fd6f7416c      1
     2 0x0a98f97e89743406a8611e1b4219a073b60ffed3      1
     3 0x0b3e3a27d85996e20b2cdd0e7f1e1208816d05b8      1
     4 0x0ca4dc095608c6b153019f734106581c85f2dcc3      1
     5 0x129c6695dfe7a906bd8fda202d26dfff601f83a4      1
     6 0x134cb213549c9f114176adb9e4a78d6a272a3672      1
     7 0x1566ae673ae80725bcce901b486c336e6acef465      1
     8 0x1a9aee029a1b8c69c4404890c85715cc726746c7      1
     9 0x1aae1bc09e1785c8cb00650db0c6015bd73b0c7e      1
    10 0x1ba6afb0620e08228cf9ecfa32052246b7f9e9eb      1
    11 0x22fbaa2dd20b848084545515fb04b9c846942779      1
    12 0x248458947c120ca057ec028b3fe7e4b3f26fdb3d      1
    13 0x27b1abafb2cb065cfaf41b4b7ee95d27192151b2      1
    14 0x2924196a2ec71ea4ae1b1357381eccdcee6c18f2      1
    15 0x2947cd7346b3a372b357256b8955ebd4b5735987      1
    16 0x2f2242b447a54cb110b8a7991cf8a27054ee6921      1
    17 0x3511ae23ee25e2b97dce883c7d496ff5d18f1dfa      1
    18 0x3cb63b82d778105e43f064ed739b0655f1f0fb87      1
    19 0x3d0a1d20ac580ca8fa35888c92dd0859887f3398      1
    20 0x40d2f4399d23f9afb82d0a6b73055f13208614f9      1
    21 0x43b0bd27d5016e969193412933387b0dd4cf3e0a      1
    22 0x477ea7a022e51b6eb0dcb6d802fb5f0cfc3b4a81      1
    23 0x48464efe55fbcae8ae0c992b306afcf21d4910cf      1
    24 0x520e8d6c5b2dbbe62bf8a40653a314d879b19b86      1
    25 0x53006f95def268f88dc1b8216654ab56f3afd052      1
    26 0x53562e8ecee54356dc89ad263a444528b34d6c80      1
    27 0x575f6540c16a72696c14a17fa64f049992d661ab      1
    28 0x59068075a799594db03c0255eed68e8e121155c8      1
    29 0x5fdb5fdb61bc2975f3be446c5ae9c5df490f55d2      1
    30 0x60d2bc84a71af61b82d1b4168c9103ff6c4898eb      1
    31 0x615502edd5cf2d59244dd0dd53d3c8ec035e172b      1
    32 0x61d9d9cc8c3203dab7100ea79ced77587201c990      1
    33 0x64f7de90dc79d775703bbec66a1591c7a26a22f0      1
    34 0x665654f2d2a151be2d0f8e3697e6ce780f732af2      1
    35 0x670a840848eba22eb2a090fd65e1ff60c3ba9e5b      1
    36 0x699990a8e7ada9e92c932d6e8fb365024fc74b43      1
    37 0x69e68074f1aada957edd39c5eae0069973343f30      1
    38 0x6f0735bf1e6c69030d6990cdd580345b370eb50a      1
    39 0x6f113b0c8c7266d7514e4986e3d6aaf013b6754f      1
    40 0x729fcbb6e1289c88ce5113ec1c83a48a8e3c9f2f      1
    41 0x73bcb8c5e30bf85806aade7fc36f16c6b80fd3b9      1
    42 0x75005a05bc163b85991b9c9facbcf3155372422e      1
    43 0x78a576d3751e8cc8f30c54c367fe8e5a2cd71228      1
    44 0x7e5ab36876a267560e7191cedbe99ee7bc04bc30      1
    45 0x82139687faae8a29851902783e02e699de0e0846      1
    46 0x85603a042455d2f36f3ec52c1c756344aa70c5ef      1
    47 0x88ccdfe9dd047b4cec4c1102d2d803e2d8bf683e      1
    48 0x896b94f4f27f12369698c302e2049cae86936bbb      1
    49 0x8ba68cfe71550efc8988d81d040473709b7f9218      1
    50 0x8bd9c4b5ea8f6f14c9b14d830ddb67f3720d77f6      1
    51 0x982d3c5223f6b5794fccb3208eb164d042cf2526      1
    52 0xa56c04347abee42f663eff9bc2d0147b97c8f782      1
    53 0xa743c8c57c425b84cb2ed18c6b9ae3ad21629cb5      1
    54 0xab2056903a7b62bac46f45a3d7a70ac799ca88cb      1
    55 0xae0d16586e5d60d334624c115216a52b9b1a0335      1
    56 0xae72c6a6fad9fa9d82d089e1ebf73b3043855425      1
    57 0xb6cf25f5cf8a1e1727d988facdd47f1dfc492caf      1
    58 0xb735af7ae1a77d1ec764c862c2c09bdbf2b34b27      1
    59 0xba4575ea27041d99e6614ec02318f1e23a623fe2      1
    60 0xbbdd72fcce73c2626719be00259ddffef0d5673d      1
    61 0xbf270918afe2ad16093ddce904fc358ad337cefa      1
    62 0xbf814810b44978de273191fd612aa47f7b69d564      1
    63 0xc02e6b0d0c1a5d8cd26beeba0fe8d76c5d2f19b9      1
    64 0xc2419841dcb9a0f8906d06463ae24e00e1470846      1
    65 0xc6400a5584db71e41b0e5dfbdc769b54b91256cd      1
    66 0xcaa1c396e70384db58dd33be74b26fb119e98c3a      1
    67 0xcabd5f77ca9d48f4ef9793f20de42ad39ce93979      1
    68 0xd007058e9b58e74c33c6bf6fbcd38baab813cbb6      1
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

## Allow Memes Phase 1

``` r
c(allow_memes_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_memes_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 150 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x022b266d1aa39725a4e64aee661f0277461e4e39      1
      2 0x02d61cc47cc9a19b2a023c33c094269d9af07862      1
      3 0x03abc3de5f7c23d9065cc5610e6496d72208357a      1
      4 0x04d8413a2e59c355cd2be59ba022dd0deb8fcd9a      1
      5 0x05503136f1063e9669764e10c1700ef6a910070d      1
      6 0x05e07f4bf676bacfd8d211895cb080e5711c0a24      1
      7 0x06fa75bfaa972845e7d78a6d928357f15a341d6e      1
      8 0x07b24ba3e50be7b4411138823176f4382163d59a      1
      9 0x0c0d7a7213cd4f5e7f112f35a0095c317267e354      1
     10 0x0c54a765c77a88d9cda0bd7ad9b10422848d92ef      1
     11 0x0da8ec9829af1b58d247078e398ce00420ddd942      1
     12 0x112d62b9b1deaa943e8befb7270a9167c7b95838      1
     13 0x14b76991c8ecda8cbe4a5d330df992880a986fd0      1
     14 0x15a415c7589efaf71345006d33bfa07a9a42c8c8      1
     15 0x174fb6da52a957b0620d0902ace4fea33ba84d35      1
     16 0x17f4d9e092d23d81860d8950c18fdf1dcce75232      1
     17 0x1ad4fb4852d257987fd43613febe787f5535af47      1
     18 0x1b7bb9251a6220b42e920350ac0fa3004b38729e      1
     19 0x1d3eda0356e492d51fde4ffb8651c549bdecbdca      1
     20 0x1f3fd666b44d0f68f50012470595ff36200908c3      1
     21 0x2879707ec9b6c9c84d9c2512b64cd6ab36882cbc      1
     22 0x2a3e5cf41a9a3ec33ae00cca569076f55a52837d      1
     23 0x2afc032aaf6d7ead33fb05f9d42d874874dfb9d1      1
     24 0x32dc7a665af94cb941de28de8bd69b8ae95f6b78      1
     25 0x330a7cb2418a2856cf8ec47c2606900fd1a57bed      1
     26 0x338a1676b8c959bb064aa8b4697d38600f85628a      1
     27 0x34433cb54d406d231cd511567f33ce1914fc888e      1
     28 0x35c1993379e9dcb3b1c152b2d7116dfcd373def4      1
     29 0x362861129a48153fe41396e0cb31bd822021e576      1
     30 0x362cf4be53248d0872ccbfba570254c08e2f3d09      1
     31 0x37e19cffe93041405c03321a2d1168317ee4a050      1
     32 0x37feeac37afa67d5211937b4fca166f91724ae80      1
     33 0x380886e656ce40bd145a9fb85ac7e3dc51d5ee8b      1
     34 0x38d779b6dc61acdf864cd289f3594ad05088df95      1
     35 0x391874c0ac91b5569b5373dd10969350b956c398      1
     36 0x3ac734159d2668a95b6478265ff4fe2792ce9802      1
     37 0x3b2dd499b5c4fdf97f794525a9586f62e334622d      1
     38 0x3b9818a507afb5b6865152a29d5f762e2f8d458d      1
     39 0x3c0fbce46ecc02f24e3916b368366300a1848ce4      1
     40 0x3c2550ddb9294c21d209999bdaa15f8249d0c161      1
     41 0x3c5d0689627d0bf30b38260a2af77a7dbbf2c033      1
     42 0x3ee9c72e4fe1e6fceafcda4c89fab3cf98e7c5d1      1
     43 0x3fa5a25f48ba1b736761706801be4f639ca4853e      1
     44 0x40b9c74ecabb64c797b284f7d4a4d945b745ec3e      1
     45 0x412d974ca20038402a4e50d586fee6ba6a9279ed      1
     46 0x4521b5e551a682ce13e302d11a8b6e66ac895657      1
     47 0x4571ca8ce79492fe7e2ba3023add71860ca363d3      1
     48 0x45dc6a497c6f1b95e9942fa6fc833b0089cbdbbe      1
     49 0x468cbad61a63ef6d753ac395d99ebacddcf437ad      1
     50 0x478e815c117bd0c33bcc6628d4e30d2776b7b090      1
     51 0x4938d68c208aa1f0a34798a4343c584c58d259cb      1
     52 0x4d2cd3e1d994ab457197efef5b8057783c2f7d32      1
     53 0x4ee1f9da5028d1dd85aca765f405180186572e81      1
     54 0x5201e3253c79b3d4003fc75a241f33379acbefcf      1
     55 0x53cbd34ca41a2373dc329c63dfca349d90139e27      1
     56 0x55205fbabda8036697dd087135440dd92df3ec36      1
     57 0x5a381564aeda17faf048d0eba3ad7e7015463f78      1
     58 0x5f22dbd716ec054a9802d1d0632902398d097c36      1
     59 0x60c4ae0ee854a20ea7796a9678090767679b30fc      1
     60 0x6ac029ae2e792a56354c544347f38d68db618492      1
     61 0x6ec04c89f16804a17cc260866e8f7087cdeac433      1
     62 0x7051beebdc1fe5a678a8504370a15f9491b30688      1
     63 0x7143f19d9df476227013260f015b06359918ef3f      1
     64 0x7165a04c41c9e5e67d4850eab1dc6ede84d117f0      1
     65 0x753c892626c4fa09d1aedc0e9b52bda97a4afa00      1
     66 0x75d7d7972a62b00ff7ef071741c929f59d185ee6      1
     67 0x78be6536548c362d4092d3a5c3baf6aa27b80ee4      1
     68 0x79900a702b993afa72f68a720af15cecf764d39c      1
     69 0x7b50b1f2634fa38d078eb067a4f8f22a666b49a2      1
     70 0x7bef8662356116cb436429f47e53322b711f4e42      1
     71 0x7e40366407d849a0133ee3fa86d7b829e00fc55b      1
     72 0x8054e8f8902287708a63856597382820cfe83167      1
     73 0x81a80a99c8ab3c57823d6235134979e8c13b2239      1
     74 0x87193fe62b63b811bbab15842c413b3bae94a6a2      1
     75 0x88d14d96aac4293b8b16aa53b379e80c96675ce4      1
     76 0x8b0996bd9573299908b7114ff55b1fa96ad86423      1
     77 0x8ca113efa0262ab27b5bf70e9fe72dd600775d1e      1
     78 0x8f4f8916a5c22aa4d0ca25695146e8e4dacc7b13      1
     79 0x91198e5b88051d7d61ae43f322641e6674697af7      1
     80 0x922d1874a9984cca520a6c078f9856a158442f57      1
     81 0x93dc8de488504932db3bc900045afcf88fa79160      1
     82 0x94bd59027a36469827442108631d4a07d0e08846      1
     83 0x953cc221d2d43ba9b7eb9bdf17617aea39cea774      1
     84 0x982b1d44ac3ff17e4e0b6e2091fe0ca62c9442a0      1
     85 0x98462fa6f7fe16966330c8f6b93a880716d54970      1
     86 0x9969db4034a136650cdb07955cdf5635499a4012      1
     87 0x9e236654f66929c0dd7bbd4a55314e24ed110262      1
     88 0x9fae8562dc4fbcdd9d26d6d10a7fb88ce60a028e      1
     89 0xa12cea84eb9e6dceecd125de27cf2e9701104956      1
     90 0xa159a996ed582d183d98fed1989b2f0a95a0cf63      1
     91 0xa36d30fc6fd9b6e23e2285474cdcb9096a72840c      1
     92 0xa46425571f389ce2ae50e24cc7efaa074adb972f      1
     93 0xa58714c5884f5e217df9583d1f8151826e938b02      1
     94 0xa698cade532bf7491f0f887284f8e10e2de97198      1
     95 0xa749aede014231311b1d7272ad64909721f085a6      1
     96 0xa976fb8ed77bc1016a06074ff5e3e4a5fa6161e0      1
     97 0xab1ed04237efedbf10ef3cab7a6409aea6bedd35      1
     98 0xaba2407532c89b522f9075c7676e1fb94d664103      1
     99 0xac1a04679039a1718d3820fbc254ce29269af784      1
    100 0xac2c874f01914c89601abddc6320ee36bb396d47      1
    101 0xacd2a5145014bc96a0bad0dc0a087873dbea7969      1
    102 0xb1adc3a1298819c6b1d18c2af1081f04b8d53176      1
    103 0xb1fdc4e671889e4f60977540056dd93e5155bd86      1
    104 0xb6d978afe342d4e1e7f8fd1276b02e69159ca845      1
    105 0xb71192988aa3759bc852455ae92128a619b97f30      1
    106 0xb99f3c94abf08d095b8d8c54c434bdbf696c597f      1
    107 0xbc74c3adc2aa6a85bda3eca5b0e235ca08532772      1
    108 0xbd45c0c21ad360df43ac5c5f84979b988eb6b74e      1
    109 0xbfa4670f66a86848ae730743227a4fe51f4d39d2      1
    110 0xc4627e5c93b5ce0697186ba3b9fb322c8d4c2e1c      1
    111 0xc70b41c06cd32245effd5fc34881e33022347cf1      1
    112 0xc72c604a60fd660256be2d7f4ebec13ce7f50daf      1
    113 0xca25f7fc6a8532a86df01e7aa9055219abd8a65c      1
    114 0xcb954087ecc62651b29ee5e5bc9d06e85a557740      1
    115 0xcc1258cf0b60452ef45100e5270eae77c45903ee      1
    116 0xcc946cb041c7d0a7493c9ea639353bfb70595812      1
    117 0xccc986e064cef2a844a9702f405d7ea6e9c9d2fc      1
    118 0xd3cbf8645d8f28f6758504b0cf2ab4206c5dd7cc      1
    119 0xd413f436a036b9773d7adccaac10242e27b9da74      1
    120 0xd63929e5f4c6e82f683d3a5f260a393d47e1cef3      1
    121 0xd683678594e7b41281b2736f1beb13ebc5b06ec7      1
    122 0xd7192081e5f481364c190022f0012a729fba37a5      1
    123 0xdbc7a4afadbff03d1ac183c9f664a95775c6d52e      1
    124 0xdbeca36c48306867d9285e0e0d5ed1f5aec28a35      1
    125 0xde3b81bce794f0031292431bd029df68931c030e      1
    126 0xdf44cb56916b7692fb13a33cf9707ca840a11142      1
    127 0xe0e876b155d720555da842ef12d35073aa22c64f      1
    128 0xe2cacedac44bd674d9c5e816422cbd603db9cc1c      1
    129 0xe3f3e433102cb26a5e8c88308c9308061ce6cf3b      1
    130 0xe487ed5ec80cca6e8a44f6d6f3f712088df8ad4f      1
    131 0xe4cbda373d6a445f83fc9125c6230ba58bc08320      1
    132 0xe5050e39addf3475f0d7ebb17c6bde117f0590e2      1
    133 0xe591f70fb3dcc0320f2a9d675c780fc440d3d574      1
    134 0xe8f0b091e408f1fac8d26707e2d3a27989d6982d      1
    135 0xe9ea67bf412f911370442f2cdcc7a5a08c00d6d0      1
    136 0xeb775bf133c142e873f2ba6925d53107550e8703      1
    137 0xeb946bfac0d78e7f0d12d53e5f5b890bff25c7db      1
    138 0xee819d8c6d914f5c7a217d1b1cef88056da786f2      1
    139 0xf17c382e51d9acd6eeee4df02076227c81256058      1
    140 0xf2626eadce87913c29e63b95e39552e1bbe26b44      1
    141 0xf3442dd014446b60d19cdcce0313160c3e515d41      1
    142 0xf448f4da8f09ff6f9303337001fc48eb83f3e1ee      1
    143 0xf54fe7f5f0f8d13ce9b685b8ad167b466b637f0d      1
    144 0xf5ada740ab105d8f38bdbb1dc6cd569d7d77a479      1
    145 0xf7496410e44878d05236dcb3de2238bc057c1582      1
    146 0xf8027e0f03c95782ef9be1826828bee931c1ab83      1
    147 0xf87b712e4c7905d6f5768eb216c1048acd35ad86      1
    148 0xfe504884abd05cb7fe828afb689c06e2a5b4fe64      1
    149 0xfea1f357b453c9cd89b893b07baa6abfe8536ca2      1
    150 0xfef8bf42f7658cfb496365cfb1d75f6e5b29edfd      1

## Allow 6529

``` r
c(allow_raw, allow_singles) %>%
tally() %T>%
readr::write_csv(file="allow_6529_phase2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 4 × 2
      address                                    amount
      <chr>                                       <int>
    1 0x13eac939d3c7ff1a985d164d1f14411505b4c822      1
    2 0x2c8875f34ceb219f61b7453b2c5f100ec2f6ed33      1
    3 0x9274f2f89fbe5dc00c21c628e46a18c7187c14d7      1
    4 0x9dc5b2cee788e3e9c67489b5d7ce634dbf48a32e      1

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
