
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "15964158.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:1074        Length:1074        Min.   :1   Length:1074       
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:1074       
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 15967569 # https://etherscan.io/block/countdown/15967569
block_hash <- "0xb4a877f65102609d56019bc2eb2dcb4208f9c0e4ff58350d62c458366e2d4d66"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4514 

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
  "0x41a322b28d0ff354040e2cbc676f0320d8c8850d"
)

hodlers_remove <- c(
  ""
)

airdrop_gradient  <- pick(snapshot, contracts=c("gradient"),address_remove=address_remove, address_pick=10)
airdrop_memes     <- pick(snapshot, contracts=c("memes"),address_remove=address_remove, address_pick=40)
airdrop_wetpotatobrain  <- pick(snapshot, contracts=c("PepeExotic","SurrealLife","SurrealEstate","Elements","WhatsInside","NeuralVision","MoreThanLucidDreams","Foundation"), address_remove=address_remove, address_pick=40)

allow_gradient    <- pick(snapshot, contracts=c("gradient"), address_remove=address_remove, address_subtract=airdrop_gradient,address_max=1)
allow_raw         <- pick(snapshot, contracts=c("raw"), address_remove=address_remove,address_max=1)
allow_singles     <- pick(snapshot, contracts=c("65291/1s"), address_remove=address_remove,address_max=1)
allow_wetpotatobrain   <- pick(snapshot, contracts=c("PepeExotic","SurrealLife","SurrealEstate","Elements","WhatsInside","NeuralVision","MoreThanLucidDreams","Foundation","PepeExoticEditions"), address_remove=address_remove, address_subtract=airdrop_wetpotatobrain,address_max=1)
```

## Airdrop Gradient

``` r
c(airdrop_gradient) %>%
tally() %T>%
readr::write_csv(file="airdrop_gradient.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 10 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x248458947c120ca057ec028b3fe7e4b3f26fdb3d      1
     2 0x27b1abafb2cb065cfaf41b4b7ee95d27192151b2      1
     3 0x48464efe55fbcae8ae0c992b306afcf21d4910cf      1
     4 0x6f0735bf1e6c69030d6990cdd580345b370eb50a      1
     5 0xab2056903a7b62bac46f45a3d7a70ac799ca88cb      1
     6 0xae0d16586e5d60d334624c115216a52b9b1a0335      1
     7 0xb9cf551e73bec54332d76a7542fdacbb77bfa430      1
     8 0xbbdd72fcce73c2626719be00259ddffef0d5673d      1
     9 0xd40b63bf04a44e43fbfe5784bcf22acaab34a180      1
    10 0xe3b41ae8785e4107cc69f988042ff4a66a367fac      1

## Airdrop Memes

``` r
c(airdrop_memes) %>%
tally() %T>%
readr::write_csv(file="airdrop_memes.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 40 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x02b294ed96e1175b8c71071a303e8a7d2396b017      1
     2 0x13eee41d67b8d99e11174161d72cf8ccd194458c      1
     3 0x1b84f9899ebc7d495b70a40ec16b36847f388e7e      1
     4 0x22e8efe40ddb7f13b17b4c10f768967fc7a9f875      1
     5 0x23602ca06e977c86339ffddad74966e824ab691e      1
     6 0x248458947c120ca057ec028b3fe7e4b3f26fdb3d      1
     7 0x2801dc73a6dcefb364b959606e0c61234105fd5a      1
     8 0x32d53a21813debfcb33a2b1ff2a396bd3a06f818      1
     9 0x3fa5a25f48ba1b736761706801be4f639ca4853e      1
    10 0x40d2f4399d23f9afb82d0a6b73055f13208614f9      1
    11 0x42d38ed60a64d0b8c36f190e185d17db3617a091      1
    12 0x47dc3a7aec5c0e1f4b2c71303a4b1eaa0bee3e4c      1
    13 0x48464efe55fbcae8ae0c992b306afcf21d4910cf      1
    14 0x4e21d3cec38509cf7a3ff9ec53c32be893e9f2c8      1
    15 0x5df5342342701b8ae5bce28f74ebb73b5fc13a54      1
    16 0x5f656037e30a003862cf878db24ab5f537177fd9      1
    17 0x69cb3b1de24e08f1cfc2994171b6c6930498f750      1
    18 0x6d1db4a7e83dae0eee7e95d421722d46d2a7e94b      1
    19 0x73bcb8c5e30bf85806aade7fc36f16c6b80fd3b9      1
    20 0x7ed69ff055d746d451f675635e9bf773accc6a97      1
    21 0x82139687faae8a29851902783e02e699de0e0846      1
    22 0x90fa40f24362c24de09b1cb86df9f0b1f1b15a90      1
    23 0xa71000e72d38e6a84d7190f59fd3dfc73931c0e8      1
    24 0xab2056903a7b62bac46f45a3d7a70ac799ca88cb      1
    25 0xb0767b217fb1530b064bb1f835c57c047c08ae72      1
    26 0xb4496906d6ea2685e7a46a14baefae9fe3bf0d2f      1
    27 0xbe9998830c38910ef83e85eb33c90dd301d5516e      1
    28 0xc02e6b0d0c1a5d8cd26beeba0fe8d76c5d2f19b9      1
    29 0xc57cc3d39dbc8786f320bdb7d55ff02efdfc6d4a      1
    30 0xc78cd2e1e8ad4a288eddafb139c9d0891ad01ae7      1
    31 0xc7bb15c11595c877302ddfb330a4082d92f5bcd7      1
    32 0xc9a194bb6a3855b14dc4699cabe7b2881c3f6552      1
    33 0xcaa1c396e70384db58dd33be74b26fb119e98c3a      1
    34 0xccc9bdd130f0c03fa5d12b9a85e9e66b087457ec      1
    35 0xce8ad80ce1a979381d209ac230d02adafb9fa897      1
    36 0xd1afbe4da2a5adc6faf30fcbea9e4eea0ba8c70a      1
    37 0xd87ca052936bcc2b6283b87d2f0aa95cf0080584      1
    38 0xe7c5ef2cee9c15b04fc7dea61a1ec443dd8e7fd1      1
    39 0xeadd2d51e75a8b42302adc2aff0157a4bf626adb      1
    40 0xef5ab90a44b68d4f5e3f6be6af4bedb12cd2c66e      1

## Airdrop Artist

``` r
c(airdrop_wetpotatobrain) %>%
tally() %T>%
readr::write_csv(file="airdrop_artist.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 35 × 2
       address                                    amount
       <chr>                                       <int>
     1 0xb0bdd53b627d7e61cfc5c13ef110e47e210fac6f      2
     2 0xbe5000950bd8e26cd4494644ad27fb2378718fe9      2
     3 0xc073786a0df6b71b2c06fcf2975d024d317e3a81      2
     4 0xc4cf93e6a744effd072d14c7516780b93542f4f6      2
     5 0xe0e4ba0030fb1a7675666747dd1343efb8a032f0      2
     6 0x017347cb75ac8725608e593ea35d83f9b2b3cfb8      1
     7 0x040fc4f814321242c4e19114cfd7493bebb3b121      1
     8 0x057931155d0c6355b1bd108b0fbfe9bc77d59ef0      1
     9 0x19989297b5641de9ceac8a0ca63ac384cf3a633d      1
    10 0x2f8cb25737f469a3479dbf3cedf428a3d9900d39      1
    11 0x37dfaa62f0fe5e6ebe990ef1a0722f5279962e37      1
    12 0x403089b876128b97012aee31e3f91b4f6cfebede      1
    13 0x496e2d69b25c130c6ff71b27c5beee5613eb1dca      1
    14 0x4dd2a681d52f783ca9e317bfb9c6b1f52acdc619      1
    15 0x4decba7ff1da83cc1a8e6530f2603b18860f5a1f      1
    16 0x5068893a404e022a4cbcfa26d17e37efdc4b8c56      1
    17 0x549f9a40e3a919ad58b3ac054d30e6ef8fa91f40      1
    18 0x6b8f5e3aa817dac35d211342819fc60d99e5f0fe      1
    19 0x8264e9e0f4cbcbbbb3f8ecaec0a625b590ae790e      1
    20 0x84af839e3b2a2ba48b99da7ad663b0e519561332      1
    21 0x85cc3c8da85612013acabe9b5d954d578860b3c1      1
    22 0x91364516d3cad16e1666261dbdbb39c881dbe9ee      1
    23 0x9e3b70524b34cc65084ac04ac23c2efadb1463b5      1
    24 0xa20416801ac2eacf2372e825b4a90ef52490c2bb      1
    25 0xa7cafd18dd8bc1e23203058d66f89f0f0ee539d9      1
    26 0xb633497a4791f8460fe42ea9b85de41eb813c421      1
    27 0xb7ffa8403e08e84db4305c169818917cf8791d8d      1
    28 0xbb8fafa8a629c4dce022d95e098ccccee1acd942      1
    29 0xc4293f52633b3603e65e9b4c2b4df40eeecca91c      1
    30 0xc72f40a397453051349f73cf2e2a04fac06e37a3      1
    31 0xcddb35f12845511e4aa5d624eb7378220701ea4e      1
    32 0xcebc5f213fcdadd3a4c88e8a186e105aa5b33a4f      1
    33 0xe174750d3a85f8d7ddae9d509761bf30b06e0062      1
    34 0xe38c9820ebac03792213a95f6886b88b66190f95      1
    35 0xeb43b5597e3bde0b0c03ee6731ba7c0247e1581e      1

## Allow 6529

``` r
c(allow_gradient, allow_raw, allow_singles) %>%
tally() %T>%
readr::write_csv(file="allow_6529.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 68 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0a98f97e89743406a8611e1b4219a073b60ffed3      2
     2 0xb5374cac6cad6b025246f19d20b0d4151b640558      2
     3 0x04df8d02f912d34fef12a1b0488ee56fd6f7416c      1
     4 0x0ca4dc095608c6b153019f734106581c85f2dcc3      1
     5 0x129c6695dfe7a906bd8fda202d26dfff601f83a4      1
     6 0x1566ae673ae80725bcce901b486c336e6acef465      1
     7 0x21b9c7556376efdbf9ee316a4ede0c8257fb7533      1
     8 0x22fbaa2dd20b848084545515fb04b9c846942779      1
     9 0x28b8d4f7516a112e2e2fd462293a1c27cde327a7      1
    10 0x2924196a2ec71ea4ae1b1357381eccdcee6c18f2      1
    11 0x2c8875f34ceb219f61b7453b2c5f100ec2f6ed33      1
    12 0x2f2242b447a54cb110b8a7991cf8a27054ee6921      1
    13 0x32d53a21813debfcb33a2b1ff2a396bd3a06f818      1
    14 0x3511ae23ee25e2b97dce883c7d496ff5d18f1dfa      1
    15 0x3cb63b82d778105e43f064ed739b0655f1f0fb87      1
    16 0x40d2f4399d23f9afb82d0a6b73055f13208614f9      1
    17 0x43b0bd27d5016e969193412933387b0dd4cf3e0a      1
    18 0x45855a3f4404aa08ffe14a366c75663f4ded2fac      1
    19 0x477ea7a022e51b6eb0dcb6d802fb5f0cfc3b4a81      1
    20 0x520e8d6c5b2dbbe62bf8a40653a314d879b19b86      1
    21 0x52cc2c8db59411310e130b47d478472d9f7e4597      1
    22 0x53006f95def268f88dc1b8216654ab56f3afd052      1
    23 0x575f6540c16a72696c14a17fa64f049992d661ab      1
    24 0x619db8e961f9b20b208da059a9156ef85e5cfd05      1
    25 0x61d9d9cc8c3203dab7100ea79ced77587201c990      1
    26 0x64f7de90dc79d775703bbec66a1591c7a26a22f0      1
    27 0x665654f2d2a151be2d0f8e3697e6ce780f732af2      1
    28 0x670a840848eba22eb2a090fd65e1ff60c3ba9e5b      1
    29 0x699990a8e7ada9e92c932d6e8fb365024fc74b43      1
    30 0x69e68074f1aada957edd39c5eae0069973343f30      1
    31 0x6d1db4a7e83dae0eee7e95d421722d46d2a7e94b      1
    32 0x6f113b0c8c7266d7514e4986e3d6aaf013b6754f      1
    33 0x729fcbb6e1289c88ce5113ec1c83a48a8e3c9f2f      1
    34 0x73bcb8c5e30bf85806aade7fc36f16c6b80fd3b9      1
    35 0x7546c60ae8d65dc6dd7a0f61c169818059ef49db      1
    36 0x76db02500f7631d57bc2dcdca9d4cf782b99e119      1
    37 0x82139687faae8a29851902783e02e699de0e0846      1
    38 0x896b94f4f27f12369698c302e2049cae86936bbb      1
    39 0x8ba68cfe71550efc8988d81d040473709b7f9218      1
    40 0x9224bbb4e0fbe2f2f8fab55debc41eb21fdfb804      1
    41 0x982d3c5223f6b5794fccb3208eb164d042cf2526      1
    42 0x9dbd781eeba135ad2a779926880adc89196a3265      1
    43 0x9dc5b2cee788e3e9c67489b5d7ce634dbf48a32e      1
    44 0xa32f90b21d11561d31ff604745907acc77fb67e3      1
    45 0xa71000e72d38e6a84d7190f59fd3dfc73931c0e8      1
    46 0xa743c8c57c425b84cb2ed18c6b9ae3ad21629cb5      1
    47 0xae72c6a6fad9fa9d82d089e1ebf73b3043855425      1
    48 0xb6b4a02dca517564eb98790ff67d42b5b37a3d4e      1
    49 0xb6cf25f5cf8a1e1727d988facdd47f1dfc492caf      1
    50 0xb735af7ae1a77d1ec764c862c2c09bdbf2b34b27      1
    51 0xba4575ea27041d99e6614ec02318f1e23a623fe2      1
    52 0xbba3ced54477c12fdf16d7009771affc7a8c9ba1      1
    53 0xbf270918afe2ad16093ddce904fc358ad337cefa      1
    54 0xc02e6b0d0c1a5d8cd26beeba0fe8d76c5d2f19b9      1
    55 0xc2419841dcb9a0f8906d06463ae24e00e1470846      1
    56 0xc26012491b9dfb2e6f2cb0305673e212721d5950      1
    57 0xc762b1081c56b3fa487c7372f7284d9558a84859      1
    58 0xcaa1c396e70384db58dd33be74b26fb119e98c3a      1
    59 0xcabd5f77ca9d48f4ef9793f20de42ad39ce93979      1
    60 0xd3e401814d1faa8ca0419ecccbfee93ac7b15b31      1
    61 0xdb561a899557404581e6180fe6d4178577dc117b      1
    62 0xe359ab04cec41ac8c62bc5016c10c749c7de5480      1
    63 0xea39c551834d07ee2ee87f1ceff843c308e089af      1
    64 0xee958e45f3464d712b8830deb5875c8ac105f698      1
    65 0xef5ab90a44b68d4f5e3f6be6af4bedb12cd2c66e      1
    66 0xf15a6b54e68884d27e1bebb1624d70c227b7d04b      1
    67 0xf2c5f1fd977dbd6de9d04bc4e62dff722d4bb1a1      1
    68 0xfd22004806a6846ea67ad883356be810f0428793      1

## Allow Artist

``` r
c(allow_wetpotatobrain) %>%
tally() %T>%
readr::write_csv(file="allow_artist.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 318 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x01688743f81b5dee8e09146e42f82af42e412a82      1
      2 0x04872d18b6fb92535cd1bc6027e5f1afc0a092ad      1
      3 0x04cba99bf19958470d03be77fd1936c1ac73784d      1
      4 0x0578aa0595e0c83530dcde1d7a339479bfe6b0b7      1
      5 0x06262d8c9452f61b50c189d85a9439ae950bbcaf      1
      6 0x06e13bd0a3cba08e61028f2326b9ea2ca2539900      1
      7 0x07ec7860f405fc7b6d6ce4a6409a019e6c073ec7      1
      8 0x087f6883133b0070e5d60d74b0820c7be4f69391      1
      9 0x092cd1a6d222a167f5d0767e6444c8b45c92cc72      1
     10 0x0da1593b61973dc8eba3b29da378cff5911d46b8      1
     11 0x0dd67314002e5d56e3a966b29fc4c09e343dc3d9      1
     12 0x0e719677cb5679ff07858f58bfd6fe2a8234863c      1
     13 0x0ee378b323239b83eddb63ff2a63a107d4594744      1
     14 0x0f0eae91990140c560d4156db4f00c854dc8f09e      1
     15 0x0ff165d9420fa705c8803ac1ec09725d887fb74b      1
     16 0x107752288b215467de25c16119787d715ec8e26e      1
     17 0x10f9a56ca8fb48997bf7ecc282521c2a05d4e54d      1
     18 0x1182bcf5e3de426230bfb9de899f9532f33a4bc3      1
     19 0x13622c1340b952abbd45947d0403b63d32d6057c      1
     20 0x15ab7dd2261026db55623d0da3946a3e022be19f      1
     21 0x161827e6ca07bf5b527a29bc3845aff27b805b1f      1
     22 0x16824b24339571ac13e54c898e26344c0df84b66      1
     23 0x16a40e20a53f6e587d36812f393295fdc3affc8a      1
     24 0x16ac8b495a93abc9239515ffe7486973ff3f02c8      1
     25 0x1726510192461c6d9df244fa3420fd66ab5aec95      1
     26 0x17fccee77ea267079a3e3d83bf1abcca104d4edb      1
     27 0x18194a7c958ac751c3fdd11afe4aa0a4bbde49a2      1
     28 0x18d9b1e9459d1d7e53d102393e3b27bad5011b26      1
     29 0x193959c6cd6a6b36f150b173383260cfeefc59f6      1
     30 0x193ac8ecb86c04292ed3f2afa04faa4f384a0dae      1
     31 0x19a02cf028ecebcb9102746aff95bff088833bd4      1
     32 0x1a60dfb071b039c6e33dcb3220891c83da72c1be      1
     33 0x1a7ab10712652fec06e439b410ff91fb354bd9d9      1
     34 0x1ad1c9ce861a072f843eab9c070a52e295ad94b5      1
     35 0x1d3dd4cc14c9bc93bc8744559be4ace760f7a1d3      1
     36 0x1e299dc52eb9fdab6a6849f9731a948d8d72e474      1
     37 0x1e6260a1e9f93d499a4c5804cf84e7723274ac2a      1
     38 0x1e8e749b2b578e181ca01962e9448006772b24a2      1
     39 0x1e94b256c7b0b07c7c0aed932d12f03034c601ab      1
     40 0x1fd9277f03bd18f740bffbf9f160f10bd8fa9f11      1
     41 0x2054662f1808b0b4d29355f3856ae7286c01553c      1
     42 0x2112134feff010321a1692f3bda6c9373552306f      1
     43 0x21bd63ffdb854270fa7dcce749f73120faee0c0e      1
     44 0x21ed7503e135feafb5328320b0eb7144da5d3a60      1
     45 0x22098012d15b4ff9a2b1da8d658dae8e5d0d3fbb      1
     46 0x23e709a657ec46f6817646c386f89e993c9a6828      1
     47 0x249a49d3201c1b92a1029aab1bc76a6ca8f5fff0      1
     48 0x249e16a08396618bfd45b02aa4eea28ad8397873      1
     49 0x284fdd26d51b2fba3db0eb36f40617cb11a6bccf      1
     50 0x2b3be96652fd3b76193ed3336dc5a652a508d7a7      1
     51 0x2d17a7381c3a7af82e3bcf02173df197ae3fea4f      1
     52 0x2dba88cb3b435f99a3e58b6e0fe450e8f1a3f20f      1
     53 0x2df96030e8dbd0e7e76ccaf110cbf808e561fa10      1
     54 0x2eb61fdca7d65ebb377130d061ee57e7b9a0ce53      1
     55 0x2fb5bf565ca66786bf2c2b3f7f47b5e0c650768c      1
     56 0x31aac01ca971b7df70cff05c213a6450ca323f80      1
     57 0x36be14c0d731d70333e4f906a0d33ef65182fd0d      1
     58 0x36e10f7bb46a7e8a9000e20ac6bdc20721d15f1a      1
     59 0x370c4fafd8b9a621d01df979866c5259a2f74211      1
     60 0x3734c5ff3db6378fd4fed019ec6ff347350a17c3      1
     61 0x394853ea40ef9a4aa1e272d07bb39c5defcaccc5      1
     62 0x3a78a990dcfe1fa140701cb4a02c7b9d8c3f3e9e      1
     63 0x3ac2cc26fbb230e704393dc79e97cca47e709247      1
     64 0x3b65c687dae049e562d923fa4801a85f6a3b51fd      1
     65 0x3d81a716c76c30b6e2f272461bf9c844aee7469f      1
     66 0x3ea7d29a645168e7d6e75a3a3a0d43e3299178cf      1
     67 0x3fc145ea0fa4290ff0304c689f78dc8fc69788f7      1
     68 0x40be9456baf0cbb6b12ac7deab9353de3478e9d9      1
     69 0x40e3314167746d6e23d4a8e19d8ad484b763c641      1
     70 0x4188c4cd0d7cd95fe54e8c55ccd2d057b9d01980      1
     71 0x423232b60d97d4a989a47ee5fefd47c8dc4b0e1d      1
     72 0x426f6d19d35d7044488fba52eb2aa0fdbabc7c4b      1
     73 0x42bee6cbd9567035a741581e878bb04207744506      1
     74 0x44306c23fc2ea97da18c349042dc62f504923a63      1
     75 0x45f1e7eee9dc2dba7d85f2ec146632dc8db8ce85      1
     76 0x467bb605c50f8f815bdeef5c03ad82e40335d185      1
     77 0x469b786bd2416eb6eb832741f2fd536f60a355d3      1
     78 0x478454599df852e11524083605457875e50ceea8      1
     79 0x47a366519eaf385804a404697eb8d5cb1fd55d8b      1
     80 0x47dc3a7aec5c0e1f4b2c71303a4b1eaa0bee3e4c      1
     81 0x483388c064fdcd036ad9df3807332023fb1cbbe7      1
     82 0x488ed54ca0ae6d64683286278be99e9c3a9c203e      1
     83 0x48b9088fb80b98c0ca8979df674d95524064192c      1
     84 0x48d26fbd89dda10c72cd8dc7b82b9eee0af6b48d      1
     85 0x495c2501392afffa14d737e117685f7d9c09c011      1
     86 0x495f2bd811c9761754658b7525a511a143ce8e33      1
     87 0x4a39ae58b605102913ac19b7c071da75b55b2674      1
     88 0x4c05b3dbdfda75df0f2560bebfa7e0d857819bea      1
     89 0x4c7ba20a0ea2af75c1bdca17bc953e81c5b88746      1
     90 0x4ccc0aa065a37a3589e2db6e165d2f8f522e9fa2      1
     91 0x4df24218795b4648fec3a2b5b3ac6b71f24f15c8      1
     92 0x4fef654560d6ad788f4b35a5cd02ed185c12fbbf      1
     93 0x52ec38b43947ade943adab4a1727e87d0d908c92      1
     94 0x52edaadb86ca9a7b9647a6ed257509c3e203bd66      1
     95 0x52f0d5805045a14579bbdcd719e0086c5ef3574d      1
     96 0x543bb72cd85f36daa40c6b1a0dafb984dadc4dd7      1
     97 0x5458c15ff220bf75b92a76229fae88f93df9c6c6      1
     98 0x5467aa7f330356e7628a7a16d4bc1c5891081c4e      1
     99 0x549cb7e8eb4b4caecf046e7770759b9a0fd8c025      1
    100 0x550f21b6ce42a3e1338d7f85f8c8775749087390      1
    101 0x564a8e13d7dd23d5525160d204165bdbcb69b4db      1
    102 0x5965e97b230264c34be1657956b7ebbf9af74569      1
    103 0x598ce158e94a87ebef2c6865d37b63dac4312c06      1
    104 0x5a384561c30d1af127c5cb52919cebaf159461db      1
    105 0x5a5572de021ff38cba5493d00cdada28e9514277      1
    106 0x5af5d005f956e448dab7f00430e6f6d6ea45209c      1
    107 0x5b41f660a464631e61fdd1689151796c5f514a08      1
    108 0x5c5b6cade3f45fca78aac5a9877f1b73c51300e3      1
    109 0x5e4bea43eb527650686f219ab682865493850edb      1
    110 0x5ed592557b6a29908b7d50305ca01904a4529cf4      1
    111 0x5f2ada0a1cfc9dd8996cf4be20b83edc9d88a5cf      1
    112 0x5f804c9a49045dc7f50a580231a2e71fba49badd      1
    113 0x609e1de29d5e543a6eba15b73d9ed335e90004b3      1
    114 0x6140f00e4ff3936702e68744f2b5978885464cbb      1
    115 0x61937e0b1814c0d259247fdded8068cc15e7ac6b      1
    116 0x62f7b0272aebd56a74cef22d23ba2fe9eb087a89      1
    117 0x6351ca30d72f41f65af21a7a48c8e79163a9dea7      1
    118 0x63529946bd3d913d34e2ab8a1a0932a93dc9c334      1
    119 0x6357aea44d2967c73bde29a865a865f4d1af2f91      1
    120 0x645fd16f3508391e8385fc0a181076994079aa1b      1
    121 0x6954ee7ede83ca555663704fe7e2b1528ff2c619      1
    122 0x69a949d2330bfac2fdbee90ccfa9923343326b19      1
    123 0x69cb3b1de24e08f1cfc2994171b6c6930498f750      1
    124 0x6a6743b85899cc2571c8034e209f74f5a92a0e9b      1
    125 0x6af7812fc932da2d34738a33f295de54011fd16b      1
    126 0x6b0846740da0a586745e1dd3d8e2058c467e3069      1
    127 0x6b4b60cd239b5b7947c98bb2b9bbd01db5bdf44c      1
    128 0x6deaebcc104e8f2fb0248fa6c44978bad249348f      1
    129 0x6e63a4caeccb4f341ee9c9175c9cc554bdb6d10b      1
    130 0x6f3f44440680cad37c8ef621f1800664febc44e8      1
    131 0x7084edd9e348ea8631e68fb8ed349fea6081d87b      1
    132 0x70f729e89eabacacf11293e40a700524112514d3      1
    133 0x718bbea45df291d0a9481c8c397ce69b8f2d7631      1
    134 0x71aa6c4e87225fcae2df49f977c7fc0d1e8d2112      1
    135 0x71e7b94490837ccaf45f9f6c7c20a3e17bbeb7d3      1
    136 0x73a6a002b9538f636fbfe6bfb8b7440b8b3a510e      1
    137 0x750a1f7347368471f8f5c446a243eef8df1dcdb5      1
    138 0x753f8dab2ce1e6ec33e2967bcdd4974f60025319      1
    139 0x757a21fbd39cccaff681e1930e273b9add0008db      1
    140 0x777f76af924eddb428c6a2c1e1d3460d89cdfa61      1
    141 0x78c89c18426dc625b9450ea8001d61bb8da46f0d      1
    142 0x7b0b5a326aa4d95968e88654af9b87624383a549      1
    143 0x7b47503ae6c88040328863c70ba71be322765ca5      1
    144 0x7baf191cfaa4a0b08991fb728179baaf3917836a      1
    145 0x7c03c6fc64293c460be652f4015ad232e695ac8e      1
    146 0x7ee75e6855647adc71b7b70d6d55d96521da4104      1
    147 0x7f27de8cb92173446ebc93b526d1f13af8e27050      1
    148 0x81845ec7df4fa5776998053457b8c4fb1e60cf84      1
    149 0x820880c9040041d6a514e069ec0a2f37997325d4      1
    150 0x828d97f928264bfd94208949a836402d91d61168      1
    151 0x82abb5df200d2998db4e80639b145d052ca40062      1
    152 0x8379ac05ef71f982d497f392dc1a14ee859efd95      1
    153 0x837b24455dc3796b1a91707741c8162fbb71c5d6      1
    154 0x840b490438d9ca67a3dad2cc9ec678232f19cd29      1
    155 0x8473e7450e77518dd85f7b15a54e1144df85aad9      1
    156 0x84b10c201fc4bf72f6cabb0d44621cee11d576b5      1
    157 0x84e538f54929dac593f2efc032926cfd0a72da3c      1
    158 0x857e694a9805808cc1e53dda2c928e1a39be3b7d      1
    159 0x87fe0acd8388af44755e4110546002541b03aca3      1
    160 0x8888888888e9997e64793849389a8faf5e8e547c      1
    161 0x88d3574660711e03196af8a96f268697590000fa      1
    162 0x8a4b89d76a1a745a4a1adebd3793253fba0adadc      1
    163 0x8a8932e0b45067d08bcf8c9b62fe87fd424695cb      1
    164 0x8bf930215c52d7068bdf14599296b00dc114aa1a      1
    165 0x8d1b58bff12419f55636805f45861054ec0f2f62      1
    166 0x8df9719d29aacac7e46db46974f58ad953352c42      1
    167 0x8ee376de530fb9a734df676e7e4342b48355f483      1
    168 0x904935e7b85844202769a15274fb45c0915c6693      1
    169 0x90eb4da64323dfa6fc9b5f73d57b2d35367732c7      1
    170 0x92fde24b25e7d0dd83abea05bf925e47044d649e      1
    171 0x93c838add3ac72b42f65417408f900a7155e1bb8      1
    172 0x9481d8bc76fb1b7d2f95733a6ec0b40d0cb1fffc      1
    173 0x94b768b0582bc6a16ae3d6f14e6660cdf50fdad8      1
    174 0x950b4bc31b54bab713902198968a9092f61c8638      1
    175 0x956c8b59f7062e2938a36a2f6858fe6913e4735a      1
    176 0x96fd61202a698ee3eac21e247a6b209ea5ffeb91      1
    177 0x973a76745727f86c45d71efdb656d0069f5ef68f      1
    178 0x9766959d8fd4b1fd2801583a95d8f6fdbccc86bf      1
    179 0x97a6c796fe543cabc2ca7ae026206e8b260c4da0      1
    180 0x988c838af5a5b4ffd9b319d8382dd0935c387ca0      1
    181 0x98949eaa1189181b765e5e6b8eb854484c8ea059      1
    182 0x98bdfe7174ed431da22604aaf87d66fe2bd8621e      1
    183 0x9a5cbdb8e1c95f6eed643b7b8c653ff53f5de39f      1
    184 0x9aa91eeed8e34d7ed2145d651f76fae3e15371d3      1
    185 0x9bae2c333d3a242645d7c7c0430f0b9cedf372d0      1
    186 0x9c163eb3589b0dcf366fc23c581307618b68fb06      1
    187 0x9d2fd63c49509e82c7abe56d864deabf41ff4a1e      1
    188 0xa0f1de4882a5cd1851989d9a1e9bbe9b4604e9a9      1
    189 0xa24111399b765325e151f5f4cd11197cf4afa9b2      1
    190 0xa266e8b5c812c6a9d63b8862599f2963d19a797e      1
    191 0xa27ec45fa707acadc0909a88a0ecaf2d08ba9375      1
    192 0xa4c8d9e4ec5f2831701a81389465498b83f9457d      1
    193 0xa4de5931b992d26651b29b351be8303f7d1875f1      1
    194 0xa529fd37113edc2fb86934eca596a72da21b7d5f      1
    195 0xa5ab0bd1ad257c107123383cbcf642ba395cea49      1
    196 0xa646746968e343cc1fe703ee0f7b72e3358df867      1
    197 0xa69f1ccf023007165dbf15b6ce96a6ce043e10a2      1
    198 0xa6ac0d9aaaac73e948905cf0ed7d8d34f7f7e435      1
    199 0xa6f4fa9840aa6825446c820af6d5eccf9f78ba05      1
    200 0xa7eed8cae4a19ada808c4fa1e374c1fbea960013      1
    201 0xa8608ec96722149566d5dbbec820063452609f81      1
    202 0xa9a8fdc2945b781a54ebd6663b4c785a0ebbd867      1
    203 0xaa986ef5ac55bacf72046e2884dc6fbfe3332f18      1
    204 0xab4273e7137f9531fcb47d558d9bab0e726e6937      1
    205 0xaebb1ac559b767b848ea9645e86dfc99c8d0a69a      1
    206 0xaee4c04c8ab29fbc397ea89dff2a81558e5772cf      1
    207 0xb11171351e638cb4b7c49ec52f9fda3ea1f363e4      1
    208 0xb3343e623d7a6860881ed9b178ecb22796812396      1
    209 0xb347dd642e9b20be829abd986a3c16d3e5cf1ffc      1
    210 0xb88f54e560fed19f20e4848f0695fe609a9181f7      1
    211 0xb908327da7eae1b0e2a9b9737c7b3dfe4769154e      1
    212 0xb9e8e3135b87a55e25febc411d2450ea63476bc6      1
    213 0xba7933402348a902064499ed883c49843eeb7019      1
    214 0xbae58357f500b2d886df1a9e141083e0c6771f2d      1
    215 0xbaea3cf94abd0d6e0f029ef5b0e54e9424a72985      1
    216 0xbbc37f68e9876d64b2c55016081528ae0a85d8b2      1
    217 0xbbd69dbccc9948e08659e1102884c1e996554786      1
    218 0xbd3a5d12af9fd82fb927fd26216e4dc2722d2337      1
    219 0xbe12e889a5af67b12369c5610187a18c0a43dce0      1
    220 0xbf2a403e23c4877dd966102eb221378eae3a9fef      1
    221 0xbffab99f686e522c7dba4578cc5ee2eb9920d5fc      1
    222 0xc02222a078d1706994e47292266239aabea7fa8b      1
    223 0xc043e6549a9572b13196b4ac153355dbb10cdd2d      1
    224 0xc0f9e8de7ce5ad40979de08b6b746ba47ca608cc      1
    225 0xc11f42d7b2ff88bc32da5d06bc4b7467ac2b80aa      1
    226 0xc171a9d4dda66330c41b6dec0a6b1dc640b2b26d      1
    227 0xc18d4c2fee42accfe7bfdce98bfacbabf76242e9      1
    228 0xc24ac1ca7c0044b86143263f6e48636ed2f27ddc      1
    229 0xc2baa32b57a91b81d35ef3052319873bbd16cd12      1
    230 0xc2d742fdbb31442b843a098dcec3c20054c28986      1
    231 0xc34acde66e516a019517a9967af10d31e4e9dae9      1
    232 0xc390024160c454f230cdf3b6f04a5eb930a5b30b      1
    233 0xc449f005667bef849261b35accf931a4bace48fb      1
    234 0xc4b02bae10902a783e0b6490a00554b2cbf863fd      1
    235 0xc4d0af96a1c580fbc9e967776a1666c27ab933e4      1
    236 0xc5301c828f3041cda5ec6b669502b6f9291c7c8d      1
    237 0xc611376086b5152a78070d89290d0a21e8d17c17      1
    238 0xc68c7771ec6a6e5d67d62aa9c6f22df69865e401      1
    239 0xc85b3cbf81e63e0524717efc4e53dc12658b1fc5      1
    240 0xc8f42217b6b1768012b4431f922be0759fed7397      1
    241 0xc957cd969dcea3494213cff980f43d5a66638e9e      1
    242 0xca7bb986cadf1bf2f6ba69924afab35e0c2285cd      1
    243 0xcb1dd0f59fdf86658016827c78b858dfafa39ccc      1
    244 0xcc3d6ac59cf8ed1f5528cad140266c9ce3d3c41b      1
    245 0xcc48857242141cc7f4f8555f8a3d5bed03556c19      1
    246 0xcd30b6de9dadb0a1598eb12a4ccf34b98c98c1df      1
    247 0xcd486efddbca4d4ac9504f4272a7148376b36e94      1
    248 0xceae4ca7d082856d140f3672780029d6e90c3dcd      1
    249 0xceb7fe5d4565f1e5412ff67a820c5fb37a08f9ca      1
    250 0xcf04c33bfea64cf5dd6ea97e2d5620f825b03b49      1
    251 0xcf0596ffe4b4b982aee1eafe38fb782c72e46f7b      1
    252 0xd297fa47bd30988358fd6a6c8feddccf957dca28      1
    253 0xd37905283adf21d3e628b423cbe20c4583ba9979      1
    254 0xd59120f2a160c9862301c1f3bb5f171279f95512      1
    255 0xd5c196b5b67e624738d85ab5a98534e68fd2c88e      1
    256 0xd5d4aafb3b2217607e5b5b5526eb6932f8df130f      1
    257 0xd66bf38e7c704f0cd7910773f5c8cc3098b6828d      1
    258 0xd7076acc76d4379610fba0dddb1d35aa1280fc89      1
    259 0xd716ad4f10eade5af4ed10006678f49bd2e6624f      1
    260 0xd722a299d14a19b56cddaeae0e1d5a5786424570      1
    261 0xd736a9d02d60ea9e7de1f7532bb22fe0b20f3d34      1
    262 0xd79a9865f5866760b77d7f82e35316662dec6793      1
    263 0xd905de91f928b20ea0ccf455e5ba07eb6373955c      1
    264 0xd9317a031dd755bcc19d64210231fea9c5b21ce4      1
    265 0xda1480828d9efeb694da1809b1bb251cf5eb7441      1
    266 0xda4ff7b6a1eb3176c347a3ffc56a26ee82fb6893      1
    267 0xdae2d80e803e7e7bc279309ede4e039788b4936d      1
    268 0xdb54c320a0b1e994d2bf7dd2ec939f6c25918011      1
    269 0xdbb643740ce08ad124dd5ca747845fe798f12624      1
    270 0xdbfd836c989e1fe9586cb0d1bfb35e7849be23a5      1
    271 0xdd0757db3121427af1267d0eb8caa9858c6dbc39      1
    272 0xdd44fabe409a25d9e02361a0f1025299f25f7a43      1
    273 0xdd762af79fbbc73b51941fdd1fef8e89101eb51b      1
    274 0xde5de41265dada8e9a0ad5493a33b86e6648150f      1
    275 0xde77a9e6249476ae451fcb8085c668ebff3963b0      1
    276 0xde89db35d4aaa281904faccf48033a66003ea50d      1
    277 0xdf36b093cbc06522b68105c3d9141db188e980b6      1
    278 0xdf38a8b16f9bb88083c1f1edad41c5323d60f8a8      1
    279 0xdf98322a314921fc6863806dae62a157081b0a5a      1
    280 0xdfcafc3b9dc57ee71486e840b3c56b8abc22f6d9      1
    281 0xe0c6c14d4f954e18652837801709ce41706e67e8      1
    282 0xe1d23eddf8f0f597a8582d86586999ffc41bb030      1
    283 0xe1e2e153e785118fa7b50e93e6b8cf887f025bc7      1
    284 0xe1e5591c71d76e4e42013808e772a41010626143      1
    285 0xe39bd9e3af2054dd193b0d0217cfbd460f104902      1
    286 0xe487ed5ec80cca6e8a44f6d6f3f712088df8ad4f      1
    287 0xe539a21fbdf04c289c167c3ff2f97754080c235f      1
    288 0xe74ca58ef724b94fac7f2e09e2e48536a5c1ad03      1
    289 0xe781fe7a6f65ee6b3efe66ed8f7f5c1e9f01cc55      1
    290 0xe884814df58e9745278dcf8003f9baec572fdca9      1
    291 0xe8c9333d6ba367ebd7ed1afd83088b5b5ea15d84      1
    292 0xe974e75da812e3764dcc746c8fa82e25fb9f0627      1
    293 0xec789b00d35451937c0919a4fb718954bea94e1d      1
    294 0xeca2444e8672ae3de62eb816be0f0e1f4bd03443      1
    295 0xedaa218b391646d0702717a8d2d9a6653ce367eb      1
    296 0xedec3a7d41be809df55225c2c8069bf2e57c79f9      1
    297 0xee91d62eb5aaea933efbfd0790613af5db305006      1
    298 0xeeb45fcf74fb92a3822515987a5b519d909bfbb9      1
    299 0xf0aa926ede618fa4771aee8f60e96859ccdccb16      1
    300 0xf1e1c701b49b1dc2405a9e8ef40f9f88802b80fa      1
    301 0xf26f2f6f86cf3e8832a07da6f053a66a7a45697d      1
    302 0xf3fbb2c0a711529ef9b81fe59a5ef5b8f1e0eb27      1
    303 0xf43917c2cd1c189437a3af4f6dd8afb2746c62f4      1
    304 0xf4c40bf7070fdcf64ecf020bcb583738a6cc3bcd      1
    305 0xf51aeb2ee5233f92ca009141fd99a98cdb21e1ba      1
    306 0xf599a05b4ea846a5afa1db274b508bb1ba1ddd93      1
    307 0xf74aa398050f15c7c58750ce71cf0be5440ba47a      1
    308 0xf8f327e4c1cbcb0ef014031b84069b9d3579f42d      1
    309 0xf907665e210ddeac7d75042ca95b4da712f52b2f      1
    310 0xfaad7f84881162239bc18ead2f3bb687a6dcd1e0      1
    311 0xfb39a75697b375daba7bf3b109c31c4a271abad6      1
    312 0xfb65beae7dc7112cda6a5eabc29e1f449eb8c3f2      1
    313 0xfbc2e15e898a614a8927c979a5b051c453fa7ebe      1
    314 0xfdb6dae2791921aeb1aaadfd0ce766225029e3e5      1
    315 0xfe2b8dc957459ac2a08fef57576bcb8a89845d8b      1
    316 0xfea6145aec3832672df239639fbc1db273ace241      1
    317 0xfefc2850b25fe1aa3e9aa154df61c6b9bc7aa374      1
    318 0xffb6d97bd1e7b7bd08595096d15037401a1f416b      1

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
