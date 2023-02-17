
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "16639869.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance          contract        
     Length:46085       Length:46085       Min.   :  1.000   Length:46085      
     Class :character   Class :character   1st Qu.:  1.000   Class :character  
     Mode  :character   Mode  :character   Median :  1.000   Mode  :character  
                                           Mean   :  1.255                     
                                           3rd Qu.:  1.000                     
                                           Max.   :652.000                     
         name          
     Length:46085      
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 16640169 # https://etherscan.io/block/16640169
block_hash <- "0x3068dda183905ceb0bdabec972fef743bbf3b2598de7883c1e7e2348b556c096"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4764 

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

airdrop_gradient  <- pick(snapshot, contracts=c("gradient"), address_pick=6,address_max=1)
airdrop_brendan    <- pick(snapshot, contracts=c("SuperRare","FromNothing","BrendanNorthOriginals","PaintedPoetry","BurningThoughts"), address_remove=address_remove,address_max=1)

allow_gradient    <- pick(snapshot, contracts=c("gradient"),address_subtract=airdrop_gradient, address_max=1)
allow_raw         <- pick(snapshot, contracts=c("raw"), address_remove=address_remove,address_max=1)
allow_singles     <- pick(snapshot, contracts=c("65291/1s"), address_remove=address_remove,address_max=1)
allow_brendan      <- pick(snapshot, contracts=c("GoldenFutureEditions","SignsoftheTimesEditions","LightsCameraActionEditions","BrendanNorthEditions","PressureEditions"), address_remove=address_remove, address_subtract=airdrop_brendan,address_max=1)
```

## Airdrop Gradient

``` r
c(airdrop_gradient) %>%
tally() %T>%
readr::write_csv(file="airdrop_gradient.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 6 × 2
      address                                    amount
      <chr>                                       <int>
    1 0x0a98f97e89743406a8611e1b4219a073b60ffed3      1
    2 0x40d2f4399d23f9afb82d0a6b73055f13208614f9      1
    3 0x520e8d6c5b2dbbe62bf8a40653a314d879b19b86      1
    4 0x82139687faae8a29851902783e02e699de0e0846      1
    5 0x982d3c5223f6b5794fccb3208eb164d042cf2526      1
    6 0xa743c8c57c425b84cb2ed18c6b9ae3ad21629cb5      1

## Airdrop Artist

``` r
c(airdrop_brendan) %>%
tally() %T>%
readr::write_csv(file="airdrop_artist.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 59 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x08e0a3e752d1338c0f02b986b9504f1009a962ca      1
     2 0x1b7844cfae4c823ac6389855d47106a70c84f067      1
     3 0x1de4d49959eafa01ab3706aa20707a3f1dbe2424      1
     4 0x1fdf89dd0eba85603cbde7f9f5ce5d830ffc7643      1
     5 0x21301d901db04724597d1b6012ac49878157580d      1
     6 0x23fed2634fdc629c1a82eb6ed3391efad2a58670      1
     7 0x259ec06af57875eaaf3e3254abc3bb16b57c5336      1
     8 0x280676491188f56fa386d9833d84702ac1e24c71      1
     9 0x2c90d7a5e09dbfe0c017ed2106b428fca7686c62      1
    10 0x2dba88cb3b435f99a3e58b6e0fe450e8f1a3f20f      1
    11 0x2f44fb58135ae5d3793803c73d471c7cde4bb774      1
    12 0x324f6ead99aeda8b0083a4445e8df08a342e22e1      1
    13 0x32606c4aa95ea78d7c5b1e0e347e3e9ae451434e      1
    14 0x3392da275706938613d08777d1f36db1e1c27730      1
    15 0x39d725ca6d9a84926597f3d4e887101eee3b7b13      1
    16 0x3b0aa499cc6acde1d4a7433da6968d7bb8bd8509      1
    17 0x3b22621a2b0e1f8dd3056975efc2150d5fbee980      1
    18 0x3cd378c9b1cb5f147ebf1b2c2564118946ae4ba1      1
    19 0x40d775827365ae4d54cbc08a1a1c4f586b2c1d0a      1
    20 0x424060738dc814b16c203650ba7e1ecffc7e504e      1
    21 0x45f2e08b35a959a87833c57c4bacbf58f95eefb7      1
    22 0x484a12a27e8c95718e0b60d14369bfeaa426aff3      1
    23 0x4f36fc5b7d34fe98fc81e5a8e2c76312a4d53865      1
    24 0x56bdc5fe7f9752e7f5a381e394acfe72a724462b      1
    25 0x596558fd287175afa7ff695f565f65e38547b0e6      1
    26 0x5c4fe960950ba0e09a72869c3d51fe70f07580e0      1
    27 0x6582a0fae4931833aa1f663785da13cbfcb90ab6      1
    28 0x6c9726afbf60be7a1fcdb5730a6c522b4f578fb1      1
    29 0x716eb921f3b346d2c5749b5380dc740d359055d7      1
    30 0x74619cfc7ec552e5051c181f49428ccd154256f7      1
    31 0x8497277c9339170a0420e86da8352e0c084624cd      1
    32 0x862464bbd83d5743f9cf873c6ea3d187d0f909bf      1
    33 0x8d1919f9f69911b0c6630703f1ab5da3d3faf1ca      1
    34 0x94de7e2c73529ebf3206aa3459e699fbcdfcd49b      1
    35 0x96d2b5bfa179ce0979fef08ca7ecec497b050b75      1
    36 0x97c0dbcb62f0579b34d2223e1b22a029f352513d      1
    37 0x9971bfed7e71b338cc46383adc84625c0688f471      1
    38 0x9c163eb3589b0dcf366fc23c581307618b68fb06      1
    39 0xa4b61e227361a9cd9e62ca10946c27748a382cab      1
    40 0xa4d8d8979961b865a120da059c88f2e0e0c5469a      1
    41 0xad0fa0c1ab907fde977113fe17a0670e87e73e50      1
    42 0xb359f72d2772930c12ea752bbeb8ad5233b67ab2      1
    43 0xbb34d08c67373e54f066e6e7e6c77846ff7d2714      1
    44 0xc3f4728f673181e20862d21d028cfeaacfb6d409      1
    45 0xc55b7614fdc6f11d649d03420262f91db1181be5      1
    46 0xc5b35696bd01ed06cc78ceb93f64cd8ebb5870af      1
    47 0xc704389a3f999f325d568df1ab676d4b750f7c37      1
    48 0xcc5de602a32469f0e32c8aa01757793746bd11a1      1
    49 0xd038d146dfab6b92df8ce2c92369f09375fc5b32      1
    50 0xd7f47f4932f65c2bc1650e6f10218a0528543437      1
    51 0xe1c9b7038a03dc898cd161827709a3c241af991e      1
    52 0xe628d7135485ffae3c1dd9b07e9cbe3815fecb74      1
    53 0xe8c4156a19951deff3203f116fc63274da746baa      1
    54 0xe9903bf790657bf13e6945c16bd87a293b905143      1
    55 0xead90127d60f78f3e97781938418004be1794eae      1
    56 0xf1e1c701b49b1dc2405a9e8ef40f9f88802b80fa      1
    57 0xf4cdfea7fdf85872141e4f8eb0e953427de7626f      1
    58 0xfdc4364c13e2c892de20ab2acb9a80534de493fc      1
    59 0xfe4b4481ecf8a0c7c3376c578384158c02dfa64b      1

## Allow Gradient

``` r
c(allow_gradient) %>%
tally() %T>%
readr::write_csv(file="allow_gradient.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 73 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x04df8d02f912d34fef12a1b0488ee56fd6f7416c      1
     2 0x0b3e3a27d85996e20b2cdd0e7f1e1208816d05b8      1
     3 0x0ca4dc095608c6b153019f734106581c85f2dcc3      1
     4 0x129c6695dfe7a906bd8fda202d26dfff601f83a4      1
     5 0x134cb213549c9f114176adb9e4a78d6a272a3672      1
     6 0x1566ae673ae80725bcce901b486c336e6acef465      1
     7 0x22fbaa2dd20b848084545515fb04b9c846942779      1
     8 0x248458947c120ca057ec028b3fe7e4b3f26fdb3d      1
     9 0x27b1abafb2cb065cfaf41b4b7ee95d27192151b2      1
    10 0x2924196a2ec71ea4ae1b1357381eccdcee6c18f2      1
    11 0x2947cd7346b3a372b357256b8955ebd4b5735987      1
    12 0x2f2242b447a54cb110b8a7991cf8a27054ee6921      1
    13 0x3511ae23ee25e2b97dce883c7d496ff5d18f1dfa      1
    14 0x3cb63b82d778105e43f064ed739b0655f1f0fb87      1
    15 0x3d0a1d20ac580ca8fa35888c92dd0859887f3398      1
    16 0x400e904b5071bc41d0c69aeaa5c5de34bf83cae4      1
    17 0x43b0bd27d5016e969193412933387b0dd4cf3e0a      1
    18 0x477ea7a022e51b6eb0dcb6d802fb5f0cfc3b4a81      1
    19 0x48464efe55fbcae8ae0c992b306afcf21d4910cf      1
    20 0x53006f95def268f88dc1b8216654ab56f3afd052      1
    21 0x53562e8ecee54356dc89ad263a444528b34d6c80      1
    22 0x575f6540c16a72696c14a17fa64f049992d661ab      1
    23 0x59068075a799594db03c0255eed68e8e121155c8      1
    24 0x5fdb5fdb61bc2975f3be446c5ae9c5df490f55d2      1
    25 0x60d2bc84a71af61b82d1b4168c9103ff6c4898eb      1
    26 0x615502edd5cf2d59244dd0dd53d3c8ec035e172b      1
    27 0x61d9d9cc8c3203dab7100ea79ced77587201c990      1
    28 0x64f7de90dc79d775703bbec66a1591c7a26a22f0      1
    29 0x665654f2d2a151be2d0f8e3697e6ce780f732af2      1
    30 0x670a840848eba22eb2a090fd65e1ff60c3ba9e5b      1
    31 0x699990a8e7ada9e92c932d6e8fb365024fc74b43      1
    32 0x69e68074f1aada957edd39c5eae0069973343f30      1
    33 0x6f0735bf1e6c69030d6990cdd580345b370eb50a      1
    34 0x6f113b0c8c7266d7514e4986e3d6aaf013b6754f      1
    35 0x729fcbb6e1289c88ce5113ec1c83a48a8e3c9f2f      1
    36 0x73bcb8c5e30bf85806aade7fc36f16c6b80fd3b9      1
    37 0x75005a05bc163b85991b9c9facbcf3155372422e      1
    38 0x78a576d3751e8cc8f30c54c367fe8e5a2cd71228      1
    39 0x7e5ab36876a267560e7191cedbe99ee7bc04bc30      1
    40 0x85603a042455d2f36f3ec52c1c756344aa70c5ef      1
    41 0x896b94f4f27f12369698c302e2049cae86936bbb      1
    42 0x8ba68cfe71550efc8988d81d040473709b7f9218      1
    43 0x8bd9c4b5ea8f6f14c9b14d830ddb67f3720d77f6      1
    44 0x8fc2326375174bfe3baccc98cb9906921dc9599c      1
    45 0x9224bbb4e0fbe2f2f8fab55debc41eb21fdfb804      1
    46 0xa56c04347abee42f663eff9bc2d0147b97c8f782      1
    47 0xab2056903a7b62bac46f45a3d7a70ac799ca88cb      1
    48 0xae0d16586e5d60d334624c115216a52b9b1a0335      1
    49 0xae72c6a6fad9fa9d82d089e1ebf73b3043855425      1
    50 0xb6cf25f5cf8a1e1727d988facdd47f1dfc492caf      1
    51 0xb735af7ae1a77d1ec764c862c2c09bdbf2b34b27      1
    52 0xba4575ea27041d99e6614ec02318f1e23a623fe2      1
    53 0xbbdd72fcce73c2626719be00259ddffef0d5673d      1
    54 0xbf270918afe2ad16093ddce904fc358ad337cefa      1
    55 0xbf814810b44978de273191fd612aa47f7b69d564      1
    56 0xc02e6b0d0c1a5d8cd26beeba0fe8d76c5d2f19b9      1
    57 0xc2419841dcb9a0f8906d06463ae24e00e1470846      1
    58 0xc6400a5584db71e41b0e5dfbdc769b54b91256cd      1
    59 0xcaa1c396e70384db58dd33be74b26fb119e98c3a      1
    60 0xcabd5f77ca9d48f4ef9793f20de42ad39ce93979      1
    61 0xd007058e9b58e74c33c6bf6fbcd38baab813cbb6      1
    62 0xd3e401814d1faa8ca0419ecccbfee93ac7b15b31      1
    63 0xde112b77a1a72c726d8c914c23dceaa53273c351      1
    64 0xe359ab04cec41ac8c62bc5016c10c749c7de5480      1
    65 0xe3b41ae8785e4107cc69f988042ff4a66a367fac      1
    66 0xea39c551834d07ee2ee87f1ceff843c308e089af      1
    67 0xee958e45f3464d712b8830deb5875c8ac105f698      1
    68 0xef5ab90a44b68d4f5e3f6be6af4bedb12cd2c66e      1
    69 0xf15a6b54e68884d27e1bebb1624d70c227b7d04b      1
    70 0xf2c5f1fd977dbd6de9d04bc4e62dff722d4bb1a1      1
    71 0xf8aace471723dba6514f6c6d43651611a0c34529      1
    72 0xfcbca52b23db28470eb97680d18e988d8b60b608      1
    73 0xfd22004806a6846ea67ad883356be810f0428793      1

## Allow 6529

``` r
c(allow_raw, allow_singles) %>%
tally() %T>%
readr::write_csv(file="allow_6529.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 4 × 2
      address                                    amount
      <chr>                                       <int>
    1 0x13eac939d3c7ff1a985d164d1f14411505b4c822      1
    2 0x2c8875f34ceb219f61b7453b2c5f100ec2f6ed33      1
    3 0x9274f2f89fbe5dc00c21c628e46a18c7187c14d7      1
    4 0x9dc5b2cee788e3e9c67489b5d7ce634dbf48a32e      1

## Allow Artist

``` r
c(allow_brendan) %>%
tally() %T>%
readr::write_csv(file="allow_artist.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 318 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x00653d03b147a663390dd5c5b1a55dc9cc0bd2fd      1
      2 0x00eb780d4246d5784a1a217801ac31c6aa9bd6f9      1
      3 0x017347cb75ac8725608e593ea35d83f9b2b3cfb8      1
      4 0x0188d86fe6ec9955cac0e5ee5b9d99eb355ee093      1
      5 0x019c2cb0e8559f0d9f5d7c68289b54a457ebbbfa      1
      6 0x01aa144771677c746af575258b3489cd25bd744c      1
      7 0x021d5abea6efbcd5dba2c8ae9237471448ea0856      1
      8 0x02bebb44a4fe9eb0c0aa0205a2494394b32d3534      1
      9 0x032ab14d5f69020afc87f43cacc0ef182a750d1c      1
     10 0x036403fbfe5e7349f09cb1b2275e4a652ea565d4      1
     11 0x039ec0b36450e9b2c5f59a3a6fe991469ac744f2      1
     12 0x03bfcbc27fc7636bd742fece03980bb7cb853dc4      1
     13 0x04490fb053a8ff110bea35f22d955c0092aae5f8      1
     14 0x04c95c6f8ea7ff7c3f7a2f148fae75650c5a875e      1
     15 0x0535c5a30b2a4699bead00215c21016608801575      1
     16 0x056b6ab6fc678bf53678ef7f6544662ba39c03ef      1
     17 0x057931155d0c6355b1bd108b0fbfe9bc77d59ef0      1
     18 0x05ad39f902d5d0f4a61003550c36682d1826a883      1
     19 0x06205d36b9fd8cd543fbf68fcc6aa0959a0de47e      1
     20 0x070585c79183b97183e34a6a37e9e1281cb34baf      1
     21 0x071489b287c20c413f28a17729b1c954bc01d5b8      1
     22 0x07781960a5d4e92f5903e9b88ecde7c17928a3cb      1
     23 0x08792a1765c94268b112f398e035e6255275dc39      1
     24 0x092f8828636a2b8061ff963f397c66de13f5928b      1
     25 0x0952255cb2f1ad756a3de77353011b1e7c3bd49c      1
     26 0x0a986ef1ae29459e09e926e1906cc443dd723381      1
     27 0x0a9fa26394df45c08e21a0d30f8a86ffc668ed63      1
     28 0x0b5f5c5ee41f49dfa674f501b2a45a3449458d51      1
     29 0x0b8edc5d3b491aa703f446e28d20f407f093c13b      1
     30 0x0bda9d14503a46565c679019337f712bfea0781f      1
     31 0x0bf988a6cc20af0cdd6f583ad2fcf057895888e6      1
     32 0x0d255dcbcc218bb3f0ddb7a02f49ae606f448721      1
     33 0x0dcee254468df83cfe9dfb2236bf253459cdb079      1
     34 0x0dd273b85e5578c488559107ac98a1ab98f208aa      1
     35 0x10d8f7259b0bdcbf73cf965e95640d4b9276b543      1
     36 0x1273b62a0e80ed80ffd46c119555eca9787fa37f      1
     37 0x13a9cf094c4f51fbe37665509b03e96c4c6993a0      1
     38 0x143dbf2a697896e1691e575cd17f76d36408c0e1      1
     39 0x14ef20167cf5c090aa1948d19e8c9efc8e833ce5      1
     40 0x15a33852846b4762d71a62928aa7c64d8aac8544      1
     41 0x162f3a373a8605fd0da52b1b6d44dc8bcc97f0aa      1
     42 0x16e9cfe34541d8601da1fab1707fe2b1b867887f      1
     43 0x17ae58ab79444ad5b8ee2e232caf13c65c32af75      1
     44 0x18612bd34a12f14fd465862ee452c5413753ed17      1
     45 0x198e363e2e7d58f521960e4175a7dfe0f59936f2      1
     46 0x1a4b7409e020140443a02ef26858a7e53ac7d652      1
     47 0x1a73306100f9f27e153ef1044be99bfba644e7d2      1
     48 0x1a962d6a95cea8060b1e9f55c5bc6622315c0108      1
     49 0x1aae1bc09e1785c8cb00650db0c6015bd73b0c7e      1
     50 0x1c7471f01889137b32258b8e96b365f3e79a7b07      1
     51 0x1d4e2d4b00f644c410f9f905908607f7c526a281      1
     52 0x1e342d8cd201ce9d9c14f3a015c8dae0f41e40fa      1
     53 0x1e3b6cd74d67421f01c588ca0a22809dc34a4c13      1
     54 0x1f08411e1493ef7c4e0c39b0dcb38495dba0fd33      1
     55 0x1f92fc15bcbeb4dd24eefbead8d7aee409f891dc      1
     56 0x1fc30deba76628e93197f438e107bcef51842d6f      1
     57 0x20345c3a4afa7a35f917068dd3def812c367f051      1
     58 0x208b4a8ef875a5e4645e23f27343f47fd887d9c5      1
     59 0x21745e9cec72c9aca26e7ea44ee2c4d21a5a1c5f      1
     60 0x23fbb0ed3da4f6355581e96569ccc8691d6fb9f2      1
     61 0x2571d0a68b9bbd0d45125784cf776dd14592bb7e      1
     62 0x2924cf0e0a91e0f8fbdb384e12ac5a5e97da4d55      1
     63 0x2a26fb1a180a2ce5c6c2d06dc4d430c37e7ecf39      1
     64 0x2c7ba77e6f1d25ee8f4905d4cd6934e47ba19bca      1
     65 0x2d17a7381c3a7af82e3bcf02173df197ae3fea4f      1
     66 0x2ee0485f71764bcd2062a84d9455688c581b90f8      1
     67 0x2f4ad85154233feda792df976a6ef79052c313a5      1
     68 0x3098c04b75c7fecba9c7a80fc24c097995e95681      1
     69 0x3272c414dddb7d86f0d12ae0d064c67740a7f583      1
     70 0x32c9a253f8753a24bce6c23627c3c787b9a79b5a      1
     71 0x330bce303d27df0eb1b856da3464278db1db1ac5      1
     72 0x34671b61d994e5cd51e5c4e4b4e7cc5ab7f09156      1
     73 0x34954c245258a61c484b28c367f8e6bd4fcf000d      1
     74 0x34dea787524a930670862a27bc7234a1897e1bf2      1
     75 0x35a8a215c6bfeccb5eb52a4f875305a38e77d32c      1
     76 0x363885776a224b03cf449de8dbf53860a9e8a80c      1
     77 0x36799ff7bf99602870b91572e2a897b06c91c87b      1
     78 0x36af69fa35c61841349f5bd3f09efe94e5cebbb9      1
     79 0x36c488771b5ee5485f83d8b9e51ebf26cc587f28      1
     80 0x373a33f0602caaa908695003b759482e7d5ee110      1
     81 0x374670a2f40043987fd31a89f3d4acf9b7d4ae7f      1
     82 0x375961e2ffd0b8b77527db297a22270c26a7e46d      1
     83 0x3805e9b956aca92899ef0c6faa7dfad20b5c5dac      1
     84 0x3807155076b24d98f4e716a968416dc5d7c11b7a      1
     85 0x385dc7d2ba88318c3a22de8cbe7b7b0ff54df842      1
     86 0x3876be5be4998adecbfbbad26604a762467e7f42      1
     87 0x388160a99390392278afdba240046b8b5e73f77b      1
     88 0x38c48591a1865e7824f9ce270c46d9cbd329c9f5      1
     89 0x393e408ec32d4493817b39f91f5a0b16e3f248a9      1
     90 0x395f265ef5c8674b12e5c96bc76cd29ced17b1a8      1
     91 0x3982ebe7d6a37789afee81b7768b5f81683f6465      1
     92 0x3a02ed3cb2e8ab814bf99804412b9982049079c5      1
     93 0x3a943f529de8461697702fd9bb8fcda5d9a8ce61      1
     94 0x3b8f0ee14a29a1194e0f0be0d65e4b1327e52b25      1
     95 0x3db0b224099e56488d93f4e887a6bad88c39cfa4      1
     96 0x3e64014d2d3f0c3a3b0404110cea9060b4565e61      1
     97 0x403afdf9ea925d3b48e719a44610da1679a57651      1
     98 0x404437b4644fe2fc2cc5293f74fa6cf3daa61d77      1
     99 0x4161cb28b929e90c284cd280daddfc5cfc76188e      1
    100 0x42b46951ae492fde820016ad6432c1360a227dd1      1
    101 0x46105de145ceff841edc1382f3e9d6836ae4a609      1
    102 0x48238faa9bd6d56fb44a32dac6185f04eabf2117      1
    103 0x48ffbbefc6609716aab7a5f25939449fe9e0077e      1
    104 0x497b5af00112f42f57b2496bde21123b1d4f85d6      1
    105 0x49c1c6683a54d962c8f2d9d36d05ef107e9033ea      1
    106 0x4b43ed9bfa49663480d8a71e7bb5edce659dbec7      1
    107 0x4b70222d33ec85c4cf182e42258a8f5ec7a9a6c2      1
    108 0x4cc2b3075c3cc735958a50ce1b3dd00173e97b04      1
    109 0x4ccc0aa065a37a3589e2db6e165d2f8f522e9fa2      1
    110 0x4d255d9b31aa437a0df8d02565c99d0faa6ed49c      1
    111 0x4d394709010193bce1e60725c8595e723215429a      1
    112 0x500f865f35049f32119c038d21a5b962f2f20e96      1
    113 0x50d21e44c3b8cbc9959de95e5738fad25d5a1df4      1
    114 0x514afe6575b5d81cecaa86a6bddf28de2f89ba45      1
    115 0x519e815db12bff4e28c41753d8f72485d55a9b40      1
    116 0x51cb87167c04abfa132a67a037b50b5dc4d978e8      1
    117 0x52349a2c4ea4e4b94ada3d75c9d3a318c024706f      1
    118 0x5467aa7f330356e7628a7a16d4bc1c5891081c4e      1
    119 0x56616a6dac6252fdbbacc8ef5e739f4b26119f92      1
    120 0x56a338c2fb40269b2ff0e57ca9c5d0484ea30e95      1
    121 0x56b367a6303b3d626bfbabc625b8e0b5bd9452f8      1
    122 0x57115f7d04c16f3983802b7c5a2d5089a188d76a      1
    123 0x571d1ad0867b02f9c90c1efe64e32e8d50006a4c      1
    124 0x578b076f33c021ca8ec8873be00c734559a99057      1
    125 0x5a0bfaf68f7aab502eaa65c254f4a4234316a52d      1
    126 0x5e15052dfa1b27dfc5985d51d51dc0539c1c10ca      1
    127 0x5f75fb6104ac8ce72879347db1041adf2f7745d6      1
    128 0x600b78235d3f33bf8523881565155282431d9a93      1
    129 0x61e935bb49ee40dae181d1e795d0deb84fbaadf6      1
    130 0x631ebefd35fa59ac01e9c0514cdbcf9cb70b18bf      1
    131 0x63784c971e102bce755b140680de86404843aedb      1
    132 0x643f4c62a319a83fd08755d0263cdf67f1c4cc0e      1
    133 0x65829d271213f09e105df3ca2661c2afe47440f9      1
    134 0x668248df4595e09aa253b31478312748078f7a20      1
    135 0x6767e9cb6762b840b96322f8ef211a24e71cba7a      1
    136 0x6954ee7ede83ca555663704fe7e2b1528ff2c619      1
    137 0x6af7812fc932da2d34738a33f295de54011fd16b      1
    138 0x6b6ae848f555f70944bc99d736fe29fcaecf8b23      1
    139 0x6cf69059dd4a86987bc8a4c334b83ea8fa9f4037      1
    140 0x6d7196598358188573b75d4d41bb86c20da2e56d      1
    141 0x6d8dec3fd68d94a7189a98346ea52b4d32e00012      1
    142 0x6e15fed85e0f5918a4a8d087f25eb0c3b706e524      1
    143 0x6e76bc675e5da2be7b535823e615ec467f33f8a1      1
    144 0x6eff21c2bfdf83a33aa690fe3db17fc672244907      1
    145 0x6f2abf19629355e834a3460abd8dd07712c1abae      1
    146 0x6f398e7872ac5f75121186678c4e6e015e83c49f      1
    147 0x700cd5cc454779e5029f9a096af5accbe2af0b5b      1
    148 0x72bc237cf598505dc00d4fcc23b1663e206455a9      1
    149 0x72d037f63a6c782f85c059fc917a12e71b2e0c73      1
    150 0x74bfa6fc87e23d64451f5bb084b6fbf79f46e495      1
    151 0x75256a46e98eb0f9c4eefc197eb3dd88d559a771      1
    152 0x759c9417c83bbd9a2b536a8c0c771da1a9889b30      1
    153 0x75c5b5498181da0ff9d3618372c4fde670b1d809      1
    154 0x791f73a83a91f9cb9c0023bda2b2759ea085cb55      1
    155 0x7939d56c13a1bdea78c4efe7eeda3e395ccd65fe      1
    156 0x7b8b101f2383e0415c2254b13894b1518db7d902      1
    157 0x7bcdf4adbb79513beb05c2f725cd5a82a1978feb      1
    158 0x7c6bf4d30f09fc76334cbdb54860f573875612a4      1
    159 0x7d5300e22db1b2850da1beed10a894893731c1a7      1
    160 0x7d77f4350945113b7f07df0465f5e87f75839595      1
    161 0x7e8ba3fa5f0102004e734437f18eb2de612c3187      1
    162 0x7f79f25ad53142ee1040a04e79a910d65e7b08e5      1
    163 0x8058d889c48b7a79cfdaa54dd6d623b09c9146a3      1
    164 0x812010bbe6652497d33c7a1fd986f629769ea9cc      1
    165 0x8122f9bcd4f39a84c5de0810389e4f2187dc9e77      1
    166 0x8454dee79effe16d9fb369594eb0321230ccdd73      1
    167 0x86f63cb8b6e04e2e59a5452773408f12e2293272      1
    168 0x88ac3cea7888c92f7f04db230268b9843bc64daf      1
    169 0x88be96dc047cde01286e991f7333a2e9a4865856      1
    170 0x88c0043b3f0eb2b915a0d5bb2864f937a459dd31      1
    171 0x88d5eb1993dd04bf2175f940e64fd49a90d13f8b      1
    172 0x89270f910155c4bbc8928f9e22da8641b97ba820      1
    173 0x89d42f152a826e28c69413ec5c98e6bccb7c7abf      1
    174 0x8ad1cefb7f9675fa7756d65c4b041fdead76ff98      1
    175 0x8b4694aa1b83faedbd8406f2c41ad2ec079a5799      1
    176 0x8bbc3d0c4a70f31d6b3325e0805283e549e72ec7      1
    177 0x8c962009eb45fb6abc9f57a40a2c71098b01b6b8      1
    178 0x8dbb75c576b71b43eea54398f8606aec530181dc      1
    179 0x8f3fdd81b626e454580289c1ab6249696afa4eeb      1
    180 0x8f4f373e9c2cd1c7e1158234fb42e48ebe6b7485      1
    181 0x9065e2d64f2559845556439b22b87e41642661ea      1
    182 0x923b2d973b407c317869124362515db392148342      1
    183 0x94484f0353c37feb11da98985bd965cff7b68830      1
    184 0x946e2dd8d7fd6126b864428459e62d6e8f910051      1
    185 0x9481d8bc76fb1b7d2f95733a6ec0b40d0cb1fffc      1
    186 0x967edc401472dc8b7dc3b9e51bc66bd6477ee209      1
    187 0x98a692316057e74b9297d53a61b4916d253c9ea6      1
    188 0x98bdfe7174ed431da22604aaf87d66fe2bd8621e      1
    189 0x9bd69d51526fc9db09bb831f5069a48cbe4d3421      1
    190 0x9cc4f2332af0b6a63ba88bd790c9061271580588      1
    191 0x9d2dacbd15f1d2f14ed36c4b6e374b2d496d93ff      1
    192 0x9eb6ac015d61d8789e83b73e492d6145abb9e86c      1
    193 0x9f81a650f1db18dfa3a4ea1d6fb5ed86b46ab842      1
    194 0xa0f1de4882a5cd1851989d9a1e9bbe9b4604e9a9      1
    195 0xa25803ab86a327786bb59395fc0164d826b98298      1
    196 0xa38b03e71f0e1c97f9a923680d55982225163599      1
    197 0xa51ae1a065166163b9ea036aeb6a4526df1f0540      1
    198 0xa67077ec8f947e8299335538167f625f3e407fff      1
    199 0xa7faf09d160d777c98beef579087266f6da167c9      1
    200 0xa83fa9c834bb5abe25f9c56eb5845b3203df94f2      1
    201 0xaa6cd3f29030fec56065aeefa2937fa1dd7c8830      1
    202 0xab4273e7137f9531fcb47d558d9bab0e726e6937      1
    203 0xab5f32d395b515d132d5a1f1e3337f4f679d13be      1
    204 0xac104d56e4f8e0462f8c001ec7dde4c68deb596f      1
    205 0xacb1ffb1b45ccfebee14de887fb44babca26f068      1
    206 0xae1b60528e6b4f050af8feb69a71a77a00642a7a      1
    207 0xae26a111bd77ca028a3ada880b1ccdccb1bc9e3d      1
    208 0xaee4c04c8ab29fbc397ea89dff2a81558e5772cf      1
    209 0xaee8ba9f3a6d4b7976b0095a05cea126a04fdac7      1
    210 0xaf6a78083708cae7aead9c96a1475eb25c671fbd      1
    211 0xaf9e18cd797548220317fb459095c6afac5c33a7      1
    212 0xafd2ed18640e4faf466b3658fcb81d18da5ea3fd      1
    213 0xafe86c6a85ac6378a3f9e1101785a88c52546808      1
    214 0xb0bdd53b627d7e61cfc5c13ef110e47e210fac6f      1
    215 0xb4854ba8f41cea0dbae0ffb8f135fa2320184ca1      1
    216 0xb52343dbb87db123289ebbd6d8277ce9f1b71ca1      1
    217 0xb59ae7f12399b7fd49478952235596151706a8a0      1
    218 0xb65d28d8d260fa51ae6eeb97543ff77271ba25ee      1
    219 0xb6c7e03ede87692deb1de7e40723b952e0fd3773      1
    220 0xb70d0906ab07ae1fb9283fefcf3df7d499c0fffc      1
    221 0xb76f60231194b9626b8ab7f68069a88e55ba0473      1
    222 0xb7ef5fb53dc42839270a94ca569536a7c7e7edde      1
    223 0xb98d10d9f6d07ba283bfd21b2dfec050f9ae282a      1
    224 0xb9c416eb0d03efdce046e4f4bba26c1c3544f4c1      1
    225 0xba738302746e83700c72c532149f4f396356bdd5      1
    226 0xbab2a9dfd248499cf2292b8a5b461cfb3f7d473c      1
    227 0xbb11796a23229457790ef1af44eafe5e429a247e      1
    228 0xbb261e9c522d826886cf9d03b200e268a7ca36cb      1
    229 0xbdd8a9ac8987e9b4aac7c078bfe638b4ce9721d0      1
    230 0xbe0c0df368687e580fa2c3ad3c9412981f0273fb      1
    231 0xbe2bbc26fedbaaf98cc4857496d5b9efa7b66ffd      1
    232 0xbe356c83377eab13a4d78ca89f1bf99b54bbd792      1
    233 0xbe48873a51c40391d5aaae897e75211b0355fc46      1
    234 0xbe5686b99530680cfcb9652388e069c464fd3198      1
    235 0xbea913f95547cb42d24403f67d3e07f6f2f49562      1
    236 0xbeea79aa02534d1a7466cb49447b62308750c95e      1
    237 0xbf29f132f9570fd222b040e141d8957e24f0e7de      1
    238 0xbf44c636a4a4bbe257a87067b25c4c875170e04d      1
    239 0xbfae66905244be67431f84af82fcb04ae3899e80      1
    240 0xc15e011b8e117fba8cc241c70950fc79f515ab3e      1
    241 0xc18d4c2fee42accfe7bfdce98bfacbabf76242e9      1
    242 0xc1d040b7ebcc0a571d9e2d54e10bd04c505e7d37      1
    243 0xc2fa34a6fcb085f5e0295f233fae7fc90fbafe85      1
    244 0xc534d309f770f2eff38ef9a9534c36265cf80aa1      1
    245 0xc68c7771ec6a6e5d67d62aa9c6f22df69865e401      1
    246 0xc73d8b193b2531da67586fc8bbd1c162aa809f02      1
    247 0xc9ae113b8451d0afc8a021d796322d545a7e7dbe      1
    248 0xcc12b2364d4b3e997f7ca648037683ff0ae6da57      1
    249 0xcc21d478ed9987e3ff80e08fe35ea2145208c59d      1
    250 0xcc2ea0b8a2ddf14ad54b5e82985762d32f674816      1
    251 0xcc811ad1fd46ff65df9e6c6dc0181874e9957707      1
    252 0xcddcbd9c2665a71cd4cf5d1fac0740bf7643f260      1
    253 0xceae4ca7d082856d140f3672780029d6e90c3dcd      1
    254 0xcf04c33bfea64cf5dd6ea97e2d5620f825b03b49      1
    255 0xcf0df56e67f469f390e5c458c9debd875b6a5eb3      1
    256 0xcfa4f3a5a0cd0dc83bb16dda9e04cea743d866d0      1
    257 0xd061c30d9711dda98b4e1b174781d9cd587493f7      1
    258 0xd0f61674d6102ca71b5a41ccf774d42cfafe04de      1
    259 0xd2ba592032a550b4ce5584a44db62744fa73210e      1
    260 0xd34d42d1335ab3ce12c8dc85ebe21cf1fcdefed8      1
    261 0xd430fc849c60107201ea4f569108716a90ab65e2      1
    262 0xd47c0f2c2da1465fd47cc052941d2fbcd9191698      1
    263 0xd4a08cf067c83d1b2cc1d26831569b7850804be7      1
    264 0xd4d3c85017cbcf26ca6c89ab5767227d7777196a      1
    265 0xd59120f2a160c9862301c1f3bb5f171279f95512      1
    266 0xd75cd3dfea2d1688961e62b08b333692df0e66ec      1
    267 0xd7d16a716f6dfcef1e9553bf12136463f6414f1a      1
    268 0xd88b150fbe5a25a3da30a10c9f556d02343586e2      1
    269 0xd945a576f522c17d113bc56863abfc4542340a41      1
    270 0xda4745f0254044e4b702c9301737c2a0b8c69a6a      1
    271 0xda4ff7b6a1eb3176c347a3ffc56a26ee82fb6893      1
    272 0xda60cea9bb3fa722bc565fe8f4ecba861d6fb3fd      1
    273 0xdd0c246165c8b4a90bc5a446da9ee0908a45966f      1
    274 0xdd762af79fbbc73b51941fdd1fef8e89101eb51b      1
    275 0xdeac32a21d7430ff1a03f1b95719f89fac8a9e93      1
    276 0xdf8b134fb7743acd805ecdee11335dd0cca921fc      1
    277 0xdfbdb9b9174862ecb1010c39ca72409c1d63b18f      1
    278 0xdfe5d3dfdca4a5e7187d987babd3814ebf95b54f      1
    279 0xe08198a7726a95386117c40c637a3f375345911e      1
    280 0xe317eb46da9d27aa3493b03ea0468ffd37ccc2e1      1
    281 0xe3232db3adee21123b18cfd2db89b4f373e4af45      1
    282 0xe328318c037754ef86ffdde28d4e2acbd8da9b6a      1
    283 0xe3efd57b602dc396f7bcdcca34b03890bd53bd6d      1
    284 0xe5895a0595f13e7f37844c29cad892f5d87f4141      1
    285 0xe6aa0f8f60f8488e2f259531a48a081cdb8dd766      1
    286 0xe6ba7ce1bbd7b811d891c2d4c09d7295bb96752c      1
    287 0xe74abe3bc87a4cf34acfd7c548208700c0e669b0      1
    288 0xe793b3549bf6f293151b2378a8b098ad00c4653e      1
    289 0xe8bee2f6bf252ed9efbbd483ca4093ace7350c92      1
    290 0xe96ba1a10f72b58934b9ac0e3fb471d2ba65b757      1
    291 0xe9cb56a31a8aaaa28dfd30a037fbcf470bce1e2e      1
    292 0xe9d5a03e6b2d7644ae73a07c866966e4fd92a4b3      1
    293 0xeafc83bd210706fc3930a32dcfd8920e953c3622      1
    294 0xeafff95282ba3053ff49bfb77fb37ef30754eb62      1
    295 0xec234324276680b8de93c048dd69d68181c00356      1
    296 0xecd19550e2505d1fe3a1a96896ae4f1c08b10a44      1
    297 0xeef498f448b2efbb35017118a6013019a84892d9      1
    298 0xef6914f95fc782a5397cff1686873da77fdeca8f      1
    299 0xf00992e7849118f1e57a07dae09a416f8cc97d7b      1
    300 0xf035839daaf652f03156b297a27eda25a35e6316      1
    301 0xf05155f792819710da259c103d30e4b70178ea9f      1
    302 0xf07d19011d9f5993ea1114dc958877815605219c      1
    303 0xf1637adedeb89559c70481c3cb4f74ebac80d829      1
    304 0xf1bd046b98dc7cb13096ef4c880eb09f148a46e2      1
    305 0xf34196597df209a04fc92b97071dcaabbc63f0e8      1
    306 0xf533a9a5bddf9d79801468d7078584d94eb316cd      1
    307 0xf5eae76d733d2670909c0c9fa9bf2cf816b812ab      1
    308 0xf7008f7d415f04e74bb2460deb800608aa991c1c      1
    309 0xf7d4df98f4be31bf93a0f3175f45e7d69668d2b2      1
    310 0xf9eb827e025c1758e758e247dfc0a3d9b80b154f      1
    311 0xfafa683d093aa5c7276cc48d24cedfebd4637688      1
    312 0xfb2ec6543d97808d8a00fca199c39022b0b3368f      1
    313 0xfce39c2c26363f063f4eb95e9ab449cbfcfc5ebc      1
    314 0xfd41bef1fd45d7db65fb8f4cd3804e4c8daff6b9      1
    315 0xfde25d8e5e1e3b03f4d0b1d5c793e3916fd85bc4      1
    316 0xfed988b69efaedb04798a7ea358eb6c708394b17      1
    317 0xff33e48ad64cc05d58d51698bf6b6747584532ad      1
    318 0xff53b23729401a257da6b64f4e769a73759be6ca      1

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
