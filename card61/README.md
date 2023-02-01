
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "16513569.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance        contract        
     Length:976         Length:976         Min.   :1.000   Length:976        
     Class :character   Class :character   1st Qu.:1.000   Class :character  
     Mode  :character   Mode  :character   Median :1.000   Mode  :character  
                                           Mean   :1.002                     
                                           3rd Qu.:1.000                     
                                           Max.   :2.000                     
         name          
     Length:976        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 16513969 # https://etherscan.io/block/16513969
block_hash <- "0xa1863202244be10a9ccd61c7ab8471d9c44113ee0a81c70318df06bd5f8e0a48"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4575 

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

airdrop_gradient  <- pick(snapshot, contracts=c("gradient"), address_pick=4,address_max=1)
airdrop_memes     <- pick(snapshot, contracts=c("memes"),address_remove=address_remove, address_pick=26,address_max=1)
airdrop_tjo    <- pick(snapshot, contracts=c("SuperRare","Foundation","TJOINTERACTIVEARTWORKS","TJOGIFTS","KnownOrigin","lombredeBLeU"), address_remove=address_remove,address_max=1)

allow_gradient    <- pick(snapshot, contracts=c("gradient"),address_subtract=airdrop_gradient, address_max=1)
allow_raw         <- pick(snapshot, contracts=c("raw"), address_remove=address_remove,address_max=1)
allow_singles     <- pick(snapshot, contracts=c("65291/1s"), address_remove=address_remove,address_max=1)
allow_tjo      <- pick(snapshot, contracts=c("KnownOriginEditions","TJOsEditions","LaisseMoiEditions","onenightonlyEditions","onenightonlyburnEditions","NobleGallery","Decal","Grails"), address_remove=address_remove, address_subtract=airdrop_tjo,address_pick=300,address_max=1)

allow_tjo_phase2 <- pick(snapshot, contracts=c("KnownOriginEditions","TJOsEditions","LaisseMoiEditions","onenightonlyEditions","onenightonlyburnEditions","NobleGallery","Decal","Grails"), address_remove=address_remove, address_subtract=allow_tjo,address_pick=200,address_max=1)
```

## Airdrop Gradient

``` r
c(airdrop_gradient) %>%
tally() %T>%
readr::write_csv(file="airdrop_gradient.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 4 × 2
      address                                    amount
      <chr>                                       <int>
    1 0x477ea7a022e51b6eb0dcb6d802fb5f0cfc3b4a81      1
    2 0x699990a8e7ada9e92c932d6e8fb365024fc74b43      1
    3 0x896b94f4f27f12369698c302e2049cae86936bbb      1
    4 0xd007058e9b58e74c33c6bf6fbcd38baab813cbb6      1

## Airdrop Memes

``` r
c(airdrop_memes) %>%
tally() %T>%
readr::write_csv(file="airdrop_memes.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 26 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x195f2cd867c1c02ae6e61a14ec53f5f01b440edc      1
     2 0x19bce10bdd129d80c210a01826f1ad4dd1ed8273      1
     3 0x1dcfdcb24456a7dba5342479cff0f9ff30145fb8      1
     4 0x20ebf9ae7c66da763fc1ce86062ce883389c8e23      1
     5 0x22e8efe40ddb7f13b17b4c10f768967fc7a9f875      1
     6 0x2801dc73a6dcefb364b959606e0c61234105fd5a      1
     7 0x3a10fd1387555cd75db8142393fbaba98efe28d4      1
     8 0x405020c797a64f155c9966c88e5c677b2dbca5ab      1
     9 0x527bb834cc5c8ff730c673880e51372282b06e14      1
    10 0x64ca4373a93e25822dcfdca0306e899a29e92b3c      1
    11 0x77acac99ac831a1574b9db4d15299e98e195e6ae      1
    12 0x80a1c9fdc26199a69d190ebc8ad287ef48758977      1
    13 0x97d2b1b4a249ec77d56fd1576546996d14f7db1a      1
    14 0xadebdeab678647a457743ea3af98f8b804e45c24      1
    15 0xbbc37f68e9876d64b2c55016081528ae0a85d8b2      1
    16 0xbc1eb4359ab755af079f6ef77e3faac465e53eda      1
    17 0xc1966d218b7492dd9cd672a5a237cef4d82004e5      1
    18 0xc457fee0564bf3eb972114762f62ab45c0e3590b      1
    19 0xc522289168311a765cf17c067f0118578c99cf08      1
    20 0xc78cd2e1e8ad4a288eddafb139c9d0891ad01ae7      1
    21 0xc7bb15c11595c877302ddfb330a4082d92f5bcd7      1
    22 0xcaf3365d474690a1ac6643d3d6ef44cb0c6deec4      1
    23 0xd193a7d7b231c204b76b9d638768ea602de515f6      1
    24 0xd3069c9e486a8b77add55ae3ceb3a4b138fb76c7      1
    25 0xd4f358d4a415b4abb6f6deb40b86d7db62562960      1
    26 0xd72c8dd33954e8aa53d5c108906b751ce4b2382b      1

## Airdrop Artist

``` r
c(airdrop_tjo) %>%
tally() %T>%
readr::write_csv(file="airdrop_artist.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 26 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0256c28028cac3e504f1b6c20bf7b3f4631e0d97      1
     2 0x04b5a01e011352624245a5150d7f36e754ef8b62      1
     3 0x1c2530c60db8339639d17a05657c005812e7af77      1
     4 0x1d4b9b250b1bd41daa35d94bf9204ec1b0494ee3      1
     5 0x205a46012bc3c1a2583fdd426679dff276fb257e      1
     6 0x2fe042e25cc3de10629095335817d57e8d9f56d5      1
     7 0x30cf9db393b40b16c62acb9da863a60082df0b82      1
     8 0x31c72d0d03adc76456d3c14a4fe19f8aa8307c16      1
     9 0x3612b2e93b49f6c797066ca8c38b7f522b32c7cb      1
    10 0x3deed956b999b83361b85bff31d388c35125411d      1
    11 0x42689518d72a0fdbc34cfe30aa36d89ed8beb5bb      1
    12 0x484a12a27e8c95718e0b60d14369bfeaa426aff3      1
    13 0x4b4d75b8b0fc7528ea2614b2ca83555824c07867      1
    14 0x665654f2d2a151be2d0f8e3697e6ce780f732af2      1
    15 0x7f86d25f106a32a25567885aee9873f461775617      1
    16 0x886c4d1d4bc99399a42ef9cdcbf0f0dfa700a860      1
    17 0x89119dac068cfc98bf2ffb7d15948e0901d997dc      1
    18 0x8cfec99f4f85ddd8eccc6fcacc27bd75710fa129      1
    19 0xa1168968174d3a4f2b6666051800115d86fdc235      1
    20 0xa7dcc417c63f24f9073b667a5d7149bd38463d0f      1
    21 0xba0995c52474dfb7c48181f71454a63a2847982c      1
    22 0xc449f005667bef849261b35accf931a4bace48fb      1
    23 0xd01fa4427a05fd7944557ae94b67d90109fad402      1
    24 0xd0ed643099041b805adec2e2af3329c06a20ffe6      1
    25 0xd83e662b83880081741d0e810c3fe8630fa6cb4d      1
    26 0xffb6d97bd1e7b7bd08595096d15037401a1f416b      1

## Allow Gradient

``` r
c(allow_gradient) %>%
tally() %T>%
readr::write_csv(file="allow_gradient.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 75 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x04df8d02f912d34fef12a1b0488ee56fd6f7416c      1
     2 0x0a98f97e89743406a8611e1b4219a073b60ffed3      1
     3 0x0b3e3a27d85996e20b2cdd0e7f1e1208816d05b8      1
     4 0x0ca4dc095608c6b153019f734106581c85f2dcc3      1
     5 0x129c6695dfe7a906bd8fda202d26dfff601f83a4      1
     6 0x134cb213549c9f114176adb9e4a78d6a272a3672      1
     7 0x1566ae673ae80725bcce901b486c336e6acef465      1
     8 0x16ecafb3b5d8e15d07bf8d3ff3a3f9ab16cda860      1
     9 0x22fbaa2dd20b848084545515fb04b9c846942779      1
    10 0x248458947c120ca057ec028b3fe7e4b3f26fdb3d      1
    11 0x27b1abafb2cb065cfaf41b4b7ee95d27192151b2      1
    12 0x2924196a2ec71ea4ae1b1357381eccdcee6c18f2      1
    13 0x2947cd7346b3a372b357256b8955ebd4b5735987      1
    14 0x2f2242b447a54cb110b8a7991cf8a27054ee6921      1
    15 0x3511ae23ee25e2b97dce883c7d496ff5d18f1dfa      1
    16 0x3cb63b82d778105e43f064ed739b0655f1f0fb87      1
    17 0x3d0a1d20ac580ca8fa35888c92dd0859887f3398      1
    18 0x400e904b5071bc41d0c69aeaa5c5de34bf83cae4      1
    19 0x40d2f4399d23f9afb82d0a6b73055f13208614f9      1
    20 0x43b0bd27d5016e969193412933387b0dd4cf3e0a      1
    21 0x48464efe55fbcae8ae0c992b306afcf21d4910cf      1
    22 0x520e8d6c5b2dbbe62bf8a40653a314d879b19b86      1
    23 0x53006f95def268f88dc1b8216654ab56f3afd052      1
    24 0x53562e8ecee54356dc89ad263a444528b34d6c80      1
    25 0x575f6540c16a72696c14a17fa64f049992d661ab      1
    26 0x5fdb5fdb61bc2975f3be446c5ae9c5df490f55d2      1
    27 0x60d2bc84a71af61b82d1b4168c9103ff6c4898eb      1
    28 0x61d9d9cc8c3203dab7100ea79ced77587201c990      1
    29 0x64f7de90dc79d775703bbec66a1591c7a26a22f0      1
    30 0x665654f2d2a151be2d0f8e3697e6ce780f732af2      1
    31 0x670a840848eba22eb2a090fd65e1ff60c3ba9e5b      1
    32 0x69e68074f1aada957edd39c5eae0069973343f30      1
    33 0x6f0735bf1e6c69030d6990cdd580345b370eb50a      1
    34 0x6f113b0c8c7266d7514e4986e3d6aaf013b6754f      1
    35 0x71677455ef1479a96596cb7fb894d16dbe6e792a      1
    36 0x729fcbb6e1289c88ce5113ec1c83a48a8e3c9f2f      1
    37 0x73bcb8c5e30bf85806aade7fc36f16c6b80fd3b9      1
    38 0x75005a05bc163b85991b9c9facbcf3155372422e      1
    39 0x78a576d3751e8cc8f30c54c367fe8e5a2cd71228      1
    40 0x7e5ab36876a267560e7191cedbe99ee7bc04bc30      1
    41 0x82139687faae8a29851902783e02e699de0e0846      1
    42 0x85603a042455d2f36f3ec52c1c756344aa70c5ef      1
    43 0x8ba68cfe71550efc8988d81d040473709b7f9218      1
    44 0x8bd9c4b5ea8f6f14c9b14d830ddb67f3720d77f6      1
    45 0x8fc2326375174bfe3baccc98cb9906921dc9599c      1
    46 0x9224bbb4e0fbe2f2f8fab55debc41eb21fdfb804      1
    47 0x982d3c5223f6b5794fccb3208eb164d042cf2526      1
    48 0xa56c04347abee42f663eff9bc2d0147b97c8f782      1
    49 0xa743c8c57c425b84cb2ed18c6b9ae3ad21629cb5      1
    50 0xab2056903a7b62bac46f45a3d7a70ac799ca88cb      1
    51 0xae0d16586e5d60d334624c115216a52b9b1a0335      1
    52 0xae72c6a6fad9fa9d82d089e1ebf73b3043855425      1
    53 0xaee4bdcf9d164d9adbbcbfd846623fbe133a6018      1
    54 0xb6cf25f5cf8a1e1727d988facdd47f1dfc492caf      1
    55 0xb735af7ae1a77d1ec764c862c2c09bdbf2b34b27      1
    56 0xba4575ea27041d99e6614ec02318f1e23a623fe2      1
    57 0xbbdd72fcce73c2626719be00259ddffef0d5673d      1
    58 0xbf270918afe2ad16093ddce904fc358ad337cefa      1
    59 0xbf814810b44978de273191fd612aa47f7b69d564      1
    60 0xc02e6b0d0c1a5d8cd26beeba0fe8d76c5d2f19b9      1
    61 0xc2419841dcb9a0f8906d06463ae24e00e1470846      1
    62 0xc6400a5584db71e41b0e5dfbdc769b54b91256cd      1
    63 0xcaa1c396e70384db58dd33be74b26fb119e98c3a      1
    64 0xcabd5f77ca9d48f4ef9793f20de42ad39ce93979      1
    65 0xde112b77a1a72c726d8c914c23dceaa53273c351      1
    66 0xe359ab04cec41ac8c62bc5016c10c749c7de5480      1
    67 0xe3b41ae8785e4107cc69f988042ff4a66a367fac      1
    68 0xea39c551834d07ee2ee87f1ceff843c308e089af      1
    69 0xee958e45f3464d712b8830deb5875c8ac105f698      1
    70 0xef5ab90a44b68d4f5e3f6be6af4bedb12cd2c66e      1
    71 0xf15a6b54e68884d27e1bebb1624d70c227b7d04b      1
    72 0xf2c5f1fd977dbd6de9d04bc4e62dff722d4bb1a1      1
    73 0xf8aace471723dba6514f6c6d43651611a0c34529      1
    74 0xfcbca52b23db28470eb97680d18e988d8b60b608      1
    75 0xfd22004806a6846ea67ad883356be810f0428793      1

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
    4 0xd3e401814d1faa8ca0419ecccbfee93ac7b15b31      1

## Allow Artist

``` r
c(allow_tjo) %>%
tally() %T>%
readr::write_csv(file="allow_artist.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 300 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x0040122ed305e0b16dfc9fd8e0e9a74a3cdd5509      1
      2 0x00575eff47d93b33423bfa8de93ba84b5c512136      1
      3 0x00c130ba24fa93373b4addc20fc71d17fe5dc4c5      1
      4 0x00ff192363430a35abbf968c535b64147e88abdb      1
      5 0x01b22bb2d473a53fae501c273e6d5592a58a131a      1
      6 0x022b266d1aa39725a4e64aee661f0277461e4e39      1
      7 0x027cae2ed1a23350a751452e907b4120330f9762      1
      8 0x04c95c6f8ea7ff7c3f7a2f148fae75650c5a875e      1
      9 0x05feef6f10fe8890b82c9bf5065d58fb9b9cb284      1
     10 0x0743882fa09a0c257b18d53dc1b101d3f32a04e5      1
     11 0x094a6d641d57edd154ede9e44dfdda9b5184ccc7      1
     12 0x09b2f0988b6d24336b348c84c2764d2b4f85142f      1
     13 0x0a9bc3b0d19bca983cd9a6d251dc5920e87b223e      1
     14 0x0c71db5adab6631b8511862ab923b27fe331b25e      1
     15 0x0ca3a3b144148642f68b88bded521ce637db8f3e      1
     16 0x0d2657935cc0721b5e4f1d2255c01c1b9c6d1cba      1
     17 0x0e3c735708f660b9d29b1f05679c13f72e14c0ea      1
     18 0x0fb83cd9543ac6f7b8a4e16792d5cf1e242f0486      1
     19 0x108289ecb4830e7de08dc8f9e58526ddeccd2d32      1
     20 0x116723a14c7ee8ac3647d37bb5e9f193c29b3489      1
     21 0x119fcd292b95551e8af472fc4721ac34cc5f48e5      1
     22 0x11a22b262e505d355f975e1e48a365b5d4811ae0      1
     23 0x12d4e572cf6b44196c6327ff0eb879ca547f5c34      1
     24 0x131cfa6b38a8022a0d569bce6247eebc7dd6a65e      1
     25 0x13a1db3301fe0dd554aa4cd4fda4e27fa1f63bba      1
     26 0x1431d4cf90be4035d113494178bbd635975c5e01      1
     27 0x15ab7dd2261026db55623d0da3946a3e022be19f      1
     28 0x161827e6ca07bf5b527a29bc3845aff27b805b1f      1
     29 0x162b2a0d3a1b006c89f2e5c7dde58d5de06b3854      1
     30 0x16380b4892e790f9547e92d2dab841189e469b68      1
     31 0x17bb4076ab59e0ef20ad5a873ab4b5341bf01b78      1
     32 0x1844247cb5320efa99b6804e73b5cae8c1181768      1
     33 0x1958b34de5bb14a8bdbd954d2a1ca6ea2eb65083      1
     34 0x1a4385c06550076264ab8d98452c7c55f47c9814      1
     35 0x1a6f0dddb884854355023a2dfe26e9174f8e0290      1
     36 0x1a839858ddd3e73869f74cdc07c8c6ad8c8ebde6      1
     37 0x1b37cb362986e1da8a83342bd5906ef5078efa38      1
     38 0x1e4323b88a097011761cf12935979a705acbbda7      1
     39 0x1f8a8ac54244e4016a376152b2f92d467552fa7b      1
     40 0x218a211431d5592316717bb9ac07d36f18d3c8ef      1
     41 0x22bd04906e808196b19f7d10463fe463ed5f88cb      1
     42 0x22cd049c57f070fd6bde748e493aa2627621ebf7      1
     43 0x230648f035da16d6d9ee3dbdbf3cfca6bd261ebb      1
     44 0x238069b3eac14ff18eca8faa31b27b83274e7a6a      1
     45 0x24fbadccd6684106e24065694ac87b0e98819235      1
     46 0x25353cc20c62b03d02c12025359eaef80adf219b      1
     47 0x25ed250fc4ca4fa44b4659b7892a1fe1a0ffc9a4      1
     48 0x268f5fa2adeb3a904fa60d4ffb904738f0dfe3b4      1
     49 0x26e7f147daa6eb33458ee531c6c22e34c7adc4c1      1
     50 0x2730fef18dcc74018439c70c9fbfc52b1d76fd3b      1
     51 0x27eb78c1eade6fc040d25b94e7acf6bbe0689f0a      1
     52 0x2820930e1f7bbf5af46602b37941ee51a31e6cd9      1
     53 0x284fdd26d51b2fba3db0eb36f40617cb11a6bccf      1
     54 0x2918ceda641716fd6bda3ccf691c3b54d4c87b6e      1
     55 0x29e3eb982c907a22af11815f6846cb7b1b434c99      1
     56 0x2ac81c02fbfa8fc432d572f2894ea61554d11dd0      1
     57 0x2d8f8dd6b16a1b5989ff9b82584cb612737de1a9      1
     58 0x2f8cb25737f469a3479dbf3cedf428a3d9900d39      1
     59 0x2fa71c300b8c0915e9f9d92df8cfc1fb62bca357      1
     60 0x3039fb04dd8c5d0d66493df168cd7aba92c57154      1
     61 0x3090fb16a6fcf9f85a8d55b710958227154f5083      1
     62 0x317c32ca0309bb8a1f9d8a3e2836544f233c841e      1
     63 0x31963b060d71ee24a6d458b75aa85e63b99bd7fb      1
     64 0x330bce303d27df0eb1b856da3464278db1db1ac5      1
     65 0x33161c528116bedf6b0d0ea4578026311b2d3a4f      1
     66 0x338a079d1403fbe7174b5f51d401874b83190174      1
     67 0x33ba48ec6863152420e0ba6f0badb286f03e9db5      1
     68 0x33e8d10459643faab56f1974291cb603b88a09f8      1
     69 0x3480c08ffcb3751721355887f689629314f36f3b      1
     70 0x36be14c0d731d70333e4f906a0d33ef65182fd0d      1
     71 0x38039da6bc26c101316af426cbf7f43834957c46      1
     72 0x38b2739bcb869494cc7953c79c97e3bcad7eac04      1
     73 0x395699d04a0ca9b7a12405306a37a9a3b5a9f068      1
     74 0x39d4051256995f8e7e2c9e383e73d37b44ccc2ca      1
     75 0x3a0a8ecd310c23e909f7ca96e0b7ec42d2c4a957      1
     76 0x3a94ec43996178f7013988358f58baf18a4cc707      1
     77 0x3adde9c976371a06c95b70030b31fc37117241b4      1
     78 0x3ae8d9ba72848bf5356f8cc56450532c9bbaf9fc      1
     79 0x3b0025b5cca0de395a561636acc602fe421d4ec8      1
     80 0x3b846d4d8823fb12cc3694a2584c5c32057808c8      1
     81 0x3db0b224099e56488d93f4e887a6bad88c39cfa4      1
     82 0x3ef81dc5f1e838e8eccfa3a176d2518eac3fef00      1
     83 0x3f131fb7c0d9e2ce83107a3f5533e38951064197      1
     84 0x3f358b4e54385ba4fc8dd9ea718d360db5c7fda5      1
     85 0x3f8a4401dfc3e07bad8e2fd13f7357096c909148      1
     86 0x3fad24d76807ef5c818863891f9e947bad91bf5f      1
     87 0x401cc1b6620e30ade449bb8f593a0d0799fbac93      1
     88 0x40b767d119dbc81547cc13c6c55b6ed40a6506f1      1
     89 0x40d775827365ae4d54cbc08a1a1c4f586b2c1d0a      1
     90 0x41254fe73d3b49f0fe3cc7743b4d40ef26bb338e      1
     91 0x446de57a85d7b8cec2ba273293b55605be27eeea      1
     92 0x4527c353ab252ae0d6f5968b17d1fad9d0f5153b      1
     93 0x4554315395ae7eb6c23f3883c53ab1b3bb4d24fe      1
     94 0x46aa797e862fa52833b6176269bf028460e79659      1
     95 0x46b249da04697c801cd592030f6ef81f9433398f      1
     96 0x497056b9e7832eeadb26c80724eb79d7ad0ca77d      1
     97 0x49d62955a8eee8cb8575a8d0671ba1ee1fcc0906      1
     98 0x4a7ed26157874f98f69e533a5b3e01651b816fdf      1
     99 0x4b2ef7127640964bf5cef3d1bf8d8f72d8d386f5      1
    100 0x4cf01a807af3fbb5fd29a5da34136ba3df0f932a      1
    101 0x4d6aa3da789ea162a5978193bd584d3067227835      1
    102 0x4df3253e42c10c4363e06282a800ad0fc0c3caad      1
    103 0x50280020a87c65b9e7937f920508f839edb5dddf      1
    104 0x50f6b36ce10436a89fdb211f731fd6f9eb672885      1
    105 0x5197b86b40a38135125ba0baacda38811431c1ca      1
    106 0x5199d6204b788016353a201b8dad4668a71f1a8a      1
    107 0x52d2eadd0c1e6ca760df239b25f59f177d241b97      1
    108 0x563b4e3be5452bd01852dc5e698ca4b2392d1200      1
    109 0x56bdc5fe7f9752e7f5a381e394acfe72a724462b      1
    110 0x5948428d0711442f6ae3c8d06e3a9895f33cecd2      1
    111 0x5aa71418ac6238c78ea9a032a768dbbbf86bfeca      1
    112 0x5b33b6f41e87111b7d22685b297f086b5535034d      1
    113 0x5b833aecb6e345d0f62a9108156167c50e6e76a9      1
    114 0x5be8f739c8ea94d99b44ab0b1421889c8b99b2e1      1
    115 0x5c5e224583172039f6e731d5a25ddb0eff41a6c5      1
    116 0x5d0d23ae81ebe1b6ac96411b2c7bc3bf7463fa25      1
    117 0x5d37431b1356d8cc54f3dd77657e566830d73186      1
    118 0x5ed94c46a3df9f4fd81a657d2ad4c18ec6218cb8      1
    119 0x5ee1e585ab2f6f8bf0a460f6475d64c373396494      1
    120 0x5eff35d620168bd496b39243fee0afdc593e972c      1
    121 0x608a69456272c7341c2785223bc96dad14485808      1
    122 0x628e74eae7f91f0a6cde507726b219e4119ecd3b      1
    123 0x6344c98cec7965bea126e591cac338a988f8af1a      1
    124 0x66259e8a66b12c610d5819573546ccadde9d0001      1
    125 0x667bedb9211dbe0dd20d4f00b51682be1e3f41ed      1
    126 0x6770f139db607c3def485851fee2e004ca1eeabd      1
    127 0x67ea5cbe1059d53e4580db289721b75b0ca963e3      1
    128 0x6811eaa638b04aedafeaddfca36c50ea303b1866      1
    129 0x687aacda9356c313f9fe88d1f4a24af7ab8a66da      1
    130 0x69083e9cf68da3a2fedec4e987c452c2ab790635      1
    131 0x692d1fce318f98865203a256b1024e070295c6f4      1
    132 0x6a3de9a0cb759f634a6a6f66c926d71b055482c4      1
    133 0x6a6d335d1ca6c4539b7cee6476b706685f97bc78      1
    134 0x6b5d9547eb066f1ee5685512efbfa38c21f33fa2      1
    135 0x6b6ae848f555f70944bc99d736fe29fcaecf8b23      1
    136 0x6ba228524d45c772d7643bb60c6bed2cd3dcf053      1
    137 0x6be53083068242bc624a4ce6174255ba0b35637e      1
    138 0x6be89fa94cadad5ce328db60e0fdf063dbd469bf      1
    139 0x6cc91dcbc8e4e20a5744d9a0737034cf04629747      1
    140 0x6ce9f49239e8adaf5bda1154262a3a054fad9db3      1
    141 0x6d1db4a7e83dae0eee7e95d421722d46d2a7e94b      1
    142 0x6ecb6038d923606b3540c4c6ac267c73ab7fa3c5      1
    143 0x6eff21c2bfdf83a33aa690fe3db17fc672244907      1
    144 0x705cea698f2b7c060df6803b3a0321178b339076      1
    145 0x722dcb2a0661b408d64904eb4734036af2890bb6      1
    146 0x732a3f4390ab037898dd49b8bf0bceb97cdbae8a      1
    147 0x73bcb8c5e30bf85806aade7fc36f16c6b80fd3b9      1
    148 0x7564314c19620a83b2c87e6d2febf0c3f846b1db      1
    149 0x75b772f2bb4f47fbb31b14d6e034b81cb0a03730      1
    150 0x765718a8e4a5562be34b122af259ce5a71372e12      1
    151 0x765cafe99e8aa861cc3cb037e584909e0539988c      1
    152 0x76d284401f4d9f95ae079eebeb7df0964987ac61      1
    153 0x77039399801f462b4ed13444a266b16355c471bf      1
    154 0x770d7eda6e52cc64737b553abc1f6fbf6dc07699      1
    155 0x773d715200ab5c07f39db9772e3c83c48534a585      1
    156 0x78941100d90c5b66b05b52e720549e8d33e89cdf      1
    157 0x79e561168835c783240a0637320d308897bd0922      1
    158 0x7a01b00bd2b99284084fc3f877194a99b47672f5      1
    159 0x7dab31a034ef37229a53d270dd973e4f559665b3      1
    160 0x7dbba7f0551aef5894fd6ee331b95dbb4349f5d4      1
    161 0x7dfa7a0ce4249c8083d91b01a871fac4a1492337      1
    162 0x812010bbe6652497d33c7a1fd986f629769ea9cc      1
    163 0x822ec855cc9f41b592622108e6f3565e976430c6      1
    164 0x82b631ccb9f9549e21a3870fdbbdcfdd3e03450f      1
    165 0x8367869beaa19ef706bca9a4e1abb96c2907b85d      1
    166 0x8386262bc928b5c7a0d19496c4921adffada05f2      1
    167 0x8400b081f5dc9c5bffe1b57276afdfab2dfbf29c      1
    168 0x8433ce2dba698afafc29422247bd5f256898cca2      1
    169 0x8463d6ffcbfb56bf20ab2e1111f9a0e8359533f2      1
    170 0x854f09dbd7c9fd93b52a1947b3dd4b41582ffadc      1
    171 0x85bcb8a0809c5dd5b03939b62f692d5ca5e3c0fe      1
    172 0x87b2dc356091c794490cbbf661384c7e6343ea61      1
    173 0x87bddc08871da3e3cb8665e4ff3be6a1906d0a9e      1
    174 0x8882b00939aa1cf4207a19c1172862f22edcbdf6      1
    175 0x8abe2749b67ad7debb7442c46ea4a0e003d4852a      1
    176 0x8c48b40dba656187896147089545439e4ff4a01c      1
    177 0x8c9d9fc19515b544f84b597af87da1f7d60c8646      1
    178 0x8cda7294507d985aae2d7a168d51fc675ee1233f      1
    179 0x8d2c9c6c5029496fd8321762b20d2529ef027c26      1
    180 0x8e166ebb2dc4686640bc70e47fdea1d2615cc443      1
    181 0x8ed55d7ae8a86032bfb78eedfd318086f9bc43bc      1
    182 0x8facc0ba5026f88c16b44b327407fb2a792662d4      1
    183 0x90d25a0bf3f4c8aca69d4fd7e8e7df9d4a89e7e8      1
    184 0x95298343ae03528a6b3c5d211005937f4987b51d      1
    185 0x952e2d065e60b0fbc9f130917f227621204ed48f      1
    186 0x9547dca1cadb86e6c3d80811de4ecc5b7ead2b1a      1
    187 0x95999c47c3e32a337ef108d657675c2757a606ed      1
    188 0x959d459fae3e8f26770afd2838b969c389915437      1
    189 0x96dac0c9e9512e4d6fad83eb658473fef5d16bb9      1
    190 0x9756a04f86312ae997db6d9d52fefd562b5200be      1
    191 0x977034d4819b17b4592033f7adfb94a45cc70467      1
    192 0x97cb3692848e43828c8289105e6c84e66bcbd169      1
    193 0x97d635d9b8c4c7e4956bf3e900d3b26816e188b6      1
    194 0x99be3f9166b7857e2b284adf9ddb8b6321483975      1
    195 0x9bbd708653f4f105d5eadd19607b7f360fa787af      1
    196 0x9bc38bf1d3f1161f831e45ae8295090eaae8bfd9      1
    197 0x9c163eb3589b0dcf366fc23c581307618b68fb06      1
    198 0x9c38e605c312f12c08517661bca2ba1e59202c03      1
    199 0x9d2e431a3010d9f3ac3c1f5ace093ac1f1d102b7      1
    200 0x9d32d65a84e2d6e53a7e6a368a7c68305f7fa199      1
    201 0x9f558ff9237242fd67b7aa42c162705f6c00db93      1
    202 0xa01701fb730631b6cf0f2e1f6efddd13241d11a1      1
    203 0xa0f1de4882a5cd1851989d9a1e9bbe9b4604e9a9      1
    204 0xa1ef1c2f8f90024e9ada814d903167e9b25f48e5      1
    205 0xa22245c6b496354b68c99adb678b5ba6435ec561      1
    206 0xa3785d2f231808d54aa207a84aee81c85dcbfb21      1
    207 0xa495e370e5987babb57580319c2ecb8e52a1239b      1
    208 0xa5855f4f40a409ffde3a171eb616926aec5d7b9c      1
    209 0xa5dce742f3775059d9406342d2ecbb0ff8e3a3c2      1
    210 0xa66ea67a418693636e4e071c3db5b5fe8b71dd7b      1
    211 0xa7cafd18dd8bc1e23203058d66f89f0f0ee539d9      1
    212 0xa7ecfc1e491baeec24b33ed24bc84916af54f240      1
    213 0xaac709a19274b53a9ce099e66c672422c019df7d      1
    214 0xab81be93f28664daf0c1638b483e4f9d2539446e      1
    215 0xac5914f52d77198d16b0eaa6c976e48110adce08      1
    216 0xada00080816c264ece458eee56802c2521c2469e      1
    217 0xaf0643a94802f8b02be94eccb6e19b738af16239      1
    218 0xb00b18b3bf6ecc43e6235ee69424d4a220437a4d      1
    219 0xb0bdd53b627d7e61cfc5c13ef110e47e210fac6f      1
    220 0xb24f0a04fa7fb5039cdb11ebf9a53f3873a061c9      1
    221 0xb3711303da2f2af927c4ab32ed4c0693572f35c7      1
    222 0xb39734396b95660707ef7f3d3a76e8566ee34377      1
    223 0xb424e5427b3694e3e5e6973d48762265f5a79320      1
    224 0xb524d93940034961b325b08be3d590166e5cfba1      1
    225 0xb5bd1ca7ef13954faf50286742aeb54d072ce21d      1
    226 0xb64199cd690aa4e78f1a6625660347ce63031d72      1
    227 0xb9bca56f33ea404a53f8ec98d0e746a2b4cb2ec5      1
    228 0xbc6d1201989d72fd3eaa9a898d354853099161bd      1
    229 0xbd088edb1aeced8d069a52f3bf253beb899437f2      1
    230 0xbde2ddc49a2e6827300faa6afc93d572114a60b1      1
    231 0xbe1dc0b87b165920419089bab0217add22707b9a      1
    232 0xbefe5d435616619253be2e448310f70136d0fddc      1
    233 0xbf9ede821b800787b7df86ad6ddebdece72d4379      1
    234 0xbfe856e4968b0a91b61342c8fdce7e0376ba1894      1
    235 0xc0bcbc0a91cd42e4808faf190e0ad6e9a6c027cc      1
    236 0xc0df7a5e3841bde77dc632c2a573644ddeec7999      1
    237 0xc47faf99100078461a63864f50f1315b6abbd5e8      1
    238 0xc4c6c27b2259794a1dd35d438e703281c0e4a004      1
    239 0xc4e19acf82441eab7e45a5e874cece70cd811434      1
    240 0xc611376086b5152a78070d89290d0a21e8d17c17      1
    241 0xc6a3b88130ff5ede3beb6ddf21a7ab097b783cb5      1
    242 0xc7282b3b928be27bf4f238bfaa473c0e1f7e9380      1
    243 0xc78997d1d8f8a6e9d87893296f0c9639de605337      1
    244 0xc7c32e908ceca0c308e6c6d58050d233cd278661      1
    245 0xc8331023f93517708ab2e7b3e4462a046b94605d      1
    246 0xc89e3a22cce756824701b51dad5251f4cbe83c01      1
    247 0xca6983947efc848df45c9e84b279438e24727d2e      1
    248 0xcb3408bad3192777e561b362e8467c213231ef9f      1
    249 0xccc1c34d851863a79165d07234b6b088150efad0      1
    250 0xce8ad80ce1a979381d209ac230d02adafb9fa897      1
    251 0xcf04c33bfea64cf5dd6ea97e2d5620f825b03b49      1
    252 0xcfeeb805ababc10a038551e486c5041131facaf2      1
    253 0xd2b12f1a75b2313980d7c410002e6a48cf389b8d      1
    254 0xd37905283adf21d3e628b423cbe20c4583ba9979      1
    255 0xd383a3b02bf956887211a8d96904da35f47c6bbb      1
    256 0xd4091d661a44648d61bd3bb51e129d0d60892056      1
    257 0xd42d71c83205e59ba0e2dfe7981d9c62af9b4493      1
    258 0xd478df699a99eb1921c7993b1aba4194cdb490e4      1
    259 0xd566f68779f1f671a6cb9b1b866af278a41653de      1
    260 0xd60f499d1a45e9aadf9633b460b2c96030eb827b      1
    261 0xd66bf38e7c704f0cd7910773f5c8cc3098b6828d      1
    262 0xd7d1c6484e4b8b3c09c7b9ca8722aa52b0046c12      1
    263 0xd809a687e957761872b440909348fa6547cafabf      1
    264 0xd8478053a45bfcdc7a8411b27c7329274c49de05      1
    265 0xd926c3dc7feca623ab94680082c6882c9783cdd7      1
    266 0xda4745f0254044e4b702c9301737c2a0b8c69a6a      1
    267 0xdc3a18b34630d85e6e3075c8f37461e615e00f67      1
    268 0xdf067d1c62b6a94e45ced3f01ebc94ae304cb045      1
    269 0xe04954decbd22658a65278cf4a2fa0fd2c37db59      1
    270 0xe1ca08c1ed5d8bdeae5ed0ea2e79cb757aa046ed      1
    271 0xe4bc73a45c9892b56d16e3a21aae062b7a7035ce      1
    272 0xe74419fda5425b88fe288923f5df60a5cda057be      1
    273 0xe7b15909675cd20a528cdc271e425b9923434f61      1
    274 0xebe923766012f51d6b003ff93932d0aa8fcf768d      1
    275 0xebea63f22998d2dcc433435cd74e3d8555b6e0a7      1
    276 0xecba5f51925e6ccec26da38dcd7d5305f6bdfbcb      1
    277 0xedc5925cab7d4a9d89e9e318f3c05cd65fbfdba9      1
    278 0xf1339ec6a48c55c2f80157778f85c1064d7c77c1      1
    279 0xf1995a4d99af77e51f5f455e6554a1d42f04fee6      1
    280 0xf205a58e5e993f93c9f25438fd6a4eac516f72ab      1
    281 0xf2439241881964006369c0e2377d45f3740f48a0      1
    282 0xf26f2f6f86cf3e8832a07da6f053a66a7a45697d      1
    283 0xf297ffe8bd936dbbef991c913ea4a3c7bb2f3bb2      1
    284 0xf3fbb2c0a711529ef9b81fe59a5ef5b8f1e0eb27      1
    285 0xf455f2432afb41745cd52a82a0b1de40732b5d64      1
    286 0xf4cdfea7fdf85872141e4f8eb0e953427de7626f      1
    287 0xf54d3e434a74865540224ee6219f60ba35de228f      1
    288 0xf6ace56abecbc8d9af8c9356fb492f78fb0b2c86      1
    289 0xf7764568b698e5954892c00ed7d9a390b105c5f7      1
    290 0xf837203e4fa9c139e41ff3241c434411930c1081      1
    291 0xfa4c1c7c023eeac5ea00046039f102cc220de21e      1
    292 0xfbfd1a69313f6b4e3c4d696d2de24b77cb3d9885      1
    293 0xfc0344564a36d3f572beca9cdaf67ca123bd4b45      1
    294 0xfce3ffd04de1ffdc60c1cc79a5127c5e4904c759      1
    295 0xfdd3980797820a22e1eda8460c2dec333a9c3ae2      1
    296 0xfdd9a3e4c07756c1dc31ba938fc062d45eab1668      1
    297 0xfe21e69ccb96e30bee474e2777daed303ab8ac0c      1
    298 0xfebaeca1976afa7a12298ec2016a0497253f2e1a      1
    299 0xfed4a9269a944d6cdfc6bfc20dc9cd5626030e51      1
    300 0xffcad6bba49e6a63489c078c3a4a9298ee21cfab      1

## Allow Artist Phase 2

``` r
c(allow_tjo_phase2) %>%
tally() %T>%
readr::write_csv(file="allow_artist_phase2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 200 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x00430ed74173cb5e61cb3d7e25b35a9790d8c4d4      1
      2 0x00897e2d7168165b81558c3cd9257efb007f2410      1
      3 0x0256c28028cac3e504f1b6c20bf7b3f4631e0d97      1
      4 0x02a5c980029cb470ac89df2e2de1cf453aee6558      1
      5 0x05576913eea5d79b83f28f0cb0d12be54fdae8dc      1
      6 0x0846931641992a70d393771b5d9a1ab8ad5b2f28      1
      7 0x0aadeef83545196ccb2ce70fabf8be1afa3c9b87      1
      8 0x0aeb17681de05ce61136b96c86f84fe5adc63f8f      1
      9 0x0d1d74535bcabda2da2cff5a53c2b899901d423b      1
     10 0x0d7f1ae643e8ca76aa71b232b46553aa92f1d8c8      1
     11 0x0ed39b4c8ca3f0faf74c780e8ec128314ee7f514      1
     12 0x0f0eae91990140c560d4156db4f00c854dc8f09e      1
     13 0x11732f4b391e6c511cb402ade3ba3dc49914039d      1
     14 0x11e7dfd1ceb3571e00888e96c77ed92fa32c3692      1
     15 0x137f41ca27f570d4c50e56d5c20ae2807225f09c      1
     16 0x13e6b490d50b2061d82421ec27b1b10942dc8bff      1
     17 0x14011cfa3e9c659e5bfb3f9f6a82bf11ebc6d6eb      1
     18 0x14368bfb69824aa8645a36e7ff38584ccfe0fbb6      1
     19 0x1469f53e90b79273f9b0e086296460d59a38d4db      1
     20 0x173dc3c983f7351721ac20e49c14a2a7ea0438df      1
     21 0x19363f5473ee1cf0bc1a647e94606b0b3e37ca2c      1
     22 0x193bb5dcb851ef8f75dbbd6950c962c3a5f4b0e3      1
     23 0x19c79a0c102a74d7bd317b4e51c229ea81769075      1
     24 0x1ae5f1c9939a5fc8928fc544c4cbb667fb505daf      1
     25 0x1b591e98dc0ed3f967359242064c76161fb4c0dc      1
     26 0x1c5762f9899b46c19d4c9b8fbdd38d35856227f3      1
     27 0x1cb65b78c71606090917e425dada70e976f28fc7      1
     28 0x1f3fd666b44d0f68f50012470595ff36200908c3      1
     29 0x1f623ee324ca178941545f01bfccfea38cda2072      1
     30 0x2045b4193d638d96dca702a8f551f6887ce9f1c2      1
     31 0x205a46012bc3c1a2583fdd426679dff276fb257e      1
     32 0x20be2aff79b1f330a798983c78c0e056f8627623      1
     33 0x21c8713c8ed18ea960e1b9bc576bbe3c7c59613e      1
     34 0x21eff9521bff7d6e0178a5e6bdd823e9178b6878      1
     35 0x2287c76cb20af782e83b389b5d72a00f6753124c      1
     36 0x249e16a08396618bfd45b02aa4eea28ad8397873      1
     37 0x28b8d4f7516a112e2e2fd462293a1c27cde327a7      1
     38 0x29e1df3647831ed307343543850810b9cd59c7a4      1
     39 0x2ad16eda0c41cdead6aa13290711697578192d00      1
     40 0x2b15456f9bda120f8eebb0a7556010685c5902e1      1
     41 0x2dba88cb3b435f99a3e58b6e0fe450e8f1a3f20f      1
     42 0x2e0d63ffcb08ea20ff3acdbb72dfec97343885d2      1
     43 0x2fe042e25cc3de10629095335817d57e8d9f56d5      1
     44 0x30cf9db393b40b16c62acb9da863a60082df0b82      1
     45 0x30e8a3bb3e8d26d05457cc2cb7c93448a2cee2c0      1
     46 0x31c72d0d03adc76456d3c14a4fe19f8aa8307c16      1
     47 0x33f99cd0e3c56a6852d4627455296f30f2851772      1
     48 0x34f49d19462bca4169d66921312e2562f1502cef      1
     49 0x3588e19d35f87c40331731c080b493c275902ed4      1
     50 0x3612b2e93b49f6c797066ca8c38b7f522b32c7cb      1
     51 0x389458f93e387fc568ca4568c231a64ffd0456d2      1
     52 0x39117cedc1769fd480ff696a61e8534893805865      1
     53 0x3b0aa499cc6acde1d4a7433da6968d7bb8bd8509      1
     54 0x3deed956b999b83361b85bff31d388c35125411d      1
     55 0x3e1f1fc109b0130a8ba13a1546c713d030b8613f      1
     56 0x43b3a12cdc49003c9537d0ab92800a97c0a8959e      1
     57 0x45f2e08b35a959a87833c57c4bacbf58f95eefb7      1
     58 0x46ae568842e3f3d2bb7e2a4f6ca8e38692c6ab86      1
     59 0x48b6f46635fcb3559fd37289256ac72c8425714e      1
     60 0x48d26fbd89dda10c72cd8dc7b82b9eee0af6b48d      1
     61 0x4969cf3d82afaebb1604da6ad393c51bb797ee2c      1
     62 0x4b4d75b8b0fc7528ea2614b2ca83555824c07867      1
     63 0x4c08bf7502ff6b8684e5ea1f350cb38241c4e19d      1
     64 0x4d91838268f6d6d4e590e8fd2a001cd91c32e7a4      1
     65 0x4de31ffa366271f90a6da19130d7ae8ba2541437      1
     66 0x4e898e4fb9d0dbc339df181fe7221a4f2ad67953      1
     67 0x50f27cdb650879a41fb07038bf2b818845c20e17      1
     68 0x5235381a2b50c450b5965dc607a46e34ecc35ed0      1
     69 0x550cfb8eb4d96d519de53459c2b9ed590560aa1f      1
     70 0x55c0c8186541868c03f434e9606e112ee0153965      1
     71 0x56439668bdb17bdf6d1cb376241ce754ec5b1872      1
     72 0x56e507da02d59190a59824308d1f63d40e0278af      1
     73 0x573555dc420c87082143e0b9af96b3413c7514a0      1
     74 0x59381f5d096a97d0fa786821431853ab2bdd290d      1
     75 0x5b6f36473aea942c3dbcede8fb2a9c645fe75fd1      1
     76 0x5b82493dab349df011cb2fb171ce80e732b1ef57      1
     77 0x5c13a95bb4130c7cc5e7353def09b79a3c557eaf      1
     78 0x5eccbbe6412c27cbb250fcf345e7b306ee020d42      1
     79 0x606a7bc70f8df3615242a12b70ecbb5257d0399e      1
     80 0x61f2d8e2b5ad7a47b2217197f5a5a3df25ae915d      1
     81 0x6224fd05d32b5e9ea20d61bfd10a5dcc2db6e76c      1
     82 0x6270d239ac3dd7ba9192367e50780031c82c6be1      1
     83 0x62a9c050f88dbf3244d1576bf5022825c4a924c1      1
     84 0x637d7dcbeea475ff91a1fbc86daf78035498373b      1
     85 0x665654f2d2a151be2d0f8e3697e6ce780f732af2      1
     86 0x686bd755b9396e93eb924da11f78f3c92076494e      1
     87 0x68858e8270ab4858eadbe94d0adf609693c59c82      1
     88 0x68f4400dac3a94d77c7f6bd12976ff5704253deb      1
     89 0x6a378eaae96285ccf255842581cdd73fca3c27a2      1
     90 0x6acbd57b033ff316c4c93502d39e9469698e6b73      1
     91 0x6b7e0944f3beb09a1fbe5f493060a22517d7d576      1
     92 0x6c0cf880cb20eefabfb09341fba9e2bd29ad3dfa      1
     93 0x6dd20703b80b0b97f87d44f377ea76b692504a13      1
     94 0x6f2148034c9615c0dc0d06589caf73d526d000e4      1
     95 0x6fc9c61a79f995275c962a7429cfa4177686807b      1
     96 0x73c4818b537a4108d3e90dc83baabe80405c66d8      1
     97 0x75182ab9bea2966bdf3eacbbc2cefba953474c65      1
     98 0x7525e71f51bda1fbc326000714d2fc68ed5aed6b      1
     99 0x757b8003c330418875f07e368c40b1b5ab4c6509      1
    100 0x773a7a0a828e19af85c487215adc4b54a8ef10bf      1
    101 0x79d8708b071adc332ce260ad263d26b89d2d34a3      1
    102 0x79dc911ba6f8424a4b6158ea0b24168a8e0e0fe4      1
    103 0x79dd8fab0661da2cd4131bb454bab060576ce2ee      1
    104 0x7cd29e0df7e442068e5751b638c9086fb62c50fa      1
    105 0x7e54fa1f85eddad0622841b0eaadeea2f19d3240      1
    106 0x7f61148a1daaa0e0c39759921f6677bfa0d88059      1
    107 0x7f742c9c3c4c9fac09d7d3487a5109c363bdc737      1
    108 0x7fc55376d5a29e0ee86c18c81bb2fc8f9f490e50      1
    109 0x7fdab8c244e0b775edeeb9ebb8356b59068c6873      1
    110 0x8025bb40e101aa2cab46fbf14b3346310422bc24      1
    111 0x819e42f5427c60d15572a1aabbd22a1fbfecfc76      1
    112 0x834cee2c58b212d37be016f303bc46e8184bd864      1
    113 0x87fc437d6ac5b9608872952abe6ac0d79843a141      1
    114 0x886c4d1d4bc99399a42ef9cdcbf0f0dfa700a860      1
    115 0x89119dac068cfc98bf2ffb7d15948e0901d997dc      1
    116 0x8a3db9109f2ef0d234b6ca0b7a08f9bc63efd9a2      1
    117 0x8abc6546a2895c27d2165776931c1276c258e903      1
    118 0x8dbbca57ea56290efa14d835bbfd34faf1d89753      1
    119 0x8f12219298c446b114c82420591e4c38fefcd323      1
    120 0x919a548b9bff7afbb92c34f981236b4a8d9bf440      1
    121 0x92c6a8f2488217a45fb607b40e38468b1de1c298      1
    122 0x945e47b62fef3385c92d42fdf655d17d10f03e6c      1
    123 0x94f052ca65a34d7f581bba95a749b1cf52689dd5      1
    124 0x9698c482b735fb3b8b882ab2bfcd19458d9b71f3      1
    125 0x994908a2e98645f1d084b69929ab9b08b564ebdd      1
    126 0x99b6b555874868ab095531c23ab906d398e740b7      1
    127 0x9c26583abdad7a5551fc1e85097a161b76e16450      1
    128 0xa042ed92a694bfdc00aadbf37ec6562f4b002495      1
    129 0xa05cd277ba50ec72fc41917437a6c09e39b99bbb      1
    130 0xa0f085121482cc4278853fb6ebbfa3967885cc63      1
    131 0xa1168968174d3a4f2b6666051800115d86fdc235      1
    132 0xa1faa82ec8348816eeb1c3e59405059042c54e53      1
    133 0xa2fb4334e0c429e8f2e98e48517d72c87acbff49      1
    134 0xa3e49fea82d17326636cece9c8c090edf013dccb      1
    135 0xa4ac3321fd639a7e2b53da62897955d920c97012      1
    136 0xa78b54a5486ce07838c1511c5ad5f72df2ac833a      1
    137 0xa7dcc417c63f24f9073b667a5d7149bd38463d0f      1
    138 0xa7f66d3ff024e639191c89170463b6ac4ff1478d      1
    139 0xa90dc6d356b622b9783f5020722e6f92b663438f      1
    140 0xa98d2867de6b1dff44fe4b355dea098e81d06aeb      1
    141 0xaa74cf491bb507fed993be33cd6bf3ca6619e1f2      1
    142 0xaaa9aca0d2aff48aab572de4d24088a80f2ae452      1
    143 0xac9169946d4f6fea622900fa6aa1812d5500bade      1
    144 0xb1ca39076de4929857d75d3cd586863331a86fef      1
    145 0xb21aae8e0276beadcce5593d108fdf6b2fdcf380      1
    146 0xb38bd68a6f0ba599808d65c7a9cf2a428105b680      1
    147 0xb4cf7b739c045c32010376d44ae23ad4f8811bd9      1
    148 0xb6f79b687c2f61fc69fef6fad96aee61228d81e1      1
    149 0xba0995c52474dfb7c48181f71454a63a2847982c      1
    150 0xc15add7eb1ba708bc7189cef6a4c47200b77a52b      1
    151 0xc18d4c2fee42accfe7bfdce98bfacbabf76242e9      1
    152 0xc25461af5e224b7aceeaf0eff443a239fd5040e6      1
    153 0xc39df8a6352425ba13b0f4b769a75de28b0db2ac      1
    154 0xc448197ffb0271cc1111f8a095c7f43fc20229d9      1
    155 0xc449f005667bef849261b35accf931a4bace48fb      1
    156 0xc55eec5488b0af81127591b6e2b571f54bad8c68      1
    157 0xc646e02557877d559e55a3984ec12654ddd84af3      1
    158 0xc7729d2b15f65b53a3bd1962e3379d617962d876      1
    159 0xc8a803eabd096e93a4d3e9b97dec4fa06fe7783e      1
    160 0xc8b4a24fa2a43ad2bee6ca3c7014918c1c1f836d      1
    161 0xc97958ff5370c56b7e0e6fb58437f00702714d49      1
    162 0xca87c9878c7b68c62c575cbb85f9c82d0ca24d75      1
    163 0xcafb98282f5ae4aa9083e031981e980cff1d9a79      1
    164 0xcbf06972ad4c32952d0485566c1ea0b4ecf11a0d      1
    165 0xcd94d3aa70986658d876ed6497636167014b1d1e      1
    166 0xd01fa4427a05fd7944557ae94b67d90109fad402      1
    167 0xd0326d8e2fb084d3a6bdc305c9ec25f81b6885a3      1
    168 0xd11d1c63911a42a2435256c013e4b88313dee2e3      1
    169 0xd20ce27f650598c2d790714b4f6a7222b8ddce22      1
    170 0xd2498b4dc8402789736f7c94caf969ea65badfa2      1
    171 0xd3fd5c2f754591a3c9148bc72cc4d28b9e88283e      1
    172 0xd70676cfa67abc65a8c39f049d62eea30e3080cf      1
    173 0xd7342ea20a5afbf24352b5ca61e09844167914cb      1
    174 0xd7d69baf8ff5a4016ebd928429c0b8d6f0d0277d      1
    175 0xd83e662b83880081741d0e810c3fe8630fa6cb4d      1
    176 0xd9b5021ba375be74b637fd9e0b8c666aebf2bd07      1
    177 0xda19d31abc3c080d32f924ccea6538d060463368      1
    178 0xda2e31b3519909c3e3dee09ece75066be599539f      1
    179 0xdb5cc422db555632d7e0489b57a3de530d3b0a23      1
    180 0xe1ddbe549962ed2d10f12c8b36d1aa67118fe131      1
    181 0xe487ed5ec80cca6e8a44f6d6f3f712088df8ad4f      1
    182 0xe540a8d94bd379c4d9c0478c8fe9f7fe7f0b06e1      1
    183 0xe5ca890a0ef2f128eb3267e4711c6bf3306ec024      1
    184 0xe85041b675b616f635798a63db467289e5aa1e4d      1
    185 0xe97a848dfe6945633108368cb2157edae483a9c8      1
    186 0xea15534dcd3a12c78345357d0d496f7fa85a5b71      1
    187 0xeab15e64b9fcf8603af00cc9ccb0a4cc6dee67d4      1
    188 0xeb1830e98bbd7f526c68bf4bc7916cbca0ec797f      1
    189 0xed46311e2b61683e30b658113c683b52890ff281      1
    190 0xed8982578836f7d498856b3f543f128bb8621f2a      1
    191 0xeed636b29784d5fdf8193f5e740465930acf6bb9      1
    192 0xf24c670d9e0e3fd6a15ca71a580963a0c11e5997      1
    193 0xf33f6c5302fee8dcaf6363f6862071092b3d56b0      1
    194 0xf38f67d8b16752b18f7358ef216a35423e45f806      1
    195 0xf39e356b26630115dd171f1d41640c45cec9ab21      1
    196 0xf8e225d47c1f484895212c608de0c22bb20d0576      1
    197 0xfa621bbc0e434c400aa45d631b9bb97be11e44c3      1
    198 0xfcac1500835b9f173dcac47a182e0c92d8100ec0      1
    199 0xfd35624baf2f0b3755b7cfbefec7bf62f9761702      1
    200 0xfe2b8dc957459ac2a08fef57576bcb8a89845d8b      1

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
