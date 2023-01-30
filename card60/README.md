
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "16513569.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance          contract        
     Length:38783       Length:38783       Min.   :  1.000   Length:38783      
     Class :character   Class :character   1st Qu.:  1.000   Class :character  
     Mode  :character   Mode  :character   Median :  1.000   Mode  :character  
                                           Mean   :  1.263                     
                                           3rd Qu.:  1.000                     
                                           Max.   :652.000                     
         name          
     Length:38783      
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

airdrop_gradient  <- pick(snapshot, contracts=c("gradient"), address_pick=3,address_max=1)
airdrop_zuphioh    <- pick(snapshot, contracts=c("SuperRare","SuperRare2","MakersPlace","MakersPlace2","FUD","StripySafari","TheZebraCollectionArchive","ZuphiohWorks"), address_remove=address_remove,address_max=1)

allow_gradient    <- pick(snapshot, contracts=c("gradient"),address_subtract=airdrop_gradient, address_max=1)
allow_raw         <- pick(snapshot, contracts=c("raw"), address_remove=address_remove,address_max=1)
allow_singles     <- pick(snapshot, contracts=c("65291/1s"), address_remove=address_remove,address_max=1)
allow_zuphioh      <- pick(snapshot, contracts=c("ZuphiohWorksEditions","TheZebraCoIIectionEditions","MakersPlaceEditions","ZuphiohEditions"), address_remove=address_remove, address_subtract=airdrop_zuphioh,address_max=1)
```

## Airdrop Gradient

``` r
c(airdrop_gradient) %>%
tally() %T>%
readr::write_csv(file="airdrop_gradient.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 3 × 2
      address                                    amount
      <chr>                                       <int>
    1 0x477ea7a022e51b6eb0dcb6d802fb5f0cfc3b4a81      1
    2 0x699990a8e7ada9e92c932d6e8fb365024fc74b43      1
    3 0xd007058e9b58e74c33c6bf6fbcd38baab813cbb6      1

## Airdrop Artist

``` r
c(airdrop_zuphioh) %>%
tally() %T>%
readr::write_csv(file="airdrop_artist.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 40 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x01b41056fa643f2391d34fb5a1d4876cc3e692e4      1
     2 0x035da52e7aa085f7e71764c0c8a1ce6690e3dfef      1
     3 0x0f0eae91990140c560d4156db4f00c854dc8f09e      1
     4 0x1adef8b7253c43caaf50500913d422ae688367bd      1
     5 0x1b967d5a3a14f661cc3bb0632cda3e152765ea32      1
     6 0x222f9ca6b2ac845850f8f424a1034483a269edf7      1
     7 0x2bbe56a2b719bb7fe64e78c539788f64c757c63a      1
     8 0x2cf0bb6d1cc1a7296ad0da0adfdf74074844a588      1
     9 0x2debdf4427ccbcfdbc7f29d63964499a0ec184f6      1
    10 0x30dec07af807a47c4c5a2ba69a22738831f002b8      1
    11 0x3a6c058fbac5b92de3a772ac3dd8975f2d967d15      1
    12 0x3b3a01fc0dd12698f19959ed0d0d042b81ff13dd      1
    13 0x3e0914fc2b3b67a476142020b7d6990d01ad234e      1
    14 0x478bb542f7658d635abba67edb987806dff5b83d      1
    15 0x57115f7d04c16f3983802b7c5a2d5089a188d76a      1
    16 0x5a8116f07937394e6ac2a51995ab6a054c08bf9e      1
    17 0x5f2ef2515fec192ac726aa02cfdc2bf823bcf6b8      1
    18 0x66ff8f288c413e61c8ee807088ce301c03076e6e      1
    19 0x6892274bb66893113dd440fd6dff3d4ff98b9d30      1
    20 0x6e63a4caeccb4f341ee9c9175c9cc554bdb6d10b      1
    21 0x78cef30e6c416b6818c0651da8d73af50c6ae5fb      1
    22 0x83c255e31afd0a79cd03809d36afb824fab0ad91      1
    23 0x89e833fbede92dbbeba671abca739a89badaf744      1
    24 0x8f903cfc0af3c2ec0d872c57538af5e071544a57      1
    25 0x945cb2e060388fb321840280e40fbc1b9eff0885      1
    26 0x9769334fc882775f4951865aa473481880669d47      1
    27 0x995e0505603a19ee5c469d2359427bea68c6e953      1
    28 0x9a3acb42d506767f7d83c3b6f2ead417adc1cdaf      1
    29 0xa5855f4f40a409ffde3a171eb616926aec5d7b9c      1
    30 0xab7473da0af4658ae2879cb641d82e7652721486      1
    31 0xaf153e755f59bb62ba8a5b7e5ffdb71c0ac43305      1
    32 0xb76a622c7fadbe11c415d141fd7b9eb4b1f414b9      1
    33 0xb88194f9bcc5ec80cf80c163ca2b123917468793      1
    34 0xc23b78f47cbf3f4ccf3bfe5899dd6a20c308ceb5      1
    35 0xc4f94270364674f3bcce1487d98a959e72427dba      1
    36 0xca536f1a439eadb69643b850a198939bae77ee4e      1
    37 0xf2439241881964006369c0e2377d45f3740f48a0      1
    38 0xf4a12bc4596e1c3e19d512f76325b52d72d375cf      1
    39 0xf62431b6a6940ec6bf07a8be542538fff6e37bf6      1
    40 0xfea1f357b453c9cd89b893b07baa6abfe8536ca2      1

## Allow Gradient

``` r
c(allow_gradient) %>%
tally() %T>%
readr::write_csv(file="allow_gradient.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 76 × 2
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
    43 0x896b94f4f27f12369698c302e2049cae86936bbb      1
    44 0x8ba68cfe71550efc8988d81d040473709b7f9218      1
    45 0x8bd9c4b5ea8f6f14c9b14d830ddb67f3720d77f6      1
    46 0x8fc2326375174bfe3baccc98cb9906921dc9599c      1
    47 0x9224bbb4e0fbe2f2f8fab55debc41eb21fdfb804      1
    48 0x982d3c5223f6b5794fccb3208eb164d042cf2526      1
    49 0xa56c04347abee42f663eff9bc2d0147b97c8f782      1
    50 0xa743c8c57c425b84cb2ed18c6b9ae3ad21629cb5      1
    51 0xab2056903a7b62bac46f45a3d7a70ac799ca88cb      1
    52 0xae0d16586e5d60d334624c115216a52b9b1a0335      1
    53 0xae72c6a6fad9fa9d82d089e1ebf73b3043855425      1
    54 0xaee4bdcf9d164d9adbbcbfd846623fbe133a6018      1
    55 0xb6cf25f5cf8a1e1727d988facdd47f1dfc492caf      1
    56 0xb735af7ae1a77d1ec764c862c2c09bdbf2b34b27      1
    57 0xba4575ea27041d99e6614ec02318f1e23a623fe2      1
    58 0xbbdd72fcce73c2626719be00259ddffef0d5673d      1
    59 0xbf270918afe2ad16093ddce904fc358ad337cefa      1
    60 0xbf814810b44978de273191fd612aa47f7b69d564      1
    61 0xc02e6b0d0c1a5d8cd26beeba0fe8d76c5d2f19b9      1
    62 0xc2419841dcb9a0f8906d06463ae24e00e1470846      1
    63 0xc6400a5584db71e41b0e5dfbdc769b54b91256cd      1
    64 0xcaa1c396e70384db58dd33be74b26fb119e98c3a      1
    65 0xcabd5f77ca9d48f4ef9793f20de42ad39ce93979      1
    66 0xde112b77a1a72c726d8c914c23dceaa53273c351      1
    67 0xe359ab04cec41ac8c62bc5016c10c749c7de5480      1
    68 0xe3b41ae8785e4107cc69f988042ff4a66a367fac      1
    69 0xea39c551834d07ee2ee87f1ceff843c308e089af      1
    70 0xee958e45f3464d712b8830deb5875c8ac105f698      1
    71 0xef5ab90a44b68d4f5e3f6be6af4bedb12cd2c66e      1
    72 0xf15a6b54e68884d27e1bebb1624d70c227b7d04b      1
    73 0xf2c5f1fd977dbd6de9d04bc4e62dff722d4bb1a1      1
    74 0xf8aace471723dba6514f6c6d43651611a0c34529      1
    75 0xfcbca52b23db28470eb97680d18e988d8b60b608      1
    76 0xfd22004806a6846ea67ad883356be810f0428793      1

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
c(allow_zuphioh) %>%
tally() %T>%
readr::write_csv(file="allow_artist.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 168 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x00b755deba4118b0d9e99803256cb7784515ce8f      1
      2 0x03777bc796f13863644621acacfc18a052fc6a20      1
      3 0x039d7c84bc43f8bc311ec21aeef57dcb45a8091d      1
      4 0x07b564ade1b8212eb5e46e2d82ea1addf6f7273d      1
      5 0x08ee7b575f206d4f5fc16b0c3cb70e0deea8a732      1
      6 0x09b2f0988b6d24336b348c84c2764d2b4f85142f      1
      7 0x0aadeef83545196ccb2ce70fabf8be1afa3c9b87      1
      8 0x0dec2c1f5825755d90748792392101b26cc6046a      1
      9 0x1022245eb550f9778209ca4514dcb693b45498be      1
     10 0x11a22b262e505d355f975e1e48a365b5d4811ae0      1
     11 0x18895c95fa5d8ce7da1a9f8f73c880d3b353c785      1
     12 0x194a6c07f82482d7d4603261699bdccf00582447      1
     13 0x1aa760cf97f1389e03a2c8b4ae7aa1d748d1b666      1
     14 0x1b612449e9eea55d71a64ec69c77e1543270a93f      1
     15 0x1dc301854bfecb4ba03eb417579585c2b8ea12c1      1
     16 0x1def9bebe8cef5f1c0f5fc71f1f0c1312a273e15      1
     17 0x1ee8ba8a4b5c5d6e7ec691ff0c93eba580324ef2      1
     18 0x1f4ecb08a111a2ee2e0b6a589923febf477a8e2f      1
     19 0x205a7e3b7db1f84b89e3098e33ecd6377dd3cb50      1
     20 0x249e16a08396618bfd45b02aa4eea28ad8397873      1
     21 0x25665e8b95792c23d6167d2d0903fdc6bfef87e9      1
     22 0x2638f91804b32190622ef82c6a265d8e9dae57a6      1
     23 0x2877d3244f0f6c8e2a51e4f01719d70cc320af30      1
     24 0x2932a3ee7fb73b44edf7403b421e232ca4c78dd8      1
     25 0x2cb3338f518b2d61470b4d1d503bdc84c1a04ecd      1
     26 0x2d13271c668d31e0ddd4563727e871f82dceb41c      1
     27 0x2d1a6268ddef23e3cf079812e83755968dd93da5      1
     28 0x2ebfede79d7b9cf0404db0d4efc6a8d6e370985f      1
     29 0x2f1390ec03a03e57a1406bdef57c9cf487f62b78      1
     30 0x2f44fb58135ae5d3793803c73d471c7cde4bb774      1
     31 0x330bce303d27df0eb1b856da3464278db1db1ac5      1
     32 0x33e913e15c2c6a0f35126ddaf6bb34c99850f79c      1
     33 0x34a35cce3edc0084a9312b4f88fdcbc11cfb89ef      1
     34 0x36be14c0d731d70333e4f906a0d33ef65182fd0d      1
     35 0x3782bee1c5cc8578808d28c10b77c9e4ea083907      1
     36 0x38dff25a32d7281d2dc463a2a0f41af22ad034d4      1
     37 0x3b63410fadbda0ec0ba932fad2997edd7a842679      1
     38 0x3e5f46ad03a924d0c1ddc9aa311fdd265be312df      1
     39 0x3e7374ee60f296ef2350c47a5110cb5233321756      1
     40 0x408388219cba8fd7eec20865fdbf7786f4cfd145      1
     41 0x44c967fd7afaf25a818563089176e6622b2f022c      1
     42 0x4518344525d32415f3ebda186bdb2c375d9443d6      1
     43 0x459fe0c0955d1a81aaa98190d75734c118fb4654      1
     44 0x45d8ba4f904c8e28ca2417b4ec99a9b61a64992e      1
     45 0x46b249da04697c801cd592030f6ef81f9433398f      1
     46 0x46b2bd5c888e6d57a839c559fd6076f2c15a8cb1      1
     47 0x46c3165fb17127b511c9bd3c1c95c787d6601c7d      1
     48 0x473c923543d9c91209b087f456d4a796b52ef96c      1
     49 0x4808d5a2b6423abc0060465fdfd338c58e758690      1
     50 0x49f04718d20ab71a237072701adecd2ce9d47e7d      1
     51 0x4fe91b32cfbba12f31ea73331f932a3f031ddb9f      1
     52 0x55eea5ec5e162003b0a51418f7dce3dfab7f7007      1
     53 0x561cc039d84f6831561121419ea7732e92ffa289      1
     54 0x57816604ab4147f2aef715067e76c51a0785c0cd      1
     55 0x58085328f6ff03212f202c601d1ee797fa67a953      1
     56 0x5b1beec213237d88cba6ab40225103b70402729e      1
     57 0x5bd7f2d70a4e868564152744ab949b572161112d      1
     58 0x5d8361206806b864bbd96349a438445176cbe00f      1
     59 0x5dab695311e4e7d5be4743187c3e40589f45c9b0      1
     60 0x5e3161efeceab0d3f7f3fe46f0e3b045d0c9ec74      1
     61 0x5f7670321668e6db551e39e7a5dff443d42dc97f      1
     62 0x60eaaa669c70166dc7504a860353161cf525da0b      1
     63 0x6126134294607dbd568dda4875a393ce1192adcf      1
     64 0x6689a21f7b22d268d197ca0397a55baf318b6a78      1
     65 0x67e45bb1359c580d04264619773d2b7b26092b02      1
     66 0x6ce9f49239e8adaf5bda1154262a3a054fad9db3      1
     67 0x6ed7f81208839e31e11840049201201c469a7a56      1
     68 0x713e87680bd4f44207a6bc03906964477302ec82      1
     69 0x7191124e9208cbc0a62dd3f78ec87d3f40c1bba2      1
     70 0x71a618889fba977def3fba6b3545b6d4e841f9e1      1
     71 0x71e22168b702bcff528b8974cd4b723250b67609      1
     72 0x722e9468d7f8c2710b6dbfdf70f01b045aca1851      1
     73 0x72406fbc5e70814b8ab21c9f383702eaf005ca72      1
     74 0x72bf9ebac7288d694bdbbc5879517b029199c821      1
     75 0x74401b66f746630ccee98689b1a20e0149fbccab      1
     76 0x762da606029d3120735aa1eec15464e265db7a3c      1
     77 0x77039399801f462b4ed13444a266b16355c471bf      1
     78 0x7787830003d8960009aca592424c8c7751cb9acc      1
     79 0x77acac99ac831a1574b9db4d15299e98e195e6ae      1
     80 0x790177fa7eefad834b7fd9d4cebcc2220c0d77aa      1
     81 0x7b2ad6af37be6a77c95556d9453e4877b6a8437d      1
     82 0x7cd29e0df7e442068e5751b638c9086fb62c50fa      1
     83 0x7d08c340fcfe051c462dcfda8b9ed47642b96b10      1
     84 0x7eb5bdc103a3674d0584c769265afed22b492126      1
     85 0x7fbea99658312561b2bb1699a2f0e77979ce745e      1
     86 0x7fd1d8e388b29ef0b8a00ca07e2266686a5559ee      1
     87 0x8157a950b55523ca4631ecff62b092fb59b5ce1a      1
     88 0x81e5cd19323ce7f6b36c9511fbc98d477a188b13      1
     89 0x82015102581112e32d7b9d80ea82a55ce47b3962      1
     90 0x844e192ca04604745d51d3f271f377d7ec363f00      1
     91 0x85bc03678220009e82206ddccbf713228750f759      1
     92 0x87108a6227129c5a64b8c9b56ea23cda0b47c5bb      1
     93 0x8abe2749b67ad7debb7442c46ea4a0e003d4852a      1
     94 0x8aef89129806b23c7930dfdf2b46e22ae1849c49      1
     95 0x8fb3ed352df6e8ea152dab12f2d060e4e52d9e6e      1
     96 0x9674ede5eb609f9483dd1682f2160ec45105250d      1
     97 0x973a2ace28745ce4715659c60ef70b9e4c044086      1
     98 0x980fad0b4e8820a8efcb4bfd626cb824d2a02b78      1
     99 0x99b2648be337168035103df00d84317d1af2ded5      1
    100 0x9a7dc5307940fa170f9093ca548bda0edb602767      1
    101 0xa1ce3d8e2c7607cc04fbcfd282ab2bf046ce8e98      1
    102 0xa5ff0d30460e7790cc4c7e37aad1ce86efc50e2b      1
    103 0xa693fe70a4180c29d0ea522dbd4833dced5fb94a      1
    104 0xa75b7c9781dd52298b3aa4c238a7fe84be90cbdf      1
    105 0xac4109a00af7d892be8512e82a9c82cae63de88b      1
    106 0xb01fa11d9c4742e820ebfd62483d995c49565a0e      1
    107 0xb03f5438f9a243de5c3b830b7841ec315034cd5f      1
    108 0xb245a959a3d2608e248239638a240c5fcfe20596      1
    109 0xb2d866297dbc7c7d8ad045c05afb48a95f28fb3c      1
    110 0xb3866e0292d10ee4bf69534479b5113697d1c681      1
    111 0xb519095fb7f2438d778f44afb9cdc2f9611d85ed      1
    112 0xb691120339b4f17bbdfa591e5f9f6278c8a15f46      1
    113 0xb6add2bb3fd4aa6dcd6c663346685f8f18acc1e2      1
    114 0xb893ae8a1824604f6df4dfde52e2754921ba1a73      1
    115 0xb9be99d3428f189a173ab9e7d903ed50d1653eba      1
    116 0xbaea3cf94abd0d6e0f029ef5b0e54e9424a72985      1
    117 0xbb34d08c67373e54f066e6e7e6c77846ff7d2714      1
    118 0xbc9aa6262e2b34d5d124bd754d74a8a61f75580c      1
    119 0xbddb878b393bf91f8d34b2edf7087ca529e3ccaf      1
    120 0xbf4a907ac732179f0a3ab15c2e9715da19a27801      1
    121 0xbf5aa87878c968c9d062f51cabd04b66023c8e36      1
    122 0xc00ae171acd39f84e66e19c7bad7bb676c1fe10c      1
    123 0xc09d7d74175e2375575e79898f86c79a39a2d4d5      1
    124 0xc0ad23a629c6de23c03203b396c5d1e1585111c3      1
    125 0xc0d10c02ccc57f67a7c13448101e23cffe73116f      1
    126 0xc1754e1edcdde07b89b7cd690ade3a3136704b28      1
    127 0xc1a5b8c0be50a98eaa0d28d4bb2dfa176444c23a      1
    128 0xc41dec3cf3a042a3684be9279d5067ab9d813033      1
    129 0xc8d2f3ff5d8feac92b77b792b47c11eb27369eda      1
    130 0xcbcb8a49716228e93f31759500a5d378f3582954      1
    131 0xcd8b72710595c69019c109aef5b1b92eea7f995f      1
    132 0xce63b511ac3855b7d5a0c92c965fd62b9ae4c0e3      1
    133 0xcf731df9c870b99003c8c01506dbda6da8e30efe      1
    134 0xd3f82a34065b221c9da38a668568f9ba7ca85666      1
    135 0xd4091d661a44648d61bd3bb51e129d0d60892056      1
    136 0xd530282d853169a23822762af0dcab045d6295d3      1
    137 0xd73280d61210370d32c3dd05639753ec7f0ad4a9      1
    138 0xd7ce4706f31606981dc35255c8ce27934f9a7624      1
    139 0xd8fae773447f127a4b23050b6ed0c5a07a6669b6      1
    140 0xd91babefdb88393cdfdde2b45df67c985bf3c82c      1
    141 0xd945290b5bad30dcfd118a48390b754f5f1f29c3      1
    142 0xdb416840b876a6cfd878ba6af02238d146dc016f      1
    143 0xdbd8fefa7f919825c22ab1a1ebfc5ce51340f1bb      1
    144 0xdcae87821fa6caea05dbc2811126f4bc7ff73bd1      1
    145 0xdcd50efb4ccdc9c409b4a4d8eb919ed301c86abe      1
    146 0xdfa413375306e2169adcbbe8551f69739328e6dd      1
    147 0xdff6b88d0372d71288103f3ac0a91a211a413794      1
    148 0xe001891949372e1aa33c50c7ea19568be32ecde7      1
    149 0xe0fe9e97aab194e31ff0beb0c2b9b3f527fdd7f2      1
    150 0xe1b020683fb18e2368f70ed238f9a564832c1217      1
    151 0xe388a6d5d020333ff9b1638bfeb042510ece6b9d      1
    152 0xe52dd720c72e5aade63e32d9d587a45a2bddf518      1
    153 0xe6b4eafd769f2d29ec4bd2f197e393ddb7d75b84      1
    154 0xe92689183961135f51bd5edc601a0aa123c069f3      1
    155 0xe9a567610a8f4efc33cca1f319d62b76b804c5f1      1
    156 0xea1c7edf57c7302679807bd7de1d21501bd61c03      1
    157 0xeb0e194d60f307391bbeffdc6d4bec8f56aac045      1
    158 0xef6ced74ab5fc5743a43d42866c5932bcea3faec      1
    159 0xefe556dfda44224325ed20d800de6908a2503603      1
    160 0xf2ff0e26ed564109acd25c544459f0e72d1a5343      1
    161 0xf3bb97e22ee247b5c38fe2ee4367145d7f908fe6      1
    162 0xf4ec9f411742a6d0b6e48381af75e15532b87908      1
    163 0xf596de113b75db5093afe4f92a70e821630b62f5      1
    164 0xf6b29a05b0023a8c219705fbe550b2999b94b1b4      1
    165 0xf7356eac86640c188eb5116b593e089f3e1f2131      1
    166 0xf7ae3ddf6a3da444fe28203861cb26691141ec6e      1
    167 0xf8a09bf413576a2e8b406dae09732aae29a1c8d4      1
    168 0xfa04975e9e80e717e7974b8967cc48d418c994ee      1

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
