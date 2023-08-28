
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:5578        Length:5578        Min.   :1   Length:5578       
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:5578       
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 18004469 # https://etherscan.io/block/18004469
block_hash <- "0xe8f36c14f955a122bdd145b6a377b043570370bcb6975ce80a963fdfdb7775b8"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4615 

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

allow_artist_phase1      <- pick(snapshot, contracts=c("SuperRare","JanSladeckoCollection","SillyCubes","Foundation","NiftyGateway"),address_remove=address_remove,address_max=1)
allow_gradient_phase1    <- pick(snapshot, contracts=c("gradient"), address_max=1)
allow_memesRandom_phase1 <- pick(snapshot, contracts=c("memes"), address_remove=address_remove,address_pick=300,address_max=1)
allow_raw                <- pick(snapshot, contracts=c("raw"), address_remove=address_remove,address_max=1)
allow_singles            <- pick(snapshot, contracts=c("65291/1s"), address_remove=address_remove,address_max=1)
```

## Allow Artist Phase 1

``` r
c(allow_artist_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_artist_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 45 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0040122ed305e0b16dfc9fd8e0e9a74a3cdd5509      1
     2 0x0b39fcc9893ba075efd6ef6535b7c0921c889db6      1
     3 0x1625ad776034675046aff7a48f3ac61b62c0e27b      1
     4 0x1ce8189b2c8a5a4d0e7263fdd60bbe0d21954f46      1
     5 0x21301d901db04724597d1b6012ac49878157580d      1
     6 0x2563123aba5d66786f7b16b60502bd839c840a83      1
     7 0x25665e8b95792c23d6167d2d0903fdc6bfef87e9      1
     8 0x2d2bab870b7e09e59de96a7ea33fee9e663e6aca      1
     9 0x3041e4002267efe34a3fd8fec78f20ea4967693e      1
    10 0x330bce303d27df0eb1b856da3464278db1db1ac5      1
    11 0x350aa47305555e8c6e80ff1b670b3db228573520      1
    12 0x3a8713065e4daa9603b91ef35d6a8336ef7b26c6      1
    13 0x3cb63b82d778105e43f064ed739b0655f1f0fb87      1
    14 0x407ed94415b5ed61d1f30950520d658a59a21ed3      1
    15 0x42c5ad864dcd9abd9012d5b918d3d22842d180b1      1
    16 0x45a4b93f36d4ed75bd839508b5d25ff22f0a59b3      1
    17 0x46b2bd5c888e6d57a839c559fd6076f2c15a8cb1      1
    18 0x51857539d1e5d44df15a3574b6c88c5770bd37b2      1
    19 0x5681ed9502c8a5eb58666587b8e6bca0fecbbbad      1
    20 0x58b3492976b3273ee7d256ab8bf7f9338f3b37fa      1
    21 0x63784c971e102bce755b140680de86404843aedb      1
    22 0x692997405c1cebbc56ff3416fa0b59ab97fac696      1
    23 0x6c3675c755c06b236cb10b5acfa0445fd8aad455      1
    24 0x6d1db4a7e83dae0eee7e95d421722d46d2a7e94b      1
    25 0x6dcb2a373398b17ef9b052d547a3785a9ac6985e      1
    26 0x746b46d87543eaf779144c8870888119d6ae0c2a      1
    27 0x7f6d34a4fdb21729ac15aa0ae01b5b47ff2f5f31      1
    28 0x8507a82600e3f2c85459c27721184698f18359fb      1
    29 0x8c2d9c1a98944963b4693f5d30f57056aa2485ce      1
    30 0x9769334fc882775f4951865aa473481880669d47      1
    31 0x98d633d2df5e70b6b93936f225fcc16106383aa2      1
    32 0xab6ca2017548a170699890214bfd66583a0c1754      1
    33 0xab7473da0af4658ae2879cb641d82e7652721486      1
    34 0xb08d161278ecd51c11f01db1862ef5b041956288      1
    35 0xb7e9a292da2e7b7d2acf9a3fd7f5f6498b37a358      1
    36 0xbe65d45635a400d895f0a2682c4fd3122b10fddd      1
    37 0xd81ce8e89dd987c8ab630858c8f1e9df14788c35      1
    38 0xe1c9b7038a03dc898cd161827709a3c241af991e      1
    39 0xe54a00d5418b4b36037edc9302c4ac2418d714af      1
    40 0xe81b4d511939ea9b0d6b551d006e7f400754ef97      1
    41 0xf1e1c701b49b1dc2405a9e8ef40f9f88802b80fa      1
    42 0xf2439241881964006369c0e2377d45f3740f48a0      1
    43 0xf430fa41b78ed89b768b957269fb0c5e2204aaaf      1
    44 0xf8986ce1fa0a656ee8d7e306c9182db06c6d6449      1
    45 0xfe5ad755edd4ec409162a0915632ce392d930d6a      1

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
    48 0xa743c8c57c425b84cb2ed18c6b9ae3ad21629cb5      1
    49 0xab2056903a7b62bac46f45a3d7a70ac799ca88cb      1
    50 0xae0d16586e5d60d334624c115216a52b9b1a0335      1
    51 0xae72c6a6fad9fa9d82d089e1ebf73b3043855425      1
    52 0xb6883c21885f5091f5064f4e29991bd9cd046473      1
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
      2 0x0150b0ec3c7b72879fbfdbdd79c18a8368b194e0      1
      3 0x0162f0b0deaf7147d5603d83c2c149737a6e80df      1
      4 0x03183fcd2d6a40b466375cdcafe2cd61d0cda659      1
      5 0x036302a75c0f7dafe15f83c8a1faec282a74a03b      1
      6 0x03abc3de5f7c23d9065cc5610e6496d72208357a      1
      7 0x03ed8e6d50bff0b8c49675f0bba94087d5e579ac      1
      8 0x03ee832367e29a5cd001f65093283eabb5382b62      1
      9 0x04c95c6f8ea7ff7c3f7a2f148fae75650c5a875e      1
     10 0x057931155d0c6355b1bd108b0fbfe9bc77d59ef0      1
     11 0x05b99f8f59d67b5e7b1dea82b1370cab65ed91b9      1
     12 0x06754d9291ab4f4b67fd5a12d7fb05b766cc2b0a      1
     13 0x077bfc14dd6725f260e1abfd5c942ee13a27091b      1
     14 0x08fc70adf6b0950749b7647f67616589b1853a53      1
     15 0x0a21a14fe203a267a13c3455abfc9d5f5ae982ee      1
     16 0x0a49fbe88cfc413181dbe93c9e3b6184b2264071      1
     17 0x0c0d8f387030c631de001d99d40b2e519cf4d10f      1
     18 0x0ce390f18af702cca546297845a4a51d102123cf      1
     19 0x0dbe146db9c963bdc56d7445e293c7c3119fa2a1      1
     20 0x0ee7d116e0c89aaefb3c3653cdb71e384766ad11      1
     21 0x0f0ef9408ca5b0dc364ff5921bf544c811e9c94e      1
     22 0x0f3c76ade30adf61b64d01154911308491784dfe      1
     23 0x0f5a2df7fb9da8c8c7371cfb523ffa527518200e      1
     24 0x0f5d5751d4f300c3d0adbcadc05f0af4f8baf788      1
     25 0x13bfd26cf7d981aae5f8d4d88af4b34b4b1d4da4      1
     26 0x147f466fef6bd04617fc0c8037ea01c73bc25a3f      1
     27 0x1513f6d3e5468a3a7c4f6b26ffd123cf0dbe4893      1
     28 0x156db724902f03f3986f6adfdc38f7696e4d5111      1
     29 0x15a33852846b4762d71a62928aa7c64d8aac8544      1
     30 0x185d5f60fb7c59d344ba939361908916cbffe1dd      1
     31 0x1a4b7409e020140443a02ef26858a7e53ac7d652      1
     32 0x1aa760cf97f1389e03a2c8b4ae7aa1d748d1b666      1
     33 0x1ae50668be1f32179bce00eb203121f00907d808      1
     34 0x1d05a2ec18c7d4707ed4cd40e7e76a680e4618e3      1
     35 0x1d25029d92a7051f46a13be9a512443c966d3542      1
     36 0x1d3dd4cc14c9bc93bc8744559be4ace760f7a1d3      1
     37 0x1da0f2a58e65d98a8d04ba3da56cff9f5ace5615      1
     38 0x1e6ef3e23dbf38428e752a7293b698b189c7317f      1
     39 0x1e7ba17cc3f031a0c5795739317f6b2022ca39f5      1
     40 0x1f916bbf39ab189a9e3d9e1823a7b1a8e9e5f204      1
     41 0x1fad9f65b39d235469bcb59bc664872b93eecac5      1
     42 0x21d79c2be4ab3de2802a60da66f86d497d06102b      1
     43 0x24d6ff72eccab41724d488a03e8ea721ec3177a3      1
     44 0x2595aaffb353e812bb5cbb63f7f62e36fe528f02      1
     45 0x25acc72796bdf4579755708fdbc8409622d224f7      1
     46 0x269c2d0badff4e365ac2a0da8c8e3f186b6adaba      1
     47 0x275c83d76b1b0cc42bd066e327c4e578a327ebcc      1
     48 0x279a84d4d6b1d4b1895086c775db1e91d5849cdf      1
     49 0x27e037e0461e3d5587de786ea23441d83772353d      1
     50 0x2b919bffea16d55828091fdb8a63f0678e17b26e      1
     51 0x2bb2ee28a5bba51db2c104b6c36a6907c21d4d4b      1
     52 0x2be9f18c46df11f05b581131af4d339c20c7254e      1
     53 0x2e8f6f8662593134dc8311e312885921855332bc      1
     54 0x2ee8670d2b936985d5fb1ee968810c155d3bb9ca      1
     55 0x3025430ae8a81cd13e3d0969b1093f8d82bbbd7d      1
     56 0x325d12fe8bce0f969f879679275adccfe8ba0605      1
     57 0x32dd9f6885332ccf8633a18cb0c2e25e68fe03d1      1
     58 0x342522ae61de25d48c66807a2cecac4681be3d33      1
     59 0x343193017fd217d19fd983e31db701385c8504f8      1
     60 0x34dea787524a930670862a27bc7234a1897e1bf2      1
     61 0x3543493a76ce5ca362dd6eeef000547de52b6875      1
     62 0x35cbedc3412d3c93dec4afa60ddf38c2a9c38865      1
     63 0x36d89738405a8325b7c560cab2dd1b88565effd3      1
     64 0x373db7e01ebfb92d9095ae6b8f6e3d24ce6f4d4d      1
     65 0x37769c5bdd7a43b0b52958ca51bdace1556c96bf      1
     66 0x386e0ad23a9f182d884ee2b8f61ec8c626b94385      1
     67 0x3877b4c39476e3af04cb1ff8003db09f39f068f9      1
     68 0x38b2739bcb869494cc7953c79c97e3bcad7eac04      1
     69 0x38cb81cea002c1a7659762e57b2878a5b93969f6      1
     70 0x3927f1b51afd205311bdddeb5646c4c6a309b2b2      1
     71 0x3a4c619284748869eb3ab16494b461256b72f875      1
     72 0x3b0b262b187001522c34edcafc378500133ab230      1
     73 0x3b9818a507afb5b6865152a29d5f762e2f8d458d      1
     74 0x3bd5e344ac629c9f232f921bafdeeec312deac9b      1
     75 0x3cd76a7a90e0f6a7dfe6791a54ec459acaebc5ca      1
     76 0x3dc3be0a9278028fde0e3ff65c6154e2e36dee9d      1
     77 0x3de4b60cb19faebf58efea6d4cd99fb5295cf95c      1
     78 0x3e3721d26d5b8612bcd6504696b82401b9951ba6      1
     79 0x3e998c50b1eaf1577416e2bf31a76d09a48bc3fb      1
     80 0x40d775827365ae4d54cbc08a1a1c4f586b2c1d0a      1
     81 0x41e9e61a699a85daf12dadebbb7e6e517bee6b54      1
     82 0x42617123b481f61df6b5b8a6484ba0a4e6929279      1
     83 0x4470e0f5a0a3969cb0d5aba71ad4e6c759dfc824      1
     84 0x47836f027d427e78c06f24eabfdd1d2089fdfd5a      1
     85 0x47e054a8fcf3e27a4bed979c33ab65a5883fe437      1
     86 0x4898203e852b3ed44cc3e8d37f702fd0a7bdac9a      1
     87 0x48be4681972473b498e8b686d38e04826c26fc4f      1
     88 0x48f823fd7f5fd781ef2da1a1e53a92d0f45ca176      1
     89 0x497452b2b3f5f5f2f4e82b36b67ccdc01964d404      1
     90 0x4b2c1ce6a01981dc3ee79825bdc3f3cd7932bf11      1
     91 0x4b4d75b8b0fc7528ea2614b2ca83555824c07867      1
     92 0x4c3d85e7fc0c91215eb6a4186592a41d975d2a4f      1
     93 0x4d6aa3da789ea162a5978193bd584d3067227835      1
     94 0x4e3cc03eb2be6122f2316b84d1c7edb900e90dba      1
     95 0x50002a9b8e9938d509de84dc3eb3aabfbec1451e      1
     96 0x5022cf478c7124d508ab4a304946cb4fa3d9c39e      1
     97 0x50b88f3f1ea8948731c6af4c0ebfdef1beccd6fa      1
     98 0x514afe6575b5d81cecaa86a6bddf28de2f89ba45      1
     99 0x52d232170f00607b555d97b943094b4ba866f0f0      1
    100 0x52f051280e850f32cbfafd8d78c31edec4c3248c      1
    101 0x533fa4a37aaffdb02456d374237091520790383e      1
    102 0x53887f0dee06c6459bc928f9f39beccac3947325      1
    103 0x5458c15ff220bf75b92a76229fae88f93df9c6c6      1
    104 0x55bae3706b678ee2d5a6d6d2faec8a41854aaf9a      1
    105 0x574a8af005c82eaf05e92c9ccc01048e1eb1ae41      1
    106 0x57dec6115e561f3de9fb7429cf14de429713b3d6      1
    107 0x593cadf136c68c720d446461c1bfee600647c6b8      1
    108 0x5d181a27b5a2f444ddf4b89d65c88214d7413ada      1
    109 0x5d25087405105bab12624c73488ec186066a6376      1
    110 0x60bc11840966ae3946ad1904a8efb6622226be25      1
    111 0x6215e708a7b61f5ed49642f692f28ba09b807725      1
    112 0x638fba3bbbe8828394d2b13f92fba75bf32c8efb      1
    113 0x63923511f7b628483f9e160cc173e68a5ded11b6      1
    114 0x6430c11231ec6723e5bbe6ab258f184f5af7c836      1
    115 0x6590ceaf9227887c43c6b3eeffab08f326a1aabe      1
    116 0x6850dd83a896ab7a1b191ee377cb46b708d4e515      1
    117 0x68cbec6e76a8cc835ae75cce5feff1f36cbaf764      1
    118 0x68d45e76a87e9f1706e0d36b7815c8dbdb7e6aea      1
    119 0x6a5ad95a3b0d6d4739de4370f51c8670a4d53700      1
    120 0x6a9b682290dfd41f19a6211c0b219050affbfbcc      1
    121 0x6b0fb37210fe1cc00b164c8a89dffec3c75cea31      1
    122 0x6b9bcb0b854d7c7430baa55167d367d28a8dbf1b      1
    123 0x6cc435f69fbac8ccddc8757e46a9d4376fdb16b5      1
    124 0x6dd97134b53f40d239842716b2cf0179a11876e9      1
    125 0x6e1510ed41ba743344e9ba80646194351cca0c7f      1
    126 0x6e9075da365be17041e704a16d39652a7a54b862      1
    127 0x7120a2b756ed48b6f433f70481b16b353df58b88      1
    128 0x71784687d4c74338bf284bea22956c74fbe6d631      1
    129 0x722e2e4c15be1fdded3c86f4100bc32b181827f5      1
    130 0x72bc237cf598505dc00d4fcc23b1663e206455a9      1
    131 0x72f52de160ece454a2d75a410f85f4978045882d      1
    132 0x730a2f3421b2967ce8e9ae1f839839944626812c      1
    133 0x7546c60ae8d65dc6dd7a0f61c169818059ef49db      1
    134 0x75850227f61183759cbbfe2b3fb196e798ce0b95      1
    135 0x7656e919f2af9c7d339333cf29a20c8d7f7bad13      1
    136 0x76db02500f7631d57bc2dcdca9d4cf782b99e119      1
    137 0x7738e4dd4fc3b6c95b8925f078363924c0f0b428      1
    138 0x78089705ed807984cf3edf9149c9369e8fabb9be      1
    139 0x782adafbf47a604f146af4a059908e946eae539f      1
    140 0x782ff3f609427680db175365c24c238f89fdd276      1
    141 0x78f0269f5b1ca914c8c58993f41181f89e212b27      1
    142 0x79d1dd0770fec45768a285da42c2521603c9b91c      1
    143 0x7ad225751ed67118e9f54f87e08f64e43ffbef0a      1
    144 0x7b640407513bc16167ef3450fd6339803982e976      1
    145 0x7bc7481751d7fcb76d08f596b03ba1e42378ea27      1
    146 0x7c5504688eb4f81ed558550066dc218ffee1f6c8      1
    147 0x7c76cf8894d42ed11b53236da54f05add1d44073      1
    148 0x7cdbb8e309ce7c701ed200be123f16c6f7c9e525      1
    149 0x7cef7db4289005fd56dfa6c73bb2be6ec13bc250      1
    150 0x7dbba7f0551aef5894fd6ee331b95dbb4349f5d4      1
    151 0x7faa22f45695a77ad4042c4cfb2c0330caa6a329      1
    152 0x808a023b72260170c95d831f589a1ae0dca1e43e      1
    153 0x8212642c68a030bab8e4e8d43952c7c4c6cf2903      1
    154 0x8451b49f03c80b8d4d116090ddf10047cd173d55      1
    155 0x848f8b6c67f4197a53ae3869d3200fac175a00af      1
    156 0x853c69418605529a68907aaf7789270e3cf69d97      1
    157 0x85914d049aa004d037cae65bad275ed4147f172e      1
    158 0x860f0aa48ec759df1034d72e0311482a8b01db83      1
    159 0x86286d93d79a9e5b78de97cbbc8dcaba0f2489fc      1
    160 0x86f0a82dfd9745ec23bc8a72c819193e74962eb3      1
    161 0x87da297a832706b5115ee1feb47b7fdb7c476261      1
    162 0x88ceca090d7d810f0d3bde0cdfb1cf1f2301bc17      1
    163 0x8a6adb9e6e8dba6bddae8bdfb17fb4657720c600      1
    164 0x8a9bfdc136d5d1166497882af5d8c9718f27fbed      1
    165 0x8ad272ac86c6c88683d9a60eb8ed57e6c304bb0c      1
    166 0x8b17e8b3f8315e73986f0c8370cea4f3a974a532      1
    167 0x8ccb07293755004942f4451aeba897db44631061      1
    168 0x8d6cfe2ff08d8b6766eaef33990b78f8990b4520      1
    169 0x8e2a6185d7c37aaab6375f9737ba0a764cde36e0      1
    170 0x8ebfe0a1b5989c87f3c34bec8c160cf9e80b2a78      1
    171 0x8f3a658a9736530e89c242ef572d7198cfd540ea      1
    172 0x8f4b933491e1319799229ed9d36b179bb859d705      1
    173 0x8f8a1e5113b11926950185d74567dbafb0aece0b      1
    174 0x8fe89aa22200d1306aed5dad7dbad89c8faf8e26      1
    175 0x91e8be20d0dfb2293b97317e906223c1fa833a48      1
    176 0x93549cf8047b3e305fb10df7110da213e9429f9c      1
    177 0x94345607093b249b5100e8d1e499928dc163dfdc      1
    178 0x95722d15041f50fe9135962cc87042b4d6dab382      1
    179 0x9617562c6f338ed18e2e8e88cbbffce6003bae0e      1
    180 0x9969db4034a136650cdb07955cdf5635499a4012      1
    181 0x99980dd0184773ea492c23033e1309609a7e75fe      1
    182 0x99c7be4c85c4e9d26b7cab28a4e6dbfc8bc19178      1
    183 0x99f70f9aa62bd71a7e0719afc2d6c67c6aaaadbc      1
    184 0x9c163eb3589b0dcf366fc23c581307618b68fb06      1
    185 0x9e027bc1bae517189285a207c69615d805670f7a      1
    186 0xa0a43e56c6df95cf01d089d1284987468f8ddcff      1
    187 0xa0ff0e41d6847b1fce33a721a766e9e040027e6e      1
    188 0xa12cea84eb9e6dceecd125de27cf2e9701104956      1
    189 0xa17bfb3a816996ac3a1e26dddcc7850663548c16      1
    190 0xa1ef5ac40d7ea6de39faef3cda7bdd82e09240d6      1
    191 0xa32560268dfff7e62cd0cda1dcc699406321aed6      1
    192 0xa44b68fc590a2fd1cf48f2689a8aa2c8b62d2261      1
    193 0xa56b61e5dee77d1668569dadd699f70eab4e193a      1
    194 0xa6c3f37c74e753777fcf535ff9e9f2c4030c9184      1
    195 0xa73769aed346319287410811639ac3bec8464d55      1
    196 0xa8174abc36b3298356d273e512cd87223a2b3ffa      1
    197 0xaa20816a724c8bcd2b8bebb60b1a7a1f90e3ec0b      1
    198 0xaaae5c0a8e05ee5b3824b2e9fe939d5dc3ba3336      1
    199 0xaad4210b800f14660ef4068029d428936ebd21fd      1
    200 0xab25a2c1a37e1f1fd689a8a9ea61eddee054f0ce      1
    201 0xab81be93f28664daf0c1638b483e4f9d2539446e      1
    202 0xac104d56e4f8e0462f8c001ec7dde4c68deb596f      1
    203 0xac1a04679039a1718d3820fbc254ce29269af784      1
    204 0xad5bd06f5fb4633888ac7506ec0c87eb99998653      1
    205 0xae220d647426e368ac321efec83a9d696bf84e7a      1
    206 0xb01b3a335db49192a31af43a88c458e5070ca5e1      1
    207 0xb16525385824e8cf877be513386e75d875043ffd      1
    208 0xb1db41aa2484e3f5f5a510e07003c29fd1b0f115      1
    209 0xb258e243a526e79c4b5c8dd5df490e42eb7927b3      1
    210 0xb33fb83c645ac2a12b138636fe59604312092484      1
    211 0xb49806cf8dcdb108d318d6f4f7ab087851445d94      1
    212 0xb5bd1ca7ef13954faf50286742aeb54d072ce21d      1
    213 0xb6328f4b747b807696da1bb2f574edcbc47682ab      1
    214 0xb6fc3c8f4e5233f2ee2ea1ab84a4d10c61f6d215      1
    215 0xb7187511a0a0f5a63a538b7bc0abdd1a01979848      1
    216 0xb774f65d9eab67c9a587eadb5b79d0a02bfa5c42      1
    217 0xb7abe0f0a1c31a88fdcdef71033cf7ae7d12f2d3      1
    218 0xb8efa945cf7f35854afb80e9ac05f5c1dc172fb3      1
    219 0xb948ea974cb95c9c73c51deac5d9e4464e769a44      1
    220 0xb98be1aba8ae2adef0daa18247273fa06021daae      1
    221 0xba264037d93f4e06d5b19e9a73f29fa64689e1ba      1
    222 0xbaa4aa9933b4548cff0be2693e7af7e14e9ee49b      1
    223 0xbd6006b93f10b484d22d5a9d4e17bfd0ade4f614      1
    224 0xbdc765d81db7e0ca47fe6bf57e987966a2c204ac      1
    225 0xc11a862ba1125a1bc6b4ff11b2ead6b8e73ea52f      1
    226 0xc14480bd7bb6d36377c75286b80452654f5b7a79      1
    227 0xc322e8ec33e9b0a34c7cd185c616087d9842ad50      1
    228 0xc53f977136e1aa9031343fad39dcb4c11a1eb3c6      1
    229 0xc5ed0799c911bf8147680756825725eb038451c8      1
    230 0xc6c3752bdab737b8cfa174e6ecfa620ec0cc162e      1
    231 0xc72f40a397453051349f73cf2e2a04fac06e37a3      1
    232 0xc7c6eec4978799e6a53da79b540432820a349d0b      1
    233 0xca8299a5f070faf59e87f6c4df6e7627d565e006      1
    234 0xcbc7e8a39a0ec84d6b0e8e0dd98655f348ecd44f      1
    235 0xcbfb9a917c2b45fbd8e820e66602ef26dff9cd40      1
    236 0xce4122fec66c21b0114a8ef6da8bcc44c396cb66      1
    237 0xce56a746edaf84e65aa6a2bbb45b2328f6a99935      1
    238 0xceae52c73db66e280f8d5be3db89dc69463c7d72      1
    239 0xceeab2af38e6b086cdce120c49f93b65f0b92b76      1
    240 0xcf281a48bb4342e7fc5cd5316d9bacd0e413e935      1
    241 0xcf7c7758512aaf43979b64812cb79bd3f60f4413      1
    242 0xcf803a49f6e37b92a4040e397b976939dc5d9841      1
    243 0xcfd648eb66b736351f48dbd5a1752708371c10f4      1
    244 0xd387a6e4e84a6c86bd90c158c6028a58cc8ac459      1
    245 0xd38a87d7b690323ef6883e887614502abcf9b1eb      1
    246 0xd38e1fb60cd50cf9ae747ecf646b4ff1a310ba55      1
    247 0xd530282d853169a23822762af0dcab045d6295d3      1
    248 0xd70676cfa67abc65a8c39f049d62eea30e3080cf      1
    249 0xd81ce8e89dd987c8ab630858c8f1e9df14788c35      1
    250 0xd86057c793bdfb77bb11dcabfe054ee4fd86ebdd      1
    251 0xd89c641cf9cca44a3435895d24494d1bb7d70ee9      1
    252 0xd939fe2e4776ccaddb823a26eacb45c66d8ad5e9      1
    253 0xd9c09e1ddacce4ee6548b66773bf4e6fe9dd0882      1
    254 0xdbc7a4afadbff03d1ac183c9f664a95775c6d52e      1
    255 0xdc0d28f3d09b292d41b836db991b157d641ad64f      1
    256 0xdc36237208adb46959e77a0052843ce5446afab4      1
    257 0xdd5f934edbcfbdb25ef5dd9b8a3868d3633de6e4      1
    258 0xddc9520acb5d1923412674a4ce07bb2e32ff0ac7      1
    259 0xde328c96ae88f37733a8d2302a0eea537d557b24      1
    260 0xdfd818378e60a1b7e26cf165cc57de8ff0a65478      1
    261 0xe048ca2aa2b5fe1991487bbe46bbfafaf4234402      1
    262 0xe0753cfcabb86c2828b79a3ddd4faf6af0db0eb4      1
    263 0xe1759f0cd1358c049e7f64c26e13de9492f9f888      1
    264 0xe1d29d0a39962a9a8d2a297ebe82e166f8b8ec18      1
    265 0xe1e57c6d779adea0f34cd3b84fb6bae3a54d1cfd      1
    266 0xe25c73435702fed11e9c5584ce6efe7cbff71739      1
    267 0xe29bfd5761a18b5c3ac4a4271eb7fadd4c7fb942      1
    268 0xe39bd9e3af2054dd193b0d0217cfbd460f104902      1
    269 0xe560646ef7a69400974d85e818bc0e054bde65c1      1
    270 0xe5fccbbc11e4a36a77e9d8e47a9b28ec44e2e2ff      1
    271 0xe61c204d9ab083240a7c8522e636298261ff354e      1
    272 0xe69cae620953aef05e9276f35a7d3cb598bf4b20      1
    273 0xe77d44e642c53db943aa0a71ef60cbff719e644e      1
    274 0xe781fe7a6f65ee6b3efe66ed8f7f5c1e9f01cc55      1
    275 0xe7bfc67952b0a48f4ce3db309ab1adda322763dc      1
    276 0xe817ed328e561507dc84d022f42b46f92e003002      1
    277 0xe8cc7c453eca25450ca16d551db93d5f5740e054      1
    278 0xe9e88f56f5431d692446ec723c2f9f9cb4eeca42      1
    279 0xea7d3818a3b736fba40852cd90471df33c6d6821      1
    280 0xeb1c22baacafac7836f20f684c946228401ff01c      1
    281 0xee2b31fcf6c51ed01ae06cce05293c30afa5eb87      1
    282 0xee58519040de3504932f4c79e1d056ef1b42a026      1
    283 0xee620a0991d57f464aad452789a4564ba51245e8      1
    284 0xf01bcb0090cd0f734688ce77ae067da58e9d8005      1
    285 0xf22742f06e4f6d68a8d0b49b9f270bb56affab38      1
    286 0xf253c885f7e5f2818c03547b16f5e9068d4735aa      1
    287 0xf262c25f75e93ddf7c1fcb4bb0d65a707ad6d023      1
    288 0xf3290df945e484a4ff846fb5baeb6d1147a7aa69      1
    289 0xf4aa005cdd64b2c80e6cbfc30c427f80b5d1f0b4      1
    290 0xf52393e120f918ffba50410b90a29b1f8250c879      1
    291 0xf54d81b2955832474a87a8dada4a042a05b83180      1
    292 0xf6dbff32fe8308fd30def6c8629dffa5c315245a      1
    293 0xfa3dabf38f872f50c62c3d39920d173d215806ec      1
    294 0xfa6bc968cf39a88aa67725463698b6a84bca865c      1
    295 0xfa766f029e703d341cc5386b9a4cf4d52be3b6a3      1
    296 0xfc382669ecf6a4f19efe1e32c64d75a1daffb1e1      1
    297 0xfc61d7884b047f9f4bf56e984f773f1bd5d51480      1
    298 0xfd8835df382b69b3cd498aba0d745fbf6a421d13      1
    299 0xfe4da73ff6543b17b2b18e0e5d94bc87bd79f527      1
    300 0xfe73f0b55ee4b411d7d1e4d5d5d4f8834064e2b5      1

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
