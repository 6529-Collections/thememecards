
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "16710969.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:3831        Length:3831        Min.   :1   Length:3831       
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:3831       
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 16711269 # https://etherscan.io/block/16711269
block_hash <- "0x2598f0b22aa6e4d6a1415df69fa2af96e4a8a1542bf88d646995b5dd674e2258"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4627 

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

airdrop_gradient    <- pick(snapshot, contracts=c("gradient"), address_pick=2,address_max=1)
airdrop_memes       <- pick(snapshot, contracts=c("memes500"),address_remove=address_remove, address_pick=24,address_max=1)
airdrop_shavonne    <- pick(snapshot, contracts=c("SuperRare","SuperRare2","SuperRare3","SuperRare4","AccelerateArt","Foundation","ConcealedEmotions"), address_remove=address_remove,address_max=1)

allow_gradient        <- pick(snapshot, contracts=c("gradient"),address_subtract=airdrop_gradient, address_max=1)
allow_raw             <- pick(snapshot, contracts=c("raw"), address_remove=address_remove,address_max=1)
allow_singles         <- pick(snapshot, contracts=c("65291/1s"), address_remove=address_remove,address_max=1)
allow_shavonne_phase1      <- pick(snapshot, contracts=c("LoveIsLove","ByProxy","NiftyGateway1","NiftyGateway2"), address_remove=address_remove, address_subtract=airdrop_shavonne,address_max=1)

allow_memes_1_phase1       <- pick(snapshot, contracts=c("memes500"), address_remove=address_remove, address_subtract=airdrop_memes,address_pick=200,address_max=1)
allow_memes_2_phase1       <- pick(snapshot, contracts=c("Memes501_1000","Memes1001_1500"), address_remove=address_remove, address_subtract = c(airdrop_memes, allow_memes_1_phase1),address_pick=200,address_max=1)
allow_memes_3_phase1       <- pick(snapshot, contracts=c("Memes1501_3000"), address_remove=address_remove, address_subtract = c(airdrop_memes, allow_memes_1_phase1, allow_memes_2_phase1),address_pick=200,address_max=1)


allow_shavonne_phase2      <- pick(snapshot, contracts=c("B&B&ODNAPhase2","B&ODNAPhase3"), address_remove=address_remove, address_subtract=allow_shavonne_phase1,address_max=1)

allow_memes_phase2       <- pick(snapshot, contracts=c("memes500","Memes501_1000"), address_remove=address_remove, address_subtract=airdrop_memes,address_max=1)
```

## Airdrop Gradient

``` r
c(airdrop_gradient) %>%
tally() %T>%
readr::write_csv(file="airdrop_gradient.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 2 × 2
      address                                    amount
      <chr>                                       <int>
    1 0x2924196a2ec71ea4ae1b1357381eccdcee6c18f2      1
    2 0x60d2bc84a71af61b82d1b4168c9103ff6c4898eb      1

## Airdrop Memes

``` r
c(airdrop_memes) %>%
tally() %T>%
readr::write_csv(file="airdrop_memes.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 24 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x04294157cfba9ff0892f48f8345ea3539995f449      1
     2 0x0dd38657d7c8024e7d62bde3d0423ca34769be50      1
     3 0x185952b3bb31da0ae18354bbb90ae40adc840c33      1
     4 0x3bc161e3a5604165f3165ed8aaaf0d490e559324      1
     5 0x431181dae361813567f35ee2abac73291820fcc9      1
     6 0x48464efe55fbcae8ae0c992b306afcf21d4910cf      1
     7 0x4d7e6948fb5ed2d92897de0605cd642eff69fbf4      1
     8 0x56b367a6303b3d626bfbabc625b8e0b5bd9452f8      1
     9 0x58059014c5952c04370bcb88a2e0503e9eafb209      1
    10 0x7dbba7f0551aef5894fd6ee331b95dbb4349f5d4      1
    11 0x843708d85621273f3bbc643b348da3a60d5b0334      1
    12 0x88ccdfe9dd047b4cec4c1102d2d803e2d8bf683e      1
    13 0x8b4d0402f7b2f063f255214b7095b5911a257a30      1
    14 0x8bd9c4b5ea8f6f14c9b14d830ddb67f3720d77f6      1
    15 0x8dcd5fa47d7ef22db41b2b66504923ccca5065a3      1
    16 0x9233389b9afa0a6e3bb403f9d211c23a3b290d69      1
    17 0x97a45305513133c62b2e3a1f2181dfb5ffdbb450      1
    18 0xa6d4758ef49d2ae8e65e1a02216cb9744aee6b23      1
    19 0xa8879c580a54f190ed53b43d30de269097ad7543      1
    20 0xa97204eb6b0f3e58e8acff48a2317511e7628949      1
    21 0xbfe384d79969bcb583b8a0e5fedc314aee480e7e      1
    22 0xc4bfc1dc25827f39ad4a66bfc968456c7852a9a2      1
    23 0xf620422e1e2cbf123386ea664f536d1016e5a5d0      1
    24 0xfa977a0125c16a658a59ca1e59c7c48d58142226      1

## Airdrop Artist

``` r
c(airdrop_shavonne) %>%
tally() %T>%
readr::write_csv(file="airdrop_artist.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 24 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0256c28028cac3e504f1b6c20bf7b3f4631e0d97      1
     2 0x1d41071761c941e854838583d1be87a2449dd036      1
     3 0x280676491188f56fa386d9833d84702ac1e24c71      1
     4 0x2c8875f34ceb219f61b7453b2c5f100ec2f6ed33      1
     5 0x3432b45b9ee95bd5c31a726b936cf2ec719a2153      1
     6 0x3b0aa499cc6acde1d4a7433da6968d7bb8bd8509      1
     7 0x45bbd6b9f4e20baa91a9af9e74ffa0b756e624e6      1
     8 0x4c0646b7b71b901c5db3c469c68dda3e2d19de3e      1
     9 0x51787a2c56d710c68140bdadefd3a98bff96feb4      1
    10 0x5fbe0ae423f1767028f150aa92545267507588ef      1
    11 0x6e5b0de0add71e1052b11abbfe6c433dd0466fb4      1
    12 0x7d8498ec7c5a638add7c48646c49c454d0c1c90e      1
    13 0x951038bb372d16180c0afb0f46ab283059154004      1
    14 0x96d2b5bfa179ce0979fef08ca7ecec497b050b75      1
    15 0x988ce7e0aea652381dd51f48279a02110aab9d59      1
    16 0x9da6640321b9c7a538b5b126e2cfbdc6a999144c      1
    17 0x9fae8562dc4fbcdd9d26d6d10a7fb88ce60a028e      1
    18 0xc8c90c83fd08d7e66703982de7a6177732240ca0      1
    19 0xc9b6321dc216d91e626e9baa61b06b0e4d55bdb1      1
    20 0xca11d10ceb098f597a0cab28117fc3465991a63c      1
    21 0xcf6f5a68d94165081a634ada997be3a93426c467      1
    22 0xd1e8a92f44bdc83bc620c56a7913fd97de5abe10      1
    23 0xf1e1c701b49b1dc2405a9e8ef40f9f88802b80fa      1
    24 0xf3290df945e484a4ff846fb5baeb6d1147a7aa69      1

## Allow Gradient

``` r
c(allow_gradient) %>%
tally() %T>%
readr::write_csv(file="allow_gradient.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 77 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x04df8d02f912d34fef12a1b0488ee56fd6f7416c      1
     2 0x06c96dc43b739cd1d7428d4cf29c3caeb14d7e82      1
     3 0x0a98f97e89743406a8611e1b4219a073b60ffed3      1
     4 0x0b3e3a27d85996e20b2cdd0e7f1e1208816d05b8      1
     5 0x0ca4dc095608c6b153019f734106581c85f2dcc3      1
     6 0x129c6695dfe7a906bd8fda202d26dfff601f83a4      1
     7 0x134cb213549c9f114176adb9e4a78d6a272a3672      1
     8 0x1566ae673ae80725bcce901b486c336e6acef465      1
     9 0x1a9aee029a1b8c69c4404890c85715cc726746c7      1
    10 0x22fbaa2dd20b848084545515fb04b9c846942779      1
    11 0x248458947c120ca057ec028b3fe7e4b3f26fdb3d      1
    12 0x27b1abafb2cb065cfaf41b4b7ee95d27192151b2      1
    13 0x2947cd7346b3a372b357256b8955ebd4b5735987      1
    14 0x2f2242b447a54cb110b8a7991cf8a27054ee6921      1
    15 0x3511ae23ee25e2b97dce883c7d496ff5d18f1dfa      1
    16 0x3cb63b82d778105e43f064ed739b0655f1f0fb87      1
    17 0x3d0a1d20ac580ca8fa35888c92dd0859887f3398      1
    18 0x40d2f4399d23f9afb82d0a6b73055f13208614f9      1
    19 0x43b0bd27d5016e969193412933387b0dd4cf3e0a      1
    20 0x477ea7a022e51b6eb0dcb6d802fb5f0cfc3b4a81      1
    21 0x48464efe55fbcae8ae0c992b306afcf21d4910cf      1
    22 0x520e8d6c5b2dbbe62bf8a40653a314d879b19b86      1
    23 0x53006f95def268f88dc1b8216654ab56f3afd052      1
    24 0x53562e8ecee54356dc89ad263a444528b34d6c80      1
    25 0x575f6540c16a72696c14a17fa64f049992d661ab      1
    26 0x59068075a799594db03c0255eed68e8e121155c8      1
    27 0x5fdb5fdb61bc2975f3be446c5ae9c5df490f55d2      1
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
    41 0x7e5ab36876a267560e7191cedbe99ee7bc04bc30      1
    42 0x82139687faae8a29851902783e02e699de0e0846      1
    43 0x85603a042455d2f36f3ec52c1c756344aa70c5ef      1
    44 0x88ccdfe9dd047b4cec4c1102d2d803e2d8bf683e      1
    45 0x896b94f4f27f12369698c302e2049cae86936bbb      1
    46 0x8ba68cfe71550efc8988d81d040473709b7f9218      1
    47 0x8bd9c4b5ea8f6f14c9b14d830ddb67f3720d77f6      1
    48 0x8fc2326375174bfe3baccc98cb9906921dc9599c      1
    49 0x982d3c5223f6b5794fccb3208eb164d042cf2526      1
    50 0xa56c04347abee42f663eff9bc2d0147b97c8f782      1
    51 0xa743c8c57c425b84cb2ed18c6b9ae3ad21629cb5      1
    52 0xab2056903a7b62bac46f45a3d7a70ac799ca88cb      1
    53 0xae0d16586e5d60d334624c115216a52b9b1a0335      1
    54 0xae72c6a6fad9fa9d82d089e1ebf73b3043855425      1
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
    66 0xd007058e9b58e74c33c6bf6fbcd38baab813cbb6      1
    67 0xd3e401814d1faa8ca0419ecccbfee93ac7b15b31      1
    68 0xde112b77a1a72c726d8c914c23dceaa53273c351      1
    69 0xe359ab04cec41ac8c62bc5016c10c749c7de5480      1
    70 0xe3b41ae8785e4107cc69f988042ff4a66a367fac      1
    71 0xea39c551834d07ee2ee87f1ceff843c308e089af      1
    72 0xef5ab90a44b68d4f5e3f6be6af4bedb12cd2c66e      1
    73 0xf15a6b54e68884d27e1bebb1624d70c227b7d04b      1
    74 0xf2c5f1fd977dbd6de9d04bc4e62dff722d4bb1a1      1
    75 0xf8aace471723dba6514f6c6d43651611a0c34529      1
    76 0xfcbca52b23db28470eb97680d18e988d8b60b608      1
    77 0xfd22004806a6846ea67ad883356be810f0428793      1

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

## Allow Artist Phase 1

``` r
c(allow_shavonne_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_artist_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 399 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x0040122ed305e0b16dfc9fd8e0e9a74a3cdd5509      1
      2 0x00e68f02d78b8864d53c5d1bbdd39cf226b71bde      1
      3 0x0164b1e872b802bf1cb03839d62fbd2eb3aa0ed8      1
      4 0x0312d0cafdcfb9a89fc0fa2aac64ebfaf4838c18      1
      5 0x03baa102a7f591482307510b3bf14bd437691f09      1
      6 0x03d183a8fcd3efc790a6f855cfa96901bbb889bc      1
      7 0x055cb3f8f05e07af027d760650cd9a28a64cb69f      1
      8 0x0570b2ef81ed0f80004a5fdbfea348175b477841      1
      9 0x061a8fe8cb0b8bd5412dcc37d40c993405e1e923      1
     10 0x0679f9107298adc4e7cca3cbf0088e875e8fcd71      1
     11 0x06d392fd56c2a883d7e4cb93ff7c54c03b8849ff      1
     12 0x06dac6a2dcfa0f0dfb3e7e766515eef5b2c6399a      1
     13 0x07075bcab2709ce620a1694e1908ee86650f68c2      1
     14 0x077a1c03793897caa2c6e2316a676c1c6cd054d7      1
     15 0x07bf01c98bdf30b84cf744bfbd0fe0a3409d1096      1
     16 0x082ed91c65ecba6ac147b115f661b1c7b584d23c      1
     17 0x09566716532cdaf133693940be8808838920d47a      1
     18 0x09aca42d1cf0f4b2e19d3e7aaa2f8274d4f71cd0      1
     19 0x0a49fbe88cfc413181dbe93c9e3b6184b2264071      1
     20 0x0b1660d48b2369a884f553f44ce6f585ec0d83f5      1
     21 0x0ba51c45a9a0df0c2ff4139d6ebf5311b74cfb38      1
     22 0x0bc6bcdda2356b42ffd79bb9914f3af5d1aad07e      1
     23 0x0c52eeeaf5b01797cdadcc273b2c42ffb278a102      1
     24 0x0cb5697e7342a15ee23531d364996da89d8cc714      1
     25 0x0edb008aceff5f0bc20f515ed1ea9497d4a6f9ae      1
     26 0x0fa4909719d96d0d7a506a4b4d0f66770c13d447      1
     27 0x11027c71a31f0eb416c6a1067c43a61f45574468      1
     28 0x116611ebc0498763e2afcaebf876cbb14e3ac163      1
     29 0x1183634e867ff81231cbd0be24383508684c6672      1
     30 0x1273b62a0e80ed80ffd46c119555eca9787fa37f      1
     31 0x13290e90a317fb89f39df1e6c3702461dc2f45d3      1
     32 0x14e9e4f62a085750b874863c7ece73b2c89cd1c2      1
     33 0x1529b1fc1c5e6b4b0ffeffc28cb595df639b5da0      1
     34 0x157ae65f17c8eac3d76b9a5253108537025afdf1      1
     35 0x1699f86a565f60fff567f23fc60bb1ef9385b16e      1
     36 0x180669310d089fafcbee40479ee719752d680d4e      1
     37 0x188bd32f4c3b5ffaee22c2ec653f5d32ed27d844      1
     38 0x1a962d6a95cea8060b1e9f55c5bc6622315c0108      1
     39 0x1b672e08c3cc6486b1c01d1114d5ced3c5682882      1
     40 0x1c4004cc9c457e837556df65499a27465a97d47e      1
     41 0x1db2a8ad03956bdc4aef6d8ebf79c7dd8127bdb5      1
     42 0x1e47bc8b6c585dfff2e166257767050666151f0a      1
     43 0x1e77025ffd58d9e9283e4ceca98057a08e7446a9      1
     44 0x1e8e749b2b578e181ca01962e9448006772b24a2      1
     45 0x1fd739bfcb6bd70919d19ed2698a8d1bb77fe16c      1
     46 0x218863d8623e439138912c32baccf1d4ec414690      1
     47 0x21ca2a6294d70e4659113ec824a590a859315d35      1
     48 0x22099bee3521d6497c46a9b16f3dd04ab0e79304      1
     49 0x2334d91317950c19a6c21c69d69f5ab8a8f275fa      1
     50 0x23857b6c939baf3bfe9724abd6a269cb70febd29      1
     51 0x2404d6857d7e90c3ed9e74e0d372a4d6d7399504      1
     52 0x240cf70d8a648be133feb342d71e5e81c686e5f8      1
     53 0x247a5c5cd6b0a7bc09850c5f5221cca9b8327ebc      1
     54 0x2525f1a08a2d5f037dda969f2fa1b56e4b4b47f3      1
     55 0x25428d29a6fa3629ff401c6dade418b19cb2d615      1
     56 0x256843ea6be2f80d77bb104f74440aa74e77eaa9      1
     57 0x2572ddf0022cc447f89ec2707e9a2d16267091c3      1
     58 0x259ae15e3b07e051ddef0b76d06f136e0bc24aad      1
     59 0x268543e8a9639d8cc2e6f0bcde6d9cae85888a54      1
     60 0x26d125498742b99b521b838f31df87b22947dd5e      1
     61 0x27487c28dbd2274adda4597fa2180a9ff46e0ba0      1
     62 0x27649256dc615b4440053b075f6c21ed4b2c933b      1
     63 0x2891eebfd126c572b5c7c36864c6fd9292db3deb      1
     64 0x2a8990902b4810d78ce9f4c76cf80c31ad57972b      1
     65 0x2b2316f11689c87bc43ca606bfb1033d5fe86ee3      1
     66 0x2b85b841b85fe6bf05e1a41330fe13882ed1f286      1
     67 0x2bd5d2e3d5852ad9960638083cb7c9f493e7a597      1
     68 0x2c9670deac4ed35099c2389d5a71a7a7b2fe81a2      1
     69 0x2e70f8a381efd5142474e8ce61fa3d05d3ce0576      1
     70 0x30290242a9c96a8b5aea1b2668b6f9fa58730b0c      1
     71 0x30439021ed5b3bb247bf0fdbb92f18010930ee7c      1
     72 0x3060dfccbc0796b58b8ec6e75e8fa3f096a8837c      1
     73 0x31ff06a31995945beb88e18d0e68b93eb8c0af82      1
     74 0x3215d92f7f2a9f135df283202bfb713c7cd572d5      1
     75 0x32bdd37b01d8c99537a3af0e087458f805d3d2f7      1
     76 0x32d0b4ea44bc22de443e7e06d53e3fe35cceb46b      1
     77 0x32f12843e7dba0e9452f5223713bb9a332313d2e      1
     78 0x330bce303d27df0eb1b856da3464278db1db1ac5      1
     79 0x3383da7e1c64fa562e41bd2101276edb31d3250b      1
     80 0x3392da275706938613d08777d1f36db1e1c27730      1
     81 0x34104a743eec28e80913c85eb3bf0d1a3e049ef4      1
     82 0x342d23b3dbe624fb87f11ef799cec13efb8586a0      1
     83 0x35150df6dbe55ecfed3e30d9afca31075a6ddd68      1
     84 0x35623aa4ad645b06efc73088db30a866fc0fa82a      1
     85 0x36250e1943dd50c0eebf5c6423a19be1c361c3d1      1
     86 0x37769c5bdd7a43b0b52958ca51bdace1556c96bf      1
     87 0x3881c3aa678f4c914d23319ee78c011b1a13331b      1
     88 0x38ea6cdd98e0d5fa0b0e4b2e321b27fd5da7bfa9      1
     89 0x39c96a99eb32db691cf59282042b3a1a531bea0f      1
     90 0x3cd378c9b1cb5f147ebf1b2c2564118946ae4ba1      1
     91 0x3d1d8c0a831553a27e1884f532d69f59be25510f      1
     92 0x3d6fe40db46d13c45f9afd21d5cebea2df8be98e      1
     93 0x3e2d6712abc52ceb07bb316f9d3c03527a3d108c      1
     94 0x3e6b87ff2168d15794a865d09a6716415e7dbecf      1
     95 0x3ef5f421e155ed29e6fa815fd875bfcd5f22ecd1      1
     96 0x3feaa35631d40a7c17e395e6d2ef16f275183afa      1
     97 0x40ebf8a762801d1706d9d16a8abfec4c452d15e5      1
     98 0x417ce1efd8e10615f1cab8dd860aba83be2b730a      1
     99 0x41ae0d3d648d6538d3221822b4f622c6f817e08a      1
    100 0x4270280f4e8e1519eaf0c0e9d885e3c8556f16c5      1
    101 0x42b919ec278a917d1474864aa378d85b0b5120c5      1
    102 0x43247c7db36054ea50c64bb1d4bafb55703552eb      1
    103 0x4354a019e9e3471ed86c86191428fd931992cfc8      1
    104 0x4476ab2c11b74576aa3abfab19dde0aadcfca238      1
    105 0x457ae5d0c3d529b23f914bab554e812e3bca8d36      1
    106 0x45f2e08b35a959a87833c57c4bacbf58f95eefb7      1
    107 0x461ae8c33224aceb7f1259095dd2a334ad20f322      1
    108 0x46a4961bf2ff575f3a63b1c55231ba4fb5784d4f      1
    109 0x46e992299fb47c7ae51af7f0e7984a3f7d44a400      1
    110 0x47279fc734466ee8acfa50aed553ff79770c8779      1
    111 0x477b6f91c9481fc64e9080ffd00527cd53563dd6      1
    112 0x47be83cd4037b98366ccaaba4e10b1a5d5486f14      1
    113 0x4943cc7813da1697265fded90a2bfb749eba0d89      1
    114 0x49d6531aea6dced6595d50da685274766e4cfc8b      1
    115 0x49e15e06d0ec6cf74b8047d4213944b1b3505a2e      1
    116 0x4a22910f4db3e7bf000eacfa06126d5db7f0efd5      1
    117 0x4ae714d2fe6c12baa49de577e1198c9df8557bf3      1
    118 0x4b46f1e241838a910945a3fac242ddf6f5d7c041      1
    119 0x4b68b5a0cfc1fa1a85f6b3c0db33f546b6cad57c      1
    120 0x4b6ab56e6b15b518d6e3d4e642400823c689d20e      1
    121 0x4b8ee1eef0bd930c2277a60c839834b142b373d2      1
    122 0x4ccc0aa065a37a3589e2db6e165d2f8f522e9fa2      1
    123 0x4d255d9b31aa437a0df8d02565c99d0faa6ed49c      1
    124 0x4da48ac48b782a7c01e70065e4a51faf2c3f7b09      1
    125 0x4fa7fc217d2da64b617eed770286fcc48baf4022      1
    126 0x4fd71c1619ccaa1ad664575b61963cf86f58f550      1
    127 0x500d4fcbc6dc7ee9cea0cccf9d533de7f9228f57      1
    128 0x505d9317b0839e92352229b0e64c3e9101b70fd6      1
    129 0x55026f38172d0631d878de17746182b32d9f7732      1
    130 0x55372173689c288552885d897d32f5f706f79aa6      1
    131 0x567ed2bd79833ee86e73c947564a4f2fd02554be      1
    132 0x57990ea8a1978a01f8d5adf52f502568b3c8d399      1
    133 0x58300539ee6ece54c9b49ee0e9be84c5c4bdd680      1
    134 0x5936eeab739c6db9a0bb5de8b79a4bbee65200a0      1
    135 0x5a3d01cfd122998d13a952d6fb1a24f6998b7149      1
    136 0x5a7eab352a0406eb72063d9ba872561c6d1232f8      1
    137 0x5a957d746ea98b7b5df4355ebe2f94415ddbbf33      1
    138 0x5b10d30eba1d5450c5429ac8aaa420cfed997afd      1
    139 0x5b458e3ef22692096f2d64e2b650e584a1e64241      1
    140 0x5c3323efb72ccf5089a79ed61088064d02779d82      1
    141 0x5c736d849527bb15fbc0111f4ca13b4cfc1a5c8d      1
    142 0x5c835eb9ece4c2c5786251d787ca2ea8c5020b38      1
    143 0x5cc1e6229708533ac0f5e9e98931334341ff24c2      1
    144 0x5e451b2d167c2a8faf0a8b23cff27996a28f28bc      1
    145 0x5ed592557b6a29908b7d50305ca01904a4529cf4      1
    146 0x6027b4c9ad98ad5dc9ee984ed699a2079d401416      1
    147 0x60911f2b72b48fb221fffd1f9a96c9f9e2710d92      1
    148 0x612181772106aedc4114c939a36a2795ce101ae9      1
    149 0x61cb53ba546c5264cbe9324843a25882cb82e1db      1
    150 0x62b9c7c14386e2e3192b7830a863addea39457a2      1
    151 0x640c15cd2f9f9455607418194345ed6a2051fed2      1
    152 0x65b4e6473e7dc62e10bb40f9f8cc72e8c0c818e6      1
    153 0x65db65fdbc22eb99949c4c6ea4eba19d7d819a51      1
    154 0x66cc9900b66d06974c9c7836e41a176ca78ffcb3      1
    155 0x66f5ddc253852f407d334f8e90e3bc46fdaaecaa      1
    156 0x67ba5775a60b13254bf65ea67295dd47543bc5e1      1
    157 0x6954ee7ede83ca555663704fe7e2b1528ff2c619      1
    158 0x6969b743f0e3bfde2f97dae01670979be554d817      1
    159 0x6a652e2d4f81c068eebe6e65b5ed941a32045d84      1
    160 0x6b8ad90480d73dd77cd719ca637c0f0289b6665e      1
    161 0x6b8c6e15818c74895c31a1c91390b3d42b336799      1
    162 0x6c40798acaec7502c3d276cfb4e79423571a5b0a      1
    163 0x6de6a9918bbf7c0787863fe51ca385337579e5e8      1
    164 0x6ede3ca405ee95034428c0eb5f032ff4606be929      1
    165 0x6f1c7b49d030d1583345462edc0010bebda374a3      1
    166 0x6f22649b860105d80c78a52b8437b53c4d074826      1
    167 0x6fb9f7b424b9f82af25b19b37f4d382098b345b1      1
    168 0x6fd6454ec244c0b935a905740ec2a633e1ac3e0a      1
    169 0x7091d6cb3e3497664e29b0022d516eaeb6d9ffe6      1
    170 0x7193b82899461a6ac45b528d48d74355f54e7f56      1
    171 0x720a4fab08cb746fc90e88d1924a98104c0822cf      1
    172 0x72cd8bd5e5c0ac6e8e4b45a37ce3bafedfb7552d      1
    173 0x73bf4353cc4fc35f40e3fe53281374069bb1ae96      1
    174 0x73c20e18134b093df54418b5b63afac19ae21c84      1
    175 0x741e312e15d56d9aa5e99dcd276628569577d10b      1
    176 0x74a2682aab14963622425e313b9acb393aef4799      1
    177 0x74ee6dcd7ed3c172f87c450a7651dcb1f49ec7a0      1
    178 0x75cf6e0dc5294bef8cb5a6fcd23dc9badbfc443e      1
    179 0x75d077ca9c587d31063a8f97dd0b72aa624f9adf      1
    180 0x76b06f8989cb6c34fb1ee60a26d8d8a8350faa55      1
    181 0x77b6aaac56a49d9f4d3174bca5088bf4af280987      1
    182 0x7896295c94aa8432ef85e9a8b7f98351d1d1e291      1
    183 0x789ccf95ad3eef429d9a68809a7cc40893e5757b      1
    184 0x78a42a84bfe3e173c3a9246b3f5f1c5aa8bbae72      1
    185 0x7971e007a4e4d4dc1f8380f5d91d3f52b5e53461      1
    186 0x7b42a219bb14d0719757a391d7cc6aa7f371e144      1
    187 0x7bd439e3b765878d3b1674a2a053e9a3c72977ba      1
    188 0x7ccd62d45c77121e4762230074a934765df4824f      1
    189 0x7cdbb8e309ce7c701ed200be123f16c6f7c9e525      1
    190 0x7ce30498e564f081ca65a226f44b1751f93a0f82      1
    191 0x7d19667db8d6d2d4fabc0bc2b2c9c7dedfd3a5a8      1
    192 0x7d3c555a0324cd9e04b1c4f1de4a34fa7ffbae02      1
    193 0x7da2f50c43e5d848d731d3b9da0952a73bd1f533      1
    194 0x7da90246ba04fd6e8375ebba5a8162fe7d08c667      1
    195 0x7e3d9629a1769d88a07d5ba939c8d4486573af35      1
    196 0x7ee3acaccc605e4de444ec630083f739b3a73d6e      1
    197 0x7f24833816b827b19576c356c741d7dd79156d5e      1
    198 0x7f9bf33a3c5fe89bb40a14648d32fccae88e7758      1
    199 0x7fb98de44a955da8d0a260433e4c42abc4a99d13      1
    200 0x7fd5cd81a9dab831684661eec4efdcbc956546dd      1
    201 0x800ebacd1971ed9c50ae9b9412a78fe824bd31b6      1
    202 0x816730b2762e10d581c4b20aaf515f3f60baaf64      1
    203 0x81a8e4acd378100e897c2a07a931ad3e410e92b0      1
    204 0x82312ff861748b95011a7908aa4a89861a736f2c      1
    205 0x8335932a9bc745f3abce2d0092b33cd375ac2b51      1
    206 0x83af3b90524637c5b004f0597d23ee8d40631df6      1
    207 0x83b3ea919dbcbaf0ada8b9b5792b9cba2ee64c7e      1
    208 0x83ccbd7ce486609a392c100d3d14b4132750863d      1
    209 0x841412043d4e6b727bae0ea5ddb5cc73dcb699f2      1
    210 0x864c269fd99ebb00ffcc76205a59b20ced2f88d6      1
    211 0x86ad40b7b57551402e191eb2e51dde23debd9e13      1
    212 0x86e61399b54595ee73d97fee511de1a97556f56f      1
    213 0x86fef6ec5320f6cf9231f524ae89e198419cdc0f      1
    214 0x879cc1bb519374c9a8d499ccae5a4cf1c4f6ce5b      1
    215 0x880e4238fef89c661dcbbb92a406f3f1273dee02      1
    216 0x885fe55d653fd389585498574d03a8fa124123a4      1
    217 0x8889ebb11295f456541901f50bcb5f382047caac      1
    218 0x8a8efb393b82e6a61620a500c1600e1c39c612df      1
    219 0x8c5a48473d814960db5c51e5346e1fe6687f4092      1
    220 0x8ca0a67d3a84102328ef670d0dea5a8f907d5cb2      1
    221 0x8d8e5bead7020dc7892815c51ea18713238659ff      1
    222 0x8debc0beaeb233df0d7ceef26d27280146efd525      1
    223 0x8e264d87fc3df58fd6e261ce843ed2b7e7e5e307      1
    224 0x8ead57a6e24940ffff90882e7dfbb92f09515e3c      1
    225 0x8f08c2cd8a4b2146df85dc19cef31a166b4dcbdf      1
    226 0x8f0c509d4706c85be68ac70535b90ad256f5f027      1
    227 0x8fde9f4b8dd2781c85c31fa8deaab60bcd2c8060      1
    228 0x90238a0a150c47e326a19ad38f05700021fcfa62      1
    229 0x90335ee2286315185a0ff7108b5f7809ce6332f9      1
    230 0x90f440e449c47f4caf78c62c131dfc38ab491d4a      1
    231 0x91744aba7b6eed976486ad119df41d315002ff71      1
    232 0x93aafd2439bcf3d88baba32a0545f7193d014c31      1
    233 0x94b7a1139d6f3d96d37c4f58be954ffdc2b65a22      1
    234 0x94c47ed35fd041953352cd483b66ad1dba05e31f      1
    235 0x967ee5faa6344323c2898a657a56118b50a45163      1
    236 0x969fb1c38080b54b92febcaa9a64a80ad9996c2b      1
    237 0x9734223269742c4c683c93a507cfb5a97252023a      1
    238 0x9759cd43042bb2ce7ba22d3e2beb675153442d80      1
    239 0x9940f53d309903b8a607d08b01eb03075c9bc1ae      1
    240 0x996438a002e6f9fdc914a8b31e508466b00165e6      1
    241 0x99ed7190511ac2b714ffbb9e4e1817f6851ef9f5      1
    242 0x9b08042e20dc4e883c41e89813be918d6729099a      1
    243 0x9dac76101b5ccda5970e60402de31a6649115b53      1
    244 0x9df0154e48563a0bdb26987823145cb297fe9cda      1
    245 0x9ef4ca1a90361aee93c4638d142ba04a5a8fb08b      1
    246 0x9f7064dfe6ca5856b3afd7a3b3974686a35bdab5      1
    247 0xa02bfd4376c12983a9749e609b209a87ed3e287c      1
    248 0xa092d2ba3c0b07db458af6f06824ccc74934ca75      1
    249 0xa28a10a1d40f45e1a31cfdac66fa7cd6bd669d63      1
    250 0xa29597016513333ad8da8c55dc990256082d1485      1
    251 0xa3abba6c8f1380e60cf8afb124471bd6658792d2      1
    252 0xa4b61e227361a9cd9e62ca10946c27748a382cab      1
    253 0xa4b8decbe1a2d8600dc5346c7e3be5163a7d984a      1
    254 0xa7695409c5fef39a8367759a279386302a683b9a      1
    255 0xa989033ecdfe3cb4f243fdddd7dddcacf77b2a2d      1
    256 0xa98d2867de6b1dff44fe4b355dea098e81d06aeb      1
    257 0xab0ca98528c1c7911cbfa8fb08abf5973f61e058      1
    258 0xab182ad74430f05ea7fb0e8d87b21f8c508c8b39      1
    259 0xab6ca2017548a170699890214bfd66583a0c1754      1
    260 0xabc6606a864b6b1dcb1a1540bda4361a4f9a5ef6      1
    261 0xac104d56e4f8e0462f8c001ec7dde4c68deb596f      1
    262 0xacf0820183f5c10fbb0fd87b25d0661549084686      1
    263 0xad4bfcc8191c751686b9ac91adad1804b875e8f9      1
    264 0xae9495a415342d56d0048d5385a69e62a9ac20bb      1
    265 0xaf8b728ec26810a9ee9467cd1cfef520e9c386d5      1
    266 0xb00a0af5ed3dcec55e2222e25a151ae35408e0e6      1
    267 0xb03e5acbf8e84c5ee06b369bd6a8dd9afc3433a0      1
    268 0xb0e984d4bf1830dcb6c8c4e0f81d0869d2f037b1      1
    269 0xb0fa409c24c131bcb1ec8400a5142150a670e746      1
    270 0xb1636b8336e98cd896e9bc6f91ca6ebb3c81bb00      1
    271 0xb194a55bd4ac4281f42b3b390d3e2ec97db9abee      1
    272 0xb1af13b2e4be55ca69829a23c09a1f5663adf8d0      1
    273 0xb20c9173530b0afcbcb9cd515f4c69c4be8f60df      1
    274 0xb21763ce87472dc76fc3b485fca9d57df5d47bfd      1
    275 0xb2dab9b031bb0e27481d0f87905388c306907402      1
    276 0xb313378ec5582c25f0c99edc38b058c1eee515b5      1
    277 0xb394f1aa30ba874df3ae6097bf4c2474b2d8f3ac      1
    278 0xb46ba586a6733e510968612d64ddcced42ae09ac      1
    279 0xb78e99d2301ee621f4332a0d1ef759423976d8b6      1
    280 0xb7f4e738bb0b85ad1ecef26a2a733dc899da500e      1
    281 0xb802162900a4e2d1b2472b14895d56a73ae647e8      1
    282 0xb8c3e2e2a3465b74b7a57f8921a1c3f6e69302e2      1
    283 0xba0bf8e06a8b244905fb6653d4075790b3dc5a2a      1
    284 0xba3e6e7918f2b767930294545988b8143257518b      1
    285 0xbb4b54ef26b7b674d767f8d08318c404f3001b06      1
    286 0xbec5d88ef9491b661ef33a596eed6a4e5cdb00cb      1
    287 0xc08e7c5aae2df9fc176e1e9d63fd35b86e68aa04      1
    288 0xc1a80d351232fd07ee5733b5f581e01c269068a9      1
    289 0xc1c42a4cbd6a71fb03c681658ac85a60e09e0c9d      1
    290 0xc1e84ef02e240ebcedad362b20daa62acf6e92a4      1
    291 0xc2fa34a6fcb085f5e0295f233fae7fc90fbafe85      1
    292 0xc32daad3ca52a097c47de56cb8187ceccfa87606      1
    293 0xc3f1d077fa590018d3924ce8da1a085b4eae506d      1
    294 0xc40ba5621434659d042f8c9ef1f0f8d5e273e701      1
    295 0xc41dec3cf3a042a3684be9279d5067ab9d813033      1
    296 0xc4a496a7bee796ae81678bcb45ed985aee40969f      1
    297 0xc571e69aba3a52effbe4227921f5aa851d434afc      1
    298 0xc5ef1c84ea5c570979da35eb7c068687d15709ee      1
    299 0xc7cfe2de78360c25dde3c8c7525c4f4f6cae30dc      1
    300 0xc877d14d9632c58949a2df019b7627cae3ca4d0d      1
    301 0xc8a7dd90d2fb862f1db3a1f987f12e31f9d6ba47      1
    302 0xc8ef3bcd9c6556792f9d3a3874659b7271809f7a      1
    303 0xc9c673d4bd1ec66a8acdc8e209938d4572769624      1
    304 0xca38f02d7594dbd1c9eefae33bac5da04f79abf2      1
    305 0xcafcbf6e42bd2a953127818ca9c2d47321be01d2      1
    306 0xcb249211091af4fc730a4a315213563e8c698f9f      1
    307 0xcb9586245ad6e07585515d7ad43d19d163b72bf2      1
    308 0xcbc853efcf3f0a78c6f52828ad6a8091a8b302b3      1
    309 0xcbdf47d77d7682655c7a69406655ea0f52123f7a      1
    310 0xcfa85be45b38f69c0dc7967da750d9c996682d9c      1
    311 0xcfc8328e9ecac2d57694b3aa9c550eda37f099ac      1
    312 0xcfcbeed29e3e01432af963e222db8c17fc5b6042      1
    313 0xd112a65b977898540bd433d269ab25d1fd02aa93      1
    314 0xd1380a4e089950ee3a23d818e24ccbbef003a432      1
    315 0xd1fbaa0c31ff10ad82cdd5b5c528639461ce7ea6      1
    316 0xd1fc6c2c64268a854a766196cdab99bc04bf4e44      1
    317 0xd230c1ff8f594aa40bec61a3d902ac38f27a1e89      1
    318 0xd255cb7583c3150784973248c9435ad1c7f49842      1
    319 0xd2f86cbed86db85c859903ff5f110b15d95b1350      1
    320 0xd37038173a4af910457f084c9cc7c55ddb1964aa      1
    321 0xd3a8c8d2c56216120db211255e5edf5eda263897      1
    322 0xd3e2e76e4ab0322614fe7aaa3be0b04bed83d0d0      1
    323 0xd3e9d60e4e4de615124d5239219f32946d10151d      1
    324 0xd54c1841e64e84c9c63fcf6c60763f0c751c3aad      1
    325 0xd59120f2a160c9862301c1f3bb5f171279f95512      1
    326 0xd5ced726dc5145edf5168fe11650c5a4eb747b69      1
    327 0xd630319b817528b4abf1f06f1499cad237fcfdfa      1
    328 0xd65ec841a6227448e4f778f2c44ad9442bba9dac      1
    329 0xd732748071e38a48b4e9a3ff1b2e7e87c0a39c2e      1
    330 0xd739429bfbdef8f25280bee993c6f5fb2e884a3b      1
    331 0xd7eb093ad52e08a5defaac67e65486300aaed91b      1
    332 0xd8ce0194d66f6c612ad46f4c36a313f775123ccb      1
    333 0xd9b5021ba375be74b637fd9e0b8c666aebf2bd07      1
    334 0xdd0e2dcbf3d473e05270fb9630c3f9d96b66dd6b      1
    335 0xdd9a128c9adde87b976d01991d0f5b3f93e9c8df      1
    336 0xdde3565350cebe053f113b2194d2fbc6afdeaaaf      1
    337 0xde0dbef331728c733e9be5eeee1ab20531fdca34      1
    338 0xde49326cbd155486f47d39b1d2d6a78e1af6d6b7      1
    339 0xde5bcf9443d837220eaf7e2b200e134c4e1caedb      1
    340 0xde6098882888e7cc081bc2ce12354eabc762f4f3      1
    341 0xdf75365217cc3e75e7be6a234153e98efc75e723      1
    342 0xdfadeb6967791846874c74c3d9e6e3a30063040c      1
    343 0xe048ca2aa2b5fe1991487bbe46bbfafaf4234402      1
    344 0xe052113bd7d7700d623414a0a4585bcae754e9d5      1
    345 0xe08f5c4c9347015907b01a8dc108942ba5827e6d      1
    346 0xe0faf18f33c307ca812b80c771ad3c9e5f043fe9      1
    347 0xe100d308bea6c0ee54448fc735a18511dd74d68d      1
    348 0xe256d7ab482d88b04d676652385cb6787c360078      1
    349 0xe25da179a70ea62e778f1d3858a3191613f99538      1
    350 0xe2a3f107fff888f773c43544045e2266472a8d3c      1
    351 0xe3d9a03a6422b56e7097ff41a86045683f0aa150      1
    352 0xe4ae8ed5500fd43026af8f07542a17f03d876924      1
    353 0xe4d3cbd20bd9ab79a70a1612853154cb80b02961      1
    354 0xe56e18ec216f601bdb355c040c26452dc3f8d1a0      1
    355 0xe58ae07dad05d19e3a191f49b2c536b7c3a5b34f      1
    356 0xe58f4c276ca684da27e4d4d21e3b20803b33b583      1
    357 0xe726d0598cfbb879459451b9df5a481add1f36c2      1
    358 0xe739c3139255e4244bc0dd45813337f9e6a4787d      1
    359 0xe74166afb1a6c0ca3c355ba33965a8c7fc3e2616      1
    360 0xe7e49bbdb50f5b9ec6c4ae72b77043eb60044dbe      1
    361 0xe8ff464b954d12db575ad0e5e5a7dc9c041ee6d6      1
    362 0xe918c668befb8140c7f4968bce6a7f5eae267e1d      1
    363 0xe9a567610a8f4efc33cca1f319d62b76b804c5f1      1
    364 0xea8718cde715d7847210cfc733b568be06d1fd47      1
    365 0xeaddbae2651e21b4ae261ffbfcc81970854c4ef6      1
    366 0xeb889d3ffd7170cd1e25a3b2cb0d522b8eaa5cb7      1
    367 0xebd85e77c21bfcf69e8c206f05990748addb4252      1
    368 0xec786e4df43f033e86affc450b7d1895e46afeae      1
    369 0xececcb0dbaadb9be687f2602ef55305bc56837b1      1
    370 0xeda4aeabae2d559e7c5aed03cc288bbf44a03134      1
    371 0xedc1040e47f75ff85867ff4358c6229801af31f8      1
    372 0xeea89c8843e8beb56e411bb4cac6dbc2d937ee1d      1
    373 0xeeaf0e02035c82b6b053c61b7bd38d051964c778      1
    374 0xefdc42f1d06cb928974d69a2eccfc2da212daaf9      1
    375 0xf05471e692eac2639ae9e6aac061c2cdcbaf9610      1
    376 0xf0ccd28c325f62ac4893b61fae0bbd962e4d36cc      1
    377 0xf1aae566f12e1333a7d7e0a994ad2c1298d4b20c      1
    378 0xf2439241881964006369c0e2377d45f3740f48a0      1
    379 0xf33f6c5302fee8dcaf6363f6862071092b3d56b0      1
    380 0xf34149c2d2fdd3955593f749755ea2a4f8e9ea41      1
    381 0xf4408b493df70a9857dfe8daef5f4cce4999a761      1
    382 0xf65ffed29a2e7ca9554cfea52ea500b5adc5fc13      1
    383 0xf6b1152426018c9ddb29f861fcb3bceeee61f635      1
    384 0xf7c454feb83b0886f21c2a1fed78ecd246529789      1
    385 0xf84277a04392ed2d12349a51d82e72d120fdaf1a      1
    386 0xf9a80cea61739e846358f8547dbfb0c80bd8e969      1
    387 0xfa01b52bc3945fd8623633779dfae7e5adee387e      1
    388 0xfa848e0d2134f0163e46b3bcd7edab9f8efed57c      1
    389 0xfb9699de6f7efc7f7d64a40852a68602ac7ef403      1
    390 0xfbc9372033a652f61fd35e9cb3fd73d071844aea      1
    391 0xfc0d949c46a1fdedd6b6e318040b18b6082807ae      1
    392 0xfc2964681367372487daa3489e252febb304e349      1
    393 0xfc2ee91d24fd57ab5d382c92818ccbcfeb854b56      1
    394 0xfc92b5faa50350d8dd1aef8573c50faa51eb9260      1
    395 0xfded90a3b1348425577688866f798f94d77a0d02      1
    396 0xff03c602bc0c2a08eb9b0ca605040391cd397ccb      1
    397 0xff4c60814adc5ee4cdf9a57e0944b1e4678ff09c      1
    398 0xffec39d3c84f2ac026a57b6a83f08bd169aad9d9      1
    399 0xfffcde6975788ff3c5ad6bc5d00c3fbc9d42483c      1

## Allow Memes 1 Phase 1

``` r
c(allow_memes_1_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_memes200_1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 200 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x017347cb75ac8725608e593ea35d83f9b2b3cfb8      1
      2 0x01ac9c8027c3c91c49b33d1dd084ed5b87c7dc92      1
      3 0x039649f7c2f548692184da3fedf316f58e8356c0      1
      4 0x03c5e3ff9514031f910c9bc9ebbc7839e02b23d8      1
      5 0x03f4cb9e297ea659f30e09341ee7155a7d136398      1
      6 0x0da4438a9cc8d41fe9c0c6efd302a173ed967da7      1
      7 0x0e3e001a00c07e2cb3219d1ec0c47cc532d87804      1
      8 0x0ea8767de750442ec4a34d81bffcfd4417396f47      1
      9 0x128ad42b82c752c5b4c7f679231d8956c98038eb      1
     10 0x13928eb9a86c8278a45b6ff2935c7730b58ac675      1
     11 0x144bf0aaf16debadcd2638f572ebe42a92f5bcc0      1
     12 0x147f466fef6bd04617fc0c8037ea01c73bc25a3f      1
     13 0x14abeea8db07c46b37a79bfa856d4c2da6d3e6df      1
     14 0x16f3d833bb91aebb5066884501242d8b3c3b5e61      1
     15 0x17e31bf839acb700e0f584797574a2c1fde46d0b      1
     16 0x18595f049a9c5c2019efda113009b0ec1c89ceaa      1
     17 0x19e7a132bd4b42f580f546197f42e19c42cdfe6c      1
     18 0x1a7d7e6abea492e7b9a3cae667f468c72e884671      1
     19 0x1ce68e5912ef8cb4e83620a50b70edc900ad4759      1
     20 0x1d10b0166a761f269adf864ebdaca9be445ca637      1
     21 0x1ecded4519a9688ef4bfb5f15811c52f048ba1a6      1
     22 0x20ebf9ae7c66da763fc1ce86062ce883389c8e23      1
     23 0x215cbb1b60e2cc1f1e8626cadca386b278aa0dee      1
     24 0x2177da04f9479496c2292d6344d306aa49beb34a      1
     25 0x21b9c7556376efdbf9ee316a4ede0c8257fb7533      1
     26 0x23602ca06e977c86339ffddad74966e824ab691e      1
     27 0x29c2188c6c318ab5cae4ae4b11a49edf2ec9ab0e      1
     28 0x2c45f65d254e85584b47ec82450c91a30cfc6be1      1
     29 0x2d1543711bd262108fd8a1afb43d8871f76c134c      1
     30 0x2d8b81cde837334beb89a9c0a88179ad234770be      1
     31 0x2d924c0bc8033e7866b90ca2a76cbf4b5714b11b      1
     32 0x2ee5fc834903186949c75e0dc10ec245397bf1fa      1
     33 0x313c8673ff69dfd7be2a46bc540305d3e2d233b0      1
     34 0x32a0d6b013cf8ecad1e37e99532570411d398d05      1
     35 0x32e263cb9003f7fef329080bdcbc82f5cfd6c02f      1
     36 0x334cb32bc18ec632ab04ee55157aae1ff165e093      1
     37 0x343e85ac3009ac01538d4251900ce1a2b7d7ffec      1
     38 0x35d7dd9230a1f079c907b53e7c5822b34e197a1d      1
     39 0x367dc97068ab54ba1dfbfc0fad12fbcb7b3a0d09      1
     40 0x378dcff8e6be1778b04f5712d686517bb6a01927      1
     41 0x37c14f24c73a9e151575a0af3dd2dd5cbbca8a29      1
     42 0x39bba3c54a3412127bdfb0f218efea52b5d28300      1
     43 0x3bf55d8aad7ca0c5cfe1f697d7783437b9e034fb      1
     44 0x3d3af253b037d3b22c4c810673c5d14de16d1af3      1
     45 0x3f4373afdde3d7de3ac433acc7de685338c3980e      1
     46 0x405020c797a64f155c9966c88e5c677b2dbca5ab      1
     47 0x40e6d6e798dc878e12987ed978f23c2391f1f570      1
     48 0x410a9f44948576c60f94b165f63eb79ce769a04c      1
     49 0x411dd34f17d5b6398f155f768ed27c32ad194862      1
     50 0x41254fe73d3b49f0fe3cc7743b4d40ef26bb338e      1
     51 0x4269aadfd043b58cba893bfe6029c9895c25cb61      1
     52 0x42d38ed60a64d0b8c36f190e185d17db3617a091      1
     53 0x44f301b1de6c3fec0f8a8aea53311f5cca499904      1
     54 0x47a79ee280b4a6a8d68a3ba16e4082a765a2b20f      1
     55 0x4c48d00997c6d23d6b47a9ce8caa6027d0068e42      1
     56 0x506452ab0dacf04db0ab1544b6be4019651ada90      1
     57 0x511c85042524a41bbf30e7a8533a84b8c5e55f43      1
     58 0x52690f90740621f89f58521433e9b0921d626708      1
     59 0x527bb834cc5c8ff730c673880e51372282b06e14      1
     60 0x53f1b3d8990472d1adcd7b76348e5508db17cea0      1
     61 0x542c5648d46462207fba8a4bd7d890b4154b722f      1
     62 0x54669ca58bf3a59caea8ae5135db491e4738f65a      1
     63 0x571493b6bb0b99dda41e2da1774b8c9b5bc302af      1
     64 0x5e3118e91b0a02a5e2b92e49700c63434e94f0b2      1
     65 0x5eefc3ae27be1483ad6b864b45ed72fc39e6bb6c      1
     66 0x5f656037e30a003862cf878db24ab5f537177fd9      1
     67 0x614b89f072ea263a9387460963142e73548fbaf1      1
     68 0x670a840848eba22eb2a090fd65e1ff60c3ba9e5b      1
     69 0x68d45e76a87e9f1706e0d36b7815c8dbdb7e6aea      1
     70 0x699990a8e7ada9e92c932d6e8fb365024fc74b43      1
     71 0x69cd43dd4ecf3a076b1b9d2cfd41589987129bc0      1
     72 0x6b29380bb9955d5b2df1624c5c6a72a5e69a6b80      1
     73 0x6b2aa5fc4508ec023a8e355959eaccef5e3a250b      1
     74 0x6b6ae848f555f70944bc99d736fe29fcaecf8b23      1
     75 0x6ce52498047b8279ccc7c25b41c95cd482525e54      1
     76 0x6d1db4a7e83dae0eee7e95d421722d46d2a7e94b      1
     77 0x6e5632d334b861dfe4312c7ba34e175b60be0b5a      1
     78 0x6e88c16794cb212bdcfd6b6f8901af2f83237d3b      1
     79 0x6f566a6672f615c2975f6c48224c46153e12ffcf      1
     80 0x7525e71f51bda1fbc326000714d2fc68ed5aed6b      1
     81 0x753c892626c4fa09d1aedc0e9b52bda97a4afa00      1
     82 0x7c741ed01ee259feba4b7f9cac114f48bcafacf3      1
     83 0x7dd14501c25c221ffe213d7d56d225f4fe411038      1
     84 0x7e737324cd6702e1b93c9240072ec9cc036dce21      1
     85 0x7ed69ff055d746d451f675635e9bf773accc6a97      1
     86 0x7fdab8c244e0b775edeeb9ebb8356b59068c6873      1
     87 0x808421753a181e96812796b7ab43d3f356cc5a77      1
     88 0x80a1c9fdc26199a69d190ebc8ad287ef48758977      1
     89 0x82139687faae8a29851902783e02e699de0e0846      1
     90 0x8623a32af48544b055fb6ae64f33eb43edf091ff      1
     91 0x8668681d2017c2322d7ba37070a8ea223ef3729c      1
     92 0x87e91205adab9e2b0be9f7562af3f5b42fad0838      1
     93 0x886478d3cf9581b624cb35b5446693fc8a58b787      1
     94 0x8af0b9a9b751e086122bc340188bd9d99b8c7ec1      1
     95 0x8b4567fa8c4715c27a682215a031033a8b544206      1
     96 0x8bc3757a675be4c2e459813769992ec2c60baaaf      1
     97 0x8d12c02671848b17c18322027a2578ea7afbb702      1
     98 0x8ffa8d00db5df56a20c30b737c5efdaabe140df4      1
     99 0x90fa40f24362c24de09b1cb86df9f0b1f1b15a90      1
    100 0x92b1b677728e36734d59067a4ce53e5bf1f3a2ab      1
    101 0x957143815f0e1d1b1a31b2bfcded5b416ee675ed      1
    102 0x96de627be6262ad2e19553824aad1af6ba1ebe9b      1
    103 0x96e861634b4c0e0a9e7c6a65dec549cc2a8a0e56      1
    104 0x9b7c0814eb341e6a4ca673148e8b577b13e70787      1
    105 0x9e1e3857fb2484379858b9daf230379015a7a100      1
    106 0x9f3c44a7331ca9da2d3432bcb0df91186a27e3d7      1
    107 0x9f6ae0370d74f0e591c64cec4a8ae0d627817014      1
    108 0xa04f4a4b7306cb72f30828834db01699362a4989      1
    109 0xa5214719feb1d07b66292a5d4d0a2712bd048435      1
    110 0xa5ce27ca5e31b1de92db2d2bc109b3e23cf1d4c4      1
    111 0xa71000e72d38e6a84d7190f59fd3dfc73931c0e8      1
    112 0xa711818b11bdd5797042ade80e3a59687558a4e1      1
    113 0xa7c342af335ea7e0747452ae2e3c1901498a9a76      1
    114 0xa94ba6f81ede0d81a5252e71916b5c5567ad6a01      1
    115 0xaa1d3f5d45333e40467e989d472effac4da00da9      1
    116 0xac02b3653a6fd4d507df142a0337951e39c5619b      1
    117 0xac6e4334a82b8434b0122bc3c9b42d0a5f190d4a      1
    118 0xac772daaa5079e005eec3e53314d6d1e9149dc87      1
    119 0xadebdeab678647a457743ea3af98f8b804e45c24      1
    120 0xae20574304b6264d3129dbfe5df72eead8c5b208      1
    121 0xb08f95dbc639621dbaf48a472ae8fce0f6f56a6e      1
    122 0xb26c452e8ac20ae7445fcf46b6478d3984f5d8e9      1
    123 0xb31a4974499daad3255206daee7d1f3595fa259e      1
    124 0xb37c5603fb0c6c2e2b62d0525affed9e1e5a6a19      1
    125 0xb42ab92f5af0f162fffefa2b1e12702ce3fc9e17      1
    126 0xb4627672ee52660a9e453ec541834e04583f3602      1
    127 0xb5976803fe7b4fa10834e98433f38ba208c53c0c      1
    128 0xb67a577a1855dc3a43df1d94b05a8f5d29570f89      1
    129 0xb6c7e03ede87692deb1de7e40723b952e0fd3773      1
    130 0xb6cf25f5cf8a1e1727d988facdd47f1dfc492caf      1
    131 0xb7546d1b04151e1e96d38275af66eaf984fdad2a      1
    132 0xb8700157128ff544c46cf35dec5a19120baa8eb2      1
    133 0xb8937891f08af9854a5ae7a5ec0cbaf4e68acd4c      1
    134 0xb9ad9d091c6841e640dba4cab02baefaf1134cfd      1
    135 0xbbc6e7b905d7794a2318e22ec011bf36b09e1d2b      1
    136 0xbd3a5d12af9fd82fb927fd26216e4dc2722d2337      1
    137 0xbd751cac1fb27d930a6a0394d97c77908d94ad5d      1
    138 0xbe8fe12b9eb1ca2a593e6c070c71c294b6fe9f00      1
    139 0xbe9998830c38910ef83e85eb33c90dd301d5516e      1
    140 0xbfb7969e75ee4e58620f74135d167f5bdd60800a      1
    141 0xc02e6b0d0c1a5d8cd26beeba0fe8d76c5d2f19b9      1
    142 0xc40d07925d1ac5500e9dfc4e0a05eb24a75d195e      1
    143 0xc457fee0564bf3eb972114762f62ab45c0e3590b      1
    144 0xc522289168311a765cf17c067f0118578c99cf08      1
    145 0xc6411ff69f1ec6bfb4b082f47db61dbedcab250d      1
    146 0xc6cf5a020bcfc65c6f8181c04cbb5ef5050fe28e      1
    147 0xc78cd2e1e8ad4a288eddafb139c9d0891ad01ae7      1
    148 0xc8cb180415b83f3f9a61691d246fe39a74c6a41e      1
    149 0xc8d3e10a72d2d846ebd134dfbd23e14cc26a4ed5      1
    150 0xc94a284808e31b9ef43f9c9a48933e5a268cff9e      1
    151 0xc9ae113b8451d0afc8a021d796322d545a7e7dbe      1
    152 0xca339bd4739227f71b646f8b23ca098c86f6c3a5      1
    153 0xcaf3365d474690a1ac6643d3d6ef44cb0c6deec4      1
    154 0xcb85b581caa71949784d2e826cf9df391c244b33      1
    155 0xcb85cf75d53fceb37af86b475903817db2b99d17      1
    156 0xcd30b6de9dadb0a1598eb12a4ccf34b98c98c1df      1
    157 0xcd69150ece65cf880eaa7b91ca6fbb7d38e97cc3      1
    158 0xcea266acb92d5603dc53b5d6bc6e568dcde0d311      1
    159 0xd0b2d2d86a45bf2f13a3af83df6e7b5fe98050f9      1
    160 0xd193a7d7b231c204b76b9d638768ea602de515f6      1
    161 0xd2ce17b0566df31f8020700fbda6521d28d98c22      1
    162 0xd2f2a56affb67a9ae2499ab9df1689ee78fad89a      1
    163 0xd3069c9e486a8b77add55ae3ceb3a4b138fb76c7      1
    164 0xd3364ea1c742cc2d396ac65e1723b8d608a939bb      1
    165 0xd36590461162795ee33099b2076a0d4e017ae17c      1
    166 0xd3b37c6567e2702aa727cff4ce5939b4a07a2fde      1
    167 0xd5ec003289265705727b622f1700fe814e54ca67      1
    168 0xd74e767c77d2e9f9e467e7914f2379da81b63a44      1
    169 0xd7e32b8a326ffd9e784a1ee1eea37684a7512171      1
    170 0xd939fe2e4776ccaddb823a26eacb45c66d8ad5e9      1
    171 0xd981a38d80de1c1528349b841833d3a9a745cb37      1
    172 0xdcefc9ff1e47e458e56959154c1cdcf56003d30b      1
    173 0xddfd836f7c9e42055b1f6ceb005fee4c7882f161      1
    174 0xde112b77a1a72c726d8c914c23dceaa53273c351      1
    175 0xdf36b093cbc06522b68105c3d9141db188e980b6      1
    176 0xe1c22662f4f1320c6e5319d002b3c3d52f0d0135      1
    177 0xe418a9a5e49dde0d13e1ef51d4bfb7fcc51c28df      1
    178 0xe74ca58ef724b94fac7f2e09e2e48536a5c1ad03      1
    179 0xe776de32c9dc8889e2dcf1cf0606010a51d1abb2      1
    180 0xe7f31a4516dbebc0f443a8d4ac8b22d1a8f096e0      1
    181 0xe8a05c7f4e9c1aa060cf941dbb50381f342d7d43      1
    182 0xe96ba1a10f72b58934b9ac0e3fb471d2ba65b757      1
    183 0xeac5f9b3cd48123a69fe69ce93a7f58100a56552      1
    184 0xeadd2d51e75a8b42302adc2aff0157a4bf626adb      1
    185 0xecea65d22233e7beed752a06c46cefc6c90a1d7f      1
    186 0xed690606f908361c26bd2b1ad4fa31ed201dca4c      1
    187 0xee2c055f7706b9dfcd98cd5a23d5629d6316c0bd      1
    188 0xee91d62eb5aaea933efbfd0790613af5db305006      1
    189 0xef5ab90a44b68d4f5e3f6be6af4bedb12cd2c66e      1
    190 0xf12e159643edeeba920518cc614820ab5726335e      1
    191 0xf28fc8d0eb98a34f2b03d384f7db6f1e7b877a33      1
    192 0xf2c5f1fd977dbd6de9d04bc4e62dff722d4bb1a1      1
    193 0xf4df143e90c2095e173f861a22f1e6a2503d06cb      1
    194 0xf84408070b4de16d6bb0ab2fd8b19d37e3fd1422      1
    195 0xfd849d8cf365fe98bc60025ff7fab45cef0f557b      1
    196 0xfed52d251e31178ff8bf4a0d611721c544f74fc0      1
    197 0xff4f0dbc9ee7583c0c61c7333a5daf8d0ab4aeb3      1
    198 0xff7e887f2b7c7594e601ed1c2e44b55bb80f774b      1
    199 0xffe6832850476eb6d5ff184d747ed84f1b686aa9      1
    200 0xfff39900273ffb1045c7cfde62df1720b63fd6bd      1

## Allow Memes 2 Phase 1

``` r
c(allow_memes_2_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_memes200_2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 200 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x0007fcf5590179336c826a60c80976623c7bbc29      1
      2 0x017ffb7c9e5aff47d9569265d7c78b2494099782      1
      3 0x01e8e9927d7c6b71671865f05783c8cbe04cc559      1
      4 0x02009370ff755704e9acbd96042c1ab832d6067e      1
      5 0x036302a75c0f7dafe15f83c8a1faec282a74a03b      1
      6 0x077bfc14dd6725f260e1abfd5c942ee13a27091b      1
      7 0x080ffeaf914180e18f69092d66de11925434b540      1
      8 0x0c358331732fe7c1a19cf78199c87ac9670c0ab9      1
      9 0x0d08c74268c1260d9b50175c41738e1a45819700      1
     10 0x0dbe146db9c963bdc56d7445e293c7c3119fa2a1      1
     11 0x0fb4e7dff29c22029975373d1bbf5873ffc3c61a      1
     12 0x11924c505ecab6a2434bdfe94bc0ff1ca081facb      1
     13 0x1236b147e44366e6b954b344cd9afc72bf71b34e      1
     14 0x12c4908dc21e88a8f268c3f363098eed251c1aa3      1
     15 0x137f41ca27f570d4c50e56d5c20ae2807225f09c      1
     16 0x14bee91efb79a8eb2332c31177fd3a61481ccc99      1
     17 0x157234e891ac0d70fffc76ff8e399c92ec264a5e      1
     18 0x167e05781fc728a22155019293fce4df335e782a      1
     19 0x183abe67478eb7e87c96ca28e2f63dec53f22e3a      1
     20 0x18eb4aece6cff56f3bd4ccd844491d55b6ab21d2      1
     21 0x197fe3ee16556577180f6578050802106e8bc446      1
     22 0x1b008929f807de617552a72a62df98e0953699c4      1
     23 0x1be1eac561b34c9b5e45eba9c3fb51fc5fc2796e      1
     24 0x1da0f2a58e65d98a8d04ba3da56cff9f5ace5615      1
     25 0x206b0526f4866e7580780015cbacbb4a8dbb8c80      1
     26 0x21d79c2be4ab3de2802a60da66f86d497d06102b      1
     27 0x231595e3673a10e846803194f4982e1cf3389161      1
     28 0x2595aaffb353e812bb5cbb63f7f62e36fe528f02      1
     29 0x271c6f480aeb26672b915a7b33159ce8f1e97fd3      1
     30 0x2733cbe186440511cd0aeef6dd37a67dac40899c      1
     31 0x2745bd23d9b23edf1ff4b695729c4804c111676c      1
     32 0x27e037e0461e3d5587de786ea23441d83772353d      1
     33 0x2c23b2ea134c3dd6d9a48676a9a41c6ade71adfc      1
     34 0x2cfdaf06e39e5d9d3c9afe20362b725b41f68725      1
     35 0x2db1307f1487c2586473f570ef6832d6873a0053      1
     36 0x2e8f6f8662593134dc8311e312885921855332bc      1
     37 0x2f44fb58135ae5d3793803c73d471c7cde4bb774      1
     38 0x317c32ca0309bb8a1f9d8a3e2836544f233c841e      1
     39 0x33a34e27a81436ba9d79276406a285e89a8bd8a8      1
     40 0x342522ae61de25d48c66807a2cecac4681be3d33      1
     41 0x348d43bd8daeb9f13611d5f0838225e6a9d89bd0      1
     42 0x34b93462e65303f3857460971584fd0d908f2f45      1
     43 0x35f4bbcc8490671edf37877684f5aaadfa4235f7      1
     44 0x37769c5bdd7a43b0b52958ca51bdace1556c96bf      1
     45 0x37dfaa62f0fe5e6ebe990ef1a0722f5279962e37      1
     46 0x38b2739bcb869494cc7953c79c97e3bcad7eac04      1
     47 0x3a4c619284748869eb3ab16494b461256b72f875      1
     48 0x3bd835333aad77686595d734ad5b80934d8b026e      1
     49 0x3cd8e762ef9b906000e23bde900f5c9061d8d4f7      1
     50 0x3f358b4e54385ba4fc8dd9ea718d360db5c7fda5      1
     51 0x41e12756498322b479f889af2b2f4b29a85d5605      1
     52 0x4212d149f77308a87ce9928f1095eddb894f4d68      1
     53 0x42617123b481f61df6b5b8a6484ba0a4e6929279      1
     54 0x429683282e5a28b81aab1f9126b8d5e4847dfbeb      1
     55 0x42e5abe19ba467b0b32a4d042d3dfd4ba2e196af      1
     56 0x4525b5402a6d9eefc656e828b9391aa9dff2446c      1
     57 0x47836f027d427e78c06f24eabfdd1d2089fdfd5a      1
     58 0x4898203e852b3ed44cc3e8d37f702fd0a7bdac9a      1
     59 0x48be4681972473b498e8b686d38e04826c26fc4f      1
     60 0x4a15e3d354043fdefbb8fa77a55da211f4058190      1
     61 0x4ab5189a020ccd88f05c36de69e4d868d29c75a9      1
     62 0x4b4d75b8b0fc7528ea2614b2ca83555824c07867      1
     63 0x50002a9b8e9938d509de84dc3eb3aabfbec1451e      1
     64 0x513e0a472522e4bd8023c032bbf01e7fb1fe0df3      1
     65 0x516c3ce95dfe03aeb3658a8f25bbd7b8c5970eaf      1
     66 0x527136cf29a1df9024a6493f219a1da64398fdc5      1
     67 0x541237a3e049f2ef1105694b491152f450aba4db      1
     68 0x55adbdf8a6396356de42aad937ea79caa2cab88f      1
     69 0x56bdc5fe7f9752e7f5a381e394acfe72a724462b      1
     70 0x592e480e0066a51eb981b532e275d6576e5730fd      1
     71 0x5c99e810eb0d87f17b0e467de52b3b2b4b2a2dca      1
     72 0x5c9e2a6fec34b510996a8e2a3d1e2c47a382a8b9      1
     73 0x5d63cbbd633bf02a9d22fcba1cbe1e7926a60851      1
     74 0x5df6dd99718f32ae362b41e78b7bbf66ebccc179      1
     75 0x618d17fa59c67aea20a88dc75d4e28e728a6ff28      1
     76 0x61e814fe997f0b2816fb9ac3c7df3aaa38d8ebb6      1
     77 0x632734882ed0127fbdf2666478df42aa916bdc84      1
     78 0x634ae4b57e6246f0318257743e5255648f9473a6      1
     79 0x66567f4ec7743e58713f44f083da3de78a52556a      1
     80 0x692997405c1cebbc56ff3416fa0b59ab97fac696      1
     81 0x6a3d9412a705f5531b029ef4a7b8040e1eb84ad3      1
     82 0x6e1ac9b3f4499a73385dd8d2daed6449406d49f4      1
     83 0x6f0735bf1e6c69030d6990cdd580345b370eb50a      1
     84 0x70e680b9493685f72e76243c09993fca768eedf1      1
     85 0x722e2e4c15be1fdded3c86f4100bc32b181827f5      1
     86 0x734740c70db02f5710207f1d12df47c710665206      1
     87 0x78089705ed807984cf3edf9149c9369e8fabb9be      1
     88 0x799c5516c59312f229af008c3e09eacfc37dd5b1      1
     89 0x79e561168835c783240a0637320d308897bd0922      1
     90 0x7be800978fafd15f6501dfc64b27fc74a68276af      1
     91 0x7cdbb8e309ce7c701ed200be123f16c6f7c9e525      1
     92 0x7f3b24d4ee18c5f92cf83894a954737fe5752fad      1
     93 0x7f94e30381aa6657c45833ec7fce2e493c1888ef      1
     94 0x7f9bea812b9b6c3c4b74ec8aae849a5745cc3ffa      1
     95 0x80cf63b40741e041515e6ba6a4d327088540c6a6      1
     96 0x8451b49f03c80b8d4d116090ddf10047cd173d55      1
     97 0x86f0a82dfd9745ec23bc8a72c819193e74962eb3      1
     98 0x884b9565d3a9d7265211f9354170a4f12ee2c4c9      1
     99 0x892eaf46a9c1cb75c3f230420dcbee6320c26932      1
    100 0x898a4ce06716dda5436b3ba315662c366c3fbdae      1
    101 0x8a730bcc5572d3cb7f7f45568161fb28b3242d15      1
    102 0x8b17e8b3f8315e73986f0c8370cea4f3a974a532      1
    103 0x8c4138dcead987bb50929ecfbeb39ef3e28b4c76      1
    104 0x8ccb07293755004942f4451aeba897db44631061      1
    105 0x8ce2b8f8b37b350feb8236f244ccb8ccb6988b36      1
    106 0x8ebd84269464b6ad7353d192f3b487e69d607e05      1
    107 0x903b1b960ecfb37d994fa0cc537777d8ed766656      1
    108 0x9045ede9bc11bc1148a0743ea518e3f36f62bb23      1
    109 0x93549cf8047b3e305fb10df7110da213e9429f9c      1
    110 0x94db5f225a1f6968cd33c84580c0adae52a04edf      1
    111 0x953b0f8afc70e0fca9e2e43b0a4914be002c4e94      1
    112 0x957b0cd4e9851537aa7cc84e9c7b22bb68d949d1      1
    113 0x9929fef8c8f3e09e6e7d9caa69e50ce185a95a69      1
    114 0x9996b56c2dd22e58d53a10d38bf299fb5dea07a4      1
    115 0x9c37bf5faf485b3db73c95300cdce93410b83792      1
    116 0x9dbc84d7199c97f9adcc9b57439d69c5ca9ad103      1
    117 0x9ede39c1dc05ae3f37b622a71fd10d7b95d8809e      1
    118 0xa0a43e56c6df95cf01d089d1284987468f8ddcff      1
    119 0xa0b35fe7c9819211896d51b670ffebdbdce67fe2      1
    120 0xa1ef5ac40d7ea6de39faef3cda7bdd82e09240d6      1
    121 0xa21e1e6c69dfed02b6a69c4c89ea46a1e6e31b86      1
    122 0xa222204acf1be4077d34102fab38a759060b77c2      1
    123 0xa8806ccb79069c976e37ed1950ada0eacef6a289      1
    124 0xa8f1b7bee4bb60311511144ecd6dab7b04ef2735      1
    125 0xa9237c4eeebc879208e59c867cb6d7cbffc1df30      1
    126 0xaad4210b800f14660ef4068029d428936ebd21fd      1
    127 0xab25a2c1a37e1f1fd689a8a9ea61eddee054f0ce      1
    128 0xac1a04679039a1718d3820fbc254ce29269af784      1
    129 0xadc9e7a7d129be37bb89d05defe860ea28d4e6fb      1
    130 0xaef63b54eacbdceff0b63609623d038195008d88      1
    131 0xaf12a9af19331fa9b89915537ce9d41bce5e4c5d      1
    132 0xaf8b04fb70bac8a686aa429fb59428e829564924      1
    133 0xafef3bb7fc3cd8ae083672b65f79e6cefd4ac619      1
    134 0xb53ee69b92ad6d12e6f3b1f849ad3e706e31a263      1
    135 0xb6c189179b204d14fcc0b608bc97243b389b0030      1
    136 0xb70c3049982c09c18dcbf8596ccef6f5b3c239a3      1
    137 0xb7187511a0a0f5a63a538b7bc0abdd1a01979848      1
    138 0xb7abe0f0a1c31a88fdcdef71033cf7ae7d12f2d3      1
    139 0xb9fdeaf50a38c18672218a89862e0a845494625e      1
    140 0xbb5d3ad9f7b33e388cd3f29a2fb752a2ad9abe47      1
    141 0xbccc968eeebffc2586a50eef0ee1cf12273c9604      1
    142 0xbd09aea4b5f03361b9d165f1d81cd8b8b065f5e9      1
    143 0xbd6006b93f10b484d22d5a9d4e17bfd0ade4f614      1
    144 0xbe7a5ccea9f7279f714d4c9b4c5436dd38fb4fe1      1
    145 0xc06a00899ebd1481f243ac4ec92a276c1ff26e8a      1
    146 0xc26012491b9dfb2e6f2cb0305673e212721d5950      1
    147 0xc30e9fda779efe2081dae6529c0d07fc16769c8f      1
    148 0xc79eb3e0f6de5d735048779b366972077458775d      1
    149 0xc7e8a6abb9b5c76c75c9bb4f77715793f7f8205e      1
    150 0xca8299a5f070faf59e87f6c4df6e7627d565e006      1
    151 0xcbfb9a917c2b45fbd8e820e66602ef26dff9cd40      1
    152 0xcd8b72710595c69019c109aef5b1b92eea7f995f      1
    153 0xcece625176cead11c8d2dd6149fda2496b0c192d      1
    154 0xcf803a49f6e37b92a4040e397b976939dc5d9841      1
    155 0xd33744da3013927fad387d24f57cfa241735ded9      1
    156 0xd34e3c34ae9828dffedb9a2f236af47119a113bd      1
    157 0xd361363a6401870ff8a23b7c77c004aecb8c8b84      1
    158 0xd38e1fb60cd50cf9ae747ecf646b4ff1a310ba55      1
    159 0xd61bee56435dc7eeca895bae90fc8b9c7fe709eb      1
    160 0xd683678594e7b41281b2736f1beb13ebc5b06ec7      1
    161 0xd70676cfa67abc65a8c39f049d62eea30e3080cf      1
    162 0xd7342ea20a5afbf24352b5ca61e09844167914cb      1
    163 0xd86057c793bdfb77bb11dcabfe054ee4fd86ebdd      1
    164 0xd905de91f928b20ea0ccf455e5ba07eb6373955c      1
    165 0xdbeca36c48306867d9285e0e0d5ed1f5aec28a35      1
    166 0xdbf55298b349984a50fa98c7c6965994e30fc3f4      1
    167 0xdc36237208adb46959e77a0052843ce5446afab4      1
    168 0xdc78107155918e230246439e4159fea4c477eae9      1
    169 0xdcab53e3c8a2c37cc5e2509c2db0caa6c04f3ce0      1
    170 0xdd5f934edbcfbdb25ef5dd9b8a3868d3633de6e4      1
    171 0xdf12ff846c99ad0beaaa0a6fb7df118a66355293      1
    172 0xe0e31366067277dde4a391cd5d77f43cdb9ffa6d      1
    173 0xe1759f0cd1358c049e7f64c26e13de9492f9f888      1
    174 0xe1d29d0a39962a9a8d2a297ebe82e166f8b8ec18      1
    175 0xe2b34bbf669096a794397376fb0587e98eb81016      1
    176 0xe375b00384ecbed3da6d2f8dec7b6784cf3693d9      1
    177 0xe39bd9e3af2054dd193b0d0217cfbd460f104902      1
    178 0xe50ae5655d227767ec3974a11da7e4f67476b96f      1
    179 0xe5fd5ec4695b93d7e43f5d96657a535e0ff499bd      1
    180 0xe64777dcdca11fd8bde96ed05e7560ae789504b6      1
    181 0xe681e5232da0c9ba5d28e6775bb3b6945eaf50f4      1
    182 0xe69cae620953aef05e9276f35a7d3cb598bf4b20      1
    183 0xe77d44e642c53db943aa0a71ef60cbff719e644e      1
    184 0xea9f3a983d965e582c34eb852c18babac52050d8      1
    185 0xf0038d118400ea9510ea3e93f264b770b81145ea      1
    186 0xf08dbb788c290cdd919e1c124f183988e91e9012      1
    187 0xf0d6999725115e3ead3d927eb3329d63afaec09b      1
    188 0xf4aa421960cce8cf25190af71acc6949e848fe46      1
    189 0xf54d81b2955832474a87a8dada4a042a05b83180      1
    190 0xf599a05b4ea846a5afa1db274b508bb1ba1ddd93      1
    191 0xf6f85d9b96a43c87fd29e2facbf644df6bb029b0      1
    192 0xf777af5cfa7a0f242b3e0cf7fff4823c15dde70f      1
    193 0xf86b8b8d3433de1514cb36eff3bd6ff8f707c2a2      1
    194 0xfa766f029e703d341cc5386b9a4cf4d52be3b6a3      1
    195 0xfbb494b311f8790072d6f2d5ed1ab8695ea8890e      1
    196 0xfc2ceff32e3a534b78c729fd23973cb9ce98fbac      1
    197 0xfcbca52b23db28470eb97680d18e988d8b60b608      1
    198 0xfdb325752f9793ae7fa4aecc641cf5386846f566      1
    199 0xfe53dccb4db09e660b2dc5ec48eaff8bc18124c8      1
    200 0xff08bbef8f2b6986018fe3bb62850aa7db843b40      1

## Allow Memes 3 Phase 1

``` r
c(allow_memes_3_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_memes200_3.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 200 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x057931155d0c6355b1bd108b0fbfe9bc77d59ef0      1
      2 0x0637213177486b93a4198719a49751d77af0b43a      1
      3 0x0664da5943dfe8f5822983492f1cfcaa02396254      1
      4 0x07c73af8440b6a35a271f146b137521db6e19df9      1
      5 0x082a847d5c3f36cfb4f10859599cc75d98ab56ff      1
      6 0x094ce175d880a7fbff2cd01382f8fa1a172b734c      1
      7 0x09b4e8c1af40a44331cad3efa80feb013fe99ea3      1
      8 0x0a81816f351dac9deea2977df128d3d25dc202d8      1
      9 0x0a9bc3b0d19bca983cd9a6d251dc5920e87b223e      1
     10 0x0c0d8f387030c631de001d99d40b2e519cf4d10f      1
     11 0x0c9d7de299b6f803279b79e8ae06c6a27e6f3f1d      1
     12 0x0d5668339c3c17aa7f06ba7fe2667536cf15d748      1
     13 0x0fb3fa40850c1e472b6eced8fb10781763d42193      1
     14 0x10eb84abd429fa4df8dcabbc7c2803822a5b82d9      1
     15 0x11a880910eb629a109b0e586167796f7438fd4e4      1
     16 0x11b7b38d5b96d02d6381e646ad8ca55db74dadc8      1
     17 0x11e964bdb0504ff35dee22862da8f72603ab4289      1
     18 0x124183252c86e093f78e5994916f044d7549bd1e      1
     19 0x14b072f1954dd88131271d597a30e9899b57eb0f      1
     20 0x14b27e03a394cadec25b2b20573e1ce88f482e91      1
     21 0x1a0335b908dce97a4ed4f265f2b649a2133fcbba      1
     22 0x1bc32fd0bb3bddf8d27e6e6ab6572440cd768ae9      1
     23 0x1c2ea5a58d54914e06bee9932f5acd0a622930e8      1
     24 0x1ce4d8f0362efa9d16eabd90a2e6eb683e7d24e0      1
     25 0x1e39ebad76f75a514ebe5432f4efa2e8e50bc52c      1
     26 0x1fae7b5831c0b57dd172da74fd8beb983ab6b20a      1
     27 0x204a79ff4ecac8164c717bd46c02cd493a159789      1
     28 0x205819b6f631c73c36f899b2f58ef996d713a1d3      1
     29 0x208b4a8ef875a5e4645e23f27343f47fd887d9c5      1
     30 0x22bd04906e808196b19f7d10463fe463ed5f88cb      1
     31 0x22fc06a488b236753a89caeed39b58046b153069      1
     32 0x2573623e65748f9848ff10caa34bc6931753fc60      1
     33 0x25e7a0e250cbacf1299392881d7058606c0f7354      1
     34 0x25e86a38b3f53d1732ec77b1384e2b767d59bc13      1
     35 0x277884f6f7c8a96aa25cd5d4ff0a6354c57017a4      1
     36 0x27d4fee2fbe69bc02be96d1e146abe0a29797424      1
     37 0x297baa7d8b38330349250799af489e35ac1ef9c1      1
     38 0x2aaba4b94a805f0fea696dc11de324c2320ff4e1      1
     39 0x2c4376b9414fefecd89c9b770416f7516e8af863      1
     40 0x2f054c2f2c3497b96c66436fc006ebe096439be6      1
     41 0x322f3f6c176027df2cd524549161c71ff66012fe      1
     42 0x32bc332d4b1a58661bc693fcd66d4f90915baf71      1
     43 0x32bdd37b01d8c99537a3af0e087458f805d3d2f7      1
     44 0x34978faf3a9f469da7248d1365ddf69ac099588c      1
     45 0x34b045e74b751dd5014653c73f910aea2402005a      1
     46 0x35f7f29f154a28674c8a50d435384744b02cf42c      1
     47 0x3819660cb4d48b192b4973cb8323d6cb1404d930      1
     48 0x38e3d4a5bf7dea9280e389864d4bf9834cc2f266      1
     49 0x3bdc75b240aeebd7a266edf867b8ad5c30760a3c      1
     50 0x3dc3be0a9278028fde0e3ff65c6154e2e36dee9d      1
     51 0x3e960c18474442fcf4e00ab1d9a6b9b5cc0a9777      1
     52 0x3f0fdcd6f59fc4ff885006678e4ada78de1b0dd9      1
     53 0x42757298759773e1af7199b18f6ad2307dfdcd88      1
     54 0x428ba87cc89d457ea0754b7fa8bf39cfb53ed63a      1
     55 0x42f34449209059717e6c48ed0110783a7df82abf      1
     56 0x4313ca73355b0b098a78d406b42a0d86296bf0f4      1
     57 0x4322aa46ffed67809862da613725728e2fb8eae3      1
     58 0x45557a43891a053d64da64d846b39b049f767d98      1
     59 0x4583e00b4d17ce9e969f94baa2a6336f923d68de      1
     60 0x47196dacaf732ef3d6502ce01fdf9b1dce8c4a81      1
     61 0x47d4f20ae83bcd350105f199f900e6e6104dab6a      1
     62 0x48ae825591a926da5f49aca43608f28fdf37210b      1
     63 0x48d26fbd89dda10c72cd8dc7b82b9eee0af6b48d      1
     64 0x48f044229918cf94343b6a37f657998686d7fd7c      1
     65 0x49dfe490c49b15625f98c543fd55e8a2fe5fa5c2      1
     66 0x4a4f1c998b89c0dc000983ffd997420dad282ca1      1
     67 0x4a6d345c287e3c315eafd0e6a4274a74c6cb9af3      1
     68 0x4bde9f2fab6d26810d3058f0275604e9312846df      1
     69 0x4cd2dc5d7ae7d5128a5f3207cede5e6409686426      1
     70 0x4cef61837e4c2186468145e6b9668af92694aa43      1
     71 0x4d255d9b31aa437a0df8d02565c99d0faa6ed49c      1
     72 0x4dff50cfe5a2d72971f944132de19d2a64e978a8      1
     73 0x4e6e2c3abd4e791e1b3fc3b91c037b4ceeb36283      1
     74 0x51965ad51816c62c58106c1a3e8fa3b8a4af7e07      1
     75 0x51f9ac9219ac68e9ae215af1aee40187abbe181a      1
     76 0x520e8d6c5b2dbbe62bf8a40653a314d879b19b86      1
     77 0x523931dcc31e1b202ad6061bb76a360bc3d0a8e3      1
     78 0x54be3b98564f0a89237ff10d5c7a053edf2af10c      1
     79 0x571b8db726c2a29d237f88c9efeb896290bf395b      1
     80 0x577ec302c94f57d09b7fa1006e6165554d576f04      1
     81 0x57b42117029b9e806e0e0d1edaa3e68a8007d032      1
     82 0x580a96bc816c2324bdff5eb2a7e159ae7ee63022      1
     83 0x58a216fa7cbf9d4f46ce69a4008e628b715651be      1
     84 0x59234591ae5246b8dc35b07eedf9e1e93d28c8be      1
     85 0x595a1b38c53d8fb5fbdf68196f22ef79fe59c9e9      1
     86 0x5bff4631326de92282197696366df57198e38dd3      1
     87 0x5c509ce530f9ac85ffdf09c93c27654d0bc24b59      1
     88 0x5d2035ebe68f4ce03d6822a68472d82c74af18cf      1
     89 0x5d999a7dcf597169a7f8dfb03c7935da39c163a7      1
     90 0x5f75fb6104ac8ce72879347db1041adf2f7745d6      1
     91 0x5fbf4dc4291b542496cc61f52448c0ea374767c0      1
     92 0x6002ebe4144d7f3b0cd10e2ac0c0f357eb1c1c51      1
     93 0x60d8d52a265cc4c1e685128b69c4a9335ef9ecc8      1
     94 0x6519f68ca26c7f59deeabce7194da4d14e1f2847      1
     95 0x66382cfe621fa301e63607120f002d8b694d8c1b      1
     96 0x686e960ebd9aceb1771850444d4d3a7810a0dac9      1
     97 0x689a19f57077f682a1d7cc19d9066f1a834721a2      1
     98 0x68b7d402c6b3c797974180cf4c7ef71846eb81bd      1
     99 0x6928bfb114d228dffa0604f990d4a964bf1b6e61      1
    100 0x69da4ccc1e4c56949639558b21ab52da336e5fba      1
    101 0x6a17d3dd92a8d8fdf8a1f27c36a9e046121fc69c      1
    102 0x6c2ef68148f96f0ff50499743b8acff699ddaa72      1
    103 0x6ebe7f9cda8619ab2c508ba6c8f6675836185e0c      1
    104 0x71a7702ff3d858a0853a918fb44c537740a8bde9      1
    105 0x7674a62ad59b4b499630a786de6de0fc2f48f036      1
    106 0x76e6161f1fd846100140cbc111d0673b216a74c6      1
    107 0x7855227676cce7592e4024056ba2073998f1ead2      1
    108 0x78f0269f5b1ca914c8c58993f41181f89e212b27      1
    109 0x7a4b4bd0dd0fca862e70d7eccf9e224b32dbcdfd      1
    110 0x7b2bf4d604ba1c0e42bb8301f6f5ecf7e1a1feeb      1
    111 0x7bb5cc37a81623d6556d64c5d89333d3aee2a39b      1
    112 0x7ee27f21db35687b8aed85b1d96d8a9bf1c8474a      1
    113 0x7efe40097e4322087d7e262b3798175261513e5f      1
    114 0x80915e89ffe836216866d16ec4f693053f205179      1
    115 0x8397e2d5fadaa6e0817e376df5ba1d016a2f804f      1
    116 0x841120ff6d1e9f29e4ffbdb50eda696bbfd98ef6      1
    117 0x85f89831b2311fa2e9a0b0a6a72d27bc40abf0be      1
    118 0x867db38c83f4287a71ffd0cdfba6ffae93150ff7      1
    119 0x86d09318844786f26df5663de29306e673e668d5      1
    120 0x87d8dcef48c9632e87de450e55941d68db0b1463      1
    121 0x885869cb03bfa16e2e21e0e0d1b64b8ae9374f4d      1
    122 0x8880b69c95c0183bd3d4f30bc4272e457552c3d2      1
    123 0x88ceca090d7d810f0d3bde0cdfb1cf1f2301bc17      1
    124 0x89119dac068cfc98bf2ffb7d15948e0901d997dc      1
    125 0x8c1f393c989c667ad20e3d9df72a4f778c5c64b2      1
    126 0x8dc287825d71f758bab052608ba8a4f156f84176      1
    127 0x8dfcf8c25dc71279e16bb5f19a0607b60f98cde6      1
    128 0x8ed4d3c67ee1a73f5df8ca3e10698f1cd4c5b14c      1
    129 0x8f9c8d7f8d44fd2486bcf0605efa5fd0c397a658      1
    130 0x91b26fffffb325e13f1ef592b0933696098044af      1
    131 0x927705d26bd2aacfd0ed7492e8e21a86fecb4d1b      1
    132 0x93c79c3ee2964efb7235b9f445f50e6ff9003ac1      1
    133 0x942d0f8ee4bf3011390fb33c8c43fee8285a3ae1      1
    134 0x95e8a4792d7e5bf6ce3973184400cc9c37b6ae67      1
    135 0x9a78c0913396f8607eac7c4ae211a9138c2a1fac      1
    136 0x9af5c0a2eef61a4f6202532bab6a6485fbb6a287      1
    137 0x9c5f3693958aa03d3279a78f04fe516f40287092      1
    138 0x9dc5b2cee788e3e9c67489b5d7ce634dbf48a32e      1
    139 0xa15b668b95fe5ea740da21db69947ac8bb400c51      1
    140 0xa2b1692289b1891f6bebfac54a3ff1192b7e9e0c      1
    141 0xa412daa017927c717887ed27aefa0b720ae25b42      1
    142 0xa43b30fb0333e5d68cbe36e20c6accab28f82cc3      1
    143 0xa495e370e5987babb57580319c2ecb8e52a1239b      1
    144 0xa4f0670468dfb825e6c2b5571e2c97d0844190a3      1
    145 0xa6c12d417553f4c9a12c6d4376bc2b56e43eb2dc      1
    146 0xa73bd72b4bf7875d4b4d53581bef3e0eadcd051b      1
    147 0xa7c85ce7d129d8bac5b325b54312d6196254f7e8      1
    148 0xa87428a3a4e3b9159e6303c89cf7723d7e237e96      1
    149 0xa891fde42e982148b5f2f6b06f04be8e706a190e      1
    150 0xa9ccc49a14b707b9ec062ad83983c41ffc2a2e4e      1
    151 0xab4273e7137f9531fcb47d558d9bab0e726e6937      1
    152 0xaccce5523ed582f41f3d21cb9e3a306c4489bb48      1
    153 0xad47863310854df7e812b6393f03b379264e5acb      1
    154 0xaea3e4bd4a5250ec413e31b95126f3f997493a8b      1
    155 0xb1df5eda984ad80a27a9c34f0237c4c6325548f8      1
    156 0xb22f9ef32638552d7af2accfb2bf8a4b8f9c313f      1
    157 0xb40f68ae2afa5ff105a57e1e08239a51dd0962c4      1
    158 0xb5fd638924ffd3a937f3e9724c46ae505cf784eb      1
    159 0xb6848f941dc6c5baae2d1ef18b22b5c02f5d83ad      1
    160 0xb6c50f4dcdbcf91e10aab06db6ec839b0e7ddd00      1
    161 0xb7b104178014f26739955526354f6e0ea9ccb19b      1
    162 0xb9ef0e8383eda06ff12b274b961febb0cc97840e      1
    163 0xbab3c738620527f4614c0dbcf18411328555f24d      1
    164 0xbefe5d435616619253be2e448310f70136d0fddc      1
    165 0xbfe6ab779517544df703159ea41e7d8a71db6ef0      1
    166 0xc3f17178311899b068d0e2c86253e087dab5ba8f      1
    167 0xc424e4642e0bcc09e545127200ed686d3869ad88      1
    168 0xc455347ff5c5bf06008477864edbeb1bf225cdfd      1
    169 0xc52650d88793cf5fc0295bee9ae49284e67fecb1      1
    170 0xc60a2326a50606c84e00f2e4c1035c8d39fe467a      1
    171 0xd0981e004f98c7e0de915a6b68a73a2773f4ec48      1
    172 0xd514dc9996c400b58d5654bec86bcef6200dafa1      1
    173 0xd5755a4276a53ee7ca2703c6e1967af59cbc9feb      1
    174 0xd5b1bf6c5ba8416d699e016bb99646ca5dbab8d7      1
    175 0xd5ff53f48f14e9409b581e41a4cddfe0e97dc724      1
    176 0xd66a2ac715fd7628bbcbf49ac3dd4a0f3a5b847f      1
    177 0xd66bf38e7c704f0cd7910773f5c8cc3098b6828d      1
    178 0xd7597df2a4e40fffb742ee7c119cdcccb6841940      1
    179 0xd78b3277352d7c7752831de72510f485f3e8a58c      1
    180 0xdaac4e26f11223cc043dfd8e3efaf72333067339      1
    181 0xdac47e0ea5dd4565dfde65080a18e42190600846      1
    182 0xdb5cf3013a437ce5437b241b9f39b04f5d0bbb71      1
    183 0xdc0d28f3d09b292d41b836db991b157d641ad64f      1
    184 0xdd762af79fbbc73b51941fdd1fef8e89101eb51b      1
    185 0xdde8df9a7dc9f68bdac815f493d1d731de911b5a      1
    186 0xe1976fd4def8ef7a89ed3149239fd982daf6f892      1
    187 0xe74207386ac0dc2091da54f39b617ee0720efb69      1
    188 0xea1126e70185400a2cf0de2a6b35428fb8affd29      1
    189 0xeb9d9836b14bb9eef7cc0f40f87f83f6f959cf52      1
    190 0xec5293d6d757089797e36db3889c5f5e9a449607      1
    191 0xeeb2abcf0ba8f51fbf6cbaf357d4193c105381c0      1
    192 0xf1914657d98373e3b3eadab8e82b0e0431e61d0a      1
    193 0xf1eeca8fc61af3ae0930dfcf71db4f5e03e194ca      1
    194 0xf43d8ffe141fad5b15f78821d0e2f1fd2964ccbc      1
    195 0xf68e4d63c8ea83083d1cb9858210cf2b03d8266b      1
    196 0xf981b1da85d5db8210cff484e364881e2d5cfb5a      1
    197 0xfc681305295ae89991d3aac5c444fb6d96b3ffae      1
    198 0xfdd41f8325e243f200a34cd2fa28594efd5e4c6f      1
    199 0xfde36cbdd65160bd83541f060f44dbafcf0a0ab8      1
    200 0xfe83a0a06d830e9e2119522538f8b440ebd42ff5      1

## Allow Artist Phase 2

``` r
c(allow_shavonne_phase2) %>%
tally() %T>%
readr::write_csv(file="allow_artist_phase2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 83 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x000cd27f10dffac73201258eaa3925c0452051a0      1
     2 0x018a9df7eb6faa70725b2bd60b0373fee7de1941      1
     3 0x0401bf20cd9e5f5bb985df9bcb78d789e7641eb7      1
     4 0x0521dd7610ca1f4f86916936143188dc6360128e      1
     5 0x07ba8711792458348096dbd5734230e2a9cbba55      1
     6 0x081fca7fe4447d1a090a63395cf7d7d11ff6fe60      1
     7 0x08a19941f813eefb563e11a054c0d2c9ab2be761      1
     8 0x08c265911bd4639e72bb547348b65f9cee0b1c66      1
     9 0x09c440e23c586bdb1905a6dbd92547453307ce92      1
    10 0x09dbbb886dcb8e7c669dc2587a4564eebc488fbe      1
    11 0x0a4c97c9856fab0fd9857ed6891f33e06ae28f1c      1
    12 0x0dd16c537cdb346826203f3ab762030e7f20c78a      1
    13 0x1084bb5da62c4808acd81e38566184dab00bc0b4      1
    14 0x196d92de75168ca5b8c3c19b52a95bed4c21ba0a      1
    15 0x1e8a9296fcea21fa2a644952ee4cf1bcf35b0183      1
    16 0x1f773b560a5c5aaca6ede0525919d58eb43ecdec      1
    17 0x2730bbb1a1dc9061a1c689bf89280c7ce89d5d9f      1
    18 0x27ae9d926322d8a8ffa6d3c7e3d462af6b4a2a60      1
    19 0x29ecd060ed9d5414eb45002e6c083fcbd8ba8640      1
    20 0x2a17068bc37705fa1710dc8bfd1ee49bc0b432b0      1
    21 0x2c6c4d9e3ca81ee64002aa18d2a20e15113b2da5      1
    22 0x2ef32710d60f94e5e7bd83c3c5307f980456f4c2      1
    23 0x34a32ad4ba1ea1eb02ccd3ed5b9af9a8d8ea07a8      1
    24 0x3805d1f0acd90867ea6700ce086145f31058b9ba      1
    25 0x4ebb8d641a2f28b78538c33414628abffdd04a2f      1
    26 0x4f941d56eae99bcbf6111905721fbeff9ba8c2cf      1
    27 0x50572583f28d918c87ffaf887e0fa8c029d2db1b      1
    28 0x5124a888545de9fb539769f41a56cb93444b2b59      1
    29 0x54d2a5ab4019f542888a971e4ca1b43959b78679      1
    30 0x557c60995797fa7b47be105227a2e46148d85750      1
    31 0x56f4b60dc45eb4c26e6b0f8933c35b7853e5b809      1
    32 0x58fc378c40a23900f039ebadfd1a64a3ef63f731      1
    33 0x595647d3a723cf2d859908e065455fe58bf2ffe5      1
    34 0x5a77b2d05afb67eacd8f9c7e98692dd1e2883cb3      1
    35 0x5b50ad735b4b70a764861478545af6e2ce1aaafe      1
    36 0x5c20b91f068f34681e0c583f2bfd7831d5589ea8      1
    37 0x5f4f293cd2b85379e70cf85b5e8bec257280b00b      1
    38 0x5f77911c11b5c131cb81fa546b89d5d83ecc6b00      1
    39 0x626bb5bc4dd1ca5f49e313cc733138a6a631f10e      1
    40 0x63ddcf135fd156730a53f6726af121603fdf3394      1
    41 0x647eb74a5135a0f24beee3c2d8432adcbb32c2a8      1
    42 0x664430df9d1a1e8fd2e8480bbb568efb6b46a402      1
    43 0x6bbe2a84895ca675b2bf8d3633e25118c12ef912      1
    44 0x72408c230471a2cbe872cd6a1c7fda7eaa375102      1
    45 0x775fc3b977115e80b278a814db062f156effbf97      1
    46 0x7f529a994434dc5a11a2eba610d2ef90f0bcca0c      1
    47 0x808c12eb806ef61360ee8fcc6d420a0a23d0157c      1
    48 0x8d92888660a6eafe0e3e7690a08373b2414d7eb9      1
    49 0x9031af58ae87e3a0a10695390cb9a255c3275626      1
    50 0x9397c91f9139a57f45113d6247b26b619fb32274      1
    51 0x99f93b966021d6c6883b84e4b184957f49af8b32      1
    52 0x9d6eff26e54d92013df1ffa5479235ff89647a6e      1
    53 0x9f3af44d2d613c0c9d21538713d94a5c5ff1196e      1
    54 0xa954b7233974944a681957de8385b46b855ffd41      1
    55 0xaa8cb25cbb227824862af1ff937d47f148266a28      1
    56 0xab964393553a4bd588b3cba1b0f2eb0ca2c4910c      1
    57 0xb131c15416828ca94ada5b1749e4b0e760d1d745      1
    58 0xb1857bdffe55ead32a8d09a8c4d435432f80a543      1
    59 0xb88055edb5c215ce0d572586ce53e051d5817a77      1
    60 0xc202e824a0f5d51075147139789d5308a1b4151b      1
    61 0xc262ae1cca7684c52f6d430862083eabfbbbec93      1
    62 0xc59ea01237c7a3f347f0afe23e2a21a5fe0fbd16      1
    63 0xcadd621d59c3880b382acd97168fc95e96a90346      1
    64 0xcc7cc2e94273e694998ba0b3c80306cf031aedb7      1
    65 0xcdbe342e5d0302166c0d389acd85b17406ce7af9      1
    66 0xcf9d60c56c337cb635e4a33e9bc4ad920a496c67      1
    67 0xcfcc09104b0d21bf9dc823ebca79beee22563d4b      1
    68 0xd3027794e08bb744a6640ef3e39694c7c840899c      1
    69 0xd3dc804506f0523f574347a6b358fd037455a448      1
    70 0xd3df214b7e64729cdcb20ef4f4de8e0b2048caba      1
    71 0xd6fc89004c5aa526a13407b37a03d700df0b2eaf      1
    72 0xd7fb200ab4924c252633d1a2b1dc7226ca06c2b6      1
    73 0xd88f110293097bcda7157a4762611fa577e04319      1
    74 0xd8a1876d2f62d899b26f61291e87fcc8b9c73bad      1
    75 0xdf573b7895fc91481ab499496ddefbe9651d13b7      1
    76 0xe1ac0c193b389d173488513da9ee1ed86bfdc347      1
    77 0xe1ba165a819a9ee57a73f05f3333c7ae3f3c68af      1
    78 0xe6e7bb336dacd37640311fbb099f7580172f5613      1
    79 0xeca9a8a69076ea2cd8e0016bf5ad6a7a6bcddf37      1
    80 0xfc7c0ef10907a1ed1aea1a34642a14f8ec2d2677      1
    81 0xfe285ae8a0f7fe2dd3743966f2f1b85596f63564      1
    82 0xfe58f236a5c5171a01340efd344d909ae06229ce      1
    83 0xfefc2850b25fe1aa3e9aa154df61c6b9bc7aa374      1

## Allow Memes Phase 2

``` r
c(allow_memes_phase2) %>%
tally() %T>%
readr::write_csv(file="allow_memes_phase2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 976 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x0064f02799ea7748a9b51b5e78bcd274d9e7d0a1      1
      2 0x017347cb75ac8725608e593ea35d83f9b2b3cfb8      1
      3 0x017ffb7c9e5aff47d9569265d7c78b2494099782      1
      4 0x018305f64f71290f16ef245dd063a7470fde44ba      1
      5 0x01ac9c8027c3c91c49b33d1dd084ed5b87c7dc92      1
      6 0x01e8e9927d7c6b71671865f05783c8cbe04cc559      1
      7 0x01f4ab9ccf822e74cd514b1fc16068e749d37b1c      1
      8 0x0216ac50fdb6b46f6b74254b84ecdd1d431d670c      1
      9 0x02b294ed96e1175b8c71071a303e8a7d2396b017      1
     10 0x03024d05b1aea10b64ff3af1fed6a3df9adeebb7      1
     11 0x036aeaa55ba6cf87984a5f0a467b21c63b9bf9d1      1
     12 0x039649f7c2f548692184da3fedf316f58e8356c0      1
     13 0x03c5e3ff9514031f910c9bc9ebbc7839e02b23d8      1
     14 0x03f4cb9e297ea659f30e09341ee7155a7d136398      1
     15 0x04490fb053a8ff110bea35f22d955c0092aae5f8      1
     16 0x047c4d7af709ae19d3bbbd5f9e10461d2554da00      1
     17 0x04df8d02f912d34fef12a1b0488ee56fd6f7416c      1
     18 0x0573383fcc26eb066010d28d96ab02f7c5cde67b      1
     19 0x05a13dcf55ea6d532f15284f39e02811fc183a8a      1
     20 0x05eec49e27704acf5c5ce31ecfcde4e382d78ba0      1
     21 0x06e13bd0a3cba08e61028f2326b9ea2ca2539900      1
     22 0x06fa75bfaa972845e7d78a6d928357f15a341d6e      1
     23 0x076097cf460e3b5fc3a94c43d9a598164bb29168      1
     24 0x078cc4caa131eb9d6d3e31847946d5f37fd3f58b      1
     25 0x080ffeaf914180e18f69092d66de11925434b540      1
     26 0x0843e80240f9304f3fe7059f25f3e1a66886a0eb      1
     27 0x0936d312ba234451fa7a90f68c7ede0f4fa962c6      1
     28 0x09b2f0988b6d24336b348c84c2764d2b4f85142f      1
     29 0x0a7669015fef82f5136c9a2e44e1ecbd2a5aec19      1
     30 0x0a87cbf5eec20d8d9880ad21f0260522ff5a675a      1
     31 0x0a98f97e89743406a8611e1b4219a073b60ffed3      1
     32 0x0aadeef83545196ccb2ce70fabf8be1afa3c9b87      1
     33 0x0acabdaceab63c08bc5aea63d7e07da7c2e42959      1
     34 0x0b9f898921b2bb8cd2d7d30faffec2c90200cc8c      1
     35 0x0c28d263fccf3ed25a48ddcf6561dd9cccd791b7      1
     36 0x0c481cd7d09bdd222351d52622d272367cb3d159      1
     37 0x0c4a911ed6af1032beec7c19225c91a3160e39e3      1
     38 0x0c6306c1ff8f0ca8432b761085d5ce74160a499a      1
     39 0x0c76717074a973f3f6e3d422166c999483f6c7fd      1
     40 0x0cb5b9eeed8d51ae57e70e46b2a3596b94ece6c7      1
     41 0x0ce390f18af702cca546297845a4a51d102123cf      1
     42 0x0ce6a432eaf0fa856a7c774170999db207748240      1
     43 0x0d08c74268c1260d9b50175c41738e1a45819700      1
     44 0x0d69a096b2c66310309f0ead1d6c97c4dfe87086      1
     45 0x0d6b4f304b91a3b67b1db34776ef7e95effbc729      1
     46 0x0da4438a9cc8d41fe9c0c6efd302a173ed967da7      1
     47 0x0db7b27b7a3ea8035d3f66f3ba7532221ef06abe      1
     48 0x0e2f031a1142ab3919685cf82aa764d9c5c0ea86      1
     49 0x0e3e001a00c07e2cb3219d1ec0c47cc532d87804      1
     50 0x0e5c3e22b7239cef46f56741ff66369f3cfecb77      1
     51 0x0e757c27de6feed6d9f43120943ef07d89335483      1
     52 0x0ea8767de750442ec4a34d81bffcfd4417396f47      1
     53 0x0ea9f4f7c371e7044d187b3da7850ccaa6370a0c      1
     54 0x0f0eae91990140c560d4156db4f00c854dc8f09e      1
     55 0x0f78afe5b1e689cc2b205a78531957a286c42511      1
     56 0x0f9ab846449ca068f9714818a6ed0da5d819e9e4      1
     57 0x0f9c4213c040a39db2ba6f833472f739e61710b4      1
     58 0x0fb4e7dff29c22029975373d1bbf5873ffc3c61a      1
     59 0x0fb7f9eb8e9bee4dd70dc334b94dee2621a32fb3      1
     60 0x100f28c9b8b1fda897eadc775caf74e0f3560f73      1
     61 0x107d1198854fbbcb2e1014c3ffab166b2595586a      1
     62 0x111818a51c4177e8980566beea68fe334be7b76a      1
     63 0x111b863c2f7d1833d8f53830647c260169e99626      1
     64 0x112d6fb9c17f75d82d5c5d50efd03ea6af12191e      1
     65 0x1187ec27eb94e65717af65f3ae8c326bd8bb47c1      1
     66 0x118d6f5d61a9d881d452510f29c703abe9d80cdc      1
     67 0x11924c505ecab6a2434bdfe94bc0ff1ca081facb      1
     68 0x11a22b262e505d355f975e1e48a365b5d4811ae0      1
     69 0x128ad42b82c752c5b4c7f679231d8956c98038eb      1
     70 0x129c6695dfe7a906bd8fda202d26dfff601f83a4      1
     71 0x12a0fc764430a24833fde0310fce8071e1b5da08      1
     72 0x12c3d9510bba6fa40c55d4f30e1be25f5f953e22      1
     73 0x12c4908dc21e88a8f268c3f363098eed251c1aa3      1
     74 0x12cd871105b29760a61f56753d54a59381f5bcc6      1
     75 0x137f41ca27f570d4c50e56d5c20ae2807225f09c      1
     76 0x13928eb9a86c8278a45b6ff2935c7730b58ac675      1
     77 0x13bfd26cf7d981aae5f8d4d88af4b34b4b1d4da4      1
     78 0x13c39b3e6e6159bc233fad90825f1f9f84fcb102      1
     79 0x13eee41d67b8d99e11174161d72cf8ccd194458c      1
     80 0x144bf0aaf16debadcd2638f572ebe42a92f5bcc0      1
     81 0x144c704bf25f1865e4b24fd6596ffed7d92470b0      1
     82 0x147f466fef6bd04617fc0c8037ea01c73bc25a3f      1
     83 0x14abeea8db07c46b37a79bfa856d4c2da6d3e6df      1
     84 0x14bee91efb79a8eb2332c31177fd3a61481ccc99      1
     85 0x1566ae673ae80725bcce901b486c336e6acef465      1
     86 0x156db724902f03f3986f6adfdc38f7696e4d5111      1
     87 0x159e5998669ec6628a6b670c2ef1ddbd93084698      1
     88 0x167a4e6066d6c96d5c012006f5cffc9f606131ec      1
     89 0x16b5ec9e34bd0a916822d6785b1c600ac911d6dd      1
     90 0x16dee223fc168abff7b979813cdf15866eed7e8d      1
     91 0x16f3d833bb91aebb5066884501242d8b3c3b5e61      1
     92 0x177a8340bddc2041cc362aabf543dee4e394e3e3      1
     93 0x17d6df353c4062b6097ff8c872ff6ce31381988e      1
     94 0x17e31bf839acb700e0f584797574a2c1fde46d0b      1
     95 0x18412b9f84df461bdc0f2e0709df507d5bdb6363      1
     96 0x18595f049a9c5c2019efda113009b0ec1c89ceaa      1
     97 0x18eb4aece6cff56f3bd4ccd844491d55b6ab21d2      1
     98 0x195f2cd867c1c02ae6e61a14ec53f5f01b440edc      1
     99 0x197fe3ee16556577180f6578050802106e8bc446      1
    100 0x19e3775d47d63e1802577aec70189e7e3d6ac17b      1
    101 0x19e7a132bd4b42f580f546197f42e19c42cdfe6c      1
    102 0x1a4370fdd7173d0a41ff7c63a8e0249479ba0225      1
    103 0x1a6f0dddb884854355023a2dfe26e9174f8e0290      1
    104 0x1a7d7e6abea492e7b9a3cae667f468c72e884671      1
    105 0x1a7f174dcd4211d41c7c4d7dbb3a0e4db5780c67      1
    106 0x1acb5e5c169aed4b9f963bdf786a8377bff678a1      1
    107 0x1af369bc1069dd286ff59cd69553550c07e0dd05      1
    108 0x1b6265a40839a331ace2d81bc82b6104703c0426      1
    109 0x1bb6dc39ebc757db05557d00b16c28a753a20558      1
    110 0x1be1eac561b34c9b5e45eba9c3fb51fc5fc2796e      1
    111 0x1c172d05b75178fc669d74407243cc932030f139      1
    112 0x1cb89e486db5774ba084f683796286848df489d0      1
    113 0x1ce68e5912ef8cb4e83620a50b70edc900ad4759      1
    114 0x1d10b0166a761f269adf864ebdaca9be445ca637      1
    115 0x1d25029d92a7051f46a13be9a512443c966d3542      1
    116 0x1d522bd33cdd1f60be71b0b7b2efe4e9f20a5263      1
    117 0x1dcfdcb24456a7dba5342479cff0f9ff30145fb8      1
    118 0x1e31c76b78ff00e3cb64d41d5844626d991ab9e8      1
    119 0x1e6ef3e23dbf38428e752a7293b698b189c7317f      1
    120 0x1e7ba17cc3f031a0c5795739317f6b2022ca39f5      1
    121 0x1ecded4519a9688ef4bfb5f15811c52f048ba1a6      1
    122 0x1ee5106b2233169b84dad2acdbf498b29c3c7d15      1
    123 0x1eec2d7e15d24c053658c2c466bbc59850a6fa22      1
    124 0x1ef43b94fcb00872ec73d60ff37f5c77df80ee66      1
    125 0x1ffc29a768e26ab393ea93e4284773410a84b660      1
    126 0x20163ec2afd9698433a031b0a7dcd70ed77ba8c7      1
    127 0x20542dffc2f7a51cbc087f54fcafd9ffd8b5c899      1
    128 0x206b0526f4866e7580780015cbacbb4a8dbb8c80      1
    129 0x20aa168e6c793646f60737399c8466dd643d4281      1
    130 0x20ebf9ae7c66da763fc1ce86062ce883389c8e23      1
    131 0x215cbb1b60e2cc1f1e8626cadca386b278aa0dee      1
    132 0x215fc4d12967ddc2432a7b8936f2849740edb7bd      1
    133 0x2177da04f9479496c2292d6344d306aa49beb34a      1
    134 0x21804e35a54aa4820e6cd409d70926a63dba3e45      1
    135 0x21b9c7556376efdbf9ee316a4ede0c8257fb7533      1
    136 0x21f023839e7b6fee67d33e4548791fa388564a02      1
    137 0x220da3fc3b905c968dfb20c81c170bc4dce56603      1
    138 0x22418183c90741bab6606c0e0d240ae6c3a148f0      1
    139 0x22e8efe40ddb7f13b17b4c10f768967fc7a9f875      1
    140 0x22fbaa2dd20b848084545515fb04b9c846942779      1
    141 0x231595e3673a10e846803194f4982e1cf3389161      1
    142 0x235ef6f6bcc455bb284ebefca68754c030bdc1ad      1
    143 0x23602ca06e977c86339ffddad74966e824ab691e      1
    144 0x2380ca49ed8e933c97905977763693e5cf8770f4      1
    145 0x23ae72f8336aca747ef02d596403de56cca489fb      1
    146 0x248458947c120ca057ec028b3fe7e4b3f26fdb3d      1
    147 0x24fbadccd6684106e24065694ac87b0e98819235      1
    148 0x269c2d0badff4e365ac2a0da8c8e3f186b6adaba      1
    149 0x271c6f480aeb26672b915a7b33159ce8f1e97fd3      1
    150 0x275c83d76b1b0cc42bd066e327c4e578a327ebcc      1
    151 0x27bb5366ef655819337d6ffd29a55905608c853b      1
    152 0x2801dc73a6dcefb364b959606e0c61234105fd5a      1
    153 0x28cf5d9d465dfaf5c616958ef8b23dbee567e2b7      1
    154 0x28e6c1352950238be088ef2a741f3c8b91b9ffad      1
    155 0x2917e947fb1925abe184d4062520a11bcf2a5b36      1
    156 0x2924196a2ec71ea4ae1b1357381eccdcee6c18f2      1
    157 0x29c02a5e462e187ec7e1c7874d012633f12c89d0      1
    158 0x29c2188c6c318ab5cae4ae4b11a49edf2ec9ab0e      1
    159 0x2a0a412e0a0d436cca7ddba177c4dd9f29801062      1
    160 0x2a3c05f993d34bab6fa50c72f2465777d1c5d118      1
    161 0x2a462fb59a5b0885b60ad31c5fe473d5ba0d79fd      1
    162 0x2adfc86a4e073169ac5f8c850a9e80c90383f3f8      1
    163 0x2afc032aaf6d7ead33fb05f9d42d874874dfb9d1      1
    164 0x2b919bffea16d55828091fdb8a63f0678e17b26e      1
    165 0x2be96c05fb7e81b020c42086b69d21bbf427b53a      1
    166 0x2c1b5f03dfcbb52493a093e8106d7a378c345097      1
    167 0x2c23b2ea134c3dd6d9a48676a9a41c6ade71adfc      1
    168 0x2c3c45a64310519849c3304c8fcac3dbd14a758f      1
    169 0x2c45f65d254e85584b47ec82450c91a30cfc6be1      1
    170 0x2c8875f34ceb219f61b7453b2c5f100ec2f6ed33      1
    171 0x2c9646bf2477a747897047b69ca1a1913ae5f611      1
    172 0x2ca75a892b59bc7cf863ba261e49ab40a4844ee3      1
    173 0x2cac9e6d1d630a35cb8019779daf4c5fd96135ca      1
    174 0x2cd00bdf1605046cef0f50420831bf3b7d675882      1
    175 0x2cfdaf06e39e5d9d3c9afe20362b725b41f68725      1
    176 0x2d1543711bd262108fd8a1afb43d8871f76c134c      1
    177 0x2d646486397bbdd894a9bb62d91fc5bdcb8d9f45      1
    178 0x2d8b81cde837334beb89a9c0a88179ad234770be      1
    179 0x2d924c0bc8033e7866b90ca2a76cbf4b5714b11b      1
    180 0x2da903666829f302b0501f76144339213259c260      1
    181 0x2db1307f1487c2586473f570ef6832d6873a0053      1
    182 0x2e299b7faa9b906117181a4550d828b1300b48d1      1
    183 0x2ec4a2bcd4f33c7c9aafab7cfa865ec15508bf62      1
    184 0x2ee5fc834903186949c75e0dc10ec245397bf1fa      1
    185 0x2f1390ec03a03e57a1406bdef57c9cf487f62b78      1
    186 0x2f2242b447a54cb110b8a7991cf8a27054ee6921      1
    187 0x2fb24b1021e51fa23db91a9b995000181fda0963      1
    188 0x301be4f5965cc6d386a3aad84f1344ecf1042c3e      1
    189 0x301e2d2a98c5873ac27fd9eae85f0153959100fa      1
    190 0x30b33f902981b5e567d3bb1a3cc847b35f626609      1
    191 0x30eedd4ea7e1959fc9d6879cd26f1d8baabbc5f6      1
    192 0x30f2a414945ba487f6a9ca909d0cc0919c6a1812      1
    193 0x313c8673ff69dfd7be2a46bc540305d3e2d233b0      1
    194 0x317c32ca0309bb8a1f9d8a3e2836544f233c841e      1
    195 0x31bedb3962ab3a1d2a2e070853aa5c4acdb734f4      1
    196 0x325d12fe8bce0f969f879679275adccfe8ba0605      1
    197 0x32a0d6b013cf8ecad1e37e99532570411d398d05      1
    198 0x32dd9f6885332ccf8633a18cb0c2e25e68fe03d1      1
    199 0x32e263cb9003f7fef329080bdcbc82f5cfd6c02f      1
    200 0x32ffe815277ff53dd2a73557664e229899e6501e      1
    201 0x331b4a1242ad6a794ef127bd61fe57d7bdfdbb80      1
    202 0x334cb32bc18ec632ab04ee55157aae1ff165e093      1
    203 0x3368b4733a4552b38b7d81c215d88cac4c78d8e5      1
    204 0x343e85ac3009ac01538d4251900ce1a2b7d7ffec      1
    205 0x346aaf2e75cfccb82cff0fcb8d2cdc21d5656358      1
    206 0x348d43bd8daeb9f13611d5f0838225e6a9d89bd0      1
    207 0x34e03545e5527f54243ce5a4f88ebb202efb0ae3      1
    208 0x35d7dd9230a1f079c907b53e7c5822b34e197a1d      1
    209 0x35f4bbcc8490671edf37877684f5aaadfa4235f7      1
    210 0x367dc97068ab54ba1dfbfc0fad12fbcb7b3a0d09      1
    211 0x36d89738405a8325b7c560cab2dd1b88565effd3      1
    212 0x36ed861a319278e5b6c39028884b0ca93df105d0      1
    213 0x36eebc48da111b14b010a97abecf8bb68d10095c      1
    214 0x378dcff8e6be1778b04f5712d686517bb6a01927      1
    215 0x37c14f24c73a9e151575a0af3dd2dd5cbbca8a29      1
    216 0x37ea6c993c5f42e1fc3455d5781e4a580760970a      1
    217 0x37feeac37afa67d5211937b4fca166f91724ae80      1
    218 0x3839a6c85a38de97abd0ecc092ef3311a7f1311e      1
    219 0x3876be5be4998adecbfbbad26604a762467e7f42      1
    220 0x387f802c6a9ecffc8b8f6bb7b80ea0db4cef9810      1
    221 0x388160a99390392278afdba240046b8b5e73f77b      1
    222 0x389458f93e387fc568ca4568c231a64ffd0456d2      1
    223 0x38b2d736e41a273d607b24e888a09473226c46b8      1
    224 0x38d779b6dc61acdf864cd289f3594ad05088df95      1
    225 0x3927e502c865a2c873a735775e224930eadfd2e3      1
    226 0x39bba3c54a3412127bdfb0f218efea52b5d28300      1
    227 0x39e6d2b65fcfa0b3b2d74973b9eb67b6d68990bd      1
    228 0x3a0c596a40bbbb558d86b0f692884aadf1dbf20d      1
    229 0x3a10fd1387555cd75db8142393fbaba98efe28d4      1
    230 0x3ac61447a75149ba67639100ecc92c76fb20941c      1
    231 0x3ae8d9ba72848bf5356f8cc56450532c9bbaf9fc      1
    232 0x3b0b262b187001522c34edcafc378500133ab230      1
    233 0x3bd835333aad77686595d734ad5b80934d8b026e      1
    234 0x3bf55d8aad7ca0c5cfe1f697d7783437b9e034fb      1
    235 0x3cb63b82d778105e43f064ed739b0655f1f0fb87      1
    236 0x3d20c3f372207074df1a0ab5272c7168ce23d178      1
    237 0x3d3af253b037d3b22c4c810673c5d14de16d1af3      1
    238 0x3d91ad906c3969f86c588c988f721d5a735c6f81      1
    239 0x3d9e77bd53dc0c2a3110732e30d0b969713fa4c2      1
    240 0x3df7131fbd4f6b27be8317c69e41817498e1bc0c      1
    241 0x3e998c50b1eaf1577416e2bf31a76d09a48bc3fb      1
    242 0x3f4373afdde3d7de3ac433acc7de685338c3980e      1
    243 0x3fa5a25f48ba1b736761706801be4f639ca4853e      1
    244 0x3fd5ede508fdb0ac9e1051bc894d4c9c04ba288f      1
    245 0x405020c797a64f155c9966c88e5c677b2dbca5ab      1
    246 0x40e6d6e798dc878e12987ed978f23c2391f1f570      1
    247 0x410a9f44948576c60f94b165f63eb79ce769a04c      1
    248 0x410ff1f298d37046a1f7e0c07fce1d8e9f91d15d      1
    249 0x411dd34f17d5b6398f155f768ed27c32ad194862      1
    250 0x41254fe73d3b49f0fe3cc7743b4d40ef26bb338e      1
    251 0x417c269b82387ab605bdfbd312d91baf03dc8516      1
    252 0x420ecd4ec65c0fea87d8dc5d16c2476c42466b65      1
    253 0x4220132c9df1ab7bd2913f0fd03297c90e7cc6fe      1
    254 0x4269aadfd043b58cba893bfe6029c9895c25cb61      1
    255 0x429683282e5a28b81aab1f9126b8d5e4847dfbeb      1
    256 0x42d38ed60a64d0b8c36f190e185d17db3617a091      1
    257 0x42e5abe19ba467b0b32a4d042d3dfd4ba2e196af      1
    258 0x438ee34be7c6deefbdf0afc21c0d5375b912e0d8      1
    259 0x43b0bd27d5016e969193412933387b0dd4cf3e0a      1
    260 0x43b9778940c53abf2ca96ba900ff181ec09d7fcc      1
    261 0x44b4d430e7c56e4ca04c5799e561063ccc1f1df2      1
    262 0x44f2afb689aea37ab17feb2420f17f9a3e851307      1
    263 0x44f301b1de6c3fec0f8a8aea53311f5cca499904      1
    264 0x45133db3eadd8718f5c3a868ec2f7148e460dcfd      1
    265 0x4525b5402a6d9eefc656e828b9391aa9dff2446c      1
    266 0x455ce1afc1403b728789b4a8c5aa512600b668d8      1
    267 0x4581c619ae0556b774f71adab6831a86da1aef17      1
    268 0x462fcc4b3fc64e0dfd751424bc3b070d79efb300      1
    269 0x46abfa031be839b1599513887a27a403e8d6598d      1
    270 0x4705bc2775cee0fe266dc2a376010c0eb1bcb474      1
    271 0x4748ce48e8979e7ab09b2e8df95a1bec7c65b869      1
    272 0x47a79ee280b4a6a8d68a3ba16e4082a765a2b20f      1
    273 0x47d539d4dd9b6f21ccabc5c96bbbf7071290938e      1
    274 0x47dc3a7aec5c0e1f4b2c71303a4b1eaa0bee3e4c      1
    275 0x47e64ae528b2d1320bae6282044d240ff67e703e      1
    276 0x4808d5a2b6423abc0060465fdfd338c58e758690      1
    277 0x48238faa9bd6d56fb44a32dac6185f04eabf2117      1
    278 0x487869b7557f0fdecbc5141da1d7c48c15c5b8eb      1
    279 0x48789de67a2c509f3676ebc70def28fddf64d31f      1
    280 0x488db5b2f8a9fbb9f50ac113ce9f88c721fd4eec      1
    281 0x488e5685b38d9412cdadae46feed3e863f57ca5b      1
    282 0x48be4681972473b498e8b686d38e04826c26fc4f      1
    283 0x493485c7b822d077f14bd6484a5c40f2adc91c96      1
    284 0x49d6531aea6dced6595d50da685274766e4cfc8b      1
    285 0x4a0e071be98b9514aad70f5d2595a8ab23a7f053      1
    286 0x4a15e3d354043fdefbb8fa77a55da211f4058190      1
    287 0x4ab5189a020ccd88f05c36de69e4d868d29c75a9      1
    288 0x4ac69caa7bc279fec93db0d10793eb8516c7a9d1      1
    289 0x4ad5bf7218b6695630208560bb62a38016ed64b3      1
    290 0x4b0c1beea2e8a8197fc48b0425a50a7204412989      1
    291 0x4b6c1d196e06446eb0534326bbfb02cc3d073a2b      1
    292 0x4bfb94bccfd860e1f9d85c10a8949a722676fc4a      1
    293 0x4c26c796abcf3513807efa54d54c34405b84a36a      1
    294 0x4c3d85e7fc0c91215eb6a4186592a41d975d2a4f      1
    295 0x4c48d00997c6d23d6b47a9ce8caa6027d0068e42      1
    296 0x4c8a8c3fcf77f37101d25930e7a086b4e0ec45ce      1
    297 0x4c941a23ec94580d3b5442aa8d5851583fd8bcce      1
    298 0x4d6aa3da789ea162a5978193bd584d3067227835      1
    299 0x4dd8f136fe8a8202cd6395ad34ef5ae49c26e78b      1
    300 0x4e1829da30cd7ae2f3e9915cb8c9f3f203ac7d83      1
    301 0x4e3cc03eb2be6122f2316b84d1c7edb900e90dba      1
    302 0x4e61548d94c8649ebfc2f5f54d2272fcbc687bf2      1
    303 0x4e8849962c43d9d7540f64b24bf76704926926ea      1
    304 0x4f492b2a3ed9bf0c65d508dab4f7c5f5c04ca6c3      1
    305 0x4fed7d0eb3bf1bf1ba69320962c81b132ad4474f      1
    306 0x5022cf478c7124d508ab4a304946cb4fa3d9c39e      1
    307 0x506452ab0dacf04db0ab1544b6be4019651ada90      1
    308 0x50d086175cc14fa5ae008591d72a93cf22b48e32      1
    309 0x50f8c08b0124092e1001b355f4b8ae2df85f715c      1
    310 0x511c85042524a41bbf30e7a8533a84b8c5e55f43      1
    311 0x513e0a472522e4bd8023c032bbf01e7fb1fe0df3      1
    312 0x51a2d3572577b6c47186c82165ab8a61e046dc83      1
    313 0x52349a2c4ea4e4b94ada3d75c9d3a318c024706f      1
    314 0x52690f90740621f89f58521433e9b0921d626708      1
    315 0x526ef837f56eb025908ba91811a6ed0a3b32c560      1
    316 0x527bb834cc5c8ff730c673880e51372282b06e14      1
    317 0x53006f95def268f88dc1b8216654ab56f3afd052      1
    318 0x53b2da71db2e266882a465d9aeeaae3e8beeb9ee      1
    319 0x53cbd34ca41a2373dc329c63dfca349d90139e27      1
    320 0x53edcefe31da6a0051df17ad80e19ff93c490b17      1
    321 0x53f1b3d8990472d1adcd7b76348e5508db17cea0      1
    322 0x541db1ed2628f2f4897417d06181af6a179e90d0      1
    323 0x542c5648d46462207fba8a4bd7d890b4154b722f      1
    324 0x54669ca58bf3a59caea8ae5135db491e4738f65a      1
    325 0x54913cc8ea17731d62589039dd0152f306473843      1
    326 0x557bd9e829f35f1d853c2c85341458892753fa1f      1
    327 0x557c60995797fa7b47be105227a2e46148d85750      1
    328 0x55adbdf8a6396356de42aad937ea79caa2cab88f      1
    329 0x563a7e710f0e6e81d68b4a2fb6c273f179783a2a      1
    330 0x56517e41ef464db3753ecfdd2dbcdd2f045b573c      1
    331 0x56543717d994d09d5862ab9c6f99bce964ae664a      1
    332 0x5663d7d0751db1a80f01d6974e02036b6d44d56a      1
    333 0x56bdc5fe7f9752e7f5a381e394acfe72a724462b      1
    334 0x571493b6bb0b99dda41e2da1774b8c9b5bc302af      1
    335 0x57dec6115e561f3de9fb7429cf14de429713b3d6      1
    336 0x593cadf136c68c720d446461c1bfee600647c6b8      1
    337 0x5a3a1461310884df894b7e973305f690fc5779d0      1
    338 0x5b1022d859a46f42fcf24bcd8f5ab616e52b4800      1
    339 0x5b68b49d886603537f8d87aaaaae39ef801353a4      1
    340 0x5b8ed97b1e85ea4e5e96c3a6e00b0835b087fce5      1
    341 0x5bd832dc1a5ac61f687b87c4199b844819a4d8ed      1
    342 0x5c3097e7fd3b97d9fdeec6d378884c892ff0545f      1
    343 0x5c95598fe454882fec1139c3ed1284255b49c8b3      1
    344 0x5c9e2a6fec34b510996a8e2a3d1e2c47a382a8b9      1
    345 0x5d0d23ae81ebe1b6ac96411b2c7bc3bf7463fa25      1
    346 0x5d20e6c0db887b5c3281edfce5bb7677565e2b8f      1
    347 0x5d25087405105bab12624c73488ec186066a6376      1
    348 0x5d5fe1910a289b24455af49ee3e41405f94b921b      1
    349 0x5d89737e854c860d25e106c498c6dca0b516ed7a      1
    350 0x5dafb2d1f8b20501da62ba00c0c283293aa0b70c      1
    351 0x5de26d392ea46ffc17131042e2584fe6ba46206f      1
    352 0x5df5342342701b8ae5bce28f74ebb73b5fc13a54      1
    353 0x5e0737b90f1db90eb4f423dec067fd6c06a958d0      1
    354 0x5e3118e91b0a02a5e2b92e49700c63434e94f0b2      1
    355 0x5eefc3ae27be1483ad6b864b45ed72fc39e6bb6c      1
    356 0x5f656037e30a003862cf878db24ab5f537177fd9      1
    357 0x5feadf8ef647da7f07584b3d33fcd565b79359a4      1
    358 0x60bc11840966ae3946ad1904a8efb6622226be25      1
    359 0x60e5299f49cdbc13d152323105af462071b22c87      1
    360 0x614a62584658d555cc28b3aabdb691e85e002c8b      1
    361 0x614b89f072ea263a9387460963142e73548fbaf1      1
    362 0x6232d7a6085d0ab8f885292078eeb723064a376b      1
    363 0x62af590649b7abe80f8c6dffbb92e0875b65e37c      1
    364 0x62ba9de7d81b96d697fc2f26b5eb647f184f9b2e      1
    365 0x62c868061cddbe38e01e7a8295fdcec72c10ca06      1
    366 0x62fc78469063720d0ea442baf2984c20b50f0c05      1
    367 0x631cd42eb1063d1fe2b49a78744f3094702edbab      1
    368 0x632734882ed0127fbdf2666478df42aa916bdc84      1
    369 0x638fba3bbbe8828394d2b13f92fba75bf32c8efb      1
    370 0x63f11d2060d147e75c2bfb83dbebe155b4d06b66      1
    371 0x646ef8e3c8e79a10daf8f32621a3e468501cc3e4      1
    372 0x64ca4373a93e25822dcfdca0306e899a29e92b3c      1
    373 0x64f02c24bb510e27b5eaec705eaa840ff7b4df15      1
    374 0x64f7de90dc79d775703bbec66a1591c7a26a22f0      1
    375 0x6576a26763cefa8f37005c067f7902960dec6b8e      1
    376 0x65ba4f92d7dfa813ddbd849d9faf38a723dd9b12      1
    377 0x660105ea6435e8d4c887a3c830b8812746eada30      1
    378 0x665654f2d2a151be2d0f8e3697e6ce780f732af2      1
    379 0x66567f4ec7743e58713f44f083da3de78a52556a      1
    380 0x66b280b5778c35c719209614428caddf00aaa3ce      1
    381 0x66fa06bd24294ab3bf123153e49b9065cb7672e7      1
    382 0x670a840848eba22eb2a090fd65e1ff60c3ba9e5b      1
    383 0x67a90218cc5fc1adde45ad27a257f268023b2ecb      1
    384 0x67c6083d540696c6aa1cf31101d78780e0d6bb47      1
    385 0x68d45e76a87e9f1706e0d36b7815c8dbdb7e6aea      1
    386 0x692997405c1cebbc56ff3416fa0b59ab97fac696      1
    387 0x692d6cf19e0c082185e20ff5ea6bb267e7aeb278      1
    388 0x699990a8e7ada9e92c932d6e8fb365024fc74b43      1
    389 0x69c8c2923005d26eaeea9500d7602eff8c81c848      1
    390 0x69cb3b1de24e08f1cfc2994171b6c6930498f750      1
    391 0x69cc363055d8c3d5e211865e805e145ab9208d57      1
    392 0x69cd43dd4ecf3a076b1b9d2cfd41589987129bc0      1
    393 0x69e68074f1aada957edd39c5eae0069973343f30      1
    394 0x6ae613a84a155fd80c8e6f14cb3a1d8958f51b2c      1
    395 0x6b29380bb9955d5b2df1624c5c6a72a5e69a6b80      1
    396 0x6b2aa5fc4508ec023a8e355959eaccef5e3a250b      1
    397 0x6b4b2cbb69b9ca07328a40e7ffff3870f627c8fc      1
    398 0x6b4fedc18a1e8cc0523e56cc9b45080e1ea00005      1
    399 0x6b539c723716628725aee9e7e26796cfe1f38fcc      1
    400 0x6b6ae848f555f70944bc99d736fe29fcaecf8b23      1
    401 0x6b6d5c57533d22d0de14db59fcd28b9ea5296090      1
    402 0x6bae54650c0d17f4aee76839a7d319dc5060d6b0      1
    403 0x6ce52498047b8279ccc7c25b41c95cd482525e54      1
    404 0x6d1db4a7e83dae0eee7e95d421722d46d2a7e94b      1
    405 0x6d25c0fa070e625dac76b29bcf0917303cd52e7b      1
    406 0x6d35392121a6687fe19d6159601b17517f73d0ea      1
    407 0x6d8dec3fd68d94a7189a98346ea52b4d32e00012      1
    408 0x6dd97134b53f40d239842716b2cf0179a11876e9      1
    409 0x6df000635d86613626c5208d7c9d71b84e091220      1
    410 0x6e1510ed41ba743344e9ba80646194351cca0c7f      1
    411 0x6e1ac9b3f4499a73385dd8d2daed6449406d49f4      1
    412 0x6e5632d334b861dfe4312c7ba34e175b60be0b5a      1
    413 0x6e88c16794cb212bdcfd6b6f8901af2f83237d3b      1
    414 0x6ecb6038d923606b3540c4c6ac267c73ab7fa3c5      1
    415 0x6edd8fa5550e29d47641f969d2ead3decaa29ba2      1
    416 0x6f0735bf1e6c69030d6990cdd580345b370eb50a      1
    417 0x6f113b0c8c7266d7514e4986e3d6aaf013b6754f      1
    418 0x6f566a6672f615c2975f6c48224c46153e12ffcf      1
    419 0x6fdd8c800cbb81790cd48a20f90ce7e725f89c27      1
    420 0x70814fb113d5b2b22902b568b67ebef657d9d66d      1
    421 0x70c8db61d09271f4c90950ba8c6cbaef918f12f2      1
    422 0x70e680b9493685f72e76243c09993fca768eedf1      1
    423 0x710852027bc25db82ba562d5b1499b6d9b02fa9c      1
    424 0x71784687d4c74338bf284bea22956c74fbe6d631      1
    425 0x71aa6c4e87225fcae2df49f977c7fc0d1e8d2112      1
    426 0x71e22168b702bcff528b8974cd4b723250b67609      1
    427 0x72703b554a7089f93ff1fc6cc6c0e623900a7b80      1
    428 0x729fcbb6e1289c88ce5113ec1c83a48a8e3c9f2f      1
    429 0x72bc237cf598505dc00d4fcc23b1663e206455a9      1
    430 0x72cea5aaaabd1cc5232bd2117e5d21e83cbd0e51      1
    431 0x72f315c115408afd9240c5be0946c8ebf7261fb1      1
    432 0x72f52de160ece454a2d75a410f85f4978045882d      1
    433 0x73bcb8c5e30bf85806aade7fc36f16c6b80fd3b9      1
    434 0x74d50ea9f8cf5652e75102c3fa049c01534dd6c4      1
    435 0x75005a05bc163b85991b9c9facbcf3155372422e      1
    436 0x7525e71f51bda1fbc326000714d2fc68ed5aed6b      1
    437 0x753c892626c4fa09d1aedc0e9b52bda97a4afa00      1
    438 0x75850227f61183759cbbfe2b3fb196e798ce0b95      1
    439 0x75cf6e0dc5294bef8cb5a6fcd23dc9badbfc443e      1
    440 0x75d4fcaffb56055bf9df3574d345473e31a16076      1
    441 0x764abe778aa96cd04972444a8e1db83df13f7e66      1
    442 0x765718a8e4a5562be34b122af259ce5a71372e12      1
    443 0x76d01054ff91afc2d515f7fea9a3e3313e248615      1
    444 0x7729cfd6841ec449c20fa7d3970d2e0d035c856c      1
    445 0x774a34da2ee2e2d242180819f1ee88783215f7b9      1
    446 0x77acac99ac831a1574b9db4d15299e98e195e6ae      1
    447 0x78143238df750e9d9f1082e12ed56f2bfa332d65      1
    448 0x782ff3f609427680db175365c24c238f89fdd276      1
    449 0x78e37b881d078d4b2f90de0422dadfd4da50ed4f      1
    450 0x78fc8aa7bb4ee047257a245ca590fac4ae4aa97b      1
    451 0x791967551708fe99eb2cd8dd580ef98c61e67ac3      1
    452 0x7988dd1dda1f35542a2b8d5f7de575563ebf068e      1
    453 0x79d1dd0770fec45768a285da42c2521603c9b91c      1
    454 0x7a231b6d33f1da74e9fb593bc785b4914a852867      1
    455 0x7a71dc357646a8149a920dbd26f66f07769f01d9      1
    456 0x7ae3b0627dac32d3ee16c204ef791e37067b8460      1
    457 0x7ae4784a907460858231609f565bd9580f609b05      1
    458 0x7af061a6ec9616135231cb8de2ed2a0a4140e90b      1
    459 0x7b8a6e7777793614f27799199e32e80f03d18dbe      1
    460 0x7c5504688eb4f81ed558550066dc218ffee1f6c8      1
    461 0x7c741ed01ee259feba4b7f9cac114f48bcafacf3      1
    462 0x7c76cf8894d42ed11b53236da54f05add1d44073      1
    463 0x7c84aab9f3eb0897191d95afb2aeb56a63973532      1
    464 0x7ca00a09e3b431d48c30e3e6cceaaeaf6385cc81      1
    465 0x7caa9f43822e288782e3e8797c8a16774c689b3d      1
    466 0x7cd9ff84abf2b23e825154aca2a62fafe185bd5e      1
    467 0x7dcb39fe010a205f16ee3249f04b24d74c4f44f1      1
    468 0x7dd14501c25c221ffe213d7d56d225f4fe411038      1
    469 0x7decf7a31168778f311c57b9a948abaa7321001e      1
    470 0x7e0051a0b48f3c5848e66fd02bea3400f979a89a      1
    471 0x7e737324cd6702e1b93c9240072ec9cc036dce21      1
    472 0x7ed69ff055d746d451f675635e9bf773accc6a97      1
    473 0x7f1796071eb91aebda10a03115fa3cd95efdb25f      1
    474 0x7f6ca49d1e50671a586a76bb082dd6b3f73fef17      1
    475 0x7f9bea812b9b6c3c4b74ec8aae849a5745cc3ffa      1
    476 0x7faa22f45695a77ad4042c4cfb2c0330caa6a329      1
    477 0x7fdab8c244e0b775edeeb9ebb8356b59068c6873      1
    478 0x8043812aea5a07dd6523b5d83abd5e606422944f      1
    479 0x808421753a181e96812796b7ab43d3f356cc5a77      1
    480 0x80a1c9fdc26199a69d190ebc8ad287ef48758977      1
    481 0x8120cd4fd4d867dae6dc1c2e9c504224ed045f34      1
    482 0x81590c16b9d1495d133cf881acf5cc0b56730b74      1
    483 0x8179e48d959a4c41d5e0432ac05b30707149d649      1
    484 0x81974c1a09e53c5129c6e4f74e547fda0adf4d2d      1
    485 0x81b55fbe66c5ffbb8468328e924af96a84438f14      1
    486 0x82139687faae8a29851902783e02e699de0e0846      1
    487 0x832834fc5b0b35ffd4cf697c4ec08815ad8d6a52      1
    488 0x834a9484537c2760ef6d6e6736d1ea800d67966c      1
    489 0x83ca0f19abd240c3b04bd55a2046c308d042e196      1
    490 0x84aeccde4c9f217e83d3fa28c31d34378b903f91      1
    491 0x84df1fb4801e72cf6958a994dc0b690f9e7cb23b      1
    492 0x8586c32dbe1c986bffb8f1ed6c3e8c6489f4fa3c      1
    493 0x85914d049aa004d037cae65bad275ed4147f172e      1
    494 0x86125855ea134f1da31015663fd3c19a657a8d16      1
    495 0x8623a32af48544b055fb6ae64f33eb43edf091ff      1
    496 0x862464bbd83d5743f9cf873c6ea3d187d0f909bf      1
    497 0x8668681d2017c2322d7ba37070a8ea223ef3729c      1
    498 0x86c71b7d27d00d7814a763f9e6f16fdb09f7e4df      1
    499 0x86ff945e6dc2470317ca082a01de7d24188671ac      1
    500 0x873dfcbbc61ef7ebadb652adb98df9f69399b70b      1
    501 0x87466eb848a4a14bfcb433e41bdf4c2a8849102b      1
    502 0x8774be790cb9e12d5edaf2eb8a3f6c89410a497d      1
    503 0x879a8a1868d620584afa0c9c8807628d553c6bbe      1
    504 0x87e91205adab9e2b0be9f7562af3f5b42fad0838      1
    505 0x8854ad314fdbcc275eb0d5f4fd6dd86ab7981cce      1
    506 0x8856dcf8d1f9be083a25251b0149ad74f971b806      1
    507 0x886478d3cf9581b624cb35b5446693fc8a58b787      1
    508 0x8874174a2366668d54fea6343f71709389563c8a      1
    509 0x88a6f6ac171e24be54d95dfa7ceff80ff185b346      1
    510 0x88f038389cbe95a917042bdb0f3afc434c680edc      1
    511 0x892eaf46a9c1cb75c3f230420dcbee6320c26932      1
    512 0x896b94f4f27f12369698c302e2049cae86936bbb      1
    513 0x8a063961e57a68a8a1f68930d758e1bde05fc6b3      1
    514 0x8a0c391ed12dfbb23d4a926a7c7353dcfcca8a47      1
    515 0x8abe2749b67ad7debb7442c46ea4a0e003d4852a      1
    516 0x8af0b9a9b751e086122bc340188bd9d99b8c7ec1      1
    517 0x8b17e8b3f8315e73986f0c8370cea4f3a974a532      1
    518 0x8b4567fa8c4715c27a682215a031033a8b544206      1
    519 0x8b6a3e1f151fbd8a45539e0942918e63d35c6cf4      1
    520 0x8bc3757a675be4c2e459813769992ec2c60baaaf      1
    521 0x8c2dabfea8751dd7901e35a015dc6c943ffa0d7c      1
    522 0x8c4138dcead987bb50929ecfbeb39ef3e28b4c76      1
    523 0x8c8fffcfc3fc82a8777886e984f9b32ab91b2a5b      1
    524 0x8ca7e4a1f59495107bdc4c7b246691d89fd3a939      1
    525 0x8d12c02671848b17c18322027a2578ea7afbb702      1
    526 0x8d5e59e11838cff72af5fb0681d96a9136ad0604      1
    527 0x8da03feb7174b4b2a6b323f3dc40d97f3421cd07      1
    528 0x8dd129965c6c770a3b34d9ff5034cd73df5eff8b      1
    529 0x8f160eb907a80517a3fa6d22f7cf20f552696a44      1
    530 0x8f4b933491e1319799229ed9d36b179bb859d705      1
    531 0x8f6504b48c66eea4e9fbf64fe220e02f1cc4934e      1
    532 0x8ffa8d00db5df56a20c30b737c5efdaabe140df4      1
    533 0x903b1b960ecfb37d994fa0cc537777d8ed766656      1
    534 0x9045ede9bc11bc1148a0743ea518e3f36f62bb23      1
    535 0x90af376346ca97d4e9d1e510e82543ef99b56a28      1
    536 0x90fa40f24362c24de09b1cb86df9f0b1f1b15a90      1
    537 0x912e844820eafe9a6c2df0615dcfea91ff32ce75      1
    538 0x9197f339cca98b2bc14e98235ec1a59cb2090d77      1
    539 0x91e8be20d0dfb2293b97317e906223c1fa833a48      1
    540 0x92b1b677728e36734d59067a4ce53e5bf1f3a2ab      1
    541 0x92c75284b0d863165a8b33e212f02ffeecb2853e      1
    542 0x93ab3c4e6daa20b2b7ea65754a8c11311fbdba38      1
    543 0x94db5f225a1f6968cd33c84580c0adae52a04edf      1
    544 0x94f052ca65a34d7f581bba95a749b1cf52689dd5      1
    545 0x953b0f8afc70e0fca9e2e43b0a4914be002c4e94      1
    546 0x954d65e5310607e12f93f6156de92cd37ffcae8e      1
    547 0x956f7db2fdb52de9453ae184d51d1883b8cd9d68      1
    548 0x957143815f0e1d1b1a31b2bfcded5b416ee675ed      1
    549 0x957b0cd4e9851537aa7cc84e9c7b22bb68d949d1      1
    550 0x95d41776812c6ca222dd5fdea56c065b55ff7655      1
    551 0x9617562c6f338ed18e2e8e88cbbffce6003bae0e      1
    552 0x96de627be6262ad2e19553824aad1af6ba1ebe9b      1
    553 0x96e861634b4c0e0a9e7c6a65dec549cc2a8a0e56      1
    554 0x97d2b1b4a249ec77d56fd1576546996d14f7db1a      1
    555 0x97ece7185467c78293f3b796bde3704421d4fa69      1
    556 0x98004498c85c4b1997b3f669475a038bbcec2160      1
    557 0x98172111480cc81621fd8b12bed2fb5095be11a5      1
    558 0x982d3c5223f6b5794fccb3208eb164d042cf2526      1
    559 0x986d1bfc94671b5633d90d0540820bd3813e3a50      1
    560 0x993220d8cc0e48e7b88c96ec7f37cc3b6e6009ce      1
    561 0x9997e55f76e34f14d7bd6b268a2e16f896389ee8      1
    562 0x99c7be4c85c4e9d26b7cab28a4e6dbfc8bc19178      1
    563 0x99f8f74b1808bd7f1e9e76b7d82151b35dfdd6ee      1
    564 0x9ad53944b52e271f1114028b918202cfaaec0a3d      1
    565 0x9b742d1e98e5bb2f4d50f9fbbc047daf288ffc8b      1
    566 0x9b7c0814eb341e6a4ca673148e8b577b13e70787      1
    567 0x9bc66bb0f94a38e55809dcf7eadfd6bf98d6e184      1
    568 0x9c37bf5faf485b3db73c95300cdce93410b83792      1
    569 0x9c7471c6d03cddab9ef6e1db6ed93fb8f9328072      1
    570 0x9caed8a01c87721570de895a9725334a0a0cdb70      1
    571 0x9cfe0d47672f0d9891dc312d242349d52d8aba8d      1
    572 0x9d45c8492f94687ec1317ee81e2b8647af5a1759      1
    573 0x9d52fd3f58557f3b5fcd2b10314bf566cabca60a      1
    574 0x9e1e3857fb2484379858b9daf230379015a7a100      1
    575 0x9e1f7b007511036b5cbe5df6d39550bdd2e8bc99      1
    576 0x9e23d2db65a1b8b6f31fb57c47148907545b3ff5      1
    577 0x9e32560cbc32e1ea4c737d65f043a0d8f8310422      1
    578 0x9e5198418081fe00026ca0ddedbfd3915ab98298      1
    579 0x9e640badecb7c628c6188b74488823e879f42a1a      1
    580 0x9ea50708e47a237182a56c481a4d11dd021b7ec2      1
    581 0x9f35af4727fb91114563d8a8f779a792cb557f3f      1
    582 0x9f3c44a7331ca9da2d3432bcb0df91186a27e3d7      1
    583 0x9f432a2964351a7830bd9ea10f65107677d73e04      1
    584 0x9f49288c4658660c82dd98019f600a3d35969fd0      1
    585 0x9f4fe19fed4a008a270840fe95660f25df36c734      1
    586 0x9f6ae0370d74f0e591c64cec4a8ae0d627817014      1
    587 0x9f9ca0285dcc35cc6831652e79c029fd0ed4bc75      1
    588 0x9fc80955aee9e3eb8c511a50a72b7e589700ffd6      1
    589 0x9feba45425c51b03890604d6ad51f4ee7c9d4866      1
    590 0xa03d04ff89308adf54badf18b46fee9980093524      1
    591 0xa04f4a4b7306cb72f30828834db01699362a4989      1
    592 0xa081ae42802e0be86f5a6dd9274adf50c9986a1d      1
    593 0xa0f1de4882a5cd1851989d9a1e9bbe9b4604e9a9      1
    594 0xa0ff0e41d6847b1fce33a721a766e9e040027e6e      1
    595 0xa10378da8fbb61b408e7816b3e85bb003c485787      1
    596 0xa10bb823fb55a9199e4f521d0a88993c4cba7150      1
    597 0xa199f8ffecafa4f3d500d3ab1d1c8e0b49c9dfd0      1
    598 0xa1cecd5e4f6683189c3d4f832a16600c20e5b42a      1
    599 0xa1fbc0a9ff967ce51b8ea952b35ff7005cd4cc63      1
    600 0xa21e1e6c69dfed02b6a69c4c89ea46a1e6e31b86      1
    601 0xa222204acf1be4077d34102fab38a759060b77c2      1
    602 0xa225158ea124f311cd0d50cfeaf3407b9412b077      1
    603 0xa2917120c698fb5f2a03e3fd3524bda85a3eaef6      1
    604 0xa2b55ffb7ada20a70a1e6e79073f2f4d6623d72c      1
    605 0xa32c38646299818ccedc6401818c2e1639c39c08      1
    606 0xa33e5e1ccf57c0caf78ae399061f270dd24ffcdf      1
    607 0xa39d385628bd00438f8a9ba4050a25a0210f84eb      1
    608 0xa40117a1394fc5cfb3576fe12632381b0b79c0c0      1
    609 0xa41341bab6588948da8503d1dfb0a0ab0ea269cb      1
    610 0xa490a0346808dda91aea6698cb19e4697d9fc5cc      1
    611 0xa4b61e227361a9cd9e62ca10946c27748a382cab      1
    612 0xa5214719feb1d07b66292a5d4d0a2712bd048435      1
    613 0xa56c04347abee42f663eff9bc2d0147b97c8f782      1
    614 0xa59422ec0634a0263bcdfce5e29dd2df3803447e      1
    615 0xa5ce27ca5e31b1de92db2d2bc109b3e23cf1d4c4      1
    616 0xa62da2ea9f5bb03a58174060535ae32131973178      1
    617 0xa71000e72d38e6a84d7190f59fd3dfc73931c0e8      1
    618 0xa711818b11bdd5797042ade80e3a59687558a4e1      1
    619 0xa73769aed346319287410811639ac3bec8464d55      1
    620 0xa749f12e124790d45e62bcd3f7bf1d2218f2f21e      1
    621 0xa76bda01601ebdd50512c2103a329db572d7a77a      1
    622 0xa7c342af335ea7e0747452ae2e3c1901498a9a76      1
    623 0xa7cafd18dd8bc1e23203058d66f89f0f0ee539d9      1
    624 0xa817b9bd1ece360e4a1692894a5ad1d40b889f20      1
    625 0xa8450d71b98ca4955c63519ef593ba886c5b2b4f      1
    626 0xa8806ccb79069c976e37ed1950ada0eacef6a289      1
    627 0xa8e54b46ae93e14eedae486a9efcd4c7b5a5be20      1
    628 0xa8f1b7bee4bb60311511144ecd6dab7b04ef2735      1
    629 0xa90aa01ec98f390cb86cd1846f7c0fd342a07955      1
    630 0xa9237c4eeebc879208e59c867cb6d7cbffc1df30      1
    631 0xa94ba6f81ede0d81a5252e71916b5c5567ad6a01      1
    632 0xa96d4549729c2a826237d91b3a3700cad7dfec4a      1
    633 0xa978eadb605761725d11d3b3a4045cf5859e2d3c      1
    634 0xaa1d3f5d45333e40467e989d472effac4da00da9      1
    635 0xaaae5c0a8e05ee5b3824b2e9fe939d5dc3ba3336      1
    636 0xaae8c50d76c76fb8947c9a203103d28b55862977      1
    637 0xab2056903a7b62bac46f45a3d7a70ac799ca88cb      1
    638 0xab3ba2d668215acd23f7da923683a5ed88bad625      1
    639 0xac01ad9a3e3c3448a5a0e6fc798def034a01a67c      1
    640 0xac02b3653a6fd4d507df142a0337951e39c5619b      1
    641 0xac1a04679039a1718d3820fbc254ce29269af784      1
    642 0xac6e4334a82b8434b0122bc3c9b42d0a5f190d4a      1
    643 0xac772daaa5079e005eec3e53314d6d1e9149dc87      1
    644 0xac9169946d4f6fea622900fa6aa1812d5500bade      1
    645 0xacb1568482f9af61568d5fd05a54bfe8b7e60ee3      1
    646 0xad102dacb54816ee8b128c63b83c7b5fc587f4d7      1
    647 0xad5bd06f5fb4633888ac7506ec0c87eb99998653      1
    648 0xadc9e7a7d129be37bb89d05defe860ea28d4e6fb      1
    649 0xadebdeab678647a457743ea3af98f8b804e45c24      1
    650 0xae0d16586e5d60d334624c115216a52b9b1a0335      1
    651 0xae20574304b6264d3129dbfe5df72eead8c5b208      1
    652 0xae72c6a6fad9fa9d82d089e1ebf73b3043855425      1
    653 0xaec3f7e735505c2b787b05d67f4adaefe3dfd678      1
    654 0xaf8b04fb70bac8a686aa429fb59428e829564924      1
    655 0xafb4fccf58bfc06ebc8ccd387b68140cca05f4c5      1
    656 0xafef3bb7fc3cd8ae083672b65f79e6cefd4ac619      1
    657 0xb05b7ea3714a95857e45629def2b9c3577690208      1
    658 0xb08f95dbc639621dbaf48a472ae8fce0f6f56a6e      1
    659 0xb099a3c3043b262518d9b6cd4d5c9f589b15e2ad      1
    660 0xb0bdd53b627d7e61cfc5c13ef110e47e210fac6f      1
    661 0xb196931ec22517b0510705eb56d5652fe73877f0      1
    662 0xb1c11a879c47debc22e3816e7b727fc8bbe3c8ac      1
    663 0xb26c452e8ac20ae7445fcf46b6478d3984f5d8e9      1
    664 0xb29479ef88dfff8dac466cf6c716e7985c4241c5      1
    665 0xb2a18003067dd5f96ce5a51f276c01873c136e5c      1
    666 0xb2aadf6bfc0a5213acb9c279394b46f50aea65a3      1
    667 0xb31a4974499daad3255206daee7d1f3595fa259e      1
    668 0xb335326c7f2cd2e4eb09ce3d1745c92f66497b7e      1
    669 0xb33fb83c645ac2a12b138636fe59604312092484      1
    670 0xb356ab415aafa4b688a6c457e61a4a8f7d5096f0      1
    671 0xb37c5603fb0c6c2e2b62d0525affed9e1e5a6a19      1
    672 0xb391806676cec7fd36dc136dbd4097bde13e5b5d      1
    673 0xb3b16685ca2d2a764882e2bb2a8d3d769cd74145      1
    674 0xb406ebb1a43cdc9c6eb5b392c712329c0b84d546      1
    675 0xb40969f60bfa246a09593099dace2ddd543254a3      1
    676 0xb42ab92f5af0f162fffefa2b1e12702ce3fc9e17      1
    677 0xb4627672ee52660a9e453ec541834e04583f3602      1
    678 0xb53ee69b92ad6d12e6f3b1f849ad3e706e31a263      1
    679 0xb5976803fe7b4fa10834e98433f38ba208c53c0c      1
    680 0xb59a6d337f8d687447fb311b8138340b8c617715      1
    681 0xb67a577a1855dc3a43df1d94b05a8f5d29570f89      1
    682 0xb692704d590e224c67fa96830c4b2526fccaf3a3      1
    683 0xb6c189179b204d14fcc0b608bc97243b389b0030      1
    684 0xb6c7e03ede87692deb1de7e40723b952e0fd3773      1
    685 0xb6cf25f5cf8a1e1727d988facdd47f1dfc492caf      1
    686 0xb7546d1b04151e1e96d38275af66eaf984fdad2a      1
    687 0xb775df51776d6767f8c2f5a14b667e300f60447f      1
    688 0xb7abe0f0a1c31a88fdcdef71033cf7ae7d12f2d3      1
    689 0xb8700157128ff544c46cf35dec5a19120baa8eb2      1
    690 0xb8937891f08af9854a5ae7a5ec0cbaf4e68acd4c      1
    691 0xb8b8ce5e3d0ad5c0169e130c64048b6d3cfcd343      1
    692 0xb8dfd425f4d6227036d5342cc4ac2b90826e1b05      1
    693 0xb8e82b32ea205b0546d0ece1a175ad00e404dfa1      1
    694 0xb8efa945cf7f35854afb80e9ac05f5c1dc172fb3      1
    695 0xb90aa714bd30e6f135ec15a0cd2605af1590d184      1
    696 0xb976163a6f251c208bcf3196b9c4ddf927b63e72      1
    697 0xb97624935dd3fb84e5492e8d01c6fcdce8060cbc      1
    698 0xb98af149b33dee33c97650e877a497bd6a934d54      1
    699 0xb98be1aba8ae2adef0daa18247273fa06021daae      1
    700 0xb9ad9d091c6841e640dba4cab02baefaf1134cfd      1
    701 0xba12fda058a14eb03c14613601c3a30d6f955196      1
    702 0xba264037d93f4e06d5b19e9a73f29fa64689e1ba      1
    703 0xbaea3cf94abd0d6e0f029ef5b0e54e9424a72985      1
    704 0xbb59498b19783e5d81f72ad07acdac619b6808e2      1
    705 0xbbc37f68e9876d64b2c55016081528ae0a85d8b2      1
    706 0xbbc6e7b905d7794a2318e22ec011bf36b09e1d2b      1
    707 0xbbdd72fcce73c2626719be00259ddffef0d5673d      1
    708 0xbc1eb4359ab755af079f6ef77e3faac465e53eda      1
    709 0xbc2ab46887142d546a976d56eb9a3d9da147ee65      1
    710 0xbc9ca5bd0f07700929f8d538233b0a9e60f4ddc5      1
    711 0xbccc968eeebffc2586a50eef0ee1cf12273c9604      1
    712 0xbd3a5d12af9fd82fb927fd26216e4dc2722d2337      1
    713 0xbd751cac1fb27d930a6a0394d97c77908d94ad5d      1
    714 0xbe2f803bfbcffbcd77aae4e9104406abfeda497a      1
    715 0xbe67847fed0f9760c36c6c627c513375673781f9      1
    716 0xbe8fe12b9eb1ca2a593e6c070c71c294b6fe9f00      1
    717 0xbe9998830c38910ef83e85eb33c90dd301d5516e      1
    718 0xbe9a677708192ae85e54fb38457b3b4f01c281cc      1
    719 0xbf270918afe2ad16093ddce904fc358ad337cefa      1
    720 0xbf9d462c9c6880d44001bcb8ca4a38a351d583d3      1
    721 0xbfb7969e75ee4e58620f74135d167f5bdd60800a      1
    722 0xc02e6b0d0c1a5d8cd26beeba0fe8d76c5d2f19b9      1
    723 0xc02f533a819c4d3149544dd1a55cf7cc87a8d30b      1
    724 0xc03e57b6ace9dd62c84a095e11e494e3c8fd4d42      1
    725 0xc04208f289d3842ac168f2c373b3164e1d872650      1
    726 0xc06a00899ebd1481f243ac4ec92a276c1ff26e8a      1
    727 0xc0bcbc0a91cd42e4808faf190e0ad6e9a6c027cc      1
    728 0xc0ced4439c1f0871f16a46e13fbe41fbf61ba265      1
    729 0xc13d5024c2ee14c5f80847afd09275f8b550a135      1
    730 0xc18d4c2fee42accfe7bfdce98bfacbabf76242e9      1
    731 0xc1966d218b7492dd9cd672a5a237cef4d82004e5      1
    732 0xc1b0307ff325527511310c7d6fd3248188742f86      1
    733 0xc2419841dcb9a0f8906d06463ae24e00e1470846      1
    734 0xc26012491b9dfb2e6f2cb0305673e212721d5950      1
    735 0xc29ded4c6ede7482068197c3e3205d850ffedb0f      1
    736 0xc2e8ed8cc0be70f91fc9aa903d5f4538719d7dec      1
    737 0xc30e9fda779efe2081dae6529c0d07fc16769c8f      1
    738 0xc33164f7ecc76869fafd44363cd094a22e0c296f      1
    739 0xc3c1744bccfe4691e332c873f9cb99b53214e03c      1
    740 0xc40d07925d1ac5500e9dfc4e0a05eb24a75d195e      1
    741 0xc457fee0564bf3eb972114762f62ab45c0e3590b      1
    742 0xc45920062985116eaac6589058ed337066d6f2e6      1
    743 0xc4c65d75dd6579671db24990bad1f502d7c96c8c      1
    744 0xc522289168311a765cf17c067f0118578c99cf08      1
    745 0xc58326c7020f26345f4568cc09daddf019a8e6d4      1
    746 0xc5a2c3a524be0792519655d4093b44b988db4f68      1
    747 0xc5ed0799c911bf8147680756825725eb038451c8      1
    748 0xc6411ff69f1ec6bfb4b082f47db61dbedcab250d      1
    749 0xc659fd78a395d70ec69ff034b4378a712e7edb64      1
    750 0xc6bd194286802c5975cf681201e41f21a4e256aa      1
    751 0xc6cf5a020bcfc65c6f8181c04cbb5ef5050fe28e      1
    752 0xc78cd2e1e8ad4a288eddafb139c9d0891ad01ae7      1
    753 0xc7a295b1b2df1efb82aa204958b98ac30171cb85      1
    754 0xc7bb15c11595c877302ddfb330a4082d92f5bcd7      1
    755 0xc7c6eec4978799e6a53da79b540432820a349d0b      1
    756 0xc7d2d152dda8cf7a7860288c3db9a1a4ce29162b      1
    757 0xc7e8a6abb9b5c76c75c9bb4f77715793f7f8205e      1
    758 0xc7ec028c706ec2fcfdcf66bba7e80ded634f6ca9      1
    759 0xc8725543112dbfb55d54edcd9c9da2239ca1bb92      1
    760 0xc8cb180415b83f3f9a61691d246fe39a74c6a41e      1
    761 0xc8d3e10a72d2d846ebd134dfbd23e14cc26a4ed5      1
    762 0xc94a284808e31b9ef43f9c9a48933e5a268cff9e      1
    763 0xc952ec84dd5d8ec32937a8e4105922c664564450      1
    764 0xc97958ff5370c56b7e0e6fb58437f00702714d49      1
    765 0xc97a5623578a832354988e7e40869f5207193d53      1
    766 0xc99360f97475dcd02cbaa1940e0e6a2cb67186c8      1
    767 0xc9ae113b8451d0afc8a021d796322d545a7e7dbe      1
    768 0xc9ceeabc1ac7b96fc45a1c94edaa3b10197cedfa      1
    769 0xca288359ba6319c7e7c7377a670183acb3781cda      1
    770 0xca339bd4739227f71b646f8b23ca098c86f6c3a5      1
    771 0xca3dff8c740dee29528916eb049cea48f500d387      1
    772 0xca6b710cbef9ffe90d0ab865b76d6e6bba4db5f9      1
    773 0xcaa1c396e70384db58dd33be74b26fb119e98c3a      1
    774 0xcabd5f77ca9d48f4ef9793f20de42ad39ce93979      1
    775 0xcaf3365d474690a1ac6643d3d6ef44cb0c6deec4      1
    776 0xcb2e86b62b822faf4f50c3251981695d5058317a      1
    777 0xcb85b581caa71949784d2e826cf9df391c244b33      1
    778 0xcb85cf75d53fceb37af86b475903817db2b99d17      1
    779 0xcd094518f2d30fdd8c7911636bd8b52bd5303066      1
    780 0xcd30b6de9dadb0a1598eb12a4ccf34b98c98c1df      1
    781 0xcd69150ece65cf880eaa7b91ca6fbb7d38e97cc3      1
    782 0xce8ad80ce1a979381d209ac230d02adafb9fa897      1
    783 0xce90a7949bb78892f159f428d0dc23a8e3584d75      1
    784 0xce990032a3ed14e197421270a9bec9e276bf2f31      1
    785 0xcea266acb92d5603dc53b5d6bc6e568dcde0d311      1
    786 0xceae4ca7d082856d140f3672780029d6e90c3dcd      1
    787 0xcece625176cead11c8d2dd6149fda2496b0c192d      1
    788 0xcf04c33bfea64cf5dd6ea97e2d5620f825b03b49      1
    789 0xcf6492639384eaf2dfdfb62c22c07928693c852a      1
    790 0xcf803a49f6e37b92a4040e397b976939dc5d9841      1
    791 0xcfd648eb66b736351f48dbd5a1752708371c10f4      1
    792 0xd02cf646520827d74d4ff0435f96220819772b4b      1
    793 0xd0b2d2d86a45bf2f13a3af83df6e7b5fe98050f9      1
    794 0xd0d9597369fc67b9eb2703d6e9ce5fac0a77f33b      1
    795 0xd1042637cdd71f99f3dc5877030e67f3789e1c19      1
    796 0xd1380a4e089950ee3a23d818e24ccbbef003a432      1
    797 0xd1386560f0fa070b6b79e9968e8197cf17f3b8ae      1
    798 0xd193a7d7b231c204b76b9d638768ea602de515f6      1
    799 0xd1afbe4da2a5adc6faf30fcbea9e4eea0ba8c70a      1
    800 0xd1b643c63d4dfcf4fae32b58d87843553a64b58e      1
    801 0xd1dd467f1630be10d67205d6c816429f8ee49124      1
    802 0xd1e508c60d923cc9e93785da0bffe01710ccbac1      1
    803 0xd2b9f9d1e6734f241b2a6eb64ef7f6e63af0a97d      1
    804 0xd2ce17b0566df31f8020700fbda6521d28d98c22      1
    805 0xd2f2a56affb67a9ae2499ab9df1689ee78fad89a      1
    806 0xd3069c9e486a8b77add55ae3ceb3a4b138fb76c7      1
    807 0xd3364ea1c742cc2d396ac65e1723b8d608a939bb      1
    808 0xd33744da3013927fad387d24f57cfa241735ded9      1
    809 0xd34e3c34ae9828dffedb9a2f236af47119a113bd      1
    810 0xd361363a6401870ff8a23b7c77c004aecb8c8b84      1
    811 0xd3641bd03a67af07550b049065a19f84333b4b5b      1
    812 0xd36590461162795ee33099b2076a0d4e017ae17c      1
    813 0xd3b37c6567e2702aa727cff4ce5939b4a07a2fde      1
    814 0xd3f5ccd478e59c82ab75f393843015e08892a94d      1
    815 0xd4e791d799b8873d86ac06a5fedf2f1a75f639be      1
    816 0xd4f358d4a415b4abb6f6deb40b86d7db62562960      1
    817 0xd5ec003289265705727b622f1700fe814e54ca67      1
    818 0xd6b2735e290ba3726d25c9762098cd26a16f023f      1
    819 0xd7192081e5f481364c190022f0012a729fba37a5      1
    820 0xd72c8dd33954e8aa53d5c108906b751ce4b2382b      1
    821 0xd7342ea20a5afbf24352b5ca61e09844167914cb      1
    822 0xd7473ef09ae9c072f2ece3fe7ce64e670eeff283      1
    823 0xd74e767c77d2e9f9e467e7914f2379da81b63a44      1
    824 0xd7d16a716f6dfcef1e9553bf12136463f6414f1a      1
    825 0xd7d941ff2890bda98f40a5dda0593d239a603f09      1
    826 0xd7e32b8a326ffd9e784a1ee1eea37684a7512171      1
    827 0xd8d97db2a8caec33d58420aaf637cb41058cb58d      1
    828 0xd8f3d5e9a199a35429870fca8befd159bfe48747      1
    829 0xd905de91f928b20ea0ccf455e5ba07eb6373955c      1
    830 0xd939fe2e4776ccaddb823a26eacb45c66d8ad5e9      1
    831 0xd981a38d80de1c1528349b841833d3a9a745cb37      1
    832 0xd9e2ad004ac82915d7472448cef2b182547487bd      1
    833 0xda23ab0b2636c3f9806a868278d73aef2439ab60      1
    834 0xda39876a118f1690e584351dd01b785a8f1297ed      1
    835 0xdafa9e3dae493f0b9d6872eff4fda0f40d1b7488      1
    836 0xdbc7a4afadbff03d1ac183c9f664a95775c6d52e      1
    837 0xdbeca36c48306867d9285e0e0d5ed1f5aec28a35      1
    838 0xdbefee517025559e7898d3a48f18221c32d3fcf5      1
    839 0xdbf55298b349984a50fa98c7c6965994e30fc3f4      1
    840 0xdc78107155918e230246439e4159fea4c477eae9      1
    841 0xdcab53e3c8a2c37cc5e2509c2db0caa6c04f3ce0      1
    842 0xdcefc9ff1e47e458e56959154c1cdcf56003d30b      1
    843 0xdd5cecf2835d465d490f79ad9773a848805a3219      1
    844 0xddc27a0a0c24d60b607dbc7b008f2016585e57bf      1
    845 0xdde27b3ba448e15357183060ae006bb54ebb3d86      1
    846 0xdded73b0bbdb3782a453c9202f54c746fd391068      1
    847 0xddfd836f7c9e42055b1f6ceb005fee4c7882f161      1
    848 0xde112b77a1a72c726d8c914c23dceaa53273c351      1
    849 0xde24926ce7fc297695a05bebc3e0239540978dc3      1
    850 0xdebd22d7f63648dfb69bb6e2a4c88264d578c0a4      1
    851 0xdf36b093cbc06522b68105c3d9141db188e980b6      1
    852 0xdf8b134fb7743acd805ecdee11335dd0cca921fc      1
    853 0xdfd818378e60a1b7e26cf165cc57de8ff0a65478      1
    854 0xdffd6968900b3f4f667b69c5f9df65d9d7796a1c      1
    855 0xe05adcb63a66e6e590961133694a382936c85d9d      1
    856 0xe05e90015f6577ea1b1eb682ff8e1db1b7b6bcce      1
    857 0xe061ea94f07de987a55a49067b0d7ca3feaffbc7      1
    858 0xe07caa87dfc96fbc74f4c113270cd385574d5bde      1
    859 0xe1759f0cd1358c049e7f64c26e13de9492f9f888      1
    860 0xe1c22662f4f1320c6e5319d002b3c3d52f0d0135      1
    861 0xe23e0bd6e320c87367b0e4b797b42dc9b4fe7ca0      1
    862 0xe25b24cebed3055236e369570a437a99e1d32602      1
    863 0xe262b5a7fa6d23be9007eeb93833d2e51d89834c      1
    864 0xe28470253f0c9c7afbae7f61795d6b1ca4644b2f      1
    865 0xe29bfd5761a18b5c3ac4a4271eb7fadd4c7fb942      1
    866 0xe2b34bbf669096a794397376fb0587e98eb81016      1
    867 0xe2b76c9f15a1e5841a91b404ab4be1c3e5d19551      1
    868 0xe2d22dc1c2f7c58f86606e405325c69f5210a6a7      1
    869 0xe3013b1d58b257684ed9862dbeef81d863e2e0f3      1
    870 0xe3b41ae8785e4107cc69f988042ff4a66a367fac      1
    871 0xe3d9a03a6422b56e7097ff41a86045683f0aa150      1
    872 0xe3f3e433102cb26a5e8c88308c9308061ce6cf3b      1
    873 0xe418a9a5e49dde0d13e1ef51d4bfb7fcc51c28df      1
    874 0xe4c8335467ce3b1854dd828f14bbb33aa4771818      1
    875 0xe59e088489a85a6de407768deb671c7e493fd799      1
    876 0xe5cd0fc813d020644c496dd964a32eb9ac17e50d      1
    877 0xe5fd5ec4695b93d7e43f5d96657a535e0ff499bd      1
    878 0xe74ca58ef724b94fac7f2e09e2e48536a5c1ad03      1
    879 0xe776de32c9dc8889e2dcf1cf0606010a51d1abb2      1
    880 0xe7985c511d1bf0c7668757650b974513628dea7c      1
    881 0xe7c5ef2cee9c15b04fc7dea61a1ec443dd8e7fd1      1
    882 0xe7de1e998ee34918cabab534282803fce02e3f40      1
    883 0xe7f31a4516dbebc0f443a8d4ac8b22d1a8f096e0      1
    884 0xe831977b52714501b52bada9034021a7cac79709      1
    885 0xe8a05c7f4e9c1aa060cf941dbb50381f342d7d43      1
    886 0xe8bee2f6bf252ed9efbbd483ca4093ace7350c92      1
    887 0xe8eb234375d59df64823ffda70207e26334ceeb5      1
    888 0xe96ba1a10f72b58934b9ac0e3fb471d2ba65b757      1
    889 0xe96eb4507a1d162bbb99301fe592d310e9489e40      1
    890 0xea0ed16746257eb2bc11de2fefd63cdeece23a98      1
    891 0xea39c551834d07ee2ee87f1ceff843c308e089af      1
    892 0xea9f3a983d965e582c34eb852c18babac52050d8      1
    893 0xeac5f9b3cd48123a69fe69ce93a7f58100a56552      1
    894 0xeadd2d51e75a8b42302adc2aff0157a4bf626adb      1
    895 0xeb43b5597e3bde0b0c03ee6731ba7c0247e1581e      1
    896 0xeb775bf133c142e873f2ba6925d53107550e8703      1
    897 0xeb8661e8548dc074b9ff411080fdd9a377e6ed1e      1
    898 0xebe4c00073f1061ad27859478adcb4f5bc8ca67c      1
    899 0xec034c3abd17fbe51b10283d0902a1210041fc77      1
    900 0xec47cbbd9e05f8c4d587eb604f41740b0b2f33e4      1
    901 0xecea65d22233e7beed752a06c46cefc6c90a1d7f      1
    902 0xed1e25178d7a6438c356d3459fc3efa9458ad52b      1
    903 0xed690606f908361c26bd2b1ad4fa31ed201dca4c      1
    904 0xee2c055f7706b9dfcd98cd5a23d5629d6316c0bd      1
    905 0xee3aa3fa5b780bc73da73a82d47fa2d8cdc1a647      1
    906 0xee58519040de3504932f4c79e1d056ef1b42a026      1
    907 0xee8fa9b1d96b23af51e93d3fe050258edb606f93      1
    908 0xee91d62eb5aaea933efbfd0790613af5db305006      1
    909 0xee958e45f3464d712b8830deb5875c8ac105f698      1
    910 0xeeb9148a32b45672e16591ee18ceabc773b099b5      1
    911 0xeecc9f94c674161e84ab0c60c020432ea8f88bc0      1
    912 0xeee4ebaa421ed7aa32b925bbe2fe7506e33e7cd4      1
    913 0xeeeb5dd03eaf4ab6b72949191d8ffcfe27f7d973      1
    914 0xef04cb0003ca57dc7bb563d5ea3248241b145bdb      1
    915 0xef15bd3f923245b24d64d6fbf57f036fe650c23a      1
    916 0xef5ab90a44b68d4f5e3f6be6af4bedb12cd2c66e      1
    917 0xefb3da5189a6169a61176d1357579e135a1d1187      1
    918 0xf0038d118400ea9510ea3e93f264b770b81145ea      1
    919 0xf054274dd74987395d28136e53f39ef4f7b19994      1
    920 0xf08dbb788c290cdd919e1c124f183988e91e9012      1
    921 0xf12e159643edeeba920518cc614820ab5726335e      1
    922 0xf15a6b54e68884d27e1bebb1624d70c227b7d04b      1
    923 0xf161ff39e19f605b2115afaeccbb3a112bbe4004      1
    924 0xf1a17ba0d48798a3cc2bab1fb3cac942c4d6817b      1
    925 0xf1e1c701b49b1dc2405a9e8ef40f9f88802b80fa      1
    926 0xf1f476f144df01480297dca47efca565e8b0c9f1      1
    927 0xf262c25f75e93ddf7c1fcb4bb0d65a707ad6d023      1
    928 0xf28fc8d0eb98a34f2b03d384f7db6f1e7b877a33      1
    929 0xf2c5f1fd977dbd6de9d04bc4e62dff722d4bb1a1      1
    930 0xf33f6c5302fee8dcaf6363f6862071092b3d56b0      1
    931 0xf3998eced96ae3cfe880ac9d2637b4118da48e22      1
    932 0xf4141aef39803327497f6b81a21bb3f2acfa2436      1
    933 0xf44b63f62c7b6c59a883fdc67bdcd692995b8bbd      1
    934 0xf4aa421960cce8cf25190af71acc6949e848fe46      1
    935 0xf4dde6ea673fb810b6f503bf441908bb30b929f8      1
    936 0xf4df143e90c2095e173f861a22f1e6a2503d06cb      1
    937 0xf54913955c6035a12465e24ba2f162df2a026d9a      1
    938 0xf5d4a3b9fb77d1d5600a10e0e1c4efd12f9be751      1
    939 0xf6109779752d1fdb8b6063a158bed1b02000b700      1
    940 0xf624e9324f9b330cc0289775d7b91e945e881134      1
    941 0xf681efdcba35c69dc0fe56b7fdb56324f5abd385      1
    942 0xf6ec5b0d097178a0acf493afdcda2a54545ab0f3      1
    943 0xf6f85d9b96a43c87fd29e2facbf644df6bb029b0      1
    944 0xf6fb4e6cdd7ed50dabd90d10b1ddb60a85720737      1
    945 0xf74b25d3f6616e51b2d8be6c8cec54b045229655      1
    946 0xf81489f074a9f70c294164e07692559269f3defc      1
    947 0xf81c1d67b9c1f80eb30c610e2a78c2a1b1fb013c      1
    948 0xf84408070b4de16d6bb0ab2fd8b19d37e3fd1422      1
    949 0xf868a2de45bc38844b16c9e63fda5e1de1d54a22      1
    950 0xf86b8b8d3433de1514cb36eff3bd6ff8f707c2a2      1
    951 0xf8be957f65e67fb0342992a51c30290d5758f880      1
    952 0xfa69519696e9d4abdf7d054c3ba44d32fe350ead      1
    953 0xfb3eb8b5af7d9c38a1b6d6f4a4882cd07c85d2dc      1
    954 0xfbc76261fd55cf91b81a97dbcc2b3f6118f2b935      1
    955 0xfc0ac71b8ca9b219b3c74625755f62202d19ad39      1
    956 0xfc382669ecf6a4f19efe1e32c64d75a1daffb1e1      1
    957 0xfc5ef50b9d7a080cd620f404efdfa287af9a3ac3      1
    958 0xfc7d068f069e712dd35b439bf6ed9df8f58d07de      1
    959 0xfcbca52b23db28470eb97680d18e988d8b60b608      1
    960 0xfcdf3c2b2b6ba00e6db59c70c424b27970ff3974      1
    961 0xfd17019d6a7ddc7ad585afa68dbef71084162601      1
    962 0xfd849d8cf365fe98bc60025ff7fab45cef0f557b      1
    963 0xfdb325752f9793ae7fa4aecc641cf5386846f566      1
    964 0xfe7ace0f186a54c0be46f992dd3072e0053a1010      1
    965 0xfe8312a959e031c7d4cbe3f9cdd3ec8726d0d80e      1
    966 0xfeb41f7dc10c98fb5a7995fd9c5de7c83e02dde7      1
    967 0xfec4465b6aeac9c90e552ae7766bdc0ee0d8dbc9      1
    968 0xfed52d251e31178ff8bf4a0d611721c544f74fc0      1
    969 0xfeea8258077cc06444679958185f4198dd4cd324      1
    970 0xff08bbef8f2b6986018fe3bb62850aa7db843b40      1
    971 0xff1a6681aee53d032e8f5fb3d27ccf29d493e922      1
    972 0xff3bc8de74bb1d2f9066c9687f62bf810c66c5ea      1
    973 0xff4f0dbc9ee7583c0c61c7333a5daf8d0ab4aeb3      1
    974 0xff7e887f2b7c7594e601ed1c2e44b55bb80f774b      1
    975 0xffe6832850476eb6d5ff184d747ed84f1b686aa9      1
    976 0xfff39900273ffb1045c7cfde62df1720b63fd6bd      1

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
