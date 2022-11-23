
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "16028869.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:16491       Length:16491       Min.   :1   Length:16491      
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:16491      
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 16032169 # https://etherscan.io/block/16032169
block_hash <- "0x3de237a6c074993001b21f7ed6741158f3ea97a021bd504023fa5924784aeb57"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4399 

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

airdrop_gradient  <- pick(snapshot, contracts=c("gradient"),address_remove=address_remove, address_pick=10)
airdrop_memes     <- pick(snapshot, contracts=c("memes"),address_remove=address_remove, address_pick=50)
airdrop_cartagena   <- pick(snapshot, contracts=c("Carpoolers","DigitalHeritage","TheEndOfTheCity","AssemblyBliss","20thCenturyPhotographs","SuburbiaMexicanaI","SuburbiaMexicanaII","SuburbiaMexicanaIII","SantaMaria","Eponym","ObscuraCurated","ObscuraCommunity","ObscuraFoundry","ObscuraMagnum","TheWorldToday","CarouselCurated","WHOWEARE200","GuyBourdinSR","HankWillisThomas","MitchEpstein","KatyGrannan","LaurieSimmons","Laszlo","JoelSternfeld","JohnDivola","GregoryCrewdsonEclipse","GregoryCrewdsonCathedral","GregoryCrewdsonDreamHouse","GregoryCrewdsonBeneath","JoelMeyerowitzDogWolf","JoelMeyerowitz","PieterHugo","CrowdedFields","SelectedPeople","GregoryHalpern","JonasBendiksen","AlessandraSanguinetti","JeffreyMilsteinAircraft","DavidBrandon","ObscuraMintPass"), address_remove=address_remove, address_pick=20,address_max=1)

allow_gradient    <- pick(snapshot, contracts=c("gradient"), address_remove=address_remove, address_subtract=airdrop_gradient,address_max=1)
allow_raw         <- pick(snapshot, contracts=c("raw"), address_remove=address_remove,address_max=1)
allow_singles     <- pick(snapshot, contracts=c("65291/1s"), address_remove=address_remove,address_max=1)
allow_cartagena     <- pick(snapshot, contracts=c("Carpoolers","DigitalHeritage","TheEndOfTheCity","AssemblyBliss","20thCenturyPhotographs","SuburbiaMexicanaI","SuburbiaMexicanaII","SuburbiaMexicanaIII","SantaMaria","Eponym","ObscuraCurated","ObscuraCommunity","ObscuraFoundry","ObscuraMagnum","TheWorldToday","CarouselCurated","WHOWEARE200","GuyBourdinSR","HankWillisThomas","MitchEpstein","KatyGrannan","LaurieSimmons","Laszlo","JoelSternfeld","JohnDivola","GregoryCrewdsonEclipse","GregoryCrewdsonCathedral","GregoryCrewdsonDreamHouse","GregoryCrewdsonBeneath","JoelMeyerowitzDogWolf","JoelMeyerowitz","PieterHugo","CrowdedFields","SelectedPeople","GregoryHalpern","JonasBendiksen","AlessandraSanguinetti","JeffreyMilsteinAircraft","DavidBrandon","ObscuraMintPass"), address_remove=address_remove, address_subtract=airdrop_cartagena,address_max=1)
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
     1 0x5fdb5fdb61bc2975f3be446c5ae9c5df490f55d2      1
     2 0x6f113b0c8c7266d7514e4986e3d6aaf013b6754f      1
     3 0x8ba68cfe71550efc8988d81d040473709b7f9218      1
     4 0x9224bbb4e0fbe2f2f8fab55debc41eb21fdfb804      1
     5 0x9dbd781eeba135ad2a779926880adc89196a3265      1
     6 0xb6cf25f5cf8a1e1727d988facdd47f1dfc492caf      1
     7 0xbba3ced54477c12fdf16d7009771affc7a8c9ba1      1
     8 0xc2419841dcb9a0f8906d06463ae24e00e1470846      1
     9 0xc26012491b9dfb2e6f2cb0305673e212721d5950      1
    10 0xc762b1081c56b3fa487c7372f7284d9558a84859      1

## Airdrop Memes

``` r
c(airdrop_memes) %>%
tally() %T>%
readr::write_csv(file="airdrop_memes.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 50 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x144c704bf25f1865e4b24fd6596ffed7d92470b0      1
     2 0x22e8efe40ddb7f13b17b4c10f768967fc7a9f875      1
     3 0x23602ca06e977c86339ffddad74966e824ab691e      1
     4 0x248458947c120ca057ec028b3fe7e4b3f26fdb3d      1
     5 0x27fd36e07bca70e64bdc1425e6bb5383d5089b62      1
     6 0x2c8875f34ceb219f61b7453b2c5f100ec2f6ed33      1
     7 0x2e0d63ffcb08ea20ff3acdbb72dfec97343885d2      1
     8 0x2ec4a2bcd4f33c7c9aafab7cfa865ec15508bf62      1
     9 0x2f2242b447a54cb110b8a7991cf8a27054ee6921      1
    10 0x405020c797a64f155c9966c88e5c677b2dbca5ab      1
    11 0x40e6d6e798dc878e12987ed978f23c2391f1f570      1
    12 0x41254fe73d3b49f0fe3cc7743b4d40ef26bb338e      1
    13 0x43b0bd27d5016e969193412933387b0dd4cf3e0a      1
    14 0x4c8a8c3fcf77f37101d25930e7a086b4e0ec45ce      1
    15 0x51097b2224ac6daf640206f31d437faeee951d54      1
    16 0x60acf8d95fd365122e56f414b2c13d9dc7742ad7      1
    17 0x6232d7a6085d0ab8f885292078eeb723064a376b      1
    18 0x7971e007a4e4d4dc1f8380f5d91d3f52b5e53461      1
    19 0x7b5af6790381f932abae790e8b0d0ff50e287f8e      1
    20 0x8874174a2366668d54fea6343f71709389563c8a      1
    21 0x88a6f6ac171e24be54d95dfa7ceff80ff185b346      1
    22 0x8b4567fa8c4715c27a682215a031033a8b544206      1
    23 0x8bc3757a675be4c2e459813769992ec2c60baaaf      1
    24 0x97d2b1b4a249ec77d56fd1576546996d14f7db1a      1
    25 0x9caed8a01c87721570de895a9725334a0a0cdb70      1
    26 0xa490a0346808dda91aea6698cb19e4697d9fc5cc      1
    27 0xa5214719feb1d07b66292a5d4d0a2712bd048435      1
    28 0xab2056903a7b62bac46f45a3d7a70ac799ca88cb      1
    29 0xac772daaa5079e005eec3e53314d6d1e9149dc87      1
    30 0xae72c6a6fad9fa9d82d089e1ebf73b3043855425      1
    31 0xb1b81fd772169a18504aee662cbd5ebb4886e0d4      1
    32 0xb4496906d6ea2685e7a46a14baefae9fe3bf0d2f      1
    33 0xb6c4ce4eae85066f8fc45093d12444c8b89a6aa9      1
    34 0xb9cf551e73bec54332d76a7542fdacbb77bfa430      1
    35 0xbbdd72fcce73c2626719be00259ddffef0d5673d      1
    36 0xbc30e90dc528ece58c1a51b6fb6d572838416489      1
    37 0xbd3a5d12af9fd82fb927fd26216e4dc2722d2337      1
    38 0xc02e6b0d0c1a5d8cd26beeba0fe8d76c5d2f19b9      1
    39 0xc2e8ed8cc0be70f91fc9aa903d5f4538719d7dec      1
    40 0xc78cd2e1e8ad4a288eddafb139c9d0891ad01ae7      1
    41 0xc7a295b1b2df1efb82aa204958b98ac30171cb85      1
    42 0xc94a284808e31b9ef43f9c9a48933e5a268cff9e      1
    43 0xc97958ff5370c56b7e0e6fb58437f00702714d49      1
    44 0xcaa1c396e70384db58dd33be74b26fb119e98c3a      1
    45 0xcd30b6de9dadb0a1598eb12a4ccf34b98c98c1df      1
    46 0xce8ad80ce1a979381d209ac230d02adafb9fa897      1
    47 0xd69e257ae6088b717ae6d2ddec9297703b4fb725      1
    48 0xe7c5ef2cee9c15b04fc7dea61a1ec443dd8e7fd1      1
    49 0xee958e45f3464d712b8830deb5875c8ac105f698      1
    50 0xf054274dd74987395d28136e53f39ef4f7b19994      1

## Airdrop Artist

``` r
c(airdrop_cartagena) %>%
tally() %T>%
readr::write_csv(file="airdrop_artist.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 20 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0dc1a10deec665da58542b6945e7d6869e05ca08      1
     2 0x10dfe5ed945fbb91b49ddd1810cf267420a8062d      1
     3 0x137320997d97dcdae021f65ceb45d6f802915247      1
     4 0x1ac0be8a7b313dc001da3c7eda12e967ea777c88      1
     5 0x1d14bfa2fedcd3f4da56d42e97582f614519d075      1
     6 0x3266b9739e0b12dc6ecdf8ef06ce1f1530330a36      1
     7 0x53562e8ecee54356dc89ad263a444528b34d6c80      1
     8 0x5a084aaef5eddd9195f28f6fe056785e01f210eb      1
     9 0x68a7ac13477aad590982293feeeb786a00276cf2      1
    10 0x7f270624480605ef2a2212eed24b886aae1e2e7f      1
    11 0x8379ac05ef71f982d497f392dc1a14ee859efd95      1
    12 0xa530f7739413e787c205233658185edc1e68c25e      1
    13 0xa70c5254f8db7c3dd740493b5e8a8dc2a1c9b233      1
    14 0xa9b86a685287a95ce8ee4cc22a8baeabc3d0e472      1
    15 0xbb11796a23229457790ef1af44eafe5e429a247e      1
    16 0xbe05e2e6d6d134790ead6e112f2044760e18e660      1
    17 0xd03504e910b56dd690fc5f57f9b758011d001c94      1
    18 0xe4cbda373d6a445f83fc9125c6230ba58bc08320      1
    19 0xec9460a75130da847cb86207c20b91dbbc9b0eb8      1
    20 0xfb74eee69be0c2bd8fb42ee67b6b6c4e05cd9ddd      1

## Allow 6529

``` r
c(allow_gradient, allow_raw, allow_singles) %>%
tally() %T>%
readr::write_csv(file="allow_6529.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 70 × 2
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
     9 0x248458947c120ca057ec028b3fe7e4b3f26fdb3d      1
    10 0x27b1abafb2cb065cfaf41b4b7ee95d27192151b2      1
    11 0x28b8d4f7516a112e2e2fd462293a1c27cde327a7      1
    12 0x2924196a2ec71ea4ae1b1357381eccdcee6c18f2      1
    13 0x2c8875f34ceb219f61b7453b2c5f100ec2f6ed33      1
    14 0x2f2242b447a54cb110b8a7991cf8a27054ee6921      1
    15 0x32d53a21813debfcb33a2b1ff2a396bd3a06f818      1
    16 0x3511ae23ee25e2b97dce883c7d496ff5d18f1dfa      1
    17 0x3cb63b82d778105e43f064ed739b0655f1f0fb87      1
    18 0x40d2f4399d23f9afb82d0a6b73055f13208614f9      1
    19 0x43b0bd27d5016e969193412933387b0dd4cf3e0a      1
    20 0x45855a3f4404aa08ffe14a366c75663f4ded2fac      1
    21 0x477ea7a022e51b6eb0dcb6d802fb5f0cfc3b4a81      1
    22 0x48464efe55fbcae8ae0c992b306afcf21d4910cf      1
    23 0x520e8d6c5b2dbbe62bf8a40653a314d879b19b86      1
    24 0x53006f95def268f88dc1b8216654ab56f3afd052      1
    25 0x575f6540c16a72696c14a17fa64f049992d661ab      1
    26 0x619db8e961f9b20b208da059a9156ef85e5cfd05      1
    27 0x61d9d9cc8c3203dab7100ea79ced77587201c990      1
    28 0x64f7de90dc79d775703bbec66a1591c7a26a22f0      1
    29 0x665654f2d2a151be2d0f8e3697e6ce780f732af2      1
    30 0x670a840848eba22eb2a090fd65e1ff60c3ba9e5b      1
    31 0x699990a8e7ada9e92c932d6e8fb365024fc74b43      1
    32 0x69e68074f1aada957edd39c5eae0069973343f30      1
    33 0x6d1db4a7e83dae0eee7e95d421722d46d2a7e94b      1
    34 0x6f0735bf1e6c69030d6990cdd580345b370eb50a      1
    35 0x729fcbb6e1289c88ce5113ec1c83a48a8e3c9f2f      1
    36 0x73bcb8c5e30bf85806aade7fc36f16c6b80fd3b9      1
    37 0x7546c60ae8d65dc6dd7a0f61c169818059ef49db      1
    38 0x76db02500f7631d57bc2dcdca9d4cf782b99e119      1
    39 0x7e5ab36876a267560e7191cedbe99ee7bc04bc30      1
    40 0x82139687faae8a29851902783e02e699de0e0846      1
    41 0x896b94f4f27f12369698c302e2049cae86936bbb      1
    42 0x982d3c5223f6b5794fccb3208eb164d042cf2526      1
    43 0x9dc5b2cee788e3e9c67489b5d7ce634dbf48a32e      1
    44 0xa32f90b21d11561d31ff604745907acc77fb67e3      1
    45 0xa71000e72d38e6a84d7190f59fd3dfc73931c0e8      1
    46 0xa743c8c57c425b84cb2ed18c6b9ae3ad21629cb5      1
    47 0xab2056903a7b62bac46f45a3d7a70ac799ca88cb      1
    48 0xae0d16586e5d60d334624c115216a52b9b1a0335      1
    49 0xae72c6a6fad9fa9d82d089e1ebf73b3043855425      1
    50 0xb6b4a02dca517564eb98790ff67d42b5b37a3d4e      1
    51 0xb735af7ae1a77d1ec764c862c2c09bdbf2b34b27      1
    52 0xb9cf551e73bec54332d76a7542fdacbb77bfa430      1
    53 0xba4575ea27041d99e6614ec02318f1e23a623fe2      1
    54 0xbbdd72fcce73c2626719be00259ddffef0d5673d      1
    55 0xbc30e90dc528ece58c1a51b6fb6d572838416489      1
    56 0xbf270918afe2ad16093ddce904fc358ad337cefa      1
    57 0xc02e6b0d0c1a5d8cd26beeba0fe8d76c5d2f19b9      1
    58 0xcaa1c396e70384db58dd33be74b26fb119e98c3a      1
    59 0xcabd5f77ca9d48f4ef9793f20de42ad39ce93979      1
    60 0xd3e401814d1faa8ca0419ecccbfee93ac7b15b31      1
    61 0xd40b63bf04a44e43fbfe5784bcf22acaab34a180      1
    62 0xdb561a899557404581e6180fe6d4178577dc117b      1
    63 0xe359ab04cec41ac8c62bc5016c10c749c7de5480      1
    64 0xe3b41ae8785e4107cc69f988042ff4a66a367fac      1
    65 0xea39c551834d07ee2ee87f1ceff843c308e089af      1
    66 0xee958e45f3464d712b8830deb5875c8ac105f698      1
    67 0xef5ab90a44b68d4f5e3f6be6af4bedb12cd2c66e      1
    68 0xf15a6b54e68884d27e1bebb1624d70c227b7d04b      1
    69 0xf2c5f1fd977dbd6de9d04bc4e62dff722d4bb1a1      1
    70 0xfd22004806a6846ea67ad883356be810f0428793      1

## Allow Artist

``` r
c(allow_cartagena) %>%
tally() %T>%
readr::write_csv(file="allow_artist.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 784 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x000cbf0bec88214aab15bc1fa40d3c30b3ca97a9      1
      2 0x007abdd190137f614701a6216fd703329afbcdf1      1
      3 0x02567d98dba79f6acf3798521fb517aa6f40a888      1
      4 0x0279b26fa484e6587514e3db9e2b02dfea968ad8      1
      5 0x0392c0d81934b52f64aa50c2cfbbcf28cd6141b2      1
      6 0x04b5762ee57e2fa1cc1dd2e13d284d0284f2d41e      1
      7 0x050920eda4014e25ded17c346b425239a468d63d      1
      8 0x057ee54e4d2e78a10c2d033c79cd35b02a4d4a26      1
      9 0x05897e0d012d3d4335b9a6bd2981927ef70c27a5      1
     10 0x06358d728027f3b4904cf55c3a7a3645c591e3fa      1
     11 0x068774358ebb17fbc8e1e51e35412422b4a6a814      1
     12 0x06cd11350360088b23cbe9c31adc241bd7154d3f      1
     13 0x06ef754ca26ea63c508805e4b840c3ff5ebe60e2      1
     14 0x0794cd47192155f6482cfb2f3f7c633ffde6d7e7      1
     15 0x07c829ff37a2f1a6ce485da1c4ebd0605c352b4f      1
     16 0x07cd101a8cd329a170d3a762d9d1645a2adb7f7a      1
     17 0x0851ced43aab7bc38b0fa4fbc4e3849634d2ca67      1
     18 0x085843dbde124ab8babd558fea60534962628338      1
     19 0x087d86e63885cf3330396dee888ee37d5a7a0dd6      1
     20 0x09134a885a1538111573553adbacd9286ddaebe4      1
     21 0x092cff73c77a9de794d25b0088ded0e430733dbb      1
     22 0x099868b3bde597ebd65839993712372e3f2dd961      1
     23 0x09ab0b019fd9c44316678b5a62ced52d179b0c1a      1
     24 0x09df7acd70041608aa7f9bba941bd64c0a512693      1
     25 0x0a02cbefa981ba86666e3d98e1652be0b168c8ed      1
     26 0x0a7e30e870e0ca3b1dd0fb0f20b45ef4be67e832      1
     27 0x0aa3a107c0dbb0840c12d5fb0d25497db46c3712      1
     28 0x0ad0b792a54704dc7b6f85cbb774106d22e814d9      1
     29 0x0ae19aab4fd6028c9a26edd0d44f16d20c68cb4a      1
     30 0x0b17ec878be272aa9aa3b357456ce58bdbe71a36      1
     31 0x0b50404f7236cde5170b796a3846964a6e662191      1
     32 0x0b53c82ae0a5caa911aa0c787ec973cf65d6a6ff      1
     33 0x0b7420a10c1d87d2edb4a612c4579bd83a8fb5ab      1
     34 0x0bc4f582e5332a54944586cb15897454a990292d      1
     35 0x0bc95f14395270a3c236ca1e15d77fb77b66c0a1      1
     36 0x0c4e2e3bf51d89db25b323c63633f4d8aaa46686      1
     37 0x0c89b309fdfd5282cc5bcde3a87a9c115080f33d      1
     38 0x0d5d188a6aabba60840d1d11b7dc8b74a5d21914      1
     39 0x0dd544d0da95666d7c3d38460feb4b5f25eaa515      1
     40 0x0e1a8ecbaed25f34fc93e224db436842f9f1bef8      1
     41 0x0e442c7d7db830276ecc7292bc57456309fe1522      1
     42 0x0ef024d299cb56805f2437cd00b8a361a7b06d54      1
     43 0x0f16b8fa33a660fcfe3c5d7b9503439d055fd39a      1
     44 0x0f456cce89743ba3788ea0368c8ecfc336359878      1
     45 0x1039893b37afd300ea40581305870ac6f9f5dd32      1
     46 0x10665f86175a79523a7eac2a3494c6f772af4c96      1
     47 0x108e0f1c26c124e92e9021b180fb65800690b916      1
     48 0x109ce05526694ef4349dc1faa1c5210adfec251f      1
     49 0x10c137a8f3539acd14601ca4e3e73ff4d75ce30f      1
     50 0x113d754ff2e6ca9fd6ab51932493e4f9dabdf596      1
     51 0x116676e23838970e48a20c3c7fc11bf69b90a3e0      1
     52 0x11a22b262e505d355f975e1e48a365b5d4811ae0      1
     53 0x11c35e301542a240b2c8ac824c5b47f79299c19e      1
     54 0x11d91151f54d4edc390ba4fc650afdaef4b676ee      1
     55 0x11fe0aa64c16b9b0f5bfb8eb0008bb6dce3fe563      1
     56 0x12481409b5c286d449446382ef35811b98dee11e      1
     57 0x1255b354efdf56e62fc70b0f0b155d026b324626      1
     58 0x125c6eba45c55d5fc7f2f85906c0475386ee2032      1
     59 0x13119e06c532d4a474379d742b7ab8eb4523a73a      1
     60 0x132fca6e4557da8288c7ff3e6caffcf35824c22d      1
     61 0x138c1dc31a0c02a1f8c06159a502b3be690bbd42      1
     62 0x13a191490d25169d24afac6a3bf85ff5010096c9      1
     63 0x13cb8579e8d423984927c3d1a4874f94369c0cde      1
     64 0x14368bfb69824aa8645a36e7ff38584ccfe0fbb6      1
     65 0x147f466fef6bd04617fc0c8037ea01c73bc25a3f      1
     66 0x1524e51d175d9f1e6cab60dce7d5a18478280e00      1
     67 0x15a33852846b4762d71a62928aa7c64d8aac8544      1
     68 0x15d4c5863ac7de69dad5b58b6f74b51109ce4c83      1
     69 0x15f91970511f6197f03a9f3661550e460aa7d908      1
     70 0x164c3df08784171e0c4b61b7682263371f1eeb74      1
     71 0x16cbe4ea5cab2a4b9030d07ef946532b38c77073      1
     72 0x1712bf0c459dc5c85b78eee46642d169be0ae1bd      1
     73 0x175e8a81363d2e362a37adaa3bdcabf1e3c54307      1
     74 0x17ea061b70871c6e1460f159dee8a7b0ad1d860e      1
     75 0x17fccdb83768b93299af457a437e63e98404f7b9      1
     76 0x180669310d089fafcbee40479ee719752d680d4e      1
     77 0x1854a7a0b4f9466dbeeb3279e247d4d0b0a06e3b      1
     78 0x18df11a7a72b254ab30dc80be6c035727aef8fab      1
     79 0x18f103faa3b1614b14fa3b7e93899bcba4c6761e      1
     80 0x191e79f3031a6523bf96b8571469d6d54be8aff2      1
     81 0x195a4076b4734a56d2ba31db481ccf683d48cbe7      1
     82 0x198109b0d2c786a230d18b622d3b7a1946131e09      1
     83 0x19d4a1f622e7491cac1f954d5d0742a76dc0335d      1
     84 0x19d74f3ab452b027c0ff27612f611477f0a3b1ad      1
     85 0x1a256faeef1a4e30ae141a3f77ed578a0d02f7fb      1
     86 0x1a905f7b5cf52bde28defbcd3f0ad149f15cad86      1
     87 0x1ad92296512dbe333a07b08325232c2c86943d4c      1
     88 0x1aed603b013385c8b41ca0e6ddd67a20a30fc79d      1
     89 0x1b6695144282442fc85e85163930088df004e0e7      1
     90 0x1b672e08c3cc6486b1c01d1114d5ced3c5682882      1
     91 0x1b7844cfae4c823ac6389855d47106a70c84f067      1
     92 0x1b885e57a1ff7c7c38aeba3a9096d6fff6077221      1
     93 0x1b8ad6f1ac6462a648ef1c292ebd9f154ef1c32d      1
     94 0x1baac94a7a44d80f6d1721c0f34d34c853062860      1
     95 0x1bb01159ab168acd0cc055ead980729a2adae919      1
     96 0x1c03412dcbbbff340c20f00d5a4c7f6005e980df      1
     97 0x1d3342efe9f92a1bef48756de67a58a217dbfdd9      1
     98 0x1d3dd4cc14c9bc93bc8744559be4ace760f7a1d3      1
     99 0x1d3eda0356e492d51fde4ffb8651c549bdecbdca      1
    100 0x1d6cf41da4b30ea54aa436285e45bedf6babdaf6      1
    101 0x1e4f6ec8163d83c23bad7ff0e9a9ff5c0dd38bf7      1
    102 0x1e8e749b2b578e181ca01962e9448006772b24a2      1
    103 0x1e8e95cef28e094c461c26545fab6e03580664d1      1
    104 0x1edbc853dc2d22a0893d62208cb1ed20046e4bc2      1
    105 0x1f695645180fe08f306e92f0a8c24e56af0821ee      1
    106 0x1f92fc15bcbeb4dd24eefbead8d7aee409f891dc      1
    107 0x1fd582af476d465c197de0176f26c4a5f3f82c29      1
    108 0x1fd739bfcb6bd70919d19ed2698a8d1bb77fe16c      1
    109 0x20126e043f96a185ac2197a5c37f1fd63d739b34      1
    110 0x201e6710f39611807bb8ec840f8c252f6c16c71e      1
    111 0x2206445d241ccb7dae93b2d2acfc67f75b90fd76      1
    112 0x2231bcb220911e08bf76a679a3cf3a58f87bd8b5      1
    113 0x224a5ba2c04b66e97743d5bea1d6da6bee8b6a8a      1
    114 0x2279f5b913aade070fc2f499a6c8f83bae8879bd      1
    115 0x22a6c748dc711aa485d6933652afd76eb7de9fc8      1
    116 0x232ade29f135a87e8cfc8f1e10aa81a8d50ba306      1
    117 0x2334d91317950c19a6c21c69d69f5ab8a8f275fa      1
    118 0x233cb1da57342a254bd8827dee771ca376dd0958      1
    119 0x23749dc28e4461362d869c7bb7ab061ad2c59e9e      1
    120 0x23787626349cc170bfc512e5b5e3d3f44e00d02d      1
    121 0x23fed2634fdc629c1a82eb6ed3391efad2a58670      1
    122 0x245e4cc32a57e3b9bd7c50eae8fc90fddd6c127b      1
    123 0x246bc8177ae939b6a2c79f02b31efa002beeeb43      1
    124 0x24dc97621258b50cd5c6da4ac12e2c4ebfa63b19      1
    125 0x250807c9d581bd8736cbab8bd867367da00ef998      1
    126 0x25679b53d666b167712b2ed5a1bbd631f03d5cb8      1
    127 0x257a9753d44d5eb9fe02a22bb3393ef13dce8abc      1
    128 0x25d2e7509fe39a25819fad4a31a1512e206fcc1b      1
    129 0x2606b7c56f84369653a5542cb702da0f3d256162      1
    130 0x260f47f318a9db3d3148d5892d1dc95f0c68ebaf      1
    131 0x26925ca5b94ce1aa86a11528422f6adfdc0a5907      1
    132 0x269d94e3eae94b5845971d1e2699e8e7b7bd55a7      1
    133 0x2702d7b843a994a388b830f64f9ca17118f965c1      1
    134 0x270cf7cc1424a20d62c471edc42a0a7fbebadd2e      1
    135 0x273061aa9b9ddddce5b7c2d1eb7237611d558d4f      1
    136 0x27637173712ea289435fb17236f7d219ada774a7      1
    137 0x27716b97cf1d39dedf93f180242c47cf456c201f      1
    138 0x27e31356cdacae6b44103c026d020acca4c30239      1
    139 0x28458f3442841da5e4773b39286447d27ec57b59      1
    140 0x291c45e783cd1e546e027d6e527ae04d86baa09b      1
    141 0x29516e4f4699c2873e2fd3f5515adc792d2e8e59      1
    142 0x29a0127e6090c1aa753d33bf5730591abb5ba83a      1
    143 0x2a26fb1a180a2ce5c6c2d06dc4d430c37e7ecf39      1
    144 0x2a3d34c4c5eec9d7401ed44f93d47e44ac19d37b      1
    145 0x2b0386bbdd314d8356c21f39be2491f975bd6361      1
    146 0x2b4ca6b96308775a3a2650fd281078a8e700afcb      1
    147 0x2b9acea4ed4f4bbf5545c4665504deab89aadb7b      1
    148 0x2c1d9b110da81433f238bb1de96f63cfffbb20b0      1
    149 0x2c22cb27e502f2c6eb1c16f39ee69af185955da2      1
    150 0x2c6cd2ac53276fa70b84924fdf8443732890d7b7      1
    151 0x2c6cd97d3236250691a6bac8e7a8684be44b991b      1
    152 0x2ca043bf85fe1553bdef8c0dbf021ad6202efb41      1
    153 0x2da903666829f302b0501f76144339213259c260      1
    154 0x2f940cb1fe951c15571efef85b83ee983dc29577      1
    155 0x2fb55f58dd40af75dc019c2a3d09eb6878d376ea      1
    156 0x30e8faa4268b5970ed09ea4ebcc8e2c223e5a3c7      1
    157 0x31880b3343fcc7891d9c7e018eaa2f4816861214      1
    158 0x318e4ba3fb1ca449868e48c060b0fc11da173b8b      1
    159 0x319af5203839970ccb04d2eda95c427941c498ae      1
    160 0x31d447ccf5ed1905287ca964b72f5fef01682944      1
    161 0x31ee47a0bf557c4949baad97d798b6b156cbd231      1
    162 0x33333ca4df33f28fab80835012927196196e185d      1
    163 0x33c651217792793cf868ffa13e81427cb9560406      1
    164 0x33cf9259c18d24971fe09aa19790c3418c3a12eb      1
    165 0x34367811c09822437b1c7e324ca5d80545b070ad      1
    166 0x34502701e686124b43e7d0b57501d0c359388044      1
    167 0x34978faf3a9f469da7248d1365ddf69ac099588c      1
    168 0x34a35cce3edc0084a9312b4f88fdcbc11cfb89ef      1
    169 0x34fd55a860f1df45bf006a81c2c3136f48254622      1
    170 0x350475899f21658ad25decf2e740ff711bff6981      1
    171 0x3527c484c887132b636eedb4603c6a178fae7dcc      1
    172 0x354ddeba429d9e43ec1438af5319b2158ad36afd      1
    173 0x35946c51a2bb1dcc7d2b417ef23d3e982f121330      1
    174 0x359b7628f98f3b73e9bdcb2d8476932342fc5aea      1
    175 0x35f2e1d3a99c3fd78cd26db53960833b994448ea      1
    176 0x35f74437d55cc331d600e6d34a435b6c129d22e3      1
    177 0x367859c494b1fb465899f398ba03cf09608bb781      1
    178 0x36a5bc205df1ed65c86301022cfc343a6ce546ff      1
    179 0x37781de47983576e45d981038710842666ab5237      1
    180 0x37b6274ecc034f96b3a1dbb77709680bc394ce8e      1
    181 0x37c9897ce4add15a275bbd8f34ed08ac56647e1c      1
    182 0x37e1c12798f469f27f3a2f81266500ab488263c3      1
    183 0x37ffedd77ec1a841d476a3a7e4d53d337868e9bf      1
    184 0x385dc7d2ba88318c3a22de8cbe7b7b0ff54df842      1
    185 0x38731e7d1b4dcf99639623cb93d1d41ed918a171      1
    186 0x388e07d4fc7f3e2c825e6cb1ff46447764798b24      1
    187 0x38baebb524bd1ce081e5c9b75739bce6e091e95f      1
    188 0x394574a46091b1db9a7e935aef8ccc5532814cef      1
    189 0x39e2b69195f7331ff531e542e7c446645e4d77c8      1
    190 0x3a6801f383c6327e7bfff23aac660627b52dccb3      1
    191 0x3a98f478825d9114eb118779a75fc9b0998cb9ec      1
    192 0x3ac2cc26fbb230e704393dc79e97cca47e709247      1
    193 0x3bf4d4c0b1890028c158ae3495af5dc75340e22e      1
    194 0x3bf5616a2336bc63a6182c631a98028070c96efb      1
    195 0x3bf5fcc559e93ea48b7a3594e7f0f9c59c81cc77      1
    196 0x3c7cb3b81e4ae4122dbc8261d57770f7af750514      1
    197 0x3ca465428874903718b47d87f0a5043f06504233      1
    198 0x3cd378c9b1cb5f147ebf1b2c2564118946ae4ba1      1
    199 0x3ce99cf4c97d44f3a91e2964ca38cd499fc990eb      1
    200 0x3d19b7fbc5f28ab28ccfae777ecc25f71ca5fdc2      1
    201 0x3d66c6ba3ce3fdc988654d72ed83a478851fde49      1
    202 0x3d7d901646f62edef0965e96489ed172dda45336      1
    203 0x3db92e26837328c1300e3f818c5da7fc9a7d9648      1
    204 0x3e1ffa5f95dd6218ec4955216e3df532385c0e2d      1
    205 0x3e81ea3a8a2d0b71a85468b02be83d09bfffe476      1
    206 0x3e9fb34e002f4ab39e25daf70952fabdc855dc83      1
    207 0x3ea4ea76efbc4da7207635b3705654ba8d674a62      1
    208 0x3f113ee632e16efd9d647bce409b805c4485f1bd      1
    209 0x3f58588b59da010031dd8a355dcd7de229663ebf      1
    210 0x3f7d89f9cc96bbee2272a9e32f9211d8a68a5ba3      1
    211 0x3fc5a126201f22e76bac42104a01e3dec498b72d      1
    212 0x3fcddb0dd912e6f68927b448546cce99f22a3e31      1
    213 0x3fe877b86a298326c13d911a99d59f4c9d195957      1
    214 0x4059f3c0064cd380276de8dbab6935005535eed6      1
    215 0x4076eca4db9684fa1d9bfac231cb516889a33e5a      1
    216 0x40ae083b384053e71f3a360fcf844632f2346568      1
    217 0x40d336b5e8fa5acef13761c22de4a17b11d7121f      1
    218 0x40d775827365ae4d54cbc08a1a1c4f586b2c1d0a      1
    219 0x4124cf34f56fa151e05c91ace550ada0dd5aabd7      1
    220 0x4163f0f8b1caae2492b8bf3ee451cee59d56ec7b      1
    221 0x41ff38896855c46d62bc0276b7c35bd05668fdd1      1
    222 0x432f7532ac0f605e4b90e7f03924019e222f50c9      1
    223 0x43423d151b29503024cf264da37068ffedbe1446      1
    224 0x435a4ccc3712b0b40ce16a1c7bfa263ed6e9bb33      1
    225 0x4392a0f2a6085cc394e928ae5b1e6c84cccec89c      1
    226 0x43b0bd27d5016e969193412933387b0dd4cf3e0a      1
    227 0x44207f6ac38fc925919cd219338e3a3789cad1db      1
    228 0x447f043bd961278787fd79318864e09d5fbf8682      1
    229 0x44a3ccddccae339d05200a8f4347f83a58847e52      1
    230 0x452f438aad8b675232c1fd7ff8e940d72d8a9f45      1
    231 0x452fcd9f73a5f09f6b6be34b708e4c2b7d8fc46b      1
    232 0x45c4a18130e88d8593e0c7877b24aa4fb25b2051      1
    233 0x46245231806318f71316db8d9d0b73dbceb76a6c      1
    234 0x462aff933672f353b9afbb648dcdae879bfa36f2      1
    235 0x463ff9b3abf5787b0c3103a559addbb4050c1ded      1
    236 0x46a20d31fb68626fde838829a455d31468367855      1
    237 0x46ab7dceafda44f501416b9862518c5ab06a3239      1
    238 0x46ecb3f576c31290e1a4b359fd993e36e86ef9e1      1
    239 0x47d4f20ae83bcd350105f199f900e6e6104dab6a      1
    240 0x47d9e2c5aaa56bb47b4ddee929840b11be55da77      1
    241 0x47ed68e069d21a39158765a10aa09d6a24389694      1
    242 0x4813ebac2e0ee9321daf34adea64f7febd84ef2e      1
    243 0x485b8ac36535fae56b2910780245dd69dda270bc      1
    244 0x48b8b83c3da69fe64b54dc6c7b948827fd72bcaf      1
    245 0x48d26fbd89dda10c72cd8dc7b82b9eee0af6b48d      1
    246 0x49140e8e60913110f2e37ffaf4f5231a03611e3d      1
    247 0x497b5af00112f42f57b2496bde21123b1d4f85d6      1
    248 0x49bc851b355479d1241d86607457feeb9026826f      1
    249 0x4a001ceaf98781380c603c981c3fe56ee6dd4fb0      1
    250 0x4a0a716d90d049686365b78364b4d085afc9727c      1
    251 0x4a39ae58b605102913ac19b7c071da75b55b2674      1
    252 0x4ac21c597fbe55f363abdd698f1fb07d5f04c9f9      1
    253 0x4ae9722124359e773cda689e04d25315eae4773c      1
    254 0x4b4d75b8b0fc7528ea2614b2ca83555824c07867      1
    255 0x4ba8ce85ae0f897a071db29576362caa90438b7a      1
    256 0x4bb3a572b4cd2edec6b0c301a323f9ed0b291348      1
    257 0x4bf3805b23c99f8e0a5797e86fd0232a04a2a629      1
    258 0x4c82ba96eba80a6ad8e7482373a9e64f0cdfd8d3      1
    259 0x4d4f20a51bea3ccae36781997ef09dfb85910f75      1
    260 0x4e7da2321288c83fd760e60d26bc55d50b3cd77b      1
    261 0x4ec8e85b5cef9df1ac16a9d953ceef453ffa28e1      1
    262 0x4ed95e1a632ba57ab02f02c7cf948f13ac0cedc2      1
    263 0x4f764a08c66251e13bdd85b4bb0652b739736328      1
    264 0x5006dc09f9245b09525fad7506d38eb1b4cb14ed      1
    265 0x5054a665b7ac3c30939b02acb34827af25aba35d      1
    266 0x5089c37defcb9e46ed4ff5fc23e277fa396632ec      1
    267 0x50add54a6e0eb6d552dc447225e22142bb8048ca      1
    268 0x50b3aee4aadaf115c4274bda7b788572d859a9b2      1
    269 0x50e82f866800d3f1f542970dcf0d292bd490db33      1
    270 0x513851eb890c5052b5039a1edcd2b259980dcab0      1
    271 0x51c92564137510602710f742250101669b1e6f83      1
    272 0x51e5c6d6fee3649b3e81d1bb5d1922ee4b1c5c95      1
    273 0x5251ad420483a18589d07535c426ec6bb5c6642a      1
    274 0x52959588ad024def2b439bfba784496c315bb09e      1
    275 0x52ce182099381cba6327fc51debcf5bdc84e1317      1
    276 0x5347baed42994e35180fe485a1381e9d74a19923      1
    277 0x539fdbe326d64b72f556be591507ceccd21838a3      1
    278 0x53d398f99a2c33e0b03d66699f3ef20c5e4e337d      1
    279 0x53dda0a69992b87e51f0f14b1337281f09a2c8da      1
    280 0x5492e0112e49e5fed0a7b1278c76df655e509a99      1
    281 0x54d401e7ce5f3262795524b121a584e68b5a41c0      1
    282 0x55dd53f98d3b62fbe64296a6aa860d49f6b4b690      1
    283 0x560962063aa6b854dc04b83fe2e23ca02d885264      1
    284 0x562829508ad81a45bf621be99c130807906d626c      1
    285 0x56f70a0bbfefa9db41f024198c7185107f42bac2      1
    286 0x576da0260daec56d8ced30536fe9fc84e7d15542      1
    287 0x578862011077c1cd95969452bd528b3ffd496d6c      1
    288 0x57cbe501092e36e87692d89ce4e75f98aa45feb2      1
    289 0x581cbf7172bde6b0c18649675c3b1295a5d93262      1
    290 0x58300539ee6ece54c9b49ee0e9be84c5c4bdd680      1
    291 0x5936eeab739c6db9a0bb5de8b79a4bbee65200a0      1
    292 0x5940d25f034af3deffe99c3558b39aa1baade443      1
    293 0x598ce158e94a87ebef2c6865d37b63dac4312c06      1
    294 0x59d9419863c18fe500d75864a4ca31ee2964fa99      1
    295 0x5a6b07f90a46d86ce6e3e8680e41d138c4404f28      1
    296 0x5a823e29d721732b0446c2189657299163b59363      1
    297 0x5aa688127811aa9390bc07c4ad7ff1cffd84e20e      1
    298 0x5aa71418ac6238c78ea9a032a768dbbbf86bfeca      1
    299 0x5aa97d5823095365acba997f23c434050aeea24e      1
    300 0x5abbeb91139db84251f40dc9d07b493083ebd5bd      1
    301 0x5acce70cae148b520ff36dcf177ee78ba54ebc04      1
    302 0x5ad309f56e0dbc4abea2c199c21033e0067c4234      1
    303 0x5af84f1318241b4224ecfc9e6d4e97a3118cd38f      1
    304 0x5bbab2b45f1e75587e5660176124725e49b2fb09      1
    305 0x5bfc001eed8abcd85c7d413e22d8fe204526ad4f      1
    306 0x5c2a00d4051455f05829d818dac18e438124e79c      1
    307 0x5c4fe960950ba0e09a72869c3d51fe70f07580e0      1
    308 0x5c736d849527bb15fbc0111f4ca13b4cfc1a5c8d      1
    309 0x5d93cfa7fa4c8838a79e7510b1d870f92f8c08ec      1
    310 0x5dfa3092be17f441f85a4b3218a7675f0efcf9a3      1
    311 0x5e15052dfa1b27dfc5985d51d51dc0539c1c10ca      1
    312 0x5f05a2b84fb9c08a9abbbdaa666833017601367a      1
    313 0x5f2bdf26f6528ce05aac77d7fa52bac7a836ef66      1
    314 0x5f9d41289ad44d17e6c51f889276999112e4fffc      1
    315 0x604b98859ccbd4f2e972a4aead0f249424193a2b      1
    316 0x606c3af5cc0bf4afc6afd1010e8fb424593eb9fb      1
    317 0x60925152883749b92b7bb2dbfbcb7c081b4f6fbe      1
    318 0x60a650e6ffa4fa48582a1da81cf7d75e499f324e      1
    319 0x60ceef10f9dd4a5d7874f22f461048ea96f475f6      1
    320 0x6112083d4bb12dbf64e4850842685aaefbeacb10      1
    321 0x620910b020fd12e7b1ff8c0d52eda66bf31213d4      1
    322 0x620f761d45a8b2f9b81079758ec002aeb8d92489      1
    323 0x6301add4fb128de9778b8651a2a9278b86761423      1
    324 0x631cd42eb1063d1fe2b49a78744f3094702edbab      1
    325 0x638ccd7e496b2828359f06fe15a67672506b0eb8      1
    326 0x6456f20453a383004f17b921cd770d187fcd30ef      1
    327 0x6486675ef9fda0e4ce00a4d580571b0a7233d4b6      1
    328 0x64a8dda827deb895a36667d01063f2ab840e46b2      1
    329 0x6566df79d7d0987fd63ccc0f76324b43886faa7e      1
    330 0x65a831d9fb2cc87a7956eb8e4720956f6bfc6eea      1
    331 0x665654f2d2a151be2d0f8e3697e6ce780f732af2      1
    332 0x66a31e7e5753331472af2a62383ac8508a47dfc1      1
    333 0x66d89b94ca219fbcb04836b99707791082fa4b6d      1
    334 0x672044e2abebaae1359737b7cc5a79071bbb1c0d      1
    335 0x676bca78ef4dfa66f59a4caf9fe051f9237de74f      1
    336 0x6807de338f28a8a520800127f3aa7581c15fc2a4      1
    337 0x68491cbb7fea61499e05081984f046f4231e2f48      1
    338 0x68bca5a8bdebe05fb8a6648c7316b4eb7e19a064      1
    339 0x68ddbe65b79f61a1f474afcb05f9239b1d501bae      1
    340 0x693fb4c126da8e126687b16c809fc19d2409d522      1
    341 0x698f0664bdc21afedd384092f185fd8d48f02bd7      1
    342 0x69b9f2bd11f68ac293093c1fcdeef0d141828e99      1
    343 0x6a78796df6889cf0880132447676d530584cbf64      1
    344 0x6acc02cafef5fe7148b4884fca2ca687818e0d4d      1
    345 0x6ace9cffeb1ddcdee9ed814f35a318ff61572630      1
    346 0x6b44862103b8a45f5ec701b69ec28a5d6d304950      1
    347 0x6b4b60cd239b5b7947c98bb2b9bbd01db5bdf44c      1
    348 0x6b6566341c9905a401e62952bd670aad7f56c18e      1
    349 0x6b67623ff56c10d9dcfc2152425f90285fc74ddd      1
    350 0x6be0c1bdf01930dfe08798879cf1396183fdbbfe      1
    351 0x6c9726afbf60be7a1fcdb5730a6c522b4f578fb1      1
    352 0x6cdcbf98f5dd30c473298081b98839f39a9d233a      1
    353 0x6d2c642ca40013226691acc85315e3dcf1cafc9a      1
    354 0x6d50c3a12a094da09d04a54f34fb7970d5b62b86      1
    355 0x6db5e720a947c7cc2f3fcad4cf5058402fc456c6      1
    356 0x6de2fba4ba6f2f6672eb9c53f5f58e6e9bed3ab4      1
    357 0x6e2b333f72de9d5adf2ad535c1d98fc9a7347aef      1
    358 0x6e70d217636feebf83ab1cacfde147d07538526e      1
    359 0x6e7304e5a90626208cef51523120a136bebc166b      1
    360 0x6e9ec6d724b87fa8b9743d75bd93798727a494b3      1
    361 0x6ed378ef6d8d17a32adad6b43457eac7c6343058      1
    362 0x6eff21c2bfdf83a33aa690fe3db17fc672244907      1
    363 0x6fb5e06e0a8860d266753e586c95ebdeb70e62c5      1
    364 0x6fe0bb2ffb7e425eca5d26a7200e01e94c1818f6      1
    365 0x70afce457d416779738f25fd932760b9e27ec8fa      1
    366 0x70ddda43d4f616263b1db0ec672cbd8795db13b1      1
    367 0x70fc1f4d3fb666e835de0141544d4ac4f34674d9      1
    368 0x7109721443af414ae6324e7082cf38419806cb40      1
    369 0x7125f00afa617267713c6b70f5f9078f6556eaf8      1
    370 0x712d0d775aee2cb158c58e24e3a1dfdd2bdbc6c4      1
    371 0x71435e346f5d714869f31f98e1a4a95efc07b077      1
    372 0x71b49fe06436d997b14228c96ac9bf4439147259      1
    373 0x72110c91bdbb0b4cdf80d54314522068aacc59f2      1
    374 0x73750f30b420761e606980a08b1923c97cec07d3      1
    375 0x73c69993d47fd9093c1637531af96405c014aeec      1
    376 0x73d97c30603b73cf4ccde4934c6027a9599d861d      1
    377 0x75331ebbe0b00b97cab532384f13c9b479f074ec      1
    378 0x755711155b0ba8520b7a0c8cdd6a2b1a49e32d6b      1
    379 0x7615ceec338bc6bb94bc73e5f8e534b8157c653d      1
    380 0x762c8b9ffe6b8b34c8667717ec9ba07e53af1f7b      1
    381 0x765718a8e4a5562be34b122af259ce5a71372e12      1
    382 0x76bbec3cf93c5f641e8dadf13fd70feaa805d645      1
    383 0x76d078d7e5755b66ff50166863329d27f2566b43      1
    384 0x7766bb604e5f67e6e26fdd88ab10a10acb5f45ee      1
    385 0x77728afb8be960222e392b63014840416fc6a738      1
    386 0x77771eb9efc4ffd5e8dc2eb952fbba20aee5975c      1
    387 0x77dc2dca78697d8379b07ea4caa046b8e0040b56      1
    388 0x786d3221621835528aa8754a9c72cfc1d9e813c2      1
    389 0x7921dcfa9f14dc980d7e8f946b7bd223624c3db9      1
    390 0x7a5e9d691164cd73b821946945fdc4d23d03c3f3      1
    391 0x7a6834a1c73ef521c9f89f65487e05ba393ad6cf      1
    392 0x7a926e039b30b9ca5a0499e6a8b1f7fe2c30aef8      1
    393 0x7b0fabe6594c973662bd7d36f19f69fd2bd25b3e      1
    394 0x7b723d6c58f22c83bcb2361dc41b4e8222dc88e2      1
    395 0x7bc7481751d7fcb76d08f596b03ba1e42378ea27      1
    396 0x7bf2b78f7d114f33789312c868ab939a145c465c      1
    397 0x7c70a7c6fb919d7b5b0f929061251538e14175d0      1
    398 0x7ccd2ee72a75f7e4776f598c1be11a119fd8d191      1
    399 0x7cdbb8e309ce7c701ed200be123f16c6f7c9e525      1
    400 0x7d422cf705af9ea93919b1a6e843b2f9f387562d      1
    401 0x7d5db3bbf958138b412511830d78c4fa5b67ac49      1
    402 0x7dd3005f938feecf091151c0e4f5f53327b035a2      1
    403 0x7e65572ff500ebb638b9b47e8d526762ebcd064a      1
    404 0x7e8fcd1046ae351af66c12b75b051fe550afe71e      1
    405 0x7ee3acaccc605e4de444ec630083f739b3a73d6e      1
    406 0x7f8b0457941ea2dfa8d806c008935a1c0dcdf3ec      1
    407 0x7fdba82ec2360d4e566175943cdefd8443a77143      1
    408 0x7fe494de8a522a2355306cb2d30544096939ecbd      1
    409 0x7ff044f4241ba28ea0c50b7181505fda610d76ef      1
    410 0x7ff17fa91fb9272c8a293d9668dea218844bf316      1
    411 0x8002a176b6e5adc952745521e9080664d6536160      1
    412 0x8009da986852694cd8876717957c8824d56c9530      1
    413 0x8030e495ea55d8ec8ae1e10bf7498bf7f0eac1aa      1
    414 0x803e96717d25ed306160d35ab0368dd595d0efb6      1
    415 0x8098e9454d4aa286d5bb09f02a04200f4f3f5bc7      1
    416 0x80c939f8a66c59b37330f93f1002541fd4e51aa2      1
    417 0x814055d90fbcef027de7ff150981c69a4b882dc6      1
    418 0x814ccd3404dbafe6285a0f63b12d2ac45ad86b5a      1
    419 0x816f21762aa71a8fd85d1e78702e495506dde00f      1
    420 0x816f5af57b56b7ded7c901e9b1fabd304d0fce89      1
    421 0x82f2916938cba252f516c1a1c9cc663ed2222867      1
    422 0x82f912aaaf7551de526d5d67eec250aac0f862d5      1
    423 0x8323f44e0b9337db92810f261bcb64878c04915d      1
    424 0x8367a713bc14212ab1bb8c55a778e43e50b8b927      1
    425 0x8401f7a58b15dfa284948a42ab0728a3f19f875e      1
    426 0x842f8f900f9932393a8040a40bbd85fa15c0be62      1
    427 0x843dd16b4da6044d69199518d0e7d01bbd31b5d6      1
    428 0x8463d6ffcbfb56bf20ab2e1111f9a0e8359533f2      1
    429 0x848c2094f8ce89857336d33b956cdceea0e8e39e      1
    430 0x8497277c9339170a0420e86da8352e0c084624cd      1
    431 0x85b65bd430f4b473d33eb97b0e5a20aabab46cfc      1
    432 0x85b6aa398adc6272bb57d50f7f2b9fa904f5c2cf      1
    433 0x85da811f6eb08f9221fb3c6ce8e4f703d4dc89e3      1
    434 0x860993dcb240bd1e60f29f1887164f15ffb6a049      1
    435 0x86286d93d79a9e5b78de97cbbc8dcaba0f2489fc      1
    436 0x87a5542bfd30a745b1057409037ca55fd3aee023      1
    437 0x88923378021bea85f9b09ce571a309e12c7d2262      1
    438 0x88c9ada911e01f3a6879a8e6066d2da45ce8bf1b      1
    439 0x88da36152d4691e8d4efc998ddb7b22fe1b16898      1
    440 0x88f516c04969f470888473458b5e09342da08b7a      1
    441 0x8998c29424636e2fd37d59a363cfd5aa633d9c88      1
    442 0x8a0a569b2937ecc360a686386e34df83cd2348a3      1
    443 0x8a93aa0bcf22876678607eb85ee1935524929405      1
    444 0x8adefe43c8b3429dfcf0436a5f54c40169f4b393      1
    445 0x8b4d56f8940595dc195b0591c62465bba89afc19      1
    446 0x8bf048b0a714ef31398097037510ba449b2b99d8      1
    447 0x8bfcd74944ffebd4e210bb65f833692812208344      1
    448 0x8c01ca84397b5a36aaa89ab4d6c243075a963efc      1
    449 0x8cb7f1a4f44593ec356b11c70c0a977c647c763c      1
    450 0x8d754be48d0ab99270da44bce0d817d49dc31ef5      1
    451 0x8d900c8aca5b3846c9ad2cbf916f20babd95f406      1
    452 0x8e6b56ca2ef39cca96048a4e7ac5db9e8f5a7c23      1
    453 0x8e82a5b8f8f0b86ead15a4e56dfdcf2418ffe647      1
    454 0x8ea6a8de40043b5e8085704d450849f99b816976      1
    455 0x8ee376de530fb9a734df676e7e4342b48355f483      1
    456 0x8ee94e820e7e898f47ac934a755d7382d088edd4      1
    457 0x8f1ba6ae944d9d02ee23406ac1ade5362ff86886      1
    458 0x8f67e1e19da5ecb7fb73e3d04bc30f4ad27e3fff      1
    459 0x8f7ceefaa1ff5dfd125106ff9e219eff360d57aa      1
    460 0x8f8b4759dc93ca55bd6997df719f20f581f10f5c      1
    461 0x8fc11ab9a5f72a4f0d12199a72f474933bd71064      1
    462 0x8fc72a97a21a45b080b54eb4f58b9d2f9ecf999b      1
    463 0x8fde81bb63f144e8ee892d41c453b480fdf3c4dd      1
    464 0x8fe989f821d30448f7a0c88ff4aacb3687655835      1
    465 0x9011b9be2b067562b732e09a25f39a59871da8eb      1
    466 0x904935e7b85844202769a15274fb45c0915c6693      1
    467 0x904f594ba4e7abbc4a112acc6ea2371cf8cb08ee      1
    468 0x909c85f8d5db527b69be6aee1f5d48f13603831b      1
    469 0x9150397d0861551d5924c844169dc69aa579be6e      1
    470 0x91e8be20d0dfb2293b97317e906223c1fa833a48      1
    471 0x923f4afc77a7451579ce0e8b0f647426951bf064      1
    472 0x9241bc732c2117fdd426214d62feaebd2b5a5ad2      1
    473 0x924564b6941595b7d88112c1c54cd277668068df      1
    474 0x9254f7f72bc6294ad6569d1ab78139121db880f6      1
    475 0x925b70ed5622876b61d122e897dddcae49e42643      1
    476 0x9260ae742f44b7a2e9472f5c299aa0432b3502fa      1
    477 0x9282ad6b06b62b2eec1ddaae233d0737ca8f50ae      1
    478 0x9391064713f14ddf6f66fe21512e48a68c1e6648      1
    479 0x93fc0c711c2df516bc442ad4b3ed47cc4001829b      1
    480 0x943164079242e24ed77201b7bdc4d209a5403bd4      1
    481 0x943288a428859e3f4811040e709cc3f3e30e398d      1
    482 0x9469c98be5afd94cd601e094bc401ddd37f480a3      1
    483 0x9499054d02a725316d61fa896c29d58550ee4a5b      1
    484 0x94de7e2c73529ebf3206aa3459e699fbcdfcd49b      1
    485 0x953044e85efcf894bce94a9b942a2ee0760beffb      1
    486 0x9553803357f9441ee426e701a1bd8031f814fcf0      1
    487 0x95d2cb5592bef123c7bfbb08972a4899274ab99a      1
    488 0x96d2b5bfa179ce0979fef08ca7ecec497b050b75      1
    489 0x974447055f110c4271d48a1c7837dfdf9d570d19      1
    490 0x975c8b41b9b89c0a62a332e14819c185ae51b911      1
    491 0x977089ca5b701e09637768cde651b94855d13eab      1
    492 0x9845a5e698087fd8ba2bd540e305df5abbb38e90      1
    493 0x984c510ab204f95baa273531daaf9bff7becd93a      1
    494 0x98a34c0b364af1d2fe92f7a7d2993b01ec8bb2e6      1
    495 0x98a692316057e74b9297d53a61b4916d253c9ea6      1
    496 0x98bdfe7174ed431da22604aaf87d66fe2bd8621e      1
    497 0x98cb6107a67da3375f16c304aeffe1fcc0b1239f      1
    498 0x98d633d2df5e70b6b93936f225fcc16106383aa2      1
    499 0x98f6307ed597e17b5a61a1152d2c81ed23dbcae4      1
    500 0x996e1d4ce3e1e558889832832004b2466153adbe      1
    501 0x99e3a56a4ced244f54a98ef11a678c64ef45b073      1
    502 0x9a78c0913396f8607eac7c4ae211a9138c2a1fac      1
    503 0x9a9c290e8d15df4a0f880ac22342775614ce19c0      1
    504 0x9ad281e2572ea23d928114eaaaea433b74acb9eb      1
    505 0x9af52329437360323d4794f6f75fad4fe3b3599a      1
    506 0x9d12360d9553fc8afb8f0559c20522d09fbe6f79      1
    507 0x9d676d4564e43cddf9318d9a50c988f62b210179      1
    508 0x9d8b05df65f6f831b75dee291c38602d719f157d      1
    509 0x9e139c20e563220202cdeb038c4af1210362202d      1
    510 0x9e427ebcca41cd9423516e0c19769ca1a1f2fc25      1
    511 0x9edeeebae83123deb3e4c837f476ab815a34b572      1
    512 0x9ff77d193c091ae350b8ce26d50d46e392631292      1
    513 0xa03585bad4ca10f1b70805184bedb114f708417d      1
    514 0xa0c537293bdf3b6962104135746624c5e27d0c41      1
    515 0xa12df8d661035509ab443f9f5918206a261afd29      1
    516 0xa1374d1ca9dfc6338e491f21df1987ade9df9953      1
    517 0xa174d161361d6b92586f12a7a67ec1fe6f21f524      1
    518 0xa1a8a29c4dec5e676ad053c52fc011a98124dcae      1
    519 0xa20416801ac2eacf2372e825b4a90ef52490c2bb      1
    520 0xa2272469c824d2b34407c08c1b377f3027406fea      1
    521 0xa23cb68780be74b254a5f7210ec6cf1c76289953      1
    522 0xa269fab02cf89f43e63db80a3a5178c7eb3a6d65      1
    523 0xa29597016513333ad8da8c55dc990256082d1485      1
    524 0xa2b54019b4f580c298bba91c0e952e0af036b09c      1
    525 0xa31a547758f695822fe582c76a6c404d5adc988f      1
    526 0xa31cfd76a23a2e4f937ace5b774dc1b0de93197c      1
    527 0xa32f90b21d11561d31ff604745907acc77fb67e3      1
    528 0xa513f40ba07832d48c3c60e9f89b9208c5ce19f0      1
    529 0xa514b4552e4c78e08ac40dcb4ed09b84d6612253      1
    530 0xa5bb18341a207e288cbc1b91f28a54f7cf3e478e      1
    531 0xa61ceb386a787bb354d854c54bddcf1434e404df      1
    532 0xa647a51a1af1ccae559f3ea0addbf2a42285a3b1      1
    533 0xa6dc53ac86bedd2fae3731048bef4a3d4dc671da      1
    534 0xa6ed9729764b3b914bb457647af1d84ed2558a7d      1
    535 0xa706bbcc244d91a56ba9368c033cec1e75688af4      1
    536 0xa927b8c23b123ec848fc69e424bfaaee96560343      1
    537 0xa9582c88b64737f47f03874731c6de3f7df39623      1
    538 0xa981d37e8bcd56e51804aba37af379d4c5b63cb1      1
    539 0xaa185c2e2c15e2b6b6f6996bab6f9dd6133fb6d3      1
    540 0xaa1a54bfa93be8ac8d01182827433f227ba59e17      1
    541 0xaa6c28a775ca64c4104057eff7d2f0a07ce89b29      1
    542 0xaa6e633be7fe76331b67fa4a897f803d79fe53b3      1
    543 0xaa94088c66c1f3b38bf2c509b57a2f2ca427b8c9      1
    544 0xaac709a19274b53a9ce099e66c672422c019df7d      1
    545 0xab35becb4f0cfc408300131e802b9c51e868c5d8      1
    546 0xab3d1186864a55154ba91a9a1c8c1f4fa00b48ba      1
    547 0xab40ef5d3d86f90a5069df913edcdc4e4b99f9a6      1
    548 0xad8464f9f099569b23014402ac0603deffa49300      1
    549 0xad8798d1c0d85a94dfb8c0b033ecaaf38029ba94      1
    550 0xae48f64abeaa92618b8180c0fd33caebfed42f2b      1
    551 0xae6b51871ece99787d5f3806715f0cee9b72cf33      1
    552 0xaf329b47d4c4adc04c9629766fee0241bb1fc4b9      1
    553 0xaf6a78083708cae7aead9c96a1475eb25c671fbd      1
    554 0xaf9e18cd797548220317fb459095c6afac5c33a7      1
    555 0xafe6520e39b77158e15b3377c1528b590a887800      1
    556 0xafe65fa1f7402cb69f9f6ec5d506e43685bb75f6      1
    557 0xb18fd1c3505f1305df7881ae353429796e6c5c7f      1
    558 0xb1d0b2a6ee874dc04b493768504a7ce34ed75174      1
    559 0xb1e653022da54a8d5f99def7f0523b0a2703d86b      1
    560 0xb242bb44cec982bf1d6540c0860deef99479214a      1
    561 0xb26c452e8ac20ae7445fcf46b6478d3984f5d8e9      1
    562 0xb2eb4de0b3aa095528dcd60c74671c6f2dcb1253      1
    563 0xb2f13661eb385286ab330ba0e7661a1636793186      1
    564 0xb3eb9e97b5e6b7ecb50c7850d8b9a2b934b79c04      1
    565 0xb44c382d160fc0e07c83f7cfea08de4feb38efdd      1
    566 0xb46adfe7e10675b1e43f9132da7f00a9ad3642f3      1
    567 0xb5bbffd7d19e0ed0e4e29f86fb6c70a8379666d4      1
    568 0xb5eebdeb1b536f1ad2afc5d5d6c2f5acffea6a54      1
    569 0xb6266f7787376bf566e36f8683cf76dd0aecc7f6      1
    570 0xb6280e57f48d7083089d4b87989caa22e1c1b9b9      1
    571 0xb675b284d6f7d59f5b03b9b4da43d89b0d8a277d      1
    572 0xb683a2056526162c4771d363204af41ea8c1ec52      1
    573 0xb721bff27bd2292e890d578153dbac9120f3b613      1
    574 0xb7887dcc38d06b6a3b24c13f793d25e042eed825      1
    575 0xb79157bca1635f4731df5828f2a28a0cbfc799ce      1
    576 0xb7ef5fb53dc42839270a94ca569536a7c7e7edde      1
    577 0xb83caf0392e520c49646b693129892f2759fc5a0      1
    578 0xb890f264b1a7591cdf9e21b3b640a741cf677ae9      1
    579 0xb8c491fe7eefb86db448765a0d3e308c3f500128      1
    580 0xb8f25506001b20e368b5276c4892673c122c8316      1
    581 0xb94404c28feaa59f8a3939d53e6b2901266fa529      1
    582 0xb947a2dac76067e077c2823a1caa9ed3690f0bbb      1
    583 0xb976163a6f251c208bcf3196b9c4ddf927b63e72      1
    584 0xb97a251df2672b0e252b28a96857a8ace9929ccc      1
    585 0xb9a7329e124edb947255e58df434d4b495def0e9      1
    586 0xba517b018c051ed2ed7cf090afd5c27b6137e34d      1
    587 0xba7cfbd459dfa75ddbb9901c661804d06fd4dbac      1
    588 0xbaa02edb7cb6dc2865bc2440de9caf6a9e31f23e      1
    589 0xbb3be72a5f745a5a055dafa72b13a6e04fc5859e      1
    590 0xbba3ced54477c12fdf16d7009771affc7a8c9ba1      1
    591 0xbc14ec25110281f0332430943b9a203c65a1b7e8      1
    592 0xbc1f37c7226fa710d0de26e63f18ff32cb53fa48      1
    593 0xbc2b15e5e105ecb3f2398e541607ab85530274c3      1
    594 0xbc3b6ba1c78e2e34f8d650cdf1b7684b5aedc8ac      1
    595 0xbc9ca5bd0f07700929f8d538233b0a9e60f4ddc5      1
    596 0xbcceb3da855bf1e833cba88363990dfb3083b612      1
    597 0xbcd11037513c18cc56f22b1da6263d344a237b1a      1
    598 0xbcef9c38100e616f26b899c69917e7650bc84b27      1
    599 0xbe1ce9584bb326322963ae244175470232756f39      1
    600 0xbe562c1bd15faa206173ff8095de985c812ce462      1
    601 0xbf82888edf59259902f864fd0c1828fb98533501      1
    602 0xc009c7ae5f3e6df9810a8244ed9c9e3d35b789a3      1
    603 0xc0ce1e8cca9d9376aeb8b4e40286d57554f46cfa      1
    604 0xc129b60376046f9132cdb90c7398209eb2db0c4e      1
    605 0xc17e1a53ec41936072be46032749fe52888e78a3      1
    606 0xc18d4c2fee42accfe7bfdce98bfacbabf76242e9      1
    607 0xc229d7d3dd662a1b107e29aa84bb0c8ff609cf3a      1
    608 0xc24cb90c69a6069a343d7691b99b9be8bdc3beb1      1
    609 0xc388a0cf7962174f976cdfbdcd5f3076920d2879      1
    610 0xc38fe30a72813293bbb1575a654fa937596b9854      1
    611 0xc481aeec5a219ca98643e71f0b967f40bf211b38      1
    612 0xc4b5bdc3b6d27a2e1c618498d3cbfd97358b2a3f      1
    613 0xc4ea7bbc6b186089ae4b90e282dec17ee08bc340      1
    614 0xc4fd28d2bf0cf81279f664035798e27257f830df      1
    615 0xc52650d88793cf5fc0295bee9ae49284e67fecb1      1
    616 0xc60384edda8b62ae9a97ad3bdde1011c6e2331be      1
    617 0xc6482be5430ea8f0262a925bfec921831eda3ffc      1
    618 0xc6605255a3be604a74e2224ab515718ee01e7fba      1
    619 0xc665a60f22dda926b920deb8ffac0ef9d8a17460      1
    620 0xc6893eeb690596e44f6c8668990a5cd7b8b1cedb      1
    621 0xc698c41c7cac9dbc9e1d931860b98b131d97fbca      1
    622 0xc6c36027878ba38964e4d5a60ddb20d09565354e      1
    623 0xc6d648a3f05b3d1d808ec01637ce52ec2721cf88      1
    624 0xc6f7237355ae0f6cae62a8e357855a2902afd0f8      1
    625 0xc704389a3f999f325d568df1ab676d4b750f7c37      1
    626 0xc70b0b44e47e8604b4234f2d2d8e79540b0cf64b      1
    627 0xc772eea3188f8344ed1463136e6556ddfe91ff42      1
    628 0xc7eba7dee877e6c94f326c2f0138daec35f07ffd      1
    629 0xc839de0fed241607d50aa4107ae582443b906e4c      1
    630 0xc842ce7214a14fa98186a010bcb43c7e99e4caf3      1
    631 0xc8691636e4c3c006ebbcbad1fda10d476f5d65dd      1
    632 0xc8a974a97f6a7f57b6ce09aed5905d5547039f11      1
    633 0xc8ba28507aa7f1825db28a7882402d616ad7f562      1
    634 0xc91e9753fdb5863c9e76b57757219c9dee5a677e      1
    635 0xc92fc8128e8508ea5ec8b68e02f1d4a454a3cb58      1
    636 0xc93c7f71581dfeaab59bed908888dac5689f312a      1
    637 0xc9989b1045ffbea79a5205ba48065062a0c9e411      1
    638 0xc9bc29afe2353d4332a37bc3728111eb1d13389a      1
    639 0xc9cfb1eec514bcad0b50d0e5b44a9bb0216b0ea4      1
    640 0xc9f5654e2f1bd05255e2fa048de7d10acdb8c382      1
    641 0xca11d10ceb098f597a0cab28117fc3465991a63c      1
    642 0xcb269f336c32c847c2e6f0f707d28373c1cd1ff8      1
    643 0xcb9bbc9da5e28d3939c1045962f48883c573a913      1
    644 0xcbbca4a6f52a9838bc743bddf82675a64af17e6d      1
    645 0xcbc37d1120d7fa7013bcf35582cb12ea17b2bd4e      1
    646 0xcc21917b8575219e2df9a56c1719f2fb89ebf0ce      1
    647 0xcc47fcbfdd6e558fb39cf5ef4d92e59890476b86      1
    648 0xcc48857242141cc7f4f8555f8a3d5bed03556c19      1
    649 0xcc62a43e3721f6ac949a5759291e05b96cd56f47      1
    650 0xcc94f921f93709281b03488ea4bdad868b955856      1
    651 0xccbe7cb4ca2a8ee015ce0a9e50b1ce46adf59d12      1
    652 0xcd241563f7288b5eb2b9b715f736232889b62d8b      1
    653 0xcd9170edaa51e970114cbae25475f89c8f0de8cc      1
    654 0xcdd370f39bcb0c5622f591a3b7b824812625ac77      1
    655 0xce435038720f5db23cc8627f825382dfe85a7e06      1
    656 0xce6920f5620df958778cb52d119ecf78b0963337      1
    657 0xcec37262d84ee63adea1639569124b55ce41802b      1
    658 0xd024f5fa05cf6298697d10e7c21972a2379d42ce      1
    659 0xd058f7f3b55cfcc600e1f3852829d3a0d07c3e5d      1
    660 0xd0d710ae6e0970e4cbf2f86c2bab71a89f4ac575      1
    661 0xd10a4426df58fe27ce1207bd6f0f04738039336c      1
    662 0xd145a23193220aa02214f6f6f247cc5f5a22a9fb      1
    663 0xd1a379d1619cd531a010f308b997445aa24c0013      1
    664 0xd1ba123d1fce1b43669305d95310bdb5a9053d2a      1
    665 0xd1be82e7fe8c23d2a335558860e844e327863e3e      1
    666 0xd25469a1d8789266574f04ac424f72db4a4f648a      1
    667 0xd2b789811ba988c44e8228eebc4dcc5663400cf4      1
    668 0xd2b7e133d4e7044f7ef36f697f4ddfe07411b717      1
    669 0xd3aecf9e0856822bd320873e905ae9f78a2977e7      1
    670 0xd3cbf8645d8f28f6758504b0cf2ab4206c5dd7cc      1
    671 0xd3e2e76e4ab0322614fe7aaa3be0b04bed83d0d0      1
    672 0xd44e9b676e74ae45c0a39150be771eb189bb2337      1
    673 0xd49e55b8078dec84a3c0315fd76d89047768d0a3      1
    674 0xd4a1dec441846fcc444dcdab7122f690370a3dc4      1
    675 0xd4baa5b1cfd74512a4243b09b544e6fb19832389      1
    676 0xd5fdac733437925cda98861420a45816a023e7d0      1
    677 0xd68773132308d3ffae55a719907e87cf00034cff      1
    678 0xd69628ca41337176a9d3eadc98a78316a1d29d92      1
    679 0xd76040b01fa18843f8983dbb1b40d3a64b5b771a      1
    680 0xd7758284b832ee28c3f3b9aba9fa1596d116acd9      1
    681 0xd78bb8dc8c5ff23cc3c1b39f8577ae8a4ebd46d6      1
    682 0xd7dd9612a21f7c249fb7f33e9c2e9144345e162b      1
    683 0xd83e662b83880081741d0e810c3fe8630fa6cb4d      1
    684 0xd85936f9566ec0b410926fa5710cba5677601a18      1
    685 0xd880f27a3c4233f0371f879cf3d29cb106c5aa1e      1
    686 0xd8d005f66296068a2efc240f7e5910af52a86ee1      1
    687 0xdae6ca75bb2afd213e5887513d8b1789122eaaea      1
    688 0xdb154a849f364eb95abb30efff4da67b55d4b023      1
    689 0xdb2ba0eabbfe920d4e501dfd92d6667f512e5870      1
    690 0xdb75ae62eec74e21d193ad27713c703a9bc84ead      1
    691 0xdbaa79abde33562e664fc449a562e9cd72270c78      1
    692 0xdd5f934edbcfbdb25ef5dd9b8a3868d3633de6e4      1
    693 0xddb606b2e344e3e08d8e0300dc2c8c229b5006f4      1
    694 0xdde40d8b412f8ef7b881386f2ea88770e8f5b9f4      1
    695 0xdf067d1c62b6a94e45ced3f01ebc94ae304cb045      1
    696 0xdf076cd8736b21ec0b02a91b02c10a53899b0be3      1
    697 0xdf56121c11286d9a6ebd6971656c9d1d55784737      1
    698 0xdf569b29fd974c75891823ecb0f17d74cccfdbee      1
    699 0xdfe8beee223412f316baf2968b17527d6eba29f1      1
    700 0xe13d4abee4b304b67c52a56871141cad1b833aa7      1
    701 0xe1c16e2278b5ed608eed8ce226594f51b52cc485      1
    702 0xe1ccd64c452096538b07fc68e89196fb6309e01c      1
    703 0xe1d23eddf8f0f597a8582d86586999ffc41bb030      1
    704 0xe26253497d7e2d62ca8d3cbe98ff7a114886d468      1
    705 0xe301612b0751f6d862a27be205fb8e3c40c8c6db      1
    706 0xe34200e26c6b0802140bd64a626b42d630171ef7      1
    707 0xe3aadd3385ff2883fd675cf0c181adc7e1f3b8cb      1
    708 0xe3fa94969505b34a0d5b590cf65cd5a208341edc      1
    709 0xe4575c894b13c99c8f3e9fc0dcdd220131aabc0b      1
    710 0xe4a1f0c5cc90fd38f4d846eaf8be6daffb7b8cf1      1
    711 0xe4c6c46645988bbafd8ef6b4d1b60d969cc857c3      1
    712 0xe4e81e23e5efddfb51ad86199a9ff5f0a296cc3f      1
    713 0xe5582876b2f827dafc810f51264b9ac3f6e6c17a      1
    714 0xe55c987aef8a15601d3dcc1609aceda43d82a8f2      1
    715 0xe5798a530bb7105e148d38ac884f05c28ed8e804      1
    716 0xe58f13c04fa451556c60f86c002de08aaa388a52      1
    717 0xe63c3b169162856ee11d1f189017c2b997aa969a      1
    718 0xe72eb31b59f85b19499a0f3b3260011894fa0d65      1
    719 0xe773382a7574de1c82b1f67099e680c043048708      1
    720 0xe7e7f922cfc016b96658b4a5dd26f59231f3c93e      1
    721 0xe8222179ebf7c93940b63928fcfc31b1bd5c0bf5      1
    722 0xe83c750b2708320bb134796c555b80df39a3d97b      1
    723 0xe8cc7c453eca25450ca16d551db93d5f5740e054      1
    724 0xe9b5a2529ad21454cb5e4d172dcb4bc501789463      1
    725 0xea47ac15029691b13eef44769b673d65ba4adbc0      1
    726 0xece31309d670365a2001dc63c645123abfe4464d      1
    727 0xed3c3bffcf147850406d0e58ce4f4ebd2b5cd96c      1
    728 0xed66da2d5e74c9c6a024e19f6c7be74bab5541ff      1
    729 0xed83f0bceef992fa58c8bc8a27269eedb3574922      1
    730 0xedad4633fb57c805f413f09246762fb357e28a37      1
    731 0xedc5925cab7d4a9d89e9e318f3c05cd65fbfdba9      1
    732 0xee0d982d26d7bf4f52a62a06d22eb7c00576cbb6      1
    733 0xeebfd0ae2ce773db3dadad8d51ad59ec11567f16      1
    734 0xeecfc563d14ad858098b6998607bd79adda2013c      1
    735 0xeee71619e1cfed4d2c502adf4f66f05320e88fd0      1
    736 0xef0b5449cb5c4fc8d0f4a9421d4a459055d4f45e      1
    737 0xefd4d50410ca6864b74a926f3443e07c5db8050e      1
    738 0xefe6a93013b1b79500a22335707a5b5d3842d461      1
    739 0xf054274dd74987395d28136e53f39ef4f7b19994      1
    740 0xf08cbd713b1cffd26c984329c6f55105aa9fcd06      1
    741 0xf0c0917bed1d2311cfa261493a21ddfc450eeea5      1
    742 0xf1133ca2ff1b4e5df7121566c0520199f8937e78      1
    743 0xf142d7baff0986b50ae24e694419c65e7091f52c      1
    744 0xf1484ba8048349e860e7d9eac49c2764e9e73e05      1
    745 0xf161ff39e19f605b2115afaeccbb3a112bbe4004      1
    746 0xf1b711b4f2af54c713cbf4b9f415d5670afd652f      1
    747 0xf2c415a4dd95f4147b4475a6eccfe16fc352efdc      1
    748 0xf3290df945e484a4ff846fb5baeb6d1147a7aa69      1
    749 0xf3860788d1597cecf938424baabe976fac87dc26      1
    750 0xf3ab1c5047cbc4cb0a61c9cada9b2929e18d11d1      1
    751 0xf4290ab1a8129b8b887d3ab331f24ecf812bda8e      1
    752 0xf44069ed505456dc813fd2bbb998b229947df309      1
    753 0xf46dfa71f8332bb833605b80bcebfa6dab95ff43      1
    754 0xf4a1b6c60c50fdd55c369ad867001450b1f45344      1
    755 0xf4cdfea7fdf85872141e4f8eb0e953427de7626f      1
    756 0xf4d176848a02eb80b507e5301e6338accbda4900      1
    757 0xf53af966dab1afaaac1171dd941148384d38417d      1
    758 0xf55cbf08ae9424c572598b29064b2fc3f67add04      1
    759 0xf5b893a96959481dcd3463fa90a1b66c57fee5b1      1
    760 0xf5c9752e5a02ff9dcc04421de916a19d4c62f8d8      1
    761 0xf67b653f0905e36dfdb8753bc890177623551886      1
    762 0xf70ecf14e33d5cc272b5f81f7dfb74364c179099      1
    763 0xf7ee6c2f811b52c72efd167a1bb3f4adaa1e0f89      1
    764 0xf8c51751579133db8039a20de33b41c0616c6933      1
    765 0xf9a5207f4dcf4a617b5f9753fcd362382ffe79ba      1
    766 0xf9b45750c92b3dab01f26c46160bf981b82f8028      1
    767 0xf9d681c3b81aa1d0ecb3fdb4c69ca57714eb63f4      1
    768 0xf9ff406304e9075d93a2328ce4550b87e0757a10      1
    769 0xfa64ca60cf0e298cebb64f4e263f23ce1bc70634      1
    770 0xfa8ebf2a9efe34701af9c9f96dc32ce3925d913d      1
    771 0xfba98496d9adfca28f4f6a863e05dd8f9e70ddb1      1
    772 0xfbb494b311f8790072d6f2d5ed1ab8695ea8890e      1
    773 0xfc295aeb2be32ef90104cb3e2807896e85647b73      1
    774 0xfc840af78a496b52e378dfa94838402e1b7f71dd      1
    775 0xfcba4dad18dc334f97e000105653ac7347294c0f      1
    776 0xfd2249bd6a703df6dbe88a0f264288d821788919      1
    777 0xfdaceafa0180f2b9b061cf0f48a19de63b6f3eb0      1
    778 0xfde25d8e5e1e3b03f4d0b1d5c793e3916fd85bc4      1
    779 0xfe899c851c20f84e1a4d53600bcab433eefcb966      1
    780 0xfe8ba5971e782173a80e8fd7b9eb85b2ead86f08      1
    781 0xfeae88b979ec76ff83f96dfbb5cfca42b92b6a1f      1
    782 0xff349d07cab9b90a4018cf182df8ec564ea61af9      1
    783 0xff93d74029596d8f91f594de4b19191f9b3d5cc3      1
    784 0xffb6d97bd1e7b7bd08595096d15037401a1f416b      1

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
