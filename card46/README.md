
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "16174069.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:248         Length:248         Min.   :1   Length:248        
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:248        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 16174669 # https://etherscan.io/block/16174669
block_hash <- "0x7554250b56f70fc9ba198febef8cac33fbc0f0aa070975f3d32d1db2fee508d8"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4889 

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

airdrop_gradient  <- pick(snapshot, contracts=c("gradient"),address_remove=address_remove, address_pick=10,address_max=1)
airdrop_memes     <- pick(snapshot, contracts=c("memes"),address_remove=address_remove, address_pick=30,address_max=1)
airdrop_rik   <- pick(snapshot, contracts=c("SuperRare","SuperRare2","Arcus"), address_remove=address_remove, address_pick=31,address_max=1)

allow_gradient    <- pick(snapshot, contracts=c("gradient"), address_remove=address_remove, address_subtract=airdrop_gradient,address_max=1)
allow_raw         <- pick(snapshot, contracts=c("raw"), address_remove=address_remove,address_max=1)
allow_singles     <- pick(snapshot, contracts=c("65291/1s"), address_remove=address_remove,address_max=1)
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
     1 0x27b1abafb2cb065cfaf41b4b7ee95d27192151b2      1
     2 0x48464efe55fbcae8ae0c992b306afcf21d4910cf      1
     3 0x64f7de90dc79d775703bbec66a1591c7a26a22f0      1
     4 0x6f113b0c8c7266d7514e4986e3d6aaf013b6754f      1
     5 0xab2056903a7b62bac46f45a3d7a70ac799ca88cb      1
     6 0xb5374cac6cad6b025246f19d20b0d4151b640558      1
     7 0xcabd5f77ca9d48f4ef9793f20de42ad39ce93979      1
     8 0xd3e401814d1faa8ca0419ecccbfee93ac7b15b31      1
     9 0xdb561a899557404581e6180fe6d4178577dc117b      1
    10 0xde112b77a1a72c726d8c914c23dceaa53273c351      1

## Airdrop Memes

``` r
c(airdrop_memes) %>%
tally() %T>%
readr::write_csv(file="airdrop_memes30.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 30 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x147f466fef6bd04617fc0c8037ea01c73bc25a3f      1
     2 0x19e7a132bd4b42f580f546197f42e19c42cdfe6c      1
     3 0x1a4370fdd7173d0a41ff7c63a8e0249479ba0225      1
     4 0x1e47bc8b6c585dfff2e166257767050666151f0a      1
     5 0x1ffc29a768e26ab393ea93e4284773410a84b660      1
     6 0x23602ca06e977c86339ffddad74966e824ab691e      1
     7 0x29273917eb777b8d227e64ea423d07e6246088fd      1
     8 0x2c8875f34ceb219f61b7453b2c5f100ec2f6ed33      1
     9 0x2ec4a2bcd4f33c7c9aafab7cfa865ec15508bf62      1
    10 0x42d38ed60a64d0b8c36f190e185d17db3617a091      1
    11 0x5c3097e7fd3b97d9fdeec6d378884c892ff0545f      1
    12 0x692d6cf19e0c082185e20ff5ea6bb267e7aeb278      1
    13 0x69fde561275b85dbcd5081d1121bcae64fb83858      1
    14 0x729fcbb6e1289c88ce5113ec1c83a48a8e3c9f2f      1
    15 0x8874174a2366668d54fea6343f71709389563c8a      1
    16 0x9caed8a01c87721570de895a9725334a0a0cdb70      1
    17 0xa490a0346808dda91aea6698cb19e4697d9fc5cc      1
    18 0xa7cafd18dd8bc1e23203058d66f89f0f0ee539d9      1
    19 0xa90aa01ec98f390cb86cd1846f7c0fd342a07955      1
    20 0xab2056903a7b62bac46f45a3d7a70ac799ca88cb      1
    21 0xb0767b217fb1530b064bb1f835c57c047c08ae72      1
    22 0xb42ab92f5af0f162fffefa2b1e12702ce3fc9e17      1
    23 0xb62b86353c9c38665f2a3843ea4eb6f7eef9e5ec      1
    24 0xb735af7ae1a77d1ec764c862c2c09bdbf2b34b27      1
    25 0xbbc37f68e9876d64b2c55016081528ae0a85d8b2      1
    26 0xc7bb15c11595c877302ddfb330a4082d92f5bcd7      1
    27 0xd1f6e5592361257aef68b96df42aef064080c5cc      1
    28 0xd87ca052936bcc2b6283b87d2f0aa95cf0080584      1
    29 0xeadd2d51e75a8b42302adc2aff0157a4bf626adb      1
    30 0xef15bd3f923245b24d64d6fbf57f036fe650c23a      1

## Airdrop Artist

``` r
c(airdrop_rik) %>%
tally() %T>%
readr::write_csv(file="airdrop_artist.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 31 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x00362d9f53023ee0fde44773c3fa79917adb094b      1
     2 0x080314581b003b60a61f8bdf457b26d283dd68b1      1
     3 0x099b8e06be2bdc25cda14b5bd843660459a01131      1
     4 0x13075a80df4a80e45e58ef871900f0e0ef2ca5cc      1
     5 0x13c0dabeb938c21524c59cdc40bcb6fdb3618754      1
     6 0x21301d901db04724597d1b6012ac49878157580d      1
     7 0x279a84d4d6b1d4b1895086c775db1e91d5849cdf      1
     8 0x28685cbf9b940923bd3de39232fb418243fb1e16      1
     9 0x2e12f3a30600c82536438051da791e6a8d092d5e      1
    10 0x3607eda07116b277c1f791ee224cb2ad9291a009      1
    11 0x3deed956b999b83361b85bff31d388c35125411d      1
    12 0x60f1bbd5d1300d8db8abcc75236eadf2adf5104e      1
    13 0x665654f2d2a151be2d0f8e3697e6ce780f732af2      1
    14 0x777c87cacd482f5f5667859bc0a83acc7c931151      1
    15 0x902552dee5e3cf54dc7a1eb97017b85788120055      1
    16 0x90c65efd30cdf57e72c9b394b6cf36707135ec7d      1
    17 0xa5a320d97d58943eb44c7f6f702148bc5f77d378      1
    18 0xae805b9eaa5a04f307b439484ed881e75d36ac73      1
    19 0xb897000365ff909dcaae2ed135e7c212eb000fd4      1
    20 0xbba3ced54477c12fdf16d7009771affc7a8c9ba1      1
    21 0xbcc462c488f8da534af98f0f2e529cded02d9cb5      1
    22 0xbf0b5e79bf66edc4e3d4d060ccbbe2aa5d45cf39      1
    23 0xc15753ae3b6099b8a3366b836433d6542645b876      1
    24 0xc3630cb705fa14d5c77cc8ca324377f7126fd14c      1
    25 0xc68bb9728103765ace6d43d98bce3281af3850cb      1
    26 0xcaec3a0196ff5450b496e618b7479f9a1b729633      1
    27 0xd1e8a92f44bdc83bc620c56a7913fd97de5abe10      1
    28 0xd3143da6441611570a85a855d842b1d4f55bc28b      1
    29 0xead90127d60f78f3e97781938418004be1794eae      1
    30 0xf1544ba9a1ad3c8c8b507de3e1f5243c3697e367      1
    31 0xfad7f76d73e79a9c65c85f23f16631180c150ec8      1

## Allow Gradient

``` r
c(allow_gradient) %>%
tally() %T>%
readr::write_csv(file="allow_gradient.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 69 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x04df8d02f912d34fef12a1b0488ee56fd6f7416c      1
     2 0x0a98f97e89743406a8611e1b4219a073b60ffed3      1
     3 0x0ca4dc095608c6b153019f734106581c85f2dcc3      1
     4 0x114c1eb0ddd7cefe5f4fa679b1f4d92c09126b9b      1
     5 0x129c6695dfe7a906bd8fda202d26dfff601f83a4      1
     6 0x1566ae673ae80725bcce901b486c336e6acef465      1
     7 0x16ecafb3b5d8e15d07bf8d3ff3a3f9ab16cda860      1
     8 0x21b9c7556376efdbf9ee316a4ede0c8257fb7533      1
     9 0x22fbaa2dd20b848084545515fb04b9c846942779      1
    10 0x248458947c120ca057ec028b3fe7e4b3f26fdb3d      1
    11 0x28b8d4f7516a112e2e2fd462293a1c27cde327a7      1
    12 0x2924196a2ec71ea4ae1b1357381eccdcee6c18f2      1
    13 0x2f2242b447a54cb110b8a7991cf8a27054ee6921      1
    14 0x3511ae23ee25e2b97dce883c7d496ff5d18f1dfa      1
    15 0x3cb63b82d778105e43f064ed739b0655f1f0fb87      1
    16 0x40d2f4399d23f9afb82d0a6b73055f13208614f9      1
    17 0x43b0bd27d5016e969193412933387b0dd4cf3e0a      1
    18 0x45855a3f4404aa08ffe14a366c75663f4ded2fac      1
    19 0x477ea7a022e51b6eb0dcb6d802fb5f0cfc3b4a81      1
    20 0x520e8d6c5b2dbbe62bf8a40653a314d879b19b86      1
    21 0x53006f95def268f88dc1b8216654ab56f3afd052      1
    22 0x575f6540c16a72696c14a17fa64f049992d661ab      1
    23 0x5fdb5fdb61bc2975f3be446c5ae9c5df490f55d2      1
    24 0x61d9d9cc8c3203dab7100ea79ced77587201c990      1
    25 0x665654f2d2a151be2d0f8e3697e6ce780f732af2      1
    26 0x670a840848eba22eb2a090fd65e1ff60c3ba9e5b      1
    27 0x699990a8e7ada9e92c932d6e8fb365024fc74b43      1
    28 0x69e68074f1aada957edd39c5eae0069973343f30      1
    29 0x6d1db4a7e83dae0eee7e95d421722d46d2a7e94b      1
    30 0x6f0735bf1e6c69030d6990cdd580345b370eb50a      1
    31 0x729fcbb6e1289c88ce5113ec1c83a48a8e3c9f2f      1
    32 0x73bcb8c5e30bf85806aade7fc36f16c6b80fd3b9      1
    33 0x7546c60ae8d65dc6dd7a0f61c169818059ef49db      1
    34 0x76db02500f7631d57bc2dcdca9d4cf782b99e119      1
    35 0x7e5ab36876a267560e7191cedbe99ee7bc04bc30      1
    36 0x82139687faae8a29851902783e02e699de0e0846      1
    37 0x896b94f4f27f12369698c302e2049cae86936bbb      1
    38 0x8ba68cfe71550efc8988d81d040473709b7f9218      1
    39 0x8ea76483c888f5bda7d96cab9839488f691daf78      1
    40 0x9224bbb4e0fbe2f2f8fab55debc41eb21fdfb804      1
    41 0x982d3c5223f6b5794fccb3208eb164d042cf2526      1
    42 0x9dbd781eeba135ad2a779926880adc89196a3265      1
    43 0xa32f90b21d11561d31ff604745907acc77fb67e3      1
    44 0xa45d6303369917528f17e14f080acf45d4edb776      1
    45 0xa71000e72d38e6a84d7190f59fd3dfc73931c0e8      1
    46 0xa743c8c57c425b84cb2ed18c6b9ae3ad21629cb5      1
    47 0xae0d16586e5d60d334624c115216a52b9b1a0335      1
    48 0xae72c6a6fad9fa9d82d089e1ebf73b3043855425      1
    49 0xb6b4a02dca517564eb98790ff67d42b5b37a3d4e      1
    50 0xb6cf25f5cf8a1e1727d988facdd47f1dfc492caf      1
    51 0xb735af7ae1a77d1ec764c862c2c09bdbf2b34b27      1
    52 0xba4575ea27041d99e6614ec02318f1e23a623fe2      1
    53 0xbba3ced54477c12fdf16d7009771affc7a8c9ba1      1
    54 0xbbdd72fcce73c2626719be00259ddffef0d5673d      1
    55 0xbf270918afe2ad16093ddce904fc358ad337cefa      1
    56 0xc02e6b0d0c1a5d8cd26beeba0fe8d76c5d2f19b9      1
    57 0xc2419841dcb9a0f8906d06463ae24e00e1470846      1
    58 0xc26012491b9dfb2e6f2cb0305673e212721d5950      1
    59 0xc762b1081c56b3fa487c7372f7284d9558a84859      1
    60 0xcaa1c396e70384db58dd33be74b26fb119e98c3a      1
    61 0xd40b63bf04a44e43fbfe5784bcf22acaab34a180      1
    62 0xe359ab04cec41ac8c62bc5016c10c749c7de5480      1
    63 0xe3b41ae8785e4107cc69f988042ff4a66a367fac      1
    64 0xea39c551834d07ee2ee87f1ceff843c308e089af      1
    65 0xee958e45f3464d712b8830deb5875c8ac105f698      1
    66 0xef5ab90a44b68d4f5e3f6be6af4bedb12cd2c66e      1
    67 0xf15a6b54e68884d27e1bebb1624d70c227b7d04b      1
    68 0xf2c5f1fd977dbd6de9d04bc4e62dff722d4bb1a1      1
    69 0xfd22004806a6846ea67ad883356be810f0428793      1

## Allow 6529

``` r
c(allow_raw, allow_singles) %>%
tally() %T>%
readr::write_csv(file="allow_raw.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 4 × 2
      address                                    amount
      <chr>                                       <int>
    1 0x2c8875f34ceb219f61b7453b2c5f100ec2f6ed33      1
    2 0x9dc5b2cee788e3e9c67489b5d7ce634dbf48a32e      1
    3 0xa45d6303369917528f17e14f080acf45d4edb776      1
    4 0xb5374cac6cad6b025246f19d20b0d4151b640558      1

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
