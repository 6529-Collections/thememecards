
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "15661916.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance           contract        
     Length:10008       Length:10008       Min.   :   1.000   Length:10008      
     Class :character   Class :character   1st Qu.:   1.000   Class :character  
     Mode  :character   Mode  :character   Median :   1.000   Mode  :character  
                                           Mean   :   1.849                     
                                           3rd Qu.:   1.000                     
                                           Max.   :4209.000                     
         name          
     Length:10008      
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 15666928 # https://etherscan.io/block/15666928
block_hash <- "0x026fa289080c65a6a19368a1f810908c0afe7917927af11322b700e1959ddf64"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4364 

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
  "0x0887773b5f43c58f0da7bd0402fc2d49482eb845"
)

airdrop_gradient  <- pick(snapshot, contracts=c("gradient"),address_remove=address_remove, address_pick=20)

allow_gradient    <- pick(snapshot, contracts=c("gradient"), address_remove=address_remove, address_subtract=airdrop_gradient,address_max=1)
allow_raw         <- pick(snapshot, contracts=c("raw"), address_remove=address_remove,address_max=1)
allow_intern      <- pick(snapshot, contracts=c("intern"), address_remove=address_remove,address_max=1)
allow_singles     <- pick(snapshot, contracts=c("65291/1s"), address_remove=address_remove,address_max=1)
```

## Airdrop

``` r
c(airdrop_gradient) %>%
tally() %T>%
readr::write_csv(file="airdrop.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 20 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x1c29dcaa0cad96ca3f60d414c7e2e47c99cd7bdd      1
     2 0x22fbaa2dd20b848084545515fb04b9c846942779      1
     3 0x477ea7a022e51b6eb0dcb6d802fb5f0cfc3b4a81      1
     4 0x575f6540c16a72696c14a17fa64f049992d661ab      1
     5 0x5df5342342701b8ae5bce28f74ebb73b5fc13a54      1
     6 0x61d9d9cc8c3203dab7100ea79ced77587201c990      1
     7 0x670a840848eba22eb2a090fd65e1ff60c3ba9e5b      1
     8 0x69e68074f1aada957edd39c5eae0069973343f30      1
     9 0x6f0735bf1e6c69030d6990cdd580345b370eb50a      1
    10 0x6f113b0c8c7266d7514e4986e3d6aaf013b6754f      1
    11 0x9224bbb4e0fbe2f2f8fab55debc41eb21fdfb804      1
    12 0x982d3c5223f6b5794fccb3208eb164d042cf2526      1
    13 0xa32f90b21d11561d31ff604745907acc77fb67e3      1
    14 0xa743c8c57c425b84cb2ed18c6b9ae3ad21629cb5      1
    15 0xc26012491b9dfb2e6f2cb0305673e212721d5950      1
    16 0xc762b1081c56b3fa487c7372f7284d9558a84859      1
    17 0xd3e401814d1faa8ca0419ecccbfee93ac7b15b31      1
    18 0xe359ab04cec41ac8c62bc5016c10c749c7de5480      1
    19 0xea39c551834d07ee2ee87f1ceff843c308e089af      1
    20 0xf15a6b54e68884d27e1bebb1624d70c227b7d04b      1

## Allow

``` r
c(allow_gradient, allow_raw, allow_intern, allow_singles) %>%
tally() %T>%
readr::write_csv(file="allow.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 71 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0a98f97e89743406a8611e1b4219a073b60ffed3      2
     2 0xb5374cac6cad6b025246f19d20b0d4151b640558      2
     3 0x04df8d02f912d34fef12a1b0488ee56fd6f7416c      1
     4 0x129c6695dfe7a906bd8fda202d26dfff601f83a4      1
     5 0x1566ae673ae80725bcce901b486c336e6acef465      1
     6 0x1a4370fdd7173d0a41ff7c63a8e0249479ba0225      1
     7 0x248458947c120ca057ec028b3fe7e4b3f26fdb3d      1
     8 0x28b8d4f7516a112e2e2fd462293a1c27cde327a7      1
     9 0x2924196a2ec71ea4ae1b1357381eccdcee6c18f2      1
    10 0x2c8875f34ceb219f61b7453b2c5f100ec2f6ed33      1
    11 0x2e54b594ff08bad82db35892f1265994c57bf46b      1
    12 0x32d53a21813debfcb33a2b1ff2a396bd3a06f818      1
    13 0x33aa9daac0cf59b34a1d4cf9175491b4bcbd98f2      1
    14 0x35bace30dcacfbfdd29e0bd822f8d3cfdad00d3d      1
    15 0x3cb63b82d778105e43f064ed739b0655f1f0fb87      1
    16 0x3f849f47f5b372d80407e442f360ad7b17f5fac4      1
    17 0x40d2f4399d23f9afb82d0a6b73055f13208614f9      1
    18 0x43b0bd27d5016e969193412933387b0dd4cf3e0a      1
    19 0x45855a3f4404aa08ffe14a366c75663f4ded2fac      1
    20 0x48464efe55fbcae8ae0c992b306afcf21d4910cf      1
    21 0x53006f95def268f88dc1b8216654ab56f3afd052      1
    22 0x5fdb5fdb61bc2975f3be446c5ae9c5df490f55d2      1
    23 0x64f7de90dc79d775703bbec66a1591c7a26a22f0      1
    24 0x665654f2d2a151be2d0f8e3697e6ce780f732af2      1
    25 0x699990a8e7ada9e92c932d6e8fb365024fc74b43      1
    26 0x69cb3b1de24e08f1cfc2994171b6c6930498f750      1
    27 0x6d1db4a7e83dae0eee7e95d421722d46d2a7e94b      1
    28 0x726022a9fe1322fa9590fb244b8164936bb00489      1
    29 0x729fcbb6e1289c88ce5113ec1c83a48a8e3c9f2f      1
    30 0x73bcb8c5e30bf85806aade7fc36f16c6b80fd3b9      1
    31 0x7546c60ae8d65dc6dd7a0f61c169818059ef49db      1
    32 0x76db02500f7631d57bc2dcdca9d4cf782b99e119      1
    33 0x80a1c9fdc26199a69d190ebc8ad287ef48758977      1
    34 0x82139687faae8a29851902783e02e699de0e0846      1
    35 0x8476b6a8aa0b4037e69e79f116f662aa0096b0c0      1
    36 0x8774be790cb9e12d5edaf2eb8a3f6c89410a497d      1
    37 0x896b94f4f27f12369698c302e2049cae86936bbb      1
    38 0x8ba68cfe71550efc8988d81d040473709b7f9218      1
    39 0x8ea76483c888f5bda7d96cab9839488f691daf78      1
    40 0x8f8b4759dc93ca55bd6997df719f20f581f10f5c      1
    41 0x9dbd781eeba135ad2a779926880adc89196a3265      1
    42 0xa4cb937bc5ec481fc2674794d4146206ecc71b15      1
    43 0xa71000e72d38e6a84d7190f59fd3dfc73931c0e8      1
    44 0xab2056903a7b62bac46f45a3d7a70ac799ca88cb      1
    45 0xae0d16586e5d60d334624c115216a52b9b1a0335      1
    46 0xae72c6a6fad9fa9d82d089e1ebf73b3043855425      1
    47 0xb6b4a02dca517564eb98790ff67d42b5b37a3d4e      1
    48 0xb6cf25f5cf8a1e1727d988facdd47f1dfc492caf      1
    49 0xb735af7ae1a77d1ec764c862c2c09bdbf2b34b27      1
    50 0xba4575ea27041d99e6614ec02318f1e23a623fe2      1
    51 0xbba3ced54477c12fdf16d7009771affc7a8c9ba1      1
    52 0xbbdd72fcce73c2626719be00259ddffef0d5673d      1
    53 0xbd1ded3bcc8103028c8ebdc61990ca777709b10a      1
    54 0xc02e6b0d0c1a5d8cd26beeba0fe8d76c5d2f19b9      1
    55 0xc2419841dcb9a0f8906d06463ae24e00e1470846      1
    56 0xc3fd1227da579220afeb28b400dacc4ad6523c7c      1
    57 0xcaa1c396e70384db58dd33be74b26fb119e98c3a      1
    58 0xcabd5f77ca9d48f4ef9793f20de42ad39ce93979      1
    59 0xd3aefe3c531e3e2eb0689206e7d495843c943550      1
    60 0xd40b63bf04a44e43fbfe5784bcf22acaab34a180      1
    61 0xd8c39a8e2779efa2f3e8c57d9bcd18ae0bbad76c      1
    62 0xdb561a899557404581e6180fe6d4178577dc117b      1
    63 0xdf0e646edd8d5cd4c5c85edcc775d2fa1858bb70      1
    64 0xe3b41ae8785e4107cc69f988042ff4a66a367fac      1
    65 0xe7c5ef2cee9c15b04fc7dea61a1ec443dd8e7fd1      1
    66 0xeafcc7bad1afede83f065146b14066a5d04ac3d2      1
    67 0xee958e45f3464d712b8830deb5875c8ac105f698      1
    68 0xef5ab90a44b68d4f5e3f6be6af4bedb12cd2c66e      1
    69 0xf2bef831670df52ae5492dcaf6ae62aac86f6cc7      1
    70 0xf2c5f1fd977dbd6de9d04bc4e62dff722d4bb1a1      1
    71 0xfd22004806a6846ea67ad883356be810f0428793      1

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
