
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "15661916.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance           contract        
     Length:10083       Length:10083       Min.   :   1.000   Length:10083      
     Class :character   Class :character   1st Qu.:   1.000   Class :character  
     Mode  :character   Mode  :character   Median :   1.000   Mode  :character  
                                           Mean   :   1.851                     
                                           3rd Qu.:   1.000                     
                                           Max.   :4209.000                     
         name          
     Length:10083      
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

airdrop_gradient  <- pick(snapshot, contracts=c("gradient"),address_remove=address_remove, address_pick=10)

allow_gradient    <- pick(snapshot, contracts=c("gradient"), address_remove=address_remove, address_subtract=airdrop_gradient,address_max=1)
allow_scobel   <- pick(snapshot, contracts=c("ScobelEditions"), address_remove=address_remove, address_max=1)
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

    # A tibble: 10 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x22fbaa2dd20b848084545515fb04b9c846942779      1
     2 0x477ea7a022e51b6eb0dcb6d802fb5f0cfc3b4a81      1
     3 0x61d9d9cc8c3203dab7100ea79ced77587201c990      1
     4 0x670a840848eba22eb2a090fd65e1ff60c3ba9e5b      1
     5 0x6f0735bf1e6c69030d6990cdd580345b370eb50a      1
     6 0xa32f90b21d11561d31ff604745907acc77fb67e3      1
     7 0xa743c8c57c425b84cb2ed18c6b9ae3ad21629cb5      1
     8 0xe359ab04cec41ac8c62bc5016c10c749c7de5480      1
     9 0xea39c551834d07ee2ee87f1ceff843c308e089af      1
    10 0xf15a6b54e68884d27e1bebb1624d70c227b7d04b      1

## Allow

``` r
c(allow_gradient, allow_scobel, allow_raw, allow_intern, allow_singles) %>%
tally() %T>%
readr::write_csv(file="allow.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 133 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x0a98f97e89743406a8611e1b4219a073b60ffed3      2
      2 0x665654f2d2a151be2d0f8e3697e6ce780f732af2      2
      3 0x69cb3b1de24e08f1cfc2994171b6c6930498f750      2
      4 0xb5374cac6cad6b025246f19d20b0d4151b640558      2
      5 0x04df8d02f912d34fef12a1b0488ee56fd6f7416c      1
      6 0x129c6695dfe7a906bd8fda202d26dfff601f83a4      1
      7 0x145358fac8cf55a4d14548169c8424f7c0f03c95      1
      8 0x1566ae673ae80725bcce901b486c336e6acef465      1
      9 0x15ab7dd2261026db55623d0da3946a3e022be19f      1
     10 0x1725070316139c7f097095df61b9e0f3e0edb9e2      1
     11 0x1a4370fdd7173d0a41ff7c63a8e0249479ba0225      1
     12 0x1c29dcaa0cad96ca3f60d414c7e2e47c99cd7bdd      1
     13 0x20345c3a4afa7a35f917068dd3def812c367f051      1
     14 0x23fed2634fdc629c1a82eb6ed3391efad2a58670      1
     15 0x248458947c120ca057ec028b3fe7e4b3f26fdb3d      1
     16 0x28b8d4f7516a112e2e2fd462293a1c27cde327a7      1
     17 0x2924196a2ec71ea4ae1b1357381eccdcee6c18f2      1
     18 0x2be2517c818399e3faff4ae37e432f1d7917f204      1
     19 0x2c8875f34ceb219f61b7453b2c5f100ec2f6ed33      1
     20 0x2e54b594ff08bad82db35892f1265994c57bf46b      1
     21 0x32d53a21813debfcb33a2b1ff2a396bd3a06f818      1
     22 0x33aa9daac0cf59b34a1d4cf9175491b4bcbd98f2      1
     23 0x35bace30dcacfbfdd29e0bd822f8d3cfdad00d3d      1
     24 0x36799ff7bf99602870b91572e2a897b06c91c87b      1
     25 0x378bcce7235d53bbc3774bff8559191f06e6818e      1
     26 0x3852471d266d9e2222ca9fdd922bafc904dc49e5      1
     27 0x3cb63b82d778105e43f064ed739b0655f1f0fb87      1
     28 0x3f849f47f5b372d80407e442f360ad7b17f5fac4      1
     29 0x40d2f4399d23f9afb82d0a6b73055f13208614f9      1
     30 0x40e22c10962851b6ef8fd2505271491a336e826f      1
     31 0x40ebf8a762801d1706d9d16a8abfec4c452d15e5      1
     32 0x43b0bd27d5016e969193412933387b0dd4cf3e0a      1
     33 0x45855a3f4404aa08ffe14a366c75663f4ded2fac      1
     34 0x48464efe55fbcae8ae0c992b306afcf21d4910cf      1
     35 0x48fcb0e61921d8480a63d82926d3ea80cc0d94d2      1
     36 0x53006f95def268f88dc1b8216654ab56f3afd052      1
     37 0x53562e8ecee54356dc89ad263a444528b34d6c80      1
     38 0x56bdc5fe7f9752e7f5a381e394acfe72a724462b      1
     39 0x575f6540c16a72696c14a17fa64f049992d661ab      1
     40 0x590f4ca9b70860d1b89be8a7e69f22e59f6dcf6f      1
     41 0x5df5342342701b8ae5bce28f74ebb73b5fc13a54      1
     42 0x5fdb5fdb61bc2975f3be446c5ae9c5df490f55d2      1
     43 0x64f7de90dc79d775703bbec66a1591c7a26a22f0      1
     44 0x64f7e4e59a5614411a1a1675de29874a923bb3ee      1
     45 0x699990a8e7ada9e92c932d6e8fb365024fc74b43      1
     46 0x69e68074f1aada957edd39c5eae0069973343f30      1
     47 0x6d1db4a7e83dae0eee7e95d421722d46d2a7e94b      1
     48 0x6f113b0c8c7266d7514e4986e3d6aaf013b6754f      1
     49 0x71bc9751ae95d09a2dca914f47bc009d3b003fb3      1
     50 0x726022a9fe1322fa9590fb244b8164936bb00489      1
     51 0x729fcbb6e1289c88ce5113ec1c83a48a8e3c9f2f      1
     52 0x73bcb8c5e30bf85806aade7fc36f16c6b80fd3b9      1
     53 0x7546c60ae8d65dc6dd7a0f61c169818059ef49db      1
     54 0x76db02500f7631d57bc2dcdca9d4cf782b99e119      1
     55 0x7cc696eeb48dc6acee89447168302a90c116bc34      1
     56 0x7e8ea603481ad8e570efbbe43653df49ed052842      1
     57 0x80a1c9fdc26199a69d190ebc8ad287ef48758977      1
     58 0x82139687faae8a29851902783e02e699de0e0846      1
     59 0x8476b6a8aa0b4037e69e79f116f662aa0096b0c0      1
     60 0x8774be790cb9e12d5edaf2eb8a3f6c89410a497d      1
     61 0x88ac3cea7888c92f7f04db230268b9843bc64daf      1
     62 0x896b94f4f27f12369698c302e2049cae86936bbb      1
     63 0x8ba68cfe71550efc8988d81d040473709b7f9218      1
     64 0x8ea76483c888f5bda7d96cab9839488f691daf78      1
     65 0x8f8b4759dc93ca55bd6997df719f20f581f10f5c      1
     66 0x9224bbb4e0fbe2f2f8fab55debc41eb21fdfb804      1
     67 0x973ef7e4e91cc9098709d540a8ddfb708b331c87      1
     68 0x982d3c5223f6b5794fccb3208eb164d042cf2526      1
     69 0x995a657d00b8c50c7f33f1d58c2474fb0e79472f      1
     70 0x9dbd781eeba135ad2a779926880adc89196a3265      1
     71 0xa26979cacb7da6b0218f288ac13e88c07a658323      1
     72 0xa33e5e1ccf57c0caf78ae399061f270dd24ffcdf      1
     73 0xa4cb937bc5ec481fc2674794d4146206ecc71b15      1
     74 0xa71000e72d38e6a84d7190f59fd3dfc73931c0e8      1
     75 0xa7b6c6f60a7053b812653d538f0e2a80e8fb04be      1
     76 0xa7ef963a9fd90076bb16601733038d0932b92d06      1
     77 0xab2056903a7b62bac46f45a3d7a70ac799ca88cb      1
     78 0xab4273e7137f9531fcb47d558d9bab0e726e6937      1
     79 0xab55e7da2d67c0200d71c2c0149abda52087b5e2      1
     80 0xacd7869c648ccb1b3478615e194aeb045a2e905f      1
     81 0xae0d16586e5d60d334624c115216a52b9b1a0335      1
     82 0xae72c6a6fad9fa9d82d089e1ebf73b3043855425      1
     83 0xb6a3fe2ef0809e1d9a71f2ef749a0d3c721fb607      1
     84 0xb6b4a02dca517564eb98790ff67d42b5b37a3d4e      1
     85 0xb6cf25f5cf8a1e1727d988facdd47f1dfc492caf      1
     86 0xb735af7ae1a77d1ec764c862c2c09bdbf2b34b27      1
     87 0xba4575ea27041d99e6614ec02318f1e23a623fe2      1
     88 0xbba3ced54477c12fdf16d7009771affc7a8c9ba1      1
     89 0xbbc37f68e9876d64b2c55016081528ae0a85d8b2      1
     90 0xbbdd72fcce73c2626719be00259ddffef0d5673d      1
     91 0xbd1ded3bcc8103028c8ebdc61990ca777709b10a      1
     92 0xc02e6b0d0c1a5d8cd26beeba0fe8d76c5d2f19b9      1
     93 0xc142dc5ceb96f73635d66b62bfde2a0eea4e4ce1      1
     94 0xc2419841dcb9a0f8906d06463ae24e00e1470846      1
     95 0xc26012491b9dfb2e6f2cb0305673e212721d5950      1
     96 0xc3fd1227da579220afeb28b400dacc4ad6523c7c      1
     97 0xc46db2d89327d4c41eb81c43ed5e3dff111f9a8f      1
     98 0xc762b1081c56b3fa487c7372f7284d9558a84859      1
     99 0xcaa1c396e70384db58dd33be74b26fb119e98c3a      1
    100 0xcabd5f77ca9d48f4ef9793f20de42ad39ce93979      1
    101 0xce90a7949bb78892f159f428d0dc23a8e3584d75      1
    102 0xd3aefe3c531e3e2eb0689206e7d495843c943550      1
    103 0xd3e401814d1faa8ca0419ecccbfee93ac7b15b31      1
    104 0xd40b63bf04a44e43fbfe5784bcf22acaab34a180      1
    105 0xd7342ea20a5afbf24352b5ca61e09844167914cb      1
    106 0xd73e55a3f739fbd783f7a2a307831afc31c6510b      1
    107 0xd8c39a8e2779efa2f3e8c57d9bcd18ae0bbad76c      1
    108 0xdb561a899557404581e6180fe6d4178577dc117b      1
    109 0xddc9520acb5d1923412674a4ce07bb2e32ff0ac7      1
    110 0xdf0e646edd8d5cd4c5c85edcc775d2fa1858bb70      1
    111 0xdf1dd8f308bd9116f5eb03157bca7ea311010974      1
    112 0xe317eb46da9d27aa3493b03ea0468ffd37ccc2e1      1
    113 0xe3b41ae8785e4107cc69f988042ff4a66a367fac      1
    114 0xe5798a530bb7105e148d38ac884f05c28ed8e804      1
    115 0xe6ba7ce1bbd7b811d891c2d4c09d7295bb96752c      1
    116 0xe70788146a23994b6b5a66eae2e88903ec0edb22      1
    117 0xe7c5ef2cee9c15b04fc7dea61a1ec443dd8e7fd1      1
    118 0xe874ba46982d7cbab5931a50c2dc81a7aeb80344      1
    119 0xeafcc7bad1afede83f065146b14066a5d04ac3d2      1
    120 0xeb8661e8548dc074b9ff411080fdd9a377e6ed1e      1
    121 0xed9788430cd53dd218c80063b2b788a99d95065d      1
    122 0xee2c055f7706b9dfcd98cd5a23d5629d6316c0bd      1
    123 0xee958e45f3464d712b8830deb5875c8ac105f698      1
    124 0xef5ab90a44b68d4f5e3f6be6af4bedb12cd2c66e      1
    125 0xf0289380edbff3ef7c5117602b3fdaed1c3d8bd3      1
    126 0xf041c4f026547f7fbea6904f77bea16997024751      1
    127 0xf0d6999725115e3ead3d927eb3329d63afaec09b      1
    128 0xf19a3eb92be971d3f314234913e0597815f3f958      1
    129 0xf1e1c701b49b1dc2405a9e8ef40f9f88802b80fa      1
    130 0xf2bef831670df52ae5492dcaf6ae62aac86f6cc7      1
    131 0xf2c5f1fd977dbd6de9d04bc4e62dff722d4bb1a1      1
    132 0xf39e356b26630115dd171f1d41640c45cec9ab21      1
    133 0xfd22004806a6846ea67ad883356be810f0428793      1

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
