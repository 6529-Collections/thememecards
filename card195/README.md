
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance         contract        
     Length:181         Length:181         Min.   : 1.000   Length:181        
     Class :character   Class :character   1st Qu.: 1.000   Class :character  
     Mode  :character   Mode  :character   Median : 1.000   Mode  :character  
                                           Mean   : 1.055                     
                                           3rd Qu.: 1.000                     
                                           Max.   :11.000                     
         name          
     Length:181        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 19096269 # https://etherscan.io/block/19096269
block_hash <- "0x7852999869b47b41f455f98a0749adfea09a27ebedfa29be9bd08b11b744e65c"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4736 

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

allow_artist1    <- pick(snapshot, contracts=c("SuperRare","Foundation","SleeplessNights","SYNTHETICA","LOUISDAZY","Silhouettes","Ledger","CelluloidDreams","NeonLife"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("ModernRomanceEditions","FutureNostalgiaEditions","DigitalBathEditions","DejaVuEditions","ParadisBleuEditions","KinesomaniaEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("SuperRare","Foundation","SleeplessNights","SYNTHETICA","LOUISDAZY","Silhouettes","Ledger","CelluloidDreams","NeonLife","ModernRomanceEditions","FutureNostalgiaEditions","DigitalBathEditions","DejaVuEditions","ParadisBleuEditions","KinesomaniaEditions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 66 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0199f4bef64212be9d5847557631ccea02ae8d26      1
     2 0x06ef754ca26ea63c508805e4b840c3ff5ebe60e2      1
     3 0x0dfd32320af10f58cd4a5c2b567b7739ea2d691c      1
     4 0x1b591e98dc0ed3f967359242064c76161fb4c0dc      1
     5 0x1de4d49959eafa01ab3706aa20707a3f1dbe2424      1
     6 0x1ec396ac5964b69c394d2f97f91908e15b3a046f      1
     7 0x1f073f77985b63d9267f51d33e7c0b61335b939c      1
     8 0x234f4e58df75cae6ef10c4635a22ba5b52dd561d      1
     9 0x2c16bdacf891061402ca95f54cb8ec5c816c5648      1
    10 0x2f63885741bcda3e66a2f5fadeeb0ce7b49c25c7      1
    11 0x34978faf3a9f469da7248d1365ddf69ac099588c      1
    12 0x371c4566e886ddff10a888a9e5f26a2bac13cfa8      1
    13 0x3807155076b24d98f4e716a968416dc5d7c11b7a      1
    14 0x3852471d266d9e2222ca9fdd922bafc904dc49e5      1
    15 0x3b0aa499cc6acde1d4a7433da6968d7bb8bd8509      1
    16 0x3d4dcea0a12087523b36335fd3593df47b5083e8      1
    17 0x402dfd31837bfac49a73596a717642d48f220038      1
    18 0x40d775827365ae4d54cbc08a1a1c4f586b2c1d0a      1
    19 0x49c1c6683a54d962c8f2d9d36d05ef107e9033ea      1
    20 0x4ef33f32c39796ae02c3ec21656ace134bab73e9      1
    21 0x55bf3dce2aec2282b8d36c80eaf96e14cff38ec3      1
    22 0x6112083d4bb12dbf64e4850842685aaefbeacb10      1
    23 0x61a11429b1b771c89fb037bd8a35ed64a5f7554d      1
    24 0x659cf4e68a97e20c9e9bd76fae9a7568e101d49f      1
    25 0x665654f2d2a151be2d0f8e3697e6ce780f732af2      1
    26 0x698f3eaf3defee3c5a00b64bd65feee9015d6970      1
    27 0x6b4b60cd239b5b7947c98bb2b9bbd01db5bdf44c      1
    28 0x6f398e7872ac5f75121186678c4e6e015e83c49f      1
    29 0x70b26a35f8f308ef8286798c33b4f7a1811c7630      1
    30 0x773a3a5c2e232024d77f531b623178985ad9e472      1
    31 0x7972c025ff9e467ecb2547d7d474081441522daa      1
    32 0x7ce30498e564f081ca65a226f44b1751f93a0f82      1
    33 0x7dc35b473b6ff652c1c437996244a8d14d026252      1
    34 0x803e96717d25ed306160d35ab0368dd595d0efb6      1
    35 0x84542c321c0eac0cee1f44eb386ece2a53a81950      1
    36 0x8497277c9339170a0420e86da8352e0c084624cd      1
    37 0x8f5656023d8e9b5dadbc4316e6f2d0c450a7053f      1
    38 0x909d3c000baa83a64811ec0f41da55ec0ebb8a67      1
    39 0x92e4c49cd297ce4e0274a85aa168f6b8f1267cae      1
    40 0x9499054d02a725316d61fa896c29d58550ee4a5b      1
    41 0x973231cf471f768f0939042e0591da1b15f4799f      1
    42 0x9752669abd5b7c5a3224bf9a3049d7961d25031a      1
    43 0xa43d0698b2afbe80c4080b12f1dc4648dfd0fea7      1
    44 0xa589b73fe983998c3040dd9f53048a807c210df9      1
    45 0xb00b18b3bf6ecc43e6235ee69424d4a220437a4d      1
    46 0xb499f502d303ff7c3d830ffd85e84b76cc616703      1
    47 0xba7cfbd459dfa75ddbb9901c661804d06fd4dbac      1
    48 0xba9c57d2426e593526ec597e4016fa5ef0e97e38      1
    49 0xbe704a53914574d634d2fba0a552beb476d1ed52      1
    50 0xc68c7771ec6a6e5d67d62aa9c6f22df69865e401      1
    51 0xc8fe8f3a13339aa7e8e64d301627a5117c308d2e      1
    52 0xcbfb9a917c2b45fbd8e820e66602ef26dff9cd40      1
    53 0xcd25be33a31263b76c30953509058bc429e20060      1
    54 0xd5e437b161b71a8df90f7ca488e095425d285edc      1
    55 0xda834e5743899eea822f877d3dbf1dc18489583f      1
    56 0xe104c24a313cd88f46dc8621744edad2a008d6bc      1
    57 0xe328318c037754ef86ffdde28d4e2acbd8da9b6a      1
    58 0xe96ba1a10f72b58934b9ac0e3fb471d2ba65b757      1
    59 0xe9d41a1b8613926cb1851c5855d27a1ea32111d0      1
    60 0xebced2d753d655e321af6ac3de9c4e1ebfaab435      1
    61 0xed83f0bceef992fa58c8bc8a27269eedb3574922      1
    62 0xf30eb1dc8408a6c92cda6a49f1909409fe572095      1
    63 0xf3290df945e484a4ff846fb5baeb6d1147a7aa69      1
    64 0xf46ac3da4a9a3e66a21496e79dfd93355533221c      1
    65 0xfa4e98a3973ac6d51918e69d49f749219518c5a5      1
    66 0xfadc0561b0f9ffe49a82c4a539cc492e3fcea1c5      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 77 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0351daf1c529980895d57be8b2c641be979b468b      1
     2 0x077bfc14dd6725f260e1abfd5c942ee13a27091b      1
     3 0x0b80905e4765cbad15cb12e79e7e7b634b26c36e      1
     4 0x1134528d239cf419f9081ceebc3430ac191c318f      1
     5 0x11674c2ebbb09e3c007492502027db7d92b3e1c7      1
     6 0x14739b2b9ca2dbfe90dd86085eb958940ad0221c      1
     7 0x1aae1bc09e1785c8cb00650db0c6015bd73b0c7e      1
     8 0x1dfab033cf5fe059fe8c5f91c68149a12cd53a6d      1
     9 0x1e2fa044a25bca1576ed94562db50f51dcc24ca8      1
    10 0x1e8e749b2b578e181ca01962e9448006772b24a2      1
    11 0x254f18b3d2bfeae6931c2432c6dd34fca16cb954      1
    12 0x2719dfd8c9f9bbf984290b54b56bfedcf1d73877      1
    13 0x27d57cd15feb468471dbe57ab6288d73980f2d29      1
    14 0x28b5215f2c5a9dbafdcbbe4aa1e8d06077a25339      1
    15 0x2cef54ba120cdbdc1ca07381e5b4747b587e9261      1
    16 0x3098c04b75c7fecba9c7a80fc24c097995e95681      1
    17 0x321ed5739440bdbd731d54a19aa20b18398d374f      1
    18 0x35ca14edabb3cc0d3fa01808dd9ab5deeb59b63a      1
    19 0x3976196e7f8569ba7994e79889ee2e8d33efd889      1
    20 0x3c75164a475c5ea67e8b63ead6b79f8f3ec46648      1
    21 0x3e06b157a7c6fdfaea072d54cd93307f48020cc4      1
    22 0x3e1f1fc109b0130a8ba13a1546c713d030b8613f      1
    23 0x43d4bdeeca8dd891e89ed80dac83b70bf2d1d753      1
    24 0x44ed32b27aebf445c209dab3ea7e3a9d50f37b2a      1
    25 0x45f2e08b35a959a87833c57c4bacbf58f95eefb7      1
    26 0x4949b46633b810bdd745b028062b30f6b647ec60      1
    27 0x4dad4d0c41c241e47c4b9c6645e56bb01dafa9c5      1
    28 0x552a0a09e46daf0eb7f18eaea50120ed4eebbbd8      1
    29 0x56f70a0bbfefa9db41f024198c7185107f42bac2      1
    30 0x62d3c2a80d3cce916b9b645cbcda14d35418275c      1
    31 0x6472a9ef69b4e4109eb1c561ff91c5dce3925489      1
    32 0x67f8a0269a8cb8b90c0dddc2e486beb6185e26c7      1
    33 0x6954ee7ede83ca555663704fe7e2b1528ff2c619      1
    34 0x6a3c1c2142e56307abda07b6bbc9b60f3c2ddd16      1
    35 0x6d66a6fe76df6418136e9e6f7af9fd7e76e52205      1
    36 0x6e22fcedff1e77c33986e9cc32889315c2799b5b      1
    37 0x6e672528f7abc31bcec8bcf16c31aba3abebdad3      1
    38 0x732498016b8c74cd11c3fb268b980314b3c94fe0      1
    39 0x73e4a2b60cf48e8baf2b777e175a5b1e4d0c2d8f      1
    40 0x74db837cfe2971aca09896f65bd9beaa8b968521      1
    41 0x78dacf90cfaa7b14e5f6560af7527a6bdae202ca      1
    42 0x791ffea64feb5de305d354237940f185723600db      1
    43 0x8df50ed0e72d90b5b86eeedb12b30f517b8cb04b      1
    44 0x8ee376de530fb9a734df676e7e4342b48355f483      1
    45 0x9eef879d954d091a5b1e6c1f72be45cd9519a14a      1
    46 0xa837c3d5fdfe4e878a3f8370df89cfd972b116e1      1
    47 0xa96d4549729c2a826237d91b3a3700cad7dfec4a      1
    48 0xabd9643f92e771831a6dcd588844a75b03b0ac12      1
    49 0xac06d32f100d50ca67f290307ec590443d639c8e      1
    50 0xafc068731db6f23f8c72872170c5100ee61b05ff      1
    51 0xb98d10d9f6d07ba283bfd21b2dfec050f9ae282a      1
    52 0xbe39ad6d10802b6ac0943eab3e6c6b1884a054c4      1
    53 0xbeffddcf2e84106f77c2b60445dc257d65e19a26      1
    54 0xbfacde4341f32b5f77ad81831e128ede4b5e6073      1
    55 0xc196eb8d5a08fb6ce0297b2748e18137f2b431fc      1
    56 0xcafb98282f5ae4aa9083e031981e980cff1d9a79      1
    57 0xcd90c980b25eaabace2a760b364ea5cccb95e2b4      1
    58 0xced183f3efe63c054f36bc2d701cfe4631e61f1a      1
    59 0xd81ce8e89dd987c8ab630858c8f1e9df14788c35      1
    60 0xd88c17aad286da44d7216f487b9a65ce1f7d4581      1
    61 0xde3975723d5de76070ca2169ab8716e48b3b4b81      1
    62 0xdee95e3bd880cae7e98961da3a127c8b572eb182      1
    63 0xe052113bd7d7700d623414a0a4585bcae754e9d5      1
    64 0xe0a703e24de2f2510fb3a8e2208b7383758f03b3      1
    65 0xe517f5274a21c74ae230d99010ad11992d0e836b      1
    66 0xe57cc5fc1415d55d3bbdd00562bd610ec35ad1fe      1
    67 0xec9e512fe7e90134d8ca7295329ccb0a57c91ecb      1
    68 0xee193f18cc3549e1158e4b940f065d8f460c143e      1
    69 0xef6cee1a956ee22ccef71b80696cb07f934df547      1
    70 0xf1b5945f25a3aca16853241eec3f4856a60c1e96      1
    71 0xf1e1c701b49b1dc2405a9e8ef40f9f88802b80fa      1
    72 0xf40fd88ac59a206d009a07f8c09828a01e2acc0d      1
    73 0xf511d47077e4090402426c90d0de5274f0f96b1a      1
    74 0xf54fe7f5f0f8d13ce9b685b8ad167b466b637f0d      1
    75 0xf70ebfd828b993e2863670530c2ec62c049f37ad      1
    76 0xfb44496ef7ce7dc94da1ec9f45115c80bba1778d      1
    77 0xff9911abdbe9d1f7d1a19595b93905c2a9ad60f4      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 143 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x0199f4bef64212be9d5847557631ccea02ae8d26      1
      2 0x0351daf1c529980895d57be8b2c641be979b468b      1
      3 0x06ef754ca26ea63c508805e4b840c3ff5ebe60e2      1
      4 0x077bfc14dd6725f260e1abfd5c942ee13a27091b      1
      5 0x0b80905e4765cbad15cb12e79e7e7b634b26c36e      1
      6 0x0dfd32320af10f58cd4a5c2b567b7739ea2d691c      1
      7 0x1134528d239cf419f9081ceebc3430ac191c318f      1
      8 0x11674c2ebbb09e3c007492502027db7d92b3e1c7      1
      9 0x14739b2b9ca2dbfe90dd86085eb958940ad0221c      1
     10 0x1aae1bc09e1785c8cb00650db0c6015bd73b0c7e      1
     11 0x1b591e98dc0ed3f967359242064c76161fb4c0dc      1
     12 0x1de4d49959eafa01ab3706aa20707a3f1dbe2424      1
     13 0x1dfab033cf5fe059fe8c5f91c68149a12cd53a6d      1
     14 0x1e2fa044a25bca1576ed94562db50f51dcc24ca8      1
     15 0x1e8e749b2b578e181ca01962e9448006772b24a2      1
     16 0x1ec396ac5964b69c394d2f97f91908e15b3a046f      1
     17 0x1f073f77985b63d9267f51d33e7c0b61335b939c      1
     18 0x234f4e58df75cae6ef10c4635a22ba5b52dd561d      1
     19 0x254f18b3d2bfeae6931c2432c6dd34fca16cb954      1
     20 0x2719dfd8c9f9bbf984290b54b56bfedcf1d73877      1
     21 0x27d57cd15feb468471dbe57ab6288d73980f2d29      1
     22 0x28b5215f2c5a9dbafdcbbe4aa1e8d06077a25339      1
     23 0x2c16bdacf891061402ca95f54cb8ec5c816c5648      1
     24 0x2cef54ba120cdbdc1ca07381e5b4747b587e9261      1
     25 0x2f63885741bcda3e66a2f5fadeeb0ce7b49c25c7      1
     26 0x3098c04b75c7fecba9c7a80fc24c097995e95681      1
     27 0x321ed5739440bdbd731d54a19aa20b18398d374f      1
     28 0x34978faf3a9f469da7248d1365ddf69ac099588c      1
     29 0x35ca14edabb3cc0d3fa01808dd9ab5deeb59b63a      1
     30 0x371c4566e886ddff10a888a9e5f26a2bac13cfa8      1
     31 0x3807155076b24d98f4e716a968416dc5d7c11b7a      1
     32 0x3852471d266d9e2222ca9fdd922bafc904dc49e5      1
     33 0x3976196e7f8569ba7994e79889ee2e8d33efd889      1
     34 0x3b0aa499cc6acde1d4a7433da6968d7bb8bd8509      1
     35 0x3c75164a475c5ea67e8b63ead6b79f8f3ec46648      1
     36 0x3d4dcea0a12087523b36335fd3593df47b5083e8      1
     37 0x3e06b157a7c6fdfaea072d54cd93307f48020cc4      1
     38 0x3e1f1fc109b0130a8ba13a1546c713d030b8613f      1
     39 0x402dfd31837bfac49a73596a717642d48f220038      1
     40 0x40d775827365ae4d54cbc08a1a1c4f586b2c1d0a      1
     41 0x43d4bdeeca8dd891e89ed80dac83b70bf2d1d753      1
     42 0x44ed32b27aebf445c209dab3ea7e3a9d50f37b2a      1
     43 0x45f2e08b35a959a87833c57c4bacbf58f95eefb7      1
     44 0x4949b46633b810bdd745b028062b30f6b647ec60      1
     45 0x49c1c6683a54d962c8f2d9d36d05ef107e9033ea      1
     46 0x4dad4d0c41c241e47c4b9c6645e56bb01dafa9c5      1
     47 0x4ef33f32c39796ae02c3ec21656ace134bab73e9      1
     48 0x552a0a09e46daf0eb7f18eaea50120ed4eebbbd8      1
     49 0x55bf3dce2aec2282b8d36c80eaf96e14cff38ec3      1
     50 0x56f70a0bbfefa9db41f024198c7185107f42bac2      1
     51 0x6112083d4bb12dbf64e4850842685aaefbeacb10      1
     52 0x61a11429b1b771c89fb037bd8a35ed64a5f7554d      1
     53 0x62d3c2a80d3cce916b9b645cbcda14d35418275c      1
     54 0x6472a9ef69b4e4109eb1c561ff91c5dce3925489      1
     55 0x659cf4e68a97e20c9e9bd76fae9a7568e101d49f      1
     56 0x665654f2d2a151be2d0f8e3697e6ce780f732af2      1
     57 0x67f8a0269a8cb8b90c0dddc2e486beb6185e26c7      1
     58 0x6954ee7ede83ca555663704fe7e2b1528ff2c619      1
     59 0x698f3eaf3defee3c5a00b64bd65feee9015d6970      1
     60 0x6a3c1c2142e56307abda07b6bbc9b60f3c2ddd16      1
     61 0x6b4b60cd239b5b7947c98bb2b9bbd01db5bdf44c      1
     62 0x6d66a6fe76df6418136e9e6f7af9fd7e76e52205      1
     63 0x6e22fcedff1e77c33986e9cc32889315c2799b5b      1
     64 0x6e672528f7abc31bcec8bcf16c31aba3abebdad3      1
     65 0x6f398e7872ac5f75121186678c4e6e015e83c49f      1
     66 0x70b26a35f8f308ef8286798c33b4f7a1811c7630      1
     67 0x732498016b8c74cd11c3fb268b980314b3c94fe0      1
     68 0x73e4a2b60cf48e8baf2b777e175a5b1e4d0c2d8f      1
     69 0x74db837cfe2971aca09896f65bd9beaa8b968521      1
     70 0x773a3a5c2e232024d77f531b623178985ad9e472      1
     71 0x78dacf90cfaa7b14e5f6560af7527a6bdae202ca      1
     72 0x791ffea64feb5de305d354237940f185723600db      1
     73 0x7972c025ff9e467ecb2547d7d474081441522daa      1
     74 0x7ce30498e564f081ca65a226f44b1751f93a0f82      1
     75 0x7dc35b473b6ff652c1c437996244a8d14d026252      1
     76 0x803e96717d25ed306160d35ab0368dd595d0efb6      1
     77 0x84542c321c0eac0cee1f44eb386ece2a53a81950      1
     78 0x8497277c9339170a0420e86da8352e0c084624cd      1
     79 0x8df50ed0e72d90b5b86eeedb12b30f517b8cb04b      1
     80 0x8ee376de530fb9a734df676e7e4342b48355f483      1
     81 0x8f5656023d8e9b5dadbc4316e6f2d0c450a7053f      1
     82 0x909d3c000baa83a64811ec0f41da55ec0ebb8a67      1
     83 0x92e4c49cd297ce4e0274a85aa168f6b8f1267cae      1
     84 0x9499054d02a725316d61fa896c29d58550ee4a5b      1
     85 0x973231cf471f768f0939042e0591da1b15f4799f      1
     86 0x9752669abd5b7c5a3224bf9a3049d7961d25031a      1
     87 0x9eef879d954d091a5b1e6c1f72be45cd9519a14a      1
     88 0xa43d0698b2afbe80c4080b12f1dc4648dfd0fea7      1
     89 0xa589b73fe983998c3040dd9f53048a807c210df9      1
     90 0xa837c3d5fdfe4e878a3f8370df89cfd972b116e1      1
     91 0xa96d4549729c2a826237d91b3a3700cad7dfec4a      1
     92 0xabd9643f92e771831a6dcd588844a75b03b0ac12      1
     93 0xac06d32f100d50ca67f290307ec590443d639c8e      1
     94 0xafc068731db6f23f8c72872170c5100ee61b05ff      1
     95 0xb00b18b3bf6ecc43e6235ee69424d4a220437a4d      1
     96 0xb499f502d303ff7c3d830ffd85e84b76cc616703      1
     97 0xb98d10d9f6d07ba283bfd21b2dfec050f9ae282a      1
     98 0xba7cfbd459dfa75ddbb9901c661804d06fd4dbac      1
     99 0xba9c57d2426e593526ec597e4016fa5ef0e97e38      1
    100 0xbe39ad6d10802b6ac0943eab3e6c6b1884a054c4      1
    101 0xbe704a53914574d634d2fba0a552beb476d1ed52      1
    102 0xbeffddcf2e84106f77c2b60445dc257d65e19a26      1
    103 0xbfacde4341f32b5f77ad81831e128ede4b5e6073      1
    104 0xc196eb8d5a08fb6ce0297b2748e18137f2b431fc      1
    105 0xc68c7771ec6a6e5d67d62aa9c6f22df69865e401      1
    106 0xc8fe8f3a13339aa7e8e64d301627a5117c308d2e      1
    107 0xcafb98282f5ae4aa9083e031981e980cff1d9a79      1
    108 0xcbfb9a917c2b45fbd8e820e66602ef26dff9cd40      1
    109 0xcd25be33a31263b76c30953509058bc429e20060      1
    110 0xcd90c980b25eaabace2a760b364ea5cccb95e2b4      1
    111 0xced183f3efe63c054f36bc2d701cfe4631e61f1a      1
    112 0xd5e437b161b71a8df90f7ca488e095425d285edc      1
    113 0xd81ce8e89dd987c8ab630858c8f1e9df14788c35      1
    114 0xd88c17aad286da44d7216f487b9a65ce1f7d4581      1
    115 0xda834e5743899eea822f877d3dbf1dc18489583f      1
    116 0xde3975723d5de76070ca2169ab8716e48b3b4b81      1
    117 0xdee95e3bd880cae7e98961da3a127c8b572eb182      1
    118 0xe052113bd7d7700d623414a0a4585bcae754e9d5      1
    119 0xe0a703e24de2f2510fb3a8e2208b7383758f03b3      1
    120 0xe104c24a313cd88f46dc8621744edad2a008d6bc      1
    121 0xe328318c037754ef86ffdde28d4e2acbd8da9b6a      1
    122 0xe517f5274a21c74ae230d99010ad11992d0e836b      1
    123 0xe57cc5fc1415d55d3bbdd00562bd610ec35ad1fe      1
    124 0xe96ba1a10f72b58934b9ac0e3fb471d2ba65b757      1
    125 0xe9d41a1b8613926cb1851c5855d27a1ea32111d0      1
    126 0xebced2d753d655e321af6ac3de9c4e1ebfaab435      1
    127 0xec9e512fe7e90134d8ca7295329ccb0a57c91ecb      1
    128 0xed83f0bceef992fa58c8bc8a27269eedb3574922      1
    129 0xee193f18cc3549e1158e4b940f065d8f460c143e      1
    130 0xef6cee1a956ee22ccef71b80696cb07f934df547      1
    131 0xf1b5945f25a3aca16853241eec3f4856a60c1e96      1
    132 0xf1e1c701b49b1dc2405a9e8ef40f9f88802b80fa      1
    133 0xf30eb1dc8408a6c92cda6a49f1909409fe572095      1
    134 0xf3290df945e484a4ff846fb5baeb6d1147a7aa69      1
    135 0xf40fd88ac59a206d009a07f8c09828a01e2acc0d      1
    136 0xf46ac3da4a9a3e66a21496e79dfd93355533221c      1
    137 0xf511d47077e4090402426c90d0de5274f0f96b1a      1
    138 0xf54fe7f5f0f8d13ce9b685b8ad167b466b637f0d      1
    139 0xf70ebfd828b993e2863670530c2ec62c049f37ad      1
    140 0xfa4e98a3973ac6d51918e69d49f749219518c5a5      1
    141 0xfadc0561b0f9ffe49a82c4a539cc492e3fcea1c5      1
    142 0xfb44496ef7ce7dc94da1ec9f45115c80bba1778d      1
    143 0xff9911abdbe9d1f7d1a19595b93905c2a9ad60f4      1

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
