
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance         contract        
     Length:357         Length:357         Min.   : 1.000   Length:357        
     Class :character   Class :character   1st Qu.: 1.000   Class :character  
     Mode  :character   Mode  :character   Median : 1.000   Mode  :character  
                                           Mean   : 1.342                     
                                           3rd Qu.: 1.000                     
                                           Max.   :20.000                     
         name          
     Length:357        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 20495569 # https://etherscan.io/block/20495569
block_hash <- "0x61ae406d56b85b0fc03017833c0979e04df2445a6a251fefbd6347aaabe583b5"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4635 

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
  "0x000000000000000000000000000000000000dead",
  "0xA19193B6Bd97798695097e71EAb6d310F99f1955",
  "0x0000000000000000000000000000000000000000"
)

hodlers_remove <- c(
  ""
)

allow_artist1    <- pick(snapshot, contracts=c("SuperRare","r0yart","Foundation","Physicals","INSOLITUS","Oracles","Reflection","WoWGoddesses","Bloomwithgrace","MidnightDreamz"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("r0yartEditions","PeacesOfMineEditions","WOMENLIFEFREEDOMEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("SuperRare","r0yart","Foundation","Physicals","INSOLITUS","Oracles","Reflection","WoWGoddesses","Bloomwithgrace","MidnightDreamz","r0yartEditions","PeacesOfMineEditions","WOMENLIFEFREEDOMEditions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 101 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x0c98d05a2e6581a36f767cec356bae4e1d95370c      1
      2 0x100417342cb70538abc22d70c6da258bc666b661      1
      3 0x107c9a79c6b9b199ffaa620a47800fe2291c0d92      1
      4 0x13c942e3f8be4faf966ef03e6038b33d000db22f      1
      5 0x14229c6e20f4b5c30b88f93a59711cf435dd2020      1
      6 0x18456ce597e8469472dcb76e9144a151681cdc52      1
      7 0x1e299dc52eb9fdab6a6849f9731a948d8d72e474      1
      8 0x1f2a9082a7692ad4f1049b573313c52e49400137      1
      9 0x2a632f56f9bdf7b66be80768e89631f3febeed24      1
     10 0x2b2c3544cbd93aeed917e0a017f3387709b4c679      1
     11 0x30d826b6338cc6bdabd36a10fd85de81974a2474      1
     12 0x330a7cb2418a2856cf8ec47c2606900fd1a57bed      1
     13 0x34805e6a3796fb04e82183667a78c2f7bff29170      1
     14 0x3580480dca8f6c7af673b75bb05ae780d2345051      1
     15 0x37009f9ae9dcc0a6d59204e1aeaf9da4968caec8      1
     16 0x3ec1a0f0923a25dff523db494125cb77e86d85d9      1
     17 0x3ef5f421e155ed29e6fa815fd875bfcd5f22ecd1      1
     18 0x3fc6c595a3445d56bf5d8322a392f3e027197c6a      1
     19 0x4187ddd9340689c8e4a124b7532fca0a13eb9511      1
     20 0x4385ff4b76d8a7fa8075ed1ee27c82ffe0951456      1
     21 0x46c3adc31f3c8b8ee11b6468b7c31a21a2564611      1
     22 0x48a591f904d0266d32d3ae188504e2a7910b703e      1
     23 0x4b56b81041b12952d68c25b0fc0b541cbdfefe7b      1
     24 0x4d9beb5a48378acc8619e1cc8ee6a1af53379ae4      1
     25 0x4f0a9445b9b2d5a2c27814cdd92cf6da0e42dff8      1
     26 0x5019fbc77879226a8ff3f37d57401081c7aa8957      1
     27 0x511531656cb4bea59d62f3b1f3bb8b1d40eebef9      1
     28 0x511876aace51210752eab85e6905f05b44e319aa      1
     29 0x549266c062531ec5fd3979b69d10f0720dce3360      1
     30 0x54966b7b70eba0a6bd5f5ab5e292f266e2f89c89      1
     31 0x5a8116f07937394e6ac2a51995ab6a054c08bf9e      1
     32 0x5b3319297c17a0f1bc751e5d43a52ffb3e5356a8      1
     33 0x5df25d805ef73c8b679dfce920e1414eb28fd41e      1
     34 0x62ac7073454f5b8cd65558711161fc9f3436f76f      1
     35 0x62c5ee7bcc0e336bc58fc2946bb9671ab2878f7c      1
     36 0x648a58121dc0de4436837dc585ded4fa5fba6d3e      1
     37 0x64ed28868990b8440bf2de0c62747a7a13393550      1
     38 0x655a7ba052e5acde220a482777981c050fa087fe      1
     39 0x689a19f57077f682a1d7cc19d9066f1a834721a2      1
     40 0x698b7dcb16b624f104b37a2578acaede89f37fc9      1
     41 0x6a08986aad2967bd659457472fefd9bcbd417316      1
     42 0x6a509096e7ebf95acd9c8b43d604ce634997462d      1
     43 0x6acbd57b033ff316c4c93502d39e9469698e6b73      1
     44 0x6b11d13822acd45aa71c9810816b71e0078dfd3f      1
     45 0x6b4b60cd239b5b7947c98bb2b9bbd01db5bdf44c      1
     46 0x6d2f18ee7aa579627fc41f4ebd583c38a45c83be      1
     47 0x7304689aac83c3b236332b0c233878f1819ca89d      1
     48 0x75192b575ed6d49780ec43f669657f3b25e73177      1
     49 0x77037050bb27ae8f09118a0c480224d897589c65      1
     50 0x793e48857f3ccdfe5cf3c504b6dfc7e8dab5b0e1      1
     51 0x7ab977f469496930f43c7918022a0915f0bd9de4      1
     52 0x7c0ca862b604c61a7b7e78647ad95563f60956c2      1
     53 0x7dcf4cfe37d08f37631193fa81dbc282e4c44da1      1
     54 0x819dfc1b757c0ed67fea97e33e70cc4cc640f99d      1
     55 0x8352b250c643d3be06e1a8bc7b9dcd67b704fe56      1
     56 0x87accabbdedce63379af9d246bb979a3a481534c      1
     57 0x89c8e066958ef12ffab46854db59b4138796c3a9      1
     58 0x8a2a8f2bd05bd04e2ab24e903f4b24e199906297      1
     59 0x8da476c64056bcc7b9be720387804c48666c1855      1
     60 0x90198c48129ac7564933f8bab85669e303ff6b9f      1
     61 0x931d0dcc21410c0410faafc6b7913c59e948e020      1
     62 0x9d30ca11c4a2fa4479ca14710c60a3bd4c1ca2f1      1
     63 0x9e8fdfc3d41b26a2870c293dc7e1be8675fa8b7b      1
     64 0xa06e3eca46df7e71238ebd04fa627bffc7d3ebde      1
     65 0xa1fa037a1fc2552efeeb0fcc1740f90916dc57b9      1
     66 0xa22414e4af5767448624017c7e0151d22490412b      1
     67 0xa25dd72698a703dcbb906cce432cba0586e04fd0      1
     68 0xa3ec0ea98a1a12c56a75e214a8972809e7594362      1
     69 0xa46664ba7222221475146c6710c812741a6c8bf5      1
     70 0xa4e9a636d9ec9472c3ebb1430266b634c4ac114f      1
     71 0xab4273e7137f9531fcb47d558d9bab0e726e6937      1
     72 0xab6ca2017548a170699890214bfd66583a0c1754      1
     73 0xb326e5db8e4e9f3a27c08e2ccac2ad9783acf310      1
     74 0xb5d9317265f775987a84e38bab0cfd8626135cf7      1
     75 0xb6bc60ce81c839c09b11e859eedd805f1db8d148      1
     76 0xbd8dc54a09d39e753d7078c30bd3f424585bf83c      1
     77 0xcb2c2cc869f8836bfd07e3f71376b35510522c59      1
     78 0xd20ce27f650598c2d790714b4f6a7222b8ddce22      1
     79 0xd263f15e76e3318f9b55fa5a8db2dbd9affdfb3d      1
     80 0xd81b26630d2fbd812af71ec88424599ce1950892      1
     81 0xd9270fd770b3e06f7b4a7452f531f035826a2eac      1
     82 0xdb25085597e4c774e2bbb02207fe52d1c7d0c1e4      1
     83 0xe1994b2fd8310218a06875b4b3d7cbca8ec83e4c      1
     84 0xe37c3eb4f67d318503faa5f84e20c49ce59b9850      1
     85 0xe42108b5ae543c7c832a29aed64add46b7f0ff54      1
     86 0xe4bef15a0d7d5a889ac0ad7c86d9ca1c4b5d18e6      1
     87 0xe597d8a65604b109510a6bdf3730e23d22e61482      1
     88 0xe679d21696f2d833e1d92cf44f88a78e796756a3      1
     89 0xe76091f84ddf27f9e773ca8bd2090830943f615c      1
     90 0xeb43b5597e3bde0b0c03ee6731ba7c0247e1581e      1
     91 0xef1ab733bf094ae53226b4deb8713a5e1093d577      1
     92 0xefffdc05e7c5b305fbd504366b01f2d6424cb8c4      1
     93 0xf0349821f8bff3b21643d7ababbaa0a28ddf2fb1      1
     94 0xf4d12c892cf540dd73745d8518491400d0a8ee42      1
     95 0xf53af966dab1afaaac1171dd941148384d38417d      1
     96 0xf6fd5c6fb7534fc647fbccbc7bab4508f622ed71      1
     97 0xfa756a92973b1bbbee9df4b2ea3dae2e5891d424      1
     98 0xfabbe64bc8779538dd4e1fb96e9691a8f0ff49b8      1
     99 0xfbc76261fd55cf91b81a97dbcc2b3f6118f2b935      1
    100 0xfc295aeb2be32ef90104cb3e2807896e85647b73      1
    101 0xfc5446efe679f109f2772e45ea623caa63791d5e      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 96 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x01c34dcbbd8d58dbd164d6f0696a479bb3cf49a2      1
     2 0x0412b93f98dcddd486859ef3163d1edb45fea3e5      1
     3 0x04c66a0474f1c6dee2a36fc6254f60989ba2651a      1
     4 0x0616ba63e300db20c760a3a5edbab3f2cc32cba4      1
     5 0x06186e1bfb2309c3cbc9dfed039a45d7187bd6d1      1
     6 0x0a8825c03100e6cd1a82266729bf90f7fe71dc6d      1
     7 0x0d71ec30c323037bdac7bd174d4a95cec5a4ee0d      1
     8 0x0f20ea5a6f1e66416b99d2461dc132ed6a5868ad      1
     9 0x0f28b9fc566712b320b01b043463be5abde8bbf2      1
    10 0x1059c7e69c743ed1cdd10380fd3127536d5eafc3      1
    11 0x1a49096ba1a258b51f7e60c703dcf799f0959cf8      1
    12 0x1a73306100f9f27e153ef1044be99bfba644e7d2      1
    13 0x1c1571174aed09ce3a22053b993cb136ea976abf      1
    14 0x1e6ce7f7978a340a62345f38441a45cc846f8501      1
    15 0x1eea95f2d2ed24cd3451da93a69efdd08767cc5b      1
    16 0x21b1327d68134363dbcaca9730fd4cd67ec84ed5      1
    17 0x241607e362a338e7dcfc508f9ad65e276d35af05      1
    18 0x26be722da18acc407012d3b86a1774184ed56025      1
    19 0x27d73b9087efb467b195be206fa857124a24aa3a      1
    20 0x2b9635abb2ab0863cfe9175d0759d62b7f71bc3d      1
    21 0x2c6f85bc84db04457963ef2d29a62cd3b7097151      1
    22 0x3b0aa499cc6acde1d4a7433da6968d7bb8bd8509      1
    23 0x438a9dde518a5510925289def9815161ff49b00f      1
    24 0x45ec8de870dddcd0a884e239598c1f913d754043      1
    25 0x4afd23d674e7266cc81f8cf38b332ece879da6fb      1
    26 0x4d255d9b31aa437a0df8d02565c99d0faa6ed49c      1
    27 0x50b88f3f1ea8948731c6af4c0ebfdef1beccd6fa      1
    28 0x526f1a63166306e97ae1e4e0245fd7f545799eb3      1
    29 0x55623b6e3e086455700c5829ae8e4f96d6c47773      1
    30 0x55dece5aae34a50840259d2d7a2da363bf54f512      1
    31 0x59968c2c25faf934664448f5ce2c022fce0b9398      1
    32 0x59cadacb45ee465582b149669710035dd0bfafe8      1
    33 0x5b655eda7d101f98934392cc3610bcb25b633789      1
    34 0x5c934f7c8e82ae60c3f209c519a9f1b67e7917bf      1
    35 0x5f23bf4d66ff4ea4e4c888ea9ef13404fbeb7b49      1
    36 0x63bd415fc10ce71af0fdd807d575fb7a42052117      1
    37 0x6972323e5aeae1af4c74185cb68d72b42fcff0e2      1
    38 0x6b24e9039ee6ecda91559a4ef1dae8424e7e5d37      1
    39 0x6e9078a3773a5035a710709901c41e1d9131db6a      1
    40 0x7154921d5bf0e0722f2d05126a9bc725c79b9895      1
    41 0x725f7066017093b83a8942d52e0750ba9b20fdcc      1
    42 0x758ce9505c827824e65d65128c29387256c6e670      1
    43 0x78a42a84bfe3e173c3a9246b3f5f1c5aa8bbae72      1
    44 0x79a27148cb08422441c560f83729cb07721ea54f      1
    45 0x7b640407513bc16167ef3450fd6339803982e976      1
    46 0x81cc15402a47f18e210aa57fb9370a1b5525047e      1
    47 0x830077d75c03ecd994c744ef4d2dca5ff4b8d52c      1
    48 0x834e1010d164cff1abddf4644fcee5f9ad355caa      1
    49 0x835f1d5910f6c6523e80e9b7e8e5f31f1a8ee6b0      1
    50 0x841e54526835b674392e2b8f0ec19be0577add87      1
    51 0x86b7a3db9f35277779eaedeee32b544fe5d2eaf5      1
    52 0x87afc8adaff6ab99af285b3e4790b1aaac2e3461      1
    53 0x88b891b5880475fc4f9a97be08c384b0b2dfc6e3      1
    54 0x8e4222c444479dbba54309a511a0651d0691d7e3      1
    55 0x912717e0658ceb44eaa12d19a18f73a306b8b264      1
    56 0x93128eae4f4481b4610a3a2b136de26dc435f11a      1
    57 0x94ede2010554cca011c8bf52dca82dbcf2f8f2ee      1
    58 0x951360e606587ab33ebcad952593b2fc99d68c3a      1
    59 0x9681c960ba910d388b7a95ee67cbbbfdde68c3a4      1
    60 0x98be4c67bb47ce927b3ce8a3a3af1489b4493a49      1
    61 0x9a0d5e139bf315506ddac837e01f71860bb3c30f      1
    62 0x9c748a6b2fd26757f6d15b82f4bf7f7aef66f4bb      1
    63 0xa26bd819bf68fcd12fa39a5cef6e41c3c000969e      1
    64 0xa7093e8a8bb5ba4300ed29822198d0367e7e6d66      1
    65 0xa7fb11eae44fc2ac891c018c97c4f529be910c14      1
    66 0xacbf5b79db3f441b3e1f0784f419ade0027ed899      1
    67 0xad8cb7dd25b6f814bc067149c43cde2ab8eded58      1
    68 0xad91a7c4052eda333e981f701d6b56158e57b961      1
    69 0xadb1ece7e393132c4a8cfa86a291b039109c4142      1
    70 0xafc3b4a393ab5bfb5d7a905141ce3a7351f55ae3      1
    71 0xb07d2d65d938296c684dcab20eb555a0707443bb      1
    72 0xb587c8f8962610c85295c94f5cf89f6d1eb00f98      1
    73 0xb998874bcb70f1075d2cac03b560245ae2917989      1
    74 0xbbca1d8fea17d36a8359708ab47e9f86d6e0f208      1
    75 0xbdddfb2ff21efcc00a2ca5a7d1fdc5798c98f07b      1
    76 0xc33da4e7e5216130d90af5a4292db6c46ada41cc      1
    77 0xc4a843ddf533540474fddb7a9a03342348f6742b      1
    78 0xc4f94270364674f3bcce1487d98a959e72427dba      1
    79 0xc68c7771ec6a6e5d67d62aa9c6f22df69865e401      1
    80 0xc8d8bbd63fd6ad5bec1b48ac08b6373caa8eb307      1
    81 0xcd432c8f076b740cb7be693a18983218588d96ca      1
    82 0xd027379794dac07352a70013f9b0164359e89985      1
    83 0xddf403807c103624d3ccee729a88103f3fb55cb2      1
    84 0xe4f44f453fbafdf39e7af6720b505fb64b041666      1
    85 0xe88807c721a2faacc42992399b406b310c7d84e3      1
    86 0xea2a9ca3d62bef63cf562b59c5709b32ed4c0eca      1
    87 0xec9e512fe7e90134d8ca7295329ccb0a57c91ecb      1
    88 0xedda20e0cc11fae46747868fcea07e60c9b25379      1
    89 0xee4243eea50d8840f459da4fada53679aec1c702      1
    90 0xeeb68e1bc04a47c354cd7f074ac918e39f3fb81e      1
    91 0xf2cea9c6492348c19faa54a85fd77965d47fc0ff      1
    92 0xf4c3dbb6c873c05750c8356c57c247915d29bbb6      1
    93 0xf511d47077e4090402426c90d0de5274f0f96b1a      1
    94 0xfab12fcfdcd933fd61ab9f85b634d68c12292f2d      1
    95 0xfe0dad687399f50ae473d8028576e1451a53b518      1
    96 0xfe588c79166bf423ddce78d7f266d5651c8405dc      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 197 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x01c34dcbbd8d58dbd164d6f0696a479bb3cf49a2      1
      2 0x0412b93f98dcddd486859ef3163d1edb45fea3e5      1
      3 0x04c66a0474f1c6dee2a36fc6254f60989ba2651a      1
      4 0x0616ba63e300db20c760a3a5edbab3f2cc32cba4      1
      5 0x06186e1bfb2309c3cbc9dfed039a45d7187bd6d1      1
      6 0x0a8825c03100e6cd1a82266729bf90f7fe71dc6d      1
      7 0x0c98d05a2e6581a36f767cec356bae4e1d95370c      1
      8 0x0d71ec30c323037bdac7bd174d4a95cec5a4ee0d      1
      9 0x0f20ea5a6f1e66416b99d2461dc132ed6a5868ad      1
     10 0x0f28b9fc566712b320b01b043463be5abde8bbf2      1
     11 0x100417342cb70538abc22d70c6da258bc666b661      1
     12 0x1059c7e69c743ed1cdd10380fd3127536d5eafc3      1
     13 0x107c9a79c6b9b199ffaa620a47800fe2291c0d92      1
     14 0x13c942e3f8be4faf966ef03e6038b33d000db22f      1
     15 0x14229c6e20f4b5c30b88f93a59711cf435dd2020      1
     16 0x18456ce597e8469472dcb76e9144a151681cdc52      1
     17 0x1a49096ba1a258b51f7e60c703dcf799f0959cf8      1
     18 0x1a73306100f9f27e153ef1044be99bfba644e7d2      1
     19 0x1c1571174aed09ce3a22053b993cb136ea976abf      1
     20 0x1e299dc52eb9fdab6a6849f9731a948d8d72e474      1
     21 0x1e6ce7f7978a340a62345f38441a45cc846f8501      1
     22 0x1eea95f2d2ed24cd3451da93a69efdd08767cc5b      1
     23 0x1f2a9082a7692ad4f1049b573313c52e49400137      1
     24 0x21b1327d68134363dbcaca9730fd4cd67ec84ed5      1
     25 0x241607e362a338e7dcfc508f9ad65e276d35af05      1
     26 0x26be722da18acc407012d3b86a1774184ed56025      1
     27 0x27d73b9087efb467b195be206fa857124a24aa3a      1
     28 0x2a632f56f9bdf7b66be80768e89631f3febeed24      1
     29 0x2b2c3544cbd93aeed917e0a017f3387709b4c679      1
     30 0x2b9635abb2ab0863cfe9175d0759d62b7f71bc3d      1
     31 0x2c6f85bc84db04457963ef2d29a62cd3b7097151      1
     32 0x30d826b6338cc6bdabd36a10fd85de81974a2474      1
     33 0x330a7cb2418a2856cf8ec47c2606900fd1a57bed      1
     34 0x34805e6a3796fb04e82183667a78c2f7bff29170      1
     35 0x3580480dca8f6c7af673b75bb05ae780d2345051      1
     36 0x37009f9ae9dcc0a6d59204e1aeaf9da4968caec8      1
     37 0x3b0aa499cc6acde1d4a7433da6968d7bb8bd8509      1
     38 0x3ec1a0f0923a25dff523db494125cb77e86d85d9      1
     39 0x3ef5f421e155ed29e6fa815fd875bfcd5f22ecd1      1
     40 0x3fc6c595a3445d56bf5d8322a392f3e027197c6a      1
     41 0x4187ddd9340689c8e4a124b7532fca0a13eb9511      1
     42 0x4385ff4b76d8a7fa8075ed1ee27c82ffe0951456      1
     43 0x438a9dde518a5510925289def9815161ff49b00f      1
     44 0x45ec8de870dddcd0a884e239598c1f913d754043      1
     45 0x46c3adc31f3c8b8ee11b6468b7c31a21a2564611      1
     46 0x48a591f904d0266d32d3ae188504e2a7910b703e      1
     47 0x4afd23d674e7266cc81f8cf38b332ece879da6fb      1
     48 0x4b56b81041b12952d68c25b0fc0b541cbdfefe7b      1
     49 0x4d255d9b31aa437a0df8d02565c99d0faa6ed49c      1
     50 0x4d9beb5a48378acc8619e1cc8ee6a1af53379ae4      1
     51 0x4f0a9445b9b2d5a2c27814cdd92cf6da0e42dff8      1
     52 0x5019fbc77879226a8ff3f37d57401081c7aa8957      1
     53 0x50b88f3f1ea8948731c6af4c0ebfdef1beccd6fa      1
     54 0x511531656cb4bea59d62f3b1f3bb8b1d40eebef9      1
     55 0x511876aace51210752eab85e6905f05b44e319aa      1
     56 0x526f1a63166306e97ae1e4e0245fd7f545799eb3      1
     57 0x549266c062531ec5fd3979b69d10f0720dce3360      1
     58 0x54966b7b70eba0a6bd5f5ab5e292f266e2f89c89      1
     59 0x55623b6e3e086455700c5829ae8e4f96d6c47773      1
     60 0x55dece5aae34a50840259d2d7a2da363bf54f512      1
     61 0x59968c2c25faf934664448f5ce2c022fce0b9398      1
     62 0x59cadacb45ee465582b149669710035dd0bfafe8      1
     63 0x5a8116f07937394e6ac2a51995ab6a054c08bf9e      1
     64 0x5b3319297c17a0f1bc751e5d43a52ffb3e5356a8      1
     65 0x5b655eda7d101f98934392cc3610bcb25b633789      1
     66 0x5c934f7c8e82ae60c3f209c519a9f1b67e7917bf      1
     67 0x5df25d805ef73c8b679dfce920e1414eb28fd41e      1
     68 0x5f23bf4d66ff4ea4e4c888ea9ef13404fbeb7b49      1
     69 0x62ac7073454f5b8cd65558711161fc9f3436f76f      1
     70 0x62c5ee7bcc0e336bc58fc2946bb9671ab2878f7c      1
     71 0x63bd415fc10ce71af0fdd807d575fb7a42052117      1
     72 0x648a58121dc0de4436837dc585ded4fa5fba6d3e      1
     73 0x64ed28868990b8440bf2de0c62747a7a13393550      1
     74 0x655a7ba052e5acde220a482777981c050fa087fe      1
     75 0x689a19f57077f682a1d7cc19d9066f1a834721a2      1
     76 0x6972323e5aeae1af4c74185cb68d72b42fcff0e2      1
     77 0x698b7dcb16b624f104b37a2578acaede89f37fc9      1
     78 0x6a08986aad2967bd659457472fefd9bcbd417316      1
     79 0x6a509096e7ebf95acd9c8b43d604ce634997462d      1
     80 0x6acbd57b033ff316c4c93502d39e9469698e6b73      1
     81 0x6b11d13822acd45aa71c9810816b71e0078dfd3f      1
     82 0x6b24e9039ee6ecda91559a4ef1dae8424e7e5d37      1
     83 0x6b4b60cd239b5b7947c98bb2b9bbd01db5bdf44c      1
     84 0x6d2f18ee7aa579627fc41f4ebd583c38a45c83be      1
     85 0x6e9078a3773a5035a710709901c41e1d9131db6a      1
     86 0x7154921d5bf0e0722f2d05126a9bc725c79b9895      1
     87 0x725f7066017093b83a8942d52e0750ba9b20fdcc      1
     88 0x7304689aac83c3b236332b0c233878f1819ca89d      1
     89 0x75192b575ed6d49780ec43f669657f3b25e73177      1
     90 0x758ce9505c827824e65d65128c29387256c6e670      1
     91 0x77037050bb27ae8f09118a0c480224d897589c65      1
     92 0x78a42a84bfe3e173c3a9246b3f5f1c5aa8bbae72      1
     93 0x793e48857f3ccdfe5cf3c504b6dfc7e8dab5b0e1      1
     94 0x79a27148cb08422441c560f83729cb07721ea54f      1
     95 0x7ab977f469496930f43c7918022a0915f0bd9de4      1
     96 0x7b640407513bc16167ef3450fd6339803982e976      1
     97 0x7c0ca862b604c61a7b7e78647ad95563f60956c2      1
     98 0x7dcf4cfe37d08f37631193fa81dbc282e4c44da1      1
     99 0x819dfc1b757c0ed67fea97e33e70cc4cc640f99d      1
    100 0x81cc15402a47f18e210aa57fb9370a1b5525047e      1
    101 0x830077d75c03ecd994c744ef4d2dca5ff4b8d52c      1
    102 0x834e1010d164cff1abddf4644fcee5f9ad355caa      1
    103 0x8352b250c643d3be06e1a8bc7b9dcd67b704fe56      1
    104 0x835f1d5910f6c6523e80e9b7e8e5f31f1a8ee6b0      1
    105 0x841e54526835b674392e2b8f0ec19be0577add87      1
    106 0x86b7a3db9f35277779eaedeee32b544fe5d2eaf5      1
    107 0x87accabbdedce63379af9d246bb979a3a481534c      1
    108 0x87afc8adaff6ab99af285b3e4790b1aaac2e3461      1
    109 0x88b891b5880475fc4f9a97be08c384b0b2dfc6e3      1
    110 0x89c8e066958ef12ffab46854db59b4138796c3a9      1
    111 0x8a2a8f2bd05bd04e2ab24e903f4b24e199906297      1
    112 0x8da476c64056bcc7b9be720387804c48666c1855      1
    113 0x8e4222c444479dbba54309a511a0651d0691d7e3      1
    114 0x90198c48129ac7564933f8bab85669e303ff6b9f      1
    115 0x912717e0658ceb44eaa12d19a18f73a306b8b264      1
    116 0x93128eae4f4481b4610a3a2b136de26dc435f11a      1
    117 0x931d0dcc21410c0410faafc6b7913c59e948e020      1
    118 0x94ede2010554cca011c8bf52dca82dbcf2f8f2ee      1
    119 0x951360e606587ab33ebcad952593b2fc99d68c3a      1
    120 0x9681c960ba910d388b7a95ee67cbbbfdde68c3a4      1
    121 0x98be4c67bb47ce927b3ce8a3a3af1489b4493a49      1
    122 0x9a0d5e139bf315506ddac837e01f71860bb3c30f      1
    123 0x9c748a6b2fd26757f6d15b82f4bf7f7aef66f4bb      1
    124 0x9d30ca11c4a2fa4479ca14710c60a3bd4c1ca2f1      1
    125 0x9e8fdfc3d41b26a2870c293dc7e1be8675fa8b7b      1
    126 0xa06e3eca46df7e71238ebd04fa627bffc7d3ebde      1
    127 0xa1fa037a1fc2552efeeb0fcc1740f90916dc57b9      1
    128 0xa22414e4af5767448624017c7e0151d22490412b      1
    129 0xa25dd72698a703dcbb906cce432cba0586e04fd0      1
    130 0xa26bd819bf68fcd12fa39a5cef6e41c3c000969e      1
    131 0xa3ec0ea98a1a12c56a75e214a8972809e7594362      1
    132 0xa46664ba7222221475146c6710c812741a6c8bf5      1
    133 0xa4e9a636d9ec9472c3ebb1430266b634c4ac114f      1
    134 0xa7093e8a8bb5ba4300ed29822198d0367e7e6d66      1
    135 0xa7fb11eae44fc2ac891c018c97c4f529be910c14      1
    136 0xab4273e7137f9531fcb47d558d9bab0e726e6937      1
    137 0xab6ca2017548a170699890214bfd66583a0c1754      1
    138 0xacbf5b79db3f441b3e1f0784f419ade0027ed899      1
    139 0xad8cb7dd25b6f814bc067149c43cde2ab8eded58      1
    140 0xad91a7c4052eda333e981f701d6b56158e57b961      1
    141 0xadb1ece7e393132c4a8cfa86a291b039109c4142      1
    142 0xafc3b4a393ab5bfb5d7a905141ce3a7351f55ae3      1
    143 0xb07d2d65d938296c684dcab20eb555a0707443bb      1
    144 0xb326e5db8e4e9f3a27c08e2ccac2ad9783acf310      1
    145 0xb587c8f8962610c85295c94f5cf89f6d1eb00f98      1
    146 0xb5d9317265f775987a84e38bab0cfd8626135cf7      1
    147 0xb6bc60ce81c839c09b11e859eedd805f1db8d148      1
    148 0xb998874bcb70f1075d2cac03b560245ae2917989      1
    149 0xbbca1d8fea17d36a8359708ab47e9f86d6e0f208      1
    150 0xbd8dc54a09d39e753d7078c30bd3f424585bf83c      1
    151 0xbdddfb2ff21efcc00a2ca5a7d1fdc5798c98f07b      1
    152 0xc33da4e7e5216130d90af5a4292db6c46ada41cc      1
    153 0xc4a843ddf533540474fddb7a9a03342348f6742b      1
    154 0xc4f94270364674f3bcce1487d98a959e72427dba      1
    155 0xc68c7771ec6a6e5d67d62aa9c6f22df69865e401      1
    156 0xc8d8bbd63fd6ad5bec1b48ac08b6373caa8eb307      1
    157 0xcb2c2cc869f8836bfd07e3f71376b35510522c59      1
    158 0xcd432c8f076b740cb7be693a18983218588d96ca      1
    159 0xd027379794dac07352a70013f9b0164359e89985      1
    160 0xd20ce27f650598c2d790714b4f6a7222b8ddce22      1
    161 0xd263f15e76e3318f9b55fa5a8db2dbd9affdfb3d      1
    162 0xd81b26630d2fbd812af71ec88424599ce1950892      1
    163 0xd9270fd770b3e06f7b4a7452f531f035826a2eac      1
    164 0xdb25085597e4c774e2bbb02207fe52d1c7d0c1e4      1
    165 0xddf403807c103624d3ccee729a88103f3fb55cb2      1
    166 0xe1994b2fd8310218a06875b4b3d7cbca8ec83e4c      1
    167 0xe37c3eb4f67d318503faa5f84e20c49ce59b9850      1
    168 0xe42108b5ae543c7c832a29aed64add46b7f0ff54      1
    169 0xe4bef15a0d7d5a889ac0ad7c86d9ca1c4b5d18e6      1
    170 0xe4f44f453fbafdf39e7af6720b505fb64b041666      1
    171 0xe597d8a65604b109510a6bdf3730e23d22e61482      1
    172 0xe679d21696f2d833e1d92cf44f88a78e796756a3      1
    173 0xe76091f84ddf27f9e773ca8bd2090830943f615c      1
    174 0xe88807c721a2faacc42992399b406b310c7d84e3      1
    175 0xea2a9ca3d62bef63cf562b59c5709b32ed4c0eca      1
    176 0xeb43b5597e3bde0b0c03ee6731ba7c0247e1581e      1
    177 0xec9e512fe7e90134d8ca7295329ccb0a57c91ecb      1
    178 0xedda20e0cc11fae46747868fcea07e60c9b25379      1
    179 0xee4243eea50d8840f459da4fada53679aec1c702      1
    180 0xeeb68e1bc04a47c354cd7f074ac918e39f3fb81e      1
    181 0xef1ab733bf094ae53226b4deb8713a5e1093d577      1
    182 0xefffdc05e7c5b305fbd504366b01f2d6424cb8c4      1
    183 0xf0349821f8bff3b21643d7ababbaa0a28ddf2fb1      1
    184 0xf2cea9c6492348c19faa54a85fd77965d47fc0ff      1
    185 0xf4c3dbb6c873c05750c8356c57c247915d29bbb6      1
    186 0xf4d12c892cf540dd73745d8518491400d0a8ee42      1
    187 0xf511d47077e4090402426c90d0de5274f0f96b1a      1
    188 0xf53af966dab1afaaac1171dd941148384d38417d      1
    189 0xf6fd5c6fb7534fc647fbccbc7bab4508f622ed71      1
    190 0xfa756a92973b1bbbee9df4b2ea3dae2e5891d424      1
    191 0xfab12fcfdcd933fd61ab9f85b634d68c12292f2d      1
    192 0xfabbe64bc8779538dd4e1fb96e9691a8f0ff49b8      1
    193 0xfbc76261fd55cf91b81a97dbcc2b3f6118f2b935      1
    194 0xfc295aeb2be32ef90104cb3e2807896e85647b73      1
    195 0xfc5446efe679f109f2772e45ea623caa63791d5e      1
    196 0xfe0dad687399f50ae473d8028576e1451a53b518      1
    197 0xfe588c79166bf423ddce78d7f266d5651c8405dc      1

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
