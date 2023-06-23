
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance        contract        
     Length:8124        Length:8124        Min.   :1.000   Length:8124       
     Class :character   Class :character   1st Qu.:1.000   Class :character  
     Mode  :character   Mode  :character   Median :1.000   Mode  :character  
                                           Mean   :1.001                     
                                           3rd Qu.:1.000                     
                                           Max.   :5.000                     
         name          
     Length:8124       
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 17533769 # https://etherscan.io/block/17533769
block_hash <- "0xa2809af7784ef1f57476c9edb1d3fa4b16161838bf39fb11c1b7ce857468157a"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4625 

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

allow_artist_phase1       <- pick(snapshot, contracts=c("SuperRare","SuperRare2","Camibus","Foundation","MakersPlace","KnownOrigin","CamibusSeries","CamibusEditions","NiftyGateway"), address_remove=address_remove,address_max=1)

allow_memesRandom1_phase1 <- pick(snapshot, contracts=c("memes1"), address_remove=address_remove,address_pick=75,address_max=1)
allow_memesRandom2_phase1 <- pick(snapshot, contracts=c("memes2"), address_remove=address_remove,address_subtract=allow_memesRandom1_phase1,address_pick=75,address_max=1)
allow_memesRandom3_phase1 <- pick(snapshot, contracts=c("memes3"), address_remove=address_remove,address_subtract = c(allow_memesRandom1_phase1, allow_memesRandom2_phase1),address_pick=75,address_max=1)
allow_memesRandom4_phase1 <- pick(snapshot, contracts=c("memes4"), address_remove=address_remove,address_subtract = c(allow_memesRandom1_phase1, allow_memesRandom2_phase1, allow_memesRandom3_phase1),address_pick=75,address_max=1)
allow_gradient_phase1     <- pick(snapshot, contracts=c("gradient"), address_max=1)


allow_raw                 <- pick(snapshot, contracts=c("raw"), address_remove=address_remove,address_max=1)
allow_singles             <- pick(snapshot, contracts=c("65291/1s"), address_remove=address_remove,address_max=1)
```

## Allow Artist Phase 1

``` r
c(allow_artist_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_artist_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 138 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x0040122ed305e0b16dfc9fd8e0e9a74a3cdd5509      1
      2 0x00ff192363430a35abbf968c535b64147e88abdb      1
      3 0x01c463dfcbd285cf4733f6ec1e4bf58e8182b088      1
      4 0x035da52e7aa085f7e71764c0c8a1ce6690e3dfef      1
      5 0x06ee4b5bd7875ce33917e9876e9375e9807753f6      1
      6 0x075f344771dbbba0260db5640f6150657b2b3c46      1
      7 0x0ac30c6aaf2ffc01938d92aca03f71f913636134      1
      8 0x0c61d3d099c6a2fc6ad9f0effee4befa8a0d29df      1
      9 0x15181b7f41eb16678fd5e270bc755afdfc2e0c08      1
     10 0x16f3774d4b22ae11bc7c568a4d847e926304cbe7      1
     11 0x196ab3e637219ea9d2efdec0523006af0c1fc8f1      1
     12 0x1de9d1ce8cba16c98ad50d0f902e6feacbb35df9      1
     13 0x20f782d468f3581cfe433bb1ff56f700c177fc5f      1
     14 0x2155df680e53eb3d978158bbeb597e7615341af7      1
     15 0x245145accb812ebf3c7ac307f326eb7df73bc223      1
     16 0x2610a6dc27423831489842b0325e1dbcc6142408      1
     17 0x26e8633256144c30624a1c1e2cd4661aeb799025      1
     18 0x274151e8ea0c1c35d681a0ffa5a9d2016cea50dd      1
     19 0x285e62f4e30d3b8d55ac62601c325996ab5872c5      1
     20 0x303c36e5c473bf36bf643debeb61c68f780641aa      1
     21 0x30b399b1900e1e2c33bdc08f0544efb15ae253d0      1
     22 0x31c72d0d03adc76456d3c14a4fe19f8aa8307c16      1
     23 0x34805e6a3796fb04e82183667a78c2f7bff29170      1
     24 0x37009f9ae9dcc0a6d59204e1aeaf9da4968caec8      1
     25 0x38c48591a1865e7824f9ce270c46d9cbd329c9f5      1
     26 0x3b0aa499cc6acde1d4a7433da6968d7bb8bd8509      1
     27 0x3c9a25de1faa077966b61d109b92ec32e7d0662a      1
     28 0x3e1f1fc109b0130a8ba13a1546c713d030b8613f      1
     29 0x4163f0f8b1caae2492b8bf3ee451cee59d56ec7b      1
     30 0x44022a7eea59beff273e0d033d76884fa6da0822      1
     31 0x46bab2aae1fa10a6f19aa522558e03080311e01c      1
     32 0x4718d398dd4b0ec472700bd1f6091ed82c49c256      1
     33 0x47ded080a4ef933e96e357d468363a8a0cac03f0      1
     34 0x482a13a18634574bdf495c03855d95c543804de6      1
     35 0x4868fe218cb5a36592869dc33fb7d4aa848d4254      1
     36 0x4dc583548a3407a5c5f6468ccc964523cb14e9d1      1
     37 0x4f4538e2553c61bce747f73c5fe8133d4b383dda      1
     38 0x4fe91b32cfbba12f31ea73331f932a3f031ddb9f      1
     39 0x50b88f3f1ea8948731c6af4c0ebfdef1beccd6fa      1
     40 0x51787a2c56d710c68140bdadefd3a98bff96feb4      1
     41 0x5235e676a71e5c8a922a8c272d475a1de94e5ccc      1
     42 0x5294057157f9c4c4c63850644c9494d3e63f0b06      1
     43 0x53ed40c04d7bf1a908e0c57e0a889b85e534ea4e      1
     44 0x56e507da02d59190a59824308d1f63d40e0278af      1
     45 0x59d14847c0d1584a867fe98acabb1dc95ab1496f      1
     46 0x5a1342632bff7cfdfd041f5b6a57f72a85aaea22      1
     47 0x5b5d9a8a140606e680176a8aeb240752bcbcb57c      1
     48 0x5f73b065c8412b43e3db33cad0efc5038040a597      1
     49 0x5fbd3561c19a53af07db9a376b08e62b75d1ad43      1
     50 0x5fcd30a446d465ebb0ff8db85feb19fe98d758f5      1
     51 0x62050220633afe524d98fe3fccadbeacc2b96110      1
     52 0x63648f2c3e96b0f548575fc1b1efca1a61f5de50      1
     53 0x63bd415fc10ce71af0fdd807d575fb7a42052117      1
     54 0x655a7ba052e5acde220a482777981c050fa087fe      1
     55 0x69cb9f5c9065e8fd858497e801d4b085638aa7ba      1
     56 0x6b6ae848f555f70944bc99d736fe29fcaecf8b23      1
     57 0x6cfe66a3a1686005a2d1d218ffa6418b049709a4      1
     58 0x6dae8d3411339c9b92eb6f9d18468215bb91b8f0      1
     59 0x6e977c8155d914e7aa99331169939978a4d1b102      1
     60 0x6ebf583ead402a32f310dc16536067e45e20c9a6      1
     61 0x7091d6cb3e3497664e29b0022d516eaeb6d9ffe6      1
     62 0x7106e9b712fa396941a023186011ebbd7d0c201e      1
     63 0x757a21fbd39cccaff681e1930e273b9add0008db      1
     64 0x7583534d2f2c3699b1acd11b9f2fac9c35acc45c      1
     65 0x762da606029d3120735aa1eec15464e265db7a3c      1
     66 0x76b045846db3c0bc9a63b552a32e7542704bca90      1
     67 0x776c3b715b8f94719371c78da5aaf968180a1ff9      1
     68 0x8136f147fcc72d8e74a78ea47993b79b99062ce7      1
     69 0x819dfc1b757c0ed67fea97e33e70cc4cc640f99d      1
     70 0x8387ab8d11fb750f4418fdf4065f1d26402c37fb      1
     71 0x8754e1ed406a72f8deca5b4c9654ad2db698ba83      1
     72 0x883ce32e186c69559e94c8ba276bfc4322fc194a      1
     73 0x8c26fb2daf4790311df4e9aaa87b356a8a9bd214      1
     74 0x8d09aeac57f577e78e2c1501d8873eeea3b1c0e4      1
     75 0x90653b66073b4b689494d3c784a996c111521b8f      1
     76 0x92b1b677728e36734d59067a4ce53e5bf1f3a2ab      1
     77 0x934bd2dafc781d323e902fb8fa25a1d015c19812      1
     78 0x935726dc7116d4d708441ebc37de72eaf648d83f      1
     79 0x93cf0a22a26895650a8aae960bf85a01ec6a551c      1
     80 0x93f0f38e05ddf78f5e5b8a15f7cd12149b7743be      1
     81 0x96951beae8a3c191800877cdde67dc12a5881584      1
     82 0x970bb85b997af2369b4be78e703e552d4d0d7158      1
     83 0x978354196b348efbe5c2b89a46ddf13030ff121b      1
     84 0x9d693b85ef0062c842693d07d66919b37e1d17f2      1
     85 0x9dea4aba5aad3cbf01dfd5ef02bb14e00063625d      1
     86 0x9f35af4727fb91114563d8a8f779a792cb557f3f      1
     87 0xa222204acf1be4077d34102fab38a759060b77c2      1
     88 0xa3ec0ea98a1a12c56a75e214a8972809e7594362      1
     89 0xa4e9a636d9ec9472c3ebb1430266b634c4ac114f      1
     90 0xa54b0b7bbb93559963efc25c232beac0895e13e5      1
     91 0xa5b36a3732937375bc579fa38159347da9938441      1
     92 0xa5eb522c4f57a5c2c679a502e8baf45a0e757732      1
     93 0xa789bf505159b640c53cd79d7fc1b23de6212b59      1
     94 0xab4273e7137f9531fcb47d558d9bab0e726e6937      1
     95 0xab6ca2017548a170699890214bfd66583a0c1754      1
     96 0xab7473da0af4658ae2879cb641d82e7652721486      1
     97 0xaf016ec2afd326126d7f43498645a33a4acf51f2      1
     98 0xb00b18b3bf6ecc43e6235ee69424d4a220437a4d      1
     99 0xb0f7057b8a6c49ed69a9007df40d151850c10efe      1
    100 0xb2e583b8315dd01a02996e76f879c7e98d60b368      1
    101 0xb618aacb9dcdc21ca69d310a6fc04674d293a193      1
    102 0xb802162900a4e2d1b2472b14895d56a73ae647e8      1
    103 0xb9d4fda29cf01dba863bba8906caada5e6c462d7      1
    104 0xba1624dcebbf017badcb7f3cd75ede06cd96feab      1
    105 0xbbbc24fe3538ed5e6bb9b9c4fa0442c4c446720f      1
    106 0xc023b4c45cbe38f9f2b90a3f2bb94330c0909d7b      1
    107 0xc185ffb12406b8bd994c7805ed0339ce9f2529ec      1
    108 0xc6060ca0b60b22111ea08ede6410a1f9c5c16ce2      1
    109 0xc6334a606bdd3699a553fc47a81796234e217b3e      1
    110 0xcb4de7db08b4dd0a9954fcfa0a09762f58436df0      1
    111 0xcce5ad053b8a02c2ecaa8c00412ef9915339542f      1
    112 0xd2b09dc390aa62da598cedd4afb3f0bee5ce69d5      1
    113 0xd38f1648a088cd3f37cda9e9a5b7e640276c93fa      1
    114 0xd69a4f613c03e991678d990a06904fab28662f08      1
    115 0xd92df6b7fe238fe2c14fdb5816eebcc131b53360      1
    116 0xd9c6c5d2d625f6370ce0da78da7847d3f436fb03      1
    117 0xdac47e0ea5dd4565dfde65080a18e42190600846      1
    118 0xdb8f75ba73dc9412ef34aa0d79404f44364dc6c8      1
    119 0xdbc53b3cc2251c10660774b08166325d3742ce88      1
    120 0xdf5a04fe2d49971e580edec9ad4cc48dee8ac705      1
    121 0xe1994b2fd8310218a06875b4b3d7cbca8ec83e4c      1
    122 0xe268f46ce0613dd0283243a1d2e6f1f7e6e92555      1
    123 0xe2c945b5093f6314e889e0c5b260d86457c47301      1
    124 0xe4ae8ed5500fd43026af8f07542a17f03d876924      1
    125 0xe8bee2f6bf252ed9efbbd483ca4093ace7350c92      1
    126 0xef49522a18a0488a0be5568df36fb2c4f828b99a      1
    127 0xf06bed3f0dad7932d8d00fe48c36751f5c10be23      1
    128 0xf1e1c701b49b1dc2405a9e8ef40f9f88802b80fa      1
    129 0xf25ad24b791e37e83f4dadfe212e0e9bb45a1f8b      1
    130 0xf29155745c8ee222d4f7d6a2a7a50c1901f27de2      1
    131 0xf53af966dab1afaaac1171dd941148384d38417d      1
    132 0xf681e15ce17c9655fe41814848976d7d6824d124      1
    133 0xf6dd6a99a970d627a3f0d673cb162f0fe3d03251      1
    134 0xf70ebfd828b993e2863670530c2ec62c049f37ad      1
    135 0xfb1bb9d4a4c50457d339a686212a0716ea0adc80      1
    136 0xfbc76261fd55cf91b81a97dbcc2b3f6118f2b935      1
    137 0xfc295aeb2be32ef90104cb3e2807896e85647b73      1
    138 0xfe99460b27bd6d4a3e8adb29bdf038be6684dd77      1

## Allow Random1 Memes Phase 1

``` r
c(allow_memesRandom1_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_random1memes_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 75 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0064f02799ea7748a9b51b5e78bcd274d9e7d0a1      1
     2 0x05a13dcf55ea6d532f15284f39e02811fc183a8a      1
     3 0x0d6b4f304b91a3b67b1db34776ef7e95effbc729      1
     4 0x111818a51c4177e8980566beea68fe334be7b76a      1
     5 0x11a22b262e505d355f975e1e48a365b5d4811ae0      1
     6 0x128ad42b82c752c5b4c7f679231d8956c98038eb      1
     7 0x14abeea8db07c46b37a79bfa856d4c2da6d3e6df      1
     8 0x167a4e6066d6c96d5c012006f5cffc9f606131ec      1
     9 0x16f3d833bb91aebb5066884501242d8b3c3b5e61      1
    10 0x20ebf9ae7c66da763fc1ce86062ce883389c8e23      1
    11 0x2177da04f9479496c2292d6344d306aa49beb34a      1
    12 0x22fbaa2dd20b848084545515fb04b9c846942779      1
    13 0x2a0a412e0a0d436cca7ddba177c4dd9f29801062      1
    14 0x2a462fb59a5b0885b60ad31c5fe473d5ba0d79fd      1
    15 0x343e85ac3009ac01538d4251900ce1a2b7d7ffec      1
    16 0x37ea6c993c5f42e1fc3455d5781e4a580760970a      1
    17 0x45133db3eadd8718f5c3a868ec2f7148e460dcfd      1
    18 0x4b6c1d196e06446eb0534326bbfb02cc3d073a2b      1
    19 0x4c26c796abcf3513807efa54d54c34405b84a36a      1
    20 0x4d7e6948fb5ed2d92897de0605cd642eff69fbf4      1
    21 0x4f492b2a3ed9bf0c65d508dab4f7c5f5c04ca6c3      1
    22 0x51a2d3572577b6c47186c82165ab8a61e046dc83      1
    23 0x5e3118e91b0a02a5e2b92e49700c63434e94f0b2      1
    24 0x62ba9de7d81b96d697fc2f26b5eb647f184f9b2e      1
    25 0x64f02c24bb510e27b5eaec705eaa840ff7b4df15      1
    26 0x692d6cf19e0c082185e20ff5ea6bb267e7aeb278      1
    27 0x69c8c2923005d26eaeea9500d7602eff8c81c848      1
    28 0x6fdd8c800cbb81790cd48a20f90ce7e725f89c27      1
    29 0x774a34da2ee2e2d242180819f1ee88783215f7b9      1
    30 0x7ae3b0627dac32d3ee16c204ef791e37067b8460      1
    31 0x7c741ed01ee259feba4b7f9cac114f48bcafacf3      1
    32 0x8623a32af48544b055fb6ae64f33eb43edf091ff      1
    33 0x8b4567fa8c4715c27a682215a031033a8b544206      1
    34 0x8bc3757a675be4c2e459813769992ec2c60baaaf      1
    35 0x8f160eb907a80517a3fa6d22f7cf20f552696a44      1
    36 0x8ffa8d00db5df56a20c30b737c5efdaabe140df4      1
    37 0x9233389b9afa0a6e3bb403f9d211c23a3b290d69      1
    38 0x95999c47c3e32a337ef108d657675c2757a606ed      1
    39 0x97ece7185467c78293f3b796bde3704421d4fa69      1
    40 0x98004498c85c4b1997b3f669475a038bbcec2160      1
    41 0x9e1f7b007511036b5cbe5df6d39550bdd2e8bc99      1
    42 0x9e640badecb7c628c6188b74488823e879f42a1a      1
    43 0xa04f4a4b7306cb72f30828834db01699362a4989      1
    44 0xa0f1de4882a5cd1851989d9a1e9bbe9b4604e9a9      1
    45 0xab3ba2d668215acd23f7da923683a5ed88bad625      1
    46 0xaec3f7e735505c2b787b05d67f4adaefe3dfd678      1
    47 0xafb4fccf58bfc06ebc8ccd387b68140cca05f4c5      1
    48 0xb8700157128ff544c46cf35dec5a19120baa8eb2      1
    49 0xb8937891f08af9854a5ae7a5ec0cbaf4e68acd4c      1
    50 0xbd3a5d12af9fd82fb927fd26216e4dc2722d2337      1
    51 0xc18d4c2fee42accfe7bfdce98bfacbabf76242e9      1
    52 0xc1966d218b7492dd9cd672a5a237cef4d82004e5      1
    53 0xc5a2c3a524be0792519655d4093b44b988db4f68      1
    54 0xc6cf5a020bcfc65c6f8181c04cbb5ef5050fe28e      1
    55 0xc8cb180415b83f3f9a61691d246fe39a74c6a41e      1
    56 0xcb85b581caa71949784d2e826cf9df391c244b33      1
    57 0xd193a7d7b231c204b76b9d638768ea602de515f6      1
    58 0xd1afbe4da2a5adc6faf30fcbea9e4eea0ba8c70a      1
    59 0xd3364ea1c742cc2d396ac65e1723b8d608a939bb      1
    60 0xd36590461162795ee33099b2076a0d4e017ae17c      1
    61 0xd5ec003289265705727b622f1700fe814e54ca67      1
    62 0xd7e32b8a326ffd9e784a1ee1eea37684a7512171      1
    63 0xd8f3d5e9a199a35429870fca8befd159bfe48747      1
    64 0xda34ee56031bacb68cc3ce57339c2a11c28d8eb3      1
    65 0xdc78107155918e230246439e4159fea4c477eae9      1
    66 0xdd5cecf2835d465d490f79ad9773a848805a3219      1
    67 0xdffd6968900b3f4f667b69c5f9df65d9d7796a1c      1
    68 0xe2d22dc1c2f7c58f86606e405325c69f5210a6a7      1
    69 0xee3aa3fa5b780bc73da73a82d47fa2d8cdc1a647      1
    70 0xeecc9f94c674161e84ab0c60c020432ea8f88bc0      1
    71 0xf054274dd74987395d28136e53f39ef4f7b19994      1
    72 0xf1e1c701b49b1dc2405a9e8ef40f9f88802b80fa      1
    73 0xf8be957f65e67fb0342992a51c30290d5758f880      1
    74 0xfa977a0125c16a658a59ca1e59c7c48d58142226      1
    75 0xffe6832850476eb6d5ff184d747ed84f1b686aa9      1

## Allow Random2 Memes Phase 1

``` r
c(allow_memesRandom2_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_random2memes_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 75 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x017347cb75ac8725608e593ea35d83f9b2b3cfb8      1
     2 0x03024d05b1aea10b64ff3af1fed6a3df9adeebb7      1
     3 0x04294157cfba9ff0892f48f8345ea3539995f449      1
     4 0x09b2f0988b6d24336b348c84c2764d2b4f85142f      1
     5 0x0a87cbf5eec20d8d9880ad21f0260522ff5a675a      1
     6 0x0c4a911ed6af1032beec7c19225c91a3160e39e3      1
     7 0x0e3e001a00c07e2cb3219d1ec0c47cc532d87804      1
     8 0x11924c505ecab6a2434bdfe94bc0ff1ca081facb      1
     9 0x159e5998669ec6628a6b670c2ef1ddbd93084698      1
    10 0x18412b9f84df461bdc0f2e0709df507d5bdb6363      1
    11 0x1ecded4519a9688ef4bfb5f15811c52f048ba1a6      1
    12 0x21f023839e7b6fee67d33e4548791fa388564a02      1
    13 0x2917e947fb1925abe184d4062520a11bcf2a5b36      1
    14 0x2ca75a892b59bc7cf863ba261e49ab40a4844ee3      1
    15 0x2e299b7faa9b906117181a4550d828b1300b48d1      1
    16 0x2fb24b1021e51fa23db91a9b995000181fda0963      1
    17 0x30b33f902981b5e567d3bb1a3cc847b35f626609      1
    18 0x387f802c6a9ecffc8b8f6bb7b80ea0db4cef9810      1
    19 0x3a10fd1387555cd75db8142393fbaba98efe28d4      1
    20 0x40e6d6e798dc878e12987ed978f23c2391f1f570      1
    21 0x410a9f44948576c60f94b165f63eb79ce769a04c      1
    22 0x487869b7557f0fdecbc5141da1d7c48c15c5b8eb      1
    23 0x49d6531aea6dced6595d50da685274766e4cfc8b      1
    24 0x50d086175cc14fa5ae008591d72a93cf22b48e32      1
    25 0x52349a2c4ea4e4b94ada3d75c9d3a318c024706f      1
    26 0x52690f90740621f89f58521433e9b0921d626708      1
    27 0x527bb834cc5c8ff730c673880e51372282b06e14      1
    28 0x571493b6bb0b99dda41e2da1774b8c9b5bc302af      1
    29 0x5bd832dc1a5ac61f687b87c4199b844819a4d8ed      1
    30 0x5d20e6c0db887b5c3281edfce5bb7677565e2b8f      1
    31 0x62c868061cddbe38e01e7a8295fdcec72c10ca06      1
    32 0x67c6083d540696c6aa1cf31101d78780e0d6bb47      1
    33 0x6e1ac9b3f4499a73385dd8d2daed6449406d49f4      1
    34 0x70e680b9493685f72e76243c09993fca768eedf1      1
    35 0x753c892626c4fa09d1aedc0e9b52bda97a4afa00      1
    36 0x7f6ca49d1e50671a586a76bb082dd6b3f73fef17      1
    37 0x84df1fb4801e72cf6958a994dc0b690f9e7cb23b      1
    38 0x88ccdfe9dd047b4cec4c1102d2d803e2d8bf683e      1
    39 0x8da03feb7174b4b2a6b323f3dc40d97f3421cd07      1
    40 0x90af376346ca97d4e9d1e510e82543ef99b56a28      1
    41 0x92b1b677728e36734d59067a4ce53e5bf1f3a2ab      1
    42 0x953b0f8afc70e0fca9e2e43b0a4914be002c4e94      1
    43 0x956f7db2fdb52de9453ae184d51d1883b8cd9d68      1
    44 0x982d3c5223f6b5794fccb3208eb164d042cf2526      1
    45 0x9997e55f76e34f14d7bd6b268a2e16f896389ee8      1
    46 0x9b7c0814eb341e6a4ca673148e8b577b13e70787      1
    47 0x9bd69d51526fc9db09bb831f5069a48cbe4d3421      1
    48 0x9f3c44a7331ca9da2d3432bcb0df91186a27e3d7      1
    49 0xa1b669414d40e68c11652b1cd82381f2a6495b89      1
    50 0xa4b61e227361a9cd9e62ca10946c27748a382cab      1
    51 0xa817b9bd1ece360e4a1692894a5ad1d40b889f20      1
    52 0xa8879c580a54f190ed53b43d30de269097ad7543      1
    53 0xa94ba6f81ede0d81a5252e71916b5c5567ad6a01      1
    54 0xb97624935dd3fb84e5492e8d01c6fcdce8060cbc      1
    55 0xbfe384d79969bcb583b8a0e5fedc314aee480e7e      1
    56 0xc04208f289d3842ac168f2c373b3164e1d872650      1
    57 0xc13d5024c2ee14c5f80847afd09275f8b550a135      1
    58 0xc58326c7020f26345f4568cc09daddf019a8e6d4      1
    59 0xc6411ff69f1ec6bfb4b082f47db61dbedcab250d      1
    60 0xce990032a3ed14e197421270a9bec9e276bf2f31      1
    61 0xd3069c9e486a8b77add55ae3ceb3a4b138fb76c7      1
    62 0xd7342ea20a5afbf24352b5ca61e09844167914cb      1
    63 0xd7d941ff2890bda98f40a5dda0593d239a603f09      1
    64 0xd9e2ad004ac82915d7472448cef2b182547487bd      1
    65 0xda23ab0b2636c3f9806a868278d73aef2439ab60      1
    66 0xdcab53e3c8a2c37cc5e2509c2db0caa6c04f3ce0      1
    67 0xe2b76c9f15a1e5841a91b404ab4be1c3e5d19551      1
    68 0xe776de32c9dc8889e2dcf1cf0606010a51d1abb2      1
    69 0xeb8661e8548dc074b9ff411080fdd9a377e6ed1e      1
    70 0xecea65d22233e7beed752a06c46cefc6c90a1d7f      1
    71 0xf15a6b54e68884d27e1bebb1624d70c227b7d04b      1
    72 0xf161ff39e19f605b2115afaeccbb3a112bbe4004      1
    73 0xf4dde6ea673fb810b6f503bf441908bb30b929f8      1
    74 0xf620422e1e2cbf123386ea664f536d1016e5a5d0      1
    75 0xfc7d068f069e712dd35b439bf6ed9df8f58d07de      1

## Allow Random3 Memes Phase 1

``` r
c(allow_memesRandom3_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_random3memes_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 75 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x02009370ff755704e9acbd96042c1ab832d6067e      1
     2 0x03ee832367e29a5cd001f65093283eabb5382b62      1
     3 0x0f3c76ade30adf61b64d01154911308491784dfe      1
     4 0x12c4908dc21e88a8f268c3f363098eed251c1aa3      1
     5 0x13bfd26cf7d981aae5f8d4d88af4b34b4b1d4da4      1
     6 0x13c39b3e6e6159bc233fad90825f1f9f84fcb102      1
     7 0x183abe67478eb7e87c96ca28e2f63dec53f22e3a      1
     8 0x195f2cd867c1c02ae6e61a14ec53f5f01b440edc      1
     9 0x240d478106bbfb613b923be58c9269c7d90a41b1      1
    10 0x27e037e0461e3d5587de786ea23441d83772353d      1
    11 0x3025430ae8a81cd13e3d0969b1093f8d82bbbd7d      1
    12 0x3368b4733a4552b38b7d81c215d88cac4c78d8e5      1
    13 0x34dea787524a930670862a27bc7234a1897e1bf2      1
    14 0x3543493a76ce5ca362dd6eeef000547de52b6875      1
    15 0x3839a6c85a38de97abd0ecc092ef3311a7f1311e      1
    16 0x3877b4c39476e3af04cb1ff8003db09f39f068f9      1
    17 0x3b748a60dfa1d58eac080a5ef24b11a082edb6d2      1
    18 0x3e998c50b1eaf1577416e2bf31a76d09a48bc3fb      1
    19 0x47d278101ee9335c3a3baa14fe184f757229a7b8      1
    20 0x48be4681972473b498e8b686d38e04826c26fc4f      1
    21 0x4c7f1354ae3d30f7a88c9d972ad7f96b44ce0dee      1
    22 0x4e3cc03eb2be6122f2316b84d1c7edb900e90dba      1
    23 0x4e61548d94c8649ebfc2f5f54d2272fcbc687bf2      1
    24 0x5458c15ff220bf75b92a76229fae88f93df9c6c6      1
    25 0x557aba8ed2791f060e8b80f76b5e3aa6fd91dfb0      1
    26 0x55d4a9e5cde6fbe40991e7570e01899891f6b0d2      1
    27 0x5659de064e163a7178b2d3b802ff13274d7d8e69      1
    28 0x574a8af005c82eaf05e92c9ccc01048e1eb1ae41      1
    29 0x5c3097e7fd3b97d9fdeec6d378884c892ff0545f      1
    30 0x5c9e2a6fec34b510996a8e2a3d1e2c47a382a8b9      1
    31 0x5dafb2d1f8b20501da62ba00c0c283293aa0b70c      1
    32 0x5df6dd99718f32ae362b41e78b7bbf66ebccc179      1
    33 0x6215e708a7b61f5ed49642f692f28ba09b807725      1
    34 0x646ef8e3c8e79a10daf8f32621a3e468501cc3e4      1
    35 0x6b9bcb0b854d7c7430baa55167d367d28a8dbf1b      1
    36 0x6ecb6038d923606b3540c4c6ac267c73ab7fa3c5      1
    37 0x782ff3f609427680db175365c24c238f89fdd276      1
    38 0x79d1dd0770fec45768a285da42c2521603c9b91c      1
    39 0x7bc7481751d7fcb76d08f596b03ba1e42378ea27      1
    40 0x7cdbb8e309ce7c701ed200be123f16c6f7c9e525      1
    41 0x8b6a3e1f151fbd8a45539e0942918e63d35c6cf4      1
    42 0x8ccb07293755004942f4451aeba897db44631061      1
    43 0x915c6cb77d302b8c514561feee12b5cb6532a97e      1
    44 0x91e8be20d0dfb2293b97317e906223c1fa833a48      1
    45 0x93c838add3ac72b42f65417408f900a7155e1bb8      1
    46 0x99980dd0184773ea492c23033e1309609a7e75fe      1
    47 0x9d52fd3f58557f3b5fcd2b10314bf566cabca60a      1
    48 0x9ea50708e47a237182a56c481a4d11dd021b7ec2      1
    49 0x9f281c5b04c091096ac468a9388f0ee6b0b8b1f5      1
    50 0x9fc80955aee9e3eb8c511a50a72b7e589700ffd6      1
    51 0xa12cea84eb9e6dceecd125de27cf2e9701104956      1
    52 0xa44b68fc590a2fd1cf48f2689a8aa2c8b62d2261      1
    53 0xa9237c4eeebc879208e59c867cb6d7cbffc1df30      1
    54 0xad5bd06f5fb4633888ac7506ec0c87eb99998653      1
    55 0xb33fb83c645ac2a12b138636fe59604312092484      1
    56 0xba264037d93f4e06d5b19e9a73f29fa64689e1ba      1
    57 0xbc1eb4359ab755af079f6ef77e3faac465e53eda      1
    58 0xbdc29767d8bc6dd6b08d64d74c8ecf11b3f5ccf4      1
    59 0xbf949494127d3cd72cd3399d4ab38312757f4d12      1
    60 0xc31ba2d3275a397c43b1a4ab707cd79adf310233      1
    61 0xc53f977136e1aa9031343fad39dcb4c11a1eb3c6      1
    62 0xc659fd78a395d70ec69ff034b4378a712e7edb64      1
    63 0xca288359ba6319c7e7c7377a670183acb3781cda      1
    64 0xd530282d853169a23822762af0dcab045d6295d3      1
    65 0xd89c641cf9cca44a3435895d24494d1bb7d70ee9      1
    66 0xdbeca36c48306867d9285e0e0d5ed1f5aec28a35      1
    67 0xe39bd9e3af2054dd193b0d0217cfbd460f104902      1
    68 0xe3d9a03a6422b56e7097ff41a86045683f0aa150      1
    69 0xe50ae5655d227767ec3974a11da7e4f67476b96f      1
    70 0xf3e6fbbd35e2ea84bdfdce9be96ffdb2b8bd1fc8      1
    71 0xf52393e120f918ffba50410b90a29b1f8250c879      1
    72 0xf599a05b4ea846a5afa1db274b508bb1ba1ddd93      1
    73 0xf81c1d67b9c1f80eb30c610e2a78c2a1b1fb013c      1
    74 0xfded90a3b1348425577688866f798f94d77a0d02      1
    75 0xff1a6681aee53d032e8f5fb3d27ccf29d493e922      1

## Allow Random4 Memes Phase 1

``` r
c(allow_memesRandom4_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_random4memes_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 75 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x00baeaa84510584a6e4624964b75321ddb6766cc      1
     2 0x00dec4b9193c7519cdc420d660b7a12559f04c94      1
     3 0x00eb4f521acf3b6245c751cb89bef91cf420a1f7      1
     4 0x0bd31f087f85de4a84a985c61501b69654595f5c      1
     5 0x0dcee254468df83cfe9dfb2236bf253459cdb079      1
     6 0x0f22657ddcf96c4d8e7cd22f0ab18fbfb579e551      1
     7 0x133fc918b3a27fa314c45a08ec7eb794ef0283fc      1
     8 0x13f9a182272f3ce3193fbda5459a8919d27624c6      1
     9 0x1df4b6d97eb84c007f52cbfd942c987a72e94ce6      1
    10 0x21f9298f9422626c94647ccf15aceb0c7a830ef3      1
    11 0x224b5e80309c565bd310f2984b0363054cba90f5      1
    12 0x258e0ae28f70ec728a9c0d84268ca3dd5c69fb3c      1
    13 0x265773b798f5a83876aa0a463526a5d86dee5e00      1
    14 0x2c50d42deffa8a0c45a72ff7a7449ef168f3d4ec      1
    15 0x2e8f6f8662593134dc8311e312885921855332bc      1
    16 0x2f944738905f93ec3fba145463e0854b7da77f71      1
    17 0x33914463964f994f093bfbaef4db18110dad60d7      1
    18 0x3f86d4046be7812c886f3b9f00aeaafde39d32e6      1
    19 0x3fbb61bedaea1c12430e6ee6d5ec62ab23a4678b      1
    20 0x42548a143764550be44273dffb098103625fd965      1
    21 0x477ea7a022e51b6eb0dcb6d802fb5f0cfc3b4a81      1
    22 0x483a124f6717ad67440af11a77dec25f1a3d1259      1
    23 0x49d18648e4aa35368a683c24881dc94225518751      1
    24 0x4ce8a8fdfd9b33b5d1f3ce93d194bbeb0d9e2257      1
    25 0x5286bc17220d51a36e55f5664d63a61bf9b127a6      1
    26 0x5322d25a83fedb9055fbdfaf3e485821438871dd      1
    27 0x547322faf3b4d0e16c8dda880da2bbd40ebd6109      1
    28 0x555a5083c82ca88cd4b4c2d0941495c9198ce6b8      1
    29 0x57dd129276d327b11891b7ae0ba4ead59215b08c      1
    30 0x5972115498096052e3f3165ad7788d49935a6c6d      1
    31 0x5b067f845fd99cf1e80e2b5833bbe6e6910f6843      1
    32 0x5fa5365962b46b863dee4e6dd0a45ad5a5ecd37e      1
    33 0x61b50a74376a9cd1dabf20bc922017441f9f8267      1
    34 0x68cbec6e76a8cc835ae75cce5feff1f36cbaf764      1
    35 0x6bad7a9cc04871d133291d418b28e35569ad56f4      1
    36 0x6c83b447b009ef76812420b936122cdbe74d3d59      1
    37 0x6f3a349df0dc8e88263a1ebdeb59a31be094fb88      1
    38 0x70604bd577edf06d618d581d76e2fa88f8670a73      1
    39 0x72e680156324b5afa5d0d84b621844424a8bce42      1
    40 0x75b772f2bb4f47fbb31b14d6e034b81cb0a03730      1
    41 0x82d32aaec5b77528d920843d84cfdf45e84ae9b4      1
    42 0x8a3add4c72da23e647b76150127b78d79c5f0847      1
    43 0x8ebd84269464b6ad7353d192f3b487e69d607e05      1
    44 0x903b1b960ecfb37d994fa0cc537777d8ed766656      1
    45 0x942dfb0c7e87fb5f07e25ec7ff805e7f973cf929      1
    46 0x9553803357f9441ee426e701a1bd8031f814fcf0      1
    47 0x9b16cc7f4ae8c718d630fe6fe474b85d2a0cbac3      1
    48 0x9ccb297fd589925f08af6e3958fe006195da0b74      1
    49 0x9e414d7ca84ec395070967223ac64c8264b90ecf      1
    50 0x9ff3d60cef60e835f142ce5e13a180c70ac89549      1
    51 0xa36fe80f6f730110ddc12245bb4cc94c1a3b5d08      1
    52 0xa3cdd8dc1d0973db547e427e6ef5e82efaa5b63c      1
    53 0xa78b54a5486ce07838c1511c5ad5f72df2ac833a      1
    54 0xa81725fae8bb3930a95debb0adeaa184b5e9cc10      1
    55 0xa8f3d8344d20029b667192b48156899a60415ed0      1
    56 0xacc4b6f3ca8d59f631c1148dcaa52b9d7f5c819a      1
    57 0xb27ef7117aa0b6aa34e051221a1932de261426c1      1
    58 0xb2c19304101072a7311373733d2dc30b56025890      1
    59 0xbc5a7cf988f7e7e8f3893cd0ee1d0786cd4af889      1
    60 0xbc9aa6262e2b34d5d124bd754d74a8a61f75580c      1
    61 0xbe09e43a57f2980998cedd08517eae7d5ae683bb      1
    62 0xc23e873f9c9b968ccdff0e413710d865ef18b860      1
    63 0xc4627e5c93b5ce0697186ba3b9fb322c8d4c2e1c      1
    64 0xc68092e6a9fcf32b1e1e3bba0e9132b6005599a1      1
    65 0xc805afc358852017ec0487b27d743091d632a4f3      1
    66 0xca0a8c0eaad3e8c0939c718cf439e30011b86294      1
    67 0xd5755a4276a53ee7ca2703c6e1967af59cbc9feb      1
    68 0xe45380c24d361b5314357ad93dcc3e0e540718af      1
    69 0xe58bc0de746eb83efaef6eebd55e9ecced6346fa      1
    70 0xe8f704418f3be2a20d38e3f1cf7531e3bfe364c4      1
    71 0xe96ead0ad625122677684448155ba4ae2700814d      1
    72 0xf2c7e2f00e72018769e419bb6511d34d59fb4d61      1
    73 0xf5ab63bfaeeddfcd37f696a9b8697e424bb22788      1
    74 0xf944030287430a84583e565e8feb3792e9db0708      1
    75 0xfa9adf59f9b51101425b487ec77f7d087bfeba5a      1

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
    22 0x477ea7a022e51b6eb0dcb6d802fb5f0cfc3b4a81      1
    23 0x48464efe55fbcae8ae0c992b306afcf21d4910cf      1
    24 0x520e8d6c5b2dbbe62bf8a40653a314d879b19b86      1
    25 0x53006f95def268f88dc1b8216654ab56f3afd052      1
    26 0x575f6540c16a72696c14a17fa64f049992d661ab      1
    27 0x59068075a799594db03c0255eed68e8e121155c8      1
    28 0x60d2bc84a71af61b82d1b4168c9103ff6c4898eb      1
    29 0x615502edd5cf2d59244dd0dd53d3c8ec035e172b      1
    30 0x61d9d9cc8c3203dab7100ea79ced77587201c990      1
    31 0x64f7de90dc79d775703bbec66a1591c7a26a22f0      1
    32 0x665654f2d2a151be2d0f8e3697e6ce780f732af2      1
    33 0x670a840848eba22eb2a090fd65e1ff60c3ba9e5b      1
    34 0x699990a8e7ada9e92c932d6e8fb365024fc74b43      1
    35 0x69e68074f1aada957edd39c5eae0069973343f30      1
    36 0x6f0735bf1e6c69030d6990cdd580345b370eb50a      1
    37 0x6f113b0c8c7266d7514e4986e3d6aaf013b6754f      1
    38 0x729fcbb6e1289c88ce5113ec1c83a48a8e3c9f2f      1
    39 0x73bcb8c5e30bf85806aade7fc36f16c6b80fd3b9      1
    40 0x75005a05bc163b85991b9c9facbcf3155372422e      1
    41 0x78a576d3751e8cc8f30c54c367fe8e5a2cd71228      1
    42 0x7e5ab36876a267560e7191cedbe99ee7bc04bc30      1
    43 0x82139687faae8a29851902783e02e699de0e0846      1
    44 0x85603a042455d2f36f3ec52c1c756344aa70c5ef      1
    45 0x88ccdfe9dd047b4cec4c1102d2d803e2d8bf683e      1
    46 0x896b94f4f27f12369698c302e2049cae86936bbb      1
    47 0x89ef05c30c821c49b67d06e630686c5c3232baab      1
    48 0x8ba68cfe71550efc8988d81d040473709b7f9218      1
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

## Allow 6529

``` r
c(allow_raw, allow_singles) %>%
tally() %T>%
readr::write_csv(file="allow_6529_phase2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 4 × 2
      address                                    amount
      <chr>                                       <int>
    1 0x2c8875f34ceb219f61b7453b2c5f100ec2f6ed33      1
    2 0x89ef05c30c821c49b67d06e630686c5c3232baab      1
    3 0x9274f2f89fbe5dc00c21c628e46a18c7187c14d7      1
    4 0x9dc5b2cee788e3e9c67489b5d7ce634dbf48a32e      1

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
