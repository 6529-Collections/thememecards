
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance         contract        
     Length:863         Length:863         Min.   : 1.000   Length:863        
     Class :character   Class :character   1st Qu.: 1.000   Class :character  
     Mode  :character   Mode  :character   Median : 1.000   Mode  :character  
                                           Mean   : 1.101                     
                                           3rd Qu.: 1.000                     
                                           Max.   :10.000                     
         name          
     Length:863        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 16789269 # https://etherscan.io/block/16789269
block_hash <- "0x9ebdd75e923ad2478528f7f6026a9c64bf295d345f67a2ffee2f6d79e007bbe9"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4788 

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

airdrop_memes_all   <- pick(snapshot, contracts=c("memes_full_bothSZNs"),address_remove=address_remove, address_pick=15,address_max=1)
airdrop_memes_szn2  <- pick(snapshot, contracts=c("memes_full_SZN2"),address_remove=address_remove,address_subtract=airdrop_memes_all,address_pick=10,address_max=1)
airdrop_gradient    <- pick(snapshot, contracts=c("gradient"),address_pick=5, address_max=1)
airdrop_muhju       <- pick(snapshot, contracts=c("SuperRare","Foundation","VITIA","OBSCURIA"), address_remove=address_remove,address_pick=30,address_max=1)


allow_muhju_singles     <- pick(snapshot, contracts=c("SuperRare","Foundation","VITIA","OBSCURIA"), address_remove=address_remove,address_subtract=airdrop_muhju,address_max=1)
allow_muhju_editions    <- pick(snapshot, contracts=c("MuhjuEditions","SpecialMuhjuEditions"), address_remove=address_remove, address_subtract=c(airdrop_muhju, allow_muhju_singles),address_max=1)
allow_gradient_phase1   <- pick(snapshot, contracts=c("gradient"), address_subtract=airdrop_gradient,address_max=1)

allow_raw           <- pick(snapshot, contracts=c("raw"), address_remove=address_remove,address_max=1)
allow_singles       <- pick(snapshot, contracts=c("65291/1s"), address_remove=address_remove,address_max=1)
```

## Airdrop Memes Both SZNs

``` r
c(airdrop_memes_all) %>%
tally() %T>%
readr::write_csv(file="airdrop_memes_15_both_szns.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 15 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x185952b3bb31da0ae18354bbb90ae40adc840c33      1
     2 0x248458947c120ca057ec028b3fe7e4b3f26fdb3d      1
     3 0x2a0a412e0a0d436cca7ddba177c4dd9f29801062      1
     4 0x2ec4a2bcd4f33c7c9aafab7cfa865ec15508bf62      1
     5 0x5df5342342701b8ae5bce28f74ebb73b5fc13a54      1
     6 0x69e68074f1aada957edd39c5eae0069973343f30      1
     7 0x82139687faae8a29851902783e02e699de0e0846      1
     8 0x8854b06ba346a703a3c043e2e7b8822db3ca6b3a      1
     9 0xa71000e72d38e6a84d7190f59fd3dfc73931c0e8      1
    10 0xadebdeab678647a457743ea3af98f8b804e45c24      1
    11 0xcaa1c396e70384db58dd33be74b26fb119e98c3a      1
    12 0xe25b24cebed3055236e369570a437a99e1d32602      1
    13 0xeadd2d51e75a8b42302adc2aff0157a4bf626adb      1
    14 0xee2c055f7706b9dfcd98cd5a23d5629d6316c0bd      1
    15 0xf84408070b4de16d6bb0ab2fd8b19d37e3fd1422      1

## Airdrop Memes SZN2

``` r
c(airdrop_memes_szn2) %>%
tally() %T>%
readr::write_csv(file="airdrop_memes_10_szn2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 10 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x111b863c2f7d1833d8f53830647c260169e99626      1
     2 0x11924c505ecab6a2434bdfe94bc0ff1ca081facb      1
     3 0x12cd871105b29760a61f56753d54a59381f5bcc6      1
     4 0x69cb3b1de24e08f1cfc2994171b6c6930498f750      1
     5 0x9f3c44a7331ca9da2d3432bcb0df91186a27e3d7      1
     6 0xb7546d1b04151e1e96d38275af66eaf984fdad2a      1
     7 0xb775df51776d6767f8c2f5a14b667e300f60447f      1
     8 0xb8b8ce5e3d0ad5c0169e130c64048b6d3cfcd343      1
     9 0xbbc37f68e9876d64b2c55016081528ae0a85d8b2      1
    10 0xc02e6b0d0c1a5d8cd26beeba0fe8d76c5d2f19b9      1

## Airdrop Gradients

``` r
c(airdrop_gradient) %>%
tally() %T>%
readr::write_csv(file="airdrop_gradient_5.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 5 × 2
      address                                    amount
      <chr>                                       <int>
    1 0x134cb213549c9f114176adb9e4a78d6a272a3672      1
    2 0x670a840848eba22eb2a090fd65e1ff60c3ba9e5b      1
    3 0xa56c04347abee42f663eff9bc2d0147b97c8f782      1
    4 0xa743c8c57c425b84cb2ed18c6b9ae3ad21629cb5      1
    5 0xae72c6a6fad9fa9d82d089e1ebf73b3043855425      1

## Airdrop Artist

``` r
c(airdrop_muhju) %>%
tally() %T>%
readr::write_csv(file="airdrop_artist.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 30 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x01b22bb2d473a53fae501c273e6d5592a58a131a      1
     2 0x14c3681d58b4f1c17735b2c7f90a8463aa51eec7      1
     3 0x1711bc52bf7e0494325799717fe640f1924617b7      1
     4 0x1d95b0b6d3582feec7ef35d2ccf91564ded0cf7f      1
     5 0x2bad77c1ae11ff611bd23f6f350d23079ad8a6e1      1
     6 0x2ddf1f982e1bdde70c7ede254a52686aeb35c9f2      1
     7 0x31c72d0d03adc76456d3c14a4fe19f8aa8307c16      1
     8 0x32a0e5b656c2777d0f4d1f834e097d76505c2cd0      1
     9 0x41e399b9243bd6f772e30cf79dcbaba791143d6c      1
    10 0x5068893a404e022a4cbcfa26d17e37efdc4b8c56      1
    11 0x51a2e95a3f7c2ec7614bd341d704ed1f51ff7f46      1
    12 0x5a7bcfb9390b37fa0055e434b041f20a719e1aa1      1
    13 0x5f20abebe0ba11a9dc0f08e654e416bbee8979f9      1
    14 0x660c3bf8d0f8b84a9e1e7a076cfe4685128f5f7c      1
    15 0x6a682375545982da6d5c926dc65e3a8d360720e2      1
    16 0x6b4b60cd239b5b7947c98bb2b9bbd01db5bdf44c      1
    17 0x8323f44e0b9337db92810f261bcb64878c04915d      1
    18 0x834c771e04770b1f54fe13d258e5a640d78fba29      1
    19 0x86e3e86b19af90d3f2a87c588e052fb68083152c      1
    20 0x8882d1640e0cad2f0acc924acc649a1a167d945f      1
    21 0x92158120bad37251c6c845c1a9e399bece66cf44      1
    22 0x946e2dd8d7fd6126b864428459e62d6e8f910051      1
    23 0xbdc4a5c0ff7275736cad102c7408555fb5d6c495      1
    24 0xc04038c0d0b6ee1034cbde6239f71ff10b81bed6      1
    25 0xc5b529de562105a0233564bad076ad14b8171bf8      1
    26 0xc820f8af29aba17dbbc155a93f1323eb2f750cbd      1
    27 0xd526ebc929877963ee14e984fdb1d63b0fc2a096      1
    28 0xe6a620649de76c9a982f56c7147c79b4279ea805      1
    29 0xf7d0696e57204e4d9626bc199e8786ddebced010      1
    30 0xf887425af9232e9288eef23308fb8396fcd1fb53      1

## Allow Artist 1/1s Phase 1

``` r
c(allow_muhju_singles) %>%
tally() %T>%
readr::write_csv(file="allow_artist_singles_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 15 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x01163f6894493f9ecb59c30807a1296b0e00cbba      1
     2 0x11a9583750806c3f521254c8e930991cd6139b30      1
     3 0x2b2c3544cbd93aeed917e0a017f3387709b4c679      1
     4 0x2dd8c08c9259b8b9e5f2119d33c1d41ec3ebe386      1
     5 0x4e626e64b44a9f299da9cb006320297f870a8397      1
     6 0x606ecba73e0550c7bde5063eb716e1e675a07844      1
     7 0x6772e726dff4ad2541c859b0799e8c18c6e2c489      1
     8 0x7304689aac83c3b236332b0c233878f1819ca89d      1
     9 0x7b2ad6af37be6a77c95556d9453e4877b6a8437d      1
    10 0xaa4a07fb5c4eca87bc080fa7404224b16a6eff38      1
    11 0xadbeee6b8760898ccb4c25e69ccdaf16bc17f873      1
    12 0xaf9e18cd797548220317fb459095c6afac5c33a7      1
    13 0xb036e72849511a481d6d1467baf8e6b1bff40d89      1
    14 0xc49786d5546edef7659621ff1746ca6baf6c50a4      1
    15 0xfbaa56d3f79800a5d9a75fa48e7299cc53906785      1

## Allow Artist Editions Phase 1

``` r
c(allow_muhju_editions) %>%
tally() %T>%
readr::write_csv(file="allow_artist_editions_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 254 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x022b266d1aa39725a4e64aee661f0277461e4e39      1
      2 0x0264a803979bd0b485a6701f1ab17aab3bcd8ff7      1
      3 0x02aa7cba72f07e7929c6b80d8dbed2a834e485f3      1
      4 0x03024d05b1aea10b64ff3af1fed6a3df9adeebb7      1
      5 0x039d7c84bc43f8bc311ec21aeef57dcb45a8091d      1
      6 0x03a2c2c9f48635e9e145771b26ff6f753e560a15      1
      7 0x04490fb053a8ff110bea35f22d955c0092aae5f8      1
      8 0x04c66a0474f1c6dee2a36fc6254f60989ba2651a      1
      9 0x08117086ddd605ba17fab621f15c533892554cd9      1
     10 0x08493a92a95d4e93bdbc49f66e7647f1445a9210      1
     11 0x0970b838d4d209b21de42ca52f7e0fe79811de26      1
     12 0x09a5943a6d10919571ee2c9f63380aea747eca97      1
     13 0x0b75630a69e27fd44b06748cd6b5f59e52994994      1
     14 0x0bb75bef057da63a0ae4b25fe9adafd35cd92b87      1
     15 0x0da1593b61973dc8eba3b29da378cff5911d46b8      1
     16 0x0e211ce31d5779b65a8d8c316e442e875c112515      1
     17 0x0e3c735708f660b9d29b1f05679c13f72e14c0ea      1
     18 0x0e8d35bb16acc48144253d703eb19358f739aaae      1
     19 0x0fb3fa40850c1e472b6eced8fb10781763d42193      1
     20 0x119fcd292b95551e8af472fc4721ac34cc5f48e5      1
     21 0x11e7dfd1ceb3571e00888e96c77ed92fa32c3692      1
     22 0x120e4809a78cfc042bcb090b8daf6b40963626a4      1
     23 0x1430997022e8ccf845af34596ef37f4754983f85      1
     24 0x180669310d089fafcbee40479ee719752d680d4e      1
     25 0x1a962d6a95cea8060b1e9f55c5bc6622315c0108      1
     26 0x1ae50668be1f32179bce00eb203121f00907d808      1
     27 0x1b5dc7f6a12471bd4f6f584e648edd8fcde50512      1
     28 0x1b7690aebc8f986a780a19d74c5a9c293edb0d44      1
     29 0x1c5762f9899b46c19d4c9b8fbdd38d35856227f3      1
     30 0x1d31e2fd737bfa84bbc6102c7e961df30aa5ce54      1
     31 0x1e4f4df03ef9dd1bceb2d3f9f2cb1ba18c816704      1
     32 0x1ec9b60aaf97dfa9073534b95bf81f0923e599a1      1
     33 0x1edbb96654e39c72824820eee63ceecfa34da790      1
     34 0x1f9dde7b25c87c97583d5771de13d60375aa3907      1
     35 0x1fabab52a067d748ee0ad3c1ab3c88d1b3a9785e      1
     36 0x1fd5031b9d8b2d4fea66e325756b357c335b6727      1
     37 0x2063c384e0cc27d32375fad27111d026e86e111e      1
     38 0x2075e5ed7702dd03cf7cfe06c45277c89817b189      1
     39 0x20d50378364a6ef801861e4633ed04ad5eb125c3      1
     40 0x223e42994eb3c7b4953c86973892ccf0bab4374a      1
     41 0x240916c131e67f2c620cd7939654af73fb3c14d2      1
     42 0x24e6063ac956d2b3526efd763caa074f8f7630e3      1
     43 0x26caeb059532421d1a365fc6b7bdd6eb455573fe      1
     44 0x270e8014a75dfc51c54349e7ebed8363a5898425      1
     45 0x27b455c481d62f91a7b55d09cf2e3cd70858ef85      1
     46 0x2894f4fe2c4dfc83b20445cf1511d9892fc7ba73      1
     47 0x28b8d4f7516a112e2e2fd462293a1c27cde327a7      1
     48 0x2918ceda641716fd6bda3ccf691c3b54d4c87b6e      1
     49 0x2a8f6c200528c6308fa3c4b94720691471ade1b6      1
     50 0x2b99146071c8e4b7dcc3848cfbaacc03b13fbeec      1
     51 0x2bd0b9e782648b66744cc22373eb600f6b987869      1
     52 0x3136f3f1f3e0b927c55bc0a2ba5e87d23fd5f118      1
     53 0x3186e349d99b5e81252075013f510e1bba5167d0      1
     54 0x323826ac9cb32aeae90d76e05aec534b9a79170f      1
     55 0x33bfd3ba3bd175148d1bb1ee87e865253fd265e5      1
     56 0x348c6fd00127e32b5c9ff8b06a50e2f85038f6ff      1
     57 0x34b045e74b751dd5014653c73f910aea2402005a      1
     58 0x353d1af0fa732f6230ff4ab0eb2a6a3dcf4c54f2      1
     59 0x35b7b7c40de5cff2f89a6648b88c1c9c0257742c      1
     60 0x36a01c633e373d7ee2706b74af61da6b0ea7752d      1
     61 0x37ba996dd7eb738af38af7839a17ad19cad5f155      1
     62 0x387b36cd38ccec8ea245cdee725757291b39ac08      1
     63 0x38ab7734a25970f3a7f7ec792b3faf8058f8f67f      1
     64 0x3950d3f8813301bef45b29c014ee5a74c47d4249      1
     65 0x39a25b5281b1433f6bf1e1dad3f6f1c21fdc480d      1
     66 0x3a661a18cb0258119636dfdde098648c6ad5ba62      1
     67 0x3bc1b52d90fcd503031ca39f718474f3b42b2200      1
     68 0x3ef5f421e155ed29e6fa815fd875bfcd5f22ecd1      1
     69 0x402df14df2080c5d946a8e2fc1b4bf78cbb1e73a      1
     70 0x406bbd4b112e77d7091e36c23b92cca918b5419a      1
     71 0x40d6cc4ad15707844b320b1d3815e0f0cf09ff30      1
     72 0x431b251725cbfcc7453a403eb824cbdf11aeeb09      1
     73 0x45753b8b1f4a93c72a91d23667f846ea31783f42      1
     74 0x464278144ff19c4939f63418b79f3e6a239158c6      1
     75 0x478bb542f7658d635abba67edb987806dff5b83d      1
     76 0x47c553e889f7bfa75cf3775473180a04e9effcfa      1
     77 0x482293e4c21130f069169e5d7e9c99f239c5ee8e      1
     78 0x49fbb9f90e78dd1c0569816e1eeeaf2c28414ed7      1
     79 0x4b70222d33ec85c4cf182e42258a8f5ec7a9a6c2      1
     80 0x4c5287834576f7f1f3732fc2dddef2d2db3fea53      1
     81 0x4c8b46acb8ea62a154a9cfd3c446f6f4a678ebb0      1
     82 0x4df9c44e40d1bea726002fd6d45962908beebd28      1
     83 0x4e323ee346748d7a11c020a38e4feec377779533      1
     84 0x4f6465f3614f9b5f3d64bb327df8a8f34ed11923      1
     85 0x4fe91b32cfbba12f31ea73331f932a3f031ddb9f      1
     86 0x50b88f3f1ea8948731c6af4c0ebfdef1beccd6fa      1
     87 0x53e14582aec64690b81a742440dfee4b707a4d03      1
     88 0x53f5f9ccb500e49f2a4701c3d402d1cac34c8684      1
     89 0x541a51cccc9fc7991ed0b1c3236e156e533b11a4      1
     90 0x55eea5ec5e162003b0a51418f7dce3dfab7f7007      1
     91 0x569ff93aec513dac5b818714268c4b1e49abcc40      1
     92 0x56ddb1cc228ac1a88d6e88312b07e9560683bfcf      1
     93 0x5707751d3eb8d72e650fee6ed1ee62276fcb4c57      1
     94 0x5834d6a86737cb9a3d5cad8035a20b0be4ca526a      1
     95 0x58f606e6a973e57165bb07057a75f047f42455a7      1
     96 0x5962cb00aba5c7eb814f99f0ffa21da121b3c553      1
     97 0x5bd7c6331ad997f1f242ae29cd31e404c28fad71      1
     98 0x5c3ac0d47de47ef775ef968a5d3305dfdbe35d1d      1
     99 0x5c85b6e4364189af994edd34af49e7586808006f      1
    100 0x5f73b065c8412b43e3db33cad0efc5038040a597      1
    101 0x611909b813359e3facc833812559e07df37e418f      1
    102 0x61f9dc3a073f06a5c70cd48c7a85ea4203087c9d      1
    103 0x62050220633afe524d98fe3fccadbeacc2b96110      1
    104 0x63be81f380d82134b71ca8663142e8dc63f7c8a8      1
    105 0x6519cc343b07b3cc7d67d2ba35ae6b56da91f135      1
    106 0x65b5ea1e9df2f92f4fe3bdb6f2cc8550c608a534      1
    107 0x6605c7c3d96235beeb96650c025a96729dfc1f00      1
    108 0x66ff8f288c413e61c8ee807088ce301c03076e6e      1
    109 0x67e1e0ca2fb369f26889e56c38d490b63d75656e      1
    110 0x68fd77f58669f91c8dc407530535cbcc33641b9b      1
    111 0x6bfa6b948af76082f3f2b59a225e2be147c5c2e7      1
    112 0x6c6e93874216112ef12a0d04e2679ecc6c3625cc      1
    113 0x6e977c8155d914e7aa99331169939978a4d1b102      1
    114 0x6f10073d9154967b209f5c1cf578e42cd7a51b70      1
    115 0x6f11fda4804fdef80fa714eed41f18b0b2467db5      1
    116 0x7091d6cb3e3497664e29b0022d516eaeb6d9ffe6      1
    117 0x70f729e89eabacacf11293e40a700524112514d3      1
    118 0x70ff537a22312615a16e1ffef022c6b49b86d0ea      1
    119 0x7106e9b712fa396941a023186011ebbd7d0c201e      1
    120 0x749e68afa5e9a34e6b416df60ebdbfc75b42a130      1
    121 0x76f17e3a8063834f95f74b5ad6dc5d70b22b8ece      1
    122 0x77863da34ca9d4872b86fb9a4973792ca6a11e16      1
    123 0x77db37410eb0dbdf7f9dec1b196565344389b462      1
    124 0x786a4bebe6b0ac5710cd14e0f07907fb66dcd5ca      1
    125 0x78a9c6ed2174584a109957580164e12c04ec3ec5      1
    126 0x7e8be70e2ba74be134930a603baecc0fd79879cc      1
    127 0x7eca4f3d197da34c5761fc707326ef29935bc4d3      1
    128 0x7f7da962a340d1bd03dc547bc1b0faeae177c3b8      1
    129 0x7fa5599cac66b7a51a8772d76f26719e77ad7c50      1
    130 0x7fc200b56b7d0120193f034aa20a51eebe15eea2      1
    131 0x7fdba87da6ed499f19fccd1b8072b4dcf6991ffe      1
    132 0x80399f0ea2644839dc5f581fd3a021b0951a2fbc      1
    133 0x832408210f74c5cd8ac6fd8ebc92bb6b8a7834a0      1
    134 0x8411a8ba27d7f582c2860758bf2f901b851c30d3      1
    135 0x845a3b23e596c501b18a916c6510bc2adee4b264      1
    136 0x84df1fb4801e72cf6958a994dc0b690f9e7cb23b      1
    137 0x854f09dbd7c9fd93b52a1947b3dd4b41582ffadc      1
    138 0x85e5514fc67b6a2c228330cdcae543783a440947      1
    139 0x8625d262e2e2f5529499c79f5e26ff20d644f3a9      1
    140 0x8699793e22403b355185d8ff76e7392f98aafa46      1
    141 0x87cb21babb72d71bf3219a848704061dc701c835      1
    142 0x887b86b6b6957f7bbea88b8cefd392f39236a88c      1
    143 0x8891b0dda5c0bbb9b28eb2c6d02920834a9489ac      1
    144 0x8abe2749b67ad7debb7442c46ea4a0e003d4852a      1
    145 0x8ac236f975c5b44948c45c662834e28f017c7097      1
    146 0x8e816b12a3474a77183bfc5bc8175aa055743550      1
    147 0x8ec27e9f5bab92b94b15ab4f1de164c5a5da2e99      1
    148 0x8ef00b6ab619cf43527eadd86395b44152922228      1
    149 0x90198c48129ac7564933f8bab85669e303ff6b9f      1
    150 0x904a819e486d4af2653eec8e424599335e2f190e      1
    151 0x93c838add3ac72b42f65417408f900a7155e1bb8      1
    152 0x970bb85b997af2369b4be78e703e552d4d0d7158      1
    153 0x9827929b310ca4e0b525ccf00333c90a16720da8      1
    154 0x98f1bda2d45ffaca73fcf2154e9e007871e14934      1
    155 0x9b86738e6662bb9dc10ea60236261ea477d83782      1
    156 0x9ba855365d2cd25009a570f246acf645da469d1c      1
    157 0x9bfe4320207a44563bde522a7e48da2acb83a857      1
    158 0x9c910a749b9fc774d7079aee0cc20deb7c69dd56      1
    159 0x9d6061f52b2a1032dc396066350c57b8ba4da430      1
    160 0x9fca5cb296333aa780d3367951f766ab3d7d4098      1
    161 0xa077dafc545abbf2167b674d74717ba3c6127fd0      1
    162 0xa11d442e1cf701a27431f695ddb95e21288cc7b3      1
    163 0xa1d0395e4c539e41bb7f6a30571ab0026e0c6a46      1
    164 0xa23f27fe27b76c8df9b6d2e1c599e1ac80b44bef      1
    165 0xa2e2bbaf934eb8d1fb6bb39d9e0666337092c09c      1
    166 0xa4a1b6efb288060c92ab1e8d3e1e9a118f1fd8df      1
    167 0xa5a8fb5d3132510381a3206d232f375205f809e6      1
    168 0xa631197ecb22608b3041e81139f4d0e9ba02dbc2      1
    169 0xa647452748966a0f13d19de886f49c3dee2fe097      1
    170 0xa82b6ad343205d37a5271f55411f02d2d5208b9e      1
    171 0xa837c3d5fdfe4e878a3f8370df89cfd972b116e1      1
    172 0xa90dc6d356b622b9783f5020722e6f92b663438f      1
    173 0xab5d4c82f35faa1b10d526c7a744ae2ebe713a94      1
    174 0xad74b6eaefae55e9e73e5189e3873ffaa17d45c2      1
    175 0xadee5ee18e77ccee0717b707d28d589292b65c9c      1
    176 0xb29548cf5ab742069387c1f4599cdca5e0273e79      1
    177 0xb2d866297dbc7c7d8ad045c05afb48a95f28fb3c      1
    178 0xb42857239183cbcc7b3c8cafdc8d0eb2712cb890      1
    179 0xb552dbae15c36ee6fbed4ad24b05e57d7bb30b87      1
    180 0xb60d319664359e5045e204c80ec244bf545b9d95      1
    181 0xb65d28d8d260fa51ae6eeb97543ff77271ba25ee      1
    182 0xb6f79b687c2f61fc69fef6fad96aee61228d81e1      1
    183 0xb89d68928ecae680c0a5b0444e361a5b62f69554      1
    184 0xb8b8ce5e3d0ad5c0169e130c64048b6d3cfcd343      1
    185 0xb8d2c0f983a53f297036205e20edf2a65251a0f5      1
    186 0xba738302746e83700c72c532149f4f396356bdd5      1
    187 0xbb3be72a5f745a5a055dafa72b13a6e04fc5859e      1
    188 0xbb8e6a7f8f106def92da212dd55d1b5f664e18c6      1
    189 0xbcd5a68725612976bec27c1c194728206974afb5      1
    190 0xbe03a6efb0fb83185c3525bf6a917490f306b50f      1
    191 0xbf3a5770b01cfad844b875b27262aacfc95c60fc      1
    192 0xc00ae171acd39f84e66e19c7bad7bb676c1fe10c      1
    193 0xc4a843ddf533540474fddb7a9a03342348f6742b      1
    194 0xc4e19acf82441eab7e45a5e874cece70cd811434      1
    195 0xc6a3b88130ff5ede3beb6ddf21a7ab097b783cb5      1
    196 0xc6b39376721ea4331b9aa0e20609e8b92024bb4d      1
    197 0xc7ec028c706ec2fcfdcf66bba7e80ded634f6ca9      1
    198 0xc83bcd5fc46f61fe331bd63398f776adcb711f12      1
    199 0xc888c19a101979e0c29274895a86f0c3bab6cf7b      1
    200 0xc8cb3384fbadb63ab4d8849a2b3f464913fcdcf1      1
    201 0xc97958ff5370c56b7e0e6fb58437f00702714d49      1
    202 0xc9b5be616727486a8828ccedd143a27c93886f56      1
    203 0xca2df7209ffc40825980b3338812175d8e46a139      1
    204 0xca50cc37abaa58d19e3a23ccb086f17f8384cb3c      1
    205 0xca66e77b3e72a181e1f185b0fd7dc9467d4c60d4      1
    206 0xcd289df46f8bd595f3a886197c1b8c260ca26eaa      1
    207 0xcd8b72710595c69019c109aef5b1b92eea7f995f      1
    208 0xceaaf79ecd1f11db5fb9259d763c6d0288ce6531      1
    209 0xcee8ef3a9773d2aa6429a8b6d899969099b1fd98      1
    210 0xcf6f5a68d94165081a634ada997be3a93426c467      1
    211 0xcfa5ec915d09993706fea55c913eaba3e2383e76      1
    212 0xcfc2729173d1ef8d25050c76b84ec5af1fe7bd44      1
    213 0xd01fa4427a05fd7944557ae94b67d90109fad402      1
    214 0xd0ed643099041b805adec2e2af3329c06a20ffe6      1
    215 0xd311b220976fd80718ff10a8b9c90505697f5cc0      1
    216 0xd38af56dac5097569d6e85da7d93540a88e5be1b      1
    217 0xd3a74341adac943de6600468393bb6ca4431a7fd      1
    218 0xd55ca002c8ac7d73b8423696ed55f8f40652914a      1
    219 0xd7ce4706f31606981dc35255c8ce27934f9a7624      1
    220 0xd88c17aad286da44d7216f487b9a65ce1f7d4581      1
    221 0xd8a87ef6a7179c6bcc617eb32c3462ed7eb9e1cd      1
    222 0xdc819a832bfa55c933238a474954f3646da275ed      1
    223 0xdca649392b3a6de6a0d733fe5a5071ae12560f39      1
    224 0xdd6dd62617e52fb5ec9c8da05b3ed562459ae942      1
    225 0xddb7a9327025d79dae4de47ff01cc6aad4995a43      1
    226 0xde77230457de373c61ed2eae1aecff08d542fa60      1
    227 0xe01a97fc4607a388a41626d5e039fdecbfc6acd9      1
    228 0xe04954decbd22658a65278cf4a2fa0fd2c37db59      1
    229 0xe21de01cdcf9186a4d223818ebec987335a3b49e      1
    230 0xe2c945b5093f6314e889e0c5b260d86457c47301      1
    231 0xe3a6f6025d361b75ac0f95b5e3820f12b357deee      1
    232 0xe5dfedcaadf4df48f2fcc56663b89ed0ad73da3e      1
    233 0xe86c12eca33b154d6eb3e367fe50918929c4bc30      1
    234 0xe9a567610a8f4efc33cca1f319d62b76b804c5f1      1
    235 0xe9d53246209e4cd87dbc5d1fee71f44bff89a611      1
    236 0xeac5f9b3cd48123a69fe69ce93a7f58100a56552      1
    237 0xeaf6c384ebcd826dd6a5d8f305a40683a1cf77f0      1
    238 0xeb83e695adcac2e83f290d2d2815fc58e6491d7a      1
    239 0xebe923766012f51d6b003ff93932d0aa8fcf768d      1
    240 0xec4762fb859a53c15d1689c08fd5e48c5c1c47fa      1
    241 0xec9e512fe7e90134d8ca7295329ccb0a57c91ecb      1
    242 0xee49f82e58a1c2b306720d0c68047cbf70c11fb5      1
    243 0xee91d62eb5aaea933efbfd0790613af5db305006      1
    244 0xef006d5ac08e1c337f89923ded88391d041b1b1d      1
    245 0xf0c3a1f1d9b79d5a8decb8e92973cdb56b7e81da      1
    246 0xf110b384ca4f89b1377f16d10d47866487b63056      1
    247 0xf311b7b3751618d2762ac3ef62e8be61662c2ffa      1
    248 0xf4d1a203b3a79385bcbf66960051522402ac917e      1
    249 0xf67da29a2382e52a94c7984049d1e20e072c1476      1
    250 0xf8027e0f03c95782ef9be1826828bee931c1ab83      1
    251 0xf824ef230b0f7fc9038f9fdbc249717419219e77      1
    252 0xfe0dad687399f50ae473d8028576e1451a53b518      1
    253 0xff1697cd18f03242ea521a80334a9060f0c25c7a      1
    254 0xff43878817cbdc09f474dcf88d5495b9b41222ac      1

## Allow Gradient Phase 1

``` r
c(allow_gradient_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_gradient_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 74 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x04df8d02f912d34fef12a1b0488ee56fd6f7416c      1
     2 0x0a98f97e89743406a8611e1b4219a073b60ffed3      1
     3 0x0b3e3a27d85996e20b2cdd0e7f1e1208816d05b8      1
     4 0x0ca4dc095608c6b153019f734106581c85f2dcc3      1
     5 0x129c6695dfe7a906bd8fda202d26dfff601f83a4      1
     6 0x1566ae673ae80725bcce901b486c336e6acef465      1
     7 0x1a9aee029a1b8c69c4404890c85715cc726746c7      1
     8 0x22fbaa2dd20b848084545515fb04b9c846942779      1
     9 0x248458947c120ca057ec028b3fe7e4b3f26fdb3d      1
    10 0x27b1abafb2cb065cfaf41b4b7ee95d27192151b2      1
    11 0x2924196a2ec71ea4ae1b1357381eccdcee6c18f2      1
    12 0x2947cd7346b3a372b357256b8955ebd4b5735987      1
    13 0x2f2242b447a54cb110b8a7991cf8a27054ee6921      1
    14 0x3511ae23ee25e2b97dce883c7d496ff5d18f1dfa      1
    15 0x3cb63b82d778105e43f064ed739b0655f1f0fb87      1
    16 0x3d0a1d20ac580ca8fa35888c92dd0859887f3398      1
    17 0x40d2f4399d23f9afb82d0a6b73055f13208614f9      1
    18 0x43b0bd27d5016e969193412933387b0dd4cf3e0a      1
    19 0x477ea7a022e51b6eb0dcb6d802fb5f0cfc3b4a81      1
    20 0x48464efe55fbcae8ae0c992b306afcf21d4910cf      1
    21 0x520e8d6c5b2dbbe62bf8a40653a314d879b19b86      1
    22 0x53006f95def268f88dc1b8216654ab56f3afd052      1
    23 0x53562e8ecee54356dc89ad263a444528b34d6c80      1
    24 0x575f6540c16a72696c14a17fa64f049992d661ab      1
    25 0x59068075a799594db03c0255eed68e8e121155c8      1
    26 0x5fdb5fdb61bc2975f3be446c5ae9c5df490f55d2      1
    27 0x60d2bc84a71af61b82d1b4168c9103ff6c4898eb      1
    28 0x615502edd5cf2d59244dd0dd53d3c8ec035e172b      1
    29 0x61d9d9cc8c3203dab7100ea79ced77587201c990      1
    30 0x64f7de90dc79d775703bbec66a1591c7a26a22f0      1
    31 0x665654f2d2a151be2d0f8e3697e6ce780f732af2      1
    32 0x699990a8e7ada9e92c932d6e8fb365024fc74b43      1
    33 0x69e68074f1aada957edd39c5eae0069973343f30      1
    34 0x6f0735bf1e6c69030d6990cdd580345b370eb50a      1
    35 0x6f113b0c8c7266d7514e4986e3d6aaf013b6754f      1
    36 0x729fcbb6e1289c88ce5113ec1c83a48a8e3c9f2f      1
    37 0x73bcb8c5e30bf85806aade7fc36f16c6b80fd3b9      1
    38 0x75005a05bc163b85991b9c9facbcf3155372422e      1
    39 0x78a576d3751e8cc8f30c54c367fe8e5a2cd71228      1
    40 0x7e5ab36876a267560e7191cedbe99ee7bc04bc30      1
    41 0x82139687faae8a29851902783e02e699de0e0846      1
    42 0x85603a042455d2f36f3ec52c1c756344aa70c5ef      1
    43 0x88ccdfe9dd047b4cec4c1102d2d803e2d8bf683e      1
    44 0x896b94f4f27f12369698c302e2049cae86936bbb      1
    45 0x8ba68cfe71550efc8988d81d040473709b7f9218      1
    46 0x8bd9c4b5ea8f6f14c9b14d830ddb67f3720d77f6      1
    47 0x8fc2326375174bfe3baccc98cb9906921dc9599c      1
    48 0x982d3c5223f6b5794fccb3208eb164d042cf2526      1
    49 0xab2056903a7b62bac46f45a3d7a70ac799ca88cb      1
    50 0xae0d16586e5d60d334624c115216a52b9b1a0335      1
    51 0xb6cf25f5cf8a1e1727d988facdd47f1dfc492caf      1
    52 0xb735af7ae1a77d1ec764c862c2c09bdbf2b34b27      1
    53 0xba4575ea27041d99e6614ec02318f1e23a623fe2      1
    54 0xbbdd72fcce73c2626719be00259ddffef0d5673d      1
    55 0xbf270918afe2ad16093ddce904fc358ad337cefa      1
    56 0xbf814810b44978de273191fd612aa47f7b69d564      1
    57 0xc02e6b0d0c1a5d8cd26beeba0fe8d76c5d2f19b9      1
    58 0xc2419841dcb9a0f8906d06463ae24e00e1470846      1
    59 0xc6400a5584db71e41b0e5dfbdc769b54b91256cd      1
    60 0xcaa1c396e70384db58dd33be74b26fb119e98c3a      1
    61 0xcabd5f77ca9d48f4ef9793f20de42ad39ce93979      1
    62 0xd007058e9b58e74c33c6bf6fbcd38baab813cbb6      1
    63 0xd3e401814d1faa8ca0419ecccbfee93ac7b15b31      1
    64 0xde112b77a1a72c726d8c914c23dceaa53273c351      1
    65 0xe359ab04cec41ac8c62bc5016c10c749c7de5480      1
    66 0xe3b41ae8785e4107cc69f988042ff4a66a367fac      1
    67 0xea39c551834d07ee2ee87f1ceff843c308e089af      1
    68 0xef5ab90a44b68d4f5e3f6be6af4bedb12cd2c66e      1
    69 0xf15a6b54e68884d27e1bebb1624d70c227b7d04b      1
    70 0xf2c5f1fd977dbd6de9d04bc4e62dff722d4bb1a1      1
    71 0xf8aace471723dba6514f6c6d43651611a0c34529      1
    72 0xfcbca52b23db28470eb97680d18e988d8b60b608      1
    73 0xfd22004806a6846ea67ad883356be810f0428793      1
    74 0xfd849d8cf365fe98bc60025ff7fab45cef0f557b      1

## Allow 6529 Phase 2

``` r
c(allow_raw, allow_singles) %>%
tally() %T>%
readr::write_csv(file="allow_6529_phase2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 4 × 2
      address                                    amount
      <chr>                                       <int>
    1 0x13eac939d3c7ff1a985d164d1f14411505b4c822      1
    2 0x2c8875f34ceb219f61b7453b2c5f100ec2f6ed33      1
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
