
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance         contract        
     Length:225         Length:225         Min.   : 1.000   Length:225        
     Class :character   Class :character   1st Qu.: 1.000   Class :character  
     Mode  :character   Mode  :character   Median : 1.000   Mode  :character  
                                           Mean   : 1.071                     
                                           3rd Qu.: 1.000                     
                                           Max.   :10.000                     
         name          
     Length:225        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 21146969 # https://etherscan.io/block/21146969
block_hash <- "0x8b2ddc2b9e19d97c1fb918554c644b8258f741aba80d271def6d6b4aa9d91cac"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4854 

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

allow_artist1    <- pick(snapshot, contracts=c("SuperRare","TheMoonSparksGoddesses","FanArtOtherMemorabilia","AngieMathotCollection","PowerfulWomen","PunksinParis","CartoonCollection"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("PtiteAngele20Editions","OfLoveLifePoetry","SeasonalGreetingsEditions","PtiteAngelEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("SuperRare","TheMoonSparksGoddesses","FanArtOtherMemorabilia","AngieMathotCollection","PowerfulWomen","PunksinParis","CartoonCollection","PtiteAngele20Editions","OfLoveLifePoetry","SeasonalGreetingsEditions","PtiteAngelEditions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 35 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x052564eb0fd8b340803df55def89c25c432f43f4      1
     2 0x155c18fcdc5f593728c5d5d158509271835decd5      1
     3 0x1aa90a5dc8a53a71826c641a3f687443586a6b2c      1
     4 0x21301d901db04724597d1b6012ac49878157580d      1
     5 0x3c322493ced5e217aaa78a3eb00ee0049cc6422b      1
     6 0x3d5078da5ae6c6608f987f8cb00ea2fe31956490      1
     7 0x45e267466a8de0196998031d59637fd5833ea69d      1
     8 0x46b2bd5c888e6d57a839c559fd6076f2c15a8cb1      1
     9 0x4ffc692a596d408bd33bf5f3821b3e21a618fe52      1
    10 0x549266c062531ec5fd3979b69d10f0720dce3360      1
    11 0x57d1e246d2e32f6f9d10ec55fc41e8b2e2988308      1
    12 0x5bea445ee2fd3a9efa65c1c9ead66e0141fb9f21      1
    13 0x5eff877eaf954d78fed508c80c68400485e673ef      1
    14 0x6ea5b321fe6204448838b6ee03aaf3ec9c294484      1
    15 0x725f7066017093b83a8942d52e0750ba9b20fdcc      1
    16 0x776c3b715b8f94719371c78da5aaf968180a1ff9      1
    17 0x778e3b5ab41f6fa20bb5812fc1ef5929bcbc422a      1
    18 0x7d544a853dbcd39a53315e7002f4951a6d2f080d      1
    19 0x8a90f7679506b79a9175bcf6cb755ecba8e905f0      1
    20 0x8cf7774f906989e900d5b75bf8787412c9f4b0a4      1
    21 0x900e7ad1ab18cebb4c21f71795364e9b636831ca      1
    22 0x9939a86f2e61dee9c2addf32942462284b62f978      1
    23 0x9e10c9fee571802194747fc9ded2aff9c846a19d      1
    24 0xa43241196b594016684dada50355c55c9f14ce70      1
    25 0xa6c46fa09b08ed529a73aae6c8315c38ff5711ad      1
    26 0xa77f7395bad3d34e641549533134f5cdcec31a30      1
    27 0xad9039b7ac9bf08b9b2c3afdd6cdec3f26fc9a45      1
    28 0xbd7a1c16d7b3af4af6d1a0fd7437466d765656ce      1
    29 0xc40df87e16339f21fbb2e59fb38bf2a957a16ffd      1
    30 0xce001a427e7d54de6de9510522809bf95b1ba8ad      1
    31 0xd817c9eaa3cf1a8f4d7353b4967aceeb4bb51fe1      1
    32 0xe02e6836062fd285cc9ac8b5396843305ca1a349      1
    33 0xebe5f307aeeaa63b5dc1f421b6a44ef82642f33f      1
    34 0xf770e42fd25d745e9f25ab57cca3e96f7fe62d14      1
    35 0xff13f9b23a16a82c3ccdd29ece6af667eeb15834      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 100 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x02fc303a3675de2ab5494cc0efd62ad02d73102e      1
      2 0x06dac6a2dcfa0f0dfb3e7e766515eef5b2c6399a      1
      3 0x0962835897d330a2e7a194e2da816dd776980cd2      1
      4 0x09d0cfaa9819f8499c17bcc5f5353f0049137728      1
      5 0x0a8825c03100e6cd1a82266729bf90f7fe71dc6d      1
      6 0x0bb75bef057da63a0ae4b25fe9adafd35cd92b87      1
      7 0x0c35506a1710fdb463062615e11ecc9c2541f2d0      1
      8 0x0d3788ab5c4b420e0add0dc8f8fa48cb77ef44aa      1
      9 0x0f5050adaa271deff7db29bf5b5108891aea137b      1
     10 0x0f9ef34d0ad4b248742f5b4d2880ccef0415c3a8      1
     11 0x11d7c4af8960cab1324893dd06be55b28580299c      1
     12 0x144b482172b8021b081ee2d849fa3166af078fe1      1
     13 0x1809754df5ac133a1cc7d4eaf45ed38cf9b39ec8      1
     14 0x189e0b364f7e3eed2aa3b463d96e9ebd0ab0b0a9      1
     15 0x19461698453e26b98cee5b984e1a86e13c0f68be      1
     16 0x19fedef56411bb49afaf65f7e4be50629be17632      1
     17 0x1aa963e68cfde4dab1df1e15e70b2bb4103c4848      1
     18 0x1bec0e6266e97f0c7c42aa720959e46598dd8ce1      1
     19 0x21339e2ce5ce1d7a19a28e774513619c0c6259da      1
     20 0x2618f923dd7797e1442ef0100992a21015b7c788      1
     21 0x263e50e9ab3e0681291658db0231bc622300287a      1
     22 0x268b3d3de3131248fec0dda6afbb5f3b5c9f5cd8      1
     23 0x2844a1f5c5164b9adae211df8931838a2d33b5ed      1
     24 0x2a193f2d2095341861b991cd6fd22ec1e9b9507c      1
     25 0x2b85b841b85fe6bf05e1a41330fe13882ed1f286      1
     26 0x2c09bd997d2b8adc73ce0af34926d36cc8ed2c0c      1
     27 0x2cb3338f518b2d61470b4d1d503bdc84c1a04ecd      1
     28 0x3368b4733a4552b38b7d81c215d88cac4c78d8e5      1
     29 0x385fcbd4fdaa1ae35263141ba39b90f305016af4      1
     30 0x3929eff8e0feb3166992942d8b9030d95646c3ce      1
     31 0x4095301a454311b507c0ef52924a7753f0e1406e      1
     32 0x4202369d813fa05203bd58e9c3e3a50c78b4c701      1
     33 0x42d6977c0e0596dfc681afc637cfa53e48301488      1
     34 0x438ca90202f5bbbbd7f1730072886dbf9579057c      1
     35 0x459fe0c0955d1a81aaa98190d75734c118fb4654      1
     36 0x473ac44a488ffc3f8237f45e469f60b416690f3d      1
     37 0x47dc3a7aec5c0e1f4b2c71303a4b1eaa0bee3e4c      1
     38 0x48645cd1770e65bc68bd0dc42874289155537bdf      1
     39 0x48eea169dc4b51e434ef5104e1d5d13b18aaae20      1
     40 0x498d18b5b3c0b7f56635f3f52d4c466618ed21ad      1
     41 0x4d0838582cab6aa41cc2fb5e65f87f2958dae620      1
     42 0x4f99d481981f464d824592604f58bcee9cb8abf6      1
     43 0x5470a0ad35a6066fdd1b47d18b6bb04bffb9f59a      1
     44 0x564a8e13d7dd23d5525160d204165bdbcb69b4db      1
     45 0x5828a6d7731588779595b47f1e23b9424652c087      1
     46 0x59794536300ef36420c9db14a91ca77eb4e4bbb3      1
     47 0x5a04ad067b3a022bbc03dd49f105d462b6bbc1ff      1
     48 0x5cd53a7c366b95e0ba100a67cd2f6ad77a767c4f      1
     49 0x64d64b8bd0af3c8ba717914b021287d4d0e05e7e      1
     50 0x66f5ddc253852f407d334f8e90e3bc46fdaaecaa      1
     51 0x66fb2d6c59c887ec535c3e80f8bcc41db9522997      1
     52 0x68099add8543308f428c30a753cd9e05158f2140      1
     53 0x72eb30d3ca53f5e839325e2eacf535e70a9e6987      1
     54 0x768f7248296e71f694c6afe3ea06923093341f0d      1
     55 0x7a175df137a809ffd34cae46cf9a650d4e28ba46      1
     56 0x7b42a219bb14d0719757a391d7cc6aa7f371e144      1
     57 0x7cb8c06443aded7a0e51ff66efb2421d3b025465      1
     58 0x7e76134ae5d8c226a22c9bd889933ed6944ee2e8      1
     59 0x7fa18ffbca3f6d6443c3556a1581dd2e71e42cdd      1
     60 0x81c77f8ac639955a7bfd5f25f9ab533b0d388f34      1
     61 0x8231ff66a7bbdc14ad89782a51c939bbc4c92784      1
     62 0x8715dcbf7394a4e6ad98ac5d112e7fb6c3ea9f70      1
     63 0x882530826b5e11a6954bced842f196fb6de59443      1
     64 0x8889ebb11295f456541901f50bcb5f382047caac      1
     65 0x895d49ea015b1e9da4af776239d0f8b029d660b2      1
     66 0x97dfa8a7d0f5c29234bfc15569700c0d4fb5437f      1
     67 0x99581599e8823ee9130380281478e729e926353f      1
     68 0x9997205cd290fb2d7bf537c919a572d16e5020a1      1
     69 0x99ddb57f6cde05fc7666920d36e9d51eafbc79f4      1
     70 0x9c335e4d8cacda86647ff4497789019c7a762a62      1
     71 0x9e8fdfc3d41b26a2870c293dc7e1be8675fa8b7b      1
     72 0xa29ad7cc2fe7ccd79805914c1a983aa3a0176d09      1
     73 0xa355440ff4995bf5d2b4453e72514465d22ec81a      1
     74 0xa3ec0ea98a1a12c56a75e214a8972809e7594362      1
     75 0xa432dc73ee0d60c193c543de42b6ae1e195020ee      1
     76 0xa633f5b4578b00e8598a16095a0a0df02ad95aa1      1
     77 0xa87648128d0bb0a4de3eb1cfc08639a752cf96c1      1
     78 0xa88fe6fa01fcc112bb2164c6e37d63395b923e5f      1
     79 0xaceec5dca9365c95972da570123e8dad38201869      1
     80 0xaf5e2e7f800029060692765e684622dfc92b512d      1
     81 0xb06d2716e71d927ed6b1d15ae8f4e403fea59350      1
     82 0xb721640f923f9eac62032097cbd916501a3d3e0f      1
     83 0xb8b86f77a34d5679daa6df4ae830294c6e108b88      1
     84 0xbc5a7cf988f7e7e8f3893cd0ee1d0786cd4af889      1
     85 0xbd440f1465cb5133b98dc6e8f6514feba6aa8a54      1
     86 0xbf89b34ede7a7dea10bcb8be91e345344ac3039e      1
     87 0xc4e8d1717c3921454792241ab7ba481f1c5304f2      1
     88 0xc74a7eee8a15383939d0546cae0812dda23e19c9      1
     89 0xc9f84ca819fac0bcf4cc58948ddd477385ce69a7      1
     90 0xcc48857242141cc7f4f8555f8a3d5bed03556c19      1
     91 0xd29d32b0f6915b609f3f283547537099b5b604d4      1
     92 0xdaff7ccb1b7b9d008b977be2611a1149c797f754      1
     93 0xddb2a15376915573f487c6f50a1d4bf49f91905e      1
     94 0xdf6160fe3e5a7d6df67c2bb3d7e100b47efa243c      1
     95 0xe4e2ea55c782e649747f645c993181eb3006e215      1
     96 0xe5b673875fc6fdceaee988fc1f0b8a785fcbacae      1
     97 0xe84d4f3463632587d03a26bb68979ef317bda4d7      1
     98 0xeafb51022207d408170a2005d1ebee09454677b0      1
     99 0xeeb3dc6e83be1cb2fe906353eacf5b64af6ea096      1
    100 0xfe99460b27bd6d4a3e8adb29bdf038be6684dd77      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 135 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x02fc303a3675de2ab5494cc0efd62ad02d73102e      1
      2 0x052564eb0fd8b340803df55def89c25c432f43f4      1
      3 0x06dac6a2dcfa0f0dfb3e7e766515eef5b2c6399a      1
      4 0x0962835897d330a2e7a194e2da816dd776980cd2      1
      5 0x09d0cfaa9819f8499c17bcc5f5353f0049137728      1
      6 0x0a8825c03100e6cd1a82266729bf90f7fe71dc6d      1
      7 0x0bb75bef057da63a0ae4b25fe9adafd35cd92b87      1
      8 0x0c35506a1710fdb463062615e11ecc9c2541f2d0      1
      9 0x0d3788ab5c4b420e0add0dc8f8fa48cb77ef44aa      1
     10 0x0f5050adaa271deff7db29bf5b5108891aea137b      1
     11 0x0f9ef34d0ad4b248742f5b4d2880ccef0415c3a8      1
     12 0x11d7c4af8960cab1324893dd06be55b28580299c      1
     13 0x144b482172b8021b081ee2d849fa3166af078fe1      1
     14 0x155c18fcdc5f593728c5d5d158509271835decd5      1
     15 0x1809754df5ac133a1cc7d4eaf45ed38cf9b39ec8      1
     16 0x189e0b364f7e3eed2aa3b463d96e9ebd0ab0b0a9      1
     17 0x19461698453e26b98cee5b984e1a86e13c0f68be      1
     18 0x19fedef56411bb49afaf65f7e4be50629be17632      1
     19 0x1aa90a5dc8a53a71826c641a3f687443586a6b2c      1
     20 0x1aa963e68cfde4dab1df1e15e70b2bb4103c4848      1
     21 0x1bec0e6266e97f0c7c42aa720959e46598dd8ce1      1
     22 0x21301d901db04724597d1b6012ac49878157580d      1
     23 0x21339e2ce5ce1d7a19a28e774513619c0c6259da      1
     24 0x2618f923dd7797e1442ef0100992a21015b7c788      1
     25 0x263e50e9ab3e0681291658db0231bc622300287a      1
     26 0x268b3d3de3131248fec0dda6afbb5f3b5c9f5cd8      1
     27 0x2844a1f5c5164b9adae211df8931838a2d33b5ed      1
     28 0x2a193f2d2095341861b991cd6fd22ec1e9b9507c      1
     29 0x2b85b841b85fe6bf05e1a41330fe13882ed1f286      1
     30 0x2c09bd997d2b8adc73ce0af34926d36cc8ed2c0c      1
     31 0x2cb3338f518b2d61470b4d1d503bdc84c1a04ecd      1
     32 0x3368b4733a4552b38b7d81c215d88cac4c78d8e5      1
     33 0x385fcbd4fdaa1ae35263141ba39b90f305016af4      1
     34 0x3929eff8e0feb3166992942d8b9030d95646c3ce      1
     35 0x3c322493ced5e217aaa78a3eb00ee0049cc6422b      1
     36 0x3d5078da5ae6c6608f987f8cb00ea2fe31956490      1
     37 0x4095301a454311b507c0ef52924a7753f0e1406e      1
     38 0x4202369d813fa05203bd58e9c3e3a50c78b4c701      1
     39 0x42d6977c0e0596dfc681afc637cfa53e48301488      1
     40 0x438ca90202f5bbbbd7f1730072886dbf9579057c      1
     41 0x459fe0c0955d1a81aaa98190d75734c118fb4654      1
     42 0x45e267466a8de0196998031d59637fd5833ea69d      1
     43 0x46b2bd5c888e6d57a839c559fd6076f2c15a8cb1      1
     44 0x473ac44a488ffc3f8237f45e469f60b416690f3d      1
     45 0x47dc3a7aec5c0e1f4b2c71303a4b1eaa0bee3e4c      1
     46 0x48645cd1770e65bc68bd0dc42874289155537bdf      1
     47 0x48eea169dc4b51e434ef5104e1d5d13b18aaae20      1
     48 0x498d18b5b3c0b7f56635f3f52d4c466618ed21ad      1
     49 0x4d0838582cab6aa41cc2fb5e65f87f2958dae620      1
     50 0x4f99d481981f464d824592604f58bcee9cb8abf6      1
     51 0x4ffc692a596d408bd33bf5f3821b3e21a618fe52      1
     52 0x5470a0ad35a6066fdd1b47d18b6bb04bffb9f59a      1
     53 0x549266c062531ec5fd3979b69d10f0720dce3360      1
     54 0x564a8e13d7dd23d5525160d204165bdbcb69b4db      1
     55 0x57d1e246d2e32f6f9d10ec55fc41e8b2e2988308      1
     56 0x5828a6d7731588779595b47f1e23b9424652c087      1
     57 0x59794536300ef36420c9db14a91ca77eb4e4bbb3      1
     58 0x5a04ad067b3a022bbc03dd49f105d462b6bbc1ff      1
     59 0x5bea445ee2fd3a9efa65c1c9ead66e0141fb9f21      1
     60 0x5cd53a7c366b95e0ba100a67cd2f6ad77a767c4f      1
     61 0x5eff877eaf954d78fed508c80c68400485e673ef      1
     62 0x64d64b8bd0af3c8ba717914b021287d4d0e05e7e      1
     63 0x66f5ddc253852f407d334f8e90e3bc46fdaaecaa      1
     64 0x66fb2d6c59c887ec535c3e80f8bcc41db9522997      1
     65 0x68099add8543308f428c30a753cd9e05158f2140      1
     66 0x6ea5b321fe6204448838b6ee03aaf3ec9c294484      1
     67 0x725f7066017093b83a8942d52e0750ba9b20fdcc      1
     68 0x72eb30d3ca53f5e839325e2eacf535e70a9e6987      1
     69 0x768f7248296e71f694c6afe3ea06923093341f0d      1
     70 0x776c3b715b8f94719371c78da5aaf968180a1ff9      1
     71 0x778e3b5ab41f6fa20bb5812fc1ef5929bcbc422a      1
     72 0x7a175df137a809ffd34cae46cf9a650d4e28ba46      1
     73 0x7b42a219bb14d0719757a391d7cc6aa7f371e144      1
     74 0x7cb8c06443aded7a0e51ff66efb2421d3b025465      1
     75 0x7d544a853dbcd39a53315e7002f4951a6d2f080d      1
     76 0x7e76134ae5d8c226a22c9bd889933ed6944ee2e8      1
     77 0x7fa18ffbca3f6d6443c3556a1581dd2e71e42cdd      1
     78 0x81c77f8ac639955a7bfd5f25f9ab533b0d388f34      1
     79 0x8231ff66a7bbdc14ad89782a51c939bbc4c92784      1
     80 0x8715dcbf7394a4e6ad98ac5d112e7fb6c3ea9f70      1
     81 0x882530826b5e11a6954bced842f196fb6de59443      1
     82 0x8889ebb11295f456541901f50bcb5f382047caac      1
     83 0x895d49ea015b1e9da4af776239d0f8b029d660b2      1
     84 0x8a90f7679506b79a9175bcf6cb755ecba8e905f0      1
     85 0x8cf7774f906989e900d5b75bf8787412c9f4b0a4      1
     86 0x900e7ad1ab18cebb4c21f71795364e9b636831ca      1
     87 0x97dfa8a7d0f5c29234bfc15569700c0d4fb5437f      1
     88 0x9939a86f2e61dee9c2addf32942462284b62f978      1
     89 0x99581599e8823ee9130380281478e729e926353f      1
     90 0x9997205cd290fb2d7bf537c919a572d16e5020a1      1
     91 0x99ddb57f6cde05fc7666920d36e9d51eafbc79f4      1
     92 0x9c335e4d8cacda86647ff4497789019c7a762a62      1
     93 0x9e10c9fee571802194747fc9ded2aff9c846a19d      1
     94 0x9e8fdfc3d41b26a2870c293dc7e1be8675fa8b7b      1
     95 0xa29ad7cc2fe7ccd79805914c1a983aa3a0176d09      1
     96 0xa355440ff4995bf5d2b4453e72514465d22ec81a      1
     97 0xa3ec0ea98a1a12c56a75e214a8972809e7594362      1
     98 0xa43241196b594016684dada50355c55c9f14ce70      1
     99 0xa432dc73ee0d60c193c543de42b6ae1e195020ee      1
    100 0xa633f5b4578b00e8598a16095a0a0df02ad95aa1      1
    101 0xa6c46fa09b08ed529a73aae6c8315c38ff5711ad      1
    102 0xa77f7395bad3d34e641549533134f5cdcec31a30      1
    103 0xa87648128d0bb0a4de3eb1cfc08639a752cf96c1      1
    104 0xa88fe6fa01fcc112bb2164c6e37d63395b923e5f      1
    105 0xaceec5dca9365c95972da570123e8dad38201869      1
    106 0xad9039b7ac9bf08b9b2c3afdd6cdec3f26fc9a45      1
    107 0xaf5e2e7f800029060692765e684622dfc92b512d      1
    108 0xb06d2716e71d927ed6b1d15ae8f4e403fea59350      1
    109 0xb721640f923f9eac62032097cbd916501a3d3e0f      1
    110 0xb8b86f77a34d5679daa6df4ae830294c6e108b88      1
    111 0xbc5a7cf988f7e7e8f3893cd0ee1d0786cd4af889      1
    112 0xbd440f1465cb5133b98dc6e8f6514feba6aa8a54      1
    113 0xbd7a1c16d7b3af4af6d1a0fd7437466d765656ce      1
    114 0xbf89b34ede7a7dea10bcb8be91e345344ac3039e      1
    115 0xc40df87e16339f21fbb2e59fb38bf2a957a16ffd      1
    116 0xc4e8d1717c3921454792241ab7ba481f1c5304f2      1
    117 0xc74a7eee8a15383939d0546cae0812dda23e19c9      1
    118 0xc9f84ca819fac0bcf4cc58948ddd477385ce69a7      1
    119 0xcc48857242141cc7f4f8555f8a3d5bed03556c19      1
    120 0xce001a427e7d54de6de9510522809bf95b1ba8ad      1
    121 0xd29d32b0f6915b609f3f283547537099b5b604d4      1
    122 0xd817c9eaa3cf1a8f4d7353b4967aceeb4bb51fe1      1
    123 0xdaff7ccb1b7b9d008b977be2611a1149c797f754      1
    124 0xddb2a15376915573f487c6f50a1d4bf49f91905e      1
    125 0xdf6160fe3e5a7d6df67c2bb3d7e100b47efa243c      1
    126 0xe02e6836062fd285cc9ac8b5396843305ca1a349      1
    127 0xe4e2ea55c782e649747f645c993181eb3006e215      1
    128 0xe5b673875fc6fdceaee988fc1f0b8a785fcbacae      1
    129 0xe84d4f3463632587d03a26bb68979ef317bda4d7      1
    130 0xeafb51022207d408170a2005d1ebee09454677b0      1
    131 0xebe5f307aeeaa63b5dc1f421b6a44ef82642f33f      1
    132 0xeeb3dc6e83be1cb2fe906353eacf5b64af6ea096      1
    133 0xf770e42fd25d745e9f25ab57cca3e96f7fe62d14      1
    134 0xfe99460b27bd6d4a3e8adb29bdf038be6684dd77      1
    135 0xff13f9b23a16a82c3ccdd29ece6af667eeb15834      1

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
